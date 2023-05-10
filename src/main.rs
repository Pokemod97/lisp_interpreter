use chumsky::prelude::*;
#[derive(Debug, PartialEq, Clone)]
enum Expr {
    Const(u32),
    Var(usize),
    Mult(Box<Expr>, Box<Expr>),
    Sub1(Box<Expr>),
    Zero(Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Let(Box<Expr>, Box<Expr>),
    Lambda(Box<Expr>),
    App(Box<Expr>, Box<Expr>),
}
fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    recursive(|expr| {
        let constant = text::keyword("const")
            .ignore_then(text::int(10).padded())
            .map(|val: String| val.parse().unwrap())
            .map(Expr::Const);
        let var = text::keyword("var")
            .ignore_then(text::int(10).padded())
            .map(|val: String| val.parse().unwrap())
            .map(Expr::Var);
        let single_box = choice::<_, Simple<char>>((
            text::keyword("sub1").to(Expr::Sub1 as fn(_) -> _),
            text::keyword("zero").to(Expr::Zero as fn(_) -> _),
            text::keyword("lambda").to(Expr::Lambda as fn(_) -> _),
        ))
        .padded()
        .then(expr.clone())
        .map(|(x, y)| x(Box::new(y)));
        let double_box = choice((
            text::keyword("mult").to(Expr::Mult as fn(_, _) -> _),
            text::keyword("let").to(Expr::Let as fn(_, _) -> _),
            text::keyword("app").to(Expr::App as fn(_, _) -> _),
        ))
        .then(expr.clone())
        .padded()
        .then(expr.clone())
        .map(|((x, y), z)| x(Box::new(y), Box::new(z)));
        let r#if = text::keyword("if")
            .ignore_then(expr.clone())
            .then(expr.clone())
            .then(expr.clone())
            .map(|((x, y), z)| Expr::If(Box::new(x), Box::new(y), Box::new(z)));

        let inner = choice((constant, var, single_box, double_box, r#if));
        inner.padded().delimited_by(just("("), just(")")).padded()
    })
}

#[test]
fn parse_const() {
    assert_eq!(parser().parse("(const 1)").unwrap(), Expr::Const(1));
    assert_eq!(
        parser().parse("  (    const 234 )     ").unwrap(),
        Expr::Const(234)
    );
}
#[test]
fn parse_var() {
    assert_eq!(parser().parse("(var 1)").unwrap(), Expr::Var(1));
    assert_eq!(
        parser().parse("  (    var 234 )     ").unwrap(),
        Expr::Var(234)
    );
}
#[test]
fn parse_sub1() {
    let parse_value = parser().parse("(sub1 (const 1))").unwrap();
    assert_eq!(parse_value, Expr::Sub1(Box::new(Expr::Const(1))));
}

#[test]
fn parse_zero() {
    let parse_value = parser().parse("(zero (const 1))").unwrap();
    assert_eq!(parse_value, Expr::Zero(Box::new(Expr::Const(1))));
}

#[test]
fn parse_lambda() {
    let parse_value = parser().parse("(lambda (const 1))").unwrap();
    assert_eq!(parse_value, Expr::Lambda(Box::new(Expr::Const(1))));
}
#[test]
fn parse_mult() {
    let parse_value = parser().parse("(mult (const 2) (const 3))").unwrap();
    assert_eq!(
        parse_value,
        Expr::Mult(Box::new(Expr::Const(2)), Box::new(Expr::Const(3)))
    );
}
#[test]
fn parse_let() {
    let parse_value = parser().parse("(let (const 2) (const 3))").unwrap();
    assert_eq!(
        parse_value,
        Expr::Let(Box::new(Expr::Const(2)), Box::new(Expr::Const(3)))
    );
}
#[test]
fn parse_if() {
    let parse_value = parser()
        .parse("(if (zero (const 0)) (const 2) (const 3))")
        .unwrap();
    assert_eq!(
        parse_value,
        Expr::If(
            Box::new(Expr::Zero(Box::new(Expr::Const(0)))),
            Box::new(Expr::Const(2)),
            Box::new(Expr::Const(3))
        )
    );
}
/*
(define value-of
  (lambda (expr env)
    (match expr
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
      [`(sub1 ,x) (sub1 (value-of x env))]
      [`(zero ,x) (zero? (value-of x env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env))]
      [`(let ,e ,body) (let ((a (value-of e env)))
                         (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(var ,y) (env y)]
      [`(lambda ,body) (lambda (a) (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(app ,rator ,rand) ((value-of rator env) (value-of rand env))])))
 */
fn valueof(e: Expr, env: &Vec<u32>) -> u32 {
    match e {
        Expr::Const(x) => x,
        Expr::Var(x) => *env.get(x).unwrap(),
        Expr::Mult(x, y) => valueof(*x, env) * valueof(*y, env),
        Expr::Sub1(x) => valueof(*x, env) - 1,
        Expr::Zero(x) => {
            if valueof(*x, env) == 0 {
                1
            } else {
                0
            }
        }
        Expr::If(x, y, z) => {
            if valueof(*x, env) == 1 {
                valueof(*y, env)
            } else {
                valueof(*z, env)
            }
        }
        Expr::Let(e, body) => {
            let mut newenv = env.clone();
            newenv.push(valueof(*e, env));
            valueof(*body, &newenv)
        }
        Expr::Lambda(x) => valueof(*x, env),
        Expr::App(rator, rand) => {
            let mut newenv = env.clone();
            newenv.push(valueof(*rand, env));
            valueof(*rator, &newenv)
        }
    }
}

fn main() {
    let result = parser().parse("(mult (const 3) (const 2))").unwrap();
    println!("{}", valueof(result, &Vec::new()));
}
