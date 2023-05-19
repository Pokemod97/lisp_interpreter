use lisp_interpreter::*;
use chumsky::Parser;
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
