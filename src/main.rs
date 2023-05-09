use std::result;

use chumsky::prelude::*;
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
            newenv.insert(0, valueof(*e, env));
            valueof(*body, &newenv)
        }
        Expr::Lambda(x) => valueof(*x, env),
        Expr::App(rator, rand) => {
            let mut newenv = env.clone();
            newenv.insert(0, valueof(*rand, env));
            valueof(*rator, &newenv)
        }
    }
}

fn main() {
    //let result = Expr::If(Box::new(Expr::Zero(Box::new(Expr::Const(0))))
    //  , Box::new(Expr::Mult(Box::new(Expr::Const(2)), Box::new(Expr::Const(2)))), Box::new(Expr::Const(5)));
    let result = Expr::App(Expr::App(Expr::Lambda(Expr::Var(1)), Expr::Const(6)), Expr::Const(5));
    println!("{}", valueof(result, &Vec::new()));
}
