
    use chumsky::prelude::*;

    #[derive(Debug)]
    pub enum ValError {
        BoolErr(Expr),
        DivZeroErr(Expr),
        NumErr(Expr),
        EvnFail(usize),
    }
    #[derive(Debug, PartialEq, Clone)]
    pub enum Expr {
        Const(u32),
        Var(usize),
        Lambda(Box<Expr>),
        Sub1(Box<Expr>),
        Add1(Box<Expr>),
        Zero(Box<Expr>),
        Let(Box<Expr>, Box<Expr>),
        Mult(Box<Expr>, Box<Expr>),
        Sub(Box<Expr>, Box<Expr>),
        Add(Box<Expr>, Box<Expr>),
        Div(Box<Expr>, Box<Expr>),
        App(Box<Expr>, Box<Expr>),
        If(Box<Expr>, Box<Expr>, Box<Expr>),
    }
    impl Expr {
        pub fn eval(&self, env: &Vec<LispReturn>) -> Result<LispReturn, ValError> {
            match self {
                Expr::Const(x) => Ok(LispReturn::Num(*x)),
                Expr::Var(x) => env.get(*x).map_or_else(|| Err(ValError::EvnFail(*x)), |x| Ok(*x)),
                Expr::Mult(x, y) => Ok(LispReturn::Num( x.eval(env)?.as_num(x)? * y.eval(env)?.as_num(y)?)),
                Expr::Add(x, y) => Ok(LispReturn::Num( x.eval(env)?.as_num(x)? + y.eval(env)?.as_num(y)?)),
                Expr::Sub(x, y) => Ok(LispReturn::Num( x.eval(env)?.as_num(x)? - y.eval(env)?.as_num(y)?)),
                Expr::Div(x, y) => Ok(LispReturn::Num( x.eval(env)?.as_num(x)? / y.eval(env)?.as_num(y)?)),
                Expr::Sub1(x) => x.eval(env)?.as_num(x).map(|val| LispReturn::Num(val -1)),
                Expr::Add1(x) => x.eval(env)?.as_num(x).map(|val| LispReturn::Num(val +1)),
                Expr::Zero(x) =>  x.eval(env)?.as_num(x).map(|val| LispReturn::Bool(val == 0)) ,
                Expr::If(cond, lhs, rhs) => cond.eval(env)?.as_bool(cond).and_then(|cond| {
                    if cond {
                        lhs.eval(env)
                    } else {
                        rhs.eval(env)
                    }
                }),
                Expr::Let(e, body) => {
                    let mut newenv = env.clone();
                    newenv.push(e.eval(env)?);
                    body.eval(&newenv)
                }
                Expr::Lambda(x) => x.eval(env),
                Expr::App(rator, rand) => {
                    let mut newenv = env.clone();
                    newenv.push(rand.eval(env)?);
                    rator.eval(&newenv)
                }
            }
        }
    }
    #[derive(Debug, Clone, Copy)]
    pub enum LispReturn {
        Num(u32),
        Bool(bool),
    }
    impl LispReturn {
        fn as_bool(&self, expr: &Expr) -> Result<bool, ValError> {
            if let LispReturn::Bool(val) = self {
                Ok(*val)
            }else{
            Err(ValError::BoolErr(expr.clone()))
            }
        }
        fn as_num(&self, expr: &Expr) -> Result<u32, ValError> {
            if let LispReturn::Num(val) = self {
                Ok(*val)
            }else{
            Err(ValError::NumErr(expr.clone()))
            }
        }
    }
    pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
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
                text::keyword("add1").to(Expr::Add1 as fn(_) -> _),
                text::keyword("zero").to(Expr::Zero as fn(_) -> _),
                text::keyword("lambda").to(Expr::Lambda as fn(_) -> _),
            ))
            .then(expr.clone())
            .map(|(x, y)| x(Box::new(y)));
            let double_box = choice((
                text::keyword("mult").to(Expr::Mult as fn(_, _) -> _),
                text::keyword("add").to(Expr::Add as fn(_, _) -> _),
                text::keyword("sub").to(Expr::Sub as fn(_, _) -> _),
                text::keyword("div").to(Expr::Div as fn(_, _) -> _),
                text::keyword("let").to(Expr::Let as fn(_, _) -> _),
                text::keyword("app").to(Expr::App as fn(_, _) -> _),
            ))
            .then(expr.clone())
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
