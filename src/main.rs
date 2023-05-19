use chumsky::Parser;
use lisp_interpreter::*;
fn main() {
    let result = parser().parse("(mult (const 3) (const 2))").unwrap();
    println!("{:#?}", result.eval(&Vec::new()).unwrap());
}
