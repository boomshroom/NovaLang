// #![feature(box_patterns,const_fn,drop_types_in_const)]
use std::io::{stdin, BufRead};

#[macro_use]
extern crate nom;

mod parser;
mod w;
mod desugar;
// mod simple;
// use simple::Expr;
// mod lambda;
// use lambda::Expr;
use parser::Node;
// use types::TypeEnv;
// mod eval;
// use eval::Value;
use w::run_infer;

// use parser::parse;

// const SRC: &'static str = "True + False";

fn main() {
    parser::test_lambda();
    parser::test_exprs();
    let stdin = stdin();
    stdin.lock()
        .lines()
        .map(|l| {
            l.map_err(|_| "Read Error.")
                .and_then(|l| Node::new(l.as_str())
                    .map_err(|e| {println!("{:?}", e); "Parse Error"}))
                //.and_then(Expr::new)
                //.and_then(|e| Value::eval(e))
                //.map_err(String::from)
                //.and_then(|e| {run_infer(&e).map_err(|e| format!("{:?}", e))} )
                .map(desugar::desugar)
        })
        .map(|e| println!("{:?}", e))
        .last();

}
