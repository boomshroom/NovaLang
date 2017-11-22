// #![feature(box_patterns,const_fn,drop_types_in_const)]
use std::io::Read;
use std::env;
use std::fs::File;
use std::collections::HashMap;

#[macro_use]
extern crate nom;
extern crate llvm_sys;
mod parser;
// mod w;
mod desugar;
mod w_ds;
mod monomorph;

mod back;
// mod simple;
// use simple::Expr;
// mod lambda;
// use lambda::Expr;
use parser::Module;
use desugar::ModDS;
use w_ds::TypedMod;
// use types::TypeEnv;
// mod eval;
// use eval::Value;
// use w::run_infer;

// use parser::parse;

// const SRC: &'static str = "True + False";

fn main() {
    parser::test_lambda();
    parser::test_exprs();

    let filename = env::args().nth(1).unwrap_or(String::from("Main.nva"));

    let mut file = Vec::new();
    File::open(filename.as_str())
        .expect(format!("File not found: {}", filename).as_str())
        .read_to_end(&mut file)
        .expect("Couldn't read file.");

    let m = Module::new(file.as_slice()).expect("Parse error.");

    //println!("{:?}", m);
    let typed = TypedMod::new(ModDS::new(m)).expect("Type error.");
    //println!("{:?}", typed.defns.into_iter().collect::<HashMap<_,_>>());
    println!("{}", back::compile(monomorph::Module::new(typed)).unwrap());
    // let stdin = stdin();
    // stdin
    // .lock()
    // .lines()
    // .map(|l| {
    // l.map_err(|_| "Read Error.")
    // .and_then(|l| Node::new(l.as_str())
    // .map_err(|e| {println!("{:?}", e); "Parse Error"}))
    // .and_then(Expr::new)
    // .and_then(|e| Value::eval(e))
    // .map_err(String::from)
    // .and_then(|e| {w::run_infer(&e).map_err(|e| format!("{:?}", e))} )
    // .map(desugar::desugar)
    // .map_err(String::from)
    // .map(|n| {println!("{:?}", n);n})
    // .and_then(|e| {w_ds::run_infer(&e).map_err(|e| format!("{:?}", e))} )
    // .map(monomorph::monomorph)
    // .and_then(|n| back::compile(n).map_err(|e| format!("{}", e)))
    // })
    // .map(|e| e.map(|m| println!("{}", m)))
    // .last();
    //
}
