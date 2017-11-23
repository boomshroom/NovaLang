use std::io::Read;
use std::env;
use std::fs::File;
use std::collections::HashMap;

#[macro_use]
extern crate nom;
extern crate llvm_sys;
mod parser;
mod desugar;
mod types;
mod monomorph;
mod back;

use parser::Module;
use desugar::ModDS;
use types::TypedMod;

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
}
