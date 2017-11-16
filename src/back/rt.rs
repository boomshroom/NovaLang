use super::wrap::{Context, Module};
use super::super::w_ds::Type;
use super::super::desugar::Arg;
use llvm_sys::prelude::*;
use llvm_sys::core::*;
use std::ffi::CString;

pub fn build_rt<'a>(ctx: &'a Context, m: &'a Module<'a>) -> Vec<(Arg, Type, LLVMValueRef)> {
	//let t = Type::Func(Box::new(Type::Tuple(vec![Type::Int, Type::Int])), Box::new(Type::Int));
	let t = Type::Func(Box::new(Type::Alias(String::from("Tuple"), vec![Type::Int, Type::Int])), Box::new(Type::Int));
	let ll_type = ctx.ll_type(&t);
	let f = m.new_function("LLVM_add_int64", unsafe { LLVMGetElementType(ll_type) }).unwrap();
	let b = ctx.builder();
	b.set_block(f.append_bb("entry").unwrap());
	let arg = unsafe { LLVMGetFirstParam(*f) };
	unsafe { LLVMBuildRet(*b, LLVMBuildAdd(*b, LLVMBuildExtractValue(*b, arg, 0, CString::new("a").unwrap().as_ptr()), LLVMBuildExtractValue(*b, arg, 1, CString::new("b").unwrap().as_ptr()), CString::new("add").unwrap().as_ptr())) };

	vec![(Arg::Ident(String::from("LLVM_add_int64")), t, *f)]
}
