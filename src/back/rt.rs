use super::wrap::{Context, Module};
use super::super::w_ds::{Type, Scheme, EnumDecl};
use super::super::desugar::Arg;
use llvm_sys::prelude::*;
use llvm_sys::core::*;
use llvm_sys::target::*;
use llvm_sys::LLVMLinkage;
use std::ffi::CString;
use std::collections::HashMap;

pub fn build_rt<'a>(
    ctx: &'a Context,
    m: &'a Module<'a>,
    target: LLVMTargetDataRef,
    types: &'a HashMap<String, Scheme<EnumDecl>>,
    structs: &'a mut HashMap<(String, Vec<Type>), LLVMTypeRef>,
) -> Vec<(Arg, Type, LLVMValueRef)> {
    // let t = Type::Func(Box::new(Type::Tuple(vec![Type::Int, Type::Int])), Box::new(Type::Int));
    let two_ints = vec![Type::Int, Type::Int];
    let t = Type::Func(
        Box::new(Type::Alias(String::from("Tuple"), two_ints.clone())),
        Box::new(Type::Int),
    );

    let ll_type = ctx.ll_type(&t, types, structs, target);
    let f = m.new_function("llvm_add_int64", unsafe { LLVMGetElementType(ll_type) })
        .unwrap();
    f.set_linkage(LLVMLinkage::LLVMInternalLinkage);

    let b = ctx.builder();
    b.set_block(f.append_bb("entry").unwrap());
    let arg = unsafe {
        LLVMBuildExtractValue(
            *b,
            LLVMGetFirstParam(*f),
            0,
            CString::new("unwrap").unwrap().as_ptr(),
        )
    };
    unsafe {
        LLVMBuildRet(
            *b,
            LLVMBuildAdd(
                *b,
                LLVMBuildExtractValue(*b, arg, 0, CString::new("a").unwrap().as_ptr()),
                LLVMBuildExtractValue(*b, arg, 1, CString::new("b").unwrap().as_ptr()),
                CString::new("add").unwrap().as_ptr(),
            ),
        )
    };

    vec![(Arg::Ident(String::from("llvm_add_int64")), t, *f)]
}
