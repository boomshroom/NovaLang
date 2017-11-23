use super::wrap::{Context, Module, Function, Builder};
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
    let b = ctx.builder();

    let add = m.new_function("llvm_add_int64", unsafe { LLVMGetElementType(ll_type) })
        .unwrap();
    let div = m.new_function("llvm_div_int64", unsafe { LLVMGetElementType(ll_type) })
        .unwrap();

    add.set_linkage(LLVMLinkage::LLVMInternalLinkage);
    div.set_linkage(LLVMLinkage::LLVMInternalLinkage);

    int_binop(LLVMBuildAdd, &add, &b);
    int_binop(LLVMBuildSDiv, &div, &b);

    vec![
        (Arg::Ident(String::from("llvm_add_int64")), t.clone(), *add),
        (Arg::Ident(String::from("llvm_div_int64")), t, *div),
    ]
}

type LLVMBinOp = unsafe extern "C" fn(LLVMBuilderRef, LLVMValueRef, LLVMValueRef, *const i8)
                                      -> LLVMValueRef;

fn int_binop(instr: LLVMBinOp, f: &Function, b: &Builder) {
    b.set_block(f.append_bb("entry").unwrap());
    let arg = unsafe {
        LLVMBuildExtractValue(
            **b,
            LLVMGetFirstParam(**f),
            0,
            CString::new("unwrap").unwrap().as_ptr(),
        )
    };
    unsafe {
        LLVMBuildRet(
            **b,
            instr(
                **b,
                LLVMBuildExtractValue(**b, arg, 0, CString::new("a").unwrap().as_ptr()),
                LLVMBuildExtractValue(**b, arg, 1, CString::new("b").unwrap().as_ptr()),
                CString::new("result").unwrap().as_ptr(),
            ),
        )
    };
}
