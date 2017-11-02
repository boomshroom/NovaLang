use super::wrap::{self, Context, Module, Builder};
use super::super::desugar::{Pat, Arg};
use super::super::w_ds::{TypedNode, Type};
use llvm_sys::prelude::*;
use llvm_sys::core::*;
use std::collections::HashMap;
use std::ffi::{NulError, CString};

struct Compiler<'a> {
    ctx: &'a Context,
    lmod: &'a Module<'a>,
    build: Builder<'a>,
    vars: HashMap<Arg, LLVMValueRef>,
    counter: &'a mut Counter,
}

struct Counter (usize);
impl Counter {
    fn new() -> Counter {
        Counter(0)
    }
    fn next(&mut self) -> CString {
        let i = self.0;
        self.0 += 1;
        CString::new(format!("_{}", i)).unwrap()
    }
}

pub fn compile(n: TypedNode) -> Result<String, NulError> {
    let ctx = Context::new();
    let modl = ctx.module("test")?;
    let mut c = Counter::new();
    let mut comp = Compiler {
        ctx: &ctx,
        lmod: &modl,
        build: ctx.builder(),
        vars: HashMap::new(),
        counter: &mut c,
    };

    let f = modl.new_function("main", comp.ll_type(&Type::Func(Box::new(Type::Int), Box::new(Type::Int))))?;
    comp.build.set_block(f.append_bb("entry")?);

    let r = comp.compile(n);

    unsafe {LLVMBuildRet(*comp.build, r)};

    Ok(comp.lmod.to_string())
}

fn null<T>() -> *mut T {
    0 as *mut T
}



impl<'a> Compiler<'a> {
    fn compile(&mut self, n: TypedNode) -> LLVMValueRef {
        match n {
            TypedNode::Lit(i) => self.compile_lit(i),
            TypedNode::App(f, a) => self.compile_app(*f, *a),
            TypedNode::Var(i, t) => self.compile_var(i, t),
            TypedNode::Abs(i, b, arg, cap) => self.compile_abs(i, *b, arg, cap),
            n => panic!("Unimplemented: {:?}", n),
        }
    }

    fn compile_lit(&mut self, l: i64) -> LLVMValueRef {
        wrap::sint(l, self.ll_type(&Type::Int))
    }

    fn compile_app(&mut self, f: TypedNode, a: TypedNode) -> LLVMValueRef {
        let f_ty = f.get_type();
        eprintln!("{:?}", f_ty);
        let f = self.compile(f);
        let mut a = self.compile(a);
        match f_ty {
            Type::Func(_, _) => {
                let v = unsafe {
                    LLVMBuildCall(*self.build,
                                  f,
                                  &mut a as *mut LLVMValueRef,
                                  1,
                                  CString::new("_2").unwrap().as_ptr())
                };
                v
            },
            Type::Closure(_, _, _) => {
                let f_ptr = unsafe {
                    LLVMBuildExtractElement(*self.build,
                                            f,
                                            wrap::uint(0, self.ctx.int_type(8)),
                                            null())
                };
                let f_cap = unsafe {
                    LLVMBuildExtractElement(*self.build,
                                            f,
                                            wrap::uint(1, self.ctx.int_type(8)),
                                            null())
                };
                let mut args = [f_cap, a];
                unsafe {
                    LLVMBuildCall(*self.build,
                                  f_ptr,
                                  args.as_mut_ptr(),
                                  args.len() as u32,
                                  0 as *const _)
                }
            }
            _ => unreachable!(),
        }
    }

    fn compile_abs(&mut self,
                   i: Option<Arg>,
                   b: TypedNode,
                   arg: Type,
                   cap: Vec<(Arg, Type)>)
                   -> LLVMValueRef {
        // let ll_type = self.ll_type(&b.get_type());
        match cap.len() {
            0 => {
                let ty = Type::Func(Box::new(arg), Box::new(b.get_type()));
                //eprintln!("Genning function. ({:?})", ty);
                let ll_type = self.ll_type(&ty);
                let f_obj = unsafe { LLVMAddFunction(**self.lmod, self.counter.next().as_ptr(), ll_type) };
                //let f_obj = self.lmod("")
                assert!(!f_obj.is_null(), "Function is null");
                //assert_eq!(, );
                // let f_obj = self.lmod.new_function("", ll_type).unwrap();
                
                let mut new_ctx = Compiler {
                    ctx: self.ctx,
                    lmod: self.lmod,
                    build: self.ctx.builder(),
                    vars: HashMap::new(),
                    counter: self.counter,
                };
                let bb = unsafe { LLVMAppendBasicBlockInContext(**new_ctx.ctx, f_obj, CString::new("_entry").unwrap().as_ptr()) };
                unsafe { LLVMPositionBuilderAtEnd(*new_ctx.build, bb) };
                // new_ctx.build.set_block(f_obj.append_bb("").unwrap());

                i.map(|i| new_ctx.vars.insert(i, {
                    assert_eq!( unsafe { LLVMCountParams(f_obj) }, 1, "Function doesn't have 1 argument.");
                    let v = unsafe { LLVMGetFirstParam(f_obj) };
                    assert!(!v.is_null(), "Parameter is null.");
                    v
                }));

                unsafe { LLVMBuildRet(*new_ctx.build, new_ctx.compile(b)) };
                
                // unsafe { LLVMDisposeBuilder(new_ctx.build) };
                f_obj
            }
            _ => {
                let ty = Type::Closure(Box::new(arg),
                                                Box::new(b.get_type()),
                                                cap.iter().map(|&(_, ref t)| t.clone()).collect());
                eprintln!("Genning closure. ({:?})", ty);
                let ll_type =
                    self.ll_type(&ty);

                let capt: Vec<_> =
                    cap.clone().into_iter().map(|(v, t)| self.compile_var(v, t)).collect();

                let f_ptr_ty = unsafe { LLVMGetElementType(LLVMStructGetTypeAtIndex(ll_type, 0)) };
                eprintln!("Adding function.");
                let f_obj = unsafe { LLVMAddFunction(**self.lmod, self.counter.next().as_ptr(), f_ptr_ty) };
                eprintln!("Added function.");
                // let f_obj = self.lmod.new_function("", f_ptr_ty).unwrap();

{
                let mut new_ctx = Compiler {
                    ctx: self.ctx,
                    lmod: self.lmod,
                    build: self.ctx.builder(),
                    vars: HashMap::new(),
                    counter: self.counter,
                };

                let bb = unsafe { LLVMAppendBasicBlockInContext(**new_ctx.ctx, f_obj, CString::new("_entry_c").unwrap().as_ptr()) };
                unsafe { LLVMPositionBuilderAtEnd(*new_ctx.build, bb) };
                // new_ctx.build.set_block(f_obj.append_bb("").unwrap());

                // frees.iter().enumerate().map(|(i, v)| )

                let capts = unsafe { LLVMGetParam(f_obj, 0) };
                let vars: Vec<_> = cap.into_iter()
                    .enumerate()
                    .map(|(i, (v, _))| {
                        (v,
                         unsafe { LLVMBuildExtractValue(*new_ctx.build, capts, i as u32, new_ctx.counter.next().as_ptr()) })
                    })
                    .collect();
                new_ctx.vars.extend(vars);
                i.map(|i| new_ctx.vars.insert(i, unsafe { LLVMGetParam(f_obj, 1) }));

                eprintln!("Recursing.");
                unsafe { LLVMBuildRet(*new_ctx.build, new_ctx.compile(b)) };
                eprintln!("Recursed.");
            }

                let capt_type = unsafe { LLVMStructGetTypeAtIndex(ll_type, 1) };
                eprintln!("Got element");

                let mut cls_vec = [f_obj, unsafe { LLVMGetUndef(capt_type) } ];
                eprintln!("finalizing");
                let cls_obj = unsafe {
                    LLVMConstStructInContext(**self.ctx,
                                             cls_vec.as_mut_ptr(),
                                             cls_vec.len() as u32,
                                             0)
                };

                let capt_obj = capt.into_iter().enumerate().fold(unsafe { LLVMGetUndef(capt_type) }, |obj, (i, v)| unsafe {
                    eprintln!("capture {}", i);
                    //LLVMBuildInsertValue(*self.build, obj, v, i as u32, self.counter.next().as_ptr())
                    self.build.build_insert_value(obj, v, i as u32, self.counter.next()).unwrap()
                });

                eprintln!("Closue Genned.");
                unsafe { LLVMBuildInsertValue(*self.build, cls_obj, capt_obj, 1, self.counter.next().as_ptr()) }
                //cls_obj
            }
        }
    }

    fn compile_var(&mut self, i: Arg, _: Type) -> LLVMValueRef {
        //eprintln!("{:?}", i);
        let v = self.vars.get(&i).expect("Value undefined.");
        assert!(!v.is_null(), "Value stored as null.");
        *v
    }

    fn ll_type(&self, t: &Type) -> LLVMTypeRef {
        match *t {
            Type::Int => self.ctx.int_type(64),
            Type::Bool => self.ctx.int_type(1),
            Type::Unit => unsafe { LLVMStructType(null(), 0, 0) },
            Type::Tuple(ref ts) => {
                let mut ts: Vec<_> = ts.iter().map(|t| self.ll_type(t)).collect();
                unsafe { LLVMStructType(ts.as_mut_ptr(), ts.len() as u32, 0) }
            }
            Type::Func(ref a, ref r) => {
                unsafe {
                    LLVMPointerType(LLVMFunctionType(self.ll_type(r),
                                                     &mut self.ll_type(a) as *mut _,
                                                     1,
                                                     0),
                                    0)
                }
            }
            Type::Closure(ref a, ref r, ref c) => {
                let mut ts: Vec<_> = c.iter().map(|t| self.ll_type(t)).collect();
                let cap = unsafe { LLVMStructType(ts.as_mut_ptr(), ts.len() as u32, 0) };
                let mut args = [cap, self.ll_type(a)];
                let f_ty = unsafe {
                    LLVMPointerType(LLVMFunctionType(self.ll_type(r),
                                                     args.as_mut_ptr(),
                                                     args.len() as u32,
                                                     0),
                                    0)
                };
                let mut closure = [f_ty, cap];
                unsafe { LLVMStructType(closure.as_mut_ptr(), closure.len() as u32, 0) }
            }
            Type::Free(_) => self.ctx.int_type(64), // Assume Int
        }
    }

    fn zero_for_type(&self, t: &Type) -> LLVMValueRef {
        match *t {
            Type::Int => unsafe { LLVMConstInt(self.ctx.int_type(64), 0, 1) },
            Type::Bool => unsafe { LLVMConstInt(self.ctx.int_type(1), 0, 0) },
            Type::Unit => unsafe { LLVMConstStruct(null(), 0, 0) },
            Type::Tuple(ref ts) => {
                let mut vs: Vec<_> = ts.iter().map(|t| self.zero_for_type(t)).collect();
                unsafe { LLVMConstStruct(vs.as_mut_ptr(), vs.len() as u32, 0) }
            }
            Type::Func(ref a, ref r) => {
                unsafe {
                    LLVMConstNull(LLVMPointerType(LLVMFunctionType(self.ll_type(r),
                                                                   &mut self.ll_type(a) as *mut _,
                                                                   1,
                                                                   0),
                                                  0))
                }
            }
            Type::Closure(ref a, ref r, ref c) => {
                let mut vs: Vec<_> = c.iter().map(|t| self.zero_for_type(t)).collect();
                let cap = unsafe { LLVMConstStruct(vs.as_mut_ptr(), vs.len() as u32, 0) };
                let mut args = [unsafe { LLVMTypeOf(cap) }, self.ll_type(a)];
                let f_ptr = unsafe {
                    LLVMConstNull(LLVMPointerType(LLVMFunctionType(self.ll_type(r),
                                                                   args.as_mut_ptr(),
                                                                   args.len() as u32,
                                                                   0),
                                                  0))
                };
                let mut closure = [f_ptr, cap];
                unsafe { LLVMConstStruct(closure.as_mut_ptr(), closure.len() as u32, 0) }
            }
            Type::Free(_) => unreachable!(),
        }
    }
}
