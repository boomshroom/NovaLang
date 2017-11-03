use super::wrap::{self, Context, Module, Builder, Function};
use super::super::desugar::{Pat, Arg};
use super::super::w_ds::{TypedNode, Type};
use llvm_sys::prelude::*;
use llvm_sys::core::*;
use llvm_sys::LLVMIntPredicate;
use std::collections::HashMap;
use std::ffi::{NulError, CString};
use std::iter;

struct Compiler<'a> {
    ctx: &'a Context,
    lmod: &'a Module<'a>,
    build: Builder<'a>,
    vars: HashMap<Arg, LLVMValueRef>,
    func: Function<'a>,
    counter: &'a mut Counter,
}

struct Counter(usize);
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
    let f = modl.new_function("main",
                      unsafe { LLVMFunctionType(ctx.int_type(32), null(), 0, 0) })?;

    let mut comp = Compiler {
        ctx: &ctx,
        lmod: &modl,
        build: ctx.builder(),
        vars: HashMap::new(),
        func: f,
        counter: &mut c,
    };

    comp.build.set_block(comp.func.append_bb("entry")?);

    let r = comp.compile(n);

    unsafe {
        LLVMBuildRet(*comp.build,
                     LLVMBuildTruncOrBitCast(*comp.build,
                                             r,
                                             ctx.int_type(32),
                                             comp.counter.next().as_ptr()))
    };

    Ok(comp.lmod.to_string())
}

fn null<T>() -> *mut T {
    0 as *mut T
}

struct Constr {
	id: usize,
	args: usize,
	variants: usize, 
}

fn constructors(cons: &str) -> Constr {
	match cons {
		"True" => Constr{id: 1, args: 0, variants: 2},
		"False" => Constr{id: 0, args: 0, variants: 2},
		"()" => Constr{id: 0, args: 0, variants: 1},
		cons => if cons.chars().all(|ch| ch == ',') {
			Constr{id: 0, args: cons.len() + 1, variants: 1}
		} else {
			panic!("Undefined constructor: {}", cons)
		},
	}
}

impl<'a> Compiler<'a> {
    fn compile(&mut self, n: TypedNode) -> LLVMValueRef {
        match n {
            TypedNode::Lit(i) => self.compile_lit(i),
            TypedNode::App(f, a) => self.compile_app(*f, *a),
            TypedNode::Var(i, t) => self.compile_var(i, t),
            TypedNode::Abs(i, b, arg, cap) => self.compile_abs(i, *b, arg, cap),
            TypedNode::Match(e, a, t) => self.compile_match(*e, a, t),
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
            }
            Type::Closure(_, _, _) => {
                let f_ptr = unsafe {
                    LLVMBuildExtractValue(*self.build, f, 0, self.counter.next().as_ptr())
                };
                let f_cap = unsafe {
                    LLVMBuildExtractValue(*self.build, f, 1, self.counter.next().as_ptr())
                };
                let mut args = [f_cap, a];
                unsafe {
                    LLVMBuildCall(*self.build,
                                  f_ptr,
                                  args.as_mut_ptr(),
                                  args.len() as u32,
                                  self.counter.next().as_ptr())
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
                // eprintln!("Genning function. ({:?})", ty);
                let ll_type = self.ll_type(&ty);

                /*let f_obj = unsafe {
                    LLVMAddFunction(**self.lmod,
                                    self.counter.next().as_ptr(),
                                    LLVMGetElementType(ll_type))
                };*/
                let f_obj = self.lmod.new_function(self.counter.next(),
                	unsafe { LLVMGetElementType(ll_type) }).unwrap();
                assert!(!f_obj.is_null(), "Function is null");
                eprintln!("{:?}", unsafe { LLVMGetTypeKind(LLVMTypeOf(*f_obj)) });
                // assert_eq!(, );
                // let f_obj = self.lmod.new_function("", ll_type).unwrap();
                let f_obj = {
	                let mut new_ctx = Compiler {
	                    ctx: self.ctx,
	                    lmod: self.lmod,
	                    build: self.ctx.builder(),
	                    vars: HashMap::new(),
	                    func: f_obj,
	                    counter: self.counter,
	                };
	                /*let bb = unsafe {
	                    LLVMAppendBasicBlockInContext(**new_ctx.ctx,
	                                                  *f_obj,
	                                                  CString::new("_entry").unwrap().as_ptr())
	                };
	                unsafe { LLVMPositionBuilderAtEnd(*new_ctx.build, bb) };*/
	                new_ctx.build.set_block(new_ctx.func.append_bb("_entry").unwrap());

	                i.map(|i| {
	                    new_ctx.vars.insert(i, {
	                        assert_eq!( unsafe { LLVMCountParams(*new_ctx.func) }, 1,
	                    	"Function doesn't have 1 argument.");
	                        let v = unsafe { LLVMGetFirstParam(*new_ctx.func) };
	                        assert!(!v.is_null(), "Parameter is null.");
	                        v
	                    })
	                });

	                unsafe { LLVMBuildRet(*new_ctx.build, new_ctx.compile(b)) };
	                new_ctx.func
	            };

                // unsafe { LLVMDisposeBuilder(new_ctx.build) };
                *f_obj
            }
            _ => {
                let ty = Type::Closure(Box::new(arg),
                                       Box::new(b.get_type()),
                                       cap.iter().map(|&(_, ref t)| t.clone()).collect());
                eprintln!("Genning closure. ({:?})", ty);
                let ll_type = self.ll_type(&ty);

                let capt: Vec<_> =
                    cap.clone().into_iter().map(|(v, t)| self.compile_var(v, t)).collect();

                let f_ptr_ty = unsafe { LLVMGetElementType(LLVMStructGetTypeAtIndex(ll_type, 0)) };
                eprintln!("Adding function.");
                /*let f_obj =
                    unsafe { LLVMAddFunction(**self.lmod, self.counter.next().as_ptr(), f_ptr_ty) };
                */
                let f_obj = self.lmod.new_function(self.counter.next(), f_ptr_ty).unwrap();
                eprintln!("Added function.");

                let f_obj = {
                    let mut new_ctx = Compiler {
                        ctx: self.ctx,
                        lmod: self.lmod,
                        build: self.ctx.builder(),
                        vars: HashMap::new(),
                        func: f_obj,
                        counter: self.counter,
                    };

                    /*let bb = unsafe {
                        LLVMAppendBasicBlockInContext(**new_ctx.ctx,
                                                      new_ctx.func,
                                                      CString::new("_entry_c").unwrap().as_ptr())
                    };
                    unsafe { LLVMPositionBuilderAtEnd(*new_ctx.build, bb) };*/
                    new_ctx.build.set_block(new_ctx.func.append_bb("_entry_c").unwrap());

                    // frees.iter().enumerate().map(|(i, v)| )

                    let capts = unsafe { LLVMGetParam(*new_ctx.func, 0) };
                    let vars: Vec<_> = cap.into_iter()
                        .enumerate()
                        .map(|(i, (v, _))| {
                            (v,
                             unsafe {
                                LLVMBuildExtractValue(*new_ctx.build,
                                                      capts,
                                                      i as u32,
                                                      new_ctx.counter.next().as_ptr())
                            })
                        })
                        .collect();
                    new_ctx.vars.extend(vars);
                    i.map(|i| new_ctx.vars.insert(i, unsafe { LLVMGetParam(*new_ctx.func, 1) }));

                    eprintln!("Recursing.");
                    unsafe { LLVMBuildRet(*new_ctx.build, new_ctx.compile(b)) };
                    eprintln!("Recursed.");
                    *new_ctx.func
                };

                let capt_type = unsafe { LLVMStructGetTypeAtIndex(ll_type, 1) };
                eprintln!("Got element");

                let mut cls_vec = [f_obj, unsafe { LLVMGetUndef(capt_type) }];
                eprintln!("finalizing");
                let cls_obj = unsafe {
                    LLVMConstStructInContext(**self.ctx,
                                             cls_vec.as_mut_ptr(),
                                             cls_vec.len() as u32,
                                             0)
                };

                let capt_obj = capt.into_iter().enumerate().fold(
                	unsafe { LLVMGetUndef(capt_type) }, |obj, (i, v)| unsafe {
                    eprintln!("capture {}", i);
                //LLVMBuildInsertValue(*self.build, obj, v, i as u32, self.counter.next().as_ptr())
                    self.build.build_insert_value(obj, v, i as u32, self.counter.next()).unwrap()
                });

                eprintln!("Closue Genned.");
                unsafe {
                    LLVMBuildInsertValue(*self.build,
                                         cls_obj,
                                         capt_obj,
                                         1,
                                         self.counter.next().as_ptr())
                }
                // cls_obj
            }
        }
    }

    fn compile_var(&mut self, i: Arg, _: Type) -> LLVMValueRef {
        // eprintln!("{:?}", i);
        let v = self.vars.get(&i).expect("Value undefined.");
        assert!(!v.is_null(), "Value stored as null.");
        *v
    }

    fn compile_match(&mut self, arg: TypedNode, arms: Vec<(Pat, TypedNode)>, res: Type) -> LLVMValueRef {
    	let res_type = self.ll_type(&res);
	    let arg_ll = self.compile(arg);

	    let end_block = self.func.append_bb(self.counter.next()).unwrap();
	    let catch = self.func.append_bb(self.counter.next()).unwrap();

	    let (mut vals, mut blks) : (Vec<LLVMValueRef>, Vec<LLVMBasicBlockRef>) = arms.into_iter().map(|(p, b)| {
	    	let end_arm = self.func.append_bb(self.counter.next()).unwrap();
	    	let else_blk = self.func.append_bb(self.counter.next()).unwrap();
	    	let args = self.compile_pattern(p, arg_ll, *else_blk);
	    	let old_vars = self.vars.clone();

	    	self.vars.extend(args);
	    	let val = self.compile(b);
	    	unsafe { LLVMBuildBr(*self.build, *end_arm) };
	    	let end = *end_arm;
	    	self.build.set_block(end_arm);
	    	unsafe { LLVMBuildBr(*self.build, *end_block) };
	    	self.build.set_block(else_blk);
	    	self.vars = old_vars;

	    	(val, end)
	    }).unzip();

	    unsafe { LLVMBuildUnreachable(*self.build); }

	    self.build.set_block(end_block);
	    let phi = unsafe { LLVMBuildPhi(*self.build, res_type, self.counter.next().as_ptr()) };
	    unsafe { LLVMAddIncoming(phi, vals.as_mut_ptr(), blks.as_mut_ptr(), vals.len() as u32) };
	    phi
    }

    fn compile_pattern(&mut self, p: Pat, arg: LLVMValueRef, else_blk: LLVMBasicBlockRef) -> HashMap<Arg, LLVMValueRef> {
    	match p {
    		Pat::Prim(a) => a.into_iter().map(|a| (a, arg)).collect(),
    		Pat::Lit(i) => {
    			let pred = unsafe { LLVMBuildICmp(*self.build, LLVMIntPredicate::LLVMIntEQ, arg, wrap::sint(i, self.ctx.int_type(64)), self.counter.next().as_ptr()) };
    			let new_blk = self.func.append_bb(self.counter.next()).unwrap();
    			unsafe { LLVMBuildCondBr(*self.build, pred, *new_blk, else_blk) };
    			self.build.set_block(new_blk);
    			HashMap::new()
    		},
    		Pat::Cons(c, args) => {
    			let cons = constructors(c.as_str());
    			assert_eq!(args.len(), cons.args, "Incorrect number of arguments to constructor.");
    			let arg = match cons.variants {
    				1 => arg,
    				_ => {
	    				let tag = unsafe { LLVMBuildExtractValue(*self.build, arg, 1, self.counter.next().as_ptr()) };
	    				let pred = unsafe { LLVMBuildICmp(*self.build, LLVMIntPredicate::LLVMIntEQ, tag, wrap::uint(cons.id as u64, LLVMTypeOf(tag)), self.counter.next().as_ptr()) };
	    				let new_blk = self.func.append_bb(self.counter.next()).unwrap();
		    			unsafe { LLVMBuildCondBr(*self.build, pred, *new_blk, else_blk) };
		    			self.build.set_block(new_blk);
	    				unsafe { LLVMBuildExtractValue(*self.build, arg, 0, self.counter.next().as_ptr()) }
    				}
    			};
    			args.into_iter().enumerate().filter_map(|(i, a)| a.map(|a| (a, unsafe {LLVMBuildExtractValue(*self.build, arg, i as u32, self.counter.next().as_ptr())}))).collect()
    		}
    	}
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
            Type::Free(_) => {
                eprintln!("Free type made to compilation!");
                self.ctx.int_type(64) // Assume Int
            }
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
