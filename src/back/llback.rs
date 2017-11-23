use super::wrap::{self, Context, Module, Builder, Function};
use super::rt::build_rt;
use super::super::desugar::{Pat, Arg};
use super::super::types::{Type, EnumDecl, Scheme, Types, TypeInfo};
use super::super::monomorph::{self, Node};
use llvm_sys::prelude::*;
use llvm_sys::core::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::{LLVMIntPredicate, LLVMLinkage};
use std::collections::HashMap;
use std::ffi::{NulError, CString, CStr};

struct Compiler<'a> {
    ctx: &'a Context,
    lmod: &'a Module<'a>,
    build: Builder<'a>,
    vars: Vec<(Arg, Type, LLVMValueRef)>,
    globals: &'a [(Arg, Type, LLVMValueRef)],
    func: Function<'a>,
    counter: &'a mut Counter,
    types: &'a HashMap<String, Scheme<EnumDecl>>,
    structs: &'a mut HashMap<(String, Vec<Type>), LLVMTypeRef>,
    target: LLVMTargetDataRef,
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

fn unit_struct(ctx: &Context) -> LLVMValueRef {
    unsafe { LLVMConstStructInContext(**ctx, null(), 0, 0) }
}

pub fn compile(m: monomorph::Module) -> Result<String, NulError> {
    let monomorph::Module {
        name: m_name,
        exports,
        types,
        defns,
    } = m;

    let ctx = Context::new();
    let modl = ctx.module(m_name.as_str())?;

    let machine = unsafe {
        assert_eq!(LLVM_InitializeNativeTarget(), 0);
        let triple = LLVMGetDefaultTargetTriple();
        let mut target = null();
        let mut err = null();
        if LLVMGetTargetFromTriple(triple, &mut target, &mut err) == 1 {
            panic!(
                "Error initializing target: {:?}",
                CStr::from_ptr(err as *const _)
            );
        }
        LLVMCreateTargetMachine(
            target,
            triple,
            &0,
            &0,
            LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
            LLVMRelocMode::LLVMRelocPIC,
            LLVMCodeModel::LLVMCodeModelDefault,
        )
    };
    let data_layout = unsafe { LLVMCreateTargetDataLayout(machine) };

    let mut c = Counter::new();
    let mut structs = HashMap::new();

    let types = types.into_iter().collect();
    let mut vars = build_rt(&ctx, &modl, data_layout, &types, &mut structs);

    vars.extend(defns
        .iter()
        .map(|&(ref name, ref ty, ref n)| {
            // let ll_name = if name.as_str() == "main" && ty == Type::Func(
            // Box::new(Type::Tuple(vec![
            // Type::Int,
            // Type::Ptr(Box::new(Type::Ptr(Box::new(Type::Unit)))),
            // ])),
            // Box::new(Type::Int),
            // ) {
            // CString::new("main").unwrap()
            // } else {
            // c.next()
            // };
            /*let ftv = &ty.ftv() | &n.type_vars();
            if ftv.len() != 0 {
                eprintln!("Free vals found in {}.\n{:?}", name, ftv);
            }*/
            let ll_type = ctx.ll_type(ty, &types, &mut structs, data_layout);
            let g = unsafe {
                LLVMAddGlobal(
                    *modl,
                    ll_type,
                    CString::new(format!("{}.{}", m_name, name))?.as_ptr(),
                )
            };

            if let Some(ref exps) = exports {
                if !exps.contains(name) {
                    unsafe { LLVMSetLinkage(g, LLVMLinkage::LLVMInternalLinkage) };
                } else {
                    unsafe { LLVMSetLinkage(g, LLVMLinkage::LLVMExternalLinkage) };
                }
            };
            unsafe { LLVMSetInitializer(g, LLVMGetUndef(ll_type)) };
            Ok((Arg::Ident(name.clone()), ty.clone(), g))
        })
        .collect::<Result<Vec<_>, _>>()?);

    let main = modl.new_function("main", unsafe {
        LLVMFunctionType(
            ctx.int_type(64),
            [
                ctx.int_type(64),
                LLVMPointerType(LLVMPointerType(ctx.int_type(8), 0), 0),
            ].as_mut_ptr(),
            2,
            0,
        )
    })?;

    let mut comp = Compiler {
        ctx: &ctx,
        lmod: &modl,
        build: ctx.builder(),
        vars: Vec::new(),
        globals: vars.as_slice(),
        func: main,
        counter: &mut c,
        types: &types,
        structs: &mut structs,
        target: data_layout,
    };

    comp.build.set_block(comp.func.append_bb("entry")?);

    for (a, t, n) in defns.into_iter().rev() {
        let v = comp.compile(n);
        let p = comp.lookup(Arg::Ident(a), t);
        unsafe { LLVMBuildStore(*comp.build, v, p) };
    }

    let main_arg_ty = Type::Alias(
        String::from("Tuple"),
        vec![
            Type::Int,
            Type::Ptr(Box::new(Type::Ptr(Box::new(Type::Int8)))),
        ],
    );

    let args = unsafe { LLVMGetUndef(comp.ll_type(&main_arg_ty)) };

    let main_fn = comp.compile_var(
        Arg::Ident(String::from("main")),
        Type::Func(Box::new(main_arg_ty), Box::new(Type::Int)),
    );

    let argn = unsafe { LLVMGetParam(*comp.func, 0) };
    let argv = unsafe { LLVMGetParam(*comp.func, 1) };

    // TODO: Non-deterministic failure!
    let (inner_ty, (argn_idx, argv_idx)) = unsafe {
        use llvm_sys::LLVMTypeKind::*;

        assert_eq!(LLVMStructTypeKind, LLVMGetTypeKind(LLVMTypeOf(args)));
        assert_eq!(LLVMCountStructElementTypes(LLVMTypeOf(args)), 1);
        let mut inner_ty = null();
        LLVMGetStructElementTypes(LLVMTypeOf(args), &mut inner_ty);
        assert_eq!(LLVMStructTypeKind, LLVMGetTypeKind(inner_ty));
        assert_eq!(LLVMCountStructElementTypes(inner_ty), 2);
        let mut inner_v = [null(), null()];
        LLVMGetStructElementTypes(inner_ty, inner_v.as_mut_ptr());

        let idxs = match (LLVMGetTypeKind(inner_v[0]), LLVMGetTypeKind(inner_v[1])) {
            (LLVMIntegerTypeKind, LLVMPointerTypeKind) => (0, 1),
            (LLVMPointerTypeKind, LLVMIntegerTypeKind) => panic!("Arguments swapped!"),//(1,0),
            (k1, k2) => panic!("Expecting int and pointer. Got {:?} and {:?}", k1, k2),
        };

        (inner_ty, idxs)
    };

    let mut args = comp.build
        .build_insert_value(unsafe { LLVMGetUndef(inner_ty) }, argn, argn_idx, "argn")
        .and_then(|args| {
            comp.build.build_insert_value(args, argv, argv_idx, "argv")
        })
        .and_then(|inner| {
            comp.build.build_insert_value(args, inner, 0, "wrap")
        })?;

    unsafe {
        LLVMBuildRet(
            *comp.build,
            LLVMBuildCall(
                *comp.build,
                main_fn,
                &mut args as *mut LLVMValueRef,
                1,
                CString::new("init")?.as_ptr(),
            ),
        )
    };

    Ok(comp.lmod.to_string())
}

fn null<T>() -> *mut T {
    0 as *mut T
}

impl<'a> Compiler<'a> {
    fn compile(&mut self, n: Node) -> LLVMValueRef {
        match n {
            Node::Lit(i) => self.compile_lit(i),
            Node::App(f, a) => self.compile_app(*f, *a),
            Node::Var(i, t) => self.compile_var(i, t),
            Node::Abs(i, b, arg, cap) => self.compile_abs(i, *b, arg, cap),
            Node::Match(e, a, t) => self.compile_match(*e, a, t),
            Node::Let(i, e, b) => self.compile_let(Arg::Ident(i), *e, *b),
            Node::Constr(args, t, i) => self.compile_constr(args, t, i),
        }
    }

    fn compile_lit(&mut self, l: i64) -> LLVMValueRef {
        wrap::sint(l, self.ll_type(&Type::Int))
    }

    fn compile_app(&mut self, f: Node, a: Node) -> LLVMValueRef {
        let f_ty = f.get_type();
        let (f, a) = (self.compile(f), self.compile(a));
        self.compile_app_raw(f_ty, f, a)
    }

    fn compile_app_raw(
        &mut self,
        f_ty: Type,
        f: LLVMValueRef,
        mut a: LLVMValueRef,
    ) -> LLVMValueRef {
        match f_ty {
            Type::Func(_, _) => {
                let v = unsafe {
                    LLVMBuildCall(
                        *self.build,
                        f,
                        &mut a as *mut LLVMValueRef,
                        1,
                        CString::new("_2").unwrap().as_ptr(),
                    )
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
                    LLVMBuildCall(
                        *self.build,
                        f_ptr,
                        args.as_mut_ptr(),
                        args.len() as u32,
                        self.counter.next().as_ptr(),
                    )
                }
            }
            t => panic!("Non function/closure type atempted to be called: {:?}", t),
        }
    }

    fn compile_abs(
        &mut self,
        i: Option<Arg>,
        b: Node,
        arg: Type,
        cap: Vec<(Arg, Type)>,
    ) -> LLVMValueRef {
        // let ll_type = self.ll_type(&b.get_type());
        match cap.len() {
            0 => {
                let ty = Type::Func(Box::new(arg.clone()), Box::new(b.get_type()));
                let ll_type = self.ll_type(&ty);

                /*let f_obj = unsafe {
                    LLVMAddFunction(**self.lmod,
                                    self.counter.next().as_ptr(),
                                    LLVMGetElementType(ll_type))
                };*/
                let f_obj = self.lmod
                    .new_function(self.counter.next(), unsafe { LLVMGetElementType(ll_type) })
                    .unwrap();
                assert!(!f_obj.is_null(), "Function is null");
                f_obj.set_linkage(LLVMLinkage::LLVMInternalLinkage);

                let f_obj = {
                    let mut new_ctx = Compiler {
                        ctx: self.ctx,
                        lmod: self.lmod,
                        build: self.ctx.builder(),
                        vars: Vec::new(),
                        func: f_obj,
                        counter: self.counter,
                        types: self.types,
                        target: self.target,
                        structs: self.structs,
                        globals: self.globals,
                    };
                    /*let bb = unsafe {
	                    LLVMAppendBasicBlockInContext(**new_ctx.ctx,
	                                                  *f_obj,
	                                                  CString::new("_entry").unwrap().as_ptr())
	                };
	                unsafe { LLVMPositionBuilderAtEnd(*new_ctx.build, bb) };*/
                    new_ctx.build.set_block(
                        new_ctx.func.append_bb("_entry").unwrap(),
                    );

                    i.map(|i| {
                        new_ctx.vars.push((i, arg, {
                            assert_eq!(
                                unsafe { LLVMCountParams(*new_ctx.func) },
                                1,
                                "Function doesn't have 1 argument."
                            );
                            let v = unsafe { LLVMGetFirstParam(*new_ctx.func) };
                            assert!(!v.is_null(), "Parameter is null.");
                            v
                        }))
                    });

                    unsafe { LLVMBuildRet(*new_ctx.build, new_ctx.compile(b)) };
                    new_ctx.func
                };

                // unsafe { LLVMDisposeBuilder(new_ctx.build) };
                *f_obj
            }
            _ => {
                let ty = Type::Closure(
                    Box::new(arg.clone()),
                    Box::new(b.get_type()),
                    cap.iter().map(|&(_, ref t)| t.clone()).collect(),
                );

                let ll_type = self.ll_type(&ty);

                let capt: Vec<_> = cap.clone()
                    .into_iter()
                    .map(|(v, t)| self.compile_var(v, t))
                    .collect();

                let f_ptr_ty = unsafe { LLVMGetElementType(LLVMStructGetTypeAtIndex(ll_type, 0)) };

                /*let f_obj =
                    unsafe { LLVMAddFunction(**self.lmod, self.counter.next().as_ptr(), f_ptr_ty) };
                */
                let f_obj = self.lmod
                    .new_function(self.counter.next(), f_ptr_ty)
                    .unwrap();
                f_obj.set_linkage(LLVMLinkage::LLVMInternalLinkage);

                let f_obj = {
                    let mut new_ctx = Compiler {
                        ctx: self.ctx,
                        lmod: self.lmod,
                        build: self.ctx.builder(),
                        vars: Vec::new(),
                        func: f_obj,
                        counter: self.counter,
                        types: self.types,
                        target: self.target,
                        structs: self.structs,
                        globals: self.globals,
                    };

                    /*let bb = unsafe {
                        LLVMAppendBasicBlockInContext(**new_ctx.ctx,
                                                      new_ctx.func,
                                                      CString::new("_entry_c").unwrap().as_ptr())
                    };
                    unsafe { LLVMPositionBuilderAtEnd(*new_ctx.build, bb) };*/
                    new_ctx.build.set_block(
                        new_ctx.func.append_bb("_entry_c").unwrap(),
                    );

                    // frees.iter().enumerate().map(|(i, v)| )

                    let capts = unsafe { LLVMGetParam(*new_ctx.func, 0) };
                    new_ctx.vars = cap.into_iter()
                        .enumerate()
                        .map(|(i, (v, t))| {
                            (v, t, unsafe {
                                LLVMBuildExtractValue(
                                    *new_ctx.build,
                                    capts,
                                    i as u32,
                                    new_ctx.counter.next().as_ptr(),
                                )
                            })
                        })
                        .collect();
                    //new_ctx.vars.extend(vars);
                    i.map(|i| {
                        new_ctx.vars.push((
                            i,
                            arg,
                            unsafe { LLVMGetParam(*new_ctx.func, 1) },
                        ))
                    });

                    unsafe { LLVMBuildRet(*new_ctx.build, new_ctx.compile(b)) };
                    *new_ctx.func
                };

                let capt_type = unsafe { LLVMStructGetTypeAtIndex(ll_type, 1) };

                let mut cls_vec = [f_obj, unsafe { LLVMGetUndef(capt_type) }];
                let cls_obj = unsafe {
                    LLVMConstStructInContext(
                        **self.ctx,
                        cls_vec.as_mut_ptr(),
                        cls_vec.len() as u32,
                        0,
                    )
                };

                let capt_obj = capt.into_iter().enumerate().fold(
                	unsafe { LLVMGetUndef(capt_type) }, |obj, (i, v)| unsafe {
                //LLVMBuildInsertValue(*self.build, obj, v, i as u32, self.counter.next().as_ptr())
                    self.build.build_insert_value(obj, v, i as u32, self.counter.next()).unwrap()
                });

                unsafe {
                    LLVMBuildInsertValue(
                        *self.build,
                        cls_obj,
                        capt_obj,
                        1,
                        self.counter.next().as_ptr(),
                    )
                }
                // cls_obj
            }
        }
    }

    fn lookup(&mut self, i: Arg, t: Type) -> LLVMValueRef {
        for &(ref a, ref ty, v) in self.vars.iter().chain(self.globals.iter()).rev() {
            if a == &i && ty.clone().unify(t.clone()).is_ok() {
                assert!(!v.is_null(), "Value stored as null.");
                return v;
            }
        }
        eprintln!("{:?}\n{:?}\n", self.vars, self.globals);
        panic!("Value undefined: {:?} :: {:?}", i, t)
    }

    fn compile_var(&mut self, i: Arg, t: Type) -> LLVMValueRef {
        let v = self.lookup(i, t);
        if unsafe { LLVMIsAFunction(v) != null() } {
            return v;
        } else if unsafe { LLVMIsAGlobalValue(v) == null() } {
            return v;
        } else {
            return unsafe { LLVMBuildLoad(*self.build, v, self.counter.next().as_ptr()) };
        }
    }

    fn compile_match(&mut self, arg: Node, arms: Vec<(Pat, Node)>, res: Type) -> LLVMValueRef {
        let res_type = self.ll_type(&res);
        let arg_type = arg.get_type();
        let arg_ll = self.compile(arg);

        let end_block = self.func.append_bb(self.counter.next()).unwrap();

        let (mut vals, mut blks): (Vec<LLVMValueRef>, Vec<LLVMBasicBlockRef>) = arms.into_iter()
            .map(|(p, b)| {
                let end_arm = self.func.append_bb(self.counter.next()).unwrap();
                let else_blk = self.func.append_bb(self.counter.next()).unwrap();
                let args = self.compile_pattern(p, &arg_type, arg_ll, *else_blk);
                // let old_vars = self.vars.clone();
                let old_len = self.vars.len();

                self.vars.extend(
                    args.into_iter().map(|((a, b), c)| (a, b, c)),
                );
                let val = self.compile(b);
                unsafe { LLVMBuildBr(*self.build, *end_arm) };
                let end = *end_arm;
                self.build.set_block(end_arm);
                unsafe { LLVMBuildBr(*self.build, *end_block) };
                self.build.set_block(else_blk);

                // self.vars = old_vars;
                self.vars.truncate(old_len);

                (val, end)
            })
            .unzip();

        unsafe {
            LLVMBuildUnreachable(*self.build);
        }

        self.build.set_block(end_block);
        let phi = unsafe { LLVMBuildPhi(*self.build, res_type, self.counter.next().as_ptr()) };
        unsafe { LLVMAddIncoming(phi, vals.as_mut_ptr(), blks.as_mut_ptr(), vals.len() as u32) };
        phi
    }

    fn compile_pattern(
        &mut self,
        p: Pat,
        ty: &Type,
        arg: LLVMValueRef,
        else_blk: LLVMBasicBlockRef,
    ) -> HashMap<(Arg, Type), LLVMValueRef> {
        match p {
            Pat::Prim(a) => a.into_iter().map(|a| ((a, ty.clone()), arg)).collect(),
            Pat::Lit(i) => {
                let pred = unsafe {
                    LLVMBuildICmp(
                        *self.build,
                        LLVMIntPredicate::LLVMIntEQ,
                        arg,
                        wrap::sint(i, self.ctx.int_type(64)),
                        self.counter.next().as_ptr(),
                    )
                };
                let new_blk = self.func.append_bb(self.counter.next()).unwrap();
                unsafe { LLVMBuildCondBr(*self.build, pred, *new_blk, else_blk) };
                self.build.set_block(new_blk);
                HashMap::new()
            }
            Pat::Cons(c, args) => {
                let (name, type_args) = match ty {
                    &Type::Alias(ref n, ref ts) => (n, ts),
                    t => panic!("Not ADT passed to match: {:?}", t),
                };
                let cons = match self.types.get(name) {
                    Some(&Scheme::Type(EnumDecl(ref cons))) => cons.clone(),
                    Some(&Scheme::Forall(ref cons, ref ids)) => {
                        let info = TypeInfo::from_subst(
                            ids.iter()
                                .map(|&(id, _)| id)
                                .zip(type_args.clone())
                                .collect(),
                        );
                        cons.clone().apply(&info).0
                    }
                    None => panic!("ADT undefined: {}", name),
                };
                let (variant, mut adt_args) =
                    cons.iter()
                        .enumerate()
                        .filter(|&(_, &(ref i, _))| i == &c)
                        .map(|(i, &(_, ref c))| (i, c))
                        .next()
                        .expect(format!("Enum {} not variant of given value.", c).as_str());
                assert_eq!(
                    args.len(),
                    adt_args.len(),
                    "Incorrect number of arguments to constructor."
                );

                let arg = match cons.len() {
                    1 => unsafe {
                        LLVMBuildExtractValue(*self.build, arg, 0, self.counter.next().as_ptr())
                    },
                    _ => {
                        let tag = unsafe {
                            LLVMBuildExtractValue(*self.build, arg, 1, self.counter.next().as_ptr())
                        };
                        let pred = unsafe {
                            LLVMBuildICmp(
                                *self.build,
                                LLVMIntPredicate::LLVMIntEQ,
                                tag,
                                wrap::uint(variant as u64, LLVMTypeOf(tag)),
                                self.counter.next().as_ptr(),
                            )
                        };
                        let new_blk = self.func.append_bb(self.counter.next()).unwrap();
                        unsafe { LLVMBuildCondBr(*self.build, pred, *new_blk, else_blk) };
                        self.build.set_block(new_blk);
                        let value = unsafe {
                            LLVMBuildExtractValue(*self.build, arg, 0, self.counter.next().as_ptr())
                        };
                        let ptr = unsafe {
                            LLVMBuildAlloca(
                                *self.build,
                                LLVMTypeOf(value),
                                self.counter.next().as_ptr(),
                            )
                        };
                        unsafe { LLVMBuildStore(*self.build, value, ptr) };
                        unsafe {
                            LLVMBuildLoad(
                                *self.build,
                                LLVMBuildBitCast(
                                    *self.build,
                                    ptr,
                                    LLVMPointerType(self.tuple_type(&mut adt_args), 0),
                                    //LLVMPointerType(self.tuple_type(), 0),
                                    self.counter.next().as_ptr(),
                                ),
                                self.counter.next().as_ptr(),
                            )
                        }
                    }
                };
                let arg_types = match ty {
                    &Type::Tuple(ref ts) => &**ts,
                    &Type::Alias(_, ref ts) => &**ts,
                    _ => &[],
                };
                args.into_iter()
                    .zip(arg_types)
                    .enumerate()
                    .filter_map(|(i, (a, t))| {
                        a.map(|a| {
                            ((a, t.clone()), unsafe {
                                LLVMBuildExtractValue(
                                    *self.build,
                                    arg,
                                    i as u32,
                                    self.counter.next().as_ptr(),
                                )
                            })
                        })
                    })
                    .collect()
            }
        }
    }

    fn compile_let(&mut self, i: Arg, e: Node, b: Node) -> LLVMValueRef {
        let t = e.get_type();
        let e = self.compile(e);
        self.vars.push((i, t.clone(), e));
        let v = self.compile(b);
        self.vars.pop();
        v
    }

    fn compile_constr(&mut self, args: Vec<Node>, t: Type, variant: u64) -> LLVMValueRef {
        let ll_type = self.ll_type(&t);
        if let Type::Alias(i, t_args) = t {
            let adt = match self.types.get(&i) {
                Some(&Scheme::Type(EnumDecl(ref adt))) => adt,
                Some(&Scheme::Forall(EnumDecl(ref adt), _)) => adt,
                None => panic!("Undefined enum: {}", i),
            };

            let (_, ref ts) = adt[variant as usize];
            let var_type = self.tuple_type(
                args.iter()
                    .map(|n| n.get_type())
                    .collect::<Vec<_>>()
                    .as_slice(),
            );
            let var_struct = args.into_iter().enumerate().fold(
                unsafe {
                    LLVMGetUndef(var_type)
                },
                |s, (i, n)| {
                    let v = self.compile(n);
                    self.build
                        .build_insert_value(s, v, i as u32, self.counter.next())
                        .unwrap()
                },
            );
            match adt.len() {
                1 => {
                    self.build
                        .build_insert_value(
                            unsafe { LLVMGetUndef(ll_type) },
                            var_struct,
                            0,
                            self.counter.next(),
                        )
                        .unwrap()
                }
                n => {
                    let l = unsafe { LLVMCountStructElementTypes(ll_type) };
                    assert_eq!(2, l, "ADT has {} fields.", l);
                    let ptr = unsafe {
                        LLVMBuildAlloca(
                            *self.build,
                            LLVMTypeOf(var_struct),
                            self.counter.next().as_ptr(),
                        )
                    };
                    unsafe { LLVMBuildStore(*self.build, var_struct, ptr) };
                    let cast = unsafe {
                        LLVMBuildLoad(
                            *self.build,
                            LLVMBuildBitCast(
                                *self.build,
                                ptr,
                                LLVMPointerType(LLVMStructGetTypeAtIndex(ll_type, 0), 0),
                                self.counter.next().as_ptr(),
                            ),
                            self.counter.next().as_ptr(),
                        )
                    };
                    let adt = self.build
                        .build_insert_value(
                            unsafe { LLVMGetUndef(ll_type) },
                            cast,
                            0,
                            self.counter.next(),
                        )
                        .unwrap();
                    self.build
                        .build_insert_value(
                            adt,
                            wrap::uint(variant, unsafe { LLVMStructGetTypeAtIndex(ll_type, 1) }),
                            1,
                            self.counter.next(),
                        )
                        .unwrap()
                }
            }
        } else {
            panic!("Invalid return type of type constructor.")
        }
    }

    fn ll_type(&mut self, t: &Type) -> LLVMTypeRef {
        self.ctx.ll_type(t, self.types, self.structs, self.target)
    }

    fn tuple_type(&mut self, ts: &[Type]) -> LLVMTypeRef {
        self.ctx.tuple_type(&mut ts.iter()
            .map(|t| self.ll_type(t))
            .collect::<Vec<_>>())
    }
}
