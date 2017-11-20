use llvm_sys::prelude::*;
use llvm_sys::core::*;
use llvm_sys::target::*;
use std::ffi::{CString, CStr, NulError};
use std::fmt::{self, Display, Formatter};
use std::marker::PhantomData;
use std::ops::Deref;
use std::convert::AsMut;
use std::collections::hash_map::{HashMap, DefaultHasher};
use std::hash::{Hash, Hasher};

use super::super::w_ds::{Type, TypeInfo, Types, Scheme, EnumDecl};

macro_rules! dispose {
	($t:ident, $f:ident) => (impl Drop for $t {
		fn drop(&mut self) {
			unsafe { $f(self.0) }
		}
	});
	($l:tt, $t:ident, $f:ident) => (impl <$l> Drop for $t<$l> {
		fn drop(&mut self) {
			unsafe { $f(self.0) }
		}
	});
}

macro_rules! deref {
	($t:ident, $i:ident) => (impl Deref for $t {
		type Target = $i;
		fn deref(&self) -> &$i{
			&self.0
		}
	});
	($l:tt, $t:ident, $i:ident) => (impl <$l> Deref for $t<$l> {
		type Target = $i;
		fn deref(&self) -> &$i{
			&self.0
		}
	});
}

fn null<T>() -> *mut T {
    0 as *mut T
}

pub struct Context(LLVMContextRef);
pub struct Module<'a>(LLVMModuleRef, &'a Context, CString);
pub struct Builder<'a>(LLVMBuilderRef, PhantomData<&'a ()>);
pub struct BasicBlock<'a>(LLVMBasicBlockRef, PhantomData<&'a ()>, CString);
pub struct Function<'a>(LLVMValueRef, &'a Context, CString);

dispose!(Context, LLVMContextDispose);
dispose!('a, Module, LLVMDisposeModule);
dispose!('a, Builder, LLVMDisposeBuilder);
//dispose!('a, BasicBlock, LLVMDeleteBasicBlock);
// dispose!('a, Function, LLVMDeleteFunction);

deref!(Context, LLVMContextRef);
deref!('a, Module, LLVMModuleRef);
deref!('a, Builder, LLVMBuilderRef);
deref!('a, BasicBlock, LLVMBasicBlockRef);
deref!('a, Function, LLVMValueRef);

impl Context {
    pub fn new() -> Context {
        Context(unsafe { LLVMContextCreate() })
    }

    pub fn module<'a, T: Into<Vec<u8>>>(&'a self, name: T) -> Result<Module<'a>, NulError> {
        let name = CString::new(name)?;
        Ok(Module(
            unsafe {
                LLVMModuleCreateWithNameInContext(name.as_ptr(), self.0)
            },
            self,
            name,
        ))
    }

    pub fn int_type(&self, bits: u32) -> LLVMTypeRef {
        unsafe { LLVMIntTypeInContext(self.0, bits) }
    }

    pub fn builder<'a>(&'a self) -> Builder<'a> {
        Builder(unsafe { LLVMCreateBuilderInContext(self.0) }, PhantomData)
    }

    pub fn ll_type(
        &self,
        t: &Type,
        types: &HashMap<String, Scheme<EnumDecl>>,
        structs: &mut HashMap<(String, Vec<Type>), LLVMTypeRef>,
        target: LLVMTargetDataRef,
    ) -> LLVMTypeRef {
        match *t {
            Type::Int => self.int_type(64),
            Type::Int8 => self.int_type(8),
            Type::Bool => self.int_type(1),
            Type::Unit => self.tuple_type(&mut []),
            Type::Tuple(ref ts) => {
                let mut ts: Vec<_> = ts.iter()
                    .map(|t| self.ll_type(t, types, structs, target))
                    .collect();
                self.tuple_type(&mut ts)
            }
            Type::Func(ref a, ref r) => unsafe {
                LLVMPointerType(
                    LLVMFunctionType(
                        self.ll_type(r, types, structs, target),
                        &mut self.ll_type(a, types, structs, target) as *mut _,
                        1,
                        0,
                    ),
                    0,
                )
            },
            Type::Closure(ref a, ref r, ref c) => {
                let mut ts: Vec<_> = c.iter()
                    .map(|t| self.ll_type(t, types, structs, target))
                    .collect();
                let cap = self.tuple_type(&mut ts);
                let mut args = [cap, self.ll_type(a, types, structs, target)];
                let f_ty = unsafe {
                    LLVMPointerType(
                        LLVMFunctionType(
                            self.ll_type(r, types, structs, target),
                            args.as_mut_ptr(),
                            args.len() as u32,
                            0,
                        ),
                        0,
                    )
                };
                self.tuple_type(&mut [f_ty, cap])
            }
            Type::Free(_) => {
                eprintln!("Free type made to compilation!");
                self.int_type(64) // Assume Int
            }
            Type::Ptr(ref t) => unsafe {
                LLVMPointerType(self.ll_type(&**t, types, structs, target), 0)
            },
            Type::Alias(ref i, ref args) => {
                let key = (i.clone(), args.clone());
                {
                    match structs.get(&key) {
                        Some(&t) => return t,
                        None => {}
                    };
                }

                let struct_type = unsafe {
                    LLVMStructCreateNamed(
                        **self,
                        CString::new(format!("{}.{}", i, hash(args.as_slice())))
                            .unwrap()
                            .as_ptr(),
                    )
                };

                structs.insert(key, struct_type);

                let e = match types.get(i) {
                    Some(&Scheme::Type(EnumDecl(ref e))) => {
                        assert_eq!(
                            args.len(),
                            0,
                            "Type arguments given for non polymorphic type."
                        );
                        e.clone()
                    }
                    Some(&Scheme::Forall(ref e, ref ty_args)) => {
                        assert_eq!(args.len(), ty_args.len(), "Wrong number of type arguments.");

                        let e = e.clone().apply(&args.clone()
                            .into_iter()
                            .zip(ty_args.iter().map(|&(id, _)| Type::Free(id)))
                            .map(|(t1, t2)| t1.unify(t2).unwrap())
                            .fold(TypeInfo::new(), |i1, i2| i1.compose(i2)));
                        e.0
                    }
                    None => panic!("Undefined declared type."),
                };

                let mut ts = e.iter()
                    .map(|&(_, ref ts)| self.tuple_type(&mut ts.iter().map(|t| self.ll_type(t, types, structs, target)).collect::<Vec<_>>()))
                    .collect::<Vec<_>>();
                // Please add unions, LLVM! PLEASE!!!! :(
                match ts.len() {
                    0 => unsafe {
                        LLVMStructSetBody(
                            struct_type,
                            &mut LLVMVoidTypeInContext(**self) as *mut LLVMTypeRef,
                            1,
                            0,
                        )
                    },
                    1 => unsafe { LLVMStructSetBody(struct_type, ts.as_mut_ptr(), 1, 0) },
                    n => {
                        let union = self.union_type(ts, target);
                        let tag_size = (n as f64).log2().ceil() as u32;
                        unsafe {
                            LLVMStructSetBody(
                                struct_type,
                                [union, self.int_type(tag_size)].as_mut_ptr(),
                                2,
                                0,
                            )
                        }
                    }
                };
                struct_type
            }
        }
    }

    pub fn tuple_type<T: ?Sized + AsMut<[LLVMTypeRef]>>(&self, ts: &mut T) -> LLVMTypeRef {
        let tmp = ts.as_mut();
        unsafe { LLVMStructTypeInContext(**self, tmp.as_mut_ptr(), tmp.len() as u32, 0) }
    }

    // Hack to support unions.
    fn union_type(&self, ts: Vec<LLVMTypeRef>, target: LLVMTargetDataRef) -> LLVMTypeRef {
        match ts.iter()
            .map(|&t| unsafe { LLVMABISizeOfType(target, t) })
            .max() {
            None | Some(0) => unsafe { LLVMStructTypeInContext(**self, null(), 0, 0) },
            Some(size) => {
                let align = match ts.iter()
                    .map(|&t| unsafe { LLVMABIAlignmentOfType(target, t) })
                    .max() {
                    None => 1,
                    Some(n) => n as u32,
                };
                assert!(size as u32 >= align, "Alignment greater than size!");
                unsafe {
                    LLVMStructTypeInContext(
                        **self,
                        [
                            self.int_type(8 * align),
                            LLVMArrayType(self.int_type(8), (size as u32) - align),
                        ].as_mut_ptr(),
                        2,
                        0,
                    )
                }
            }
        }
    }
}

fn hash<T: Hash>(v: &[T]) -> u64 {
    let mut h = DefaultHasher::new();
    T::hash_slice(v, &mut h);
    h.finish()
}

impl<'a> Display for Module<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let ptr = unsafe { CStr::from_ptr(LLVMPrintModuleToString(self.0)) };

        write!(f, "{}", ptr.to_string_lossy())
    }
}

impl<'a> Module<'a> {
    pub fn new_function<T: Into<Vec<u8>>>(
        &'a self,
        name: T,
        ty: LLVMTypeRef,
    ) -> Result<Function<'a>, NulError> {
        let name = CString::new(name)?;
        // let ty = unsafe {LLVMFunctionType(ret, args.as_mut_ptr(), args.len() as u32, 0) };
        Ok(Function(
            unsafe { LLVMAddFunction(self.0, name.as_ptr(), ty) },
            self.1,
            name,
        ))
    }
}

impl<'a> Function<'a> {
    pub fn append_bb<T: Into<Vec<u8>>>(&self, name: T) -> Result<BasicBlock<'a>, NulError> {
        let name = CString::new(name)?;
        // eprintln!("{:?}", name);
        Ok(BasicBlock(
            unsafe {
                LLVMAppendBasicBlockInContext((self.1).0, self.0, name.as_ptr())
            },
            PhantomData,
            name,
        ))
    }
}

impl<'a> Builder<'a> {
    pub fn set_block(&self, bb: BasicBlock<'a>) {
        unsafe { LLVMPositionBuilderAtEnd(self.0, bb.0) }
    }

    pub fn build_insert_value<T: Into<Vec<u8>>>(
        &self,
        agg: LLVMValueRef,
        elem: LLVMValueRef,
        idx: u32,
        name: T,
    ) -> Result<LLVMValueRef, NulError> {
        assert!(
            !agg.is_null() && !elem.is_null(),
            "One of paramenters is null."
        );
        unsafe {
            use llvm_sys::LLVMTypeKind::*;
            let obj_ty = LLVMTypeOf(agg);
            let elem_kind = LLVMGetTypeKind(LLVMTypeOf(elem));
            match LLVMGetTypeKind(obj_ty) {
                LLVMStructTypeKind => {
                    assert!(idx < LLVMCountStructElementTypes(obj_ty));
                    assert_eq!(
                        LLVMGetTypeKind(LLVMStructGetTypeAtIndex(obj_ty, idx)),
                        elem_kind,
                        "Invalid type insertion."
                    );
                }
                LLVMArrayTypeKind => {
                    assert!(idx < LLVMGetArrayLength(obj_ty));
                    assert_eq!(
                        LLVMGetTypeKind(LLVMGetElementType(obj_ty)),
                        elem_kind,
                        "Invalid type insertion."
                    );
                }
                k => panic!("Only arrays and structs allowed. Got a {:?}", k),
            }
        }

        let name = CString::new(name)?;
        Ok(unsafe {
            LLVMBuildInsertValue(self.0, agg, elem, idx, name.as_ptr())
        })
    }
}

pub fn uint(i: u64, t: LLVMTypeRef) -> LLVMValueRef {
    unsafe { LLVMConstInt(t, i, 0) }
}

pub fn sint(i: i64, t: LLVMTypeRef) -> LLVMValueRef {
    unsafe { LLVMConstInt(t, i as u64, 0) }
}
