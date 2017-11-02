use llvm_sys::prelude::*;
use llvm_sys::core::*;
use std::ffi::{CString, CStr, NulError};
use std::fmt::{self, Display, Formatter};
use std::marker::PhantomData;
use std::ops::Deref;

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

pub struct Context(LLVMContextRef);
pub struct Module<'a>(LLVMModuleRef, &'a Context, CString);
pub struct Builder<'a>(LLVMBuilderRef, PhantomData<&'a ()>);
pub struct BasicBlock<'a>(LLVMBasicBlockRef, PhantomData<&'a ()>, CString);
pub struct Function<'a>(LLVMValueRef, &'a Context, CString);

dispose!(Context, LLVMContextDispose);
dispose!('a, Module, LLVMDisposeModule);
dispose!('a, Builder, LLVMDisposeBuilder);
//dispose!('a, BasicBlock, LLVMDeleteBasicBlock);
//dispose!('a, Function, LLVMDeleteFunction);

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
        Ok(Module(unsafe { LLVMModuleCreateWithNameInContext(name.as_ptr(), self.0) },
                  self,
                  name))
    }

    pub fn int_type(&self, bits: u32) -> LLVMTypeRef {
        unsafe { LLVMIntTypeInContext(self.0, bits) }
    }

    pub fn builder<'a>(&'a self) -> Builder<'a> {
        Builder(unsafe { LLVMCreateBuilderInContext(self.0) }, PhantomData)
    }
}

impl<'a> Display for Module<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let ptr = unsafe { CStr::from_ptr(LLVMPrintModuleToString(self.0)) };

        write!(f, "{}", ptr.to_string_lossy())
    }
}

impl<'a> Module<'a> {
    pub fn new_function<T: Into<Vec<u8>>>(&'a self,
                                          name: T,
                                          ty: LLVMTypeRef)
                                          -> Result<Function<'a>, NulError> {
        let name = CString::new(name)?;
        // let ty = unsafe {LLVMFunctionType(ret, args.as_mut_ptr(), args.len() as u32, 0) };
        Ok(Function(unsafe { LLVMAddFunction(self.0, name.as_ptr(), ty) },
                    self.1,
                    name))
    }
}

impl<'a> Function<'a> {
    pub fn append_bb<T: Into<Vec<u8>>>(&self, name: T) -> Result<BasicBlock<'a>, NulError> {
        let name = CString::new(name)?;
        //eprintln!("{:?}", name);
        Ok(BasicBlock(unsafe { LLVMAppendBasicBlockInContext((self.1).0, self.0, name.as_ptr()) },
                      PhantomData,
                      name))
    }
}

impl<'a> Builder<'a> {
    pub fn set_block(&self, bb: BasicBlock<'a>) {
        unsafe { LLVMPositionBuilderAtEnd(self.0, bb.0) }
    }

    pub fn build_insert_value<T: Into<Vec<u8>>>(&self, agg: LLVMValueRef, elem: LLVMValueRef, idx: u32, name: T) -> Result<LLVMValueRef, NulError> {
        assert!(!agg.is_null() && !elem.is_null(), "One of paramenters is null.");
        unsafe {
            use llvm_sys::LLVMTypeKind::*;
            let obj_ty = LLVMTypeOf(agg);
            let elem_kind = LLVMGetTypeKind( LLVMTypeOf(elem) );
            match LLVMGetTypeKind(obj_ty) {
                LLVMStructTypeKind => {
                    assert!(idx < LLVMCountStructElementTypes(obj_ty));
                    assert_eq!(LLVMGetTypeKind(LLVMStructGetTypeAtIndex(obj_ty, idx)), elem_kind, "Invalid type insertion.");
                },
                LLVMArrayTypeKind => {
                    assert!(idx < LLVMGetArrayLength(obj_ty));
                    assert_eq!(LLVMGetTypeKind(LLVMGetElementType(obj_ty)), elem_kind, "Invalid type insertion.");
                },
                k => panic!("Only arrays and structs allowed. Got a {:?}", k),
            }
        }
        eprintln!("Type checks passed.");

        let name = CString::new(name)?;
        Ok(unsafe { LLVMBuildInsertValue(self.0, agg, elem, idx, name.as_ptr()) })
    }
}

pub fn uint(i: u64, t: LLVMTypeRef) -> LLVMValueRef {
    unsafe { LLVMConstInt(t, i, 0) }
}

pub fn sint(i: i64, t: LLVMTypeRef) -> LLVMValueRef {
    unsafe { LLVMConstInt(t, i as u64, 0) }
}
