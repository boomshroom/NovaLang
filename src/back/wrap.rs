use llvm_sys::prelude::*;
use llvm_sys::core::*;
use std::ffi::{CString, CStr, NulError};
use std::fmt::{self, Display, Formatter};

macro_rules! dispose {
	($t:ident, $f:ident) => (impl Drop for $t {
		fn drop(&mut self) {
			unsafe { $f(self.0) }
		}
	});
	($l:tt, $t:ident, $f:ident) == (impl <$l> Drop for $t<$l> {
		fn drop(&mut self) {
			unsafe { $f(self.0) }
		}
	});
}

pub struct Context (LLVMContextRef);
pub struct Module<'a> (LLVMModuleRef, &'a Context, CString);
pub struct Builder<'a> (LLVMBuilderRef, PhantomData<'a>);
pub struct BasicBlock<'a> (LLVMBasicBlockRef, PhantomData<'a>, CString);
pub struct Function<'a> (LLVMValueRef, &'a Context, CString);

dispose(Context, LLVMContextDispose);
dispose('a, Module, LLVMDisposeModule);
dispose('a, Builder, LLVMDisposeBuilder);
dispose('a, BasicBlock, LLVMDeleteBasicBlock);
dispose('a, Function, LLVMDeleteFunction);

impl Context {
	pub fn new() -> Context {
		Context(unsafe { LLVMContextCreate() })
	}

	pub fn module<'a, T: Into<Vec<u8>>>(&'a self, name: T) -> Result<Module<'a>, NulError> {
		let name = CString::new(name)?;
		Ok(Module(unsafe { LLVMModuleCreateWithNameInContext(name.as_ptr(), self.0) }, self, name)
	}

	pub fn int_type(&self, bits: u32) -> LLVMTypeRef {
		unsafe { LLVMIntTypeInContext(self, bits) };
	}
}

impl <'a>Display for Module<'a> {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		let ptr = unsafe { CStr::fromPtr(LLVMPrintModuleToString(self.0)) };

		for byte in ptr.to_bytes().iter().flat_map(|&b| ascii::escape_default(b)) {
            f.write_char(byte as char)?;
        };
        Ok()
	}
}

impl <'a> Module<'a> {
	fn new_function<T: Into<Vec<u8>>>(&'a self, name: T, ret: LLVMTypeRef, args: &'a [LLVMTypeRef]) -> Result<Function<'a>, NulError> {
		let name = CString::new(name)?;
		let ty = LLVMFunctionType(ret, args.as_ptr(), args.len(), 0);
		Ok(Function(unsafe { LLVMAddFunction(name.as_ptr(), self.0, ty) }, self.1, name)
	}
}

impl <'a> Function<'a> {
	pub fn append_bb<T: Into<Vec<u8>>>(&self, name: T) -> Result<BasicBlock<'a>, NulError> {
		let name = CString::new(name)?;
		Ok(BasicBlock(unsafe { LLVMAppendBasicBlockInContext(self.1, self.0, name.as_ptr()) }, PhantomData, name))
	}
}

impl <'a> Builder<'a> {
	pub fn set_block(&self, bb: BasicBlock<'a>) {
		unsafe { LLVMPositionBuilderAtEnd(self.0, bb.0) }
	}
}

pub fn uint(i: u64, t: LLVMTypeRef) -> LLVMValueRef {
	unsafe { LLVMConstInt(t, i, 0) }
}

pub fn sint(i: i64, t: LLVMTypeRef) -> LLVMValueRef {
	unsafe { LLVMConstInt(t, i as u64, 0) }
}