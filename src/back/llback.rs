use super::back::wrap::{self, Context, Module};
use super::desugar::{NodeDS, Pat, Arg};
use super::w_ds::{self, Type, TypeInfo};

#[derive(Debug)]
struct Compiler<'a> {
 	ctx: Context,
 	mod: Module<'a>,
 	build: Builder<'a>,
 	vars: HashMap<Arg, LLVMValueRef>,
} 

fn compile(n: NodeDS) -> String {

}

fn null<T>() -> *const T {
	0 as *const T
}

impl Compiler {
	fn compile(&mut self, n: NodeDS) -> LLVMValueRef {
		match n {
			NodeDS::Lit(i) => self.compile_lit(i),
			NodeDS::App(f, a) => self.compile_app(*f, *a),
			NodeDS::Var(i) => self.compile_var(i),
			NodeDS::Abs(i, b) => self.compile_abs(i, *b, )
		}
	}

	fn compile_lit(&mut self, l: i64) -> LLVMValueRef {
		wrap::sint(l, self.ll_type(&Type::Int))
	}

	fn compile_app(&mut self, f: NodeDS, a: NodeDS, f_ty: Type) -> LLVMValueRef {
		let f = self.compile(f);
		let a = self.compile(a);
		match f_ty {
			Type::Func(_, _) => {
				unsafe { LLVMBuildCall(self.build, f, &a as *mut LLVMValueRef, 1, 0 as *const _) }
			},
			Type::Closure(_, _, _) => {
				let f_ptr = unsafe { LLVMBuildExtractElement(self.build, f, wrap::uint(0, 8), null()) };
				let f_cap = unsafe { LLVMBuildExtractElement(self.build, f, wrap::uint(1, 8), null()) };
				let args = [f_cap, a];
				unsafe { LLVMBuildCall(self.build, f, &args.as_ptr(), args.len(), 0 as *const _) }
			}
		}
	}

	fn compile_abs(&mut self, i: Arg, b: NodeDS, f_ty: Type) -> LLVMValueRef {
		let ll_type = self.ll_type(&f_ty);
		match &f_ty {
			&Type::Func(_, _) => {
				let f_obj = unsafe { LLVMAddFunction(null, self.mod, ll_type) }
				let new_ctx = Compile{ctx: self.ctx, mod: self.mod, build: unsafe {LLVMCreateBuilderInContext(self.ctx)}, vars: HashMap::new()};
				let bb = unsafe { LLVMAppendBasicBlockInContext(new_ctx.ctx, f_obj, null()) }
				unsafe {LLVMPositionBuilderAtEnd(new_ctx.build, bb)};
				new_ctx.vars.insert(i, unsafe { LLVMGetParam(f_obj, 0) });
				unsafe { LLVMBuildRet(new_ctx.build, new_ctx.compile(b));
				unsafe { LLVMDisposeBuilder(new_ctx.build) };
				f_obj
			},
			&Type::Closure(_, _, _) => {
				let frees = b.free_vars();
				let capt = frees.iter().map(|v| self.vars.get(v).unwrap());

				let f_ptr_ty = unsafe { LLVMGetElementType(LLVMStructGetTypeAtIndex(ll_type, 0)) }
				let f_obj = unsafe { LLVMAddFunction(null, self.mod, ll_type) }

				let new_ctx = Compile{ctx: self.ctx, mod: self.mod, build: unsafe {LLVMCreateBuilderInContext(self.ctx)}, vars: HashMap::new()};
				
				let bb = unsafe { LLVMAppendBasicBlockInContext(new_ctx.ctx, f_obj, null()) }
				unsafe {LLVMPositionBuilderAtEnd(new_ctx.build, bb)};

				frees.iter().enumerate().map(|(i, v)| )
				let capts = unsafe { LLVMGetParam(f_obj, 0) };
				new_ctx.vars.extend(frees.iter().enumerate().map(|(i, v)| (v, unsafe { LLVMBuildExtractValue(new_ctx.build, capts, i, null()) }) ));
				new_ctx.vars.insert(i, unsafe { LLVMGetParam(f_obj, 1) });

				unsafe { LLVMBuildRet(new_ctx.build, new_ctx.compile(b));
				unsafe { LLVMDisposeBuilder(new_ctx.build) };

				let capt_type = unsafe { LLVMStructGetTypeAtIndex(ll_type, 1) };
				let capt_obj = unsafe { LLVMGetUndef(capt_type) };
				
				let cls_vec = [f_obj, capt_obj];
				let cls_obj = unsafe { LLVMConstStructInContext(self.ctx, cls_vec.as_ptr(), cls_vec.len(), 0) };

				let cap_built = capt.enumerate().fold(capt_obj |obj, (i, v)| unsafe { LLVMBuildInsertValue(self.build, obj, v, i as u32, null()) })				
				unsafe { LLVMBuildInsertValue(self.build, cls_obj, cap_built, 1, null()) }
			}
		}
	}

	fn compile_var(&mut self, i: Arg) -> LLVMValueRef {
		self.vars.get(&i).unwrap()
	}

	fn ll_type(&self, t: &Type) -> LLVMTypeRef {
		match *t {
			Type::Int => self.ctx.int_type(64),
			Type::Bool => self.ctx.int_type(1),
			Type::Unit => unsafe { LLVMStructType(null(), 0, 0) },
			Type::Tuple(ts) => {
				let ts = ts.iter().map(|t| self.ll_type(t)).collect();
				unsafe { LLVMStructType(ts.as_ptr(), ts.len(), 0) }
			},
			Type::Func(ref a, ref r) => unsafe { LLVMPointerType(LLVMFunctionType(self.ll_type(r), &self.ll_type(a) as *mut _, 1, 0 ), 0) },
			Type::Closure(ref a, ref r, ref c) => {
				let ts = c.iter().map(|t| self.ll_type(t)).collect();
				let cap = unsafe { LLVMStructType(ts.as_ptr(), ts.len(), 0) }
				let args = [cap, self.ll_type(a)];
				let f_ty = unsafe { LLVMPointerType(LLVMFunctionType(self.ll_type(r), args.as_ptr(), args.len(), 0 ), 0) };
				let closure = [f_ty, cap];
				unsafe { LLVMStructType(closure.as_ptr(), closure.len(), 0) }
			},
			Type::Free(_) => unreachable!(),
		}
	}

	fn zero_for_type(&self, t: &Type) -> LLVMValueRef {
		match *t {
			Type::Int => unsafe { LLVMConstInt(self.ctx.int_type(64), 0, 1),
			Type::Bool => unsafe { LLVMConstInt(self.ctx.int_type(1), 0, 0),
			Type::Unit => unsafe { LLVMConstStruct(null(), 0, 0) },
			Type::Tuple(ts) => {
				let vs = ts.iter().map(|t| self.zero_for_type(t)).collect();
				unsafe { LLVMConstStruct(ts.as_ptr(), ts.len(), 0) }
			},
			Type::Func(ref a, ref r) => unsafe { LLVMConstNull(LLVMPointerType(LLVMFunctionType(self.ll_type(r), &self.ll_type(a) as *mut _, 1, 0 ), 0)) },
			Type::Closure(ref a, ref r, ref c) => {
				let vs = c.iter().map(|t| self.zero_for_type(t)).collect();
				let cap = unsafe { LLVMConstStruct(vs.as_ptr(), vs.len(), 0) }
				let args = [cap, self.ll_type(a)];
				let f_ptr = unsafe { LLVMConstNull(LLVMPointerType(LLVMFunctionType(self.ll_type(r), args.as_ptr(), args.len(), 0 ), 0)) };
				let closure = [f_ptr, cap];
				unsafe { LLVMConstStruct(closure.as_ptr(), closure.len(), 0) }
			},
			Type::Free(_) => unreachable!(),
		}
	}
}