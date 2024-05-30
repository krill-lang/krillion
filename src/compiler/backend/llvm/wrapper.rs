use llvm_sys::prelude::*;
use llvm_sys as llvm;
use std::ffi::*;

pub struct Llvm {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,

    current_fn: Option<LLVMValueRef>,
}

impl Llvm {
    pub fn new() -> Self {
        unsafe {
            let context = llvm::core::LLVMContextCreate();
            let module = llvm::core::LLVMModuleCreateWithName(b"krill\0".as_ptr() as *const _);
            let builder = llvm::core::LLVMCreateBuilderInContext(context);

            Self {
                context,
                module,
                builder,

                current_fn: None,
            }
        }
    }

    pub fn dump(&self) {
        unsafe { llvm::core::LLVMDumpModule(self.module); }
    }

    pub fn emit_obj(&self) {
        unsafe {
            llvm::target::LLVM_InitializeAllTargetInfos();
            llvm::target::LLVM_InitializeAllTargets();
            llvm::target::LLVM_InitializeAllTargets();
            llvm::target::LLVM_InitializeAllAsmParsers();
            llvm::target::LLVM_InitializeAllAsmPrinters();

            let triple = llvm::target_machine::LLVMGetDefaultTargetTriple();
            assert!(!triple.is_null());

            println!("{}", CStr::from_ptr(triple).to_str().unwrap());

            let mut target = core::ptr::null_mut();
            let mut error = core::ptr::null_mut();
            let result = llvm::target_machine::LLVMGetTargetFromTriple(triple, &mut target, &mut error);
            assert!(!target.is_null());
            assert!(error.is_null());
            assert!(result == 0);

            println!("{}", CStr::from_ptr(llvm::target_machine::LLVMGetTargetName(target)).to_str().unwrap());

            let machine = llvm::target_machine::LLVMCreateTargetMachine(
                target,
                triple,
                b"generic\0".as_ptr() as *const _,
                b"\0".as_ptr() as *const _,
                llvm::target_machine::LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
                llvm::target_machine::LLVMRelocMode::LLVMRelocDefault,
                llvm::target_machine::LLVMCodeModel::LLVMCodeModelDefault,
            );

            llvm::core::LLVMSetTarget(self.module, triple);

            println!("a");

            llvm::target_machine::LLVMTargetMachineEmitToFile(
                machine,
                self.module,
                b"test.o\0".clone().as_mut_ptr() as *mut _,
                llvm::target_machine::LLVMCodeGenFileType::LLVMObjectFile,
                core::ptr::null_mut(),
            );
            println!("a");

            llvm::target_machine::LLVMDisposeTargetMachine(machine);
        }
    }

    pub fn create_bb(&mut self, name: &str) -> LLVMBasicBlockRef {
        unsafe {
            llvm::core::LLVMAppendBasicBlockInContext(
                self.context,
                self.current_fn.expect("make a function before calling `create_bb`!"),
                self.new_name(name).as_ptr(),
            )
        }
    }

    pub fn create_fn_and_switch(&mut self, name: &str, fn_t: LLVMTypeRef) -> LLVMValueRef {
        unsafe {
            let ret = llvm::core::LLVMAddFunction(self.module, self.new_name(name).as_ptr(), fn_t);
            self.current_fn = Some(ret);
            ret
        }
    }

    pub fn switch_to_bb(&self, bb: LLVMBasicBlockRef) {
        unsafe {
            llvm::core::LLVMPositionBuilderAtEnd(self.builder, bb);
        }
    }

    // types
    pub fn type_void(&self) -> LLVMTypeRef {
        unsafe { llvm::core::LLVMVoidTypeInContext(self.context) }
    }

    pub fn type_fn(&mut self, mut args: Vec<LLVMTypeRef>, ret: LLVMTypeRef) -> LLVMTypeRef {
        unsafe {
            llvm::core::LLVMFunctionType(ret, args.as_mut_ptr(), args.len() as u32, 0)
        }
    }

    pub fn type_int(&self, bits: usize) -> LLVMTypeRef {
        unsafe { llvm::core::LLVMIntType(bits as u32) }
    }

    // terms
    pub fn build_ret(&self, val: Option<LLVMValueRef>) {
        unsafe {
            if let Some(val) = val {
                llvm::core::LLVMBuildRet(self.builder, val);
            } else {
                llvm::core::LLVMBuildRetVoid(self.builder);
            }
        }
    }

    pub fn build_br(&self, tar: LLVMBasicBlockRef) {
        unsafe {
            llvm::core::LLVMBuildBr(self.builder, tar);
        }
    }

    // insts
    pub fn build_alloca(&mut self, name: &str, ty: LLVMTypeRef) -> LLVMValueRef {
        unsafe {
            llvm::core::LLVMBuildAlloca(self.builder, ty, self.new_name(name).as_ptr())
        }
    }

    // private helpers
    fn new_name(&mut self, n: &str) -> CString {
        CString::new(n).unwrap()
    }
}

impl Drop for Llvm {
    fn drop(&mut self) {
        unsafe {
            llvm::core::LLVMDisposeBuilder(self.builder);
            llvm::core::LLVMDisposeModule(self.module);
            llvm::core::LLVMContextDispose(self.context);
        }
    }
}
