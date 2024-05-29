use llvm_sys::prelude::*;
use llvm_sys as llvm;

pub struct Llvm {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,

    current_fn: Option<LLVMValueRef>,

    fn_args: Vec<LLVMTypeRef>,
    names: Vec<u8>,
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

                fn_args: vec![],
                names: vec![],
            }
        }
    }

    pub fn dump(&self) {
        unsafe { llvm::core::LLVMDumpModule(self.module); }
    }

    pub fn emit_obj(&self) {
        unsafe {
            let triple = llvm::target_machine::LLVMGetDefaultTargetTriple();
            llvm::target::LLVM_InitializeAllTargetInfos();
            llvm::target::LLVM_InitializeAllTargets();
            llvm::target::LLVM_InitializeAllTargets();
            llvm::target::LLVM_InitializeAllAsmParsers();
            llvm::target::LLVM_InitializeAllAsmPrinters();

            let machine = llvm::target_machine::LLVMCreateTargetMachine(
                todo!(),
                triple,
                b"generic\0".as_ptr() as _,
                b"\0".as_ptr() as _,
                todo!(),
                todo!(),
                todo!(),
            );

            llvm::target_machine::LLVMTargetMachineEmitToFile(
                todo!(),
                self.module,
                b"test.o\0".as_mut_ptr() as *mut _,
                todo!(),
                core::ptr::null_mut(),
            );
        }
    }

    pub fn create_bb(&mut self, name: &str) -> LLVMBasicBlockRef {
        unsafe {
            llvm::core::LLVMAppendBasicBlockInContext(
                self.context,
                self.current_fn.expect("make a function before calling `create_bb`!"),
                self.new_name(name),
            )
        }
    }

    pub fn create_fn_and_switch(&mut self, name: &str, fn_t: LLVMTypeRef) -> LLVMValueRef {
        unsafe {
            let ret = llvm::core::LLVMAddFunction(self.module, self.new_name(name), fn_t);
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

    pub fn type_fn(&mut self, args: Vec<LLVMTypeRef>, ret: LLVMTypeRef) -> LLVMTypeRef {
        unsafe {
            let len = args.len();
            let start = self.fn_args.len();
            self.fn_args.extend(args);
            llvm::core::LLVMFunctionType(ret, self.fn_args[start..].as_mut_ptr(), len as u32, 0)
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
            llvm::core::LLVMBuildAlloca(self.builder, ty, self.new_name(name))
        }
    }

    // private helpers
    fn new_name(&mut self, n: &str) -> *const libc::c_char {
        let start = self.names.len();
        self.names.extend(n.bytes());
        self.names.push(0);
        self.names[start..].as_ptr() as *const _
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
