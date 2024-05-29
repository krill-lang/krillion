mod wrapper;
use wrapper as llvm;

pub fn emit_code() {
    let mut llvm = llvm::Llvm::new();

    let void = llvm.type_void();
    let fn_t = llvm.type_fn(vec![], void);
    llvm.create_fn_and_switch("main", fn_t);

    let int = llvm.type_int(8);

    let a = llvm.create_bb("a");
    let b = llvm.create_bb("b");

    llvm.switch_to_bb(a);
    llvm.build_alloca("abc", int);
    llvm.build_br(b);

    llvm.switch_to_bb(b);
    llvm.build_ret(None);

    llvm.dump();
}
