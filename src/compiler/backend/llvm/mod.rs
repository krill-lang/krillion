mod wrapper;
use wrapper as llvm;

pub fn emit_code() {
    let mut llvm = llvm::Llvm::new();

    let void = llvm.type_void();
    let int = llvm.type_int(8);

    let fn_t = llvm.type_fn(vec![int], void);
    llvm.create_fn_and_switch("main", fn_t);

    let a = llvm.create_bb("a");
    llvm.switch_to_bb(a);
    llvm.build_ret(None);

    llvm.dump();
    llvm.emit_obj();
}
