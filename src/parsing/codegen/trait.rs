pub trait SimpleCodeGen {
    fn codegen(&self, indent: usize) -> String;
}
