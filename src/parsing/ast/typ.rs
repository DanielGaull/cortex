use crate::parsing::codegen::r#trait::SimpleCodeGen;

pub enum CType {

}
impl SimpleCodeGen for CType {
    fn codegen(&self, _: usize) -> String {
        String::new()
    }
}
