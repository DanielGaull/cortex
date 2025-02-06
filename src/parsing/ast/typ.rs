use crate::parsing::codegen::r#trait::SimpleCodeGen;

#[derive(Clone)]
pub enum CType {
    Basic {
        name: String,
        is_nullable: bool,
    }
}
impl SimpleCodeGen for CType {
    fn codegen(&self, _: usize) -> String {
        match self {
            Self::Basic { name, is_nullable } => {
                let mut s = String::new();
                s.push_str(name);
                if *is_nullable {
                    s.push_str("?");
                }
                s
            }
        }
    }
}
