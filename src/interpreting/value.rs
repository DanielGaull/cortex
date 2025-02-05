#[derive(Clone)]
pub enum CortexValue {
    Number(f64),
    Boolean(bool),
    String(String),
    Void,
    Null,
}
