#[derive(Debug, Clone)]
pub struct VTable {
    functions: Vec<usize>,
}
impl VTable {
    pub fn new(functions: Vec<usize>) -> Self {
        Self {
            functions
        }
    }

    pub fn get(&self, index: usize) -> Option<usize> {
        self.functions.get(index).copied()
    }

    pub fn add(&mut self, address: usize) {
        self.functions.push(address);
    }
}
