// Here's how vtables are contructed:
// If a type is `follows X`, then the methods of X are added in order to the vtable
// If a type is `follows X + Y`, then the methods of Y will appear after the methods of X 
// (and so on for adding more entries to the follows clause)

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
