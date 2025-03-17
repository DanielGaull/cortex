use std::{cell::RefCell, collections::{HashMap, HashSet}, rc::Rc};

use crate::interpreting::value::CortexValue;

pub(crate) struct Heap {
    store: HashMap<usize, Rc<RefCell<CortexValue>>>,
    next_id: usize,
    gc_threshold: usize,
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            store: HashMap::new(),
            next_id: 0,
            gc_threshold: 100,
        }
    }

    pub fn sz(&self) -> usize {
        self.store.len()
    }

    pub fn get(&self, addr: usize) -> Rc<RefCell<CortexValue>> {
        self.store.get(&addr).unwrap().clone()
    }

    pub fn allocate(&mut self, value: CortexValue) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        self.store.insert(id, Rc::new(RefCell::new(value)));
        id
    }

    pub fn gc(&mut self, roots: HashSet<usize>) {
        let reachables = self.find_reachables(roots);

        let sz_before = self.store.len();
        self.sweep(reachables);
        let sz_after = self.store.len();

        if sz_after > sz_before / 2 {
            self.gc_threshold *= 2;
        } else {
            self.gc_threshold = (self.gc_threshold / 2).max(1000);
        }
    }

    pub fn is_at_gc_threshold(&self) -> bool {
        self.store.len() > self.gc_threshold
    }

    fn mark(&self, marked: &mut HashSet<usize>, addr: usize) {
        if marked.contains(&addr) {
            return;
        }
        if let Some(value) = self.store.get(&addr) {
            marked.insert(addr);
            self.mark_children(marked, value.clone());
        }
    }

    fn mark_children(&self, marked: &mut HashSet<usize>, value: Rc<RefCell<CortexValue>>) {
        if let CortexValue::Composite { struct_name: _, field_values, type_arg_names: _, type_args: _ } = &*value.borrow() {
            for (_, fvalue_ref) in field_values {
                let fvalue = fvalue_ref.borrow();
                if let CortexValue::Reference(addr, _, _) = *fvalue {
                    self.mark(marked, addr);
                } else if let CortexValue::Composite { struct_name: _, field_values: _, type_arg_names: _, type_args: _ } = *fvalue {
                    // NOTE: we are allowed to clone fields of composites
                    // We are only not allowed to clone values that directly appear on the heap
                    self.mark_children(marked, Rc::new(RefCell::new(fvalue.clone())));
                }
            }
        }
    }

    fn find_reachables(&self, roots: HashSet<usize>) -> HashSet<usize> {
        let mut result = HashSet::<usize>::new();
        for root in &roots {
            result.insert(*root);
        }
        for root in &roots {
            self.mark(&mut result, *root);
        }
        result
    }

    fn sweep(&mut self, reachables: HashSet<usize>) {
        self.store.retain(|addr, _| reachables.contains(addr));
    }
}
