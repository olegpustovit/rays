//! Partial capacity-based accounting of arena values, environments, records and checked storage.
//! Map nodes, type/diagnostic payloads, callback captures and compiled-code caches stay excluded.
use super::*;
use flow_syntax::syntax::Memory;

#[derive(Default)]
struct Storage {
    heap: Memory,
    checked: Memory,
    syntax: Memory,
    workspace: W::Memory,
}
impl Storage {
    fn shared<T>(&mut self, value: &Arc<T>) -> bool {
        self.heap.allocation(Arc::as_ptr(value), std::mem::size_of::<T>())
    }
    fn buffer<T>(&mut self, value: &Vec<T>) -> bool {
        self.heap.allocation(value.as_ptr(), value.capacity() * std::mem::size_of::<T>())
    }
    fn text(&mut self, value: &String) { self.heap.allocation(value.as_ptr(), value.capacity()); }
    fn path(&mut self, path: &Path) {
        if self.buffer(path) { for name in path { self.text(name); } }
    }
    fn term(&mut self, term: &Term) { term.arena().attribute(&mut self.checked, &mut self.syntax); }
    fn fields(&mut self, fields: &Vec<(String, Value)>) {
        if self.buffer(fields) { for (name, value) in fields { self.text(name); self.value(value); } }
    }
    fn values(&mut self, values: &Vec<Value>) {
        if self.buffer(values) { for value in values { self.value(value); } }
    }
    fn value(&mut self, value: &Value) {
        let mut pending = vec![value];
        while let Some(value) = pending.pop() {
            match value {
                Value::Text(bytes) => { self.buffer(bytes); }
                Value::Int_array(values) => { if self.shared(values) { self.buffer(values); } }
                Value::Floats(1..=4, values) => {
                    if self.shared(values) { self.buffer(values); }
                }
                Value::Text_array(values) => {
                    self.heap.allocation(Arc::as_ptr(values), std::mem::size_of_val(&**values) + values.payload_bytes());
                }
                Value::List(values) => { if self.shared(values) && self.buffer(values) { pending.extend(values.iter()); } }
                Value::Struct(g) | Value::Source(g) => {
                    self.text(&g.name);
                    if self.buffer(&g.fields) { for (name, value) in &g.fields { self.text(name); pending.push(value); } }
                }
                Value::Record(fields) => {
                    if self.buffer(fields) { for (name, value) in fields { self.text(name); pending.push(value); } }
                }
                _ => (),
            }
        }
    }
    fn context(&mut self, context: &ExecCtx) {
        if self.shared(&context.prefix) { self.path(&context.prefix); }
        if self.shared(&context.base) { self.path(&context.base); }
        let mut at = &context.iter.0;
        while let Some(node) = at { if !self.heap.allocation(Arc::as_ptr(node), std::mem::size_of::<IterNode>()) { break; } at = &node.up.0; }
    }
    fn env(&mut self, env: &Captured) {
        if self.heap.allocation(Arc::as_ptr(env) as *const u8, std::mem::size_of_val(&**env)) { for (name, value) in env.iter() { self.name(name); self.value(value); } }
    }
    fn name(&mut self, name: &Arc<str>) { self.heap.allocation(Arc::as_ptr(name) as *const u8, name.len()); }
    fn arena(&mut self, arena: &Arena) {
        self.workspace.workspace(&arena.workspace.0);
        if self.shared(&arena.source_key) { if let Some(key) = arena.source_key.get() { self.text(key); } }
        self.buffer(&arena.authored);
        self.buffer(&arena.observed);
        if self.shared(&arena.res_log.0) { self.buffer(&arena.res_log); }
        if self.shared(&arena.fn_log.0) { self.buffer(&arena.fn_log); }
        if self.shared(&arena.nodes.0) && self.buffer(&arena.nodes) {
            for node in arena.nodes.iter() { self.path(&node.site); self.buffer(&node.iter); self.text(&node.kind); self.fields(&node.args); }
        }
        if self.shared(&arena.cells.0) && self.buffer(&arena.cells) {
            for cell in arena.cells.iter() { self.text(&cell.graph); self.fields(&cell.inputs); self.value(&cell.result); }
        }
        if self.shared(&arena.states.0) { self.values(&arena.states); }
        if self.shared(&arena.records.0) {
            for (path, records) in arena.records.iter() {
                self.path(path);
                if self.buffer(records) { for (iter, value) in records { self.buffer(iter); self.value(value); } }
            }
        }
        if self.shared(&arena.functions.0) {
            for function in arena.functions.values() {
                match function {
                    Function::Named(named) => self.text(&named.name),
                    Function::Closure(function) if self.shared(function) => {
                        self.term(&function.params.0);
                        if self.shared(&function.body) { self.term(&function.body); }
                        self.env(&function.env); self.path(&function.zone); self.context(&function.at);
                    }
                    _ => (),
                }
            }
        }
        if self.shared(&arena.residuals.0) {
            for residual in arena.residuals.values() {
                if self.shared(&residual.term) { self.term(&residual.term); }
                self.env(&residual.env); self.context(&residual.ctx);
            }
        }
        if self.shared(&arena.memo.0) { for value in arena.memo.values() { self.value(value); } }
        if self.shared(&arena.bases.0) { for path in arena.bases.iter() { if self.shared(path) { self.path(path); } } }
        if self.shared(&arena.packed_bases.0) { for path in arena.packed_bases.values() { if self.shared(path) { self.path(path); } } }
        if self.shared(&arena.names.0) { for name in arena.names.iter() { self.heap.allocation(Arc::as_ptr(name) as *const u8, name.len()); } }
        if self.shared(&arena.arg_pool.0) { for fields in arena.arg_pool.values() { self.fields(fields); } }
        if self.shared(&arena.terms.0) { for (_, term) in arena.terms.values() { if self.shared(term) { self.term(term); } } }
        if self.shared(&arena.frees.0) {
            for (_, names) in arena.frees.values() { if self.shared(names) && self.buffer(names) { for name in names.iter() { self.name(name); } } }
        }
    }
    fn allocations(self) -> Vec<(usize, usize)> {
        let (syntax, checked) = self.workspace.into_allocations();
        self.heap.into_allocations().into_iter().chain(self.checked.into_allocations()).chain(self.syntax.into_allocations()).chain(syntax).chain(checked).collect()
    }
}

/// Snapshot inspectable storage of live evaluator arenas. Busy/checked-out arenas are skipped;
/// this never waits for evaluation or retains arenas after reporting. Opaque storage is excluded.
pub fn memory_allocations() -> Vec<(usize, usize)> {
    let slots: Vec<_> = { ARENAS.lock().unwrap_or_else(|e| e.into_inner()).values().filter_map(Weak::upgrade).collect() };
    let mut memory = Storage::default();
    for slot in &slots {
        memory.shared(slot);
        let cell = match slot.cell.try_lock() {
            Ok(cell) => cell,
            Err(std::sync::TryLockError::Poisoned(error)) => error.into_inner(),
            Err(std::sync::TryLockError::WouldBlock) => continue,
        };
        if let Some(arena) = cell.as_ref() { memory.arena(arena); }
    }
    memory.allocations()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn shared_capacity_is_counted_once_and_busy_arenas_never_wait() {
        let checked = crate::test_workspace_eval::check("(workspace w (graph g :context host (+ t 1.0)))");
        let evaluated = static_eval(&checked).unwrap();
        let mut values = Vec::with_capacity(128);
        values.resize(64, 1.0);
        let data = Arc::new(values);
        let address = data.as_ptr() as usize;
        let expected = data.capacity() * std::mem::size_of::<f64>();
        {
            let mut arena = checkout(&evaluated.owner);
            arena.states.push(Value::Floats(1, data.clone()));
            arena.states.push(Value::Floats(1, data.clone()));
            let replay = arena.clone();
            let mut storage = Storage::default();
            storage.arena(&arena);
            let before = storage.heap.bytes();
            storage.arena(&replay);
            assert_eq!(storage.heap.bytes(), before, "replays share the retained buffers");
            assert_eq!(storage.heap.into_allocations().iter().filter(|(p, _)| *p == address).copied().collect::<Vec<_>>(), [(address, expected)]);
            assert!(!memory_allocations().iter().any(|(p, _)| *p == address), "checked-out arenas are skipped");
        }
        let owners = Arc::strong_count(&evaluated.owner);
        let report = memory_allocations();
        assert_eq!(Arc::strong_count(&evaluated.owner), owners, "reports do not retain arenas");
        assert_eq!(report.iter().filter(|(p, _)| *p == address).copied().collect::<Vec<_>>(), [(address, expected)]);
        let _busy = evaluated.owner.cell.lock().unwrap();
        assert!(!memory_allocations().iter().any(|(p, _)| *p == address), "locked arenas are skipped");
    }
}
