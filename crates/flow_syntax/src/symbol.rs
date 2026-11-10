use std::collections::HashMap;
use std::sync::{LazyLock, RwLock};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(u32);

struct Symbols {
    names: Vec<&'static str>,
    by_name: HashMap<&'static str, Symbol>,
}
// ponytail: process symbols retain names until exit; add scoped generations if live editing creates
// millions of distinct names. IDs are never reused or serialized.
static SYMBOLS: LazyLock<RwLock<Symbols>> = LazyLock::new(|| RwLock::new(Symbols {
    names: Vec::new(), by_name: HashMap::new(),
}));

pub fn find(name: &str) -> Option<Symbol> {
    SYMBOLS.read().unwrap_or_else(|e| e.into_inner()).by_name.get(name).copied()
}
pub fn intern(name: &str) -> Symbol {
    find(name).unwrap_or_else(|| intern_owned(name.to_owned()))
}
pub fn intern_owned(name: String) -> Symbol {
    let mut symbols = SYMBOLS.write().unwrap_or_else(|e| e.into_inner());
    if let Some(&id) = symbols.by_name.get(name.as_str()) { return id; }
    let id = Symbol(u32::try_from(symbols.names.len()).expect("process symbol limit exceeded"));
    // The requested process interner owns these bytes until exit. A static slice avoids a second
    // ownership wrapper/atomic clone on every field/path read, without unsafe references to a Vec.
    let name: &'static str = Box::leak(name.into_boxed_str());
    symbols.names.push(name);
    symbols.by_name.insert(name, id);
    id
}
impl Symbol {
    pub fn id(self) -> u32 { self.0 }
    pub fn as_str(self) -> &'static str {
        SYMBOLS.read().unwrap_or_else(|e| e.into_inner()).names[self.0 as usize]
    }
}
impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.write_str(self.as_str()) }
}
impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { std::fmt::Debug::fmt(self.as_str(), f) }
}
impl std::cmp::Ord for Symbol {
    fn cmp(&self, rhs: &Self) -> std::cmp::Ordering {
        if self == rhs { std::cmp::Ordering::Equal } else { self.as_str().cmp(rhs.as_str()) }
    }
}
impl std::cmp::PartialOrd for Symbol {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> { Some(self.cmp(rhs)) }
}
impl From<&str> for Symbol { fn from(name: &str) -> Self { intern(name) } }
impl From<String> for Symbol { fn from(name: String) -> Self { intern_owned(name) } }
impl std::ops::Deref for Symbol {
    type Target = str;
    fn deref(&self) -> &str { self.as_str() }
}
// Deliberately no Borrow<str>: numeric Symbol hashing must not claim string-hash equivalence.
impl PartialEq<str> for Symbol { fn eq(&self, rhs: &str) -> bool { find(rhs) == Some(*self) } }
impl PartialEq<&str> for Symbol { fn eq(&self, rhs: &&str) -> bool { find(rhs) == Some(*self) } }
impl PartialEq<String> for Symbol { fn eq(&self, rhs: &String) -> bool { find(rhs) == Some(*self) } }

pub fn write_path(out: &mut String, path: &[Symbol], separator: &str) {
    for (index, name) in path.iter().enumerate() {
        if index != 0 { out.push_str(separator); }
        out.push_str(name.as_str());
    }
}
impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str { self.as_str() }
}
/// Formats names only at a display, diagnostic or authored persistence boundary.
pub fn join_path(path: &[Symbol], separator: &str) -> String {
    let mut out = String::new();
    write_path(&mut out, path, separator);
    out
}
/// Capacity-based lower bound: hash-table control bytes and allocator metadata are excluded.
pub fn retained_bytes() -> usize {
    let symbols = SYMBOLS.read().unwrap_or_else(|e| e.into_inner());
    symbols.names.capacity() * std::mem::size_of::<&str>()
        + symbols.by_name.capacity() * std::mem::size_of::<(&str, Symbol)>()
        + symbols.names.iter().map(|name| name.len()).sum::<usize>()
}

/// Attributes the process-owned table once across all retained value/arena roots.
pub fn attribute(memory: &mut crate::syntax::Memory) {
    memory.allocation(&*SYMBOLS as *const RwLock<Symbols>, retained_bytes());
}

pub fn valid_name(name: &str) -> bool {
    let mut bytes = name.bytes();
    matches!(bytes.next(), Some(b'a'..=b'z'))
        && bytes.all(|byte| matches!(byte, b'a'..=b'z' | b'0'..=b'9' | b'_'))
}

pub fn reserved(name: &str) -> bool {
    matches!(name, "t" | "pi" | "nil" | "true" | "false" | "let" | "let*" | "values" | "graph" | "defgraph" | "+" | "-" | "*" | "/" | "pow" | "min" | "max" | "sin" | "cos" | "abs" | "floor" | "sqrt")
}
#[cfg(test)]
mod tests {
    use super::{reserved, valid_name, intern, find, Symbol, write_path, retained_bytes};

    #[test]
    fn process_symbols_preserve_names_and_share_parallel_insertions() {
        let z = intern("z2_process_z");
        let a = intern("z2_process_a");
        assert!(a < z, "visible sorting follows text, not insertion IDs");
        assert_eq!(std::mem::size_of::<Symbol>(), 4);
        let mut path = String::new();
        write_path(&mut path, &[z, a], "/");
        assert_eq!(path, "z2_process_z/z2_process_a");
        let ids = std::thread::scope(|scope| {
            let threads: Vec<_> = (0..8).map(|_| scope.spawn(|| intern("z2_same_parallel_symbol"))).collect();
            threads.into_iter().map(|thread| thread.join().unwrap()).collect::<Vec<_>>()
        });
        assert!(ids.iter().all(|id| *id == ids[0]));
        assert_eq!(ids[0].as_str(), "z2_same_parallel_symbol");
        let before = retained_bytes();
        for _ in 0..1024 {
            assert_eq!(intern("z2_process_a"), a);
            assert_eq!(find("z2_never_interned_query"), None);
        }
        assert_eq!(retained_bytes(), before);
    }

    #[test]
    fn accepts_only_lowercase_unqualified_names() {
        assert!(valid_name("uv_sphere"));
        for name in ["", "9a", "A", "a-b", "a.b", "a/b"] {
            assert!(!valid_name(name));
        }
        assert!(reserved("let*"));
        assert!(!reserved("uv_sphere"));
    }
}
