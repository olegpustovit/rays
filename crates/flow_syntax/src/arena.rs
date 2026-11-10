use crate::syntax::{Id, QuoteKind, Trivia};
use rays_diagnostic::Span;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, atomic::{AtomicU64, Ordering}};

static NEXT_OWNER: AtomicU64 = AtomicU64::new(1);

pub fn fresh_owner_id() -> u64 {
    NEXT_OWNER.fetch_update(Ordering::Relaxed, Ordering::Relaxed, |id| id.checked_add(1))
        .expect("arena identity space exhausted")
}

/// Shared word hasher for internal name/id maps: keys come from our own arenas, so HashDoS
/// resistance buys nothing and SipHash showed up in the checker profile.
#[derive(Default, Clone, Copy)]
pub struct FastHasher(u64);
impl std::hash::Hasher for FastHasher {
    fn finish(&self) -> u64 { self.0 }
    fn write(&mut self, bytes: &[u8]) {
        self.0 = rays_math::hash::bytes(self.0, bytes);
    }
    fn write_u8(&mut self, n: u8) { self.write_u64(n.into()) }
    fn write_u32(&mut self, n: u32) { self.write_u64(n.into()) }
    fn write_usize(&mut self, n: usize) { self.write_u64(n as u64) }
    fn write_u64(&mut self, n: u64) { self.0 = rays_math::hash::word(self.0, n); }
}
pub type FastMap<K, V> = HashMap<K, V, std::hash::BuildHasherDefault<FastHasher>>;

fn index(value: usize) -> u32 { u32::try_from(value).expect("arena exceeds u32 storage") }

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NameId(pub u32);
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SyntaxId(pub u32);
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RangeId(pub u32);
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BytesId(pub u32);

/// Only a building arena owns a name lookup map. Frozen arenas keep the strings once.
#[derive(Default)]
pub struct NamesBuilder {
    names: Vec<String>,
    lookup: FastMap<String, NameId>,
}
impl NamesBuilder {
    pub fn intern(&mut self, name: &str) -> NameId {
        if let Some(&id) = self.lookup.get(name) { return id; }
        let id = NameId(index(self.names.len()));
        self.names.push(name.to_owned());
        self.lookup.insert(name.to_owned(), id);
        id
    }
    pub fn get(&self, id: NameId) -> &str { &self.names[id.0 as usize] }
    pub fn freeze(self) -> Vec<String> { self.names }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Node {
    Sym(NameId), Kw(NameId), Num(NameId), Str(BytesId),
    List(RangeId), Vec(RangeId), Map(RangeId), Quote(QuoteKind, SyntaxId),
}

/// Construction-only payloads; child IDs always belong to the target builder.
pub enum Draft {
    Sym(String), Kw(String), Num(String), Str(Vec<u8>),
    List(Vec<SyntaxId>), Vec(Vec<SyntaxId>), Map(Vec<SyntaxId>), Quote(QuoteKind, SyntaxId),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SyntaxRow {
    pub id: Id,
    pub node: Node,
    pub span: Span,
    trivia: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Range { start: u32, len: u32 }

/// Rows and edges contain IDs only; dropping a document never recursively drops its tree.
pub struct SyntaxArena {
    owner: u64,
    rows: Vec<SyntaxRow>,
    edges: Vec<SyntaxId>,
    ranges: Vec<Range>,
    names: Vec<String>,
    bytes: Vec<Arc<[u8]>>,
    trivia: Vec<Trivia>,
}

#[derive(Clone)]
pub struct Syntax {
    arena: Arc<SyntaxArena>,
    row: SyntaxId,
}

impl std::ops::Deref for Syntax {
    type Target = SyntaxRow;
    fn deref(&self) -> &SyntaxRow { self.arena.row(self.row) }
}
impl Syntax {
    pub fn arena(&self) -> &Arc<SyntaxArena> { &self.arena }
    pub fn row_id(&self) -> SyntaxId { self.row }
    pub fn identity(&self) -> (u64, u32) { (self.arena.owner, self.row.0) }
    pub fn child(&self, row: SyntaxId) -> Syntax { self.arena.handle(row) }
    pub fn name(&self, name: NameId) -> &str { self.arena.name(name) }
    pub fn bytes(&self, bytes: BytesId) -> &[u8] { self.arena.bytes(bytes) }
    pub fn shared_bytes(&self, bytes: BytesId) -> Arc<[u8]> { self.arena.shared_bytes(bytes) }
    pub fn range(&self, range: RangeId) -> &[SyntaxId] { self.arena.range(range) }
    pub fn children(&self) -> &[SyntaxId] { self.arena.children(self.row) }
    pub fn trivia(&self) -> Option<&Trivia> { self.arena.trivia(self.row) }
    pub fn notes(&self) -> &[String] { self.trivia().map_or(&[], |trivia| &trivia.notes) }
    pub fn meta(&self) -> &[String] { self.trivia().map_or(&[], |trivia| &trivia.meta) }
    pub fn tail(&self) -> &[String] { self.trivia().map_or(&[], |trivia| &trivia.tail) }
    pub fn debug_node(&self) -> impl std::fmt::Debug { DebugNode(self.clone()) }
}

impl SyntaxArena {
    pub fn row(&self, id: SyntaxId) -> &SyntaxRow { &self.rows[id.0 as usize] }
    pub fn rows(&self) -> &[SyntaxRow] { &self.rows }
    pub fn name(&self, id: NameId) -> &str { &self.names[id.0 as usize] }
    pub fn bytes(&self, id: BytesId) -> &[u8] { &self.bytes[id.0 as usize] }
    pub fn shared_bytes(&self, id: BytesId) -> Arc<[u8]> { self.bytes[id.0 as usize].clone() }
    pub fn range(&self, id: RangeId) -> &[SyntaxId] {
        let range = self.ranges[id.0 as usize];
        &self.edges[range.start as usize..range.start as usize + range.len as usize]
    }
    pub fn children(&self, id: SyntaxId) -> &[SyntaxId] {
        match &self.row(id).node {
            Node::List(range) | Node::Vec(range) | Node::Map(range) => self.range(*range),
            Node::Quote(_, child) => std::slice::from_ref(child),
            _ => &[],
        }
    }
    pub fn trivia(&self, id: SyntaxId) -> Option<&Trivia> {
        let trivia = self.row(id).trivia;
        (trivia != u32::MAX).then(|| &self.trivia[trivia as usize])
    }
    pub fn notes(&self, id: SyntaxId) -> &[String] { self.trivia(id).map_or(&[], |trivia| &trivia.notes) }
    pub fn meta(&self, id: SyntaxId) -> &[String] { self.trivia(id).map_or(&[], |trivia| &trivia.meta) }
    pub fn tail(&self, id: SyntaxId) -> &[String] { self.trivia(id).map_or(&[], |trivia| &trivia.tail) }
    pub fn handle(self: &Arc<Self>, id: SyntaxId) -> Syntax {
        self.row(id);
        Syntax { arena: self.clone(), row: id }
    }
    pub fn attribute(&self, memory: &mut crate::syntax::Memory) {
        if !memory.allocation(self as *const Self, std::mem::size_of::<Self>()) { return; }
        memory.allocation(self.rows.as_ptr(), self.rows.capacity() * std::mem::size_of::<SyntaxRow>());
        memory.allocation(self.edges.as_ptr(), self.edges.capacity() * std::mem::size_of::<SyntaxId>());
        memory.allocation(self.ranges.as_ptr(), self.ranges.capacity() * std::mem::size_of::<Range>());
        memory.allocation(self.names.as_ptr(), self.names.capacity() * std::mem::size_of::<String>());
        for name in &self.names { memory.allocation(name.as_ptr(), name.capacity()); }
        memory.allocation(self.bytes.as_ptr(), self.bytes.capacity() * std::mem::size_of::<Arc<[u8]>>());
        for bytes in &self.bytes { memory.allocation(bytes.as_ptr(), bytes.len() + 2 * std::mem::size_of::<usize>()); }
        memory.allocation(self.trivia.as_ptr(), self.trivia.capacity() * std::mem::size_of::<Trivia>());
        for trivia in &self.trivia {
            for lines in [&trivia.notes, &trivia.meta, &trivia.tail] {
                memory.allocation(lines.as_ptr(), lines.capacity() * std::mem::size_of::<String>());
                for line in lines { memory.allocation(line.as_ptr(), line.capacity()); }
            }
        }
    }
}

#[derive(Default)]
pub struct SyntaxBuilder {
    rows: Vec<SyntaxRow>,
    edges: Vec<SyntaxId>,
    ranges: Vec<Range>,
    names: NamesBuilder,
    bytes: Vec<Arc<[u8]>>,
    trivia: Vec<Trivia>,
    imports: FastMap<(u64, u32), SyntaxId>,
}
impl SyntaxBuilder {
    pub fn new() -> Self { Self::default() }
    pub fn row(&self, id: SyntaxId) -> &SyntaxRow { &self.rows[id.0 as usize] }
    pub fn name(&self, id: NameId) -> &str { self.names.get(id) }
    pub fn intern(&mut self, name: &str) -> NameId { self.names.intern(name) }
    pub fn imported(&self, form: &Syntax) -> Option<SyntaxId> { self.imports.get(&form.identity()).copied() }
    pub fn children(&self, id: SyntaxId) -> &[SyntaxId] {
        match &self.row(id).node {
            Node::List(range) | Node::Vec(range) | Node::Map(range) => self.range_ids(*range),
            Node::Quote(_, child) => std::slice::from_ref(child),
            _ => &[],
        }
    }
    pub fn raw_bytes(&self, id: BytesId) -> &[u8] { &self.bytes[id.0 as usize] }
    pub fn push_draft(&mut self, id: Id, draft: Draft, span: Span, trivia: Option<Trivia>) -> SyntaxId {
        let node = match draft {
            Draft::Sym(name) => Node::Sym(self.intern(&name)), Draft::Kw(name) => Node::Kw(self.intern(&name)),
            Draft::Num(name) => Node::Num(self.intern(&name)), Draft::Str(bytes) => {
                let id = BytesId(index(self.bytes.len())); self.bytes.push(bytes.into()); Node::Str(id)
            }
            Draft::List(children) => Node::List(self.range(&children)), Draft::Vec(children) => Node::Vec(self.range(&children)),
            Draft::Map(children) => Node::Map(self.range(&children)), Draft::Quote(kind, child) => Node::Quote(kind, child),
        };
        self.push(id, node, span, trivia)
    }
    pub fn with_node(&mut self, source: &Syntax, draft: Draft) -> SyntaxId {
        self.push_draft(source.id, draft, source.span.clone(), source.trivia().cloned())
    }
    pub fn with_location(&mut self, id: SyntaxId, authored: Id, span: Span) -> SyntaxId {
        self.push(authored, self.row(id).node, span, self.trivia(id).cloned())
    }
    pub fn with_trivia(&mut self, id: SyntaxId, trivia: Option<Trivia>) -> SyntaxId {
        let row = self.row(id);
        self.push(row.id, row.node, row.span.clone(), trivia)
    }
    pub fn renumber(&mut self, first: Id, root: SyntaxId) -> (SyntaxId, Id) {
        let mut next = first.checked_add(1).expect("authored form ID overflow");
        let mut pending = vec![(root, first, 0_usize)];
        let mut children = Vec::new();
        while let Some((source, authored, child_index)) = pending.last_mut() {
            let source_children = self.children(*source);
            if *child_index < source_children.len() {
                let child = source_children[*child_index];
                *child_index += 1;
                let authored = next;
                next = next.checked_add(1).expect("authored form ID overflow");
                pending.push((child, authored, 0));
                continue;
            }
            let count = source_children.len();
            let (source, authored) = (*source, *authored);
            let row = self.row(source).clone();
            let start = children.len() - count;
            let node = match row.node {
                Node::List(_) | Node::Vec(_) | Node::Map(_) => {
                    let range = self.range(&children[start..]);
                    match row.node { Node::List(_) => Node::List(range), Node::Vec(_) => Node::Vec(range), _ => Node::Map(range) }
                }
                Node::Quote(kind, _) => Node::Quote(kind, children[start]),
                node => node,
            };
            let id = self.push(authored, node, row.span, self.trivia(source).cloned());
            children.truncate(start);
            children.push(id);
            pending.pop();
        }
        (children[0], next)
    }
    pub fn attribute_args(&mut self, head: &str, args: &[SyntaxId]) -> Vec<SyntaxId> {
        self.normalize_args(AttributeArgs::new(head), args.to_vec())
    }
    pub fn attribute_args_of(&mut self, call: SyntaxId) -> Vec<SyntaxId> {
        let Node::List(range) = self.row(call).node else { return Vec::new(); };
        let args = self.range_ids(range);
        let Some(&first) = args.first() else { return Vec::new(); };
        let rules = match self.row(first).node { Node::Sym(name) => AttributeArgs::new(self.name(name)), _ => AttributeArgs::new("") };
        let args = args[1..].to_vec();
        self.normalize_args(rules, args)
    }
    fn normalize_args(&mut self, rules: AttributeArgs, mut normalized: Vec<SyntaxId>) -> Vec<SyntaxId> {
        let (mut index, mut position) = (0, 0);
        while rules.positional && index < normalized.len() {
            if index + 1 < normalized.len() && matches!(self.row(normalized[index]).node, Node::Kw(key) if AttributeArgs::named(self.name(key))) { index += 2; }
            else {
                if position == 1 {
                    if let Node::Kw(name) = self.row(normalized[index]).node { normalized[index] = self.stringify(normalized[index], name); }
                }
                position += 1;
                index += 1;
            }
        }
        let mut index = 0;
        while index + 1 < normalized.len() {
            if let (Node::Kw(kind), Node::Sym(name)) = (self.row(normalized[index]).node, self.row(normalized[index + 1]).node) {
                if rules.enum_value(self.name(kind), self.name(name)) {
                    normalized[index + 1] = self.stringify(normalized[index + 1], name);
                    index += 2;
                    continue;
                }
            }
            index += 1;
        }
        normalized
    }
    fn stringify(&mut self, id: SyntaxId, name: NameId) -> SyntaxId {
        let row = self.row(id).clone();
        let bytes = self.name(name).as_bytes().to_vec();
        self.push_draft(row.id, Draft::Str(bytes), row.span, self.trivia(id).cloned())
    }
    pub fn range_ids(&self, id: RangeId) -> &[SyntaxId] {
        let range = self.ranges[id.0 as usize];
        &self.edges[range.start as usize..range.start as usize + range.len as usize]
    }
    pub fn trivia(&self, id: SyntaxId) -> Option<&Trivia> {
        let index = self.row(id).trivia;
        (index != u32::MAX).then(|| &self.trivia[index as usize])
    }
    pub fn trivia_mut(&mut self, id: SyntaxId) -> &mut Trivia {
        if self.row(id).trivia == u32::MAX {
            let index = index(self.trivia.len());
            assert_ne!(index, u32::MAX, "syntax trivia limit");
            self.trivia.push(Trivia::default());
            self.rows[id.0 as usize].trivia = index;
        }
        &mut self.trivia[self.rows[id.0 as usize].trivia as usize]
    }
    pub fn set_location(&mut self, id: SyntaxId, authored: Id, span: Span) {
        let row = &mut self.rows[id.0 as usize];
        row.id = authored;
        row.span = span;
    }
    pub fn bytes(&mut self, bytes: &[u8]) -> BytesId { self.shared_bytes(bytes.into()) }
    fn shared_bytes(&mut self, bytes: Arc<[u8]>) -> BytesId {
        let id = BytesId(index(self.bytes.len()));
        self.bytes.push(bytes);
        id
    }
    pub fn range(&mut self, children: &[SyntaxId]) -> RangeId {
        assert!(children.iter().all(|id| (id.0 as usize) < self.rows.len()), "syntax edges must refer to earlier rows");
        let range = Range { start: index(self.edges.len()), len: index(children.len()) };
        index(self.edges.len().checked_add(children.len()).expect("syntax edge overflow"));
        let id = RangeId(index(self.ranges.len()));
        self.edges.extend_from_slice(children);
        self.ranges.push(range);
        id
    }
    pub fn push(&mut self, id: Id, node: Node, span: Span, trivia: Option<Trivia>) -> SyntaxId {
        match node {
            Node::Sym(name) | Node::Kw(name) | Node::Num(name) => { self.names.get(name); }
            Node::Str(bytes) => { let _ = &self.bytes[bytes.0 as usize]; }
            Node::List(range) | Node::Vec(range) | Node::Map(range) => { let _ = &self.ranges[range.0 as usize]; }
            Node::Quote(_, child) => { self.row(child); }
        }
        let trivia = match trivia {
            Some(trivia) if !trivia.notes.is_empty() || !trivia.meta.is_empty() || !trivia.tail.is_empty() => {
                let id = index(self.trivia.len());
                assert_ne!(id, u32::MAX, "syntax trivia limit");
                self.trivia.push(trivia);
                id
            }
            _ => u32::MAX,
        };
        let row = SyntaxId(index(self.rows.len()));
        self.rows.push(SyntaxRow { id, node, span, trivia });
        row
    }
    /// Import reachable rows once per builder, never retain a source document in the frozen arena.
    pub fn import(&mut self, syntax: &Syntax) -> SyntaxId {
        if let Some(id) = self.imported(syntax) { return id; }
        let source = &syntax.arena;
        let mut pending = vec![(syntax.row, false)];
        while let Some((id, ready)) = pending.pop() {
            let key = (source.owner, id.0);
            if self.imports.contains_key(&key) { continue; }
            if !ready {
                pending.push((id, true));
                pending.extend(source.children(id).iter().rev().map(|child| (*child, false)));
                continue;
            }
            let row = source.row(id);
            let node = match row.node {
                Node::Sym(name) => Node::Sym(self.intern(source.name(name))),
                Node::Kw(name) => Node::Kw(self.intern(source.name(name))),
                Node::Num(name) => Node::Num(self.intern(source.name(name))),
                Node::Str(bytes) => Node::Str(self.shared_bytes(source.shared_bytes(bytes))),
                Node::List(range) | Node::Vec(range) | Node::Map(range) => {
                    let children: Vec<SyntaxId> = source.range(range).iter().map(|child| self.imports[&(source.owner, child.0)]).collect();
                    let range = self.range(&children);
                    match row.node { Node::List(_) => Node::List(range), Node::Vec(_) => Node::Vec(range), _ => Node::Map(range) }
                }
                Node::Quote(kind, child) => Node::Quote(kind, self.imports[&(source.owner, child.0)]),
            };
            let imported = self.push(row.id, node, row.span.clone(), source.trivia(id).cloned());
            self.imports.insert(key, imported);
        }
        self.imports[&syntax.identity()]
    }
    pub fn freeze(mut self) -> Arc<SyntaxArena> {
        self.rows.shrink_to_fit();
        self.edges.shrink_to_fit();
        self.ranges.shrink_to_fit();
        Arc::new(SyntaxArena { owner: fresh_owner_id(), rows: self.rows, edges: self.edges,
            ranges: self.ranges, names: self.names.freeze(), bytes: self.bytes, trivia: self.trivia })
    }
}

// Debug deliberately prints authored IDs and content, never storage IDs or arena ownership.
impl std::fmt::Debug for Syntax {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Syntax").field("id", &self.id).field("node", &DebugNode(self.clone()))
            .field("span", &self.span).field("notes", &self.notes()).field("meta", &self.meta())
            .field("tail", &self.tail()).finish()
    }
}
struct DebugNode(Syntax);
impl std::fmt::Debug for DebugNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.node {
            Node::Sym(name) => f.debug_tuple("Sym").field(&self.0.name(name)).finish(),
            Node::Kw(name) => f.debug_tuple("Kw").field(&self.0.name(name)).finish(),
            Node::Num(name) => f.debug_tuple("Num").field(&self.0.name(name)).finish(),
            Node::Str(bytes) => f.debug_tuple("Str").field(&self.0.bytes(bytes)).finish(),
            Node::List(_) | Node::Vec(_) | Node::Map(_) => {
                let name = match self.0.node { Node::List(_) => "List", Node::Vec(_) => "Vec", _ => "Map" };
                f.debug_tuple(name).field(&DebugChildren(self.0.clone())).finish()
            }
            Node::Quote(kind, child) => f.debug_tuple("Quote").field(&kind).field(&self.0.child(child)).finish(),
        }
    }
}
struct DebugChildren(Syntax);
impl std::fmt::Debug for DebugChildren {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.0.children().iter().map(|&id| self.0.child(id))).finish()
    }
}
/// Standalone atoms/empty containers only; build composite forms in one shared SyntaxBuilder.
pub fn make(draft: Draft) -> Syntax { make_with(draft, Vec::new(), Vec::new()) }
pub fn make_with(draft: Draft, notes: Vec<String>, meta: Vec<String>) -> Syntax {
    let mut builder = SyntaxBuilder::new();
    let row = builder.push_draft(0, draft, Span { start: 0, finish: 0 }, Some(Trivia { notes, meta, tail: Vec::new() }));
    builder.freeze().handle(row)
}

impl PartialEq for Syntax {
    fn eq(&self, other: &Self) -> bool { compare(self, other, true) }
}
pub fn equal(a: &Syntax, b: &Syntax) -> bool { compare(a, b, false) }
pub fn equal_node(a: &Syntax, b: &Syntax) -> bool {
    match (a.node, b.node) {
        (Node::Sym(x), Node::Sym(y)) | (Node::Kw(x), Node::Kw(y)) | (Node::Num(x), Node::Num(y)) => a.name(x) == b.name(y),
        (Node::Str(x), Node::Str(y)) => a.bytes(x) == b.bytes(y),
        (Node::List(x), Node::List(y)) | (Node::Vec(x), Node::Vec(y)) | (Node::Map(x), Node::Map(y)) => {
            let (x, y) = (a.range(x), b.range(y));
            x.len() == y.len() && x.iter().zip(y).all(|(&x, &y)| a.child(x) == b.child(y))
        }
        (Node::Quote(x, child), Node::Quote(y, other)) => x == y && a.child(child) == b.child(other),
        _ => false,
    }
}
fn compare(a: &Syntax, b: &Syntax, location: bool) -> bool {
    if a.identity() == b.identity() { return true; }
    compare_roots(&a.arena, &b.arena, &[(a.row, b.row)], location)
}

/// Exact comparison of several source roots, sharing one visited-row set.
pub fn equal_roots(a: &SyntaxArena, b: &SyntaxArena, roots: &[(SyntaxId, SyntaxId)]) -> bool {
    compare_roots(a, b, roots, true)
}

fn compare_roots(a: &SyntaxArena, b: &SyntaxArena, roots: &[(SyntaxId, SyntaxId)], location: bool) -> bool {
    let mut pending = roots.to_vec();
    let mut seen = HashSet::new();
    while let Some((a_id, b_id)) = pending.pop() {
        if a.owner == b.owner && a_id == b_id { continue; }
        if !seen.insert((a_id, b_id)) { continue; }
        let (x, y) = (a.row(a_id), b.row(b_id));
        if location && (x.id != y.id || x.span != y.span) { return false; }
        if a.notes(a_id) != b.notes(b_id) || a.meta(a_id) != b.meta(b_id)
            || a.tail(a_id) != b.tail(b_id) { return false; }
        match (x.node, y.node) {
            (Node::Sym(x), Node::Sym(y)) | (Node::Kw(x), Node::Kw(y)) | (Node::Num(x), Node::Num(y)) => if a.name(x) != b.name(y) { return false; },
            (Node::Str(x), Node::Str(y)) => if a.bytes(x) != b.bytes(y) { return false; },
            (Node::List(x), Node::List(y)) | (Node::Vec(x), Node::Vec(y)) | (Node::Map(x), Node::Map(y)) => {
                let (x, y) = (a.range(x), b.range(y));
                if x.len() != y.len() { return false; }
                pending.extend(x.iter().copied().zip(y.iter().copied()));
            }
            (Node::Quote(x, a), Node::Quote(y, b)) if x == y => pending.push((a, b)),
            _ => return false,
        }
    }
    true
}
impl Eq for Syntax {}

pub fn head(form: &Syntax) -> Option<&str> {
    let Node::List(range) = form.node else { return None; };
    let first = *form.range(range).first()?;
    match form.arena.row(first).node { Node::Sym(name) => Some(form.name(name)), _ => None }
}

/// Authored IDs remain preorder occurrence IDs, even when storage shares a child row.
pub fn renumber(first: Id, form: &Syntax) -> (Syntax, Id) {
    let source = &form.arena;
    let mut builder = SyntaxBuilder::new();
    let mut next = first.checked_add(1).expect("authored form ID overflow");
    let mut pending = vec![(form.row, first, 0_usize)];
    let mut children = Vec::new();
    while let Some((source_id, authored, child_index)) = pending.last_mut() {
        let source_children = source.children(*source_id);
        if *child_index < source_children.len() {
            let source_id = source_children[*child_index];
            *child_index += 1;
            let authored = next;
            next = next.checked_add(1).expect("authored form ID overflow");
            pending.push((source_id, authored, 0));
            continue;
        }
        let (source_id, authored) = (*source_id, *authored);
        let row = source.row(source_id);
        let start = children.len() - source_children.len();
        let node = match row.node {
            Node::Sym(name) => Node::Sym(builder.intern(source.name(name))),
            Node::Kw(name) => Node::Kw(builder.intern(source.name(name))),
            Node::Num(name) => Node::Num(builder.intern(source.name(name))),
            Node::Str(bytes) => Node::Str(builder.shared_bytes(source.shared_bytes(bytes))),
            Node::List(_) | Node::Vec(_) | Node::Map(_) => {
                let range = builder.range(&children[start..]);
                match row.node { Node::List(_) => Node::List(range), Node::Vec(_) => Node::Vec(range), _ => Node::Map(range) }
            }
            Node::Quote(kind, _) => Node::Quote(kind, children[start]),
        };
        let row = builder.push(authored, node, row.span.clone(), source.trivia(source_id).cloned());
        children.truncate(start);
        children.push(row);
        pending.pop();
    }
    let root = children[0];
    (builder.freeze().handle(root), next)
}

struct AttributeArgs {
    positional: bool, attribute: bool, group: bool, attribute_metadata: bool, group_metadata: bool,
    group_head: bool, swap_head: bool, type_names: &'static [&'static str],
}
impl AttributeArgs {
    fn new(head: &str) -> Self {
        let attribute = ["sop/attr", "sop/attr_checked", "sop/with_attr", "sop/has_attr", "sop/attr_type", "sop/attr_names", "sop/attr_entries", "attr", "attr_checked", "with_attr", "has_attr"].contains(&head);
        Self { positional: attribute || head == "sop/count", attribute,
            group: ["sop/group", "sop/group_names", "sop/group_order", "sop/with_group"].contains(&head),
            attribute_metadata: ["sop/rename_attr", "sop/swap_attr"].contains(&head),
            group_metadata: ["sop/rename_group", "sop/delete_group"].contains(&head),
            group_head: head == "sop/group", swap_head: head == "sop/swap_attr",
            type_names: if attribute { &["float", "int", "vec2", "vec3", "vec4", "text", "float_array", "int_array"] }
                else if head == "array/max_length" { &["float", "int", "text", "vec2", "vec3", "vec4"] }
                else if head == "array/range" { &["float", "int"] } else { &[] } }
    }
    fn named(kind: &str) -> bool { ["owner", "type", "selection", "default"].contains(&kind) }
    fn enum_value(&self, kind: &str, name: &str) -> bool {
        (self.attribute && kind == "owner" && ["point", "vertex", "primitive", "detail"].contains(&name))
            || (self.group && kind == "owner" && ["point", "vertex", "primitive", "edge"].contains(&name))
            || (self.group_head && kind == "as_owner" && name == "point")
            || (self.attribute_metadata && kind == "owner" && ["point", "vertex", "primitive", "detail", "any"].contains(&name))
            || (self.group_metadata && kind == "owner" && ["point", "vertex", "primitive", "edge", "any"].contains(&name))
            || ((self.attribute_metadata || self.group_metadata) && kind == "conflict" && ["skip", "error", "overwrite", "union"].contains(&name))
            || (self.swap_head && kind == "method" && ["swap", "move", "copy"].contains(&name))
            || (kind == "type" && self.type_names.contains(&name))
    }
}

/// Borrowed logical normalization: callers enumerate to distinguish repeated row occurrences.
pub fn attribute_args_view<'a>(head: &str, arena: &'a SyntaxArena, args: &'a [SyntaxId]) -> impl Iterator<Item = (SyntaxId, Option<&'a [u8]>)> + 'a {
    let rules = AttributeArgs::new(head);
    let (mut index, mut position, mut name_index) = (0, 0, None);
    while rules.positional && index < args.len() {
        if index + 1 < args.len() && matches!(arena.row(args[index]).node, Node::Kw(key) if AttributeArgs::named(arena.name(key))) { index += 2; }
        else {
            if position == 1 { name_index = Some(index); break; }
            position += 1;
            index += 1;
        }
    }
    let mut pending = None;
    args.iter().copied().enumerate().map(move |(index, id)| {
        if let Some(text) = pending.take() { return (id, Some(text)); }
        if Some(index) == name_index {
            if let Node::Kw(name) = arena.row(id).node { return (id, Some(arena.name(name).as_bytes())); }
        }
        if Some(index) != name_index && index + 1 < args.len() {
            if let (Node::Kw(kind), Node::Sym(name)) = (arena.row(id).node, arena.row(args[index + 1]).node) {
                if rules.enum_value(arena.name(kind), arena.name(name)) { pending = Some(arena.name(name).as_bytes()); }
            }
        }
        (id, None)
    })
}

/// Normalize only changed atoms into one small owner; never re-import unchanged argument trees.
pub fn attribute_args(head: &str, args: &[Syntax]) -> Vec<Syntax> {
    let rules = AttributeArgs::new(head);
    let mut builder = SyntaxBuilder::new();
    let mut replacements = HashMap::new();
    let (mut index, mut position) = (0, 0);
    while rules.positional && index < args.len() {
        let form = &args[index];
        if index + 1 < args.len() && matches!(form.node, Node::Kw(key) if AttributeArgs::named(form.name(key))) { index += 2; }
        else {
            if position == 1 {
                if let Node::Kw(name) = form.node { replacements.insert(index, string_row(&mut builder, form, form.name(name).as_bytes())); }
            }
            position += 1;
            index += 1;
        }
    }
    let mut index = 0;
    while index + 1 < args.len() {
        let (key, value) = (&args[index], &args[index + 1]);
        if !replacements.contains_key(&index) && !replacements.contains_key(&(index + 1)) {
            if let (Node::Kw(kind), Node::Sym(name)) = (key.node, value.node) {
                let (kind, name) = (key.name(kind), value.name(name));
                let enum_value = rules.enum_value(kind, name);
                if enum_value { replacements.insert(index + 1, string_row(&mut builder, value, name.as_bytes())); index += 2; continue; }
            }
        }
        index += 1;
    }
    let mut normalized = args.to_vec();
    if !replacements.is_empty() {
        let arena = builder.freeze();
        for (index, row) in replacements { normalized[index] = arena.handle(row); }
    }
    normalized
}

fn string_row(builder: &mut SyntaxBuilder, form: &Syntax, bytes: &[u8]) -> SyntaxId {
    let bytes = builder.bytes(bytes);
    builder.push(form.id, Node::Str(bytes), form.span.clone(), form.trivia().cloned())
}


const _: () = assert!(std::mem::size_of::<Syntax>() == 16);
const _: () = assert!(std::mem::size_of::<Node>() == 8);
const _: () = assert!(std::mem::size_of::<SyntaxRow>() == 32);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn packed_rows_import_deduplicate_and_release_the_source() {
        let mut source = SyntaxBuilder::new();
        let name = source.intern("same");
        assert_eq!(name, source.intern("same"));
        let atom = source.push(900, Node::Sym(name), Span { start: 2, finish: 6 }, None);
        let range = source.range(&[atom, atom]);
        let root = source.push(-7, Node::List(range), Span { start: 1, finish: 7 }, None);
        let source = source.freeze();
        let weak = Arc::downgrade(&source);
        let root = source.handle(root);
        let mut target = SyntaxBuilder::new();
        let imported = target.import(&root);
        assert_eq!(imported, target.import(&root));
        let target = target.freeze();
        assert_eq!(target.rows().len(), 2);
        assert_eq!(target.handle(imported), root);
        assert_eq!(format!("{:?}", target.handle(imported)), format!("{root:?}"));
        let (numbered, next) = renumber(100, &root);
        assert_eq!(next, 103);
        assert_eq!(numbered.id, 100);
        assert_eq!(numbered.child(numbered.children()[0]).id, 101);
        assert_eq!(numbered.child(numbered.children()[1]).id, 102);
        assert!(equal(&numbered, &root));
        assert_ne!(numbered, root);
        drop(root);
        drop(source);
        assert!(weak.upgrade().is_none(), "imported graph must not retain its source document");
        assert_eq!(target.handle(imported).id, -7);
        let mut memory = crate::syntax::Memory::default();
        target.attribute(&mut memory);
        let bytes = memory.bytes();
        target.attribute(&mut memory);
        assert_eq!(bytes, memory.bytes());
    }

    #[test]
    fn deep_drop_is_iterative_and_identity_survives_handle_moves() {
        let mut builder = SyntaxBuilder::new();
        let name = builder.intern("x");
        let mut root = builder.push(0, Node::Sym(name), Span { start: 0, finish: 1 }, None);
        for id in 1..100_000 { root = builder.push(id, Node::Quote(QuoteKind::Plain, root), Span { start: 0, finish: 1 }, None); }
        let arena = builder.freeze();
        let syntax = arena.handle(root);
        let identity = syntax.identity();
        let moved = vec![syntax].pop().unwrap();
        assert_eq!(identity, moved.identity());
        let (numbered, next) = renumber(10, &moved);
        assert_eq!(next, 100_010);
        assert!(equal(&numbered, &moved));
        drop(numbered);
        let weak = Arc::downgrade(&arena);
        drop(arena);
        drop(moved);
        assert!(weak.upgrade().is_none());
    }
}
