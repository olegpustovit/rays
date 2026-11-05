use crate::workspace::{Context, DiagnosticPolicy, HofKind, LoopKind, Pattern};
use flow_syntax::{arena::{self, NamesBuilder, Syntax, SyntaxArena, SyntaxBuilder}, ty::Ty};
pub use flow_syntax::arena::{NameId, SyntaxId};
use std::collections::HashMap;
use std::sync::Arc;

fn index(value: usize) -> u32 { u32::try_from(value).expect("term arena exceeds u32 storage") }

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TermId(pub u32);
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeId(pub u32);
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct PathId(std::num::NonZeroU32);
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct PatternId(pub u32);
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RangeId(pub u32);
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Range { start: u32, len: u32 }

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Node {
    Lit(u32), Text(u32), Nil, Time, Vec(RangeId), RefBinding(u32),
    Call(u32), Op(u32), CallFn(u32), FnRef(NameId), GraphRef(u32), Let(u32),
    State(u32), Loop(u32), WithError(u32), ErrorRows(u32), If(u32), Cond(u32), Case(u32),
    Fn(u32), Hof(u32), ListLit(RangeId), Record(RangeId), Get(u32), Assoc(u32),
    Str(RangeId), ListOp(u32), Bypass(TermId), Expanded(u32),
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal { Bool(bool), Int(i64), Float(f64), Text(u32), Choice(NameId) }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PatternNode { Name(NameId), Seq(RangeId), Keys(RangeId) }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TermRow {
    pub ty: TypeId,
    pub path: Option<PathId>,
    pub form: SyntaxId,
    pub node: Node,
    children: RangeId,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Field { pub name: NameId, pub term: TermId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Binding { pub pattern: PatternId, pub term: TermId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Arm { pub test: TermId, pub body: TermId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CaseArm { pub form: SyntaxId, pub body: TermId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Parameter { pub pattern: PatternId, pub ty: Option<TypeId> }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RefBinding { pub name: NameId, pub fields: RangeId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Call { pub kind: NameId, pub ctx: Context, pub args: RangeId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Op { pub op: NameId, pub args: RangeId, pub skip: RangeId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CallFn { pub name: NameId, pub args: RangeId, pub body: Option<TermId> }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GraphRef { pub graph: NameId, pub inputs: RangeId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Let { pub bindings: RangeId, pub body: TermId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct State { pub binder: PatternId, pub init: TermId, pub step: TermId, pub zone: PathId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Loop { pub kind: LoopKind, pub accs: RangeId, pub clauses: RangeId, pub skip: RangeId, pub body: TermId, pub zone: PathId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Pair { pub first: TermId, pub second: TermId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ErrorRows { pub rows: TermId, pub policy: DiagnosticPolicy, pub mapper: TermId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct If { pub test: TermId, pub yes: TermId, pub no: TermId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Cond { pub arms: RangeId, pub default: TermId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Case { pub value: TermId, pub arms: RangeId, pub default: TermId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Function { pub params: RangeId, pub body: TermId, pub zone: PathId, pub capture: Option<TermId> }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Hof { pub kind: HofKind, pub args: RangeId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Get { pub value: TermId, pub name: NameId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Assoc { pub value: TermId, pub fields: RangeId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ListOp { pub name: NameId, pub args: RangeId }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Expanded { pub name: NameId, pub body: TermId }

/// Construction-only values use local child IDs; no node imports its own recursive subtree.
pub enum Draft {
    Lit(param::Value), Text(Vec<u8>), Nil, Time, Vec(Vec<TermId>), RefBinding(String, Vec<String>),
    Call { kind: String, ctx: Context, args: Vec<(String, TermId)> },
    Op { op: String, args: Vec<(String, TermId)>, skip: Vec<Vec<usize>> },
    CallFn { fn_name: String, args: Vec<TermId>, body: Option<TermId> }, FnRef(String),
    GraphRef { graph: String, inputs: Vec<(String, TermId)> }, Let(Vec<(Pattern, TermId)>, TermId),
    State { binder: Pattern, init: TermId, step: TermId, zone: Vec<String> },
    Loop { kind: LoopKind, accs: Vec<(Pattern, TermId)>, clauses: Vec<(Pattern, TermId)>, skip: Vec<Vec<usize>>, body: TermId, zone: Vec<String> },
    WithError(TermId, TermId), ErrorRows { rows: TermId, policy: DiagnosticPolicy, mapper: TermId },
    If(TermId, TermId, TermId), Cond(Vec<(TermId, TermId)>, TermId), Case(TermId, Vec<(SyntaxId, TermId)>, TermId),
    Fn { params: Vec<(Pattern, Option<Ty>)>, body: TermId, zone: Vec<String>, capture: Option<TermId> },
    Hof(HofKind, Vec<TermId>), ListLit(Vec<TermId>), Record(Vec<(String, TermId)>), Get(TermId, String),
    Assoc(TermId, Vec<(String, TermId)>), Str(Vec<TermId>), ListOp(String, Vec<TermId>), Bypass(TermId),
    Expanded { macro_name: String, body: TermId },
}

#[derive(Default)]
struct Tables {
    rows: Vec<TermRow>, ranges: Vec<Range>, terms: Vec<TermId>, names: Vec<NameId>,
    fields: Vec<Field>, bindings: Vec<Binding>, arms: Vec<Arm>, case_arms: Vec<CaseArm>, params: Vec<Parameter>,
    patterns: Vec<PatternNode>, pattern_edges: Vec<PatternId>, paths: Vec<RangeId>, types: Vec<Ty>,
    bytes: Vec<Vec<u8>>, literals: Vec<Literal>, skips: Vec<RangeId>, indices: Vec<usize>,
    refs: Vec<RefBinding>, calls: Vec<Call>, ops: Vec<Op>, call_fns: Vec<CallFn>, graphs: Vec<GraphRef>,
    lets: Vec<Let>, states: Vec<State>, loops: Vec<Loop>, pairs: Vec<Pair>, errors: Vec<ErrorRows>,
    ifs: Vec<If>, conds: Vec<Cond>, cases: Vec<Case>, functions: Vec<Function>, hofs: Vec<Hof>,
    gets: Vec<Get>, assocs: Vec<Assoc>, list_ops: Vec<ListOp>, expanded: Vec<Expanded>,
}

pub struct TermArena {
    owner: u64,
    tables: Tables,
    names: Vec<String>,
    syntax: Arc<SyntaxArena>,
}
#[derive(Clone)]
pub struct Term { arena: Arc<TermArena>, row: TermId }
impl std::ops::Deref for Term {
    type Target = TermRow;
    fn deref(&self) -> &TermRow { self.arena.row(self.row) }
}
impl Term {
    pub fn arena(&self) -> &Arc<TermArena> { &self.arena }
    pub fn row_id(&self) -> TermId { self.row }
    pub fn identity(&self) -> (u64, u32) { (self.arena.owner, self.row.0) }
    pub fn child(&self, row: TermId) -> Term { self.arena.handle(row) }
    pub fn ty(&self) -> &Ty { self.arena.ty(self.ty) }
    pub fn path(&self) -> Option<&[NameId]> { self.path.map(|path| self.arena.path(path)) }
    pub fn form(&self) -> Syntax { self.arena.syntax.handle(self.form) }
    pub fn name(&self, id: NameId) -> &str { self.arena.name(id) }
    pub fn children(&self) -> &[TermId] { self.arena.children(self.row) }
    pub fn debug_node(&self) -> impl std::fmt::Debug { DebugPart::new(&self.arena, DebugKind::Node(self.node)) }
}

macro_rules! payload_accessors {
    ($($method:ident : $table:ident -> $ty:ty),* $(,)?) => { $(
        pub fn $method(&self, id: u32) -> &$ty { &self.tables.$table[id as usize] }
    )* };
}
impl TermArena {
    pub fn handle(self: &Arc<Self>, id: TermId) -> Term { self.row(id); Term { arena: self.clone(), row: id } }
    pub fn row(&self, id: TermId) -> &TermRow { &self.tables.rows[id.0 as usize] }
    pub fn rows(&self) -> &[TermRow] { &self.tables.rows }
    pub fn ty(&self, id: TypeId) -> &Ty { &self.tables.types[id.0 as usize] }
    pub fn name(&self, id: NameId) -> &str { &self.names[id.0 as usize] }
    pub fn syntax(&self) -> &Arc<SyntaxArena> { &self.syntax }
    fn bounds(&self, id: RangeId) -> std::ops::Range<usize> {
        let range = self.tables.ranges[id.0 as usize];
        range.start as usize..range.start as usize + range.len as usize
    }
    pub fn term_ids(&self, id: RangeId) -> &[TermId] { &self.tables.terms[self.bounds(id)] }
    pub fn name_ids(&self, id: RangeId) -> &[NameId] { &self.tables.names[self.bounds(id)] }
    pub fn fields(&self, id: RangeId) -> &[Field] { &self.tables.fields[self.bounds(id)] }
    pub fn bindings(&self, id: RangeId) -> &[Binding] { &self.tables.bindings[self.bounds(id)] }
    pub fn arms(&self, id: RangeId) -> &[Arm] { &self.tables.arms[self.bounds(id)] }
    pub fn case_arms(&self, id: RangeId) -> &[CaseArm] { &self.tables.case_arms[self.bounds(id)] }
    pub fn params(&self, id: RangeId) -> &[Parameter] { &self.tables.params[self.bounds(id)] }
    pub fn pattern(&self, id: PatternId) -> PatternNode { self.tables.patterns[id.0 as usize] }
    pub fn owned_pattern(&self, id: PatternId) -> Pattern { source_pattern(self, id) }
    pub fn pattern_ids(&self, id: RangeId) -> &[PatternId] { &self.tables.pattern_edges[self.bounds(id)] }
    pub fn path(&self, id: PathId) -> &[NameId] { self.name_ids(self.tables.paths[(id.0.get() - 1) as usize]) }
    pub fn skip(&self, id: RangeId) -> &[RangeId] { &self.tables.skips[self.bounds(id)] }
    pub fn indices(&self, id: RangeId) -> &[usize] { &self.tables.indices[self.bounds(id)] }
    pub fn bytes(&self, id: u32) -> &[u8] { &self.tables.bytes[id as usize] }
    pub fn literal(&self, id: u32) -> Literal { self.tables.literals[id as usize] }
    pub fn children(&self, id: TermId) -> &[TermId] { self.term_ids(self.row(id).children) }
    payload_accessors! {
        ref_binding: refs -> RefBinding, call: calls -> Call, op: ops -> Op, call_fn: call_fns -> CallFn,
        graph_ref: graphs -> GraphRef, let_node: lets -> Let, state: states -> State, loop_node: loops -> Loop,
        pair: pairs -> Pair, error_rows: errors -> ErrorRows, if_node: ifs -> If, cond: conds -> Cond,
        case: cases -> Case, function: functions -> Function, hof: hofs -> Hof, get: gets -> Get,
        assoc: assocs -> Assoc, list_op: list_ops -> ListOp, expanded: expanded -> Expanded,
    }
    pub fn attribute(&self, checked: &mut flow_syntax::syntax::Memory, syntax: &mut flow_syntax::syntax::Memory) {
        if !checked.allocation(self as *const Self, std::mem::size_of::<Self>()) { return; }
        macro_rules! buffers {
            ($($field:ident: $ty:ty),* $(,)?) => { $(
                checked.allocation(self.tables.$field.as_ptr(), self.tables.$field.capacity() * std::mem::size_of::<$ty>());
            )* };
        }
        buffers! {
            rows: TermRow, ranges: Range, terms: TermId, names: NameId, fields: Field, bindings: Binding,
            arms: Arm, case_arms: CaseArm, params: Parameter, patterns: PatternNode, pattern_edges: PatternId,
            paths: RangeId, types: Ty, bytes: Vec<u8>, literals: Literal, skips: RangeId, indices: usize,
            refs: RefBinding, calls: Call, ops: Op, call_fns: CallFn, graphs: GraphRef, lets: Let, states: State,
            loops: Loop, pairs: Pair, errors: ErrorRows, ifs: If, conds: Cond, cases: Case, functions: Function,
            hofs: Hof, gets: Get, assocs: Assoc, list_ops: ListOp, expanded: Expanded,
        }
        checked.allocation(self.names.as_ptr(), self.names.capacity() * std::mem::size_of::<String>());
        for name in &self.names { checked.allocation(name.as_ptr(), name.capacity()); }
        for bytes in &self.tables.bytes { checked.allocation(bytes.as_ptr(), bytes.capacity()); }
        for ty in &self.tables.types { attribute_type(checked, ty); }
        self.syntax.attribute(syntax);
    }
}

pub struct TermBuilder {
    tables: Tables,
    names: NamesBuilder,
    syntax: SyntaxBuilder,
    paths: HashMap<Vec<NameId>, PathId>,
    imports: HashMap<(u64, u32), TermId>,
    source_forms: HashMap<SyntaxId, Syntax>,
    scratch: Vec<TermId>,
}
impl Default for TermBuilder {
    fn default() -> Self {
        Self { tables: Tables { ranges: vec![Range { start: 0, len: 0 }], ..Tables::default() },
            names: NamesBuilder::default(), syntax: SyntaxBuilder::new(), paths: HashMap::new(),
            imports: HashMap::new(), source_forms: HashMap::new(), scratch: Vec::new() }
    }
}
impl TermBuilder {
    pub fn new() -> Self { Self::default() }
    pub fn row(&self, id: TermId) -> &TermRow { &self.tables.rows[id.0 as usize] }
    pub fn ty(&self, id: TermId) -> &Ty { &self.tables.types[self.row(id).ty.0 as usize] }
    pub fn intern(&mut self, name: &str) -> NameId { self.names.intern(name) }
    pub fn import_form(&mut self, form: &Syntax) -> SyntaxId {
        let id = self.syntax.import(form);
        self.source_forms.entry(id).or_insert_with(|| form.clone());
        id
    }
    pub fn source_form(&self, id: TermId) -> Option<&Syntax> { self.source_forms.get(&self.row(id).form) }
    pub fn imported(&self, term: &Term) -> Option<TermId> { self.imports.get(&term.identity()).copied() }
    pub fn syntax_builder(&mut self) -> &mut SyntaxBuilder { &mut self.syntax }
    pub fn syntax(&self) -> &SyntaxBuilder { &self.syntax }
    pub fn name(&self, id: NameId) -> &str { self.names.get(id) }
    pub fn type_by_id(&self, id: TypeId) -> &Ty { &self.tables.types[id.0 as usize] }
    pub fn node(&self, id: TermId) -> Node { self.row(id).node }
    pub fn children(&self, id: TermId) -> &[TermId] { self.term_ids(self.row(id).children) }
    fn bounds(&self, id: RangeId) -> std::ops::Range<usize> {
        let range = self.tables.ranges[id.0 as usize];
        range.start as usize..range.start as usize + range.len as usize
    }
    pub fn term_ids(&self, id: RangeId) -> &[TermId] { &self.tables.terms[self.bounds(id)] }
    pub fn name_ids(&self, id: RangeId) -> &[NameId] { &self.tables.names[self.bounds(id)] }
    pub fn field_ids(&self, id: RangeId) -> &[Field] { &self.tables.fields[self.bounds(id)] }
    pub fn binding_ids(&self, id: RangeId) -> &[Binding] { &self.tables.bindings[self.bounds(id)] }
    pub fn arm_ids(&self, id: RangeId) -> &[Arm] { &self.tables.arms[self.bounds(id)] }
    pub fn case_arm_ids(&self, id: RangeId) -> &[CaseArm] { &self.tables.case_arms[self.bounds(id)] }
    pub fn parameters(&self, id: RangeId) -> &[Parameter] { &self.tables.params[self.bounds(id)] }
    pub fn pattern_node(&self, id: PatternId) -> PatternNode { self.tables.patterns[id.0 as usize] }
    pub fn pattern_ids(&self, id: RangeId) -> &[PatternId] { &self.tables.pattern_edges[self.bounds(id)] }
    pub fn path_names(&self, id: PathId) -> &[NameId] { self.name_ids(self.tables.paths[(id.0.get() - 1) as usize]) }
    pub fn skip_ranges(&self, id: RangeId) -> &[RangeId] { &self.tables.skips[self.bounds(id)] }
    pub fn indices(&self, id: RangeId) -> &[usize] { &self.tables.indices[self.bounds(id)] }
    pub fn raw_bytes(&self, id: u32) -> &[u8] { &self.tables.bytes[id as usize] }
    pub fn literal(&self, id: u32) -> Literal { self.tables.literals[id as usize] }
    payload_accessors! {
        ref_binding: refs -> RefBinding, call: calls -> Call, op: ops -> Op, call_fn: call_fns -> CallFn,
        graph_ref: graphs -> GraphRef, let_node: lets -> Let, state: states -> State, loop_node: loops -> Loop,
        pair: pairs -> Pair, error_rows: errors -> ErrorRows, if_node: ifs -> If, cond: conds -> Cond,
        case: cases -> Case, function: functions -> Function, hof: hofs -> Hof, get: gets -> Get,
        assoc: assocs -> Assoc, list_op: list_ops -> ListOp, expanded: expanded -> Expanded,
    }
    pub fn set_type(&mut self, id: TermId, ty: &Ty) {
        let ty = self.type_id(ty);
        self.tables.rows[id.0 as usize].ty = ty;
    }
    pub fn set_form(&mut self, id: TermId, form: &Syntax) {
        let form = self.import_form(form);
        self.tables.rows[id.0 as usize].form = form;
    }
    /// Metadata-only rewrites share immutable payload/edge ranges, never copy a subtree.
    pub fn with_path(&mut self, id: TermId, path: Option<&[String]>) -> TermId {
        let mut row = *self.row(id);
        row.path = path.map(|path| self.path(path));
        self.push_row(row)
    }
    pub fn with_type(&mut self, id: TermId, ty: &Ty) -> TermId {
        let mut row = *self.row(id);
        row.ty = self.type_id(ty);
        self.push_row(row)
    }
    pub fn with_form(&mut self, id: TermId, form: &Syntax) -> TermId {
        let mut row = *self.row(id);
        row.form = self.import_form(form);
        self.push_row(row)
    }
    pub fn with_function_body(&mut self, id: TermId, body: TermId, ty: &Ty, capture: Option<TermId>) -> TermId {
        let mut row = *self.row(id);
        let Node::Fn(payload) = row.node else { panic!("with_function_body requires a function"); };
        let function = Function { body, capture, ..self.tables.functions[payload as usize] };
        let children = [body, capture.unwrap_or(body)];
        row.children = self.terms(&children[..1 + usize::from(capture.is_some())]);
        row.ty = self.type_id(ty);
        row.node = Node::Fn(index(self.tables.functions.len()));
        self.tables.functions.push(function);
        self.push_row(row)
    }
    fn push_row(&mut self, row: TermRow) -> TermId {
        let id = TermId(index(self.tables.rows.len()));
        self.tables.rows.push(row);
        id
    }
    fn range(&mut self, start: usize, len: usize) -> RangeId {
        if len == 0 { return RangeId(0); }
        index(start.checked_add(len).expect("term range overflow"));
        let id = RangeId(index(self.tables.ranges.len()));
        self.tables.ranges.push(Range { start: index(start), len: index(len) });
        id
    }
    fn terms(&mut self, terms: &[TermId]) -> RangeId {
        assert!(terms.iter().all(|id| (id.0 as usize) < self.tables.rows.len()), "term edges must refer to earlier rows");
        let range = self.range(self.tables.terms.len(), terms.len());
        self.tables.terms.extend_from_slice(terms);
        range
    }
    fn names(&mut self, names: &[String]) -> RangeId {
        let range = self.range(self.tables.names.len(), names.len());
        for name in names {
            let name = self.intern(name);
            self.tables.names.push(name);
        }
        range
    }
    fn name_range(&mut self, names: &[NameId]) -> RangeId {
        let range = self.range(self.tables.names.len(), names.len());
        self.tables.names.extend_from_slice(names);
        range
    }
    pub fn path(&mut self, path: &[String]) -> PathId {
        let names: Vec<NameId> = path.iter().map(|name| self.intern(name)).collect();
        if let Some(&id) = self.paths.get(&names) { return id; }
        let range = self.name_range(&names);
        let id = PathId(std::num::NonZeroU32::new(index(self.tables.paths.len()).checked_add(1).expect("term path limit")).unwrap());
        self.tables.paths.push(range);
        self.paths.insert(names, id);
        id
    }
    pub fn type_id(&mut self, ty: &Ty) -> TypeId {
        // ponytail: one graph has few distinct types; use a keyed type table if this scan profiles hot.
        if let Some(id) = self.tables.types.iter().position(|known| known == ty) { return TypeId(index(id)); }
        let id = TypeId(index(self.tables.types.len()));
        self.tables.types.push(ty.clone());
        id
    }
    fn pattern(&mut self, pattern: &Pattern) -> PatternId {
        let node = match pattern {
            Pattern::Name(name) => PatternNode::Name(self.intern(name)),
            Pattern::Keys(names) => PatternNode::Keys(self.names(names)),
            Pattern::Seq(patterns) => {
                let children: Vec<PatternId> = patterns.iter().map(|pattern| self.pattern(pattern)).collect();
                let range = self.range(self.tables.pattern_edges.len(), children.len());
                self.tables.pattern_edges.extend_from_slice(&children);
                PatternNode::Seq(range)
            }
        };
        let id = PatternId(index(self.tables.patterns.len()));
        self.tables.patterns.push(node);
        id
    }
    fn fields(&mut self, fields: Vec<(String, TermId)>) -> RangeId {
        let range = self.range(self.tables.fields.len(), fields.len());
        for (name, term) in fields {
            let name = self.intern(&name);
            self.tables.fields.push(Field { name, term });
        }
        range
    }
    fn bindings(&mut self, bindings: Vec<(Pattern, TermId)>) -> RangeId {
        let range = self.range(self.tables.bindings.len(), bindings.len());
        for (pattern, term) in bindings {
            let pattern = self.pattern(&pattern);
            self.tables.bindings.push(Binding { pattern, term });
        }
        range
    }
    fn skip(&mut self, skips: Vec<Vec<usize>>) -> RangeId {
        let range = self.range(self.tables.skips.len(), skips.len());
        for tuple in skips {
            let tuple_range = self.range(self.tables.indices.len(), tuple.len());
            self.tables.indices.extend(tuple);
            self.tables.skips.push(tuple_range);
        }
        range
    }
    fn bytes(&mut self, bytes: Vec<u8>) -> u32 {
        let id = index(self.tables.bytes.len());
        self.tables.bytes.push(bytes);
        id
    }
    pub fn set_path(&mut self, id: TermId, path: Option<&[String]>) {
        let path = path.map(|path| self.path(path));
        self.tables.rows[id.0 as usize].path = path;
    }
    pub fn push(&mut self, path: Option<&[String]>, ty: &Ty, form: SyntaxId, draft: Draft) -> TermId {
        self.syntax.row(form);
        let mut children = std::mem::take(&mut self.scratch);
        let node = match draft {
            Draft::Lit(value) => {
                let literal = match value {
                    param::Value::Bool(value) => Literal::Bool(value), param::Value::Int(value) => Literal::Int(value),
                    param::Value::Float(value) => Literal::Float(value), param::Value::Text(bytes) => Literal::Text(self.bytes(bytes)),
                    param::Value::Choice(name) => Literal::Choice(self.intern(&name)),
                };
                let id = index(self.tables.literals.len()); self.tables.literals.push(literal); Node::Lit(id)
            }
            Draft::Text(bytes) => Node::Text(self.bytes(bytes)), Draft::Nil => Node::Nil, Draft::Time => Node::Time,
            Draft::Vec(terms) => { children.extend_from_slice(&terms); Node::Vec(self.terms(&terms)) }
            Draft::ListLit(terms) => { children.extend_from_slice(&terms); Node::ListLit(self.terms(&terms)) }
            Draft::Str(terms) => { children.extend_from_slice(&terms); Node::Str(self.terms(&terms)) }
            Draft::RefBinding(name, fields) => {
                let value = RefBinding { name: self.intern(&name), fields: self.names(&fields) };
                let id = index(self.tables.refs.len()); self.tables.refs.push(value); Node::RefBinding(id)
            }
            Draft::FnRef(name) => Node::FnRef(self.intern(&name)),
            Draft::Call { kind, ctx, args } => {
                children.extend(args.iter().map(|(_, term)| *term));
                let value = Call { kind: self.intern(&kind), ctx, args: self.fields(args) };
                let id = index(self.tables.calls.len()); self.tables.calls.push(value); Node::Call(id)
            }
            Draft::Op { op, args, skip } => {
                children.extend(args.iter().map(|(_, term)| *term));
                let value = Op { op: self.intern(&op), args: self.fields(args), skip: self.skip(skip) };
                let id = index(self.tables.ops.len()); self.tables.ops.push(value); Node::Op(id)
            }
            Draft::CallFn { fn_name, args, body } => {
                children.extend_from_slice(&args); children.extend(body);
                let value = CallFn { name: self.intern(&fn_name), args: self.terms(&args), body };
                let id = index(self.tables.call_fns.len()); self.tables.call_fns.push(value); Node::CallFn(id)
            }
            Draft::GraphRef { graph, inputs } => {
                children.extend(inputs.iter().map(|(_, term)| *term));
                let value = GraphRef { graph: self.intern(&graph), inputs: self.fields(inputs) };
                let id = index(self.tables.graphs.len()); self.tables.graphs.push(value); Node::GraphRef(id)
            }
            Draft::Let(bindings, body) => {
                children.extend(bindings.iter().map(|(_, term)| *term)); children.push(body);
                let value = Let { bindings: self.bindings(bindings), body };
                let id = index(self.tables.lets.len()); self.tables.lets.push(value); Node::Let(id)
            }
            Draft::State { binder, init, step, zone } => {
                children.extend([init, step]);
                let value = State { binder: self.pattern(&binder), init, step, zone: self.path(&zone) };
                let id = index(self.tables.states.len()); self.tables.states.push(value); Node::State(id)
            }
            Draft::Loop { kind, accs, clauses, skip, body, zone } => {
                children.extend(accs.iter().chain(&clauses).map(|(_, term)| *term)); children.push(body);
                let value = Loop { kind, accs: self.bindings(accs), clauses: self.bindings(clauses), skip: self.skip(skip), body, zone: self.path(&zone) };
                let id = index(self.tables.loops.len()); self.tables.loops.push(value); Node::Loop(id)
            }
            Draft::WithError(first, second) => {
                children.extend([first, second]);
                let id = index(self.tables.pairs.len()); self.tables.pairs.push(Pair { first, second }); Node::WithError(id)
            }
            Draft::ErrorRows { rows, policy, mapper } => {
                children.extend([rows, mapper]);
                let id = index(self.tables.errors.len()); self.tables.errors.push(ErrorRows { rows, policy, mapper }); Node::ErrorRows(id)
            }
            Draft::If(test, yes, no) => {
                children.extend([test, yes, no]);
                let id = index(self.tables.ifs.len()); self.tables.ifs.push(If { test, yes, no }); Node::If(id)
            }
            Draft::Cond(arms, default) => {
                for (test, body) in &arms { children.extend([*test, *body]); } children.push(default);
                let range = self.range(self.tables.arms.len(), arms.len());
                self.tables.arms.extend(arms.into_iter().map(|(test, body)| Arm { test, body }));
                let id = index(self.tables.conds.len()); self.tables.conds.push(Cond { arms: range, default }); Node::Cond(id)
            }
            Draft::Case(value, arms, default) => {
                children.push(value); children.extend(arms.iter().map(|(_, body)| *body)); children.push(default);
                for (form, _) in &arms { self.syntax.row(*form); }
                let range = self.range(self.tables.case_arms.len(), arms.len());
                self.tables.case_arms.extend(arms.into_iter().map(|(form, body)| CaseArm { form, body }));
                let id = index(self.tables.cases.len()); self.tables.cases.push(Case { value, arms: range, default }); Node::Case(id)
            }
            Draft::Fn { params, body, zone, capture } => {
                children.push(body); children.extend(capture);
                let range = self.range(self.tables.params.len(), params.len());
                for (pattern, ty) in params {
                    let pattern = self.pattern(&pattern);
                    let ty = ty.as_ref().map(|ty| self.type_id(ty));
                    self.tables.params.push(Parameter { pattern, ty });
                }
                let value = Function { params: range, body, zone: self.path(&zone), capture };
                let id = index(self.tables.functions.len()); self.tables.functions.push(value); Node::Fn(id)
            }
            Draft::Hof(kind, args) => {
                children.extend_from_slice(&args);
                let value = Hof { kind, args: self.terms(&args) };
                let id = index(self.tables.hofs.len()); self.tables.hofs.push(value); Node::Hof(id)
            }
            Draft::Record(fields) => { children.extend(fields.iter().map(|(_, term)| *term)); Node::Record(self.fields(fields)) }
            Draft::Get(value, name) => {
                children.push(value); let value = Get { value, name: self.intern(&name) };
                let id = index(self.tables.gets.len()); self.tables.gets.push(value); Node::Get(id)
            }
            Draft::Assoc(value, fields) => {
                children.push(value); children.extend(fields.iter().map(|(_, term)| *term));
                let value = Assoc { value, fields: self.fields(fields) };
                let id = index(self.tables.assocs.len()); self.tables.assocs.push(value); Node::Assoc(id)
            }
            Draft::ListOp(name, args) => {
                children.extend_from_slice(&args); let value = ListOp { name: self.intern(&name), args: self.terms(&args) };
                let id = index(self.tables.list_ops.len()); self.tables.list_ops.push(value); Node::ListOp(id)
            }
            Draft::Bypass(child) => { children.push(child); Node::Bypass(child) }
            Draft::Expanded { macro_name, body } => {
                children.push(body); let value = Expanded { name: self.intern(&macro_name), body };
                let id = index(self.tables.expanded.len()); self.tables.expanded.push(value); Node::Expanded(id)
            }
        };
        let child_range = match node {
            Node::Vec(range) | Node::ListLit(range) | Node::Str(range) => range,
            Node::Hof(id) => self.tables.hofs[id as usize].args,
            Node::ListOp(id) => self.tables.list_ops[id as usize].args,
            Node::CallFn(id) if self.tables.call_fns[id as usize].body.is_none() => self.tables.call_fns[id as usize].args,
            _ => self.terms(&children),
        };
        children.clear();
        self.scratch = children;
        let ty = self.type_id(ty);
        let path = path.map(|path| self.path(path));
        let id = TermId(index(self.tables.rows.len()));
        self.tables.rows.push(TermRow { ty, path, form, node, children: child_range });
        id
    }
    /// Import reachable rows once into this graph; frozen storage never keeps an old owner alive.
    pub fn import(&mut self, term: &Term) -> TermId {
        if let Some(id) = self.imported(term) { return id; }
        let source = &term.arena;
        let mut pending = vec![(term.row, false)];
        while let Some((id, ready)) = pending.pop() {
            let key = (source.owner, id.0);
            if self.imports.contains_key(&key) { continue; }
            if !ready {
                pending.push((id, true));
                pending.extend(source.children(id).iter().rev().map(|child| (*child, false)));
                continue;
            }
            let row = source.row(id);
            let draft = self.import_node(source, row.node, None);
            let path = row.path.map(|id| source.path(id).iter().map(|&name| source.name(name).to_owned()).collect::<Vec<_>>());
            let form = self.import_form(&source.syntax.handle(row.form));
            let imported = self.push(path.as_deref(), source.ty(row.ty), form, draft);
            self.imports.insert(key, imported);
        }
        self.imports[&term.identity()]
    }

    // This allocating reconstruction is construction-only, never a read projection.
    fn import_node(&mut self, source: &TermArena, node: Node, mapped: Option<&HashMap<(u64, u32), TermId>>) -> Draft {
        let imports = mapped.unwrap_or(&self.imports);
        let child = |id: TermId| imports[&(source.owner, id.0)];
        let terms = |range| source.term_ids(range).iter().copied().map(child).collect();
        let names = |range| source.name_ids(range).iter().map(|&id| source.name(id).to_owned()).collect();
        let fields = |range| source.fields(range).iter().map(|field| (source.name(field.name).to_owned(), child(field.term))).collect();
        let bindings = |range| source.bindings(range).iter().map(|binding| (source_pattern(source, binding.pattern), child(binding.term))).collect();
        let path = |id| source.path(id).iter().map(|&name| source.name(name).to_owned()).collect();
        let skip = |range| source.skip(range).iter().map(|&range| source.indices(range).to_vec()).collect();
        match node {
            Node::Lit(id) => Draft::Lit(match source.literal(id) {
                Literal::Bool(value) => param::Value::Bool(value), Literal::Int(value) => param::Value::Int(value),
                Literal::Float(value) => param::Value::Float(value), Literal::Text(id) => param::Value::Text(source.bytes(id).to_vec()),
                Literal::Choice(name) => param::Value::Choice(source.name(name).to_owned()),
            }),
            Node::Text(id) => Draft::Text(source.bytes(id).to_vec()), Node::Nil => Draft::Nil, Node::Time => Draft::Time,
            Node::Vec(range) => Draft::Vec(terms(range)), Node::ListLit(range) => Draft::ListLit(terms(range)),
            Node::Str(range) => Draft::Str(terms(range)), Node::Record(range) => Draft::Record(fields(range)),
            Node::FnRef(name) => Draft::FnRef(source.name(name).to_owned()),
            Node::RefBinding(id) => { let value = source.ref_binding(id); Draft::RefBinding(source.name(value.name).to_owned(), names(value.fields)) }
            Node::Call(id) => { let value = source.call(id); Draft::Call { kind: source.name(value.kind).to_owned(), ctx: value.ctx, args: fields(value.args) } }
            Node::Op(id) => { let value = source.op(id); Draft::Op { op: source.name(value.op).to_owned(), args: fields(value.args), skip: skip(value.skip) } }
            Node::CallFn(id) => { let value = source.call_fn(id); Draft::CallFn { fn_name: source.name(value.name).to_owned(), args: terms(value.args), body: value.body.map(child) } }
            Node::GraphRef(id) => { let value = source.graph_ref(id); Draft::GraphRef { graph: source.name(value.graph).to_owned(), inputs: fields(value.inputs) } }
            Node::Let(id) => { let value = source.let_node(id); Draft::Let(bindings(value.bindings), child(value.body)) }
            Node::State(id) => { let value = source.state(id); Draft::State { binder: source_pattern(source, value.binder), init: child(value.init), step: child(value.step), zone: path(value.zone) } }
            Node::Loop(id) => { let value = source.loop_node(id); Draft::Loop { kind: value.kind, accs: bindings(value.accs), clauses: bindings(value.clauses), skip: skip(value.skip), body: child(value.body), zone: path(value.zone) } }
            Node::WithError(id) => { let value = source.pair(id); Draft::WithError(child(value.first), child(value.second)) }
            Node::ErrorRows(id) => { let value = source.error_rows(id); Draft::ErrorRows { rows: child(value.rows), policy: value.policy, mapper: child(value.mapper) } }
            Node::If(id) => { let value = source.if_node(id); Draft::If(child(value.test), child(value.yes), child(value.no)) }
            Node::Cond(id) => { let value = source.cond(id); Draft::Cond(source.arms(value.arms).iter().map(|arm| (child(arm.test), child(arm.body))).collect(), child(value.default)) }
            Node::Case(id) => {
                let value = source.case(id);
                let mut arms = Vec::with_capacity(source.case_arms(value.arms).len());
                for arm in source.case_arms(value.arms) {
                    arms.push((self.syntax.import(&source.syntax.handle(arm.form)), child(arm.body)));
                }
                Draft::Case(child(value.value), arms, child(value.default))
            }
            Node::Fn(id) => {
                let value = source.function(id);
                Draft::Fn { params: source.params(value.params).iter().map(|param| (source_pattern(source, param.pattern), param.ty.map(|ty| source.ty(ty).clone()))).collect(),
                    body: child(value.body), zone: path(value.zone), capture: value.capture.map(child) }
            }
            Node::Hof(id) => { let value = source.hof(id); Draft::Hof(value.kind, terms(value.args)) }
            Node::Get(id) => { let value = source.get(id); Draft::Get(child(value.value), source.name(value.name).to_owned()) }
            Node::Assoc(id) => { let value = source.assoc(id); Draft::Assoc(child(value.value), fields(value.fields)) }
            Node::ListOp(id) => { let value = source.list_op(id); Draft::ListOp(source.name(value.name).to_owned(), terms(value.args)) }
            Node::Bypass(id) => Draft::Bypass(child(id)),
            Node::Expanded(id) => { let value = source.expanded(id); Draft::Expanded { macro_name: source.name(value.name).to_owned(), body: child(value.body) } }
        }
    }

    pub fn rewrite(&mut self, root: &Term, rewrite: &mut dyn FnMut(&mut TermBuilder, &Term, Draft) -> Draft) -> TermId {
        self.rewrite_roots(std::slice::from_ref(root), rewrite)[0]
    }
    /// One construction traversal across graph roots; payload drafts never become a read projection.
    pub fn rewrite_roots(&mut self, roots: &[Term], rewrite: &mut dyn FnMut(&mut TermBuilder, &Term, Draft) -> Draft) -> Vec<TermId> {
        self.rewrite_roots_with(roots, &mut |builder, source, draft| {
            let draft = rewrite(builder, source, draft);
            builder.rebuild(source, draft)
        })
    }
    pub fn rebuild(&mut self, source: &Term, draft: Draft) -> TermId {
        let path = source.path().map(|path| path.iter().map(|&name| source.name(name).to_owned()).collect::<Vec<_>>());
        let form = self.import_form(&source.form());
        self.push(path.as_deref(), source.ty(), form, draft)
    }
    pub fn rewrite_roots_with(&mut self, roots: &[Term], rewrite: &mut dyn FnMut(&mut TermBuilder, &Term, Draft) -> TermId) -> Vec<TermId> {
        let mut mapped = HashMap::new();
        for root in roots {
            let source = &root.arena;
            let mut pending = vec![(root.row, false)];
            while let Some((id, ready)) = pending.pop() {
                let key = (source.owner, id.0);
                if mapped.contains_key(&key) { continue; }
                if !ready {
                    pending.push((id, true));
                    pending.extend(source.children(id).iter().rev().map(|child| (*child, false)));
                    continue;
                }
                let row = source.row(id);
                let draft = self.import_node(source, row.node, Some(&mapped));
                let target = rewrite(self, &source.handle(id), draft);
                self.row(target);
                mapped.insert(key, target);
            }
        }
        roots.iter().map(|root| mapped[&root.identity()]).collect()
    }

    /// Seal one graph's roots, discarding checking-only rows and detaching its authored forms.
    pub fn finish(self, roots: &[TermId], forms: &[SyntaxId]) -> (Arc<TermArena>, Vec<TermId>, Vec<SyntaxId>) {
        // ponytail: compact once per graph through the importer; compact tables in place if this copy profiles hot.
        let source = self.freeze();
        let mut builder = TermBuilder::new();
        let forms = forms.iter().map(|&id| builder.import_form(&source.syntax.handle(id))).collect();
        let roots = roots.iter().map(|&id| builder.import(&source.handle(id))).collect();
        (builder.freeze(), roots, forms)
    }

    pub fn freeze(self) -> Arc<TermArena> {
        Arc::new(TermArena { owner: arena::fresh_owner_id(), tables: self.tables, names: self.names.freeze(), syntax: self.syntax.freeze() })
    }
}

fn source_pattern(source: &TermArena, id: PatternId) -> Pattern {
    match source.pattern(id) {
        PatternNode::Name(name) => Pattern::Name(source.name(name).to_owned()),
        PatternNode::Keys(range) => Pattern::Keys(source.name_ids(range).iter().map(|&name| source.name(name).to_owned()).collect()),
        PatternNode::Seq(range) => Pattern::Seq(source.pattern_ids(range).iter().map(|&id| source_pattern(source, id)).collect()),
    }
}

fn attribute_type(memory: &mut flow_syntax::syntax::Memory, ty: &Ty) {
    match ty {
        Ty::Named(name) => { memory.allocation(name.as_ptr(), name.capacity()); }
        Ty::List(element) | Ty::Array(element) => {
            if memory.allocation(element.as_ref() as *const Ty, std::mem::size_of::<Ty>()) { attribute_type(memory, element); }
        }
        Ty::Record(fields) => {
            if memory.allocation(fields.as_ptr(), fields.capacity() * std::mem::size_of::<(String, Ty)>()) {
                for (name, ty) in fields { memory.allocation(name.as_ptr(), name.capacity()); attribute_type(memory, ty); }
            }
        }
        Ty::Fn(Some(signature)) => {
            if memory.allocation(signature.as_ref() as *const flow_syntax::ty::FnSignature, std::mem::size_of::<flow_syntax::ty::FnSignature>()) {
                if memory.allocation(signature.params.as_ptr(), signature.params.capacity() * std::mem::size_of::<Ty>()) {
                    for ty in &signature.params { attribute_type(memory, ty); }
                }
                attribute_type(memory, &signature.result);
            }
        }
        _ => (),
    }
}

impl std::fmt::Debug for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Term")
            .field("path", &self.path.map(|id| DebugPart::new(&self.arena, DebugKind::Path(id))))
            .field("ty", self.ty())
            .field("node", &DebugPart::new(&self.arena, DebugKind::Node(self.node)))
            .field("form", &self.form()).finish()
    }
}

struct DebugPart { arena: Arc<TermArena>, kind: DebugKind }
#[derive(Clone, Copy)]
enum DebugKind {
    Node(Node), Terms(RangeId), Fields(RangeId), Bindings(RangeId), Arms(RangeId), CaseArms(RangeId),
    Params(RangeId), Path(PathId), Pattern(PatternId), Patterns(RangeId), Names(RangeId), Skip(RangeId), Literal(u32),
}
impl DebugPart {
    fn new(arena: &Arc<TermArena>, kind: DebugKind) -> Self { Self { arena: arena.clone(), kind } }
    fn part(&self, kind: DebugKind) -> Self { Self::new(&self.arena, kind) }
}
impl std::fmt::Debug for DebugPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let a = &self.arena;
        match self.kind {
            DebugKind::Terms(range) => f.debug_list().entries(a.term_ids(range).iter().map(|&id| a.handle(id))).finish(),
            DebugKind::Fields(range) => f.debug_list().entries(a.fields(range).iter().map(|field| (a.name(field.name), a.handle(field.term)))).finish(),
            DebugKind::Bindings(range) => f.debug_list().entries(a.bindings(range).iter().map(|binding| (self.part(DebugKind::Pattern(binding.pattern)), a.handle(binding.term)))).finish(),
            DebugKind::Arms(range) => f.debug_list().entries(a.arms(range).iter().map(|arm| (a.handle(arm.test), a.handle(arm.body)))).finish(),
            DebugKind::CaseArms(range) => f.debug_list().entries(a.case_arms(range).iter().map(|arm| (a.syntax.handle(arm.form), a.handle(arm.body)))).finish(),
            DebugKind::Params(range) => f.debug_list().entries(a.params(range).iter().map(|param| (self.part(DebugKind::Pattern(param.pattern)), param.ty.map(|id| a.ty(id))))).finish(),
            DebugKind::Path(id) => f.debug_list().entries(a.path(id).iter().map(|&id| a.name(id))).finish(),
            DebugKind::Names(range) => f.debug_list().entries(a.name_ids(range).iter().map(|&id| a.name(id))).finish(),
            DebugKind::Patterns(range) => f.debug_list().entries(a.pattern_ids(range).iter().map(|&id| self.part(DebugKind::Pattern(id)))).finish(),
            DebugKind::Skip(range) => f.debug_list().entries(a.skip(range).iter().map(|&range| a.indices(range))).finish(),
            DebugKind::Pattern(id) => match a.pattern(id) {
                PatternNode::Name(name) => f.debug_tuple("Name").field(&a.name(name)).finish(),
                PatternNode::Seq(range) => f.debug_tuple("Seq").field(&self.part(DebugKind::Patterns(range))).finish(),
                PatternNode::Keys(range) => f.debug_tuple("Keys").field(&self.part(DebugKind::Names(range))).finish(),
            },
            DebugKind::Literal(id) => match a.literal(id) {
                Literal::Bool(value) => f.debug_tuple("Bool").field(&value).finish(),
                Literal::Int(value) => f.debug_tuple("Int").field(&value).finish(),
                Literal::Float(value) => f.debug_tuple("Float").field(&value).finish(),
                Literal::Text(id) => f.debug_tuple("Text").field(&a.bytes(id)).finish(),
                Literal::Choice(name) => f.debug_tuple("Choice").field(&a.name(name)).finish(),
            },
            DebugKind::Node(node) => match node {
                Node::Lit(id) => f.debug_tuple("Lit").field(&self.part(DebugKind::Literal(id))).finish(),
                Node::Text(id) => f.debug_tuple("Text").field(&a.bytes(id)).finish(),
                Node::Nil => f.write_str("Nil"), Node::Time => f.write_str("Time"),
                Node::Vec(range) => f.debug_tuple("Vec").field(&self.part(DebugKind::Terms(range))).finish(),
                Node::ListLit(range) => f.debug_tuple("ListLit").field(&self.part(DebugKind::Terms(range))).finish(),
                Node::Str(range) => f.debug_tuple("Str").field(&self.part(DebugKind::Terms(range))).finish(),
                Node::Record(range) => f.debug_tuple("Record").field(&self.part(DebugKind::Fields(range))).finish(),
                Node::FnRef(name) => f.debug_tuple("FnRef").field(&a.name(name)).finish(),
                Node::Bypass(id) => f.debug_tuple("Bypass").field(&a.handle(id)).finish(),
                Node::RefBinding(id) => {
                    let value = a.ref_binding(id);
                    f.debug_tuple("RefBinding").field(&a.name(value.name)).field(&self.part(DebugKind::Names(value.fields))).finish()
                }
                Node::Call(id) => {
                    let value = a.call(id);
                    f.debug_struct("Call").field("kind", &a.name(value.kind)).field("ctx", &value.ctx).field("args", &self.part(DebugKind::Fields(value.args))).finish()
                }
                Node::Op(id) => {
                    let value = a.op(id);
                    f.debug_struct("Op").field("op", &a.name(value.op)).field("args", &self.part(DebugKind::Fields(value.args))).field("skip", &self.part(DebugKind::Skip(value.skip))).finish()
                }
                Node::CallFn(id) => {
                    let value = a.call_fn(id);
                    f.debug_struct("CallFn").field("fn_name", &a.name(value.name)).field("args", &self.part(DebugKind::Terms(value.args))).field("body", &value.body.map(|id| a.handle(id))).finish()
                }
                Node::GraphRef(id) => {
                    let value = a.graph_ref(id);
                    f.debug_struct("GraphRef").field("graph", &a.name(value.graph)).field("inputs", &self.part(DebugKind::Fields(value.inputs))).finish()
                }
                Node::Let(id) => {
                    let value = a.let_node(id);
                    f.debug_tuple("Let").field(&self.part(DebugKind::Bindings(value.bindings))).field(&a.handle(value.body)).finish()
                }
                Node::State(id) => {
                    let value = a.state(id);
                    f.debug_struct("State").field("binder", &self.part(DebugKind::Pattern(value.binder))).field("init", &a.handle(value.init))
                        .field("step", &a.handle(value.step)).field("zone", &self.part(DebugKind::Path(value.zone))).finish()
                }
                Node::Loop(id) => {
                    let value = a.loop_node(id);
                    f.debug_struct("Loop").field("kind", &value.kind).field("accs", &self.part(DebugKind::Bindings(value.accs))).field("clauses", &self.part(DebugKind::Bindings(value.clauses)))
                        .field("skip", &self.part(DebugKind::Skip(value.skip))).field("body", &a.handle(value.body)).field("zone", &self.part(DebugKind::Path(value.zone))).finish()
                }
                Node::WithError(id) => {
                    let value = a.pair(id); f.debug_tuple("WithError").field(&a.handle(value.first)).field(&a.handle(value.second)).finish()
                }
                Node::ErrorRows(id) => {
                    let value = a.error_rows(id); f.debug_struct("ErrorRows").field("rows", &a.handle(value.rows)).field("policy", &value.policy).field("mapper", &a.handle(value.mapper)).finish()
                }
                Node::If(id) => {
                    let value = a.if_node(id); f.debug_tuple("If").field(&a.handle(value.test)).field(&a.handle(value.yes)).field(&a.handle(value.no)).finish()
                }
                Node::Cond(id) => {
                    let value = a.cond(id); f.debug_tuple("Cond").field(&self.part(DebugKind::Arms(value.arms))).field(&a.handle(value.default)).finish()
                }
                Node::Case(id) => {
                    let value = a.case(id); f.debug_tuple("Case").field(&a.handle(value.value)).field(&self.part(DebugKind::CaseArms(value.arms))).field(&a.handle(value.default)).finish()
                }
                Node::Fn(id) => {
                    let value = a.function(id); f.debug_struct("Fn").field("params", &self.part(DebugKind::Params(value.params))).field("body", &a.handle(value.body))
                        .field("zone", &self.part(DebugKind::Path(value.zone))).field("capture", &value.capture.map(|id| a.handle(id))).finish()
                }
                Node::Hof(id) => {
                    let value = a.hof(id); f.debug_tuple("Hof").field(&value.kind).field(&self.part(DebugKind::Terms(value.args))).finish()
                }
                Node::Get(id) => {
                    let value = a.get(id); f.debug_tuple("Get").field(&a.handle(value.value)).field(&a.name(value.name)).finish()
                }
                Node::Assoc(id) => {
                    let value = a.assoc(id); f.debug_tuple("Assoc").field(&a.handle(value.value)).field(&self.part(DebugKind::Fields(value.fields))).finish()
                }
                Node::ListOp(id) => {
                    let value = a.list_op(id); f.debug_tuple("ListOp").field(&a.name(value.name)).field(&self.part(DebugKind::Terms(value.args))).finish()
                }
                Node::Expanded(id) => {
                    let value = a.expanded(id); f.debug_struct("Expanded").field("macro_name", &a.name(value.name)).field("body", &a.handle(value.body)).finish()
                }
            },
        }
    }
}

impl PartialEq for Term {
    fn eq(&self, other: &Self) -> bool {
        if self.identity() == other.identity() { return true; }
        let (a, b) = (&self.arena, &other.arena);
        let mut pending = vec![(self.row, other.row)];
        let mut seen = std::collections::HashSet::new();
        let mut forms = Vec::new();
        while let Some((x, y)) = pending.pop() {
            if !seen.insert((x, y)) { continue; }
            let (x, y) = (a.row(x), b.row(y));
            if a.ty(x.ty) != b.ty(y.ty) || !paths_equal(a, x.path, b, y.path)
                || !metadata_equal(a, x.node, b, y.node) { return false; }
            forms.push((x.form, y.form));
            let (x, y) = (a.term_ids(x.children), b.term_ids(y.children));
            if x.len() != y.len() { return false; }
            pending.extend(x.iter().copied().zip(y.iter().copied()));
        }
        arena::equal_roots(&a.syntax, &b.syntax, &forms)
    }
}

fn names_equal(a: &TermArena, x: &[NameId], b: &TermArena, y: &[NameId]) -> bool {
    x.len() == y.len() && x.iter().zip(y).all(|(&x, &y)| a.name(x) == b.name(y))
}
fn paths_equal(a: &TermArena, x: Option<PathId>, b: &TermArena, y: Option<PathId>) -> bool {
    match (x, y) { (None, None) => true, (Some(x), Some(y)) => names_equal(a, a.path(x), b, b.path(y)), _ => false }
}
fn fields_equal(a: &TermArena, x: RangeId, b: &TermArena, y: RangeId) -> bool {
    let (x, y) = (a.fields(x), b.fields(y));
    x.len() == y.len() && x.iter().zip(y).all(|(x, y)| a.name(x.name) == b.name(y.name))
}
fn bindings_equal(a: &TermArena, x: RangeId, b: &TermArena, y: RangeId) -> bool {
    let (x, y) = (a.bindings(x), b.bindings(y));
    x.len() == y.len() && x.iter().zip(y).all(|(x, y)| patterns_equal(a, x.pattern, b, y.pattern))
}
fn skip_equal(a: &TermArena, x: RangeId, b: &TermArena, y: RangeId) -> bool {
    let (x, y) = (a.skip(x), b.skip(y));
    x.len() == y.len() && x.iter().zip(y).all(|(&x, &y)| a.indices(x) == b.indices(y))
}
fn patterns_equal(a: &TermArena, x: PatternId, b: &TermArena, y: PatternId) -> bool {
    let mut pending = vec![(x, y)];
    while let Some((x, y)) = pending.pop() {
        match (a.pattern(x), b.pattern(y)) {
            (PatternNode::Name(x), PatternNode::Name(y)) if a.name(x) == b.name(y) => (),
            (PatternNode::Keys(x), PatternNode::Keys(y)) if names_equal(a, a.name_ids(x), b, b.name_ids(y)) => (),
            (PatternNode::Seq(x), PatternNode::Seq(y)) => {
                let (x, y) = (a.pattern_ids(x), b.pattern_ids(y));
                if x.len() != y.len() { return false; }
                pending.extend(x.iter().copied().zip(y.iter().copied()));
            }
            _ => return false,
        }
    }
    true
}
fn metadata_equal(a: &TermArena, x: Node, b: &TermArena, y: Node) -> bool {
    match (x, y) {
        (Node::Nil, Node::Nil) | (Node::Time, Node::Time) | (Node::Vec(_), Node::Vec(_))
        | (Node::ListLit(_), Node::ListLit(_)) | (Node::Str(_), Node::Str(_))
        | (Node::Bypass(_), Node::Bypass(_)) | (Node::WithError(_), Node::WithError(_)) | (Node::If(_), Node::If(_)) => true,
        (Node::Lit(x), Node::Lit(y)) => match (a.literal(x), b.literal(y)) {
            (Literal::Bool(x), Literal::Bool(y)) => x == y, (Literal::Int(x), Literal::Int(y)) => x == y,
            (Literal::Float(x), Literal::Float(y)) => x == y, (Literal::Text(x), Literal::Text(y)) => a.bytes(x) == b.bytes(y),
            (Literal::Choice(x), Literal::Choice(y)) => a.name(x) == b.name(y), _ => false,
        },
        (Node::Text(x), Node::Text(y)) => a.bytes(x) == b.bytes(y),
        (Node::FnRef(x), Node::FnRef(y)) => a.name(x) == b.name(y),
        (Node::RefBinding(x), Node::RefBinding(y)) => {
            let (x, y) = (a.ref_binding(x), b.ref_binding(y));
            a.name(x.name) == b.name(y.name) && names_equal(a, a.name_ids(x.fields), b, b.name_ids(y.fields))
        }
        (Node::Call(x), Node::Call(y)) => {
            let (x, y) = (a.call(x), b.call(y));
            a.name(x.kind) == b.name(y.kind) && x.ctx == y.ctx && fields_equal(a, x.args, b, y.args)
        }
        (Node::Op(x), Node::Op(y)) => {
            let (x, y) = (a.op(x), b.op(y));
            a.name(x.op) == b.name(y.op) && fields_equal(a, x.args, b, y.args) && skip_equal(a, x.skip, b, y.skip)
        }
        (Node::CallFn(x), Node::CallFn(y)) => {
            let (x, y) = (a.call_fn(x), b.call_fn(y));
            a.name(x.name) == b.name(y.name) && a.term_ids(x.args).len() == b.term_ids(y.args).len() && x.body.is_some() == y.body.is_some()
        }
        (Node::GraphRef(x), Node::GraphRef(y)) => {
            let (x, y) = (a.graph_ref(x), b.graph_ref(y));
            a.name(x.graph) == b.name(y.graph) && fields_equal(a, x.inputs, b, y.inputs)
        }
        (Node::Let(x), Node::Let(y)) => { let (x, y) = (a.let_node(x), b.let_node(y)); bindings_equal(a, x.bindings, b, y.bindings) }
        (Node::State(x), Node::State(y)) => {
            let (x, y) = (a.state(x), b.state(y));
            patterns_equal(a, x.binder, b, y.binder) && paths_equal(a, Some(x.zone), b, Some(y.zone))
        }
        (Node::Loop(x), Node::Loop(y)) => {
            let (x, y) = (a.loop_node(x), b.loop_node(y));
            x.kind == y.kind && bindings_equal(a, x.accs, b, y.accs) && bindings_equal(a, x.clauses, b, y.clauses)
                && skip_equal(a, x.skip, b, y.skip) && paths_equal(a, Some(x.zone), b, Some(y.zone))
        }
        (Node::ErrorRows(x), Node::ErrorRows(y)) => a.error_rows(x).policy == b.error_rows(y).policy,
        (Node::Cond(x), Node::Cond(y)) => a.arms(a.cond(x).arms).len() == b.arms(b.cond(y).arms).len(),
        (Node::Case(x), Node::Case(y)) => {
            let (x, y) = (a.case_arms(a.case(x).arms), b.case_arms(b.case(y).arms));
            x.len() == y.len() && x.iter().zip(y).all(|(x, y)| a.syntax.handle(x.form) == b.syntax.handle(y.form))
        }
        (Node::Fn(x), Node::Fn(y)) => {
            let (x, y) = (a.function(x), b.function(y));
            let (xp, yp) = (a.params(x.params), b.params(y.params));
            paths_equal(a, Some(x.zone), b, Some(y.zone)) && x.capture.is_some() == y.capture.is_some() && xp.len() == yp.len()
                && xp.iter().zip(yp).all(|(x, y)| patterns_equal(a, x.pattern, b, y.pattern) && x.ty.map(|id| a.ty(id)) == y.ty.map(|id| b.ty(id)))
        }
        (Node::Hof(x), Node::Hof(y)) => a.hof(x).kind == b.hof(y).kind,
        (Node::Record(x), Node::Record(y)) => fields_equal(a, x, b, y),
        (Node::Get(x), Node::Get(y)) => a.name(a.get(x).name) == b.name(b.get(y).name),
        (Node::Assoc(x), Node::Assoc(y)) => fields_equal(a, a.assoc(x).fields, b, b.assoc(y).fields),
        (Node::ListOp(x), Node::ListOp(y)) => a.name(a.list_op(x).name) == b.name(b.list_op(y).name),
        (Node::Expanded(x), Node::Expanded(y)) => a.name(a.expanded(x).name) == b.name(b.expanded(y).name),
        _ => false,
    }
}

const _: () = assert!(std::mem::size_of::<Term>() == 16);
const _: () = assert!(std::mem::size_of::<Node>() == 8);
const _: () = assert!(std::mem::size_of::<TermRow>() == 24);

#[cfg(test)]
mod tests {
    use super::*;
    use flow_syntax::syntax::QuoteKind;
    use rays_diagnostic::Span;

    fn form(builder: &mut TermBuilder) -> SyntaxId {
        let syntax = builder.syntax_builder();
        let name = syntax.intern("authored");
        let atom = syntax.push(1_i64 << 40, arena::Node::Sym(name), Span { start: 0, finish: 8 }, None);
        syntax.push(-100, arena::Node::Quote(QuoteKind::Plain, atom), Span { start: 0, finish: 8 }, None)
    }

    #[test]
    fn all_payloads_import_once_without_retaining_historical_graphs() {
        let mut builder = TermBuilder::new();
        let form = form(&mut builder);
        let atom = builder.push(None, &Ty::Float, form, Draft::Lit(param::Value::Float(2.0)));
        let other = builder.push(None, &Ty::Float, form, Draft::Lit(param::Value::Float(3.0)));
        let pattern = Pattern::Seq(vec![Pattern::Name("x".into()), Pattern::Keys(vec!["y".into()])]);
        let fields = vec![("x".into(), atom), ("y".into(), other)];
        let bindings = vec![(pattern.clone(), atom)];
        let zone = vec!["g".into(), "x".into()];
        let drafts = vec![
            Draft::Lit(param::Value::Bool(true)), Draft::Lit(param::Value::Int(-3)),
            Draft::Lit(param::Value::Text(vec![0, 255])), Draft::Lit(param::Value::Choice("x".into())),
            Draft::Text(vec![0, 254]), Draft::Nil, Draft::Time, Draft::Vec(vec![atom, atom]),
            Draft::RefBinding("x".into(), vec!["y".into()]),
            Draft::Call { kind: "sop/test".into(), ctx: flow_syntax::context::sop(), args: fields.clone() },
            Draft::Op { op: "+".into(), args: fields.clone(), skip: vec![vec![0, 1], vec![2]] },
            Draft::CallFn { fn_name: "f".into(), args: vec![atom], body: Some(other) }, Draft::FnRef("f".into()),
            Draft::GraphRef { graph: "g".into(), inputs: fields.clone() }, Draft::Let(bindings.clone(), atom),
            Draft::State { binder: pattern.clone(), init: atom, step: other, zone: zone.clone() },
            Draft::Loop { kind: LoopKind::Scan, accs: bindings.clone(), clauses: bindings.clone(), skip: vec![vec![1]], body: atom, zone: zone.clone() },
            Draft::WithError(atom, other), Draft::ErrorRows { rows: atom, policy: DiagnosticPolicy::RangeLast, mapper: other },
            Draft::If(atom, other, atom), Draft::Cond(vec![(atom, other)], atom), Draft::Case(atom, vec![(form, other)], atom),
            Draft::Fn { params: vec![(pattern, Some(Ty::Float))], body: atom, zone: zone.clone(), capture: Some(other) },
            Draft::Hof(HofKind::Map, vec![atom, other]), Draft::ListLit(vec![atom, other]), Draft::Record(fields.clone()),
            Draft::Get(atom, "x".into()), Draft::Assoc(atom, fields), Draft::Str(vec![atom, other]),
            Draft::ListOp("append".into(), vec![atom, other]), Draft::Bypass(atom), Draft::Expanded { macro_name: "m".into(), body: atom },
        ];
        let mut children = Vec::new();
        for draft in drafts { children.push(builder.push(Some(&zone), &Ty::Float, form, draft)); }
        let root = builder.push(None, &Ty::Any, form, Draft::Vec(children));
        let source = builder.freeze();
        let source_weak = Arc::downgrade(&source);
        let syntax_weak = Arc::downgrade(source.syntax());
        let root = source.handle(root);
        let expected_debug = format!("{root:?}");
        assert!(!expected_debug.contains("TermId"));
        let mut target = TermBuilder::new();
        let imported = target.import(&root);
        assert_eq!(imported, target.import(&root));
        let target = target.freeze();
        assert_eq!(target.rows().len(), source.rows().len());
        assert_eq!(target.handle(imported), root);
        assert_eq!(format!("{:?}", target.handle(imported)), expected_debug);
        let mut checked = flow_syntax::syntax::Memory::default();
        let mut syntax = flow_syntax::syntax::Memory::default();
        target.attribute(&mut checked, &mut syntax);
        let bytes = (checked.bytes(), syntax.bytes());
        target.attribute(&mut checked, &mut syntax);
        assert_eq!(bytes, (checked.bytes(), syntax.bytes()));
        drop(root);
        drop(source);
        assert!(source_weak.upgrade().is_none());
        assert!(syntax_weak.upgrade().is_none());
        assert!(!target.rows().is_empty());
    }

    #[test]
    fn term_equality_checks_source_payload_locations_and_trivia_at_every_root() {
        fn term(source: &Syntax) -> Term {
            let mut builder = TermBuilder::new();
            let root_form = builder.import_form(&arena::make(arena::Draft::Sym("root".into())));
            let child_form = builder.import_form(source);
            let child = builder.push(None, &Ty::Int, child_form, Draft::Lit(param::Value::Int(1)));
            let root = builder.push(None, &Ty::Any, root_form, Draft::Vec(vec![child, child]));
            builder.freeze().handle(root)
        }
        let source = arena::make(arena::Draft::Sym("leaf".into()));
        let expected = term(&source);
        assert_eq!(expected, term(&source));
        let mut builder = SyntaxBuilder::new();
        let imported = builder.import(&source);
        let different = builder.with_node(&source, arena::Draft::Sym("other".into()));
        let location = builder.with_location(imported, 99, source.span.clone());
        let span = builder.with_location(imported, source.id, Span { start: 1, finish: 2 });
        let notes = builder.with_trivia(imported, Some(flow_syntax::syntax::Trivia { notes: vec!["note".into()], ..Default::default() }));
        let metadata = builder.with_trivia(imported, Some(flow_syntax::syntax::Trivia { meta: vec!["tag".into()], ..Default::default() }));
        let tail = builder.with_trivia(imported, Some(flow_syntax::syntax::Trivia { tail: vec!["tail".into()], ..Default::default() }));
        let syntax = builder.freeze();
        for changed in [different, location, span, notes, metadata, tail] {
            assert_ne!(expected, term(&syntax.handle(changed)));
        }
    }

    #[test]
    fn deep_drop_has_no_recursive_owners_and_handles_have_stable_distinct_identity() {
        let mut builder = TermBuilder::new();
        let form = form(&mut builder);
        let mut row = builder.push(None, &Ty::Int, form, Draft::Lit(param::Value::Int(1)));
        let first = row;
        for _ in 0..100_000 { row = builder.push(None, &Ty::Int, form, Draft::Bypass(row)); }
        let arena = builder.freeze();
        let weak = Arc::downgrade(&arena);
        let mut handle = arena.handle(first);
        let first_identity = handle.identity();
        handle = arena.handle(row);
        assert_ne!(first_identity, handle.identity(), "reused stack slots must never be term cache keys");
        let identity = handle.identity();
        let moved = vec![handle].pop().unwrap();
        assert_eq!(identity, moved.identity());
        drop(arena);
        drop(moved);
        assert!(weak.upgrade().is_none());
    }

    #[test]
    #[should_panic(expected = "term edges must refer to earlier rows")]
    fn forward_edges_cannot_create_cycles() {
        let mut builder = TermBuilder::new();
        let form = form(&mut builder);
        builder.push(None, &Ty::Any, form, Draft::Bypass(TermId(0)));
    }
}
