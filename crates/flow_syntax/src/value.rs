use crate::{packed_ops, symbol::{self, Symbol}, ty::{self, Ty}};
use rays_diagnostic::{Diagnostic, Severity};
use rays_math::text_array::TextArray;
use std::sync::{Arc, LazyLock, Mutex, Weak};

/// Function/residual IDs carry their home evaluation arena so resolution never enters a different evaluation. Equality, hashing and display use the deterministic ID alone, preserving plan cache keys across evaluations.
#[derive(Clone, Copy)]
pub struct FnId(pub i64, pub i64);
#[derive(Clone, Copy)]
pub struct ResidualId(pub i64, pub i64);
macro_rules! by_id {
    ($t:ident) => {
        impl PartialEq for $t { fn eq(&self, other: &Self) -> bool { self.0 == other.0 } }
        impl Eq for $t {}
        impl std::hash::Hash for $t { fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.0.hash(state) } }
        impl std::fmt::Debug for $t { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}({})", stringify!($t), self.0) } }
    };
}
by_id!(FnId);
by_id!(ResidualId);

// Sorted value storage is private. Public iteration follows authored order through the shared shape.
struct RecordShape {
    authored: Box<[(Symbol, u32)]>,
}
struct Shapes {
    slots: [std::sync::Weak<RecordShape>; 128],
    next: usize,
}
// ponytail: 128 weak hot shapes; index this bounded ring if shape lookup dominates a profile.
static SHAPES: LazyLock<std::sync::Mutex<Shapes>> = LazyLock::new(|| std::sync::Mutex::new(Shapes {
    slots: std::array::from_fn(|_| std::sync::Weak::new()), next: 0,
}));

fn record_shape(fields: &[(Symbol, Value)]) -> Arc<RecordShape> {
    let mut shapes = SHAPES.lock().unwrap_or_else(|e| e.into_inner());
    for slot in &shapes.slots {
        if let Some(shape) = slot.upgrade() {
            if shape.authored.len() == fields.len() && shape.authored.iter().zip(fields).all(|((name, _), field)| *name == field.0) {
                return shape;
            }
        }
    }
    let mut indices: Vec<usize> = (0..fields.len()).collect();
    indices.sort_unstable_by_key(|&ordinal| (fields[ordinal].0.id(), ordinal));
    let mut authored: Vec<(Symbol, u32)> = fields.iter().map(|(name, _)| (*name, 0)).collect();
    for (sorted, ordinal) in indices.into_iter().enumerate() {
        authored[ordinal].1 = u32::try_from(sorted).expect("record field limit exceeded");
    }
    let shape = Arc::new(RecordShape { authored: authored.into_boxed_slice() });
    let next = shapes.next;
    shapes.slots[next] = Arc::downgrade(&shape);
    shapes.next = (next + 1) % shapes.slots.len();
    shape
}

#[derive(Clone)]
pub struct Record {
    fields: Arc<[(Symbol, Value)]>,
    shape: Arc<RecordShape>,
}
impl Record {
    pub fn new(mut fields: Vec<(Symbol, Value)>) -> Self {
        static EMPTY: LazyLock<Record> = LazyLock::new(|| Record {
            fields: Arc::from([]), shape: Arc::new(RecordShape { authored: Box::new([]) }),
        });
        if fields.is_empty() { return EMPTY.clone(); }
        let shape = record_shape(&fields);
        // Stable sorting preserves first-match duplicate semantics at host construction boundaries.
        fields.sort_by_key(|(name, _)| name.id());
        Self { fields: fields.into(), shape }
    }
    pub fn len(&self) -> usize { self.fields.len() }
    pub fn is_empty(&self) -> bool { self.fields.is_empty() }
    pub fn iter(&self) -> std::iter::Map<
        std::iter::Zip<std::iter::RepeatN<&[(Symbol, Value)]>, std::slice::Iter<'_, (Symbol, u32)>>,
        for<'a> fn((&'a [(Symbol, Value)], &'a (Symbol, u32))) -> &'a (Symbol, Value),
    > {
        std::iter::repeat_n(self.fields.as_ref(), self.len()).zip(self.shape.authored.iter()).map(record_field)
    }
    pub fn get(&self, name: Symbol) -> Option<&Value> {
        let at = self.fields.partition_point(|(key, _)| key.id() < name.id());
        self.fields.get(at).filter(|(key, _)| *key == name).map(|(_, value)| value)
    }
    pub fn get_str(&self, name: &str) -> Option<&Value> { symbol::find(name).and_then(|name| self.get(name)) }
    pub fn field_at(&self, index: usize) -> Option<&(Symbol, Value)> {
        self.shape.authored.get(index).map(|(_, sorted)| &self.fields[*sorted as usize])
    }
    pub fn insert(&mut self, index: usize, field: (Symbol, Value)) {
        let mut fields: Vec<_> = self.iter().cloned().collect();
        fields.insert(index, field);
        *self = Self::new(fields);
    }
    /// Returns whether this field owner is new to the report; shape owners are deduplicated too.
    pub fn attribute(&self, memory: &mut crate::syntax::Memory) -> bool {
        let fresh = memory.allocation(self.fields.as_ptr(), std::mem::size_of_val(&*self.fields) + 2 * std::mem::size_of::<usize>());
        if memory.allocation(Arc::as_ptr(&self.shape), std::mem::size_of::<RecordShape>() + 2 * std::mem::size_of::<usize>()) {
            memory.allocation(self.shape.authored.as_ptr(), std::mem::size_of_val(&*self.shape.authored));
        }
        fresh
    }
    pub fn set(&mut self, name: Symbol, value: Value) {
        let at = self.fields.partition_point(|(key, _)| key.id() < name.id());
        if self.fields.get(at).is_some_and(|(key, _)| *key == name) {
            Arc::make_mut(&mut self.fields)[at].1 = value;
        } else {
            let mut fields: Vec<_> = self.iter().cloned().collect();
            fields.push((name, value));
            *self = Self::new(fields);
        }
    }
}
// The standard iterator pairs sorted storage with authored shape order without allocating.
fn record_field<'a>((fields, (_, sorted)): (&'a [(Symbol, Value)], &'a (Symbol, u32))) -> &'a (Symbol, Value) {
    &fields[*sorted as usize]
}
impl std::fmt::Debug for Record {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.debug_list().entries(self.iter()).finish() }
}
impl PartialEq for Record { fn eq(&self, other: &Self) -> bool { self.iter().eq(other.iter()) } }
impl IntoIterator for Record {
    type Item = (Symbol, Value);
    type IntoIter = std::vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter { self.iter().cloned().collect::<Vec<_>>().into_iter() }
}
impl<'a> IntoIterator for &'a Record {
    type Item = &'a (Symbol, Value);
    type IntoIter = std::iter::Map<
        std::iter::Zip<std::iter::RepeatN<&'a [(Symbol, Value)]>, std::slice::Iter<'a, (Symbol, u32)>>,
        for<'b> fn((&'b [(Symbol, Value)], &'b (Symbol, u32))) -> &'b (Symbol, Value),
    >;
    fn into_iter(self) -> Self::IntoIter { self.iter() }
}

impl Default for Record { fn default() -> Self { Self::new(Vec::new()) } }
impl From<Vec<(Symbol, Value)>> for Record {
    fn from(fields: Vec<(Symbol, Value)>) -> Self { Self::new(fields) }
}
impl std::ops::Index<usize> for Record {
    type Output = (Symbol, Value);
    fn index(&self, index: usize) -> &Self::Output { &self.fields[self.shape.authored[index].1 as usize] }
}

/// Immutable Struct/Source payload; shared so cloning a Value never copies its fields.
#[derive(Clone, Debug, PartialEq)]
pub struct GeoPayload {
    pub name: String,
    pub ty: Ty,
    pub fields: Record,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Text(Arc<[u8]>),
    Vec2(f64, f64),
    Vec3(f64, f64, f64),
    Vec4(f64, f64, f64, f64),
    List(Arc<Vec<Value>>),
    Int_array(Arc<Vec<i64>>),
    Text_array(Arc<TextArray>),
    /// Packed float/vector rows; width is 1..=4 and storage must contain complete rows.
    Floats(usize, Arc<Vec<f64>>),
    Record(Record),
    Deferred(Ty, i64),
    No_geo,
    Struct(Arc<GeoPayload>),
    Source(Arc<GeoPayload>),
    Fn(FnId),
    Residual(ResidualId),
}

pub type T = Value;

pub fn struct_value(name: impl Into<String>, ty: Ty, fields: Record) -> Value {
    Value::Struct(Arc::new(GeoPayload { name: name.into(), ty, fields }))
}
pub fn source_value(name: impl Into<String>, ty: Ty, fields: Record) -> Value {
    Value::Source(Arc::new(GeoPayload { name: name.into(), ty, fields }))
}

impl Value {
    /// Record-like fields for Record / Struct / Source (plan M14).
    pub fn geo_fields(&self) -> Option<&Record> {
        match self {
            Value::Record(fields) => Some(fields),
            Value::Struct(g) | Value::Source(g) => Some(&g.fields),
            _ => None,
        }
    }
    pub fn geo_payload(&self) -> Option<&GeoPayload> {
        match self {
            Value::Struct(g) | Value::Source(g) => Some(g),
            _ => None,
        }
    }
    /// A host data source awaiting resolution: any `Source`, or a `Struct` of array type.
    pub fn is_data_source(&self) -> bool {
        match self {
            Value::Source(_) => true,
            Value::Struct(g) => matches!(g.ty, Ty::Array(_)),
            _ => false,
        }
    }
}

fn diagnostic(code: &str, message: &str) -> Diagnostic {
    Diagnostic {
        code: code.to_owned(), severity: Severity::ErrorSeverity, position: None,
        message: message.to_owned(), span: None, cause: None, hints: Vec::new(), trace: Vec::new(),
    }
}
pub fn raise_diagnostic(diagnostic: Diagnostic) -> Result<Value, Diagnostic> { Err(diagnostic) }
pub fn fail(code: &str, message: &str) -> Diagnostic { diagnostic(code, message) }
pub fn failf(code: &str, message: &str) -> Diagnostic { diagnostic(code, message) }

pub fn hash(values: &[f64]) -> f64 {
    rays_math::hash::float_sequence(values)
}

fn round(value: f64) -> f64 { (value + 0.5).floor() }
pub fn to_int(name: &str, value: f64) -> Result<i64, Diagnostic> {
    if value.is_finite() && value.abs() < 4.0e18 { Ok(value.trunc() as i64) }
    else { Err(failf("E_NONFINITE", &format!("{name} produced a nonfinite value."))) }
}
pub fn fin(name: &str, value: f64) -> Result<f64, Diagnostic> {
    if value.is_finite() { Ok(value) } else { Err(failf("E_NONFINITE", &format!("{name} produced a nonfinite value."))) }
}
pub fn num(value: &Value) -> Result<f64, Diagnostic> {
    match value {
        Value::Int(n) => Ok(*n as f64),
        Value::Float(n) => Ok(*n),
        Value::Bool(value) => Ok(if *value { 1.0 } else { 0.0 }),
        _ => Err(fail("E_TYPE", "Expected a number.")),
    }
}
pub fn truthy(value: &Value) -> Result<bool, Diagnostic> {
    match value {
        Value::Bool(value) => Ok(*value),
        Value::Int(value) => Ok(*value != 0),
        Value::Float(value) => Ok(*value != 0.0),
        _ => Err(fail("E_TYPE", "Expected a bool.")),
    }
}
pub fn int_of(value: &Value) -> Result<i64, Diagnostic> {
    match value { Value::Int(n) => Ok(*n), _ => to_int("int", round(num(value)?)) }
}

pub fn ty_of(value: &Value) -> Ty {
    match value {
        Value::Int(_) => Ty::Int,
        Value::Float(_) => Ty::Float,
        Value::Bool(_) => Ty::Bool,
        Value::Text(_) => Ty::Text,
        Value::Vec2(..) => Ty::Vec2,
        Value::Vec3(..) => Ty::Vec3,
        Value::Vec4(..) => Ty::Vec4,
        Value::List(values) => Ty::List(Box::new(elem_ty(values))),
        Value::Int_array(_) => Ty::Array(Box::new(Ty::Int)),
        Value::Text_array(_) => Ty::Array(Box::new(Ty::Text)),
        Value::Floats(width, _) => Ty::Array(Box::new(match width { 1 => Ty::Float, 2 => Ty::Vec2, 3 => Ty::Vec3, 4 => Ty::Vec4, _ => Ty::Any })),
        Value::Record(fields) => Ty::Record(fields.iter().map(|(name, value)| (*name, ty_of(value))).collect()),
        Value::Deferred(ty, _) => ty.clone(),
        Value::Struct(g) | Value::Source(g) => g.ty.clone(),
        Value::No_geo => ty::geometry(),
        Value::Fn(_) => Ty::Fn(None),
        Value::Residual(_) => Ty::Any,
    }
}
pub fn elem_ty(values: &[Value]) -> Ty {
    let mut current = Ty::Any;
    for value in values {
        // a deferred/struct element already of the running type needs no clone and no join
        if let Value::Deferred(ty, _) = value { if *ty == current { continue; } }
        if let Value::Struct(g) | Value::Source(g) = value { if g.ty == current { continue; } }
        current = ty::join(&current, &ty_of(value)).unwrap_or(current);
    }
    current
}

pub fn coerce_to(want: &Ty, value: &Value) -> Result<Value, Diagnostic> {
    match (want, value) {
        (Ty::Any, _) | (_, Value::Residual(_) | Value::Source(..)) => Ok(value.clone()),
        (Ty::Int, Value::Float(f)) => Ok(Value::Int(to_int("int", round(*f))?)),
        (Ty::Int, Value::Bool(value)) => Ok(Value::Int(i64::from(*value))),
        (Ty::Float, Value::Int(value)) => Ok(Value::Float(*value as f64)),
        (Ty::Float, Value::Bool(value)) => Ok(Value::Float(if *value { 1.0 } else { 0.0 })),
        (Ty::Bool, Value::Int(_) | Value::Float(_)) => Ok(Value::Bool(truthy(value)?)),
        (Ty::Vec3, Value::Int(_) | Value::Float(_)) => { let n = num(value)?; Ok(Value::Vec3(n, n, n)) },
        (Ty::Vec2, Value::Int(_) | Value::Float(_)) => { let n = num(value)?; Ok(Value::Vec2(n, n)) },
        (Ty::Vec4, Value::Int(_) | Value::Float(_)) => { let n = num(value)?; Ok(Value::Vec4(n, n, n, n)) },
        (Ty::List(element), Value::List(values)) => Ok(Value::List(Arc::new(values.iter().map(|value| coerce_to(element, value)).collect::<Result<Vec<_>, _>>()?))),
        (Ty::Array(element), Value::Int_array(values)) if **element == Ty::Float => Ok(Value::Floats(1, Arc::new(values.iter().map(|value| *value as f64).collect()))),
        (Ty::Record(wanted), Value::Record(fields)) => Ok(Value::Record(fields.iter().map(|(name, value)| {
            Ok((*name, wanted.iter().find(|(field, _)| field == name).map_or_else(|| Ok(value.clone()), |(_, want)| coerce_to(want, value))?))
        }).collect::<Result<Vec<_>, Diagnostic>>()?.into())),
        _ => Ok(value.clone()),
    }
}

pub fn show_with(conc: &dyn Fn(&Value) -> Value, value: &Value) -> Vec<u8> {
    match conc(value) {
        Value::Int(n) => n.to_string().into_bytes(),
        Value::Float(n) => param::float_text(n).into_bytes(),
        Value::Bool(value) => if value { b"true".to_vec() } else { b"false".to_vec() },
        Value::Text(text) => text.to_vec(),
        Value::Vec2(x, y) => format!("[{} {}]", param::float_text(x), param::float_text(y)).into_bytes(),
        Value::Vec3(x, y, z) => format!("[{} {} {}]", param::float_text(x), param::float_text(y), param::float_text(z)).into_bytes(),
        Value::Vec4(x, y, z, w) => format!("[{} {} {} {}]", param::float_text(x), param::float_text(y), param::float_text(z), param::float_text(w)).into_bytes(),
        Value::List(values) => {
            let mut output = vec![b'['];
            for (index, value) in values.iter().enumerate() {
                if index > 0 { output.push(b' '); }
                output.extend(show_with(conc, value));
            }
            output.push(b']');
            output
        }
        Value::Int_array(values) => format!("{} × int", values.len()).into_bytes(),
        Value::Text_array(values) => format!("{} × text", values.len()).into_bytes(),
        value @ Value::Floats(width, _) => format!("{} × {}", array_length(&value).unwrap_or(0), match width { 1 => "float", 2 => "vec2", 3 => "vec3", 4 => "vec4", _ => "any" }).into_bytes(),
        Value::Record(fields) => {
            let mut output = vec![b'{'];
            for (index, (name, value)) in fields.iter().enumerate() {
                if index > 0 { output.push(b' '); }
                output.push(b':');
                output.extend(name.as_bytes());
                output.push(b' ');
                output.extend(show_with(conc, value));
            }
            output.push(b'}');
            output
        }
        Value::Residual(_) => b"?".to_vec(),
        value => ty::to_string(&ty_of(&value)).into_bytes(),
    }
}

// ponytail: fixed 32-slot weak identity rings match the source's bounded array caches.
#[derive(Clone)]
enum MemoKey {
    Int(Weak<Vec<i64>>),
    Text(Weak<TextArray>),
    Float(Weak<Vec<f64>>),
}
impl MemoKey {
    fn int(values: &Arc<Vec<i64>>) -> Self { Self::Int(Arc::downgrade(values)) }
    fn text(values: &Arc<TextArray>) -> Self { Self::Text(Arc::downgrade(values)) }
    fn float(values: &Arc<Vec<f64>>) -> Self { Self::Float(Arc::downgrade(values)) }
    fn live(&self) -> bool {
        match self { Self::Int(key) => key.strong_count() > 0, Self::Text(key) => key.strong_count() > 0, Self::Float(key) => key.strong_count() > 0 }
    }
    fn same(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => a.upgrade().zip(b.upgrade()).is_some_and(|(a, b)| Arc::ptr_eq(&a, &b)),
            (Self::Text(a), Self::Text(b)) => a.upgrade().zip(b.upgrade()).is_some_and(|(a, b)| Arc::ptr_eq(&a, &b)),
            (Self::Float(a), Self::Float(b)) => a.upgrade().zip(b.upgrade()).is_some_and(|(a, b)| Arc::ptr_eq(&a, &b)),
            _ => false,
        }
    }
}
struct DigestEntry { key: MemoKey, digest: String }
struct DigestData { slots: [Option<DigestEntry>; 32], next: usize }
impl DigestData {
    fn new() -> Self { Self { slots: std::array::from_fn(|_| None), next: 0 } }
    fn get(&self, key: &MemoKey) -> Option<String> { self.slots.iter().flatten().find(|entry| entry.key.same(key)).map(|entry| entry.digest.clone()) }
    fn insert(&mut self, key: MemoKey, digest: String) {
        let next = self.next;
        self.slots[next] = Some(DigestEntry { key, digest });
        self.next = (next + 1) % self.slots.len();
    }
    fn retained_bytes(&mut self) -> usize {
        self.slots.iter_mut().map(|slot| match slot {
            Some(entry) if entry.key.live() => std::mem::size_of::<DigestEntry>() + entry.digest.capacity(),
            Some(_) => { *slot = None; 0 },
            None => 0,
        }).sum()
    }
}
struct DigestCache { data: Mutex<DigestData> }
impl DigestCache {
    fn new() -> Self { Self { data: Mutex::new(DigestData::new()) } }
    fn get_or_compute(&self, key: MemoKey, compute: &dyn Fn() -> String) -> String {
        if let Some(digest) = self.data.lock().unwrap_or_else(|poisoned| poisoned.into_inner()).get(&key) { return digest; }
        let digest = compute();
        let mut data = self.data.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
        if let Some(existing) = data.get(&key) { return existing; }
        data.insert(key, digest.clone());
        digest
    }
    fn retained_bytes(&self) -> usize { self.data.lock().unwrap_or_else(|poisoned| poisoned.into_inner()).retained_bytes() }
}

struct FiniteEntry { key: Weak<Vec<f64>> }
struct FiniteData { slots: [Option<FiniteEntry>; 32], next: usize }
impl FiniteData {
    fn new() -> Self { Self { slots: std::array::from_fn(|_| None), next: 0 } }
    fn contains(&self, key: &Arc<Vec<f64>>) -> bool {
        self.slots.iter().flatten().any(|entry| entry.key.upgrade().is_some_and(|old| Arc::ptr_eq(&old, key)))
    }
    fn insert(&mut self, key: &Arc<Vec<f64>>) {
        let next = self.next;
        self.slots[next] = Some(FiniteEntry { key: Arc::downgrade(key) });
        self.next = (next + 1) % self.slots.len();
    }
    fn retained_bytes(&mut self) -> usize {
        self.slots.iter_mut().map(|slot| match slot {
            Some(entry) if entry.key.strong_count() > 0 => std::mem::size_of::<FiniteEntry>(),
            Some(_) => { *slot = None; 0 },
            None => 0,
        }).sum()
    }
}
struct FiniteArrayCache { data: Mutex<FiniteData> }
impl FiniteArrayCache {
    fn new() -> Self { Self { data: Mutex::new(FiniteData::new()) } }
    fn validate_once(&self, key: &Arc<Vec<f64>>, check: &dyn Fn() -> Result<(), Diagnostic>) -> Result<(), Diagnostic> {
        if self.data.lock().unwrap_or_else(|poisoned| poisoned.into_inner()).contains(key) { return Ok(()); }
        check()?;
        let mut data = self.data.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
        if !data.contains(key) { data.insert(key); }
        Ok(())
    }
    fn retained_bytes(&self) -> usize { self.data.lock().unwrap_or_else(|poisoned| poisoned.into_inner()).retained_bytes() }
}

static DIGESTS: LazyLock<DigestCache> = LazyLock::new(DigestCache::new);
static FINITE_ARRAYS: LazyLock<FiniteArrayCache> = LazyLock::new(FiniteArrayCache::new);
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Retained { pub owner: &'static str, pub bytes: usize }
pub fn remembered() -> Vec<Retained> {
    [
        Retained { owner: "array_key", bytes: DIGESTS.retained_bytes() },
        Retained { owner: "finite_arrays", bytes: FINITE_ARRAYS.retained_bytes() },
    ].into_iter().filter(|entry| entry.bytes > 0).collect()
}
pub fn retained_bytes() -> usize { DIGESTS.retained_bytes() + FINITE_ARRAYS.retained_bytes() }

fn digest_float_array(values: &[f64]) -> String {
    let mut hash = rays_math::hash::word(rays_math::hash::SEED, values.len() as u64);
    for value in values { hash = rays_math::hash::word(hash, value.to_bits()); }
    format!("{hash:016x}")
}
fn digest_int_array(values: &[i64]) -> String {
    let mut hash = rays_math::hash::word(rays_math::hash::SEED, values.len() as u64);
    for value in values { hash = rays_math::hash::word(hash, *value as u64); }
    format!("{hash:016x}")
}
fn digest_text_array(values: &TextArray) -> String {
    let mut hash = rays_math::hash::word(rays_math::hash::SEED, values.len() as u64);
    for value in values.iter() { hash = rays_math::hash::bytes(hash, value); }
    format!("{hash:016x}")
}
fn memo_digest(key: MemoKey, digest: &dyn Fn() -> String) -> String { DIGESTS.get_or_compute(key, digest) }
fn key_float_array(values: &Arc<Vec<f64>>, tag: &str) -> Vec<u8> {
    if values.len() <= 4096 { return format!("{tag}[{}]", values.iter().map(|value| format!("{:016x}", value.to_bits())).collect::<Vec<_>>().join(",")).into_bytes(); }
    let digest = memo_digest(MemoKey::float(values), &|| digest_float_array(values));
    format!("{tag}#{}:{digest}", values.len()).into_bytes()
}
fn key_int_array(values: &Arc<Vec<i64>>) -> Vec<u8> {
    if values.len() <= 4096 { return format!("AI[{}]", values.iter().map(ToString::to_string).collect::<Vec<_>>().join(",")).into_bytes(); }
    let digest = memo_digest(MemoKey::int(values), &|| digest_int_array(values));
    format!("AI#{}:{digest}", values.len()).into_bytes()
}
fn key_text_array(values: &Arc<TextArray>) -> Vec<u8> {
    if values.len() <= 4096 {
        let mut key = b"AT[".to_vec();
        for (index, text) in values.iter().enumerate() {
            if index > 0 { key.push(b','); }
            key.extend(text.len().to_string().bytes());
            key.push(b':');
            key.extend_from_slice(text);
        }
        key.push(b']');
        return key;
    }
    let digest = memo_digest(MemoKey::text(values), &|| digest_text_array(values));
    format!("AT#{}:{digest}", values.len()).into_bytes()
}

pub fn key_of(residual: &dyn Fn(ResidualId) -> i64, value: &Value) -> Vec<u8> {
    match value {
        Value::Int(n) => format!("i{n}").into_bytes(),
        Value::Float(n) => format!("f{:016x}", n.to_bits()).into_bytes(),
        Value::Bool(value) => if *value { b"T".to_vec() } else { b"F".to_vec() },
        Value::Text(text) => {
            let mut key = format!("s{}:", text.len()).into_bytes();
            key.extend_from_slice(text);
            key
        }
        Value::Vec2(a, b) => format!("v2:{:016x},{:016x}", a.to_bits(), b.to_bits()).into_bytes(),
        Value::Vec3(a, b, c) => format!("v{:016x},{:016x},{:016x}", a.to_bits(), b.to_bits(), c.to_bits()).into_bytes(),
        Value::Vec4(a, b, c, d) => format!("v4:{:016x},{:016x},{:016x},{:016x}", a.to_bits(), b.to_bits(), c.to_bits(), d.to_bits()).into_bytes(),
        Value::List(values) => {
            let mut key = vec![b'['];
            for (index, value) in values.iter().enumerate() {
                if index > 0 { key.push(b','); }
                key.extend(key_of(residual, value));
            }
            key.push(b']');
            key
        }
        Value::Int_array(values) => key_int_array(values),
        Value::Text_array(values) => key_text_array(values),
        Value::Floats(width, values) => key_float_array(values, match width { 1 => "A", 2 => "V2", 3 => "V", 4 => "V4", _ => return key_float_array(values, &format!("invalid{width}")) }),
        Value::Record(fields) => {
            let mut key = vec![b'{'];
            for (index, (name, value)) in fields.iter().enumerate() {
                if index > 0 { key.push(b','); }
                key.extend(name.bytes());
                key.push(b'=');
                key.extend(key_of(residual, value));
            }
            key.push(b'}');
            key
        }
        Value::Deferred(ty, id) if *ty == ty::geometry() => format!("g{id}").into_bytes(),
        Value::Deferred(ty, id) if *ty == ty::image() => format!("image:{id}").into_bytes(),
        Value::Deferred(ty, id) => format!("node:{}:{id}", ty::to_string(ty)).into_bytes(),
        Value::No_geo => b"G".to_vec(),
        Value::Struct(g) => {
            let mut key = format!("S{}", g.name).into_bytes();
            key.extend(key_of(residual, &Value::Record(g.fields.clone())));
            key
        }
        Value::Source(g) => {
            let mut key = format!("source:{}:{}", g.name, ty::to_string(&g.ty)).into_bytes();
            key.extend(key_of(residual, &Value::Record(g.fields.clone())));
            key
        }
        Value::Fn(_) => b"fn".to_vec(),
        Value::Residual(id) => format!("r{}", residual(*id)).into_bytes(),
    }
}

pub fn comps(value: &Value) -> Result<(f64, f64, f64), Diagnostic> {
    match value { Value::Vec3(x, y, z) => Ok((*x, *y, *z)), _ => { let n = num(value)?; Ok((n, n, n)) } }
}
pub fn is_vec(value: &Value) -> bool { matches!(value, Value::Vec2(..) | Value::Vec3(..) | Value::Vec4(..)) }
pub fn arith(name: &str, operation: &dyn Fn(f64, f64) -> f64, a: &Value, b: &Value) -> Result<Value, Diagnostic> {
    if matches!(a, Value::Vec2(..) | Value::Vec4(..)) || matches!(b, Value::Vec2(..) | Value::Vec4(..)) {
        let ty = ty_of(if is_vec(a) { a } else { b });
        let apply = |x, y| fin(name, operation(x, y));
        let promote = |value: &Value| if is_vec(value) { Ok(value.clone()) } else { coerce_to(&ty, &Value::Float(num(value)?)) };
        return match (promote(a)?, promote(b)?) {
            (Value::Vec2(ax, ay), Value::Vec2(bx, by)) => Ok(Value::Vec2(apply(ax, bx)?, apply(ay, by)?)),
            (Value::Vec4(ax, ay, az, aw), Value::Vec4(bx, by, bz, bw)) => Ok(Value::Vec4(apply(ax, bx)?, apply(ay, by)?, apply(az, bz)?, apply(aw, bw)?)),
            _ => Err(fail("E_TYPE", "Vector arithmetic needs matching widths or a number.")),
        };
    }
    if is_vec(a) || is_vec(b) {
        let (ax, ay, az) = comps(a)?;
        let (bx, by, bz) = comps(b)?;
        Ok(Value::Vec3(fin(name, operation(ax, bx))?, fin(name, operation(ay, by))?, fin(name, operation(az, bz))?))
    } else {
        let result = fin(name, operation(num(a)?, num(b)?))?;
        if matches!((a, b), (Value::Int(_), Value::Int(_))) && name != "/" && name != "pow" { Ok(Value::Int(to_int(name, round(result))?)) }
        else { Ok(Value::Float(result)) }
    }
}

pub fn list_arg(value: &Value) -> Result<&[Value], Diagnostic> {
    match value {
        Value::List(values) => Ok(values),
        Value::Deferred(ty, _) if *ty == ty::geometry() => Err(fail("E_TYPE", "A loop over geometry yields its merged geometry, not a list; give it to sop/merge.")),
        _ => Err(fail("E_TYPE", "Expected a list.")),
    }
}
pub fn array_length(value: &Value) -> Result<usize, Diagnostic> {
    match value {
        Value::Int_array(values) => Ok(values.len()),
        Value::Text_array(values) => Ok(values.len()),
        Value::Floats(width @ 1..=4, values) if values.len() % width == 0 => Ok(values.len() / width),
        Value::Floats(width, _) => Err(fail("E_ARRAY_TYPE", match width {
            2 => "Packed vec2 storage has two coordinates per element.",
            3 => "Packed vec3 storage has three coordinates per element.",
            4 => "Packed vec4 storage has four coordinates per element.",
            _ => "Packed float storage needs a width from one to four.",
        })),
        _ => Err(fail("E_ARRAY_TYPE", "Expected a packed array.")),
    }
}
pub fn array_get(value: &Value, index: i64) -> Result<Value, Diagnostic> {
    let length = array_length(value)?;
    if index < 0 || index as u128 >= length as u128 { return Err(fail("E_ARRAY_RANGE", "Array index is outside its length.")); }
    let index = index as usize;
    let value = match value {
        Value::Int_array(values) => Value::Int(values[index]),
        Value::Text_array(values) => { let mut text = array_buffer(values[index].len())?; text.extend_from_slice(&values[index]); Value::Text(text.into()) },
        Value::Floats(1, values) => Value::Float(values[index]),
        Value::Floats(2, values) => Value::Vec2(values[index * 2], values[index * 2 + 1]),
        Value::Floats(3, values) => Value::Vec3(values[index * 3], values[index * 3 + 1], values[index * 3 + 2]),
        Value::Floats(4, values) => Value::Vec4(values[index * 4], values[index * 4 + 1], values[index * 4 + 2], values[index * 4 + 3]),
        _ => return Err(fail("E_ARRAY_TYPE", "Expected a packed array.")),
    };
    Ok(value)
}
pub fn array_put(data: &mut [f64], width: usize, index: usize, value: &Value) -> Result<(), Diagnostic> {
    let at = width * index;
    match (width, value) {
        (1, _) => data[at] = fin("array", num(value)?)?,
        (2, Value::Vec2(x, y)) => { data[at] = fin("array", *x)?; data[at + 1] = fin("array", *y)?; },
        (3, Value::Vec3(x, y, z)) => { data[at] = fin("array", *x)?; data[at + 1] = fin("array", *y)?; data[at + 2] = fin("array", *z)?; },
        (4, Value::Vec4(x, y, z, w)) => { data[at] = fin("array", *x)?; data[at + 1] = fin("array", *y)?; data[at + 2] = fin("array", *z)?; data[at + 3] = fin("array", *w)?; },
        (_, Value::Int(_) | Value::Float(_) | Value::Bool(_)) => { let value = fin("array", num(value)?)?; for slot in &mut data[at..at + width] { *slot = value; } },
        _ => return Err(fail("E_ARRAY_TYPE", "Packed array element has the wrong vector width.")),
    }
    Ok(())
}
pub fn int_array_put(data: &mut [i64], index: usize, value: &Value) -> Result<(), Diagnostic> {
    match coerce_to(&Ty::Int, value)? {
        Value::Int(n) => data[index] = n,
        _ => return Err(fail("E_ARRAY_TYPE", "Packed integer arrays contain integer values.")),
    }
    Ok(())
}
/// Logical element capacity of one native allocation, not a promise of available memory.
pub fn array_max_length(element: &Ty) -> Option<usize> {
    let width = match element {
        Ty::Text => return Some(isize::MAX as usize / size_of::<usize>() - 1),
        Ty::Int | Ty::Float => 1,
        Ty::Vec2 => 2, Ty::Vec3 => 3, Ty::Vec4 => 4,
        _ => return None,
    };
    Some(isize::MAX as usize / size_of::<f64>() / width)
}
fn allocation_count(count: i64) -> Result<usize, Diagnostic> {
    if count < 0 { return Err(fail("E_ARRAY_RANGE", "Array length is outside native storage bounds.")); }
    usize::try_from(count).map_err(|_| fail("E_ARRAY_MEMORY", "Frame data exceeds available memory."))
}
/// Reserve a user-sized result before filling it; allocation failure is a Lisp diagnostic.
/// This cannot recover from an OS killing the process after an overcommitted allocation.
pub fn array_buffer<T>(count: usize) -> Result<Vec<T>, Diagnostic> {
    let mut values = Vec::new();
    values.try_reserve_exact(count).map_err(|_| fail("E_ARRAY_MEMORY", "Frame data exceeds available memory."))?;
    Ok(values)
}
pub fn array_init(element: &Ty, count: i64, f: &dyn Fn(usize) -> Value) -> Result<Value, Diagnostic> {
    if *element == Ty::Text {
        let count = allocation_count(count)?;
        let mut values = TextArray::try_with_capacity(count).map_err(|_| fail("E_ARRAY_MEMORY", "Frame data exceeds available memory."))?;
        for index in 0..count { match f(index) { Value::Text(text) => values.try_push(&text).map_err(|_| fail("E_ARRAY_MEMORY", "Frame data exceeds available memory."))?, _ => return Err(fail("E_ARRAY_TYPE", "Text arrays contain text values.")) } }
        return Ok(Value::Text_array(Arc::new(values)));
    }
    if *element == Ty::Int {
        let count = allocation_count(count)?;
        let mut values = array_buffer(count)?;
        values.resize(count, 0);
        for (index, slot) in values.iter_mut().enumerate() { *slot = match coerce_to(&Ty::Int, &f(index))? { Value::Int(value) => value, _ => return Err(fail("E_ARRAY_TYPE", "Packed integer arrays contain integer values.")) }; }
        return Ok(Value::Int_array(Arc::new(values)));
    }
    let width = match element {
        Ty::Float | Ty::Any => 1,
        Ty::Vec2 | Ty::Vec3 | Ty::Vec4 => match packed_ops::scalar_width(element) {
            Some(width) => width,
            None => return Err(fail("E_ARRAY_TYPE", "Packed arrays contain float, vec2, vec3 or vec4 values.")),
        },
        _ => return Err(fail("E_ARRAY_TYPE", "Packed arrays contain float, vec2, vec3 or vec4 values.")),
    };
    let count = allocation_count(count)?;
    let slots = count.checked_mul(width).ok_or_else(|| fail("E_ARRAY_MEMORY", "Frame data exceeds available memory."))?;
    let mut values = array_buffer(slots)?;
    values.resize(slots, 0.0);
    for index in 0..count { array_put(&mut values, width, index, &f(index))?; }
    let values = Arc::new(values);
    Ok(Value::Floats(width, values))
}

fn check_float_array(values: &[f64]) -> Result<(), Diagnostic> {
    if let Some(value) = values.iter().find(|value| !value.is_finite()) { fin("array input", *value)?; }
    Ok(())
}
pub fn validate(value: &Value) -> Result<(), Diagnostic> {
    match value {
        Value::Float(value) => { fin("input", *value)?; },
        Value::Vec2(x, y) => { fin("input", *x)?; fin("input", *y)?; },
        Value::Vec3(x, y, z) => { fin("input", *x)?; fin("input", *y)?; fin("input", *z)?; },
        Value::Vec4(x, y, z, w) => { fin("input", *x)?; fin("input", *y)?; fin("input", *z)?; fin("input", *w)?; },
        Value::Floats(_, values) => {
            array_length(value)?;
            if values.len() <= 4096 { check_float_array(values)?; }
            else { FINITE_ARRAYS.validate_once(values, &|| check_float_array(values))?; }
        }
        Value::List(values) => for value in values.iter() { validate(value)?; },
        value if value.geo_fields().is_some() => {
            for (_, value) in value.geo_fields().unwrap() { validate(value)?; }
        }
        _ => (),
    }
    Ok(())
}

fn cancelled(diagnostic: &Diagnostic) -> bool { diagnostic.code == "cancelled" || diagnostic.code == "E_CANCELLED" }
fn inherit_envelope(original: &Diagnostic, mut mapped: Diagnostic) -> Diagnostic {
    mapped.hints = original.hints.clone();
    mapped.trace = original.trace.clone();
    if mapped.span.is_none() { mapped.span = original.span.clone(); }
    if mapped.position.is_none() { mapped.position = original.position; }
    mapped
}
fn error_payload(diagnostic: &Diagnostic) -> Value {
    Value::Record(vec![
        ("code".into(), Value::Text(diagnostic.code.as_bytes().into())),
        ("message".into(), Value::Text(diagnostic.message.as_bytes().into())),
        ("cause".into(), Value::List(Arc::new(diagnostic.cause.iter().map(|cause| Value::Text(cause.clone().into())).collect()))),
    ].into())
}
pub fn with_error(protected: &dyn Fn() -> Result<Value, Diagnostic>, map: &dyn Fn(Value) -> Result<Value, Diagnostic>) -> Result<Value, Diagnostic> {
    let original = match protected() { Ok(value) => return Ok(value), Err(error) => error };
    if cancelled(&original) { return Err(original); }
    let mapped = match map(error_payload(&original)) {
        Ok(value) => value,
        Err(mapped) if cancelled(&mapped) => return Err(mapped),
        Err(mapped) => return Err(inherit_envelope(&original, mapped)),
    };
    let malformed = || {
        let mut error = original.clone();
        error.code = "E_DIAGNOSTIC_MAP".into();
        error.message = "An error mapper returns code, message and zero or one text causes.".into();
        error
    };
    let Value::Record(fields) = mapped else { return Err(malformed()); };
    if fields.len() != 3 { return Err(malformed()); }
    let Some(Value::Text(code)) = fields.iter().find(|(name, _)| name == "code").map(|(_, value)| value) else { return Err(malformed()); };
    let Ok(code) = String::from_utf8(code.to_vec()) else { return Err(malformed()); };
    let Some(Value::Text(message)) = fields.iter().find(|(name, _)| name == "message").map(|(_, value)| value) else { return Err(malformed()); };
    let Ok(message) = String::from_utf8(message.to_vec()) else { return Err(malformed()); };
    let Some(Value::List(causes)) = fields.iter().find(|(name, _)| name == "cause").map(|(_, value)| value) else { return Err(malformed()); };
    let cause = match causes.as_slice() {
        [] => None,
        [Value::Text(cause)] => Some(cause.to_vec()),
        _ => return Err(malformed()),
    };
    let mut result = original;
    result.code = code;
    result.message = message;
    result.cause = cause;
    Err(result)
}

pub fn error_row_index(value: &Value, grain: Option<i64>, check: Option<&dyn Fn() -> Result<(), Diagnostic>>) -> Result<Option<i64>, Diagnostic> {
    let Value::Int_array(rows) = value else { return Err(fail("E_DIAGNOSTIC_ROWS", "Diagnostic rows require an integer 0/1 array.")); };
    if grain.is_some_and(|grain| grain <= 0) { return Err(fail("E_DIAGNOSTIC_CONTEXT", "Diagnostic grain must be positive.")); }
    if let Some(check) = check { check()?; }
    let mut first = None;
    for (index, value) in rows.iter().enumerate() {
        if index & 4095 == 0 { if let Some(check) = check { check()?; } }
        if *value != 0 && *value != 1 { return Err(fail("E_DIAGNOSTIC_ROWS", "Diagnostic row values must be 0 or 1.")); }
        if *value == 1 && first.is_none() { first = Some(index); }
    }
    let Some(first) = first else { return Ok(None); };
    let Some(grain) = grain else { return Ok(Some(first as i64)); };
    let grain = grain as usize;
    let last = first + (grain - first % grain).min(rows.len() - first);
    let mut selected = first;
    for index in first + 1..last {
        if index & 4095 == 0 { if let Some(check) = check { check()?; } }
        if rows[index] == 1 { selected = index; }
    }
    if let Some(check) = check { check()?; }
    Ok(Some(selected as i64))
}
pub fn diagnose_rows(select: &dyn Fn(&Value, i64) -> i64, map: &dyn Fn(Value) -> Result<Value, Diagnostic>, value: &Value) -> Result<Value, Diagnostic> {
    let Some(first) = error_row_index(value, None, None)? else { return Ok(Value::No_geo); };
    let index = select(value, first);
    match value {
        Value::Int_array(rows) if index >= 0 && (index as u128) < rows.len() as u128 && rows[index as usize] == 1 => (),
        _ => return Err(fail("E_DIAGNOSTIC_CONTEXT", "Host diagnosis returned a non-failing row.")),
    }
    with_error(&|| Err(fail("E_DIAGNOSTIC_ROW", "A diagnostic row failed.")), &|_| map(Value::Int(index)))
}

#[cfg(test)]
mod tests {
    #[test]
    fn array_capacities_use_platform_bytes_and_oversize_is_typed_memory() {
        let slots = isize::MAX as usize / size_of::<f64>();
        for (element, expected) in [
            (Ty::Int, slots), (Ty::Float, slots), (Ty::Vec2, slots / 2),
            (Ty::Vec3, slots / 3), (Ty::Vec4, slots / 4),
            (Ty::Text, isize::MAX as usize / size_of::<usize>() - 1),
        ] {
            assert_eq!(array_max_length(&element), Some(expected));
            assert_eq!(array_init(&element, expected as i64 + 1, &|_| panic!("oversized array initialized an element")).unwrap_err().code, "E_ARRAY_MEMORY");
        }
        assert_eq!(array_max_length(&Ty::Bool), None);
        assert_eq!(array_buffer::<u8>(usize::MAX).unwrap_err().code, "E_ARRAY_MEMORY");
    }

    #[test]
    fn shared_sorted_records_preserve_authored_order_and_first_duplicate() {
        let binary: Arc<[u8]> = (0..65536).map(|i| [0, 255, 65, 128][i % 4]).collect();
        let z = symbol::intern("z");
        let a = symbol::intern("a");
        let same = symbol::intern("same");
        let original = Record::new(vec![
            (z, Value::Text(binary)),
            (a, Value::Record(vec![(same, Value::Int(7))].into())),
            (z, Value::Bool(true)),
        ]);
        assert!(original.fields.windows(2).all(|pair| pair[0].0.id() <= pair[1].0.id()));
        assert!(matches!(original.get(z), Some(Value::Text(_))), "first duplicate wins");
        assert_eq!(original.iter().map(|(name, _)| *name).collect::<Vec<_>>(), [z, a, z]);
        assert_eq!(original.field_at(2), Some(&(z, Value::Bool(true))));
        assert_eq!(original.field_at(3), None);
        assert_eq!(original.iter().rev().map(|(name, _)| *name).collect::<Vec<_>>(), [z, a, z]);
        let value = Value::Record(original.clone());
        assert_eq!(rays_math::hash::md5_hex(&key_of(&|_| 0, &value)), "8e640fe7beef4e570ef5815a2b15d1c6");
        assert_eq!(rays_math::hash::md5_hex(&show_with(&Clone::clone, &value)), "eddfe8c787c4a0f3c4f3c5f7a9c3f6f1");
        assert_eq!(std::mem::size_of::<Value>(), 40);
        let mut cloned = original.clone();
        assert!(Arc::ptr_eq(&original.fields, &cloned.fields));
        assert!(Arc::ptr_eq(&original.shape, &cloned.shape));
        cloned.set(z, Value::Int(9));
        assert_eq!(cloned.get(z), Some(&Value::Int(9)));
        assert!(matches!(original.get(z), Some(Value::Text(_))), "COW preserves the retained snapshot");
        assert!(Arc::ptr_eq(&original.shape, &cloned.shape), "value-only edits retain the shape");
        assert_eq!(cloned[2], (z, Value::Bool(true)), "assoc replaces only the first duplicate");
        let fresh = symbol::intern("z2_appended_field");
        cloned.set(fresh, Value::Int(10));
        assert_eq!(cloned[3], (fresh, Value::Int(10)));
        cloned.insert(0, (z, Value::Int(11)));
        assert_eq!(cloned.get(z), Some(&Value::Int(11)));
        let threaded = std::thread::scope(|scope| {
            let handles: Vec<_> = (0..8).map(|_| scope.spawn(|| Record::new(vec![
                (z, Value::Int(1)), (a, Value::Int(2)), (z, Value::Int(3)),
            ]))).collect();
            handles.into_iter().map(|handle| handle.join().unwrap()).collect::<Vec<_>>()
        });
        assert!(threaded.iter().all(|other| Arc::ptr_eq(&original.shape, &other.shape)));
        let mut memory = crate::syntax::Memory::default();
        assert!(original.attribute(&mut memory));
        let before = memory.bytes();
        assert!(!original.clone().attribute(&mut memory));
        assert_eq!(memory.bytes(), before);
        assert_eq!(original.get_str("z2_not_an_interned_field"), None);
        assert_eq!(symbol::find("z2_not_an_interned_field"), None);
    }

    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[test]
    fn pure_values_and_packed_arrays() {
        assert_eq!(key_of(&|id| id.0, &Value::Deferred(ty::image(), 3)), b"image:3".to_vec());
        assert_ne!(key_of(&|id| id.0, &Value::Deferred(ty::image(), 3)), key_of(&|id| id.0, &Value::Int(3)));
        assert_eq!(coerce_to(&Ty::Float, &Value::Int(3)).unwrap(), Value::Float(3.0));
        assert_eq!(coerce_to(&Ty::Int, &Value::Float(1.5)).unwrap(), Value::Int(2));
        assert_eq!(coerce_to(&Ty::Int, &Value::Float(-1.5)).unwrap(), Value::Int(-1));
        assert_eq!(coerce_to(&Ty::Bool, &Value::Float(-0.0)).unwrap(), Value::Bool(false));
        assert_eq!(coerce_to(&Ty::Bool, &Value::Int(-1)).unwrap(), Value::Bool(true));
        assert_eq!(coerce_to(&Ty::Float, &Value::Bool(true)).unwrap(), Value::Float(1.0));
        assert_eq!(coerce_to(&Ty::Int, &Value::Bool(false)).unwrap(), Value::Int(0));
        assert_eq!(coerce_to(&Ty::Vec3, &Value::Int(2)).unwrap(), Value::Vec3(2.0, 2.0, 2.0));
        assert_eq!(coerce_to(&Ty::Vec3, &Value::Float(3.0)).unwrap(), Value::Vec3(3.0, 3.0, 3.0));
        assert_eq!(coerce_to(&Ty::Vec3, &Value::Vec3(1.0, 2.0, 3.0)).unwrap(), Value::Vec3(1.0, 2.0, 3.0));
        for value in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] { assert_eq!(coerce_to(&Ty::Int, &Value::Float(value)).unwrap_err().code, "E_NONFINITE"); }
        let integers = Arc::new(vec![-(1_i64 << 62), -(1_i64 << 62) + 1, -(1_i64 << 53) - 1, 0, (1_i64 << 53) + 1, (1_i64 << 62) - 1]);
        let packed = Value::Int_array(integers.clone());
        assert_eq!(ty_of(&packed), Ty::Array(Box::new(Ty::Int)));
        assert_eq!(array_length(&packed), Ok(integers.len()));
        for (index, value) in integers.iter().enumerate() { assert_eq!(array_get(&packed, index as i64), Ok(Value::Int(*value))); }
        validate(&packed).unwrap();
        assert_eq!(show_with(&|value| value.clone(), &packed), "6 × int".as_bytes().to_vec());
        assert_ne!(key_of(&|id| id.0, &packed), key_of(&|id| id.0, &Value::Floats(1, Arc::new(integers.iter().map(|n| *n as f64).collect()))));
        assert_eq!(array_init(&Ty::Int, 0, &|_| unreachable!()).unwrap(), Value::Int_array(Arc::new(vec![])));
        assert_eq!(array_init(&Ty::Text, 0, &|_| unreachable!()).unwrap(), Value::Text_array(Arc::new(TextArray::new())));
        assert_eq!(array_init(&Ty::Text, 2, &|index| Value::Text(vec![0xff, index as u8].into())).unwrap(), Value::Text_array(Arc::new(vec![vec![0xff, 0], vec![0xff, 1]].into())));
        assert_eq!(array_init(&Ty::Text, 1, &|_| Value::Int(0)).unwrap_err().code, "E_ARRAY_TYPE");
        assert_eq!(array_init(&Ty::Int, -1, &|_| unreachable!()).unwrap_err().code, "E_ARRAY_RANGE");
        assert_eq!(array_init(&Ty::Int, i64::MAX, &|_| unreachable!()).unwrap_err().code, "E_ARRAY_MEMORY");
        for element in [Ty::Int, Ty::Float, Ty::Vec2, Ty::Vec3, Ty::Vec4, Ty::Text] {
            assert_eq!(array_init(&element, (1_i64 << 52) - 1, &|_| unreachable!("allocation must fail before initialization")).unwrap_err().code, "E_ARRAY_MEMORY");
        }
        for (element, value, expected) in [
            (Ty::Vec2, Value::Vec2(1.0, -0.0), 3_i64),
            (Ty::Vec4, Value::Vec4(1.0, 2.0, 3.0, 4.0), 3_i64),
        ] {
            let packed = array_init(&element, expected, &|_| value.clone()).unwrap();
            assert_eq!(ty_of(&packed), Ty::Array(Box::new(element)));
            assert_eq!(array_length(&packed), Ok(expected as usize));
            assert_eq!(array_get(&packed, expected - 1), Ok(value));
        }
        let mut data = vec![0.0; 6];
        array_put(&mut data, 2, 1, &Value::Float(2.0)).unwrap();
        assert_eq!(data, [0.0, 0.0, 2.0, 2.0, 0.0, 0.0]);
        assert_eq!(array_put(&mut data, 2, 0, &Value::Vec3(1.0, 2.0, 3.0)).unwrap_err().code, "E_ARRAY_TYPE");
        assert_eq!(array_init(&Ty::Bool, 1, &|_| Value::Bool(true)).unwrap_err().code, "E_ARRAY_TYPE");
        assert_eq!(arith("+", &|a, b| a + b, &Value::Int(2), &Value::Int(3)).unwrap(), Value::Int(5));
        assert_eq!(arith("+", &|a, b| a + b, &Value::Vec3(1.0, 2.0, 3.0), &Value::Int(1)).unwrap(), Value::Vec3(2.0, 3.0, 4.0));
        let frame = Value::Record(vec![("dt".into(), Value::Float(0.125)), ("index".into(), Value::Int(24))].into());
        assert_eq!(ty_of(&frame), Ty::Record(vec![("dt".into(), Ty::Float), ("index".into(), Ty::Int)]));
        validate(&frame).unwrap();
        let state = Value::List(Arc::new(vec![Value::Float(1.0), packed.clone()]));
        assert_eq!(ty_of(&state), Ty::List(Box::new(Ty::Float)));
        validate(&state).unwrap();
        assert_eq!(array_get(&packed, -1).unwrap_err().code, "E_ARRAY_RANGE");
        assert_eq!(array_get(&packed, integers.len() as i64).unwrap_err().code, "E_ARRAY_RANGE");
        let texts = Arc::new(TextArray::from(vec![Vec::new(), "猫".as_bytes().to_vec(), b"a\0b".to_vec(), "é".as_bytes().to_vec(), "😀".as_bytes().to_vec(), b"quote\"\\\n".to_vec()]));
        let array = Value::Text_array(texts.clone());
        assert_eq!(ty_of(&array), Ty::Array(Box::new(Ty::Text)));
        assert_eq!(array_length(&array), Ok(texts.len()));
        for (index, text) in texts.iter().enumerate() { assert_eq!(array_get(&array, index as i64), Ok(Value::Text(text.into()))); }
        assert_eq!(show_with(&|value| value.clone(), &array), "6 × text".as_bytes().to_vec());
        let bytes = (0_u8..=255).collect::<Vec<_>>();
        let every_byte = Value::Text(bytes.clone().into());
        assert_eq!(show_with(&|value| value.clone(), &every_byte), bytes);
        let mut expected_key = b"s256:".to_vec();
        expected_key.extend(0_u8..=255);
        assert_eq!(key_of(&|id| id.0, &every_byte), expected_key);
        let byte_array = Value::Text_array(Arc::new(vec![(0_u8..=255).collect::<Vec<u8>>()].into()));
        assert_eq!(array_get(&byte_array, 0), Ok(Value::Text((0_u8..=255).collect::<std::sync::Arc<[u8]>>())));
        let mut expected_array_key = b"AT[256:".to_vec();
        expected_array_key.extend(0_u8..=255);
        expected_array_key.push(b']');
        assert_eq!(key_of(&|id| id.0, &byte_array), expected_array_key);
        let source = source_value("texts", Ty::Array(Box::new(Ty::Text)), vec![("values".into(), array.clone())].into());
        assert_eq!(ty_of(&source), Ty::Array(Box::new(Ty::Text)));
        assert_ne!(key_of(&|id| id.0, &source), key_of(&|id| id.0, &array));
        assert_eq!(param::float_text(1e-5), "0.00001");
        assert_eq!(param::float_text(1.23456), "1.23456");
    }

    #[test]
    fn float_array_widths_preserve_types_keys_and_storage() {
        assert_eq!(std::mem::size_of::<Value>(), 40);
        let data = Arc::new(vec![0.25; 12]);
        let types = [Ty::Float, Ty::Vec2, Ty::Vec3, Ty::Vec4];
        let tags = ["A", "V2", "V", "V4"];
        let names = ["float", "vec2", "vec3", "vec4"];
        for width in 1..=4 {
            let value = Value::Floats(width, data.clone());
            assert_eq!(ty_of(&value), Ty::Array(Box::new(types[width - 1].clone())));
            assert_eq!(array_length(&value), Ok(12 / width));
            assert_eq!(show_with(&Clone::clone, &value), format!("{} × {}", 12 / width, names[width - 1]).as_bytes());
            let expected = format!("{}[{}]", tags[width - 1], vec!["3fd0000000000000"; 12].join(","));
            assert_eq!(key_of(&|id| id.0, &value), expected.as_bytes());
            let Value::Floats(cloned_width, cloned) = value.clone() else { unreachable!() };
            assert_eq!(cloned_width, width);
            assert!(Arc::ptr_eq(&cloned, &data));
            let expected_row = match width { 1 => Value::Float(0.25), 2 => Value::Vec2(0.25, 0.25), 3 => Value::Vec3(0.25, 0.25, 0.25), _ => Value::Vec4(0.25, 0.25, 0.25, 0.25) };
            assert_eq!(array_get(&value, (12 / width - 1) as i64), Ok(expected_row.clone()));
            assert_eq!(array_init(&types[width - 1], (12 / width) as i64, &|_| expected_row.clone()), Ok(value.clone()));
            validate(&value).unwrap();
            for other_width in 1..=4 {
                let other = Value::Floats(other_width, data.clone());
                assert_eq!(value == other, width == other_width);
                assert_eq!(key_of(&|id| id.0, &value) == key_of(&|id| id.0, &other), width == other_width);
            }
        }
        for width in [0, 5, usize::MAX] {
            for data in [Arc::new(Vec::new()), data.clone()] {
                let invalid = Value::Floats(width, data);
                assert_eq!(array_length(&invalid).unwrap_err().code, "E_ARRAY_TYPE");
                assert_eq!(validate(&invalid).unwrap_err().code, "E_ARRAY_TYPE");
                assert_eq!(array_get(&invalid, 0).unwrap_err().code, "E_ARRAY_TYPE");
            }
        }
    }

    #[test]
    fn arrays_validation_memo_and_error_rows() {
        for width in 1..=4 {
            let values = Arc::new(vec![0.25; width * 16]);
            let packed = Value::Floats(width, values.clone());
            validate(&packed).unwrap();
            for index in 0..values.len() {
                let invalid = Arc::new((0..values.len()).map(|i| if i == index { f64::NAN } else { 0.25 }).collect());
                let bad = Value::Floats(width, invalid);
                assert_eq!(validate(&bad).unwrap_err().code, "E_NONFINITE");
            }
        }
        let malformed = [Value::Floats(2, Arc::new(vec![f64::NAN])), Value::Floats(3, Arc::new(vec![f64::INFINITY, 0.0])), Value::Floats(4, Arc::new(vec![f64::NEG_INFINITY, 0.0, 0.0]))];
        for value in malformed { assert_eq!(validate(&value).unwrap_err().code, "E_ARRAY_TYPE"); }
        let large = Arc::new(vec![0.25; 3 * 16_384]);
        let packed = Value::Floats(3, large.clone());
        validate(&packed).unwrap();
        let original = large.clone();
        validate(&packed).unwrap();
        assert!(Arc::ptr_eq(&large, &original));
        let memo = DigestCache::new();
        let keys: Vec<_> = (0..33).map(|index| Arc::new(vec![index as f64])).collect();
        for (index, key) in keys.iter().enumerate() {
            assert_eq!(memo.get_or_compute(MemoKey::float(key), &|| index.to_string()), index.to_string());
        }
        let recomputes = AtomicUsize::new(0);
        assert_eq!(memo.get_or_compute(MemoKey::float(&keys[0]), &|| { recomputes.fetch_add(1, Ordering::Relaxed); "evicted".into() }), "evicted");
        assert_eq!(recomputes.load(Ordering::Relaxed), 1);
        let expiring = Arc::new(vec![99.0]);
        let weak = Arc::downgrade(&expiring);
        let weak_memo = DigestCache::new();
        weak_memo.get_or_compute(MemoKey::float(&expiring), &|| "retained".into());
        assert!(weak_memo.retained_bytes() > 0);
        drop(expiring);
        assert!(weak.upgrade().is_none());
        assert_eq!(weak_memo.retained_bytes(), 0);
        let finite = FiniteArrayCache::new();
        let values = Arc::new(vec![0.25; 4097]);
        let checks = AtomicUsize::new(0);
        let check = || { checks.fetch_add(1, Ordering::Relaxed); check_float_array(&values) };
        finite.validate_once(&values, &check).unwrap();
        finite.validate_once(&values, &check).unwrap();
        assert_eq!(checks.load(Ordering::Relaxed), 1);
        let large_ints = Value::Int_array(Arc::new(vec![7; 4097]));
        let large_texts = Value::Text_array(Arc::new(vec![b"x".to_vec(); 4097].into()));
        assert_eq!(key_of(&|id| id.0, &packed), key_of(&|id| id.0, &packed));
        assert_eq!(key_of(&|id| id.0, &large_ints), key_of(&|id| id.0, &large_ints));
        assert_eq!(key_of(&|id| id.0, &large_texts), key_of(&|id| id.0, &large_texts));
        let large_invalid = Value::Text_array(Arc::new(vec![vec![0xff]; 4097].into()));
        let large_other = Value::Text_array(Arc::new(vec![vec![0xfe]; 4097].into()));
        assert_ne!(key_of(&|id| id.0, &large_invalid), key_of(&|id| id.0, &large_other));
        assert!(remembered().iter().any(|entry| entry.owner == "array_key" && entry.bytes > 0));
        assert!(remembered().iter().any(|entry| entry.owner == "finite_arrays" && entry.bytes > 0));
        assert!(retained_bytes() <= 64 * 256);
        let rows = Value::Int_array(Arc::new(vec![0, 1, 0, 1, 1, 0]));
        assert_eq!(error_row_index(&rows, None, None), Ok(Some(1)));
        assert_eq!(error_row_index(&rows, Some(3), None), Ok(Some(1)));
        assert_eq!(error_row_index(&Value::Int_array(Arc::new(vec![0, 0])), None, None), Ok(None));
        assert_eq!(diagnose_rows(&|_, first| first, &|_| Ok(Value::No_geo), &rows).unwrap_err().code, "E_DIAGNOSTIC_MAP");
        assert_eq!(diagnose_rows(&|_, first| first, &|_| Ok(Value::No_geo), &Value::Int_array(Arc::new(vec![0, 0]))).unwrap(), Value::No_geo);
        assert_eq!(error_row_index(&Value::Int_array(Arc::new(vec![0, 2])), None, None).unwrap_err().code, "E_DIAGNOSTIC_ROWS");
        assert_eq!(error_row_index(&rows, Some(0), None).unwrap_err().code, "E_DIAGNOSTIC_CONTEXT");
    }

    #[test]
    fn error_mapping_preserves_provenance() {
        let original = fail("E_SOURCE", "original");
        let mapped = with_error(&|| Err(original.clone()), &|_| Ok(Value::Record(vec![
            ("code".into(), Value::Text(b"E_MAPPED".as_slice().into())),
            ("message".into(), Value::Text(b"mapped".as_slice().into())),
            ("cause".into(), Value::List(Arc::new(vec![]))),
        ].into()))).unwrap_err();
        assert_eq!(mapped.code, "E_MAPPED");
        assert_eq!(mapped.message, "mapped");
        let malformed = with_error(&|| Err(original.clone()), &|_| Ok(Value::Int(0))).unwrap_err();
        assert_eq!(malformed.code, "E_DIAGNOSTIC_MAP");
    }
}
