use crate::{packed_ops, ty::{self, Ty}};
use rays_diagnostic::{Diagnostic, Severity};
use std::sync::{Arc, LazyLock, Mutex, Weak};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FnId(pub i64);
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ResidualId(pub i64);

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Text(Vec<u8>),
    Vec2(f64, f64),
    Vec3(f64, f64, f64),
    Vec4(f64, f64, f64, f64),
    List(Vec<Value>),
    Int_array(Arc<Vec<i64>>),
    Text_array(Arc<Vec<Vec<u8>>>),
    Float_array(Arc<Vec<f64>>),
    Vec2_array(Arc<Vec<f64>>),
    Vec3_array(Arc<Vec<f64>>),
    Vec4_array(Arc<Vec<f64>>),
    Record(Vec<(String, Value)>),
    Deferred(Ty, i64),
    No_geo,
    Struct(String, Ty, Vec<(String, Value)>),
    Source(String, Ty, Vec<(String, Value)>),
    Fn(FnId),
    Residual(ResidualId),
}

pub type T = Value;

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
    (rays_math::hash::float_sequence32(values) % 1_000_000) as f64 / 1_000_000.0
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

fn trim_decimal(mut value: String) -> String {
    if value.contains('.') {
        while value.ends_with('0') { value.pop(); }
        if value.ends_with('.') { value.pop(); }
    }
    value
}
pub fn general(value: f64) -> String { cfmt::g(value, 6) }
fn fixed(digits: usize, value: f64) -> String { format!("{value:.digits$}") }
pub fn fmt4(value: f64) -> String {
    if value.is_nan() { return "NaN".into(); }
    if !value.is_finite() { return if value > 0.0 { "Infinity".into() } else { "-Infinity".into() }; }
    let absolute = value.abs();
    let full = fixed(40, absolute);
    let tail = full.split_once('.').map_or("", |(_, fraction)| fraction);
    let tie = tail.as_bytes().get(4) == Some(&b'5') && tail.get(5..).is_some_and(|rest| rest.bytes().all(|byte| byte == b'0'));
    let rendered = trim_decimal(fixed(4, if tie { absolute + 1e-6 } else { absolute }));
    if value < 0.0 && rendered != "0" { format!("-{rendered}") } else { rendered }
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
        Value::Float_array(_) => Ty::Array(Box::new(Ty::Float)),
        Value::Vec2_array(_) => Ty::Array(Box::new(Ty::Vec2)),
        Value::Vec3_array(_) => Ty::Array(Box::new(Ty::Vec3)),
        Value::Vec4_array(_) => Ty::Array(Box::new(Ty::Vec4)),
        Value::Record(fields) => Ty::Record(fields.iter().map(|(name, value)| (name.clone(), ty_of(value))).collect()),
        Value::Deferred(ty, _) | Value::Struct(_, ty, _) | Value::Source(_, ty, _) => ty.clone(),
        Value::No_geo => ty::geometry(),
        Value::Fn(_) => Ty::Fn(None),
        Value::Residual(_) => Ty::Any,
    }
}
pub fn elem_ty(values: &[Value]) -> Ty {
    values.iter().fold(Ty::Any, |current, value| ty::join(&current, &ty_of(value)).unwrap_or(current))
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
        (Ty::List(element), Value::List(values)) => Ok(Value::List(values.iter().map(|value| coerce_to(element, value)).collect::<Result<Vec<_>, _>>()?)),
        (Ty::Array(element), Value::Int_array(values)) if **element == Ty::Float => Ok(Value::Float_array(Arc::new(values.iter().map(|value| *value as f64).collect()))),
        (Ty::Record(wanted), Value::Record(fields)) => Ok(Value::Record(fields.iter().map(|(name, value)| {
            Ok((name.clone(), wanted.iter().find(|(field, _)| field == name).map_or_else(|| Ok(value.clone()), |(_, want)| coerce_to(want, value))?))
        }).collect::<Result<Vec<_>, Diagnostic>>()?)),
        _ => Ok(value.clone()),
    }
}

pub fn show_with(conc: &dyn Fn(&Value) -> Value, value: &Value) -> Vec<u8> {
    match conc(value) {
        Value::Int(n) => n.to_string().into_bytes(),
        Value::Float(n) => fmt4(n).into_bytes(),
        Value::Bool(value) => if value { b"true".to_vec() } else { b"false".to_vec() },
        Value::Text(text) => text,
        Value::Vec2(x, y) => format!("[{} {}]", fmt4(x), fmt4(y)).into_bytes(),
        Value::Vec3(x, y, z) => format!("[{} {} {}]", fmt4(x), fmt4(y), fmt4(z)).into_bytes(),
        Value::Vec4(x, y, z, w) => format!("[{} {} {} {}]", fmt4(x), fmt4(y), fmt4(z), fmt4(w)).into_bytes(),
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
        Value::Float_array(values) => format!("{} × float", values.len()).into_bytes(),
        Value::Vec2_array(values) => format!("{} × vec2", values.len() / 2).into_bytes(),
        Value::Vec3_array(values) => format!("{} × vec3", values.len() / 3).into_bytes(),
        Value::Vec4_array(values) => format!("{} × vec4", values.len() / 4).into_bytes(),
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
    Text(Weak<Vec<Vec<u8>>>),
    Float(Weak<Vec<f64>>),
}
impl MemoKey {
    fn int(values: &Arc<Vec<i64>>) -> Self { Self::Int(Arc::downgrade(values)) }
    fn text(values: &Arc<Vec<Vec<u8>>>) -> Self { Self::Text(Arc::downgrade(values)) }
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
    let mut hash = 0xcbf29ce484222325_u64;
    for value in values { for byte in value.to_bits().to_le_bytes() { hash = (hash ^ u64::from(byte)).wrapping_mul(0x100000001b3); } }
    format!("{:016x}", hash)
}
fn digest_int_array(values: &[i64]) -> String {
    let mut hash = 0xcbf29ce484222325_u64;
    for value in values { for byte in value.to_le_bytes() { hash = (hash ^ u64::from(byte)).wrapping_mul(0x100000001b3); } }
    format!("{:016x}", hash)
}
fn digest_text_array(values: &[Vec<u8>]) -> String {
    let mut hash = 0xcbf29ce484222325_u64;
    for value in values {
        for byte in (value.len() as u64).to_le_bytes() { hash = (hash ^ u64::from(byte)).wrapping_mul(0x100000001b3); }
        for byte in value { hash = (hash ^ u64::from(*byte)).wrapping_mul(0x100000001b3); }
    }
    format!("{:016x}", hash)
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
fn key_text_array(values: &Arc<Vec<Vec<u8>>>) -> Vec<u8> {
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
        Value::Float_array(values) => key_float_array(values, "A"),
        Value::Vec2_array(values) => key_float_array(values, "V2"),
        Value::Vec3_array(values) => key_float_array(values, "V"),
        Value::Vec4_array(values) => key_float_array(values, "V4"),
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
        Value::Struct(name, _, fields) => {
            let mut key = format!("S{name}").into_bytes();
            key.extend(key_of(residual, &Value::Record(fields.clone())));
            key
        }
        Value::Source(name, ty, fields) => {
            let mut key = format!("source:{name}:{}", ty::to_string(ty)).into_bytes();
            key.extend(key_of(residual, &Value::Record(fields.clone())));
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
        Value::Float_array(values) => Ok(values.len()),
        Value::Vec2_array(values) if values.len() % 2 == 0 => Ok(values.len() / 2),
        Value::Vec3_array(values) if values.len() % 3 == 0 => Ok(values.len() / 3),
        Value::Vec4_array(values) if values.len() % 4 == 0 => Ok(values.len() / 4),
        Value::Vec2_array(_) => Err(fail("E_ARRAY_TYPE", "Packed vec2 storage has two coordinates per element.")),
        Value::Vec3_array(_) => Err(fail("E_ARRAY_TYPE", "Packed vec3 storage has three coordinates per element.")),
        Value::Vec4_array(_) => Err(fail("E_ARRAY_TYPE", "Packed vec4 storage has four coordinates per element.")),
        _ => Err(fail("E_ARRAY_TYPE", "Expected a packed array.")),
    }
}
pub fn array_get(value: &Value, index: i64) -> Result<Value, Diagnostic> {
    let length = array_length(value)?;
    if index < 0 || index as u128 >= length as u128 { return Err(fail("E_ARRAY_RANGE", "Array index is outside its length.")); }
    let index = index as usize;
    let value = match value {
        Value::Int_array(values) => Value::Int(values[index]),
        Value::Text_array(values) => Value::Text(values[index].clone()),
        Value::Float_array(values) => Value::Float(values[index]),
        Value::Vec2_array(values) => Value::Vec2(values[index * 2], values[index * 2 + 1]),
        Value::Vec3_array(values) => Value::Vec3(values[index * 3], values[index * 3 + 1], values[index * 3 + 2]),
        Value::Vec4_array(values) => Value::Vec4(values[index * 4], values[index * 4 + 1], values[index * 4 + 2], values[index * 4 + 3]),
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
const MAX_ARRAY_LENGTH: u128 = (1u128 << 54) - 1; // OCaml 5: Sys.max_array_length = Sys.max_floatarray_length
fn allocation_count_ok(count: i64, width: usize) -> Result<usize, Diagnostic> {
    if count < 0 || (count as u128) > MAX_ARRAY_LENGTH / width as u128 { Err(fail("E_ARRAY_RANGE", "Array length is outside native storage bounds.")) }
    else { Ok(count as usize) }
}
pub fn array_init(element: &Ty, count: i64, f: &dyn Fn(usize) -> Value) -> Result<Value, Diagnostic> {
    if *element == Ty::Text {
        let count = allocation_count_ok(count, 1)?;
        let mut values = Vec::with_capacity(count);
        for index in 0..count { match f(index) { Value::Text(text) => values.push(text), _ => return Err(fail("E_ARRAY_TYPE", "Text arrays contain text values.")) } }
        return Ok(Value::Text_array(Arc::new(values)));
    }
    if *element == Ty::Int {
        let count = allocation_count_ok(count, 1)?;
        let mut values = vec![0; count];
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
    let count = allocation_count_ok(count, width)?;
    let mut values = vec![0.0; count * width];
    for index in 0..count { array_put(&mut values, width, index, &f(index))?; }
    let values = Arc::new(values);
    Ok(match width { 2 => Value::Vec2_array(values), 3 => Value::Vec3_array(values), 4 => Value::Vec4_array(values), _ => Value::Float_array(values) })
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
        Value::Float_array(values) | Value::Vec2_array(values) | Value::Vec3_array(values) | Value::Vec4_array(values) => {
            array_length(value)?;
            if values.len() <= 4096 { check_float_array(values)?; }
            else { FINITE_ARRAYS.validate_once(values, &|| check_float_array(values))?; }
        }
        Value::List(values) => for value in values { validate(value)?; },
        Value::Record(fields) | Value::Struct(_, _, fields) | Value::Source(_, _, fields) => for (_, value) in fields { validate(value)?; },
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
        ("code".into(), Value::Text(diagnostic.code.as_bytes().to_vec())),
        ("message".into(), Value::Text(diagnostic.message.as_bytes().to_vec())),
        ("cause".into(), Value::List(diagnostic.cause.iter().map(|cause| Value::Text(cause.as_bytes().to_vec())).collect())),
    ])
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
    let Ok(code) = String::from_utf8(code.clone()) else { return Err(malformed()); };
    let Some(Value::Text(message)) = fields.iter().find(|(name, _)| name == "message").map(|(_, value)| value) else { return Err(malformed()); };
    let Ok(message) = String::from_utf8(message.clone()) else { return Err(malformed()); };
    let Some(Value::List(causes)) = fields.iter().find(|(name, _)| name == "cause").map(|(_, value)| value) else { return Err(malformed()); };
    let cause = match causes.as_slice() {
        [] => None,
        [Value::Text(cause)] => match String::from_utf8(cause.clone()) { Ok(cause) => Some(cause), Err(_) => return Err(malformed()) },
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
        assert_ne!(key_of(&|id| id.0, &packed), key_of(&|id| id.0, &Value::Float_array(Arc::new(integers.iter().map(|n| *n as f64).collect()))));
        assert_eq!(array_init(&Ty::Int, 0, &|_| unreachable!()).unwrap(), Value::Int_array(Arc::new(vec![])));
        assert_eq!(array_init(&Ty::Text, 0, &|_| unreachable!()).unwrap(), Value::Text_array(Arc::new(vec![])));
        assert_eq!(array_init(&Ty::Text, 2, &|index| Value::Text(vec![0xff, index as u8])).unwrap(), Value::Text_array(Arc::new(vec![vec![0xff, 0], vec![0xff, 1]])));
        assert_eq!(array_init(&Ty::Text, 1, &|_| Value::Int(0)).unwrap_err().code, "E_ARRAY_TYPE");
        for count in [-1, i64::MAX] { assert_eq!(array_init(&Ty::Int, count, &|_| unreachable!()).unwrap_err().code, "E_ARRAY_RANGE"); }
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
        let frame = Value::Record(vec![("dt".into(), Value::Float(0.125)), ("index".into(), Value::Int(24))]);
        assert_eq!(ty_of(&frame), Ty::Record(vec![("dt".into(), Ty::Float), ("index".into(), Ty::Int)]));
        validate(&frame).unwrap();
        let state = Value::List(vec![Value::Float(1.0), packed.clone()]);
        assert_eq!(ty_of(&state), Ty::List(Box::new(Ty::Float)));
        validate(&state).unwrap();
        assert_eq!(array_get(&packed, -1).unwrap_err().code, "E_ARRAY_RANGE");
        assert_eq!(array_get(&packed, integers.len() as i64).unwrap_err().code, "E_ARRAY_RANGE");
        let texts = Arc::new(vec![Vec::new(), "猫".as_bytes().to_vec(), b"a\0b".to_vec(), "é".as_bytes().to_vec(), "😀".as_bytes().to_vec(), b"quote\"\\\n".to_vec()]);
        let array = Value::Text_array(texts.clone());
        assert_eq!(ty_of(&array), Ty::Array(Box::new(Ty::Text)));
        assert_eq!(array_length(&array), Ok(texts.len()));
        for (index, text) in texts.iter().enumerate() { assert_eq!(array_get(&array, index as i64), Ok(Value::Text(text.clone()))); }
        assert_eq!(show_with(&|value| value.clone(), &array), "6 × text".as_bytes().to_vec());
        let bytes = (0_u8..=255).collect::<Vec<_>>();
        let every_byte = Value::Text(bytes.clone());
        assert_eq!(show_with(&|value| value.clone(), &every_byte), bytes);
        let mut expected_key = b"s256:".to_vec();
        expected_key.extend(0_u8..=255);
        assert_eq!(key_of(&|id| id.0, &every_byte), expected_key);
        let byte_array = Value::Text_array(Arc::new(vec![(0_u8..=255).collect()]));
        assert_eq!(array_get(&byte_array, 0), Ok(Value::Text((0_u8..=255).collect())));
        let mut expected_array_key = b"AT[256:".to_vec();
        expected_array_key.extend(0_u8..=255);
        expected_array_key.push(b']');
        assert_eq!(key_of(&|id| id.0, &byte_array), expected_array_key);
        let source = Value::Source("texts".into(), Ty::Array(Box::new(Ty::Text)), vec![("values".into(), array.clone())]);
        assert_eq!(ty_of(&source), Ty::Array(Box::new(Ty::Text)));
        assert_ne!(key_of(&|id| id.0, &source), key_of(&|id| id.0, &array));
        assert_eq!(general(1e-5), "1e-05");
        assert_eq!(fmt4(1.23456), "1.2346");
    }

    #[test]
    fn arrays_validation_memo_and_error_rows() {
        for (width, kind) in [(1, 0), (2, 1), (3, 2), (4, 3)] {
            let values = Arc::new(vec![0.25; width * 16]);
            let packed = match kind { 0 => Value::Float_array(values.clone()), 1 => Value::Vec2_array(values.clone()), 2 => Value::Vec3_array(values.clone()), _ => Value::Vec4_array(values.clone()) };
            validate(&packed).unwrap();
            for index in 0..values.len() {
                let invalid = Arc::new((0..values.len()).map(|i| if i == index { f64::NAN } else { 0.25 }).collect());
                let bad = match kind { 0 => Value::Float_array(invalid), 1 => Value::Vec2_array(invalid), 2 => Value::Vec3_array(invalid), _ => Value::Vec4_array(invalid) };
                assert_eq!(validate(&bad).unwrap_err().code, "E_NONFINITE");
            }
        }
        let malformed = [Value::Vec2_array(Arc::new(vec![f64::NAN])), Value::Vec3_array(Arc::new(vec![f64::INFINITY, 0.0])), Value::Vec4_array(Arc::new(vec![f64::NEG_INFINITY, 0.0, 0.0]))];
        for value in malformed { assert_eq!(validate(&value).unwrap_err().code, "E_ARRAY_TYPE"); }
        let large = Arc::new(vec![0.25; 3 * 16_384]);
        let packed = Value::Vec3_array(large.clone());
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
        let large_texts = Value::Text_array(Arc::new(vec![b"x".to_vec(); 4097]));
        assert_eq!(key_of(&|id| id.0, &packed), key_of(&|id| id.0, &packed));
        assert_eq!(key_of(&|id| id.0, &large_ints), key_of(&|id| id.0, &large_ints));
        assert_eq!(key_of(&|id| id.0, &large_texts), key_of(&|id| id.0, &large_texts));
        let large_invalid = Value::Text_array(Arc::new(vec![vec![0xff]; 4097]));
        let large_other = Value::Text_array(Arc::new(vec![vec![0xfe]; 4097]));
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
            ("code".into(), Value::Text(b"E_MAPPED".to_vec())),
            ("message".into(), Value::Text(b"mapped".to_vec())),
            ("cause".into(), Value::List(vec![])),
        ]))).unwrap_err();
        assert_eq!(mapped.code, "E_MAPPED");
        assert_eq!(mapped.message, "mapped");
        let malformed = with_error(&|| Err(original.clone()), &|_| Ok(Value::Int(0))).unwrap_err();
        assert_eq!(malformed.code, "E_DIAGNOSTIC_MAP");
    }
}
