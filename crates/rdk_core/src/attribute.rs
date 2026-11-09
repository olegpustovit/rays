use std::sync::Arc;

use rays_math::text_array::TextArray;

use crate::{error, packed, support};
use crate::error::Code;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Owner {
    Point,
    Vertex,
    Primitive,
    Detail,
}

/// Lowercase owner names used in attribute diagnostics.
pub fn owner_name(owner: Owner) -> &'static str {
    match owner {
        Owner::Point => "point",
        Owner::Vertex => "vertex",
        Owner::Primitive => "primitive",
        Owner::Detail => "detail",
    }
}

/// Quote opaque bytes: printable ASCII is literal, other bytes use decimal escapes.
pub fn quote_bytes(text: &[u8]) -> String {
    let mut out = String::from("\"");
    for &byte in text {
        match byte {
            b'"' => out.push_str("\\\""),
            b'\\' => out.push_str("\\\\"),
            b'\n' => out.push_str("\\n"),
            b'\t' => out.push_str("\\t"),
            b'\r' => out.push_str("\\r"),
            8 => out.push_str("\\b"),
            32..=126 => out.push(byte as char),
            _ => out.push_str(&format!("\\{byte:03}")),
        }
    }
    out.push('"');
    out
}

/// Quote UTF-8 text, preserving Unicode and escaping ASCII controls.
pub fn quote(text: &str) -> String {
    let mut out = String::from("\"");
    for c in text.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            c if (c as u32) < 32 || c as u32 == 127 => out.push_str(&format!("\\{:03}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

#[derive(Clone)]
pub enum Storage {
    Float(Arc<Vec<f64>>),
    Int(Arc<Vec<i64>>),
    IntArray(packed::int_array::T),
    FloatArray(packed::float_array::T),
    Float2(packed::float2::T),
    Float3(packed::float3::T),
    Float4(packed::float4::T),
    /// Opaque byte strings; attribute names and schema identifiers remain UTF-8.
    Text(Arc<TextArray>),
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum StorageKind {
    Float,
    Int,
    IntArray,
    FloatArray,
    Float2,
    Float3,
    Float4,
    Text,
}

#[derive(Clone, Copy)]
pub struct Kind {
    kind: StorageKind,
}

pub struct Key {
    key_name: String,
    key_owner: Owner,
    key_kind: Kind,
}

enum Body {
    Ready(Storage),
    Deferred {
        kind: StorageKind,
        length: i64,
        value: packed::deferred::T,
    },
}

#[derive(Clone)]
pub struct T {
    name: String,
    owner: Owner,
    body: Arc<Body>,
    data_id: i64,
    storage_id: i64,
}

fn invalid_attribute(operation: &str, message: &str) -> error::T {
    error::new(operation, Code::InvalidAttribute, message.to_owned())
}
fn storage_kind(storage: &Storage) -> StorageKind {
    match storage {
        Storage::Float(_) => StorageKind::Float,
        Storage::Int(_) => StorageKind::Int,
        Storage::IntArray(_) => StorageKind::IntArray,
        Storage::FloatArray(_) => StorageKind::FloatArray,
        Storage::Float2(_) => StorageKind::Float2,
        Storage::Float3(_) => StorageKind::Float3,
        Storage::Float4(_) => StorageKind::Float4,
        Storage::Text(_) => StorageKind::Text,
    }
}

fn storage_length(storage: &Storage) -> i64 {
    match storage {
        Storage::Float(values) => i64::try_from(values.len()).unwrap_or(i64::MAX),
        Storage::Int(values) => i64::try_from(values.len()).unwrap_or(i64::MAX),
        Storage::IntArray(values) => packed::int_array::length(values),
        Storage::FloatArray(values) => packed::float_array::length(values),
        Storage::Float2(values) => packed::float2::length(values),
        Storage::Float3(values) => packed::float3::length(values),
        Storage::Float4(values) => packed::float4::length(values),
        Storage::Text(values) => i64::try_from(values.len()).unwrap_or(i64::MAX),
    }
}

fn raw(value: &T) -> Result<&Storage, error::T> {
    match value.body.as_ref() {
        Body::Ready(storage) => Ok(storage),
        Body::Deferred { value, .. } => {
            match packed::deferred::force_ref(value)? {
                packed::deferred::Value::AttributeStorage(storage) => Ok(storage),
                _ => Err(invalid_attribute(
                    "Attribute.create_deferred",
                    "materialised storage is not an attribute",
                )),
            }
        }
    }
}


fn kind_matches(kind: Kind, storage: &Storage) -> bool {
    kind.kind == storage_kind(storage)
}

pub fn float() -> Kind {
    Kind {
        kind: StorageKind::Float,
    }
}

pub fn float2() -> Kind {
    Kind {
        kind: StorageKind::Float2,
    }
}

pub fn key(name: &str, owner: Owner, kind: Kind) -> Key {
    Key {
        key_name: name.to_owned(),
        key_owner: owner,
        key_kind: kind,
    }
}

pub fn create_key_owned(key: &Key, value: Storage) -> Result<T, error::T> {
    if !kind_matches(key.key_kind, &value) {
        return Err(invalid_attribute(
            "Attribute.create_key_owned",
            "value kind does not match key",
        ));
    }
    create_owned(&key.key_name, key.key_owner, value)
}

pub fn get(key: &Key, attribute: &T) -> Result<Option<Storage>, error::T> {
    if key.key_owner != attribute.owner || key.key_name != attribute.name {
        return Ok(None);
    }
    let value = raw(attribute)?;
    if !kind_matches(key.key_kind, value) {
        return Ok(None);
    }
    Ok(Some(clone_storage(value)))
}

fn clone_storage(value: &Storage) -> Storage {
    match value {
        Storage::Float(values) => Storage::Float(values.clone()),
        Storage::Int(values) => Storage::Int(values.clone()),
        Storage::IntArray(values) => Storage::IntArray(values.clone()),
        Storage::FloatArray(values) => Storage::FloatArray(values.clone()),
        Storage::Float2(values) => Storage::Float2(values.clone()),
        Storage::Float3(values) => Storage::Float3(values.clone()),
        Storage::Float4(values) => Storage::Float4(values.clone()),
        Storage::Text(values) => Storage::Text(values.clone()),
    }
}

pub fn normal(owner: Owner) -> Key {
    key(
        "N",
        owner,
        Kind {
            kind: StorageKind::Float3,
        },
    )
}

pub fn color(owner: Owner) -> Key {
    key(
        "Cd",
        owner,
        Kind {
            kind: StorageKind::Float4,
        },
    )
}

pub fn tex_coord(owner: Owner) -> Key {
    key(
        "uv",
        owner,
        Kind {
            kind: StorageKind::Float2,
        },
    )
}

pub fn create_owned(name: &str, owner: Owner, storage: Storage) -> Result<T, error::T> {
    if support::ocaml_trim_is_empty(name) {
        return Err(invalid_attribute("Attribute.create_owned", "empty name"));
    }
    Ok(T {
        name: name.to_owned(),
        owner,
        body: Arc::new(Body::Ready(storage)),
        data_id: support::fresh_data_id(),
        storage_id: support::fresh_data_id(),
    })
}

pub fn create_deferred(
    holder: Option<Arc<dyn packed::deferred::Holder>>,
    release: Option<packed::deferred::Release>,
    name: &str,
    owner: Owner,
    like: &Storage,
    length: i64,
    materialise: Arc<dyn Fn() -> Storage + Send + Sync>,
) -> Result<T, error::T> {
    if support::ocaml_trim_is_empty(name) {
        return Err(invalid_attribute("Attribute.create_deferred", "empty name"));
    }
    let kind = storage_kind(like);
    if length < 0
        || !matches!(
            kind,
            StorageKind::Float
                | StorageKind::Int
                | StorageKind::Float2
                | StorageKind::Float3
                | StorageKind::Float4
        )
    {
        return Err(invalid_attribute(
            "Attribute.create_deferred",
            "a numeric storage of a nonnegative length",
        ));
    }
    let expected_kind = kind;
    let materialiser = Arc::clone(&materialise);
    let make: Arc<
        dyn Fn() -> Result<packed::deferred::Value, error::T> + Send + Sync,
    > = Arc::new(move || {
        let storage = materialiser();
        if storage_kind(&storage) != expected_kind || storage_length(&storage) != length {
            return Err(invalid_attribute(
                "Attribute.create_deferred",
                "materialised storage changed shape",
            ));
        }
        Ok(packed::deferred::Value::AttributeStorage(storage))
    });
    let value = packed::deferred::create(make, holder, release);
    Ok(T {
        name: name.to_owned(),
        owner,
        body: Arc::new(Body::Deferred {
            kind,
            length,
            value,
        }),
        data_id: support::fresh_data_id(),
        storage_id: support::fresh_data_id(),
    })
}

pub fn resident(value: &T) -> bool {
    match value.body.as_ref() {
        Body::Ready(_) => false,
        Body::Deferred { value, .. } => !packed::deferred::forced(value),
    }
}

pub fn holder(value: &T) -> Option<Arc<dyn packed::deferred::Holder>> {
    match value.body.as_ref() {
        Body::Ready(_) => None,
        Body::Deferred { value, .. } => packed::deferred::holder(value),
    }
}

pub fn releaser(value: &T) -> Option<packed::deferred::Release> {
    match value.body.as_ref() {
        Body::Ready(_) => None,
        Body::Deferred { value, .. } => packed::deferred::releaser(value),
    }
}

pub fn name(value: &T) -> &str {
    &value.name
}

pub fn owner(value: &T) -> Owner {
    value.owner
}

pub fn storage(value: &T) -> Result<Storage, error::T> {
    raw(value).map(clone_storage)
}

pub fn length(value: &T) -> i64 {
    match value.body.as_ref() {
        Body::Ready(storage) => storage_length(storage),
        Body::Deferred { length, .. } => *length,
    }
}

pub fn data_id(value: &T) -> i64 {
    value.data_id
}

pub fn storage_id(value: &T) -> i64 {
    value.storage_id
}

pub fn with_name(name: &str, value: &T) -> Result<T, error::T> {
    if support::ocaml_trim_is_empty(name) {
        return Err(invalid_attribute("Attribute.with_name", "empty name"));
    }
    if name == value.name {
        return Ok(value.clone());
    }
    Ok(T {
        name: name.to_owned(),
        owner: value.owner,
        body: Arc::clone(&value.body),
        data_id: support::fresh_data_id(),
        storage_id: value.storage_id,
    })
}

fn storage_payload_bytes(storage: &Storage) -> i64 {
    match storage {
        Storage::Float(values) => i64::try_from(values.len()).unwrap_or(i64::MAX).saturating_mul(8),
        Storage::Int(values) => i64::try_from(values.len())
            .unwrap_or(i64::MAX)
            .saturating_mul(std::mem::size_of::<i64>() as i64),
        Storage::IntArray(values) => packed::int_array::payload_bytes(values),
        Storage::FloatArray(values) => packed::float_array::payload_bytes(values),
        Storage::Float2(values) => packed::float2::payload_bytes(values),
        Storage::Float3(values) => packed::float3::payload_bytes(values),
        Storage::Float4(values) => packed::float4::payload_bytes(values),
        Storage::Text(values) => i64::try_from(values.payload_bytes()).unwrap_or(i64::MAX),
    }
}

pub fn payload_bytes(value: &T) -> i64 {
    match value.body.as_ref() {
        Body::Ready(storage) => storage_payload_bytes(storage),
        Body::Deferred { kind, length, .. } => {
            let bytes_per_item = match kind {
                StorageKind::Float => 8,
                StorageKind::Float2 => 16,
                StorageKind::Float3 => 24,
                StorageKind::Float4 => 32,
                StorageKind::Int => std::mem::size_of::<i64>() as i64,
                _ => 0,
            };
            length.saturating_mul(bytes_per_item)
        }
    }
}

pub fn kind_name(value: &T) -> &'static str {
    let kind = match value.body.as_ref() {
        Body::Ready(storage) => storage_kind(storage),
        Body::Deferred { kind, .. } => *kind,
    };
    match kind {
        StorageKind::Float => "float",
        StorageKind::Int => "int",
        StorageKind::IntArray => "int_array",
        StorageKind::FloatArray => "float_array",
        StorageKind::Float2 => "float2",
        StorageKind::Float3 => "float3",
        StorageKind::Float4 => "float4",
        StorageKind::Text => "text",
    }
}


pub mod view {
    use super::{raw, Storage, T};
    use crate::error;

    pub fn storage(value: &T) -> Result<&Storage, error::T> {
        raw(value)
    }
}

pub fn float_planes(storage: &Storage) -> Result<Option<Vec<&[f64]>>, error::T> {
    match storage {
        Storage::Float(values) => Ok(Some(vec![values.as_slice()])),
        Storage::Float2(values) => Ok(Some(packed::float2::view::borrowed(values).to_vec())),
        Storage::Float3(values) => Ok(Some(
            packed::float3::view::borrowed(values)?.to_vec(),
        )),
        Storage::Float4(values) => Ok(Some(packed::float4::view::borrowed(values).to_vec())),
        Storage::Int(_) | Storage::IntArray(_) | Storage::FloatArray(_) | Storage::Text(_) => {
            Ok(None)
        }
    }
}

/// `plane` applied to every float plane of a float-kind storage, rebuilt in the same kind (`None`
/// for integer, text and CSR storage).
pub fn map_float_planes(
    storage: &Storage,
    mut plane: impl FnMut(&[f64]) -> Result<Vec<f64>, error::T>,
) -> Result<Option<Storage>, error::T> {
    let Some(planes) = float_planes(storage)? else { return Ok(None) };
    let planes = planes.into_iter().map(|source| plane(source).map(Arc::new)).collect::<Result<Vec<_>, _>>()?;
    let mut planes = planes.into_iter();
    let mut next = || planes.next().expect("one output plane per source plane");
    Ok(Some(match storage {
        Storage::Float(_) => Storage::Float(next()),
        Storage::Float2(_) => Storage::Float2(packed::float2::of_shared(next(), next())?),
        Storage::Float3(_) => Storage::Float3(packed::float3::view::of_shared_exn(next(), next(), next())?),
        _ => Storage::Float4(packed::float4::of_shared(next(), next(), next(), next())?),
    }))
}

/// Rebuild the template's float kind, transferring ownership of the output planes.
pub fn of_float_planes(
    like: &Storage,
    planes: Vec<Vec<f64>>,
) -> Result<Storage, error::T> {
    let mut planes = planes.into_iter();
    let mut next = || planes.next().ok_or_else(|| invalid_attribute("Attribute.of_float_planes", "missing component plane"));
    match like {
        Storage::Float(_) => Ok(Storage::Float(Arc::new(next()?))),
        Storage::Float2(_) => packed::float2::of_owned(next()?, next()?).map(Storage::Float2),
        Storage::Float3(_) => packed::float3::of_owned(next()?, next()?, next()?).map(Storage::Float3),
        Storage::Float4(_) => packed::float4::of_owned(next()?, next()?, next()?, next()?).map(Storage::Float4),
        _ => Err(invalid_attribute("Attribute.of_float_planes", "not a float storage")),
    }.map_err(|error| invalid_attribute("Attribute.of_float_planes", crate::error::message(&error)))
}

#[cfg(test)]
mod tests {
    use super::{Arc, Owner, Storage};

    #[test]
    fn quote_codecs_preserve_bytes_and_unicode_separately() {
        assert_eq!(
            super::quote_bytes(b"\"\\\n\t\r\x08\x00\x1f\x7f\xc3\xa9\xff"),
            r#""\"\\\n\t\r\b\000\031\127\195\169\255""#,
        );
        assert_eq!(
            super::quote("\"\\\n\t\r\x08\x00\x1f\x7fé"),
            r#""\"\\\n\t\r\008\000\031\127é""#,
        );
        for (owner, name) in [(Owner::Point, "point"), (Owner::Vertex, "vertex"), (Owner::Primitive, "primitive"), (Owner::Detail, "detail")] {
            assert_eq!(super::owner_name(owner), name);
        }
    }


    #[test]
    fn name_whitespace_is_ascii_only() {
        let whitespace = " \t\n\r\u{000c}";
        assert!(super::create_owned(whitespace, Owner::Point, Storage::Float(Arc::new(vec![]))).is_err());
        let nbsp = "\u{00a0}";
        let value = super::create_owned(nbsp, Owner::Point, Storage::Float(Arc::new(vec![]))).unwrap();
        assert_eq!(super::name(&value), nbsp);
        assert!(super::with_name(nbsp, &value).is_ok());
    }
    #[test]
    fn typed_key_checks_owner_and_storage_and_rename_shares_payload_identity() {
        let value = super::create_owned("signal", Owner::Point, Storage::Float(Arc::new(vec![1.0, 2.0])))
            .unwrap();
        let key = super::key("signal", Owner::Point, super::float());
        assert!(matches!(
            super::get(&key, &value).unwrap(),
            Some(Storage::Float(values)) if *values == vec![1.0, 2.0]
        ));
        assert!(super::get(&super::key("signal", Owner::Vertex, super::float()), &value).unwrap().is_none());
        let renamed = super::with_name("renamed", &value).unwrap();
        assert_ne!(super::data_id(&renamed), super::data_id(&value));
        assert_eq!(super::storage_id(&renamed), super::storage_id(&value));
    }

    #[test]
    fn deferred_metadata_does_not_materialise_and_first_read_is_cached() {
        use std::sync::atomic::{AtomicUsize, Ordering};
        use std::sync::Arc;

        let reads = Arc::new(AtomicUsize::new(0));
        let count_reads = Arc::clone(&reads);
        let materialise = Arc::new(move || {
            count_reads.fetch_add(1, Ordering::SeqCst);
            Storage::Float(Arc::new(vec![1.0, 2.0]))
        });
        let value = super::create_deferred(
            None,
            None,
            "lazy",
            Owner::Point,
            &Storage::Float(Arc::new(Vec::new())),
            2,
            materialise,
        )
        .unwrap();
        assert!(super::resident(&value));
        assert_eq!(super::length(&value), 2);
        assert_eq!(super::kind_name(&value), "float");
        assert_eq!(super::payload_bytes(&value), 16);
        assert_eq!(reads.load(Ordering::SeqCst), 0);
        assert!(matches!(super::storage(&value).unwrap(), Storage::Float(values) if *values == vec![1.0, 2.0]));
        assert!(super::storage(&value).is_ok());
        assert_eq!(reads.load(Ordering::SeqCst), 1);
        assert!(!super::resident(&value));
    }

    #[test]
    fn deferred_kind_metadata_is_available_without_materialising() {
        use std::sync::atomic::{AtomicUsize, Ordering};
        use std::sync::Arc;

        let reads = Arc::new(AtomicUsize::new(0));
        let count_reads = Arc::clone(&reads);
        let materialise = Arc::new(move || {
            count_reads.fetch_add(1, Ordering::SeqCst);
            Storage::Int(Arc::new(vec![1, 2, 3]))
        });
        let value = super::create_deferred(
            None,
            None,
            "lazy_int",
            Owner::Point,
            &Storage::Int(Arc::new(vec![0; 32])),
            3,
            materialise,
        )
        .unwrap();
        assert_eq!(super::kind_name(&value), "int");
        assert_eq!(super::length(&value), 3);
        assert_eq!(super::payload_bytes(&value), 24);
        assert_eq!(reads.load(Ordering::SeqCst), 0);
    }

    #[test]
    fn float_planes_are_borrowed_and_rebuilt_with_matching_kind() {
        let value = Storage::Float(Arc::new(vec![1.0, 2.0]));
        let planes = super::float_planes(&value).unwrap().unwrap();
        assert_eq!(planes[0], &[1.0, 2.0]);
        assert!(matches!(super::of_float_planes(&value, planes.iter().map(|plane| plane.to_vec()).collect()).unwrap(), Storage::Float(result) if *result == vec![1.0, 2.0]));
    }

    #[test]
    fn rebuilding_float_planes_moves_buffers_and_validates_shape() {
        use crate::{error, packed};
        let templates = [
            Storage::Float(Arc::new(vec![])),
            Storage::Float2(packed::float2::of_owned(vec![], vec![]).unwrap()),
            Storage::Float3(packed::float3::of_owned(vec![], vec![], vec![]).unwrap()),
            Storage::Float4(packed::float4::of_owned(vec![], vec![], vec![], vec![]).unwrap()),
        ];
        for (index, template) in templates.iter().enumerate() {
            let width = index + 1;
            let planes: Vec<Vec<f64>> = (0..width).map(|p| vec![p as f64, 10.0 + p as f64]).collect();
            let pointers: Vec<_> = planes.iter().map(|plane| plane.as_ptr()).collect();
            let output = super::of_float_planes(template, planes).unwrap();
            let output = super::float_planes(&output).unwrap().unwrap();
            assert_eq!(output.len(), width);
            for (p, plane) in output.iter().enumerate() {
                assert_eq!(plane.as_ptr(), pointers[p], "reconstruction copied a plane");
                assert_eq!(*plane, &[p as f64, 10.0 + p as f64]);
            }
            let missing = super::of_float_planes(template, vec![vec![]; width - 1]).err().unwrap();
            assert_eq!(error::code(&missing), error::Code::InvalidAttribute);
            if width > 1 {
                let mut mismatched = vec![vec![1.0]; width];
                mismatched[width - 1].clear();
                let bad = super::of_float_planes(template, mismatched).err().unwrap();
                assert_eq!(error::code(&bad), error::Code::InvalidAttribute);
            }
        }
        assert!(super::of_float_planes(&Storage::Int(Arc::new(vec![])), vec![]).is_err());
    }

    #[test]
    fn text_storage_and_remap_preserve_every_byte() {
        let bytes: Vec<u8> = (0..=255).collect();
        let reversed: Vec<u8> = bytes.iter().rev().copied().collect();
        let source = super::create_owned("bytes", Owner::Point, Storage::Text(Arc::new(vec![bytes.clone(), reversed.clone()].into()))).unwrap();
        assert!(matches!(super::view::storage(&source).unwrap(), Storage::Text(values) if **values == vec![bytes.clone(), reversed.clone()]));
        for grain in [1, 97] {
            let remapped = crate::topology_remap::attribute(None, grain, Some(true), &[1, -1, 0, 1], &source).unwrap();
            assert!(matches!(super::storage(&remapped).unwrap(), Storage::Text(values) if *values == vec![reversed.clone(), Vec::new(), bytes.clone(), reversed.clone()]));
        }
        assert!(matches!(super::storage(&source).unwrap(), Storage::Text(values) if *values == vec![bytes, reversed]));
    }
}
