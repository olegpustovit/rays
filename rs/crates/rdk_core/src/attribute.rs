use std::sync::Arc;

use crate::{error, packed, support};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Owner {
    Point,
    Vertex,
    Primitive,
    Detail,
}

#[derive(Clone)]
pub enum Storage {
    Float(Vec<f64>),
    Int(Vec<i64>),
    IntArray(packed::IntArray::T),
    FloatArray(packed::FloatArray::T),
    Float2(packed::Float2::T),
    Float3(packed::Float3::T),
    Float4(packed::Float4::T),
    /// Opaque byte strings; attribute names and schema identifiers remain UTF-8.
    Text(Vec<Vec<u8>>),
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
        value: packed::Deferred::T,
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
    error::internal(operation, "invalid_attribute", message.to_owned())
}


fn name_is_blank(name: &str) -> bool {
    name.as_bytes().iter().all(|byte| {
        matches!(*byte, b' ' | b'\t' | b'\n' | b'\r' | 0x0c)
    })
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
        Storage::IntArray(values) => packed::IntArray::length(values),
        Storage::FloatArray(values) => packed::FloatArray::length(values),
        Storage::Float2(values) => packed::Float2::length(values),
        Storage::Float3(values) => packed::Float3::length(values),
        Storage::Float4(values) => packed::Float4::length(values),
        Storage::Text(values) => i64::try_from(values.len()).unwrap_or(i64::MAX),
    }
}

fn raw(value: &T) -> Result<&Storage, error::T> {
    match value.body.as_ref() {
        Body::Ready(storage) => Ok(storage),
        Body::Deferred { value, .. } => {
            match packed::Deferred::force_ref(value)? {
                packed::Deferred::Value::AttributeStorage(storage) => Ok(storage),
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
    if name_is_blank(name) {
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
    holder: Option<Arc<dyn packed::Deferred::Holder>>,
    release: Option<packed::Deferred::Release>,
    name: &str,
    owner: Owner,
    like: &Storage,
    length: i64,
    materialise: Arc<dyn Fn() -> Storage + Send + Sync>,
) -> Result<T, error::T> {
    if name_is_blank(name) {
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
        dyn Fn() -> Result<packed::Deferred::Value, error::T> + Send + Sync,
    > = Arc::new(move || {
        let storage = materialiser();
        if storage_kind(&storage) != expected_kind || storage_length(&storage) != length {
            return Err(invalid_attribute(
                "Attribute.create_deferred",
                "materialised storage changed shape",
            ));
        }
        Ok(packed::Deferred::Value::AttributeStorage(storage))
    });
    let value = packed::Deferred::create(make, holder, release);
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
        Body::Deferred { value, .. } => !packed::Deferred::forced(value),
    }
}

pub fn holder(value: &T) -> Option<Arc<dyn packed::Deferred::Holder>> {
    match value.body.as_ref() {
        Body::Ready(_) => None,
        Body::Deferred { value, .. } => packed::Deferred::holder(value),
    }
}

pub fn releaser(value: &T) -> Option<packed::Deferred::Release> {
    match value.body.as_ref() {
        Body::Ready(_) => None,
        Body::Deferred { value, .. } => packed::Deferred::releaser(value),
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
    if name_is_blank(name) {
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
        Storage::IntArray(values) => packed::IntArray::payload_bytes(values),
        Storage::FloatArray(values) => packed::FloatArray::payload_bytes(values),
        Storage::Float2(values) => packed::Float2::payload_bytes(values),
        Storage::Float3(values) => packed::Float3::payload_bytes(values),
        Storage::Float4(values) => packed::Float4::payload_bytes(values),
        Storage::Text(values) => values.iter().fold(
            i64::try_from(values.len())
                .unwrap_or(i64::MAX)
                .saturating_mul(std::mem::size_of::<usize>() as i64),
            |total, text| total.saturating_add(i64::try_from(text.len()).unwrap_or(i64::MAX)),
        ),
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


#[allow(non_snake_case)]
pub mod View {
    use super::{raw, Storage, T};
    use crate::error;

    pub fn storage(value: &T) -> Result<&Storage, error::T> {
        raw(value)
    }
}

pub fn float_planes(storage: &Storage) -> Result<Option<Vec<&[f64]>>, error::T> {
    match storage {
        Storage::Float(values) => Ok(Some(vec![values.as_slice()])),
        Storage::Float2(values) => Ok(Some(packed::Float2::View::borrowed(values).to_vec())),
        Storage::Float3(values) => Ok(Some(
            packed::Float3::View::borrowed(values)?.to_vec(),
        )),
        Storage::Float4(values) => Ok(Some(packed::Float4::View::borrowed(values).to_vec())),
        Storage::Int(_) | Storage::IntArray(_) | Storage::FloatArray(_) | Storage::Text(_) => {
            Ok(None)
        }
    }
}

pub fn of_float_planes(
    like: &Storage,
    planes: &[&[f64]],
) -> Result<Storage, error::T> {
    let missing = || invalid_attribute("Attribute.of_float_planes", "missing component plane");
    match like {
        Storage::Float(_) => planes
            .first()
            .map(|plane| Storage::Float(plane.to_vec()))
            .ok_or_else(missing),
        Storage::Float2(_) => {
            let x = planes.first().ok_or_else(missing)?;
            let y = planes.get(1).ok_or_else(missing)?;
            packed::Float2::of_owned(x.to_vec(), y.to_vec())
                .map(Storage::Float2)
                .map_err(|error| {
                    invalid_attribute(
                        "Attribute.of_float_planes",
                        crate::error::message(&error),
                    )
                })
        }
        Storage::Float3(_) => {
            let x = planes.first().ok_or_else(missing)?;
            let y = planes.get(1).ok_or_else(missing)?;
            let z = planes.get(2).ok_or_else(missing)?;
            packed::Float3::of_owned(x.to_vec(), y.to_vec(), z.to_vec())
                .map(Storage::Float3)
                .map_err(|error| {
                    invalid_attribute(
                        "Attribute.of_float_planes",
                        crate::error::message(&error),
                    )
                })
        }
        Storage::Float4(_) => {
            let x = planes.first().ok_or_else(missing)?;
            let y = planes.get(1).ok_or_else(missing)?;
            let z = planes.get(2).ok_or_else(missing)?;
            let w = planes.get(3).ok_or_else(missing)?;
            packed::Float4::of_owned(x.to_vec(), y.to_vec(), z.to_vec(), w.to_vec())
                .map(Storage::Float4)
                .map_err(|error| {
                    invalid_attribute(
                        "Attribute.of_float_planes",
                        crate::error::message(&error),
                    )
                })
        }
        Storage::Int(_) | Storage::IntArray(_) | Storage::FloatArray(_) | Storage::Text(_) => {
            Err(invalid_attribute(
                "Attribute.of_float_planes",
                "not a float storage",
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Owner, Storage};


    #[test]
    fn name_whitespace_is_ascii_only() {
        let whitespace = " \t\n\r\u{000c}";
        assert!(super::create_owned(whitespace, Owner::Point, Storage::Float(vec![])).is_err());
        let nbsp = "\u{00a0}";
        let value = super::create_owned(nbsp, Owner::Point, Storage::Float(vec![])).unwrap();
        assert_eq!(super::name(&value), nbsp);
        assert!(super::with_name(nbsp, &value).is_ok());
    }
    #[test]
    fn typed_key_checks_owner_and_storage_and_rename_shares_payload_identity() {
        let value = super::create_owned("signal", Owner::Point, Storage::Float(vec![1.0, 2.0]))
            .unwrap();
        let key = super::key("signal", Owner::Point, super::float());
        assert!(matches!(
            super::get(&key, &value).unwrap(),
            Some(Storage::Float(values)) if values == vec![1.0, 2.0]
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
            Storage::Float(vec![1.0, 2.0])
        });
        let value = super::create_deferred(
            None,
            None,
            "lazy",
            Owner::Point,
            &Storage::Float(Vec::new()),
            2,
            materialise,
        )
        .unwrap();
        assert!(super::resident(&value));
        assert_eq!(super::length(&value), 2);
        assert_eq!(super::kind_name(&value), "float");
        assert_eq!(super::payload_bytes(&value), 16);
        assert_eq!(reads.load(Ordering::SeqCst), 0);
        assert!(matches!(super::storage(&value).unwrap(), Storage::Float(values) if values == vec![1.0, 2.0]));
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
            Storage::Int(vec![1, 2, 3])
        });
        let value = super::create_deferred(
            None,
            None,
            "lazy_int",
            Owner::Point,
            &Storage::Int(vec![0; 32]),
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
        let value = Storage::Float(vec![1.0, 2.0]);
        let planes = super::float_planes(&value).unwrap().unwrap();
        assert_eq!(planes[0], &[1.0, 2.0]);
        assert!(matches!(super::of_float_planes(&value, &planes).unwrap(), Storage::Float(result) if result == vec![1.0, 2.0]));
    }

    #[test]
    fn text_storage_and_remap_preserve_every_byte() {
        let bytes: Vec<u8> = (0..=255).collect();
        let reversed: Vec<u8> = bytes.iter().rev().copied().collect();
        let source = super::create_owned("bytes", Owner::Point, Storage::Text(vec![bytes.clone(), reversed.clone()])).unwrap();
        assert!(matches!(super::View::storage(&source).unwrap(), Storage::Text(values) if values == &vec![bytes.clone(), reversed.clone()]));
        for grain in [1, 97] {
            let remapped = crate::topology_remap::attribute(None, grain, Some(true), &[1, -1, 0, 1], &source).unwrap();
            assert!(matches!(super::storage(&remapped).unwrap(), Storage::Text(values) if values == vec![reversed.clone(), Vec::new(), bytes.clone(), reversed.clone()]));
        }
        assert!(matches!(super::storage(&source).unwrap(), Storage::Text(values) if values == vec![bytes, reversed]));
    }
}
