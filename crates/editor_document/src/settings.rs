//! sketch-owned settings, a typed `Param` record stored
//! with its schema so the document stays one immutable value. A record is the schema's `Value`
//! slice, positional.
use param::{Effects, FieldView, Schema, Value};
use std::sync::Arc;

#[derive(Clone)]
pub struct T {
    schema: Arc<Schema>,
    values: Arc<Vec<Value>>,
}

pub fn make(schema: &Arc<Schema>, values: Vec<Value>) -> T {
    T { schema: schema.clone(), values: Arc::new(values) }
}

/// The empty settings record is shared: two none() calls have the same stored values.
pub fn none() -> T {
    static NONE: std::sync::OnceLock<T> = std::sync::OnceLock::new();
    NONE.get_or_init(|| make(&Arc::new(param::schema("none".to_owned(), Vec::new()).expect("valid schema")), Vec::new())).clone()
}

/// The values, when they are a record of `schema` itself.
pub fn record_of<'a>(settings: &'a T, schema: &Schema) -> Option<&'a [Value]> {
    std::ptr::eq(&*settings.schema, schema).then(|| settings.values.as_slice())
}

pub fn fields(settings: &T) -> Vec<FieldView> {
    settings.schema.view(&settings.values).expect("settings are valid by construction")
}

/// One field's current value, without viewing every field.
pub fn value(settings: &T, name: &str) -> Option<Value> {
    settings.schema.index_of(name).and_then(|i| settings.values.get(i)).cloned()
}

pub fn defaults(settings: &T) -> T {
    make(&settings.schema, settings.schema.default().to_vec())
}

/// Read the stored record through its schema by field name; return Err for an incompatible schema.
pub fn get(schema: &Schema, settings: &T) -> Result<Vec<Value>, String> {
    let values: Vec<(String, Value)> =
        fields(settings).into_iter().map(|field| (field.name, field.current)).collect();
    schema
        .apply_all(schema.default(), &values)
        .map(|(value, _)| value)
        .map_err(|message| format!("Rays_editor.Settings.get: {message}"))
}

pub fn apply(settings: &T, changes: &[(String, Value)]) -> Result<(T, Effects), String> {
    settings
        .schema
        .apply_all(&settings.values, changes)
        .map(|(values, effects)| (T { schema: settings.schema.clone(), values: Arc::new(values) }, effects))
}

/// Physical identity of the stored value record.
pub fn same_values(a: &T, b: &T) -> bool {
    Arc::ptr_eq(&a.values, &b.values)
}

pub fn schema_of(settings: &T) -> &Arc<Schema> {
    &settings.schema
}

#[cfg(test)]
mod tests {
    #[test]
    fn none_is_one_shared_value() {
        assert!(super::same_values(&super::none(), &super::none()));
    }
}
