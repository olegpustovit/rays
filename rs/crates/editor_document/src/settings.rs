//! Port of lib/editor_document/settings.ml: sketch-owned settings, a typed `Param` record stored
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

/// One shared value, as in OCaml: `same_values (none ()) (none ())` holds.
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

pub fn defaults(settings: &T) -> T {
    make(&settings.schema, settings.schema.default().to_vec())
}

/// The stored record read through `schema`, matching fields by name. `Err` when `schema` does not
/// describe it (OCaml raises `Invalid_argument`).
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

/// Physical identity of the stored record (OCaml `==` on `values`).
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
