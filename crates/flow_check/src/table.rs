//! Typed TSV resources. A checked document owns its table snapshots; no frame opens a file.
use crate::op::{Op, Shape, Signature};
use flow_syntax::{context, symbol::Symbol, ty::Ty, value::{self, Value}};
use rays_diagnostic::Diagnostic;
use rays_math::text_array::TextArray;
use std::{collections::HashMap, path::PathBuf, sync::{Arc, Mutex}};

fn error(code: &str, message: impl Into<String>) -> Diagnostic { value::failf(code, &message.into()) }

/// TSV has a `name:int`, `name:float` or `name:text` header. Text uses backslash escapes
/// (`\\`, `\t`, `\n`, `\r`); quotes have no special meaning.
pub fn parse(source: &str) -> Result<(Ty, Value), Diagnostic> {
    let mut lines = source.lines();
    let header = lines.next().ok_or_else(|| error("E_TABLE_HEADER", "A table needs a typed header."))?;
    let mut fields = Vec::new();
    let mut columns = Vec::new();
    for cell in header.trim_start_matches('\u{feff}').split('\t') {
        let (name, kind) = cell.split_once(':').ok_or_else(|| error("E_TABLE_HEADER", "Expected name:int, name:float or name:text."))?;
        if name.is_empty() || !name.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_') || name.as_bytes()[0].is_ascii_digit() || fields.iter().any(|(old, _)| old == name) {
            return Err(error("E_TABLE_HEADER", format!("Invalid or duplicate column {name:?}.")));
        }
        let ty = match kind { "int" => Ty::Int, "float" => Ty::Float, "text" => Ty::Text, _ => return Err(error("E_TABLE_HEADER", format!("Unknown column type {kind:?}."))) };
        columns.push(match ty { Ty::Int => Value::IntArray(Arc::new(Vec::new())), Ty::Float => Value::Floats(1, Arc::new(Vec::new())), _ => Value::TextArray(Arc::new(TextArray::new())) });
        fields.push((Symbol::from(name), Ty::Array(Box::new(ty))));
    }
    for (row, line) in lines.enumerate() {
        let cells = line.split('\t').collect::<Vec<_>>();
        if cells.len() != columns.len() { return Err(error("E_TABLE_ROW", format!("Row {} has {} columns; expected {}.", row + 2, cells.len(), columns.len()))); }
        for (column, cell) in columns.iter_mut().zip(cells) {
            let bad = || error("E_TABLE_VALUE", format!("Invalid value at row {}: {cell:?}.", row + 2));
            match column {
                Value::IntArray(values) => Arc::make_mut(values).push(cell.parse::<i64>().map_err(|_| bad())?),
                Value::Floats(_, values) => { let n = cell.parse::<f64>().map_err(|_| bad())?; if !n.is_finite() { return Err(bad()); } Arc::make_mut(values).push(n); }
                Value::TextArray(values) => {
                    let mut text = Vec::with_capacity(cell.len());
                    let mut bytes = cell.bytes();
                    while let Some(b) = bytes.next() {
                        text.push(if b != b'\\' { b } else { match bytes.next() { Some(b'\\') => b'\\', Some(b't') => b'\t', Some(b'n') => b'\n', Some(b'r') => b'\r', _ => return Err(bad()) } });
                    }
                    Arc::make_mut(values).try_push(&text).map_err(|_| error("E_TABLE_SIZE", "Text table exceeds array capacity."))?;
                }
                _ => unreachable!(),
            }
        }
    }
    let values = fields.iter().zip(columns).map(|((name, _), value)| (*name, value)).collect::<Vec<_>>();
    Ok((Ty::Record(fields), Value::Record(values.into())))
}

pub const DOC: &str = "Load a typed UTF-8 TSV relative to the workspace file. Headers are name:int, name:float or name:text. Text escapes are backslash, \\t, \\n and \\r. The resource is snapshotted at check time; reload the workspace to refresh it. Native only.";

pub fn operator(base: PathBuf) -> Op {
    // ponytail: snapshots are retained per authored table path until document reload; use an LRU
    // if a long editing session starts opening thousands of distinct tables.
    let cache = Arc::new(Mutex::new(HashMap::<String, (Ty, Value)>::new()));
    let types = cache.clone();
    Op {
        name: "table/load".into(),
        doc: DOC.into(),
        example: "(table/load \"atlas.tsv\")".into(),
        ctx: context::value(),
        signature: Signature { pos: vec![("path".into(), Ty::Text)], opt: vec![], rest: None, kw: vec![] },
        out: Arc::new(|_| Ok(Ty::Any)),
        out_args: Some(Arc::new(move |args| {
            let Some(param::Value::Text(bytes)) = args.iter().find(|(name, _, _)| name == "path").and_then(|(_, _, value)| value.as_ref()) else { return Err(error("E_TABLE_PATH", "table/load requires a literal path.")); };
            let path = std::str::from_utf8(bytes).map_err(|_| error("E_TABLE_PATH", "Table path must be UTF-8."))?;
            let source = std::fs::read_to_string(base.join(path)).map_err(|e| error("E_TABLE_IO", format!("{path}: {e}. Generate the Source Atlas table with tools/source_atlas_index.")))?;
            let (ty, value) = parse(&source)?;
            types.lock().unwrap().insert(path.to_owned(), (ty.clone(), value));
            Ok(ty)
        })),
        body: Arc::new(move |_, _, args| {
            let Some(Value::Text(path)) = args.iter().find(|(name, _)| name == "path").map(|(_, v)| v) else { return Err(error("E_TABLE_PATH", "table/load requires a literal path.")); };
            let path = std::str::from_utf8(path).map_err(|_| error("E_TABLE_PATH", "Table path must be UTF-8."))?;
            cache.lock().unwrap().get(path).map(|(_, value)| value.clone()).ok_or_else(|| error("E_TABLE_PATH", "Table was not checked before evaluation."))
        }),
        shape: Shape::Scalar, live: false, check: Arc::new(|_| Ok(())),
        any_num: false, choices: vec![], category: "Table".into(), arithmetic: None, packed_extension: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn typed_tsv_and_diagnostics() {
        let (ty, value) = parse("parent:int\tweight:float\tlabel:text\r\n-1\t1.5\tλ\\tquoted\"\\n\\\\\r\n").unwrap();
        assert!(matches!(ty, Ty::Record(ref fields) if fields.len() == 3));
        let Value::Record(fields) = value else { panic!("record") };
        assert_eq!(fields.get_str("parent"), Some(&Value::IntArray(Arc::new(vec![-1]))));
        assert_eq!(value::array_get(fields.get_str("label").unwrap(), 0).unwrap(), Value::Text("λ\tquoted\"\n\\".as_bytes().into()));
        for (source, code) in [("", "E_TABLE_HEADER"), ("x:nope", "E_TABLE_HEADER"), ("x:int\tx:text", "E_TABLE_HEADER"), ("x:int\n1\t2", "E_TABLE_ROW"), ("x:int\n1.5", "E_TABLE_VALUE"), ("x:float\nNaN", "E_TABLE_VALUE"), ("x:text\n\\q", "E_TABLE_VALUE")] { assert_eq!(parse(source).unwrap_err().code, code); }
        assert!(parse("x:text\n\n").is_ok());
    }
}
