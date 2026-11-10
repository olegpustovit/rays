mod eval;
pub use eval::*;
#[cfg(test)]
mod test_op;
#[cfg(test)]
mod test_workspace;
#[cfg(test)]
mod test_frame;
#[cfg(test)]
mod test_state;
#[cfg(test)]
mod test_array;
#[cfg(test)]
mod test_workspace_eval;
#[cfg(test)]
mod test_reentrant;
#[cfg(test)]
mod test_safety;

#[cfg(test)]
mod shared_ownership {
    use flow_syntax::{ty::Ty, value::{self, Value}};

    #[test]
    fn populated_binary_records_and_geometry_payloads_clone_without_allocating() {
        let binary = (0..65536).map(|i| [0, 255, 65, 128][i % 4]).collect();
        let fields = vec![
            ("z".into(), Value::Text(binary)),
            ("a".into(), Value::Record(vec![("same".into(), Value::Int(7))].into())),
            ("z".into(), Value::Bool(true)),
        ];
        let record = Value::Record(fields.into());
        let fields = record.geo_fields().unwrap().clone();
        let sources = [record, value::struct_value("z2/struct", Ty::Any, fields.clone()), value::source_value("z2/source", Ty::Any, fields)];
        let started = std::time::Instant::now();
        mem_sys::local_reset();
        let begin = mem_sys::local_alloc_count();
        for _ in 0..1024 {
            for source in &sources { std::hint::black_box(source.clone()); }
        }
        let allocations = mem_sys::local_alloc_count() - begin;
        let elapsed = started.elapsed();
        eprintln!("Z2: 3072 populated binary/nested record and geometry clones: {allocations} allocations/{elapsed:?}");
        assert_eq!(allocations, 0, "immutable values share text, fields and shape owners");
    }
}
