# Porting a SOP node (the ppx_rays replacement)

OCaml declares a node as a `parameters` record with `[@sop.*]` field attributes and `[@@sop.node_*]`
type attributes, then `[@@deriving sop_params, sop_node]` generates `parameters_default`,
`parameters_schema`, `parameters_build` and `parameters_factory`. There are no proc macros here:
those four are `decl.rs` (`Meta`, `Spec`, `schema`, `build`, `factory`), and a node file supplies the
data and the operator. `rs/crates/sop/src/test_manifest.rs` renders every factory in
`nodes::factories()` in the format of `lib/flow_sop/flow_manifest.sexp` and compares it with that
file's `(kind "sop/<key>" ...)` block, so a wrong label, slot, field, range or default fails a test.

## Mapping (every ppx attribute has one home)

| OCaml | Rust |
|---|---|
| `[@@sop.node_key/_label/_category/_inputs]` | `Meta { key, label, category, inputs, .. }` |
| `_operation`, `_slots "a, b"`, `_optional "2"`, `_rest 2`, `_types`, `_keywords` | same-named `Meta` fields (`operation`, `slots`, `optional`, `rest`, `types`, `keywords`) |
| `[@@sop.node_facts {..}]`, `[@@sop.validate f]` | `Meta.facts: Some(FactsDecl {..})`, `Meta.validate: Some(f)` |
| `[@@sop.node_fallible]` | nothing: the operator always returns `Result<node::T, String>` |
| record type name (schema name) | `Meta.schema_name` (default `"parameters"`) |
| one record field | one `Spec` in `specs()`, built with `decl::spec(name, kind, default)` and struct update |
| `[@sop.label/description/folder/impact/primary/unit/vec3]` | `Spec { label, description, folder, impact, primary, unit, vec3, .. }` |
| `[@sop.min/max/hard_min/hard_max]` on `int`/`float` | `decl::int_kind(min, max, hard_min, hard_max)` / `decl::float_kind(..)` |
| `bool` / `string` field | `decl::toggle_kind()` / `decl::text_kind()` |
| `[@sop.kind some_parameter]` (a `Param.choice`/`encoded`) | a `Kind` fn from `support.rs` (`group_owner_kind()`, ...) or the module's own |
| `[@sop.ignore]` | not a `Spec`; keep the data out of the values slice |
| `[@sop.default e]` | the `Value` passed to `spec` (`Value::Bool/Int/Float/Text/Choice`) |
| `parameters_default` | `decl.schema.default()`; there is no second copy |
| `type parameters = unit` | `specs()` returns `Vec::new()` |
| the record | `Parameters` struct + `from_values(&[Value])` (+ `to_values`), field order = `Spec` order |
| `parameters_build ?schema (fun ~label p a b -> node)` | `operator(label, values, inputs) -> Result<node::T, String>`; `?schema` is `Decl.dynamic_schema` |
| `parameters_factory build` | `decl::factory(&decl())` |

Values are positional: `values[i]` is the i-th `Spec`. Strings in `Value::Text` are bytes
(`Vec<u8>`); `Value::Choice` holds the label.

`inputs: &[Option<node::T>]` has one entry per slot. A required slot is always `Some`; an optional
slot may be `None`; a rest slot continues to the end (use `decl::connected(inputs)` for the
connected ones, which is OCaml's `List.filter_map Fun.id`). `decl::build` does the arity and
"every required input connected" checks, `validate`, the facts refinement, `Node.parameterize`
(schema normalisation, cache text and key) and the rebuild closure that survives parameter edits
and wiring changes. `decl::factory` builds `Edit_graph.factory` or `factory_slots` from `Meta`.

Choice parameters over an OCaml variant (support.rs): an enum whose discriminants are the table
positions, `FOO_LABELS: [&str; N]`, `FOO: [E; N]`, `foo_kind()`, `foo_of_value(&Value) -> E` and
`foo_value(E) -> Value`. An rdk enum gets the same tables and the same two conversions.

## The recipe, with `groups::group_transfer` (src/groups.rs) as the complete example

1. Create `pub mod <name>` in the node's file (`shapes.rs`, `sop_topology.rs`, ...), one per OCaml
   `module X = struct ... end`.
2. `Parameters`, `from_values`, `to_values` (see `group_transfer`). `from_values` may `expect`: the
   schema has already validated and normalised the record.
3. `pub const META: Meta = Meta { key: "...", label: "...", category: "A/B", inputs: N, ..decl::META };`
4. `pub fn specs() -> Vec<Spec>`: copy the record fields in order; map each attribute with the table.
5. `pub fn operator(label, values, inputs) -> Result<node::T, String>`: the body of the OCaml
   `parameters_build` lambda. `Node.Build.make_geometry ?label ~operation ... cook` becomes
   `build::make_geometry(build::Args { label: Some(label.to_owned()), cook_mode, inputs, ..build::args("op") }, expand, Arc::new(move |node_id, context, inputs| ...))`;
   clone what the closure needs into it (it must be `Send + Sync`). `cooked g` is
   `support::cooked(g)`, `structured_rdk_error e` is `support::structured_rdk_error(&e)`, and
   `Context.cancel_token`/`grain` are `context::cancel_token(context)` (an `Arc<cancel::T>`) /
   `context::grain(context)`.
6. `pub fn decl() -> Decl` and `pub fn factory() -> Factory` exactly as in the examples.
7. Add the factory to `nodes::build_all` at the OCaml position (uncomment its line).
8. Run `cargo t -p sop`: `test_manifest` compares the new node with the manifest.

`shapes::null` is the smallest example (no fields). `shapes::switch` shows optional and rest
slots, an `input_policy`, a `cook_mode` with an index, and a dynamic schema: `specs(choices)` takes
the optional choice labels, `schema(values, made_node)` is the `?schema` callback.

## Rules the translation keeps

- Node ids, versions and `parameters`/`parameter_key`/`facts_key` bytes are cache identity: keep
  `~parameters:"..."` strings byte-identical (`Vec<u8>` via `.into_bytes()`).
- Cook closures run on pool threads: no `Rc`/`RefCell`; share with `Arc`.
- An OCaml exception from a cook (`invalid_arg`) is a `panic!`/`assert!`; a `Result` is a `Result`.
- Do not add a parameter, default or range that the Lisp node does not have; the manifest is the
  contract.
