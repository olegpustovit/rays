# Adding a native SOP node

A native SOP supplies a `Meta` constant, ordered `Spec` values, and an `operator` function.
`decl::native(&META, specs(), operator)` builds its factory through the shared schema,
validation, arity, facts and parameter-edit machinery. Lisp `defnode` bodies remain the public
implementation for operations already expressible in Rays Lisp.

Native operators read schema-normalised `Value`s directly into the RDK kernel's `Params`.
Capture scalar values and decode encoded tables when constructing the node; resolve named
geometry selections and take the current context's cancellation token and grain inside the
cook closure. There is no intermediate `Parameters` record or conversion pair.

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
| the record | read the schema values directly; field order = `Spec` order |
| `parameters_build ?schema (fun ~label p a b -> node)` | `operator(label, values, inputs) -> Result<node::T, String>`; `?schema` is `Decl.dynamic_schema` |
| `parameters_factory build` | `decl::native(&META, specs(), operator)` |

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

## The recipe

1. Add `pub mod <name>` to the appropriate node file (`shapes.rs`, `sop_topology.rs`, ...).
2. Define `pub const META: Meta` and `specs() -> Vec<Spec>`. Preserve field order, defaults,
   labels, ranges, slots, facts and validation callbacks: they are catalog contracts.
3. Implement `operator(label, values, inputs) -> Result<node::T, String>`. Read only the values
   it needs, once, and construct the native `Params` inside its cook closure. Required inputs
   are `Some`; use `decl::connected` for rest inputs. Cook closures must be `Send + Sync`.
4. Implement `factory() -> Factory { decl::native(&META, specs(), operator) }` and register it
   in `nodes::build_all` in the catalog's existing order.
5. Run `cargo t -p sop`, then the affected Flow and editor catalog checks. The manifest must
   stay byte-identical unless a schema change was intentional.

`shapes::null` is the smallest example. `groups::group_transfer` demonstrates a table decoded
once before cooking. `shapes::switch` keeps a custom `Decl` because its schema depends on the
connected branches; its optional/rest slots and rebuild path still use `decl::factory`.

Do not move `Meta.validate` into a cook: invalid edits must be rejected before cooking, with
the same diagnostic. Geometry-dependent group lookup belongs inside the cook so changed
inputs are resolved again. Decode encoded rules outside the closure and clone their native
representation only when the kernel needs ownership.

## Rules the translation keeps

- Node ids, versions and `parameters`/`parameter_key`/`facts_key` bytes are cache identity: keep
  their existing bytes unchanged.
- Cook closures run on pool threads: no `Rc`/`RefCell`; share with `Arc`.
- An OCaml exception from a cook (`invalid_arg`) is a `panic!`/`assert!`; a `Result` is a `Result`.
- Do not add a parameter, default or range that the Lisp node does not have; the manifest is the
  contract.
