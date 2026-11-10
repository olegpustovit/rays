# Adding a native SOP node

A native SOP supplies a `Meta` constant, ordered `Spec` values, and an `operator` function.
`decl::native(&META, specs(), operator)` builds its factory through the shared schema,
validation, arity, facts and parameter-edit machinery. Lisp `defnode` bodies remain the public
implementation for operations already expressible in Rays Lisp.

Native operators read schema-normalised `Value`s directly into the RDK kernel's `Params`.
Capture scalar values and decode encoded tables when constructing the node; resolve named
geometry selections and take the current context's cancellation token and grain inside the
cook closure. There is no intermediate `Parameters` record or conversion pair.

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

## Native contracts

- Node ids, versions and `parameters`/`parameter_key`/`facts_key` bytes are cache identity: keep
  their existing bytes unchanged.
- Cook closures run on pool threads: no `Rc`/`RefCell`; share with `Arc`.
- Cooks report typed failures through `Result`; invalid input must not become a process panic.
- Do not add a parameter, default or range that the Lisp node does not have; the manifest is the
  contract.

Historical mapping notes: [docs/history/ocaml-sop-porting.md](../../docs/history/ocaml-sop-porting.md).
