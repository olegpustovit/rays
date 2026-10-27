# lib/ogpu rules

OGPU is Rays's GPU API: modelled on Metal's object model with immediate-mode
encoders and backend-agnostic. It carries what the runtime
and the path tracer call, plus what keeps it backend-agnostic (the virtual
library, the driver records, `Caps`); an operation with no product caller is
deleted, with its Metal binding, mock arm and conformance case, and comes
back with its first caller. Metal (`ogpu_metal`) is the only backend today; a
Vulkan backend must be addable without changing callers.

- `ogpu_core` depends only on `native_layer_token`. Virtual `ogpu` aliases
  the core and depends only on it. Backend types never appear in its interface.
- Every non-baseline feature is checked through `Caps` and otherwise returns a
  typed `Unsupported` error. Never a silent no-op, never a lowest common
  denominator cut.
- `test/dependency_gate.ml` enforces the native dependency direction: product
  code above the Metal backend cannot reach `Metal.` or `Ogpu_metal_native.`.
  Tooling and backend conformance have explicit source-token allowances.
- Every feature has conformance tests on every backend: exact behavior when
  `Caps.has` it, the typed `Unsupported` otherwise; include lifetime and
  no-handle-leak checks.
- Shader descriptors are backend-tagged; interfaces are checked against
  reflection at pipeline creation. Rays Lisp and the shared region IR are the
  shader source direction. Existing handwritten shaders are retired by the
  tracked path-tracer and raster phases; do not add another shader language.
- Shape: dune virtual library with `ogpu_metal`/`ogpu_mock` implementations,
  one backend per executable, conformance suite over the mock and Metal
  covering every capability that exists (compute and render pipelines, ray
  tracing incl. motion, curves and intersection tables).
- Workflow for a new capability: the `add-ogpu-feature` skill. Removing one:
  delete the caller, then the `prune-dead-code` skill (`prune`, `dead-fields`
  for the driver records, `dead-stubs`, `drop-c-unused`).
