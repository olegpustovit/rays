---
name: add-ogpu-feature
description: Add a GPU feature to OGPU in rays, capability-gated and implemented on both backends. Use when the runtime or path tracer needs a GPU operation the virtual ogpu API does not expose yet.
---

# Add an OGPU feature

1. Name the consumer (`runtime`, `scene_execution`, `rays_pathtracer`)
   and the `Ogpu_core.Caps.feature` that gates it. Reuse an existing feature
   when one fits; otherwise add the variant and its `t` field in
   `lib/ogpu_core/caps.ml/.mli` and teach `has`/`require` about it. Baseline
   features (buffers, textures, queues, pipelines) need no gate.
2. Add the operation to `ogpu_core`: a driver-record function in
   `backend.mli` plus the typed wrapper module callers use. The interface
   never names a backend type. Errors are `Error.t` results; a missing
   capability is `Error.Unsupported`, never a silent no-op.
3. Implement it in `lib/ogpu_metal` (Metal calls via the `metal-workflow`
   skill) and in `lib/ogpu_mock/impl.ml`: real behaviour when the mock can
   model it, otherwise the module's `unsupported` helper.
4. Add conformance cases in `lib/ogpu/conformance/conformance.ml`: exact
   behaviour when `Caps.has` the feature, typed `Unsupported` otherwise, and
   no live handle left after destroy. The same file runs on the mock and on
   Metal. Unit cases go in `lib/ogpu/test_ogpu_*.ml` and
   `lib/ogpu_metal/test_ogpu_metal_*.ml`.
5. If the feature's Metal symbols changed, update `feature_map` in
   `lib/metal/gen/registry.ml` (the generator rejects a missing entry). Record
   the feature in the OGPU section of `specification/backend.md`.
6. Run `dune build @lib/ogpu/runtest @lib/ogpu_metal/runtest
   @lib/ogpu/conformance/runtest`, then `@all` and `git diff --check`.
