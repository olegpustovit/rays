# Metal binding contract

`metal_sys` provides the foundational Apple Silicon Metal binding
under `ogpu_metal`. They do not own SDL windows or depend on the editor. Native
rendering has no CPU, browser or OpenGL fallback; web export is separate.

## Build boundary

Required Objective-C++ ABI code and checked-in declarations live under
`crates/metal_sys/src/`. The owning build script uses the installed public SDK
as its type and availability oracle, compiles through `cc` with ARC, and links
the required Apple frameworks. SDK headers and framework binaries are system
inputs and are not distributed. No binding generator runs during the build.
Enum constants and shared record layouts are checked against SDK declarations.
Capabilities must describe actual device support and retain explicit availability
guards for newer APIs. Bind only what a real `ogpu_metal` consumer needs.

## Validation and ownership

- Expose opaque owned handles and typed errors. Validate live state, generation,
  same-device ownership, ranges, alignment, cardinality, encoder state and
  capabilities before sending a native update.
- Expected rejections never invoke an Objective-C update. Native exceptions and
  failures cross the ABI as owned diagnostics. Unsupported features return a
  typed unsupported/startup error.
- Native operations run on the initial thread and platform main executor.
  Wrong-thread calls fail before native access. Deferred release queues carry
  opaque tokens; draining and native destruction stay on the initial thread.
- Objective-C calls own autorelease pools. Retain handle lookups explicitly;
  copied strings, reflection data and callback roots cannot outlive their owners.
- Destruction is idempotent. Stale use, cross-device binding and destruction of a
  parent with live dependents fail before native access. Submission resources
  remain retained until completion, including across blocking waits.
- Buffers, mapped ranges, textures, samplers, shader libraries/functions,
  pipelines, encoders, acceleration structures and presentation resources use
  the same ownership checks. Release accounting must return to baseline after
  complete lifecycles.

## Verification and extension

Run `cargo t -p metal_sys` and `cargo t -p ogpu_metal` for
binding and adapter checks. Success, rejection, retention, exact readback and
live-handle deltas need coverage. Display-dependent lifecycle checks require a
real display; unsupported hardware is a visible failure unless an explicit
skip is authorized.

`RAYS_METAL_SANITIZERS` selects supported native bridge sanitizers. Address and
undefined checks may combine; thread sanitizer is exclusive. Shader source
compiles at runtime through the public Metal API. There is no offline native
shader artifact dependency.

A new binding needs a concrete consumer, capability check, narrow safe API,
validation, owned errors, required sys ABI code and success/rejection checks.
Historical generator migrations and measurements are preserved in
[history](../docs/history/README.md).
