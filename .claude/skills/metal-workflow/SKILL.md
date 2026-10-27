---
name: metal-workflow
description: Take a GPU capability rays does not have yet from Metal to its first caller - declare the Metal call in the binding registry (generated, not handwritten), wrap it safely, expose it through OGPU, and use it from the runtime or path tracer. Use for any new Metal API, new OGPU feature backed by Metal, or when removing one.
---

# Metal workflow: need → generated binding → OGPU → caller

`eval "$(opam env --switch=. --set-switch)"` first. Loops: `dune build @check`
(types), `dune build @all` (also compiles the Objective-C++ bridge with
`-Werror`), focused tests per step below.

## 0. Is it already there?

- Callers (`runtime`, `scene_execution`, `rays_pathtracer`) talk to the
  virtual `Ogpu` API only; grep `lib/ogpu_core/*.mli` for the operation.
- The Metal backend calls the safe `Metal` API: grep `lib/metal/metal.mli`.
- Both exist → just call it. Only the Metal side exists → skip to step 3.

## 1. Declare the native call (`lib/metal/gen/registry.ml`)

Add an entry to `entries`; the generator writes the raw external
`Metal_raw.Registry.<ocaml>` and a typed Objective-C stub under `_build`.

```ocaml
  ; Method   (* [device newBufferWithLength:options:] -> owned MTLBuffer *)
      { recv = "Device"; objc = "id<MTLDevice>"
      ; sel = "newBufferWithLength:options:"
      ; args = [ Scalar Nsuint; Enum_of "MTLResourceOptions" ]
      ; ret = Some (Obj "Buffer"); error = false
      ; ocaml = "device_new_buffer"; since = None
      ; feature = Ogpu_core.Caps.Buffer }
  ; Property (* object.label, read and write *)
      { recv = "Buffer"; objc = "id<MTLBuffer>"; name = "label"
      ; ty = Str; access = Get_set          (* -> buffer_label, set_buffer_label *)
      ; ocaml = "buffer_label"; since = None
      ; feature = Ogpu_core.Caps.Buffer }
```

- `recv`/`Obj k`/`Opt_obj k` name a bridge `Handle_kind` (see `enum class
  Handle_kind` in `metal_bridge.mm`; add a kind there if the object is new).
- Types: `Scalar Bool|Int|Nsuint|Nsint|Float|Double`, `Enum_of "MTLType"`
  (int64), `Str` (NSString), `Obj k` (handle, retained when returned),
  `Opt_obj k`. `error = true` appends `error:` (NSError**) and turns failure
  into `Error`. `since = Some (major, minor)` guards with `@available`.
- A constant set or fixed struct: `Enum` / `Record` entries (same file).
- The SDK is the oracle: a wrong selector, type or missing `since` is a clang
  error in `dune build @all`. Fix the entry; never silence the warning.
- Only blocks/callbacks, descriptor graphs, handle arrays, ownership
  transfer and native structs stay handwritten (`metal_bridge.mm` +
  `metal_raw.ml`). If you edit a handwritten stub the registry can express,
  move it to the registry and delete the handwritten one.

## 2. Safe wrapper (`lib/metal/metal.ml` + `.mli`)

The generator refuses an entry `metal.ml` does not call, so the wrapper lands
in the same change. Follow the existing shape:

```ocaml
  let label (value : t) =
    let operation = "Metal.Buffer.label" in
    on_main operation (fun () ->
        match ensure_live operation value.lifetime with
        | Error _ as failure -> failure
        | Ok () -> (
            match Metal_raw.Registry.buffer_label value.raw with
            | Error message -> native_error operation message
            | Ok label -> Ok label))
```

Validate before the native call (live handle, same device, ranges, NUL-free
strings) and return typed errors; the `.mli` exposes no raw values.
Test: one success and one rejection in a `lib/metal/test_metal_*_safe.ml`
suite (new file → add it to `modules` in `lib/metal/dune` and to
`lib/metal/test_main.ml`). `dune build @lib/metal/runtest`.

## 3. Expose through OGPU

Follow the `add-ogpu-feature` skill: capability gate in
`lib/ogpu_core/caps.ml`, the operation in `ogpu_core` (`backend.mli` driver
field + typed wrapper module), the Metal implementation in `lib/ogpu_metal`
calling the step-2 function, the mock in `lib/ogpu_mock`, conformance cases in
`lib/ogpu/conformance`. If the feature's Metal types changed, update
`feature_map` in `registry.ml`. `dune build @lib/ogpu/runtest
@lib/ogpu_metal/runtest @lib/ogpu/conformance/runtest`.

## 4. Use it

Call `Ogpu.*` from `runtime`/`scene_execution`/`rays_pathtracer` — never
`Metal.` (the dependency gate rejects it). Then `dune build @all`, window-free
`SDL_VIDEODRIVER=dummy SDL_AUDIODRIVER=dummy dune runtest`, `dune build
@smoke`, and `git diff --check`. Record the capability in `specification/backend.md`
and anything user-visible in `specification/metal.md`.

## Removing a capability

Delete the caller; then `dune exec tools/codemod/codemod.exe -- prune
lib/ogpu_core lib/ogpu_metal lib/metal` removes the now-unreferenced OGPU and
Metal functions, the registry entry and the raw external; finish with
`codemod.exe -- dead-stubs lib/metal/metal_bridge.mm lib tools test` and
`codemod.exe -- drop-c-unused lib/metal` for handwritten stubs (see the
`prune-dead-code` skill).
