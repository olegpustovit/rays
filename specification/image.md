# Images and native textures

`Image.t` is an opaque, explicitly owned image resource. It preserves logical
width and height, RGBA8 content, stable identity, and a generation counter
inside the private resource layer. Public code never receives an SDL3 surface,
Metal texture, OGPU handle, or native pointer.

## Construction and loading

```ocaml
val load : string -> (Image.t, string) result
val load_exn : string -> Image.t
val create : width:int -> height:int -> ?color:Color.t -> unit -> Image.t
val get_width : Image.t -> int
val get_height : Image.t -> int
val get_size : Image.t -> int * int
val destroy : Image.t -> unit
```

`Image.load` reads a still image with SDL3_image and copies the decoded result
into tightly packed RGBA8 storage. Supported formats follow the qualified
SDL3_image build. Missing files, malformed data, invalid dimensions, and native
allocation failures return contextual errors without publishing a partial
resource. `load_exn` is the prototyping convenience that raises on the same
error.

`Image.create` allocates a checked RGBA8 image and initializes every pixel to
the supplied color, or transparent black by default. Dimensions must be
positive and their byte cardinality must fit the supported integer range.

Decode and native-resource work execute on the initial domain. Parallel asset
preparation may read ordinary file bytes, but SDL3_image decode and Metal upload
join back to the initial domain.

## Drawing and snapshots

The high-level path is pure scene construction:

```ocaml
Scene.image image ~at:(x, y) ~scale:2. ~angle ()
```

`Scene.image` records image identity and generation in the immutable scene.
The effect boundary resolves that snapshot into a checked OGPU sampled resource
and retains it through Metal command completion.

`Canvas.to_image` returns a new owned image snapshot. Internal stable-identity
copying can refresh an existing same-sized image without allocating another
wrapper. Native readback is explicit through `Canvas.capture`; ordinary image
drawing never reads pixels back from the GPU.

## Ownership

Lisp `image/map` evaluates a Vec2-to-Vec4 function at pixel-center UV
coordinates, top row first, and cooks one owned RGBA8 output buffer. Each
finite channel is clamped to [0,1] and multiplied by 255. Let `n=floor(q)`:
increment it when the fractional part exceeds 0.5, or equals 0.5 and `n` is
odd. Thus scaled values 0.5, 1.5, 2.5 and 254.5 become 0, 2, 2 and 254.
Nonfinite channels refuse the cook. One/eight-domain CPU cooks produce the
same bytes. Existing float-backed procedural images retain their float
precision; exact sampling of RGBA8 images expands bytes by division by 255.

Loaded or created images are owned by the caller and belong in
`Sketch.run_state ~on_stop`:

```ocaml
let on_stop model =
  match Image.destroy model.sprite with
  | Ok () -> ()
  | Error message -> prerr_endline message
```

Destruction is idempotent. Stale resource use is rejected at the checked native
boundary, and command completion retains submitted resources long enough for
in-flight GPU work.

Private runtime images can also borrow completed RGBA8 GPU output. Publication
validates dimensions, format, single-sample depth-one storage and sampling/readback
usages before changing the image. It preserves image identity, increments the
generation on replacement and keeps no CPU pixel buffer. The owner supplies a
callback that returns the published texture while its output remains current.
Expiration, destruction or substitution without republication returns a typed
resource error, including when a renderer reuses a cached Scene. Publication
does not read pixels; explicit pixel and snapshot requests perform checked
readback. Snapshot bytes remain valid across replacement, resize and destruction.
Destroying an image drops the borrow without destroying its owner's texture.
Successful CPU replacement or Canvas copying clears the GPU backing.

## Performance contract

Image storage is packed rather than a boxed per-pixel OCaml structure. Stable
identity plus generation-based invalidation lets renderer caches reuse unchanged
uploads. Changing content invalidates only the affected resource generation;
unreachable or destroyed images do not keep native textures alive indefinitely.
