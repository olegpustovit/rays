# Vec2

`Vec2` is an immutable, renderer-independent mathematical value. It contains no
native resource and is safe to share across worker threads.

```text
type t = { x : float; y : float }
```

The module provides construction, zero/unit vectors, addition, subtraction,
negation, scalar multiplication, dot product, squared length, length,
normalization, distance, angle, rotation, interpolation, and epsilon-aware
comparison.

`normalize Vec2.zero` follows the module's defined safe result; callers that
need to distinguish a degenerate direction should check `length_sq` first.
Pair conversions are explicit: integer conversion is for logical drawing
coordinates, while `to_pair_float` preserves the mathematical value.

## Scene use

`Scene.translate`, `rotate`, `scale`, and grouped transforms remain pure scene
constructors. Native lowering composes their mathematical values before OGPU
command recording. Metal receives the final checked transform data.

## Regression requirements

- vector identities, zero/near-zero behavior, rotation, and interpolation;
- nested scene-transform order through a native framebuffer fixture;
- exact one-domain/multi-domain results for pure bulk transforms.
