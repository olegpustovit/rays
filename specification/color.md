# Color

`Color.t` is an immutable RGBA value with four clamped 8-bit channels:

```text
type t = { r : int; g : int; b : int; a : int }
```

`rgb`, `rgba`, and `gray` clamp integer input to `0..255`. `of_floats` accepts
normalized channels and returns the corresponding byte representation.
`to_tuple`, `to_floats`, `to_hex`, and `to_string` expose deterministic value
conversions.

## Construction and parsing

- `from_hex` interprets the integer form used by the public API.
- `hex` parses `#rgb`, `#rgba`, `#rrggbb`, and `#rrggbbaa`, returning a result.
- `hex_exn` is the convenience for trusted literals.
- `hsv` and `hsl` take hue in degrees and normalized remaining channels; hue
  wraps and other components are clamped.
- Named constants include black, white, primary/secondary colors, grays, and
  transparent.

## Operations

`with_alpha`, `blend`, `gradient`, `lighten`, `darken`, `saturate`,
`desaturate`, `rotate_hue`, and `invert` return new values. `gradient` clamps
the sample position and interpolates the supplied stops. Current blending and
palette interpolation operate on encoded sRGB bytes; a future linear-light
API must use a distinct name rather than changing these results silently.

## Native boundary

Scene construction stores `Color.t` unchanged. Native lowering converts
channels to the format required by Metal uniforms, vertex payloads, clear
values, or texture uploads. Color values do not contain renderer handles and
are safe to share between domains.

The public byte ordering and parse/round-trip behavior are regression-tested.
Framebuffer qualification additionally verifies clear, vertex, texture,
blend, and alpha behavior through the native render path.
