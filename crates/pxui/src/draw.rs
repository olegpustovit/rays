//! The draw list: plain 64-byte instances, one vector per UI layer. `pxui_render` draws a layer in
//! one call; the instance layout and its conventions are documented there.

/// Instance kinds (byte 50).
pub mod kind {
    pub const RECT: u8 = 0;
    pub const MASK: u8 = 1;
    pub const IMAGE: u8 = 2;
    pub const SHADOW: u8 = 3;
    pub const LINE: u8 = 4;
    pub const ARC: u8 = 5;
    pub const CURVE: u8 = 6;
}
/// Instance flags (byte 51).
pub mod flags {
    pub const LINEAR: u8 = 1;
    pub const RADIAL: u8 = 2;
    pub const CONIC: u8 = 3;
    pub const INNER: u8 = 4;
    pub const ROUND: u8 = 8;
}
pub const NO_CLIP: [i16; 4] = [i16::MIN, i16::MIN, i16::MAX, i16::MAX];

/// One quad. Plain data: build with the constructors below or write the fields per the layout table.
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Instance {
    pub rect: [f32; 4],
    pub clip: [i16; 4],
    pub color: [u8; 4],
    pub color2: [u8; 4],
    pub aux: [u32; 2],
    pub radii: [u16; 4],
    pub width: u16,
    pub kind: u8,
    pub flags: u8,
    pub view: u16,
    pub clip_radius: u16,
    pub extra: [u32; 2],
}
const _: () = assert!(size_of::<Instance>() == 64);

/// From `at_instance` on, the layer's [`kind::IMAGE`] instances sample `images[image]`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Break {
    pub at_instance: u32,
    pub image: u32,
}

/// IEEE half bits of `x` (round to nearest even).
pub fn f16(x: f32) -> u16 {
    let bits = (x as f64).to_bits();
    let sign = if bits >> 63 != 0 { 0x8000 } else { 0 };
    let exponent = ((bits >> 52) & 0x7ff) as i32;
    let mantissa = bits & 0x000f_ffff_ffff_ffff;
    let round = |value: u64, shift: u32| -> u64 {
        let q = value >> shift;
        let rest = value & ((1u64 << shift) - 1);
        let half = 1u64 << (shift - 1);
        q + if rest > half || (rest == half && q & 1 == 1) { 1 } else { 0 }
    };
    (if exponent == 0x7ff {
        sign | if mantissa == 0 { 0x7c00 } else { 0x7e00 }
    } else if exponent - 1008 >= 31 {
        sign | 0x7c00
    } else if exponent - 1008 > 0 {
        sign | ((((exponent - 1008) as u64) << 10) + round(mantissa, 42))
    } else if 1051 - exponent >= 54 {
        sign
    } else {
        sign | round(mantissa | (1u64 << 52), (1051 - exponent) as u32)
    }) as u16
}
fn halves(a: f32, b: f32) -> u32 { f16(a) as u32 | (f16(b) as u32) << 16 }
fn unorms(a: f32, b: f32) -> u32 {
    let u = |x: f32| (x.clamp(0.0, 1.0) * 65535.0).round() as u32;
    u(a) | u(b) << 16
}

impl Instance {
    pub const DEFAULT: Instance = Instance {
        rect: [0.0; 4], clip: NO_CLIP, color: [0; 4], color2: [0; 4], aux: [0; 2], radii: [0; 4],
        width: 0, kind: kind::RECT, flags: 0, view: 0, clip_radius: 0, extra: [0; 2],
    };
    /// A filled rect `[x0, y0, x1, y1]`.
    pub fn rect(rect: [f32; 4], color: [u8; 4]) -> Self { Instance { rect, color, ..Self::DEFAULT } }
    /// A glyph from the R8 mask atlas; `uv` is `[u0, v0, u1, v1]` normalised.
    pub fn mask(rect: [f32; 4], uv: [f32; 4], color: [u8; 4]) -> Self {
        Instance { rect, color, kind: kind::MASK, aux: [unorms(uv[0], uv[1]), unorms(uv[2], uv[3])], ..Self::DEFAULT }
    }
    /// A colour glyph or image from the RGBA atlas (or the texture of the enclosing break).
    pub fn image(rect: [f32; 4], uv: [f32; 4], tint: [u8; 4]) -> Self { Instance { kind: kind::IMAGE, ..Self::mask(rect, uv, tint) } }
    /// An analytic gaussian shadow of the rounded rect `rect`/`radii`.
    pub fn shadow(rect: [f32; 4], radii: [f32; 4], color: [u8; 4], offset: [f32; 2], sigma: f32, spread: f32, inner: bool) -> Self {
        Instance {
            rect, color, kind: kind::SHADOW, flags: if inner { flags::INNER } else { 0 },
            extra: [halves(offset[0], offset[1]), halves(sigma, spread)], ..Self::DEFAULT
        }.radii(radii)
    }
    pub fn line(a: [f32; 2], b: [f32; 2], width: f32, color: [u8; 4]) -> Self {
        Instance { rect: [a[0], a[1], b[0], b[1]], color, width: f16(width), kind: kind::LINE, ..Self::DEFAULT }
    }
    /// A circular arc from angle `start` sweeping `sweep` radians (positive is clockwise on screen).
    pub fn arc(center: [f32; 2], radius: f32, start: f32, sweep: f32, width: f32, color: [u8; 4]) -> Self {
        Instance {
            rect: [center[0], center[1], radius, 0.0], color, width: f16(width), kind: kind::ARC,
            extra: [start.to_bits(), sweep.to_bits()], ..Self::DEFAULT
        }
    }
    /// A quadratic bezier `a`–`control`–`b`, round caps.
    pub fn curve(a: [f32; 2], control: [f32; 2], b: [f32; 2], width: f32, color: [u8; 4]) -> Self {
        Instance {
            rect: [a[0], a[1], b[0], b[1]], color, width: f16(width), kind: kind::CURVE,
            extra: [control[0].to_bits(), control[1].to_bits()], ..Self::DEFAULT
        }
    }
    /// Corner radii top-left, top-right, bottom-right, bottom-left.
    pub fn radii(self, radii: [f32; 4]) -> Self { Instance { radii: radii.map(f16), ..self } }
    pub fn border(self, width: f32, color: [u8; 4]) -> Self { Instance { width: f16(width), color2: color, ..self } }
    fn gradient(self, kind: u8, geometry: [f32; 4], to: [u8; 4]) -> Self {
        Instance { flags: self.flags & !3 | kind, aux: [u32::from_le_bytes(to), 0], extra: [halves(geometry[0], geometry[1]), halves(geometry[2], geometry[3])], ..self }
    }
    pub fn linear(self, from: [f32; 2], to: [f32; 2], color: [u8; 4]) -> Self { self.gradient(flags::LINEAR, [from[0], from[1], to[0], to[1]], color) }
    pub fn radial(self, center: [f32; 2], radius: [f32; 2], color: [u8; 4]) -> Self { self.gradient(flags::RADIAL, [center[0], center[1], radius[0], radius[1]], color) }
    pub fn conic(self, center: [f32; 2], start_turns: f32, color: [u8; 4]) -> Self { self.gradient(flags::CONIC, [center[0], center[1], start_turns, 0.0], color) }
    /// Dashes for lines and arcs, in points along the stroke.
    // ponytail: curves have no dashes (needs arc length of a quadratic); emit short curve pieces if needed.
    pub fn dash(self, on: f32, off: f32, phase: f32) -> Self { Instance { aux: [halves(on, off), halves(phase, 0.0)], ..self } }
    pub fn round_caps(self) -> Self { Instance { flags: self.flags | flags::ROUND, ..self } }
    pub fn clip(self, rect: [i16; 4], radius: f32) -> Self { Instance { clip: rect, clip_radius: f16(radius), ..self } }
    pub fn view(self, view: u16) -> Self { Instance { view, ..self } }
}
