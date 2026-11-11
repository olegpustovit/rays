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
    /// An image (kind 2) sampled with the painter's linear filter: a picture drawn at any size
    /// (`Paint::image_fit`). Without it images sample nearest, as glyphs do.
    pub const SMOOTH: u8 = 1;
    pub const INNER: u8 = 4;
    pub const ROUND: u8 = 8;
    /// Rect border sides (bits 4–7); none set, or all four, borders every side.
    pub const TOP: u8 = 16;
    pub const RIGHT: u8 = 32;
    pub const BOTTOM: u8 = 64;
    pub const LEFT: u8 = 128;
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

/// `Break::image` of a break back to the colour atlas (colour glyphs after an image).
pub const ATLAS: u32 = u32::MAX;

/// From `at_instance` on, the layer's [`kind::IMAGE`] instances sample `images[image]` (or the
/// colour atlas for [`ATLAS`]).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Break {
    pub at_instance: u32,
    pub image: u32,
}

/// CSS `border` with a width and colour per side `[top, right, bottom, left]`: one border-only rect
/// instance per side with a non-zero width (draw the fill separately), `count` of them valid.
// ponytail: one instance per side (up to 4); a side's inner edge uses the radii minus its width, so
// mixed widths meet at corners with a square seam. A packed per-side format if borders dominate.
pub fn borders(rect: [f32; 4], radii: [f32; 4], widths: [f32; 4], colors: [[u8; 4]; 4]) -> ([Instance; 4], usize) {
    let mut out = [Instance::DEFAULT; 4];
    let mut n = 0;
    for (i, side) in [flags::TOP, flags::RIGHT, flags::BOTTOM, flags::LEFT].into_iter().enumerate() {
        if widths[i] > 0.0 {
            out[n] = Instance::rect(rect, [0; 4]).radii(radii).border(widths[i], colors[i]).sides(side);
            n += 1;
        }
    }
    (out, n)
}

/// How an image fills a box, like CSS `object-fit`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Fit {
    /// Stretch to the box.
    Fill,
    /// Scale uniformly to fit inside the box, centred; the rect shrinks.
    Contain,
    /// Scale uniformly to cover the box, centred; the uv crop.
    Cover,
    /// Natural size, centred: smaller images shrink the rect, larger ones crop the uv.
    None,
}

/// The rect and uv to draw an `image` (width, height) in `bounds` (`[x0, y0, x1, y1]`) under `mode`.
/// `uv` is the image's normalised rect in its texture (a whole texture is `[0, 0, 1, 1]`, an atlas
/// entry its slot); the returned uv is the visible part of it. Pass both to [`Instance::image`].
pub fn fit(mode: Fit, image: [f32; 2], bounds: [f32; 4], uv: [f32; 4]) -> ([f32; 4], [f32; 4]) {
    let (bw, bh) = (bounds[2] - bounds[0], bounds[3] - bounds[1]);
    if mode == Fit::Fill || !(image[0] > 0.0 && image[1] > 0.0) { return (bounds, uv); }
    let k = match mode {
        Fit::Contain => (bw / image[0]).min(bh / image[1]),
        Fit::Cover => (bw / image[0]).max(bh / image[1]),
        _ => 1.0,
    };
    let (w, h) = (image[0] * k, image[1] * k);
    let (x0, y0) = (bounds[0] + (bw - w) * 0.5, bounds[1] + (bh - h) * 0.5);
    let rect = [x0.max(bounds[0]), y0.max(bounds[1]), (x0 + w).min(bounds[2]), (y0 + h).min(bounds[3])];
    let lerp = |a: f32, b: f32, t: f32| a + (b - a) * t;
    let visible = [(rect[0] - x0) / w, (rect[1] - y0) / h, (rect[2] - x0) / w, (rect[3] - y0) / h];
    (rect, [lerp(uv[0], uv[2], visible[0]), lerp(uv[1], uv[3], visible[1]), lerp(uv[0], uv[2], visible[2]), lerp(uv[1], uv[3], visible[3])])
}

/// CSS `border-image` nine-slice: nine image instances stretching the centre and edges of an `image`
/// (width, height in texels) between corners of `inset` (`[left, top, right, bottom]` in image units)
/// drawn 1:1 in `rect`, row-major from the top-left. Corners shrink together if the rect is smaller
/// than the insets. `uv` as for [`fit`].
pub fn nine_slice(rect: [f32; 4], uv: [f32; 4], image: [f32; 2], inset: [f32; 4], tint: [u8; 4]) -> [Instance; 9] {
    let squeeze = |a: f32, b: f32, room: f32| if a + b > room && a + b > 0.0 { room.max(0.0) / (a + b) } else { 1.0 };
    let (kx, ky) = (squeeze(inset[0], inset[2], rect[2] - rect[0]), squeeze(inset[1], inset[3], rect[3] - rect[1]));
    let xs = [rect[0], rect[0] + inset[0] * kx, rect[2] - inset[2] * kx, rect[2]];
    let ys = [rect[1], rect[1] + inset[1] * ky, rect[3] - inset[3] * ky, rect[3]];
    let (du, dv) = (uv[2] - uv[0], uv[3] - uv[1]);
    let us = [uv[0], uv[0] + du * inset[0] / image[0], uv[2] - du * inset[2] / image[0], uv[2]];
    let vs = [uv[1], uv[1] + dv * inset[1] / image[1], uv[3] - dv * inset[3] / image[1], uv[3]];
    std::array::from_fn(|i| {
        let (x, y) = (i % 3, i / 3);
        Instance::image([xs[x], ys[y], xs[x + 1], ys[y + 1]], [us[x], vs[y], us[x + 1], vs[y + 1]], tint)
    })
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
    /// A colour glyph or image from the RGBA atlas (or the texture of the enclosing break). `tint`
    /// multiplies the texel per channel, alpha included: white keeps the image, a grey scales brightness,
    /// a translucent white fades it, a colour tints a white-on-transparent icon. Corner radii (`radii`)
    /// round the image, see [`fit`] and [`nine_slice`] for placing it.
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
    /// Draw the border (see [`Instance::border`]) only on the sides in `sides` (`flags::TOP | …`).
    pub fn sides(self, sides: u8) -> Self { Instance { flags: self.flags & 0x0f | sides & 0xf0, ..self } }
    fn gradient(self, kind: u8, geometry: [f32; 4], to: [u8; 4]) -> Self {
        Instance { flags: self.flags & !3 | kind, aux: [u32::from_le_bytes(to), 0], extra: [halves(geometry[0], geometry[1]), halves(geometry[2], geometry[3])], ..self }
    }
    /// Colour the gradient from row `row` of the colour atlas (see `pxui_render::ramp`) instead of the
    /// two inline stops; call after `linear`/`radial`/`conic`.
    pub fn ramp(self, row: u32) -> Self { Instance { aux: [self.aux[0], row + 1], ..self } }
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
