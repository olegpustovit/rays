use crate::hash::wrap_int63;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct T {
    pub r: i64,
    pub g: i64,
    pub b: i64,
    pub a: i64,
}

fn clamp_byte(value: i64) -> i64 { value.clamp(0, 255) }

pub fn rgb(r: i64, g: i64, b: i64) -> T {
    T { r: clamp_byte(r), g: clamp_byte(g), b: clamp_byte(b), a: 255 }
}

pub fn rgba(r: i64, g: i64, b: i64, a: i64) -> T {
    T { r: clamp_byte(r), g: clamp_byte(g), b: clamp_byte(b), a: clamp_byte(a) }
}

fn unit_byte(value: f64) -> i64 {
    clamp_byte((value * 255.0).round() as i64)
}

pub fn of_unit_floats(alpha: Option<f64>, r: f64, g: f64, b: f64) -> T {
    rgba(unit_byte(r), unit_byte(g), unit_byte(b), unit_byte(alpha.unwrap_or(1.0)))
}

pub fn gray(value: i64) -> T {
    let value = clamp_byte(value);
    T { r: value, g: value, b: value, a: 255 }
}

fn hex_nibble(value: u8) -> Option<u8> {
    match value {
        b'0'..=b'9' => Some(value - b'0'),
        b'a'..=b'f' => Some(value - b'a' + 10),
        b'A'..=b'F' => Some(value - b'A' + 10),
        _ => None,
    }
}

pub fn hex(value: &str) -> Result<T, String> {
    let value = value.strip_prefix('#').unwrap_or(value).as_bytes();
    let expanded = if value.len() == 3 || value.len() == 4 {
        Some(value.iter().flat_map(|byte| [*byte, *byte]).collect::<Vec<_>>())
    } else {
        None
    };
    let bytes = expanded.as_deref().unwrap_or(value);
    if bytes.len() != 6 && bytes.len() != 8 {
        return Err("Color.hex: expected #rgb, #rgba, #rrggbb, or #rrggbbaa".to_owned());
    }
    let channel = |offset: usize| -> Option<i64> {
        Some(i64::from(hex_nibble(bytes[offset])? * 16 + hex_nibble(bytes[offset + 1])?))
    };
    match (channel(0), channel(2), channel(4), if bytes.len() == 8 { channel(6) } else { Some(255) }) {
        (Some(r), Some(g), Some(b), Some(a)) => Ok(rgba(r, g, b, a)),
        _ => Err("Color.hex: invalid hexadecimal digit".to_owned()),
    }
}

pub fn hex_exn(value: &str) -> Result<T, String> { hex(value) }

fn ocaml_min(a: f64, b: f64) -> f64 { if a <= b { a } else { b } }
fn ocaml_max(a: f64, b: f64) -> f64 { if a >= b { a } else { b } }
fn clamp_unit(value: f64) -> f64 { ocaml_max(0.0, ocaml_min(1.0, value)) }

fn wrap_hue(hue: f64) -> f64 {
    let wrapped = hue % 360.0;
    if wrapped < 0.0 { wrapped + 360.0 } else { wrapped }
}

fn byte(value: f64) -> i64 { (clamp_unit(value) * 255.0 + 0.5) as i64 }

pub fn hsl(alpha: Option<f64>, hue: f64, saturation: f64, lightness: f64) -> T {
    let hue = wrap_hue(hue) / 60.0;
    let saturation = clamp_unit(saturation);
    let lightness = clamp_unit(lightness);
    let chroma = (1.0 - ((2.0 * lightness) - 1.0).abs()) * saturation;
    let x = chroma * (1.0 - ((hue % 2.0) - 1.0).abs());
    let (r, g, b) = if hue < 1.0 {
        (chroma, x, 0.0)
    } else if hue < 2.0 {
        (x, chroma, 0.0)
    } else if hue < 3.0 {
        (0.0, chroma, x)
    } else if hue < 4.0 {
        (0.0, x, chroma)
    } else if hue < 5.0 {
        (x, 0.0, chroma)
    } else {
        (chroma, 0.0, x)
    };
    let m = lightness - chroma / 2.0;
    rgba(byte(r + m), byte(g + m), byte(b + m), byte(alpha.unwrap_or(1.0)))
}

pub fn hsl_channels(hue: f64, saturation: f64, lightness: f64) -> (f64, f64, f64) {
    let modulo = |x: f64, y: f64| (x % y + y) % y;
    let unit = |value: f64| 0.0_f64.max(1.0_f64.min(value));
    let hue = modulo(hue, 360.0) / 60.0;
    let saturation = unit(saturation);
    let lightness = unit(lightness);
    let chroma = (1.0 - (2.0 * lightness - 1.0).abs()) * saturation;
    let x = chroma * (1.0 - (modulo(hue, 2.0) - 1.0).abs());
    let (r, g, b) = if hue < 1.0 {
        (chroma, x, 0.0)
    } else if hue < 2.0 {
        (x, chroma, 0.0)
    } else if hue < 3.0 {
        (0.0, chroma, x)
    } else if hue < 4.0 {
        (0.0, x, chroma)
    } else if hue < 5.0 {
        (x, 0.0, chroma)
    } else {
        (chroma, 0.0, x)
    };
    let m = lightness - chroma / 2.0;
    let byte = |value: f64| ((unit(value + m) * 255.0 + 0.5) as i64) as f64 / 255.0;
    (byte(r), byte(g), byte(b))
}

pub fn hsv(hue: f64, saturation: f64, value: f64) -> (f64, f64, f64) {
    let hue = (hue % 1.0 + 1.0) % 1.0;
    let h6 = hue * 6.0;
    let sector = h6.floor() as i64;
    let fraction = h6 - sector as f64;
    let p = value * (1.0 - saturation);
    let q = value * (1.0 - (fraction * saturation));
    let t = value * (1.0 - ((1.0 - fraction) * saturation));
    match sector % 6 {
        0 => (value, t, p),
        1 => (q, value, p),
        2 => (p, value, t),
        3 => (p, q, value),
        4 => (t, p, value),
        _ => (value, p, q),
    }
}

pub fn aces(value: f64) -> f64 {
    value * ((2.51 * value) + 0.03) / ((value * ((2.43 * value) + 0.59)) + 0.14)
}

pub fn to_hex_string(color: T) -> String {
    if color.a == 255 {
        format!("#{:02x}{:02x}{:02x}", color.r, color.g, color.b)
    } else {
        format!("#{:02x}{:02x}{:02x}{:02x}", color.r, color.g, color.b, color.a)
    }
}

#[allow(non_snake_case)]
pub mod Linear {
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub struct T {
        pub r: f64,
        pub g: f64,
        pub b: f64,
    }

    pub fn rgb(r: f64, g: f64, b: f64) -> T { T { r, g, b } }
}

const BLACK: T = T { r: 0, g: 0, b: 0, a: 255 };
const WHITE: T = T { r: 255, g: 255, b: 255, a: 255 };
const RED: T = T { r: 255, g: 0, b: 0, a: 255 };
const TRANSPARENT: T = T { r: 0, g: 0, b: 0, a: 0 };
pub fn black() -> T { BLACK }
pub fn white() -> T { WHITE }
pub fn red() -> T { RED }
pub fn transparent() -> T { TRANSPARENT }

pub fn with_alpha(color: T, alpha: i64) -> T { T { a: clamp_byte(alpha), ..color } }
pub fn to_tuple(color: T) -> (i64, i64, i64, i64) { (color.r, color.g, color.b, color.a) }

pub fn blend(c1: T, c2: T, pct: f64) -> T {
    let pct = ocaml_max(0.0, ocaml_min(1.0, pct));
    let inv_pct = 1.0 - pct;
    T {
        r: ((c1.r as f64 * inv_pct) + (c2.r as f64 * pct) + 0.5) as i64,
        g: ((c1.g as f64 * inv_pct) + (c2.g as f64 * pct) + 0.5) as i64,
        b: ((c1.b as f64 * inv_pct) + (c2.b as f64 * pct) + 0.5) as i64,
        a: ((c1.a as f64 * inv_pct) + (c2.a as f64 * pct) + 0.5) as i64,
    }
}

pub fn gradient(colors: &[T], position: f64) -> Result<T, String> {
    match colors {
        [] => Err("Color.gradient: empty palette".to_owned()),
        [color] => Ok(*color),
        _ => {
            let position = clamp_unit(position) * (colors.len() - 1) as f64;
            let left = (position as usize).min(colors.len() - 2);
            Ok(blend(colors[left], colors[left + 1], position - left as f64))
        }
    }
}

pub fn to_string(color: T) -> String {
    format!("Color(r={}, g={}, b={}, a={})", color.r, color.g, color.b, color.a)
}

pub fn to_hex(color: T) -> i64 {
    wrap_int63(wrap_int63(color.r.wrapping_shl(16))
        | wrap_int63(color.g.wrapping_shl(8))
        | wrap_int63(color.b))
}

pub fn of_floats(r: f64, g: f64, b: f64, a: f64) -> T {
    rgba(
        (r * 255.0 + 0.5) as i64,
        (g * 255.0 + 0.5) as i64,
        (b * 255.0 + 0.5) as i64,
        (a * 255.0 + 0.5) as i64,
    )
}

pub fn to_floats(color: T) -> (f64, f64, f64, f64) {
    (
        color.r as f64 / 255.0,
        color.g as f64 / 255.0,
        color.b as f64 / 255.0,
        color.a as f64 / 255.0,
    )
}
