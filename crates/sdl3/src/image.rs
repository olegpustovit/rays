use std::fmt;

use crate::{Rgba, Version};
use sdl3_sys::image as sys;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErrorKind {
    DecoderError,
    WrongDomain,
    InvalidArgument,
    IncompatibleVersion,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    operation: &'static str,
    kind: ErrorKind,
    message: String,
}

impl Error {
    pub fn operation(&self) -> &'static str { self.operation }
    pub fn kind(&self) -> ErrorKind { self.kind }
    pub fn message(&self) -> &str { &self.message }
}


impl fmt::Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}: {}", self.operation, self.message)
    }
}

impl std::error::Error for Error {}

fn error(operation: &'static str, kind: ErrorKind, message: &str) -> Error {
    Error { operation, kind, message: message.to_string() }
}

fn version(number: i32) -> Version {
    Version { major: number / 1_000_000, minor: number / 1_000 % 1_000, patch: number % 1_000 }
}

pub fn linked_version() -> Version {
    version(sys::linked_version())
}

/// The headers' own version macro, not a checked-in constant.
pub fn compiled_version() -> Version {
    version(sys::compiled_version())
}

pub fn check_version(release: bool) -> std::result::Result<(), Error> {
    crate::validate_version_for("SDL3_image", compiled_version(), release, linked_version())
        .map_err(|source| error("SDL3_image.check_version", ErrorKind::IncompatibleVersion, source.message()))
}

fn byte(bytes: &[u8], offset: usize) -> u8 {
    bytes[offset]
}

fn range(bytes: &[u8], offset: usize, length: usize) -> bool {
    bytes.len() >= length && offset <= bytes.len() - length
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Endian {
    Little,
    Big,
}

fn u16_at(bytes: &[u8], endian: Endian, offset: usize) -> Option<i64> {
    if !range(bytes, offset, 2) {
        return None;
    }
    let (first, second) = (byte(bytes, offset) as i64, byte(bytes, offset + 1) as i64);
    Some(match endian {
        Endian::Little => first | (second << 8),
        Endian::Big => (first << 8) | second,
    })
}

fn u32_at(bytes: &[u8], endian: Endian, offset: usize) -> Option<i64> {
    if !range(bytes, offset, 4) {
        return None;
    }
    let [a, b, c, d] = [0, 1, 2, 3].map(|i| byte(bytes, offset + i) as i64);
    Some(match endian {
        Endian::Little => a | (b << 8) | (c << 16) | (d << 24),
        Endian::Big => (a << 24) | (b << 16) | (c << 8) | d,
    })
}

fn exif_orientation(bytes: &[u8], offset: usize, length: usize) -> Option<i64> {
    if length < 14 || !range(bytes, offset, length) || &bytes[offset..offset + 6] != b"Exif\0\0" {
        return None;
    }
    let tiff = offset + 6;
    let endian = match (byte(bytes, tiff), byte(bytes, tiff + 1)) {
        (0x49, 0x49) => Endian::Little,
        (0x4d, 0x4d) => Endian::Big,
        _ => return None,
    };
    let (Some(42), Some(directory_offset)) = (u16_at(bytes, endian, tiff + 2), u32_at(bytes, endian, tiff + 4)) else {
        return None;
    };
    if directory_offset > length as i64 - 8 {
        return None;
    }
    let directory = tiff + directory_offset as usize;
    let count = u16_at(bytes, endian, directory)?;
    for index in 0..count as usize {
        let position = directory + 2 + index * 12;
        if !range(bytes, position, 12) || position + 12 > offset + length {
            return None;
        }
        if let (Some(0x0112), Some(3), Some(1), Some(orientation)) = (
            u16_at(bytes, endian, position),
            u16_at(bytes, endian, position + 2),
            u32_at(bytes, endian, position + 4),
            u16_at(bytes, endian, position + 8),
        ) {
            if (1..=8).contains(&orientation) {
                return Some(orientation);
            }
        }
    }
    None
}

fn jpeg_orientation(bytes: &[u8]) -> i64 {
    if !range(bytes, 0, 4) || byte(bytes, 0) != 0xff || byte(bytes, 1) != 0xd8 {
        return 1;
    }
    let mut offset = 2;
    loop {
        if !range(bytes, offset, 4) {
            return 1;
        }
        if byte(bytes, offset) != 0xff {
            offset += 1;
            continue;
        }
        let mut position = offset + 1;
        loop {
            if !range(bytes, position, 1) {
                return 1;
            }
            if byte(bytes, position) == 0xff {
                position += 1;
            } else {
                break;
            }
        }
        let code = byte(bytes, position);
        if code == 0xd9 || code == 0xda {
            return 1;
        }
        let size_offset = position + 1;
        if !range(bytes, size_offset, 2) {
            return 1;
        }
        let size = ((byte(bytes, size_offset) as usize) << 8) | byte(bytes, size_offset + 1) as usize;
        if size < 2 || !range(bytes, size_offset + 2, size - 2) {
            return 1;
        }
        if code == 0xe1 {
            if let Some(orientation) = exif_orientation(bytes, size_offset + 2, size - 2) {
                return orientation;
            }
        }
        offset = size_offset + size;
    }
}

fn orient(decoded: Rgba, orientation: i64) -> Rgba {
    if orientation == 1 {
        return decoded;
    }
    let (source_width, source_height) = (decoded.width as usize, decoded.height as usize);
    let (width, height) = if orientation >= 5 { (source_height, source_width) } else { (source_width, source_height) };
    let mut pixels = vec![0u8; decoded.pixels.len()];
    for y in 0..height {
        for x in 0..width {
            let (source_x, source_y) = match orientation {
                2 => (source_width - 1 - x, y),
                3 => (source_width - 1 - x, source_height - 1 - y),
                4 => (x, source_height - 1 - y),
                5 => (y, x),
                6 => (y, source_height - 1 - x),
                7 => (source_width - 1 - y, source_height - 1 - x),
                8 => (source_width - 1 - y, x),
                _ => (x, y),
            };
            let source = (source_y * source_width + source_x) * 4;
            let target = (y * width + x) * 4;
            pixels[target..target + 4].copy_from_slice(&decoded.pixels[source..source + 4]);
        }
    }
    Rgba { width: width as i32, height: height as i32, pixels }
}

fn decode(operation: &'static str, kind: Option<&str>, bytes: &[u8]) -> std::result::Result<Rgba, Error> {
    match sys::decode(bytes, kind) {
        Err(message) => Err(error(operation, ErrorKind::DecoderError, &message)),
        Ok((width, height, pixels)) => Ok(orient(Rgba { width, height, pixels }, jpeg_orientation(bytes))),
    }
}

fn read_file(operation: &'static str, path: &str) -> std::result::Result<Vec<u8>, Error> {
    std::fs::read(path).map_err(|failure| error(operation, ErrorKind::DecoderError, &format!("{path}: {failure}")))
}

fn kind_of_path(path: &str) -> Option<&'static str> {
    let base = path.rsplit(['/', '\\']).next().unwrap_or(path);
    let extension = match base.rfind('.') {
        Some(position) if position > 0 => base[position..].to_ascii_lowercase(),
        _ => String::new(),
    };
    Some(match extension.as_str() {
        ".ani" => "ANI",
        ".avif" | ".avifs" => "AVIF",
        ".bmp" => "BMP",
        ".cur" => "CUR",
        ".gif" => "GIF",
        ".ico" => "ICO",
        ".jpg" | ".jpeg" | ".jpe" => "JPG",
        ".jxl" => "JXL",
        ".lbm" | ".iff" => "LBM",
        ".pcx" => "PCX",
        ".png" => "PNG",
        ".pnm" | ".pbm" | ".pgm" | ".ppm" => "PNM",
        ".qoi" => "QOI",
        ".svg" | ".svgz" => "SVG",
        ".tga" => "TGA",
        ".tif" | ".tiff" => "TIF",
        ".webp" => "WEBP",
        ".xcf" => "XCF",
        ".xpm" => "XPM",
        ".xv" => "XV",
        _ => return None,
    })
}

fn enter_main(operation: &'static str) -> std::result::Result<(), Error> {
    if !main_thread() {
        return Err(error(
            operation,
            ErrorKind::WrongDomain,
            "SDL3_image decoding must run on the initial thread and SDL main thread",
        ));
    }
    check_version(true)?;
    Ok(())
}

pub fn load_file(path: &str) -> std::result::Result<Rgba, Error> {
    let operation = "SDL3_image.load_file";
    if path.contains('\0') {
        return Err(error(operation, ErrorKind::InvalidArgument, "image path contains a NUL byte"));
    }
    enter_main(operation)?;
    let bytes = read_file(operation, path)?;
    decode(operation, kind_of_path(path), &bytes)
}

#[cfg(test)]
mod tests;

// The test harness never runs a test on the process main thread, so a test claims that role.
#[cfg(not(test))]
fn main_thread() -> bool {
    crate::thread::is_initial_domain() && crate::thread::is_sdl_main_thread()
}

#[cfg(test)]
fn main_thread() -> bool {
    tests::is_main()
}
