use std::fmt;
use std::sync::Mutex;
use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};

use crate::{Rgba, Version};
use sdl3_sys::ttf as sys;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErrorKind {
    TtfError,
    WrongDomain,
    Destroyed,
    InvalidArgument,
    IncompatibleVersion,
    NotInitialized,
    FontNotFound,
    FontsStillOpen,
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
    crate::validate_version_for("SDL3_ttf", compiled_version(), release, linked_version())
        .map_err(|source| error("SDL3_ttf.check_version", ErrorKind::IncompatibleVersion, source.message()))
}

// Fonts dropped off the main thread wait here, unbounded, until the next main-thread operation.
static RELEASE_QUEUE: Mutex<Vec<usize>> = Mutex::new(Vec::new());

fn drain() {
    let pending = std::mem::take(&mut *RELEASE_QUEUE.lock().unwrap());
    for font in pending {
        sys::close_font(font);
    }
}

fn require_main(operation: &'static str) -> std::result::Result<(), Error> {
    if !main_thread() {
        Err(error(operation, ErrorKind::WrongDomain, "SDL3_ttf operation must run on the initial thread and SDL main thread"))
    } else {
        Ok(())
    }
}

fn enter_main(operation: &'static str) -> std::result::Result<(), Error> {
    require_main(operation)?;
    drain();
    Ok(())
}

static INIT_COUNT: AtomicI64 = AtomicI64::new(0);
static LIVE_FONTS: AtomicI64 = AtomicI64::new(0);

pub mod init {
    use super::*;

    pub fn init() -> std::result::Result<(), Error> {
        enter_main("SDL3_ttf.Init.init")?;
            check_version(true)?;
            sys::init().map_err(|message| error("SDL3_ttf.Init.init", ErrorKind::TtfError, &message))?;
            INIT_COUNT.fetch_add(1, Ordering::SeqCst);
            Ok(())
    }

    pub fn initialized() -> std::result::Result<bool, Error> {
        enter_main("SDL3_ttf.Init.initialized")?;
        Ok(sys::was_init() > 0)
    }

    pub fn quit() -> std::result::Result<(), Error> {
        enter_main("SDL3_ttf.Init.quit")?;
            let open_count = LIVE_FONTS.load(Ordering::SeqCst);
            if open_count != 0 {
                Err(error("SDL3_ttf.Init.quit", ErrorKind::FontsStillOpen, &format!("{open_count} font handle(s) are still open")))
            } else if INIT_COUNT.load(Ordering::SeqCst) == 0 {
                Ok(())
            } else {
                INIT_COUNT.fetch_sub(1, Ordering::SeqCst);
                sys::quit();
                Ok(())
            }
    }
}

pub mod font {
    use super::*;

    pub struct Font {
        raw: usize,
        destroyed: AtomicBool,
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct Metrics {
        pub height: i64,
        pub ascent: i64,
        pub descent: i64,
        pub line_skip: i64,
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Alignment {
        Left,
        Center,
        Right,
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Hinting {
        Normal,
        Light,
        Mono,
        None,
        LightSubpixel,
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct GlyphMetrics {
        pub min_x: i64,
        pub max_x: i64,
        pub min_y: i64,
        pub max_y: i64,
        pub advance: i64,
    }

    fn valid_size(value: f64) -> bool {
        value.is_finite() && value > 0.0
    }

    // ponytail: Rust's &str is always valid UTF-8, so the OCaml validator has nothing left to check.
    fn validate_text(operation: &'static str, text: &str) -> std::result::Result<(), Error> {
        if text.contains('\0') {
            Err(error(operation, ErrorKind::InvalidArgument, "text contains a NUL byte"))
        } else {
            Ok(())
        }
    }

    const SYSTEM_FONT_CANDIDATES: [&str; 8] = [
        "/System/Library/Fonts/SFNSMono.ttf",
        "/System/Library/Fonts/SFNS.ttf",
        "/System/Library/Fonts/SFCompact.ttf",
        "/System/Library/Fonts/HelveticaNeue.ttc",
        "/System/Library/Fonts/Helvetica.ttc",
        "/System/Library/Fonts/LucidaGrande.ttc",
        // Windows: only reached when no macOS font exists, so macOS lookups are unchanged
        "C:\\Windows\\Fonts\\segoeui.ttf",
        "C:\\Windows\\Fonts\\arial.ttf",
    ];

    /// Locate an installed UI font. `font` is authoritative when non-empty; otherwise the macOS
    /// system fonts are tried in stable order.
    pub fn system_path(font: Option<&str>) -> std::result::Result<String, Error> {
        match font {
            Some(path) if !path.is_empty() => {
                if std::path::Path::new(path).exists() {
                    Ok(path.to_owned())
                } else {
                    Err(error(
                        "SDL3_ttf.Font.system_path",
                        ErrorKind::FontNotFound,
                        &format!("font override does not name a readable font: {path}"),
                    ))
                }
            }
            _ => SYSTEM_FONT_CANDIDATES
                .iter()
                .find(|path| std::path::Path::new(path).exists())
                .map(|path| (*path).to_owned())
                .ok_or_else(|| {
                    error("SDL3_ttf.Font.system_path", ErrorKind::FontNotFound, "no supported installed system UI font was found")
                }),
        }
    }

    fn require_initialized(operation: &'static str) -> std::result::Result<(), Error> {
        if init::initialized()? {
            Ok(())
        } else {
            Err(error(operation, ErrorKind::NotInitialized, "SDL3_ttf is not initialized"))
        }
    }

    fn valid_channel(value: i64) -> bool {
        (0..=255).contains(&value)
    }

    fn valid_codepoint(value: i64) -> bool {
        (0..=0x10ffff).contains(&value) && !(0xd800..=0xdfff).contains(&value)
    }

    impl Font {
        fn raw(&self, operation: &'static str) -> std::result::Result<usize, Error> {
            enter_main(operation)?;
                if self.destroyed.load(Ordering::SeqCst) {
                    return Err(error(operation, ErrorKind::Destroyed, "font is destroyed"));
                }
                require_initialized(operation)?;
                Ok(self.raw)
        }

        pub fn open_file(path: &str, size: f64) -> std::result::Result<Font, Error> {
            let operation = "SDL3_ttf.Font.open_file";
            if path.is_empty() || path.contains('\0') {
                Err(error(operation, ErrorKind::InvalidArgument, "font path must be non-empty and NUL-free"))
            } else if !valid_size(size) {
                Err(error(operation, ErrorKind::InvalidArgument, "font size must be finite and positive"))
            } else {
                enter_main(operation)?;
                    require_initialized(operation)?;
                    let raw = sys::open_font(path, size).map_err(|message| error(operation, ErrorKind::TtfError, &message))?;
                    LIVE_FONTS.fetch_add(1, Ordering::SeqCst);
                    Ok(Font { raw, destroyed: AtomicBool::new(false) })
            }
        }

        pub fn metrics(&self) -> std::result::Result<Metrics, Error> {
            let raw = self.raw("SDL3_ttf.Font.metrics")?;
                let (height, ascent, descent, line_skip) = sys::font_metrics(raw);
                Ok(Metrics { height: height.into(), ascent: ascent.into(), descent: descent.into(), line_skip: line_skip.into() })
        }

        pub fn set_size_dpi(&self, size: f64, horizontal: i64, vertical: i64) -> std::result::Result<(), Error> {
            let operation = "SDL3_ttf.Font.set_size_dpi";
            if !valid_size(size) {
                Err(error(operation, ErrorKind::InvalidArgument, "font size must be finite and positive"))
            } else if horizontal <= 0 || vertical <= 0 {
                Err(error(operation, ErrorKind::InvalidArgument, "font DPI must be positive"))
            } else {
                let (horizontal, vertical) = (horizontal.min(i32::MAX.into()) as i32, vertical.min(i32::MAX.into()) as i32);
                let raw = self.raw(operation)?;
                sys::set_font_size_dpi(raw, size, horizontal, vertical).map_err(|message| error(operation, ErrorKind::TtfError, &message))
            }
        }

        pub fn set_hinting(&self, hinting: Hinting) -> std::result::Result<(), Error> {
            let code = match hinting {
                Hinting::Normal => 0,
                Hinting::Light => 1,
                Hinting::Mono => 2,
                Hinting::None => 3,
                Hinting::LightSubpixel => 4,
            };
            let raw = self.raw("SDL3_ttf.Font.set_hinting")?;
                sys::set_font_hinting(raw, code);
                Ok(())
        }

        /// Line alignment inside `render_blended_wrapped` output.
        pub fn set_wrap_alignment(&self, alignment: Alignment) -> std::result::Result<(), Error> {
            let code = match alignment {
                Alignment::Left => 0,
                Alignment::Center => 1,
                Alignment::Right => 2,
            };
            let raw = self.raw("SDL3_ttf.Font.set_wrap_alignment")?;
                sys::set_font_wrap_alignment(raw, code);
                Ok(())
        }

        pub fn glyph_metrics(&self, codepoint: i64) -> std::result::Result<GlyphMetrics, Error> {
            let operation = "SDL3_ttf.Font.glyph_metrics";
            if !valid_codepoint(codepoint) {
                return Err(error(operation, ErrorKind::InvalidArgument, "codepoint is not a Unicode scalar value"));
            }
            let raw = self.raw(operation)?;
                let (min_x, max_x, min_y, max_y, advance) = sys::glyph_metrics(raw, codepoint as u32).map_err(|message| error(operation, ErrorKind::TtfError, &message))?;
                Ok(GlyphMetrics { min_x: min_x.into(), max_x: max_x.into(), min_y: min_y.into(), max_y: max_y.into(), advance: advance.into() })
        }

        fn render(&self, operation: &'static str, color: (i64, i64, i64, i64), wrap_width: i64, text: &str) -> std::result::Result<Option<Rgba>, Error> {
            let (red, green, blue, alpha) = color;
            if !(valid_channel(red) && valid_channel(green) && valid_channel(blue) && valid_channel(alpha)) {
                return Err(error(operation, ErrorKind::InvalidArgument, "RGBA channels must be in 0..255"));
            }
            validate_text(operation, text)?;
            if text.is_empty() {
                self.raw(operation)?;
                return Ok(None);
            }
            let wrap_width = wrap_width.min(i32::MAX.into()) as i32;
            let raw = self.raw(operation)?;
                let (width, height, pixels) = sys::render_blended(raw, text, (red as i32, green as i32, blue as i32, alpha as i32), wrap_width).map_err(|message| error(operation, ErrorKind::TtfError, &message))?;
                Ok(Some(Rgba { width, height, pixels }))
        }

        /// Render UTF-8 to tightly packed RGBA8 rows. Empty text is `Ok(None)`, preserving the
        /// high-level no-op contract instead of asking SDL_ttf for a 0-width bitmap. Color
        /// channels are straight RGBA values in [0,255].
        pub fn render_blended(&self, color: (i64, i64, i64, i64), text: &str) -> std::result::Result<Option<Rgba>, Error> {
            self.render("SDL3_ttf.Font.render_blended", color, -1, text)
        }

        pub fn render_blended_wrapped(&self, color: (i64, i64, i64, i64), wrap_width: i64, text: &str) -> std::result::Result<Option<Rgba>, Error> {
            let operation = "SDL3_ttf.Font.render_blended_wrapped";
            if wrap_width < 0 {
                return Err(error(operation, ErrorKind::InvalidArgument, "wrap width must be non-negative"));
            }
            self.render(operation, color, wrap_width, text)
        }

        pub fn destroy(&self) -> std::result::Result<(), Error> {
            enter_main("SDL3_ttf.Font.destroy")?;
                if !self.destroyed.swap(true, Ordering::SeqCst) {
                    LIVE_FONTS.fetch_sub(1, Ordering::SeqCst);
                    sys::close_font(self.raw);
                }
                Ok(())
        }
    }

    impl Drop for Font {
        fn drop(&mut self) {
            if !self.destroyed.swap(true, Ordering::SeqCst) {
                LIVE_FONTS.fetch_sub(1, Ordering::SeqCst);
                RELEASE_QUEUE.lock().unwrap().push(self.raw);
            }
        }
    }
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
