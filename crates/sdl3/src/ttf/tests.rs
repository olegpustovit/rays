use super::font::{Alignment, Font, Hinting, system_path};
use super::{ErrorKind, check_version, compiled_version, init, linked_version};
use crate::Rgba;

fn has_visible_alpha(pixels: &[u8]) -> bool {
    pixels.len() >= 4 && pixels.iter().skip(3).step_by(4).any(|&alpha| alpha != 0)
}

/// Reads `(floor ...)` for `key` from packaging/sdl3.lock.
fn lock_floor(key: &str) -> (i32, i32, i32) {
    let path = format!("{}/../../packaging/sdl3.lock", env!("CARGO_MANIFEST_DIR"));
    let text = std::fs::read_to_string(path).expect("read sdl3.lock");
    let line = text
        .lines()
        .map(|line| line.trim().trim_start_matches('('))
        .find(|line| line.starts_with(&format!("{key} ")))
        .expect("lock entry");
    let floor = line.split("(floor ").nth(1).and_then(|rest| rest.split(')').next()).expect("floor");
    let parts: Vec<i32> = floor.split('.').map(|part| part.parse().unwrap()).collect();
    (parts[0], parts[1], parts[2])
}

fn some(result: std::result::Result<Option<Rgba>, super::Error>, what: &str) -> Rgba {
    result.unwrap().unwrap_or_else(|| panic!("{what}"))
}

#[test]
fn cpu_font_conformance() {
    claim_main();
    let font_path = system_path(None).unwrap();
    assert!(std::path::Path::new(&font_path).exists(), "installed system font discovery returned a missing path");
    let triple = |v: crate::Version| (v.major, v.minor, v.patch);
    let floor = lock_floor("sdl3_ttf");
    assert!(triple(compiled_version()) >= floor && triple(linked_version()) >= floor, "installed SDL3_ttf is below the lock floor");
    check_version(true).unwrap();
    assert_eq!(Font::open_file(&font_path, 18.0).err().map(|failure| failure.kind()), Some(ErrorKind::NotInitialized), "pre-init font open");
    init::init().unwrap();
    assert!(init::initialized().unwrap(), "TTF initialization was not retained");
    assert_eq!(std::thread::spawn(init::initialized).join().unwrap().err().map(|failure| failure.kind()), Some(ErrorKind::WrongDomain));
    assert_eq!(Font::open_file(&font_path, f64::NAN).err().map(|failure| failure.kind()), Some(ErrorKind::InvalidArgument), "NaN font size");
    assert_eq!(Font::open_file("", 18.0).err().map(|failure| failure.kind()), Some(ErrorKind::InvalidArgument), "empty font path");
    match Font::open_file("/definitely/missing/font.ttf", 18.0) {
        Err(captured) if captured.kind() == ErrorKind::TtfError && !captured.message().is_empty() => {
            let original = captured.message().to_owned();
            let _ = linked_version();
            assert_eq!(captured.message(), original, "TTF error text changed after a subsequent native call");
        }
        _ => panic!("missing font returned the wrong result"),
    }

    let font = Font::open_file(&font_path, 18.0).unwrap();
    let metrics = font.metrics().unwrap();
    assert!(metrics.height > 0 && metrics.ascent > 0 && metrics.line_skip > 0, "font metrics are not positive");
    assert_eq!(font.render_blended((256, 0, 0, 255), "bad").err().map(|failure| failure.kind()), Some(ErrorKind::InvalidArgument));
    assert_eq!(font.render_blended((0, 0, 0, 255), "bad\0text").err().map(|failure| failure.kind()), Some(ErrorKind::InvalidArgument));
    assert!(font.render_blended((12, 34, 56, 200), "").unwrap().is_none(), "empty text allocated a raster");
    let rgba = some(font.render_blended((12, 34, 56, 200), "Rays ž"), "non-empty text did not rasterize");
    assert!(
        rgba.width > 0
            && rgba.height > 0
            && rgba.pixels.len() == (rgba.width * rgba.height * 4) as usize
            && has_visible_alpha(&rgba.pixels),
        "CPU glyph rasterization disagrees with text metrics or alpha"
    );
    // every render owns its buffer
    let again = some(font.render_blended((12, 34, 56, 200), "Rays ž"), "second raster of the same text failed");
    assert_eq!(again.pixels, rgba.pixels, "two renders of one text differ");
    assert_ne!(again.pixels.as_ptr(), rgba.pixels.as_ptr(), "two renders of one text share a buffer");

    // Setters used by the runtime and glyph metrics.
    font.set_hinting(Hinting::Mono).unwrap();
    font.set_hinting(Hinting::Normal).unwrap();
    let glyph = font.glyph_metrics('A' as i64).unwrap();
    assert!(glyph.advance > 0 && glyph.max_x >= glyph.min_x && glyph.max_y >= glyph.min_y, "ASCII glyph metrics are invalid");
    for codepoint in [-1, 0xd800, 0x110000] {
        assert_eq!(font.glyph_metrics(codepoint).err().map(|failure| failure.kind()), Some(ErrorKind::InvalidArgument), "invalid Unicode scalar was accepted");
    }
    font.set_wrap_alignment(Alignment::Center).unwrap();
    let wrapped = some(
        font.render_blended_wrapped((200, 180, 160, 255), 80, "a deterministic wrapped line with several words"),
        "wrapped non-empty text did not rasterize",
    );
    assert!(
        wrapped.width > 0
            && wrapped.width <= 80
            && wrapped.height > metrics.line_skip as i32
            && wrapped.pixels.len() == (wrapped.width * wrapped.height * 4) as usize
            && has_visible_alpha(&wrapped.pixels),
        "wrapped raster disagrees with wrapped metrics"
    );
    // ponytail: the malformed-UTF-8 case (bad\xed\xa0\x80) cannot be written as a Rust &str.

    // density: the same text at 144 DPI is about twice the size of 72 DPI
    font.set_size_dpi(18.0, 72, 72).unwrap();
    let one_x = some(font.render_blended((12, 34, 56, 200), "Density"), "72-DPI density raster was empty");
    font.set_size_dpi(18.0, 144, 144).unwrap();
    let two_x = some(font.render_blended((12, 34, 56, 200), "Density"), "144-DPI density raster was empty");
    let width_scale = two_x.width as f64 / one_x.width as f64;
    let height_scale = two_x.height as f64 / one_x.height as f64;
    assert!(
        (1.8..=2.2).contains(&width_scale) && (1.8..=2.2).contains(&height_scale),
        "144-DPI raster metrics are not approximately twice 72-DPI metrics: {}x{} -> {}x{}",
        one_x.width, one_x.height, two_x.width, two_x.height
    );
    assert_eq!(font.set_size_dpi(18.0, 0, 144).err().map(|failure| failure.kind()), Some(ErrorKind::InvalidArgument), "non-positive font DPI");

    let wrong_domain = std::thread::scope(|scope| scope.spawn(|| font.metrics()).join().unwrap());
    assert_eq!(wrong_domain.err().map(|failure| failure.kind()), Some(ErrorKind::WrongDomain), "wrong-domain font access");
    assert_eq!(init::quit().err().map(|failure| failure.kind()), Some(ErrorKind::FontsStillOpen), "TTF quit ignored a live font");

    font.destroy().unwrap();
    font.destroy().unwrap();
    assert_eq!(font.metrics().err().map(|failure| failure.kind()), Some(ErrorKind::Destroyed), "stale font access");
    for _ in 0..10_000 {
        let transient = Font::open_file(&font_path, 8.0).unwrap();
        transient.destroy().unwrap();
    }
    init::quit().unwrap();
    init::quit().unwrap();
}

#[test]
fn invalid_font_override_is_a_typed_discovery_failure() {
    match system_path(Some("/definitely/missing/rays-font.ttf")) {
        Err(error) if error.kind() == ErrorKind::FontNotFound && !error.message().is_empty() => {}
        Ok(path) => panic!("invalid font override resolved to {path}"),
        Err(_) => panic!("invalid font override returned the wrong error"),
    }
}

static MAIN: std::sync::OnceLock<std::thread::ThreadId> = std::sync::OnceLock::new();

/// The calling test thread becomes the thread the library treats as the SDL main thread.
fn claim_main() {
    MAIN.get_or_init(|| std::thread::current().id());
}

pub(crate) fn is_main() -> bool {
    MAIN.get() == Some(&std::thread::current().id())
}
