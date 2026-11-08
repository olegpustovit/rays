use super::{ErrorKind, check_version, compiled_version, linked_version, load_file};
use crate::Rgba;

fn root() -> String {
    format!("{}/fixtures", env!("CARGO_MANIFEST_DIR"))
}

fn fixture(name: &str) -> String {
    format!("{}/{}", root(), name)
}

fn get_image(result: std::result::Result<Rgba, super::Error>) -> Rgba {
    result.unwrap_or_else(|error| panic!("{error}"))
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

struct Fixture {
    file: &'static str,
    width: i32,
    height: i32,
}

// Every still format the distribution enables.
const FIXTURES: [Fixture; 19] = [
    Fixture { file: "sample.avif", width: 23, height: 42 },
    Fixture { file: "sample.bmp", width: 23, height: 42 },
    Fixture { file: "sample.cur", width: 23, height: 42 },
    Fixture { file: "palette.gif", width: 23, height: 42 },
    Fixture { file: "sample.ico", width: 23, height: 42 },
    Fixture { file: "sample.jpg", width: 23, height: 42 },
    Fixture { file: "sample.jxl", width: 23, height: 42 },
    Fixture { file: "sample.lbm", width: 16, height: 1 },
    Fixture { file: "sample.pcx", width: 23, height: 42 },
    Fixture { file: "sample.png", width: 23, height: 42 },
    Fixture { file: "sample.pnm", width: 23, height: 42 },
    Fixture { file: "sample.qoi", width: 23, height: 42 },
    Fixture { file: "svg.svg", width: 32, height: 32 },
    Fixture { file: "sample.tga", width: 23, height: 42 },
    Fixture { file: "sample.tif", width: 23, height: 42 },
    Fixture { file: "sample.webp", width: 23, height: 42 },
    Fixture { file: "sample.xcf", width: 23, height: 42 },
    Fixture { file: "sample.xpm", width: 23, height: 42 },
    Fixture { file: "sample.xv", width: 2, height: 1 },
];

fn checked_rgba(rgba: Rgba, width: i32, height: i32) -> Rgba {
    assert!(
        rgba.width == width && rgba.height == height && rgba.pixels.len() == (width * height * 4) as usize,
        "decoded {}x{} with {} bytes, expected tightly packed {width}x{height} RGBA8",
        rgba.width, rgba.height, rgba.pixels.len(),
    );
    rgba
}

fn check_fixture(value: &Fixture) {
    let path = fixture(value.file);
    let mut decoded = match load_file(&path) {
        Ok(rgba) => rgba,
        Err(error) => panic!("{} decode: {error}", value.file),
    };
    checked_rgba(decoded.clone(), value.width, value.height);
    // each decode owns its buffer: scribbling on one never shows in the next
    let first = decoded.pixels.clone();
    decoded.pixels.fill(0xAA);
    let again = get_image(load_file(&path));
    assert_ne!(again.pixels.as_ptr(), decoded.pixels.as_ptr(), "{} shared a buffer between two decodes", value.file);
    assert_eq!(again.pixels, first, "{} decoded differently the second time", value.file);
}

fn pixel(pixels: &[u8], width: usize, x: usize, y: usize) -> &[u8] {
    let offset = (y * width + x) * 4;
    &pixels[offset..offset + 4]
}

// A reload that fails leaves the caller's previous pixels alone; one that succeeds replaces them whole.
fn check_reload() {
    let path = std::env::temp_dir().join(format!("rays-sdl3-image-watch-{}.png", std::process::id()));
    let path_text = path.to_string_lossy().into_owned();
    std::fs::write(&path, std::fs::read(fixture("sample.png")).unwrap()).unwrap();
    let mut current = get_image(load_file(&path_text));
    let valid = current.pixels.clone();
    std::fs::write(&path, b"malformed replacement").unwrap();
    match load_file(&path_text) {
        Err(error) if error.kind() == ErrorKind::DecoderError && !error.message().is_empty() => {}
        _ => panic!("malformed reload returned the wrong result"),
    }
    assert_eq!(current.pixels, valid, "a failed reload changed the old pixels");
    std::fs::write(&path, std::fs::read(fixture("rgbrgb.png")).unwrap()).unwrap();
    current = get_image(load_file(&path_text));
    let _ = std::fs::remove_file(&path);
    assert_eq!((current.width, current.height), (256, 256), "a successful reload did not replace the content");
}

#[test]
fn cpu_decode_conformance() {
    claim_main();
    assert!(std::path::Path::new(&root()).exists(), "expected the SDL3_image fixture directory");
    let triple = |v: crate::Version| (v.major, v.minor, v.patch);
    let floor = lock_floor("sdl3_image");
    assert!(triple(compiled_version()) >= floor && triple(linked_version()) >= floor, "installed SDL3_image is below the lock floor");
    if let Err(error) = check_version(true) {
        panic!("{error}");
    }
    for value in &FIXTURES {
        check_fixture(value);
    }

    let alpha = checked_rgba(get_image(load_file(&fixture("alpha.svg"))), 2, 1);
    let alphas: Vec<u8> = alpha.pixels.iter().skip(3).step_by(4).copied().collect();
    assert!(alphas.iter().any(|&a| a < 255) && alphas.contains(&255), "alpha fixture lost transparent or opaque pixels");

    let source = checked_rgba(get_image(load_file(&fixture("sample.jpg"))), 23, 42);
    let rotated = checked_rgba(get_image(load_file(&fixture("orientation-6.jpg"))), 42, 23);
    for y in 0..23 {
        for x in 0..42 {
            assert_eq!(
                pixel(&rotated.pixels, 42, x, y),
                pixel(&source.pixels, 23, y, 41 - x),
                "EXIF orientation-6 pixel mapping changed"
            );
        }
    }

    // malformed input is a typed error for every decoder the extension picks
    for value in &FIXTURES {
        let extension = value.file.rsplit('.').next().unwrap();
        let path = std::env::temp_dir().join(format!("rays-sdl3-image-bad-{}.{extension}", std::process::id()));
        std::fs::write(&path, b"not an image").unwrap();
        let result = load_file(&path.to_string_lossy());
        let _ = std::fs::remove_file(&path);
        match result {
            Err(error) if error.kind() == ErrorKind::DecoderError && !error.message().is_empty() => {}
            Ok(_) => panic!("{} malformed bytes decoded", value.file),
            Err(_) => panic!("{} malformed bytes returned wrong error", value.file),
        }
    }
    match load_file("/definitely/missing/rays-image.png") {
        Err(error) if error.kind() == ErrorKind::DecoderError && !error.message().is_empty() => {}
        _ => panic!("missing image returned the wrong result"),
    }
    match load_file("bad\0path") {
        Err(error) if error.kind() == ErrorKind::InvalidArgument => {}
        _ => panic!("NUL path returned the wrong result"),
    }
    let sample = fixture("sample.png");
    match std::thread::spawn(move || load_file(&sample)).join().unwrap() {
        Err(error) if error.kind() == ErrorKind::WrongDomain => {}
        _ => panic!("wrong-domain decode returned the wrong result"),
    }
    check_reload();

    for _ in 0..10_000 {
        get_image(load_file(&fixture("sample.png")));
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
