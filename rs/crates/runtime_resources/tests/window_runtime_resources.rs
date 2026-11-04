// Ports of test_runtime_resources.ml, test_png_rows.ml, test_resources_font.ml and
// test_resources_audio.ml. They run on the process main thread (see common/mod.rs).
// Not ported: the `Gc.allocated_bytes` / `Gc.quick_stat` ceilings, which measure OCaml's heap;
// the storage hand-over they guard is checked by identity (`Rc::ptr_eq`) and by pixels instead.
mod common;
use ogpu::error::ErrorKind;
use runtime_resources::*;

fn get<A>(result: Result<A, error>) -> A {
    result.unwrap_or_else(|e| {
        let mut text = String::new();
        pp_error(&mut text, &e);
        panic!("{text}")
    })
}

fn kind<A>(result: Result<A, error>) -> ErrorKind {
    match result {
        Ok(_) => panic!("expected an error"),
        Err(e) => e.kind,
    }
}

// A canvas is written a pixel at a time: [0xRRGGBBAA] at every point.
fn fill(canvas: &Canvas::t, color: i32) {
    let (width, height) = get(Canvas::size(canvas));
    for y in 0..height {
        for x in 0..width {
            get(Canvas::set_pixel(canvas, x, y, color));
        }
    }
}

fn solid(count: usize, pattern: &[u8; 4]) -> Vec<u8> {
    (0..count * 4).map(|i| pattern[i % 4]).collect()
}

fn temp_dir(prefix: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!("{prefix}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

fn atomic_file_writes() {
    let root = temp_dir("rays-resources-io");
    let directory = root.join("nested").join("deeper");
    let target = directory.join("state.txt");
    let blocker = root.join("not-a-directory");
    let (directory_text, target_text) = (directory.to_str().unwrap(), target.to_str().unwrap());
    ensure_directory(directory_text).unwrap();
    write_text(target_text, "old").unwrap();
    assert_eq!(write_atomic(target_text, &|_| Err("writer failed".to_string())), Err("writer failed".to_string()));
    let names: Vec<String> =
        std::fs::read_dir(&directory).unwrap().map(|e| e.unwrap().file_name().to_string_lossy().into_owned()).collect();
    assert!(std::fs::read_to_string(&target).unwrap() == "old" && names == ["state.txt"], "failed atomic write replaced or leaked a temporary file");
    write_text(target_text, "new\ntext").unwrap();
    assert_eq!(std::fs::read_to_string(&target).unwrap(), "new\ntext", "atomic text replacement");
    std::fs::write(&blocker, "file").unwrap();
    assert!(ensure_directory(blocker.to_str().unwrap()).is_err(), "file accepted as directory");
    std::fs::remove_dir_all(&root).unwrap();
}

fn image_and_canvas() {
    let original = b"\x11\x22\x33\xff\x44\x55\x66\xff\x77\x88\x99\xff\xaa\xbb\xcc\xff".to_vec();
    let kept = original.clone();
    let mut caller = original;
    let image = get(Image::create(2, 2, &caller));
    caller.fill(0);
    assert_eq!(get(Image::pixels(&image)), kept, "image kept the caller's storage");
    assert_eq!(kind(Image::create(2, 2, &[0; 15])), ErrorKind::InvalidArgument, "image accepted a short pixel buffer");
    let identity = Image::identity(&image);
    let generation = Image::generation(&image);
    get(Image::replace(&image, 1, 1, b"\xff\x00\x00\xff"));
    assert!(
        Image::identity(&image) == identity
            && Image::generation(&image) == generation + 1
            && get(Image::size(&image)) == (1, 1)
            && get(Image::pixels(&image)) == b"\xff\x00\x00\xff",
        "image identity/generation"
    );
    let canvas = get(Canvas::create(4, 4));
    let canvas_generation = Canvas::generation(&canvas);
    get(Canvas::set_pixel(&canvas, 1, 2, 0x010203ff));
    get(Canvas::set_pixel(&canvas, 3, 3, 0x7f7f7f7f));
    assert_eq!(Canvas::generation(&canvas), canvas_generation + 2, "canvas mutation generation");
    for (x, y) in [(-1, 0), (0, -1), (4, 0), (0, 4)] {
        assert!(
            kind(Canvas::set_pixel(&canvas, x, y, 0)) == ErrorKind::InvalidArgument
                && Canvas::generation(&canvas) == canvas_generation + 2,
            "canvas accepted a pixel outside its extent"
        );
    }
    let mut expected = vec![0u8; 64];
    expected[(2 * 4 + 1) * 4..][..4].copy_from_slice(b"\x01\x02\x03\xff");
    expected[(3 * 4 + 3) * 4..][..4].copy_from_slice(b"\x7f\x7f\x7f\x7f");
    let snapshot = get(Canvas::snapshot(&canvas));
    assert_eq!(snapshot, (4, 4, Canvas::generation(&canvas), expected.clone()), "canvas atomic snapshot");
    let bulk_bytes = b"\x01\x02\x03\xff\x10\x20\x30\x40".to_vec();
    let bulk = get(Canvas::of_rgba_bytes(2, 1, bulk_bytes.clone()));
    assert_eq!(get(Canvas::snapshot(&bulk)).3, bulk_bytes, "bulk RGBA canvas changed pixel bytes");
    assert_eq!(kind(Canvas::of_rgba_bytes(2, 1, vec![0; 7])), ErrorKind::InvalidArgument, "bulk RGBA accepted a short buffer");
    get(Canvas::destroy(&bulk));
    let stable = get(Image::create(4, 4, &[0; 64]));
    let stable_identity = Image::identity(&stable);
    let moved = get(Image::create(4, 4, &[0x42; 64]));
    let moved_generation = Image::generation(&stable);
    get(Image::replace_owned(&stable, &moved));
    assert!(
        Image::destroyed(&moved)
            && Image::identity(&stable) == stable_identity
            && Image::generation(&stable) == moved_generation + 1
            && get(Image::pixels(&stable)) == [0x42; 64],
        "owned image replacement"
    );
    let rejected = get(Image::create(1, 1, &[0x31; 4]));
    let rejected_pixels = get(Image::pixels(&rejected));
    let stable_pixels = get(Image::pixels(&stable));
    let stable_generation = Image::generation(&stable);
    assert_eq!(kind(Image::replace_owned(&stable, &stable)), ErrorKind::InvalidArgument, "self owned replacement accepted");
    assert!(
        !Image::destroyed(&rejected)
            && get(Image::pixels(&rejected)) == rejected_pixels
            && Image::generation(&stable) == stable_generation
            && get(Image::pixels(&stable)) == stable_pixels,
        "owned replacement rejection mutated images"
    );
    get(Image::destroy(&rejected));
    for _ in 0..32 {
        let source = get(Image::create(640, 480, &vec![0x5a; 640 * 480 * 4]));
        get(Image::replace_owned(&stable, &source));
    }
    assert_eq!(get(Image::size(&stable)), (640, 480));
    get(Image::destroy(&stable));
    // Leased bytes stay as they were lent: a replacement goes to a second buffer, at most two are
    // lent at once, and a leased source cannot be moved.
    let leased_image = get(Image::create(8, 8, &solid(64, b"\x10\x20\x30\xff")));
    let (_, _, _, leased_pixels, mut lease1) = get(Image::borrow_snapshot(&leased_image));
    let leased_copy = leased_pixels.borrow().clone();
    let lease_target = get(Image::create(1, 1, &[0x5c; 4]));
    let lease_target_before = get(Image::pixels(&lease_target));
    assert_eq!(kind(Image::replace_owned(&lease_target, &leased_image)), ErrorKind::InvalidArgument, "leased source storage was moved");
    assert!(
        !Image::destroyed(&leased_image) && get(Image::pixels(&lease_target)) == lease_target_before,
        "leased owned replacement was not atomic"
    );
    get(Image::destroy(&lease_target));
    get(Image::replace(&leased_image, 8, 8, &solid(64, b"\xaa\xbb\xcc\xff")));
    assert_eq!(*leased_pixels.borrow(), leased_copy, "image mutation changed leased snapshot");
    let (_, _, _, second_pixels, mut lease2) = get(Image::borrow_snapshot(&leased_image));
    assert_eq!(*second_pixels.borrow(), solid(64, b"\xaa\xbb\xcc\xff"), "second lease is not the replacement");
    assert_eq!(
        kind(Image::replace(&leased_image, 8, 8, &solid(64, b"\x11\x22\x33\xff"))),
        ErrorKind::InvalidArgument,
        "bounded image snapshot buffers were exceeded"
    );
    assert_eq!(get(Image::pixels(&leased_image)), solid(64, b"\xaa\xbb\xcc\xff"), "a refused replacement changed the image");
    Image::release_snapshot(&mut lease1);
    get(Image::replace(&leased_image, 8, 8, &solid(64, b"\x11\x22\x33\xff")));
    assert_eq!(*second_pixels.borrow(), solid(64, b"\xaa\xbb\xcc\xff"), "image mutation changed the second lease");
    Image::release_snapshot(&mut lease2);
    Image::release_snapshot(&mut lease2);
    get(Image::destroy(&leased_image));
    let destroy_leased = get(Image::create(1, 1, b"\x12\x34\x56\x78"));
    let (_, _, _, destroy_bytes, mut destroy_lease) = get(Image::borrow_snapshot(&destroy_leased));
    get(Image::destroy(&destroy_leased));
    assert_eq!(*destroy_bytes.borrow(), b"\x12\x34\x56\x78", "destroy invalidated leased image bytes");
    Image::release_snapshot(&mut destroy_lease);
    Image::release_snapshot(&mut destroy_lease);
    let direct = get(Canvas::create(640, 480));
    assert!(Canvas::gpu_snapshot(&direct).is_none(), "fresh Canvas reports a GPU frame");
    // A canvas published every frame: a capture lends the canvas storage, the next write moves
    // the canvas to its spare, and destroying the previous capture returns that storage.
    let mut published = get(Canvas::capture(&direct));
    let mut publish = |frame: i64| {
        get(Canvas::set_pixel(&direct, frame % 640, frame % 480, ((((frame & 255) << 24) | 0x111fff) as u32) as i32));
        let next = get(Canvas::capture(&direct));
        get(Image::destroy(&published));
        published = next;
    };
    for frame in 0..=601 {
        publish(frame);
    }
    let last = get(Image::pixels(&published));
    let at = (601 % 480 * 640 + 601) * 4;
    assert_eq!(&last[at..at + 4], b"\x59\x11\x1f\xff", "the published capture is not the canvas after its last write");
    get(Image::destroy(&published));
    get(Canvas::destroy(&direct));
    // A capture is a picture of its moment: later canvas writes and a replacement of another
    // capture leave it alone.
    let bank_canvas = get(Canvas::create(4, 4));
    fill(&bank_canvas, 0x2468acffu32 as i32);
    let bank_image = get(Canvas::capture(&bank_canvas));
    for round in 1..=2000 {
        get(Canvas::set_pixel(&bank_canvas, round & 3, 0, 0x2468acffu32 as i32));
        get(Image::destroy(&get(Canvas::capture(&bank_canvas))));
    }
    let first_pixels = get(Image::pixels(&bank_image));
    assert_eq!(first_pixels, solid(16, b"\x24\x68\xac\xff"), "capture is not the canvas pixels");
    let second = get(Canvas::capture(&bank_canvas));
    fill(&bank_canvas, 0x010203ff);
    assert!(
        get(Image::pixels(&bank_image)) == first_pixels && get(Image::pixels(&second)) == first_pixels,
        "a canvas write changed its captures"
    );
    get(Image::replace(&bank_image, 4, 4, &[0x3c; 64]));
    assert_eq!(get(Canvas::snapshot(&bank_canvas)).3, solid(16, b"\x01\x02\x03\xff"), "external image replacement changed canvas");
    get(Image::destroy(&bank_image));
    get(Image::destroy(&second));
    get(Canvas::destroy(&bank_canvas));
    let (captures_before, _) = Canvas::pixel_stats(&canvas);
    for _ in 0..4 {
        let capture = get(Canvas::capture(&canvas));
        assert!(get(Image::size(&capture)) == (4, 4) && get(Image::pixels(&capture)) == expected, "capture");
        get(Image::destroy(&capture));
    }
    assert_eq!(Canvas::pixel_stats(&canvas), (captures_before + 4, 0), "CPU Canvas successful capture/readback counters");
    let png = std::env::temp_dir().join(format!("rays-next-{}.png", std::process::id()));
    get(Canvas::save_png(&canvas, png.to_str().unwrap()));
    let signature = std::fs::read(&png).unwrap();
    std::fs::remove_file(&png).unwrap();
    assert_eq!(&signature[..8], b"\x89PNG\r\n\x1a\n", "PNG");
    get(Canvas::destroy(&canvas));
    get(Canvas::destroy(&canvas));
    assert!(
        kind(Canvas::set_pixel(&canvas, 0, 0, 0)) == ErrorKind::Destroyed && kind(Canvas::capture(&canvas)) == ErrorKind::Destroyed,
        "a destroyed canvas was written or captured"
    );
    get(Image::destroy(&image));
    assert_eq!(kind(Image::pixels(&image)), ErrorKind::Destroyed, "a destroyed image was read");
    for _ in 0..2000 {
        let x = get(Image::create(1, 1, &[0; 4]));
        get(Image::destroy(&x));
    }
    let off_thread = std::thread::spawn(|| Image::create(1, 1, &[0; 4]).err()).join().unwrap();
    match off_thread {
        Some(e)
            if e.operation == "Image.create"
                && e.kind == ErrorKind::WrongDomain
                && e.message == "resource operation requires the initial SDL domain" => {}
        _ => panic!("resource domain error lost its shared operation, kind, or message"),
    }
}

// Mutation through a captured mirror reuses storage by identity: the capture shares the
// canvas's bytes until the canvas writes, then the canvas moves to a second buffer.
fn capture_shares_storage_until_write() {
    let canvas = get(Canvas::create(2, 2));
    let image = get(Canvas::capture(&canvas));
    let (_, _, _, before, mut lease) = get(Image::borrow_snapshot(&image));
    get(Canvas::set_pixel(&canvas, 0, 0, 0x01020304));
    assert_eq!(&before.borrow()[..4], &[0, 0, 0, 0], "the capture changed with the canvas");
    assert_eq!(get(Canvas::snapshot(&canvas)).3[..4], [1, 2, 3, 4]);
    Image::release_snapshot(&mut lease);
    get(Image::destroy(&image));
    get(Canvas::destroy(&canvas));
}

fn png_scanlines(png: &[u8]) -> Vec<u8> {
    let mut data = Vec::new();
    let mut i = 8;
    while i < png.len() {
        let len = u32::from_be_bytes(png[i..i + 4].try_into().unwrap()) as usize;
        if &png[i + 4..i + 8] == b"IDAT" {
            data.extend_from_slice(&png[i + 8..i + 8 + len]);
        }
        i += 12 + len;
    }
    let mut raw = Vec::new();
    let mut i = 2;
    loop {
        let header = data[i];
        assert_eq!(header & 6, 0);
        let len = u16::from_le_bytes([data[i + 1], data[i + 2]]) as usize;
        assert_eq!(u16::from_le_bytes([data[i + 3], data[i + 4]]), !(len as u16));
        raw.extend_from_slice(&data[i + 5..i + 5 + len]);
        if header & 1 == 0 {
            i += 5 + len;
        } else {
            return raw;
        }
    }
}

// The writer deliberately emits stored DEFLATE blocks. Inspect every decoded scanline, across
// block boundaries, not just the PNG signature.
fn png_rows() {
    let (width, height) = (257usize, 129usize);
    let pixels: Vec<u8> = (0..257 * 129 * 4).map(|i: usize| ((i * 37 + 19) & 255) as u8).collect();
    let canvas = get(Canvas::create(width as i64, height as i64));
    let path = std::env::temp_dir().join(format!("rays-png-rows-{}.png", std::process::id()));
    for y in 0..height {
        for x in 0..width {
            let o = (y * width + x) * 4;
            get(Canvas::set_pixel(&canvas, x as i64, y as i64, i32::from_be_bytes(pixels[o..o + 4].try_into().unwrap())));
        }
    }
    let mut previous: Option<Vec<u8>> = None;
    for _ in 0..4 {
        get(Canvas::save_png(&canvas, path.to_str().unwrap()));
        let png = std::fs::read(&path).unwrap();
        let raw = png_scanlines(&png);
        assert_eq!(raw.len(), height * (width * 4 + 1));
        for y in 0..height {
            let offset = y * (width * 4 + 1);
            assert_eq!(raw[offset], 0, "PNG filter byte is not initialized to None");
            assert_eq!(raw[offset + 1..offset + 1 + width * 4], pixels[y * width * 4..(y + 1) * width * 4]);
        }
        if let Some(expected) = &previous {
            assert!(*expected == png);
        }
        previous = Some(png);
    }
    std::fs::remove_file(&path).unwrap();
    get(Canvas::destroy(&canvas));
}

fn font() {
    let mut font = get(Font::open_system(14.));
    get(Font::set_hinting(&mut font, Font::hinting::Light_hinting));
    let white = (255, 255, 255, 255);
    assert!(get(Font::render(&mut font, None, None, 1, white, b"")).is_none(), "empty text");
    assert_eq!(kind(Font::render(&mut font, None, None, 1, white, b"\xc0\x80")), ErrorKind::InvalidArgument, "UTF-8");
    let mut transferred = get(Font::render(&mut font, None, None, 1, white, b"transfer")).unwrap();
    let mut expected = get(Text::pixels(&transferred));
    let image = get(Text::into_image(&mut transferred));
    assert!(Text::destroyed(&transferred) && get(Image::pixels(&image)) == expected, "text-to-image ownership transfer changed pixels");
    expected.fill(0);
    assert!(get(Image::pixels(&image)) != expected, "public text pixels still alias the transferred image");
    get(Text::destroy(&mut transferred));
    get(Image::destroy(&image));
    let mut advance = |density| get(Font::glyph_metrics_at(&mut font, density, 'A' as i64)).advance;
    let (a1, a2) = (advance(1), advance(2));
    assert!(a1 > 0 && a2 > a1, "glyph metrics");
    assert!(
        matches!(Font::glyph_metrics_at(&mut font, 0, 'A' as i64), Err(e) if e.kind == ErrorKind::InvalidArgument)
            && kind(Font::render(&mut font, None, None, 17, (0, 0, 0, 255), b"x")) == ErrorKind::InvalidArgument,
        "density outside 1..16 accepted"
    );
    // Rendered text is its own picture: twice the density is about twice the size, and neither a
    // font mutation nor the font's teardown shortens it.
    let mut size = |density| {
        let mut text = get(Font::render(&mut font, None, None, density, (255, 0, 0, 255), b"explicit")).unwrap();
        let size = get(Text::size(&text));
        get(Text::destroy(&mut text));
        size
    };
    let (w1, h1) = size(1);
    let mut explicit = get(Font::render(&mut font, None, None, 2, (255, 0, 0, 255), b"explicit")).unwrap();
    let (width, height) = get(Text::size(&explicit));
    assert!(width > 0 && height > 0 && (width - 2 * w1).abs() <= 4 && (height - 2 * h1).abs() <= 2, "explicit text");
    let generation = Text::generation(&explicit);
    let pixels = get(Text::pixels(&explicit));
    get(Font::set_hinting(&mut font, Font::hinting::Mono_hinting));
    get(Font::destroy(&mut font));
    assert!(
        !Text::destroyed(&explicit)
            && get(Text::size(&explicit)) == (width, height)
            && Text::generation(&explicit) == generation
            && get(Text::pixels(&explicit)) == pixels,
        "font mutation or teardown shortened text lifetime"
    );
    get(Text::destroy(&mut explicit));
    assert!(Text::destroyed(&explicit), "text survived its own destroy");
    get(Font::destroy(&mut font));
    assert_eq!(kind(Font::render(&mut font, None, None, 1, (0, 0, 0, 0), b"dead")), ErrorKind::Destroyed, "stale font");
}

fn wav() -> Vec<u8> {
    let n = 800usize;
    let mut b = vec![0u8; 44 + n * 2];
    let u16s = |b: &mut Vec<u8>, o: usize, v: u32| {
        b[o] = (v & 255) as u8;
        b[o + 1] = ((v >> 8) & 255) as u8;
    };
    let u32s = |b: &mut Vec<u8>, o: usize, v: u32| {
        u16s(b, o, v & 65535);
        u16s(b, o + 2, (v >> 16) & 65535);
    };
    b[0..4].copy_from_slice(b"RIFF");
    u32s(&mut b, 4, 36 + n as u32 * 2);
    b[8..16].copy_from_slice(b"WAVEfmt ");
    u32s(&mut b, 16, 16);
    u16s(&mut b, 20, 1);
    u16s(&mut b, 22, 1);
    u32s(&mut b, 24, 8000);
    u32s(&mut b, 28, 16000);
    u16s(&mut b, 32, 2);
    u16s(&mut b, 34, 16);
    b[36..40].copy_from_slice(b"data");
    u32s(&mut b, 40, n as u32 * 2);
    for i in 0..n {
        let v: i32 = if i & 8 == 0 { 8000 } else { -8000 };
        u16s(&mut b, 44 + i * 2, (v & 65535) as u32);
    }
    b
}

fn audio() {
    let owner = get(Audio::create_device(16));
    let mut encoded = wav();
    let mut sample = get(Audio::load_sample_bytes(&owner, &encoded));
    encoded.fill(0);
    assert!(matches!(Audio::load_sample_bytes(&owner, b"bad"), Err(e) if e.kind == ErrorKind::Decode), "undecodable sample accepted");
    // The sample owns its copy of the bytes: it still plays after the caller zeroed them.
    let channel = get(Audio::play_sample(&owner, Some(1), Some(0.5), &sample));
    assert!((0..16).contains(&channel), "sample played outside the channel bank");
    for volume in [1.5, -0.1, f64::NAN] {
        assert_eq!(kind(Audio::play_sample(&owner, None, Some(volume), &sample)), ErrorKind::InvalidArgument, "volume outside 0..1 accepted");
    }
    let second = get(Audio::play_sample(&owner, None, None, &sample));
    assert_ne!(second, channel, "a second voice took the playing channel");
    get(Audio::destroy_sample(&mut sample));
    get(Audio::destroy_sample(&mut sample));
    assert_eq!(kind(Audio::play_sample(&owner, None, None, &sample)), ErrorKind::Destroyed, "destroyed sample played");
    get(Audio::destroy(&owner));
    get(Audio::destroy(&owner));
    assert!(matches!(Audio::load_sample_bytes(&owner, &wav()), Err(e) if e.kind == ErrorKind::Destroyed), "stale audio");
}

fn assets() {
    let mut assets = Assets::create();
    get(Assets::destroy(&mut assets));
    get(Assets::destroy(&mut assets));
}

fn main() {
    common::run(&[
        ("atomic_file_writes", atomic_file_writes),
        ("image_and_canvas", image_and_canvas),
        ("capture_shares_storage_until_write", capture_shares_storage_until_write),
        ("png_rows", png_rows),
        ("font", font),
        ("audio", audio),
        ("assets", assets),
    ]);
}
