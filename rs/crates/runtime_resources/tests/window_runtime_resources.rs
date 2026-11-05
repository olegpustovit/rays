// Ports of test_runtime_resources.ml, test_png_rows.ml, test_resources_font.ml and
// test_resources_audio.ml. They run on the process main thread (see common/mod.rs).
// Not ported: the `Gc.allocated_bytes` / `Gc.quick_stat` ceilings, which measure OCaml's heap;
// the storage hand-over they guard is checked by snapshot lifetime and pixels instead.
mod common;
use ogpu::error::ErrorKind;
use runtime_resources::*;

// A canvas is written a pixel at a time: [0xRRGGBBAA] at every point.
fn fill(canvas: &Canvas::t, color: i32) {
    let (width, height) = Canvas::size(canvas).unwrap();
    for y in 0..height {
        for x in 0..width {
            Canvas::set_pixel(canvas, x, y, color).unwrap();
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
    let image = Image::create(2, 2, &caller).unwrap();
    caller.fill(0);
    assert_eq!(Image::pixels(&image).unwrap(), kept, "image kept the caller's storage");
    assert_eq!(match Image::create(2, 2, &[0; 15]) { Ok(_) => panic!("expected an error"), Err(e) => e.kind }, ErrorKind::InvalidArgument, "image accepted a short pixel buffer");
    let identity = Image::identity(&image);
    let generation = Image::generation(&image);
    Image::replace(&image, 1, 1, b"\xff\x00\x00\xff").unwrap();
    assert!(
        Image::identity(&image) == identity
            && Image::generation(&image) == generation + 1
            && Image::size(&image).unwrap() == (1, 1)
            && Image::pixels(&image).unwrap() == b"\xff\x00\x00\xff",
        "image identity/generation"
    );
    let canvas = Canvas::create(4, 4).unwrap();
    let canvas_generation = Canvas::generation(&canvas);
    Canvas::set_pixel(&canvas, 1, 2, 0x010203ff).unwrap();
    Canvas::set_pixel(&canvas, 3, 3, 0x7f7f7f7f).unwrap();
    assert_eq!(Canvas::generation(&canvas), canvas_generation + 2, "canvas mutation generation");
    for (x, y) in [(-1, 0), (0, -1), (4, 0), (0, 4)] {
        assert!(
            match Canvas::set_pixel(&canvas, x, y, 0) { Ok(_) => panic!("expected an error"), Err(e) => e.kind } == ErrorKind::InvalidArgument
                && Canvas::generation(&canvas) == canvas_generation + 2,
            "canvas accepted a pixel outside its extent"
        );
    }
    let mut expected = vec![0u8; 64];
    expected[(2 * 4 + 1) * 4..][..4].copy_from_slice(b"\x01\x02\x03\xff");
    expected[(3 * 4 + 3) * 4..][..4].copy_from_slice(b"\x7f\x7f\x7f\x7f");
    let snapshot = Canvas::snapshot(&canvas).unwrap();
    assert_eq!(snapshot, (4, 4, Canvas::generation(&canvas), expected.clone()), "canvas atomic snapshot");
    let bulk_bytes = b"\x01\x02\x03\xff\x10\x20\x30\x40".to_vec();
    let bulk = Canvas::of_rgba_bytes(2, 1, bulk_bytes.clone()).unwrap();
    assert_eq!(Canvas::snapshot(&bulk).unwrap().3, bulk_bytes, "bulk RGBA canvas changed pixel bytes");
    assert_eq!(match Canvas::of_rgba_bytes(2, 1, vec![0; 7]) { Ok(_) => panic!("expected an error"), Err(e) => e.kind }, ErrorKind::InvalidArgument, "bulk RGBA accepted a short buffer");
    Canvas::destroy(&bulk).unwrap();
    let stable = Image::create(4, 4, &[0; 64]).unwrap();
    let stable_identity = Image::identity(&stable);
    let moved = Image::create(4, 4, &[0x42; 64]).unwrap();
    let moved_generation = Image::generation(&stable);
    Image::replace_owned(&stable, &moved).unwrap();
    assert!(
        Image::destroyed(&moved)
            && Image::identity(&stable) == stable_identity
            && Image::generation(&stable) == moved_generation + 1
            && Image::pixels(&stable).unwrap() == [0x42; 64],
        "owned image replacement"
    );
    let rejected = Image::create(1, 1, &[0x31; 4]).unwrap();
    let rejected_pixels = Image::pixels(&rejected).unwrap();
    let stable_pixels = Image::pixels(&stable).unwrap();
    let stable_generation = Image::generation(&stable);
    assert_eq!(match Image::replace_owned(&stable, &stable) { Ok(_) => panic!("expected an error"), Err(e) => e.kind }, ErrorKind::InvalidArgument, "self owned replacement accepted");
    assert!(
        !Image::destroyed(&rejected)
            && Image::pixels(&rejected).unwrap() == rejected_pixels
            && Image::generation(&stable) == stable_generation
            && Image::pixels(&stable).unwrap() == stable_pixels,
        "owned replacement rejection mutated images"
    );
    Image::destroy(&rejected).unwrap();
    for _ in 0..32 {
        let source = Image::create(640, 480, &vec![0x5a; 640 * 480 * 4]).unwrap();
        Image::replace_owned(&stable, &source).unwrap();
    }
    assert_eq!(Image::size(&stable).unwrap(), (640, 480));
    Image::destroy(&stable).unwrap();
    // Leased bytes stay as they were lent: a replacement goes to a second buffer, at most two are
    // lent at once, and a leased source cannot be moved.
    let leased_image = Image::create(8, 8, &solid(64, b"\x10\x20\x30\xff")).unwrap();
    let (_, _, _, leased_pixels, mut lease1) = Image::borrow_snapshot(&leased_image).unwrap();
    let leased_copy = leased_pixels.borrow().clone();
    let lease_target = Image::create(1, 1, &[0x5c; 4]).unwrap();
    let lease_target_before = Image::pixels(&lease_target).unwrap();
    assert_eq!(match Image::replace_owned(&lease_target, &leased_image) { Ok(_) => panic!("expected an error"), Err(e) => e.kind }, ErrorKind::InvalidArgument, "leased source storage was moved");
    assert!(
        !Image::destroyed(&leased_image) && Image::pixels(&lease_target).unwrap() == lease_target_before,
        "leased owned replacement was not atomic"
    );
    Image::destroy(&lease_target).unwrap();
    Image::replace(&leased_image, 8, 8, &solid(64, b"\xaa\xbb\xcc\xff")).unwrap();
    assert_eq!(*leased_pixels.borrow(), leased_copy, "image mutation changed leased snapshot");
    let (_, _, _, second_pixels, mut lease2) = Image::borrow_snapshot(&leased_image).unwrap();
    assert_eq!(*second_pixels.borrow(), solid(64, b"\xaa\xbb\xcc\xff"), "second lease is not the replacement");
    assert_eq!(
        match Image::replace(&leased_image, 8, 8, &solid(64, b"\x11\x22\x33\xff")) { Ok(_) => panic!("expected an error"), Err(e) => e.kind },
        ErrorKind::InvalidArgument,
        "bounded image snapshot buffers were exceeded"
    );
    assert_eq!(Image::pixels(&leased_image).unwrap(), solid(64, b"\xaa\xbb\xcc\xff"), "a refused replacement changed the image");
    Image::release_snapshot(&mut lease1);
    Image::replace(&leased_image, 8, 8, &solid(64, b"\x11\x22\x33\xff")).unwrap();
    assert_eq!(*second_pixels.borrow(), solid(64, b"\xaa\xbb\xcc\xff"), "image mutation changed the second lease");
    Image::release_snapshot(&mut lease2);
    Image::release_snapshot(&mut lease2);
    Image::destroy(&leased_image).unwrap();
    let destroy_leased = Image::create(1, 1, b"\x12\x34\x56\x78").unwrap();
    let (_, _, _, destroy_bytes, mut destroy_lease) = Image::borrow_snapshot(&destroy_leased).unwrap();
    Image::destroy(&destroy_leased).unwrap();
    assert_eq!(*destroy_bytes.borrow(), b"\x12\x34\x56\x78", "destroy invalidated leased image bytes");
    Image::release_snapshot(&mut destroy_lease);
    Image::release_snapshot(&mut destroy_lease);
    let direct = Canvas::create(640, 480).unwrap();
    assert!(Canvas::gpu_snapshot(&direct).is_none(), "fresh Canvas reports a GPU frame");
    // A canvas published every frame: a capture lends the canvas storage, the next write moves
    // the canvas to its spare, and destroying the previous capture returns that storage.
    let mut published = Canvas::capture(&direct).unwrap();
    let mut publish = |frame: i64| {
        Canvas::set_pixel(&direct, frame % 640, frame % 480, ((((frame & 255) << 24) | 0x111fff) as u32) as i32).unwrap();
        let next = Canvas::capture(&direct).unwrap();
        Image::destroy(&published).unwrap();
        published = next;
    };
    for frame in 0..=601 {
        publish(frame);
    }
    let last = Image::pixels(&published).unwrap();
    let at = (601 % 480 * 640 + 601) * 4;
    assert_eq!(&last[at..at + 4], b"\x59\x11\x1f\xff", "the published capture is not the canvas after its last write");
    Image::destroy(&published).unwrap();
    Canvas::destroy(&direct).unwrap();
    // A capture is a picture of its moment: later canvas writes and a replacement of another
    // capture leave it alone.
    let bank_canvas = Canvas::create(4, 4).unwrap();
    fill(&bank_canvas, 0x2468acffu32 as i32);
    let bank_image = Canvas::capture(&bank_canvas).unwrap();
    for round in 1..=100_000 {
        Canvas::set_pixel(&bank_canvas, round & 3, 0, 0x2468acffu32 as i32).unwrap();
        Image::destroy(&Canvas::capture(&bank_canvas).unwrap()).unwrap();
    }
    let first_pixels = Image::pixels(&bank_image).unwrap();
    assert_eq!(first_pixels, solid(16, b"\x24\x68\xac\xff"), "capture is not the canvas pixels");
    let second = Canvas::capture(&bank_canvas).unwrap();
    fill(&bank_canvas, 0x010203ff);
    assert!(
        Image::pixels(&bank_image).unwrap() == first_pixels && Image::pixels(&second).unwrap() == first_pixels,
        "a canvas write changed its captures"
    );
    Image::replace(&bank_image, 4, 4, &[0x3c; 64]).unwrap();
    assert_eq!(Canvas::snapshot(&bank_canvas).unwrap().3, solid(16, b"\x01\x02\x03\xff"), "external image replacement changed canvas");
    Image::destroy(&bank_image).unwrap();
    Image::destroy(&second).unwrap();
    Canvas::destroy(&bank_canvas).unwrap();
    let (captures_before, _) = Canvas::pixel_stats(&canvas);
    for _ in 0..4 {
        let capture = Canvas::capture(&canvas).unwrap();
        assert!(Image::size(&capture).unwrap() == (4, 4) && Image::pixels(&capture).unwrap() == expected, "capture");
        Image::destroy(&capture).unwrap();
    }
    assert_eq!(Canvas::pixel_stats(&canvas), (captures_before + 4, 0), "CPU Canvas successful capture/readback counters");
    let png = std::env::temp_dir().join(format!("rays-next-{}.png", std::process::id()));
    Canvas::save_png(&canvas, png.to_str().unwrap()).unwrap();
    let signature = std::fs::read(&png).unwrap();
    std::fs::remove_file(&png).unwrap();
    assert_eq!(&signature[..8], b"\x89PNG\r\n\x1a\n", "PNG");
    Canvas::destroy(&canvas).unwrap();
    Canvas::destroy(&canvas).unwrap();
    assert!(
        match Canvas::set_pixel(&canvas, 0, 0, 0) { Ok(_) => panic!("expected an error"), Err(e) => e.kind } == ErrorKind::Destroyed && match Canvas::capture(&canvas) { Ok(_) => panic!("expected an error"), Err(e) => e.kind } == ErrorKind::Destroyed,
        "a destroyed canvas was written or captured"
    );
    Image::destroy(&image).unwrap();
    assert_eq!(match Image::pixels(&image) { Ok(_) => panic!("expected an error"), Err(e) => e.kind }, ErrorKind::Destroyed, "a destroyed image was read");
    for _ in 0..100_000 {
        let x = Image::create(1, 1, &[0; 4]).unwrap();
        Image::destroy(&x).unwrap();
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
    let canvas = Canvas::create(2, 2).unwrap();
    let image = Canvas::capture(&canvas).unwrap();
    let (_, _, _, before, mut lease) = Image::borrow_snapshot(&image).unwrap();
    Canvas::set_pixel(&canvas, 0, 0, 0x01020304).unwrap();
    assert_eq!(&before.borrow()[..4], &[0, 0, 0, 0], "the capture changed with the canvas");
    assert_eq!(Canvas::snapshot(&canvas).unwrap().3[..4], [1, 2, 3, 4]);
    Image::release_snapshot(&mut lease);
    Image::destroy(&image).unwrap();
    Canvas::destroy(&canvas).unwrap();
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
    let canvas = Canvas::create(width as i64, height as i64).unwrap();
    let path = std::env::temp_dir().join(format!("rays-png-rows-{}.png", std::process::id()));
    for y in 0..height {
        for x in 0..width {
            let o = (y * width + x) * 4;
            Canvas::set_pixel(&canvas, x as i64, y as i64, i32::from_be_bytes(pixels[o..o + 4].try_into().unwrap())).unwrap();
        }
    }
    let mut previous: Option<Vec<u8>> = None;
    for _ in 0..16 {
        Canvas::save_png(&canvas, path.to_str().unwrap()).unwrap();
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
    Canvas::destroy(&canvas).unwrap();
}

fn font() {
    let mut font = Font::open_system(14.).unwrap();
    Font::set_hinting(&mut font, Font::hinting::Light_hinting).unwrap();
    let white = (255, 255, 255, 255);
    assert!(Font::render(&mut font, None, None, 1, white, b"").unwrap().is_none(), "empty text");
    assert_eq!(match Font::render(&mut font, None, None, 1, white, b"\xc0\x80") { Ok(_) => panic!("expected an error"), Err(e) => e.kind }, ErrorKind::InvalidArgument, "UTF-8");
    let mut transferred = Font::render(&mut font, None, None, 1, white, b"transfer").unwrap().unwrap();
    let mut expected = Text::pixels(&transferred).unwrap();
    let image = Text::into_image(&mut transferred).unwrap();
    assert!(Text::destroyed(&transferred) && Image::pixels(&image).unwrap() == expected, "text-to-image ownership transfer changed pixels");
    expected.fill(0);
    assert!(Image::pixels(&image).unwrap() != expected, "public text pixels still alias the transferred image");
    Text::destroy(&mut transferred).unwrap();
    Image::destroy(&image).unwrap();
    let mut advance = |density| Font::glyph_metrics_at(&mut font, density, 'A' as i64).unwrap().advance;
    let (a1, a2) = (advance(1), advance(2));
    assert!(a1 > 0 && a2 > a1, "glyph metrics");
    assert!(
        matches!(Font::glyph_metrics_at(&mut font, 0, 'A' as i64), Err(e) if e.kind == ErrorKind::InvalidArgument)
            && match Font::render(&mut font, None, None, 17, (0, 0, 0, 255), b"x") { Ok(_) => panic!("expected an error"), Err(e) => e.kind } == ErrorKind::InvalidArgument,
        "density outside 1..16 accepted"
    );
    // Rendered text is its own picture: twice the density is about twice the size, and neither a
    // font mutation nor the font's teardown shortens it.
    let mut size = |density| {
        let mut text = Font::render(&mut font, None, None, density, (255, 0, 0, 255), b"explicit").unwrap().unwrap();
        let size = Text::size(&text).unwrap();
        Text::destroy(&mut text).unwrap();
        size
    };
    let (w1, h1) = size(1);
    let mut explicit = Font::render(&mut font, None, None, 2, (255, 0, 0, 255), b"explicit").unwrap().unwrap();
    let (width, height) = Text::size(&explicit).unwrap();
    assert!(width > 0 && height > 0 && (width - 2 * w1).abs() <= 4 && (height - 2 * h1).abs() <= 2, "explicit text");
    let generation = Text::generation(&explicit);
    let pixels = Text::pixels(&explicit).unwrap();
    Font::set_hinting(&mut font, Font::hinting::Mono_hinting).unwrap();
    Font::destroy(&mut font).unwrap();
    assert!(
        !Text::destroyed(&explicit)
            && Text::size(&explicit).unwrap() == (width, height)
            && Text::generation(&explicit) == generation
            && Text::pixels(&explicit).unwrap() == pixels,
        "font mutation or teardown shortened text lifetime"
    );
    Text::destroy(&mut explicit).unwrap();
    assert!(Text::destroyed(&explicit), "text survived its own destroy");
    Font::destroy(&mut font).unwrap();
    assert_eq!(match Font::render(&mut font, None, None, 1, (0, 0, 0, 0), b"dead") { Ok(_) => panic!("expected an error"), Err(e) => e.kind }, ErrorKind::Destroyed, "stale font");
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
    let owner = Audio::create_device(16).unwrap();
    let mut encoded = wav();
    let mut sample = Audio::load_sample_bytes(&owner, &encoded).unwrap();
    encoded.fill(0);
    assert!(matches!(Audio::load_sample_bytes(&owner, b"bad"), Err(e) if e.kind == ErrorKind::Decode), "undecodable sample accepted");
    // The sample owns its copy of the bytes: it still plays after the caller zeroed them.
    let channel = Audio::play_sample(&owner, Some(1), Some(0.5), &sample).unwrap();
    assert!((0..16).contains(&channel), "sample played outside the channel bank");
    for volume in [1.5, -0.1, f64::NAN] {
        assert_eq!(match Audio::play_sample(&owner, None, Some(volume), &sample) { Ok(_) => panic!("expected an error"), Err(e) => e.kind }, ErrorKind::InvalidArgument, "volume outside 0..1 accepted");
    }
    let second = Audio::play_sample(&owner, None, None, &sample).unwrap();
    assert_ne!(second, channel, "a second voice took the playing channel");
    Audio::destroy_sample(&mut sample).unwrap();
    Audio::destroy_sample(&mut sample).unwrap();
    assert_eq!(match Audio::play_sample(&owner, None, None, &sample) { Ok(_) => panic!("expected an error"), Err(e) => e.kind }, ErrorKind::Destroyed, "destroyed sample played");
    Audio::destroy(&owner).unwrap();
    Audio::destroy(&owner).unwrap();
    assert!(matches!(Audio::load_sample_bytes(&owner, &wav()), Err(e) if e.kind == ErrorKind::Destroyed), "stale audio");
}

fn assets() {
    let mut assets = Assets::create();
    Assets::destroy(&mut assets).unwrap();
    Assets::destroy(&mut assets).unwrap();
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
