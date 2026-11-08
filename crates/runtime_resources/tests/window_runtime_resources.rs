// Ports of test_runtime_resources.ml, test_png_rows.ml, test_resources_font.ml and
// test_resources_audio.ml. They run on the process main thread (see common/mod.rs).
// Not ported: the `Gc.allocated_bytes` / `Gc.quick_stat` ceilings, which measure OCaml's heap;
// the storage hand-over they guard is checked by snapshot lifetime and pixels instead.
mod common;
use ogpu_core::error::ErrorKind;
use runtime_resources::*;

// A canvas is written a pixel at a time: [0xRRGGBBAA] at every point.
fn fill(canvas: &canvas::Canvas, color: i32) {
    let (width, height) = canvas::size(canvas).unwrap();
    for y in 0..height {
        for x in 0..width {
            canvas::set_pixel(canvas, x, y, color).unwrap();
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
    let image = image::create(2, 2, &caller).unwrap();
    caller.fill(0);
    assert_eq!(image::pixels(&image).unwrap(), kept, "image kept the caller's storage");
    assert_eq!(match image::create(2, 2, &[0; 15]) { Ok(_) => panic!("expected an error"), Err(e) => e.kind }, ErrorKind::InvalidArgument, "image accepted a short pixel buffer");
    let identity = image::identity(&image);
    let generation = image::generation(&image);
    image::replace(&image, 1, 1, b"\xff\x00\x00\xff").unwrap();
    assert!(
        image::identity(&image) == identity
            && image::generation(&image) == generation + 1
            && image::size(&image).unwrap() == (1, 1)
            && image::pixels(&image).unwrap() == b"\xff\x00\x00\xff",
        "image identity/generation"
    );
    let canvas = canvas::create(4, 4).unwrap();
    let canvas_generation = canvas::generation(&canvas);
    canvas::set_pixel(&canvas, 1, 2, 0x010203ff).unwrap();
    canvas::set_pixel(&canvas, 3, 3, 0x7f7f7f7f).unwrap();
    assert_eq!(canvas::generation(&canvas), canvas_generation + 2, "canvas mutation generation");
    for (x, y) in [(-1, 0), (0, -1), (4, 0), (0, 4)] {
        assert!(
            match canvas::set_pixel(&canvas, x, y, 0) { Ok(_) => panic!("expected an error"), Err(e) => e.kind } == ErrorKind::InvalidArgument
                && canvas::generation(&canvas) == canvas_generation + 2,
            "canvas accepted a pixel outside its extent"
        );
    }
    let mut expected = vec![0u8; 64];
    expected[(2 * 4 + 1) * 4..][..4].copy_from_slice(b"\x01\x02\x03\xff");
    expected[(3 * 4 + 3) * 4..][..4].copy_from_slice(b"\x7f\x7f\x7f\x7f");
    let snapshot = canvas::snapshot(&canvas).unwrap();
    assert_eq!(snapshot, (4, 4, canvas::generation(&canvas), expected.clone()), "canvas atomic snapshot");
    let bulk_bytes = b"\x01\x02\x03\xff\x10\x20\x30\x40".to_vec();
    let bulk = canvas::of_rgba_bytes(2, 1, bulk_bytes.clone()).unwrap();
    assert_eq!(canvas::snapshot(&bulk).unwrap().3, bulk_bytes, "bulk RGBA canvas changed pixel bytes");
    assert_eq!(match canvas::of_rgba_bytes(2, 1, vec![0; 7]) { Ok(_) => panic!("expected an error"), Err(e) => e.kind }, ErrorKind::InvalidArgument, "bulk RGBA accepted a short buffer");
    canvas::destroy(&bulk).unwrap();
    let stable = image::create(4, 4, &[0; 64]).unwrap();
    let stable_identity = image::identity(&stable);
    let moved = image::create(4, 4, &[0x42; 64]).unwrap();
    let moved_generation = image::generation(&stable);
    image::replace_owned(&stable, &moved).unwrap();
    assert!(
        image::destroyed(&moved)
            && image::identity(&stable) == stable_identity
            && image::generation(&stable) == moved_generation + 1
            && image::pixels(&stable).unwrap() == [0x42; 64],
        "owned image replacement"
    );
    let rejected = image::create(1, 1, &[0x31; 4]).unwrap();
    let rejected_pixels = image::pixels(&rejected).unwrap();
    let stable_pixels = image::pixels(&stable).unwrap();
    let stable_generation = image::generation(&stable);
    assert_eq!(match image::replace_owned(&stable, &stable) { Ok(_) => panic!("expected an error"), Err(e) => e.kind }, ErrorKind::InvalidArgument, "self owned replacement accepted");
    assert!(
        !image::destroyed(&rejected)
            && image::pixels(&rejected).unwrap() == rejected_pixels
            && image::generation(&stable) == stable_generation
            && image::pixels(&stable).unwrap() == stable_pixels,
        "owned replacement rejection mutated images"
    );
    image::destroy(&rejected).unwrap();
    for _ in 0..32 {
        let source = image::create(640, 480, &vec![0x5a; 640 * 480 * 4]).unwrap();
        image::replace_owned(&stable, &source).unwrap();
    }
    assert_eq!(image::size(&stable).unwrap(), (640, 480));
    image::destroy(&stable).unwrap();
    // Leased bytes stay as they were lent: a replacement goes to a second buffer, at most two are
    // lent at once, and a leased source cannot be moved.
    let leased_image = image::create(8, 8, &solid(64, b"\x10\x20\x30\xff")).unwrap();
    let (_, _, _, leased_pixels, mut lease1) = image::borrow_snapshot(&leased_image).unwrap();
    let leased_copy = leased_pixels.to_vec();
    let lease_target = image::create(1, 1, &[0x5c; 4]).unwrap();
    let lease_target_before = image::pixels(&lease_target).unwrap();
    assert_eq!(match image::replace_owned(&lease_target, &leased_image) { Ok(_) => panic!("expected an error"), Err(e) => e.kind }, ErrorKind::InvalidArgument, "leased source storage was moved");
    assert!(
        !image::destroyed(&leased_image) && image::pixels(&lease_target).unwrap() == lease_target_before,
        "leased owned replacement was not atomic"
    );
    image::destroy(&lease_target).unwrap();
    image::replace(&leased_image, 8, 8, &solid(64, b"\xaa\xbb\xcc\xff")).unwrap();
    assert_eq!(*leased_pixels, leased_copy, "image mutation changed leased snapshot");
    let (_, _, _, second_pixels, mut lease2) = image::borrow_snapshot(&leased_image).unwrap();
    assert_eq!(*second_pixels, solid(64, b"\xaa\xbb\xcc\xff"), "second lease is not the replacement");
    assert_eq!(
        match image::replace(&leased_image, 8, 8, &solid(64, b"\x11\x22\x33\xff")) { Ok(_) => panic!("expected an error"), Err(e) => e.kind },
        ErrorKind::InvalidArgument,
        "bounded image snapshot buffers were exceeded"
    );
    assert_eq!(image::pixels(&leased_image).unwrap(), solid(64, b"\xaa\xbb\xcc\xff"), "a refused replacement changed the image");
    image::release_snapshot(&mut lease1);
    image::replace(&leased_image, 8, 8, &solid(64, b"\x11\x22\x33\xff")).unwrap();
    assert_eq!(*second_pixels, solid(64, b"\xaa\xbb\xcc\xff"), "image mutation changed the second lease");
    image::release_snapshot(&mut lease2);
    image::release_snapshot(&mut lease2);
    image::destroy(&leased_image).unwrap();
    let destroy_leased = image::create(1, 1, b"\x12\x34\x56\x78").unwrap();
    let (_, _, _, destroy_bytes, mut destroy_lease) = image::borrow_snapshot(&destroy_leased).unwrap();
    image::destroy(&destroy_leased).unwrap();
    assert_eq!(&destroy_bytes[..], b"\x12\x34\x56\x78", "destroy invalidated leased image bytes");
    image::release_snapshot(&mut destroy_lease);
    image::release_snapshot(&mut destroy_lease);
    let direct = canvas::create(640, 480).unwrap();
    assert!(canvas::gpu_snapshot(&direct).is_none(), "fresh Canvas reports a GPU frame");
    // A canvas published every frame: a capture lends the canvas storage, the next write moves
    // the canvas to its spare, and destroying the previous capture returns that storage.
    let mut published = canvas::capture(&direct).unwrap();
    let mut publish = |frame: i64| {
        canvas::set_pixel(&direct, frame % 640, frame % 480, ((((frame & 255) << 24) | 0x111fff) as u32) as i32).unwrap();
        let next = canvas::capture(&direct).unwrap();
        image::destroy(&published).unwrap();
        published = next;
    };
    for frame in 0..=601 {
        publish(frame);
    }
    let last = image::pixels(&published).unwrap();
    let at = (601 % 480 * 640 + 601) * 4;
    assert_eq!(&last[at..at + 4], b"\x59\x11\x1f\xff", "the published capture is not the canvas after its last write");
    image::destroy(&published).unwrap();
    canvas::destroy(&direct).unwrap();
    // A capture is a picture of its moment: later canvas writes and a replacement of another
    // capture leave it alone.
    let bank_canvas = canvas::create(4, 4).unwrap();
    fill(&bank_canvas, 0x2468acffu32 as i32);
    let bank_image = canvas::capture(&bank_canvas).unwrap();
    for round in 1..=100_000 {
        canvas::set_pixel(&bank_canvas, round & 3, 0, 0x2468acffu32 as i32).unwrap();
        image::destroy(&canvas::capture(&bank_canvas).unwrap()).unwrap();
    }
    let first_pixels = image::pixels(&bank_image).unwrap();
    assert_eq!(first_pixels, solid(16, b"\x24\x68\xac\xff"), "capture is not the canvas pixels");
    let second = canvas::capture(&bank_canvas).unwrap();
    fill(&bank_canvas, 0x010203ff);
    assert!(
        image::pixels(&bank_image).unwrap() == first_pixels && image::pixels(&second).unwrap() == first_pixels,
        "a canvas write changed its captures"
    );
    image::replace(&bank_image, 4, 4, &[0x3c; 64]).unwrap();
    assert_eq!(canvas::snapshot(&bank_canvas).unwrap().3, solid(16, b"\x01\x02\x03\xff"), "external image replacement changed canvas");
    image::destroy(&bank_image).unwrap();
    image::destroy(&second).unwrap();
    canvas::destroy(&bank_canvas).unwrap();
    let (captures_before, _) = canvas::pixel_stats(&canvas);
    for _ in 0..4 {
        let capture = canvas::capture(&canvas).unwrap();
        assert!(image::size(&capture).unwrap() == (4, 4) && image::pixels(&capture).unwrap() == expected, "capture");
        image::destroy(&capture).unwrap();
    }
    assert_eq!(canvas::pixel_stats(&canvas), (captures_before + 4, 0), "CPU Canvas successful capture/readback counters");
    let png = std::env::temp_dir().join(format!("rays-next-{}.png", std::process::id()));
    canvas::save_png(&canvas, png.to_str().unwrap()).unwrap();
    let signature = std::fs::read(&png).unwrap();
    std::fs::remove_file(&png).unwrap();
    assert_eq!(&signature[..8], b"\x89PNG\r\n\x1a\n", "PNG");
    canvas::destroy(&canvas).unwrap();
    canvas::destroy(&canvas).unwrap();
    assert!(
        match canvas::set_pixel(&canvas, 0, 0, 0) { Ok(_) => panic!("expected an error"), Err(e) => e.kind } == ErrorKind::Destroyed && match canvas::capture(&canvas) { Ok(_) => panic!("expected an error"), Err(e) => e.kind } == ErrorKind::Destroyed,
        "a destroyed canvas was written or captured"
    );
    image::destroy(&image).unwrap();
    assert_eq!(match image::pixels(&image) { Ok(_) => panic!("expected an error"), Err(e) => e.kind }, ErrorKind::Destroyed, "a destroyed image was read");
    for _ in 0..100_000 {
        let x = image::create(1, 1, &[0; 4]).unwrap();
        image::destroy(&x).unwrap();
    }
    let off_thread = std::thread::spawn(|| image::create(1, 1, &[0; 4]).err()).join().unwrap();
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
    let canvas = canvas::create(2, 2).unwrap();
    let image = canvas::capture(&canvas).unwrap();
    let (_, _, _, before, mut lease) = image::borrow_snapshot(&image).unwrap();
    canvas::set_pixel(&canvas, 0, 0, 0x01020304).unwrap();
    assert_eq!(&before[..4], &[0, 0, 0, 0], "the capture changed with the canvas");
    assert_eq!(canvas::snapshot(&canvas).unwrap().3[..4], [1, 2, 3, 4]);
    image::release_snapshot(&mut lease);
    image::destroy(&image).unwrap();
    canvas::destroy(&canvas).unwrap();
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
    let canvas = canvas::create(width as i64, height as i64).unwrap();
    let path = std::env::temp_dir().join(format!("rays-png-rows-{}.png", std::process::id()));
    for y in 0..height {
        for x in 0..width {
            let o = (y * width + x) * 4;
            canvas::set_pixel(&canvas, x as i64, y as i64, i32::from_be_bytes(pixels[o..o + 4].try_into().unwrap())).unwrap();
        }
    }
    let mut previous: Option<Vec<u8>> = None;
    for _ in 0..16 {
        canvas::save_png(&canvas, path.to_str().unwrap()).unwrap();
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
    canvas::destroy(&canvas).unwrap();
}

fn font() {
    let mut font = font::open_system(14.).unwrap();
    font::set_hinting(&mut font, font::hinting::Light_hinting).unwrap();
    let white = (255, 255, 255, 255);
    assert!(font::render(&mut font, None, None, 1, white, b"").unwrap().is_none(), "empty text");
    assert_eq!(match font::render(&mut font, None, None, 1, white, b"\xc0\x80") { Ok(_) => panic!("expected an error"), Err(e) => e.kind }, ErrorKind::InvalidArgument, "UTF-8");
    let mut transferred = font::render(&mut font, None, None, 1, white, b"transfer").unwrap().unwrap();
    let mut expected = text::pixels(&transferred).unwrap();
    let image = text::into_image(&mut transferred).unwrap();
    assert!(text::destroyed(&transferred) && image::pixels(&image).unwrap() == expected, "text-to-image ownership transfer changed pixels");
    expected.fill(0);
    assert!(image::pixels(&image).unwrap() != expected, "public text pixels still alias the transferred image");
    text::destroy(&mut transferred).unwrap();
    image::destroy(&image).unwrap();
    let mut advance = |density| font::glyph_metrics_at(&mut font, density, 'A' as i64).unwrap().advance;
    let (a1, a2) = (advance(1), advance(2));
    assert!(a1 > 0 && a2 > a1, "glyph metrics");
    assert!(
        matches!(font::glyph_metrics_at(&mut font, 0, 'A' as i64), Err(e) if e.kind == ErrorKind::InvalidArgument)
            && match font::render(&mut font, None, None, 17, (0, 0, 0, 255), b"x") { Ok(_) => panic!("expected an error"), Err(e) => e.kind } == ErrorKind::InvalidArgument,
        "density outside 1..16 accepted"
    );
    // Rendered text is its own picture: twice the density is about twice the size, and neither a
    // font mutation nor the font's teardown shortens it.
    let mut size = |density| {
        let mut text = font::render(&mut font, None, None, density, (255, 0, 0, 255), b"explicit").unwrap().unwrap();
        let size = text::size(&text).unwrap();
        text::destroy(&mut text).unwrap();
        size
    };
    let (w1, h1) = size(1);
    let mut explicit = font::render(&mut font, None, None, 2, (255, 0, 0, 255), b"explicit").unwrap().unwrap();
    let (width, height) = text::size(&explicit).unwrap();
    assert!(width > 0 && height > 0 && (width - 2 * w1).abs() <= 4 && (height - 2 * h1).abs() <= 2, "explicit text");
    let generation = text::generation(&explicit);
    let pixels = text::pixels(&explicit).unwrap();
    font::set_hinting(&mut font, font::hinting::Mono_hinting).unwrap();
    font::destroy(&mut font).unwrap();
    assert!(
        !text::destroyed(&explicit)
            && text::size(&explicit).unwrap() == (width, height)
            && text::generation(&explicit) == generation
            && text::pixels(&explicit).unwrap() == pixels,
        "font mutation or teardown shortened text lifetime"
    );
    text::destroy(&mut explicit).unwrap();
    assert!(text::destroyed(&explicit), "text survived its own destroy");
    font::destroy(&mut font).unwrap();
    assert_eq!(match font::render(&mut font, None, None, 1, (0, 0, 0, 0), b"dead") { Ok(_) => panic!("expected an error"), Err(e) => e.kind }, ErrorKind::Destroyed, "stale font");
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
    let owner = audio::create_device(16).unwrap();
    let mut encoded = wav();
    let mut sample = audio::load_sample_bytes(&owner, &encoded).unwrap();
    encoded.fill(0);
    assert!(matches!(audio::load_sample_bytes(&owner, b"bad"), Err(e) if e.kind == ErrorKind::Decode), "undecodable sample accepted");
    // The sample owns its copy of the bytes: it still plays after the caller zeroed them.
    let channel = audio::play_sample(&owner, Some(1), Some(0.5), &sample).unwrap();
    assert!((0..16).contains(&channel), "sample played outside the channel bank");
    for volume in [1.5, -0.1, f64::NAN] {
        assert_eq!(match audio::play_sample(&owner, None, Some(volume), &sample) { Ok(_) => panic!("expected an error"), Err(e) => e.kind }, ErrorKind::InvalidArgument, "volume outside 0..1 accepted");
    }
    let second = audio::play_sample(&owner, None, None, &sample).unwrap();
    assert_ne!(second, channel, "a second voice took the playing channel");
    audio::destroy_sample(&mut sample).unwrap();
    audio::destroy_sample(&mut sample).unwrap();
    assert_eq!(match audio::play_sample(&owner, None, None, &sample) { Ok(_) => panic!("expected an error"), Err(e) => e.kind }, ErrorKind::Destroyed, "destroyed sample played");
    audio::destroy(&owner).unwrap();
    audio::destroy(&owner).unwrap();
    assert!(matches!(audio::load_sample_bytes(&owner, &wav()), Err(e) if e.kind == ErrorKind::Destroyed), "stale audio");
}

fn assets() {
    let mut assets = assets::create();
    assets::destroy(&mut assets).unwrap();
    assets::destroy(&mut assets).unwrap();
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
