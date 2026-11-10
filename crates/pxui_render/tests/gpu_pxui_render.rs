//! Offscreen painter cases on Metal (macOS) or fatal-validation Vulkan (Windows): one golden scene
//! with every instance kind, analytic spot checks, the one-draw-per-layer rule and a zero-allocation
//! warm frame. `RAYS_UPDATE_FIXTURES=1` rewrites the golden.
use mem_sys as _; // the counting global allocator
use ogpu_core::backend::{self as be, ColorAttachment, Device, Queue, RenderEncoder, RenderTarget, Texture};
use ogpu_core::error::{self, Error};
use ogpu_core::pipeline::{ColorFormat, DepthFormat};
use ogpu_core::render_pass::{Load, Store};
use ogpu_core::types::{Extent, Origin, TextureDescriptor, TextureFormat, TextureUsage};
use pxui_render::{AtlasKind, Break, Instance, Painter, View};

const W: u32 = 128;
const H: u32 = 96;
const SCALE: u32 = 2;
const BG: [u8; 4] = [204, 204, 204, 255];

fn ok<T>(result: Result<T, Error>) -> T { result.unwrap_or_else(|e| panic!("{}", error::to_string(&e))) }

/// A device on the platform backend, or None (reported as a skip) where neither exists.
fn open() -> Option<Device> {
    #[cfg(windows)]
    ogpu_core::impl_::select(ogpu_core::impl_::Impl {
        backend: ogpu_vulkan::impl_::BACKEND,
        create_driver: || ogpu_vulkan::backend::create_with_validation((true, true)),
        live_handles: ogpu_vulkan::impl_::live_handles,
    });
    #[cfg(target_os = "macos")]
    ogpu_core::impl_::select(ogpu_metal::impl_::implementation());
    if !cfg!(any(windows, target_os = "macos")) {
        println!("RAYS_TEST_SKIP: no native OGPU implementation on this platform");
        assert!(std::env::var("RAYS_ALLOW_SKIP").as_deref() == Ok("1"), "RAYS_ALLOW_SKIP=1 explicitly permits skipping");
        return None;
    }
    Some(ok(be::create_device(ok(ogpu_core::impl_::create_driver()))))
}

fn texture(device: &Device, width: u32, height: u32, usage: &[TextureUsage]) -> Texture {
    ok(be::create_texture(device, TextureDescriptor {
        label: Some("pxui_render.test".into()), width: width as i64, height: height as i64, depth: 1, mip_levels: 1,
        sample_count: 1, format: TextureFormat::Rgba8Unorm, usage: usage.to_vec(),
    }))
}

fn target(device: &Device) -> Texture {
    texture(device, W * SCALE, H * SCALE, &[TextureUsage::RenderAttachment, TextureUsage::TextureCopySrc, TextureUsage::TextureBinding])
}

/// Open a clearing pass on `target`, run `body`, submit and wait.
fn pass(queue: &Queue, target: &Texture, body: &mut dyn FnMut(&RenderEncoder)) {
    let commands = ok(be::begin_commands(queue));
    let clear = (BG[0] as f64 / 255.0, BG[1] as f64 / 255.0, BG[2] as f64 / 255.0, 1.0);
    let encoder = ok(be::render_encoder(&commands, &RenderTarget {
        colors: vec![ColorAttachment { texture: target.clone(), resolve: None, load: Load::Clear, store: Store::Store, clear }],
        depth: None,
    }));
    body(&encoder);
    ok(be::end_render(&encoder));
    let receipt = ok(be::commit(&commands));
    ok(be::complete_through(queue, receipt.epoch));
}

fn painter(device: &Device) -> Painter {
    ok(Painter::new(device, ColorFormat::Rgba8Unorm, DepthFormat::NoDepth, 1, (64, 64), (32, 32)))
}

/// Synthetic atlas content: an 8x8 ring glyph in the mask atlas at (16, 16), a 4x4 checker at (0, 0)
/// of the colour atlas.
fn upload_atlases(painter: &mut Painter, glyph: &[u8], checker: &[u8]) {
    ok(painter.upload(AtlasKind::Mask, 16, 16, 8, 8, glyph, 8));
    ok(painter.upload(AtlasKind::Color, 0, 0, 4, 4, checker, 16));
}

fn atlas_bytes() -> (Vec<u8>, Vec<u8>) {
    let glyph = (0..64).map(|i| {
        let (x, y) = ((i % 8) as f32 - 3.5, (i / 8) as f32 - 3.5);
        let d = (x * x + y * y).sqrt();
        if (1.5..3.5).contains(&d) { 255 } else { 0 }
    }).collect();
    let checker = (0..16).flat_map(|i| if (i % 4 + i / 4) % 2 == 0 { [255, 255, 255, 255] } else { [20, 40, 160, 255] }).collect();
    (glyph, checker)
}

const RED: [u8; 4] = [220, 40, 40, 255];
const BLUE: [u8; 4] = [40, 80, 220, 255];
const GREEN: [u8; 4] = [30, 170, 70, 255];
const INK: [u8; 4] = [30, 30, 40, 255];

/// Every kind in a 128x96-point scene.
fn scene() -> Vec<Instance> {
    let tau = std::f32::consts::TAU;
    vec![
        Instance::rect([4.0, 4.0, 28.0, 20.0], RED),
        Instance::rect([32.0, 4.0, 60.0, 20.0], BLUE).radii([0.0, 4.0, 8.0, 2.0]),
        Instance::rect([64.0, 4.0, 92.0, 20.0], [250, 210, 60, 255]).radii([4.0; 4]).border(2.0, INK),
        Instance::rect([96.0, 4.0, 124.0, 20.0], RED).linear([0.0, 0.5], [1.0, 0.5], BLUE),
        Instance::rect([4.0, 24.0, 28.0, 48.0], GREEN).radial([0.5, 0.5], [0.5, 0.5], [200, 40, 200, 255]),
        Instance::rect([32.0, 24.0, 56.0, 48.0], RED).radii([12.0; 4]).conic([0.5, 0.5], 0.0, BLUE),
        Instance::shadow([64.0, 26.0, 84.0, 42.0], [4.0; 4], [0, 0, 0, 160], [2.0, 2.0], 3.0, 0.0, false),
        Instance::rect([64.0, 26.0, 84.0, 42.0], [255, 255, 255, 255]).radii([4.0; 4]),
        Instance::rect([96.0, 26.0, 120.0, 44.0], [255, 255, 255, 255]).radii([6.0; 4]),
        Instance::shadow([96.0, 26.0, 120.0, 44.0], [6.0; 4], [0, 0, 0, 200], [2.0, 2.0], 2.0, 1.0, true),
        Instance::line([4.0, 54.0], [40.0, 60.0], 2.0, INK),
        Instance::line([4.0, 64.0], [40.0, 64.0], 2.0, BLUE).dash(4.0, 2.0, 0.0).round_caps(),
        Instance::arc([56.0, 62.0], 8.0, 0.0, 0.7 * tau, 2.0, RED),
        Instance::arc([56.0, 62.0], 4.0, 0.0, -0.5 * tau, 1.0, BLUE).dash(2.0, 1.5, 0.0),
        Instance::curve([72.0, 68.0], [82.0, 48.0], [92.0, 68.0], 1.5, GREEN),
        Instance::mask([100.0, 52.0, 116.0, 68.0], [16.0 / 64.0, 16.0 / 64.0, 24.0 / 64.0, 24.0 / 64.0], INK),
        Instance::image([4.0, 72.0, 20.0, 88.0], [0.0, 0.0, 4.0 / 32.0, 4.0 / 32.0], [255, 255, 255, 255]),
        Instance::rect([24.0, 72.0, 56.0, 92.0], GREEN).clip([30, 76, 50, 88], 4.0),
        Instance::rect([60.0, 72.0, 80.0, 92.0], BLUE).view(1),
    ]
}

fn render_scene(device: &Device, queue: &Queue) -> Vec<u8> {
    let mut painter = painter(device);
    let (glyph, checker) = atlas_bytes();
    upload_atlases(&mut painter, &glyph, &checker);
    ok(painter.set_view(1, View { offset: [8.0, 0.0], opacity: 0.5, ..View::IDENTITY }));
    let target = target(device);
    let instances = scene();
    painter.begin_frame();
    pass(queue, &target, &mut |encoder| ok(painter.draw(encoder, [W * SCALE, H * SCALE], SCALE as f32, &instances, &[])));
    assert_eq!(painter.draw_calls(), 1, "a layer without breaks is one draw call");
    let pixels = ok(be::read_texture(&target, (W * SCALE * 4) as i64));
    ok(be::destroy_texture(&target));
    pixels
}

fn finish(device: Device, queue: Queue) {
    ok(be::destroy_queue(&queue));
    ok(be::destroy_device(&device));
}

/// The pixel under logical point (x, y).
fn at(pixels: &[u8], x: f32, y: f32) -> [u8; 4] {
    let (px, py) = ((x * SCALE as f32) as usize, (y * SCALE as f32) as usize);
    let i = (py * (W * SCALE) as usize + px) * 4;
    pixels[i..i + 4].try_into().unwrap()
}

fn near(a: [u8; 4], b: [u8; 4], tolerance: i32) -> bool { a.iter().zip(b).all(|(x, y)| (*x as i32 - y as i32).abs() <= tolerance) }

/// Metal handles live on the process main thread, which libtest never gives a test: this target is
/// `harness = false` and speaks just enough of libtest's protocol (`--list`, names to filter).
fn main() {
    let cases: [(&str, fn()); 3] = [
        ("gpu_scene_golden_and_spot_checks", gpu_scene_golden_and_spot_checks),
        ("gpu_one_draw_per_layer", gpu_one_draw_per_layer),
        ("gpu_warm_frame_allocates_nothing", gpu_warm_frame_allocates_nothing),
    ];
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.iter().any(|a| a == "--ignored") { return; }
    if args.iter().any(|a| a == "--list") {
        for (name, _) in cases { println!("{name}: test"); }
        return;
    }
    let filters: Vec<&String> = args.iter().filter(|a| !a.starts_with("--")).collect();
    for (name, case) in cases {
        if !filters.is_empty() && !filters.iter().any(|f| *f == name) { continue; }
        if std::panic::catch_unwind(case).is_err() { std::process::exit(101); }
        println!("test {name} ... ok");
    }
}

fn gpu_scene_golden_and_spot_checks() {
    let Some(device) = open() else { return };
    let queue = ok(be::create_queue(&device));
    let pixels = render_scene(&device, &queue);
    finish(device, queue);

    assert_eq!(at(&pixels, 16.0, 12.0), RED, "solid fill centre");
    assert_eq!(at(&pixels, 46.0, 12.0), BLUE, "rounded rect centre");
    assert_eq!(at(&pixels, 59.6, 4.4), BG, "pixel inside the 4-point top-right corner is outside the shape");
    assert_eq!(at(&pixels, 32.2, 4.2), BLUE, "square top-left corner is filled");
    assert_eq!(at(&pixels, 78.0, 12.0), [250, 210, 60, 255], "bordered rect fill");
    assert_eq!(at(&pixels, 78.0, 4.6), INK, "inside border");
    assert_eq!(at(&pixels, 29.5, 80.0), BG, "clip excludes the rect left of it");
    assert_eq!(at(&pixels, 40.0, 82.0), GREEN, "inside the clip");
    assert_eq!(at(&pixels, 30.6, 76.4), BG, "rounded clip corner excludes");
    assert_eq!(at(&pixels, 64.0, 82.0), BG, "view offset moved the rect right");
    assert!(near(at(&pixels, 86.0, 82.0), [122, 142, 212, 255], 2), "view opacity halves the rect: {:?}", at(&pixels, 86.0, 82.0));
    assert!(near(at(&pixels, 108.0, 60.0), BG, 0) && near(at(&pixels, 101.0, 60.0), BG, 0), "glyph hole and margin are background");
    assert_eq!(at(&pixels, 104.6, 60.0), INK, "glyph ring is ink");
    assert_eq!(at(&pixels, 5.0, 73.0), [255, 255, 255, 255], "checker texel 0");
    assert_eq!(at(&pixels, 9.0, 73.0), [20, 40, 160, 255], "checker texel 1");

    let golden = format!("{}/../../fixtures/pxui_render/scene_{SCALE}x.png", env!("CARGO_MANIFEST_DIR"));
    if std::env::var("RAYS_UPDATE_FIXTURES").as_deref() == Ok("1") {
        std::fs::create_dir_all(std::path::Path::new(&golden).parent().unwrap()).unwrap();
        std::fs::write(&golden, png::encode(W * SCALE, H * SCALE, &pixels)).unwrap();
    }
    let (w, h, expected) = png::decode(&std::fs::read(&golden).expect("golden; refresh with RAYS_UPDATE_FIXTURES=1"));
    assert_eq!((w, h), (W * SCALE, H * SCALE), "golden size");
    let (mut different, mut first) = (0, None);
    for (i, (a, b)) in pixels.chunks(4).zip(expected.chunks(4)).enumerate() {
        // ponytail: 4 levels absorb Metal/Vulkan transcendental differences on edges; tighten after the Windows run.
        if !near(a.try_into().unwrap(), b.try_into().unwrap(), 4) {
            different += 1;
            first.get_or_insert((i as u32 % w, i as u32 / w));
        }
    }
    assert_eq!(different, 0, "{different} pixels differ from {golden}, first at {first:?}");
}

fn gpu_one_draw_per_layer() {
    let Some(device) = open() else { return };
    let queue = ok(be::create_queue(&device));
    let mut painter = painter(&device);
    let target = target(&device);
    let external = texture(&device, 4, 4, &[TextureUsage::TextureBinding, TextureUsage::TextureCopyDst]);
    ok(be::write_texture(&external, None, Origin { x: 0, y: 0, z: 0 }, Extent { width: 4, height: 4, depth: 1 }, 16, &[255; 64]));
    // A scroll list: every row clipped differently, rows of three kinds.
    let rows: Vec<Instance> = (0..300).map(|i| {
        let y = (i % 40) as f32 * 2.0;
        let clip = [0, (i % 7) as i16 * 4, 128, 96 - (i % 5) as i16 * 4];
        match i % 3 {
            0 => Instance::rect([2.0, y, 60.0, y + 2.0], RED).clip(clip, 2.0),
            1 => Instance::mask([62.0, y, 70.0, y + 2.0], [0.0, 0.0, 0.1, 0.1], INK).clip(clip, 0.0),
            _ => Instance::line([72.0, y], [120.0, y + 1.0], 1.0, BLUE).clip(clip, 0.0),
        }
    }).collect();
    painter.begin_frame();
    pass(&queue, &target, &mut |encoder| ok(painter.draw(encoder, [W * SCALE, H * SCALE], SCALE as f32, &rows, &[])));
    assert_eq!(painter.draw_calls(), 1, "clips do not split a layer");
    let breaks = [Break { at_instance: 100, texture: Some(&external) }, Break { at_instance: 200, texture: None }];
    painter.begin_frame();
    pass(&queue, &target, &mut |encoder| ok(painter.draw(encoder, [W * SCALE, H * SCALE], SCALE as f32, &rows, &breaks)));
    assert_eq!(painter.draw_calls(), 4, "two breaks make three draws");
    let bad = [Break { at_instance: 200, texture: None }, Break { at_instance: 100, texture: None }];
    pass(&queue, &target, &mut |encoder| assert!(painter.draw(encoder, [W * SCALE, H * SCALE], SCALE as f32, &rows, &bad).is_err(), "unordered breaks are refused"));
    assert!(painter.set_view(0, View::IDENTITY).is_err(), "view 0 stays the identity");
    for texture in [&target, &external] { ok(be::destroy_texture(texture)); }
    drop(painter);
    finish(device, queue);
}

fn gpu_warm_frame_allocates_nothing() {
    let Some(device) = open() else { return };
    let queue = ok(be::create_queue(&device));
    let mut painter = painter(&device);
    ok(painter.set_view(1, View { offset: [8.0, 0.0], opacity: 0.5, ..View::IDENTITY }));
    let (glyph, checker) = atlas_bytes();
    let target = target(&device);
    let instances = scene();
    let mut allocations = u64::MAX;
    for frame in 0..4 {
        painter.begin_frame();
        let commands = ok(be::begin_commands(&queue));
        let encoder = ok(be::render_encoder(&commands, &RenderTarget {
            colors: vec![ColorAttachment { texture: target.clone(), resolve: None, load: Load::Clear, store: Store::Store, clear: (0.0, 0.0, 0.0, 1.0) }],
            depth: None,
        }));
        mem_sys::local_reset();
        upload_atlases(&mut painter, &glyph, &checker);
        ok(painter.draw(&encoder, [W * SCALE, H * SCALE], SCALE as f32, &instances, &[]));
        if frame == 3 { allocations = mem_sys::local_alloc_count(); }
        ok(be::end_render(&encoder));
        ok(be::complete_through(&queue, ok(be::commit(&commands)).epoch));
    }
    assert_eq!(allocations, 0, "a warm upload + draw allocates nothing");
    ok(be::destroy_texture(&target));
    drop(painter);
    finish(device, queue);
}

/// PNG with stored (uncompressed) deflate blocks: viewable anywhere, no codec dependency.
mod png {
    fn crc(bytes: &[u8]) -> u32 {
        let mut c = !0u32;
        for b in bytes {
            c ^= *b as u32;
            for _ in 0..8 { c = if c & 1 != 0 { 0xedb8_8320 ^ (c >> 1) } else { c >> 1 }; }
        }
        !c
    }
    fn chunk(out: &mut Vec<u8>, kind: &[u8; 4], data: &[u8]) {
        out.extend((data.len() as u32).to_be_bytes());
        let start = out.len();
        out.extend(kind);
        out.extend(data);
        let c = crc(&out[start..]);
        out.extend(c.to_be_bytes());
    }
    pub fn encode(w: u32, h: u32, rgba: &[u8]) -> Vec<u8> {
        let raw: Vec<u8> = rgba.chunks(w as usize * 4).flat_map(|row| std::iter::once(0).chain(row.iter().copied())).collect();
        let mut z = vec![0x78, 0x01];
        let blocks: Vec<&[u8]> = raw.chunks(65535).collect();
        for (i, block) in blocks.iter().enumerate() {
            z.push((i + 1 == blocks.len()) as u8);
            z.extend((block.len() as u16).to_le_bytes());
            z.extend((!(block.len() as u16)).to_le_bytes());
            z.extend(*block);
        }
        let (mut a, mut b) = (1u32, 0u32);
        for x in &raw { a = (a + *x as u32) % 65521; b = (b + a) % 65521; }
        z.extend((b << 16 | a).to_be_bytes());
        let mut out = b"\x89PNG\r\n\x1a\n".to_vec();
        let mut header = Vec::new();
        header.extend(w.to_be_bytes());
        header.extend(h.to_be_bytes());
        header.extend([8, 6, 0, 0, 0]);
        chunk(&mut out, b"IHDR", &header);
        chunk(&mut out, b"IDAT", &z);
        chunk(&mut out, b"IEND", &[]);
        out
    }
    /// Decodes only what `encode` writes.
    pub fn decode(png: &[u8]) -> (u32, u32, Vec<u8>) {
        let (mut at, mut size, mut z) = (8, (0, 0), Vec::new());
        while at + 8 <= png.len() {
            let length = u32::from_be_bytes(png[at..at + 4].try_into().unwrap()) as usize;
            let data = &png[at + 8..at + 8 + length];
            match &png[at + 4..at + 8] {
                b"IHDR" => size = (u32::from_be_bytes(data[..4].try_into().unwrap()), u32::from_be_bytes(data[4..8].try_into().unwrap())),
                b"IDAT" => z.extend(data),
                _ => {}
            }
            at += 12 + length;
        }
        let (mut raw, mut at) = (Vec::new(), 2);
        loop {
            let last = z[at] & 1 == 1;
            let length = u16::from_le_bytes([z[at + 1], z[at + 2]]) as usize;
            raw.extend(&z[at + 5..at + 5 + length]);
            at += 5 + length;
            if last { break; }
        }
        let rgba = raw.chunks(size.0 as usize * 4 + 1).flat_map(|row| { assert_eq!(row[0], 0, "unfiltered rows only"); row[1..].to_vec() }).collect();
        (size.0, size.1, rgba)
    }
}
