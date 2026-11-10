//! Headless rendering for PXUI's own tests: a device on the platform backend (Metal on macOS,
//! fatal-validation Vulkan elsewhere), the UI's layers drawn by pxui_render's painter into an
//! offscreen texture and read back. No window, no SDL.
use ogpu_core::backend::{self as be, ColorAttachment, Device, Queue, RenderTarget};
use ogpu_core::error::{self, Error};
use ogpu_core::pipeline::{ColorFormat, DepthFormat};
use ogpu_core::render_pass::{Load, Store};
use ogpu_core::types::{TextureDescriptor, TextureFormat, TextureUsage};
use pxui::text::AtlasKind as TextKind;
use pxui::ui::Ui;
use pxui_render::{AtlasKind, Break, Painter};
use std::cell::OnceCell;

fn ok<T>(result: Result<T, Error>) -> T {
    result.unwrap_or_else(|e| panic!("{}", error::to_string(&e)))
}

thread_local! {
    /// One device per process, opened on first use and kept for the process (cases share it).
    static DEVICE: OnceCell<Option<(Device, Queue)>> = const { OnceCell::new() };
}

fn open() -> Option<(Device, Queue)> {
    #[cfg(any(windows, target_os = "linux"))]
    ogpu_core::impl_::select(ogpu_core::impl_::Impl {
        backend: ogpu_vulkan::impl_::BACKEND,
        create_driver: || ogpu_vulkan::backend::create_with_validation((true, true)),
        live_handles: ogpu_vulkan::impl_::live_handles,
    });
    #[cfg(target_os = "macos")]
    ogpu_core::impl_::select(ogpu_metal::impl_::implementation());
    if !cfg!(any(windows, target_os = "linux", target_os = "macos")) {
        println!("RAYS_TEST_SKIP: no native OGPU implementation on this platform");
        assert!(std::env::var("RAYS_ALLOW_SKIP").as_deref() == Ok("1"), "RAYS_ALLOW_SKIP=1 explicitly permits skipping");
        return None;
    }
    let device = ok(be::create_device(ok(ogpu_core::impl_::create_driver())));
    let queue = ok(be::create_queue(&device));
    Some((device, queue))
}

fn device() -> Option<(Device, Queue)> {
    DEVICE.with(|d| d.get_or_init(open).clone())
}

/// Releases the shared device before the process exits (Vulkan crashes on exit with a live device).
pub fn finish() {
    DEVICE.with(|d| if let Some(Some((device, queue))) = d.get() {
        ok(be::destroy_queue(queue));
        ok(be::destroy_device(device));
    });
}

/// Whether this platform has a GPU backend to render on.
pub fn available() -> bool {
    device().is_some()
}

/// The UI's last frame drawn over `ground` into a `size` x `density` texture: its RGBA pixels and
/// the painter's draw calls.
pub fn render(ui: &mut Ui, size: (u32, u32), density: u32, ground: [u8; 4]) -> (Vec<u8>, u64) {
    let (device, queue) = device().expect("a GPU backend");
    let text = ui.text_mut();
    let (mask, color) = (text.atlas_size(TextKind::Mask), text.atlas_size(TextKind::Color));
    let mut painter = ok(Painter::new(&device, ColorFormat::Rgba8Unorm, DepthFormat::NoDepth, 1, mask, color));
    for (kind, atlas, texel, (w, h)) in [(TextKind::Mask, AtlasKind::Mask, 1, mask), (TextKind::Color, AtlasKind::Color, 4, color)] {
        ok(painter.upload(atlas, 0, 0, w, h, text.atlas_pixels(kind), (w * texel) as usize));
    }
    text.drain_uploads(&mut |_, _, _, _, _, _, _| {});
    let (w, h) = (size.0 * density, size.1 * density);
    let target = ok(be::create_texture(&device, TextureDescriptor {
        label: Some("pxui.test".into()), width: w as i64, height: h as i64, depth: 1, mip_levels: 1, sample_count: 1,
        format: TextureFormat::Rgba8Unorm, usage: vec![TextureUsage::RenderAttachment, TextureUsage::TextureCopySrc],
    }));
    let commands = ok(be::begin_commands(&queue));
    let clear = (ground[0] as f64 / 255.0, ground[1] as f64 / 255.0, ground[2] as f64 / 255.0, ground[3] as f64 / 255.0);
    let encoder = ok(be::render_encoder(&commands, &RenderTarget {
        colors: vec![ColorAttachment { texture: target.clone(), resolve: None, load: Load::Clear, store: Store::Store, clear }],
        depth: None,
    }));
    painter.begin_frame();
    for layer in ui.output().draw {
        // ponytail: PXUI's own tests draw no host images; a break back to the colour atlas is all
        let breaks: Vec<Break> = layer.breaks.iter().map(|b| {
            assert_eq!(b.image, pxui::draw::ATLAS, "host images need a host");
            Break { at_instance: b.at_instance, texture: None }
        }).collect();
        ok(painter.draw(&encoder, [w, h], density as f32, &layer.instances, &breaks));
    }
    ok(be::end_render(&encoder));
    ok(be::complete_through(&queue, ok(be::commit(&commands)).epoch));
    let pixels = ok(be::read_texture(&target, (w * 4) as i64));
    ok(be::destroy_texture(&target));
    (pixels, painter.draw_calls())
}
