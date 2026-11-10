//! The host loop's GPU half, shared by the example and the compositor test: a Vulkan swapchain on the window's
//! token, and the UI painter drawing into an image that the present blits to the swapchain.
use ogpu_core::backend::{self as be, AcquireResult, ColorAttachment, Device, Driver, Queue, RenderTarget, Surface, Texture};
use ogpu_core::error::{self, Error};
use ogpu_core::pipeline::{ColorFormat, DepthFormat};
use ogpu_core::render_pass::{Load, Store};
use ogpu_core::surface::{Configuration, Format, PresentMode};
use ogpu_core::types::{TextureDescriptor, TextureFormat, TextureUsage};
use pxui_render::{Instance, Painter};
use pxui_wayland::{Extent, Window};

pub fn ok<T>(result: Result<T, Error>) -> T {
    result.unwrap_or_else(|e| panic!("{}", error::to_string(&e)))
}

pub struct Gpu {
    device: Device,
    queue: Queue,
    surface: Surface,
    target: Texture,
    painter: Option<Painter>,
    extent: Extent,
}

fn configuration(window: &mut Window) -> Configuration {
    let extent = window.extent();
    Configuration {
        logical_width: extent.logical.0,
        logical_height: extent.logical.1,
        physical_width: extent.drawable.0,
        physical_height: extent.drawable.1,
        format: Format::Bgra8Unorm,
        present_mode: if window.vsync() { PresentMode::Fifo } else { PresentMode::Immediate },
        max_acquired: 1,
        layer: Some(window.token().unwrap()),
    }
}

/// What the painter draws into; the present copies it to the swapchain image.
fn target(device: &Device, (width, height): (i64, i64)) -> Texture {
    ok(be::create_texture(device, TextureDescriptor {
        label: Some("pxui_wayland.target".into()), width, height, depth: 1, mip_levels: 1, sample_count: 1,
        format: TextureFormat::Rgba8Unorm, usage: vec![TextureUsage::RenderAttachment, TextureUsage::TextureBinding],
    }))
}

impl Gpu {
    pub fn open(window: &mut Window, driver: fn() -> Box<dyn Driver>) -> Gpu {
        ogpu_core::impl_::select(ogpu_core::impl_::Impl {
            backend: ogpu_vulkan::impl_::BACKEND,
            create_driver: driver,
            live_handles: ogpu_vulkan::impl_::live_handles,
        });
        let device = ok(be::create_device(ok(ogpu_core::impl_::create_driver())));
        let queue = ok(be::create_queue(&device));
        let surface = ok(be::create_surface(&device, configuration(window)));
        let target = target(&device, window.extent().drawable);
        let painter = ok(Painter::new(&device, ColorFormat::Rgba8Unorm, DepthFormat::NoDepth, 1, (64, 64), (64, 64)));
        Gpu { device, queue, surface, target, painter: Some(painter), extent: window.extent() }
    }

    /// Draws `instances` (logical points) and presents; false when the compositor gave no image this time.
    pub fn frame(&mut self, window: &mut Window, instances: &[Instance]) -> bool {
        if window.extent() != self.extent {
            self.extent = window.extent();
            ok(be::configure(&self.surface, configuration(window)));
            ok(be::destroy_texture(&self.target));
            self.target = target(&self.device, self.extent.drawable);
        }
        let AcquireResult::Acquired(frame) = ok(be::acquire_sync(&self.surface)) else { return false };
        let painter = self.painter.as_mut().unwrap();
        painter.begin_frame();
        let commands = ok(be::begin_commands(&self.queue));
        let encoder = ok(be::render_encoder(&commands, &RenderTarget {
            colors: vec![ColorAttachment { texture: self.target.clone(), resolve: None, load: Load::Clear, store: Store::Store, clear: (0.12, 0.12, 0.14, 1.0) }],
            depth: None,
        }));
        let size = [self.extent.drawable.0 as u32, self.extent.drawable.1 as u32];
        ok(painter.draw(&encoder, size, window.scale() as f32, instances, &[]));
        ok(be::end_render(&encoder));
        let receipt = ok(be::commit_present(&commands, &self.target, &frame));
        ok(be::complete_through(&self.queue, receipt.epoch));
        true
    }

    /// Releases the GPU side; the window (whose token the surface uses) must outlive this.
    pub fn close(mut self) {
        drop(self.painter.take());
        ok(be::destroy_texture(&self.target));
        ok(be::destroy_surface(&self.surface));
        ok(be::destroy_queue(&self.queue));
        ok(be::destroy_device(&self.device));
    }
}
