use crate::native::resource::{enter_main, error, sys_error, CommandBuffer, Device, Error, ErrorKind, Lifetime, PresentTime, Texture};
use ogpu_core::types::TextureFormat as Format;
use native_layer_token_sys::portable::Token;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;


#[derive(Clone, Copy, Debug)]
pub struct Configuration {
    pub width: usize,
    pub height: usize,
    pub format: Format,
    pub framebuffer_only: bool,
    pub maximum_drawables: usize,
    pub allows_timeout: bool,
    pub display_sync: bool,
    pub presents_with_transaction: bool,
}

impl Configuration {
    pub fn default(width: usize, height: usize) -> Self {
        Self {
            width,
            height,
            format: Format::Bgra8Unorm,
            framebuffer_only: false,
            maximum_drawables: 3,
            allows_timeout: true,
            display_sync: false,
            presents_with_transaction: false,
        }
    }

    fn validate(self, operation: &str) -> Result<(), Error> {
        if self.width == 0 || self.height == 0 || !(2..=3).contains(&self.maximum_drawables) {
            return Err(error(operation, ErrorKind::InvalidArgument, "invalid drawable size or maximum count"));
        }
        if !matches!(self.format, Format::Bgra8Unorm | Format::Bgra8UnormSrgb | Format::Rgba16Float) {
            return Err(error(operation, ErrorKind::Unsupported, "pixel format is not supported by CAMetalLayer"));
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Loss {
    TimeoutOrUnavailable,
}

pub struct Surface(Arc<SurfaceInner>);

struct SurfaceInner {
    raw: metal_sys::Handle,
    device: Device,
    lifetime: Lifetime,
    device_attached: AtomicBool,
}

impl Drop for SurfaceInner {
    fn drop(&mut self) {
        if self.device_attached.swap(false, Ordering::AcqRel) {
            self.device.detach_child();
        }
    }
}

impl Surface {
    pub fn adopt_borrowed(
        device: &Device,
        token: &Token,
        owner_id: i64,
        generation: i64,
        configuration: Configuration,
    ) -> Result<Self, Error> {
        let operation = "Metal.Surface.adopt_borrowed";
        enter_main(operation)?;
        device.ensure_live(operation)?;
        configuration.validate(operation)?;
        let layer = token.borrow_raw(owner_id, generation).ok_or_else(|| {
            error(operation, ErrorKind::StaleHandle, "native layer token is stale or belongs to another owner")
        })?;
        let raw = metal_sys::native::surface::adopt_borrowed_layer(device.raw(), layer).map_err(|failure| sys_error(operation, failure))?;
        if let Err(failure) = configure_raw(&raw, configuration).map_err(|failure| sys_error(operation, failure)) {
            drop(raw);
            return Err(failure);
        }
        device.attach_child(operation)?;
        Ok(Self(Arc::new(SurfaceInner {
            raw,
            device: device.clone(),
            lifetime: Lifetime::new(),
            device_attached: AtomicBool::new(true),
        })))
    }

    pub fn device(&self) -> &Device {
        &self.0.device
    }

    pub fn configure(&self, configuration: Configuration) -> Result<(), Error> {
        let operation = "Metal.Surface.configure";
        self.ensure_live(operation)?;
        configuration.validate(operation)?;
        configure_raw(&self.0.raw, configuration).map_err(|failure| sys_error(operation, failure))
    }

    pub fn acquire(&self) -> Result<Result<Drawable, Loss>, Error> {
        let operation = "Metal.Surface.acquire";
        enter_main(operation)?;
        self.ensure_live(operation)?;
        let Some(raw) = metal_sys::native::surface::next_drawable(&self.0.raw).map_err(|failure| sys_error(operation, failure))? else {
            return Ok(Err(Loss::TimeoutOrUnavailable));
        };
        self.0.lifetime.attach();
        Ok(Ok(Drawable(Arc::new(DrawableInner {
            raw,
            surface: self.0.clone(),
            lifetime: Lifetime::new(),
            presentation_scheduled: AtomicBool::new(false),
            surface_attached: AtomicBool::new(true),
        }))))
    }

    pub fn destroy(&self) -> Result<(), Error> {
        let operation = "Metal.Surface.destroy";
        self.0.lifetime.destroy_parent(operation)?;
        if self.0.device_attached.swap(false, Ordering::AcqRel) {
            self.0.device.detach_child();
        }
        Ok(())
    }

    fn ensure_live(&self, operation: &str) -> Result<(), Error> {
        ensure_surface_live(&self.0, operation)
    }
}

struct DrawableInner {
    raw: metal_sys::Handle,
    surface: Arc<SurfaceInner>,
    lifetime: Lifetime,
    presentation_scheduled: AtomicBool,
    surface_attached: AtomicBool,
}

impl Drop for DrawableInner {
    fn drop(&mut self) {
        if self.surface_attached.swap(false, Ordering::AcqRel) {
            self.surface.lifetime.detach();
        }
    }
}

#[derive(Clone)]
pub struct Drawable(Arc<DrawableInner>);

impl Drawable {
    pub fn texture(&self) -> Result<Texture, Error> {
        let operation = "Metal.Drawable.texture";
        enter_main(operation)?;
        self.ensure_live(operation)?;
        let (raw, width, height, code) = metal_sys::native::surface::drawable_texture(&self.0.raw).map_err(|failure| sys_error(operation, failure))?;
        let format = match code {
            80 => Format::Bgra8Unorm,
            81 => Format::Bgra8UnormSrgb,
            115 => Format::Rgba16Float,
            _ => return Err(error(operation, ErrorKind::Unsupported, "drawable returned an unsupported pixel format")),
        };
        let texture = Texture::from_drawable(
            self.0.surface.device.clone(),
            raw,
            width,
            height,
            format,
            self.dependent_counter(),
            self.keepalive(),
        )?;
        Ok(texture)
    }

    pub fn present(&self, command_buffer: &CommandBuffer, time: PresentTime) -> Result<(), Error> {
        command_buffer.present(self, time)
    }

    pub fn destroy(&self) -> Result<(), Error> {
        let operation = "Metal.Drawable.destroy";
        self.0.lifetime.destroy_parent(operation)?;
        if self.0.surface_attached.swap(false, Ordering::AcqRel) {
            self.0.surface.lifetime.detach();
        }
        Ok(())
    }

    pub(crate) fn ensure_live(&self, operation: &str) -> Result<(), Error> {
        self.0.lifetime.ensure_live(operation)?;
        ensure_surface_live(&self.0.surface, operation)
    }

    pub(crate) fn is_presentation_scheduled(&self) -> bool {
        self.0.presentation_scheduled.load(Ordering::Acquire)
    }
    pub(crate) fn mark_presentation_scheduled(&self) {
        self.0.presentation_scheduled.store(true, Ordering::Release);
    }
    pub(crate) fn raw(&self) -> &metal_sys::Handle { &self.0.raw }
    pub(crate) fn device(&self) -> &Device { &self.0.surface.device }
    pub(crate) fn dependent_counter(&self) -> Arc<AtomicUsize> {
        self.0.lifetime.dependent_counter()
    }
    pub(crate) fn keepalive(&self) -> Arc<dyn std::any::Any> {
        self.0.clone()
    }
}

fn ensure_surface_live(surface: &SurfaceInner, operation: &str) -> Result<(), Error> {
    surface.lifetime.ensure_live(operation)?;
    surface.device.ensure_live(operation)
}

fn configure_raw(layer: &metal_sys::Handle, configuration: Configuration) -> Result<(), metal_sys::Error> {
    let width = u64::try_from(configuration.width).map_err(|_| metal_sys::Error::InvalidArgument("drawable width is too large"))?;
    let height = u64::try_from(configuration.height).map_err(|_| metal_sys::Error::InvalidArgument("drawable height is too large"))?;
    let maximum_drawables = u64::try_from(configuration.maximum_drawables).map_err(|_| metal_sys::Error::InvalidArgument("maximum drawable count is too large"))?;
    metal_sys::native::surface::configure_layer(
        layer,
        width,
        height,
        crate::native::resource::pixel_format_code(configuration.format) as i64,
        configuration.framebuffer_only,
        maximum_drawables,
        configuration.allows_timeout,
        configuration.display_sync,
        configuration.presents_with_transaction,
    )
}
