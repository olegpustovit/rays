// Port of test_gpu_image.ml over ogpu_mock: storage-free publication, checked source lifetime and
// resize, CPU transitions, leases, domain and borrowed ownership. The `Gc.allocated_bytes`
// ceilings are not ported (they measure the OCaml heap); zero CPU storage is checked separately.
mod common;
use ogpu_core::backend::{self as b, Driver, DriverDevice, DriverResource, Texture};
use ogpu_core::error::{self, ErrorKind};
use ogpu_core::types::{TextureDescriptor, TextureFormat, TextureUsage};
use runtime_resources::{canvas, image as I, error as Error};
use std::cell::{Cell, RefCell};
use std::rc::Rc;

type State = (Rc<Cell<i64>>, Rc<RefCell<Option<Error>>>, Vec<u8>);
struct Counted { inner: Box<dyn DriverResource>, reads: Rc<Cell<i64>>, error: Rc<RefCell<Option<Error>>> }
impl Counted {
    fn gate(&self) -> Result<(), ogpu_core::error::Error> {
        self.reads.set(self.reads.get() + 1);
        match self.error.borrow().clone() { Some(error) => Err(error), None => Ok(()) }
    }
}
impl DriverResource for Counted {
    fn token(&self) -> b::Token { self.inner.token() }
    fn write(&self, offset: i64, bytes: &[u8]) -> Result<(), ogpu_core::error::Error> { self.inner.write(offset, bytes) }
    fn read(&self, offset: i64, length: usize) -> Result<Vec<u8>, ogpu_core::error::Error> { self.gate()?; self.inner.read(offset, length) }
    fn read_into(&self, offset: i64, destination: &mut [u8], destination_offset: usize, length: usize) -> Result<(), ogpu_core::error::Error> {
        self.gate()?;
        self.inner.read_into(offset, destination, destination_offset, length)
    }
    fn destroy(&self) -> Result<(), ogpu_core::error::Error> { self.inner.destroy() }
}
struct Dev { inner: Box<dyn DriverDevice>, state: Rc<State> }
macro_rules! forward {
    ($($name:ident($($arg:ident: $ty:ty),*) -> $ret:ty;)*) => {
        $(fn $name(&self, $($arg: $ty),*) -> $ret { self.inner.$name($($arg),*) })*
    };
}
impl DriverDevice for Dev {
    forward! {
        device_handle() -> ogpu_core::handle::Device;
        capabilities() -> ogpu_core::caps::Caps;
        allocated_size() -> i64;
        recommended_working_set() -> i64;
        create_buffer(memory: ogpu_core::types::Memory, descriptor: &ogpu_core::types::BufferDescriptor) -> Result<Box<dyn DriverResource>, ogpu_core::error::Error>;
        create_mapped_buffer(descriptor: &ogpu_core::types::BufferDescriptor) -> Result<b::DriverMapped, ogpu_core::error::Error>;
        create_depth_texture(descriptor: &TextureDescriptor) -> Result<Box<dyn DriverResource>, ogpu_core::error::Error>;
        create_memoryless_texture(depth: bool, descriptor: &TextureDescriptor) -> Result<Box<dyn DriverResource>, ogpu_core::error::Error>;
        create_library(shader: &ogpu_core::shader::Shader) -> Result<Box<dyn b::DriverLibrary>, ogpu_core::error::Error>;
        warm_library(shader: &ogpu_core::shader::Shader) -> Option<b::Warm>;
        create_accel(descriptor: &b::DriverAccelDescriptor) -> Result<Box<dyn b::DriverAccel>, ogpu_core::error::Error>;
        instance_layout(kind: b::InstanceKind) -> Vec<i64>;
        create_sampler(descriptor: &ogpu_core::types::SamplerDescriptor) -> Result<Box<dyn b::DriverSampler>, ogpu_core::error::Error>;
        create_render_pipeline(options: b::RenderPipelineOptions, descriptor: &ogpu_core::pipeline::RenderDescriptor) -> Result<Box<dyn b::DriverPipeline>, ogpu_core::error::Error>;
        create_icb(max_commands: i64) -> Result<Box<dyn b::DriverIcb>, ogpu_core::error::Error>;
        create_argument(pipeline: b::Token, stage: b::ShaderStage, index: i64) -> Result<Box<dyn b::DriverArgument>, ogpu_core::error::Error>;
        create_queue() -> Result<Box<dyn b::DriverQueue>, ogpu_core::error::Error>;
        create_surface(configuration: &ogpu_core::surface::Configuration) -> Result<Box<dyn b::DriverSurface>, ogpu_core::error::Error>;
        destroy_device() -> Result<(), ogpu_core::error::Error>;
    }
    fn create_texture(&self, descriptor: &TextureDescriptor) -> Result<Box<dyn DriverResource>, ogpu_core::error::Error> {
        let resource = self.inner.create_texture(descriptor)?;
        if descriptor.width == 65 && descriptor.height == 3 {
            resource.write(0, &self.state.2)?;
        }
        Ok(Box::new(Counted { inner: resource, reads: self.state.0.clone(), error: self.state.1.clone() }))
    }
}
struct Wrap { base: Box<dyn Driver>, state: Rc<State> }
impl Driver for Wrap {
    fn create_device(&self) -> Result<Box<dyn DriverDevice>, ogpu_core::error::Error> {
        Ok(Box::new(Dev { inner: self.base.create_device()?, state: self.state.clone() }))
    }
}

fn gpu_image() {
    let base = ogpu_mock::impl_::create_driver();
    let reads = Rc::new(Cell::new(0i64));
    let read_error: Rc<RefCell<Option<Error>>> = Rc::new(RefCell::new(None));
    let bytes: Vec<u8> = (0..65 * 3 * 4).map(|i| (i % 256) as u8).collect();
    let driver: Box<dyn Driver> = Box::new(Wrap { base, state: Rc::new((reads.clone(), read_error.clone(), bytes.clone())) });
    let device = b::create_device(driver).unwrap();
    let descriptor = TextureDescriptor {
        label: None,
        width: 65,
        height: 3,
        depth: 1,
        mip_levels: 1,
        sample_count: 1,
        format: TextureFormat::Rgba8Unorm,
        usage: vec![TextureUsage::TextureBinding, TextureUsage::TextureCopySrc, TextureUsage::TextureCopyDst],
    };
    let textures: RefCell<Vec<Texture>> = RefCell::new(Vec::new());
    let texture = |descriptor: &TextureDescriptor| {
        let t = b::create_texture(&device, descriptor.clone()).unwrap();
        textures.borrow_mut().push(t.clone());
        t
    };
    let same = |a: &Texture, c: &Texture| b::texture_id(a) == b::texture_id(c);
    let first = texture(&descriptor);
    let second = texture(&descriptor);
    let resized = texture(&TextureDescriptor { width: 17, height: 5, ..descriptor.clone() });
    let source: Rc<RefCell<Option<Texture>>> = Rc::new(RefCell::new(Some(first.clone())));
    let from = |source: &Rc<RefCell<Option<Texture>>>| -> I::gpu_source_fn {
        let source = source.clone();
        Rc::new(move || source.borrow().clone())
    };
    let image = I::of_gpu(65, 3, from(&source)).unwrap();
    assert!(I::cpu_storage_bytes(&image) == 0 && I::readbacks(&image) == 0 && reads.get() == 0);
    let identity = I::identity(&image);
    let generation = I::generation(&image);
    let snapshot = |image: &I::Image| {
        let (w, h, g, t) = I::gpu_snapshot(image).unwrap().expect("a GPU snapshot");
        let (sw, sh) = I::size(image).unwrap();
        assert_eq!((w, h, g), (sw, sh, I::generation(image)));
        t
    };
    assert!(same(&snapshot(&image), &first) && reads.get() == 0);
    assert!(I::pixels(&image).unwrap() == bytes && reads.get() == 1 && I::readbacks(&image) == 1);
    let (_, _, _, saved, mut lease) = I::borrow_snapshot(&image).unwrap();
    assert!(*saved == bytes && reads.get() == 2);
    *source.borrow_mut() = Some(second.clone());
    assert!(I::gpu_snapshot(&image).is_err() && I::pixels(&image).is_err());
    assert!(reads.get() == 2 && I::generation(&image) == generation);
    I::replace_gpu_source(&image, 65, 3, from(&source)).unwrap();
    assert!(I::identity(&image) == identity && I::generation(&image) == generation + 1 && same(&snapshot(&image), &second));
    *source.borrow_mut() = None;
    assert!(I::gpu_snapshot(&image).is_err());
    assert!(I::borrow_snapshot(&image).is_err());
    assert!(I::replace_gpu_source(&image, 65, 3, from(&source)).is_err());
    assert!(I::generation(&image) == generation + 1 && reads.get() == 2);
    let resized_source: Rc<RefCell<Option<Texture>>> = Rc::new(RefCell::new(Some(resized.clone())));
    I::replace_gpu_source(&image, 17, 5, from(&resized_source)).unwrap();
    assert!(I::identity(&image) == identity && I::size(&image).unwrap() == (17, 5) && I::generation(&image) == generation + 2);
    assert!(same(&snapshot(&image), &resized) && *saved == bytes && I::cpu_storage_bytes(&image) == 0);
    I::release_snapshot(&mut lease);
    I::release_snapshot(&mut lease);
    assert_eq!(I::cpu_storage_bytes(&image), 0);
    let first_source = Rc::new(RefCell::new(Some(first.clone())));
    for (w, h) in [(0, 3), (65, 0), (i64::MAX >> 1, 3), (65, i64::MAX >> 1), (17, 5)] {
        assert!(I::of_gpu(w, h, from(&first_source)).is_err());
    }
    let bad_shapes = [
        TextureDescriptor { format: TextureFormat::Rgba16Float, ..descriptor.clone() },
        TextureDescriptor { format: TextureFormat::Rgba32Float, ..descriptor.clone() },
        TextureDescriptor { depth: 2, ..descriptor.clone() },
        TextureDescriptor { sample_count: 4, ..descriptor.clone() },
        TextureDescriptor { usage: vec![TextureUsage::TextureCopySrc], ..descriptor.clone() },
        TextureDescriptor { usage: vec![TextureUsage::TextureBinding], ..descriptor.clone() },
    ];
    for shape in &bad_shapes {
        let bad = Rc::new(RefCell::new(Some(texture(shape))));
        assert!(I::of_gpu(65, 3, from(&bad)).is_err());
        assert!(I::replace_gpu_source(&image, 65, 3, from(&bad)).is_err());
        assert!(I::size(&image).unwrap() == (17, 5) && I::generation(&image) == generation + 2 && same(&snapshot(&image), &resized));
    }
    // Resource operations belong to the initial SDL domain.
    let off = std::thread::spawn(|| {
        let r = I::of_gpu(65, 3, Rc::new(|| unreachable!()));
        matches!(r, Err(e) if e.kind == ErrorKind::WrongDomain)
    });
    assert!(off.join().unwrap());
    let cpu = vec![0x31u8; 17 * 5 * 4];
    I::replace(&image, 17, 5, &cpu).unwrap();
    assert!(I::gpu_snapshot(&image).unwrap().is_none() && I::pixels(&image).unwrap() == cpu && reads.get() == 2);
    let (_, _, _, cpu_saved, mut cpu_lease) = I::borrow_snapshot(&image).unwrap();
    I::replace_gpu_source(&image, 17, 5, from(&resized_source)).unwrap();
    assert!(*cpu_saved == cpu && I::cpu_storage_bytes(&image) == 0);
    I::release_snapshot(&mut cpu_lease);
    assert_eq!(I::cpu_storage_bytes(&image), 0);
    I::replace_gpu_source(&image, 17, 5, from(&resized_source)).unwrap();
    b::destroy_texture(&resized).unwrap();
    assert!(I::gpu_snapshot(&image).is_err() && I::pixels(&image).is_err());
    assert_eq!(reads.get(), 2);
    I::destroy(&image).unwrap();
    I::destroy(&image).unwrap();
    assert!(!b::texture_destroyed(&first) && !b::texture_destroyed(&second));
    let large = texture(&TextureDescriptor { width: 1024, height: 1024, ..descriptor.clone() });
    let large_source = Rc::new(RefCell::new(Some(large.clone())));
    let large_image = I::of_gpu(1024, 1024, from(&large_source)).unwrap();
    assert!(I::cpu_storage_bytes(&large_image) == 0 && reads.get() == 2);
    I::destroy(&large_image).unwrap();
    let backend_error = error::make("driver.read_texture", ErrorKind::DeviceLost, "injected readback failure");
    *read_error.borrow_mut() = Some(backend_error.clone());
    let failed_image = I::of_gpu(1024, 1024, from(&large_source)).unwrap();
    assert_eq!(I::pixels(&failed_image).err(), Some(backend_error.clone()), "Image.pixels changed the backend readback error");
    let failed_canvas = canvas::create(1024, 1024).unwrap();
    canvas::publish_gpu(&failed_canvas, &large).unwrap();
    assert_eq!(canvas::snapshot(&failed_canvas).err(), Some(backend_error), "Canvas.snapshot changed the backend readback error");
    *read_error.borrow_mut() = None;
    canvas::destroy(&failed_canvas).unwrap();
    I::destroy(&failed_image).unwrap();
    let destroyed = I::create(1, 1, &[0x5a; 4]).unwrap();
    let (_, _, _, saved, mut lease) = I::borrow_snapshot(&destroyed).unwrap();
    I::destroy(&destroyed).unwrap();
    I::release_snapshot(&mut lease);
    I::release_snapshot(&mut lease);
    assert!(*saved == [0x5a; 4] && I::cpu_storage_bytes(&destroyed) == 0);
    for t in textures.borrow().iter() {
        let _ = b::destroy_texture(t);
    }
    b::destroy_device(&device).unwrap();
    assert_eq!(ogpu_mock::impl_::live_handles(), 0);
}

fn main() {
    common::run(&[("gpu_image", gpu_image)]);
}
