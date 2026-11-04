// Port of test_gpu_image.ml over ogpu_mock: storage-free publication, checked source lifetime and
// resize, CPU transitions, leases, domain and borrowed ownership. The `Gc.allocated_bytes`
// ceilings are not ported (they measure the OCaml heap); `cpu_storage_bytes = 0` states the same.
mod common;
use ogpu::backend::{self as b, Driver, DriverResource, Texture};
use ogpu::error::{self, ErrorKind};
use ogpu::types::{TextureDescriptor, TextureFormat, TextureUsage};
use runtime_resources::{Canvas, Image as I, error as Error};
use std::cell::{Cell, RefCell};
use std::rc::Rc;

fn gpu_image() {
    let base = ogpu_mock::impl_::create_driver();
    let reads = Rc::new(Cell::new(0i64));
    let read_error: Rc<RefCell<Option<Error>>> = Rc::new(RefCell::new(None));
    let bytes: Vec<u8> = (0..65 * 3 * 4).map(|i| (i % 256) as u8).collect();
    let driver = {
        let (reads, read_error, bytes) = (reads.clone(), read_error.clone(), bytes.clone());
        Driver {
            create_device: Box::new(move || {
                let raw = (base.create_device)()?;
                let create = raw.create_texture;
                let (reads, read_error, bytes) = (reads.clone(), read_error.clone(), bytes.clone());
                Ok(b::DriverDevice {
                    create_texture: Box::new(move |descriptor| {
                        let resource = create(descriptor)?;
                        if descriptor.width == 65 && descriptor.height == 3 {
                            (resource.write)(0, &bytes)?;
                        }
                        let DriverResource { token, write, read, read_into, destroy } = resource;
                        let (r1, e1) = (reads.clone(), read_error.clone());
                        let (r2, e2) = (reads.clone(), read_error.clone());
                        Ok(DriverResource {
                            token,
                            write,
                            destroy,
                            read: Box::new(move |offset, length| {
                                r1.set(r1.get() + 1);
                                match e1.borrow().clone() {
                                    Some(error) => Err(error),
                                    None => read(offset, length),
                                }
                            }),
                            read_into: Box::new(move |offset, destination, destination_offset, length| {
                                r2.set(r2.get() + 1);
                                match e2.borrow().clone() {
                                    Some(error) => Err(error),
                                    None => read_into(offset, destination, destination_offset, length),
                                }
                            }),
                        })
                    }),
                    ..raw
                })
            }),
        }
    };
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
    let snapshot = |image: &I::t| {
        let (w, h, g, t) = I::gpu_snapshot(image).unwrap().expect("a GPU snapshot");
        let (sw, sh) = I::size(image).unwrap();
        assert_eq!((w, h, g), (sw, sh, I::generation(image)));
        t
    };
    assert!(same(&snapshot(&image), &first) && reads.get() == 0);
    assert!(I::pixels(&image).unwrap() == bytes && reads.get() == 1 && I::readbacks(&image) == 1);
    let (_, _, _, saved, mut lease) = I::borrow_snapshot(&image).unwrap();
    assert!(*saved.borrow() == bytes && reads.get() == 2);
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
    assert!(same(&snapshot(&image), &resized) && *saved.borrow() == bytes && I::cpu_storage_bytes(&image) == 0);
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
    assert!(*cpu_saved.borrow() == cpu && I::cpu_storage_bytes(&image) == 0);
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
    let failed_canvas = Canvas::create(1024, 1024).unwrap();
    Canvas::publish_gpu(&failed_canvas, &large).unwrap();
    assert_eq!(Canvas::snapshot(&failed_canvas).err(), Some(backend_error), "Canvas.snapshot changed the backend readback error");
    *read_error.borrow_mut() = None;
    Canvas::destroy(&failed_canvas).unwrap();
    I::destroy(&failed_image).unwrap();
    let destroyed = I::create(1, 1, &[0x5a; 4]).unwrap();
    let (_, _, _, saved, mut lease) = I::borrow_snapshot(&destroyed).unwrap();
    I::destroy(&destroyed).unwrap();
    I::release_snapshot(&mut lease);
    I::release_snapshot(&mut lease);
    assert!(*saved.borrow() == [0x5a; 4] && I::cpu_storage_bytes(&destroyed) == 0);
    for t in textures.borrow().iter() {
        let _ = b::destroy_texture(t);
    }
    b::destroy_device(&device).unwrap();
    assert_eq!(ogpu_mock::impl_::live_handles(), 0);
}

fn main() {
    common::run(&[("gpu_image", gpu_image)]);
}
