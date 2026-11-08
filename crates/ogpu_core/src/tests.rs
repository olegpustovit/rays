use crate::{
    acceleration, binding, caps, error::ErrorKind, handle, pipeline, shader, surface, types,
    validation,
};


fn has_kind(error: Option<crate::error::Error>, kind: ErrorKind) -> bool {
    error.is_some_and(|error| error.kind == kind)
}

#[test]
fn handles_capabilities_validation_and_binary16() {
    let device = handle::create_device();
    let other = handle::create_device();
    let resource = handle::create(&device);
    assert!(handle::validate_for("test", &device, &resource).is_ok());
    assert!(has_kind(
        handle::validate_for("test", &other, &resource).err(),
        ErrorKind::CrossDevice
    ));
    handle::destroy(&resource);
    handle::destroy(&resource);
    assert!(has_kind(
        handle::validate_for("test", &device, &resource).err(),
        ErrorKind::StaleHandle
    ));

    let capabilities = caps::minimum_m1();
    assert!(caps::validate(&capabilities).is_ok());
    assert!(has_kind(
        validation::validate_buffer(
            &capabilities,
            &types::BufferDescriptor {
                label: None,
                size: 0,
                usage: vec![types::BufferUsage::CopyDst],
            },
        ).err(),
        ErrorKind::InvalidArgument
    ));
    for (value, expected) in [
        (1.0, 0x3c00),
        (-2.0, 0xc000),
        (65520.0, 0x7c00),
        (f64::NEG_INFINITY, 0xfc00),
        (2f64.powi(-24), 1),
        (2f64.powi(-25), 0),
        (-0.0, 0x8000),
    ] {
        assert_eq!(types::half_of_float(value), expected);
    }
}

#[test]
fn shader_bindings_and_pipeline_validation() {
    let vertex = shader::create(shader::Descriptor {
        backend: "mock".into(),
        label: None,
        bytes: b"vertex".to_vec(),
        entry_points: vec![shader::EntryPoint {
            name: "vs".into(),
            stage: shader::Stage::Vertex,
        }],
        bindings: Vec::new(),
    })
    .unwrap();
    let device = handle::create_device();
    let pipeline_layout = binding::create_pipeline_layout(&device, &caps::minimum_m1(), &[]).unwrap();
    let descriptor = pipeline::RenderDescriptor {
        backend: "mock".into(),
        label: None,
        layout: pipeline_layout,
        vertex,
        vertex_entry: "vs".into(),
        fragment: None,
        fragment_entry: None,
        color_format: pipeline::ColorFormat::Rgba8Unorm,
        depth_format: pipeline::DepthFormat::NoDepth,
        sample_count: 1,
    };
    assert!(pipeline::create_render(&caps::minimum_m1(), &descriptor).is_ok());
    assert!(has_kind(
        pipeline::create_render(
            &caps::minimum_m1(),
            &pipeline::RenderDescriptor {
                sample_count: 3,
                ..descriptor
            },
        ).err(),
        ErrorKind::InvalidArgument
    ));
    assert!(binding::create_layout(&[binding::LayoutEntry {
        binding: 0,
        kind: binding::Kind::Buffer,
        visibility: vec![binding::Stage::Vertex, binding::Stage::Vertex],
    }])
    .is_err());
}

#[test]
fn surface_frame_lifecycle_and_acceleration_validation() {
    let device = handle::create_device();
    let configuration = surface::Configuration {
        logical_width: 64,
        logical_height: 48,
        physical_width: 128,
        physical_height: 96,
        format: surface::Format::Bgra8Unorm,
        present_mode: surface::PresentMode::Fifo,
        max_acquired: 1,
        layer: None,
    };
    let target = surface::create(&device, configuration).unwrap_or_else(|_| panic!("surface create failed"));
    let frame = match surface::acquire(&target).unwrap_or_else(|_| panic!("surface acquire failed")) {
        surface::AcquireResult::Acquired(frame) => frame,
        _ => panic!("expected acquired frame"),
    };
    assert!(has_kind(surface::acquire(&target).err(), ErrorKind::Capacity));
    assert!(surface::present(&target, &frame).is_ok());
    assert!(has_kind(
        surface::present(&target, &frame).err(),
        ErrorKind::InvalidState
    ));
    surface::destroy(&target);
    assert!(has_kind(surface::acquire(&target).err(), ErrorKind::InvalidState));

    let range = acceleration::BufferRange {
        buffer: handle::create(&device),
        buffer_size: 4096,
        offset: 0,
        length: 1024,
    };
    let mut structure = acceleration::create(
        &device,
        true,
        false,
        acceleration::Descriptor::Blas {
            geometries: vec![acceleration::Geometry::Triangles {
                vertices: range,
                vertex_stride: 12,
                vertex_count: 3,
            }],
            motion_keyframes: None,
        },
    )
    .unwrap_or_else(|_| panic!("acceleration create failed"));
    assert!(acceleration::build(&device, &mut structure).is_ok());
    assert!(acceleration::built(&structure));
}
fn discard_test_error() -> crate::error::Error {
    crate::error::make(
        "discard test",
        ErrorKind::Unsupported,
        "unused driver operation",
    )
}

struct DiscardSurface { fail_first: bool, attempts: std::rc::Rc<std::cell::Cell<i64>> }
impl crate::backend::DriverSurface for DiscardSurface {
    fn configure(&self, _: &crate::surface::Configuration) -> Result<(), crate::error::Error> { Ok(()) }
    fn acquire_sync(&self) -> Result<crate::backend::DriverAcquire, crate::error::Error> {
        Ok(crate::backend::DriverAcquire::Acquired(crate::backend::DriverFrame { frame_token: 7 }))
    }
    fn discard(&self, _: crate::backend::DriverFrame) -> Result<(), crate::error::Error> {
        let current = self.attempts.get();
        self.attempts.set(current + 1);
        if self.fail_first && current == 0 {
            Err(crate::error::make("discard test", ErrorKind::Io, "driver discard failed"))
        } else {
            Ok(())
        }
    }
    fn destroy_surface(&self) -> Result<(), crate::error::Error> { Ok(()) }
}

/// A device whose every operation but surfaces is unsupported.
struct DiscardDevice { fail_first: bool, attempts: std::rc::Rc<std::cell::Cell<i64>> }
impl crate::backend::DriverDevice for DiscardDevice {
    fn device_handle(&self) -> handle::Device { handle::create_device() }
    fn capabilities(&self) -> caps::Caps { caps::minimum_m1() }
    fn allocated_size(&self) -> i64 { 0 }
    fn recommended_working_set(&self) -> i64 { 0 }
    fn create_buffer(&self, _: types::Memory, _: &types::BufferDescriptor) -> Result<Box<dyn crate::backend::DriverResource>, crate::error::Error> { Err(discard_test_error()) }
    fn create_mapped_buffer(&self, _: &types::BufferDescriptor) -> Result<crate::backend::DriverMapped, crate::error::Error> { Err(discard_test_error()) }
    fn create_texture(&self, _: &types::TextureDescriptor) -> Result<Box<dyn crate::backend::DriverResource>, crate::error::Error> { Err(discard_test_error()) }
    fn create_depth_texture(&self, _: &types::TextureDescriptor) -> Result<Box<dyn crate::backend::DriverResource>, crate::error::Error> { Err(discard_test_error()) }
    fn create_memoryless_texture(&self, _: bool, _: &types::TextureDescriptor) -> Result<Box<dyn crate::backend::DriverResource>, crate::error::Error> { Err(discard_test_error()) }
    fn create_library(&self, _: &shader::Shader) -> Result<Box<dyn crate::backend::DriverLibrary>, crate::error::Error> { Err(discard_test_error()) }
    fn warm_library(&self, _: &shader::Shader) -> Option<crate::backend::Warm> { None }
    fn create_accel(&self, _: &crate::backend::DriverAccelDescriptor) -> Result<Box<dyn crate::backend::DriverAccel>, crate::error::Error> { Err(discard_test_error()) }
    fn instance_layout(&self, _: crate::backend::InstanceKind) -> Vec<i64> { vec![64; 12] }
    fn create_sampler(&self, _: &types::SamplerDescriptor) -> Result<Box<dyn crate::backend::DriverSampler>, crate::error::Error> { Err(discard_test_error()) }
    fn create_render_pipeline(&self, _: crate::backend::RenderPipelineOptions, _: &pipeline::RenderDescriptor) -> Result<Box<dyn crate::backend::DriverPipeline>, crate::error::Error> { Err(discard_test_error()) }
    fn create_icb(&self, _: i64) -> Result<Box<dyn crate::backend::DriverIcb>, crate::error::Error> { Err(discard_test_error()) }
    fn create_argument(&self, _: i64, _: crate::backend::ShaderStage, _: i64) -> Result<Box<dyn crate::backend::DriverArgument>, crate::error::Error> { Err(discard_test_error()) }
    fn create_queue(&self) -> Result<Box<dyn crate::backend::DriverQueue>, crate::error::Error> { Err(discard_test_error()) }
    fn create_surface(&self, _: &surface::Configuration) -> Result<Box<dyn crate::backend::DriverSurface>, crate::error::Error> {
        Ok(Box::new(DiscardSurface { fail_first: self.fail_first, attempts: self.attempts.clone() }))
    }
    fn destroy_device(&self) -> Result<(), crate::error::Error> { Ok(()) }
}

struct DiscardDriver { fail_first: bool, attempts: std::rc::Rc<std::cell::Cell<i64>> }
impl crate::backend::Driver for DiscardDriver {
    fn create_device(&self) -> Result<Box<dyn crate::backend::DriverDevice>, crate::error::Error> {
        Ok(Box::new(DiscardDevice { fail_first: self.fail_first, attempts: self.attempts.clone() }))
    }
}

fn discard_test_device(fail_first: bool) -> (crate::backend::Device, std::rc::Rc<std::cell::Cell<i64>>) {
    let attempts = std::rc::Rc::new(std::cell::Cell::new(0));
    let driver = Box::new(DiscardDriver { fail_first, attempts: attempts.clone() });
    let device = crate::backend::create_device(driver).unwrap_or_else(|_| panic!("test device creation failed"));
    (device, attempts)
}

fn discard_test_configuration() -> crate::surface::Configuration {
    crate::surface::Configuration {
        logical_width: 64,
        logical_height: 48,
        physical_width: 128,
        physical_height: 96,
        format: crate::surface::Format::Bgra8Unorm,
        present_mode: crate::surface::PresentMode::Fifo,
        max_acquired: 1,
        layer: None,
    }
}

#[test]
fn backend_discard_releases_frame_before_reconfigure_and_destroy() {
    let (device, attempts) = discard_test_device(false);
    let target = crate::backend::create_surface(&device, discard_test_configuration())
        .unwrap_or_else(|_| panic!("test surface creation failed"));
    let frame = match crate::backend::acquire_sync(&target)
        .unwrap_or_else(|_| panic!("test frame acquisition failed"))
    {
        crate::backend::AcquireResult::Acquired(frame) => frame,
        _ => panic!("expected acquired frame"),
    };
    assert!(crate::backend::discard(&frame).is_ok());
    assert_eq!(attempts.get(), 1);
    assert!(crate::backend::configure(&target, discard_test_configuration()).is_ok());
    assert!(has_kind(
        crate::backend::discard(&frame).err(),
        ErrorKind::InvalidState
    ));
    assert_eq!(attempts.get(), 1);
    assert!(crate::backend::destroy_surface(&target).is_ok());
    assert!(crate::backend::destroy_device(&device).is_ok());
}

#[test]
fn backend_discard_callback_failure_keeps_frame_retryable() {
    let (device, attempts) = discard_test_device(true);
    let target = crate::backend::create_surface(&device, discard_test_configuration())
        .unwrap_or_else(|_| panic!("test surface creation failed"));
    let frame = match crate::backend::acquire_sync(&target)
        .unwrap_or_else(|_| panic!("test frame acquisition failed"))
    {
        crate::backend::AcquireResult::Acquired(frame) => frame,
        _ => panic!("expected acquired frame"),
    };
    assert!(has_kind(
        crate::backend::discard(&frame).err(),
        ErrorKind::Io
    ));
    assert_eq!(attempts.get(), 1);
    assert!(has_kind(
        crate::backend::configure(&target, discard_test_configuration()).err(),
        ErrorKind::InvalidState
    ));
    assert!(crate::backend::discard(&frame).is_ok());
    assert_eq!(attempts.get(), 2);
    assert!(crate::backend::configure(&target, discard_test_configuration()).is_ok());
    assert!(crate::backend::destroy_surface(&target).is_ok());
    assert!(crate::backend::destroy_device(&device).is_ok());
}
