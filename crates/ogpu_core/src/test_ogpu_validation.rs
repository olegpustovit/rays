use crate::{
    caps::{self, Caps},
    error::ErrorKind,
    types::{self, StorageMode, TextureKind, TextureDescriptor, TextureFormat, TextureUsage},
    validation::{self, TextureProfile},
};

fn kind(result: Result<(), crate::error::Error>) -> &'static str {
    match result {
        Ok(()) => "ok",
        Err(error) => match error.kind {
            ErrorKind::InvalidArgument => "invalid",
            ErrorKind::Unsupported => "unsupported",
            ErrorKind::Capacity => "capacity",
            _ => "other",
        },
    }
}

fn subsets(values: &[TextureUsage]) -> Vec<Vec<TextureUsage>> {
    match values.split_first() {
        None => vec![Vec::new()],
        Some((value, rest)) => {
            let tail = subsets(rest);
            let mut all = tail.clone();
            all.extend(tail.into_iter().map(|mut values| {
                values.insert(0, *value);
                values
            }));
            all
        }
    }
}

fn matrix(capabilities: Caps) -> Vec<&'static str> {
    use TextureFormat::*;
    let formats = [R8Unorm, Rgba8Unorm, Bgra8Unorm, Bgra8UnormSrgb, Rgba16Float, Rgba32Float, Depth32Float];
    let storages = [StorageMode::Private, StorageMode::Shared];
    let samples = [0, 1, 2, 3, 4, 8];
    let usages = subsets(&[
        TextureUsage::TextureBinding,
        TextureUsage::RenderAttachment,
        TextureUsage::TextureCopySrc,
        TextureUsage::TextureCopyDst,
    ]);
    let mut out = Vec::new();
    for format in formats {
        for storage in storages {
            for sample_count in samples {
                for usage in &usages {
                    out.push(kind(validation::validate_texture_profile(
                        &capabilities,
                        &{
                            let mut profile = TextureProfile::descriptor_2d(format, 64, 64, storage, usage.clone(), None);
                            profile.mip_levels = if sample_count == 1 { 7 } else { 1 };
                            profile.sample_count = sample_count;
                            profile.kind = if sample_count > 1 { TextureKind::Texture2dMultisample } else { TextureKind::Texture2d };
                            profile
                        },
                    )));
                }
            }
        }
    }
    out
}

fn texture(format: TextureFormat, usage: Vec<TextureUsage>) -> TextureDescriptor {
    TextureDescriptor {
        label: Some("unchanged".into()),
        width: 4,
        height: 4,
        depth: 1,
        mip_levels: 3,
        sample_count: 1,
        format,
        usage,
    }
}

#[test]
fn centralized_validation() {
    let capabilities = caps::minimum_m1();
    let sequential = matrix(capabilities);
    assert_eq!(sequential.len(), 1344, "validation matrix cardinality drift");
    let workers: Vec<_> = (0..4).map(|_| std::thread::spawn(move || matrix(capabilities))).collect();
    for worker in workers {
        assert_eq!(worker.join().unwrap(), sequential, "validation result domain drift");
    }
    let descriptor = texture(TextureFormat::Rgba8Unorm, vec![TextureUsage::TextureBinding]);
    validation::validate_texture(&capabilities, &descriptor).unwrap();
    assert_eq!(descriptor.label.as_deref(), Some("unchanged"), "label mutated");
    let float_target = texture(
        TextureFormat::Rgba32Float,
        vec![TextureUsage::TextureBinding, TextureUsage::RenderAttachment],
    );
    match validation::validate_texture(&capabilities, &float_target) {
        Err(error) if error.kind == ErrorKind::Unsupported => {}
        _ => panic!("float render attachment accepted"),
    }
    validation::validate_texture(
        &capabilities,
        &texture(
            TextureFormat::Rgba16Float,
            vec![TextureUsage::TextureBinding, TextureUsage::StorageBinding],
        ),
    )
    .unwrap();
    for format in [
        TextureFormat::R8Unorm,
        TextureFormat::Bgra8Unorm,
        TextureFormat::Bgra8UnormSrgb,
        TextureFormat::Depth32Float,
    ] {
        match validation::validate_texture(&capabilities, &texture(format, vec![TextureUsage::TextureBinding])) {
            Err(error) if error.kind == ErrorKind::InvalidArgument => {}
            _ => panic!("non-RGBA OGPU texture descriptor accepted"),
        }
    }
    // binary16: rounding is to nearest even.
    let p = f64::powi;
    for (value, bits) in [
        (1.0, 0x3c00),
        (-2.0, 0xc000),
        (65504.0, 0x7bff),
        (65519.0, 0x7bff),
        (65520.0, 0x7c00),
        (1e9, 0x7c00),
        (f64::NEG_INFINITY, 0xfc00),
        (1.0 + p(2.0, -11), 0x3c00),
        (1.0 + 3.0 * p(2.0, -11), 0x3c02),
        (p(2.0, -24), 0x0001),
        (p(2.0, -25), 0x0000),
        (3.0 * p(2.0, -25), 0x0002),
        (p(2.0, -14) - p(2.0, -25), 0x0400),
        (-0.0, 0x8000),
    ] {
        assert_eq!(types::half_of_float(value), bits, "half_of_float {value:e}");
    }
}

#[test]
fn admitted_descriptors_preserve_advanced_limits_and_transfer_ownership() {
    use validation::{admit_texture, admit_base_texture_read, StorageMode, TextureKind};
    let mut caps = caps::minimum_m1();
    caps.limits.max_texture_dimension_1d = 32;
    caps.limits.max_texture_dimension_2d = 64;
    caps.limits.max_texture_dimension_3d = 8;
    caps.limits.max_texture_dimension_cube = 16;
    caps.limits.max_texture_array_layers = 4;
    caps.limits.color_sample_counts = 1 | 2 | 8;
    caps.limits.depth_sample_counts = 1 | 2;
    caps.limits.sample_counts = 1 | 2;
    caps::validate(&caps).unwrap();
    let profile = || TextureProfile::descriptor_2d(TextureFormat::Rgba8Unorm, 4, 4,
        StorageMode::Shared, vec![TextureUsage::TextureBinding], None);
    let invalid = |profile| assert_eq!(admit_texture(&caps, profile).unwrap_err().kind, ErrorKind::InvalidArgument);
    for axis in 0..3 {
        let mut d = profile(); d.kind = TextureKind::Texture3d;
        d.width = 8; d.height = 8; d.depth = 8;
        admit_texture(&caps, d.clone()).unwrap();
        match axis { 0 => d.width += 1, 1 => d.height += 1, _ => d.depth += 1 }
        invalid(d);
    }
    for (kind, maximum) in [(TextureKind::Texture1d,32), (TextureKind::Texture2d,64), (TextureKind::TextureCube,16)] {
        let mut d = profile(); d.kind = kind; d.width = maximum;
        if kind == TextureKind::Texture1d { d.height = 1; }
        if kind == TextureKind::TextureCube { d.height = maximum; }
        admit_texture(&caps, d.clone()).unwrap();
        d.width += 1; if kind == TextureKind::TextureCube { d.height += 1; }
        invalid(d);
    }
    let mut array = profile(); array.kind = TextureKind::Texture2dArray; array.array_length = 4;
    admit_texture(&caps, array.clone()).unwrap(); array.array_length += 1; invalid(array);
    for samples in [2, 8] {
        let mut d = profile(); d.kind = TextureKind::Texture2dMultisample; d.storage = StorageMode::Private;
        d.sample_count = samples; d.usage = vec![TextureUsage::RenderAttachment];
        admit_texture(&caps, d.clone()).unwrap();
        d.format = TextureFormat::Depth32Float;
        assert_eq!(admit_texture(&caps,d).is_ok(), samples == 2);
    }
    for samples in [0,3,4,16] {
        let mut d = profile(); d.kind = TextureKind::Texture2dMultisample; d.storage = StorageMode::Private;
        d.sample_count = samples; d.usage = vec![TextureUsage::RenderAttachment]; invalid(d);
    }
    let admitted = admit_texture(&caps, profile()).unwrap();
    let same_shape = admit_texture(&caps, profile()).unwrap();
    let read = admit_base_texture_read(&admitted,16).unwrap();
    assert!(read.belongs_to(&admitted)); assert!(!read.belongs_to(&same_shape));
    let mut short = [0xa5;63];
    assert_eq!(read.into_destination(&mut short).err().unwrap().kind, ErrorKind::InvalidArgument);
    assert_eq!(short,[0xa5;63]);
    for row in [0,15,17,i64::MAX] {
        assert_eq!(admit_base_texture_read(&admitted,row).err().unwrap().kind, ErrorKind::InvalidArgument);
    }
    let mut destination = [0xa5;64];
    let mut read = admit_base_texture_read(&admitted,16).unwrap().into_destination(&mut destination).unwrap();
    let (layout, borrowed) = read.parts(); assert_eq!(layout.length(),borrowed.len());
    borrowed[0] = 7; assert_eq!(destination[0],7);
    let mut other_caps = caps; other_caps.limits.max_texture_dimension_3d += 1;
    assert!(admitted.admitted_for(&caps)); assert!(!admitted.admitted_for(&other_caps));
    admitted.retire();
    assert_eq!(admit_base_texture_read(&admitted,16).err().unwrap().kind, ErrorKind::StaleHandle);
}

#[test]
fn native_function_sampler_pass_and_layer_arguments_are_admitted_in_core() {
    use crate::{render_pass as pass, shader, surface};
    use shader::FunctionConstantValue as C;
    for constants in [vec![("x".into(),C::Int8(128))],vec![("x".into(),C::Uint32(0x1_0000_0000))],
        vec![("x".into(),C::Bool(true)),("x".into(),C::Bool(false))]] {
        assert_eq!(shader::admit_function("entry".into(),&constants,true).unwrap_err().kind,ErrorKind::InvalidArgument);
    }
    let admitted = shader::admit_function("entry".into(),&[("x".into(),C::Uint32(0xffff_ffff))],true).unwrap();
    assert_eq!(admitted.constants()[0].2,0xffff_ffff);
    assert_eq!(admitted.pipeline_options(true).label(),Some("entry"));
    assert!(shader::of_metal_source("kernel void k() {}".into(),Some(String::new())).is_ok());
    assert!(shader::of_metal_source("\0".into(),None).is_err());
    for value in [f64::NAN,f64::INFINITY,f64::MAX,-1.] {
        let mut sampler=validation::SamplerProfile::default(); sampler.lod_min=value;
        assert_eq!(validation::admit_sampler(sampler).unwrap_err().kind,ErrorKind::InvalidArgument);
    }
    let mut sampler=validation::SamplerProfile::default(); sampler.normalized_coordinates=false;
    validation::admit_sampler(sampler.clone()).unwrap(); sampler.address_s=types::AddressMode::Repeat;
    assert!(validation::admit_sampler(sampler).is_err());
    let mut actions=*pass::AdmittedActions::PRESENT;
    for value in [f64::NAN,-0.1,1.1] { actions.clear_depth=value; assert!(pass::admit_actions(actions).is_err()); }
    actions.clear_depth=1.; actions.clear_stencil=256; assert!(pass::admit_actions(actions).is_err());
    assert!(pass::admit_clear_color([0.,0.,f64::NAN,1.]).is_err());
    let mut layer=surface::LayerProfile::default(4,4); surface::admit_layer("test",layer).unwrap();
    layer.maximum_drawables=1; assert!(surface::admit_layer("test",layer).is_err());
    assert_eq!(validation::admit_acceleration_size("test",0).unwrap_err().kind,ErrorKind::InvalidArgument);
    assert_eq!(validation::admit_acceleration_size("test",u64::MAX).unwrap().get(),u64::MAX);
    for index in [-1,31,i64::MAX] {
        assert_eq!(validation::admit_slot("test",index).unwrap_err().kind,ErrorKind::InvalidArgument);
    }
    for index in [0,30] { assert_eq!(validation::admit_slot("test",index).unwrap().index() as i64,index); }
    use validation::{admit_icb, IcbDescriptor, IcbCommandType as I};
    let descriptor = || IcbDescriptor::new(vec![I::IndirectDraw, I::IndirectDrawIndexed, I::IndirectConcurrentDispatch, I::IndirectConcurrentDispatchThreads]);
    let advanced = admit_icb(65_537, descriptor()).unwrap();
    assert_eq!(advanced.capacity(),65_537); assert_eq!(advanced.command_types_bits(),1|2|32|64);
    let index=validation::admit_icb_index(advanced.capacity(),65_536).unwrap();
    assert_eq!(index.index(),65_536); assert!(index.belongs_to_capacity(65_537));
    assert!(!index.belongs_to_capacity(1));
    for invalid in [-1,65_537,i64::MAX] {
        assert_eq!(validation::admit_icb_index(advanced.capacity(),invalid).unwrap_err().kind,ErrorKind::InvalidArgument);
    }
    assert_eq!(admit_icb(0,descriptor()).unwrap_err().kind,ErrorKind::InvalidArgument);
    for types in [vec![],vec![I::IndirectDraw,I::IndirectDraw],vec![I::IndirectDrawIndexed,I::IndirectDraw]] {
        assert_eq!(admit_icb(1,IcbDescriptor::new(types)).unwrap_err().kind,ErrorKind::InvalidArgument);
    }
    for set in [
        (|d: &mut IcbDescriptor,n| d.max_vertex_buffer_bind_count=n) as fn(&mut IcbDescriptor,usize),
        |d,n| d.max_fragment_buffer_bind_count=n, |d,n| d.max_kernel_buffer_bind_count=n,
        |d,n| d.max_kernel_threadgroup_memory_bind_count=n, |d,n| d.max_object_buffer_bind_count=n,
        |d,n| d.max_mesh_buffer_bind_count=n, |d,n| d.max_object_threadgroup_memory_bind_count=n,
    ] {
        let mut d=descriptor(); set(&mut d,31); admit_icb(1,d.clone()).unwrap(); set(&mut d,32);
        assert_eq!(admit_icb(1,d).unwrap_err().kind,ErrorKind::InvalidArgument);
    }
}
