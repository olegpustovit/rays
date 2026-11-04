use crate::{
    caps::{self, Caps},
    error::ErrorKind,
    types::{self, Memory, TextureDescriptor, TextureFormat, TextureUsage},
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
    let storages = [Memory::DeviceLocal, Memory::Shared];
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
                        &TextureProfile {
                            format,
                            storage,
                            width: 64,
                            height: 64,
                            depth: 1,
                            mip_levels: if sample_count == 1 { 7 } else { 1 },
                            sample_count,
                            usage: usage.clone(),
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
