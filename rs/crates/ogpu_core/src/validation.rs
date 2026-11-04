use crate::caps::Caps;
use crate::error::{self, Error, ErrorKind};
use crate::types::{
    BufferDescriptor, Memory, SamplerDescriptor, TextureDescriptor, TextureFormat, TextureUsage,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextureProfile {
    pub format: TextureFormat,
    pub storage: Memory,
    pub width: i64,
    pub height: i64,
    pub depth: i64,
    pub mip_levels: i64,
    pub sample_count: i64,
    pub usage: Vec<TextureUsage>,
}

fn invalid(operation: &str, message: &str) -> Error {
    error::make(operation, ErrorKind::InvalidArgument, message)
}

pub fn validate_label(operation: &str, label: Option<&str>) -> Result<(), Error> {
    if label.is_some_and(|label| label.contains('\0')) {
        Err(invalid(operation, "label contains NUL"))
    } else {
        Ok(())
    }
}

fn validate_buffer_shape(
    operation: &str,
    max_size: i64,
    size: i64,
    usage_count: usize,
) -> Result<(), Error> {
    if max_size <= 0 || size <= 0 || size > max_size {
        Err(invalid(operation, "buffer size is outside device limits"))
    } else if usage_count == 0 {
        Err(invalid(operation, "buffer usage is empty"))
    } else {
        Ok(())
    }
}

pub fn validate_texture_shape(
    operation: &str,
    max_dimension: i64,
    max_samples: i64,
    width: i64,
    height: i64,
    depth: i64,
    mip_levels: i64,
    sample_count: i64,
    usage_count: i64,
) -> Result<(), Error> {
    if max_dimension <= 0
        || max_samples <= 0
        || width <= 0
        || height <= 0
        || depth <= 0
        || width > max_dimension
        || height > max_dimension
    {
        return Err(invalid(operation, "texture extent is outside device limits"));
    }
    if mip_levels <= 0
        || sample_count <= 0
        || sample_count > max_samples
        || sample_count & (sample_count - 1) != 0
    {
        return Err(invalid(operation, "texture cardinality is invalid"));
    }
    if usage_count <= 0 {
        return Err(invalid(operation, "texture usage is empty"));
    }
    if width.checked_mul(height).and_then(|wh| wh.checked_mul(depth)).is_none() {
        return Err(invalid(operation, "texture texel cardinality overflows"));
    }
    let mut n = width.max(height).max(depth);
    let mut maximum_mips = 1;
    while n > 1 {
        n /= 2;
        maximum_mips += 1;
    }
    if mip_levels > maximum_mips {
        Err(invalid(operation, "texture mip cardinality exceeds its extent"))
    } else {
        Ok(())
    }
}

pub fn validate_texture_profile(capabilities: &Caps, profile: &TextureProfile) -> Result<(), Error> {
    let operation = "Ogpu.Validation.validate_texture_profile";
    let limits = capabilities.limits;
    validate_texture_shape(
        operation,
        limits.max_texture_dimension_2d,
        limits.max_sample_count,
        profile.width,
        profile.height,
        profile.depth,
        profile.mip_levels,
        profile.sample_count,
        profile.usage.len() as i64,
    )?;
    if profile.usage.windows(2).any(|pair| pair[0] as u8 >= pair[1] as u8) {
        return Err(invalid(operation, "texture usage must be sorted and unique"));
    }
    if profile.sample_count > 1
        && (profile.mip_levels != 1
            || profile.depth != 1
            || profile.usage.len() != 1
            || profile.usage[0] != TextureUsage::RenderAttachment)
    {
        return Err(invalid(
            operation,
            "multisample textures require one attachment-only mip and depth slice",
        ));
    }
    if profile.storage == Memory::Shared && profile.sample_count > 1 {
        return Err(invalid(operation, "shared multisample textures are unsupported"));
    }
    Ok(())
}


pub fn validate_buffer(capabilities: &Caps, descriptor: &BufferDescriptor) -> Result<(), Error> {
    let operation = "Ogpu.Types.validate_buffer";
    validate_label(operation, descriptor.label.as_deref())?;
    validate_buffer_shape(
        operation,
        capabilities.limits.max_buffer_size,
        descriptor.size,
        descriptor.usage.len(),
    )
}

pub fn validate_texture(capabilities: &Caps, descriptor: &TextureDescriptor) -> Result<(), Error> {
    let operation = "Ogpu.Types.validate_texture";
    validate_label(operation, descriptor.label.as_deref())?;
    let limits = capabilities.limits;
    validate_texture_shape(
        operation,
        limits.max_texture_dimension_2d,
        limits.max_sample_count,
        descriptor.width,
        descriptor.height,
        descriptor.depth,
        descriptor.mip_levels,
        descriptor.sample_count,
        descriptor.usage.len() as i64,
    )?;
    if matches!(
        descriptor.format,
        TextureFormat::R8Unorm
            | TextureFormat::Bgra8Unorm
            | TextureFormat::Bgra8UnormSrgb
            | TextureFormat::Depth32Float
    ) {
        return Err(invalid(
            operation,
            "OGPU texture descriptors require an RGBA color format",
        ));
    }
    if descriptor.format != TextureFormat::Rgba8Unorm
        && (descriptor.usage.contains(&TextureUsage::RenderAttachment)
            || descriptor.sample_count > 1)
    {
        return Err(error::make(
            operation,
            ErrorKind::Unsupported,
            "float textures are not render attachments",
        ));
    }
    Ok(())
}

pub fn validate_sampler(descriptor: &SamplerDescriptor) -> Result<(), Error> {
    let operation = "Ogpu.Types.validate_sampler";
    validate_label(operation, descriptor.label.as_deref())?;
    if !descriptor.lod_min.is_finite()
        || !descriptor.lod_max.is_finite()
        || descriptor.lod_min < 0.0
        || descriptor.lod_max < descriptor.lod_min
        || descriptor.max_anisotropy < 1
        || descriptor.max_anisotropy > 16
    {
        Err(error::make(
            operation,
            ErrorKind::InvalidArgument,
            "sampler LOD/anisotropy limits are invalid",
        ))
    } else {
        Ok(())
    }
}

pub fn validate_layout(operation: &str, entries: &[(i64, Vec<i64>)]) -> Result<(), Error> {
    let mut entries = entries.to_vec();
    entries.sort_by_key(|(binding, _)| *binding);
    let mut previous = None;
    for (binding, visibility) in entries {
        if binding < 0 {
            return Err(invalid(operation, "binding is negative"));
        }
        if visibility.is_empty() {
            return Err(invalid(operation, "visibility is empty"));
        }
        if visibility.windows(2).any(|pair| pair[0] >= pair[1]) {
            return Err(invalid(operation, "visibility must be sorted and unique"));
        }
        if previous == Some(binding) {
            return Err(invalid(operation, "duplicate binding"));
        }
        previous = Some(binding);
    }
    Ok(())
}

pub fn validate_groups(operation: &str, max_groups: i64, groups: &[i64]) -> Result<(), Error> {
    let mut groups = groups.to_vec();
    groups.sort_unstable();
    if max_groups < 0 || groups.len() as i64 > max_groups {
        return Err(error::make(operation, ErrorKind::Capacity, "bind-group limit exceeded"));
    }
    for (expected, group) in groups.into_iter().enumerate() {
        if group != expected as i64 {
            return Err(invalid(operation, "groups must be contiguous from zero"));
        }
    }
    Ok(())
}
