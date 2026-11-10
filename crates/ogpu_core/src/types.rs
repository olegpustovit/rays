#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BufferUsage {
    CopySrc,
    CopyDst,
    Uniform,
    Storage,
    Vertex,
    Index,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Memory {
    Shared,
    DeviceLocal,
    Memoryless,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BufferDescriptor {
    pub label: Option<String>,
    pub size: i64,
    pub usage: Vec<BufferUsage>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TextureUsage {
    TextureBinding,
    StorageBinding,
    RenderAttachment,
    TextureCopySrc,
    TextureCopyDst,
    TextureView,
    TextureAtomic,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TextureFormat {
    R8Unorm,
    Rgba8Unorm,
    Bgra8Unorm,
    Bgra8UnormSrgb,
    Rgba16Float,
    Rgba32Float,
    Depth32Float,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextureDescriptor {
    pub label: Option<String>,
    pub width: i64,
    pub height: i64,
    pub depth: i64,
    pub mip_levels: i64,
    pub sample_count: i64,
    pub format: TextureFormat,
    pub usage: Vec<TextureUsage>,
}

pub fn texel_bytes(format: TextureFormat) -> i64 {
    match format {
        TextureFormat::R8Unorm => 1,
        TextureFormat::Rgba8Unorm
        | TextureFormat::Bgra8Unorm
        | TextureFormat::Bgra8UnormSrgb
        | TextureFormat::Depth32Float => 4,
        TextureFormat::Rgba16Float => 8,
        TextureFormat::Rgba32Float => 16,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Origin {
    pub x: i64,
    pub y: i64,
    pub z: i64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Extent {
    pub width: i64,
    pub height: i64,
    pub depth: i64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SamplerFilter {
    Nearest,
    Linear,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MipFilter {
    NoMip,
    NearestMip,
    LinearMip,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AddressMode {
    ClampToEdge,
    Repeat,
    MirrorRepeat,
    MirrorClampToEdge,
    ClampToZero,
    ClampToBorder,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SamplerDescriptor {
    pub label: Option<String>,
    pub min_filter: SamplerFilter,
    pub mag_filter: SamplerFilter,
    pub mip_filter: MipFilter,
    pub address_u: AddressMode,
    pub address_v: AddressMode,
    pub lod_min: f64,
    pub lod_max: f64,
    pub max_anisotropy: i64,
}

pub fn half_of_float(value: f64) -> i64 {
    let bits = value.to_bits();
    let sign = if bits >> 63 != 0 { 0x8000 } else { 0 };
    let exponent = ((bits >> 52) & 0x7ff) as i32;
    let mantissa = bits & 0x000f_ffff_ffff_ffff;
    let round = |value: u64, shift: u32| -> u64 {
        let q = value >> shift;
        let rest = value & ((1u64 << shift) - 1);
        let half = 1u64 << (shift - 1);
        q + if rest > half || (rest == half && q & 1 == 1) { 1 } else { 0 }
    };
    if exponent == 0x7ff {
        (sign | if mantissa == 0 { 0x7c00 } else { 0x7e00 }) as i64
    } else if exponent - 1008 >= 31 {
        (sign | 0x7c00) as i64
    } else if exponent - 1008 > 0 {
        (sign | (((exponent - 1008) as u64) << 10) + round(mantissa, 42)) as i64
    } else if 1051 - exponent >= 54 {
        sign as i64
    } else {
        (sign | round(mantissa | (1u64 << 52), (1051 - exponent) as u32)) as i64
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StorageMode { Shared, Managed, Private, Memoryless }
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CpuCacheMode { DefaultCache, WriteCombined }
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum HazardTrackingMode { Default, Untracked, Tracked }

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TextureKind { Texture1d, Texture1dArray, Texture2d, Texture2dArray, Texture2dMultisample, TextureCube, TextureCubeArray, Texture3d, Texture2dMultisampleArray, TextureBuffer }
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CompressionType { Lossless, Lossy }
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SwizzleChannel { Zero, One, Red, Green, Blue, Alpha }
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Swizzle { pub red: SwizzleChannel, pub green: SwizzleChannel, pub blue: SwizzleChannel, pub alpha: SwizzleChannel }
impl Swizzle { pub const IDENTITY: Self = Self { red: SwizzleChannel::Red, green: SwizzleChannel::Green, blue: SwizzleChannel::Blue, alpha: SwizzleChannel::Alpha }; }
