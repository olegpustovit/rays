#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Format {
    R8Unorm,
    Rgba8Unorm,
    Bgra8Unorm,
    Bgra8UnormSrgb,
    Rgba16Float,
    Rgba32Float,
    Depth32Float,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Layout {
    pub block_width: usize,
    pub block_height: usize,
    pub bytes_per_block: usize,
}

impl Format {
    pub const fn code(self) -> u32 {
        match self {
            Self::R8Unorm => 10,
            Self::Rgba8Unorm => 70,
            Self::Bgra8Unorm => 80,
            Self::Bgra8UnormSrgb => 81,
            Self::Rgba16Float => 115,
            Self::Rgba32Float => 125,
            Self::Depth32Float => 252,
        }
    }

    pub const fn layout(self) -> Layout {
        let bytes_per_block = match self {
            Self::R8Unorm => 1,
            Self::Rgba8Unorm | Self::Bgra8Unorm | Self::Bgra8UnormSrgb | Self::Depth32Float => 4,
            Self::Rgba16Float => 8,
            Self::Rgba32Float => 16,
        };
        Layout { block_width: 1, block_height: 1, bytes_per_block }
    }

    pub const fn is_depth_or_stencil(self) -> bool {
        matches!(self, Self::Depth32Float)
    }
}
