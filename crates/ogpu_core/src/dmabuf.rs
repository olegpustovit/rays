//! Linux buffer sharing for a compositor: dmabuf descriptions, the DRM formats a device can import,
//! and the capability flags for sync_file fences. Backends without these return `Unsupported`.
use crate::types::TextureFormat;
use std::os::fd::BorrowedFd;

pub const fn fourcc(code: &[u8; 4]) -> u32 { u32::from_le_bytes(*code) }
pub const R8: u32 = fourcc(b"R8  ");
pub const XRGB8888: u32 = fourcc(b"XR24");
pub const ARGB8888: u32 = fourcc(b"AR24");
pub const XBGR8888: u32 = fourcc(b"XB24");
pub const ABGR8888: u32 = fourcc(b"AB24");
pub const XBGR16161616F: u32 = fourcc(b"XB4H");
pub const ABGR16161616F: u32 = fourcc(b"AB4H");
pub const MOD_LINEAR: u64 = 0;
pub const MOD_INVALID: u64 = (1 << 56) - 1;
pub const MAX_PLANES: usize = 4;

/// DRM fourcc to texture format, ported from wlroots `render/vulkan/pixel_format.c` (MIT). DRM channel
/// order is little-endian, texture formats are in memory byte order, so the names read reversed.
/// The X variants import as the alpha format; their alpha byte is undefined and the caller ignores it.
/// ponytail: only formats with a `TextureFormat`; packed 16-bit, 10-bit and YCbCr need new variants (and
/// YCbCr samplers) when a client must use them.
pub const FORMATS: [(u32, TextureFormat); 7] = [
    (R8, TextureFormat::R8Unorm),
    (XRGB8888, TextureFormat::Bgra8Unorm),
    (ARGB8888, TextureFormat::Bgra8Unorm),
    (XBGR8888, TextureFormat::Rgba8Unorm),
    (ABGR8888, TextureFormat::Rgba8Unorm),
    (XBGR16161616F, TextureFormat::Rgba16Float),
    (ABGR16161616F, TextureFormat::Rgba16Float),
];

pub fn texture_format(fourcc: u32) -> Option<TextureFormat> {
    FORMATS.iter().find(|(code, _)| *code == fourcc).map(|(_, format)| *format)
}

/// One memory plane: the fd is borrowed, the importer duplicates it.
#[derive(Clone, Copy, Debug)]
pub struct Plane<'a> { pub fd: BorrowedFd<'a>, pub offset: u32, pub stride: u32 }

/// A client or scanout buffer, as `zwp_linux_buffer_params_v1` describes it.
#[derive(Clone, Copy, Debug)]
pub struct Dmabuf<'a> { pub width: u32, pub height: u32, pub fourcc: u32, pub modifier: u64, pub planes: &'a [Plane<'a>] }

/// A modifier the device can import for one format, and how.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Modifier { pub modifier: u64, pub planes: u32, pub texture: bool, pub render: bool, pub max_width: u32, pub max_height: u32 }

/// What a compositor advertises through linux-dmabuf: each format with its importable modifiers.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Format { pub fourcc: u32, pub modifiers: Vec<Modifier> }

#[cfg(test)]
#[test]
fn fourcc_codes_match_drm_fourcc_h() {
    assert_eq!([R8, XRGB8888, ARGB8888, XBGR8888, ABGR8888, XBGR16161616F, ABGR16161616F],
        [0x2020_3852, 0x3432_5258, 0x3432_5241, 0x3432_4258, 0x3432_4241, 0x4834_4258, 0x4834_4241]);
    assert_eq!(texture_format(ARGB8888), Some(TextureFormat::Bgra8Unorm));
    assert_eq!(texture_format(ABGR8888), Some(TextureFormat::Rgba8Unorm));
    assert_eq!(texture_format(fourcc(b"NV12")), None);
    assert_eq!(MOD_INVALID, 0x00ff_ffff_ffff_ffff);
}

/// Which compositor hooks the device has. All false on backends without them.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct External {
    /// Dmabufs import as textures and render targets.
    pub dmabuf: bool,
    /// Any DRM modifier the device lists, not only linear.
    pub drm_modifiers: bool,
    /// `export_sync_file` hands out a sync_file fence.
    pub sync_file_export: bool,
    /// `import_sync_file` makes the GPU wait; without it the call waits on the CPU.
    pub sync_file_import: bool,
    /// The kernel can attach a sync_file to a dmabuf's implicit fences, and read them back.
    pub dmabuf_sync_file: bool,
}
