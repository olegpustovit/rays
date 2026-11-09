use super::{Error, Handle, NativeHandle};
#[cfg(target_vendor = "apple")]
use std::ffi::c_char;
use std::ptr::{null, null_mut};

#[derive(Clone)]
pub struct Keyframe {
    pub buffer: Handle,
    pub offset: u64,
}

#[derive(Clone)]
pub struct IndexBuffer {
    pub buffer: Handle,
    pub offset: u64,
    pub uint16: bool,
}

#[derive(Clone, Copy)]
pub struct GeometryCommon {
    pub opaque: bool,
    pub allow_duplicate_intersection: bool,
}

#[derive(Clone)]
pub enum Geometry {
    Triangles {
        vertices: Vec<Keyframe>,
        vertex_stride: u64,
        triangle_count: u64,
        index: Option<IndexBuffer>,
        common: GeometryCommon,
    },
    BoundingBoxes {
        boxes: Vec<Keyframe>,
        stride: u64,
        count: u64,
        common: GeometryCommon,
    },
    Curves {
        control_points: Vec<Keyframe>,
        control_stride: u64,
        control_point_count: u64,
        radii: Vec<Keyframe>,
        radius_stride: u64,
        index: IndexBuffer,
        segment_count: u64,
        control_points_per_segment: u32,
        curve_type: i32,
        basis: i32,
        end_caps: i32,
        common: GeometryCommon,
    },
}

#[derive(Clone, Copy)]
pub struct Motion {
    pub keyframe_count: u64,
    pub start_time: f64,
    pub end_time: f64,
    pub start_border: i32,
    pub end_border: i32,
}

#[derive(Clone)]
pub struct PrimitiveDescriptor {
    pub geometries: Vec<Geometry>,
    pub motion: Option<Motion>,
    pub refit: bool,
    pub fast_build: bool,
}

#[derive(Clone)]
pub struct MotionTransforms {
    pub buffer: Handle,
    pub offset: u64,
    pub count: u64,
}

#[derive(Clone)]
pub struct InstancesDescriptor {
    pub buffer: Handle,
    pub offset: u64,
    pub stride: u64,
    pub count: u64,
    /// 1 is user-ID descriptors, 2 is motion descriptors.
    pub instance_kind: i32,
    pub structures: Vec<Handle>,
    pub motion_transforms: Option<MotionTransforms>,
    pub refit: bool,
}

pub enum Descriptor {
    Primitive(PrimitiveDescriptor),
    Instances(InstancesDescriptor),
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct Sizes {
    pub acceleration_structure_size: u64,
    pub build_scratch_buffer_size: u64,
    pub refit_scratch_buffer_size: u64,
}

#[repr(C)]
struct RawKeyframe {
    buffer: *mut NativeHandle,
    offset: u64,
}

#[repr(C)]
struct RawGeometry {
    kind: i32,
    buffer: *mut NativeHandle,
    offset: u64,
    stride: u64,
    count: u64,
    secondary_buffer: *mut NativeHandle,
    secondary_offset: u64,
    secondary_stride: u64,
    index_buffer: *mut NativeHandle,
    index_offset: u64,
    index_uint16: u8,
    segment_count: u64,
    segment_control_points: u32,
    curve_type: i32,
    basis: i32,
    end_caps: i32,
    keyframes: *const RawKeyframe,
    keyframe_count: usize,
    secondary_keyframes: *const RawKeyframe,
    secondary_keyframe_count: usize,
    opaque: u8,
    allow_duplicate_intersection: u8,
}

#[repr(C)]
struct RawMotion {
    keyframe_count: u64,
    start_time: f64,
    end_time: f64,
    start_border: i32,
    end_border: i32,
}

#[repr(C)]
struct RawPrimitiveDescriptor {
    geometries: *const RawGeometry,
    geometry_count: usize,
    has_motion: u8,
    motion: RawMotion,
    refit: u8,
    fast_build: u8,
}

#[repr(C)]
struct RawInstancesDescriptor {
    buffer: *mut NativeHandle,
    offset: u64,
    stride: u64,
    count: u64,
    instance_kind: i32,
    structures: *const *mut NativeHandle,
    structure_count: usize,
    has_motion_transforms: u8,
    motion_transform_buffer: *mut NativeHandle,
    motion_transform_offset: u64,
    motion_transform_count: u64,
    refit: u8,
}

#[repr(C)]
struct RawBuildDescriptor {
    kind: i32,
    primitive: *const RawPrimitiveDescriptor,
    instances: *const RawInstancesDescriptor,
}

#[cfg(target_vendor = "apple")]
#[repr(C)]
#[derive(Default)]
struct RawSizes {
    acceleration_structure_size: u64,
    build_scratch_buffer_size: u64,
    refit_scratch_buffer_size: u64,
}

#[repr(C)]
#[cfg(target_vendor = "apple")]
#[derive(Default)]
struct RawInstanceLayout {
    values: [i64; 12],
}

struct PreparedDescriptor {
    primitive: Option<RawPrimitiveDescriptor>,
    instances: Option<RawInstancesDescriptor>,
    geometries: Vec<RawGeometry>,
    keyframes: Vec<Vec<RawKeyframe>>,
    secondary_keyframes: Vec<Vec<RawKeyframe>>,
    structure_handles: Vec<*mut NativeHandle>,
}

impl PreparedDescriptor {
    fn raw(&self) -> RawBuildDescriptor {
        RawBuildDescriptor {
            kind: if self.primitive.is_some() { 0 } else { 1 },
            primitive: self.primitive.as_ref().map_or(null(), |value| value as *const _),
            instances: self.instances.as_ref().map_or(null(), |value| value as *const _),
        }
    }
}

fn raw_keyframes(values: &[Keyframe]) -> Result<Vec<RawKeyframe>, Error> {
    values
        .iter()
        .map(|value| {
            Ok(RawKeyframe {
                buffer: value.buffer.as_native_ptr()?,
                offset: value.offset,
            })
        })
        .collect()
}

fn prepare_descriptor(descriptor: &Descriptor) -> Result<PreparedDescriptor, Error> {
    match descriptor {
        Descriptor::Primitive(value) => {
            if value.geometries.is_empty() {
                return Err(Error::InvalidArgument("primitive descriptor requires geometry"));
            }
            let expected = match value.motion {
                Some(motion) if motion.keyframe_count >= 2 => usize::try_from(motion.keyframe_count)
                    .map_err(|_| Error::InvalidArgument("motion keyframe count exceeds native bounds"))?,
                Some(_) => return Err(Error::InvalidArgument("motion requires at least two keyframes")),
                None => 1,
            };
            for geometry in &value.geometries {
                let (primary, secondary) = match geometry {
                    Geometry::Triangles { vertices, .. } => (vertices.len(), None),
                    Geometry::BoundingBoxes { boxes, .. } => (boxes.len(), None),
                    Geometry::Curves { control_points, radii, .. } => {
                        (control_points.len(), Some(radii.len()))
                    }
                };
                if primary != expected || secondary.is_some_and(|count| count != expected) {
                    return Err(Error::InvalidArgument("acceleration geometry keyframe counts do not match the descriptor"));
                }
            }
            let mut prepared = PreparedDescriptor {
                primitive: None,
                instances: None,
                geometries: Vec::with_capacity(value.geometries.len()),
                keyframes: Vec::with_capacity(value.geometries.len()),
                secondary_keyframes: Vec::with_capacity(value.geometries.len()),
                structure_handles: Vec::new(),
            };
            for geometry in &value.geometries {
                let (primary, secondary) = match geometry {
                    Geometry::Triangles { vertices, .. } => (vertices.as_slice(), &[][..]),
                    Geometry::BoundingBoxes { boxes, .. } => (boxes.as_slice(), &[][..]),
                    Geometry::Curves { control_points, radii, .. } => {
                        (control_points.as_slice(), radii.as_slice())
                    }
                };
                prepared.keyframes.push(if primary.len() > 1 {
                    raw_keyframes(primary)?
                } else {
                    Vec::new()
                });
                prepared.secondary_keyframes.push(if secondary.len() > 1 {
                    raw_keyframes(secondary)?
                } else {
                    Vec::new()
                });
            }
            for (index, geometry) in value.geometries.iter().enumerate() {
                let keyframes = &prepared.keyframes[index];
                let secondary_keyframes = &prepared.secondary_keyframes[index];
                let keyframe_ptr = if keyframes.is_empty() { null() } else { keyframes.as_ptr() };
                let secondary_keyframe_ptr = if secondary_keyframes.is_empty() {
                    null()
                } else {
                    secondary_keyframes.as_ptr()
                };
                let raw = match geometry {
                    Geometry::Triangles {
                        vertices,
                        vertex_stride,
                        triangle_count,
                        index,
                        common,
                    } => {
                        let first = &vertices[0];
                        RawGeometry {
                            kind: 0,
                            buffer: first.buffer.as_native_ptr()?,
                            offset: first.offset,
                            stride: *vertex_stride,
                            count: *triangle_count,
                            secondary_buffer: null_mut(),
                            secondary_offset: 0,
                            secondary_stride: 0,
                            index_buffer: index
                                .as_ref()
                                .map_or(Ok(null_mut()), |value| value.buffer.as_native_ptr())?,
                            index_offset: index.as_ref().map_or(0, |value| value.offset),
                            index_uint16: index.as_ref().is_some_and(|value| value.uint16).into(),
                            segment_count: 0,
                            segment_control_points: 0,
                            curve_type: 0,
                            basis: 0,
                            end_caps: 0,
                            keyframes: keyframe_ptr,
                            keyframe_count: keyframes.len(),
                            secondary_keyframes: null(),
                            secondary_keyframe_count: 0,
                            opaque: common.opaque.into(),
                            allow_duplicate_intersection: common.allow_duplicate_intersection.into(),
                        }
                    }
                    Geometry::BoundingBoxes {
                        boxes,
                        stride,
                        count,
                        common,
                    } => {
                        let first = &boxes[0];
                        RawGeometry {
                            kind: 1,
                            buffer: first.buffer.as_native_ptr()?,
                            offset: first.offset,
                            stride: *stride,
                            count: *count,
                            secondary_buffer: null_mut(),
                            secondary_offset: 0,
                            secondary_stride: 0,
                            index_buffer: null_mut(),
                            index_offset: 0,
                            index_uint16: 0,
                            segment_count: 0,
                            segment_control_points: 0,
                            curve_type: 0,
                            basis: 0,
                            end_caps: 0,
                            keyframes: keyframe_ptr,
                            keyframe_count: keyframes.len(),
                            secondary_keyframes: null(),
                            secondary_keyframe_count: 0,
                            opaque: common.opaque.into(),
                            allow_duplicate_intersection: common.allow_duplicate_intersection.into(),
                        }
                    }
                    Geometry::Curves {
                        control_points,
                        control_stride,
                        control_point_count,
                        radii,
                        radius_stride,
                        index,
                        segment_count,
                        control_points_per_segment,
                        curve_type,
                        basis,
                        end_caps,
                        common,
                    } => {
                        let control = &control_points[0];
                        let radius = &radii[0];
                        RawGeometry {
                            kind: 2,
                            buffer: control.buffer.as_native_ptr()?,
                            offset: control.offset,
                            stride: *control_stride,
                            count: *control_point_count,
                            secondary_buffer: radius.buffer.as_native_ptr()?,
                            secondary_offset: radius.offset,
                            secondary_stride: *radius_stride,
                            index_buffer: index.buffer.as_native_ptr()?,
                            index_offset: index.offset,
                            index_uint16: index.uint16.into(),
                            segment_count: *segment_count,
                            segment_control_points: *control_points_per_segment,
                            curve_type: *curve_type,
                            basis: *basis,
                            end_caps: *end_caps,
                            keyframes: keyframe_ptr,
                            keyframe_count: keyframes.len(),
                            secondary_keyframes: secondary_keyframe_ptr,
                            secondary_keyframe_count: secondary_keyframes.len(),
                            opaque: common.opaque.into(),
                            allow_duplicate_intersection: common.allow_duplicate_intersection.into(),
                        }
                    }
                };
                prepared.geometries.push(raw);
            }
            let motion = value.motion.unwrap_or(Motion {
                keyframe_count: 0,
                start_time: 0.0,
                end_time: 0.0,
                start_border: 0,
                end_border: 0,
            });
            let raw_motion = RawMotion {
                keyframe_count: motion.keyframe_count,
                start_time: motion.start_time,
                end_time: motion.end_time,
                start_border: motion.start_border,
                end_border: motion.end_border,
            };
            prepared.primitive = Some(RawPrimitiveDescriptor {
                geometries: if prepared.geometries.is_empty() {
                    null()
                } else {
                    prepared.geometries.as_ptr()
                },
                geometry_count: prepared.geometries.len(),
                has_motion: value.motion.is_some().into(),
                motion: raw_motion,
                refit: value.refit.into(),
                fast_build: value.fast_build.into(),
            });
            Ok(prepared)
        }
        Descriptor::Instances(value) => {
            if value.structures.is_empty() {
                return Err(Error::InvalidArgument("instance descriptor requires structures"));
            }
            let buffer = value.buffer.as_native_ptr()?;
            let mut prepared = PreparedDescriptor {
                primitive: None,
                instances: None,
                geometries: Vec::new(),
                keyframes: Vec::new(),
                secondary_keyframes: Vec::new(),
                structure_handles: value
                    .structures
                    .iter()
                    .map(Handle::as_native_ptr)
                    .collect::<Result<Vec<_>, _>>()?,
            };
            let (has_motion_transforms, motion_transform_buffer, motion_transform_offset, motion_transform_count) =
                match &value.motion_transforms {
                    Some(transforms) => (
                        1,
                        transforms.buffer.as_native_ptr()?,
                        transforms.offset,
                        transforms.count,
                    ),
                    None => (0, null_mut(), 0, 0),
                };
            prepared.instances = Some(RawInstancesDescriptor {
                buffer,
                offset: value.offset,
                stride: value.stride,
                count: value.count,
                instance_kind: value.instance_kind,
                structures: if prepared.structure_handles.is_empty() {
                    null()
                } else {
                    prepared.structure_handles.as_ptr()
                },
                structure_count: prepared.structure_handles.len(),
                has_motion_transforms,
                motion_transform_buffer,
                motion_transform_offset,
                motion_transform_count,
                refit: value.refit.into(),
            });
            Ok(prepared)
        }
    }
}

#[cfg(target_vendor = "apple")]
unsafe extern "C" {
    fn rays_metal_acceleration_descriptor_sizes(
        device: *mut NativeHandle,
        descriptor: *const RawBuildDescriptor,
        output: *mut RawSizes,
        message: *mut *const c_char,
    ) -> i32;
    fn rays_metal_acceleration_encoder_build(
        encoder: *mut NativeHandle,
        destination: *mut NativeHandle,
        descriptor: *const RawBuildDescriptor,
        scratch: *mut NativeHandle,
        scratch_offset: u64,
        refit: u8,
        message: *mut *const c_char,
    ) -> i32;
    fn rays_metal_acceleration_encoder_write_size(
        encoder: *mut NativeHandle,
        structure: *mut NativeHandle,
        buffer: *mut NativeHandle,
        offset: u64,
        size_data_type: i64,
        message: *mut *const c_char,
    ) -> i32;
    fn rays_metal_acceleration_instance_layout(
        kind: i32,
        output: *mut RawInstanceLayout,
        message: *mut *const c_char,
    ) -> i32;
}

pub fn descriptor_sizes(device: &Handle, descriptor: &Descriptor) -> Result<Sizes, Error> {
    let prepared = prepare_descriptor(descriptor)?;
    let raw = prepared.raw();
    #[cfg(target_vendor = "apple")]
    {
        let mut output = RawSizes::default();
        let mut message = std::ptr::null();
        let status = unsafe {
            rays_metal_acceleration_descriptor_sizes(
                device.as_native_ptr()?,
                &raw,
                &mut output,
                &mut message,
            )
        };
        if status != 0 {
            return Err(unsafe { Error::from_native(status, message) });
        }
        Ok(Sizes {
            acceleration_structure_size: output.acceleration_structure_size,
            build_scratch_buffer_size: output.build_scratch_buffer_size,
            refit_scratch_buffer_size: output.refit_scratch_buffer_size,
        })
    }
    #[cfg(not(target_vendor = "apple"))]
    {
        let _ = (device, raw);
        Err(Error::Unavailable)
    }
}

pub fn encoder_build(
    encoder: &Handle,
    destination: &Handle,
    descriptor: &Descriptor,
    scratch: &Handle,
    scratch_offset: u64,
    refit: bool,
) -> Result<(), Error> {
    let prepared = prepare_descriptor(descriptor)?;
    let raw = prepared.raw();
    #[cfg(target_vendor = "apple")]
    {
        let mut message = std::ptr::null();
        let status = unsafe {
            rays_metal_acceleration_encoder_build(
                encoder.as_native_ptr()?,
                destination.as_native_ptr()?,
                &raw,
                scratch.as_native_ptr()?,
                scratch_offset,
                refit.into(),
                &mut message,
            )
        };
        if status == 0 {
            Ok(())
        } else {
            Err(unsafe { Error::from_native(status, message) })
        }
    }
    #[cfg(not(target_vendor = "apple"))]
    {
        let _ = (encoder, destination, raw, scratch, scratch_offset, refit);
        Err(Error::Unavailable)
    }
}

pub fn encoder_write_compacted_size(
    encoder: &Handle,
    structure: &Handle,
    buffer: &Handle,
    offset: u64,
    uint64: bool,
) -> Result<(), Error> {
    #[cfg(target_vendor = "apple")]
    {
        let mut message = std::ptr::null();
        let data_type = if uint64 { 0x55 } else { 0x21 };
        let status = unsafe {
            rays_metal_acceleration_encoder_write_size(
                encoder.as_native_ptr()?,
                structure.as_native_ptr()?,
                buffer.as_native_ptr()?,
                offset,
                data_type,
                &mut message,
            )
        };
        if status == 0 {
            Ok(())
        } else {
            Err(unsafe { Error::from_native(status, message) })
        }
    }
    #[cfg(not(target_vendor = "apple"))]
    {
        let _ = (encoder, structure, buffer, offset, uint64);
        Err(Error::Unavailable)
    }
}

pub fn instance_layout(kind: i32) -> Result<[i64; 12], Error> {
    #[cfg(target_vendor = "apple")]
    {
        let mut output = RawInstanceLayout::default();
        let mut message = std::ptr::null();
        let status = unsafe { rays_metal_acceleration_instance_layout(kind, &mut output, &mut message) };
        if status == 0 {
            Ok(output.values)
        } else {
            Err(unsafe { Error::from_native(status, message) })
        }
    }
    #[cfg(not(target_vendor = "apple"))]
    {
        let _ = kind;
        Err(Error::Unavailable)
    }
}
