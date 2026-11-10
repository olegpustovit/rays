use super::{Error, Handle, NativeHandle};
#[cfg(target_vendor = "apple")]
use std::ffi::c_char;
use std::ptr::{null, null_mut};

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

use ogpu_core::acceleration::{AdmittedBuild, BuildGeometry, BuildLayout, BuildRange, InstanceKind};
use ogpu_core::backend::{Border, CurveBasis, CurveCaps, CurveType};

pub struct Descriptor {
    admission: AdmittedBuild,
    device: Handle,
    _buffers: Vec<Handle>,
    _structures: Vec<Handle>,
    prepared: PreparedDescriptor,
}
fn admission_error(error: ogpu_core::error::Error) -> Error {
    Error::InvalidArgument(error.message.into())
}
impl Descriptor {
    pub fn create(device: &Handle, admission: AdmittedBuild, buffers: Vec<Handle>, structures: Vec<Handle>) -> Result<Self, Error> {
        // Check real OS allocation/device/kind bindings once, at their owning boundary.
        // These pointer arrays are transient ABI arguments, never admission identities.
        admission.bind_structures(structures.len()).map_err(admission_error)?;
        let pointers = buffers.iter().map(Handle::as_native_ptr).collect::<Result<Vec<_>, _>>()?;
        let structure_pointers = structures.iter().map(Handle::as_native_ptr).collect::<Result<Vec<_>, _>>()?;
        if pointers.len() != admission.buffer_lengths().len() {
            return Err(Error::InvalidArgument("native buffer bindings do not match admission".into()));
        }
        #[cfg(target_vendor = "apple")]
        {
            super::super::drain_releases()?;
            let mut message = std::ptr::null();
            let status = unsafe { rays_metal_acceleration_validate_bindings(device.as_native_ptr()?, pointers.as_ptr(),
                admission.buffer_lengths().as_ptr(), pointers.len(), structure_pointers.as_ptr(), structure_pointers.len(), &mut message) };
            if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = (&pointers, &structure_pointers); return Err(Error::Unavailable); }
        if let BuildLayout::Instances { kind, .. } = admission.layout() {
            let layout = instance_layout(instance_code(*kind))?;
            admission.bind_instance_record_size(layout[0] as u64).map_err(admission_error)?;
        }
        let prepared = prepare_admitted(&admission, &buffers, &structures)?;
        Ok(Self { admission, device:device.clone(), _buffers:buffers, _structures:structures, prepared })
    }
    pub fn admission(&self) -> &AdmittedBuild { &self.admission }
    fn for_device(&self, device: &Handle) -> Result<(), Error> {
        if self.device.same(device) || super::super::registry::device_registry_id(&self.device)?
            == super::super::registry::device_registry_id(device)? { Ok(()) }
        else { Err(Error::InvalidArgument("acceleration descriptor belongs to another device".into())) }
    }
}
fn border_code(border: Border) -> i32 { match border { Border::Clamp => 0, Border::Vanish => 1 } }
fn instance_code(kind: InstanceKind) -> i32 { match kind { InstanceKind::UserIdInstances => 1, InstanceKind::MotionInstances => 2 } }
fn raw_admitted_frames(ranges: &[BuildRange], buffers: &[Handle]) -> Result<Vec<RawKeyframe>, Error> {
    ranges.iter().map(|range| Ok(RawKeyframe { buffer:buffers[range.binding].as_native_ptr()?, offset:range.offset })).collect()
}
fn prepare_admitted(admission: &AdmittedBuild, buffers: &[Handle], structures: &[Handle]) -> Result<PreparedDescriptor, Error> {
    let mut prepared = PreparedDescriptor { primitive:None,instances:None,geometries:Vec::new(),
        keyframes:Vec::new(),secondary_keyframes:Vec::new(),structure_handles:Vec::new() };
    let pointer = |range: &BuildRange| buffers[range.binding].as_native_ptr();
    match admission.layout() {
        BuildLayout::Primitive { geometries, motion, refit, fast_build } => {
            for geometry in geometries {
                let (primary, secondary) = match geometry {
                    BuildGeometry::Triangles { vertices,.. } => (vertices.as_slice(),&[][..]),
                    BuildGeometry::BoundingBoxes { boxes,.. } => (boxes.as_slice(),&[][..]),
                    BuildGeometry::Curves { control_points,radii,.. } => (control_points.as_slice(),radii.as_slice()),
                };
                prepared.keyframes.push(if primary.len()>1 { raw_admitted_frames(primary,buffers)? } else { Vec::new() });
                prepared.secondary_keyframes.push(if secondary.len()>1 { raw_admitted_frames(secondary,buffers)? } else { Vec::new() });
            }
            for (i,geometry) in geometries.iter().enumerate() {
                let frames=&prepared.keyframes[i];
                let secondary=&prepared.secondary_keyframes[i];
                let mut raw=RawGeometry { kind:0,buffer:null_mut(),offset:0,stride:0,count:0,secondary_buffer:null_mut(),
                    secondary_offset:0,secondary_stride:0,index_buffer:null_mut(),index_offset:0,index_uint16:0,
                    segment_count:0,segment_control_points:0,curve_type:0,basis:0,end_caps:0,
                    keyframes:if frames.is_empty() { null() } else { frames.as_ptr() },keyframe_count:frames.len(),
                    secondary_keyframes:if secondary.is_empty() { null() } else { secondary.as_ptr() },secondary_keyframe_count:secondary.len(),
                    opaque:0,allow_duplicate_intersection:0 };
                let common=match geometry {
                    BuildGeometry::Triangles { vertices,vertex_stride,triangle_count,index,common } => {
                        raw.buffer=pointer(&vertices[0])?; raw.offset=vertices[0].offset;
                        raw.stride=*vertex_stride; raw.count=*triangle_count;
                        if let Some(index)=index {
                            raw.index_buffer=pointer(&index.range)?; raw.index_offset=index.range.offset; raw.index_uint16=index.uint16.into();
                        }
                        common
                    }
                    BuildGeometry::BoundingBoxes { boxes,stride,count,common } => {
                        raw.kind=1; raw.buffer=pointer(&boxes[0])?; raw.offset=boxes[0].offset; raw.stride=*stride; raw.count=*count; common
                    }
                    BuildGeometry::Curves { control_points,control_stride,control_point_count,radii,radius_stride,index,
                        segment_count,control_points_per_segment,curve_type,basis,end_caps,common } => {
                        raw.kind=2; raw.buffer=pointer(&control_points[0])?; raw.offset=control_points[0].offset;
                        raw.stride=*control_stride; raw.count=*control_point_count;
                        raw.secondary_buffer=pointer(&radii[0])?; raw.secondary_offset=radii[0].offset; raw.secondary_stride=*radius_stride;
                        raw.index_buffer=pointer(&index.range)?; raw.index_offset=index.range.offset; raw.index_uint16=index.uint16.into();
                        raw.segment_count=*segment_count; raw.segment_control_points=*control_points_per_segment;
                        raw.curve_type=match curve_type { CurveType::RoundCurve=>0,CurveType::FlatCurve=>1 };
                        raw.basis=match basis { CurveBasis::Bspline=>0,CurveBasis::CatmullRom=>1,CurveBasis::LinearBasis=>2,CurveBasis::Bezier=>3 };
                        raw.end_caps=match end_caps { CurveCaps::NoCaps=>0,CurveCaps::DiskCaps=>1,CurveCaps::SphereCaps=>2 };
                        common
                    }
                };
                raw.opaque=common.opaque.into(); raw.allow_duplicate_intersection=common.duplicate_intersection.into();
                prepared.geometries.push(raw);
            }
            prepared.primitive=Some(RawPrimitiveDescriptor { geometries:prepared.geometries.as_ptr(),geometry_count:prepared.geometries.len(),
                has_motion:motion.is_some().into(),motion:match motion {
                    Some(m)=>RawMotion { keyframe_count:m.keyframe_count,start_time:m.start_time,end_time:m.end_time,
                        start_border:border_code(m.start_border),end_border:border_code(m.end_border) },
                    None=>RawMotion { keyframe_count:0,start_time:0.,end_time:0.,start_border:0,end_border:0 },
                },refit:(*refit).into(),fast_build:(*fast_build).into() });
        }
        BuildLayout::Instances { instances,stride,count,kind,motion_transforms,refit,.. } => {
            prepared.structure_handles=structures.iter().map(Handle::as_native_ptr).collect::<Result<Vec<_>,_>>()?;
            let (has_motion_transforms,motion_transform_buffer,motion_transform_offset,motion_transform_count)=match motion_transforms {
                Some((range,count))=>(1,pointer(range)?,range.offset,*count),None=>(0,null_mut(),0,0),
            };
            prepared.instances=Some(RawInstancesDescriptor { buffer:pointer(instances)?,offset:instances.offset,stride:*stride,count:*count,
                instance_kind:instance_code(*kind),structures:prepared.structure_handles.as_ptr(),structure_count:prepared.structure_handles.len(),
                has_motion_transforms,motion_transform_buffer,motion_transform_offset,motion_transform_count,refit:(*refit).into() });
        }
    }
    Ok(prepared)
}

#[cfg(target_vendor = "apple")]
unsafe extern "C" {
    fn rays_metal_acceleration_validate_bindings(device: *mut NativeHandle,
        buffers: *const *mut NativeHandle, declared_lengths: *const u64, buffer_count: usize,
        structures: *const *mut NativeHandle, structure_count: usize, message: *mut *const c_char) -> i32;
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
    descriptor.for_device(device)?;
    let raw=descriptor.prepared.raw();
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
    let raw=descriptor.prepared.raw();
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
