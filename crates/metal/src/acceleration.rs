use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::metal::{error, sys_error, enter_main, Buffer, CommandBuffer, Device, Error, ErrorKind, Lifetime};
use metal_sys;


struct AccelerationStructureInner {
    raw: metal_sys::Handle,
    lifetime: Lifetime,
    device: Device,
    size: u64,
    device_attached: AtomicBool,
}
impl Drop for AccelerationStructureInner {
    fn drop(&mut self) {
        self.lifetime.destroyed.store(true, Ordering::Release);
        if self.device_attached.swap(false, Ordering::AcqRel) {
            self.device.detach_child();
        }
    }
}

#[derive(Clone)]
pub struct AccelerationStructure(Arc<AccelerationStructureInner>);
impl AccelerationStructure {
    pub fn create(device: &Device, size: u64) -> Result<Self, Error> {
        const OP: &str = "Metal.Acceleration_structure.create";
        enter_main(OP)?;
        device.ensure_live(OP)?;
        if size == 0 {
            return Err(error(OP, ErrorKind::InvalidArgument, "size must be positive"));
        }
        let raw = metal_sys::registry::acceleration_structure_create(device.raw(), size).map_err(|failure| sys_error(OP, failure))?;
        device.attach_child(OP)?;
        Ok(Self(Arc::new(AccelerationStructureInner {
            raw,
            lifetime: Lifetime::new(),
            device: device.clone(),
            size,
            device_attached: AtomicBool::new(true),
        })))
    }

    pub fn size(&self) -> u64 {
        self.0.size
    }

    pub fn is_destroyed(&self) -> bool {
        self.0.lifetime.destroyed.load(Ordering::Acquire)
    }

    pub fn destroy(&self) -> Result<(), Error> {
        const OP: &str = "Metal.Acceleration_structure.destroy";
        self.0.lifetime.destroy_parent(OP)?;
        if self.0.device_attached.swap(false, Ordering::AcqRel) {
            self.0.device.detach_child();
        }
        Ok(())
    }

    pub(crate) fn raw(&self) -> &metal_sys::Handle {
        &self.0.raw
    }
    pub(crate) fn lifetime(&self) -> &Lifetime {
        &self.0.lifetime
    }
    pub(crate) fn device(&self) -> &Device {
        &self.0.device
    }
}

#[derive(Clone)]
pub struct Keyframe {
    pub buffer: Buffer,
    pub offset: u64,
}

#[derive(Clone)]
pub struct Index {
    pub buffer: Buffer,
    pub offset: u64,
    pub uint16: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct GeometryCommon {
    pub opaque: bool,
    pub allow_duplicate_intersection: bool,
}
impl Default for GeometryCommon {
    fn default() -> Self {
        Self {
            opaque: true,
            allow_duplicate_intersection: false,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Border {
    Clamp,
    Vanish,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Motion {
    pub keyframe_count: usize,
    pub start_time: f64,
    pub end_time: f64,
    pub start_border: Border,
    pub end_border: Border,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CurveType {
    Round,
    Flat,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CurveBasis {
    Bspline,
    CatmullRom,
    Linear,
    Bezier,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EndCaps {
    None,
    Disk,
    Sphere,
}

#[derive(Clone)]
pub enum Geometry {
    Triangles {
        vertices: Vec<Keyframe>,
        vertex_stride: u64,
        triangle_count: u64,
        index: Option<Index>,
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
        index: Index,
        segment_count: u64,
        control_points_per_segment: u32,
        curve_type: CurveType,
        basis: CurveBasis,
        end_caps: EndCaps,
        common: GeometryCommon,
    },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InstanceKind {
    UserIdInstances,
    MotionInstances,
}
impl InstanceKind {
    fn code(self) -> i32 {
        match self {
            Self::UserIdInstances => 1,
            Self::MotionInstances => 2,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct InstanceLayout {
    pub size: u64,
    pub transform: i64,
    pub options: i64,
    pub mask: i64,
    pub structure_index: i64,
    pub user_id: i64,
    pub transforms_start: i64,
    pub transforms_count: i64,
    pub start_border_offset: i64,
    pub end_border_offset: i64,
    pub start_time_offset: i64,
    pub end_time_offset: i64,
}
impl InstanceLayout {
    fn from_native(values: [i64; 12]) -> Result<Self, Error> {
        let size = u64::try_from(values[0]).map_err(|_| {
            error(
                "Metal.Acceleration_structure.Build.instance_layout",
                ErrorKind::NativeError,
                "Metal returned an invalid instance descriptor size",
            )
        })?;
        Ok(Self {
            size,
            transform: values[1],
            options: values[2],
            mask: values[3],
            structure_index: values[4],
            user_id: values[5],
            transforms_start: values[6],
            transforms_count: values[7],
            start_border_offset: values[8],
            end_border_offset: values[9],
            start_time_offset: values[10],
            end_time_offset: values[11],
        })
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct Sizes {
    pub acceleration_structure_size: u64,
    pub build_scratch_buffer_size: u64,
    pub refit_scratch_buffer_size: u64,
}

pub struct BuildDescriptor {
    lifetime: Lifetime,
    device: Device,
    data: metal_sys::native::acceleration::Descriptor,
    buffers: Vec<Buffer>,
    structures: Vec<AccelerationStructure>,
    instance_kind: Option<InstanceKind>,
    device_attached: AtomicBool,
}
impl Drop for BuildDescriptor {
    fn drop(&mut self) {
        self.lifetime.destroyed.store(true, Ordering::Release);
        if self.device_attached.swap(false, Ordering::AcqRel) {
            self.device.detach_child();
        }
    }
}
impl BuildDescriptor {
    pub fn primitive(
        device: &Device,
        geometries: Vec<Geometry>,
        motion: Option<Motion>,
        refit: bool,
    ) -> Result<Self, Error> {
        const OP: &str = "Metal.Acceleration_structure.Build.primitive";
        device.ensure_live(OP)?;
        if geometries.is_empty() {
            return Err(error(OP, ErrorKind::InvalidArgument, "descriptor requires at least one geometry"));
        }
        if let Some(value) = motion {
            let start = value.start_time as f32;
            let end = value.end_time as f32;
            if value.keyframe_count < 2
                || !value.start_time.is_finite()
                || !value.end_time.is_finite()
                || !start.is_finite()
                || !end.is_finite()
                || start >= end
            {
                return Err(error(
                    OP,
                    ErrorKind::InvalidArgument,
                    "motion requires at least two keyframes and an increasing finite time range",
                ));
            }
        }
        let mut buffers = Vec::new();
        for geometry in &geometries {
            validate_geometry(OP, device, geometry, motion)?;
            append_geometry_buffers(geometry, &mut buffers);
        }
        let raw_geometries = geometries
            .iter()
            .map(raw_geometry)
            .collect::<Vec<_>>();
        let native_motion = motion.map(|value| {
            Ok(metal_sys::native::acceleration::Motion {
                keyframe_count: u64::try_from(value.keyframe_count).map_err(|_| {
                    error(OP, ErrorKind::InvalidArgument, "motion keyframe count exceeds native bounds")
                })?,
                start_time: value.start_time,
                end_time: value.end_time,
                start_border: border_code(value.start_border),
                end_border: border_code(value.end_border),
            })
        }).transpose()?;
        device.attach_child(OP)?;
        Ok(Self {
            lifetime: Lifetime::new(),
            device: device.clone(),
            data: metal_sys::native::acceleration::Descriptor::Primitive(metal_sys::native::acceleration::PrimitiveDescriptor {
                geometries: raw_geometries,
                motion: native_motion,
                refit,
                fast_build: false,
            }),
            buffers,
            structures: Vec::new(),
            instance_kind: None,
            device_attached: AtomicBool::new(true),
        })
    }

    pub fn instances(
        device: &Device,
        buffer: &Buffer,
        offset: u64,
        count: u64,
        kind: InstanceKind,
        motion_transforms: Option<(&Buffer, u64, u64)>,
        structures: &[AccelerationStructure],
    ) -> Result<Self, Error> {
        const OP: &str = "Metal.Acceleration_structure.Build.instances";
        device.ensure_live(OP)?;
        let layout = InstanceLayout::from_native(metal_sys::native::acceleration::instance_layout(kind.code()).map_err(|failure| sys_error(OP, failure))?)?;
        buffer.lifetime().ensure_live(OP)?;
        if !device.same(buffer.device()) {
            return Err(error(OP, ErrorKind::DeviceMismatch, "instance buffer belongs to another device"));
        }
        if structures.is_empty() {
            return Err(error(OP, ErrorKind::InvalidArgument, "instances reference no structures"));
        }
        if count == 0
            || layout.size == 0
            || offset % 4 != 0
            || !range_fits(buffer.length(), offset, layout.size, count, layout.size)
        {
            return Err(error(OP, ErrorKind::InvalidArgument, "instance range is invalid, unaligned, or exceeds its buffer"));
        }
        if kind == InstanceKind::MotionInstances && motion_transforms.is_none() {
            return Err(error(OP, ErrorKind::InvalidArgument, "motion instances require a transform buffer"));
        }
        for structure in structures {
            structure.lifetime().ensure_live(OP)?;
            if !device.same(structure.device()) {
                return Err(error(OP, ErrorKind::DeviceMismatch, "primitive belongs to another device"));
            }
        }
        let mut buffers = vec![buffer.clone()];
        let native_transforms = if let Some((transform_buffer, transform_offset, transform_count)) = motion_transforms {
            transform_buffer.lifetime().ensure_live(OP)?;
            if !device.same(transform_buffer.device()) {
                return Err(error(OP, ErrorKind::DeviceMismatch, "motion transform buffer belongs to another device"));
            }
            if !range_fits(transform_buffer.length(), transform_offset, 48, transform_count, 48) {
                return Err(error(OP, ErrorKind::InvalidArgument, "motion transform range exceeds its buffer"));
            }
            buffers.push(transform_buffer.clone());
            Some(metal_sys::native::acceleration::MotionTransforms {
                buffer: transform_buffer.raw().clone(),
                offset: transform_offset,
                count: transform_count,
            })
        } else {
            None
        };
        let raw_structures = structures.iter().map(|value| value.raw().clone()).collect();
        device.attach_child(OP)?;
        Ok(Self {
            lifetime: Lifetime::new(),
            device: device.clone(),
            data: metal_sys::native::acceleration::Descriptor::Instances(metal_sys::native::acceleration::InstancesDescriptor {
                buffer: buffer.raw().clone(),
                offset,
                stride: layout.size,
                count,
                instance_kind: kind.code(),
                structures: raw_structures,
                motion_transforms: native_transforms,
                refit: false,
            }),
            buffers,
            structures: structures.to_vec(),
            instance_kind: Some(kind),
            device_attached: AtomicBool::new(true),
        })
    }

    pub fn instance_layout(kind: InstanceKind) -> Result<InstanceLayout, Error> {
        const OP: &str = "Metal.Acceleration_structure.Build.instance_layout";
        let values = metal_sys::native::acceleration::instance_layout(kind.code()).map_err(|failure| sys_error(OP, failure))?;
        InstanceLayout::from_native(values)
    }

    pub fn sizes(&self, device: &Device) -> Result<Sizes, Error> {
        const OP: &str = "Metal.Acceleration_structure.Build.sizes";
        self.lifetime.ensure_live(OP)?;
        if !device.same(&self.device) {
            return Err(error(OP, ErrorKind::DeviceMismatch, "descriptor belongs to another device"));
        }
        let sizes = metal_sys::native::acceleration::descriptor_sizes(device.raw(), self.native_descriptor()).map_err(|failure| sys_error(OP, failure))?;
        Ok(Sizes {
            acceleration_structure_size: sizes.acceleration_structure_size,
            build_scratch_buffer_size: sizes.build_scratch_buffer_size,
            refit_scratch_buffer_size: sizes.refit_scratch_buffer_size,
        })
    }

    pub fn instance_kind(&self) -> Option<InstanceKind> {
        self.instance_kind
    }

    pub fn destroy(&self) -> Result<(), Error> {
        const OP: &str = "Metal.Acceleration_structure.Build.destroy";
        self.lifetime.destroy_parent(OP)?;
        if self.device_attached.swap(false, Ordering::AcqRel) {
            self.device.detach_child();
        }
        Ok(())
    }

    fn native_descriptor(&self) -> &metal_sys::native::acceleration::Descriptor {
        &self.data
    }

    fn validate_resources(&self, operation: &str, device: &Device) -> Result<(), Error> {
        self.lifetime.ensure_live(operation)?;
        if !device.same(&self.device) {
            return Err(error(operation, ErrorKind::DeviceMismatch, "descriptor belongs to another device"));
        }
        for buffer in &self.buffers {
            buffer.lifetime().ensure_live(operation)?;
            if !device.same(buffer.device()) {
                return Err(error(operation, ErrorKind::DeviceMismatch, "build resource belongs to another device"));
            }
        }
        for structure in &self.structures {
            structure.lifetime().ensure_live(operation)?;
            if !device.same(structure.device()) {
                return Err(error(operation, ErrorKind::DeviceMismatch, "structure belongs to another device"));
            }
        }
        Ok(())
    }
}

fn border_code(value: Border) -> i32 {
    match value {
        Border::Clamp => 0,
        Border::Vanish => 1,
    }
}

fn range_fits(length: u64, offset: u64, stride: u64, count: u64, element: u64) -> bool {
    if offset > length || stride < element || element == 0 || count == 0 {
        return false;
    }
    let remaining = length - offset;
    if element > remaining {
        return false;
    }
    count - 1 <= (remaining - element) / stride
}

fn validate_keyframes(
    operation: &str,
    device: &Device,
    name: &str,
    keyframes: &[Keyframe],
    motion: Option<Motion>,
    check: &dyn Fn(&Keyframe) -> bool,
) -> Result<(), Error> {
    let expected = motion.map_or(1, |value| value.keyframe_count);
    if keyframes.len() != expected {
        return Err(error(
            operation, ErrorKind::InvalidArgument, &format!("{name} keyframe count must match the descriptor's motion keyframes (or be one)"),
        ));
    }
    for keyframe in keyframes {
        keyframe.buffer.lifetime().ensure_live(operation)?;
        if !device.same(keyframe.buffer.device()) {
            return Err(error(operation, ErrorKind::DeviceMismatch, &format!("{name} buffer belongs to another device")));
        }
        if !check(keyframe) {
            return Err(error(operation, ErrorKind::InvalidArgument, &format!("{name} range exceeds its buffer")));
        }
    }
    Ok(())
}

fn validate_index(operation: &str, device: &Device, index: &Index, count: u64) -> Result<(), Error> {
    index.buffer.lifetime().ensure_live(operation)?;
    if !device.same(index.buffer.device()) {
        return Err(error(operation, ErrorKind::DeviceMismatch, "index buffer belongs to another device"));
    }
    let element = if index.uint16 { 2 } else { 4 };
    if index.offset % element != 0 || !range_fits(index.buffer.length(), index.offset, element, count, element) {
        return Err(error(operation, ErrorKind::InvalidArgument, "index range is invalid or exceeds its buffer"));
    }
    Ok(())
}

fn validate_geometry(operation: &str, device: &Device, geometry: &Geometry, motion: Option<Motion>) -> Result<(), Error> {
    match geometry {
        Geometry::Triangles { vertices, vertex_stride, triangle_count, index, .. } => {
            if *vertex_stride < 12 || *vertex_stride % 4 != 0 || *triangle_count == 0 || *triangle_count > u64::MAX / 3 {
                return Err(error(operation, ErrorKind::InvalidArgument, "triangle stride must be at least 12 and four-byte aligned with a positive count"));
            }
            let vertex_count = if index.is_none() { *triangle_count * 3 } else { 1 };
            validate_keyframes(operation, device, "vertex", vertices, motion, &|keyframe| {
                range_fits(keyframe.buffer.length(), keyframe.offset, *vertex_stride, vertex_count, 12)
            })?;
            if let Some(index) = index {
                validate_index(operation, device, index, *triangle_count * 3)?;
            }
        }
        Geometry::BoundingBoxes { boxes, stride, count, .. } => {
            if *stride < 24 || *stride % 4 != 0 || *count == 0 {
                return Err(error(operation, ErrorKind::InvalidArgument, "bounding box stride must be at least 24 and four-byte aligned with a positive count"));
            }
            validate_keyframes(operation, device, "bounding box", boxes, motion, &|keyframe| {
                range_fits(keyframe.buffer.length(), keyframe.offset, *stride, *count, 24)
            })?;
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
            ..
        } => {
            if *control_stride < 12 || *radius_stride < 4 || *control_point_count < 2 || *segment_count == 0 || !(2..=4).contains(control_points_per_segment) {
                return Err(error(operation, ErrorKind::InvalidArgument, "curve strides, counts, or segment shape are invalid"));
            }
            if radii.len() != control_points.len() {
                return Err(error(operation, ErrorKind::InvalidArgument, "curve radius keyframes must match control point keyframes"));
            }
            validate_keyframes(operation, device, "control point", control_points, motion, &|keyframe| {
                range_fits(keyframe.buffer.length(), keyframe.offset, *control_stride, *control_point_count, 12)
            })?;
            validate_keyframes(operation, device, "radius", radii, motion, &|keyframe| {
                range_fits(keyframe.buffer.length(), keyframe.offset, *radius_stride, *control_point_count, 4)
            })?;
            let index_count = segment_count
                .checked_mul(u64::from(*control_points_per_segment))
                .ok_or_else(|| error(operation, ErrorKind::InvalidArgument, "curve index count exceeds native bounds"))?;
            validate_index(operation, device, index, index_count)?;
        }
    }
    Ok(())
}

fn append_geometry_buffers(geometry: &Geometry, buffers: &mut Vec<Buffer>) {
    match geometry {
        Geometry::Triangles { vertices, index, .. } => {
            buffers.extend(vertices.iter().map(|value| value.buffer.clone()));
            if let Some(index) = index {
                buffers.push(index.buffer.clone());
            }
        }
        Geometry::BoundingBoxes { boxes, .. } => {
            buffers.extend(boxes.iter().map(|value| value.buffer.clone()));
        }
        Geometry::Curves { control_points, radii, index, .. } => {
            buffers.extend(control_points.iter().chain(radii).map(|value| value.buffer.clone()));
            buffers.push(index.buffer.clone());
        }
    }
}

fn raw_keyframe(value: &Keyframe) -> metal_sys::native::acceleration::Keyframe {
    metal_sys::native::acceleration::Keyframe {
        buffer: value.buffer.raw().clone(),
        offset: value.offset,
    }
}

fn raw_index(value: &Index) -> metal_sys::native::acceleration::IndexBuffer {
    metal_sys::native::acceleration::IndexBuffer {
        buffer: value.buffer.raw().clone(),
        offset: value.offset,
        uint16: value.uint16,
    }
}

fn raw_common(value: GeometryCommon) -> metal_sys::native::acceleration::GeometryCommon {
    metal_sys::native::acceleration::GeometryCommon {
        opaque: value.opaque,
        allow_duplicate_intersection: value.allow_duplicate_intersection,
    }
}

fn raw_geometry(value: &Geometry) -> metal_sys::native::acceleration::Geometry {
    match value {
        Geometry::Triangles { vertices, vertex_stride, triangle_count, index, common } => {
            metal_sys::native::acceleration::Geometry::Triangles {
                vertices: vertices.iter().map(raw_keyframe).collect(),
                vertex_stride: *vertex_stride,
                triangle_count: *triangle_count,
                index: index.as_ref().map(raw_index),
                common: raw_common(*common),
            }
        }
        Geometry::BoundingBoxes { boxes, stride, count, common } => {
            metal_sys::native::acceleration::Geometry::BoundingBoxes {
                boxes: boxes.iter().map(raw_keyframe).collect(),
                stride: *stride,
                count: *count,
                common: raw_common(*common),
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
        } => metal_sys::native::acceleration::Geometry::Curves {
            control_points: control_points.iter().map(raw_keyframe).collect(),
            control_stride: *control_stride,
            control_point_count: *control_point_count,
            radii: radii.iter().map(raw_keyframe).collect(),
            radius_stride: *radius_stride,
            index: raw_index(index),
            segment_count: *segment_count,
            control_points_per_segment: *control_points_per_segment,
            curve_type: match curve_type { CurveType::Round => 0, CurveType::Flat => 1 },
            basis: match basis { CurveBasis::Bspline => 0, CurveBasis::CatmullRom => 1, CurveBasis::Linear => 2, CurveBasis::Bezier => 3 },
            end_caps: match end_caps { EndCaps::None => 0, EndCaps::Disk => 1, EndCaps::Sphere => 2 },
            common: raw_common(*common),
        },
    }
}

struct AccelerationEncoderInner {
    raw: metal_sys::Handle,
    lifetime: Lifetime,
    command_buffer: CommandBuffer,
    command_buffer_attached: AtomicBool,
}
impl Drop for AccelerationEncoderInner {
    fn drop(&mut self) {
        self.lifetime.destroyed.store(true, Ordering::Release);
        if self.command_buffer_attached.swap(false, Ordering::AcqRel) {
            self.command_buffer.lifetime().detach();
        }
    }
}

#[derive(Clone)]
pub struct AccelerationEncoder(Arc<AccelerationEncoderInner>);
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CompactedSizeType {
    Uint32,
    Uint64,
}
impl AccelerationEncoder {
    pub fn create(command_buffer: &CommandBuffer) -> Result<Self, Error> {
        const OP: &str = "Metal.Acceleration_encoder.create";
        enter_main(OP)?;
        command_buffer.lifetime().ensure_live(OP)?;
        command_buffer.ensure_recording(OP)?;
        let raw = metal_sys::registry::command_buffer_acceleration_encoder(command_buffer.raw()).map_err(|failure| sys_error(OP, failure))?;
        command_buffer.lifetime().attach();
        Ok(Self(Arc::new(AccelerationEncoderInner {
            raw,
            lifetime: Lifetime::new(),
            command_buffer: command_buffer.clone(),
            command_buffer_attached: AtomicBool::new(true),
        })))
    }

    pub fn build_with(
        &self,
        destination: &AccelerationStructure,
        descriptor: &BuildDescriptor,
        scratch: &Buffer,
        scratch_offset: u64,
    ) -> Result<(), Error> {
        self.with_descriptor("Metal.Acceleration_encoder.build_with", destination, descriptor, scratch, scratch_offset, false)
    }

    pub fn refit_with(
        &self,
        source: &AccelerationStructure,
        descriptor: &BuildDescriptor,
        scratch: &Buffer,
        scratch_offset: u64,
    ) -> Result<(), Error> {
        self.with_descriptor("Metal.Acceleration_encoder.refit_with", source, descriptor, scratch, scratch_offset, true)
    }

    fn with_descriptor(
        &self,
        operation: &'static str,
        destination: &AccelerationStructure,
        descriptor: &BuildDescriptor,
        scratch: &Buffer,
        scratch_offset: u64,
        refit: bool,
    ) -> Result<(), Error> {
        self.0.lifetime.ensure_live(operation)?;
        let device = self.0.command_buffer.device();
        descriptor.validate_resources(operation, device)?;
        destination.lifetime().ensure_live(operation)?;
        if !device.same(destination.device()) {
            return Err(error(operation, ErrorKind::DeviceMismatch, "structure belongs to another device"));
        }
        scratch.lifetime().ensure_live(operation)?;
        if !device.same(scratch.device()) {
            return Err(error(operation, ErrorKind::DeviceMismatch, "build resource belongs to another device"));
        }
        if scratch_offset > scratch.length() {
            return Err(error(operation, ErrorKind::InvalidArgument, "scratch offset exceeds its buffer"));
        }
        let sizes = metal_sys::native::acceleration::descriptor_sizes(device.raw(), descriptor.native_descriptor()).map_err(|failure| sys_error(operation, failure))?;
        let scratch_needed = if refit { sizes.refit_scratch_buffer_size } else { sizes.build_scratch_buffer_size };
        if destination.size() < sizes.acceleration_structure_size {
            return Err(error(operation, ErrorKind::InvalidArgument, "destination acceleration structure is too small"));
        }
        if scratch_needed > scratch.length() - scratch_offset {
            return Err(error(operation, ErrorKind::InvalidArgument, "scratch range is too small"));
        }
        metal_sys::native::acceleration::encoder_build(
                &self.0.raw,
                destination.raw(),
                descriptor.native_descriptor(),
                scratch.raw(),
                scratch_offset,
                refit,
            ).map_err(|failure| sys_error(operation, failure))?;
        self.0.command_buffer.retain_buffer(scratch);
        for buffer in &descriptor.buffers {
            self.0.command_buffer.retain_buffer(buffer);
        }
        self.0.command_buffer.retain_acceleration(destination);
        for structure in &descriptor.structures {
            self.0.command_buffer.retain_acceleration(structure);
        }
        Ok(())
    }

    pub fn copy_and_compact(
        &self,
        source: &AccelerationStructure,
        destination: &AccelerationStructure,
    ) -> Result<(), Error> {
        const OP: &str = "Metal.Acceleration_encoder.copy_and_compact";
        self.0.lifetime.ensure_live(OP)?;
        let device = self.0.command_buffer.device();
        validate_structure(OP, device, source)?;
        validate_structure(OP, device, destination)?;
        metal_sys::registry::acceleration_encoder_copy_and_compact(
            &self.0.raw,
            source.raw(),
            destination.raw(),
        ).map_err(|failure| sys_error(OP, failure))?;
        self.0.command_buffer.retain_acceleration(source);
        self.0.command_buffer.retain_acceleration(destination);
        Ok(())
    }

    pub fn write_compacted_size_typed(
        &self,
        source: &AccelerationStructure,
        destination: &Buffer,
        offset: u64,
        size_type: CompactedSizeType,
    ) -> Result<(), Error> {
        const OP: &str = "Metal.Acceleration_encoder.write_compacted_size_typed";
        self.0.lifetime.ensure_live(OP)?;
        let device = self.0.command_buffer.device();
        validate_structure(OP, device, source)?;
        destination.lifetime().ensure_live(OP)?;
        if !device.same(destination.device()) {
            return Err(error(OP, ErrorKind::DeviceMismatch, "resource belongs to another device"));
        }
        let (bytes, uint64) = match size_type {
            CompactedSizeType::Uint32 => (4, false),
            CompactedSizeType::Uint64 => (8, true),
        };
        if offset % bytes != 0 || offset > destination.length() || bytes > destination.length() - offset {
            return Err(error(OP, ErrorKind::InvalidArgument, "compacted-size output range is invalid"));
        }
        metal_sys::native::acceleration::encoder_write_compacted_size(
            &self.0.raw,
            source.raw(),
            destination.raw(),
            offset,
            uint64,
        ).map_err(|failure| sys_error(OP, failure))?;
        self.0.command_buffer.retain_acceleration(source);
        self.0.command_buffer.retain_buffer(destination);
        Ok(())
    }

    pub fn end_encoding(&self) -> Result<(), Error> {
        const OP: &str = "Metal.Acceleration_encoder.end_encoding";
        self.0.lifetime.ensure_live(OP)?;
        metal_sys::registry::acceleration_encoder_end(&self.0.raw).map_err(|failure| sys_error(OP, failure))?;
        self.0.lifetime.destroyed.store(true, Ordering::Release);
        if self.0.command_buffer_attached.swap(false, Ordering::AcqRel) {
            self.0.command_buffer.lifetime().detach();
        }
        Ok(())
    }
}

fn validate_structure(operation: &str, device: &Device, value: &AccelerationStructure) -> Result<(), Error> {
    value.lifetime().ensure_live(operation)?;
    if !device.same(value.device()) {
        return Err(error(operation, ErrorKind::DeviceMismatch, "acceleration structure belongs to another device"));
    }
    Ok(())
}
