//! Ogpu_metal.Acceleration: acceleration structures (triangles, instances, procedural boxes,
//! curves, motion) built from the driver's token descriptors.
use crate::{
    buffer::Buffer,
    device::{fail, native_of, of_metal_error, Device, R},
};
use metal::acceleration as ma;
use ogpu_core::{
    backend::{DriverAccelDescriptor, DriverGeometry, DriverKeyframe, InstanceKind, Token},
    error::{Error, ErrorKind},
};
use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

struct AccelInner {
    device: Device,
    descriptor: RefCell<Option<ma::BuildDescriptor>>,
    metal: ma::AccelerationStructure,
    sizes: ma::Sizes,
    buffers: RefCell<Vec<Buffer>>,
    structures: Vec<Acceleration>,
    dead: Cell<bool>,
    submission_uses: Cell<i64>,
    destroy_requested: Cell<bool>,
}

#[derive(Clone)]
pub struct Acceleration(Rc<AccelInner>);

pub type Resolve<'a> = &'a dyn Fn(Token) -> R<Buffer>;
pub type Release = Box<dyn FnOnce()>;

pub fn validate_scratch_plan(buffer_size: i64, offset: i64, required: i64) -> R<()> {
    if buffer_size < 0 || offset < 0 || required < 0 || offset % 256 != 0 || offset > buffer_size - required {
        fail("Ogpu_metal.Acceleration.validate_scratch_plan", ErrorKind::InvalidArgument, "scratch range is invalid or insufficient")
    } else {
        Ok(())
    }
}

fn keyframes(frames: &[(Buffer, i64)]) -> Vec<ma::Keyframe> {
    frames.iter().map(|(buffer, offset)| ma::Keyframe { buffer: buffer.metal().clone(), offset: *offset as u64 }).collect()
}

fn resolve_frames(tokens: &[DriverKeyframe], resolve: Resolve) -> R<Vec<(Buffer, i64)>> {
    tokens.iter().map(|(token, offset)| resolve(*token).map(|b| (b, *offset))).collect()
}

/// Metal's `Build` validates every range; the caller already resolved tokens.
fn build_geometry(geometry: &DriverGeometry, resolve: Resolve) -> R<(ma::Geometry, Vec<Buffer>)> {
    let buffers_of = |frames: &[(Buffer, i64)]| frames.iter().map(|(b, _)| b.clone()).collect::<Vec<_>>();
    match geometry {
        DriverGeometry::Triangles { vertices, offset, length: _, vertex_stride, vertex_count } => {
            let vertices = resolve(*vertices)?;
            Ok((
                ma::Geometry::Triangles {
                    vertices: keyframes(&[(vertices.clone(), *offset)]),
                    vertex_stride: *vertex_stride as u64,
                    triangle_count: (*vertex_count / 3) as u64,
                    index: None,
                    common: ma::GeometryCommon::default(),
                },
                vec![vertices],
            ))
        }
        DriverGeometry::MotionTriangles { keyframes: tokens, vertex_stride, vertex_count } => {
            let frames = resolve_frames(tokens, resolve)?;
            Ok((
                ma::Geometry::Triangles {
                    vertices: keyframes(&frames),
                    vertex_stride: *vertex_stride as u64,
                    triangle_count: (*vertex_count / 3) as u64,
                    index: None,
                    common: ma::GeometryCommon::default(),
                },
                buffers_of(&frames),
            ))
        }
        DriverGeometry::Boxes { boxes, stride, count, opaque, duplicate } => {
            let frames = resolve_frames(boxes, resolve)?;
            Ok((
                ma::Geometry::BoundingBoxes {
                    boxes: keyframes(&frames),
                    stride: *stride as u64,
                    count: *count as u64,
                    common: ma::GeometryCommon { opaque: *opaque, allow_duplicate_intersection: *duplicate },
                },
                buffers_of(&frames),
            ))
        }
        DriverGeometry::Curves {
            control,
            control_stride,
            control_count,
            radii,
            radius_stride,
            indices,
            index_offset,
            segment_count,
            per_segment,
            curve_type,
            basis,
            caps,
        } => {
            let control = resolve_frames(control, resolve)?;
            let radii = resolve_frames(radii, resolve)?;
            let indices = resolve(*indices)?;
            let mut buffers = buffers_of(&control);
            buffers.extend(buffers_of(&radii));
            buffers.push(indices.clone());
            Ok((
                ma::Geometry::Curves {
                    control_points: keyframes(&control),
                    control_stride: *control_stride as u64,
                    control_point_count: *control_count as u64,
                    radii: keyframes(&radii),
                    radius_stride: *radius_stride as u64,
                    index: ma::Index { buffer: indices.metal().clone(), offset: *index_offset as u64, uint16: false },
                    segment_count: *segment_count as u64,
                    control_points_per_segment: *per_segment as u32,
                    curve_type: if *curve_type == 0 { ma::CurveType::Round } else { ma::CurveType::Flat },
                    basis: match *basis {
                        0 => ma::CurveBasis::Bspline,
                        1 => ma::CurveBasis::CatmullRom,
                        2 => ma::CurveBasis::Linear,
                        _ => ma::CurveBasis::Bezier,
                    },
                    end_caps: match *caps {
                        0 => ma::EndCaps::None,
                        1 => ma::EndCaps::Disk,
                        _ => ma::EndCaps::Sphere,
                    },
                    common: ma::GeometryCommon::default(),
                },
                buffers,
            ))
        }
    }
}

/// A primitive descriptor and the buffers it reads.
fn primitive_build(operation: &str, device: &Device, descriptor: &DriverAccelDescriptor, resolve: Resolve) -> R<(ma::BuildDescriptor, Vec<Buffer>)> {
    match descriptor {
        DriverAccelDescriptor::Tlas { .. } | DriverAccelDescriptor::Sized { .. } => {
            fail(operation, ErrorKind::InvalidArgument, "a primitive descriptor is required")
        }
        DriverAccelDescriptor::Blas { geometries, motion, refit } => {
            let mut converted = Vec::new();
            let mut buffers = Vec::new();
            for geometry in geometries {
                let (g, bs) = build_geometry(geometry, resolve)?;
                converted.push(g);
                buffers.extend(bs);
            }
            let motion = motion.as_ref().map(|m| ma::Motion {
                keyframe_count: m.motion_keyframes as usize,
                start_time: m.motion_start,
                end_time: m.motion_end,
                start_border: if m.motion_start_border == 0 { ma::Border::Clamp } else { ma::Border::Vanish },
                end_border: if m.motion_end_border == 0 { ma::Border::Clamp } else { ma::Border::Vanish },
            });
            let build = native_of(operation, ma::BuildDescriptor::primitive(device.metal(), converted, motion, *refit))?;
            Ok((build, buffers))
        }
    }
}

fn allocate(operation: &str, device: &Device, descriptor: Option<ma::BuildDescriptor>, sizes: ma::Sizes, buffers: Vec<Buffer>, structures: Vec<Acceleration>) -> R<Acceleration> {
    let metal = native_of(operation, ma::AccelerationStructure::create(device.metal(), sizes.acceleration_structure_size))?;
    device.attach_resource();
    Ok(Acceleration(Rc::new(AccelInner {
        device: device.clone(),
        descriptor: RefCell::new(descriptor),
        metal,
        sizes,
        buffers: RefCell::new(buffers),
        structures,
        dead: Cell::new(false),
        submission_uses: Cell::new(0),
        destroy_requested: Cell::new(false),
    })))
}

pub fn create(device: &Device, descriptor: &DriverAccelDescriptor, resolve_buffer: Resolve, resolve_structure: &dyn Fn(Token) -> R<Acceleration>) -> R<Acceleration> {
    let operation = "Ogpu_metal.Acceleration.create";
    if !device.capabilities().ray_tracing {
        return fail(operation, ErrorKind::Unsupported, "ray tracing is unavailable on this adapter");
    }
    let sizes_of = |build: &ma::BuildDescriptor| native_of(operation, build.sizes(device.metal()));
    match descriptor {
        DriverAccelDescriptor::Blas { .. } => {
            let (build, buffers) = primitive_build(operation, device, descriptor, resolve_buffer)?;
            match sizes_of(&build) {
                Err(e) => {
                    let _ = build.destroy();
                    Err(e)
                }
                Ok(sizes) => allocate(operation, device, Some(build), sizes, buffers, Vec::new()),
            }
        }
        DriverAccelDescriptor::Tlas { instances, offset, instance_count, kind, structures, motion_transforms } => {
            let instances = resolve_buffer(*instances)?;
            let structures = structures.iter().map(|t| resolve_structure(*t)).collect::<R<Vec<_>>>()?;
            if structures.iter().any(|s| s.0.descriptor.borrow().as_ref().is_some_and(|d| d.instance_kind().is_some())) {
                return fail(operation, ErrorKind::InvalidArgument, "instances must reference primitive structures");
            }
            let transforms = match motion_transforms {
                None => None,
                Some((token, offset, count)) => Some((resolve_buffer(*token)?, *offset, *count)),
            };
            let kind = match kind {
                InstanceKind::UserIdInstances => ma::InstanceKind::UserIdInstances,
                InstanceKind::MotionInstances => ma::InstanceKind::MotionInstances,
            };
            let natives: Vec<ma::AccelerationStructure> = structures.iter().map(|s| s.0.metal.clone()).collect();
            let build = native_of(
                operation,
                ma::BuildDescriptor::instances(
                    device.metal(),
                    instances.metal(),
                    *offset as u64,
                    *instance_count as u64,
                    kind,
                    transforms.as_ref().map(|(b, o, c)| (b.metal(), *o as u64, *c as u64)),
                    &natives,
                ),
            )?;
            match sizes_of(&build) {
                Err(e) => {
                    let _ = build.destroy();
                    Err(e)
                }
                Ok(sizes) => {
                    let mut buffers = vec![instances];
                    if let Some((b, _, _)) = transforms {
                        buffers.push(b);
                    }
                    allocate(operation, device, Some(build), sizes, buffers, structures)
                }
            }
        }
        DriverAccelDescriptor::Sized { size, template } => {
            let template = resolve_structure(*template)?;
            if *size <= 0 {
                return fail(operation, ErrorKind::InvalidArgument, "structure size must be positive");
            }
            let sizes = ma::Sizes { acceleration_structure_size: *size as u64, ..template.0.sizes };
            allocate(operation, device, None, sizes, template.0.buffers.borrow().clone(), template.0.structures.clone())
        }
    }
}

fn finish_destroy(value: &AccelInner) -> R<()> {
    let operation = "Ogpu_metal.Acceleration.destroy";
    let released = match value.descriptor.borrow().as_ref() {
        None => Ok(()),
        Some(d) => d.destroy(),
    };
    released.map_err(|e| of_metal_error(operation, &e))?;
    value.metal.destroy().map_err(|e| of_metal_error(operation, &e))?;
    value.device.detach_resource();
    Ok(())
}

impl Acceleration {
    pub fn sizes(&self) -> ma::Sizes {
        self.0.sizes
    }
    pub fn metal(&self) -> &ma::AccelerationStructure {
        &self.0.metal
    }

    fn validate(&self, operation: &str, device: &Device) -> R<()> {
        if self.0.dead.get() {
            fail(operation, ErrorKind::StaleHandle, "acceleration structure is destroyed")
        } else if device.destroyed() {
            fail(operation, ErrorKind::StaleHandle, "device is destroyed")
        } else if device.id() != self.0.device.id() {
            fail(operation, ErrorKind::CrossDevice, "acceleration structure belongs to another device")
        } else {
            Ok(())
        }
    }

    fn validate_scratch(&self, operation: &str, device: &Device, scratch: &Buffer, scratch_offset: i64, required: u64) -> R<()> {
        self.validate(operation, device)?;
        let descriptor = scratch.descriptor(device)?;
        validate_scratch_plan(descriptor.size, scratch_offset, required as i64)
    }

    pub fn encode_build(&self, encoder: &ma::AccelerationEncoder, device: &Device, scratch: &Buffer, scratch_offset: i64) -> R<()> {
        let operation = "Ogpu_metal.Acceleration.encode_build";
        self.validate_scratch(operation, device, scratch, scratch_offset, self.0.sizes.build_scratch_buffer_size)?;
        match self.0.descriptor.borrow().as_ref() {
            None => fail(operation, ErrorKind::InvalidState, "a sized structure is filled by copy or compaction"),
            Some(descriptor) => native_of(operation, encoder.build_with(&self.0.metal, descriptor, scratch.metal(), scratch_offset as u64)),
        }
    }

    /// The structure keeps the refit's descriptor (its buffers are the ones a later refit replaces).
    pub fn encode_refit(&self, encoder: &ma::AccelerationEncoder, device: &Device, descriptor: &DriverAccelDescriptor, resolve_buffer: Resolve, scratch: &Buffer, scratch_offset: i64) -> R<()> {
        let operation = "Ogpu_metal.Acceleration.encode_refit";
        self.validate_scratch(operation, device, scratch, scratch_offset, self.0.sizes.refit_scratch_buffer_size)?;
        let (build, buffers) = primitive_build(operation, device, descriptor, resolve_buffer)?;
        match encoder.refit_with(&self.0.metal, &build, scratch.metal(), scratch_offset as u64) {
            Err(e) => {
                let _ = build.destroy();
                Err(of_metal_error(operation, &e))
            }
            Ok(()) => {
                if let Some(old) = self.0.descriptor.borrow().as_ref() {
                    let _ = old.destroy();
                }
                *self.0.descriptor.borrow_mut() = Some(build);
                *self.0.buffers.borrow_mut() = buffers;
                Ok(())
            }
        }
    }

    pub fn encode_compact(encoder: &ma::AccelerationEncoder, device: &Device, src: &Acceleration, dst: &Acceleration) -> R<()> {
        let operation = "Ogpu_metal.Acceleration.encode_compact";
        src.validate(operation, device)?;
        dst.validate(operation, device)?;
        native_of(operation, encoder.copy_and_compact(&src.0.metal, &dst.0.metal))
    }

    pub fn encode_compacted_size(&self, encoder: &ma::AccelerationEncoder, device: &Device, destination: &Buffer, offset: i64) -> R<()> {
        let operation = "Ogpu_metal.Acceleration.encode_compacted_size";
        self.validate(operation, device)?;
        native_of(operation, encoder.write_compacted_size_typed(&self.0.metal, destination.metal(), offset as u64, ma::CompactedSizeType::Uint64))
    }

    pub fn destroy(&self) -> R<()> {
        let value = &self.0;
        if value.dead.get() {
            Ok(())
        } else if value.submission_uses.get() > 0 {
            value.dead.set(true);
            value.destroy_requested.set(true);
            Ok(())
        } else {
            finish_destroy(value)?;
            value.dead.set(true);
            Ok(())
        }
    }

    /// Holds the structure, its buffers and its referenced structures for one submission; the
    /// returned closure releases them all, finishing a destroy requested meanwhile.
    pub fn retain_submission(&self) -> R<Release> {
        if self.0.dead.get() {
            return fail("Ogpu_metal.Acceleration.retain_submission", ErrorKind::StaleHandle, "acceleration structure is destroyed");
        }
        let mut retained: Vec<Release> = Vec::new();
        let rollback = |retained: Vec<Release>, failure: Error| -> R<Release> {
            for release in retained {
                release();
            }
            Err(failure)
        };
        for buffer in self.0.buffers.borrow().iter() {
            if let Err(e) = buffer.retain_submission() {
                return rollback(retained, e);
            }
            let b = buffer.clone();
            retained.push(Box::new(move || b.release_submission()));
        }
        for structure in &self.0.structures {
            match structure.retain_submission() {
                Err(e) => return rollback(retained, e),
                Ok(release) => retained.push(release),
            }
        }
        self.0.submission_uses.set(self.0.submission_uses.get() + 1);
        let this = self.clone();
        Ok(Box::new(move || {
            for release in retained {
                release();
            }
            let value = &this.0;
            value.submission_uses.set(value.submission_uses.get() - 1);
            if value.submission_uses.get() == 0 && value.destroy_requested.get() {
                let _ = finish_destroy(value);
            }
        }))
    }
}
