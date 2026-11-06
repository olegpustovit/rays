use crate::error::{self, Error, ErrorKind};
use crate::handle::{self, Device, ResourceHandle};

#[derive(Clone)]
pub struct BufferRange {
    pub buffer: ResourceHandle,
    pub buffer_size: i64,
    pub offset: i64,
    pub length: i64,
}

#[derive(Clone)]
pub enum Geometry {
    Triangles {
        vertices: BufferRange,
        vertex_stride: i64,
        vertex_count: i64,
    },
    MotionTriangles {
        keyframes: Vec<BufferRange>,
        vertex_stride: i64,
        vertex_count: i64,
    },
    BoundingBoxes {
        boxes: Vec<BufferRange>,
        stride: i64,
        count: i64,
    },
    Curves {
        control_points: Vec<BufferRange>,
        control_stride: i64,
        control_point_count: i64,
        radii: Vec<BufferRange>,
        radius_stride: i64,
        indices: BufferRange,
        segment_count: i64,
        control_points_per_segment: i64,
    },
}

#[derive(Clone, Copy)]
pub enum InstanceKind {
    UserIdInstances,
    MotionInstances,
}

#[derive(Clone)]
pub enum Descriptor {
    Blas {
        geometries: Vec<Geometry>,
        motion_keyframes: Option<i64>,
    },
    Tlas {
        instances: BufferRange,
        instance_stride: i64,
        instance_count: i64,
        instance_kind: InstanceKind,
        structures: Vec<ResourceHandle>,
    },
    Sized {
        size: i64,
        template: ResourceHandle,
    },
}

enum State {
    Empty,
    Built,
    Compacted,
}

pub struct Acceleration {
    handle: ResourceHandle,
    descriptor: Descriptor,
    refit: bool,
    state: State,
}

fn invalid(operation: &str, message: &str) -> Error {
    error::make(operation, ErrorKind::InvalidArgument, message)
}

fn invalid_state(operation: &str, message: &str) -> Error {
    error::make(operation, ErrorKind::InvalidState, message)
}

fn validate_range(device: &Device, range: &BufferRange) -> Result<(), Error> {
    let operation = "Ogpu.Acceleration.validate_range";
    handle::validate_for(operation, device, &range.buffer)?;
    if range.buffer_size < 0 || range.offset < 0 || range.length <= 0 || range.offset % 4 != 0 {
        return Err(invalid(operation, "buffer range is invalid or unaligned"));
    }
    match range.offset.checked_add(range.length) {
        Some(end) if end <= range.buffer_size => Ok(()),
        _ => Err(invalid(operation, "buffer range exceeds its buffer")),
    }
}

fn validate_ranges(device: &Device, ranges: &[BufferRange]) -> Result<(), Error> {
    for range in ranges {
        validate_range(device, range)?;
    }
    Ok(())
}

fn keyframes_ok(expected: Option<i64>, ranges: &[BufferRange]) -> bool {
    ranges.len() as i64 == expected.unwrap_or(1)
}

fn validate_geometry(device: &Device, motion: Option<i64>, geometry: &Geometry) -> Result<(), Error> {
    match geometry {
        Geometry::Triangles {
            vertices,
            vertex_stride,
            vertex_count,
        } => {
            if *vertex_stride < 12 || *vertex_stride % 4 != 0 || *vertex_count < 3 || *vertex_count % 3 != 0 {
                Err(invalid(
                    "Ogpu.Acceleration.Triangles",
                    "triangle cardinality/stride is invalid",
                ))
            } else if motion.is_some() {
                Err(invalid(
                    "Ogpu.Acceleration.Triangles",
                    "static triangles cannot join a motion structure",
                ))
            } else {
                validate_range(device, vertices)
            }
        }
        Geometry::MotionTriangles {
            keyframes,
            vertex_stride,
            vertex_count,
        } => {
            if *vertex_stride < 12 || *vertex_stride % 4 != 0 || *vertex_count < 3 || *vertex_count % 3 != 0 {
                Err(invalid(
                    "Ogpu.Acceleration.Motion_triangles",
                    "triangle cardinality/stride is invalid",
                ))
            } else if motion.is_none() || !keyframes_ok(motion, keyframes) {
                Err(invalid(
                    "Ogpu.Acceleration.Motion_triangles",
                    "keyframe count must match the structure's motion keyframes",
                ))
            } else {
                validate_ranges(device, keyframes)
            }
        }
        Geometry::BoundingBoxes { boxes, stride, count } => {
            if *stride < 24 || *stride % 4 != 0 || *count <= 0 {
                Err(invalid(
                    "Ogpu.Acceleration.Bounding_boxes",
                    "box cardinality/stride is invalid",
                ))
            } else if !keyframes_ok(motion, boxes) {
                Err(invalid(
                    "Ogpu.Acceleration.Bounding_boxes",
                    "keyframe count must match the structure's motion keyframes",
                ))
            } else {
                validate_ranges(device, boxes)
            }
        }
        Geometry::Curves {
            control_points,
            control_stride,
            control_point_count,
            radii,
            radius_stride,
            indices,
            segment_count,
            control_points_per_segment,
        } => {
            if *control_stride < 12
                || *radius_stride < 4
                || *control_point_count < 2
                || *segment_count <= 0
                || *control_points_per_segment < 2
                || *control_points_per_segment > 4
            {
                Err(invalid(
                    "Ogpu.Acceleration.Curves",
                    "curve strides, counts, or segment shape are invalid",
                ))
            } else if !keyframes_ok(motion, control_points) || radii.len() != control_points.len() {
                Err(invalid(
                    "Ogpu.Acceleration.Curves",
                    "keyframe count must match the structure's motion keyframes",
                ))
            } else {
                validate_ranges(device, control_points)
                    .and_then(|()| validate_ranges(device, radii))
                    .and_then(|()| validate_range(device, indices))
            }
        }
    }
}

fn geometry_kind(geometry: &Geometry) -> u8 {
    match geometry {
        Geometry::Triangles { .. } | Geometry::MotionTriangles { .. } => 0,
        Geometry::BoundingBoxes { .. } => 1,
        Geometry::Curves { .. } => 2,
    }
}

fn validate_descriptor(device: &Device, descriptor: &Descriptor) -> Result<(), Error> {
    match descriptor {
        Descriptor::Blas {
            geometries,
            ..
        } if geometries.is_empty() => Err(invalid(
            "Ogpu.Acceleration.create",
            "BLAS geometry array is empty",
        )),
        Descriptor::Blas {
            motion_keyframes: Some(keyframes),
            ..
        } if *keyframes < 2 => Err(invalid(
            "Ogpu.Acceleration.create",
            "motion requires at least two keyframes",
        )),
        Descriptor::Blas { geometries, .. }
            if geometries
                .iter()
                .any(|geometry| geometry_kind(geometry) != geometry_kind(&geometries[0])) =>
        {
            Err(invalid(
                "Ogpu.Acceleration.create",
                "BLAS geometries must all be one kind (triangles, bounding boxes, or curves)",
            ))
        }
        Descriptor::Blas {
            geometries,
            motion_keyframes,
        } => {
            for geometry in geometries {
                validate_geometry(device, *motion_keyframes, geometry)?;
            }
            Ok(())
        }
        Descriptor::Tlas {
            instances,
            instance_stride,
            instance_count,
            instance_kind,
            structures,
        } => {
            let minimum_stride = match instance_kind {
                InstanceKind::UserIdInstances => 68,
                InstanceKind::MotionInstances => 44,
            };
            if *instance_count <= 0 || *instance_stride < minimum_stride || *instance_stride % 4 != 0 {
                return Err(invalid(
                    "Ogpu.Acceleration.create",
                    "instance cardinality/stride is invalid",
                ));
            }
            if structures.is_empty() {
                return Err(invalid(
                    "Ogpu.Acceleration.create",
                    "TLAS references no structures",
                ));
            }
            validate_range(device, instances)?;
            for structure in structures {
                handle::validate_for("Ogpu.Acceleration.create", device, structure)?;
            }
            Ok(())
        }
        Descriptor::Sized { size, template } => {
            if *size <= 0 {
                Err(invalid(
                    "Ogpu.Acceleration.create",
                    "structure size must be positive",
                ))
            } else {
                handle::validate_for("Ogpu.Acceleration.create", device, template)
            }
        }
    }
}

impl Acceleration {
    fn create(
        device: &Device,
        ray_tracing: bool,
        refit: bool,
        descriptor: Descriptor,
    ) -> Result<Self, Error> {
        if !ray_tracing {
            return Err(error::make(
                "Ogpu.Acceleration.create",
                ErrorKind::Unsupported,
                "ray tracing is unsupported",
            ));
        }
        if refit && !matches!(&descriptor, Descriptor::Blas { .. }) {
            return Err(invalid(
                "Ogpu.Acceleration.create",
                "only a primitive structure is refittable",
            ));
        }
        validate_descriptor(device, &descriptor)?;
        Ok(Self {
            handle: handle::create(device),
            descriptor,
            refit,
            state: State::Empty,
        })
    }

    fn check(&self, operation: &str, device: &Device) -> Result<(), Error> {
        handle::validate_for(operation, device, &self.handle)
    }

    fn build(&mut self, device: &Device) -> Result<(), Error> {
        let operation = "Ogpu.Acceleration.build";
        self.check(operation, device)?;
        match (&self.descriptor, &self.state) {
            (Descriptor::Sized { .. }, _) => Err(invalid_state(
                operation,
                "a sized structure is filled by copy or compaction",
            )),
            (_, State::Empty) => {
                self.state = State::Built;
                Ok(())
            }
            (_, State::Built | State::Compacted) => Err(invalid_state(
                operation,
                "structure has already been built",
            )),
        }
    }

    fn refit(&mut self, device: &Device, descriptor: &Descriptor) -> Result<(), Error> {
        let operation = "Ogpu.Acceleration.refit";
        self.check(operation, device)?;
        if !self.refit {
            return Err(invalid_state(operation, "structure was not created refittable"));
        }
        if !matches!(self.state, State::Built) {
            return Err(invalid_state(
                operation,
                "refit requires a built, uncompacted structure",
            ));
        }
        validate_descriptor(device, descriptor)?;
        if same_shape(&self.descriptor, descriptor) {
            Ok(())
        } else {
            Err(invalid(
                operation,
                "refit geometry differs in kind, count or stride from the build",
            ))
        }
    }

    fn compact_into(
        source: &mut Self,
        destination: &mut Self,
        device: &Device,
    ) -> Result<(), Error> {
        let operation = "Ogpu.Acceleration.compact";
        source.check(operation, device)?;
        destination.check(operation, device)?;
        match (&source.state, &destination.state) {
            (State::Built, State::Empty) => {
                destination.state = State::Compacted;
                Ok(())
            }
            (State::Empty | State::Compacted, _) => Err(invalid_state(
                operation,
                "compaction requires an uncompacted built source",
            )),
            (_, State::Built | State::Compacted) => Err(invalid_state(
                operation,
                "compaction destination is already filled",
            )),
        }
    }

    fn compacted_size(&self, device: &Device) -> Result<(), Error> {
        let operation = "Ogpu.Acceleration.compacted_size";
        self.check(operation, device)?;
        match &self.state {
            State::Built => Ok(()),
            State::Empty | State::Compacted => Err(invalid_state(
                operation,
                "compacted size requires an uncompacted built structure",
            )),
        }
    }

    fn destroy(&self) {
        handle::destroy(&self.handle);
    }

    fn handle(&self) -> ResourceHandle {
        self.handle.clone()
    }

    fn built(&self) -> bool {
        matches!(self.state, State::Built | State::Compacted)
    }

    fn descriptor(&self) -> &Descriptor {
        &self.descriptor
    }
}

pub fn create(
    device: &Device,
    ray_tracing: bool,
    refit: bool,
    descriptor: Descriptor,
) -> Result<Acceleration, Error> {
    Acceleration::create(device, ray_tracing, refit, descriptor)
}

pub fn build(device: &Device, value: &mut Acceleration) -> Result<(), Error> {
    value.build(device)
}

pub fn refit(
    device: &Device,
    value: &mut Acceleration,
    descriptor: Descriptor,
) -> Result<(), Error> {
    value.refit(device, &descriptor)
}

pub fn compact_into(
    device: &Device,
    source: &mut Acceleration,
    destination: &mut Acceleration,
) -> Result<(), Error> {
    Acceleration::compact_into(source, destination, device)
}

pub fn compacted_size(device: &Device, value: &Acceleration) -> Result<(), Error> {
    value.compacted_size(device)
}

pub fn destroy(value: &Acceleration) {
    value.destroy();
}

pub fn handle(value: &Acceleration) -> ResourceHandle {
    value.handle()
}

pub fn built(value: &Acceleration) -> bool {
    value.built()
}

pub fn descriptor(value: &Acceleration) -> &Descriptor {
    value.descriptor()
}

fn same_shape(first: &Descriptor, second: &Descriptor) -> bool {
    match (first, second) {
        (
            Descriptor::Blas {
                geometries: first_geometries,
                motion_keyframes: first_motion,
            },
            Descriptor::Blas {
                geometries: second_geometries,
                motion_keyframes: second_motion,
            },
        ) => {
            first_motion == second_motion
                && first_geometries.len() == second_geometries.len()
                && first_geometries
                    .iter()
                    .zip(second_geometries)
                    .all(|(first, second)| match (first, second) {
                        (
                            Geometry::Triangles {
                                vertex_stride: first_stride,
                                vertex_count: first_count,
                                ..
                            },
                            Geometry::Triangles {
                                vertex_stride: second_stride,
                                vertex_count: second_count,
                                ..
                            },
                        ) => first_stride == second_stride && first_count == second_count,
                        (
                            Geometry::MotionTriangles {
                                vertex_stride: first_stride,
                                vertex_count: first_count,
                                ..
                            },
                            Geometry::MotionTriangles {
                                vertex_stride: second_stride,
                                vertex_count: second_count,
                                ..
                            },
                        ) => first_stride == second_stride && first_count == second_count,
                        (
                            Geometry::BoundingBoxes {
                                stride: first_stride,
                                count: first_count,
                                ..
                            },
                            Geometry::BoundingBoxes {
                                stride: second_stride,
                                count: second_count,
                                ..
                            },
                        ) => first_stride == second_stride && first_count == second_count,
                        (
                            Geometry::Curves {
                                control_stride: first_control_stride,
                                control_point_count: first_point_count,
                                radius_stride: first_radius_stride,
                                segment_count: first_segment_count,
                                control_points_per_segment: first_points_per_segment,
                                ..
                            },
                            Geometry::Curves {
                                control_stride: second_control_stride,
                                control_point_count: second_point_count,
                                radius_stride: second_radius_stride,
                                segment_count: second_segment_count,
                                control_points_per_segment: second_points_per_segment,
                                ..
                            },
                        ) => {
                            first_control_stride == second_control_stride
                                && first_point_count == second_point_count
                                && first_radius_stride == second_radius_stride
                                && first_segment_count == second_segment_count
                                && first_points_per_segment == second_points_per_segment
                        }
                        _ => false,
                    })
        }
        _ => false,
    }
}

pub type InstanceLayout = Vec<i64>;
pub type Instance = (Vec<f64>, i64, i64, i64);
pub type MotionInstance = ((i64, i64, i64), (i64, i64), (i64, i64), (f64, f64));

fn put_transform(bytes: &mut [u8], base: usize, transform: &[f64]) {
    for column in 0..4 {
        for row in 0..3 {
            let offset = base + (((column * 3) + row) * 4);
            put_float(bytes, offset, transform[(row * 4) + column]);
        }
    }
}

fn put32(bytes: &mut [u8], offset: usize, value: i64) {
    bytes[offset..offset + 4].copy_from_slice(&(value as i32).to_le_bytes());
}

fn put_record32(bytes: &mut [u8], base: usize, offset: i64, value: i64) {
    if offset >= 0 {
        let offset = usize::try_from(offset).expect("instance offset is too large");
        put32(
            bytes,
            base.checked_add(offset).expect("instance offset is too large"),
            value,
        );
    }
}

fn put_float(bytes: &mut [u8], offset: usize, value: f64) {
    bytes[offset..offset + 4].copy_from_slice(&(value as f32).to_bits().to_le_bytes());
}

fn put_record_float(bytes: &mut [u8], base: usize, offset: i64, value: f64) {
    if offset >= 0 {
        let offset = usize::try_from(offset).expect("instance offset is too large");
        put_float(
            bytes,
            base.checked_add(offset).expect("instance offset is too large"),
            value,
        );
    }
}

pub(crate) fn record_bytes(layout: &[i64], count: usize) -> (Vec<u8>, usize) {
    let size = usize::try_from(layout[0]).expect("instance record size is invalid");
    let length = count.checked_mul(size).expect("instance data is too large");
    (vec![0; length], size)
}

pub(crate) fn pack_instance_record_into(
    layout: &[i64],
    bytes: &mut [u8],
    base: usize,
    transform: &[f64],
    mask: i64,
    structure_index: i64,
    user_id: i64,
) {
    if layout[1] >= 0 {
        let offset = usize::try_from(layout[1]).expect("transform offset is too large");
        put_transform(
            bytes,
            base.checked_add(offset).expect("transform offset is too large"),
            transform,
        );
    }
    put_record32(bytes, base, layout[3], mask);
    put_record32(bytes, base, layout[4], structure_index);
    put_record32(bytes, base, layout[5], user_id);
}

pub(crate) fn pack_motion_instance_into(
    layout: &[i64],
    bytes: &mut [u8],
    base: usize,
    mask: i64,
    structure_index: i64,
    user_id: i64,
    transforms_start: i64,
    transforms_count: i64,
    start_border: i64,
    end_border: i64,
    start_time: f64,
    end_time: f64,
) {
    put_record32(bytes, base, layout[3], mask);
    put_record32(bytes, base, layout[4], structure_index);
    put_record32(bytes, base, layout[5], user_id);
    put_record32(bytes, base, layout[6], transforms_start);
    put_record32(bytes, base, layout[7], transforms_count);
    put_record32(bytes, base, layout[8], start_border);
    put_record32(bytes, base, layout[9], end_border);
    put_record_float(bytes, base, layout[10], start_time);
    put_record_float(bytes, base, layout[11], end_time);
}

pub fn pack_instances(layout: &InstanceLayout, instances: &[Instance]) -> Vec<u8> {
    let (mut bytes, size) = record_bytes(layout, instances.len());
    assert!(size != 0 || instances.is_empty(), "instance record size is zero");
    for (index, (transform, mask, structure_index, user_id)) in instances.iter().enumerate() {
        pack_instance_record_into(
            layout,
            &mut bytes,
            index * size,
            transform,
            *mask,
            *structure_index,
            *user_id,
        );
    }
    bytes
}

pub fn pack_motion_instances(layout: &InstanceLayout, instances: &[MotionInstance]) -> Vec<u8> {
    let (mut bytes, size) = record_bytes(layout, instances.len());
    assert!(size != 0 || instances.is_empty(), "instance record size is zero");
    for (index, instance) in instances.iter().enumerate() {
        let ((mask, structure_index, user_id), (transforms_start, transforms_count), (start_border, end_border), (start_time, end_time)) =
            instance;
        pack_motion_instance_into(
            layout,
            &mut bytes,
            index * size,
            *mask,
            *structure_index,
            *user_id,
            *transforms_start,
            *transforms_count,
            *start_border,
            *end_border,
            *start_time,
            *end_time,
        );
    }
    bytes
}
pub fn pack_transforms(transforms: &[Vec<f64>]) -> Vec<u8> {
    let mut bytes = vec![
        0;
        transforms
            .len()
            .checked_mul(48)
            .expect("transform data is too large")
    ];
    for (index, transform) in transforms.iter().enumerate() {
        put_transform(&mut bytes, index * 48, transform);
    }
    bytes
}
