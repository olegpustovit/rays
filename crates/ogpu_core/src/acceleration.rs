use crate::error::{self, Error, ErrorKind};
use crate::handle::{self, Device, ResourceHandle};

#[derive(Clone, Copy)]
pub enum InstanceKind { UserIdInstances, MotionInstances }

#[derive(Clone)]
pub enum Descriptor {
    Build { profile:AdmittedBuild, buffers:Vec<ResourceHandle>, structures:Vec<ResourceHandle> },
    Sized { size:std::num::NonZeroU64, template:ResourceHandle },
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

impl Acceleration {
    fn create(device: &Device, descriptor: Descriptor) -> Result<Self, Error> {
        validate_bindings(device, &descriptor)?;
        let refit = match &descriptor {
            Descriptor::Build { profile,.. } => match profile.layout() {
                BuildLayout::Primitive { refit,.. } | BuildLayout::Instances { refit,.. } => *refit,
            },
            Descriptor::Sized { .. } => false,
        };
        Ok(Self { handle:handle::create(device), descriptor, refit, state:State::Empty })
    }

    fn check(&self, operation: &str, device: &Device) -> Result<(), Error> {
        handle::validate_for(operation, device, &self.handle)
    }

    fn validate_build(&self, device: &Device) -> Result<(), Error> {
        let operation = "Ogpu.Acceleration.build";
        self.check(operation, device)?;
        match (&self.descriptor, &self.state) {
            (Descriptor::Sized { .. }, _) => Err(invalid_state(
                operation,
                "a sized structure is filled by copy or compaction",
            )),
            (_, State::Empty) => Ok(()),
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
        validate_bindings(device, descriptor)?;
        if same_shape(&self.descriptor, descriptor) {
            Ok(())
        } else {
            Err(invalid(
                operation,
                "refit geometry differs in kind, count or stride from the build",
            ))
        }
    }

    fn validate_compaction(
        source: &Self,
        destination: &Self,
        device: &Device,
    ) -> Result<(), Error> {
        let operation = "Ogpu.Acceleration.compact";
        source.check(operation, device)?;
        destination.check(operation, device)?;
        match (&source.state, &destination.state) {
            (State::Built, State::Empty) => Ok(()),
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

pub fn create(device: &Device, descriptor: Descriptor) -> Result<Acceleration, Error> {
    Acceleration::create(device, descriptor)
}

pub(crate) fn validate_build(device: &Device, value: &Acceleration) -> Result<(), Error> {
    value.validate_build(device)
}
pub(crate) fn mark_built(value: &mut Acceleration) { value.state = State::Built; }

pub fn refit(
    device: &Device,
    value: &mut Acceleration,
    descriptor: &Descriptor,
) -> Result<(), Error> {
    value.refit(device, descriptor)
}

pub(crate) fn validate_compaction(device: &Device, source: &Acceleration, destination: &Acceleration) -> Result<(), Error> {
    Acceleration::validate_compaction(source, destination, device)
}
pub(crate) fn mark_compacted(destination: &mut Acceleration) { destination.state = State::Compacted; }

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

fn validate_bindings(device: &Device, descriptor: &Descriptor) -> Result<(), Error> {
    const OP:&str="Ogpu.Acceleration.bind";
    match descriptor {
        Descriptor::Build { profile,buffers,structures } => {
            if buffers.len()!=profile.buffer_lengths().len() {
                return Err(invalid(OP,"buffer bindings do not match admitted geometry"));
            }
            profile.bind_structures(structures.len())?;
            for resource in buffers.iter().chain(structures) { handle::validate_for(OP,device,resource)?; }
            Ok(())
        }
        Descriptor::Sized { template,.. } => handle::validate_for(OP,device,template),
    }
}

fn same_shape(first: &Descriptor, second: &Descriptor) -> bool {
    let (Descriptor::Build { profile:a,.. },Descriptor::Build { profile:b,.. })=(first,second) else { return false };
    let (BuildLayout::Primitive { geometries:a,motion:am,.. },BuildLayout::Primitive { geometries:b,motion:bm,.. })=(a.layout(),b.layout()) else { return false };
    am.map(|m|m.keyframe_count)==bm.map(|m|m.keyframe_count) && a.len()==b.len() && a.iter().zip(b).all(|(a,b)| match(a,b) {
        (BuildGeometry::Triangles { vertices:av,vertex_stride:astride,triangle_count:ac,index:ai,.. },
         BuildGeometry::Triangles { vertices:bv,vertex_stride:bstride,triangle_count:bc,index:bi,.. }) =>
            av.len()==bv.len() && astride==bstride && ac==bc && ai.map(|i|i.uint16)==bi.map(|i|i.uint16),
        (BuildGeometry::BoundingBoxes { boxes:av,stride:astride,count:ac,.. },
         BuildGeometry::BoundingBoxes { boxes:bv,stride:bstride,count:bc,.. }) => av.len()==bv.len() && astride==bstride && ac==bc,
        (BuildGeometry::Curves { control_points:av,control_stride:as_,control_point_count:ac,radius_stride:ar,index:ai,segment_count:an,control_points_per_segment:ap,.. },
         BuildGeometry::Curves { control_points:bv,control_stride:bs,control_point_count:bc,radius_stride:br,index:bi,segment_count:bn,control_points_per_segment:bp,.. }) =>
            av.len()==bv.len() && as_==bs && ac==bc && ar==br && an==bn && ap==bp && ai.uint16==bi.uint16,
        _=>false,
    })
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

// Pure fields replace the public Sys geometry/motion/descriptor mirrors.
#[derive(Clone, Copy)]
pub struct BuildRange {
    pub binding: usize,
    pub offset: u64,
    pub length: u64,
}
#[derive(Clone, Copy)]
pub struct BuildIndex {
    pub range: BuildRange,
    pub uint16: bool,
}
#[derive(Clone)]
pub enum BuildGeometry {
    Triangles {
        vertices: Vec<BuildRange>,
        vertex_stride: u64,
        triangle_count: u64,
        index: Option<BuildIndex>,
        common: crate::backend::GeometryOptions,
    },
    BoundingBoxes {
        boxes: Vec<BuildRange>,
        stride: u64,
        count: u64,
        common: crate::backend::GeometryOptions,
    },
    Curves {
        control_points: Vec<BuildRange>,
        control_stride: u64,
        control_point_count: u64,
        radii: Vec<BuildRange>,
        radius_stride: u64,
        index: BuildIndex,
        segment_count: u64,
        control_points_per_segment: u32,
        curve_type: crate::backend::CurveType,
        basis: crate::backend::CurveBasis,
        end_caps: crate::backend::CurveCaps,
        common: crate::backend::GeometryOptions,
    },
}
#[derive(Clone, Copy)]
pub struct BuildMotion {
    pub keyframe_count: u64,
    pub start_time: f64,
    pub end_time: f64,
    pub start_border: crate::backend::Border,
    pub end_border: crate::backend::Border,
}
#[derive(Clone)]
pub enum BuildLayout {
    Primitive {
        geometries: Vec<BuildGeometry>,
        motion: Option<BuildMotion>,
        refit: bool,
        fast_build: bool,
    },
    Instances {
        instances: BuildRange,
        stride: u64,
        count: u64,
        // Actual ABI fact queried by the owning backend; Sys binds it to the native layout.
        record_size: u64,
        kind: InstanceKind,
        structures: usize,
        motion_transforms: Option<(BuildRange, u64)>,
        refit: bool,
    },
}
#[derive(Clone)]
pub struct AdmittedBuild(std::rc::Rc<AdmittedBuildInner>);
struct AdmittedBuildInner {
    capabilities: crate::caps::Caps,
    buffer_lengths: Vec<u64>,
    layout: BuildLayout,
}
impl AdmittedBuild {
    pub fn layout(&self) -> &BuildLayout { &self.0.layout }
    pub fn buffer_lengths(&self) -> &[u64] { &self.0.buffer_lengths }
    pub fn admitted_for(&self, capabilities: &crate::caps::Caps) -> bool {
        self.0.capabilities == *capabilities
    }
    pub fn bind_buffers(&self, actual: &[u64]) -> Result<(), Error> {
        // Bind declared extents to current OS allocations; no geometry admission is rerun.
        if actual.len() != self.0.buffer_lengths.len()
            || actual.iter().zip(&self.0.buffer_lengths).any(|(actual, declared)| actual < declared) {
            Err(invalid("Ogpu.Acceleration.bind_buffers", "native buffer bindings do not contain the admitted ranges"))
        } else { Ok(()) }
    }
    pub fn bind_instance_record_size(&self, actual: u64) -> Result<(), Error> {
        if let BuildLayout::Instances { record_size, .. } = self.layout()
            && *record_size != actual {
            Err(invalid("Ogpu.Acceleration.bind_instance_layout", "native instance ABI layout does not match admission"))
        } else { Ok(()) }
    }
    pub fn bind_structures(&self, count: usize) -> Result<(), Error> {
        let expected = match self.layout() {
            BuildLayout::Primitive { .. } => 0,
            BuildLayout::Instances { structures, .. } => *structures,
        };
        if count != expected {
            Err(invalid("Ogpu.Acceleration.bind_structures", "native structure bindings do not match admission"))
        } else { Ok(()) }
    }
}
fn build_range(lengths: &[u64], range: &BuildRange) -> Result<(), Error> {
    if range.length == 0
        || lengths.get(range.binding).is_none_or(|size| range.offset.checked_add(range.length).is_none_or(|end| end > *size)) {
        Err(invalid("Ogpu.Acceleration.admit_build", "buffer subrange is invalid or exceeds its allocation"))
    } else { Ok(()) }
}
fn build_elements(lengths: &[u64], range: &BuildRange, stride: u64, count: u64, element: u64) -> Result<(), Error> {
    build_range(lengths, range)?;
    let end = count.checked_sub(1).and_then(|count| count.checked_mul(stride)).and_then(|size| size.checked_add(element));
    if range.offset % 4 != 0 || stride < element || stride % 4 != 0 || end.is_none_or(|end| end > range.length) {
        Err(invalid("Ogpu.Acceleration.admit_build", "geometry elements exceed the declared subrange"))
    } else { Ok(()) }
}
fn build_frames(lengths: &[u64], ranges: &[BuildRange], expected: u64, stride: u64, count: u64, element: u64) -> Result<(), Error> {
    if ranges.len() as u128 != expected as u128 {
        return Err(invalid("Ogpu.Acceleration.admit_build", "keyframe count does not match the descriptor"));
    }
    for range in ranges { build_elements(lengths, range, stride, count, element)?; }
    Ok(())
}
pub fn admit_build(capabilities: &crate::caps::Caps, buffer_lengths: Vec<u64>, layout: BuildLayout) -> Result<AdmittedBuild, Error> {
    use crate::caps::{self, Feature};
    const OP: &str = "Ogpu.Acceleration.admit_build";
    caps::require(Some(OP), capabilities, &Feature::RayTracing)?;
    if capabilities.limits.max_buffer_size <= 0 || buffer_lengths.iter().any(|size| *size == 0 || *size > capabilities.limits.max_buffer_size as u64) {
        return Err(invalid(OP, "buffer extent is outside device limits"));
    }
    match &layout {
        BuildLayout::Primitive { geometries, motion, refit, .. } => {
            if *refit { caps::require(Some(OP), capabilities, &Feature::RayTracingRefit)?; }
            let expected = match motion {
                None => 1,
                Some(m) => {
                    caps::require(Some(OP), capabilities, &Feature::RayTracingMotion)?;
                    if m.keyframe_count < 2 || !m.start_time.is_finite() || !m.end_time.is_finite()
                        || !(m.start_time as f32).is_finite() || !(m.end_time as f32).is_finite()
                        || (m.start_time as f32) >= (m.end_time as f32) {
                        return Err(invalid(OP, "motion keyframes or time range are invalid"));
                    }
                    m.keyframe_count
                }
            };
            if geometries.is_empty() { return Err(invalid(OP, "primitive descriptor requires geometry")); }
            for geometry in geometries {
                match geometry {
                    BuildGeometry::Triangles { vertices, vertex_stride, triangle_count, index, .. } => {
                        let vertex_count = triangle_count.checked_mul(3).ok_or_else(|| invalid(OP, "triangle cardinality overflows"))?;
                        if let Some(index) = index {
                            let width = if index.uint16 { 2 } else { 4 };
                            build_range(&buffer_lengths, &index.range)?;
                            if index.range.offset % width != 0 || vertex_count.checked_mul(width).is_none_or(|size| size > index.range.length) {
                                return Err(invalid(OP, "triangle indices exceed the declared subrange"));
                            }
                            // Index CONTENT may be device-written; require addressable vertices,
                            // without a CPU readback or a false promise to validate index values.
                            if *triangle_count == 0 { return Err(invalid(OP, "triangle array is empty")); }
                            build_frames(&buffer_lengths, vertices, expected, *vertex_stride, 1, 12)?;
                        } else {
                            build_frames(&buffer_lengths, vertices, expected, *vertex_stride, vertex_count, 12)?;
                        }
                    }
                    BuildGeometry::BoundingBoxes { boxes, stride, count, .. } => {
                        build_frames(&buffer_lengths, boxes, expected, *stride, *count, 24)?;
                    }
                    BuildGeometry::Curves { control_points, control_stride, control_point_count, radii, radius_stride, index, segment_count, control_points_per_segment, .. } => {
                        caps::require(Some(OP), capabilities, &Feature::RayTracingCurves)?;
                        if *control_point_count < 2 || !(2..=4).contains(control_points_per_segment) {
                            return Err(invalid(OP, "curve control point cardinality is invalid"));
                        }
                        build_frames(&buffer_lengths, control_points, expected, *control_stride, *control_point_count, 12)?;
                        build_frames(&buffer_lengths, radii, expected, *radius_stride, *control_point_count, 4)?;
                        let width = if index.uint16 { 2 } else { 4 };
                        build_range(&buffer_lengths, &index.range)?;
                        if *segment_count == 0 || index.range.offset % width != 0
                            || segment_count.checked_mul(width).is_none_or(|size| size > index.range.length) {
                            return Err(invalid(OP, "curve indices exceed the declared subrange"));
                        }
                    }
                }
            }
        }
        BuildLayout::Instances { instances, stride, count, record_size, kind, structures, motion_transforms, refit } => {
            if *refit { caps::require(Some(OP), capabilities, &Feature::RayTracingRefit)?; }
            if *record_size == 0 || *structures == 0 { return Err(invalid(OP, "instances require a native layout and bottom-level structures")); }
            build_elements(&buffer_lengths, instances, *stride, *count, *record_size)?;
            if matches!(kind, InstanceKind::MotionInstances) {
                caps::require(Some(OP), capabilities, &Feature::RayTracingMotion)?;
                if motion_transforms.is_none() { return Err(invalid(OP, "motion instances require transforms")); }
            }
            if let Some((range, count)) = motion_transforms { build_elements(&buffer_lengths, range, 48, *count, 48)?; }
        }
    }
    Ok(AdmittedBuild(std::rc::Rc::new(AdmittedBuildInner { capabilities: *capabilities, buffer_lengths, layout })))
}

#[cfg(test)]
#[test]
fn native_build_admission_preserves_ranges_widths_and_flags() {
    use crate::backend::{GeometryOptions, Border};
    use crate::error::ErrorKind;
    let mut caps = crate::caps::minimum_m1();
    caps.ray_tracing = true;
    caps.ray_tracing_refit = true;
    caps.ray_tracing_motion = true;
    caps.ray_tracing_curves = true;
    let range = BuildRange { binding:0, offset:0, length:36 };
    let triangle = |range| BuildGeometry::Triangles { vertices:vec![range], vertex_stride:12, triangle_count:1,
        index:None, common:GeometryOptions { opaque:true, duplicate_intersection:true } };
    let primitive = |geometry| BuildLayout::Primitive { geometries:vec![geometry], motion:None, refit:true, fast_build:true };
    let invalid = |lengths, layout| assert_eq!(admit_build(&caps,lengths,layout).err().unwrap().kind,ErrorKind::InvalidArgument);
    let admitted = admit_build(&caps,vec![36],primitive(triangle(range))).unwrap();
    assert!(admitted.admitted_for(&caps));
    let mut different = caps; different.limits.max_buffer_size += 1;
    assert!(!admitted.admitted_for(&different));
    admitted.bind_buffers(&[36]).unwrap();
    assert_eq!(admitted.bind_buffers(&[35]).unwrap_err().kind,ErrorKind::InvalidArgument);
    assert_eq!(admitted.bind_buffers(&[36,36]).unwrap_err().kind,ErrorKind::InvalidArgument);
    if let BuildLayout::Primitive { refit, fast_build, geometries, .. } = admitted.layout() {
        assert!(*refit && *fast_build);
        assert!(matches!(&geometries[0],BuildGeometry::Triangles { common:GeometryOptions { opaque:true,duplicate_intersection:true },.. }));
    } else { panic!("primitive admission changed its shape"); }
    invalid(vec![36],primitive(triangle(BuildRange { length:35,..range })));
    invalid(vec![36],primitive(triangle(BuildRange { offset:u64::MAX-3,length:8,..range })));
    invalid(vec![36],BuildLayout::Primitive { geometries:Vec::new(),motion:None,refit:false,fast_build:false });
    let indexed = BuildGeometry::Triangles { vertices:vec![range], vertex_stride:12,triangle_count:1,
        index:Some(BuildIndex { range:BuildRange { binding:1,offset:2,length:6 },uint16:true }),
        common:GeometryOptions { opaque:false,duplicate_intersection:false } };
    admit_build(&caps,vec![36,8],primitive(indexed)).unwrap();
    let mut motion = BuildMotion { keyframe_count:1,start_time:0.,end_time:1.,start_border:Border::Clamp,end_border:Border::Vanish };
    invalid(vec![36],BuildLayout::Primitive { geometries:vec![triangle(range)],motion:Some(motion),refit:false,fast_build:false });
    motion.keyframe_count=2;
    invalid(vec![36],BuildLayout::Primitive { geometries:vec![triangle(range)],motion:Some(motion),refit:false,fast_build:false });
    let curve = |radii| BuildGeometry::Curves {control_points:vec![range,range],control_stride:12,control_point_count:3,
        radii,radius_stride:4,index:BuildIndex {range:BuildRange {binding:1,offset:2,length:2},uint16:true},segment_count:1,
        control_points_per_segment:3,curve_type:crate::backend::CurveType::RoundCurve,basis:crate::backend::CurveBasis::LinearBasis,
        end_caps:crate::backend::CurveCaps::SphereCaps,common:GeometryOptions {opaque:false,duplicate_intersection:true}};
    let motion_curve=|g| BuildLayout::Primitive {geometries:vec![g],motion:Some(motion),refit:true,fast_build:true};
    admit_build(&caps,vec![36,4],motion_curve(curve(vec![range,range]))).unwrap();
    invalid(vec![36,4],motion_curve(curve(vec![range])));
    invalid(vec![36],primitive(triangle(BuildRange {binding:1,..range})));
    let instances = || BuildLayout::Instances { instances:BuildRange { binding:0,offset:0,length:68 },stride:68,count:1,
        record_size:68,kind:InstanceKind::UserIdInstances,structures:1,motion_transforms:None,refit:false };
    let admitted = admit_build(&caps,vec![68],instances()).unwrap();
    admitted.bind_structures(1).unwrap();
    assert_eq!(admitted.bind_structures(0).unwrap_err().kind,ErrorKind::InvalidArgument);
    admitted.bind_instance_record_size(68).unwrap();
    assert_eq!(admitted.bind_instance_record_size(64).unwrap_err().kind,ErrorKind::InvalidArgument);
}
