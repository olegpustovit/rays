use crate::{
    acceleration::{self, Acceleration, BufferRange, Descriptor, Geometry, InstanceKind},
    error::{Error, ErrorKind},
    handle,
};

fn die(error: Error) -> ! {
    panic!("{}", crate::error::to_string(&error))
}

fn expect(kind: ErrorKind, result: Option<Error>) {
    match result {
        Some(error) if error.kind == kind => {}
        _ => panic!("unexpected acceleration result"),
    }
}

fn i32_at(bytes: &[u8], at: usize) -> i32 {
    i32::from_le_bytes(bytes[at..at + 4].try_into().unwrap())
}

fn triangles(range: &BufferRange, vertex_count: i64) -> Geometry {
    Geometry::Triangles { vertices: range.clone(), vertex_stride: 12, vertex_count }
}

fn boxes(range: &BufferRange, count: i64) -> Geometry {
    Geometry::BoundingBoxes { boxes: vec![range.clone()], stride: 24, count }
}

fn curves(range: &BufferRange, per_segment: i64) -> Geometry {
    Geometry::Curves {
        control_points: vec![range.clone()],
        control_stride: 12,
        control_point_count: 4,
        radii: vec![range.clone()],
        radius_stride: 4,
        indices: range.clone(),
        segment_count: 1,
        control_points_per_segment: per_segment,
    }
}

fn blas(geometries: Vec<Geometry>, motion_keyframes: Option<i64>) -> Descriptor {
    Descriptor::Blas { geometries, motion_keyframes }
}

fn tlas(range: &BufferRange, instance_stride: i64, instance_kind: InstanceKind, structure: &Acceleration) -> Descriptor {
    Descriptor::Tlas {
        instances: range.clone(),
        instance_stride,
        instance_count: 1,
        instance_kind,
        structures: vec![acceleration::handle(structure)],
    }
}

#[test]
fn acceleration_lifecycle_validation() {
    let d1 = handle::create_device();
    let d2 = handle::create_device();
    let range = BufferRange { buffer: handle::create(&d1), buffer_size: 4096, offset: 0, length: 1024 };
    let make = |descriptor: Descriptor| acceleration::create(&d1, true, false, descriptor);
    // One BLAS holds one geometry kind: Metal rejects a mix at sizing.
    expect(
        ErrorKind::InvalidArgument,
        make(blas(vec![triangles(&range, 3), boxes(&range, 1), curves(&range, 4)], None)).err(),
    );
    make(blas(vec![boxes(&range, 1), boxes(&range, 2)], None)).unwrap_or_else(|failure| die(failure));
    make(blas(vec![curves(&range, 4)], None)).unwrap_or_else(|failure| die(failure));
    let descriptor = blas(vec![triangles(&range, 3), triangles(&range, 6)], None);
    expect(ErrorKind::Unsupported, acceleration::create(&d1, false, false, descriptor.clone()).err());
    // Refit: only a built refittable primitive structure, from a descriptor of its shape.
    let mut refittable = acceleration::create(&d1, true, true, descriptor.clone()).unwrap_or_else(|failure| die(failure));
    expect(ErrorKind::InvalidState, acceleration::refit(&d1, &mut refittable, descriptor.clone()).err());
    acceleration::build(&d1, &mut refittable).unwrap_or_else(|failure| die(failure));
    acceleration::refit(&d1, &mut refittable, descriptor.clone()).unwrap_or_else(|failure| die(failure));
    acceleration::refit(&d1, &mut refittable, descriptor.clone()).unwrap_or_else(|failure| die(failure));
    expect(
        ErrorKind::InvalidArgument,
        acceleration::refit(&d1, &mut refittable, blas(vec![triangles(&range, 3)], None)).err(),
    );
    expect(ErrorKind::CrossDevice, acceleration::refit(&d2, &mut refittable, descriptor.clone()).err());
    let mut value = make(descriptor.clone()).unwrap_or_else(|failure| die(failure));
    acceleration::build(&d1, &mut value).unwrap_or_else(|failure| die(failure));
    expect(ErrorKind::InvalidState, acceleration::refit(&d1, &mut value, descriptor.clone()).err());
    expect(
        ErrorKind::InvalidArgument,
        acceleration::create(
            &d1,
            true,
            true,
            Descriptor::Sized { size: 512, template: acceleration::handle(&value) },
        ).err(),
    );
    expect(ErrorKind::CrossDevice, acceleration::compacted_size(&d2, &value).err());
    let mut sized = make(Descriptor::Sized { size: 512, template: acceleration::handle(&value) }).unwrap_or_else(|failure| die(failure));
    expect(ErrorKind::InvalidState, acceleration::build(&d1, &mut sized).err());
    acceleration::compacted_size(&d1, &value).unwrap_or_else(|failure| die(failure));
    acceleration::compact_into(&d1, &mut value, &mut sized).unwrap_or_else(|failure| die(failure));
    expect(ErrorKind::InvalidState, acceleration::compact_into(&d1, &mut value, &mut sized).err());
    expect(ErrorKind::InvalidState, acceleration::compacted_size(&d1, &sized).err());
    // Motion: every keyframed range must match the keyframe count.
    let motion_triangles = |keyframes: Vec<BufferRange>| Geometry::MotionTriangles {
        keyframes,
        vertex_stride: 12,
        vertex_count: 3,
    };
    make(blas(vec![motion_triangles(vec![range.clone(), range.clone()])], Some(2))).unwrap_or_else(|failure| die(failure));
    expect(ErrorKind::InvalidArgument, make(blas(vec![motion_triangles(vec![range.clone()])], Some(2))).err());
    expect(ErrorKind::InvalidArgument, make(blas(vec![triangles(&range, 3)], Some(2))).err());
    expect(ErrorKind::InvalidArgument, make(blas(vec![curves(&range, 5)], None)).err());
    for vertices in [
        BufferRange { offset: -1, ..range.clone() },
        BufferRange { offset: 2, ..range.clone() },
        BufferRange { offset: 4000, length: 128, ..range.clone() },
    ] {
        expect(
            ErrorKind::InvalidArgument,
            make(blas(
                vec![Geometry::Triangles { vertices, vertex_stride: 12, vertex_count: 3 }],
                None,
            )).err(),
        );
    }
    expect(ErrorKind::InvalidArgument, make(tlas(&range, 67, InstanceKind::UserIdInstances, &value)).err());
    make(tlas(&range, 68, InstanceKind::UserIdInstances, &value)).unwrap_or_else(|failure| die(failure));
    // Each instance kind has its own minimum record size: 44-byte motion records are valid, user-id records need 68.
    make(tlas(&range, 44, InstanceKind::MotionInstances, &value)).unwrap_or_else(|failure| die(failure));
    expect(ErrorKind::InvalidArgument, make(tlas(&range, 64, InstanceKind::UserIdInstances, &value)).err());
    // Packing follows the driver layout exactly.
    let layout = vec![68, 0, 48, 52, 60, 64, -1, -1, -1, -1, -1, -1];
    let packed = acceleration::pack_instances(
        &layout,
        &[(vec![1., 0., 0., 5., 0., 1., 0., 6., 0., 0., 1., 7.], 0xff, 2, 9)],
    );
    assert!(
        packed.len() == 68
            && i32_at(&packed, 52) == 0xff
            && i32_at(&packed, 56) == 0
            && i32_at(&packed, 60) == 2
            && i32_at(&packed, 64) == 9
            && f32::from_bits(i32_at(&packed, 36) as u32) as f64 == 5.,
        "instance record packing drift"
    );
    let motion_layout = vec![44, -1, 8, 12, 20, 24, 0, 4, 28, 32, 36, 40];
    let packed = acceleration::pack_motion_instances(&motion_layout, &[((0xff, 1, 4), (2, 3), (0, 1), (0.25, 0.75))]);
    assert!(
        packed.len() == 44
            && i32_at(&packed, 0) == 2
            && i32_at(&packed, 4) == 3
            && i32_at(&packed, 12) == 0xff
            && i32_at(&packed, 24) == 4
            && i32_at(&packed, 32) == 1
            && f32::from_bits(i32_at(&packed, 40) as u32) as f64 == 0.75,
        "motion instance packing drift"
    );
}
