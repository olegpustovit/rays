use crate::{display_list as dl, marks, path, render_ir as ir, shape2, ui_batch as ui};
use std::sync::Arc;

fn rect_geometry(index: i64) -> ir::Geometry {
    let x = index as f64;
    ir::Geometry { vertices: vec![x, 2.0, x + 3.0, 2.0, x + 3.0, 6.0, x, 6.0], indices: vec![0, 1, 2, 0, 2, 3], color: index as i32 }
}

fn pair(x: f64, y: f64) -> marks::PairAttr { marks::PairAttr::Constant((x, y)) }
fn scalar(value: f64) -> marks::ScalarAttr { marks::ScalarAttr::Constant(value) }
fn color(value: i32) -> marks::ColorAttr { marks::ColorAttr::Constant(value) }
fn pair_buffer(id: i64, count: i64, width: i64) -> marks::PairAttr { marks::PairAttr::Buffer(marks::Buffer { id, count, width }) }
fn scalar_buffer(id: i64, count: i64, width: i64) -> marks::ScalarAttr { marks::ScalarAttr::Buffer(marks::Buffer { id, count, width }) }
fn color_buffer(id: i64, count: i64, width: i64) -> marks::ColorAttr { marks::ColorAttr::Buffer(marks::Buffer { id, count, width }) }
fn pair_device(id: i64, count: i64, width: i64) -> marks::PairAttr { marks::PairAttr::Device(marks::Buffer { id, count, width }) }
fn scalar_device(id: i64, count: i64, width: i64) -> marks::ScalarAttr { marks::ScalarAttr::Device(marks::Buffer { id, count, width }) }
fn color_device(id: i64, count: i64, width: i64) -> marks::ColorAttr { marks::ColorAttr::Device(marks::Buffer { id, count, width }) }

#[test]
fn path_and_shape_primitives_preserve_subpixels_and_contours() {
    match shape2::polygon(&[(0.25, 0.5), (3.25, 0.5), (3.25, 2.75), (0.25, 2.75)], Some(-1), None) {
        Ok(geometries) => assert!(geometries[0].vertices.contains(&0.25)),
        Err(error) => assert!(false, "{error}"),
    }
    let path = path::of_commands(&[
        path::Command::MoveTo(path::Point { x: 0.0, y: 0.0 }),
        path::Command::LineTo(path::Point { x: 4.0, y: 0.0 }),
        path::Command::LineTo(path::Point { x: 4.0, y: 4.0 }),
        path::Command::LineTo(path::Point { x: 0.0, y: 4.0 }),
        path::Command::Close,
    ]);
    match path::tessellate(0.25, path::FillRule::NonZero, &path) {
        Ok(mesh) => assert!(!mesh.vertices.is_empty() && mesh.indices.len() % 3 == 0),
        Err(error) => assert!(false, "{error:?}"),
    }
    assert_eq!(path::tessellate(0.25, path::FillRule::NonZero, &path::of_commands(&[])), Err(path::Error::EmptyPath));
    assert_eq!(path::stroke(0.25, 0.0, path::Cap::Butt, path::Join::Miter, 4.0, &path), Err(path::Error::InvalidWidth(0.0)));
}

#[test]
fn shape_float_edges_match_ocaml_nan_rules() {
    assert_eq!(
        shape2::line((0.0, 0.0), (2.0, 2.0), f64::NAN, 1),
        Err("Shape2 stroke".to_owned())
    );
    let nan_radius = shape2::rounded_rect(10.0, 8.0, f64::NAN, Some(1), None).unwrap();
    let max_radius = shape2::rounded_rect(10.0, 8.0, 4.0, Some(1), None).unwrap();
    assert_eq!(nan_radius, max_radius);
}

#[test]
fn display_list_encoding_ids_growth_publication_and_ownership() {
    let encoded = match ir::create(vec![
        ir::Command::Clear(0x89abcdefu32 as i32),
        ir::Command::Geometry(ir::Geometry { vertices: vec![-0.0, 1.0], indices: vec![], color: 0x10203040 }),
    ]) {
        Ok(value) => ir::serialize(&value),
        Err(error) => { assert!(false, "{error:?}"); return; }
    };
    assert_eq!(encoded.as_slice(), &b"\x31\x49\x32\x52\x02\x00\x00\x00\x00\x00\x00\x00\x00\xef\xcd\xab\x89\x05\x40\x30\x20\x10\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\xf0\x3f\x00\x00\x00\x00\x00\x00\x00\x00"[..]);

    let workers: Vec<_> = (0..4).map(|_| std::thread::spawn(|| (0..4096).map(|_| dl::fresh_id()).collect::<Result<Vec<_>, String>>())).collect();
    let mut ids = Vec::new();
    for worker in workers {
        match worker.join() {
            Ok(Ok(mut values)) => ids.append(&mut values),
            Ok(Err(error)) => { assert!(false, "{error}"); return; }
            Err(_) => { assert!(false, "identity worker panicked"); return; }
        }
    }
    ids.sort_unstable();
    assert!(ids.windows(2).all(|pair| pair[1] == pair[0] + 1));

    let mut builder = dl::Builder::create();
    assert!(builder.push_clip(0.0, 0.0, 64.0, 64.0).is_ok());
    for index in 0..16 { assert!(builder.geometry(&rect_geometry(index)).is_ok()); }
    assert!(builder.pop_clip().is_ok());
    let first = match builder.publish(7, 1) { Ok(value) => value, Err(error) => { assert!(false, "{error:?}"); return; } };
    let same = match builder.publish(7, 1) { Ok(value) => value, Err(error) => { assert!(false, "{error:?}"); return; } };
    assert!(Arc::ptr_eq(&first, &same));
    assert_eq!(dl::id(&same), 7);
    assert_eq!(dl::version(&same), 1);
    assert_eq!(ir::identity(dl::render_ir(&first)), ir::identity(dl::render_ir(&same)));
    let commands = ir::commands_readonly(dl::render_ir(&same));
    assert_eq!(commands.len(), 18);
    assert!(matches!(commands.first(), Some(ir::Command::PushClip(ir::Rect { x: 0.0, y: 0.0, width: 64.0, height: 64.0 }))));
    assert!(matches!(commands.last(), Some(ir::Command::PopClip)));
    for index in 0..16 {
        if let Some(ir::Command::Geometry(geometry)) = commands.get(index + 1) { assert_eq!(geometry, &rect_geometry(index as i64)); }
        else { assert!(false, "display-list command order drift"); }
    }
    let word = std::mem::size_of::<usize>();
    let expected_bytes = |capacity: usize, geometries: &[ir::Geometry]| capacity + capacity * 8 * word + capacity * 4 + capacity * word + geometries.iter().map(|g| (g.vertices.len() + g.indices.len()) * word).sum::<usize>();
    assert_eq!(dl::source_bytes(&same), expected_bytes(32, &(0..16).map(rect_geometry).collect::<Vec<_>>()) as i64);
    let first_ir = ir::identity(dl::render_ir(&same));
    builder.reset();
    let reset = match builder.publish(7, 1) { Ok(value) => value, Err(error) => { assert!(false, "{error:?}"); return; } };
    assert!(!Arc::ptr_eq(&first, &reset));
    assert_eq!(ir::commands_readonly(dl::render_ir(&reset)).len(), 0);
    assert_ne!(ir::identity(dl::render_ir(&reset)), first_ir);
    assert_eq!(dl::source_bytes(&reset), expected_bytes(32, &[]) as i64);
    assert!(builder.geometry(&rect_geometry(3)).is_ok());
    let second = match builder.publish(7, 2) { Ok(value) => value, Err(error) => { assert!(false, "{error:?}"); return; } };
    assert_eq!(dl::version(&second), 2);
    assert_eq!(ir::commands_readonly(dl::render_ir(&second)).len(), 1);
    assert_eq!(dl::source_bytes(&second), expected_bytes(32, &[rect_geometry(3)]) as i64);

    let mut invalid = dl::Builder::create();
    assert!(invalid.pop_clip().is_ok());
    assert!(matches!(invalid.publish(8, 1), Err(ir::Error::UnbalancedClip)));
    let mut complete = dl::Builder::create();
    let geometry = ir::Geometry { vertices: vec![0.0, 0.0, 2.0, 0.0, 0.0, 2.0], indices: vec![0, 1, 2], color: 0x01020304 };
    assert!(complete.geometry(&geometry).is_ok());
    assert!(complete.image(17, ir::Rect { x: 1.0, y: 2.0, width: 3.0, height: 4.0 }, ir::Rect { x: 5.0, y: 6.0, width: 7.0, height: 8.0 }).is_ok());
    let mut changed = geometry.clone();
    changed.vertices[0] = 99.0;
    let published = match complete.publish(9, 4) { Ok(value) => value, Err(error) => { assert!(false, "{error:?}"); return; } };
    if let [ir::Command::Geometry(owned), ir::Command::Image(image)] = ir::commands_readonly(dl::render_ir(&published)) {
        assert_eq!(owned.vertices[0], 0.0);
        assert_eq!(*image, ir::Image { resource_id: 17, source: ir::Rect { x: 1.0, y: 2.0, width: 3.0, height: 4.0 }, destination: ir::Rect { x: 5.0, y: 6.0, width: 7.0, height: 8.0 } });
    } else { assert!(false, "display-list complete command or ownership parity drift"); }
    assert_eq!(changed.vertices[0], 99.0);
    assert!(matches!(dl::Builder::create().publish(0, 0), Err(ir::Error::InvalidArgument("Display_list.publish: ID must be positive"))));
}

#[test]
fn marks_validation_and_stable_encoding() {
    use marks::{ColorAttr as C, Entry as E, Kind as K};
    let valid = vec![
        E::MarkSet { count: 2, kind: K::Rect { position: pair_buffer(1, 2, 2), size: pair(3.0, 4.0), fill: color_buffer(2, 2, 3), stroke: color(0), stroke_width: scalar(1.0) } },
        E::MarkSet { count: 0, kind: K::Circle { position: pair(1.0, 2.0), radius: scalar(1.0), fill: color(0), stroke: color(0), stroke_width: scalar(0.0) } },
        E::MarkSet { count: 2, kind: K::Line { position: pair_buffer(1, 2, 3), end: pair_buffer(2, 2, 2), stroke: color_buffer(3, 2, 3), stroke_width: scalar(1.0) } },
        E::MarkSet { count: 2, kind: K::Point { position: pair_buffer(1, 2, 2), fill: -1 } },
        E::Geometry { vertices: vec![], indices: vec![], color: 0 },
        E::Geometry { vertices: vec![0.0, 0.0, 1.0, 0.0, 1.0, 1.0], indices: vec![0, 1, 2], color: 1 },
        E::Image { resource: 1, x: 0, y: 0, scale: 1.0, angle: 0.0 },
        E::Text { x: 0, y: 0, size: 0, color: 0, text: Vec::new() },
        E::PushTransform { xx: 1.0, xy: 0.0, yx: 0.0, yy: 1.0, tx: 0.0, ty: 0.0 },
        E::PushClip { x: 0.0, y: 0.0, width: 1.0, height: 1.0 }, E::PopClip, E::PopTransform,
    ];
    assert_eq!(marks::validate(&valid), Ok(()));
    let device_circle = |fill: C, stroke: C| E::MarkSet { count: 2, kind: K::Circle { position: pair_device(4, 2, 3), radius: scalar_device(4, 2, 1), fill, stroke, stroke_width: scalar(1.0) } };
    assert_eq!(marks::validate(&[device_circle(color_device(4, 2, 3), color_device(4, 2, 3))]), Ok(()));
    assert_eq!(marks::validate(&[E::MarkSet { count: 2, kind: K::Line { position: pair_buffer(1, 2, 2), end: pair_buffer(2, 2, 2), stroke: color_device(4, 2, 3), stroke_width: scalar(1.0) } }]), Ok(()));
    assert!(marks::validate(&[device_circle(color_device(4, 2, 4), color(0))]).is_err());
    assert!(marks::validate(&[device_circle(color(0), color_device(4, 2, 1))]).is_err());
    assert!(marks::validate(&[device_circle(color_device(4, 3, 3), color(0))]).is_err());

    let invalid = [
        vec![E::MarkSet { count: -1, kind: K::Point { position: pair(0.0, 0.0), fill: 0 } }],
        vec![E::MarkSet { count: 1_000_001, kind: K::Point { position: pair(0.0, 0.0), fill: 0 } }],
        vec![E::MarkSet { count: 2, kind: K::Point { position: pair_buffer(0, 2, 2), fill: 0 } }],
        vec![E::MarkSet { count: 2, kind: K::Point { position: pair_buffer(1, 3, 2), fill: 0 } }],
        vec![E::MarkSet { count: 2, kind: K::Point { position: pair_buffer(1, 2, 1), fill: 0 } }],
        vec![E::MarkSet { count: 1, kind: K::Rect { position: pair(0.0, 0.0), size: pair(1.0, 1.0), fill: color(0), stroke: color(0), stroke_width: scalar(f64::NAN) } }],
        vec![E::MarkSet { count: 1, kind: K::Rect { position: pair(0.0, 0.0), size: pair(1.0, 1.0), fill: color(0), stroke: color(0), stroke_width: scalar(-1.0) } }],
        vec![E::MarkSet { count: 1, kind: K::Rect { position: pair(0.0, 0.0), size: pair(1.0, 1.0), fill: color(0), stroke: color(0), stroke_width: scalar(3.5e38) } }],
        vec![E::MarkSet { count: 2, kind: K::Rect { position: pair(0.0, 0.0), size: pair(-1.0, 1.0), fill: color(0), stroke: color(0), stroke_width: scalar(1.0) } }],
        vec![E::MarkSet { count: 1, kind: K::Point { position: pair(f64::INFINITY, 0.0), fill: 0 } }],
        vec![E::Geometry { vertices: vec![0.0, 0.0, 1.0], indices: vec![], color: 0 }],
        vec![E::Geometry { vertices: vec![0.0, 0.0], indices: vec![0, 0], color: 0 }],
        vec![E::Geometry { vertices: vec![0.0, f64::NAN], indices: vec![], color: 0 }],
        vec![E::Geometry { vertices: vec![0.0, 0.0], indices: vec![0, 0, 1], color: 0 }],
        vec![E::Image { resource: 0, x: 0, y: 0, scale: 1.0, angle: 0.0 }],
        vec![E::Image { resource: 1, x: 0, y: 0, scale: 0.0, angle: 0.0 }],
        vec![E::Image { resource: 1, x: 0, y: 0, scale: 1.0, angle: f64::NAN }],
        vec![E::PushTransform { xx: f64::NAN, xy: 0.0, yx: 0.0, yy: 1.0, tx: 0.0, ty: 0.0 }],
        vec![E::PushClip { x: 0.0, y: 0.0, width: -1.0, height: 1.0 }],
        vec![E::PopTransform], vec![E::PopClip], vec![E::PushClip { x: 0.0, y: 0.0, width: 1.0, height: 1.0 }],
        vec![E::PushTransform { xx: 1.0, xy: 0.0, yx: 0.0, yy: 1.0, tx: 0.0, ty: 0.0 }, E::PopClip],
    ];
    for table in invalid { assert!(marks::validate(&table).is_err()); }
    assert!(marks::validate(&vec![E::Text { x: 0, y: 0, size: 1, color: 0, text: Vec::new() }; marks::max_entries() as usize + 1]).is_err());

    let tiny = vec![
        E::MarkSet { count: 1, kind: K::Point { position: pair(1.0, 2.0), fill: 0x11223344 } },
        E::Text { x: 1, y: 2, size: 3, color: 5, text: b"hi".to_vec() },
    ];
    let mut expected = Vec::new();
    expected.extend_from_slice(&0x524d4b31i32.to_le_bytes());
    let put_i64 = |out: &mut Vec<u8>, value: i64| out.extend_from_slice(&value.to_le_bytes());
    put_i64(&mut expected, 2); expected.extend_from_slice(&[0, 3]); put_i64(&mut expected, 1);
    expected.push(0); expected.extend_from_slice(&1.0f64.to_bits().to_le_bytes()); expected.extend_from_slice(&2.0f64.to_bits().to_le_bytes());
    expected.push(0); expected.extend_from_slice(&0x11223344i32.to_le_bytes());
    expected.push(3); put_i64(&mut expected, 1); put_i64(&mut expected, 2); put_i64(&mut expected, 3); expected.extend_from_slice(&5i32.to_le_bytes()); put_i64(&mut expected, 2); expected.extend_from_slice(b"hi");
    assert_eq!(marks::serialize(&tiny), expected);
    assert_eq!(marks::serialize(&tiny), marks::serialize(&tiny));
    assert_eq!(&marks::serialize(&tiny)[..4], b"1KMR");
    let opaque = marks::serialize(&[E::Text { x: 0, y: 0, size: 0, color: 0, text: vec![0xff] }]);
    assert_eq!(opaque.last(), Some(&0xff));
    assert_eq!(marks::gpu_token(4, 2, 8).map(|token| (marks::gpu_identity(token), marks::gpu_count(token), marks::gpu_stamp(token))), Ok((4, 2, 8)));
    assert_eq!(marks::gpu_token(0, 2, 8), Err("Marks.gpu_token: invalid identity, count or stamp".into()));
}

fn finish_ui(mut builder: ui::Builder, result: Result<(), String>) -> (Result<(), String>, Vec<u8>) {
    match builder.publish() {
        Ok(table) => (result, table.instances().to_vec()),
        Err(error) => (Err(error), Vec::new()),
    }
}
fn publish_ui(builder: &mut ui::Builder) -> ui::Table {
    match builder.publish() {
        Ok(table) => table,
        Err(error) => { assert!(false, "{error}"); ui::empty() }
    }
}
fn at(data: &[f64], stride: i64, i: i64, component: i64) -> Result<f64, String> {
    let index = stride.checked_mul(i).and_then(|v| v.checked_add(component)).and_then(|v| usize::try_from(v).ok()).ok_or_else(|| String::from("index out of bounds"))?;
    data.get(index).copied().ok_or_else(|| String::from("index out of bounds"))
}
fn circle_bulk_result(count: i64, position: &[f64], ps: i64, radius: &[f64], rs: i64, width: &[f64], ws: i64, fill: i32, stroke: i32) -> (Result<(), String>, Vec<u8>) {
    let mut builder = ui::Builder::with_capacity(0);
    let result = builder.circles(count, position, ps, radius, rs, fill, stroke, width, ws);
    finish_ui(builder, result)
}
fn circle_each_result(count: i64, position: &[f64], ps: i64, radius: &[f64], rs: i64, width: &[f64], ws: i64, fill: i32, stroke: i32) -> (Result<(), String>, Vec<u8>) {
    let mut builder = ui::Builder::with_capacity(0);
    let mut result = Ok(());
    for i in 0..count.max(0) {
        let values = (at(position, ps, i, 0), at(position, ps, i, 1), at(radius, rs, i, 0), at(width, ws, i, 0));
        let (x, y, radius, stroke_width) = match values {
            (Ok(x), Ok(y), Ok(radius), Ok(stroke_width)) => (x, y, radius, stroke_width),
            _ => { result = Err("index out of bounds".into()); break; }
        };
        result = builder.circle(x, y, radius, Some(fill), Some(stroke), stroke_width);
        if result.is_err() { break; }
    }
    finish_ui(builder, result)
}
fn rect_bulk_result(count: i64, position: &[f64], ps: i64, size: &[f64], ss: i64, fill: i32, stroke: i32) -> (Result<(), String>, Vec<u8>) {
    let mut builder = ui::Builder::with_capacity(0);
    let result = builder.rects(count, position, ps, size, ss, fill, stroke);
    finish_ui(builder, result)
}
fn rect_each_result(count: i64, position: &[f64], ps: i64, size: &[f64], ss: i64, fill: i32, stroke: i32) -> (Result<(), String>, Vec<u8>) {
    let mut builder = ui::Builder::with_capacity(0);
    let mut result = Ok(());
    for i in 0..count.max(0) {
        let values = (at(position, ps, i, 0), at(position, ps, i, 1), at(size, ss, i, 0), at(size, ss, i, 1));
        let (x, y, width, height) = match values {
            (Ok(x), Ok(y), Ok(width), Ok(height)) => (x, y, width, height),
            _ => { result = Err("index out of bounds".into()); break; }
        };
        result = builder.shape_rect(x, y, width, height, fill, stroke);
        if result.is_err() { break; }
    }
    finish_ui(builder, result)
}
fn line_bulk_result(count: i64, from: &[f64], fs: i64, to: &[f64], ts: i64, width: &[f64], ws: i64, color: i32) -> (Result<(), String>, Vec<u8>) {
    let mut builder = ui::Builder::with_capacity(0);
    let result = builder.lines(count, from, fs, to, ts, color, width, ws);
    finish_ui(builder, result)
}
fn line_each_result(count: i64, from: &[f64], fs: i64, to: &[f64], ts: i64, width: &[f64], ws: i64, color: i32) -> (Result<(), String>, Vec<u8>) {
    let mut builder = ui::Builder::with_capacity(0);
    let mut result = Ok(());
    for i in 0..count.max(0) {
        let values = (at(from, fs, i, 0), at(from, fs, i, 1), at(to, ts, i, 0), at(to, ts, i, 1), at(width, ws, i, 0));
        let (x0, y0, x1, y1, stroke_width) = match values {
            (Ok(x0), Ok(y0), Ok(x1), Ok(y1), Ok(stroke_width)) => (x0, y0, x1, y1, stroke_width),
            _ => { result = Err("index out of bounds".into()); break; }
        };
        result = builder.line(x0, y0, x1, y1, color, stroke_width);
        if result.is_err() { break; }
    }
    finish_ui(builder, result)
}
fn compare_circle(count: i64, position: &[f64], ps: i64, radius: &[f64], rs: i64, width: &[f64], ws: i64, fill: i32, stroke: i32) {
    assert_eq!(circle_bulk_result(count, position, ps, radius, rs, width, ws, fill, stroke), circle_each_result(count, position, ps, radius, rs, width, ws, fill, stroke));
}
fn compare_rects(count: i64, position: &[f64], ps: i64, size: &[f64], ss: i64, fill: i32, stroke: i32) {
    assert_eq!(rect_bulk_result(count, position, ps, size, ss, fill, stroke), rect_each_result(count, position, ps, size, ss, fill, stroke));
}
fn compare_lines(count: i64, from: &[f64], fs: i64, to: &[f64], ts: i64, width: &[f64], ws: i64, color: i32) {
    assert_eq!(line_bulk_result(count, from, fs, to, ts, width, ws, color), line_each_result(count, from, fs, to, ts, width, ws, color));
}

#[test]
fn ui_batch_packing_clipping_batches_and_immutable_reset() {
    let mut builder = ui::Builder::with_capacity(1);
    assert!(builder.rect(1.25, -2.5, 3.75, 4.5, 0x12345678, 0, 0.0, 0.0, false).is_ok());
    let clip = ui::Clip { x: 0.0, y: 0.0, width: 10.0, height: 10.0 };
    builder.set_clip(Some(clip));
    assert!(builder.wire([(100.0, 100.0), (120.0, 100.0), (140.0, 100.0), (160.0, 100.0)], 1.0, 0x01020304).is_ok());
    assert!(builder.wire([(1.0, 2.0), (3.0, 2.0), (5.0, 2.0), (7.0, 2.0)], 1.0, 0x01020304).is_ok());
    for texture in [2, 3] { assert!(builder.textured(texture, 0.0, 0.0, 2.0, 2.0, 3.0, 4.0, 5.0, 6.0, -1).is_ok()); }
    let packed = publish_ui(&mut builder);
    assert_eq!(packed.count(), 7);
    for (word, expected) in [1.25, -2.5, 5.0, 2.0].iter().enumerate() { assert_eq!(packed.float(0, word as i64), Ok(*expected)); }
    assert_eq!(&packed.instances()[32..36], &0x12345678i32.to_le_bytes()[..]);
    for i in 0..4 {
        for (word, expected) in [1.0, 2.0, 3.0, 2.0, 5.0, 2.0, 7.0, 2.0].iter().enumerate() { assert_eq!(packed.float(i as i64 + 1, word as i64), Ok(*expected)); }
        assert_eq!(packed.float(i as i64 + 1, 14), Ok(i as f64 / 4.0));
        assert_eq!(packed.float(i as i64 + 1, 15), Ok((i + 1) as f64 / 4.0));
    }
    assert_eq!(packed.batches(), &[ui::Batch { first: 0, count: 1, clip: None, texture: 0 }, ui::Batch { first: 1, count: 5, clip: Some(clip), texture: 2 }, ui::Batch { first: 6, count: 1, clip: Some(clip), texture: 3 }]);
    assert_eq!(packed.textures(), vec![2, 3]);
    let bytes = packed.instances().to_vec();
    builder.reset();
    assert!(builder.rect(50.0, 60.0, 1.0, 1.0, 0, 0, 0.0, 0.0, false).is_ok());
    let reset = publish_ui(&mut builder);
    assert_eq!(reset.count(), 1);
    assert_eq!(reset.batches().len(), 1);
    assert_eq!(reset.batches()[0].clip, None);
    assert_eq!(packed.instances(), bytes.as_slice());
    assert_eq!(packed.batches().len(), 3);
    assert_eq!(packed.float(-1, 0), Err("index out of bounds".into()));
    assert_eq!(packed.float(0, 16), Err("index out of bounds".into()));
}

#[test]
fn ui_shapes_validation_and_bulk_writers_match_individual_writes() {
    let mut builder = ui::Builder::with_capacity(20_000);
    for i in 0..20_000 { assert!(builder.circle(i as f64, 1.0, 0.5, None, None, 1.0).is_ok()); }
    let batch = publish_ui(&mut builder);
    let scene = |dx| vec![ir::Command::PushTransform(ir::Transform { xx: 1.0, xy: 0.0, yx: 0.0, yy: 1.0, tx: dx, ty: 0.0 }), ir::Command::Shapes(batch.clone()), ir::Command::PopTransform];
    assert!(ir::create_owned(scene(0.0)).is_ok());
    for i in 1..10 { assert!(ir::create_owned(scene(i as f64)).is_ok()); }
    assert!(batch.valid_shapes());
    let copy = batch.copy();
    assert!(ui::valid_shapes(&copy));

    let mut full = ui::Builder::with_capacity(20_000);
    for i in 0..20_000 { assert!(full.circle(i as f64, 1.0, 0.5, None, None, 1.0).is_ok()); }
    let stable = publish_ui(&mut full);
    let snapshot = stable.instances().to_vec();
    for i in 0..3 { assert!(full.circle(i as f64, 1.0, 0.5, None, None, 1.0).is_ok()); }
    let again = publish_ui(&mut full);
    assert_eq!(stable.count(), 20_000);
    assert_eq!(stable.instances(), snapshot.as_slice());
    assert_eq!(again.count(), 20_003);
    assert_eq!(&again.instances()[..snapshot.len()], snapshot.as_slice());
    let mut roomy = ui::Builder::with_capacity(20_001);
    for i in 0..20_000 { assert!(roomy.circle(i as f64, 1.0, 0.5, None, None, 1.0).is_ok()); }
    let partial = publish_ui(&mut roomy);
    let partial_snapshot = partial.instances().to_vec();
    assert!(roomy.circle(20_000.0, 1.0, 0.5, None, None, 1.0).is_ok());
    assert_eq!(partial.count(), 20_000);
    assert_eq!(partial.instances(), partial_snapshot.as_slice());
    let partial_again = publish_ui(&mut roomy);
    assert_eq!(&partial_again.instances()[..partial_snapshot.len()], partial_snapshot.as_slice());

    let n = 257i64;
    let v3: Vec<_> = (0..3 * n as usize).map(|k| (k % 97) as f64 * 1.37 - 20.0).collect();
    let v2: Vec<_> = (0..2 * n as usize).map(|k| (k % 89) as f64 * 0.83 + 0.25).collect();
    let s1: Vec<_> = (0..n as usize).map(|k| (k % 7) as f64 * 0.45).collect();
    let position_constant = [4.5, -2.25];
    let scalar_constant = [1.75];
    let size_constant = [3.5, 0.0];
    let v3_abs: Vec<_> = v3.iter().map(|x| x.abs()).collect();
    let positions: Vec<(&[f64], i64)> = vec![(&v3, 3), (&v2, 2), (&position_constant, 0)];
    let scalars: Vec<(&[f64], i64)> = vec![(&s1, 1), (&scalar_constant, 0)];
    let sizes: Vec<(&[f64], i64)> = vec![(&v2, 2), (&v3_abs, 3), (&size_constant, 0)];
    for &(position, ps) in &positions {
        for &(radius, rs) in &scalars {
            for &(width, ws) in &scalars {
                for &(fill, stroke) in &[(0x76bacaff, 0), (0, -1), (0x10203040, 0x0000ff80)] { compare_circle(n, position, ps, radius, rs, width, ws, fill, stroke); }
            }
        }
        for &(size, ss) in &sizes {
            for &(fill, stroke) in &[(0x76bacaff, 0), (0, -1)] { compare_rects(n, position, ps, size, ss, fill, stroke); }
        }
        for &(to, ts) in &positions {
            for &(width, ws) in &scalars { compare_lines(n, position, ps, to, ts, width, ws, 0xff8040ffu32 as i32); }
        }
    }

    let mut bad_radius = s1.clone(); bad_radius[200] = -1.0;
    let bulk = circle_bulk_result(n, &v3, 3, &bad_radius, 1, &scalar_constant, 0, 1, 1);
    let each = circle_each_result(n, &v3, 3, &bad_radius, 1, &scalar_constant, 0, 1, 1);
    assert_eq!(bulk, each);
    assert_eq!(bulk.0, Err("Ui_batch radius".into()));
    let mut nan_radius = s1.clone(); nan_radius[3] = f64::NAN;
    assert_eq!(circle_bulk_result(n, &v3, 3, &nan_radius, 1, &scalar_constant, 0, 1, 1), circle_each_result(n, &v3, 3, &nan_radius, 1, &scalar_constant, 0, 1, 1));
    let mut bad_coordinate = v3.clone(); bad_coordinate[300] = 3.5e38;
    assert_eq!(circle_bulk_result(n, &bad_coordinate, 3, &s1, 1, &scalar_constant, 0, 1, 1), circle_each_result(n, &bad_coordinate, 3, &s1, 1, &scalar_constant, 0, 1, 1));
    let mut bad_width = s1.clone(); bad_width[100] = -2.0;
    assert_eq!(circle_bulk_result(n, &v3, 3, &s1, 1, &bad_width, 1, 1, 1), circle_each_result(n, &v3, 3, &s1, 1, &bad_width, 1, 1, 1));
    let short_position = &v3[..30];
    assert_eq!(circle_bulk_result(n, short_position, 3, &s1, 1, &scalar_constant, 0, 1, 1), circle_each_result(n, short_position, 3, &s1, 1, &scalar_constant, 0, 1, 1));
    let mut bad_size = v2.clone(); bad_size[41] = -0.5;
    assert_eq!(rect_bulk_result(n, &v3, 3, &bad_size, 2, 1, 0), rect_each_result(n, &v3, 3, &bad_size, 2, 1, 0));
    assert_eq!(line_bulk_result(n, &v3, 3, &v2, 2, &bad_width, 1, 1), line_each_result(n, &v3, 3, &v2, 2, &bad_width, 1, 1));

    let mut invalid = ui::Builder::with_capacity(1);
    assert_eq!(invalid.textured(0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, -1), Err("Ui_batch.textured: texture must be positive".into()));
    let mut shape = ui::Builder::create();
    assert_eq!(shape.shape_rect(1.0, 1.0, -2.0, 2.0, -1, 0), Err("Ui_batch extent".into()));
    assert!(shape.shape_rect(1.0, 1.0, 2.0, 2.0, -1, 0).is_ok());
    let published = publish_ui(&mut shape);
    assert!(published.valid_shapes());
    let token = match marks::gpu_token(4, 2, 8) { Ok(token) => token, Err(error) => { assert!(false, "{error}"); return; } };
    let marks_batch = ui::of_marks(token);
    assert!(marks_batch.valid_shapes());
    assert_eq!(marks_batch.count(), 2);
}
