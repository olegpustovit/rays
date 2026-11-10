use crate::marks::{self, Buffer, ColorAttr, Entry, Kind, PairAttr, ScalarAttr};

fn ok(table: &[Entry]) {
    assert_eq!(marks::validate(table), Ok(()));
}

fn bad(table: &[Entry]) {
    assert!(marks::validate(table).is_err());
}

// Construct typed attribute-buffer fixtures with explicit ID/count/width.
fn buf(id: i64, count: i64, width: i64) -> Buffer {
    Buffer { id, count, width }
}

fn pair_buf(id: i64, width: i64) -> PairAttr {
    PairAttr::Buffer(buf(id, 2, width))
}

fn scalar_buf(id: i64, width: i64) -> ScalarAttr {
    ScalarAttr::Buffer(buf(id, 2, width))
}

fn color_buf(id: i64, width: i64) -> ColorAttr {
    ColorAttr::Buffer(buf(id, 2, width))
}

fn pair(x: f64, y: f64) -> PairAttr {
    PairAttr::Constant((x, y))
}

fn scalar(value: f64) -> ScalarAttr {
    ScalarAttr::Constant(value)
}

fn color(value: i32) -> ColorAttr {
    ColorAttr::Constant(value)
}

const WHITE: i32 = -1; // 0xffffffff as int32

fn rect(count: i64, position: PairAttr, size: PairAttr, stroke_width: ScalarAttr) -> Vec<Entry> {
    vec![Entry::MarkSet {
        count,
        kind: Kind::Rect {
            position,
            size,
            fill: color(WHITE),
            stroke: color_buf(2, 3),
            stroke_width,
        },
    }]
}

// A device colour is a region's float r, g, b: three floats per instance, like a colour buffer.
fn device(count: i64, width: i64) -> Buffer {
    buf(4, count, width)
}

fn coloured(fill: ColorAttr, stroke: ColorAttr) -> Vec<Entry> {
    vec![Entry::MarkSet {
        count: 2,
        kind: Kind::Circle {
            position: PairAttr::Device(device(2, 3)),
            radius: ScalarAttr::Device(device(2, 1)),
            fill,
            stroke,
            stroke_width: scalar(1.0),
        },
    }]
}

fn device_color(count: i64, width: i64) -> ColorAttr {
    ColorAttr::Device(device(count, width))
}

#[test]
fn valid_tables_of_every_entry_kind_pass() {
    ok(&[
        Entry::MarkSet {
            count: 2,
            kind: Kind::Rect {
                position: pair_buf(1, 2),
                size: pair(3.0, 4.0),
                fill: color_buf(2, 3),
                stroke: color(0),
                stroke_width: scalar(1.0),
            },
        },
        Entry::MarkSet {
            count: 0,
            kind: Kind::Circle {
                position: pair(1.0, 2.0),
                radius: scalar(1.0),
                fill: color(0),
                stroke: color(0),
                stroke_width: scalar(0.0),
            },
        },
        Entry::MarkSet {
            count: 2,
            kind: Kind::Line {
                position: pair_buf(1, 3),
                end: pair_buf(2, 2),
                stroke: color(WHITE),
                stroke_width: scalar_buf(3, 1),
            },
        },
        Entry::MarkSet { count: 2, kind: Kind::Point { position: pair_buf(1, 2), fill: WHITE } },
        Entry::Geometry { vertices: vec![], indices: vec![], color: 0 },
        Entry::Geometry { vertices: vec![0.0, 0.0, 1.0, 0.0, 1.0, 1.0], indices: vec![0, 1, 2], color: 1 },
        Entry::Image { resource: 1, x: 0, y: 0, scale: 1.0, angle: 0.0 },
        Entry::Text { x: 0, y: 0, size: 0, color: 0, text: vec![] },
        Entry::PushTransform { xx: 1.0, xy: 0.0, yx: 0.0, yy: 1.0, tx: 0.0, ty: 0.0 },
        Entry::PushClip { x: 0.0, y: 0.0, width: 1.0, height: 1.0 },
        Entry::PopClip,
        Entry::PopTransform,
    ]);
    ok(&rect(2, pair_buf(1, 2), pair_buf(1, 3), scalar(1.0)));
    // A device colour is a region's float r, g, b: three floats per instance, like a colour buffer.
    ok(&coloured(device_color(2, 3), device_color(2, 3)));
    ok(&[Entry::MarkSet {
        count: 2,
        kind: Kind::Line {
            position: pair_buf(1, 2),
            end: pair_buf(2, 2),
            stroke: device_color(2, 3),
            stroke_width: scalar(1.0),
        },
    }]);
    bad(&coloured(device_color(2, 4), color(0)));
    bad(&coloured(color(0), ColorAttr::Device(device(2, 1))));
    bad(&coloured(device_color(3, 3), color(0)));
}

#[test]
fn refusals_name_each_invalid_entry() {
    bad(&rect(-1, pair_buf(1, 2), pair_buf(1, 3), scalar(1.0)));
    bad(&rect(1_000_001, pair(0.0, 0.0), pair(1.0, 1.0), scalar(1.0)));
    bad(&rect(2, PairAttr::Buffer(buf(1, 3, 2)), pair_buf(1, 3), scalar(1.0)));
    bad(&rect(2, PairAttr::Buffer(buf(0, 2, 2)), pair_buf(1, 3), scalar(1.0)));
    bad(&rect(2, pair_buf(1, 1), pair_buf(1, 3), scalar(1.0)));
    bad(&rect(2, pair(0.0, 0.0), pair(-1.0, 1.0), scalar(1.0)));
    bad(&rect(2, pair(0.0, 0.0), pair(1.0, 1.0), scalar(f64::NAN)));
    bad(&rect(2, pair(0.0, 0.0), pair(1.0, 1.0), scalar(-1.0)));
    bad(&rect(2, pair(0.0, 0.0), pair(1.0, 1.0), scalar(3.5e38)));
    bad(&[Entry::MarkSet { count: 1, kind: Kind::Point { position: pair(f64::INFINITY, 0.0), fill: 0 } }]);
    bad(&[Entry::Geometry { vertices: vec![0.0, 0.0, 1.0], indices: vec![], color: 0 }]);
    bad(&[Entry::Geometry { vertices: vec![0.0, 0.0], indices: vec![0, 0], color: 0 }]);
    bad(&[Entry::Geometry { vertices: vec![0.0, f64::NAN], indices: vec![], color: 0 }]);
    bad(&[Entry::Geometry { vertices: vec![0.0, 0.0], indices: vec![0, 0, 1], color: 0 }]);
    bad(&[Entry::Image { resource: 0, x: 0, y: 0, scale: 1.0, angle: 0.0 }]);
    bad(&[Entry::Image { resource: 1, x: 0, y: 0, scale: 0.0, angle: 0.0 }]);
    bad(&[Entry::Image { resource: 1, x: 0, y: 0, scale: 1.0, angle: f64::NAN }]);
    bad(&[Entry::PushTransform { xx: f64::NAN, xy: 0.0, yx: 0.0, yy: 1.0, tx: 0.0, ty: 0.0 }]);
    bad(&[Entry::PushClip { x: 0.0, y: 0.0, width: -1.0, height: 1.0 }]);
    bad(&[Entry::PopTransform]);
    bad(&[Entry::PopClip]);
    bad(&[Entry::PushClip { x: 0.0, y: 0.0, width: 1.0, height: 1.0 }]);
    // Families are independent: a clip pop cannot close a transform push.
    bad(&[Entry::PushTransform { xx: 1.0, xy: 0.0, yx: 0.0, yy: 1.0, tx: 0.0, ty: 0.0 }, Entry::PopClip]);
    let too_many = vec![Entry::Text { x: 0, y: 0, size: 1, color: 0, text: vec![] }; (marks::max_entries() + 1) as usize];
    bad(&too_many);
}

#[test]
fn serialize_is_exact_and_deterministic() {
    fn i64le(out: &mut Vec<u8>, n: i64) {
        out.extend_from_slice(&n.to_le_bytes());
    }
    fn f64le(out: &mut Vec<u8>, x: f64) {
        out.extend_from_slice(&x.to_bits().to_le_bytes());
    }
    let tiny = vec![
        Entry::MarkSet { count: 1, kind: Kind::Point { position: pair(1.0, 2.0), fill: 0x11223344 } },
        Entry::Text { x: 1, y: 2, size: 3, color: 5, text: b"hi".to_vec() },
    ];
    let mut expected = Vec::new();
    expected.extend_from_slice(&0x524d4b31_u32.to_le_bytes());
    i64le(&mut expected, 2);
    expected.push(0);
    expected.push(3);
    i64le(&mut expected, 1);
    expected.push(0);
    f64le(&mut expected, 1.0);
    f64le(&mut expected, 2.0);
    expected.push(0);
    expected.extend_from_slice(&0x11223344_i32.to_le_bytes());
    expected.push(3);
    i64le(&mut expected, 1);
    i64le(&mut expected, 2);
    i64le(&mut expected, 3);
    expected.extend_from_slice(&5_i32.to_le_bytes());
    i64le(&mut expected, 2);
    expected.extend_from_slice(b"hi");
    assert_eq!(marks::serialize(&tiny), expected);
    assert_eq!(marks::serialize(&tiny), marks::serialize(&tiny));
    assert_eq!(&marks::serialize(&tiny)[..4], b"1KMR");
}
