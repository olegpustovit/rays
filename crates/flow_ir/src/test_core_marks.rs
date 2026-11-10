//! Drawing register programs match core_marks.expected.
use crate::core_draw;
use crate::core_exec::{self, Buffers, DeviceFn, DrawOptions};
use crate::core_ir as C;
use crate::flow_ir::{executor, gpu};
use crate::packed::{self, host};
use crate::test_support::{self as ts, ok};
use flow_check::workspace::{self as W, Paths};
use flow_eval as E;
use flow_syntax::syntax;
use flow_syntax::value::{ResidualId, Value};
use input::Frame;
use scene_command::marks::{ColorAttr, Entry, Kind, PairAttr, ScalarAttr};
use std::cell::Cell;
use std::rc::Rc;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::{Arc, Mutex};

fn checked(body: &str) -> W::Workspace {
    let forms = ok(syntax::parse(&format!("(workspace w (graph g :context draw {body}))")));
    match W::check(&[], false, false, None, &ts::catalog(), &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{}", ts::show_ds(&ds)),
    }
}

const PICTURES: [&str; 22] = [
    r##"(draw/background "#14161c")"##,
    r##"(draw/merge (draw/rect [1 2 0] [30 40 0]) (draw/rect [1 2 0] [3 4 0] :stroke "#fff") (draw/rect [0 0 0] [5 5 0] :fill "#f00" :stroke "#00f"))"##,
    "(draw/circle [10.5 20 0] 4)",
    r##"(draw/circle [10 20 0] (+ 4 (* 2 (sin t))) :fill "#5aaaf080")"##,
    "(draw/line [0 0 0] [5 5 0])",
    r##"(draw/line [0 0 0] [5 5 0] :color "#0f0" :width 3)"##,
    r##"(draw/point [3.9 -4.9 0] :color "#abc")"##,
    "(draw/translate [1.9 -2.5 0] (draw/merge (draw/point [0 0 0]) (draw/rotate 0.5 (draw/scale 2 (draw/point [1 1 0])))))",
    r##"(draw/scale [2 3 0] (draw/background "#000"))"##,
    "(let* [p (map (fn [i] [(* i 10) (* i t) 0]) (array/range 3))] (draw/circles p))",
    r##"(let* [p (map (fn [i] [(* i 10) (* i t) 0]) (array/range 3)) r (map (fn [i] (* i 0.5)) (array/range 3))] (draw/circles p :radius r :stroke "#fff"))"##,
    r##"(let* [p (map (fn [i] [i 0 0]) (array/range 3))] (draw/rects p [10 10 0] :fill "#f00"))"##,
    r##"(let* [p (map (fn [i] [i 0 0]) (array/range 3)) c (map (fn [i] [(/ i 4) 0 0]) (array/range 3)) s (map (fn [i] [i 1 0]) (array/range 3))] (draw/rects p s :fill c))"##,
    "(let* [p (map (fn [i] [i 0 0]) (array/range 3))] (draw/lines p p :width 2))",
    r##"(let* [p (map (fn [i] [i 0 0]) (array/range 3))] (draw/points p :color "#1e1e1e"))"##,
    "(let* [p (map (fn [i] [i 0 0]) (array/range 0))] (draw/merge (draw/circles p) (draw/points p)))",
    r##"(draw/text [12.7 12 0] "hi" :color "#abc" :size 20)"##,
    r##"(draw/text [12 12 0] "")"##,
    r##"(draw/rounded_rect [4 5 0] [60 40 0] :radius 8 :fill "#123")"##,
    "(draw/rounded_rect [4 5 0] [60 40 0])",
    r##"(draw/polygon (map (fn [i] [i (* i i) 0]) (array/range 4)) :stroke "#fff")"##,
    r##"(draw/polyline (map (fn [i] [i (* i i) 0]) (array/range 4)) :color "#f0f")"##,
];

const ERRORS: [(&str, &str); 6] = [
    ("(let* [p (map (fn [i] [i 0 0]) (array/range 3)) r (map (fn [i] 1.0) (array/range 2))] (draw/circles p :radius r))", "E_DRAW_LENGTH"),
    ("(let* [p (map (fn [i] [i 0 0]) (array/range 3)) c (map (fn [i] [0.5 0 0]) (array/range 2))] (draw/rects p [1 1 0] :fill c))", "E_DRAW_LENGTH"),
    ("(let* [p (map (fn [i] [i 0 0]) (array/range 3)) q (map (fn [i] [i 0 0]) (array/range 2))] (draw/lines p q))", "E_DRAW_LENGTH"),
    ("(draw/rect [0 0 0] [-1 1 0])", "E_DRAW_RANGE"),
    ("(draw/point [1e40 0 0])", "E_DRAW_RANGE"),
    ("(draw/image (image/noise))", "E_IMAGE"),
];

fn ink(c: i32) -> String { format!("{:08x}", c as u32) }

fn pair(buffers: &Buffers, a: &PairAttr) -> String {
    match a {
        PairAttr::Constant((x, y)) => format!("const {},{}", flow_syntax::lisp::float(*x), flow_syntax::lisp::float(*y)),
        PairAttr::Buffer(b) => buffer(buffers, b.id, b.count, b.width),
        PairAttr::Device(_) => panic!("no device attribute without a device host"),
    }
}

fn scalar(buffers: &Buffers, a: &ScalarAttr) -> String {
    match a {
        ScalarAttr::Constant(x) => format!("const {}", flow_syntax::lisp::float(*x)),
        ScalarAttr::Buffer(b) => buffer(buffers, b.id, b.count, b.width),
        ScalarAttr::Device(_) => panic!("no device attribute without a device host"),
    }
}

fn color(buffers: &Buffers, a: &ColorAttr) -> String {
    match a {
        ColorAttr::Constant(c) => format!("const {}", ink(*c)),
        ColorAttr::Buffer(b) => buffer(buffers, b.id, b.count, b.width),
        ColorAttr::Device(_) => panic!("no device attribute without a device host"),
    }
}

fn buffer(buffers: &Buffers, id: i64, count: i64, width: i64) -> String {
    let data = buffers.find(id).expect("buffer");
    format!("buffer {id} count={count} width={width} [{}]", data.iter().map(|x| flow_syntax::lisp::float(*x)).collect::<Vec<_>>().join(" "))
}

fn lowered(body: &str) -> (C::Program, Option<ResidualId>) {
    let evaluation = ok(E::static_eval(&checked(body)));
    let value = evaluation.results.iter().find(|(n, _)| n == "g").expect("g").1.clone();
    core_draw::lower(None, &evaluation.plan, &value).unwrap_or_else(|name| panic!("draw lowering refused {name}"))
}

fn prepare(program: &C::Program) -> core_exec::Prepared { core_exec::prepare(program, false, false, &Paths::new(), false) }

fn draw_options(root: Option<ResidualId>, device: Option<DeviceFn>) -> DrawOptions { DrawOptions { root, device, ..Default::default() } }

fn dump() -> String {
    let mut out = String::new();
    for (i, body) in PICTURES.iter().enumerate() {
        out.push_str(&format!("picture {i}: {body}\n"));
        let (program, root) = lowered(body);
        let prepared = prepare(&program);
        let buffers = Buffers::create();
        let live = Frame { size: (200, 100), ..input::at_time(0.25) };
        let marks = ok(core_exec::draw(&prepared, &live, (200, 100), &buffers, &draw_options(root, None)));
        for entry in &marks {
            match entry {
                Entry::MarkSet { count, kind } => match kind {
                    Kind::Rect { position, size, fill, stroke, stroke_width } => out.push_str(&format!(
                        "  rect {count} position={} size={} fill={} stroke={} width={}\n",
                        pair(&buffers, position), pair(&buffers, size), color(&buffers, fill), color(&buffers, stroke), scalar(&buffers, stroke_width)
                    )),
                    Kind::Circle { position, radius, fill, stroke, stroke_width } => out.push_str(&format!(
                        "  circle {count} position={} radius={} fill={} stroke={} width={}\n",
                        pair(&buffers, position), scalar(&buffers, radius), color(&buffers, fill), color(&buffers, stroke), scalar(&buffers, stroke_width)
                    )),
                    Kind::Line { position, end, stroke, stroke_width } => out.push_str(&format!(
                        "  line {count} from={} to={} color={} width={}\n",
                        pair(&buffers, position), pair(&buffers, end), color(&buffers, stroke), scalar(&buffers, stroke_width)
                    )),
                    Kind::Point { position, fill } => out.push_str(&format!("  point {count} position={} fill={}\n", pair(&buffers, position), ink(*fill))),
                },
                Entry::Geometry { vertices, indices, color } => out.push_str(&format!(
                    "  geometry {} vertices=[{}] indices=[{}]\n",
                    ink(*color),
                    vertices.iter().map(|x| flow_syntax::lisp::float(*x)).collect::<Vec<_>>().join(" "),
                    indices.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" ")
                )),
                Entry::Image { .. } => out.push_str("  image\n"),
                Entry::Text { x, y, size, color, text } => out.push_str(&format!("  text {x},{y} size={size} {} {:?}\n", ink(*color), String::from_utf8_lossy(text))),
                Entry::PushTransform { xx, xy, yx, yy, tx, ty } => {
                    out.push_str(&format!("  push {} {} {} {} {} {}\n", flow_syntax::lisp::float(*xx), flow_syntax::lisp::float(*xy), flow_syntax::lisp::float(*yx), flow_syntax::lisp::float(*yy), flow_syntax::lisp::float(*tx), flow_syntax::lisp::float(*ty)))
                }
                Entry::PopTransform => out.push_str("  pop\n"),
                Entry::PushClip { .. } => out.push_str("  push-clip\n"),
                Entry::PopClip => out.push_str("  pop-clip\n"),
            }
        }
    }
    for (body, code) in ERRORS {
        let (program, root) = lowered(body);
        match core_exec::draw(&prepare(&program), &input::at_time(0.), (10, 10), &Buffers::create(), &draw_options(root, None)) {
            Err(d) => {
                assert!(d.code == code);
                out.push_str(&format!("error {}: {}\n", d.code, d.message));
            }
            Ok(_) => panic!("expected {code} for {body}"),
        }
    }
    out
}

#[test]
fn core_marks_snapshot_is_byte_identical() {
    let actual = dump();
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("core_marks.expected");
    if std::env::var_os("RAYS_UPDATE_FIXTURES").is_some() { std::fs::write(&path, &actual).unwrap(); }
    let expected = std::fs::read_to_string(path).unwrap().replace("\r\n", "\n");
    if actual != expected {
        let first = actual.lines().zip(expected.lines()).position(|(a, b)| a != b).unwrap_or(0);
        panic!("Core marks changed at line {}:\nactual:   {:?}\nexpected: {:?}", first + 1, actual.lines().nth(first), expected.lines().nth(first));
    }
}

// A host that takes every region it is offered (here it runs them on the CPU and hands back a
// token). A region read only as attributes of mark sets is offered, a colour like a position, and
// becomes a device buffer of its width; one the frame program also reads is not offered.
fn device_marks(body: &str) -> (i64, Vec<Entry>) {
    let (program, root) = lowered(body);
    let live = input::at_time(0.25);
    let offered = Rc::new(Cell::new(0i64));
    let counter = offered.clone();
    let live_for_device = live.clone();
    let device: DeviceFn = Arc::new(move |packed: &packed::T| {
        let values = packed::force_all(packed, &live_for_device, &Default::default())?;
        counter.set(counter.get() + 1);
        Ok(Some(
            values
                .iter()
                .map(|value| {
                    let (count, width) = match value {
                        Value::Floats(1, xs) => (xs.len() as i64, 1),
                        Value::Floats(2, xs) => (xs.len() as i64 / 2, 2),
                        Value::Floats(3, xs) => (xs.len() as i64 / 3, 3),
                        _ => panic!("a region is a packed array"),
                    };
                    (counter.get(), count, width)
                })
                .collect(),
        ))
    });
    let marks = ok(core_exec::draw(&prepare(&program), &live, (200, 100), &Buffers::create(), &draw_options(root, Some(device))));
    (offered.get(), marks)
}

fn device_pair(a: &PairAttr, width: i64) -> bool { matches!(a, PairAttr::Device(b) if b.count == 3 && b.width == width) }
fn device_scalar(a: &ScalarAttr, width: i64) -> bool { matches!(a, ScalarAttr::Device(b) if b.count == 3 && b.width == width) }
fn device_color(a: &ColorAttr, width: i64) -> bool { matches!(a, ColorAttr::Device(b) if b.count == 3 && b.width == width) }

const P: &str = "p (map (fn [i] [(* i 10) (* i t) 0]) (array/range 3))";
const COLORS: &str = "c (map (fn [i] [(/ i 4) t 0]) (array/range 3))";

#[test]
fn device_buffers_for_attributes_only_read_by_mark_sets() {
    match device_marks(&format!("(let* [{P} {COLORS}] (draw/rects p [10 10 0] :fill c :stroke c))")) {
        (1, marks) => match marks.as_slice() {
            [Entry::MarkSet { count: 3, kind: Kind::Rect { position, size: PairAttr::Constant(_), fill, stroke, .. } }] => {
                assert!(device_pair(position, 3) && device_color(fill, 3) && device_color(stroke, 3))
            }
            _ => panic!("rect positions, fill and stroke are device buffers"),
        },
        _ => panic!("rect positions, fill and stroke are device buffers"),
    }
    match device_marks(&format!("(let* [{P} {COLORS} r (map (fn [i] (+ i t)) (array/range 3))] (draw/circles p :radius r :fill c))")) {
        (1, marks) => match marks.as_slice() {
            [Entry::MarkSet { kind: Kind::Circle { position, radius, fill, stroke: ColorAttr::Constant(0), .. }, .. }] => {
                assert!(device_pair(position, 3) && device_scalar(radius, 1) && device_color(fill, 3))
            }
            _ => panic!("circle positions, radii and fill are device buffers"),
        },
        _ => panic!("circle positions, radii and fill are device buffers"),
    }
    match device_marks(&format!("(let* [{P} {COLORS}] (draw/lines p p :color c))")) {
        (1, marks) => match marks.as_slice() {
            [Entry::MarkSet { kind: Kind::Line { position, end, stroke, .. }, .. }] => assert!(device_pair(position, 3) && device_pair(end, 3) && device_color(stroke, 3)),
            _ => panic!("line ends and colour are device buffers"),
        },
        _ => panic!("line ends and colour are device buffers"),
    }
    // The frame reads the colours too (their count), so they stay a host buffer.
    match device_marks(&format!("(let* [{P} {COLORS}] (draw/merge (draw/rects p [10 10 0] :fill c) (draw/text [0 0 0] (str (array/count c)))))")) {
        (1, marks) => match marks.as_slice() {
            [Entry::MarkSet { kind: Kind::Rect { position, fill: ColorAttr::Buffer(b), .. }, .. }, Entry::Text { .. }] if b.count == 3 && b.width == 3 => assert!(device_pair(position, 3)),
            _ => panic!("a colour array the frame reads is a host buffer"),
        },
        _ => panic!("a colour array the frame reads is a host buffer"),
    }
}

#[test]
fn a_device_colour_of_the_wrong_length_is_refused() {
    let (program, root) = lowered(&format!("(let* [{P} c (map (fn [i] [(/ i 4) t 0]) (array/range 2))] (draw/rects p [10 10 0] :fill c))"));
    let live = input::at_time(0.25);
    let live_for_device = live.clone();
    let device: DeviceFn = Arc::new(move |packed: &packed::T| match packed::force(packed, &live_for_device, &Default::default())? {
        Value::Floats(3, xs) => Ok(Some(vec![(1, xs.len() as i64 / 3, 3)])),
        _ => Ok(None),
    });
    match core_exec::draw(&prepare(&program), &live, (200, 100), &Buffers::create(), &draw_options(root, Some(device))) {
        Err(d) => assert!(d.code == "E_DRAW_LENGTH"),
        Ok(_) => panic!("a device colour of the wrong length is refused"),
    }
}

// A fold whose body maps over the previous level with gathers, integer arithmetic and noise (the
// shape of recursive_rectangles' split-level): a drawing runs its nested maps as packed kernels,
// and the points it draws equal the reference walker's at one, four and eight domains, past the
// 4,096 elements where a kernel splits across domains.
#[test]
fn nested_maps_draw_the_walkers_points_at_one_four_and_eight_domains() {
    // each level keeps the rows a filter of a range picks, then maps them
    let boxes = "(fold [b (map (fn [i] [i (* i 2) 0]) (array/range 600))] [d (range 4)] (let* [kept (filter (fn [i] (>= (int/mod (int/mul (int i) 3) 7) 1)) (array/range (array/count b)))] (map (fn [i] (let* [p (array/nth b (int/div (int (array/nth kept (int/div (int i) 2))) 1)) k (int/mod (int/mul (int i) 7) 5)] [(+ p.x (sin (* (* p.y 0.01) (+ d t)))) (+ p.y k) 0])) (array/range (* (array/count kept) 2)))))";
    let live = Frame { size: (200, 100), ..input::at_time(0.25) };
    // the walker's points, summed in order as x + 1000 y: the same sum of the drawn buffer must match it
    let ws = ts::check_workspace(&format!("(workspace w (graph g :context value (array/sum (map (fn [v] (+ v.x (* v.y 1000))) {boxes}))))"));
    let evaluation = ok(E::static_eval(&ws));
    let reference_value = evaluation.results.iter().find(|(n, _)| n == "g").unwrap().1.clone();
    let expected = match E::host::force_reference(&reference_value, &live, None, None, None) {
        Ok(Value::Float(f)) => f,
        Ok(_) => panic!("nested maps: the reference sum is not a float"),
        Err(d) => panic!("{}", rays_diagnostic::to_string(&d)),
    };
    let (program, root) = lowered(&format!("(let* [p {boxes}] (draw/points (map (fn [v] [v.x v.y 0]) p)))"));
    let text = C::pp(&program);
    if !text.split(' ').any(|w| w == "iterate") {
        panic!("nested maps: captured fold is hidden inside a packed source");
    }
    let bytes = text.as_bytes();
    let regions = (0..bytes.len()).filter(|j| bytes[*j] == b'[' && *j >= 6 && &bytes[*j - 6..*j] == b"region").count();
    if regions < 2 {
        panic!("nested maps: the fold's maps are not packed regions");
    }
    let animated = program.body.iter().find(|s| matches!(s.node, C::StatementNode::Iterate { .. })).expect("an iterate statement");
    assert!(core_exec::footprint(&program, animated).is_none());
    assert!(core_exec::resident_live(&program, animated).is_some());
    // 1, 4 and 8 domains in RAYS_SLOW=1; the default loop keeps 1 and 4.
    let domain_counts: &[i64] = if std::env::var("RAYS_SLOW").as_deref() == Ok("1") { &[1, 4, 8] } else { &[1, 4] };
    for &domains in domain_counts {
        ts::with_domains(domains, &|| {
            let buffers = Buffers::create();
            let marks = ok(core_exec::draw(&prepare(&program), &live, (200, 100), &buffers, &draw_options(root, None)));
            match marks.as_slice() {
                [Entry::MarkSet { kind: Kind::Point { position: PairAttr::Buffer(b), .. }, count }] => {
                    let xs = buffers.find(b.id).expect("points buffer");
                    let mut sum = 0.;
                    for k in 0..*count {
                        sum += xs[(k * b.width) as usize] + xs[(k * b.width + 1) as usize] * 1000.;
                    }
                    assert!(*count > 4096);
                    if sum.to_bits() != expected.to_bits() {
                        panic!("nested maps: points differ from the walker at {domains} domains ({} vs {})", flow_syntax::lisp::float(sum), flow_syntax::lisp::float(expected));
                    }
                }
                _ => panic!("nested maps: one point set expected"),
            }
        });
    }
}

// A drawing's loops keep their last results by their inputs: the same inputs at another time give
// the fresh program's points, and changed inputs are never answered from the cache. [k] is drawn
// once before the fold, so its frame read is a statement of its own, outside the loop.
#[test]
fn remembered_loops_never_answer_changed_inputs_from_the_cache() {
    let body = "(let* [k (int (/ t 2)) p (fold [b (map (fn [i] [i 0 0]) (array/range 3))] [d (range 3)] (map (fn [i] (let* [q (array/nth b (int i))] [(+ q.x (* k 10)) (+ q.y d) 0])) (array/range 3)))] (draw/merge (draw/point [k 0 0]) (draw/points p)))";
    let (program, root) = lowered(body);
    let remembered = core_exec::prepare(&program, false, true, &Paths::new(), false);
    let points = |prepared: &core_exec::Prepared, t: f64| -> Vec<f64> {
        let buffers = Buffers::create();
        let live = Frame { size: (200, 100), ..input::at_time(t) };
        let marks = ok(core_exec::draw(prepared, &live, (200, 100), &buffers, &draw_options(root, None)));
        match marks.as_slice() {
            [_, Entry::MarkSet { kind: Kind::Point { position: PairAttr::Buffer(b), .. }, .. }] => buffers.find(b.id).expect("buffer").to_vec(),
            _ => panic!("remembered loops: a point and a point set expected"),
        }
    };
    for t in [0.25, 0.5, 3.0, 0.25, 5.0] {
        if points(&remembered, t) != points(&prepare(&program), t) {
            panic!("remembered loops: a cached frame at t={t} differs from a fresh one");
        }
    }
}

fn group_sizes(groups: &[core_draw::Group]) -> Vec<usize> { groups.iter().map(|g| g.members.len()).collect() }

fn groups(program: &C::Program, approx: &Paths, legal: &dyn Fn(&C::RegionT) -> bool) -> Vec<core_draw::Group> {
    core_draw::device_groups(approx, false, program, &|k| Some(program.regions[k as usize].outputs[0].len() as i64), legal).1
}

#[test]
fn compatible_regions_group_into_one_fused_dispatch() {
    let body = "(let* [a (+ t 2) b (+ t 4) p (map (fn [i] [(+ i a) i 0]) (array/range 4)) r (map (fn [i] (+ i b)) (array/range 4))] (draw/circles p :radius r))";
    let (program, root) = lowered(body);
    let empty = Paths::new();
    let always = |_: &C::RegionT| true;
    assert_eq!(group_sizes(&groups(&program, &empty, &always)), vec![2]);
    let first = groups(&program, &empty, &always).remove(0);
    let (_, site, _) = packed::site(&program.regions[first.members[0] as usize]);
    let mut approx = Paths::new();
    approx.insert(site);
    assert_eq!(groups(&program, &approx, &always).len(), 2);
    assert_eq!(groups(&program, &empty, &|_| false).len(), 2);
    let mut count = 0;
    let mut barrier = Vec::new();
    for s in &program.body {
        if matches!(s.node, C::StatementNode::Invoke { .. }) {
            count += 1;
            if count == 2 {
                barrier.push(C::Statement { node: C::StatementNode::Draw(C::Draw::Pop), ..s.clone() });
            }
        }
        barrier.push(s.clone());
    }
    assert_eq!(groups(&C::Program { body: barrier, ..program.clone() }, &empty, &always).len(), 2);
    let (static_program, _) = lowered("(let* [p (map (fn [i] [(+ i t) i 0]) (array/range 4)) r (map (fn [i] (+ i 4)) (array/range 4))] (draw/circles p :radius r))");
    // The static radius was already staged; it stays outside the live group.
    assert_eq!(group_sizes(&groups(&static_program, &empty, &always)), vec![1]);
    let (different, _) = lowered("(let* [p (map (fn [i] [(+ i t) i 0]) (array/range 4)) r (map (fn [i] (+ i t)) (array/range 3))] (draw/circles p :radius r))");
    assert_eq!(groups(&different, &empty, &always).len(), 2);
    let live = input::at_time(0.25);
    let prepared = core_exec::prepare(&program, false, true, &Paths::new(), false);
    let draw = |device: Option<DeviceFn>| core_exec::draw(&prepared, &live, (200, 100), &Buffers::create(), &draw_options(root, device));
    let expected = ok(draw(None));
    for refuse in [false, true] {
        let offers: Rc<std::cell::RefCell<Vec<usize>>> = Rc::new(std::cell::RefCell::new(Vec::new()));
        let record = offers.clone();
        let live_for_device = live.clone();
        let device: DeviceFn = Arc::new(move |packed: &packed::T| {
            let outputs = packed::force_all(packed, &live_for_device, &Default::default())?.len();
            record.borrow_mut().push(outputs);
            if refuse && outputs > 1 { Err(rays_diagnostic::error("E_GPU".into(), "injected fused refusal".into(), None, None, None, Vec::new(), Vec::new())) } else { Ok(None) }
        });
        assert!(ok(draw(Some(device))) == expected);
        assert_eq!(*offers.borrow(), vec![2, 1, 1]);
    }
    let live_for_device = live.clone();
    let device: DeviceFn = Arc::new(move |packed: &packed::T| match packed::force_all(packed, &live_for_device, &Default::default())?.as_slice() {
        [Value::Floats(3, positions), Value::Floats(1, radii)] => {
            assert!(**positions == vec![2.25, 0., 0., 3.25, 1., 0., 4.25, 2., 0., 5.25, 3., 0.]);
            assert!(**radii == vec![4.25, 5.25, 6.25, 7.25]);
            Ok(Some(vec![(0, 4, 3), (1, 4, 1)]))
        }
        _ => panic!("two fused outputs in authored order"),
    });
    ok(draw(Some(device)));
    let (bad, root) = lowered("(let* [p (map (fn [i] [(pow (+ i t) 1000) i 0]) (array/range 4)) r (map (fn [i] (pow (+ i t) 1001)) (array/range 4))] (draw/circles p :radius r))");
    let prepared = prepare(&bad);
    let fail = |device: Option<DeviceFn>| core_exec::draw(&prepared, &input::at_time(0.), (200, 100), &Buffers::create(), &draw_options(root, device));
    let cpu = fail(None);
    assert!(cpu.is_err());
    let declining: DeviceFn = Arc::new(|_| Ok(None));
    assert!(fail(Some(declining)) == cpu);
}

/// Records the inputs of each dispatch; nothing is read back.
struct FusedBackend { seen: Arc<Mutex<Vec<Vec<Arc<Vec<f64>>>>>>, serial: Arc<AtomicI64> }
struct FusedKernel { width: i64, seen: Arc<Mutex<Vec<Vec<Arc<Vec<f64>>>>>>, serial: Arc<AtomicI64> }

impl gpu::Backend for FusedBackend {
    fn cost(&self, _inputs: Option<&packed::Inputs>, _fast: bool, _packed: &packed::T, _count: i64) -> Option<f64> { Some(0.) }
    fn prepare(&self, _fast: bool, packed: &packed::T) -> Result<Arc<dyn gpu::Kernel>, rays_diagnostic::Diagnostic> {
        let width = host::view(packed).outputs[0].len() as i64;
        Ok(Arc::new(FusedKernel { width, seen: self.seen.clone(), serial: self.serial.clone() }))
    }
}

impl gpu::Kernel for FusedKernel {
    fn run(&self, inputs: &packed::Inputs) -> Result<gpu::Value, rays_diagnostic::Diagnostic> {
        self.seen.lock().unwrap().insert(0, inputs.arrays.clone());
        let stamp = self.serial.fetch_add(1, Ordering::SeqCst) + 1;
        Ok(gpu::Value { identity: 1, output: 0, count: inputs.count, width: self.width, stamp, gpu_seconds: None })
    }
    fn readback(&self, _value: &gpu::Value) -> Result<Value, rays_diagnostic::Diagnostic> { unreachable!("no readback in this test") }
    fn planes(&self, _value: &gpu::Value) -> Result<gpu::Planes, rays_diagnostic::Diagnostic> { unreachable!("no planes in this test") }
}

#[test]
fn fused_device_dispatches_share_their_inputs_and_follow_the_frame_count() {
    let seen: Arc<Mutex<Vec<Vec<Arc<Vec<f64>>>>>> = Arc::new(Mutex::new(Vec::new()));
    let serial = Arc::new(AtomicI64::new(0));
    let backend = {
        let seen = seen.clone();
        let serial = serial.clone();
        FusedBackend { seen, serial }
    };
    let gpu = gpu::Config { backend: Some(Arc::new(backend)), fast: true, policy: gpu::Policy::Qualification, readback_per_float: None, ..gpu::default() };
    let display = Rc::new(executor::region_display(Some(gpu), None, None));
    let prepared = |count: &str| {
        let body = format!("(draw/circles (map (fn [i] [(+ i t) i 0]) (array/range {count})) :radius (map (fn [i] (+ i t)) (array/range {count})))");
        let (program, root) = lowered(&body);
        (core_exec::prepare(&program, false, false, &Paths::new(), true), root)
    };
    let draw = |(program, root): &(core_exec::Prepared, Option<ResidualId>), time: f64| {
        let live = input::at_time(time);
        let display = display.clone();
        let live_for_device = live.clone();
        let device: DeviceFn = Arc::new(move |packed: &packed::T| {
            Ok(display.call(None, None, packed, &live_for_device)?.map(|v| host::view(packed).outputs.iter().map(|output| (v.identity, v.count, output.len() as i64)).collect()))
        });
        ok(core_exec::draw(program, &live, (200, 100), &Buffers::create(), &draw_options(*root, Some(device))));
    };
    let fixed = prepared("1024");
    draw(&fixed, 0.25);
    draw(&fixed, 0.5);
    {
        let seen_now = seen.lock().unwrap();
        assert_eq!(seen_now.len(), 2, "one fused dispatch each frame");
        let (second, first) = (&seen_now[0], &seen_now[1]);
        assert!(!first.is_empty() && !second.is_empty());
        assert!(first.iter().all(|values| Arc::ptr_eq(values, &first[0])));
        assert!(second.iter().all(|values| Arc::ptr_eq(values, &first[0])));
    }
    seen.lock().unwrap().clear();
    let dynamic = prepared("(+ 1024 (int t))");
    draw(&dynamic, 1.);
    draw(&dynamic, 2.);
    let counts: Vec<usize> = seen.lock().unwrap().iter().map(|arrays| arrays[0].len()).collect();
    let mut unique = counts.clone();
    unique.sort();
    unique.dedup();
    assert_eq!(unique, vec![1025, 1026]);
    assert!(counts[0] == 1026 && counts[counts.len() - 1] == 1025);
}

/// sibling maps over one range with an array op between them group where the first runs (a
/// resident fold's positions, normals and colors), and a merged region keeps its gathers' tables
#[test]
fn sibling_regions_group_at_the_first() {
    let body = "(let* [w (map (fn [i] (* i t)) (array/range 8)) a (map (fn [i] (+ (array/nth w (int i)) t)) (array/range 4)) k (array/int (array/count w) 1) b (map (fn [i] (* (array/nth w (int (+ i 1))) t)) (array/range 4)) p (map (fn [i] [(+ (array/nth a (int i)) (array/nth b (int i))) (array/nth k (int i)) 0]) (array/range 4))] (draw/circles p))";
    let (program, _) = lowered(body);
    let found = groups(&program, &Paths::new(), &|_| true);
    let sibling = found.iter().find(|g| g.at_first).expect("a sibling group");
    assert_eq!(sibling.members.len(), 2);
    assert_eq!(sibling.region.outputs.len(), 2);
    assert!(sibling.region.code.iter().any(|i| matches!(i, C::region::Instruction::Gather(..))));
}
