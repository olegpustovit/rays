use flow_ir::{core_draw, core_exec::{self, Buffers}};
use flow_native::{drawing, marks_scene as ms};
use rays::scene;
use rays_diagnostic::Diagnostic;
use scene_command::{marks::{self as m, Entry, Kind, PairAttr as P, ScalarAttr as S, ColorAttr as C}, ui_batch::Builder};
use std::cell::Cell;

fn ok<T>(r: Result<T, Diagnostic>) -> T { r.unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d))) }
fn evaluate(body: &str) -> flow_eval::Eval {
    let forms = ok(flow_syntax::syntax::parse(&format!("(workspace w (graph g :context draw {body}))")));
    let (ws, ds) = flow_check::workspace::check(&[], false, false, None, &flow_check::catalog::Catalog { version: 1, kinds: vec![] }, &forms);
    assert!(ds.is_empty(), "{ds:?}");
    ok(flow_eval::static_eval(&ws.expect("checked workspace")))
}
fn live() -> input::Frame { input::Frame { size: (200,100), ..input::at_time(0.25) } }
fn marks(body: &str) -> (Buffers, Vec<Entry>) {
    let e = evaluate(body);
    let (program, root) = core_draw::lower(None, &e.plan, &e.results.iter().find(|(n,_)| n == "g").unwrap().1).unwrap();
    let buffers = Buffers::create();
    let prepared = core_exec::prepare(&program, false, true, &Default::default(), false);
    let marks = ok(core_exec::draw(&prepared, &live(), (200,100), &buffers, &core_exec::DrawOptions { root, ..Default::default() }));
    (buffers, marks)
}
fn serialize(s: &[scene::node]) -> Vec<u8> { scene_command::render_ir::serialize(&scene::layer::to_ir(s).unwrap()) }
fn new_scene(body: &str) -> Result<Vec<scene::node>, Diagnostic> {
    let (buffers, marks) = marks(body);
    ms::scene(&buffers, None, None, None, None, &marks)
}
fn pictures() {
    let pictures = [
        r##"(draw/background "#14161c")"##,
        r##"(draw/merge (draw/background "#102030") (draw/rect [1 2 0] [30 40 0]) (draw/rect [1.5 2 0] [3 4 0] :stroke "#fff8") (draw/rect [0 0 0] [5 5 0] :fill "#f00" :stroke "#00f"))"##,
        r##"(draw/circle [10.5 20 0] 4)"##,
        r##"(draw/circle [10 20 0] (+ 4 (* 2 (sin t))) :fill "#5aaaf080" :stroke "#000")"##,
        r##"(draw/line [0 0 0] [5 5.5 0])"##,
        r##"(draw/line [0 0 0] [5 5 0] :color "#0f0" :width 3)"##,
        r##"(draw/point [3.9 -4.9 0] :color "#abc")"##,
        r##"(draw/translate [1.9 -2.5 0] (draw/merge (draw/point [0 0 0]) (draw/rotate (* t 40) (draw/scale 2 (draw/point [1 1 0])))))"##,
        r##"(draw/rotate (* t 4) (draw/translate [10 10 0] (draw/scale [2 3 0] (draw/rect [0 0 0] [4 4 0]))))"##,
        r##"(draw/scale [2 3 0] (draw/background "#000"))"##,
        r##"(draw/circles (map (fn [i] [(* i 7.5) (+ 3 (* (* i t) 9)) 0]) (array/range 5)))"##,
        r##"(let* [p (map (fn [i] [(* i 7.5) (+ 3 (* (* i t) 9)) 0]) (array/range 5)) r (map (fn [i] (* i 0.5)) (array/range 5))] (draw/circles p :radius r :stroke "#fff"))"##,
        r##"(let* [p (map (fn [i] [(* i 7.5) (+ 3 (* (* i t) 9)) 0]) (array/range 5)) c (map (fn [i] [(/ i 5) 0.5 0.25]) (array/range 5))] (draw/circles p :radius 3 :fill c))"##,
        r##"(let* [p (map (fn [i] [(* i 7.5) (+ 3 (* (* i t) 9)) 0]) (array/range 5))] (draw/rects p [10 10 0] :fill "#f00"))"##,
        r##"(let* [p (map (fn [i] [(* i 7.5) (+ 3 (* (* i t) 9)) 0]) (array/range 5)) c (map (fn [i] [(/ i 5) 0 0]) (array/range 5)) s (map (fn [i] [i 1 0]) (array/range 5))] (draw/rects p s :fill c :stroke "#123"))"##,
        r##"(let* [p (map (fn [i] [(* i 7.5) (+ 3 (* (* i t) 9)) 0]) (array/range 5))] (draw/lines p (map (fn [q] [q.y q.x 0]) p) :width 2))"##,
        r##"(let* [p (map (fn [i] [(* i 7.5) (+ 3 (* (* i t) 9)) 0]) (array/range 5)) c (map (fn [i] [(/ i 5) 0 1]) (array/range 5))] (draw/lines p p :color c))"##,
        r##"(let* [p (map (fn [i] [(* i 7.5) (+ 3 (* (* i t) 9)) 0]) (array/range 5))] (draw/merge (draw/points p :color "#1e1e1e") (draw/points p)))"##,
        r##"(let* [p (map (fn [i] [i 0 0]) (array/range 0))] (draw/merge (draw/circles p) (draw/rects p [1 1 0]) (draw/points p) (draw/lines p p)))"##,
        r##"(draw/rounded_rect [4 5 0] [60 40 0] :radius 8 :fill "#123" :stroke "#fff")"##,
        r##"(draw/rounded_rect [4 5 0] [60 40 0])"##,
        r##"(draw/translate [3 4 0] (draw/rounded_rect [4 5 0] [-60 40 0] :radius 80 :stroke "#f0f"))"##,
        r##"(draw/polygon (map (fn [i] [i (* i i) 0]) (array/range 4)) :stroke "#fff")"##,
        r##"(draw/polygon (map (fn [i] [i (* i i) 0]) (array/range 4)))"##,
        r##"(draw/polyline (map (fn [i] [i (* i i) 0]) (array/range 4)) :color "#f0f")"##,
        r##"(draw/merge (draw/text [1 2 0] "") (draw/circle [5 5 0] 2 :fill "#fff"))"##,
    ];
    let digests = [
        "f4ff989c22afb8f52ab260b54d17bb7891e07ab75f29b2de12f487f832f3d92a",
        "38167b07c39a01579645ef0d43fd6ec8c1b524edc07e49436d9754eecbb11d2c",
        "66494d0f3ffb1159cdf6fb629fbd8105e690fa96d3f4956f4edf8b52e7eba59f",
        "c3ffa1398d4a79818e0afa089e1131a048b20881dbf0858482fa7f0c28bfcffd",
        "23a418fc491c5d3a95aa3029d2113bf002423a4a78e65dfc8a71c79818b8c89b",
        "3b93ccef6bb7daa17148022b6a0fdac1b409ce7c2ad6a370c4797151e146a5d1",
        "988dd6e539be91ba2f51ced75a01ac4deeaa2480a60aaa3a9c3039e06cf369eb",
        "1113c42cc2148b7eff360ec11fbdf7cd1735930475d0ccef8429d6a75d0118a5",
        "68ecc6c3903328f5dad628fb39ab2fdcbbd03b55ce2caac17cfd4f3b52693483",
        "71f685a283353a1f6081e6fa92ec9ad61674563f3ef8cf8d9e8309814bbef0f6",
        "14a723a3cffd08ceec72997d010416750af9e001bf15283d13d495d083f2faa2",
        "c0bcc14d631dc745aa86c73d3fff98fce242b956f3e8924e187d20d0267dcff6",
        "d175863740935e5233b0a46f97e47a31a8233830684c736b444fe95c1b666401",
        "5f1de8f73c12b33352bb627731d0c3cd347c07c818278f41d81fefd93bdbba37",
        "34a94c9916d7321808a516723e5d5b0d51ddaedbf6671ee96293b6e58b9fcd4a",
        "e560c7f427e139fcaf098f3993ff6ba3b3ac536391b2b8e504f56da2d52b4654",
        "53eee607276c67a0280cb9fcbb59e5ebc81e0f6502a82077c0c2bdc7d7323c8b",
        "4622e6a3ff25f8cc3c389f231513adc2a69e0bf11f4db96d6b072e8db860beb8",
        "6b3831072362e57f0f8b90919a3f922911b46d54d87b1eaed1d45b82d3033a33",
        // stroked paths (19, 21, 22) and the polyline (24) follow Z6's stroke triangle vertex order (9deaa70d)
        "8818d6ca8c41ddf418d49d7d02e34f1331a492083ce4aea89b0cc915af813c23",
        "447a8178eb85cb3be6245c4d0ba5e559ef876fb7495596970eaa09f8b9b5fadb",
        "cf089aabca2fc59b4556e90ed66ba5b75dcd273bd4425a2b1c1173404b9bac92",
        "9477535cbdc70565f24764bf56a79d59dff009a18114430db0e12fd5c8c60583",
        "b967bc0170e57ab1d5239e87988c193dc731db361c815ff8edfd089fe9526c53",
        "2e4fe0387832fb887cd208451e9ea48be546c95f17ee99c6e7adcb1456ce2775",
        "bc9a96cc00caf757afb46c0b94ab6e514f8676ad396541d50171b303b82e9c73",
    ];
    assert_eq!(pictures.len(), digests.len());
    assert_eq!(sha256(b"abc"), editor_document::lowering::sha256("abc"));
    for (i, body) in pictures.iter().enumerate() {
        assert_eq!(sha256(&serialize(&ok(new_scene(body)))), digests[i], "picture {i}: {body}");
    }
}

fn fractional_points() {
    let point = ok(new_scene(r##"(draw/point [3.25 -4.75 0] :color "#abc")"##));
    let expected: Vec<_> = scene_command::shape2::polygon(&[(3.25,-4.75),(4.25,-4.75),(4.25,-3.75),(3.25,-3.75)], Some(0xaabbccffu32 as i32), None).unwrap().into_iter().map(scene::geometry).collect();
    assert_eq!(serialize(&point), serialize(&expected));
    let points = ok(new_scene(r##"(draw/points (map (fn [i] [(+ 1.25 (* i 2.25)) (+ -2.75 (* i 3.5)) 0]) (array/range 2)) :color "#def")"##));
    let mut b = Builder::with_capacity(2);
    b.shape_rect(1.25,-2.75,1.,1.,0xddeeffffu32 as i32,0).unwrap();
    b.shape_rect(3.5,0.75,1.,1.,0xddeeffffu32 as i32,0).unwrap();
    assert_eq!(serialize(&points),serialize(&[scene::shapes(b.publish().unwrap())]));
}
fn hand_marks() {
    let angle: f64 = 10.;
    let entries = [
        Entry::MarkSet { count:1, kind:Kind::Circle { position:P::Constant((1.5,2.)), radius:S::Constant(3.), fill:C::Constant(0xff0000ffu32 as i32), stroke:C::Constant(0), stroke_width:S::Constant(1.) } },
        Entry::PushTransform { xx:angle.cos(),xy:angle.sin(),yx:-angle.sin(),yy:angle.cos(),tx:0.,ty:0. },
        Entry::MarkSet { count:0, kind:Kind::Rect { position:P::Constant((0.,0.)),size:P::Constant((1.,1.)),fill:C::Constant(0),stroke:C::Constant(0),stroke_width:S::Constant(1.) } },
        Entry::PopTransform,
    ];
    let buffers = Buffers::create();
    let mut b = Builder::create();
    b.circle(1.5,2.,3.,Some(0xff0000ffu32 as i32),Some(0),1.).unwrap();
    let expected = vec![scene::shapes(b.publish().unwrap()), scene::rotate(angle,vec![scene::shapes(Builder::create().publish().unwrap())])];
    assert_eq!(serialize(&ok(ms::scene(&buffers,None,None,None,None,&entries))),serialize(&expected));
    assert!(ms::scene(&buffers,None,None,None,None,&[Entry::PopTransform]).is_err());
    let image = Entry::Image { resource:1,x:0,y:0,scale:1.,angle:0. };
    match ms::scene(&buffers,None,None,None,None,&[image]) { Err(d) => assert_eq!(d.code,"E_IMAGE"), Ok(_) => panic!("image without resolver accepted") }
}
fn prepare(n: usize) -> drawing::prepared {
    let e = evaluate(&format!(r##"(draw/translate [(* 10 (sin t)) 0 0] (draw/rects (map (fn [i] [(* (mod i 800) 1.0) (/ i 1250) 0]) (array/range {n})) [3 3 0] :fill "#5aaaf0"))"##));
    ok(drawing::prepare(&drawing::PrepareOptions { states:e.states.clone(), ..Default::default() },&e.plan,&e.results.iter().find(|(n,_)| n=="g").unwrap().1))
}
fn frame(p: &drawing::prepared,s: &flow_eval::State,i: i64) -> Vec<scene::node> {
    let live = input::Frame { frame:i,tick:i,size:(800,600),..input::at_time(i as f64/60.) };
    ok(drawing::render_prepared(p,&live,live.size,&drawing::RenderOptions { state:Some(s.clone()),..Default::default() }))
}
fn retention() {
    let allocated = |n| {
        let p = prepare(n); let s = flow_eval::create_state();
        drop(frame(&p,&s,0)); drop(frame(&p,&s,1));
        let before = mem_sys::alloc_bytes_total();
        let scene = frame(&p,&s,2);
        let bytes = mem_sys::alloc_bytes_total()-before;
        drop(scene); bytes
    };
    let small = allocated(10_000); let large = allocated(100_000);
    assert!(large <= small+100_000,"moving static batch: {small} at 10,000, {large} at 100,000");
    let p = prepare(2000); let s = flow_eval::create_state(); drop(frame(&p,&s,0));
    assert_eq!(serialize(&frame(&p,&s,3)),serialize(&frame(&prepare(2000),&flow_eval::create_state(),3)));
}
fn device_registration() {
    let registered = std::rc::Rc::new(Cell::new(0));
    let count = registered.clone();
    let gpu = move |_:i64,_:i64,_:i64,_:&[ms::attribute]| { count.set(count.get()+1); Ok(m::gpu_token(1,2,1).unwrap()) };
    let device = |_| Ok(flow_ir::flow_ir::gpu::Value { identity:1,count:2,width:1,output:0,stamp:1,gpu_seconds:None });
    let run = |x| {
        let (buffers, mut entries) = marks(&format!("(draw/circles (map (fn [i] [(+ {x} i) 0 0]) (array/range 2)) :radius (map (fn [i] 1.0) (array/range 2)))"));
        for entry in &mut entries { if let Entry::MarkSet { kind:Kind::Circle { radius,.. },.. } = entry { if let S::Buffer(b)=radius { *radius=S::Device(*b); } } }
        ms::scene(&buffers,None,Some(&gpu),None,Some(&device),&entries)
    };
    drop(ok(run("1.0"))); assert_eq!(registered.get(),1);
    for bad in ["3.5e38","1e300"] { match run(bad) { Err(d)=>assert_eq!(d.code,"E_DRAW_RANGE"),Ok(_)=>panic!("bad host coordinate reached GPU") } }
    assert_eq!(registered.get(),1);
}
fn builder_case(index: usize) {
    let range="(array/range 9)";
    let colours=format!("(map (fn [i] [1 0.5 0.25]) {range})");
    let p=format!("(map (fn [i] [(* i 7.3) (- 3.1 (* i 0.7)) 5]) {range})");
    let q=format!("(map (fn [i] [(+ 2.2 (* i 1.9)) (* (* i i) 0.3) 0]) {range})");
    let floats=|bad|format!("(map (fn [i] (if (= i 6) {bad} (+ 0.4 (* i 0.3)))) {range})");
    let sizes=|bad|format!("(map (fn [i] [(if (= i 4) {bad} (+ 1.1 i)) 2.3 0]) {range})");
    let far=|bad|format!("(map (fn [i] [(if (= i 2) {bad} (* i 7.3)) 1.7 0]) {range})");
    let make=|c:&str|match index {
        0=>format!("(draw/circles {p} :radius 2.5 :fill {c})"),
        1=>format!("(draw/circles {p} :radius {} :fill {c})",floats("1.6")),
        2=>format!("(draw/circles {p} :radius {} :stroke {c})",floats("1.6")),
        3=>format!("(draw/circles {p} :radius {} :fill {c})",floats("-0.5")),
        4=>format!("(draw/circles {} :radius 1 :fill {c})",far("1e300")),
        5=>format!("(draw/rects {p} [4.5 3.25 0] :fill {c})"),
        6=>format!("(draw/rects {p} {} :stroke {c})",sizes("0.75")),
        7=>format!("(draw/rects {p} {} :fill {c})",sizes("-1")),
        8=>format!("(draw/rects {} [1 1 0] :fill {c})",far("3.5e38")),
        9=>format!("(draw/lines {p} {q} :color {c} :width 1.5)"),
        10=>format!("(draw/lines {p} {} :color {c})",far("-1e300")),
        _=>unreachable!(),
    };
    let result=|body:&str|new_scene(body).map(|s|serialize(&s)).map_err(|d|(d.code,d.message));
    let whole=result(&make("\"#ff8040\"")); let each=result(&make(&colours));
    assert_eq!(whole,each,"constant-colour builder case {index}");
    if matches!(index,3|4|7|8|10) {
        let (code,message)=whole.unwrap_err(); assert_eq!(code,"E_DRAW_RANGE"); assert!(message.starts_with("Ui_batch"),"{message}");
    } else { whole.unwrap(); }
}
fn allocation() {
    let (buffers, marks) = marks(r##"(draw/circles (map (fn [i] [(* i 0.001) (* i 0.002) 0]) (array/range 100000)) :radius 0.6 :fill "#76baca")"##);
    let before = mem_sys::alloc_bytes_total();
    let scene = ok(ms::scene(&buffers,None,None,None,None,&marks));
    let bytes = mem_sys::alloc_bytes_total()-before;
    match scene::layer::commands(None,&scene).unwrap().as_slice() {
        [scene_command::render_ir::Command::Shapes(batch)]=>assert_eq!(batch.count(),100000),
        _=>panic!("a circle set is one Shapes command"),
    }
    assert!(bytes <= 72*100000,"static circle set allocates {} bytes per mark",bytes as f64/100000.);
}
fn bounded_cache() {
    let sets:Vec<_>=(0..3000).map(|i|Entry::MarkSet { count:1,kind:Kind::Point { position:P::Constant((i as f64,0.)),fill:0 } }).collect();
    let cache=ms::cache_new();
    drop(ok(ms::scene(&Buffers::create(),None,None,Some(&cache),None,&sets)));
    assert!(ms::cache_size(&cache)<=1024);
}
pub fn cases() -> Vec<(&'static str, fn())> {
    vec![
        ("marks_scene_fractional_points",fractional_points),
        ("marks_scene_picture_digests",pictures),
        ("marks_scene_hand_marks",hand_marks),
        ("marks_scene_retention",retention),
        ("marks_scene_device_registration",device_registration),
        ("marks_scene_static_allocation",allocation),
        ("marks_scene_bounded_cache",bounded_cache),
        ("marks_scene_circles_constant_radius",||builder_case(0)),
        ("marks_scene_circles_radius_array",||builder_case(1)),
        ("marks_scene_circles_stroke",||builder_case(2)),
        ("marks_scene_circles_negative_radius",||builder_case(3)),
        ("marks_scene_circles_binary32",||builder_case(4)),
        ("marks_scene_rects_constant_size",||builder_case(5)),
        ("marks_scene_rects_size_array",||builder_case(6)),
        ("marks_scene_rects_negative_size",||builder_case(7)),
        ("marks_scene_rects_binary32",||builder_case(8)),
        ("marks_scene_lines",||builder_case(9)),
        ("marks_scene_lines_binary32",||builder_case(10)),
    ]
}

// Same SHA-256 implementation as editor_document::lowering, accepting arbitrary bytes.
const K: [u32; 64] = [
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
    0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
    0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
];

/// Lower-case hex SHA-256: the digest of a sketch source.
fn sha256(text: &[u8]) -> String {
    let mut h: [u32; 8] = [0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19];
    let mut data = text.to_vec();
    let bits = (data.len() as u64).wrapping_mul(8);
    data.push(0x80);
    while data.len() % 64 != 56 {
        data.push(0);
    }
    data.extend_from_slice(&bits.to_be_bytes());
    for chunk in data.chunks(64) {
        let mut m = [0u32; 64];
        for i in 0..16 {
            m[i] = u32::from_be_bytes([chunk[4 * i], chunk[4 * i + 1], chunk[4 * i + 2], chunk[4 * i + 3]]);
        }
        for i in 16..64 {
            let s0 = m[i - 15].rotate_right(7) ^ m[i - 15].rotate_right(18) ^ (m[i - 15] >> 3);
            let s1 = m[i - 2].rotate_right(17) ^ m[i - 2].rotate_right(19) ^ (m[i - 2] >> 10);
            m[i] = m[i - 16].wrapping_add(s0).wrapping_add(m[i - 7]).wrapping_add(s1);
        }
        let [mut a, mut b, mut c, mut d, mut e, mut f, mut g, mut hh] = h;
        for i in 0..64 {
            let s1 = e.rotate_right(6) ^ e.rotate_right(11) ^ e.rotate_right(25);
            let ch = (e & f) ^ (!e & g);
            let t1 = hh.wrapping_add(s1).wrapping_add(ch).wrapping_add(K[i]).wrapping_add(m[i]);
            let s0 = a.rotate_right(2) ^ a.rotate_right(13) ^ a.rotate_right(22);
            let maj = (a & b) ^ (a & c) ^ (b & c);
            let t2 = s0.wrapping_add(maj);
            hh = g;
            g = f;
            f = e;
            e = d.wrapping_add(t1);
            d = c;
            c = b;
            b = a;
            a = t1.wrapping_add(t2);
        }
        for (x, y) in h.iter_mut().zip([a, b, c, d, e, f, g, hh]) {
            *x = x.wrapping_add(y);
        }
    }
    h.iter().map(|x| format!("{x:08x}")).collect()
}
