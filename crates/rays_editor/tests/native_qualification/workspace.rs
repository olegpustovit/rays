//! Behavioral ports of test_workspace_gpu_native.ml and test_workspace_gpu_marks.ml.
use crate::common;
use flow_eval as e;
use flow_native::{drawing, workspace_gpu as wg, workspace_images as wi, workspace_resources as wr};
use flow_sop::{lower as l, network, value_lane};
use flow_syntax::{ty, value::Value};
use rays::{canvas, image};
use rays_diagnostic::Diagnostic;
use runtime_resources::Image as Native;
use sop::{context, edit_graph, image as si, node, payload, session};
use std::{cell::Cell, rc::Rc, sync::Arc};

fn ok<T>(r: Result<T, Diagnostic>) -> T { r.unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d))) }
fn lower(text: &str) -> l::T { ok(l::workspace(None, Vec::new(), l::Options::default(), &ok(flow_syntax::syntax::parse(text)))) }
fn at(frame: i64) -> input::Frame { input::Frame { frame, ..input::at_time(frame as f64) } }
fn value(id: i64) -> Value { Value::Deferred(ty::image(), id) }
fn node_of<'a>(p: &'a l::T, kind: &str) -> &'a e::Node { p.plan.nodes.iter().find(|n| n.kind == kind).unwrap_or_else(|| panic!("missing {kind}")) }
fn bytes(p: &si::T) -> Vec<u8> { si::rgba8(p).expect("RGBA8 payload").to_vec() }
fn handles() -> i64 { ogpu::impl_::live_handles().unwrap() }
fn result(p: &l::T, name: &str) -> Value { p.evaluated.results.iter().find(|(n, _)| n == name).unwrap().1.clone() }
struct Owner { images: wi::t, resources: wr::t, gpu: wg::t }
impl Owner {
    fn new(domains: i64, qualification: bool) -> Self {
        let gpu = wg::create(None);
        if qualification { wg::qualification(&gpu); }
        let resources = wr::create();
        let images = wi::create(None, None, Some(domains), &gpu, &resources);
        Self { images, resources, gpu }
    }
    fn payload(&self, p: &l::T, s: &e::State, live: &input::Frame, v: &Value) -> Result<si::T, Diagnostic> {
        wi::payload(&self.images)(None, None, &p.plan, s, live, v)
    }
    fn image(&self, p: &l::T, s: &e::State, live: &input::Frame, v: &Value) -> Result<image::t, Diagnostic> {
        wi::image(None, true, &self.images, s, live, &p.plan, v)
    }
    fn close(&self) { wr::close(&self.resources); wi::close(&self.images); wg::close(&self.gpu); }
    fn created(&self) -> i64 { self.resources.0.borrow().images_created }
    fn balanced(&self) -> bool { let r = self.resources.0.borrow(); r.images_created == r.images_destroyed }
}
impl Drop for Owner { fn drop(&mut self) { self.close(); } }
fn advance(p: &l::T, state: &e::State, frame: i64) { for v in &p.states { ok(e::force(v, &at(frame), Some(state))); } }
fn near(actual: &[u8], expected: &[u8]) {
    assert_eq!(actual.len(), expected.len());
    assert!(actual.iter().zip(expected).all(|(&a, &b)| a.abs_diff(b) <= 1), "GPU image differs by more than one channel unit");
}
fn packed_node<'a>(p: &'a l::T, overridden: bool, kind: &str) -> &'a e::Node {
    p.plan.nodes.iter().find(|n| {
        let matches = n.kind == kind || (n.kind == "$card" && p.compiled.get(&n.id).and_then(|id| (p.card_node)(*id).ok()).is_some_and(|c| format!("sop/{}", node::operation(&c)) == kind));
        matches && p.plan.instances[n.inst as usize].graph == "mesh" && p.plan.instances[n.inst as usize].default != overridden
    }).unwrap_or_else(|| panic!("missing packed node {kind}, override={overridden}"))
}
fn inspect_packed(p: &l::T, domains: i64, state: &e::State, live: &input::Frame, overridden: bool) {
    let source = packed_node(p, overridden, "sop/copy_to_points").id;
    let context_ = ok(l::source_context(p, source));
    let (cone, root) = ok(l::source_cone(&context_, source));
    let lane = value_lane::create(Some(e::fork_state(state)));
    let resolved = ok(value_lane::resolve(None, Some(live), &lane, live.t, &cone));
    let root = edit_graph::compile_node(&resolved.geometry, root).unwrap();
    let mut session = session::create(0, 0).unwrap();
    let ctx = context::create(context::Options { domains: Some(domains), ..Default::default() }).unwrap();
    let output = ok(session::cook(&mut session, None, &ctx, &root));
    assert_eq!(rdk_core::geometry::point_count(&payload::geometry(&output.payload).unwrap()), 3);
    assert_eq!(output.instances.as_ref().unwrap().len(), 2);
    session::close(&mut session);
}
fn packed_expected(offset: f64, bias: f64) -> Vec<u8> {
    let sum_x = 0.03125 + 3. * (offset + bias - 0.03125);
    let rgba: Vec<_> = (0..65 * 17 * 4).map(|i| match i % 4 {
        0 => ((i / 4 % 65) as f64 + 0.5) / 65. + sum_x / 4.,
        1 => ((i / 4 / 65) as f64 + 0.5) / 17. + offset + bias - 0.0625,
        2 => 6. * 0.0625,
        _ => 1.,
    }).collect();
    bytes(&ok(si::of_vec4(&context::create(Default::default()).unwrap(), 65, 17, &rgba)))
}
fn packed_capture(stateful: bool) {
    assert!(common::gpu(), "native Vulkan device required");
    let bias = if stateful { "(state [n 0.015625] (+ n 0.015625))" } else { "(* t 0.015625)" };
    let p = lower(&format!(r##"(workspace packed_capture
    (graph drawing :context draw [(image : image (image/noise :width 1 :height 1))] (draw/image image))
    (graph mesh :context sop [(offset : float 0.125)]
      (let* [bias {bias}
             prototype (sop/curve (list [0 0 0] [0.03125 0.015625 0] [-0.015625 0 0.03125]))
             colored (sop/with_attr prototype :Cd
               (exact (map (fn [p] [0.03125 0.0625 0.125]) (sop/attr prototype :P))))
             targets (sop/curve (list [(+ offset bias) 0.03125 0] [-0.03125 0.0625 0.015625]))
             geo (sop/copy_to_points colored targets :pack true)
             p (reduce + [0 0 0] (sop/attr geo :P))
             a (array/nth (sop/attr geo :P) 0)
             b (array/nth (sop/attr geo :P) 3)
             c (reduce + [0 0 0] (sop/attr geo :Cd))
             img (image/map (fn [uv] [(+ uv.x (/ p.x 4)) (+ uv.y (+ a.x (* b.x 2))) c.y 1])
               :width 65 :height 17)
             frozen (exact img)
             parent (image/render (ref drawing :image img) :width 65 :height 17)] geo))
    (graph other :context sop (ref mesh :offset 0.25))
    (graph backdrop :context draw (draw/background "#102030"))
    (graph fixed :context image (image/render (ref backdrop) :width 65 :height 17)))"##));
    let owners = [Owner::new(1, false), Owner::new(8, false), Owner::new(1, true), Owner::new(8, true)];
    for o in &owners { wi::bind(&o.images, &p); }
    let before = handles();
    let state = e::create_state();
    let backdrops: Vec<_> = owners[2..].iter().map(|o| {
        let i = ok(o.image(&p, &state, &at(0), &result(&p, "fixed")));
        let generation = Native::generation(&image::resource(&i)); (i, generation)
    }).collect();
    let mut retained: Vec<(si::T, Vec<u8>)> = Vec::new();
    let mut check = |state: &e::State, frame: i64, bias: f64| {
        let live = at(frame);
        let stamp = e::state_stamp(state);
        for overridden in [false, true] {
            let expected = packed_expected(if overridden { 0.25 } else { 0.125 }, bias);
            for domains in [1, 8] { inspect_packed(&p, domains, state, &live, overridden); }
            let v = |kind| value(packed_node(&p, overridden, kind).id);
            let one = ok(owners[0].payload(&p, state, &live, &v("image/map")));
            let eight = ok(owners[1].payload(&p, state, &live, &v("image/map")));
            assert_eq!(bytes(&one), expected); assert_eq!(bytes(&eight), expected);
            if retained.is_empty() { retained.push((one.clone(), bytes(&one))); }
            for o in &owners[2..] {
                let child = ok(o.image(&p, state, &live, &v("image/map")));
                let native = image::resource(&child);
                assert_eq!(Native::cpu_storage_bytes(&native), 0);
                let actual = image::pixels(&child).unwrap(); near(&actual, &expected);
                let parent = ok(o.image(&p, state, &live, &v("image/render")));
                near(&image::pixels(&parent).unwrap(), &expected);
                let stats = wi::capture_stats(&o.images);
                let reads = wg::image_stats(&o.gpu).0.status_reads;
                let parent_native = image::resource(&parent);
                let child_reads = Native::readbacks(&native);
                let parent_reads = Native::readbacks(&parent_native);
                assert!(image::same(&ok(o.image(&p, state, &live, &v("image/map"))), &child));
                assert!(image::same(&ok(o.image(&p, state, &live, &v("image/render"))), &parent));
                assert_eq!(wi::capture_stats(&o.images), stats);
                assert_eq!(Native::readbacks(&native), child_reads);
                assert_eq!(Native::readbacks(&parent_native), parent_reads);
                let host = wg::image_stats(&o.gpu).0;
                assert_eq!(host.status_reads, reads); assert_eq!(host.readback_bytes, 0);
                assert_eq!(bytes(&ok(o.payload(&p, state, &live, &v("image/map")))), expected);
                assert_eq!(Native::cpu_storage_bytes(&native), 0);
                if frame == 0 {
                    let frozen = ok(o.payload(&p, state, &live, &v("exact")));
                    assert_eq!(bytes(&frozen), actual);
                    retained.push((frozen.clone(), bytes(&frozen)));
                }
            }
        }
        for (o, (saved, generation)) in owners[2..].iter().zip(&backdrops) {
            let fixed = ok(o.image(&p, &state, &at(0), &result(&p, "fixed")));
            assert!(image::same(&fixed, saved)); assert_eq!(Native::generation(&image::resource(&fixed)), *generation);
        }
        assert_eq!(e::state_stamp(state), stamp);
        for (payload, saved) in &retained { assert_eq!(&bytes(payload), saved); }
    };
    if stateful { advance(&p, &state, 0); }
    check(&state, 0, if stateful { 0.03125 } else { 0. });
    if stateful { advance(&p, &state, 1); }
    check(&state, 1, if stateful { 0.046875 } else { 0.015625 });
    if stateful {
        let fresh = e::create_state(); advance(&p, &fresh, 1);
        check(&fresh, 1, 0.03125); check(&state, 1, 0.046875);
    }
    for o in &owners[2..] {
        let (metadata, data, charged, cooks, flattens) = wi::capture_stats(&o.images);
        let expected = if stateful { 8 } else { 4 };
        assert_eq!((metadata, data, cooks, flattens), (2, 2, expected, 2 * expected));
        assert!(charged <= 64 * 1024 * 1024);
        let host = wg::image_stats(&o.gpu).0;
        assert_eq!(host.status_reads, expected); assert_eq!(host.readback_bytes, 0);
    }
    for o in &owners { o.close(); assert!(o.balanced()); }
    assert_eq!(handles(), before);
}
fn packed_live() { packed_capture(false); }
fn packed_state() { packed_capture(true); }

fn state_failure(domains: i64) {
    assert!(common::gpu(), "native Vulkan device required");
    let p = lower(r#"(workspace state_failure
    (graph level :context value (state [n 0] (+ n 1)))
    (graph drawing :context draw [(image : image (image/noise :width 1 :height 1))] (draw/image image))
    (graph mesh :context sop
      (let* [points (if (< (ref level) 2)
                     (list [0 0 0] [0.015625 0 0] [0.03125 0 0]) (list [0 0 0] [0.015625 0 0]))
             geo (sop/curve points)
             point (array/nth (sop/attr geo :P) 2)
             img (image/map (fn [uv] [(+ uv.x point.x) uv.y 0.5 1]) :width 65 :height 17)
             parent (image/render (ref drawing :image img) :width 65 :height 17)] geo)))"#);
    let o = Owner::new(domains, true); wi::bind(&o.images, &p);
    let state = e::create_state(); let before = handles();
    let child = value(node_of(&p, "image/map").id); let parent = value(node_of(&p, "image/render").id);
    advance(&p, &state, 0);
    let saved = ok(o.payload(&p, &state, &at(0), &child)); let saved_bytes = bytes(&saved);
    let first = ok(o.image(&p, &state, &at(0), &parent)); let original = image::pixels(&first).unwrap();
    assert_eq!(original, saved_bytes); assert_eq!(o.created(), 2);
    advance(&p, &state, 1); let stamp = e::state_stamp(&state);
    for _ in 0..2 {
        assert_eq!(o.image(&p, &state, &at(1), &parent).err().expect("invalid cardinality must refuse").code, "E_ARRAY_RANGE");
        assert_eq!(e::state_stamp(&state), stamp); assert_eq!(o.created(), 2); assert_eq!(bytes(&saved), saved_bytes);
        assert!(image::pixels(&first).is_err());
    }
    let fresh = e::create_state(); advance(&p, &fresh, 1); let fresh_stamp = e::state_stamp(&fresh);
    let recovered = ok(o.image(&p, &fresh, &at(1), &parent));
    assert!(image::same(&recovered, &first)); assert_eq!(image::pixels(&recovered).unwrap(), original);
    assert_eq!(e::state_stamp(&fresh), fresh_stamp); assert_eq!(e::state_stamp(&state), stamp); assert_eq!(o.created(), 2);
    assert_eq!(bytes(&ok(o.payload(&p, &fresh, &at(1), &child))), saved_bytes);
    o.close(); assert_eq!(bytes(&saved), saved_bytes); assert_eq!(handles(), before); assert!(o.balanced());
}
fn state_failure_one() { state_failure(1); }
fn state_failure_eight() { state_failure(8); }

fn lower_frozen(w: i64, h: i64) -> l::T { lower(&format!(r#"(workspace frozen
    (graph img :context image
      (let* [bias (* t 0.125)]
        (image/map (fn [uv] [bias uv.y (- 0.5 0.000000001) 1]) :width {w} :height {h})))
    (graph frozen :context image (exact (ref img)))
    (graph picture :context draw [(image : image (image/noise :width 1 :height 1))]
      (draw/image image))
    (graph composed :context draw (ref picture :image (ref frozen))))"#)) }
fn frozen_exact() {
    assert!(common::gpu(), "native Vulkan device required");
    let o = Owner::new(1, true); let state = e::create_state(); let stamp = e::state_stamp(&state); let before = handles();
    let mut p = lower_frozen(65, 17); wi::bind(&o.images, &p);
    let get = |p: &l::T, kind, t| ok(o.payload(p, &state, &input::at_time(t), &value(node_of(p, kind).id)));
    let img = |p: &l::T, kind, t| ok(o.image(p, &state, &input::at_time(t), &value(node_of(p, kind).id)));
    let ordinary = get(&p, "image/map", 0.); assert_eq!(bytes(&ordinary)[2], 127); assert_eq!(o.created(), 0);
    let first = get(&p, "exact", 0.); let saved_bytes = bytes(&first);
    let frozen = img(&p, "exact", 0.); let child = img(&p, "image/map", 0.); let native = image::resource(&child);
    assert_eq!(saved_bytes[2], 128); assert!(!image::same(&frozen, &child)); assert_eq!(Native::readbacks(&native), 1); assert_eq!(o.created(), 2);
    assert!(si::ptr_eq(&get(&p, "exact", 0.), &first)); assert!(si::ptr_eq(&get(&p, "image/map", 0.), &ordinary)); assert_eq!(Native::readbacks(&native), 1);
    let prepared = ok(drawing::prepare(&Default::default(), &p.plan, &result(&p, "composed")));
    let (images, plan, state2) = (o.images.clone(), p.plan.clone(), state.clone());
    let scene = ok(drawing::render_prepared(&prepared, &input::at_time(0.), (65, 17), &drawing::RenderOptions {
        state: Some(state.clone()), image: Some(Rc::new(move |v| wi::image(None, true, &images, &state2, &input::at_time(0.), &plan, v))), ..Default::default()
    }));
    let render_saved = || {
        let c = canvas::create(65, 17).unwrap(); canvas::render(None, &c, &scene).unwrap();
        let i = canvas::to_image(&c).unwrap(); let pixels = image::pixels(&i).unwrap();
        image::destroy(&i).unwrap(); canvas::destroy(&c).unwrap(); pixels
    };
    let saved_scene = render_saved(); assert_eq!(saved_scene, saved_bytes);
    let second = get(&p, "exact", 1.);
    assert!(!si::ptr_eq(&second, &first)); assert_ne!(bytes(&second), saved_bytes); assert_eq!(bytes(&first), saved_bytes);
    assert!(!image::same(&img(&p, "exact", 1.), &frozen)); assert_eq!(Native::readbacks(&native), 2); assert_eq!(render_saved(), saved_scene);
    let texture = Native::gpu_snapshot(&native).unwrap().unwrap().3;
    let available = Rc::new(Cell::new(true)); let flag = available.clone();
    Native::replace_gpu_source(&native, 65, 17, Rc::new(move || flag.get().then(|| texture.clone()))).unwrap();
    let third = get(&p, "exact", 1.); let reads = Native::readbacks(&native); let created = o.created(); let generation = Native::generation(&native);
    available.set(false);
    for _ in 0..2 { assert_eq!(o.payload(&p, &state, &input::at_time(1.), &value(node_of(&p, "exact").id)).err().expect("expired source").code, "E_IMAGE"); }
    assert_eq!(Native::readbacks(&native), reads); assert_eq!(o.created(), created); assert_eq!(Native::generation(&native), generation); assert_eq!(e::state_stamp(&state), stamp);
    available.set(true); assert!(si::ptr_eq(&get(&p, "exact", 1.), &third)); assert_eq!(Native::readbacks(&native), reads);
    assert!(o.payload(&p, &state, &input::at_time(f64::INFINITY), &value(node_of(&p, "exact").id)).is_err());
    assert_eq!(e::state_stamp(&state), stamp); assert_eq!(bytes(&first), saved_bytes);
    let recovered = get(&p, "exact", 2.); assert_ne!(bytes(&recovered), bytes(&second)); assert_eq!(render_saved(), saved_scene);
    p = lower_frozen(17, 5); wi::bind(&o.images, &p); let resized = get(&p, "exact", 2.);
    assert_eq!((si::width(&resized), si::height(&resized)), (17, 5)); assert!(!si::ptr_eq(&resized, &recovered)); assert_eq!(bytes(&first), saved_bytes); assert_eq!(render_saved(), saved_scene);
    p = lower_frozen(17, 5); wi::bind(&o.images, &p); let replanned = get(&p, "exact", 2.);
    assert!(!si::ptr_eq(&replanned, &resized)); assert_eq!(bytes(&replanned), bytes(&resized)); assert_eq!(render_saved(), saved_scene); assert_eq!(e::state_stamp(&state), stamp);
    o.close(); assert_eq!(bytes(&first)[2], 128); assert!(o.balanced()); assert_eq!(handles(), before);
}
fn frozen_capacity(w: i64, h: i64) {
    assert!(common::gpu(), "native Vulkan device required");
    let o = Owner::new(1, true);
    let p = lower(&format!("(workspace versions (graph img :context image (exact (image/map (fn [uv] [(* t 0.0078125) uv.y 0.5 1]) :width {w} :height {h}))))"));
    wi::bind(&o.images, &p); let state = e::create_state(); let before = handles();
    let exact = value(node_of(&p, "exact").id); let child = value(node_of(&p, "image/map").id);
    let resolve = |frame| o.payload(&p, &state, &at(frame), &exact);
    let held: Vec<_> = (0..63).map(|frame| ok(resolve(frame))).collect();
    let source = ok(o.image(&p, &state, &at(62), &child)); let native = image::resource(&source);
    assert_eq!(o.created(), 64); assert_eq!(Native::readbacks(&native), 63);
    for _ in 0..2 { assert_eq!(resolve(63).err().expect("capacity refusal").code, "E_IMAGE"); }
    assert_eq!(o.created(), 64); assert_eq!(Native::readbacks(&native), 63);
    for (frame, p) in held.iter().enumerate() { assert_eq!(bytes(p)[0], (frame as f64 * 0.0078125 * 255.).round() as u8); }
    o.close(); assert!(o.balanced()); assert_eq!(handles(), before); assert_eq!(held.len(), 63); assert_eq!(si::width(&held[0]), w);
}
fn frozen_capacity_small() { frozen_capacity(7, 3); }
fn frozen_capacity_large() { frozen_capacity(65, 17); }

fn capture_geometry(count: i64) -> rdk_core::geometry::T {
    let mut b = rdk_core::packed::Float3::Builder::create(count).unwrap();
    rdk_core::packed::Float3::Builder::set(&mut b, 0, 0.03125, 0., 0.).unwrap();
    rdk_core::geometry::create(rdk_core::packed::Float3::Builder::freeze(&mut b).unwrap(), rdk_core::topology::empty(count).unwrap(), None, None, None).unwrap()
}
fn lower_budget(geometry: rdk_core::geometry::T) -> l::T {
    let factory = edit_graph::factory(edit_graph::factory_args("capture_budget", "Capture budget", &["Test"]), 0, Arc::new(move |args| {
        assert!(args.is_empty());
        let geometry = geometry.clone();
        Ok(node::build::make(node::build::Args { cook_mode: node::CookMode::Generator, ..node::build::args("capture_budget") }, None, Arc::new(move |_, _, _| {
            Ok(node::Cooked { payload: payload::T::Geometry(geometry.clone()), instances: None })
        })))
    }));
    let mut factories = vec![factory]; factories.extend(sop::nodes::factories());
    let forms = ok(flow_syntax::syntax::parse(r#"(workspace budget (graph mesh :context sop
      (let* [geo (sop/capture_budget) p (array/nth (sop/attr geo :P) 0)
        img (image/map (fn [uv] [(+ uv.x (+ p.x (* t 0.015625))) uv.y 0.5 1]) :width 65 :height 17)] geo)))"#));
    ok(l::workspace(Some(factories), Vec::new(), Default::default(), &forms))
}
fn capture_budget() {
    assert!(common::gpu(), "native Vulkan device required");
    let large = capture_geometry(64 * 1024 * 1024 / 24 + 1);
    assert!(rdk_core::geometry::payload_bytes(&large) > 64 * 1024 * 1024);
    let o = Owner::new(1, true); let before = handles();
    let run = |p: &l::T, time| {
        wi::bind(&o.images, p);
        let i = ok(o.image(p, &e::create_state(), &input::at_time(time), &value(node_of(p, "image/map").id)));
        assert_eq!(image::pixels(&i).unwrap()[0], (((0.5 / 65.) + 0.03125 + time * 0.015625) * 255.).round() as u8);
    };
    let p = lower_budget(large); run(&p, 0.); run(&p, 1.);
    assert_eq!(wi::capture_stats(&o.images), (1, 0, 0, 2, 2));
    let p = lower_budget(capture_geometry(1)); run(&p, 0.); run(&p, 1.);
    let (metadata, data, bytes, cooks, flattens) = wi::capture_stats(&o.images);
    assert_eq!((metadata, data, cooks, flattens), (1, 1, 3, 3)); assert!(bytes <= 64 * 1024 * 1024);
    assert_eq!(wg::image_stats(&o.gpu).0.status_reads, 4);
    o.close(); assert_eq!(handles(), before); assert_eq!(wi::capture_stats(&o.images), (0, 0, 0, 3, 3));
}
fn capture_capacity() {
    assert!(common::gpu(), "native Vulkan device required");
    let bindings = (0..65).map(|i| format!("g{i} (sop/box :consolidate_points true :center [0.015625 0 0]) p{i} (array/sum (sop/attr g{i} :P))")).collect::<Vec<_>>().join(" ");
    let sum = (0..65).rev().map(|i| format!("p{i}")).collect::<Vec<_>>().join(" ");
    let p = lower(&format!(r#"(workspace capacity (graph mesh :context sop
    (let* [{bindings} total (reduce + [0 0 0] (list {sum}))
           img (image/map (fn [uv] [(+ (+ uv.x (* t 0.015625)) total.x) uv.y 0.5 1]) :width 65 :height 17)
           last (image/map (fn [uv] [(+ (+ uv.x (* t 0.015625)) p64.x) uv.y 0.5 1]) :width 65 :height 17)] g0)))"#));
    let o = Owner::new(1, true); wi::bind(&o.images, &p); let state = e::create_state();
    let v = |name: &str| value(p.plan.nodes.iter().find(|n| n.kind == "image/map" && n.site == ["mesh", name]).unwrap().id);
    ok(o.image(&p, &state, &input::at_time(0.), &v("img")));
    let (metadata, data, charged, cooks, flattens) = wi::capture_stats(&o.images);
    assert_eq!((metadata, data, cooks, flattens), (64, 64, 65, 65)); assert!(charged <= 64 * 1024 * 1024);
    ok(o.image(&p, &state, &input::at_time(1.), &v("last")));
    let (_, _, _, later_cooks, later_flattens) = wi::capture_stats(&o.images);
    assert_eq!((later_cooks, later_flattens), (cooks, flattens)); assert_eq!(wg::image_stats(&o.gpu).0.status_reads, 2);
    assert_eq!(bytes(&ok(o.payload(&p, &state, &input::at_time(1.), &v("last"))))[0], 38);
    o.close(); assert_eq!(wi::capture_stats(&o.images), (0, 0, 0, 65, 65));
}
fn lower_cycle(center: f64) -> l::T { lower(&format!(r#"(workspace cycle
    (graph mesh :context sop
      (let* [geo (sop/box :consolidate_points true :center [{center} 0 0])
             p (reduce + [0 0 0] (sop/attr geo :P))
             img (image/map (fn [uv] [(+ uv.x p.x) uv.y 0.5 1]) :width 7 :height 3)] geo)))"#)) }
fn capture_cycles() {
    assert!(common::gpu(), "native Vulkan device required");
    let p = lower_cycle(0.015625); let graph = &p.graphs[0]; let image_node = node_of(&p, "image/map");
    let id = p.compiled[&image_node.id]; let template = edit_graph::find(&graph.network.geometry, id).unwrap().clone();
    let callback = graph.network.frame_nodes[&id].clone();
    let mut frames = graph.network.frame_nodes.clone();
    frames.insert(graph.root.unwrap(), Arc::new(move |network, state, live, node| {
        callback(network, state, live, &template).map(|_| node.clone())
    }));
    let cyclic = network::with_frame_nodes(frames, &graph.network);
    let state = e::create_state(); let stamp = e::state_stamp(&state); let before = handles();
    let run = |domains, fail| {
        let o = Owner::new(domains, false); wi::bind(&o.images, &p);
        let resolve = |network: &network::Network| wi::payload(&o.images)(None, Some(&l::ImageContext { compiled: p.compiled.clone(), network: network.clone() }), &p.plan, &state, &input::at_time(0.), &value(image_node.id));
        if fail {
            for _ in 0..2 {
                assert_eq!(resolve(&cyclic).err().expect("cycle must refuse").code, "E_IMAGE_CYCLE");
                assert_eq!(e::state_stamp(&state), stamp); assert_eq!(o.created(), 0);
            }
        }
        let payload = ok(resolve(&graph.network)); assert_eq!(e::state_stamp(&state), stamp); assert_eq!(o.created(), 0);
        let saved = bytes(&payload);
        let replanned = lower_cycle(0.03125); wi::bind(&o.images, &replanned);
        let rebuilt = ok(o.payload(&replanned, &state, &input::at_time(0.), &value(node_of(&replanned, "image/map").id)));
        let changed = bytes(&rebuilt);
        assert_ne!(changed, saved); assert_eq!(bytes(&payload), saved); assert_eq!(e::state_stamp(&state), stamp); assert_eq!(o.created(), 0);
        o.close(); (saved, changed)
    };
    let expected = run(1, false);
    assert_eq!(run(1, true), expected); assert_eq!(run(8, false), expected); assert_eq!(run(8, true), expected); assert_eq!(handles(), before);
}

fn publish(gpu: &wg::t, key: &str, v: &flow_ir::flow_ir::Gpu::Value) -> Result<flow_gpu::image_sink::Output, Diagnostic> {
    let mut output = None;
    wg::image(gpu, key, 65, 17, &mut |o| { output = Some(o.clone()); Ok(()) }, v)?;
    Ok(output.unwrap())
}
fn gpu_sinks() {
    assert!(common::gpu(), "native Vulkan device required");
    mem::configure(&|key| std::env::var(key).ok()).unwrap();
    let before = handles(); let owner = wg::create(None); wg::qualification(&owner);
    let p = lower("(workspace test (graph image :context image (image/map (fn [uv] [uv.x uv.y 0.5 1]))))");
    let n = &p.plan.nodes[0];
    let function = match &n.args.iter().find(|(k, _)| k == "function").unwrap().1 { Value::Fn(f) => *f, _ => panic!("function") };
    let kernel = ok(flow_sop::image_kernel::prepare(Some((n.inst, n.site.clone(), n.iter.clone())), p.image_sites[0].clone(), Some(p.approx.clone()), 0, 65, 17, function, &[], &[]));
    let program = flow_sop::image_kernel::program(&kernel);
    let mut shown = None;
    wg::with_backend(&owner, false, &mut |gpu| {
        shown = match ok(flow_ir::flow_ir::Executor::try_display(None, None, false, gpu, &program, &input::at_time(0.))) {
            Some(flow_ir::flow_ir::Executor::Displayed::Gpu(v)) => Some(v), _ => panic!("resident GPU output required")
        };
    });
    let v = shown.unwrap(); let held = ok(publish(&owner, "held", &v)); let native_handles = handles();
    for i in 0..80 {
        assert!(wg::image(&owner, &format!("bad-shape:{i}"), 65, 18, &mut |_| Ok(()), &v).is_err());
        assert!(wg::image(&owner, &format!("bad-publication:{i}"), 65, 17, &mut |_| Err(flow_syntax::value::fail("E_TEST", "publication refused")), &v).is_err());
        assert!(flow_gpu::image_sink::texture(&held).is_some()); assert_eq!(handles(), native_handles);
    }
    let (host, created, closed, buffers, textures) = wg::image_stats(&owner);
    assert_eq!((created, closed, buffers, textures, host.status_reads, host.readback_bytes), (161, 160, 161, 81, 1, 0));
    let panic = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let _ = wg::image(&owner, "raised", 65, 17, &mut |_| panic!("publication raised"), &v);
    })).expect_err("publication panic must propagate");
    assert!(panic.downcast_ref::<&str>().is_some_and(|s| *s == "publication raised") || panic.downcast_ref::<String>().is_some_and(|s| s == "publication raised"));
    assert_eq!(handles(), native_handles);
    let (_, created, closed, buffers, textures) = wg::image_stats(&owner);
    assert_eq!((created, closed, buffers, textures), (162, 161, 162, 82));
    let outputs: Vec<_> = (0..63).map(|i| ok(publish(&owner, &format!("accepted:{i}"), &v))).collect();
    assert!(publish(&owner, "over-capacity", &v).is_err());
    assert!(outputs.iter().all(|o| flow_gpu::image_sink::texture(o).is_some())); assert!(flow_gpu::image_sink::texture(&held).is_some());
    assert!(wg::image(&owner, "held", 65, 17, &mut |_| Err(flow_syntax::value::fail("E_TEST", "update refused")), &v).is_err());
    assert!(flow_gpu::image_sink::texture(&held).is_none()); assert!(flow_gpu::image_sink::texture(&ok(publish(&owner, "held", &v))).is_some());
    let (before_stats, created, closed, buffers, textures) = wg::image_stats(&owner);
    assert_eq!((created, closed, buffers, textures), (225, 161, 225, 145));
    wg::close(&owner); wg::close(&owner);
    let (after, created, closed, after_buffers, after_textures) = wg::image_stats(&owner);
    assert_eq!((created, closed, after_buffers, after_textures), (225, 225, buffers, textures));
    assert_eq!(after.status_reads, before_stats.status_reads); assert_eq!(after.buffer_creations, before_stats.buffer_creations);
    assert_eq!(after.input_uploads, before_stats.input_uploads); assert_eq!(after.input_uploaded_bytes, before_stats.input_uploaded_bytes);
    assert_eq!(after.readback_bytes, before_stats.readback_bytes); assert_eq!(after.runners_created, after.runners_released);
    assert_eq!(handles(), before);
}

const MARKS: &str = r##"(workspace gpu_marks
  (graph picture :context draw
    (let* [indices (array/range 2048)
           positions (map (fn [i] (let* [column (mod i 64)
                                         row (/ (- i column) 64)]
                                    [(+ (* column 2) (* t 8)) (* row 2) 0])) indices)
           radii (map (fn [i] (+ 1 (* (mod (+ i (* t 8)) 3) 0.5))) indices)]
      (draw/merge (draw/background "#102030")
        (draw/translate [5 7 0] (draw/circles positions :radius radii :fill "#76baca" :stroke "#ffffff"))
        (draw/translate [3 40 0]
          (draw/rects positions (map (fn [i] [2 1.5 0]) indices)
            :fill (map (fn [i] [(/ (mod (+ i (* t 8)) 8) 8) 0.5 0.25]) indices) :stroke "#ff8800"))
        (draw/lines positions (map (fn [i] [(+ (* (mod i 64) 2) 3) (+ (/ (- i (mod i 64)) 32) 5) 0]) indices)
          :color (map (fn [i] [0.5 (/ (mod (+ i (* t 8)) 4) 4) 1]) indices) :width 1.5)))))"##;
fn mark_tokens(scene: &[rays::scene::node]) -> usize {
    use rays::scene::node as N;
    scene.iter().map(|n| match n {
        N::Shapes(s) => usize::from(s.marks().is_some()),
        N::Group(children) | N::Translate(_, _, children) | N::Rotate(_, children)
        | N::Scale(_, _, children) | N::Affine(_, children) | N::Clip(_, _, _, _, children)
        | N::Blend(_, children) => mark_tokens(children),
        _ => 0,
    }).sum()
}
fn gpu_marks() {
    assert!(common::gpu(), "native Vulkan device required");
    let gpu = wg::create(None); wg::qualification(&gpu); let config = wg::config(&gpu, false);
    let p = ok(l::workspace(None, Vec::new(), l::Options { gpu: config.clone(), ..Default::default() }, &ok(flow_syntax::syntax::parse(MARKS))));
    let v = result(&p, "picture");
    let options = drawing::PrepareOptions { gpu: Some(config), approx: Some(p.approx.clone()), states: p.states.clone(), ..Default::default() };
    let prepared = ok(drawing::prepare(&options, &p.plan, &v));
    let c = canvas::create(160, 96).unwrap();
    let capture = |scene: &[rays::scene::node]| { canvas::render(None, &c, scene).unwrap(); canvas::pixels(&c).unwrap() };
    for time in [0., 0.5, 0.25] {
        let live = input::Frame { size: (160, 96), ..input::at_time(time) };
        let cpu = ok(drawing::render_prepared(&prepared, &live, (160, 96), &Default::default()));
        assert_eq!(mark_tokens(&cpu), 0); let expected = capture(&cpu);
        let mut scene = None;
        wg::with_backend(&gpu, false, &mut |_| {
            scene = Some(ok(drawing::render_prepared(&prepared, &live, (160, 96), &drawing::RenderOptions { gpu: Some(wg::marks(&gpu)), ..Default::default() })));
        });
        let scene = scene.unwrap(); assert_eq!(mark_tokens(&scene), 3);
        assert_eq!(capture(&scene), expected, "GPU marks differ at t={time}");
        assert!(expected.iter().filter(|c| **c != expected[159]).count() > 500);
    }
    canvas::destroy(&c).unwrap();
    let again = ok(drawing::prepare(&options, &p.plan, &v));
    let live = input::Frame { size: (160, 96), ..input::at_time(0.5) };
    let mut both = None;
    wg::with_backend(&gpu, false, &mut |_| {
        let opts = drawing::RenderOptions { gpu: Some(wg::marks(&gpu)), ..Default::default() };
        both = Some((ok(drawing::render_prepared(&prepared, &live, (160, 96), &opts)), ok(drawing::render_prepared(&again, &live, (160, 96), &opts))));
    });
    let (a, b) = both.unwrap(); let c = canvas::create(160, 96).unwrap();
    canvas::render(None, &c, &a).unwrap(); let expected = canvas::pixels(&c).unwrap();
    canvas::render(None, &c, &b).unwrap(); assert_eq!(canvas::pixels(&c).unwrap(), expected); canvas::destroy(&c).unwrap();
    assert_eq!(wg::image_stats(&gpu).0.runners_created, 6, "fused positions/radii, fill, colour, twice");
    wg::close(&gpu);
}

pub fn cases() -> Vec<(&'static str, fn())> {
    vec![
        ("workspace_packed_capture_live", packed_live),
        ("workspace_packed_capture_state", packed_state),
        ("workspace_state_failure_one", state_failure_one),
        ("workspace_state_failure_eight", state_failure_eight),
        ("workspace_frozen_exact", frozen_exact),
        ("workspace_frozen_capacity_small", frozen_capacity_small),
        ("workspace_frozen_capacity_large", frozen_capacity_large),
        ("workspace_capture_budget", capture_budget),
        ("workspace_capture_capacity", capture_capacity),
        ("workspace_capture_cycles", capture_cycles),
        ("workspace_gpu_sinks", gpu_sinks),
        ("workspace_gpu_marks", gpu_marks),
    ]
}
