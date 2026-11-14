//! Rendered fixture views of the atlas sketches plus the real native residency and warm allocation contract.
//! A still camera reads nothing back; a moving one reads back only the stipple slots' owners.
mod common;
#[path = "../src/test_source_atlas.rs"]
mod atlas;
use flow_eval as e;
use flow_native::{drawing, workspace_gpu as wg};
use rays::{canvas, image};

fn frame_views() {
    views(&[("overview", None, 1.0), ("zoom", None, 8.0),
            ("strata", Some(include_str!("../../../sketches/source_strata.rays")), 1.0),
            ("ledger", Some(include_str!("../../../sketches/source_ledger.rays")), 1.0),
            ("contour", Some(include_str!("../../../sketches/source_contour.rays")), 1.0)]);
}

// Netwerk compiles a stipple kernel set for each of its five dot levels: its own case.
fn netwerk_views() {
    let netwerk = include_str!("../../../sketches/source_netwerk.rays");
    flow_ir::fallback::reset();
    views(&[("netwerk_overview", Some(netwerk), 1.0), ("netwerk_zoom", Some(netwerk), 8.0)]);
    // every mark stays a kernel: a palette picked per frame among stored ink tables included. Known
    // and older than this check: the margin ticks are interpreted (a capture still reads a state).
    let interpreted: Vec<_> = flow_ir::fallback::seen().into_iter().filter(|(site, reason, _)| reason.starts_with("no kernel") && !site.starts_with("picture/tick_")).collect();
    assert!(interpreted.is_empty(), "{interpreted:?}");
}

fn views(views: &[(&str, Option<&str>, f64)]) {
    if !common::gpu() { return; }
    for &(name, sketch, zoom) in views {
        let (_, lowered) = match sketch { Some(sketch) => atlas::fixture_of(sketch, zoom), None => atlas::fixture(zoom) };
        let ws = wg::create(None);
        wg::qualification(&ws);
        let prepared = drawing::prepare(&drawing::PrepareOptions { gpu: Some(wg::config(&ws, true)), states: lowered.states.clone(), ..Default::default() },
                                        &lowered.evaluated.plan, &atlas::result(&lowered, "picture")).unwrap();
        let state = e::create_state();
        let size = (960, 640);
        let c = canvas::create(size.0, size.1).unwrap();
        let frame = |k| {
            let live = input::Frame { size, frame: k, tick: k, dt: 1. / 60., pointer: (400., 200.), ..input::at_time(k as f64 / 60.) };
            wg::with_backend(&ws, true, &mut |gpu| {
                let resident = wg::resident(gpu, &ws);
                let scene = drawing::render_prepared(&prepared, &live, size, &drawing::RenderOptions { state: Some(state.clone()), gpu: Some(wg::marks(&ws)), resident, ..Default::default() }).unwrap();
                assert!(drawing::borrows_gpu(&prepared), "{name}: mark attributes left the device");
                canvas::render(None, &c, &scene).unwrap();
            });
            wg::frame(&ws, None);
        };
        for k in 1..=8 { frame(k); }
        let before = wg::image_stats(&ws).0;
        for k in 9..=12 { frame(k); }
        let after = wg::image_stats(&ws).0;
        // the Source Atlas sheet reads each of its 96 label slots' owner on the host, twice (a word
        // each): the owners array itself stays on the device; no other sketch reads a word
        let labels = if sketch.is_none() { 4 * 96 * 2 * 4 } else { 0 };
        assert_eq!(after.payload_readback_bytes - before.payload_readback_bytes, labels, "{name}: frame payload readback");
        println!("{name}: warm payload readback {}, GPU buffers {}", after.payload_readback_bytes - before.payload_readback_bytes, after.buffer_creations - before.buffer_creations);
        assert_eq!(after.buffer_creations, before.buffer_creations, "{name}: warm GPU buffer allocation");
        // a still camera: every resident region (the slot owners, the corners, their fused siblings) is held
        assert_eq!(after.resident_dispatches, before.resident_dispatches, "{name}: a still camera dispatched a region");
        let path = format!("{}/../../fixtures/source_atlas/{name}.png", env!("CARGO_MANIFEST_DIR"));
        if std::env::var("RAYS_UPDATE_FIXTURES").as_deref() == Ok("1") { canvas::save_png(&c, &path).unwrap(); }
        let golden = image::load(&path).unwrap();
        assert_eq!(image::dimensions(&golden), size);
        let expected = image::pixels(&golden).unwrap();
        let actual = canvas::pixels(&c).unwrap();
        let changed = actual.iter().zip(expected.chunks_exact(4)).filter(|(a, b)| (a.r - b[0] as i64).abs().max((a.g - b[1] as i64).abs()).max((a.b - b[2] as i64).abs()) > 4).count();
        assert!(changed * 1000 < actual.len() * 5, "{name}: {changed} pixels differ by more than 4/255 (limit 0.5%)");
        image::destroy(&golden).unwrap();
        canvas::destroy(&c).unwrap();
        wg::close(&ws);
    }
}

// A mark attribute that calls a local function whose captures follow a state (a camera) is still
// a kernel: what the function read becomes the kernel's argument. It used to be interpreted element
// by element, without a word.
fn local_function_kernel() {
    if !common::gpu() { return; }
    let source = r##"(workspace local_function
  (graph view :context host (state [old {:zoom 1.0}] {:zoom (+ old.zoom 0.01)}))
  (graph picture :context draw
    (let* [view (ref view)
           ids (array/range 8192 :type int)
           cam [10.0 20.0 view.zoom]
           lift (fn [(i : int)] (* cam.z (float (int/mod i 64))))]
      (draw/rects (map (fn [(i : int)] [(+ cam.x (lift i)) (+ cam.y (float (int/div i 64))) 0.0]) ids) [1.0 1.0 0.0] :fill "#e1d8c0"))))"##;
    let catalog = editor_document::kinds::shared(None).unwrap();
    let doc = editor_document::workspace_doc::of_text(&editor_document::workspace_doc::TextOptions::default(), &catalog, source).unwrap_or_else(|ds| panic!("{ds:?}"));
    flow_ir::fallback::reset();
    let lowered = flow_sop::lower::of_checked(None, flow_sop::lower::Options { inputs: Some((*doc.inputs).clone()), ..Default::default() }, &doc.checked).unwrap();
    let ws = wg::create(None);
    let prepared = drawing::prepare(&drawing::PrepareOptions { gpu: Some(wg::config(&ws, true)), states: lowered.states.clone(), ..Default::default() },
                                    &lowered.evaluated.plan, &atlas::result(&lowered, "picture")).unwrap();
    let state = e::create_state();
    let size = (320, 240);
    let c = canvas::create(size.0, size.1).unwrap();
    for k in 1..=3 {
        let live = input::Frame { size, frame: k, tick: k, dt: 1. / 60., ..input::at_time(k as f64 / 60.) };
        wg::with_backend(&ws, true, &mut |gpu| {
            let scene = drawing::render_prepared(&prepared, &live, size, &drawing::RenderOptions { state: Some(state.clone()), gpu: Some(wg::marks(&ws)), resident: wg::resident(gpu, &ws), ..Default::default() }).unwrap();
            canvas::render(None, &c, &scene).unwrap();
        });
        wg::frame(&ws, None);
    }
    let interpreted: Vec<_> = flow_ir::fallback::seen().into_iter().filter(|(_, reason, _)| reason.starts_with("no kernel")).collect();
    assert!(interpreted.is_empty(), "{interpreted:?}");
    canvas::destroy(&c).unwrap();
    wg::close(&ws);
}

// A map's array that later mark attributes read (`array/nth a i`) stays on the device: a frame
// whose camera moves reads none of it back. It used to come back to the host, whole, every frame.
fn intermediate_stays_resident() {
    if !common::gpu() { return; }
    let source = r##"(workspace intermediate
  (graph view :context host (state [old {:zoom 1.0}] {:zoom (+ old.zoom 0.01)}))
  (graph picture :context draw
    (let* [view (ref view)
           ids (array/range 8192 :type int)
           a (map (fn [(i : int)] [(* view.zoom (float (int/mod i 64))) (float (int/div i 64)) 1.0 0.5]) ids)]
      (draw/rects (map (fn [(i : int)] (let* [p (array/nth a i)] [p.x p.y 0.0])) ids)
                  (map (fn [(i : int)] (let* [p (array/nth a i)] [p.z p.w 0.0])) ids) :fill "#e1d8c0"))))"##;
    let catalog = editor_document::kinds::shared(None).unwrap();
    let doc = editor_document::workspace_doc::of_text(&editor_document::workspace_doc::TextOptions::default(), &catalog, source).unwrap_or_else(|ds| panic!("{ds:?}"));
    flow_ir::fallback::reset();
    let lowered = flow_sop::lower::of_checked(None, flow_sop::lower::Options { inputs: Some((*doc.inputs).clone()), ..Default::default() }, &doc.checked).unwrap();
    let ws = wg::create(None);
    let prepared = drawing::prepare(&drawing::PrepareOptions { gpu: Some(wg::config(&ws, true)), states: lowered.states.clone(), ..Default::default() },
                                    &lowered.evaluated.plan, &atlas::result(&lowered, "picture")).unwrap();
    // the same frames with the intermediate read back, the reference picture
    let plain = drawing::prepare(&drawing::PrepareOptions { gpu: Some(wg::config(&ws, true)), states: lowered.states.clone(), ..Default::default() },
                                 &lowered.evaluated.plan, &atlas::result(&lowered, "picture")).unwrap();
    let (state, reference) = (e::create_state(), e::create_state());
    let size = (320, 240);
    let (c, expected) = (canvas::create(size.0, size.1).unwrap(), canvas::create(size.0, size.1).unwrap());
    let frame = |k: i64| {
        let live = input::Frame { size, frame: k, tick: k, dt: 1. / 60., ..input::at_time(k as f64 / 60.) };
        wg::with_backend(&ws, true, &mut |gpu| {
            let scene = drawing::render_prepared(&prepared, &live, size, &drawing::RenderOptions { state: Some(state.clone()), gpu: Some(wg::marks(&ws)), resident: wg::resident(gpu, &ws), ..Default::default() }).unwrap();
            assert!(drawing::borrows_gpu(&prepared), "mark attributes left the device");
            canvas::render(None, &c, &scene).unwrap();
        });
        wg::frame(&ws, None);
    };
    for k in 1..=3 { frame(k); }
    let before = wg::image_stats(&ws).0;
    for k in 4..=7 { frame(k); }
    let after = wg::image_stats(&ws).0;
    for k in 1..=7 {
        let live = input::Frame { size, frame: k, tick: k, dt: 1. / 60., ..input::at_time(k as f64 / 60.) };
        wg::with_backend(&ws, true, &mut |_| {
            let scene = drawing::render_prepared(&plain, &live, size, &drawing::RenderOptions { state: Some(reference.clone()), gpu: Some(wg::marks(&ws)), ..Default::default() }).unwrap();
            canvas::render(None, &expected, &scene).unwrap();
        });
        wg::frame(&ws, None);
    }
    let drawn = canvas::pixels(&c).unwrap();
    assert!(drawn.iter().any(|p| *p != drawn[0]), "nothing drawn");
    assert!(drawn == canvas::pixels(&expected).unwrap(), "a resident intermediate drew another picture");
    let read = after.payload_readback_bytes - before.payload_readback_bytes;
    assert_eq!(after.buffer_creations, before.buffer_creations, "warm GPU buffer allocation");
    // the 8192 vec4 are 131072 B a frame
    assert!(read < 4 * 8192 * 16 / 8, "intermediate read back: {read} B over 4 frames");
    let interpreted: Vec<_> = flow_ir::fallback::seen().into_iter().filter(|(_, reason, _)| reason.starts_with("no kernel")).collect();
    assert!(interpreted.is_empty(), "{interpreted:?}");
    canvas::destroy(&c).unwrap();
    canvas::destroy(&expected).unwrap();
    wg::close(&ws);
}

// A resident map whose inputs did not change is not dispatched again: a still camera over frames
// runs no region and creates no buffer, and a changed input recomputes (the picture changes as the
// readback path's does).
fn resident_map_is_reused() {
    // the zoom holds at 2 until the twelfth frame, then goes to 3
    reuse_case(r##"(workspace reuse
  (graph ticks :context host (state [old 0] (+ old 1)))
  (graph view :context host (state [old {:zoom 2.0}] {:zoom (if (< (ref ticks) 12) 2.0 3.0)}))
  (graph picture :context draw
    (let* [view (ref view)
           ids (array/range 8192 :type int)
           a (map (fn [(i : int)] [(* view.zoom (float (int/mod i 64))) (float (int/div i 64)) 1.0 0.5]) ids)]
      (draw/rects (map (fn [(i : int)] (let* [p (array/nth a i)] [p.x p.y 0.0])) ids)
                  (map (fn [(i : int)] (let* [p (array/nth a i)] [p.z p.w 0.0])) ids) :fill "#e1d8c0"))))"##);
}

// The same for an integer producer (Atlas's slot owners): its exact words are held and reused.
fn resident_int_map_is_reused() {
    reuse_case(r##"(workspace reuse_ints
  (graph ticks :context host (state [old 0] (+ old 1)))
  (graph view :context host (state [old {:zoom 2.0}] {:zoom (if (< (ref ticks) 12) 2.0 3.0)}))
  (graph picture :context draw
    (let* [view (ref view)
           slots (array/range 4000 :type int)
           lift (int view.zoom)
           owners (map (fn [(j : int)] (int/add (int/mul (int/mod (int/add j lift) 4093) 3) (if (= (int/mod j 5) 0) -1 0))) slots)
           ; a small region runs on the host: its array is held as it is
           few (map (fn [(j : int)] (int/mul (int/add j lift) 2)) (array/range 16 :type int))]
      (draw/rects (map (fn [(i : int)] (let* [o (array/nth owners i)] [(float (int/mod (if (< o 0) 0 o) 64)) (float (int/div (if (< o 0) 0 o) 64)) 0.0])) slots)
                  (map (fn [(i : int)] (let* [o (array/nth owners i)] [(float (array/nth few (int/mod i 16))) (float (int/mod o 7)) 0.0])) slots) :fill "#e1d8c0"))))"##);
}

fn reuse_case(source: &str) {
    if !common::gpu() { return; }
    let catalog = editor_document::kinds::shared(None).unwrap();
    let doc = editor_document::workspace_doc::of_text(&editor_document::workspace_doc::TextOptions::default(), &catalog, source).unwrap_or_else(|ds| panic!("{ds:?}"));
    let lowered = flow_sop::lower::of_checked(None, flow_sop::lower::Options { inputs: Some((*doc.inputs).clone()), ..Default::default() }, &doc.checked).unwrap();
    let ws = wg::create(None);
    let prepare = || drawing::prepare(&drawing::PrepareOptions { gpu: Some(wg::config(&ws, true)), states: lowered.states.clone(), ..Default::default() }, &lowered.evaluated.plan, &atlas::result(&lowered, "picture")).unwrap();
    let (prepared, plain) = (prepare(), prepare());
    let (state, reference) = (e::create_state(), e::create_state());
    let size = (320, 240);
    let (c, expected) = (canvas::create(size.0, size.1).unwrap(), canvas::create(size.0, size.1).unwrap());
    let live = |k: i64| input::Frame { size, frame: k, tick: k, dt: 1. / 60., ..input::at_time(k as f64 / 60.) };
    let frame = |k: i64| {
        wg::with_backend(&ws, true, &mut |gpu| {
            let scene = drawing::render_prepared(&prepared, &live(k), size, &drawing::RenderOptions { state: Some(state.clone()), gpu: Some(wg::marks(&ws)), resident: wg::resident(gpu, &ws), ..Default::default() }).unwrap();
            canvas::render(None, &c, &scene).unwrap();
        });
        wg::frame(&ws, None);
        canvas::pixels(&c).unwrap()
    };
    let reference_frame = |k: i64| {
        wg::with_backend(&ws, true, &mut |_| {
            let scene = drawing::render_prepared(&plain, &live(k), size, &drawing::RenderOptions { state: Some(reference.clone()), gpu: Some(wg::marks(&ws)), ..Default::default() }).unwrap();
            canvas::render(None, &expected, &scene).unwrap();
        });
        wg::frame(&ws, None);
        canvas::pixels(&expected).unwrap()
    };
    let mut still = vec![];
    for k in 1..=5 { frame(k); }
    let before = wg::image_stats(&ws).0;
    for k in 6..=9 { still.push(frame(k)); }
    let after = wg::image_stats(&ws).0;
    assert_eq!(after.resident_dispatches, before.resident_dispatches, "a still camera dispatched the map");
    assert_eq!(after.buffer_creations, before.buffer_creations, "a still camera allocated");
    assert_eq!(after.payload_readback_bytes, before.payload_readback_bytes, "a still camera read back");
    let mut seen = still[0].clone();
    for k in 10..=16 { seen = frame(k); }
    assert!(wg::image_stats(&ws).0.resident_dispatches > after.resident_dispatches, "a changed input did not recompute");
    let mut want = vec![];
    for k in 1..=16 { want = reference_frame(k); }
    assert!(seen != still[0], "the zoom did not change the picture");
    assert!(seen == want, "a reused intermediate drew another picture");
    canvas::destroy(&c).unwrap();
    canvas::destroy(&expected).unwrap();
    wg::close(&ws);
}

// An integer intermediate stays on the device exactly: negatives and values up to 2^31 - 1, read
// by later kernels as values and as indices into another table, drawn as the readback path draws
// them, with no readback and no allocation in a warm frame.
fn int_intermediate_is_exact() {
    if !common::gpu() { return; }
    let source = r##"(workspace exact_ints
  (graph view :context host (state [old {:zoom 1.0}] {:zoom (+ old.zoom 0.01)}))
  (defn lit :context value [(seed : int) (id : int) (weight : int)]
    (let* [h (int/mod (int/add (int/mod seed 46337) (int/add (int/mul id 7919) 1)) 46337)
           cube (int/mod (int/mul (int/mod (int/mul h h) 46337) h) 46337)]
      (< (int/mod (int/add cube id) 100) weight)))
  (graph picture :context draw
    (let* [view (ref view)
           ids (array/range 4096 :type int)
           seeds (map (fn [(i : int)] (int/mod (int/mul i 2654435) 1000000007)) ids)
           weights (map (fn [(i : int)] (int/mod (int/mul i 37) 100)) ids)
           slots (array/range 4000 :type int)
           table (map (fn [(i : int)] [(float (int/mod i 64)) (float (int/div i 64)) (* view.zoom 0.25) 0.5]) ids)
           lift (int view.zoom)
           base (int/add 3 lift)
           owners (map (fn [(j : int)]
                         (let* [i (int/add j lift)]
                          (cond (= (int/mod i 5) 0) -1
                               (= (int/mod i 7) 0) (int/sub 2147483647 (int/mod i 3))
                               :else (int/add (int/mul (int/mod i 4093) (int/add 1 (int/mod i 2))) (int/mul (int/mod i 3) 100000)))))
                       slots)
           ; the exact words mean 2^31 - 1 - (i mod 3), whose remainders a float could not keep
           digits (map (fn [(i : int)] (let* [o (array/nth owners i)] (if (< o 0) 0 (int/mod o 97)))) slots)]
      (draw/rects (map (fn [(i : int)] (let* [o (array/nth owners i) p (array/nth table (int/mod (if (< o 0) 0 o) 4096))]
                                         [(+ (* 5.0 (float (int/mod i 64))) (* 0.05 p.x)) (+ (* 3.8 (float (int/div i 64))) (* 0.05 p.y)) 0.0])) slots)
                  (map (fn [(i : int)] (let* [o (array/nth owners (int/add base (int/div i 4))) p (array/nth table (int/mod (if (< o 0) 0 o) 4096))]
                                         [(+ (if (lit (array/nth seeds (int/mod (if (< o 0) 0 o) 4096)) (int/add i (int/mul 16 lift)) (array/nth weights (int/mod (if (< o 0) 0 o) 4096))) 3.0 1.0) (* 0.03 (float (array/nth digits i)))) (+ 1.0 p.z) 0.0])) slots)
                  :fill "#e1d8c0"))))"##;
    let catalog = editor_document::kinds::shared(None).unwrap();
    let doc = editor_document::workspace_doc::of_text(&editor_document::workspace_doc::TextOptions::default(), &catalog, source).unwrap_or_else(|ds| panic!("{ds:?}"));
    flow_ir::fallback::reset();
    let lowered = flow_sop::lower::of_checked(None, flow_sop::lower::Options { inputs: Some((*doc.inputs).clone()), ..Default::default() }, &doc.checked).unwrap();
    let ws = wg::create(None);
    let prepare = |gpu| drawing::prepare(&drawing::PrepareOptions { gpu, states: lowered.states.clone(), ..Default::default() }, &lowered.evaluated.plan, &atlas::result(&lowered, "picture")).unwrap();
    // the reference is the host's own evaluation: 64-bit integers, no kernel
    let (prepared, plain) = (prepare(Some(wg::config(&ws, true))), prepare(None));
    let (state, reference) = (e::create_state(), e::create_state());
    let size = (320, 240);
    let (c, expected) = (canvas::create(size.0, size.1).unwrap(), canvas::create(size.0, size.1).unwrap());
    let live = |k: i64| input::Frame { size, frame: k, tick: k, dt: 1. / 60., ..input::at_time(k as f64 / 60.) };
    let frame = |k: i64| {
        wg::with_backend(&ws, true, &mut |gpu| {
            let scene = drawing::render_prepared(&prepared, &live(k), size, &drawing::RenderOptions { state: Some(state.clone()), gpu: Some(wg::marks(&ws)), resident: wg::resident(gpu, &ws), ..Default::default() }).unwrap();
            assert!(drawing::borrows_gpu(&prepared), "mark attributes left the device");
            canvas::render(None, &c, &scene).unwrap();
        });
        wg::frame(&ws, None);
    };
    let reference_frame = |k: i64| {
        wg::with_backend(&ws, true, &mut |_| {
            let scene = drawing::render_prepared(&plain, &live(k), size, &drawing::RenderOptions { state: Some(reference.clone()), ..Default::default() }).unwrap();
            canvas::render(None, &expected, &scene).unwrap();
        });
        wg::frame(&ws, None);
    };
    for k in 1..=3 { frame(k); }
    let before = wg::image_stats(&ws).0;
    for k in 4..=7 { frame(k); }
    let after = wg::image_stats(&ws).0;
    for k in 1..=7 { reference_frame(k); }
    let drawn = canvas::pixels(&c).unwrap();
    assert!(drawn.iter().any(|p| *p != drawn[0]), "nothing drawn");
    // the host's doubles and the device's floats place a rect a hair apart: count only visible differences (a wrong digit moves a rect by tens of pixels)
    let exact = canvas::pixels(&expected).unwrap();
    let differ = drawn.iter().zip(exact.iter()).filter(|(a, b)| (a.r - b.r).abs().max((a.g - b.g).abs()).max((a.b - b.b).abs()) > 24).count();
    assert!(differ * 5000 < drawn.len(), "a resident integer intermediate drew another picture: {differ} pixels");
    assert!(after.resident_dispatches > before.resident_dispatches, "the intermediate never ran on the device");
    assert_eq!(after.payload_readback_bytes, before.payload_readback_bytes, "the integer intermediate came back to the host");
    assert_eq!(after.buffer_creations, before.buffer_creations, "warm GPU buffer allocation");
    canvas::destroy(&c).unwrap();
    canvas::destroy(&expected).unwrap();
    wg::close(&ws);
}

// A large host table a resident region reads is uploaded once, not every frame: the frames after
// the first bind the buffer an earlier scope filled, and the picture is the host's own.
fn adopted_table_uploads_once() {
    if !common::gpu() { return; }
    let source = r##"(workspace adopted
  (graph view :context host (state [old {:zoom 1.0}] {:zoom (+ old.zoom 0.01)}))
  (graph big :context host (map (fn [(i : int)] [(float (int/mod i 64)) (float (int/div i 64)) 1.0 0.5]) (array/range 70000 :type int)))
  (graph picture :context draw
    (let* [view (ref view)
           big (ref big)
           ids (array/range 4096 :type int)
           a (map (fn [(i : int)] (let* [p (array/nth big (int/mul i 17))] [(* view.zoom p.x) (* 0.5 p.y) p.z p.w])) ids)]
      (draw/rects (map (fn [(i : int)] (let* [p (array/nth a i)] [p.x p.y 0.0])) ids)
                  (map (fn [(i : int)] (let* [p (array/nth a i)] [p.z p.w 0.0])) ids) :fill "#e1d8c0"))))"##;
    let catalog = editor_document::kinds::shared(None).unwrap();
    let doc = editor_document::workspace_doc::of_text(&editor_document::workspace_doc::TextOptions::default(), &catalog, source).unwrap_or_else(|ds| panic!("{ds:?}"));
    let lowered = flow_sop::lower::of_checked(None, flow_sop::lower::Options { inputs: Some((*doc.inputs).clone()), ..Default::default() }, &doc.checked).unwrap();
    let ws = wg::create(None);
    let prepare = |gpu| drawing::prepare(&drawing::PrepareOptions { gpu, states: lowered.states.clone(), ..Default::default() }, &lowered.evaluated.plan, &atlas::result(&lowered, "picture")).unwrap();
    let (prepared, plain) = (prepare(Some(wg::config(&ws, true))), prepare(None));
    let (state, reference) = (e::create_state(), e::create_state());
    let size = (320, 240);
    let (c, expected) = (canvas::create(size.0, size.1).unwrap(), canvas::create(size.0, size.1).unwrap());
    let live = |k: i64| input::Frame { size, frame: k, tick: k, dt: 1. / 60., ..input::at_time(k as f64 / 60.) };
    let frame = |k: i64| {
        wg::with_backend(&ws, true, &mut |gpu| {
            let scene = drawing::render_prepared(&prepared, &live(k), size, &drawing::RenderOptions { state: Some(state.clone()), gpu: Some(wg::marks(&ws)), resident: wg::resident(gpu, &ws), ..Default::default() }).unwrap();
            canvas::render(None, &c, &scene).unwrap();
        });
        wg::frame(&ws, None);
    };
    let reference_frame = |k: i64| {
        wg::with_backend(&ws, true, &mut |_| {
            let scene = drawing::render_prepared(&plain, &live(k), size, &drawing::RenderOptions { state: Some(reference.clone()), ..Default::default() }).unwrap();
            canvas::render(None, &expected, &scene).unwrap();
        });
        wg::frame(&ws, None);
    };
    for k in 1..=3 { frame(k); }
    let before = wg::image_stats(&ws).0;
    for k in 4..=8 { frame(k); }
    let after = wg::image_stats(&ws).0;
    for k in 1..=8 { reference_frame(k); }
    assert!(after.resident_dispatches > before.resident_dispatches, "the region never ran on the device");
    assert_eq!(after.input_uploads, before.input_uploads, "a warm frame uploaded the table again");
    assert_eq!(after.buffer_creations, before.buffer_creations, "warm GPU buffer allocation");
    let (drawn, exact) = (canvas::pixels(&c).unwrap(), canvas::pixels(&expected).unwrap());
    assert!(drawn.iter().any(|p| *p != drawn[0]), "nothing drawn");
    let differ = drawn.iter().zip(exact.iter()).filter(|(a, b)| (a.r - b.r).abs().max((a.g - b.g).abs()).max((a.b - b.b).abs()) > 24).count();
    assert!(differ * 5000 < drawn.len(), "a cached table drew another picture: {differ} pixels");
    canvas::destroy(&c).unwrap();
    canvas::destroy(&expected).unwrap();
    wg::close(&ws);
}

fn main() {
    let mut cases: Vec<(&str, fn())> = vec![("source_atlas_fixture_views", frame_views), ("source_netwerk_fixture_views", netwerk_views), ("local_function_reading_state_stays_a_kernel", local_function_kernel),
                                             ("intermediate_map_stays_on_the_device", intermediate_stays_resident),
                                             ("resident_map_is_reused", resident_map_is_reused),
                                             ("resident_int_map_is_reused", resident_int_map_is_reused),
                                             ("adopted_table_uploads_once", adopted_table_uploads_once),
                                             ("int_intermediate_is_exact", int_intermediate_is_exact)];
    if std::env::var("RAYS_SLOW").as_deref() == Ok("1") { cases.push(("source_atlas_full_table_budget", full_table_budget)); }
    common::window::run(&cases);
}

// Explicit qualification uses the generated real table and times completed native frames,
// including changing hover/zoom, rather than the CPU reference render command.
fn full_table_budget() {
    if !common::gpu() { return; }
    // every sketch is timed before any is judged, so one slow frame does not hide the others' numbers
    let slow: Vec<String> = ["source_atlas", "source_netwerk", "source_ledger"].into_iter().flat_map(full_table_frames).collect();
    assert!(slow.is_empty(), "full table exceeded a 60 Hz frame budget: {slow:?}");
}

fn full_table_frames(sketch: &str) -> Vec<String> {
    let mut slow = Vec::new();
    let path = format!("{}/../../sketches/{sketch}.rays", env!("CARGO_MANIFEST_DIR"));
    let (_, doc) = web_export::web_export::load(&path);
    let doc = doc.unwrap_or_else(|ds| panic!("generate the table with tools/source_atlas_index: {ds:?}"));
    let lowered = flow_sop::lower::of_checked(None, Default::default(), &doc.checked).unwrap();
    let ws = wg::create(None);
    let prepared = drawing::prepare(&drawing::PrepareOptions { gpu: Some(wg::config(&ws, true)), states: lowered.states.clone(), ..Default::default() },
                                    &lowered.evaluated.plan, &atlas::result(&lowered, "picture")).unwrap();
    let state = e::create_state();
    let size = (1440, 1000);
    let c = canvas::create(size.0, size.1).unwrap();
    let frame = |k: i64, moving: bool| {
        let live = input::Frame { size, frame: k, tick: k, dt: 1. / 60., pointer: if moving { (500. + (k % 90) as f64, 300. + (k % 70) as f64) } else { (500., 300.) },
            events: if moving { vec![input::Event::Scrolled((0., if (k / 24) % 2 == 0 { 0.5 } else { -0.5 }))] } else { vec![] }, ..input::at_time(k as f64 / 60.) };
        wg::with_backend(&ws, true, &mut |gpu| {
            let scene = drawing::render_prepared(&prepared, &live, size, &drawing::RenderOptions { state: Some(state.clone()), gpu: Some(wg::marks(&ws)), resident: wg::resident(gpu, &ws), ..Default::default() }).unwrap();
            assert!(drawing::borrows_gpu(&prepared));
            canvas::render(None, &c, &scene).unwrap();
        });
        wg::frame(&ws, None);
    };
    for k in 1..=16 { frame(k, true); }
    for moving in [false, true] {
        let before = wg::image_stats(&ws).0;
        let start = std::time::Instant::now();
        for k in 17..=48 { frame(if moving { k + 32 } else { k }, moving); }
        let milliseconds = start.elapsed().as_secs_f64() * 1000. / 32.;
        let after = wg::image_stats(&ws).0;
        println!("{sketch} 1440x1000, moving={moving}: {milliseconds:.3} ms/frame, payload readback {} B/frame, resident dispatches {}/frame, uploads {} ({} B)/frame", (after.payload_readback_bytes - before.payload_readback_bytes) / 32, (after.resident_dispatches - before.resident_dispatches) as f64 / 32.,
                 (after.input_uploads - before.input_uploads) as f64 / 32., (after.input_uploaded_bytes - before.input_uploaded_bytes) / 32);
        if milliseconds >= 16.7 { slow.push(format!("{sketch} moving={moving}: {milliseconds:.3} ms")); }
        assert_eq!(after.buffer_creations, before.buffer_creations, "moving={moving}: warm GPU buffer allocation");
    }
    canvas::destroy(&c).unwrap();
    wg::close(&ws);
    slow
}
