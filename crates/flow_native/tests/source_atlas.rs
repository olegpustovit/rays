//! Rendered fixture views of the atlas sketches plus the real native residency and warm allocation contract.
//! A still camera reads nothing back; a moving one reads back only the stipple slots' owners.
mod common;
#[path = "../src/test_source_atlas.rs"]
mod atlas;
use flow_eval as e;
use flow_native::{drawing, workspace_gpu as wg};
use rays::{canvas, image};

fn frame_views() {
    if !common::gpu() { return; }
    let netwerk = include_str!("../../../sketches/source_netwerk.rays");
    let views = [("overview", None, 1.0), ("zoom", None, 8.0), ("netwerk_overview", Some(netwerk), 1.0), ("netwerk_zoom", Some(netwerk), 8.0),
                 ("strata", Some(include_str!("../../../sketches/source_strata.rays")), 1.0)];
    for (name, sketch, zoom) in views {
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
        assert_eq!(after.payload_readback_bytes, before.payload_readback_bytes, "{name}: frame payload readback");
        println!("{name}: warm payload readback {}, GPU buffers {}", after.payload_readback_bytes - before.payload_readback_bytes, after.buffer_creations - before.buffer_creations);
        assert_eq!(after.buffer_creations, before.buffer_creations, "{name}: warm GPU buffer allocation");
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

fn main() {
    let mut cases: Vec<(&str, fn())> = vec![("source_atlas_fixture_views", frame_views), ("local_function_reading_state_stays_a_kernel", local_function_kernel)];
    if std::env::var("RAYS_SLOW").as_deref() == Ok("1") { cases.push(("source_atlas_full_table_budget", full_table_budget)); }
    common::window::run(&cases);
}

// Explicit qualification uses the generated real table and times completed native frames,
// including changing hover/zoom, rather than the CPU reference render command.
fn full_table_budget() {
    if !common::gpu() { return; }
    for sketch in ["source_atlas", "source_netwerk"] { full_table_frames(sketch); }
}

fn full_table_frames(sketch: &str) {
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
        println!("{sketch} 1440x1000, moving={moving}: {milliseconds:.3} ms/frame, payload readback {} B/frame", (after.payload_readback_bytes - before.payload_readback_bytes) / 32);
        assert!(milliseconds < 16.7, "{sketch}: full table exceeded a 60 Hz frame budget: {milliseconds:.3} ms");
        assert_eq!(after.buffer_creations, before.buffer_creations, "moving={moving}: warm GPU buffer allocation");
    }
    canvas::destroy(&c).unwrap();
    wg::close(&ws);
}
