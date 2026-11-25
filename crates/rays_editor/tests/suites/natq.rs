//! Native editor qualification (offscreen Vulkan/Metal device).
#[path = "../native_qualification/workspace.rs"]
mod workspace;
#[path = "../native_qualification/placement.rs"]
mod placement;
#[path = "../native_qualification/isogrid.rs"]
mod isogrid;
#[path = "../native_qualification/chains.rs"]
mod chains;
#[path = "../native_qualification/live_mesh.rs"]
mod live_mesh;
#[path = "../native_qualification/marks_scene.rs"]
mod marks_scene;
#[path = "../native_qualification/smoke.rs"]
mod smoke;
#[path = "../native_qualification/alloc_budget.rs"]
mod alloc_budget;
#[path = "../native_qualification/click_budget.rs"]
mod click_budget;
#[path = "../native_qualification/graph_budget.rs"]
mod graph_budget;
#[path = "../native_qualification/typing_budget.rs"]
mod typing_budget;
#[path = "../native_qualification/inspector_l5.rs"]
mod inspector_l5;
#[path = "../native_qualification/list_l5.rs"]
mod list_l5;

pub fn scrub_child() -> bool {
    chains::scrub_child()
}

pub fn cases() -> Vec<(&'static str, fn())> {
    let mut cases: Vec<(&str, fn())> = Vec::new();
    cases.extend(workspace::cases());
    cases.extend(placement::cases());
    cases.extend(isogrid::cases());
    cases.extend(chains::cases());
    cases.extend(live_mesh::cases());
    cases.extend(marks_scene::cases());
    cases.extend(smoke::cases());
    cases.extend(alloc_budget::cases());
    cases.extend(click_budget::cases());
    cases.extend(graph_budget::cases());
    cases.push(("native_ui_replays_updated_pixels", native_ui_replays_updated_pixels));
    cases.extend(typing_budget::cases());
    cases.extend(inspector_l5::cases());
    cases.extend(list_l5::cases());
    cases
}

fn native_ui_replays_updated_pixels() {
    use ogpu_core::backend::{self, ColorAttachment, RenderTarget};
    use ogpu_core::render_pass::{Load, Store};
    use rays::{canvas, scene};
    use std::{cell::Cell, rc::Rc};

    assert!(crate::common::gpu(), "native GPU required");
    let color = Rc::new(Cell::new((1., 0., 0., 1.)));
    let last_frame = Rc::new(Cell::new(0));
    let paint: scene::UiPaint = Rc::new({
        let color = color.clone();
        let last_frame = last_frame.clone();
        move |pass, _| {
            assert!(pass.frame > last_frame.replace(pass.frame), "replay reused the frame serial");
            pass.split(&mut |commands, target| {
                let encoder = backend::render_encoder(commands, &RenderTarget {
                    colors: vec![ColorAttachment {
                        texture: target.clone(), resolve: None,
                        load: Load::Clear, store: Store::Store, clear: color.get(),
                    }],
                    depth: None,
                })?;
                backend::end_render(&encoder)
            })?;
            Ok(())
        }
    });
    let context = scene::layer::create_cache_context();
    let frame = |stamp| [scene::layer::ui(paint.clone(), 0, stamp)];
    scene::layer::with_cache_context(&context, &mut || {
        let first = scene::layer::stage_native_render(8, 8, &frame(1)).unwrap();
        let second = scene::layer::stage_native_render(8, 8, &frame(2)).unwrap();
        assert!(Rc::ptr_eq(&first, &second), "UI paint changes must reuse native staging");
    });
    let canvas = canvas::create(8, 8).unwrap();
    canvas::render(None, &canvas, &frame(1)).unwrap();
    assert_eq!(canvas::pixels(&canvas).unwrap(), [rays_math::color::red(); 64], "the first UI frame paints red");
    color.set((0., 1., 0., 1.));
    canvas::render(None, &canvas, &frame(2)).unwrap();
    assert_eq!(canvas::pixels(&canvas).unwrap(), [rays_math::color::rgb(0, 255, 0); 64], "the replay paints the new green UI frame");
    canvas::destroy(&canvas).unwrap();
}
