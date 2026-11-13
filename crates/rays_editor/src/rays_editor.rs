//! Public editor APIs retain typed create/measure options, sketch-owned inspector results and shared document/model snapshots.

use editor_document::lowering;

pub use editor_document::kinds::Window as window;

use editor_document::kinds;
use sop::edit_graph::Factory;

/// The catalog a workspace text is checked against (`workspace_doc::of_text`): the SOP factories
/// (default `sop::nodes::factories`) plus the scene, world and settings kinds generated from the
/// editor's schemas.
pub fn workspace_catalog(factories: Option<&[Factory]>) -> Result<std::sync::Arc<flow_check::catalog::Catalog>, rays_diagnostic::Diagnostic> {
    kinds::shared(factories)
}

pub fn workspace_window(doc: &editor_document::workspace_doc::T) -> Result<window, rays_diagnostic::Diagnostic> {
    lowering::window(doc)
}

pub mod editor {
    use std::any::Any;
    use std::rc::Rc;

    use editor_document::kind_graph as kg;
    use editor_document::objects;
    use editor_document::workspace_doc;
    use flow_graph::flow_edit as fe;
    use flow_native::{workspace_gpu, workspace_images};
    use rays::{easy_camera, frame};

    use crate::cook;
    use crate::editor_inspect;
    use crate::editor_loop;
    use crate::editor_model;
    use crate::editor_panels;
    use crate::editor_renderer as renderer_mod;
    use crate::editor_status;
    use crate::loop_model;
    use crate::source_file;
    use crate::viewport as V;

    pub use crate::editor_model::Picked;
    pub use crate::loop_model::Create;
    pub use crate::loop_model::DrawFn;
    pub use crate::loop_model::{
        camera, can_redo, can_undo, carry_line, carrying, displayed_node, document, edit, edit_layout, edit_phases, level, network, node_box, panes, prepared, probe, redo_label, scene_document, selected_node, set_probe, set_settings, settings, timeline,
        undo_label, workspace, world,
    };
    pub type PreparedT = cook::Prepared;
    pub type Editor = loop_model::LoopModel;
    pub type Prepared = cook::Prepared;

    /// What the app prepares of a cooked object: its geometry and its surface.
    pub fn prepare(settings: &editor_document::settings::T, output: &sop::session::Output) -> Result<cook::Prepared, String> {
        cook::prepare(settings, output)
    }

    /// The object's 3D scene, pieces exploded as its node says.
    pub fn draw(node: &sop::node::T, prepared: &cook::Prepared) -> Result<rays::scene3::Scene3, rdk_rays::surface::ConversionError> {
        cook::draw(node, prepared)
    }

    /// `prepare` and `draw` as the closures `create` and `run` take.
    pub fn prepare_fn() -> cook::PrepareFn {
        Rc::new(|s, o| cook::prepare(s, o))
    }
    pub fn draw_fn() -> DrawFn {
        Rc::new(|n, p| cook::draw(n, p))
    }

    /// `[await_]` (default false) makes each `update` block on the cook it submits.
    pub fn create(options: Create, workspace: &workspace_doc::T, source: Option<source_file::SourceFile>, prepare: cook::PrepareFn, draw: DrawFn) -> Result<Editor, String> {
        loop_model::create(options, workspace, source, prepare, draw)
    }

    pub fn update(mut value: Editor, frame_: &frame::Frame) -> Editor {
        editor_loop::update(&mut value, frame_);
        value
    }

    /// `update`, also building sketch-owned kit widgets below the camera and render sections shown
    /// while no node is selected. The result is `None` on frames where that panel is not built.
    pub fn update_with(mut value: Editor, frame_: &frame::Frame, inspector: &dyn Fn(&mut pxui::ui::Ui) -> Rc<dyn Any>) -> (Editor, Option<Rc<dyn Any>>) {
        let inspected = editor_loop::update_with(&mut value, frame_, inspector, None);
        (value, inspected)
    }

    /// `update_with` for widgets that show nothing but what `revision` stands for (the sketch bumps it
    /// when that changes): the panel is a cached view kept while `revision` and the editor's own
    /// sections stay, `inspector` not called; the result is `None` on frames it is kept.
    pub fn update_with_revision(mut value: Editor, frame_: &frame::Frame, revision: u64, inspector: &dyn Fn(&mut pxui::ui::Ui) -> Rc<dyn Any>) -> (Editor, Option<Rc<dyn Any>>) {
        let inspected = editor_loop::update_with(&mut value, frame_, inspector, Some(revision));
        (value, inspected)
    }

    /// Call from the sketch's `after_present` when driving the environment manually so PNG requests
    /// save the completed frame.
    pub fn after_present(mut value: Editor, frame_: &frame::Frame) -> Editor {
        editor_loop::after_present(&mut value, frame_);
        value
    }

    pub fn scene(value: &Editor, frame_: &frame::Frame) -> Vec<rays::scene::Node> {
        editor_loop::scene_of(value, frame_)
    }

    pub fn close(value: &Editor) {
        editor_loop::close(value)
    }

    pub fn crash_dump(value: &Editor, directory: &str) -> Result<(), String> {
        editor_loop::crash_dump(value, directory)
    }

    pub fn viewport_camera(value: &Editor, key: &str) -> rays::camera::Camera {
        loop_model::viewport_camera(value, key)
    }

    pub fn lights(value: &Editor) -> Vec<rays::light::Light> {
        loop_model::lights(value)
    }

    /// Visible, renderable geometry objects' latest cooks at their world transforms.
    pub fn objects(value: &Editor) -> Vec<(rays_math::mat4::T, cook::PreparedRef)> {
        loop_model::objects_placed(value)
    }

    /// Replace the model's core outside a frame (a test's `{ editor with core }`).
    #[doc(hidden)]
    pub fn with_core(value: Editor, core: editor_model::EditorModel) -> Editor {
        loop_model::with_core(value, core)
    }

    #[doc(hidden)]
    pub mod private {
        use super::*;

        /// Explicit native benchmark selection.
        pub fn gpu_qualification(value: &Editor) {
            workspace_gpu::qualification(&value.host.borrow().gpu)
        }

        /// The editor's UI (its last frame's draw list, for exact comparisons).
        pub fn ui(value: &Editor) -> Rc<std::cell::RefCell<pxui::ui::Ui>> {
            value.core.ui.clone()
        }

        /// The crash dump's editor.txt, without writing the dump.
        pub fn state_text(value: &Editor) -> String {
            editor_loop::state_text(value)
        }

        /// The shell the editor's own lowering of its workspace made.
        pub fn shell(value: &Editor) -> Option<editor_document::document::Shell> {
            value.core.doc.shell.as_deref().cloned()
        }

        pub fn resolved_scene(value: &Editor) -> kg::T {
            loop_model::resolved_scene(value)
        }

        /// The status strip's text: a cook or edit error reads "Cook rejected: ..." in it.
        pub fn status_text(value: &Editor) -> String {
            editor_status::status_text(&(&value.core).into(), false)
        }

        /// The frame facts the sketch read last frame: the pointer only while it is the picture's.
        pub fn live_frame(value: &Editor) -> input::Frame {
            value.core.live_frame.clone()
        }

        /// The node the inspector shows for the selection, when it is exactly one.
        pub fn inspector_subject(value: &Editor) -> Option<flow_check::workspace::Path> {
            editor_panels::inspector_subject(&(&value.core).into())
        }

        /// The `Set_graph` a "New graph" of this context name (`sop`, `scene`, ...) writes.
        pub fn new_graph(value: &Editor, context: &str) -> fe::Op {
            editor_inspect::new_graph(&value.core, context).1
        }

        /// The language forms of the add menu with the Lisp each is added as.
        pub fn menu_forms() -> Vec<(String, String)> {
            crate::editor_add::FORMS.iter().map(|(_, form, _)| (form.to_string(), crate::editor_add::form_example(form).to_string())).collect()
        }

        /// Completions from the vocabulary currently used by the workspace text panes.
        pub fn lisp_completions(value: &Editor, text: &str) -> Vec<pxui::ui::Completion> {
            let core = &value.core;
            crate::lisp_text::complete(&crate::editor_shell::completion_names(core), &core.doc.workspace.ws.import_sources, core.lisp_vocab.force(), text, text.len())
        }

        pub fn image_plan(value: &Editor) -> flow_eval::Plan {
            value.core.doc.workspace.lowered.plan.clone()
        }

        /// Inspect the owned plan and request an independent immutable CPU image cook.
        pub fn image_payload(value: &Editor, image: &flow_syntax::value::Value, state: Option<&flow_eval::State>, live: Option<&input::Frame>) -> Result<sop::image::T, rays_diagnostic::Diagnostic> {
            let core = &value.core;
            let host = value.host.borrow();
            workspace_images::bind(&host.images, &core.doc.workspace.lowered);
            let resolver = workspace_images::payload(&host.images);
            let state = state.cloned().unwrap_or_else(|| core.cook.state.clone());
            let live = live.cloned().unwrap_or_else(|| core.live_frame.clone());
            resolver(None, None, &image_plan(value), &state, &live, image)
        }

        pub fn image_stats(value: &Editor) -> (i64, i64) {
            let host = value.host.borrow();
            let resources = host.resources.0.borrow();
            (resources.images_created, resources.images_destroyed)
        }

        /// Canvases created/destroyed, successful captures and actual GPU pixel readbacks.
        pub fn image_render_stats(value: &Editor) -> (i64, i64, i64, i64) {
            workspace_images::render_stats(&value.host.borrow().images)
        }

        /// The picture each canvas pane drew last frame, in layout order.
        pub fn canvas_scenes(value: &Editor) -> Vec<Vec<rays::scene::Node>> {
            value.canvases.iter().map(|(_, p)| p.scene.clone()).collect()
        }

        /// Cumulative Host totals, sinks created/closed and sink buffer/texture creations.
        pub fn image_gpu_stats(value: &Editor) -> (flow_gpu::host::Stats, i64, i64, i64, i64) {
            workspace_gpu::image_stats(&value.host.borrow().gpu)
        }

        pub fn image(value: &Editor, image: &flow_syntax::value::Value) -> Result<rays::image::Image, rays_diagnostic::Diagnostic> {
            let core = &value.core;
            let host = value.host.borrow();
            workspace_images::bind(&host.images, &core.doc.workspace.lowered);
            let gpu = workspace_gpu::config(&host.gpu, editor_document::lowering::fast(&core.doc));
            workspace_images::image(Some(&gpu), true, &host.images, &core.cook.state, &core.live_frame, &core.doc.workspace.lowered.plan, image)
        }

        /// Scoped production image ownership and initial-domain SOP snapshots for qualification:
        /// `run` receives the image and texture resolvers.
        pub fn with_images(
            value: &Editor,
            state: Option<&flow_eval::State>,
            live: Option<&input::Frame>,
            plan: Option<&flow_eval::Plan>,
            run: &mut dyn FnMut(&dyn Fn(&flow_syntax::value::Value) -> Result<rays::image::Image, rays_diagnostic::Diagnostic>, &dyn Fn(&flow_syntax::value::Value) -> Result<rays::texture::Texture, rays_diagnostic::Diagnostic>),
        ) {
            let core = &value.core;
            let state = state.cloned().unwrap_or_else(|| core.cook.state.clone());
            let live = live.cloned().unwrap_or_else(|| core.live_frame.clone());
            let plan = plan.cloned().unwrap_or_else(|| core.doc.workspace.lowered.plan.clone());
            let (images, gpu_t) = {
                let host = value.host.borrow();
                (host.images.clone(), host.gpu.clone())
            };
            workspace_images::bind(&images, &core.doc.workspace.lowered);
            workspace_gpu::with_backend(&gpu_t, editor_document::lowering::fast(&core.doc), &mut |gpu| {
                let peek_images = images.clone();
                let metadata: flow_sop::lower::ImageMetadata = {
                    // ponytail: the dimensions callback is Send + Sync; the images are not, so it answers none
                    let _ = &peek_images;
                    std::sync::Arc::new(|_, _| None)
                };
                flow_sop::lower::with_images(Some(gpu.clone()), Some(metadata), workspace_images::payload(&images), &mut || {
                    let image = |v: &flow_syntax::value::Value| workspace_images::image(Some(gpu), true, &images, &state, &live, &plan, v);
                    let texture = |v: &flow_syntax::value::Value| workspace_images::texture(Some(gpu), &images, &state, &live, &plan, v);
                    run(&image, &texture);
                });
            });
        }

        /// Quit requested, effects fired, samples created and samples destroyed.
        pub fn host_stats(value: &Editor) -> (bool, i64, i64, i64) {
            let host = value.host.borrow();
            let resources = host.resources.0.borrow();
            (host.quit_requested, host.fired, resources.samples_created, resources.samples_destroyed)
        }
    }

    /// The scene root's output resolution in pixels and the samples per pixel at which a sketch's
    /// progressive renderer stops.
    pub type RenderSettings = objects::root::Render;

    /// The ACTIVE camera object's view and lens (the viewport camera when the document has none).
    pub fn render_camera(value: &Editor) -> rays::camera::Camera {
        value.extra.document_camera.clone()
    }

    /// What the view pane shows this frame, for a sketch's own renderer.
    pub fn view_camera(value: &Editor) -> rays::camera::Camera {
        loop_model::view_camera(value)
    }

    pub fn render_settings(value: &Editor) -> RenderSettings {
        objects::root::render(&value.extra.root)
    }

    /// The rect inside the view pane (pane-relative) that the render fills.
    pub fn film(value: &Editor, frame_: &frame::Frame) -> (i64, i64, i64, i64) {
        loop_model::film(value, frame_)
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct Slot {
        pub film: (i64, i64),
        pub step: i64,
        pub samples: i64,
        pub max_spp: i64,
        pub viewports: i64,
    }

    /// The path tracer slot serving the viewport with this key, when it has one.
    pub fn slot_of(value: &Editor, key: &str) -> Option<Slot> {
        renderer_mod::info(&value.extra.renderer, key).map(|i| Slot { film: i.size, step: i.step, samples: i.samples, max_spp: i.cap, viewports: i.viewports })
    }

    pub fn flying(value: &Editor) -> bool {
        value.extra.fly.is_some()
    }

    pub fn look_through(value: &Editor) -> bool {
        V::look_through(&value.extra)
    }

    pub fn renderer(value: &Editor) -> renderer_mod::Mode {
        value.extra.renderer.mode
    }

    pub fn set_renderer(value: Editor, mode: renderer_mod::Mode) -> Editor {
        let mut value = value;
        value.extra.renderer_request = Some(mode);
        value.rest = None;
        value
    }

    pub fn set_camera(value: Editor, camera: easy_camera::EasyCamera) -> Editor {
        let mut value = value;
        value.camera = camera;
        value.rest = None;
        value
    }

    #[allow(clippy::too_many_arguments)]
    pub fn run(
        options: Create,
        max_frames: Option<i64>,
        crash_dir: Option<String>,
        profile: Option<bool>,
        config: rays::sketch::Config,
        workspace: workspace_doc::T,
        source: Option<source_file::SourceFile>,
        prepare: cook::PrepareFn,
        draw: DrawFn,
    ) -> Result<(), rays::sketch::Error> {
        editor_loop::run(options, max_frames, crash_dir, profile, config, workspace, source, prepare, draw)
    }
}

/// The frame reducer without a UI frame: an unstable hook for tests, like `Editor::Private`.
#[doc(hidden)]
pub mod reduce {
    use flow_syntax::symbol::Symbol;
    use editor_core::panels::Panel;
    use flow_graph::flow_edit as fe;
    use rays::frame;

    use super::editor;
    use crate::editor_commands::Action;
    use crate::editor_inspect;
    use crate::editor_panes;
    use crate::editor_reduce;
    use crate::editor_scope;
    use crate::editor_shell;

    pub fn open_import(editor: editor::Editor, file: &str) -> editor::Editor {
        let mut core = (*editor.core).clone();
        core.open_import = Some(file.to_string());
        editor::with_core(editor, core)
    }

    pub fn spreadsheet(editor: &editor::Editor, path: &[usize]) -> (i64, Option<rdk_rays::geometry_view::T>) {
        let key = editor_shell::panel_key(&editor.core.doc, path);
        let pane = editor_panes::shown_as(&(&editor.core).into(), (&key, &path.to_vec(), &Panel::Spreadsheet));
        (editor_panes::local_of(&editor.core.locals, &key).sheet_owner, editor_inspect::spreadsheet_source(&pane).cloned())
    }

    /// Select the checked lexical path; unstable test/diagnostic hook.
    pub fn select_path(editor: editor::Editor, path: &[Symbol]) -> editor::Editor {
        match path.first() {
            Some(graph) => {
                let mut core = (*editor.core).clone();
                editor_panes::go(&mut core, graph);
                core.select_later = vec![path.to_vec()];
                editor::with_core(editor, core)
            }
            None => editor,
        }
    }

    /// `v` on the checked lexical path, as the graph pane requests it; unstable test hook.
    pub fn view(editor: editor::Editor, path: &[Symbol]) -> editor::Editor {
        let mut core = (*editor.core).clone();
        editor_scope::view_node(&mut core, path);
        editor::with_core(editor, core)
    }

    /// Reduces one frame in which no pane was built: the keys are `actions`, the list's selection
    /// is `select` (scene object ids), and `preview` puts a payload in flight whose hot target shows
    /// that edit on a scratch document, as a carry does.
    pub fn step(editor: editor::Editor, select: Option<&[i64]>, preview: Option<&fe::Op>, actions: &[Action], frame_: &frame::Frame) -> Result<editor::Editor, String> {
        let core = editor_reduce::reduce_idle(select, preview, &editor.core, actions, frame_)?;
        Ok(editor::with_core(editor, core))
    }
}

/// A `.rays` sketch as a program. Command-S rewrites the file when it is still the text the
/// document came from (comments intact), else saves a preset; an edit of the file from any editor
/// reloads the running document as one history entry, "Reload <file>", and a file that does not
/// check leaves the last good document with the diagnostics in the status bar and the text pane.
pub mod workspace {
    use std::rc::Rc;

    use editor_document::document::T as Document;
    use editor_document::lowering;
    use editor_document::workspace_doc;
    use flow_native::{drawing, workspace_gpu, workspace_host, workspace_images};
    use rays::easy_camera;
    use rays_diagnostic::Diagnostic;
    use rays_math::vec3;
    use sop::edit_graph::Factory;

    use super::{workspace_catalog, workspace_window};
    use crate::cook;
    use crate::editor_loop;
    use crate::loop_model;
    use crate::source_file;

    /// Where a workspace came from: `path` relative to the project root, `digest` the SHA-256 of
    /// its text.
    #[derive(Clone, Debug, PartialEq)]
    pub struct Source {
        pub path: String,
        pub digest: String,
    }

    /// What the process environment says, parsed once by the executable; the libraries never read it.
    #[derive(Clone, Debug, PartialEq, Default)]
    pub struct Process {
        pub max_frames: Option<i64>,
        pub crash_dir: Option<String>,
        pub profile: bool,
        pub preferences: Option<String>,
        pub ui_font: Option<String>,
    }

    pub fn no_process() -> Process {
        Process::default()
    }

    /// Load resources relative to the authored workspace, independent of the process directory.
    pub fn load_file(path: &str, text: &str) -> Result<workspace_doc::T, Vec<Diagnostic>> {
        let imports = source_file::read_imports(path, text)?;
        let mut ops = flow_sop::operators::all().to_vec();
        let base = std::path::Path::new(path).parent().unwrap_or_else(|| std::path::Path::new("."));
        if let Some(op) = ops.iter_mut().find(|op| op.name == "table/load") { *op = flow_check::table::operator(base.to_path_buf()); }
        load(Some(ops), imports, None, text)
    }

    /// Parse and check a `.rays` text against `workspace_catalog`.
    pub fn load(ops: Option<Vec<flow_check::op::Op>>, imports: Vec<(String, String)>, factories: Option<&[Factory]>, text: &str) -> Result<workspace_doc::T, Vec<Diagnostic>> {
        match workspace_catalog(factories) {
            Err(d) => Err(vec![d]),
            Ok(catalog) => workspace_doc::of_text(&workspace_doc::TextOptions { ops: ops.map(std::sync::Arc::new), imports, ..Default::default() }, &catalog, text),
        }
    }

    /// Every `sop` graph of the document, lowered and compiled, for headless cooking.
    pub fn sop_graphs(factories: Option<Vec<Factory>>, doc: &workspace_doc::T) -> Result<Vec<(String, sop::node::T)>, String> {
        let factories = factories.unwrap_or_else(sop::nodes::factories);
        let lowered = flow_sop::lower::workspace(
            Some(factories),
            editor_document::kinds::descriptors(),
            flow_sop::lower::Options { inputs: Some((*doc.inputs).clone()), ..Default::default() },
            &doc.source,
        )
        .map_err(|d| rays_diagnostic::to_string(&d))?;
        let mut out = Vec::new();
        for g in &lowered.graphs {
            if let Some(node_id) = g.root {
                out.push((g.name.clone(), sop::edit_graph::compile_node(&g.network.geometry, node_id)?));
            }
        }
        Ok(out)
    }

    pub fn document_camera(document: &Document, base: &easy_camera::EasyCamera) -> easy_camera::EasyCamera {
        let scene = &document.scene;
        let first = editor_document::objects::ids("camera", scene).first().copied();
        match first.and_then(|id| editor_document::kind_graph::find(scene, id)).and_then(|n| editor_document::objects::camera::of_node(&n)) {
            Some((view, _)) => easy_camera::of_view(rays::camera::position(&view), rays::camera::target(&view), base),
            None => base.clone(),
        }
    }

    /// The camera of the scene's first camera node, or the default base camera.
    pub fn declared_camera(factories: Option<Vec<Factory>>, doc: &workspace_doc::T, base: &easy_camera::EasyCamera) -> easy_camera::EasyCamera {
        let options = lowering::Options { factories: Some(factories.unwrap_or_else(sop::nodes::factories)), ..Default::default() };
        match lowering::of_workspace(&options, doc) {
            Err(_) => base.clone(),
            Ok(document) => document_camera(&document, base),
        }
    }

    /// One static evaluation a start: the document the editor opens is built here, and the window
    /// and the camera are read from it.
    fn run(inputs: Option<Rc<workspace_doc::Inputs>>, factories: Option<Vec<Factory>>, source: Option<Source>, process: Process, doc: workspace_doc::T) -> Result<(), Diagnostic> {
        let mut doc = doc;
        if let Some(inputs) = &inputs {
            doc.inputs = std::sync::Arc::new((**inputs).clone());
        }
        let gpu_owner = workspace_gpu::create(None);
        let result = (|| {
            let gpu = workspace_gpu::config(&gpu_owner, false);
            let source = source
                .and_then(|s| source_file::find(&s.path, &s.digest))
                .map(|s| source_file::with_imports(&s, &workspace_doc::import_texts(&doc)));
            let opened = match &factories {
                Some(f) if f.is_empty() => None,
                _ => lowering::of_workspace(&lowering::Options { factories: Some(factories.clone().unwrap_or_else(sop::nodes::factories)), gpu: Some(gpu.clone()), ..Default::default() }, &doc).ok(),
            };
            let window = match &opened {
                Some(document) => lowering::document_window(document),
                None => workspace_window(&doc),
            }?;
            let config = rays::sketch::Config { width: window.width, height: window.height, title: window.title.clone(), fps: Some(window.fps), ..rays::sketch::default_config() };
            let lights = vec![rays::light::directional(Some(rays_math::color::white()), None, vec3::create(-1., -1.4, -0.8)).map_err(|m| rays_diagnostic::error("E_RUNTIME".to_string(), m, None, None, None, Vec::new(), Vec::new()))?];
            let base = easy_camera::create(Some(vec3::zero()), Some(3.6), Some(0.4), Some(0.6), None, None).expect("base camera");
            let camera = match (&opened, &factories) {
                (Some(document), _) => document_camera(document, &base),
                (None, Some(f)) if f.is_empty() => declared_camera(Some(Vec::new()), &doc, &base),
                (None, _) => base,
            };
            editor_loop::run(
                loop_model::Create {
                    inputs: inputs.clone(),
                    gpu_owner: Some(gpu_owner.clone()),
                    lights,
                    camera: Some(camera),
                    factories: factories.clone(),
                    seed: Some(window.seed),
                    preferences: process.preferences.clone(),
                    ui_font: process.ui_font.clone(),
                    opened,
                    ..Default::default()
                },
                process.max_frames,
                process.crash_dir.clone(),
                Some(process.profile),
                config,
                doc.clone(),
                source,
                Rc::new(|s, o| cook::prepare(s, o)),
                Rc::new(|n, p| cook::draw(n, p)),
            )
            .map_err(|error| rays_diagnostic::error("E_RUNTIME".to_string(), rays::sketch::error_to_string(&error), None, None, None, Vec::new(), Vec::new()))
        })();
        workspace_gpu::close(&gpu_owner);
        result
    }

    /// Render only a draw graph to a native offscreen target, with a fresh deterministic clock.
    pub fn render_png(graph: Option<&str>, size: Option<(i64, i64)>, frame: i64, path: &str, doc: &workspace_doc::T) -> Result<(), Diagnostic> {
        let fail = |code: &str, message: String| rays_diagnostic::error(code.to_string(), message, None, None, None, Vec::new(), Vec::new());
        let window = workspace_window(doc)?;
        let (width, height) = size.unwrap_or((window.width, window.height));
        if frame < 0 || width <= 0 || height <= 0 {
            return Err(fail("E_RENDER_RANGE", "Frame must be nonnegative and dimensions positive.".into()));
        }
        let chosen = doc.checked.graphs.iter().find(|g| g.context == flow_syntax::context::draw() && graph.is_none_or(|n| g.name == n));
        let graph = chosen.ok_or_else(|| fail("E_DRAW_GRAPH", format!("Render needs a draw graph{}.", graph.map(|g| format!(" named {g}")).unwrap_or_default())))?;
        let lowered = flow_sop::lower::of_checked(Some(sop::nodes::factories()), flow_sop::lower::Options { reference: true, inputs: Some((*doc.inputs).clone()), ..Default::default() }, &doc.checked)?;
        let evaluated = &lowered.evaluated;
        let value = evaluated.results.iter().find(|(name, _)| *name == graph.name).map(|(_, value)| value.clone()).ok_or_else(|| fail("E_DRAW_GRAPH", "Draw graph has no result.".into()))?;
        let prepared = drawing::prepare(&drawing::PrepareOptions { states: evaluated.states.clone(), ..Default::default() }, &evaluated.plan, &value)?;
        let mut host = workspace_host::create(workspace_host::CreateOptions { seed: Some(window.seed), ..Default::default() });
        workspace_images::bind(&host.images, &lowered);
        let result = (|| {
            let canvas = rays::canvas::create(width, height).map_err(|m| fail("E_GPU_STARTUP", m))?;
            let result = (|| {
                rays::canvas::render(Some(1), &canvas, &[]).map_err(|m| fail("E_GPU_STARTUP", m))?;
                let state = flow_eval::create_state();
                for tick in 0..=frame {
                    let live = input::Frame { dt: 1. / 60., frame: tick, tick, size: (width, height), pointer: (-100., -100.), ..input::at_time(tick as f64 / 60.) };
                    let (images, image_state, image_plan, image_live) = (host.images.clone(), state.clone(), evaluated.plan.clone(), live.clone());
                    let resolver: drawing::ImageResolver = Rc::new(move |v| workspace_images::image(None, false, &images, &image_state, &image_live, &image_plan, v));
                    let scene = drawing::render_prepared(&prepared, &live, live.size, &drawing::RenderOptions { state: Some(state.clone()), image: Some(resolver), ..Default::default() })?;
                    if tick == frame {
                        rays::canvas::render(Some(1), &canvas, &scene).map_err(|m| fail("E_RENDER", m))?;
                    }
                }
                rays::canvas::save_png(&canvas, path).map_err(|m| fail("E_IO", m))
            })();
            let closed = rays::canvas::destroy(&canvas).map_err(|m| fail("E_RENDER", m));
            result.and(closed)
        })();
        workspace_host::close(&mut host);
        result
    }

    /// Export a draw graph (the first by default) through `Sketch::export_state`. Each invocation
    /// starts a fresh fold and pins every frame fact.
    pub fn export(graph: Option<&str>, fps: Option<i64>, directory: &str, frames: i64, doc: &workspace_doc::T) -> Result<(), Diagnostic> {
        let fps = fps.unwrap_or(60);
        let factories = sop::nodes::factories();
        if !(fps > 0 && frames > 0) {
            return Err(rays_diagnostic::error("E_EXPORT_RANGE".to_string(), "Export frame count and fps must be positive.".to_string(), None, None, None, Vec::new(), Vec::new()));
        }
        let window = workspace_window(doc)?;
        let lowered = flow_sop::lower::of_checked(Some(factories), flow_sop::lower::Options { reference: true, inputs: Some((*doc.inputs).clone()), ..Default::default() }, &doc.checked)?;
        let evaluated = lowered.evaluated.clone();
        workspace_host::export_check(doc, &evaluated.plan)?;
        let chosen = doc.checked.graphs.iter().find(|g| g.context == flow_syntax::context::draw() && graph.is_none_or(|n| g.name == n));
        let Some(graph) = chosen else {
            return Err(rays_diagnostic::error("E_DRAW_GRAPH".to_string(), "Export needs a draw graph.".to_string(), None, None, None, Vec::new(), Vec::new()));
        };
        let state = flow_eval::create_state();
        let host = Rc::new(std::cell::RefCell::new(workspace_host::create(workspace_host::CreateOptions { seed: Some(window.seed), ..Default::default() })));
        let images = host.borrow().images.clone();
        workspace_images::bind(&images, &lowered);
        let value = evaluated.results.iter().find(|(n, _)| *n == graph.name).map(|(_, v)| v.clone()).expect("graph result");
        let prepared = drawing::prepare(&drawing::PrepareOptions { states: evaluated.states.clone(), ..Default::default() }, &evaluated.plan, &value)?;
        let failure: Rc<std::cell::RefCell<Option<Diagnostic>>> = Rc::new(std::cell::RefCell::new(None));
        let (view_host, view_images, view_failure, view_doc, view_plan, view_state) = (host.clone(), images.clone(), failure.clone(), doc.clone(), evaluated.plan.clone(), state.clone());
        let (width, height) = (window.width, window.height);
        let view = move |_model: &dyn std::any::Any, frame_: &rays::frame::Frame| -> Vec<rays::scene::Node> {
            let live = input::Frame { dt: 1. / fps as f64, frame: frame_.input.frame, tick: frame_.input.tick, size: (width, height), ..input::at_time(frame_.input.t) };
            let images = view_images.clone();
            let (state, plan) = (view_state.clone(), view_plan.clone());
            let live_for_image = live.clone();
            let resolver: drawing::ImageResolver = Rc::new(move |v| workspace_images::image(None, false, &images, &state, &live_for_image, &plan, v));
            let options = drawing::RenderOptions { state: Some(view_state.clone()), image: Some(resolver), ..Default::default() };
            match drawing::render_prepared(&prepared, &live, live.size, &options) {
                Ok(scene) => match workspace_host::export_update(&mut view_host.borrow_mut(), &view_state, &live, &view_doc, &view_plan) {
                    Ok(()) => scene,
                    Err(d) => {
                        view_failure.borrow_mut().get_or_insert(d);
                        Vec::new()
                    }
                },
                Err(d) => {
                    view_failure.borrow_mut().get_or_insert(d);
                    Vec::new()
                }
            }
        };
        let config = rays::sketch::Config { width, height, title: window.title.clone(), clock: rays::sketch::Clock::Fixed(1. / fps as f64), ..rays::sketch::default_config() };
        let after_host = host.clone();
        let after_failure = failure.clone();
        let stop_host = host.clone();
        let program = rays::sketch::Program {
            init: Box::new(|_| Box::new(())),
            update: Box::new(|_, _| None),
            view: Box::new(view),
            after_present: Some(Box::new(move |_, _| {
                if let Err(d) = workspace_host::save_pending(&mut after_host.borrow_mut(), &mut |p| rays::canvas::save_screen_png(p)) {
                    after_failure.borrow_mut().get_or_insert(d);
                }
                None
            })),
            crash_dump: None,
            idle: None,
            on_stop: Some(Box::new(move |_| workspace_host::close(&mut stop_host.borrow_mut()))),
        };
        let exported = rays::sketch::export_state(Some(config), Some(fps), None, directory, frames, program);
        if let Some(d) = failure.borrow_mut().take() {
            return Err(d);
        }
        exported.map(|_| ()).map_err(|error| rays_diagnostic::error("E_EXPORT".to_string(), rays::sketch::error_to_string(&error), None, None, None, Vec::new(), Vec::new()))
    }

    /// `main path` opens the `.rays` file at `path` and its imports beside it in the editor; `text`
    /// opens that text instead, saved to `path` (the empty editor). On a failure it returns
    /// diagnostics without printing or exiting.
    pub fn main(text: Option<String>, process: Option<Process>, path: &str) -> Result<(), Vec<Diagnostic>> {
        let text = match text {
            Some(text) => text,
            None => std::fs::read(path)
                .map_err(|e| vec![rays_diagnostic::error("E_IO".to_string(), e.to_string(), None, None, None, Vec::new(), Vec::new())])
                .map(|bytes| String::from_utf8_lossy(&bytes).into_owned())?,
        };
        let doc = load_file(path, &text)?;
        run(None, None, Some(Source { path: path.to_string(), digest: lowering::sha256(&text) }), process.unwrap_or_default(), doc).map_err(|d| vec![d])
    }
}

/// One workspace's real frames without a window: update, render into an offscreen canvas, and the
/// frame boundary (`after_present`), on one domain. The one runner behind `rays mem`.
pub mod measure {
    use super::editor;
    use flow_native::workspace_gpu;
    use rays::frame;

    #[derive(Clone, Debug)]
    pub struct Sample {
        pub frame: i64,
        pub live: i64,
        /// The device's allocated bytes, through OGPU.
        pub device: i64,
        pub footprint: i64,
        pub allocated: f64,
        pub promoted: f64,
        pub report: mem::Report,
    }

    #[derive(Clone, Debug)]
    pub struct Outcome {
        pub early: Sample,
        pub fin: Sample,
        pub fallbacks: Vec<(String, String, i64)>,
        pub readback_bytes: i64,
        /// arrays read back; the rest is bounds a selection box asks for
        pub payload_readback_bytes: i64,
        /// the process's highest, read before the last report's walk
        pub peak_footprint: i64,
        pub memory_error: Option<String>,
        pub cook_error: Option<String>,
    }

    pub struct MeasureOptions {
        pub width: i64,
        pub height: i64,
        pub device: bool,
    }

    impl Default for MeasureOptions {
        fn default() -> MeasureOptions {
            MeasureOptions { width: 960, height: 640, device: false }
        }
    }

    fn frame_at(width: i64, height: i64, tick: i64) -> frame::Frame {
        frame::Frame {
            input: input::Frame { dt: 1. / 60., frame: tick, tick, size: (width, height), pointer: (-100., -100.), ..input::at_time(tick as f64 / 60.) },
            pixel_scale: (1., 1.),
            fps: 60.,
            mouse_delta: (0., 0.),
        }
    }

    /// `step` runs before each frame's update, for scripted edits.
    pub fn run(options: MeasureOptions, step: &dyn Fn(i64, editor::Editor) -> editor::Editor, early: i64, frames: i64, workspace: &editor_document::workspace_doc::T) -> Result<Outcome, String> {
        let MeasureOptions { width, height, device } = options;
        let canvas = rays::canvas::create(width, height)?;
        let result = (|| {
            // its renderer and pipelines before the document opens, as a window makes them at
            // startup: a canvas makes them at its first render, which would land them on the first
            // cook's peak
            rays::canvas::render(Some(1), &canvas, &[])?;
            let gpu_owner = workspace_gpu::create(None);
            if device {
                workspace_gpu::qualification(&gpu_owner);
            }
            // its pixels: a walk of the canvas would reach the renderer's charged caches too
            let canvas_owner = mem::plan("rays.canvas");
            let canvas_bytes = width * height * 4;
            mem::charge(&canvas_owner, canvas_bytes);
            let result = (|| {
                let editor = editor::create(
                    editor::Create { gpu_owner: Some(gpu_owner.clone()), ui_text_size: Some(11), await_: Some(true), domains: Some(1), ..Default::default() },
                    workspace,
                    None,
                    editor::prepare_fn(),
                    editor::draw_fn(),
                )?;
                let mut editor = editor;
                let allocated0 = mem::allocated_bytes() as f64;
                let measure = |editor: &editor::Editor, walk: bool, tick: i64| -> Sample {
                    let device = workspace_gpu::device_limits(&editor.host.borrow().gpu).map(|(b, _, _)| b).unwrap_or(0);
                    // one walk, the last: a walk's hash set stays in the footprint of the next sample
                    let report = mem::report_with(mem::ReportOptions { walk, device, budget: None });
                    Sample { frame: tick, live: report.live_bytes, device, footprint: report.footprint, allocated: mem::allocated_bytes() as f64 - allocated0, promoted: 0., report }
                };
                let mut first: Option<Sample> = None;
                let mut failure: Option<String> = None;
                let mut tick = 0;
                while failure.is_none() && tick < frames {
                    tick += 1;
                    let at = frame_at(width, height, tick);
                    editor = step(tick, editor);
                    editor = editor::update(editor, &at);
                    if let Err(message) = rays::canvas::render(Some(1), &canvas, &editor::scene(&editor, &at)) {
                        failure = Some(message);
                    }
                    editor = editor::after_present(editor, &at);
                    if tick == early {
                        first = Some(measure(&editor, false, tick));
                    }
                }
                let outcome = match failure {
                    Some(message) => Err(message),
                    None => {
                        let peak_footprint = mem::peak_footprint().unwrap_or(0);
                        let fin = measure(&editor, true, frames);
                        let early = first.unwrap_or_else(|| fin.clone());
                        let (stats, _, _, _, _) = workspace_gpu::image_stats(&editor.host.borrow().gpu);
                        Ok(Outcome {
                            early,
                            fin,
                            fallbacks: flow_ir::fallback::seen(),
                            readback_bytes: stats.readback_bytes,
                            payload_readback_bytes: stats.payload_readback_bytes,
                            peak_footprint,
                            memory_error: editor.memory_error.clone(),
                            cook_error: editor.core.cook.error.clone(),
                        })
                    }
                };
                editor::close(&editor);
                outcome
            })();
            mem::release(&canvas_owner, canvas_bytes);
            workspace_gpu::close(&gpu_owner);
            result
        })();
        let _ = rays::canvas::destroy(&canvas);
        result
    }
}
