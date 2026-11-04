//! Port of lib/flow_native/workspace_host.ml: the host effects a workspace's `effects` list asks
//! for (quit, save a PNG, play a sample, open a file dialog), their rising-edge bookkeeping and the
//! resources the workspace owns.
use crate::workspace_gpu;
use crate::workspace_images;
use crate::workspace_resources;
use editor_document::workspace_doc;
use flow_eval as e;
use flow_ir::flow_ir::Executor as I;
use flow_syntax::ty::Ty;
use flow_syntax::value::{self as v, Value};
use input::{Event, Frame};
use rays::audio::Sample;
use rays::sketch;
use rays_diagnostic::Diagnostic;

pub struct prepared {
    pub plan: e::Plan,
    /// the plan's identity (see `workspace_images::plan_id`), not its value
    pub plan_id: u64,
    pub effects: I::Program,
    pub args: Vec<Option<I::Program>>,
    pub edges: Vec<bool>,
}

pub struct t {
    pub resources: workspace_resources::t,
    pub images: workspace_images::t,
    pub gpu: workspace_gpu::t,
    pub prepared: Option<prepared>,
    /// newest first
    pub pending_saves: Vec<String>,
    pub quit_requested: bool,
    pub fired: i64,
    pub deterministic: bool,
    pub pending_events: Vec<Event>,
}

/// The optional arguments of [`create`].
#[derive(Default)]
pub struct CreateOptions {
    pub seed: Option<i64>,
    pub grain: Option<i64>,
    pub domains: Option<i64>,
    pub gpu: Option<workspace_gpu::t>,
}

pub fn create(o: CreateOptions) -> t {
    let resources = workspace_resources::create();
    let gpu = match o.gpu {
        Some(gpu) => gpu,
        None => workspace_gpu::create(None),
    };
    t {
        images: workspace_images::create(o.seed, o.grain, o.domains, &gpu, &resources),
        resources,
        gpu,
        prepared: None,
        pending_saves: Vec::new(),
        quit_requested: false,
        fired: 0,
        deterministic: false,
        pending_events: Vec::new(),
    }
}

pub fn take_events(host: &mut t) -> Vec<Event> {
    let mut events = std::mem::take(&mut host.pending_events);
    events.reverse();
    events
}

fn assoc<'a>(args: &'a [(String, Value)], key: &str) -> Option<&'a Value> {
    args.iter().find(|(k, _)| k == key).map(|(_, x)| x)
}

fn effect_ty() -> Ty {
    Ty::Named("effect".into())
}

fn sample_ty() -> Ty {
    Ty::Named("sample".into())
}

pub fn effects(workspace: &workspace_doc::T, plan: &e::Plan) -> Option<Value> {
    let graph = workspace_doc::editor_graph(workspace)?;
    let instance = plan.instances.iter().find(|i| i.default && i.graph == graph.name)?;
    match &instance.result {
        Value::Struct(name, _, args) if name == "ui/workspace" => assoc(args, "effects").cloned(),
        _ => None,
    }
}

pub fn prepare(plan: &e::Plan, effects: &Value) -> Result<prepared, Diagnostic> {
    let options = I::CompileOptions::default();
    let effects = I::compile(effects, &options)?;
    let mut args = Vec::with_capacity(plan.nodes.len());
    for node in &plan.nodes {
        if node.ty == effect_ty() || node.ty == sample_ty() {
            args.push(Some(I::compile(&Value::Record(node.args.clone()), &options)?));
        } else {
            args.push(None);
        }
    }
    Ok(prepared { plan: plan.clone(), plan_id: workspace_images::plan_id(plan), effects, edges: vec![false; plan.nodes.len()], args })
}

/// A rising-edge effect: its node id, `when`, kind and arguments.
pub struct request {
    pub id: i64,
    pub when_: bool,
    pub kind: String,
    pub args: Vec<(String, Value)>,
}

fn force(program: &I::Program, state: &e::State, reference: bool, live: &Frame) -> Result<Value, Diagnostic> {
    I::force(program, &I::ForceOptions { state: Some(state.clone()), reference, ..Default::default() }, live)
}

fn collect(plan: &e::Plan, prepared: &prepared, state: &e::State, reference: bool, live: &Frame, value: &Value, out: &mut Vec<request>) -> Result<(), Diagnostic> {
    match value {
        Value::List(values) => {
            for x in values.iter() {
                collect(plan, prepared, state, reference, live, x, out)?;
            }
            Ok(())
        }
        Value::Deferred(ty, id) if *ty == effect_ty() && *id >= 0 && (*id as usize) < plan.nodes.len() => {
            let forced = force(prepared.args[*id as usize].as_ref().expect("effect program"), state, reference, live)?;
            let Value::Record(args) = forced else { unreachable!("assert false") };
            let when_ = v::truthy(assoc(&args, "when").expect("when"))?;
            out.push(request { id: *id, when_, kind: plan.nodes[*id as usize].kind.clone(), args });
            Ok(())
        }
        _ => Err(v::fail("E_EFFECT", "Workspace effects are host effect nodes.")),
    }
}

/// The effects whose `when` rose this frame.
pub fn requests(host: &mut t, reference: bool, state: &e::State, live: &Frame, workspace: &workspace_doc::T, plan: &e::Plan) -> Result<Vec<request>, Diagnostic> {
    let Some(value) = effects(workspace, plan) else { return Ok(Vec::new()) };
    let id = workspace_images::plan_id(plan);
    if !host.prepared.as_ref().is_some_and(|p| p.plan_id == id) {
        host.prepared = Some(prepare(plan, &value)?);
    }
    let prepared = host.prepared.as_mut().expect("prepared");
    let mut result: Vec<request> = Vec::new();
    {
        let prepared_ref: &prepared = prepared;
        e::transaction(state, &mut || {
            let forced = force(&prepared_ref.effects, state, reference, live)?;
            let mut collected = Vec::new();
            collect(plan, prepared_ref, state, reference, live, &forced, &mut collected)?;
            result = collected;
            Ok(())
        })?;
    }
    let rising: Vec<request> = result.iter().filter(|r| r.when_ && !prepared.edges[r.id as usize]).map(|r| request { id: r.id, when_: r.when_, kind: r.kind.clone(), args: r.args.clone() }).collect();
    for r in &result {
        prepared.edges[r.id as usize] = r.when_;
    }
    Ok(rising)
}

fn text(value: &Value) -> Result<String, Diagnostic> {
    match value {
        Value::Text(text) => Ok(String::from_utf8_lossy(text).into_owned()),
        _ => Err(v::fail("E_TYPE", "Host paths and dialog kinds are text.")),
    }
}

fn sample(host: &t, state: &e::State, live: &Frame, value: &Value) -> Result<std::rc::Rc<std::cell::RefCell<Sample::t>>, Diagnostic> {
    let prepared = host.prepared.as_ref().expect("prepared");
    let audio_error = |message: &str| v::fail("E_AUDIO", message);
    match value {
        Value::Deferred(ty, id) if *ty == sample_ty() && *id >= 0 && (*id as usize) < prepared.plan.nodes.len() => {
            let forced = force(prepared.args[*id as usize].as_ref().expect("sample program"), state, false, live)?;
            let Value::Record(args) = forced else { unreachable!("assert false") };
            let text_of = |key: &str, default: &str| -> Result<String, Diagnostic> {
                match assoc(&args, key) {
                    None => Ok(default.to_owned()),
                    Some(Value::Text(t)) => Ok(String::from_utf8_lossy(t).into_owned()),
                    _ => Err(v::fail("E_TYPE", "A resource path or waveform is text.")),
                }
            };
            let number = |key: &str, default: f64| -> Result<f64, Diagnostic> { assoc(&args, key).map_or(Ok(default), v::num) };
            match prepared.plan.nodes[*id as usize].kind.as_str() {
                "audio/load" => {
                    let path = text_of("path", "")?;
                    workspace_resources::sample(&host.resources, &format!("load:{path}"), &|audio| Sample::load(audio, &path))
                }
                "audio/synth" => {
                    let wave = text_of("waveform", "sine")?;
                    let (frequency, duration, volume) = (number("frequency", 440.)?, number("duration", 0.1)?, number("volume", 1.)?);
                    let waveform = match wave.as_str() {
                        "sine" => Sample::waveform::Sine,
                        "square" => Sample::waveform::Square,
                        "triangle" => Sample::waveform::Triangle,
                        "sawtooth" => Sample::waveform::Saw,
                        _ => return Err(audio_error("Unknown audio waveform.")),
                    };
                    // the OCaml key prints the floats in hex; their bits name them just as well
                    let key = format!("synth:{wave}:{:x}:{:x}:{:x}", frequency.to_bits(), duration.to_bits(), volume.to_bits());
                    workspace_resources::sample(&host.resources, &key, &|audio| Sample::synth(audio, Some(volume), waveform, frequency, duration))
                }
                _ => Err(audio_error("Expected an audio sample node.")),
            }
        }
        _ => Err(audio_error("Expected an audio sample value.")),
    }
}

fn perform(host: &mut t, state: &e::State, live: &Frame, requests: &[request]) -> Result<(), Diagnostic> {
    for r in requests {
        host.fired += 1;
        let arg = |name: &str| assoc(&r.args, name).expect("effect argument");
        if host.deterministic && (r.kind == "host/quit" || r.kind == "host/dialog") {
            return Err(v::fail("E_EFFECT_EXPORT", "Quit and file dialogs are unavailable in fixed-step runs."));
        }
        match r.kind.as_str() {
            "host/quit" => {
                host.quit_requested = true;
                sketch::quit();
            }
            "host/save_png" => host.pending_saves.insert(0, text(arg("path"))?),
            "host/play" if host.deterministic => {}
            "host/play" => {
                let sample_ = sample(host, state, live, arg("sample"))?;
                let audio = host.resources.0.borrow().audio.clone().expect("audio");
                let volume = assoc(&r.args, "volume").map(v::num).transpose()?;
                let loops = assoc(&r.args, "loops").map(v::int_of).transpose()?;
                Sample::play(&audio, loops, volume, &sample_.borrow()).map_err(|m| v::fail("E_AUDIO", &m))?;
            }
            "host/dialog" => {
                let kind = match text(arg("kind"))?.as_str() {
                    "open_file" => sketch::dialog::Open_file,
                    "open_files" => sketch::dialog::Open_files,
                    "save_file" => sketch::dialog::Save_file,
                    "open_folder" => sketch::dialog::Open_folder,
                    _ => return Err(v::fail("E_EFFECT", "Unknown file dialog kind.")),
                };
                let filters: Option<Vec<(String, Vec<String>)>> = match assoc(&r.args, "filters") {
                    None => None,
                    Some(Value::List(entries)) => {
                        let mut out = Vec::new();
                        for entry in entries.iter() {
                            out.push(match entry {
                                Value::Record(fields) => match (assoc(fields, "label"), assoc(fields, "extensions")) {
                                    (Some(Value::Text(label)), Some(Value::List(extensions))) => {
                                        (String::from_utf8_lossy(label).into_owned(), extensions.iter().map(text).collect::<Result<Vec<_>, _>>()?)
                                    }
                                    _ => return Err(v::fail("E_EFFECT", "A filter has text :label and list :extensions.")),
                                },
                                Value::List(pair) => match pair.as_slice() {
                                    [Value::Text(label), Value::List(extensions)] => {
                                        (String::from_utf8_lossy(label).into_owned(), extensions.iter().map(text).collect::<Result<Vec<_>, _>>()?)
                                    }
                                    _ => return Err(v::fail("E_EFFECT", "A filter has :label and :extensions.")),
                                },
                                _ => return Err(v::fail("E_EFFECT", "A filter has :label and :extensions.")),
                            });
                        }
                        Some(out)
                    }
                    Some(_) => return Err(v::fail("E_EFFECT", "Dialog filters are a list.")),
                };
                let default_location = assoc(&r.args, "default").map(text).transpose()?;
                let id = sketch::show_file_dialog(filters.as_deref(), default_location.as_deref(), kind).map_err(|m| v::fail("E_EFFECT", &m))?;
                host.pending_events.insert(0, Event::Dialog_opened(id));
            }
            other => return Err(v::fail("E_EFFECT", &format!("Unknown host effect {other}"))),
        }
    }
    Ok(())
}

pub fn update(host: &mut t, state: &e::State, live: &Frame, workspace: &workspace_doc::T, plan: &e::Plan) -> Result<(), Diagnostic> {
    let requests = requests(host, false, state, live, workspace, plan)?;
    perform(host, state, live, &requests)
}

pub fn save_pending(host: &mut t, save: &mut dyn FnMut(&str) -> Result<(), String>) -> Result<(), Diagnostic> {
    let mut paths = std::mem::take(&mut host.pending_saves);
    paths.reverse();
    for path in paths {
        save(&path).map_err(|m| v::fail("E_EFFECT", &m))?;
    }
    Ok(())
}

pub fn close(host: &mut t) {
    workspace_resources::close(&host.resources);
    workspace_images::close(&host.images);
    workspace_gpu::close(&host.gpu);
}

pub fn export_check(workspace: &workspace_doc::T, plan: &e::Plan) -> Result<(), Diagnostic> {
    let Some(effects) = effects(workspace, plan) else { return Ok(()) };
    fn unsafe_(plan: &e::Plan, value: &Value) -> bool {
        match value {
            Value::List(values) => values.iter().any(|x| unsafe_(plan, x)),
            Value::Deferred(ty, id) if *ty == effect_ty() && *id >= 0 && (*id as usize) < plan.nodes.len() => {
                let node = &plan.nodes[*id as usize];
                (node.kind == "host/quit" || node.kind == "host/dialog") && assoc(&node.args, "when") == Some(&Value::Bool(true))
            }
            _ => false,
        }
    }
    if unsafe_(plan, &effects) {
        Err(v::fail("E_EFFECT_EXPORT", "Quit and file dialogs are unavailable during export."))
    } else {
        Ok(())
    }
}

pub fn export_update(host: &mut t, state: &e::State, live: &Frame, workspace: &workspace_doc::T, plan: &e::Plan) -> Result<(), Diagnostic> {
    let requests = requests(host, false, state, live, workspace, plan)?;
    // Export never initializes audio. Save paths are captured after presentation.
    if requests.iter().any(|r| r.kind == "host/quit" || r.kind == "host/dialog") {
        return Err(v::fail("E_EFFECT_EXPORT", "Quit and file dialogs are unavailable during export."));
    }
    for r in &requests {
        if r.kind == "host/save_png" {
            match assoc(&r.args, "path") {
                Some(Value::Text(path)) => host.pending_saves.insert(0, String::from_utf8_lossy(path).into_owned()),
                _ => return Err(v::fail("E_TYPE", "A PNG path is text.")),
            }
        }
    }
    Ok(())
}
