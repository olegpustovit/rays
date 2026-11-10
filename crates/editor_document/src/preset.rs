//! sketch presets: the editable document as one
//! s-expression file (`workspace_doc::to_text`, then a `(view ...)` form). No other format.
use crate::document;
use crate::kinds;
use crate::lowering;
use crate::settings;
use crate::workspace_doc::{self, TextOptions};
use flow_check::op::Op as FlowOp;
use flow_syntax::lisp;
use flow_syntax::syntax::{self as s, Node, Syntax};
use sop::edit_graph::Factory;

type S = Syntax;

pub struct Loaded {
    pub doc: document::T,
    /// Environment camera/render settings.
    pub view: S,
}

/// Keep `A-Za-z0-9_-`; every other character becomes `_`.
pub fn sanitize(name: &str) -> String {
    name.trim()
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() || c == '_' || c == '-' { c } else { '_' })
        .collect()
}

/// Time as `YYYY-MM-DD_HH-MM-SS`.
/// ponytail: UTC, not local time (std has no time zone); add the offset when a caller needs it.
pub fn default_name(now: f64) -> String {
    let secs = now.floor() as i64;
    let days = secs.div_euclid(86400);
    let rem = secs.rem_euclid(86400);
    // civil from days (Howard Hinnant)
    let z = days + 719468;
    let era = z.div_euclid(146097);
    let doe = z.rem_euclid(146097);
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let y = if m <= 2 { y + 1 } else { y };
    format!("{:04}-{:02}-{:02}_{:02}-{:02}-{:02}", y, m, d, rem / 3600, rem % 3600 / 60, rem % 60)
}

const SUFFIX: &str = ".rays";

/// `<directory>/<name>.rays`.
pub fn path(directory: &str, name: &str) -> String {
    format!("{}/{}{}", directory.trim_end_matches(['/', '\\']), sanitize(name), SUFFIX)
}

fn finite(f: &S) -> bool {
    match &f.node {
        Node::Num(n) => (f.name(*n)).parse::<f64>().ok().is_some_and(|x| x.is_finite()),
        Node::Vec(range) | Node::Map(range) | Node::List(range) => f.range(*range).iter().all(|id| finite(&f.child(*id))),
        _ => true,
    }
}

/// The canonical workspace text of `doc` (its settings beside it).
pub fn text(doc: &document::T) -> Result<String, String> {
    let workspace = &doc.workspace.ws;
    let text = if lowering::has_settings(workspace) {
        workspace_doc::to_text(workspace)
    } else {
        workspace_doc::to_text(&workspace_doc::T { settings: doc.settings.clone(), ..workspace.clone() })
    };
    text.map_err(|error| rays_diagnostic::to_string(&error))
}

/// Write atomically; returns the path. `Err` when the view has nonfinite numbers or the file
/// cannot be written.
pub fn save(directory: &str, name: &str, doc: &document::T, view: &S) -> Result<String, String> {
    if sanitize(name).is_empty() {
        return Err("preset name is empty".to_owned());
    }
    if !finite(view) {
        return Err("viewport contains nonfinite values".to_owned());
    }
    let body = text(doc)?;
    let mut builder = flow_syntax::arena::SyntaxBuilder::new();
    let head = builder.push_draft(0, flow_syntax::arena::Draft::Sym("view".to_owned()), rays_diagnostic::Span { start: 0, finish: 0 }, None);
    let view = builder.import(view);
    let root = builder.push_draft(0, flow_syntax::arena::Draft::List(vec![head, view]), rays_diagnostic::Span { start: 0, finish: 0 }, None);
    let view_text = lisp::print(&[builder.freeze().handle(root)]).map_err(|error| rays_diagnostic::to_string(&error))?.0;
    let text = format!("{body}\n{view_text}");
    rays_diagnostic::check_source_len(text.len()).map_err(|error| rays_diagnostic::to_string(&error))?;
    let target = path(directory, name);
    editor_core::store::write_text(&target, &text)?;
    Ok(target)
}

/// Preset names with modification times, newest first; empty when the directory is missing.
pub fn list(directory: &str) -> Vec<(String, f64)> {
    let Ok(files) = std::fs::read_dir(directory) else { return Vec::new() };
    let mut out: Vec<(String, f64)> = files
        .filter_map(|e| {
            let e = e.ok()?;
            let file = e.file_name().to_string_lossy().into_owned();
            let stem = file.strip_suffix(SUFFIX)?.to_owned();
            let mtime = e.metadata().ok()?.modified().ok()?.duration_since(std::time::UNIX_EPOCH).ok()?.as_secs_f64();
            Some((stem, mtime))
        })
        .collect();
    out.sort_by(|(a, at), (b, bt)| bt.total_cmp(at).then_with(|| a.as_bytes().cmp(b.as_bytes())));
    out
}

pub fn delete(directory: &str, name: &str) -> Result<(), String> {
    std::fs::remove_file(path(directory, name)).map_err(|e| e.to_string())
}

/// Parse, check and lower the file; `settings` is the sketch's settings value the saved fields
/// apply to. An unreadable, unchecked or unlowerable file is an `Err` carrying the diagnostic text.
pub fn load_with_ops(gpu: Option<flow_ir::flow_ir::gpu::Config>, ops: Vec<FlowOp>, path: &str, factories: Option<&[Factory]>, settings_value: &settings::T) -> Result<Loaded, String> {
    let text = editor_core::store::read_text(path)?;
    let catalog = kinds::shared(factories).map_err(|d| rays_diagnostic::to_string(&d))?;
    let options = TextOptions { ops: Some(std::sync::Arc::new(ops)), settings: Some(settings::defaults(settings_value)), ..Default::default() };
    let mut workspace = workspace_doc::of_text(&options, &catalog, &text)
        .map_err(|ds| ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n"))?;
    // the view is the preset's own form, not the document's
    workspace.extra = Vec::new();
    let forms = s::parse(&text).map_err(|d| rays_diagnostic::to_string(&d))?;
    let view = match forms.iter().find(|form| s::head(form) == Some("view")) {
        Some(form) if form.children().len() == 2 => form.child(form.children()[1]),
        Some(_) => return Err("view: expected one value".to_owned()),
        None => {
            let mut builder = flow_syntax::arena::SyntaxBuilder::new();
            let root = builder.push_draft(0, flow_syntax::arena::Draft::Map(Vec::new()), rays_diagnostic::Span { start: 0, finish: 0 }, None);
            builder.freeze().handle(root)
        }
    };
    let doc = lowering::of_workspace(&lowering::Options { factories: Some(factories.map(|f| f.to_vec()).unwrap_or_else(sop::nodes::factories)), gpu, ..Default::default() }, &workspace).map_err(|d| rays_diagnostic::to_string(&d))?;
    document::validate(&doc)?;
    Ok(Loaded { doc, view })
}

pub fn load(gpu: Option<flow_ir::flow_ir::gpu::Config>, path: &str, factories: Option<&[Factory]>, settings_value: &settings::T) -> Result<Loaded, String> {
    load_with_ops(gpu, flow_sop::operators::all().to_vec(), path, factories, settings_value)
}
