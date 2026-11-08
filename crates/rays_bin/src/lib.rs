//! rays: the command line (port of bin/rays.ml). `check` and `fmt` are library functions so the
//! e2e goldens (fixtures/e2e) run them in process.
use editor_document::{lowering, workspace_doc as doc};
use flow_syntax::syntax as s;
use rays_diagnostic::{Diagnostic, Severity};

pub type Fail = String;

fn catalog() -> Result<std::sync::Arc<flow_check::catalog::Catalog>, Fail> {
    editor_document::kinds::shared(None).map_err(|d| format!("rays: catalog: {}", rays_diagnostic::to_string(&d)))
}

fn lower_error(message: String) -> Diagnostic {
    rays_diagnostic::error("E_LOWER".to_string(), message, None, None, None, Vec::new(), Vec::new())
}

fn check_root(lowered: &flow_sop::lower::T, instance: usize, fallback: Diagnostic) -> Result<(), Diagnostic> {
    let Some(graph) = lowered.graphs.iter().find(|g| g.instance == instance) else { return Err(fallback) };
    let Some(root) = graph.root else { return Ok(()) };
    let resolved = flow_sop::value_lane::resolve(None, None, &flow_sop::value_lane::create(None), 0., &graph.network)?;
    let node = sop::edit_graph::compile_node(&resolved.geometry, root).map_err(lower_error)?;
    let mut session = sop::session::create(16, 1 << 28).expect("session");
    let context = sop::context::create(Default::default()).expect("context");
    let result = sop::session::cook(&mut session, None, &context, &node).map(|_| ());
    sop::session::close(&mut session);
    result
}

fn check_meshes(lowered: &flow_sop::lower::T) -> Result<(), Diagnostic> {
    for node in &lowered.plan.nodes {
        if (node.kind == "sop/mesh" || node.kind == "sop/mesh_planes") && !flow_eval::is_live(&flow_eval::Value::Record(node.args.clone())) {
            if let Err(d) = flow_sop::topology_kernel::mesh(&node.args) {
                // On failure the ordinary cook applies authored error mappers and leaves an unused,
                // unprotected constructor uncooked.
                check_root(lowered, node.inst as usize, d)?;
            }
        }
    }
    for (index, instance) in lowered.plan.instances.iter().enumerate() {
        if let flow_eval::Value::Residual(id) = &instance.result {
            let geometry = flow_eval::residual::residual_view(*id).map(|v| *v.term.ty() == flow_syntax::ty::geometry()).unwrap_or(false);
            if geometry && !(flow_eval::frame_dependent(&instance.result) || flow_eval::state_dependent(&instance.result)) {
                check_root(lowered, index, lower_error("Missing static geometry graph".to_string()))?;
            }
        }
    }
    Ok(())
}

/// The checked document and every diagnostic to print; `allow` is `^:allow-warnings`.
pub fn check_text(imports: Vec<(String, String)>, text: &str, lower: bool) -> Result<(Option<doc::T>, Vec<Diagnostic>, bool), Fail> {
    let allow = match s::parse(text) {
        Ok(forms) => forms.iter().any(|f| s::head(f) == Some("workspace") && f.meta().iter().any(|m| m == "allow-warnings")),
        Err(_) => false,
    };
    let catalog = catalog()?;
    let options = doc::TextOptions { imports, ..Default::default() };
    Ok(match doc::check_text(&options, &catalog, text) {
        Err(ds) => (None, ds, allow),
        Ok((workspace, warnings)) if !lower => (Some(workspace), warnings, allow),
        Ok((workspace, mut warnings)) => {
            let options = lowering::Options { defer: Some(false), ..Default::default() };
            match lowering::of_workspace(&options, &workspace) {
                Ok(lowered) => match check_meshes(&lowered.workspace.lowered) {
                    Ok(()) => (Some(workspace), warnings, allow),
                    Err(d) => {
                        warnings.push(d);
                        (None, warnings, allow)
                    }
                },
                Err(d) => {
                    warnings.push(d);
                    (None, warnings, allow)
                }
            }
        }
    })
}

/// The text, the document when the file reads, the printed diagnostics and whether it passes.
pub fn check_file(path: &str, lower: bool) -> Result<(String, Option<doc::T>, String, bool), Fail> {
    let text = std::fs::read(path).map(|b| String::from_utf8_lossy(&b).into_owned()).map_err(|e| format!("rays: {e}"))?;
    let (document, ds, allow) = match rays_editor::source_file::read_imports(path, &text) {
        Ok(imports) => check_text(imports, &text, lower)?,
        Err(ds) => (None, ds, false),
    };
    let mut printed = String::new();
    for d in &ds {
        printed.push_str(&rays_diagnostic::report(path, &text, d));
        printed.push('\n');
    }
    let failed = document.is_none() || ds.iter().any(|d| d.severity == Severity::ErrorSeverity || !allow);
    Ok((text, document, printed, !failed))
}

/// `rays fmt FILE`: (stdout, stderr, exit code).
pub fn fmt(path: &str) -> (String, String, i32) {
    match check_file(path, true) {
        Ok((_, Some(document), printed, _)) => match doc::to_text(&document) {
            Ok(text) => (text, printed, 0),
            Err(error) => (String::new(), format!("{printed}{}\n", rays_diagnostic::to_string(&error)), 1),
        },
        Ok((_, None, printed, _)) => (String::new(), printed, 1),
        Err(m) => (String::new(), format!("{m}\n"), 1),
    }
}

/// `rays check FILE`: (stderr, exit code).
pub fn check(path: &str) -> (String, i32) {
    match check_file(path, true) {
        Ok((_, _, printed, passed)) => (printed, if passed { 0 } else { 1 }),
        Err(m) => (format!("{m}\n"), 1),
    }
}

/// The workspace `rays` opens with no file (also the editor's per-frame allocation guard).
pub const EMPTY: &str = include_str!("empty.rays");

/// `rays web FILE -o DIR`.
pub fn web(path: &str, dir: &str) -> Result<(), Fail> {
    let (text, document, printed, passed) = check_file(path, true)?;
    eprint!("{printed}");
    let document = match document {
        Some(d) if passed => d,
        _ => return Err(String::new()),
    };
    let files = web_export::web_export::lower(&document).and_then(|l| web_export::web_export::files(&document, &l));
    match files {
        Err(d) => Err(rays_diagnostic::report(path, &text, &d)),
        Ok(files) => {
            for (name, contents) in files {
                let file = std::path::Path::new(dir).join(name);
                if let Some(p) = file.parent() {
                    std::fs::create_dir_all(p).map_err(|e| format!("rays: web: {e}"))?;
                }
                std::fs::write(&file, contents).map_err(|e| format!("rays: web: {e}"))?;
            }
            Ok(())
        }
    }
}

/// `check` and `fmt` of one file from a single check: (stderr, check code, fmt stdout).
pub fn check_fmt(path: &str, lower: bool) -> (String, i32, String) {
    match check_file(path, lower) {
        Ok((_, document, printed, passed)) => match document.map(|d| doc::to_text(&d)).transpose() {
            Ok(text) => (printed, if passed { 0 } else { 1 }, text.unwrap_or_default()),
            Err(error) => (format!("{printed}{}\n", rays_diagnostic::to_string(&error)), 1, String::new()),
        },
        Err(m) => (format!("{m}\n"), 1, String::new()),
    }
}
