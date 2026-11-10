//! `check` / `fmt` live in `web_export`; the native CLI supplies its image decoder for `web`.
pub use web_export::{check, check_fmt, fmt, EMPTY, Fail};

pub fn web(path: &str, dir: &str) -> Result<(), Fail> {
    web_export::web(path, dir, &|path| {
        let image = sdl3::image::load_file(path).map_err(|error| error.to_string())?;
        Ok((i64::from(image.width), i64::from(image.height), image.pixels))
    })
}

pub mod documentation;

/// One JSON object per original diagnostic, preserving spans, hints and evaluator traces.
pub fn check_json(path: &str) -> (String, i32) {
    let (text, diagnostics, passed) = match web_export::check_cli::check_diagnostics(path, true) {
        Ok((text, _, ds, passed)) => (text, ds, passed),
        Err(message) => (String::new(), vec![rays_diagnostic::error("E_IO".into(), message, None, None, None, Vec::new(), Vec::new())], false),
    };
    let output = diagnostics.iter().map(|d| diagnostic_json(path, &text, d) + "\n").collect();
    (output, if passed { 0 } else { 1 })
}

pub fn diagnostic_json(path: &str, source: &str, d: &rays_diagnostic::Diagnostic) -> String {
    use web_export::check_cli::json_string as q;
    let position = d.span.as_ref().map(|s| rays_diagnostic::position_of_offset(source, s.start)).or(d.position);
    let end = d.span.as_ref().map(|s| rays_diagnostic::position_of_offset(source, s.finish));
    let number = |n: Option<u32>| n.map(|n| n.to_string()).unwrap_or_else(|| "null".into());
    let hints = d.hints.iter().map(|h| q(h)).collect::<Vec<_>>().join(",");
    let trace = d.trace.iter().map(|t| format!("{{\"node_id\":{},\"label\":{},\"operation\":{}}}", t.node_id, q(&t.label), q(&t.operation))).collect::<Vec<_>>().join(",");
    let span = d.span.as_ref().map(|s| format!("{{\"start\":{},\"finish\":{}}}", s.start, s.finish)).unwrap_or_else(|| "null".into());
    let signature = d.signature.as_ref().map(|s| q(s)).unwrap_or_else(|| "null".into());
    let suggest = d.suggest.iter().take(3).map(|s| q(s)).collect::<Vec<_>>().join(",");
    format!("{{\"file\":{},\"line\":{},\"col\":{},\"end_line\":{},\"end_col\":{},\"severity\":{},\"code\":{},\"message\":{},\"span\":{},\"cause\":{},\"hints\":[{}],\"trace\":[{}],\"suggest\":[{}],\"signature\":{}}}", q(path), number(position.map(|p|p.line)), number(position.map(|p|p.col)), number(end.map(|p|p.line)), number(end.map(|p|p.col)), q(match d.severity { rays_diagnostic::Severity::ErrorSeverity => "error", rays_diagnostic::Severity::Warning => "warning" }), q(&d.code), q(&d.message), span, d.cause.as_ref().map(|b| web_export::check_cli::json_bytes(b)).unwrap_or_else(|| "null".into()), hints, trace, suggest, signature)
}
