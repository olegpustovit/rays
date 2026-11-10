#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub finish: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    ErrorSeverity,
    Warning,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Position {
    pub line: u32,
    pub col: u32,
}

const _: () = assert!(std::mem::size_of::<Span>() == 8);
const _: () = assert!(std::mem::size_of::<Position>() == 8);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Trace {
    pub node_id: i64,
    pub label: String,
    pub operation: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub code: String,
    pub severity: Severity,
    pub position: Option<Position>,
    pub message: String,
    pub span: Option<Span>,
    /// Original Text bytes; decode only at a display boundary.
    pub cause: Option<Vec<u8>>,
    pub hints: Vec<String>,
    pub trace: Vec<Trace>,
    pub signature: Option<String>,
    pub suggest: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error(pub Diagnostic);

/// Leave one byte for the one-based position at end of input.
pub const MAX_SOURCE_BYTES: usize = u32::MAX as usize - 1;

pub fn check_source_len(bytes: usize) -> Result<(), Diagnostic> {
    if bytes <= MAX_SOURCE_BYTES { Ok(()) } else {
        Err(error("E_SOURCE_SIZE".into(), format!("Source exceeds {MAX_SOURCE_BYTES} bytes."), None, None, None, Vec::new(), Vec::new()))
    }
}

pub fn position_of_offset(source: &str, offset: impl Into<i64>) -> Position {
    let end = offset.into().clamp(0, source.len() as i64) as usize;
    let (mut line, mut col) = (1_u32, 1_u32);
    for &byte in source.as_bytes().iter().take(end) {
        if byte == b'\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    Position { line, col }
}

pub fn error(
    code: String,
    message: String,
    span: Option<Span>,
    position: Option<Position>,
    cause: Option<Vec<u8>>,
    hints: Vec<String>,
    trace: Vec<Trace>,
) -> Diagnostic {
    Diagnostic {
        code,
        severity: Severity::ErrorSeverity,
        position,
        message,
        span,
        cause,
        hints,
        trace,
        signature: None,
        suggest: Vec::new(),
    }
}

pub fn warning(span: Option<Span>, code: String, message: String) -> Diagnostic {
    Diagnostic {
        code,
        severity: Severity::Warning,
        position: None,
        message,
        span,
        cause: None,
        hints: Vec::new(),
        trace: Vec::new(),
        signature: None,
        suggest: Vec::new(),
    }
}

pub fn prepend_trace(frame: Trace, mut value: Diagnostic) -> Diagnostic {
    value.trace.insert(0, frame);
    value
}

pub fn to_string(value: &Diagnostic) -> String {
    match value.position {
        None => format!("{}: {}", value.code, value.message),
        Some(position) => format!(
            "{} at {}:{}: {}",
            value.code, position.line, position.col, value.message
        ),
    }
}

pub fn error_to_string(value: &Diagnostic) -> String {
    let mut output = String::new();
    for (index, frame) in value.trace.iter().enumerate() {
        if index > 0 {
            output.push('/');
        }
        output.push_str(&frame.label);
    }
    if !value.trace.is_empty() {
        output.push_str(": ");
    }
    output.push('[');
    output.push_str(&value.code);
    output.push_str("] ");
    output.push_str(&value.message);
    if let Some(cause) = &value.cause {
        output.push_str("\n  cause: ");
        output.push_str(&String::from_utf8_lossy(cause));
    }
    for hint in &value.hints {
        output.push_str("\n  hint: ");
        output.push_str(hint);
    }
    output
}

pub fn report(file: &str, source: &str, value: &Diagnostic) -> String {
    let line_of = |offset: i64| {
        let end = offset.clamp(0, source.len() as i64) as usize;
        let (mut line, mut start) = (1, 0);
        for (index, &byte) in source.as_bytes().iter().take(end).enumerate() {
            if byte == b'\n' {
                line += 1;
                start = index + 1;
            }
        }
        (line, start as i64)
    };
    let (line, a, b) = match (value.span.as_ref(), value.position) {
        (Some(span), _) => {
            let (line, first) = line_of(i64::from(span.start));
            let eol = source.as_bytes()[first as usize..]
                .iter()
                .position(|&byte| byte == b'\n')
                .map_or(source.len() as i64, |offset| (first as usize + offset) as i64);
            (
                line,
                i64::from(span.start) - first,
                i64::from(span.finish).max(i64::from(span.start) + 1).min(eol) - first,
            )
        }
        (None, Some(position)) => {
            let col = i64::from(position.col.saturating_sub(1));
            (i64::from(position.line), col, col)
        }
        (None, None) => (1, 0, 0),
    };
    format!(
        "File \"{}\", line {}, characters {}-{}:\n{} [{}]: {}",
        file,
        line,
        a,
        b,
        match value.severity {
            Severity::ErrorSeverity => "Error",
            Severity::Warning => "Warning",
        },
        value.code,
        value.message
    )
}

#[cfg(test)]
mod tests {

    #[test]
    fn compact_positions_reject_oversized_sources_without_allocating_them() {
        assert_eq!(std::mem::size_of::<Span>(), 8);
        assert_eq!(std::mem::size_of::<Position>(), 8);
        assert!(check_source_len(MAX_SOURCE_BYTES).is_ok());
        assert_eq!(check_source_len(MAX_SOURCE_BYTES + 1).unwrap_err().code, "E_SOURCE_SIZE");
        assert_eq!(check_source_len(usize::MAX).unwrap_err().code, "E_SOURCE_SIZE");
    }
    use super::*;

    #[test]
    fn diagnostic_behavior() {
        let diagnostic = error(
            "E_TEST".into(),
            "failed".into(),
            Some(Span { start: 2, finish: 5 }),
            Some(Position { line: 2, col: 3 }),
            Some("backend".into()),
            vec!["retry".into()],
            vec![Trace {
                node_id: 7,
                label: "image".into(),
                operation: "image/map".into(),
            }],
        );
        assert_eq!(to_string(&diagnostic), "E_TEST at 2:3: failed");
        assert_eq!(
            error_to_string(&diagnostic),
            "image: [E_TEST] failed\n  cause: backend\n  hint: retry"
        );
        assert_eq!(
            prepend_trace(
                Trace {
                    node_id: 8,
                    label: "parent".into(),
                    operation: "scene/root".into(),
                },
                diagnostic.clone(),
            )
            .trace,
            vec![
                Trace {
                    node_id: 8,
                    label: "parent".into(),
                    operation: "scene/root".into(),
                },
                Trace {
                    node_id: 7,
                    label: "image".into(),
                    operation: "image/map".into(),
                },
            ]
        );
        assert_eq!(
            report("x.rays", "a\nbcde", &diagnostic),
            "File \"x.rays\", line 2, characters 0-3:\nError [E_TEST]: failed"
        );
        assert!(matches!(
            warning(None, "W_TEST".into(), "warning".into()),
            Diagnostic {
                severity: Severity::Warning,
                cause: None,
                hints,
                trace,
                ..
            } if hints.is_empty() && trace.is_empty()
        ));
        let raised = Result::<(), _>::Err(Error(diagnostic.clone()));
        let raised = match raised {
            Err(Error(value)) => value,
            Ok(()) => unreachable!(),
        };
        assert_eq!(raised, diagnostic);
        assert_eq!(position_of_offset("a\nbcde", 2), Position { line: 2, col: 1 });
    }

    #[test]
    fn offsets_and_reports_use_clamped_byte_positions() {
        let source = "é\nx";
        assert_eq!(position_of_offset(source, -1), Position { line: 1, col: 1 });
        assert_eq!(position_of_offset(source, 1), Position { line: 1, col: 2 });
        assert_eq!(position_of_offset(source, 99), Position { line: 2, col: 2 });

        let diagnostic = warning(
            Some(Span { start: 1, finish: 99 }),
            "W_TEST".into(),
            "warning".into(),
        );
        assert_eq!(
            report("x.rays", source, &diagnostic),
            "File \"x.rays\", line 1, characters 1-2:\nWarning [W_TEST]: warning"
        );
        let positioned = error(
            "E_TEST".into(),
            "failed".into(),
            None,
            Some(Position { line: 2, col: 4 }),
            None,
            Vec::new(),
            Vec::new(),
        );
        assert_eq!(
            report("x.rays", source, &positioned),
            "File \"x.rays\", line 2, characters 3-3:\nError [E_TEST]: failed"
        );
    }
}

