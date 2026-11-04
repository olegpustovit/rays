#[derive(Clone, Debug, PartialEq, Eq)]
pub struct T {
    operation: String,
    code: String,
    message: String,
    hints: Vec<String>,
}

pub(crate) fn internal(operation: &str, code: &str, message: String) -> T {
    T {
        operation: operation.to_owned(),
        code: code.to_owned(),
        message,
        hints: Vec::new(),
    }
}

pub fn make(
    operation: String,
    code: String,
    message: String,
    hints: Option<Vec<String>>,
) -> Result<T, T> {
    if operation.trim().is_empty() {
        return Err(internal(
            "Rdk.Error.make",
            "invalid_argument",
            "Rdk.Error.make: empty operation".to_owned(),
        ));
    }
    if code.trim().is_empty() {
        return Err(internal(
            "Rdk.Error.make",
            "invalid_argument",
            "Rdk.Error.make: empty code".to_owned(),
        ));
    }
    Ok(T {
        operation,
        code,
        message,
        hints: hints.unwrap_or_default(),
    })
}

pub fn operation(value: &T) -> &str {
    &value.operation
}

pub fn code(value: &T) -> &str {
    &value.code
}

pub fn message(value: &T) -> &str {
    &value.message
}

pub fn hints(value: &T) -> &[String] {
    &value.hints
}

pub fn to_string(value: &T) -> String {
    let base = format!("{} [{}]: {}", value.operation, value.code, value.message);
    if value.hints.is_empty() {
        base
    } else {
        format!("{base} ({})", value.hints.join("; "))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn structured_error_validation_and_formatting() {
        let error = make(
            "mesh".to_owned(),
            "invalid_topology".to_owned(),
            "bad offsets".to_owned(),
            Some(vec!["rebuild topology".to_owned()]),
        )
        .unwrap();
        assert_eq!(
            to_string(&error),
            "mesh [invalid_topology]: bad offsets (rebuild topology)"
        );
        let invalid = make(String::new(), "bad".to_owned(), String::new(), None).unwrap_err();
        assert_eq!(code(&invalid), "invalid_argument");
        assert_eq!(message(&invalid), "Rdk.Error.make: empty operation");
    }
}
