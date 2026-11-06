pub fn valid_name(name: &str) -> bool {
    let mut bytes = name.bytes();
    matches!(bytes.next(), Some(b'a'..=b'z'))
        && bytes.all(|byte| matches!(byte, b'a'..=b'z' | b'0'..=b'9' | b'_'))
}

pub fn reserved(name: &str) -> bool {
    matches!(name, "t" | "pi" | "nil" | "true" | "false" | "let*" | "values" | "graph" | "defgraph" | "+" | "-" | "*" | "/" | "pow" | "min" | "max" | "sin" | "cos" | "abs" | "floor" | "sqrt")
}
#[cfg(test)]
mod tests {
    use super::{reserved, valid_name};

    #[test]
    fn accepts_only_lowercase_unqualified_names() {
        assert!(valid_name("uv_sphere"));
        for name in ["", "9a", "A", "a-b", "a.b", "a/b"] {
            assert!(!valid_name(name));
        }
        assert!(reserved("let*"));
        assert!(!reserved("uv_sphere"));
    }
}
