//! Point replicate: the shared error text of the generator (the algorithm itself is a Lisp node).

/// The `Error` payload of `Rdk_attrib.Point_replicate.point_replicate`.
pub fn error(message: &str) -> String {
    format!("Rdk_attrib.Point_replicate.point_replicate: {message}")
}

#[cfg(test)]
mod tests {
    #[test]
    fn error_text_carries_the_operation_prefix() {
        assert_eq!(
            super::error("count must be non-negative"),
            "Rdk_attrib.Point_replicate.point_replicate: count must be non-negative"
        );
    }
}
