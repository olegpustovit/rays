//! Output attribute names of the curvature analysis; `None` leaves that measure unwritten.

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Outputs {
    pub mean: Option<String>,
    pub gaussian: Option<String>,
    pub minimum: Option<String>,
    pub maximum: Option<String>,
    pub curvedness: Option<String>,
    pub shape_index: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_writes_nothing() {
        let outputs = Outputs::default();
        assert!(outputs.mean.is_none() && outputs.shape_index.is_none());
    }
}
