//! The primitive graph as point/edge adjacency: one edge per two-sided topology edge (same edge
//! ids), listed per primitive in vertex order.

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Dual {
    pub offsets: Vec<i64>,
    pub edges: Vec<i64>,
    pub edge_a: Vec<i64>,
    pub edge_b: Vec<i64>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dual_is_plain_data() {
        let dual = Dual { offsets: vec![0, 1], edges: vec![0], edge_a: vec![0], edge_b: vec![1] };
        assert_eq!(dual.clone(), dual);
    }
}
