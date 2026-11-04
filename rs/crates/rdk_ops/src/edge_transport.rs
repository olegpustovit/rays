//! Option types of Edge Transport (the algorithm itself lives with the graph kernels).
use rdk_core::group;

#[derive(Clone)]
pub enum Roots {
    TransportFirstPoint,
    TransportLastPoint,
    TransportRootGroup(group::T),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operation {
    Transport,
    TransportFromRoot,
    TransportTotal,
    TransportMaximum,
    TransportMinimum,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RootValue {
    TransportRootZero,
    TransportRootHold,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Split {
    TransportCopy,
    TransportSplit,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Normalization {
    TransportNoNormalization,
    TransportNormalizeComponents,
    TransportNormalizeGlobal,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Direction {
    TransportForward,
    TransportBackward,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Merge {
    TransportMergeAdd,
    TransportMergeMaximum,
    TransportMergeMinimum,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn variants_compare_by_constructor() {
        assert_ne!(Operation::Transport, Operation::TransportTotal);
        assert_eq!(Merge::TransportMergeAdd, Merge::TransportMergeAdd);
        assert!(matches!(Roots::TransportFirstPoint, Roots::TransportFirstPoint));
    }
}
