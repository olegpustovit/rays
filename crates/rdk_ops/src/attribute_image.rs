//! Image channel selector for attribute-from-image generators.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Channel {
    Red,
    Green,
    Blue,
    Alpha,
    Luminance,
}
