mod metal_raw;
pub(crate) mod metal_raw_types;

pub mod metal;
pub mod descriptors;
pub mod library;
pub mod surface;
pub mod acceleration;
pub mod indirect;
pub mod pipeline;
pub mod metal_argument_reflection_snapshot;
pub mod metal_format;
pub mod metal_gen;
pub mod reflection;
pub use acceleration::*;
pub use descriptors::*;
pub use indirect::*;
pub use library::*;
pub use pipeline::*;
pub use surface::*;

pub use metal::*;
