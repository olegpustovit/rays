//! OGPU's Metal implementation: lib/ogpu_metal (the driver record) and lib/ogpu_metal_native (the
//! device, resource, queue, surface and encoder modules under it) in one crate, one file per OCaml
//! module. It compiles everywhere; on a platform without Metal the native backend refuses with a typed
//! error, so `create_device` reports `NoAdapter`/`Unsupported` rather than the build failing.
pub mod device;
pub mod buffer;
pub mod texture;
pub mod sampler;
pub mod library;
pub mod pipeline;
pub mod queue;
pub mod present_source;
pub mod presentation;
pub mod surface;
pub mod acceleration;
pub mod command_control;
pub mod compute_encoder;
pub mod blit_encoder;
pub mod render_encoder;
pub mod backend;
pub mod impl_;

#[doc(hidden)]
pub mod native;
