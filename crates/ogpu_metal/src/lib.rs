//! The macOS Metal driver owns native device, resource, queue, surface and encoder modules.
#![cfg(target_os = "macos")]
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
