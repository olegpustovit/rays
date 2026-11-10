//! The Linux AT-SPI2 bridge: serves a `pxui::a11y::A11yTree` to screen readers (Orca) on the accessibility
//! bus and hands their requests back as `A11yAction`s. Raw libdbus-1 through src/shim.c; no threads: the
//! host polls `Bridge::fd` and calls `pump`. The role/state tables (`map`) build everywhere; the bridge is
//! Linux only.
pub mod map;
#[cfg(target_os = "linux")]
pub mod wire;
#[cfg(target_os = "linux")]
mod bridge;
#[cfg(target_os = "linux")]
pub use bridge::{Bridge, Error, NODE_PREFIX, NULL_PATH, ROOT_PATH, a11y_bus_address};
