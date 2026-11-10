//! Static Web export modules and owned tests.

pub mod check_cli;
pub mod web_export;
pub mod web_runtime;
pub use check_cli::{check, check_fmt, fmt, web, EMPTY, Fail};
#[cfg(test)]
mod test_web_export;
