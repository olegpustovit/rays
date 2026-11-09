//! `check` / `fmt` / `web` live in `web_export::check_cli` so e2e skips the editor build.
pub use web_export::{check, check_fmt, fmt, web, EMPTY, Fail};
