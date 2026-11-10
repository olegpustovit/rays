//! `check` / `fmt` live in `web_export`; the native CLI supplies its image decoder for `web`.
pub use web_export::{check, check_fmt, fmt, EMPTY, Fail};

pub fn web(path: &str, dir: &str) -> Result<(), Fail> {
    web_export::web(path, dir, &|path| {
        let image = sdl3::image::load_file(path).map_err(|error| error.to_string())?;
        Ok((i64::from(image.width), i64::from(image.height), image.pixels))
    })
}
