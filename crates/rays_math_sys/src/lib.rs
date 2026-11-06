#[cfg(not(windows))]
#[link(name = "m")]
unsafe extern "C" {
    #[link_name = "fma"]
    fn system_fma(a: f64, b: f64, c: f64) -> f64;
}

#[cfg(windows)]
#[link(name = "ucrt")]
unsafe extern "C" {
    #[link_name = "fma"]
    fn system_fma(a: f64, b: f64, c: f64) -> f64;
}

pub fn fma(a: f64, b: f64, c: f64) -> f64 {
    unsafe { system_fma(a, b, c) }
}
