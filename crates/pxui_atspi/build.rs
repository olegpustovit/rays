//! Compiles the libdbus-1 shim and links libdbus-1 on Linux. Nothing on other targets.
// ponytail: headers from the Debian/Ubuntu multiarch layout; use pkg-config if another layout matters.
fn main() {
    println!("cargo:rerun-if-changed=src/shim.c");
    if std::env::var("CARGO_CFG_TARGET_OS").as_deref() != Ok("linux") {
        return;
    }
    let arch = std::env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_default();
    cc::Build::new()
        .file("src/shim.c")
        .include("/usr/include/dbus-1.0")
        .include(format!("/usr/lib/{arch}-linux-gnu/dbus-1.0/include"))
        .warnings(true)
        .compile("pxui_atspi_shim");
    println!("cargo:rustc-link-lib=dbus-1");
}
