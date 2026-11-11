//! Compiles the Wayland shim and the checked-in wayland-scanner protocol code (no scanner at build time) and
//! links libwayland-client and libxkbcommon. Nothing on other targets.
//! The protocol files come from wayland-protocols 1.45 (`wayland-scanner client-header|private-code <xml>`; only
//! tablet-v2's code, which cursor-shape-v1 references), with trailing blank lines stripped.
// ponytail: system headers and libraries from the default search paths (Debian/Ubuntu layout); use pkg-config if another layout matters.
fn main() {
    println!("cargo:rerun-if-changed=src/shim.c");
    println!("cargo:rerun-if-changed=protocol");
    if std::env::var("CARGO_CFG_TARGET_OS").as_deref() != Ok("linux") {
        return;
    }
    let protocols = ["xdg-shell", "xdg-decoration-unstable-v1", "viewporter", "fractional-scale-v1", "cursor-shape-v1", "tablet-v2", "text-input-unstable-v3", "relative-pointer-unstable-v1", "pointer-constraints-unstable-v1"];
    let mut build = cc::Build::new();
    build.file("src/shim.c").warnings(true);
    for protocol in protocols {
        build.file(format!("protocol/{protocol}.c"));
    }
    build.compile("pxui_wayland_shim");
    println!("cargo:rustc-link-lib=wayland-client");
    println!("cargo:rustc-link-lib=xkbcommon");
}
