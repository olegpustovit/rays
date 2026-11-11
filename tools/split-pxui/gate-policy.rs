//! PXUI dependency policy: the rules about this repository's own crates. The engine in main.rs is the
//! one Rays uses (tools/gate there); only this file differs.
use std::collections::{HashMap, HashSet};

/// Helper leaves any product crate may name.
pub const HELPER_LEAVES: [&str; 1] = ["cfmt"];
/// Leaves a crate may name directly when its allowed dependencies reach them.
pub const OCAML_LEAVES: [&str; 1] = ["input"];

/// Direct production edges. Sys crates own their native ABI bridges.
pub fn direct_allowlist() -> HashMap<&'static str, &'static [&'static str]> {
    HashMap::from([
        ("cfmt", &[][..]),
        ("native_layer_token_sys", &[][..]),
        ("mem_sys", &[][..]),
        ("input", &[][..]),
        ("sdl3_sys", &["native_layer_token_sys"][..]),
        ("sdl3", &["sdl3_sys", "native_layer_token_sys"][..]),
        ("metal_sys", &["ogpu_core"][..]),
        ("vulkan_sys", &["ogpu_core"][..]),
        ("ogpu_core", &["native_layer_token_sys"][..]),
        ("ogpu_mock", &["ogpu_core"][..]),
        ("ogpu_metal", &["ogpu_core", "metal_sys", "native_layer_token_sys"][..]),
        ("ogpu_vulkan", &["ogpu_core", "vulkan_sys", "native_layer_token_sys"][..]),
        // Plain data in (input), a draw list out.
        ("pxui", &["input", "cfmt", "pxui_text"][..]),
        ("pxui_text", &[][..]),
        // The UI painter: the portable GPU API and PXUI's draw list.
        ("pxui_render", &["ogpu_core", "pxui"][..]),
        // The window hosts are leaves: SDL, the input types and the native layer token.
        ("pxui_sdl", &["sdl3", "input", "native_layer_token_sys"][..]),
        ("pxui_wayland", &["input", "native_layer_token_sys"][..]),
        // The Linux AT-SPI2 bridge: libdbus-1 through its own C shim, over pxui's accessibility tree.
        ("pxui_atspi", &["pxui"][..]),
        // The kit gallery app: the engine, the painter and the platform's window host and GPU backend.
        ("pxui_gallery", &["pxui", "input", "mem_sys", "ogpu_core", "pxui_render", "ogpu_metal", "ogpu_vulkan", "pxui_sdl", "pxui_wayland", "pxui_atspi"][..]),
    ])
}

pub fn local_workspace_only() -> HashSet<&'static str> {
    HashSet::new()
}

pub fn forbidden_reaches() -> HashMap<String, HashSet<String>> {
    let mut rules: HashMap<String, HashSet<String>> = HashMap::new();
    let mut forbid = |library: &str, names: &[&str]| {
        rules.entry(library.to_owned()).or_default().extend(names.iter().map(|name| (*name).to_owned()));
    };
    // GPU backends know nothing about windows, the UI or each other.
    for library in ["ogpu_core", "ogpu_mock", "metal_sys", "vulkan_sys", "ogpu_metal", "ogpu_vulkan"] {
        forbid(library, &["sdl3", "sdl3_sys", "pxui", "pxui_text", "pxui_render", "pxui_sdl", "pxui_wayland", "pxui_atspi"]);
    }
    forbid("pxui_sdl", &["ogpu_core"]);
    forbid("pxui_wayland", &["sdl3", "pxui_sdl", "ogpu_core", "pxui", "pxui_render"]);
    forbid("pxui_atspi", &["sdl3", "pxui_sdl", "pxui_wayland", "ogpu_core", "pxui_render"]);
    forbid("pxui", &["sdl3", "ogpu_core"]);
    forbid("pxui_render", &["sdl3", "metal_sys", "vulkan_sys", "ogpu_metal", "ogpu_vulkan", "ogpu_mock", "mem_sys"]);
    rules
}

/// External crates a package may name (package, dependency); `cc` is allowed everywhere as a build dependency.
pub const EXTERNAL_CRATES: &[(&str, &str)] = &[("pxui_text", "cosmic-text")];

/// Direct edges that are forbidden outright (from, to, reason).
pub const NO_DIRECT: &[(&str, &str, &str)] = &[];

/// Crates whose tests (dev dependencies) may not reach the window host or SDL either.
pub const TESTS_INCLUDED: &[&str] = &["pxui", "pxui_text", "pxui_render"];
pub const TESTS_INCLUDED_FORBIDDEN: &[&str] = &["sdl3", "sdl3_sys", "pxui_sdl"];

/// No registry checks here.
pub const DOCS_CHECKS: &[(&str, &str, Option<&str>)] = &[];

/// Crates besides `*_sys` that may use `unsafe`.
pub const UNSAFE_CRATES: &[&str] = &["pxui_wayland", "pxui_atspi"]; // own their C shims and the extern blocks over them, like *_sys crates

/// Files besides those of `UNSAFE_CRATES` that may use `unsafe`, each with its reason.
pub const UNSAFE_FILES: &[(&str, &str)] = &[
    ("crates/pxui/src/arena.rs", "frame bump arena: canvas closures are written into raw bytes and called through type-erased records"),
    ("crates/pxui_render/src/lib.rs", "writes instance bytes into the host-mapped upload ring and views instance structs as bytes"),
];
