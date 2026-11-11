//! Dependency policy of this repository. The engine in main.rs is generic; this file is the only
//! per-repository part. Rows tagged `// pxui` belong to the PXUI repository and are deleted from Rays
//! by tools/split-pxui/rays-side.sh; the PXUI gate ships its own policy.rs (tools/split-pxui/gate-policy.rs).
use std::collections::{HashMap, HashSet};

/// Rust-only helper leaves that stand in for OCaml stdlib, printf and int semantics: any product crate may name them.
pub const HELPER_LEAVES: [&str; 1] = ["cfmt"];
/// OCaml leaves a crate may name directly when its allowed dependencies reach them (flow reaches
/// rays_diagnostic and input; lru reaches mem).
// param: OCaml flow depends on it, so anything reaching flow reaches param.
pub const OCAML_LEAVES: [&str; 4] = ["rays_diagnostic", "input", "mem", "param"];

/// Direct production edges in the Rust architecture. Sys crates own their native ABI bridges.
pub fn direct_allowlist() -> HashMap<&'static str, &'static [&'static str]> {
    HashMap::from([
        ("cfmt", &[][..]), // pxui
        ("rays_diagnostic", &[][..]),
        ("native_layer_token_sys", &[][..]), // pxui
        ("mem_sys", &[][..]), // pxui
        ("mem", &["mem_sys"][..]),
        ("lru", &["mem"][..]),
        ("param", &["cfmt"][..]),
        ("input", &[][..]), // pxui
        ("rays_math_sys", &[][..]),
        ("rays_math", &["rays_math_sys"][..]),
        ("scene_command", &["rays_math"][..]),
        ("parallel", &[][..]),
        ("sdl3_sys", &["native_layer_token_sys"][..]), // pxui
        ("metal_sys", &["ogpu_core"][..]), // pxui
        ("vulkan_sys", &["ogpu_core"][..]), // pxui
        ("rdk_core", &["mem", "parallel", "rays_math"][..]),
        ("rdk_geom", &["rdk_core", "rays_math", "parallel"][..]),
        ("rdk_ops", &["rdk_core", "rdk_geom", "rays_math", "parallel"][..]),
        ("rdk_boolean", &["rdk_core", "rdk_geom", "rays_math", "parallel"][..]),
        ("rdk_rays", &["rdk_core", "rdk_geom", "rdk_ops", "rays", "rays_math", "param"][..]),
        ("scene_core", &["rays_math", "lru", "parallel"][..]),
        ("rays", &[
            "input", "rays_math", "lru", "runtime_resources", "runtime", "scene_command", "renderer",
            "parallel", "ogpu_core", "scene_core", "pxui_text",
        ][..]),
        // The window host is a leaf: it may reach only SDL, the input types and the native layer token.
        ("pxui_sdl", &["sdl3", "input", "native_layer_token_sys"][..]), // pxui
        // Its Wayland sibling: libwayland-client and xkbcommon through its own C shim, no other crate.
        ("pxui_wayland", &["input", "native_layer_token_sys"][..]), // pxui
        // The Linux AT-SPI2 bridge: libdbus-1 through its own C shim, over pxui's plain-data accessibility tree.
        ("pxui_atspi", &["pxui"][..]), // pxui
        ("runtime", &["pxui_sdl", "pxui_wayland", "sdl3", "ogpu_core", "input"][..]),
        ("runtime_resources", &["sdl3", "ogpu_core"][..]),
        ("renderer", &["ogpu_core", "scene_command", "runtime", "lru", "rays_math", "runtime_resources", "scene_core"][..]),
        ("flow_syntax", &["param", "rays_diagnostic", "input", "rays_math"][..]),
        ("flow_check", &["flow_syntax", "param", "rays_diagnostic", "input", "rays_math", "lru"][..]),
        ("flow_eval", &["flow_syntax", "flow_check", "param", "rays_diagnostic", "input", "rays_math", "mem_sys"][..]),
        ("flow_graph", &["flow_syntax", "flow_check", "flow_eval", "param"][..]),
        ("flow_ir", &[
            "flow_syntax", "flow_check", "flow_eval", "param", "rays_math", "scene_command", "lru", "parallel",
        ][..]),
        ("flow_shader", &["flow_syntax", "flow_check", "flow_eval", "flow_ir", "ogpu_core", "rays_math"][..]),
        ("flow_gpu", &[
            "flow_syntax", "flow_check", "flow_eval", "flow_ir", "flow_shader", "param", "rays_math", "ogpu_core",
            "renderer", "lru", "mem", "parallel",
        ][..]),
        ("flow_web", &[
            "flow_syntax", "flow_check", "flow_eval", "flow_ir", "param", "rays_math", "scene_command", "parallel",
        ][..]),
        ("sop", &["param", "input", "rays_diagnostic", "rays_math", "rdk_core", "rdk_geom", "rdk_ops", "rdk_boolean", "lru", "mem", "parallel"][..]),
        ("flow_sop", &[
            "flow_syntax", "flow_check", "flow_eval", "flow_ir", "lru", "param", "sop", "rays_diagnostic", "rays_math",
            "parallel", "rdk_core", "rdk_geom", "rdk_ops", "flow_sop_catalog",
        ][..]),
        ("flow_sop_catalog", &["flow_syntax", "flow_check", "flow_eval", "flow_ir", "param", "sop", "rays_math"][..]),
        ("flow_native", &[
            "editor_document", "rays", "rays_math", "input", "flow_syntax", "flow_check", "flow_eval",
            "flow_graph", "flow_ir", "flow_gpu", "flow_sop", "scene_command", "renderer", "lru", "runtime_resources",
            "parallel", "sop", "flow_shader", "ogpu_core", "rdk_core",
        ][..]),
        ("ogpu_core", &["native_layer_token_sys"][..]), // pxui
        ("ogpu_mock", &["ogpu_core"][..]), // pxui
        // Rust merges OCaml's ogpu_metal_native into ogpu_metal.
        ("ogpu_metal", &["ogpu_core", "metal_sys", "native_layer_token_sys"][..]), // pxui
        ("ogpu_vulkan", &["ogpu_core", "vulkan_sys", "native_layer_token_sys"][..]), // pxui
        // PXUI is headed for its own repository: plain data in (input), a draw list out.
        ("pxui", &["input", "cfmt", "pxui_text"][..]), // pxui
        ("pxui_text", &[][..]), // pxui
        // The UI painter: the portable GPU API and PXUI's draw list, so it moves to the PXUI repository unchanged.
        ("pxui_render", &["ogpu_core", "pxui"][..]), // pxui
        ("rays_shell", &["rays", "rays_math", "editor_core", "pxui", "pxui_render", "ogpu_core", "mem"][..]),
        ("rays_graph", &[
            "rays", "rays_math", "flow_syntax", "flow_check", "flow_eval", "flow_graph", "pxui", "editor_core",
        ][..]),
        ("editor_core", &["param", "rays", "rays_math", "flow_syntax", "flow_check", "flow_eval", "runtime_resources"][..]),
        ("web_export", &[
            "flow_syntax", "flow_check", "flow_eval", "flow_ir", "flow_web", "flow_sop", "editor_document", "input",
            "param", "scene_command", "flow_shader", "rays_math", "sop",
        ][..]),
        ("rays_pathtracer", &[
            "rays", "ogpu_core", "renderer", "flow_syntax", "flow_check", "flow_eval", "flow_ir", "flow_gpu", "input",
            "runtime_resources", "rays_math", "flow_shader", "runtime",
        ][..]),
        ("rays_bin", &["editor_document", "rays_editor", "web_export", "flow_syntax", "flow_check", "flow_eval", "flow_sop", "sop", "runtime", "sdl3", "ogpu_core", "ogpu_metal", "ogpu_vulkan"][..]),
    ])
}

pub fn local_workspace_only() -> HashSet<&'static str> {
    HashSet::from([
        "sdl3", "editor_document", "rays_editor",
    ])
}

pub fn forbidden_reaches() -> HashMap<String, HashSet<String>> {
    let mut rules: HashMap<String, HashSet<String>> = HashMap::new();
    let upper = [
        "param", "flow_ir", "flow_web", "flow_shader", "flow_gpu", "flow_graph", "flow_sop",
        "flow_native", "rays", "editor_core", "editor_document", "pxui", "rays_shell", "rays_graph",
        "sop", "rdk_core", "rdk_geom", "rdk_ops", "rdk_boolean", "rdk_rays", "rays_pathtracer",
        "web_export", "rays_editor", "rays_bin", "flow_sop_catalog",
    ];
    let gpu = [
        "sdl3", "metal_sys", "vulkan_sys", "ogpu_core",
        "ogpu_mock", "ogpu_metal", "ogpu_vulkan", "runtime",
        "runtime_resources", "renderer",
    ];
    let mut upper_no_gpu = upper.to_vec();
    upper_no_gpu.extend(["runtime", "runtime_resources", "renderer"]);
    let foundational = [
        "cfmt",
        "rays_diagnostic", "native_layer_token_sys", "mem", "mem_sys",
        "lru", "param", "input", "rays_math", "rays_math_sys", "parallel", "scene_command", "sdl3", "pxui_sdl", "pxui_wayland",
        "metal_sys", "vulkan_sys", "ogpu_core",
        "rdk_core",
    ];
    for library in foundational {
        rules.entry(library.to_owned()).or_default().extend(upper_no_gpu.iter().map(|name| (*name).to_owned()));
    }
    let mut forbid = |library: &str, names: &[&str]| {
        rules.entry(library.to_owned()).or_default().extend(names.iter().map(|name| (*name).to_owned()));
    };
    for library in ["runtime", "renderer", "rays"] {
        forbid(library, &["metal_sys", "ogpu_metal", "vulkan_sys", "ogpu_vulkan"]);
    }
    for library in ["ogpu_metal", "vulkan_sys", "ogpu_vulkan"] {
        forbid(library, &["sdl3", "runtime", "rays", "renderer"]);
    }
    forbid("runtime", &upper);
    forbid("pxui_sdl", &["ogpu_core", "runtime", "renderer"]); // pxui
    forbid("pxui_wayland", &["sdl3", "pxui_sdl", "ogpu_core", "runtime", "renderer", "pxui", "pxui_render"]); // pxui
    forbid("pxui_atspi", &["sdl3", "pxui_sdl", "pxui_wayland", "ogpu_core", "runtime", "renderer", "pxui_render"]); // pxui
    forbid("flow_native", &["pxui", "rays_shell", "rays_graph", "rays_editor"]);
    forbid("flow_ir", &[
        "rays", "pxui", "rays_shell", "rays_graph", "sop", "rdk_core", "rdk_geom", "rdk_ops",
        "rdk_boolean", "rdk_rays", "rays_pathtracer", "rays_editor",
    ]);
    forbid("flow_shader", &[
        "runtime", "renderer", "ogpu_mock", "ogpu_metal", "ogpu_vulkan",
        "rays", "rdk_core", "rdk_geom", "rdk_ops", "rdk_boolean", "rdk_rays", "rays_pathtracer", "flow_sop",
        "sop", "editor_core", "pxui", "rays_shell", "rays_graph", "rays_editor",
    ]);
    forbid("flow_gpu", &[
        "sop", "rdk_core", "rdk_geom", "rdk_ops", "rdk_boolean", "rdk_rays", "rays_pathtracer", "flow_sop",
        "editor_core", "pxui", "rays_shell", "rays_graph", "rays_editor",
    ]);
    forbid("flow_web", &[
        "rays", "sop", "rdk_core", "rdk_geom", "rdk_ops", "rdk_boolean", "rdk_rays", "rays_pathtracer",
        "flow_sop", "flow_gpu", "editor_core", "pxui", "rays_shell", "rays_graph", "rays_editor",
    ]);
    forbid("flow_web", &gpu);
    // G6: the web target needs no SDL, runtime or renderer, directly or transitively.
    forbid("web_export", &["pxui", "rays_shell", "rays_graph", "rays_editor", "rays", "runtime", "runtime_resources", "renderer", "sdl3"]);
    forbid("rays", &[
        "pxui", "rays_shell", "rays_graph", "sop", "rdk_core", "rdk_geom", "rdk_ops", "rdk_boolean",
        "rdk_rays", "rays_pathtracer", "rays_editor",
    ]);
    forbid("rays_math", &["rdk_core", "rdk_geom", "rdk_ops", "rdk_boolean", "rdk_rays", "sop"]);
    forbid("rays_math", &gpu);
    forbid("rdk_core", &["rdk_geom", "rdk_ops", "rdk_boolean", "rays", "rdk_rays", "sop"]);
    forbid("rdk_core", &gpu);
    forbid("rdk_geom", &["rdk_ops", "rdk_boolean", "rays", "rdk_rays", "sop"]);
    forbid("rdk_geom", &gpu);
    forbid("rdk_ops", &["rdk_boolean", "rays", "rdk_rays", "sop"]);
    forbid("rdk_ops", &gpu);
    forbid("rdk_boolean", &["rdk_ops", "rays", "rdk_rays", "sop"]);
    forbid("rdk_boolean", &gpu);
    forbid("sop", &["rays", "rdk_rays", "pxui", "rays_shell", "rays_graph", "rays_editor"]);
    forbid("sop", &gpu);
    forbid("editor_core", &["pxui", "rays_shell", "rays_graph", "rays_editor", "sop"]);
    forbid("editor_document", &["pxui", "rays_shell", "rays_graph", "rays_editor"]);
    forbid("pxui", &["rays", "param", "mem", "lru", "runtime", "runtime_resources", "renderer", "sdl3", "editor_core", "rays_shell", "sop", "rays_graph", "rays_editor", "scene_command", "rays_math", "ogpu_core"]); // pxui
    forbid("rays_shell", &["sop", "rays_graph", "rays_editor"]);
    forbid("rays_graph", &["sop", "rays_shell", "rays_editor"]);
    forbid("pxui_render", &upper_no_gpu.iter().copied().filter(|name| *name != "pxui").collect::<Vec<_>>()); // pxui
    forbid("pxui_render", &["scene_command", "scene_core", "rays_math", "mem", "mem_sys", "lru", "sdl3", "metal_sys", "vulkan_sys", "ogpu_metal", "ogpu_vulkan", "ogpu_mock"]); // pxui
    forbid("rays_pathtracer", &[
        "metal_sys", "ogpu_metal", "ogpu_mock", "rdk_core", "sop", "flow_sop", "editor_core",
        "editor_document", "pxui", "rays_shell", "rays_graph", "rays_editor",
    ]);
    forbid("rdk_rays", &[
        "metal_sys", "ogpu_metal", "rdk_boolean", "sop", "editor_core",
        "editor_document", "pxui", "rays_shell", "rays_graph", "rays_pathtracer", "rays_editor",
    ]);
    // Flow is UI-free and GPU-free, but its three crates may reach the OCaml flow's leaves (param, input, diagnostic).
    for library in ["flow_syntax", "flow_check", "flow_eval"] {
        forbid(library, &[
            "flow_ir", "flow_web", "flow_shader", "flow_gpu", "flow_graph", "flow_sop", "flow_native", "rays",
            "editor_core", "pxui", "rays_shell", "rays_graph", "sop", "rays_editor",
        ]);
        forbid(library, &gpu);
    }
    forbid("flow_sop_catalog", &[
        "flow_sop", "flow_native", "rays", "editor_core", "editor_document", "pxui", "rays_shell", "rays_graph", "rays_editor", "rays_bin",
    ]);
    forbid("flow_sop_catalog", &gpu);
    rules
}


/// External crates a package may name (package, dependency): `cc` is allowed everywhere as a build dependency.
pub const EXTERNAL_CRATES: &[(&str, &str)] = &[
    ("pxui_text", "cosmic-text"), // pxui
];

/// Direct edges that are forbidden outright (from, to, reason).
pub const NO_DIRECT: &[(&str, &str, &str)] = &[("rays", "sdl3", "SDL services belong to runtime")];

/// Crates whose tests (dev dependencies) may not reach the window host, SDL or any Rays crate either.
pub const TESTS_INCLUDED: &[&str] = &[
    "pxui", "pxui_text", "pxui_render", // pxui
];
pub const TESTS_INCLUDED_FORBIDDEN: &[&str] = &[
    "sdl3", "sdl3_sys", "pxui_sdl", "runtime", "runtime_resources", "renderer", "rays", "rays_shell", "rays_graph", "rays_editor", "param", "mem", // pxui
];

/// `cargo t` registry checks run by `gate docs` (package, filter, test target).
pub const DOCS_CHECKS: &[(&str, &str, Option<&str>)] = &[
    ("flow_check", "examples_check", None),
    ("rays_bin", "cli_help_errors_json_and_documentation", Some("cli")),
    ("rays_bin", "registry_metadata_contracts", Some("cli")),
];

/// Crates besides `*_sys` that may use `unsafe`.
pub const UNSAFE_CRATES: &[&str] = &[
    "parallel",
    "pxui_wayland", // pxui: owns its C shim and the extern block over it, like a *_sys crate
    "pxui_atspi", // pxui: likewise, over libdbus-1
];

/// Files besides those of `UNSAFE_CRATES` that may use `unsafe`, each with its reason.
pub const UNSAFE_FILES: &[(&str, &str)] = &[
    ("crates/pxui/src/arena.rs", "frame bump arena: canvas closures are written into raw bytes and called through type-erased records"), // pxui
    ("crates/pxui_render/src/lib.rs", "writes instance bytes into the host-mapped upload ring and views instance structs as bytes"), // pxui
];
