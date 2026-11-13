use std::{path::PathBuf, process::{Command, Output}};

fn root() -> PathBuf { PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..") }
fn rays(args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_rays")).args(args).current_dir(root()).output().expect("run rays")
}
fn stdout(out: &Output) -> String { String::from_utf8(out.stdout.clone()).unwrap() }

#[test]
fn cli_help_errors_json_and_documentation() {
    let help = rays(&["--help"]);
    assert!(help.status.success());
    for command in ["check", "doc", "render"] { assert!(stdout(&help).contains(command)); }
    let bad = rays(&["check", "--bogus"]);
    assert_eq!(bad.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&bad.stderr).contains("--bogus"));
    for command in ["fmt", "mem", "web"] {
        let bad = rays(&[command, "--bogus"]);
        assert_eq!(bad.status.code(), Some(1));
        assert!(String::from_utf8_lossy(&bad.stderr).contains("--bogus"));
    }
    let missing = rays(&["check", "--json", "absent.rays"]);
    assert_eq!(missing.status.code(), Some(1));
    assert!(missing.stderr.is_empty());
    assert!(stdout(&missing).contains("\"code\":\"E_IO\""));
    let doc = rays(&["doc", "draw/circle", "--json"]);
    assert!(doc.status.success(), "{}", String::from_utf8_lossy(&doc.stderr));
    assert!(stdout(&doc).contains("\"signature\":\"(draw/circle"));
    assert!(stdout(&doc).contains("\"arguments\":["));
    let guide = rays(&["doc", "--markdown"]);
    assert!(guide.status.success());
    assert!(stdout(&guide).lines().count() < 400);
    let guide_path = root().join("docs/lisp.md");
    if std::env::var_os("RAYS_UPDATE_FIXTURES").is_some() { std::fs::write(&guide_path, &guide.stdout).unwrap(); }
    assert_eq!(stdout(&guide), std::fs::read_to_string(guide_path).unwrap());
    for name in ["-", "->", "import", "layout", "panel", "node"] {
        let lookup = rays(&["doc", name]);
        assert!(lookup.status.success(), "{}: {}", name, String::from_utf8_lossy(&lookup.stderr));
    }
    let json = rays(&["doc", "--json"]);
    assert!(json.status.success());
    let golden = root().join("fixtures/cli/doc.json");
    if std::env::var_os("RAYS_UPDATE_FIXTURES").is_some() {
        std::fs::create_dir_all(golden.parent().unwrap()).unwrap();
        std::fs::write(&golden, &json.stdout).unwrap();
    }
    assert_eq!(stdout(&json), std::fs::read_to_string(golden).unwrap());
}

#[test]
fn diagnostic_json_probe_goldens() {
    let mut probes = std::fs::read_dir(root().join("fixtures/cli/probes")).unwrap().map(|entry| entry.unwrap().path()).collect::<Vec<_>>();
    probes.sort();
    assert_eq!(probes.len(), 10);
    let mut combined = String::new();
    for probe in probes {
        let relative = probe.strip_prefix(root()).unwrap();
        let argument = relative.to_str().unwrap().replace('\\', "/");
        let out = rays(&["check", "--json", &argument]);
        assert_eq!(out.status.code(), Some(1), "{}", relative.display());
        assert!(out.stderr.is_empty(), "{}", String::from_utf8_lossy(&out.stderr));
        assert!(stdout(&out).contains("\"span\":"));
        combined.push_str(&stdout(&out));
    }
    let golden = root().join("fixtures/cli/diagnostics.jsonl");
    if std::env::var_os("RAYS_UPDATE_FIXTURES").is_some() { std::fs::write(&golden, &combined).unwrap(); }
    assert_eq!(combined, std::fs::read_to_string(golden).unwrap());
}

#[test]
fn gpu_cli_render_canvas_fixed_clock_and_diagnostics() {
    let directory = std::env::temp_dir().join(format!("rays-cli-{}", std::process::id()));
    std::fs::create_dir_all(&directory).unwrap();
    let first = directory.join("first.png");
    let second = directory.join("second.png");
    let out = rays(&["render", "sketches/basic.rays", "-o", first.to_str().unwrap(), "--frame", "30"]);
    assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
    let out = rays(&["render", "sketches/basic.rays", "-o", second.to_str().unwrap(), "--frame", "30"]);
    assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
    let png = std::fs::read(&first).unwrap();
    assert_eq!(&png[..8], b"\x89PNG\r\n\x1a\n");
    assert_eq!(u32::from_be_bytes(png[16..20].try_into().unwrap()), 640);
    assert_eq!(u32::from_be_bytes(png[20..24].try_into().unwrap()), 360);
    assert_eq!(png, std::fs::read(&second).unwrap(), "fixed clock must produce identical canvas pixels");
    let broken = directory.join("bad.rays");
    std::fs::write(&broken, "(workspace bad (graph picture :context draw (draw/background (str (frame/index) \"notacolor\"))))").unwrap();
    let out = rays(&["render", broken.to_str().unwrap(), "-o", second.to_str().unwrap()]);
    assert_eq!(out.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&out.stderr).contains("E_DRAW_COLOR"), "{}", String::from_utf8_lossy(&out.stderr));
    assert_eq!(png, std::fs::read(&second).unwrap(), "a failed render preserves the previous PNG");
    std::fs::remove_dir_all(directory).unwrap();
}

#[test]
fn registry_metadata_contracts() {
    let (catalog, _) = flow_check::catalog::catalog_of_manifest(include_str!("../../flow_sop/flow_manifest.sexp")).unwrap();
    let entries = flow_check::registry::entries_with_ops(&catalog, flow_sop::operators::all());
    for entry in &entries {
        assert!(!entry.doc.trim().is_empty(), "{} has no documentation", entry.name);
        assert!(!entry.example.trim().is_empty(), "{} has no example", entry.name);
    }
    for &(name, _, _, example) in flow_check::registry::DIRECTIVES {
        assert!(!flow_check::workspace::special_forms().contains(&name), "{name} belongs to host syntax");
        assert!(entries.iter().any(|entry| entry.name == name));
        if name == "import" {
            assert_eq!(editor_document::workspace_doc::import_paths(example).unwrap(), vec!["swarm_lib.rays"]);
            let imports = rays_editor::source_file::read_imports("sketches/directive_example.rays", example).unwrap();
            let source = format!("{example}\n(workspace directive_example (graph value :context value 1))");
            let (document, diagnostics, _) = web_export::check_cli::check_text(imports, &source, false).unwrap();
            assert!(document.is_some(), "{diagnostics:?}");
            assert!(!diagnostics.iter().any(|d| d.severity == rays_diagnostic::Severity::ErrorSeverity), "{diagnostics:?}");
        } else {
            let source = if name == "layout" { example.to_owned() } else { format!("(layout {example})") };
            let forms = flow_syntax::syntax::parse(&source).unwrap();
            let layout = editor_document::layout_by_path::of_syntax(&forms[0]).unwrap();
            assert!(!editor_document::layout_by_path::is_empty(&layout));
            assert_eq!(layout, editor_document::layout_by_path::of_syntax(&editor_document::layout_by_path::to_syntax(&layout)).unwrap());
        }
    }
    let mut failures = Vec::new();
    for kind in &catalog.kinds {
        assert!(!kind.description.trim().is_empty(), "{} has no description", kind.qualified);
        assert!(![" geometry operation.", " scene node.", " world node."].iter().any(|suffix| kind.description.ends_with(suffix)), "{} has a placeholder description", kind.qualified);
        assert!(entries.iter().any(|entry| entry.name == kind.qualified));
        for parameter in &kind.parameters {
            let numeric = matches!(parameter.ty.as_ref(), Some(flow_check::port_type::PortType::Float | flow_check::port_type::PortType::Vec3));
            let angle_words = ["rotate", "rotation", "angle", "fov", "azimuth", "elevation", "latitude", "longitude", "cone", "twist", "yaw", "roll"];
            let angle = parameter.name.split('_').any(|part| angle_words.contains(&part))
                || parameter.label.to_lowercase().split(|c: char| !c.is_alphanumeric()).any(|word| angle_words.contains(&word) || word == "radians");
            if numeric && angle {
                if parameter.unit.as_deref() != Some("rad") { failures.push(format!("{} :{} has no rad unit", kind.qualified, parameter.name)); }
            }
        }
    }
    // Physical positions, extents and spatial distances in the existing geometry/scene schemas.
    // UV coordinates, normal/direction vectors, scales, attribute tolerances and angular sizes are dimensionless or use rad.
    let lengths: &[(&str, &str)] = &[
        ("scene/camera", "aperture eye_x eye_y eye_z far focus_distance near target_x target_y target_z"),
        ("scene/geometry", "translate_x translate_y translate_z"),
        ("scene/light", "height target_x target_y target_z translate_x translate_y translate_z width"),
        ("sop/attribute_mirror", "distance origin_x origin_y origin_z"),
        ("sop/attribute_noise", "offset_x offset_y offset_z"),
        ("sop/attribute_transfer", "blend_width kernel_radius max_distance"),
        ("sop/attribute_transfer_all", "blend_width kernel_radius max_distance"),
        ("sop/attribute_transfer_surface", "blend_width max_distance"),
        ("sop/bend", "length origin_x origin_y origin_z"),
        ("sop/blast_by_attribute", "center width"),
        ("sop/boolean", "point_tolerance"),
        ("sop/boolean_detect", "tolerance"),
        ("sop/boolean_fracture", "point_tolerance"),
        ("sop/bound", "lower_x lower_y lower_z minimum_radius upper_x upper_y upper_z"),
        ("sop/box", "center_x center_y center_z size_x size_y size_z"),
        ("sop/circle", "center_x center_y center_z radius radius_x radius_y"),
        ("sop/circle_from_edges", "radius"),
        ("sop/clean", "consolidate_distance"),
        ("sop/clip", "distance origin_x origin_y origin_z snapping_tolerance"),
        ("sop/convert_line", "maximum_distance"),
        ("sop/distance_along_geometry", "radius"),
        ("sop/distance_from_geometry", "radius"),
        ("sop/distance_from_target", "origin_x origin_y origin_z radius"),
        ("sop/facet", "consolidate_distance inline_distance"),
        ("sop/fuse", "tolerance"),
        ("sop/grid", "center_x center_y center_z height size width"),
        ("sop/group_backface", "viewpoint_x viewpoint_y viewpoint_z"),
        ("sop/group_bounds", "center_x center_y center_z radius size_x size_y size_z"),
        ("sop/group_edges", "max_length min_length"),
        ("sop/group_transfer", "distance"),
        ("sop/intersection_analysis", "tolerance"),
        ("sop/iso_surface", "max_x max_y max_z min_x min_y min_z"),
        ("sop/join_curves", "tolerance"),
        ("sop/line", "length origin_x origin_y origin_z"),
        ("sop/match_size", "offset_x offset_y offset_z target_center_x target_center_y target_center_z target_size_x target_size_y target_size_z"),
        ("sop/mirror", "origin_x origin_y origin_z"),
        ("sop/mountain", "height offset_x offset_y offset_z"),
        ("sop/peak", "distance"),
        ("sop/platonic", "center_x center_y center_z radius"),
        ("sop/point_replicate", "center_x center_y center_z size_x size_y size_z"),
        ("sop/poly_bevel", "distance"),
        ("sop/poly_extrude", "distance"),
        ("sop/poly_path", "maximum_distance"),
        ("sop/polywire", "radius"),
        ("sop/ray", "lift max_distance min_distance"),
        ("sop/remesh", "target_length"),
        ("sop/resample", "maximum_segment_length"),
        ("sop/revolve", "origin_x origin_y origin_z"),
        ("sop/separate_pieces", "gap"),
        ("sop/snap_to_grid", "max_distance offset_x offset_y offset_z spacing_x spacing_y spacing_z"),
        ("sop/soft_transform", "pivot_x pivot_y pivot_z radius translate_x translate_y translate_z"),
        ("sop/spiral", "center_x center_y center_z end_radius height pitch radius_change start_radius"),
        ("sop/sweep_circle", "radius"),
        ("sop/torus", "center_x center_y center_z major_radius minor_radius"),
        ("sop/transform", "pivot_x pivot_y pivot_z translate_x translate_y translate_z"),
        ("sop/triangulate_2d", "minimum_edge_length plane_origin_x plane_origin_y plane_origin_z target_edge_length"),
        ("sop/tube", "bottom_radius center_x center_y center_z height top_radius"),
        ("sop/uv_project", "height origin_x origin_y origin_z"),
        ("sop/uv_sphere", "base_radius center_x center_y center_z radius_x radius_y radius_z"),
        ("world/room", "depth height width"),
    ];
    for &(name, fields) in lengths {
        let kind = catalog.kinds.iter().find(|kind| kind.qualified == name).unwrap();
        for field in fields.split_whitespace() {
            let parameter = kind.parameters.iter().find(|parameter| parameter.name == field || parameter.fields.iter().any(|(name, _, _)| name == field)).unwrap_or_else(|| panic!("{name} has no {field}"));
            if parameter.unit.as_deref() != Some("m") { failures.push(format!("{name} :{field} has no m unit")); }
        }
    }
    for entry in entries.iter().filter(|entry| catalog.kinds.iter().any(|kind| kind.qualified == entry.name) || flow_sop::operators::all().iter().any(|op| op.name == entry.name)) {
        let context = entry.context.unwrap();
        let context = if context == flow_syntax::context::value() { flow_syntax::context::host() } else { context };
        let example = if context == flow_syntax::context::sop() { format!("(let [result {}] nil)", entry.example) } else { entry.example.clone() };
        let source = if entry.example.starts_with("(workspace ") { entry.example.clone() } else { format!("(workspace example (graph result :context {} {example}))", flow_syntax::context::name(context)) };
        let forms = flow_syntax::syntax::parse(&source).unwrap();
        let (_, diagnostics) = flow_check::workspace::check(flow_sop::operators::all(), false, false, None, &catalog, &forms);
        for diagnostic in diagnostics.iter().filter(|d| d.severity == rays_diagnostic::Severity::ErrorSeverity) {
            failures.push(format!("{}: {} -> {}", entry.name, entry.example, diagnostic.message));
        }
    }
    assert!(failures.is_empty(), "Invalid registry metadata/examples:\n{}", failures.join("\n"));
    let guide = rays_bin::documentation::guide().unwrap();
    if std::env::var_os("RAYS_UPDATE_FIXTURES").is_some() { std::fs::write(root().join("docs/lisp.md"), &guide).unwrap(); }
    assert!(guide.lines().count() < 400);
    assert_eq!(guide, std::fs::read_to_string(root().join("docs/lisp.md")).unwrap());
}

#[test]
fn table_load_is_relative_typed_and_refused_by_web() {
    let directory = std::env::temp_dir().join(format!("rays-table-{}", std::process::id()));
    std::fs::create_dir_all(&directory).unwrap();
    let table = directory.join("sample.tsv");
    let sketch = directory.join("sample.rays");
    std::fs::write(&table, "x:float\tlabel:text\n2.5\tλ\\tcaption\n").unwrap();
    std::fs::write(&sketch, "(workspace w (graph data :context host (table/load \"sample.tsv\")) (graph picture :context draw (let* [d (ref data)] (draw/text [(array/nth d.x 0) 10 0] (array/nth d.label 0)))))").unwrap();
    let check = rays(&["check", "--json", sketch.to_str().unwrap()]);
    assert!(check.status.success(), "{}", stdout(&check));
    let formatted = rays(&["fmt", sketch.to_str().unwrap()]);
    assert!(formatted.status.success() && stdout(&formatted).contains("\"sample.tsv\""));
    let web = rays(&["web", sketch.to_str().unwrap(), "-o", directory.join("web").to_str().unwrap()]);
    assert_eq!(web.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&web.stderr).contains("E_WEB_TABLE"));
    for (source, code) in [("broken\n1\n", "E_TABLE_HEADER"), ("x:int\n1\t2\n", "E_TABLE_ROW")] {
        std::fs::write(&table, source).unwrap();
        let check = rays(&["check", "--json", sketch.to_str().unwrap()]);
        assert_eq!(check.status.code(), Some(1));
        assert!(stdout(&check).contains(code), "{}", stdout(&check));
    }
    std::fs::remove_dir_all(directory).unwrap();
}
