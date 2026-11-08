//! Port of lib/rays_editor/test/test_materials.ml: material refs, group preservation and domain
//! exactness (the file's main body), `face_materials_preserve_explosion` and the editor outline /
//! follow-and-back / rename / assign test (`follow_tests`). Not ported: `carry_tests` and
//! `viewport_carry_tests` (see the report).
use super::support::*;
use super::t3c::*;
use flow_graph::flow_edit as fe;
use input::{Event, Key, MouseButton};
use rays_editor::navigator as n;
use rays_editor::editor;
use sop::context::{self as sctx, Options as ContextOptions};
use sop::session;
use std::rc::Rc;

fn fail(message: &str) -> ! {
    panic!("test_materials: {message}")
}

fn check(condition: bool, message: &str) {
    if !condition {
        fail(message);
    }
}

fn ok<T, E: std::fmt::Debug>(r: Result<T, E>) -> T {
    r.unwrap_or_else(|e| fail(&format!("{e:?}")))
}

const SOURCE: &str = r##"(workspace materials
  (graph blue :context material
    (material/standard :name "blue" :color "#2670f5" :roughness 0.3))
  (graph white :context material
    (material/standard :name "white" :color "#eeeeee" :roughness 0.8))
  (graph geo :context sop
    (let* [base (sop/material (sop/box) :material (ref blue))
           group (sop/group_range base :owner "Primitives" :name "accent"
                   :start 0 :end_ 0)]
      (sop/material group :group "accent" :material (ref white)))))"##;

fn cook(domains: i64, node: &sop::node::T) -> sop::session::Output {
    let mut s = ok(session::create(32, 1 << 24));
    let context = ok(sctx::create(ContextOptions { domains: Some(domains), ..Default::default() }));
    let out = session::cook(&mut s, None, &context, node).unwrap_or_else(|e| fail(&rays_diagnostic::error_to_string(&e)));
    session::close(&mut s);
    out
}

fn geometry_of(o: &sop::session::Output) -> rdk_core::geometry::T {
    ok(sop::payload::geometry(&o.payload))
}

fn surface_scene(node: &sop::node::T, surface: &rdk_rays::surface::T) -> Vec<rays::scene3::walk::drawing> {
    let explosion = rdk_rays::packed_pieces::explosion(sop::node::operation(node), &sop::node::parameter_fields(node));
    let scene = ok(rdk_rays::surface::scene3(explosion.as_ref(), None, surface));
    rays::scene3::walk::drawings(&scene)
}

fn attr(name: &str, g: &rdk_core::geometry::T) -> rdk_core::attribute::Storage {
    let a = rdk_core::geometry::find_attribute(rdk_core::attribute::Owner::Primitive, name, g).unwrap_or_else(|| fail(&format!("no {name}")));
    ok(rdk_core::attribute::storage(&a))
}

fn signature(g: &rdk_core::geometry::T) -> Vec<String> {
    ["shop_materialpath", "material_roughness", "material_color", "material_emission"]
        .iter()
        .map(|name| match attr(name, g) {
            rdk_core::attribute::Storage::Text(a) => format!("{a:?}"),
            rdk_core::attribute::Storage::Float(a) => format!("{:?}", a.iter().map(|x| x.to_bits()).collect::<Vec<_>>()),
            rdk_core::attribute::Storage::Float4(a) => format!("{:?}", (0..rdk_core::packed::Float4::length(&a)).map(|i| rdk_core::packed::Float4::get(&a, i).map(|(w, x, y, z)| (w.to_bits(), x.to_bits(), y.to_bits(), z.to_bits()))).collect::<Vec<_>>()),
            _ => fail("unexpected material attribute"),
        })
        .collect()
}

pub fn face_materials_preserve_explosion() {
    let piece = |x: f64, id: i64| {
        node(&[], &format!(r#"(-> (sop/box :normals "Auto" :connectivity "Triangles" :size [1.0 1.0 1.0])
    (sop/transform :mode "Matrix" :m03 {} :m13 0.0 :m23 0.0)
    (sop/set_int :owner "Primitive" :name "piece" :value {id}))"#, float(x)))
    };
    let merged = node(&[("in2".to_string(), piece(-2., 0)), ("in3".to_string(), piece(2., 1))], "(sop/merge (sop/ext_in2) (sop/ext_in3))");
    let geometry = geometry_of(&cook(1, &merged));
    let group = ok(rdk_core::group::init(None, rdk_core::group::Owner::Primitive, "one_face", rdk_core::geometry::primitive_count(&geometry), &|i| i == 0));
    let with_group = ok(rdk_core::geometry::with_group(group, &geometry));
    let n = node(
        &[("in4".to_string(), snapshot(&with_group))],
        r#"(-> (sop/material
       (sop/ext_in4)
       :material "blue"
       :color [0.15 0.43 0.96]
       :roughness 0.3
       :emission [0.0 0.0 0.0])
     (sop/material
       :group "one_face"
       :material "white"
       :color [1.0 1.0 1.0]
       :roughness 0.8
       :emission [0.0 0.0 0.0])
     (sop/exploded_view :amount 1.0))"#,
    );
    let out = cook(1, &n);
    let surface = ok(rdk_rays::surface::of_geometry(out.instances.as_ref().map(|a| a.as_ref().clone()), &geometry_of(&out)));
    let drawings = surface_scene(&n, &surface);
    check(drawings.len() == 2, "two drawings");
    // Centers +-2 move to +-4. Painting just one face must leave every face of each cube at exactly the
    // same rigid displacement.
    for d in &drawings {
        let v = ok(rays::mesh::view::packed_view(&d.mesh));
        for index in v.indices.iter() {
            let i = *index as usize;
            let x = v.vertices.x[i].abs();
            check((3.5..=4.5).contains(&x), &format!("x displacement {x} (explosion {})", rdk_rays::packed_pieces::explosion(sop::node::operation(&n), &sop::node::parameter_fields(&n)).is_some()));
            check(v.vertices.y[i].abs() <= 0.5 && v.vertices.z[i].abs() <= 0.5, "y/z extent");
        }
    }
}

/// The main body: material refs, group preservation, render batches and domain exactness.
pub fn material_refs() {
    let ws = load(SOURCE);
    let graphs = ok(rays_editor::workspace::sop_graphs(None, &ws));
    let node = graphs.iter().find(|(n, _)| n == "geo").expect("geo").1.clone();
    let (one, four) = (cook(1, &node), cook(4, &node));
    check(signature(&geometry_of(&one)) == signature(&geometry_of(&four)), "domain exactness");
    check(rdk_core::geometry::primitive_count(&geometry_of(&one)) == 6, "six primitives");
    match (attr("shop_materialpath", &geometry_of(&one)), attr("material_roughness", &geometry_of(&one))) {
        (rdk_core::attribute::Storage::Text(names), rdk_core::attribute::Storage::Float(rough)) => {
            check(&names[0] == b"white" && rough[0] == 0.8, "the accent face");
            check((1..6).all(|n| &names[n] == b"blue") && rough[1..6].iter().all(|r| *r == 0.3), "the other faces");
        }
        _ => fail("material attribute types"),
    }
    let surface = ok(rdk_rays::surface::of_geometry(None, &geometry_of(&one)));
    let drawings = surface_scene(&node, &surface);
    check(drawings.len() == 2, "two drawings");
    check(drawings.iter().map(|d| rays::mesh::index_count(&d.mesh)).sum::<i64>() == 36, "36 indices");
    check(drawings.iter().any(|d| d.material.diffuse == rays_math::color::hex_exn("#2670f5").unwrap()), "the blue material");
    check(signature(&geometry_of(&one)) == signature(&geometry_of(&cook(1, &node))), "repeatable");
    for text in [
        "(workspace x (graph m :context material (material/standard :roughness 2)))",
        "(workspace x (graph m :context material (material/standard :color \"invalid\")))",
        "(workspace x (graph m :context sop (material/standard)))",
    ] {
        check(rays_editor::workspace::load(None, Vec::new(), None, text).is_err(), &format!("{text} loaded"));
    }
}

const FOLLOW: &str = r##"(workspace follow
  (graph cobalt :context material
    (material/standard :name "cobalt blue" :color "#2670f5" :roughness 0.3))
  (graph spare :context material (material/standard :name "spare" :color "#ff0000"))
  (graph shards :context sop
    (let* [m (sop/material (sop/box) :material (ref cobalt))] m))
  (graph scene :context scene
    (let* [obj (scene/geometry (ref shards) :name "shards")] (scene/merge obj))))"##;

pub fn follow_tests() {
    let mut d = Driver::new(create(editor::Create { domains: Some(1), ..options() }, &load(FOLLOW)));
    let pane = |d: &Driver| dump_line(d.e(), "pane graph");
    let route = |d: &Driver| dump_line(d.e(), "route");
    let click = |d: &mut Driver, keys: &[Key], (x, y): (f64, f64)| {
        d.step_full(&[], keys, (x, y), vec![Event::Pointer_moved((x, y))], 1);
        d.step_full(&[MouseButton::LeftButton], keys, (x, y), vec![Event::Pointer_pressed(MouseButton::LeftButton, (x, y))], 1);
        d.step_full(&[], keys, (x, y), vec![Event::Pointer_released(MouseButton::LeftButton, (x, y))], 1);
    };
    let jump = |d: &mut Driver, name: &str| {
        d.step(vec![key(Key::KeyChar('/')), ch('j')]);
        d.step(vec![Event::Text_input(name.into())]);
        d.step(vec![key(Key::Enter)]);
        d.step(vec![]);
    };
    for _ in 0..12 {
        d.step(vec![]);
    }
    check(pane(&d) == "scene" && route(&d) == "scene", &format!("opens on the scene: {}", pane(&d)));

    // the outline: groups, chip, use count, unused
    let ws = editor::workspace(d.e()).checked;
    let chips = n::chips(&flow_eval::static_eval(&ws).unwrap_or_else(|e| fail(&rays_diagnostic::to_string(&e))));
    let params = n::Params {
        workspace: ws.clone(),
        active: Some("scene".into()),
        scope: None,
        records: None,
        probes: Rc::new(|_| 0),
        selected: Vec::new(),
        chips,
        objects: Vec::new(),
        root_detail: String::new(),
        layouts: None,
        notes: Vec::new(),
    };
    let lines: Vec<String> = n::rows(true, &n::initial(), &params).iter().map(n::describe).collect();
    let index = |text: &str| lines.iter().position(|l| has(l, text)).unwrap_or_else(|| fail(&format!("{text} is not in the outline: {}", lines.join(" | "))));
    check(index("SCENE") < index("GEOMETRY") && index("GEOMETRY") < index("MATERIALS"), "the groups are in dependency order");
    check(index("MATERIALS") < index("cobalt") && index("shards") < index("MATERIALS"), "a graph sits in its group");
    index("cobalt · ×1 · #2670f5");
    index("spare · unused · #ff0000");
    check(lines.iter().filter(|l| has(l, "unused")).count() == 1, "only the unread material is unused");
    check(n::rows(true, &n::with_query("cob", &n::initial()), &params).len() == 2, "typing filters across the groups");

    // / j, then follow with i and back with u
    jump(&mut d, "shards");
    check(pane(&d) == "shards" && route(&d) == "scene > shards", &format!("/ j jumps: {}", route(&d)));
    let tile = |d: &Driver, path: &[&str]| -> (f64, f64) {
        let p: Vec<String> = path.iter().map(|s| s.to_string()).collect();
        let (bx, by, bw, bh) = editor::node_box(d.e(), &p).expect("node box");
        (bx as f64 + bw as f64 / 2., by as f64 + bh as f64 - 6.)
    };
    let at = tile(&d, &["shards", "m"]);
    click(&mut d, &[], at);
    d.step(vec![ch('i')]);
    d.step(vec![]);
    check(pane(&d) == "cobalt" && route(&d) == "scene > shards > cobalt", &format!("i follows the :material of the selected node: {}", route(&d)));
    d.step(vec![ch('u')]);
    d.step(vec![]);
    check(pane(&d) == "shards" && route(&d) == "scene > shards", &format!("u goes back one: {}", route(&d)));
    d.step(vec![ch('u')]);
    d.step(vec![]);
    check(pane(&d) == "scene" && route(&d) == "scene", &format!("and back to the scene: {}", route(&d)));
    d.step(vec![ch('u')]);
    d.step(vec![]);
    check(pane(&d) == "scene", "with nothing to go back to u stays at the scene");

    // a double-click on the node follows too
    jump(&mut d, "shards");
    let body = tile(&d, &["shards", "m"]);
    click(&mut d, &[], body);
    click(&mut d, &[], body);
    d.step(vec![]);
    check(pane(&d) == "cobalt", &format!("a double-click follows: {} / {}", pane(&d), route(&d)));
    jump(&mut d, "scene");

    // a pick in the viewport lands in the right graph: the surface says "cobalt blue", the graph is cobalt
    let (vx, vy, vw, vh) = editor::panes(d.e(), &frame_at((0., 0.), vec![], d.count)).view;
    click(&mut d, &[Key::Alt], ((vx + vw / 2) as f64, (vy + vh / 2) as f64));
    d.step(vec![]);
    d.step(vec![]);
    check(pane(&d) == "cobalt", &format!("Alt-click opens the material of the surface: {} / {}", pane(&d), route(&d)));
    jump(&mut d, "scene");

    // i in a viewport with nothing selected follows to the scene it shows
    jump(&mut d, "shards");
    click(&mut d, &[], (vx as f64 + 4., vy as f64 + 4.));
    d.step(vec![ch('i')]);
    d.step(vec![]);
    check(pane(&d) == "scene", &format!("i in a viewport follows to its scene: {} / {}", pane(&d), route(&d)));

    // rename: the graph and every (ref) to it, one undo entry
    let renamed = editor::edit(d.e().clone(), &fe::Op::RenameGraph { name: "cobalt".into(), to_: "navy".into() }).unwrap_or_else(|m| fail(&m));
    let text = text_of(&renamed);
    check(has(&text, "(graph navy") && has(&text, "(ref navy)") && !has(&text, "(ref cobalt)"), &format!("rename rewrote the graph and its reader: {text}"));
    check(editor::undo_label(&renamed) == Some("Rename graph".to_string()), "one history entry");
    check(editor::edit(d.e().clone(), &fe::Op::RenameGraph { name: "cobalt".into(), to_: "spare".into() }).is_err(), "a taken name is refused");
    d.set(renamed);
    d.step_keys(&[Key::Meta], vec![ch('z')]);
    check(has(&d.text(), "(graph cobalt") && has(&d.text(), "(ref cobalt)"), "one undo restores both");

    // a read material is not deleted, and the refusal names who reads it; an unread one goes
    match editor::edit(d.e().clone(), &fe::Op::RemoveGraph { name: "cobalt".into() }) {
        Ok(_) => fail("a read material was removed"),
        Err(m) => check(has(&m, "shards"), &format!("the refusal names its reader: {m}")),
    }
    check(editor::edit(d.e().clone(), &fe::Op::RemoveGraph { name: "spare".into() }).is_ok(), "an unread material can go");

    // I peeks in a floating window
    jump(&mut d, "shards");
    let body = tile(&d, &["shards", "m"]);
    click(&mut d, &[], body);
    d.step_keys(&[Key::Shift], vec![ch('i')]);
    d.step(vec![]);
    check(has(&d.text(), "(ui/floating (ui/graph \"cobalt\"))") || has(&d.text(), "ui/graph \"cobalt\""), &format!("I opens the material in a floating graph: {}", d.text()));
    // / a: Material makes a graph and opens it; Material of... fills the reference
    jump(&mut d, "scene");
    d.step(vec![key(Key::KeyChar('/')), ch('a')]);
    d.step(vec![Event::Text_input("material".into())]);
    d.step(vec![key(Key::Enter)]);
    d.step(vec![]);
    check(has(&d.text(), "(graph material :context material"), &format!("/ a made a material graph: {}", d.text()));
    check(pane(&d) == "material" && editor::undo_label(d.e()) == Some("New material".to_string()), "and opened it");
    d.step(vec![ch('u')]);
    d.step(vec![]);
    check(pane(&d) == "scene", "u returns from it");
    jump(&mut d, "shards");
    let body = tile(&d, &["shards", "m"]);
    click(&mut d, &[], body);
    d.step(vec![key(Key::KeyChar('/')), ch('a')]);
    d.step(vec![Event::Text_input("spare".into())]);
    d.step(vec![key(Key::Enter)]);
    d.step(vec![]);
    check(has(&d.text(), ":material (ref spare)") && has(&d.text(), "(sop/material m"), &format!("Material of... adds a sop/material after the selection: {}", d.text()));
    d.close();
}
