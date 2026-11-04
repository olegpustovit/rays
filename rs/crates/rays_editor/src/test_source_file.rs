//! `Source_file`: polled for edits, saved over only the text the document came from; `Carry`'s text
//! helpers; `Editor_renderer`'s film and turns; `Cook`'s box maths.
use crate::carry;
use crate::cook;
use crate::editor_renderer as r;
use crate::source_file as sf;
use rays_math::{mat4, vec3};

fn temp(name: &str) -> String {
    let dir = std::env::temp_dir().join(format!("rays_edA_{}_{name}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    dir.join("a.rays").to_string_lossy().into_owned()
}

#[test]
fn polls_the_file_and_saves_over_the_text_it_came_from() {
    let file = temp("poll");
    std::fs::write(&file, "(workspace w)\n").unwrap();
    let digest = editor_document::lowering::sha256("(workspace w)\n");
    // unchanged since the build: the first poll reads and reports nothing
    let (s, text) = sf::poll(1.0, &sf::at(&file, &digest));
    assert_eq!(text, None);
    // an edit from another writer is reported once
    std::fs::write(&file, "(workspace w2)\n").unwrap();
    let (s, text) = sf::poll(2.0, &s);
    assert_eq!(text.as_deref(), Some("(workspace w2)\n"));
    let (s, text) = sf::poll(3.0, &s);
    assert_eq!(text, None, "the same refused text is not offered again");
    // saving over text that is not the one accepted is refused
    assert_eq!(sf::save(&s, "x").err(), Some(sf::Save_error::Changed));
    let s = sf::accepted(&s, "(workspace w2)\n");
    let saved = sf::save(&s, "(workspace w3)").unwrap();
    assert_eq!(std::fs::read_to_string(&file).unwrap(), "(workspace w3)\n");
    assert_eq!(saved.digest, editor_document::lowering::sha256("(workspace w3)\n"));
    // a missing file is an error of the poll, not a change
    let (missing, none) = sf::poll(9.0, &sf::at("no/such/file.rays", "d"));
    assert!(missing.error.is_some() && none.is_none());
}

#[test]
fn carry_inserts_spaced_from_what_it_touches() {
    assert_eq!(carry::insert_at("(a)", 2, "(ref b)"), "(a (ref b))");
    assert_eq!(carry::insert_at("(a ", 3, "(ref b)"), "(a (ref b)");
    assert_eq!(carry::insert_at("", 0, "x"), "x");
    let p = |kind: &str, value: &str| carry::Payload { kind: kind.into(), value: value.into() };
    assert_eq!(carry::parse(&p("material", "(ref cobalt)")).unwrap().1, "cobalt");
    assert_eq!(carry::parse(&p("camera", "cam")).unwrap().1, "cam");
    assert_eq!(carry::parse(&p("sop", "cobalt")).err().as_deref(), Some("A carried value is a graph reference"));
    assert_eq!(carry::parse(&p("sop", "(ref a) (ref b)")).err().as_deref(), Some("A carried value is one form"));
}

#[test]
fn renderer_film_steps_and_turns() {
    assert_eq!(r::film(true, (1920, 1080), (1000, 600)), ((960, 540), 2));
    assert_eq!(r::film(true, (4000, 4000), (100, 100)), ((500, 500), 8));
    assert_eq!(r::film(false, (1920, 1080), (300, 200)), ((300, 200), 1));
    let order: Vec<String> = ["a", "b", "c"].iter().map(|s| s.to_string()).collect();
    let wanting: Vec<String> = ["a", "c"].iter().map(|s| s.to_string()).collect();
    assert_eq!(r::next_turn(&order, Some("a"), &wanting).as_deref(), Some("c"));
    assert_eq!(r::next_turn(&order, Some("c"), &wanting).as_deref(), Some("a"));
    assert_eq!(r::next_turn(&order, None, &[]), None);
    assert_eq!(r::of_label(r::label(r::Mode::Wireframe)), Some(r::Mode::Wireframe));
    assert_eq!(r::of_label("nope"), None);
    assert_eq!(r::wire_color(&rays_math::color::rgb(255, 255, 255)), rays_math::color::hex_exn("#285f77").unwrap());
}

#[test]
fn cook_boxes_follow_transforms() {
    let b = (vec3::create(0.0, 0.0, 0.0), vec3::create(1.0, 2.0, 3.0));
    let moved = cook::transformed(&mat4::translation(vec3::create(10.0, 0.0, 0.0)), &b);
    assert_eq!((moved.0.x, moved.1.x, moved.1.z), (10.0, 11.0, 3.0));
    let both = cook::transformed_all(&[mat4::identity(), mat4::translation(vec3::create(0.0, 5.0, 0.0))], &b);
    assert_eq!((both.0.y, both.1.y), (0.0, 7.0));
    let u = cook::union(&b, &moved);
    assert_eq!((u.0.x, u.1.x), (0.0, 11.0));
}
