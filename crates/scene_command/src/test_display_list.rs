// Port of lib/scene_command/test_display_list.ml, assertion for assertion. The OCaml file has no
// `check` calls; its `require` calls are the assertions here.
use crate::display_list::{self, Builder};
use crate::render_ir::{self, Command, Error, Geometry, Rect};
use std::sync::Arc;

fn rectangle(index: i64) -> Geometry {
    let x = index as f64;
    Geometry {
        vertices: vec![x, 2.0, x + 3.0, 2.0, x + 3.0, 6.0, x, 6.0],
        indices: vec![0, 1, 2, 0, 2, 3],
        color: index as i32,
    }
}

// What a builder of `capacity` slots holding these geometries reports: an opcode byte, eight values,
// a colour and an integer a slot, and a word for each vertex coordinate and index.
fn source_bytes(capacity: i64, geometries: &[Geometry]) -> i64 {
    let word = std::mem::size_of::<usize>() as i64;
    capacity
        + capacity * 8 * word
        + capacity * 4
        + capacity * word
        + geometries
            .iter()
            .map(|geometry| (geometry.vertices.len() as i64 + geometry.indices.len() as i64) * word)
            .sum::<i64>()
}

fn commands(segment: &display_list::Segment) -> &[Command] {
    render_ir::commands_readonly(display_list::render_ir(segment))
}

#[test]
fn render_ir_encoding_keeps_version_little_endian_words_and_signed_zero() {
    let encoded = render_ir::serialize(
        &render_ir::create(vec![
            Command::Clear(0x89abcdef_u32 as i32),
            Command::Geometry(Geometry { vertices: vec![-0.0, 1.0], indices: vec![], color: 0x10203040 }),
        ])
        .unwrap(),
    );
    assert_eq!(
        encoded,
        b"\x31\x49\x32\x52\x02\x00\x00\x00\x00\x00\x00\x00\x00\xef\xcd\xab\x89\x05\x40\x30\x20\x10\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\xf0\x3f\x00\x00\x00\x00\x00\x00\x00\x00"
    );
}

#[test]
fn fresh_ids_are_unique_and_consecutive_across_threads() {
    let workers: Vec<_> = (0..4)
        .map(|_| {
            std::thread::spawn(|| (0..4096).map(|_| display_list::fresh_id().unwrap()).collect::<Vec<i64>>())
        })
        .collect();
    let mut ids: Vec<i64> = workers.into_iter().flat_map(|worker| worker.join().unwrap()).collect();
    ids.sort_unstable();
    for (index, id) in ids.iter().enumerate() {
        assert_eq!(*id, ids[0] + index as i64, "display-list identities must be unique and consecutive across domains");
    }
}

#[test]
fn publication_growth_reset_and_validation() {
    // Sixteen rectangles inside one clip are 18 commands: more than the builder's first 16 slots,
    // so its storage doubles once to 32.
    let mut builder = Builder::create();
    builder.push_clip(0.0, 0.0, 64.0, 64.0).unwrap();
    for index in 0..16 {
        builder.geometry(&rectangle(index)).unwrap();
    }
    builder.pop_clip().unwrap();
    let first = builder.publish(7, 1).unwrap();
    let same = builder.publish(7, 1).unwrap();
    assert!(Arc::ptr_eq(&first, &same) && display_list::id(&first) == 7 && display_list::version(&first) == 1, "display-list stable publication identity drift");
    assert_eq!(
        render_ir::identity(display_list::render_ir(&first)),
        render_ir::identity(display_list::render_ir(&same)),
        "unchanged display-list IR lost its identity"
    );
    let cmds = commands(&first);
    assert_eq!(cmds.len(), 18, "display-list command cardinality drift");
    match (&cmds[0], &cmds[17]) {
        (Command::PushClip(Rect { x: 0.0, y: 0.0, width: 64.0, height: 64.0 }), Command::PopClip) => {}
        _ => panic!("display-list command order drift"),
    }
    for index in 0..16 {
        match &cmds[index + 1] {
            Command::Geometry(geometry) if *geometry == rectangle(index as i64) => {}
            _ => panic!("display-list growth lost or reordered a command"),
        }
    }
    assert_eq!(
        display_list::source_bytes(&first),
        source_bytes(32, &(0..16).map(rectangle).collect::<Vec<_>>()),
        "display-list geometric growth drift"
    );
    builder.reset();
    let reset = builder.publish(7, 1).unwrap();
    assert!(!Arc::ptr_eq(&reset, &first) && commands(&reset).is_empty(), "display-list reset reused a stale publication");
    assert_ne!(
        render_ir::identity(display_list::render_ir(&reset)),
        render_ir::identity(display_list::render_ir(&first)),
        "changed display-list IR reused its old identity"
    );
    assert_eq!(display_list::source_bytes(&reset), source_bytes(32, &[]), "display-list reset released capacity");
    builder.geometry(&rectangle(3)).unwrap();
    let second = builder.publish(7, 2).unwrap();
    assert!(
        !Arc::ptr_eq(&second, &first)
            && !Arc::ptr_eq(&second, &reset)
            && display_list::version(&second) == 2
            && matches!(commands(&second), [Command::Geometry(g)] if *g == rectangle(3))
            && display_list::source_bytes(&second) == source_bytes(32, &[rectangle(3)]),
        "display-list reset released capacity or failed to republish"
    );
    let mut invalid = Builder::create();
    invalid.pop_clip().unwrap();
    assert_eq!(invalid.publish(8, 1).err(), Some(Error::UnbalancedClip), "display-list accepted an unbalanced clip");

    let mut complete = Builder::create();
    let mut geometry = Geometry { vertices: vec![0.0, 0.0, 2.0, 0.0, 0.0, 2.0], indices: vec![0, 1, 2], color: 0x01020304 };
    complete.geometry(&geometry).unwrap();
    complete
        .image(
            17,
            Rect { x: 1.0, y: 2.0, width: 3.0, height: 4.0 },
            Rect { x: 5.0, y: 6.0, width: 7.0, height: 8.0 },
        )
        .unwrap();
    geometry.vertices[0] = 99.0;
    let complete = complete.publish(9, 4).unwrap();
    match commands(&complete) {
        [Command::Geometry(owned), Command::Image(image)]
            if owned.vertices[0] == 0.0
                && image.resource_id == 17
                && image.source == (Rect { x: 1.0, y: 2.0, width: 3.0, height: 4.0 })
                && image.destination == (Rect { x: 5.0, y: 6.0, width: 7.0, height: 8.0 }) => {}
        _ => panic!("display-list complete command or ownership parity drift"),
    }
}
