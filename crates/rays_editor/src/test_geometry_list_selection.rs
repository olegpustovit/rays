use std::rc::Rc;

use editor_document::{document::Level, kinds, workspace_doc};
use rays::frame::Frame;
use rays_graph::scope_state as scope;
use rays_shell::tree::Intent;

use crate::editor_model::{pane_view, EditorModel};
use crate::{cook, editor_host, editor_inspect, editor_list, editor_reduce, editor_scope, editor_setup, selection};

#[test]
fn geometry_list_replaces_the_scope_selection() {
    let catalog = kinds::catalog(flow_sop::manifest::VERSION, &sop::nodes::factories()).unwrap();
    let workspace = workspace_doc::of_text(&Default::default(), &catalog, "
        (workspace list_selection
          (graph g :context sop
            (let* [box (sop/box)
                   moved (sop/transform box :translate [1 0 0])]
              moved)))
    ").unwrap();
    let mut value: EditorModel = editor_setup::create(
        editor_setup::CreateOpts { await_: Some(true), ..Default::default() },
        &workspace,
        Rc::new(cook::prepare),
    ).unwrap();
    let object = *value.doc.networks.keys().next().unwrap();
    editor_list::open_level(&mut value, Level::Inside(object));
    editor_scope::sync_scope(&mut value);
    let (_, rows) = editor_list::rows(&(&value).into());
    let (rows, _) = rows();
    let (ids, paths): (Vec<_>, Vec<_>) = rows.iter().filter(|row| !row.ghost)
        .filter_map(|row| Some((row.id, editor_inspect::node_path(&(&value).into(), row.id)?)))
        .unzip();
    assert_eq!(ids.len(), 2);

    let mut input = input::at_time(0.0);
    input.size = (800, 600);
    let frame = Frame { input, pixel_scale: (1.0, 1.0), fps: 60.0, mouse_delta: (0.0, 0.0) };
    rays_shell::host::frame(&mut value.ui.borrow_mut(), &frame, &mut |_| {});
    for (ids, expected) in [(&ids[..1], &paths[..1]), (&ids[..], &paths[..]), (&ids[..0], &paths[..0])] {
        let mut result = editor_reduce::idle_result(&(&value).into(), selection::empty());
        result.tree_intents.push(Intent::Select(ids.to_vec()));
        let input = editor_reduce::idle_input(&(&value).into(), &[], &frame);
        editor_reduce::reduce(None, None, input, &mut value, result);
        let actual = scope::selected(pane_view(&(&value).into()));
        assert_eq!(actual.len(), expected.len(), "list selected {ids:?}, scope selected {actual:?}");
        assert!(expected.iter().all(|path| actual.contains(path)));
    }
    editor_host::close(&value);
}
