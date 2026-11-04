// Port of lib/flow_graph/edit_tree.ml(i): the syntax-tree helpers a layout edit needs, public so
// the editor's own edits rewrite a workspace the way `flow_edit` does.
pub use crate::edit_base::{
    arg_get, arg_set, collapse, count_refs, dedup, edit_scope, ensure, fail, fail_code, first_pair_index, find_pair, fresh, get_node,
    has_prefix, head_sym, insert_at, keep_notes, kw_get, kwf, last_child, map_children, map_items, mk, num, pat_key, positional,
    rebuild, reorder, rename_place, rename_ref, root_name, root_used, scope_of, set_node, split_node, sym, sym_list, workspace_parts,
    ArgKey, Scope, call,
};
pub use crate::edit_ops::apply_rewrite;
