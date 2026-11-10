// every graph gesture as a pure rewrite of the workspace
// source. A gesture names its target by a `workspace::Path`: the scope path of the binding, then its
// name (`["flower", "ring", "u"]`, or `["g", "@result"]` for a body result).
pub use crate::edit_base::{
    context_sym, default_for, fresh_among, fresh_name, keywords, literals, nested, node_call, pat_key, pat_names, positional, rename_ref,
    skip_value, value_base, value_call, ArgKey, Loop, Op, Path,
};
pub use crate::edit_base::{free_names, nested_leaf};
pub use crate::edit_ops::{
    apply, apply_checked, apply_checked_with, arg_of, arg_text, defn_draft, duplicated, gesture, label, leaf_keys,
    macro_draft, macro_op, nested_nodes, remap, Checker, DefnDraft, MacroDraft,
};
