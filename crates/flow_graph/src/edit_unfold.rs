use crate::edit_base::{ids::*, ArgKey, R, fail};
use flow_syntax::syntax::SyntaxBuilder;

// the call, loop or scope written in input `key` of `leaf` becomes a binding just before the
// binding that held it; the scope with it, and its name
pub fn unfold_in(used: &mut Vec<String>, name: Option<&str>, sx: &S, leaf: &str, key: &ArgKey, sub: &[i64], builder: &mut SyntaxBuilder) -> R<(S, String)> {
    let e = get_node(sx, leaf, builder)?;
    let whole = arg_get(&e, key, builder).ok_or_else(|| fail("That input is not set."))?;
    let inner = get_sub(&whole, sub, builder).ok_or_else(|| fail("That part does not exist."))?;
    let h = match head_sym(&inner, builder) {
        Some(h) if h != "ref" => h.to_owned(),
        _ => return Err(fail("Only a call, loop or scope can be unfolded.")),
    };
    let base = split_leaf(leaf)?.0;
    let name = match name {
        Some(n) => n.to_owned(),
        None => {
            let wanted = if is_zone(&inner, builder) {
                if base == "@result" {
                    "each".to_owned()
                } else {
                    format!("{}_{}", base, if h == "sum" { "total" } else { "each" })
                }
            } else if h == "let*" {
                "block".to_owned()
            } else {
                value_base(&h)
            };
            fresh(used, &wanted)
        }
    };
    let e2 = arg_set(&e, key, Some(set_sub(&whole, sub, sym(&name, builder), builder)?), builder)?;
    let sc = ensure(&set_node(sx, leaf, e2, builder)?, builder);
    let at = if base == "@result" { sc.ps.len() } else { first_pair_index(&sc, leaf, builder)? };
    let ps = insert_at(&sc.ps, at as i64, &[(sym(&name, builder), inner)]);
    Ok((rebuild(&sc, &ps, sc.res, builder), name))
}

// a wire never deletes a node: the nested node an input held stays, as a binding nothing reads
pub fn keep_nested(pick: &dyn Fn(&S, &SyntaxBuilder) -> bool, used: &mut Vec<String>, sx: &S, leaf: &str, key: &ArgKey, builder: &mut SyntaxBuilder) -> R<S> {
    match key {
        ArgKey::Pos(_) | ArgKey::Kw(_) | ArgKey::Arm(_) | ArgKey::Cell(_) => match arg_get(&get_node(sx, leaf, builder)?, key, builder) {
            Some(a) if pick(&a,builder) => Ok(unfold_in(used, None, sx, leaf, key, &[], builder)?.0),
            _ => Ok(*sx),
        },
        _ => Ok(*sx),
    }
}

pub fn default_pick(a: &S, builder: &SyntaxBuilder) -> bool {
    node_call(a, builder) || value_call(a, builder)
}
