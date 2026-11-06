//! Attribute copy rule. OCaml's `Int_table`/`String_table` (`Hashtbl.Make`) have no other users and
//! are plain `HashMap`s in Rust.
use rdk_core::attribute;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rule {
    pub copy_owner: attribute::Owner,
    pub copy_pattern: String,
    pub copy_into: Option<String>,
}
