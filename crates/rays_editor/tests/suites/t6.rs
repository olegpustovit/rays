//! Editor suite t6: flow_shader, workspace images/ir, sop catalog, …
mod support {
    pub use crate::support::t6::*;
}
#[path = "../suite_t6/misc.rs"]
mod misc;
#[path = "../suite_t6/sopcat.rs"]
mod sopcat;
#[path = "../suite_t6/images.rs"]
mod images;

pub fn cases() -> Vec<(&'static str, fn())> {
    let mut cases: Vec<(&str, fn())> = Vec::new();
    cases.extend(misc::cases());
    cases.extend(sopcat::cases());
    cases.extend(images::cases());
    cases
}
