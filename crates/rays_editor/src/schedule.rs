//! Effect- and dependency-aware cook scheduler. It fires initially, after a committed cook
//! parameter change, after `force`, and whenever the sketch clock changed and any reachable node
//! declares `Time` or `Frame`. While a primary-pointer edit is held it cooks only when `live` and
//! the worker is idle; otherwise only the latest desired cook is retained until release.
#![allow(non_camel_case_types)]
use input::MouseButton;
use param::Effects;
use rays::frame;
use sop::context::{self, dependencies as deps};
use sop::node;

#[derive(Clone)]
pub struct Schedule {
    pub initialized: bool,
    pub dirty: bool,
    pub graphs: Vec<node::T>,
    pub dependencies: deps::T,
    /// the host input the last step saw
    pub input: Option<input::Frame>,
}

pub fn initial() -> Schedule {
    Schedule { initialized: false, dirty: false, graphs: Vec::new(), dependencies: deps::STATIC, input: None }
}

pub fn step(
    live: bool,
    input: Option<&input::Frame>,
    value: &Schedule,
    graphs: &[node::T],
    effects: Effects,
    context_changed: bool,
    force: bool,
    busy: bool,
    frame: &frame::Frame,
) -> (Schedule, bool) {
    let dirty = value.dirty || effects.cook || force;
    let same = graphs.len() == value.graphs.len() && graphs.iter().zip(value.graphs.iter()).all(|(a, b)| node::ptr_eq(a, b));
    let dependencies = if same {
        value.dependencies
    } else {
        graphs.iter().fold(deps::STATIC, |union, graph| deps::union(union, node::graph_dependencies(graph)))
    };
    // a graph recooks when an input slice it reads differs: a stopped clock, a still pointer or an
    // unread key leave every idle frame the same
    let dynamic = (match (input, value.input.as_ref()) {
        (Some(now), Some(before)) => context::input_changed(dependencies, before, now),
        _ => [deps::Fact::Input, deps::Fact::Pointer, deps::Fact::Keys, deps::Fact::Size, deps::Fact::Step]
            .iter()
            .any(|fact| deps::mem(*fact, dependencies)),
    }) || (context_changed && (deps::mem(deps::Fact::Time, dependencies) || deps::mem(deps::Fact::Frame, dependencies)));
    let held = frame::mouse_down(MouseButton::LeftButton, frame);
    let desired = !value.initialized || dirty || dynamic;
    let urgent = !value.initialized || effects.cook || force;
    // Live: while a drag holds the pointer, cook whenever the worker is idle, so each finished cook
    // shows and the next picks up the latest value.
    let fire = if held { live && desired && !busy } else { desired && (!busy || urgent) };
    (
        Schedule { initialized: value.initialized || fire, dirty: desired && !fire, graphs: graphs.to_vec(), dependencies, input: input.cloned() },
        fire,
    )
}
