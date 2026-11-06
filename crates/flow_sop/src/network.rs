//! Port of lib/flow_sop/network.ml: SOP geometry with the live arguments of a lowered workspace
//! graph. Layout and probes stay in the editor document; resolution never changes these literal
//! records.
use flow_check::workspace::Paths;
use flow_eval::{State, Value};
use flow_ir::flow_ir::Profile;
use input::Frame;
use rays_diagnostic::Diagnostic;
use sop::edit_graph;
use sop::node;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use crate::port::{self, Parameter, Port};

/// Static-plan-bounded geometry zones rebuilt with an immutable fold snapshot on the initial
/// domain, before worker submission.
pub type FrameNode = Arc<dyn Fn(&Network, &State, &Frame, &node::T) -> Result<node::T, Diagnostic> + Send + Sync>;

static NEXT_IDENTITY: AtomicU64 = AtomicU64::new(1);

/// A lowered graph's geometry with its live drives. Every `with_*` makes a network with a fresh
/// identity ({@link ptr_eq}, OCaml `==`); a clone keeps it.
#[derive(Clone)]
pub struct Network {
    pub geometry: edit_graph::T,
    /// A workspace argument that depends on `t` (`Flow.Eval.is_live`), evaluated by
    /// `Value_lane::resolve` for each time (a scalar, a vec3, a colour text, or a list of vec3 for
    /// a text-encoded list parameter), keyed (node, argument name).
    pub drives: BTreeMap<Port, Value>,
    /// Frame folds of this workspace, advanced before its live parameters.
    pub states: Vec<Value>,
    pub frame_nodes: BTreeMap<i64, FrameNode>,
    pub profile: Option<Arc<Profile::T>>,
    /// Independent reference execution for parity checks and inspected cones.
    pub reference: bool,
    pub approx: Paths,
    identity: u64,
    _state_roots: flow_eval::Roots,
    _drive_roots: flow_eval::Roots,
}

fn fresh() -> u64 {
    NEXT_IDENTITY.fetch_add(1, Ordering::Relaxed)
}

/// OCaml `==` on networks.
pub fn ptr_eq(a: &Network, b: &Network) -> bool {
    a.identity == b.identity
}

fn error(code: &str, message: &str) -> Diagnostic {
    port::error(code, message)
}

pub fn of_geometry(geometry: &edit_graph::T) -> Network {
    Network {
        geometry: geometry.clone(),
        drives: BTreeMap::new(),
        states: Vec::new(),
        frame_nodes: BTreeMap::new(),
        profile: None,
        reference: false,
        approx: BTreeSet::new(),
        identity: fresh(),
        _state_roots: flow_eval::Roots::default(),
        _drive_roots: flow_eval::Roots::default(),
    }
}

pub fn with_approx(approx: Paths, network: &Network) -> Network {
    Network { approx, identity: fresh(), ..network.clone() }
}

pub fn with_profile(profile: Arc<Profile::T>, network: &Network) -> Network {
    Network { profile: Some(profile), identity: fresh(), ..network.clone() }
}

pub fn with_reference(reference: bool, network: &Network) -> Network {
    Network { reference, identity: fresh(), ..network.clone() }
}

pub fn with_states(states: Vec<Value>, network: &Network) -> Network {
    let state_roots = flow_eval::roots(&states);
    Network { states, _state_roots: state_roots, identity: fresh(), ..network.clone() }
}

pub fn with_frame_nodes(frame_nodes: BTreeMap<i64, FrameNode>, network: &Network) -> Network {
    Network { frame_nodes, identity: fresh(), ..network.clone() }
}

fn fields(network: &Network, node_id: i64) -> Result<Vec<param::FieldView>, Diagnostic> {
    match edit_graph::find(&network.geometry, node_id) {
        Some(found) => {
            let fields = node::parameter_fields(&found);
            if !fields.is_empty() {
                Ok(fields)
            } else {
                Ok(edit_graph::node_factory_fields(&network.geometry, node_id))
            }
        }
        None => Err(error("E_UNBOUND", &format!("No node {node_id}"))),
    }
}

fn parameters(network: &Network, node_id: i64) -> Result<Vec<Parameter>, Diagnostic> {
    port::parameters(false, &fields(network, node_id)?)
}

pub fn parameter(network: &Network, target: &Port) -> Result<Parameter, Diagnostic> {
    port::find_parameter(&parameters(network, target.node)?, &target.path)
}

/// A slot and a parameter never share a name; every drive targets a parameter of a node.
fn validate_node(network: &Network, node_id: i64) -> Result<(), Diagnostic> {
    let found = parameters(network, node_id)?;
    let slots = edit_graph::node_slot_names(&network.geometry, node_id).unwrap_or_default();
    if found.iter().any(|p| slots.contains(&p.path)) {
        Err(error("E_PORT", "A slot and parameter have the same name"))
    } else {
        Ok(())
    }
}

pub fn validate(network: &Network) -> Result<(), Diagnostic> {
    for info in edit_graph::inspect(&network.geometry) {
        validate_node(network, info.id)?;
    }
    for target in network.drives.keys() {
        parameter(network, target)?;
    }
    Ok(())
}

pub fn with_drives(drives: BTreeMap<Port, Value>, network: &Network) -> Result<Network, Diagnostic> {
    let drive_roots = flow_eval::roots(drives.values());
    let next = Network { drives, _drive_roots: drive_roots, identity: fresh(), ..network.clone() };
    validate(&next)?;
    Ok(next)
}

pub fn with_geometry(geometry: &edit_graph::T, network: &Network) -> Result<Network, Diagnostic> {
    if edit_graph::ptr_eq(geometry, &network.geometry) {
        return Ok(network.clone());
    }
    let next = Network { geometry: geometry.clone(), identity: fresh(), ..network.clone() };
    validate(&next)?;
    Ok(next)
}

/// Apply validated node fields without changing slots or drive targets.
pub fn apply_parameters(node_id: i64, changes: &[(String, param::Value)], network: &Network) -> Result<Network, Diagnostic> {
    let (geometry, _) = edit_graph::apply_parameters(&network.geometry, node_id, changes)
        .map_err(|message| error("E_TYPE", &message))?;
    if edit_graph::ptr_eq(&geometry, &network.geometry) {
        return Ok(network.clone());
    }
    let next = Network { geometry, identity: fresh(), ..network.clone() };
    validate_node(&next, node_id)?;
    for target in next.drives.keys() {
        if target.node == node_id {
            parameter(&next, target)?;
        }
    }
    Ok(next)
}

pub fn remove_nodes(ids: &[i64], network: &Network) -> Result<Network, Diagnostic> {
    let removed: BTreeSet<i64> = ids.iter().copied().collect();
    let geometry_ids: Vec<i64> =
        ids.iter().copied().filter(|id| edit_graph::find(&network.geometry, *id).is_some()).collect();
    if geometry_ids.is_empty() {
        return Ok(network.clone());
    }
    Ok(Network {
        geometry: edit_graph::remove_nodes(&geometry_ids, &network.geometry),
        frame_nodes: network
            .frame_nodes
            .iter()
            .filter(|(id, _)| !removed.contains(id))
            .map(|(id, f)| (*id, f.clone()))
            .collect(),
        drives: network.drives.iter().filter(|(t, _)| !removed.contains(&t.node)).map(|(t, v)| (t.clone(), v.clone())).collect(),
        identity: fresh(),
        ..network.clone()
    })
}

/// Retitle a node (`Network.relabel`); an empty label changes nothing.
pub fn relabel(node_id: i64, label: &str, network: &Network) -> Result<Network, Diagnostic> {
    if label.trim().is_empty() {
        return Ok(network.clone());
    }
    match edit_graph::find(&network.geometry, node_id) {
        None => Err(error("E_UNBOUND", &format!("No node {node_id}"))),
        Some(found) => {
            let geometry = edit_graph::replace_node(&node::relabel(label, &found), &network.geometry)
                .map_err(|message| error("E_GEOMETRY", &message))?;
            if edit_graph::ptr_eq(&geometry, &network.geometry) {
                Ok(network.clone())
            } else {
                Ok(Network { geometry, identity: fresh(), ..network.clone() })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identity_changes_with_every_edit() {
        let network = of_geometry(&edit_graph::empty());
        let same = network.clone();
        assert!(ptr_eq(&network, &same));
        let next = with_reference(true, &network);
        assert!(!ptr_eq(&network, &next));
        assert!(next.reference);
        assert!(validate(&next).is_ok());
        let kept = with_geometry(&network.geometry, &network).expect("same geometry");
        assert!(ptr_eq(&kept, &network));
    }
}
