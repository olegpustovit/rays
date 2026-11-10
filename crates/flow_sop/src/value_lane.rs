//! live drives, the arguments of a lowered graph that depend
//! on `t`. One cache retains the last plan and resolution; a static network reuses its result
//! without recomputation.
//!
//! Environment-owned cache. It retains one network plan and one resolution, bounded by that
//! network's live drives. Call outside `Ui.frame`, before submitting a cook; document literals
//! remain untouched.
use flow_check::port_type::PortValue;
use flow_eval::{State, Value};
use flow_ir::flow_ir::{executor, gpu, Sink};
use input::Frame;
use param::{KindView, Value as ParamValue};
use rays_diagnostic::Diagnostic;
use sop::context::{self, dependencies};
use sop::edit_graph;
use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::attribute_kernel;
use crate::curve;
use crate::network::{self, Network};
use crate::port::{self, Literal, Parameter, Port};

#[derive(Clone)]
struct Target {
    port: Port,
    parameter: Parameter,
    program: executor::Program,
}

#[derive(Clone)]
struct CurveTarget {
    target: Target,
    closed: bool,
    filled: bool,
}

#[derive(Clone)]
enum LateTarget {
    Text(Target),
    Curve(CurveTarget),
}

struct Plan {
    network: Network,
    targets: Vec<Target>,
    late: Vec<LateTarget>,
    states: Option<executor::Program>,
    time_dependent: bool,
    reads: dependencies::T,
}

/// The resolution of a network at one time.
#[derive(Clone)]
pub struct Resolved {
    pub geometry: edit_graph::T,
    pub applied: BTreeMap<Port, PortValue>,
    pub applied_text: BTreeMap<Port, Arc<[u8]>>,
    /// Live curve points remain a typed array and are not text-encoded.
    pub applied_points: BTreeMap<Port, Vec<(f64, f64, f64)>>,
    pub time_dependent: bool,
}

struct Cache {
    plan: Arc<Plan>,
    live: Frame,
    gpu: gpu::Config,
    result: Result<Resolved, Diagnostic>,
}

/// Environment-owned cache of one plan and one resolution.
pub struct Lane {
    previous: Mutex<Option<Cache>>,
    state: State,
}

pub fn create(state: Option<State>) -> Lane {
    Lane { previous: Mutex::new(None), state: state.unwrap_or_else(flow_eval::create_state) }
}

/// Clear cached resolution and fold values for a fresh playback.
pub fn reset(host_state: bool, lane: &Lane) {
    *lane.previous.lock().unwrap_or_else(std::sync::PoisonError::into_inner) = None;
    flow_eval::reset_state(&lane.state, host_state);
}

/// The host-input slices (time, pointer, keys, size, step) the network's drives and folds read;
/// `resolve` recomputes only when one of them differs.
pub fn dependencies(network: &Network) -> dependencies::T {
    let found = network
        .states
        .iter()
        .fold(dependencies::STATIC, |found, value| dependencies::union(found, attribute_kernel::input_reads(value)));
    let found = network
        .drives
        .values()
        .fold(found, |found, value| dependencies::union(found, attribute_kernel::input_reads(value)));
    if network.frame_nodes.is_empty() {
        found
    } else {
        dependencies::union(found, dependencies::one(dependencies::Fact::Input))
    }
}

fn live_error(message: &str) -> Diagnostic {
    port::error("E_LIVE", message)
}

fn compile(network: &Network, value: &Value) -> Result<executor::Program, Diagnostic> {
    executor::compile(
        value,
        &executor::CompileOptions {
            profile: network.profile.clone(),
            approx: network.approx.clone(),
            sink: Some(Sink::Sop_input),
            ..executor::CompileOptions::default()
        },
    )
}

fn curve_settings(network: &Network, target: &Target) -> Option<(bool, bool)> {
    let node_id = target.port.node;
    let found = edit_graph::find(&network.geometry, node_id)?;
    if target.port.path != curve::PARAMETER
        || edit_graph::node_factory_key(&network.geometry, node_id).as_deref() != Some(curve::KEY)
    {
        return None;
    }
    let fields = sop::node::parameter_fields(&found);
    let setting = |name: &str| {
        fields.iter().find_map(|field| match &field.current {
            ParamValue::Bool(value) if field.name == name => Some(*value),
            _ => None,
        })
    };
    Some((setting(curve::CLOSED_PARAMETER)?, setting(curve::FILLED_PARAMETER)?))
}

fn prepare(network: &Network) -> Result<Plan, Diagnostic> {
    let states = if network.states.is_empty() {
        None
    } else {
        Some(compile(network, &Value::List(Arc::new(network.states.clone())))?)
    };
    if network.drives.is_empty() {
        return Ok(Plan {
            network: network.clone(),
            targets: Vec::new(),
            late: Vec::new(),
            states,
            time_dependent: !network.states.is_empty() || !network.frame_nodes.is_empty(),
            reads: dependencies(network),
        });
    }
    network::validate(network)?;
    let mut ordered: Vec<Target> = Vec::new();
    for (port, value) in &network.drives {
        let parameter = network::parameter(network, port)?;
        let program = compile(network, value)?;
        ordered.push(Target { port: port.clone(), parameter, program });
    }
    let mut targets = Vec::new();
    let mut late = Vec::new();
    for target in ordered {
        if let Some((closed, filled)) = curve_settings(network, &target) {
            late.push(LateTarget::Curve(CurveTarget { target, closed, filled }));
        } else if target.parameter.ty.is_none() {
            late.push(LateTarget::Text(target));
        } else {
            targets.push(target);
        }
    }
    Ok(Plan {
        network: network.clone(),
        targets,
        late,
        states,
        time_dependent: true,
        reads: dependencies(network),
    })
}

fn live_port_value(value: &Value) -> Result<PortValue, Diagnostic> {
    match value {
        Value::Int(n) => Ok(PortValue::Int(*n)),
        Value::Float(f) => Ok(PortValue::Float(*f)),
        Value::Bool(b) => Ok(PortValue::Bool(*b)),
        Value::Vec3(x, y, z) => Ok(PortValue::Vec3(*x, *y, *z)),
        Value::Text(text) => match port::color_of_text(&String::from_utf8_lossy(text)) {
            Some((x, y, z)) => Ok(PortValue::Vec3(x, y, z)),
            None => Err(live_error(&format!("Live value is not a colour: {}", String::from_utf8_lossy(text)))),
        },
        _ => Err(live_error("Live value is not a number, bool, vec3 or colour")),
    }
}

/// Live text parameters. Curve points stay on their typed path below.
fn live_text(parameter: &Parameter, value: Value) -> Result<Arc<[u8]>, Diagnostic> {
    match value {
        Value::Text(text) => Ok(text),
        _ => Err(live_error(&format!("Live value does not fit {}", parameter.path))),
    }
}

fn live_points(value: Value) -> Result<Vec<(f64, f64, f64)>, Diagnostic> {
    match value {
        Value::List(values) => values
            .iter()
            .map(|v| match v {
                Value::Vec3(x, y, z) => Ok((*x, *y, *z)),
                _ => Err(live_error("Live list must hold vec3")),
            })
            .collect(),
        _ => Err(live_error("Live list must hold vec3")),
    }
}

fn force(gpu: &gpu::Config, reference: bool, state: &State, live: &Frame, program: &executor::Program, resolve: &flow_ir::core_exec::ResolveFn) -> Result<Value, Diagnostic> {
    match executor::force_display(Some(state), Some(resolve), reference, Some(gpu.clone()), program, live)? {
        executor::Displayed::Cpu(value) => Ok(value),
        executor::Displayed::Gpu(_) => Err(port::error("E_APPROX_SINK", "Live SOP values require (exact x).")),
    }
}

fn apply_geometry(geometry: &edit_graph::T, target: &Port, changes: &[(String, ParamValue)]) -> Result<edit_graph::T, Diagnostic> {
    edit_graph::apply_parameters(geometry, target.node, changes)
        .map(|(geometry, _)| geometry)
        .map_err(|message| port::error("E_TYPE", &message))
}

fn geometry_error(message: String) -> Diagnostic {
    port::error("E_GEOMETRY", &message)
}

fn same_points(old: &[(f64, f64, f64)], points: &[(f64, f64, f64)]) -> bool {
    old.len() == points.len() && old.iter().zip(points).all(|(a, b)| a.0 == b.0 && a.1 == b.1 && a.2 == b.2)
}

fn compute(
    gpu: &gpu::Config,
    state: &State,
    previous: Option<&Cache>,
    plan: &Plan,
    live: &Frame,
    resolve: &flow_ir::core_exec::ResolveFn,
) -> Result<Resolved, Diagnostic> {
    let reference = plan.network.reference;
    if let Some(program) = &plan.states {
        // Device state owns its holders beyond this scope; host consumers read them on demand.
        let scope = if gpu.fast && !reference { gpu.backend.as_ref().and_then(|b| b.residency(true)) } else { None };
        match scope {
            Some(scope) => {
                let outcome = executor::force(
                    program,
                    &executor::ForceOptions {
                        resident: Some(scope.executor().clone()),
                        resolve: Some(resolve.clone()),
                        state: Some(state.clone()),
                        reference,
                        ..executor::ForceOptions::default()
                    },
                    live,
                );
                scope.executor().close();
                outcome?;
            }
            None => {
                force(gpu, reference, state, live, program, resolve)?;
            }
        }
    }
    if plan.targets.is_empty() && plan.late.is_empty() && plan.network.frame_nodes.is_empty() {
        return Ok(Resolved {
            geometry: plan.network.geometry.clone(),
            applied: BTreeMap::new(),
            applied_text: BTreeMap::new(),
            applied_points: BTreeMap::new(),
            time_dependent: plan.time_dependent,
        });
    }
    let mut applied: BTreeMap<Port, PortValue> = BTreeMap::new();
    let previous: Option<(&Resolved, &Network)> = previous.and_then(|cache| match &cache.result {
        Ok(resolved) => Some((resolved, &cache.plan.network)),
        Err(_) => None,
    });
    let same_geometry = match previous {
        None => false,
        Some((_, old_network)) => edit_graph::ptr_eq(&plan.network.geometry, &old_network.geometry),
    };
    let mut geometry = match previous {
        Some((resolved, _)) if same_geometry => resolved.geometry.clone(),
        _ => plan.network.geometry.clone(),
    };
    if let (true, Some((old, old_network))) = (same_geometry, previous) {
        for port in old.applied.keys() {
            if plan.network.drives.contains_key(port) || edit_graph::find(&geometry, port.node).is_none() {
                continue;
            }
            let parameter = network::parameter(old_network, port)?;
            let changes = port::literal_changes(&parameter, &port::literal(&parameter))?;
            geometry = apply_geometry(&geometry, port, &changes)?;
        }
        for port in old.applied_points.keys() {
            if plan.network.drives.contains_key(port) {
                continue;
            }
            if let Some(found) = edit_graph::find(&plan.network.geometry, port.node) {
                geometry = edit_graph::replace_node(&found, &geometry).map_err(geometry_error)?;
            }
        }
    }
    for target in &plan.targets {
        let value = force(gpu, reference, state, live, &target.program, resolve)?;
        let (value, changes) = port::normalize(&target.parameter, &live_port_value(&value)?)?;
        applied.insert(target.port.clone(), value);
        if edit_graph::find(&plan.network.geometry, target.port.node).is_none() {
            continue;
        }
        if same_geometry && previous.is_some_and(|(old, _)| old.applied.get(&target.port) == Some(&value)) {
            continue;
        }
        geometry = apply_geometry(&geometry, &target.port, &changes)?;
    }
    let mut applied_text: BTreeMap<Port, Arc<[u8]>> = BTreeMap::new();
    let mut applied_points: BTreeMap<Port, Vec<(f64, f64, f64)>> = BTreeMap::new();
    for late in &plan.late {
        match late {
            LateTarget::Text(target) => {
                let value = force(gpu, reference, state, live, &target.program, resolve)?;
                let text = live_text(&target.parameter, value)?;
                applied_text.insert(target.port.clone(), text.clone());
                if edit_graph::find(&plan.network.geometry, target.port.node).is_none() {
                    continue;
                }
                if same_geometry && previous.is_some_and(|(old, _)| old.applied_text.get(&target.port) == Some(&text)) {
                    continue;
                }
                let literal = match target.parameter.fields.as_slice() {
                    [field] if matches!(field.kind, KindView::Choice(_)) => {
                        ParamValue::Choice(String::from_utf8_lossy(&text).into_owned())
                    }
                    _ => ParamValue::Text(text),
                };
                let changes = port::literal_changes(&target.parameter, &Literal::Scalar(literal))?;
                geometry = apply_geometry(&geometry, &target.port, &changes)?;
            }
            LateTarget::Curve(CurveTarget { target, closed, filled }) => {
                let value = force(gpu, reference, state, live, &target.program, resolve)?;
                let points = live_points(value)?;
                applied_points.insert(target.port.clone(), points.clone());
                let prior = previous.and_then(|(old, _)| old.applied_points.get(&target.port));
                if same_geometry && prior.is_some_and(|old| same_points(old, &points)) {
                    continue;
                }
                let option = |name: &str, default: bool| {
                    match applied.get(&Port { node: target.port.node, path: name.to_owned() }) {
                        Some(PortValue::Bool(value)) => *value,
                        _ => default,
                    }
                };
                let Some(source) = edit_graph::find(&geometry, target.port.node) else { continue };
                let replaced = curve::replace_points(
                    &source,
                    option(curve::CLOSED_PARAMETER, *closed),
                    option(curve::FILLED_PARAMETER, *filled),
                    &points,
                );
                geometry = edit_graph::replace_node(&replaced, &geometry).map_err(geometry_error)?;
            }
        }
    }
    for (id, rebuild) in &plan.network.frame_nodes {
        let Some(found) = edit_graph::find(&geometry, *id) else { continue };
        let rebuilt = rebuild(&plan.network, state, live, &found)?;
        geometry = edit_graph::replace_node(&rebuilt, &geometry).map_err(geometry_error)?;
    }
    Ok(Resolved { geometry, applied, applied_text, applied_points, time_dependent: plan.time_dependent })
}

fn same_gpu(a: &gpu::Config, b: &gpu::Config) -> bool {
    a.fast == b.fast
        && a.policy == b.policy
        && match (&a.backend, &b.backend) {
            (None, None) => true,
            (Some(x), Some(y)) => Arc::ptr_eq(x, y),
            _ => false,
        }
}

/// Evaluate the live drives at `time` through prepared Flow IR programs; only changed SOP ports
/// are applied while the literal graph is unchanged. Static networks reuse their successful or
/// failed result without recomputation.
pub fn resolve(
    gpu: Option<&gpu::Config>,
    live: Option<&Frame>,
    lane: &Lane,
    time: f64,
    network: &Network,
) -> Result<Resolved, Diagnostic> {
    let empty: attribute_kernel::GeometryLookup = Arc::new(|_| None);
    let resolve = crate::lisp_node::resolver(Arc::new(move |value| attribute_kernel::resolve(None, &empty, value)));
    resolve_with(gpu, live, lane, time, network, resolve)
}

pub(crate) fn resolve_with(
    gpu: Option<&gpu::Config>, live: Option<&Frame>, lane: &Lane, time: f64,
    network: &Network, resolve: flow_ir::core_exec::ResolveFn,
) -> Result<Resolved, Diagnostic> {
    let gpu = gpu.cloned().unwrap_or_else(gpu::default);
    let live = live.cloned().unwrap_or_else(|| input::at_time(time));
    let previous = lane.previous.lock().unwrap_or_else(std::sync::PoisonError::into_inner).take();
    if let Some(cache) = &previous {
        if network::ptr_eq(&cache.plan.network, network)
            && same_gpu(&cache.gpu, &gpu)
            && (!cache.plan.time_dependent || !context::input_changed(cache.plan.reads, &cache.live, &live))
        {
            let result = cache.result.clone();
            *lane.previous.lock().unwrap_or_else(std::sync::PoisonError::into_inner) = previous;
            return result;
        }
    }
    let plan = match &previous {
        Some(cache) if network::ptr_eq(&cache.plan.network, network) => Ok(cache.plan.clone()),
        _ => prepare(network).map(Arc::new),
    };
    let plan = match plan {
        Ok(plan) => plan,
        Err(diagnostic) => {
            *lane.previous.lock().unwrap_or_else(std::sync::PoisonError::into_inner) = previous;
            return Err(diagnostic);
        }
    };
    let mut outcome: Option<Resolved> = None;
    let transaction = flow_eval::transaction(&lane.state, &mut || {
        outcome = Some(compute(&gpu, &lane.state, previous.as_ref(), &plan, &live, &resolve)?);
        Ok(())
    });
    let result = match transaction {
        Ok(()) => Ok(outcome.expect("a settled transaction computed")),
        Err(diagnostic) => Err(diagnostic),
    };
    *lane.previous.lock().unwrap_or_else(std::sync::PoisonError::into_inner) =
        Some(Cache { plan, live, gpu, result: result.clone() });
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_static_network_resolves_to_its_own_geometry_and_is_reused() {
        let network = network::of_geometry(&edit_graph::empty());
        let lane = create(None);
        let first = resolve(None, None, &lane, 0.0, &network).expect("static resolves");
        assert!(edit_graph::ptr_eq(&first.geometry, &network.geometry));
        assert!(first.applied.is_empty() && !first.time_dependent);
        let again = resolve(None, None, &lane, 3.0, &network).expect("reused");
        assert!(edit_graph::ptr_eq(&again.geometry, &first.geometry));
        reset(true, &lane);
        assert!(resolve(None, None, &lane, 0.0, &network).is_ok());
    }
}
