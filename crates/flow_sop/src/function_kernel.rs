//! Port of lib/flow_sop/function_kernel.ml(i): language-owned compiled bulk functions behind the Sop
//! payload boundary.
use std::io::Write as _;
use crate::attribute_kernel;
use flow_eval as e;
use flow_ir::flow_ir::executor;
use flow_ir::packed;
use flow_syntax::ty::FnSignature;
use flow_syntax::value::{self, FnId, ResidualId, Value};
use rays_diagnostic::Diagnostic;
use sop::kernel::{self, Column, Runner};
use sop::node::{self, build, CookMode, Cooked};
use sop::{context, payload};
use std::collections::HashSet;
use std::sync::Arc;

fn column_value(column: &Column) -> Value {
    match column {
        Column::Floats(xs) => Value::Floats(1, Arc::new(xs.clone())),
        Column::Vec3s(xs) => Value::Floats(3, Arc::new(xs.clone())),
    }
}

fn to_column(value: Value) -> Result<Column, Diagnostic> {
    match value {
        Value::Floats(1, xs) => Ok(Column::Floats((*xs).clone())),
        Value::Floats(3, xs) => Ok(Column::Vec3s((*xs).clone())),
        _ => Err(value::fail("E_KERNEL_FORM", "Bulk function did not produce a packed column.")),
    }
}

fn add_bytes(a: i64, b: i64) -> i64 {
    a.saturating_add(b)
}

/// Bytes of the packed data a function value keeps alive through its captures.
fn storage(values: &[Value]) -> i64 {
    struct Seen {
        functions: HashSet<FnId>,
        residuals: HashSet<ResidualId>,
    }
    fn visit(seen: &mut Seen, value: &Value) -> i64 {
        match value {
            Value::Floats(1..=4, xs) => xs.len() as i64 * 8,
            Value::Text(s) => s.len() as i64,
            Value::List(xs) => xs.iter().fold(0, |bytes, x| add_bytes(bytes, visit(seen, x))),
            v @ (Value::Record(_) | Value::Struct(_)) => fields(seen, v.geo_fields().unwrap()),
            Value::Fn(f) if seen.functions.insert(*f) => fields(seen, &e::residual::function_bindings(*f)),
            Value::Residual(r) if seen.residuals.insert(*r) => match e::residual::residual_view(*r) {
                Ok(view) => view.bindings.iter().fold(0, |bytes, (_, x)| add_bytes(bytes, visit(seen, x))),
                Err(_) => 0,
            },
            _ => 0,
        }
    }
    fn fields(seen: &mut Seen, fs: &[(String, Value)]) -> i64 {
        fs.iter().fold(0, |bytes, (_, x)| add_bytes(bytes, visit(seen, x)))
    }
    let mut seen = Seen { functions: HashSet::new(), residuals: HashSet::new() };
    values.iter().fold(0, |bytes, x| add_bytes(bytes, visit(&mut seen, x)))
}

/// A node whose payload is a kernel over the bulk function `fn_`: cooks nothing until a consumer
/// prepares it with its columns.
#[allow(clippy::too_many_arguments)]
pub fn node(
    state: Option<&e::State>,
    reference: bool,
    elems: Option<&[(String, Value)]>,
    identity: i64,
    signature: &FnSignature,
    function: FnId,
    sources: &[i64],
    inputs: Vec<node::T>,
) -> node::T {
    // ponytail: fresh binding identity recooks after equivalent re-lowering; structural body/capture
    // keys can replace it if this becomes measurable.
    let mut parameters = identity.to_string().into_bytes();
    if let Some(bindings) = elems {
        parameters.extend(value::key_of(&|r: ResidualId| r.0, &Value::Record(bindings.to_vec())));
    }
    if let Some(state) = state {
        write!(parameters, "{}", e::state_stamp(state)).unwrap();
    }
    let (signature, sources_owned, state, elems_owned) = (signature.clone(), sources.to_vec(), state.cloned(), elems.map(|e| e.to_vec()));
    let function_value = Value::Fn(function);
    let state_values = state.as_ref().map(e::host::state_values);
    let roots = e::roots(std::iter::once(&function_value)
        .chain(elems_owned.iter().flatten().map(|(_, value)| value))
        .chain(state_values.iter().flatten()));
    let dependencies = attribute_kernel::input_reads(&Value::Fn(function));
    let held_inputs = inputs.clone();
    build::make(
        build::Args { parameters, cook_mode: CookMode::Generic, dependencies, inputs, ..build::args("flow.function") },
        None,
        Arc::new(move |_node_id, _context, payloads| {
            let captured = Arc::new(payloads.to_vec());
            let geometry_capture: kernel::GeometryCapture = {
                let (captured, sources) = (captured.clone(), sources_owned.clone());
                Arc::new(move |id| sources.iter().position(|s| *s == id).and_then(|i| payload::geometry(&captured[i]).ok()))
            };
            let resolve: packed::Resolve = {
                let capture = geometry_capture.clone();
                let lookup: attribute_kernel::GeometryLookup = capture;
                Arc::new(move |v| attribute_kernel::resolve(None, &lookup, v))
            };
            let prepare: kernel::Prepare = {
                let (signature, sources, inputs, state, elems, resolve) =
                    (signature.clone(), sources_owned.clone(), held_inputs.clone(), state.clone(), elems_owned.clone(), resolve);
                let roots = roots.clone();
                Arc::new(move |columns: &[Column]| -> Result<Runner, Diagnostic> {
                    let arrays: Vec<Value> = columns.iter().map(column_value).collect();
                    let values = e::host::map_function(None, None, &signature, function, &arrays)?;
                    let program = attribute_kernel::prepare(None, true, None, None, &sources, &inputs, &values)?;
                    let Some(packed_region) = executor::packed_region(&program) else {
                        return Err(value::fail("E_KERNEL_FORM", "This function cannot compile to a packed kernel."));
                    };
                    let (state, elems, resolve) = (state.clone(), elems.clone(), resolve.clone());
                    let roots = roots.clone();
                    Ok(Box::new(move |ctx: &context::T| {
                        let _roots = &roots;
                        if context::cancelled(ctx) {
                            return Err(value::fail("E_CANCELLED", "Bulk function cancelled."));
                        }
                        let live = context::input(ctx);
                        let forced = if reference {
                            let options = executor::ForceOptions {
                                state: state.clone(),
                                elems: elems.clone(),
                                resolve: Some(resolve.clone()),
                                reference: true,
                                ..Default::default()
                            };
                            executor::force(&program, &options, live)?
                        } else {
                            let options = packed::ForceOptions {
                                state: state.clone(),
                                elems: elems.clone(),
                                resolve: Some(resolve.clone()),
                                strict: true,
                                ..Default::default()
                            };
                            packed::force(&packed_region, live, &options)?
                        };
                        if context::cancelled(ctx) {
                            return Err(value::fail("E_CANCELLED", "Bulk function cancelled."));
                        }
                        to_column(forced)
                    }))
                })
            };
            // Captured payloads stay alive through the resolver even if their own cache entries are
            // evicted. Account conservatively for that storage.
            let mut roots: Vec<Value> = vec![Value::Fn(function)];
            roots.extend(elems_owned.iter().flatten().map(|(_, v)| v.clone()));
            if let Some(state) = &state {
                roots.extend(e::host::state_values(state));
            }
            let mut bytes = storage(&roots);
            for payload in payloads {
                for (_, size) in payload::payload_components(payload) {
                    bytes = add_bytes(bytes, size);
                }
            }
            let made = kernel::create(Some(geometry_capture), bytes, prepare);
            Ok(Cooked { payload: payload::T::Kernel(made), instances: None })
        }),
    )
}
