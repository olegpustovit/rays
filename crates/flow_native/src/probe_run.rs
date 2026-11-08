//! Port of lib/flow_native/probe_run.ml: what the graph pane's probes run on, kept out of the
//! editor so it reaches no `flow_ir`. The exact frame/region compiler the canvas uses, and the
//! profile of its groups read as the footer's "ran on" line.
use flow_eval as e;
use flow_gpu::small_lru::List;
use flow_graph::probe::{Execution as ProbeExecution, Force};
use flow_ir::flow_ir::{Executor, Profile, Tier};
use flow_syntax::value::{ResidualId, Value};
use std::cell::RefCell;
use std::rc::Rc;

/// The profile of the executed groups (a snapshot of at most 512).
pub type reports = Vec<flow_ir::flow_ir::Execution>;

pub fn report_count(profile: &Profile::T) -> usize {
    Profile::execution_count(profile)
}

pub fn reports(profile: &Profile::T) -> reports {
    Profile::executions(profile)
}

pub fn no_reports() -> reports {
    Vec::new()
}

/// A probe that first runs adds a group: the count is what tells a footer to refresh.
pub fn same_count(a: &reports, b: &reports) -> bool {
    a.len() == b.len()
}

/// The probes' forcing function over the frame/region compiler, with its own bounded program cache
/// (64, most recent): per probed value, a compiled program of the document's own terms.
pub fn force() -> Force {
    let programs: Rc<RefCell<List<ResidualId, Executor::Program>>> = Rc::new(RefCell::new(List::new(64, Box::new(|_, _| {}))));
    Rc::new(move |state, elems, resolve, value, live| {
        let prepared = match value {
            Value::Residual(residual) => {
                let found = programs.borrow_mut().find(&|k, _| k == residual).cloned();
                match found {
                    Some(program) => Ok(program),
                    None => {
                        let program = Executor::compile(value, &Executor::CompileOptions::default());
                        if let Ok(p) = &program {
                            programs.borrow_mut().add(*residual, p.clone());
                        }
                        program
                    }
                }
            }
            _ => Executor::compile(value, &Executor::CompileOptions::default()),
        };
        match prepared {
            Ok(program) => {
                let o = Executor::ForceOptions { state: state.cloned(), elems: elems.map(|e| e.to_vec()), resolve, ..Default::default() };
                Executor::force(&program, &o, live)
            }
            Err(_) => e::Host::force_reference(value, live, state, elems, resolve),
        }
    })
}

fn covers(site: &[String], path: &[String]) -> bool {
    match (site, path) {
        ([a], [b, ..]) => a == b || b.starts_with(&format!("{a}#")),
        ([a, rest @ ..], [b, tail @ ..]) if a == b => covers(rest, tail),
        _ => false,
    }
}

fn prefix(xs: &[i64], ys: &[i64]) -> bool {
    match (xs, ys) {
        ([], _) => true,
        ([x, xs @ ..], [y, ys @ ..]) => x == y && prefix(xs, ys),
        _ => false,
    }
}

/// Where the node at a path ran, in the given graph instance at the given iterations.
pub fn find(reports: &reports, instance: Option<i64>, path: &[String], probes: &[i64]) -> Option<ProbeExecution> {
    let mut best: Option<(i64, &flow_ir::flow_ir::Execution)> = None;
    for report in reports {
        for (origin, site, iter) in &report.sites {
            if !((*origin < 0 || instance == Some(*origin)) && covers(site, path) && prefix(iter, probes)) {
                continue;
            }
            let score: i64 = site.iter().map(|s| s.len() as i64 + 1).sum();
            match best {
                Some((previous, _)) if previous >= score => {}
                _ => best = Some((score, report)),
            }
        }
    }
    best.map(|(_, report)| ProbeExecution {
        tier: match report.tier {
            Tier::Interp => "Interp",
            Tier::Core => "Core",
            Tier::Cpu_kernel => "CPU",
            Tier::Gpu => "GPU",
            Tier::Gpu_compile => "Gpu_compile",
            Tier::Gpu_readback => "Readback",
        }
        .to_owned(),
        group: report.owner.0.clone(),
        seconds: if report.owner.0 == path { Some(report.seconds) } else { None },
    })
}
