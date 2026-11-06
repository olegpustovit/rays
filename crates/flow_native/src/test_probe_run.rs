//! Tests of the probe glue: where a node ran is the best-covering site, and the forcing function
//! evaluates through the frame/region compiler.
use crate::probe_run;
use flow_ir::flow_ir::{Execution, Tier};
use flow_syntax::value::Value;

fn path(parts: &[&str]) -> Vec<String> {
    parts.iter().map(|s| (*s).to_owned()).collect()
}

fn report(owner: &[&str], sites: Vec<(i64, Vec<String>, Vec<i64>)>, tier: Tier) -> Execution {
    Execution { owner: (path(owner), Vec::new()), sites, tier, seconds: 0.5 }
}

#[test]
fn find_picks_the_longest_covering_site() {
    let reports = vec![
        report(&["g", "a"], vec![(0, path(&["g"]), Vec::new())], Tier::Core),
        report(&["g", "a", "b"], vec![(0, path(&["g", "a"]), vec![1])], Tier::Gpu),
    ];
    let found = probe_run::find(&reports, Some(0), &path(&["g", "a", "b"]), &[1, 2]).expect("found");
    assert_eq!((found.tier.as_str(), found.group), ("GPU", path(&["g", "a", "b"])));
    assert_eq!(found.seconds, Some(0.5));
    // an iteration the site does not cover falls back to the shorter site
    let other = probe_run::find(&reports, Some(0), &path(&["g", "a", "b"]), &[2]).expect("found");
    assert_eq!(other.tier, "Core");
    assert_eq!(other.seconds, None);
    assert!(probe_run::find(&reports, Some(1), &path(&["g", "a"]), &[]).is_none());
    // a nested node `holder#input` is covered by its holder's leaf
    let nested = vec![report(&["g"], vec![(-1, path(&["g", "x"]), Vec::new())], Tier::Cpu_kernel)];
    assert_eq!(probe_run::find(&nested, None, &path(&["g", "x#in"]), &[]).expect("found").tier, "CPU");
}

#[test]
fn counts_tell_a_footer_to_refresh() {
    let a = probe_run::no_reports();
    let b = vec![report(&["g"], Vec::new(), Tier::Interp)];
    assert!(probe_run::same_count(&a, &a));
    assert!(!probe_run::same_count(&a, &b));
}

#[test]
fn force_evaluates_plain_values() {
    let force = probe_run::force();
    let v = force(None, None, None, &Value::Int(3), &input::at_time(0.)).expect("forced");
    assert_eq!(v, Value::Int(3));
}
