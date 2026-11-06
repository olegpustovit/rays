//! Port of lib/flow_sop/test_iterate.ml: `iterate` in one cook node, against a direct smoothing.
use super::support::*;
use crate::lower;
use rdk::geometry as geo;

const POINTS: usize = 40;

const TEMPLATE: &str = r#"(workspace iterate
  (defnode sop/smooth_loop [(geo : geometry) (passes : int 1 :min 0 :max 100000000)]
    (let* [n (sop/count geo :points)
           lo (map (fn [(i : int)] (int/mod (int/add i (int/sub n 1)) n)) (array/range n :type int))
           hi (map (fn [(i : int)] (int/mod (int/add i 1) n)) (array/range n :type int))]
      (sop/with_attr geo :P (iterate passes (sop/attr geo :P)
        (fn [p] (exact (map (fn [(i : int)] (* 0.5 (+ (array/nth p (array/nth lo i)) (array/nth p (array/nth hi i)))))
          (array/range n :type int))))))))
  (defnode sop/relax_loop [(geo : geometry) (passes : int 1 :min 0 :max 100000000)]
    (let* [n (sop/count geo :points)
           lo (map (fn [(i : int)] (int/mod (int/add i (int/sub n 1)) n)) (array/range n :type int))
           hi (map (fn [(i : int)] (int/mod (int/add i 1) n)) (array/range n :type int))
           spike (exact (map (fn [(i : int)] (if (int/eq i 0) 1.0 0.0)) (array/range n :type int)))]
      (sop/with_attr geo :w :type float (iterate passes spike
        (fn [w] (exact (map (fn [(i : int)] (/ (+ (+ (array/nth w (array/nth lo i)) (array/nth w i)) (array/nth w (array/nth hi i))) 3.0))
          (array/range n :type int))))
        :until (fn [(prev : (array float)) (next : (array float))] (< (array/max (exact (map (fn [a b] (abs (- a b))) prev next))) 0.01))))))
  (graph g :context sop (KIND (sop/line :points N) :passes PASSES)))"#;

fn text(kind: &str, passes: i64) -> String {
    TEMPLATE.replace("KIND", kind).replace("PASSES", &passes.to_string()).replace(" N)", &format!(" {POINTS})"))
}

fn lower_ref(reference: bool, text: &str) -> sop::node::T {
    lower_text(lower::Options { reference, ..lower::Options::default() }, text).1
}

fn neighbours(n: usize) -> Vec<(usize, usize)> {
    (0..n).map(|i| ((i + n - 1) % n, (i + 1) % n)).collect()
}

fn relax_step(lo_hi: &[(usize, usize)], a: &[f64]) -> Vec<f64> {
    a.iter().enumerate().map(|(i, x)| (a[lo_hi[i].0] + x + a[lo_hi[i].1]) / 3.0).collect()
}

fn smooth_step(lo_hi: &[(usize, usize)], a: &[f64]) -> Vec<f64> {
    (0..a.len()).map(|i| 0.5 * (a[lo_hi[i].0] + a[lo_hi[i].1])).collect()
}

fn planes(g: &geo::T) -> (Vec<f64>, Vec<f64>, Vec<f64>) {
    let p = geo::positions(g);
    let pts: Vec<(f64, f64, f64)> = (0..rdk::packed::Float3::length(p)).map(|i| rdk::packed::Float3::get(p, i).unwrap()).collect();
    (pts.iter().map(|p| p.0).collect(), pts.iter().map(|p| p.1).collect(), pts.iter().map(|p| p.2).collect())
}

#[test]

fn iterate_smoothing_matches_direct_computation() {
    let base = cook_once(1, &lower_ref(false, &text("sop/smooth_loop", 0)));
    let (x0, y0, z0) = planes(&base);
    let lo_hi = neighbours(POINTS);
    let expected = |passes: usize| -> Vec<u8> {
        let (mut x, mut y, mut z) = (x0.clone(), y0.clone(), z0.clone());
        for _ in 0..passes {
            (x, y, z) = (smooth_step(&lo_hi, &x), smooth_step(&lo_hi, &y), smooth_step(&lo_hi, &z));
        }
        geometry_bytes(&geo::with_positions(rdk::packed::Float3::of_owned(x, y, z).unwrap(), &base).unwrap())
    };
    for passes in [0usize, 1, 7, 500] {
        let want = expected(passes);
        for reference in [false, true] {
            for domains in [1i64, 8] {
                let actual = cook_once(domains, &lower_ref(reference, &text("sop/smooth_loop", passes as i64)));
                assert!(geometry_bytes(&actual) == want, "iterate passes={passes} reference={reference} domains={domains} differs from the direct smoothing");
            }
        }
    }
    // the pass count is data, not graph structure: the plan has the same nodes for 1 and 5000 passes
    let plan_nodes = |passes: i64| lower::workspace(None, Vec::new(), lower::Options::default(), &parse(&text("sop/smooth_loop", passes))).unwrap().plan.nodes.len();
    assert_eq!(plan_nodes(1), plan_nodes(5000), "the pass count changed the number of cook nodes");
    // a negative count returns the seed
    assert!(geometry_bytes(&cook_once(1, &lower_ref(false, &text("sop/smooth_loop", -3)))) == expected(0), "a negative count changed the seed");
    // a parameter change is an edit of the call's text: the card relowers
    let node = lower_ref(false, &text("sop/smooth_loop", 1));
    let mut s = new_session(64, 64 * 1024 * 1024);
    let ctx = context(1, 257);
    assert!(geometry_bytes(&cook(&mut s, &ctx, &node)) == expected(1), "one pass");
    let changed = lower_ref(false, &text("sop/smooth_loop", 7));
    assert!(geometry_bytes(&cook(&mut s, &ctx, &changed)) == expected(7), "parameter change to seven passes");
    sop::session::close(&mut s);
}

fn weights(g: &geo::T) -> Vec<f64> {
    let a = geo::find_attribute(rdk::attribute::Owner::Point, "w", g).expect("w attribute");
    match rdk::attribute::storage(&a) {
        Ok(rdk::attribute::Storage::Float(w)) => w,
        _ => panic!("w is not a float attribute"),
    }
}

#[test]

fn iterate_until_stops_after_the_satisfying_pass() {
    let lo_hi = neighbours(POINTS);
    let w0: Vec<f64> = (0..POINTS).map(|i| if i == 0 { 1.0 } else { 0.0 }).collect();
    let (passes_until, want) = {
        let mut k = 0;
        let mut w = w0.clone();
        loop {
            let next = relax_step(&lo_hi, &w);
            let delta = w.iter().zip(next.iter()).map(|(a, b)| (a - b).abs()).fold(f64::NEG_INFINITY, f64::max);
            k += 1;
            if delta < 0.01 {
                break (k, next);
            }
            w = next;
        }
    };
    assert!(passes_until > 5 && passes_until < 500, "the :until fixture must stop mid-run");
    for reference in [false, true] {
        for domains in [1i64, 8] {
            for passes in [1, passes_until - 1, passes_until, passes_until + 3, 500] {
                let w = weights(&cook_once(domains, &lower_ref(reference, &text("sop/relax_loop", passes))));
                let target = if passes < passes_until {
                    let mut w = w0.clone();
                    for _ in 0..passes {
                        w = relax_step(&lo_hi, &w);
                    }
                    w
                } else {
                    want.clone()
                };
                assert!(w.len() == target.len() && w.iter().zip(target.iter()).all(|(a, b)| a.to_bits() == b.to_bits()), ":until passes={passes} reference={reference} domains={domains} did not stop at pass {passes_until}");
            }
        }
    }
}
