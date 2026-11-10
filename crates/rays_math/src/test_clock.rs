// Readings never go back, and two within a microsecond or two
// of work differ: no step of a millisecond hides a short run.
use crate::clock;

#[test]
fn clock_is_monotonic_with_resolution_finer_than_100_microseconds() {
    let first = clock::now();
    let mut step = f64::INFINITY;
    let mut last = first;
    for _ in 0..10 {
        // Bounded spin: a clock that never advances fails here instead of hanging.
        let mut next = clock::now();
        let mut spins = 0_u32;
        while next <= last {
            spins += 1;
            assert!(spins < 10_000_000, "clock did not advance past {last}");
            next = clock::now();
        }
        step = step.min(next - last);
        last = next;
    }
    assert!(last > first);
    assert!(step < 1e-4);
}
