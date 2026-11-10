use std::{cell::RefCell, marker::PhantomData, rc::Rc};

#[derive(Clone, Debug, PartialEq)]
pub struct Entry { pub phase: String, pub calls: i64, pub seconds: f64 }
#[derive(Clone, Debug, PartialEq)]
pub struct PhaseTimer { pub total: f64, pub entries: Vec<Entry> }
pub type T = PhaseTimer;
pub fn empty() -> PhaseTimer { PhaseTimer { total: 0.0, entries: Vec::new() } }
struct Scope { phase: String, started: f64, children: f64 }
struct Sample { clock: Box<dyn FnMut() -> f64>, started: f64, last: f64, entries: Vec<Entry>, stack: Vec<Scope>, enabled: bool }
thread_local! { static CURRENT: RefCell<Option<Sample>> = const { RefCell::new(None) }; }
fn now(sample: &mut Sample) -> f64 {
    sample.last = sample.last.max((sample.clock)());
    sample.last
}

pub struct SampleGuard { finished: bool, owns_sample: bool, _not_send: PhantomData<Rc<()>> }
impl SampleGuard {
    pub fn start_with_clock(clock: Box<dyn FnMut() -> f64 + 'static>) -> Self {
        let owns_sample = CURRENT.with(|current| {
            let mut current = current.borrow_mut();
            if current.is_some() { false } else {
                let mut clock = clock;
                let started = clock();
                *current = Some(Sample { clock, started, last: started, entries: Vec::new(), stack: Vec::new(), enabled: true });
                true
            }
        });
        Self { finished: false, owns_sample, _not_send: PhantomData }
    }
    pub fn finish(mut self) -> PhaseTimer {
        self.finished = true;
        CURRENT.with(|current| {
            let mut current = current.borrow_mut();
            if self.owns_sample {
                current.take().map_or_else(empty, |mut sample| PhaseTimer { total: now(&mut sample) - sample.started, entries: sorted_entries(sample.entries) })
            } else {
                current.as_mut().map_or_else(empty, |sample| PhaseTimer { total: now(sample) - sample.started, entries: sorted_entries(sample.entries.clone()) })
            }
        })
    }
}
impl Drop for SampleGuard {
    fn drop(&mut self) { if self.owns_sample && !self.finished { CURRENT.with(|current| { current.replace(None); }); } }
}

pub struct MeasureGuard { active: bool, _not_send: PhantomData<Rc<()>> }
pub fn measure(phase: &str) -> MeasureGuard {
    let active = CURRENT.with(|current| {
        let mut current = current.borrow_mut();
        if let Some(sample) = current.as_mut().filter(|sample| sample.enabled) {
            let started = now(sample);
            sample.stack.push(Scope { phase: phase.to_owned(), started, children: 0.0 });
            true
        } else { false }
    });
    MeasureGuard { active, _not_send: PhantomData }
}
impl Drop for MeasureGuard {
    fn drop(&mut self) {
        if !self.active { return; }
        CURRENT.with(|current| {
            let mut current = current.borrow_mut();
            let Some(sample) = current.as_mut() else { return; };
            let elapsed = now(sample) - sample.stack.last().map_or(sample.last, |scope| scope.started);
            let Some(scope) = sample.stack.pop() else { return; };
            let elapsed = elapsed.max(0.0);
            if let Some(parent) = sample.stack.last_mut() { parent.children += elapsed; }
            let seconds = (elapsed - scope.children).max(0.0);
            if let Some(entry) = sample.entries.iter_mut().find(|entry| entry.phase == scope.phase) {
                entry.calls += 1;
                entry.seconds += seconds;
            } else { sample.entries.push(Entry { phase: scope.phase, calls: 1, seconds }); }
        });
    }
}

pub struct WithoutGuard { previous: Option<Sample>, _not_send: PhantomData<Rc<()>> }
pub fn without() -> WithoutGuard {
    let previous = CURRENT.with(|current| current.replace(None));
    WithoutGuard { previous, _not_send: PhantomData }
}
impl Drop for WithoutGuard {
    fn drop(&mut self) {
        if let Some(previous) = self.previous.take() {
            CURRENT.with(|current| { current.replace(Some(previous)); });
        }
    }
}

fn sorted_entries(mut entries: Vec<Entry>) -> Vec<Entry> { entries.sort_by(|a, b| a.phase.cmp(&b.phase)); entries }
pub fn calls(timer: &PhaseTimer, phase: &str) -> i64 { timer.entries.iter().find(|entry| entry.phase == phase).map_or(0, |entry| entry.calls) }
pub fn seconds(timer: &PhaseTimer, phase: &str) -> f64 { timer.entries.iter().find(|entry| entry.phase == phase).map_or(0.0, |entry| entry.seconds) }
pub fn summary(timer: &PhaseTimer) -> String { timer.entries.iter().map(|entry| format!("{} {:.1} ms", entry.phase, entry.seconds * 1000.0)).collect::<Vec<_>>().join(" · ") }

#[cfg(test)]
mod tests {
    use super::*;
    use std::{cell::Cell, rc::Rc};
    fn clock(time: Rc<Cell<f64>>) -> Box<dyn FnMut() -> f64> { Box::new(move || time.get()) }
    //  exercises actual unwinding, not only guard drop order.
    #[test]
    fn panics_restore_suspended_samples_and_clear_aborted_samples() {
        use std::panic::{catch_unwind, AssertUnwindSafe};
        let time = Rc::new(Cell::new(0.0));
        let sample = SampleGuard::start_with_clock(clock(time.clone()));
        {
            let _lower = measure("lower");
            time.set(1.0);
            assert!(catch_unwind(AssertUnwindSafe(|| {
                let _without = without();
                let _evaluate = measure("evaluate");
                time.set(2.0);
                panic!("body");
            })).is_err());
            {
                let _check = measure("check");
                time.set(3.0);
            }
        }
        let timer = sample.finish();
        assert_eq!(calls(&timer, "lower"), 1);
        assert_eq!(seconds(&timer, "lower"), 2.0);
        assert_eq!(calls(&timer, "evaluate"), 0);
        assert_eq!(calls(&timer, "check"), 1);
        assert!(catch_unwind(AssertUnwindSafe(|| {
            let _sample = SampleGuard::start_with_clock(clock(time.clone()));
            let _phase = measure("aborted");
            panic!("outside");
        })).is_err());
        drop(measure("outside"));
        assert!(SampleGuard::start_with_clock(clock(time)).finish().entries.is_empty());
    }
    #[test]
    fn injected_clock_tracks_nested_time_and_guard_restoration() {
        let time = Rc::new(Cell::new(0.0));
        let sample = SampleGuard::start_with_clock(clock(time.clone()));
        {
            let _outer = measure("outer");
            time.set(1.0);
            { let _inner = measure("inner"); time.set(3.0); }
            time.set(4.0);
            { let _inner = measure("inner"); time.set(5.0); }
            time.set(6.0);
        }
        let timer = sample.finish();
        assert_eq!(timer.total, 6.0);
        assert_eq!(seconds(&timer, "outer"), 3.0);
        assert_eq!(calls(&timer, "inner"), 2);
        assert_eq!(seconds(&timer, "inner"), 3.0);
        assert_eq!(summary(&timer), "inner 3000.0 ms · outer 3000.0 ms");

        let time = Rc::new(Cell::new(6.0));
        let sample = SampleGuard::start_with_clock(clock(time.clone()));
        {
            let _lower = measure("lower");
            time.set(7.0);
            // the guards a panic would unwind through, dropped in the same order
            {
                let _without = without();
                let _unmeasured = measure("evaluate");
            }
            time.set(8.0);
            {
                let _check = measure("check");
                time.set(9.0);
            }
        }
        let timer = sample.finish();
        assert_eq!(seconds(&timer, "lower"), 2.0);
        assert_eq!(calls(&timer, "evaluate"), 0);
        assert_eq!(calls(&timer, "check"), 1);

        let time = Rc::new(Cell::new(0.0));
        let sample = SampleGuard::start_with_clock(clock(time.clone()));
        let nested = SampleGuard::start_with_clock(clock(time.clone()));
        { let _parse = measure("parse"); time.set(2.0); }
        let nested_timer = nested.finish();
        assert_eq!(calls(&nested_timer, "parse"), 1);
        let timer = sample.finish();
        assert_eq!(calls(&timer, "parse"), 1);

        let parent_time = Rc::new(Cell::new(0.0));
        let parent = SampleGuard::start_with_clock(clock(parent_time.clone()));
        {
            let _parent_phase = measure("parent");
            parent_time.set(1.0);
            let _without = without();
            let child_time = Rc::new(Cell::new(0.0));
            let child = SampleGuard::start_with_clock(clock(child_time.clone()));
            { let _inside = measure("inside"); child_time.set(4.0); }
            let child_timer = child.finish();
            assert_eq!(child_timer.total, 4.0);
            assert_eq!(calls(&child_timer, "inside"), 1);
            drop(_without);
            parent_time.set(5.0);
            {
                let _after = measure("after");
                parent_time.set(7.0);
            }
        }
        let parent_timer = parent.finish();
        assert_eq!(seconds(&parent_timer, "parent"), 5.0);
        assert_eq!(calls(&parent_timer, "after"), 1);
        assert_eq!(calls(&parent_timer, "inside"), 0);

        let time = Rc::new(Cell::new(5.0));
        let sample = SampleGuard::start_with_clock(clock(time.clone()));
        { let _check = measure("check"); time.set(0.0); }
        let timer = sample.finish();
        assert_eq!(timer.total, 0.0);
        assert_eq!(seconds(&timer, "check"), 0.0);
        let sample = SampleGuard::start_with_clock(clock(time));
        assert!(sample.finish().entries.is_empty());
        let time = Rc::new(Cell::new(0.0));
        {
            let _sample = SampleGuard::start_with_clock(clock(time.clone()));
            let _phase = measure("panic");
        }
        assert!(SampleGuard::start_with_clock(clock(time)).finish().entries.is_empty());
        let parent = SampleGuard::start_with_clock(Box::new(|| 0.0));
        std::thread::spawn(|| {
            let child = SampleGuard::start_with_clock(Box::new(|| 0.0));
            { let _phase = measure("lower"); }
            let timer = child.finish();
            assert_eq!(calls(&timer, "lower"), 1);
        }).join().unwrap();
        assert!(parent.finish().entries.is_empty());
    }
}
