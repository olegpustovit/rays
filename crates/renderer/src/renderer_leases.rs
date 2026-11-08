use runtime_resources::Image;
use std::cell::RefCell;
use std::rc::Rc;

/// Image snapshot leases a submission keeps until it closes.
#[derive(Clone)]
pub struct Leases(Rc<RefCell<Vec<Image::lease>>>);

/// The leases held when a lowering began: how many there were.
pub type Checkpoint = usize;

#[derive(Clone)]
pub enum Policy {
    CopyImageSnapshots,
    RetainImageSnapshots(Leases),
}

pub fn create() -> Leases {
    Leases(Rc::new(RefCell::new(Vec::new())))
}

pub fn checkpoint(policy_: &Policy) -> Option<Checkpoint> {
    match policy_ {
        Policy::CopyImageSnapshots => None,
        Policy::RetainImageSnapshots(value) => Some(value.0.borrow().len()),
    }
}

pub fn retain(value: &Leases, lease: Image::lease) {
    value.0.borrow_mut().push(lease);
}

pub fn release(value: &Leases) {
    let mut leases = std::mem::take(&mut *value.0.borrow_mut());
    for lease in leases.iter_mut().rev() {
        Image::release_snapshot(lease);
    }
}

/// Releases what was retained since `checkpoint`, newest first.
pub fn rollback(policy_: &Policy, checkpoint_: Option<Checkpoint>) {
    match (policy_, checkpoint_) {
        (Policy::CopyImageSnapshots, _) => {}
        (Policy::RetainImageSnapshots(value), Some(before)) => {
            let mut leases = value.0.borrow_mut();
            while leases.len() > before {
                if let Some(mut lease) = leases.pop() {
                    Image::release_snapshot(&mut lease);
                }
            }
        }
        (Policy::RetainImageSnapshots(_), None) => unreachable!("Renderer_leases.rollback: no checkpoint"),
    }
}
