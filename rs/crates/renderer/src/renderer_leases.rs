use runtime_resources::Image;
use std::cell::RefCell;
use std::rc::Rc;

/// Image snapshot leases a submission keeps until it closes.
#[derive(Clone)]
pub struct t(Rc<RefCell<Vec<Image::lease>>>);

/// The leases held when a lowering began: how many there were.
pub type checkpoint = usize;

#[derive(Clone)]
pub enum policy {
    Copy_image_snapshots,
    Retain_image_snapshots(t),
}

pub fn create() -> t {
    t(Rc::new(RefCell::new(Vec::new())))
}

pub fn checkpoint(policy_: &policy) -> Option<checkpoint> {
    match policy_ {
        policy::Copy_image_snapshots => None,
        policy::Retain_image_snapshots(value) => Some(value.0.borrow().len()),
    }
}

pub fn retain(value: &t, lease: Image::lease) {
    value.0.borrow_mut().push(lease);
}

pub fn release(value: &t) {
    let mut leases = std::mem::take(&mut *value.0.borrow_mut());
    for lease in leases.iter_mut().rev() {
        Image::release_snapshot(lease);
    }
}

/// Releases what was retained since `checkpoint`, newest first.
pub fn rollback(policy_: &policy, checkpoint_: Option<checkpoint>) {
    match (policy_, checkpoint_) {
        (policy::Copy_image_snapshots, _) => {}
        (policy::Retain_image_snapshots(value), Some(before)) => {
            let mut leases = value.0.borrow_mut();
            while leases.len() > before {
                if let Some(mut lease) = leases.pop() {
                    Image::release_snapshot(&mut lease);
                }
            }
        }
        (policy::Retain_image_snapshots(_), None) => panic!("Renderer_leases.rollback: no checkpoint"),
    }
}
