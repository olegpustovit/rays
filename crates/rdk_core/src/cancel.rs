use std::sync::atomic::{AtomicBool, Ordering};

use crate::error;

pub struct T {
    cancelled: AtomicBool,
}

pub fn create() -> T {
    T {
        cancelled: AtomicBool::new(false),
    }
}

pub fn cancel(value: &T) {
    value.cancelled.store(true, Ordering::Release);
}

pub fn is_cancelled(value: &T) -> bool {
    value.cancelled.load(Ordering::Acquire)
}

pub fn check(value: &T) -> Result<(), error::T> {
    if is_cancelled(value) {
        Err(error::cancelled("Cancel.check"))
    } else {
        Ok(())
    }
}

pub fn check_opt(value: Option<&T>) -> Result<(), error::T> {
    match value {
        Some(value) => check(value),
        None => Ok(()),
    }
}

/// Checks cancellation when `index & mask == 0`; `mask` must be a power of two minus one.
pub fn poll(value: Option<&T>, index: i64, mask: i64) -> Result<(), error::T> {
    if index & mask == 0 {
        check_opt(value)
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::Code;

    #[test]
    fn cancellation_is_shared_and_poll_respects_mask() {
        let token = create();
        assert!(!is_cancelled(&token));
        assert!(check_opt(Some(&token)).is_ok());
        assert!(poll(Some(&token), 1, 3).is_ok());
        cancel(&token);
        assert!(is_cancelled(&token));
        assert_eq!(error::code(&check(&token).unwrap_err()), Code::Cancelled);
        assert_eq!(error::code(&poll(Some(&token), 4, 3).unwrap_err()), Code::Cancelled);
    }
}
