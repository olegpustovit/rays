use std::ffi::c_void;
use std::fmt;
use std::ptr::{self, NonNull};
use std::sync::{Arc, Mutex, MutexGuard};

pub type Retain = unsafe extern "C" fn(*mut c_void);
pub type Release = unsafe extern "C" fn(*mut c_void);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CreateError {
    OwnershipCallbacksUnpaired,
}

impl fmt::Display for CreateError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OwnershipCallbacksUnpaired => {
                f.write_str("native layer ownership callbacks must be paired")
            }
        }
    }
}

impl std::error::Error for CreateError {}

struct State {
    layer: *mut c_void,
    alive: bool,
    release: Option<Release>,
}

struct Inner {
    owner_id: i64,
    generation: i64,
    state: Mutex<State>,
}

// Access to the pointer and liveness is serialized by state. `from_raw` requires
// callbacks to be safe on whichever thread invalidates or drops the final token.
unsafe impl Send for Inner {}
unsafe impl Sync for Inner {}

impl Inner {
    fn state(&self) -> MutexGuard<'_, State> {
        self.state.lock().unwrap_or_else(|poisoned| poisoned.into_inner())
    }
}

impl Drop for Inner {
    fn drop(&mut self) {
        let state = self
            .state
            .get_mut()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let layer = std::mem::replace(&mut state.layer, ptr::null_mut());
        state.alive = false;
        if !layer.is_null() {
            if let Some(release) = state.release.take() {
                // SAFETY: `from_raw` requires the release callback to accept this
                // retained handle and remain valid until the final token is dropped.
                unsafe { release(layer) };
            }
        }
    }
}

/// Shared ownership of one opaque native layer handle.
#[derive(Clone)]
pub struct Token {
    inner: Arc<Inner>,
}

impl Token {
    /// Creates a live token from an OS-owned opaque pointer.
    ///
    /// # Safety
    /// `layer` must remain valid until invalidation or final drop. `retain` and
    /// `release` must always be supplied together or both omitted. If supplied,
    /// callbacks must remain valid and be safe on the thread that creates,
    /// invalidates, or drops the final clone; the retained object remains valid
    /// until the matching release callback runs.
    pub unsafe fn from_raw(
        layer: *mut c_void,
        owner_id: i64,
        generation: i64,
        retain: Option<Retain>,
        release: Option<Release>,
    ) -> Result<Self, CreateError> {
        if retain.is_some() != release.is_some() {
            return Err(CreateError::OwnershipCallbacksUnpaired);
        }

        let token = Self {
            inner: Arc::new(Inner {
                owner_id,
                generation,
                state: Mutex::new(State {
                    layer,
                    alive: !layer.is_null(),
                    release,
                }),
            }),
        };
        if !layer.is_null() {
            if let Some(retain) = retain {
                // SAFETY: required by this function's contract.
                unsafe { retain(layer) };
            }
        }
        Ok(token)
    }

    /// Constructs the dead token represented by a null native handle.
    pub fn empty(owner_id: i64, generation: i64) -> Self {
        Self {
            inner: Arc::new(Inner {
                owner_id,
                generation,
                state: Mutex::new(State {
                    layer: ptr::null_mut(),
                    alive: false,
                    release: None,
                }),
            }),
        }
    }

    pub fn owner_id(&self) -> i64 {
        self.inner.owner_id
    }

    pub fn generation(&self) -> i64 {
        self.inner.generation
    }

    pub fn is_alive(&self) -> bool {
        self.inner.state().alive
    }

    /// Returns the opaque pointer only while live and both IDs match. The result
    /// is borrowed; callers must synchronize against invalidation while using it.
    pub fn borrow(&self, owner_id: i64, generation: i64) -> Option<NonNull<c_void>> {
        let state = self.inner.state();
        if state.alive && self.inner.owner_id == owner_id && self.inner.generation == generation {
            NonNull::new(state.layer)
        } else {
            None
        }
    }

    /// Invalidates all clones and releases an owned handle at most once.
    pub fn invalidate(&self) {
        let (layer, release) = {
            let mut state = self.inner.state();
            let layer = std::mem::replace(&mut state.layer, ptr::null_mut());
            state.alive = false;
            (layer, state.release.take())
        };
        if !layer.is_null() {
            if let Some(release) = release {
                // SAFETY: `from_raw` requires the matching release callback to be
                // valid until it has run; taking it under the lock makes this once-only.
                unsafe { release(layer) };
            }
        }
    }
}

/// Opaque token handle used by the C ABI. Destroying it drops its Rust owner.
#[repr(C)]
pub struct CToken {
    token: Token,
}

/// Creates a token; returns null when retain/release callbacks are unpaired.
/// A non-null result must be passed to `rays_native_layer_token_destroy` once.
///
/// # Safety
/// `layer` and callbacks must satisfy [`Token::from_raw`]'s safety contract.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rays_native_layer_token_create(
    layer: *mut c_void,
    owner_id: i64,
    generation: i64,
    retain: Option<Retain>,
    release: Option<Release>,
) -> *mut CToken {
    // SAFETY: the caller contract is the same as `Token::from_raw`.
    let Ok(token) = (unsafe { Token::from_raw(layer, owner_id, generation, retain, release) })
    else {
        return ptr::null_mut();
    };
    Box::into_raw(Box::new(CToken { token }))
}

/// Marks a token dead and releases its retained native pointer once.
/// Null is accepted as a no-op.
///
/// # Safety
/// A non-null pointer must be a live C token returned by `create` and must not
/// be concurrently destroyed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rays_native_layer_token_invalidate(token: *const CToken) {
    // SAFETY: pointer validity is required by this function's contract.
    if let Some(token) = unsafe { token.as_ref() } {
        token.token.invalidate();
    }
}

/// Borrows the native pointer only for a live token with matching owner/generation.
/// Returns null for a null token or any mismatch.
///
/// # Safety
/// A non-null pointer must be a live C token returned by `create` and must not
/// be concurrently destroyed. The returned pointer is borrowed; callers must
/// ensure the token is not invalidated while they use it.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rays_native_layer_token_borrow(
    token: *const CToken,
    owner_id: i64,
    generation: i64,
) -> *mut c_void {
    // SAFETY: pointer validity is required by this function's contract.
    unsafe { token.as_ref() }
        .and_then(|token| token.token.borrow(owner_id, generation))
        .map_or(ptr::null_mut(), NonNull::as_ptr)
}

/// Returns zero for a null token.
///
/// # Safety
/// A non-null pointer must be a live C token returned by `create` and must not
/// be concurrently destroyed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rays_native_layer_token_owner_id(token: *const CToken) -> i64 {
    // SAFETY: pointer validity is required by this function's contract.
    unsafe { token.as_ref() }.map_or(0, |token| token.token.owner_id())
}

/// Returns zero for a null token.
///
/// # Safety
/// A non-null pointer must be a live C token returned by `create` and must not
/// be concurrently destroyed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rays_native_layer_token_generation(token: *const CToken) -> i64 {
    // SAFETY: pointer validity is required by this function's contract.
    unsafe { token.as_ref() }.map_or(0, |token| token.token.generation())
}

/// Returns false for a null token.
///
/// # Safety
/// A non-null pointer must be a live C token returned by `create` and must not
/// be concurrently destroyed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rays_native_layer_token_alive(token: *const CToken) -> bool {
    // SAFETY: pointer validity is required by this function's contract.
    unsafe { token.as_ref() }.is_some_and(|token| token.token.is_alive())
}

/// Destroys the C token, releasing a still-live retained native pointer.
/// Null is accepted as a no-op.
///
/// # Safety
/// A non-null pointer must be a live C token returned by `create` and must not
/// be used by any other thread or call after destruction.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rays_native_layer_token_destroy(token: *mut CToken) {
    if !token.is_null() {
        // SAFETY: ownership of a live `create` result is consumed by this call.
        drop(unsafe { Box::from_raw(token) });
    }
}

#[cfg(test)]
mod tests {
    use super::{CreateError, Token};
    use std::ffi::c_void;
    use std::ptr;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::thread;

    #[derive(Default)]
    struct Counts {
        retained: AtomicUsize,
        released: AtomicUsize,
    }

    unsafe extern "C" fn retain(pointer: *mut c_void) {
        // SAFETY: tests pass a live pointer to `Counts` through token destruction.
        let counts = unsafe { &*pointer.cast::<Counts>() };
        counts.retained.fetch_add(1, Ordering::SeqCst);
    }

    unsafe extern "C" fn release(pointer: *mut c_void) {
        // SAFETY: tests pass a live pointer to `Counts` through token destruction.
        let counts = unsafe { &*pointer.cast::<Counts>() };
        counts.released.fetch_add(1, Ordering::SeqCst);
    }

    #[test]
    fn identity_borrow_invalidation_and_ownership() {
        let counts = Counts::default();
        let layer = (&counts as *const Counts).cast_mut().cast::<c_void>();
        // SAFETY: the counts outlive the token, and callbacks only update atomics.
        let token = unsafe { Token::from_raw(layer, 17, 23, Some(retain), Some(release)) }.unwrap();
        assert_eq!(counts.retained.load(Ordering::SeqCst), 1);
        assert_eq!(counts.released.load(Ordering::SeqCst), 0);
        assert!(token.is_alive());
        assert_eq!(token.owner_id(), 17);
        assert_eq!(token.generation(), 23);
        assert_eq!(token.borrow(17, 23).unwrap().as_ptr(), layer);
        assert!(token.borrow(18, 23).is_none());
        assert!(token.borrow(17, 24).is_none());

        token.invalidate();
        token.invalidate();
        assert!(!token.is_alive());
        assert!(token.borrow(17, 23).is_none());
        assert_eq!(counts.released.load(Ordering::SeqCst), 1);
        drop(token);
        assert_eq!(counts.released.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn rejects_unpaired_callbacks_and_null_handles_are_dead() {
        assert!(matches!(
            // SAFETY: null is never passed to either callback; this call only checks pairing.
            unsafe { Token::from_raw(ptr::null_mut(), 1, 1, Some(retain), None) },
            Err(CreateError::OwnershipCallbacksUnpaired)
        ));
        let counts = Counts::default();
        // SAFETY: the callbacks are paired and the counts outlive the token.
        let token = unsafe {
            Token::from_raw(ptr::null_mut(), 9, 11, Some(retain), Some(release))
        }
        .unwrap();
        assert!(!token.is_alive());
        assert_eq!(token.owner_id(), 9);
        assert_eq!(token.generation(), 11);
        assert!(token.borrow(9, 11).is_none());
        drop(token);
        assert_eq!(counts.retained.load(Ordering::SeqCst), 0);
        assert_eq!(counts.released.load(Ordering::SeqCst), 0);
    }

    #[test]
    fn final_drop_releases_without_invalidation() {
        let counts = Counts::default();
        let layer = (&counts as *const Counts).cast_mut().cast::<c_void>();
        // SAFETY: the counts outlive both clones and callbacks only update atomics.
        let token = unsafe { Token::from_raw(layer, 2, 3, Some(retain), Some(release)) }.unwrap();
        let clone = token.clone();
        drop(token);
        assert_eq!(counts.released.load(Ordering::SeqCst), 0);
        drop(clone);
        assert_eq!(counts.released.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn final_clone_releases_and_concurrent_invalidation_is_idempotent() {
        let counts = Counts::default();
        let layer = (&counts as *const Counts).cast_mut().cast::<c_void>();
        // SAFETY: the counts outlive all token clones and callbacks only update atomics.
        let token = unsafe { Token::from_raw(layer, 1, 1, Some(retain), Some(release)) }.unwrap();
        let clone = token.clone();
        drop(token);
        assert_eq!(counts.released.load(Ordering::SeqCst), 0);

        let threads: Vec<_> = (0..4)
            .map(|_| {
                let token = clone.clone();
                thread::spawn(move || token.invalidate())
            })
            .collect();
        for thread in threads {
            thread.join().unwrap();
        }
        assert!(!clone.is_alive());
        assert_eq!(counts.released.load(Ordering::SeqCst), 1);
        drop(clone);
        assert_eq!(counts.released.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn c_abi_uses_opaque_token_and_explicit_destroy() {
        let counts = Counts::default();
        let layer = (&counts as *const Counts).cast_mut().cast::<c_void>();
        // SAFETY: the counts outlive the C token and callbacks only update atomics.
        let token = unsafe {
            super::rays_native_layer_token_create(
                layer,
                31,
                47,
                Some(retain),
                Some(release),
            )
        };
        assert!(!token.is_null());
        // SAFETY: `token` is the live handle returned by `create`.
        unsafe {
            assert!(super::rays_native_layer_token_alive(token));
            assert_eq!(super::rays_native_layer_token_owner_id(token), 31);
            assert_eq!(super::rays_native_layer_token_generation(token), 47);
            assert_eq!(
                super::rays_native_layer_token_borrow(token, 31, 47),
                layer
            );
            assert!(super::rays_native_layer_token_borrow(token, 31, 48).is_null());
            super::rays_native_layer_token_invalidate(token);
            super::rays_native_layer_token_invalidate(token);
            assert!(!super::rays_native_layer_token_alive(token));
            super::rays_native_layer_token_destroy(token);
        }
        assert_eq!(counts.retained.load(Ordering::SeqCst), 1);
        assert_eq!(counts.released.load(Ordering::SeqCst), 1);
    }
}
