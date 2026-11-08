use std::ffi::c_void;
use std::ptr::NonNull;

/// Safe, portable identity and liveness view of a native layer token.
///
/// Construct live tokens at the platform boundary with
/// `Token::from_raw`, then move them into this facade.
#[derive(Clone)]
pub struct Token(crate::Token);

impl Token {
    pub fn from_sys(token: crate::Token) -> Self {
        Self(token)
    }

    /// Creates a dead token for a missing native layer.
    pub fn empty(owner_id: i64, generation: i64) -> Self {
        Self(crate::Token::empty(owner_id, generation))
    }

    pub fn owner_id(&self) -> i64 {
        self.0.owner_id()
    }

    pub fn generation(&self) -> i64 {
        self.0.generation()
    }

    pub fn alive(&self) -> bool {
        self.0.is_alive()
    }

    /// Reports whether the token can be borrowed for this exact identity.
    pub fn borrow(&self, owner_id: i64, generation: i64) -> bool {
        self.borrow_raw(owner_id, generation).is_some()
    }

    /// Returns the native pointer only for a live token with matching identity.
    /// Callers must synchronize against invalidation while using the pointer.
    pub fn borrow_raw(&self, owner_id: i64, generation: i64) -> Option<NonNull<c_void>> {
        self.0.borrow(owner_id, generation)
    }

    pub fn invalidate(&self) {
        self.0.invalidate();
    }
}

#[cfg(test)]
mod tests {
    use super::Token;

    #[test]
    fn empty_token_keeps_identity_and_never_borrows() {
        let token = Token::empty(17, 23);
        assert_eq!(token.owner_id(), 17);
        assert_eq!(token.generation(), 23);
        assert!(!token.alive());
        assert!(!token.borrow(17, 23));
        token.invalidate();
        assert!(!token.alive());
    }
}
