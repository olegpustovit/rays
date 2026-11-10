// which parameter rows a card shows (flow.md section 5.1).

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Facts {
    /// a geometry slot: always shown
    pub slot: bool,
    /// a wire, an expression or a nested node is written there
    pub driven: bool,
    /// the node's row pin (`layout.rows`), when it has one
    pub pin: Option<bool>,
    /// a literal is written there (the default too: what the text says is drawn)
    pub differs: bool,
    /// a primary row of the schema
    pub primary: bool,
}

/// Evaluated in order: a geometry slot shows; a driven row shows and no pin can hide it; a pin
/// decides; a written row shows; a primary row shows; the rest is hidden.
pub fn shown(f: &Facts) -> bool {
    if f.slot || f.driven {
        true
    } else {
        match f.pin {
            Some(pinned) => pinned,
            None => f.differs || f.primary,
        }
    }
}
