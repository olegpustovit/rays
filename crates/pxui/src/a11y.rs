//! The platform-neutral accessibility tree: plain data a platform bridge (pxui_atspi on Linux) serves to
//! screen readers. Allocation-free when warm: `begin` clears the vectors and keeps their capacity, and every
//! name, description and text value is a byte range into one shared string buffer.
//!
//! Engine integration (for the ui_core owner; nothing here is wired yet):
//! - Ids: `node_id(slot, generation)` of the retained slot, so an id lives exactly as long as its node.
//! - Building: `Ui` owns one `A11yTree`. When a frame rebuilds any view (or a patch changes a node's named
//!   value), `Ui::frame` ends with `begin()`, one walk of the box tree in build order calling
//!   `push(parent_index, node_id, role, rect)` for every node that declared a role, then `end()`. Nodes with
//!   no role are skipped and their children attach to the nearest pushed ancestor. When nothing rebuilt the
//!   tree is left alone and `revision` stays put, so a bridge's `update` returns at once.
//! - Declaring: the role and name come from the kit/text widget functions, which know them (`button(text)`
//!   -> Button + name `text`, `toggle(text, value)` -> Toggle + CHECKED, inspector rows -> Label + the
//!   control, text fields -> TextField with value text and `set_selection` from the edit state, sliders ->
//!   Slider with `set_value_number`). Store `(role, name range)` on the slot, e.g. a `Node::role` plus a
//!   `Ui::a11y_name(handle, &str)` call next to `Ui::text`; keep Role::Panel/Window on panels and modals.
//! - States: from the node flags and signal state at the walk: FOCUSABLE from `node.flags & FOCUSABLE`,
//!   FOCUSED from `Ui::focused`, PRESSED from capture/held, VISIBLE when the resolved rect intersects its
//!   clip, ENABLED unless the widget says disabled. Rect: the resolved slot rect (rx, ry, rw, rh) in window
//!   points, moved by its view entry's offset. Actions: CLICKABLE -> PRESS, FOCUSABLE -> FOCUS, SCROLL ->
//!   SCROLL, sliders SET_VALUE, sections EXPAND|COLLAPSE.
//! - Output: `FrameOut.a11y: &A11yTree`; the host passes it to `Bridge::update` after every frame.
//! - Actions back: the host collects `A11yAction`s from `Bridge::pump` and hands them to
//!   `Ui::inject_a11y(&[A11yAction])` before the next frame. That resolves the id to a live slot (stale ids
//!   are dropped) and sets the slot's accumulator as real input would: Press -> `clicked` on the target (no
//!   synthetic pointer motion, so overlays and the hover state are untouched), Focus -> `focus_box`,
//!   Scroll -> scroll the node by `payload` points (positive down; 0 scrolls it into view), SetValue -> a pending value the widget
//!   reads from its `Signal` (`a11y_value: Option<f64>`) and applies as one undo step like a scrub,
//!   Expand/Collapse -> `clicked` on the disclosure when its EXPANDED state differs.

use crate::geom::Rect;
use pxui_text::FastMap;

/// No parent: a top-level node (a window).
pub const NO_PARENT: u32 = u32::MAX;

/// The stable id of the node in retained slot `slot` at `generation`.
pub fn node_id(slot: usize, generation: u32) -> u64 {
    (u64::from(generation) << 32) | slot as u64
}

/// What a node is to a screen reader: the kit's widgets and the planned widget set.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Role {
    Unknown,
    Window,
    Dialog,
    Panel,
    Group,
    Label,
    Heading,
    Paragraph,
    Link,
    Button,
    Toggle,
    CheckBox,
    Radio,
    Slider,
    SpinButton,
    Progress,
    TextField,
    TextArea,
    ComboBox,
    List,
    ListItem,
    Tree,
    TreeItem,
    Table,
    Row,
    Cell,
    ColumnHeader,
    TabList,
    Tab,
    TabPanel,
    MenuBar,
    Menu,
    MenuItem,
    Toolbar,
    StatusBar,
    Tooltip,
    ScrollArea,
    ScrollBar,
    Splitter,
    Image,
    Canvas,
    Separator,
}

/// Node states, a bitmask.
pub mod state {
    pub const FOCUSABLE: u32 = 1 << 0;
    pub const FOCUSED: u32 = 1 << 1;
    pub const ENABLED: u32 = 1 << 2;
    pub const CHECKABLE: u32 = 1 << 3;
    pub const CHECKED: u32 = 1 << 4;
    /// neither checked nor unchecked (a mixed checkbox)
    pub const MIXED: u32 = 1 << 5;
    pub const SELECTABLE: u32 = 1 << 6;
    pub const SELECTED: u32 = 1 << 7;
    pub const EXPANDABLE: u32 = 1 << 8;
    pub const EXPANDED: u32 = 1 << 9;
    pub const EDITABLE: u32 = 1 << 10;
    pub const MULTILINE: u32 = 1 << 11;
    pub const READ_ONLY: u32 = 1 << 12;
    pub const VISIBLE: u32 = 1 << 13;
    pub const PRESSED: u32 = 1 << 14;
    pub const MODAL: u32 = 1 << 15;
    /// the window holding keyboard focus
    pub const ACTIVE: u32 = 1 << 16;
    pub const HORIZONTAL: u32 = 1 << 17;
    pub const VERTICAL: u32 = 1 << 18;
    pub const BUSY: u32 = 1 << 19;
    pub const DEFAULT: u32 = 1 << 20;
}

/// What a node lets a bridge ask for, a bitmask of `Action` bits.
pub mod actions {
    pub const PRESS: u32 = 1 << 0;
    pub const FOCUS: u32 = 1 << 1;
    pub const SET_VALUE: u32 = 1 << 2;
    pub const SCROLL: u32 = 1 << 3;
    pub const EXPAND: u32 = 1 << 4;
    pub const COLLAPSE: u32 = 1 << 5;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum Action {
    Press = actions::PRESS,
    Focus = actions::FOCUS,
    SetValue = actions::SET_VALUE,
    Scroll = actions::SCROLL,
    Expand = actions::EXPAND,
    Collapse = actions::COLLAPSE,
}

/// A request from an assistive technology, handed back to the UI as input: the target node, the action and
/// its number (the new value for SetValue; for Scroll, points to scroll down, or 0 to scroll the node into
/// view; 0 otherwise).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct A11yAction {
    pub id: u64,
    pub action: Action,
    pub payload: f64,
}

/// A byte range into `A11yTree::strings`.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Str {
    pub start: u32,
    pub end: u32,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Value {
    None,
    /// a text field's contents or a readout's text
    Text(Str),
    Number { now: f64, min: f64, max: f64, step: f64 },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct A11yNode {
    pub id: u64,
    /// index of the parent in `A11yTree::nodes`, or `NO_PARENT`
    pub parent: u32,
    pub role: Role,
    pub name: Str,
    pub description: Str,
    pub value: Value,
    /// logical points, window-relative
    pub rect: Rect,
    pub states: u32,
    pub actions: u32,
    /// editable text: the selection's anchor and the caret, in chars (equal when nothing is selected)
    pub selection: (u32, u32),
    /// children are linked first_child -> next_sibling -> ... (`NO_PARENT` ends); top-level nodes likewise
    /// from `A11yTree::first_child(NO_PARENT)`
    pub child_count: u32,
    pub first_child: u32,
    pub next_sibling: u32,
    last_child: u32,
}

/// The tree in declaration order: a parent always precedes its children.
#[derive(Clone, Debug)]
pub struct A11yTree {
    pub nodes: Vec<A11yNode>,
    pub strings: String,
    /// bumped by `end`; equal revisions mean equal trees
    pub revision: u64,
    index: FastMap<u64, u32>,
    /// the top-level nodes: first, last, count
    top: (u32, u32, u32),
}

impl A11yTree {
    pub fn new() -> A11yTree {
        A11yTree { nodes: Vec::new(), strings: String::new(), revision: 0, index: FastMap::default(), top: (NO_PARENT, NO_PARENT, 0) }
    }

    /// Start a rebuild: clears the nodes and strings, keeping their capacity.
    pub fn begin(&mut self) {
        self.nodes.clear();
        self.strings.clear();
        self.index.clear();
        self.top = (NO_PARENT, NO_PARENT, 0);
    }

    /// Add a node under `parent` (an index `push` returned, or `NO_PARENT`) and return its index.
    pub fn push(&mut self, parent: u32, id: u64, role: Role, rect: Rect) -> u32 {
        let index = self.nodes.len() as u32;
        assert!(parent == NO_PARENT || parent < index, "a11y: parent {parent} is not an earlier node");
        self.nodes.push(A11yNode {
            id,
            parent,
            role,
            name: Str::default(),
            description: Str::default(),
            value: Value::None,
            rect,
            states: state::ENABLED | state::VISIBLE,
            actions: 0,
            selection: (0, 0),
            child_count: 0,
            first_child: NO_PARENT,
            next_sibling: NO_PARENT,
            last_child: NO_PARENT,
        });
        let last = if parent == NO_PARENT {
            self.top.2 += 1;
            if self.top.0 == NO_PARENT {
                self.top.0 = index;
            }
            std::mem::replace(&mut self.top.1, index)
        } else {
            let p = &mut self.nodes[parent as usize];
            p.child_count += 1;
            if p.first_child == NO_PARENT {
                p.first_child = index;
            }
            std::mem::replace(&mut p.last_child, index)
        };
        if last != NO_PARENT {
            self.nodes[last as usize].next_sibling = index;
        }
        index
    }

    fn intern(&mut self, text: &str) -> Str {
        let start = self.strings.len() as u32;
        self.strings.push_str(text);
        Str { start, end: self.strings.len() as u32 }
    }

    pub fn set_name(&mut self, node: u32, name: &str) {
        self.nodes[node as usize].name = self.intern(name);
    }

    pub fn set_description(&mut self, node: u32, description: &str) {
        self.nodes[node as usize].description = self.intern(description);
    }

    pub fn set_value(&mut self, node: u32, text: &str) {
        self.nodes[node as usize].value = Value::Text(self.intern(text));
    }

    pub fn set_value_number(&mut self, node: u32, now: f64, min: f64, max: f64, step: f64) {
        self.nodes[node as usize].value = Value::Number { now, min, max, step };
    }

    /// Replace the default states (ENABLED | VISIBLE).
    pub fn set_states(&mut self, node: u32, states: u32) {
        self.nodes[node as usize].states = states;
    }

    pub fn set_actions(&mut self, node: u32, actions: u32) {
        self.nodes[node as usize].actions = actions;
    }

    pub fn set_selection(&mut self, node: u32, anchor: u32, caret: u32) {
        self.nodes[node as usize].selection = (anchor, caret);
    }

    /// Finish a rebuild: index the ids and bump the revision.
    pub fn end(&mut self) {
        for (i, node) in self.nodes.iter().enumerate() {
            let old = self.index.insert(node.id, i as u32);
            assert!(old.is_none(), "a11y: id {} pushed twice", node.id);
        }
        self.revision += 1;
    }

    pub fn str(&self, range: Str) -> &str {
        &self.strings[range.start as usize..range.end as usize]
    }

    /// A node's text value ("" for none or a number).
    pub fn value_text(&self, node: &A11yNode) -> &str {
        match node.value {
            Value::Text(range) => self.str(range),
            _ => "",
        }
    }

    /// Make `self` equal to `other`, reusing `self`'s allocations.
    pub fn copy_from(&mut self, other: &A11yTree) {
        self.nodes.clone_from(&other.nodes);
        self.strings.clone_from(&other.strings);
        self.index.clone_from(&other.index);
        self.revision = other.revision;
        self.top = other.top;
    }

    /// The index of the node with `id`.
    pub fn find(&self, id: u64) -> Option<u32> {
        self.index.get(&id).copied()
    }

    /// The first child of `parent` (`NO_PARENT`: the first top-level node); `NO_PARENT` when none. Walk on
    /// with `nodes[i].next_sibling`.
    pub fn first_child(&self, parent: u32) -> u32 {
        if parent == NO_PARENT { self.top.0 } else { self.nodes[parent as usize].first_child }
    }

    pub fn child_count(&self, parent: u32) -> u32 {
        if parent == NO_PARENT { self.top.2 } else { self.nodes[parent as usize].child_count }
    }

    /// The `n`th child of `parent` (`NO_PARENT`: the `n`th top-level node).
    pub fn child(&self, parent: u32, n: u32) -> Option<u32> {
        let mut at = self.first_child(parent);
        for _ in 0..n {
            if at == NO_PARENT {
                break;
            }
            at = self.nodes[at as usize].next_sibling;
        }
        (at != NO_PARENT).then_some(at)
    }

    /// The position of node `index` among its parent's children.
    pub fn index_in_parent(&self, index: u32) -> u32 {
        let (mut at, mut k) = (self.first_child(self.nodes[index as usize].parent), 0);
        while at != index && at != NO_PARENT {
            at = self.nodes[at as usize].next_sibling;
            k += 1;
        }
        k
    }
}

/// What changed on a node between two trees.
pub mod change {
    pub const ROLE: u32 = 1 << 0;
    pub const NAME: u32 = 1 << 1;
    pub const DESCRIPTION: u32 = 1 << 2;
    pub const VALUE: u32 = 1 << 3;
    pub const RECT: u32 = 1 << 4;
    pub const STATES: u32 = 1 << 5;
    pub const ACTIONS: u32 = 1 << 6;
    pub const SELECTION: u32 = 1 << 7;
    pub const PARENT: u32 = 1 << 8;
}

/// Node ids added, removed and changed (with a `change` mask) between two trees; reuse it across diffs.
#[derive(Clone, Debug, Default)]
pub struct A11yDiff {
    pub added: Vec<u64>,
    pub removed: Vec<u64>,
    pub changed: Vec<(u64, u32)>,
}

impl A11yDiff {
    pub fn is_empty(&self) -> bool {
        self.added.is_empty() && self.removed.is_empty() && self.changed.is_empty()
    }
}

/// Compare `old` with `new` into `out` (cleared first), in node order of each tree. Allocation-free when
/// `out` is warm.
pub fn diff(old: &A11yTree, new: &A11yTree, out: &mut A11yDiff) {
    out.added.clear();
    out.removed.clear();
    out.changed.clear();
    let parent_id = |tree: &A11yTree, n: &A11yNode| (n.parent != NO_PARENT).then(|| tree.nodes[n.parent as usize].id);
    for n in &new.nodes {
        let Some(o) = old.find(n.id).map(|i| &old.nodes[i as usize]) else {
            out.added.push(n.id);
            continue;
        };
        let value_changed = match (o.value, n.value) {
            (Value::Text(a), Value::Text(b)) => old.str(a) != new.str(b),
            (a, b) => a != b,
        };
        let mask = (o.role != n.role) as u32 * change::ROLE
            | (old.str(o.name) != new.str(n.name)) as u32 * change::NAME
            | (old.str(o.description) != new.str(n.description)) as u32 * change::DESCRIPTION
            | value_changed as u32 * change::VALUE
            | (o.rect != n.rect) as u32 * change::RECT
            | (o.states != n.states) as u32 * change::STATES
            | (o.actions != n.actions) as u32 * change::ACTIONS
            | (o.selection != n.selection) as u32 * change::SELECTION
            | (parent_id(old, o) != parent_id(new, n)) as u32 * change::PARENT;
        if mask != 0 {
            out.changed.push((n.id, mask));
        }
    }
    out.removed.extend(old.nodes.iter().filter(|o| new.find(o.id).is_none()).map(|o| o.id));
}
