//! PXUI roles, states and actions as AT-SPI2 constants (AtspiRole, AtspiStateType in atspi-constants.h).
//! Plain tables, built on every target so the mapping is tested anywhere.
use pxui::a11y::{A11yNode, Role, actions, state};

/// (PXUI role, AtspiRole, role name as at-spi2 spells it).
pub const ROLES: &[(Role, u32, &str)] = &[
    (Role::Unknown, 67, "unknown"),
    (Role::Window, 23, "frame"),
    (Role::Dialog, 16, "dialog"),
    (Role::Panel, 39, "panel"),
    (Role::Group, 99, "grouping"),
    (Role::Label, 29, "label"),
    (Role::Heading, 83, "heading"),
    (Role::Paragraph, 73, "paragraph"),
    (Role::Link, 88, "link"),
    (Role::Button, 43, "push button"),
    (Role::Toggle, 62, "toggle button"),
    (Role::CheckBox, 7, "check box"),
    (Role::Radio, 44, "radio button"),
    (Role::Slider, 51, "slider"),
    (Role::SpinButton, 52, "spin button"),
    (Role::Progress, 42, "progress bar"),
    (Role::TextField, 79, "entry"),
    (Role::TextArea, 61, "text"),
    (Role::ComboBox, 11, "combo box"),
    (Role::List, 98, "list box"),
    (Role::ListItem, 32, "list item"),
    (Role::Tree, 65, "tree"),
    (Role::TreeItem, 91, "tree item"),
    (Role::Table, 55, "table"),
    (Role::Row, 90, "table row"),
    (Role::Cell, 56, "table cell"),
    (Role::ColumnHeader, 10, "column header"),
    (Role::TabList, 38, "page tab list"),
    (Role::Tab, 37, "page tab"),
    (Role::TabPanel, 39, "panel"),
    (Role::MenuBar, 34, "menu bar"),
    (Role::Menu, 33, "menu"),
    (Role::MenuItem, 35, "menu item"),
    (Role::Toolbar, 63, "tool bar"),
    (Role::StatusBar, 54, "status bar"),
    (Role::Tooltip, 64, "tool tip"),
    (Role::ScrollArea, 49, "scroll pane"),
    (Role::ScrollBar, 48, "scroll bar"),
    (Role::Splitter, 53, "split pane"),
    (Role::Image, 27, "image"),
    (Role::Canvas, 6, "canvas"),
    (Role::Separator, 50, "separator"),
];
pub const ROLE_APPLICATION: u32 = 75;

pub fn role(role: Role) -> (u32, &'static str) {
    ROLES.iter().find(|r| r.0 == role).map_or((67, "unknown"), |r| (r.1, r.2))
}

/// AtspiStateType values used here.
pub mod atspi_state {
    pub const ACTIVE: u32 = 1;
    pub const BUSY: u32 = 3;
    pub const CHECKED: u32 = 4;
    pub const COLLAPSED: u32 = 5;
    pub const EDITABLE: u32 = 7;
    pub const ENABLED: u32 = 8;
    pub const EXPANDABLE: u32 = 9;
    pub const EXPANDED: u32 = 10;
    pub const FOCUSABLE: u32 = 11;
    pub const FOCUSED: u32 = 12;
    pub const HORIZONTAL: u32 = 14;
    pub const MODAL: u32 = 16;
    pub const MULTI_LINE: u32 = 17;
    pub const PRESSED: u32 = 20;
    pub const SELECTABLE: u32 = 22;
    pub const SELECTED: u32 = 23;
    pub const SENSITIVE: u32 = 24;
    pub const SHOWING: u32 = 25;
    pub const SINGLE_LINE: u32 = 26;
    pub const VERTICAL: u32 = 29;
    pub const VISIBLE: u32 = 30;
    pub const INDETERMINATE: u32 = 32;
    pub const IS_DEFAULT: u32 = 39;
    pub const CHECKABLE: u32 = 41;
    pub const READ_ONLY: u32 = 43;
}

/// (PXUI state bit, the AT-SPI states it sets with their StateChanged detail names).
pub const STATES: &[(u32, &[(u32, &str)])] = {
    use atspi_state as s;
    &[
        (state::FOCUSABLE, &[(s::FOCUSABLE, "focusable")]),
        (state::FOCUSED, &[(s::FOCUSED, "focused")]),
        (state::ENABLED, &[(s::ENABLED, "enabled"), (s::SENSITIVE, "sensitive")]),
        (state::CHECKABLE, &[(s::CHECKABLE, "checkable")]),
        (state::CHECKED, &[(s::CHECKED, "checked")]),
        (state::MIXED, &[(s::INDETERMINATE, "indeterminate")]),
        (state::SELECTABLE, &[(s::SELECTABLE, "selectable")]),
        (state::SELECTED, &[(s::SELECTED, "selected")]),
        (state::EXPANDABLE, &[(s::EXPANDABLE, "expandable")]),
        (state::EXPANDED, &[(s::EXPANDED, "expanded")]),
        (state::EDITABLE, &[(s::EDITABLE, "editable")]),
        (state::MULTILINE, &[(s::MULTI_LINE, "multi-line")]),
        (state::READ_ONLY, &[(s::READ_ONLY, "read-only")]),
        (state::VISIBLE, &[(s::VISIBLE, "visible"), (s::SHOWING, "showing")]),
        (state::PRESSED, &[(s::PRESSED, "pressed")]),
        (state::MODAL, &[(s::MODAL, "modal")]),
        (state::ACTIVE, &[(s::ACTIVE, "active")]),
        (state::HORIZONTAL, &[(s::HORIZONTAL, "horizontal")]),
        (state::VERTICAL, &[(s::VERTICAL, "vertical")]),
        (state::BUSY, &[(s::BUSY, "busy")]),
        (state::DEFAULT, &[(s::IS_DEFAULT, "default")]),
    ]
};

/// The AT-SPI state set of `node` as GetState's two 32-bit words.
pub fn states(node: &A11yNode) -> [u32; 2] {
    let mut set = [0u32; 2];
    let mut add = |s: u32| set[(s / 32) as usize] |= 1 << (s % 32);
    for (bit, atspi) in STATES {
        if node.states & bit != 0 {
            atspi.iter().for_each(|s| add(s.0));
        }
    }
    if node.states & state::EDITABLE != 0 && node.states & state::MULTILINE == 0 {
        add(atspi_state::SINGLE_LINE);
    }
    if node.states & (state::EXPANDABLE | state::EXPANDED) == state::EXPANDABLE {
        add(atspi_state::COLLAPSED);
    }
    set
}

/// The Action interface's actions: (PXUI action, AT-SPI name, description). Focus, SetValue and Scroll go
/// through Component.GrabFocus, Value.CurrentValue and Component.ScrollTo instead.
pub const ACTIONS: &[(pxui::a11y::Action, &str, &str)] = &[
    (pxui::a11y::Action::Press, "click", "Activate"),
    (pxui::a11y::Action::Expand, "expand", "Expand"),
    (pxui::a11y::Action::Collapse, "collapse", "Collapse"),
];

/// The node's Action-interface actions in order.
pub fn node_actions(node: &A11yNode) -> impl Iterator<Item = &'static (pxui::a11y::Action, &'static str, &'static str)> {
    let mask = node.actions;
    ACTIONS.iter().filter(move |a| mask & a.0 as u32 != 0)
}

pub fn has_action_interface(node: &A11yNode) -> bool {
    node.actions & (actions::PRESS | actions::EXPAND | actions::COLLAPSE) != 0
}
