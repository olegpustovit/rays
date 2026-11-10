//! The bridge on a real bus: a private session dbus-daemon, the accessibility bus from at-spi-bus-launcher (with
//! the registry activated by Socket.Embed) when it is installed, else the private daemon standing in as the
//! accessibility bus. A second connection through the same shim walks the tree with AT-SPI calls, invokes
//! actions and watches the change events. With python3 + gi + Atspi present, libatspi walks it too.
//! Skips with a note when dbus-daemon is missing.
#![cfg(target_os = "linux")]
use pxui::a11y::{A11yAction, A11yTree, Action, NO_PARENT, Role, actions, node_id, state};
use pxui::geom::Rect;
use pxui_atspi::wire::{Conn, ERROR, METHOD_RETURN, Msg, SIGNAL};
use pxui_atspi::{Bridge, NODE_PREFIX, ROOT_PATH, a11y_bus_address};
use std::io::{BufRead, BufReader};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

const ACCESSIBLE: &str = "org.a11y.atspi.Accessible";
const PROPS: &str = "org.freedesktop.DBus.Properties";

/// A spawned daemon, terminated (SIGTERM, so it can clean up its own children) on drop.
struct Daemon(Child);
impl Drop for Daemon {
    fn drop(&mut self) {
        let _ = Command::new("kill").arg("-TERM").arg(self.0.id().to_string()).status();
        let deadline = Instant::now() + Duration::from_secs(2);
        while Instant::now() < deadline {
            if let Ok(Some(_)) = self.0.try_wait() {
                return;
            }
            std::thread::sleep(Duration::from_millis(20));
        }
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

fn private_bus() -> Option<(Daemon, String)> {
    let mut child = Command::new("dbus-daemon")
        .args(["--session", "--nofork", "--nopidfile", "--print-address=1"])
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .ok()?;
    let mut line = String::new();
    BufReader::new(child.stdout.take()?).read_line(&mut line).ok()?;
    Some((Daemon(child), line.trim().to_owned()))
}

fn path(id: u64) -> String {
    format!("{NODE_PREFIX}{id}")
}

struct Client {
    conn: Conn,
    bridge: Bridge,
    actions: Vec<A11yAction>,
    /// (path, member, detail, detail1, any as text)
    signals: Vec<(String, String, String, i32, String)>,
}

impl Client {
    fn record(&mut self, mut m: Msg) {
        let (path, member) = (m.path().to_owned(), m.member().to_owned());
        let detail = m.read_str().unwrap_or("").to_owned();
        let d1 = m.read_i32().unwrap_or(0);
        m.skip();
        let any = if m.enter() { m.read_str().map(str::to_owned).unwrap_or_default() } else { String::new() };
        self.signals.push((path, member, detail, d1, any));
    }

    /// Call `member` on `dest`, pumping the bridge until the reply comes.
    fn call_to(&mut self, dest: &str, path: &str, iface: &str, member: &str, args: impl FnOnce(&mut Msg)) -> Msg {
        let mut m = Msg::call(dest, path, iface, member);
        args(&mut m);
        let serial = self.conn.send(m).expect("call is well-formed");
        self.conn.flush();
        let deadline = Instant::now() + Duration::from_secs(4);
        while Instant::now() < deadline {
            assert!(self.bridge.pump(&mut self.actions), "bridge lost its bus");
            match self.conn.wait(5).expect("client connected") {
                Some(r) if r.reply_serial() == serial && (r.kind() == METHOD_RETURN || r.kind() == ERROR) => return r,
                Some(r) if r.kind() == SIGNAL && r.interface().starts_with("org.a11y.atspi.Event") => self.record(r),
                _ => {}
            }
        }
        panic!("no reply to {iface}.{member} on {path}");
    }

    fn call(&mut self, path: &str, iface: &str, member: &str, args: impl FnOnce(&mut Msg)) -> Msg {
        let dest = self.bridge.bus_name().to_owned();
        let r = self.call_to(&dest, path, iface, member, args);
        assert_eq!(r.kind(), METHOD_RETURN, "{iface}.{member} on {path} failed: {}", r.error_name());
        r
    }

    fn prop(&mut self, path: &str, iface: &str, name: &str) -> Msg {
        let mut r = self.call(path, PROPS, "Get", |m| {
            m.s(iface).s(name);
        });
        assert!(r.enter(), "Get returns a variant");
        r
    }

    fn name(&mut self, path: &str) -> String {
        self.prop(path, ACCESSIBLE, "Name").read_str().unwrap().to_owned()
    }

    fn role(&mut self, path: &str) -> u32 {
        self.call(path, ACCESSIBLE, "GetRole", |_| {}).read_u32().unwrap()
    }

    fn children(&mut self, path: &str) -> Vec<String> {
        let mut r = self.call(path, ACCESSIBLE, "GetChildren", |_| {});
        let mut out = Vec::new();
        assert!(r.enter());
        while r.arg() == b'r' {
            r.enter();
            r.read_str();
            out.push(r.read_str().unwrap().to_owned());
            r.leave();
        }
        out
    }

    fn drain_signals(&mut self, for_ms: u64) {
        let deadline = Instant::now() + Duration::from_millis(for_ms);
        while Instant::now() < deadline {
            self.bridge.pump(&mut self.actions);
            if let Some(m) = self.conn.wait(5).unwrap() {
                if m.kind() == SIGNAL && m.interface().starts_with("org.a11y.atspi.Event") {
                    self.record(m);
                }
            }
        }
    }
}

const WINDOW: u64 = 1;
const PANEL: u64 = 2;
const BUTTON: u64 = 3;
const TOGGLE: u64 = 4;
const SLIDER: u64 = 5;
const FIELD: u64 = 6;

fn tree(t: &mut A11yTree, button: &str, checked: bool, text: &str, caret: u32) {
    let ids = |slot: u64| node_id(slot as usize, 1);
    t.begin();
    let w = t.push(NO_PARENT, ids(WINDOW), Role::Window, Rect::new(0.0, 0.0, 400.0, 300.0));
    t.set_name(w, "Demo window");
    t.set_states(w, state::ENABLED | state::VISIBLE | state::ACTIVE);
    let p = t.push(w, ids(PANEL), Role::Panel, Rect::new(0.0, 0.0, 400.0, 300.0));
    let b = t.push(p, ids(BUTTON), Role::Button, Rect::new(10.0, 10.0, 80.0, 20.0));
    t.set_name(b, button);
    t.set_states(b, state::ENABLED | state::VISIBLE | state::FOCUSABLE);
    t.set_actions(b, actions::PRESS | actions::FOCUS);
    let g = t.push(p, ids(TOGGLE), Role::Toggle, Rect::new(10.0, 40.0, 80.0, 20.0));
    t.set_name(g, "Snap");
    t.set_states(g, state::ENABLED | state::VISIBLE | state::CHECKABLE | if checked { state::CHECKED } else { 0 });
    t.set_actions(g, actions::PRESS);
    let s = t.push(p, ids(SLIDER), Role::Slider, Rect::new(10.0, 70.0, 200.0, 20.0));
    t.set_name(s, "Opacity");
    t.set_value_number(s, 0.5, 0.0, 1.0, 0.1);
    t.set_actions(s, actions::SET_VALUE);
    let f = t.push(p, ids(FIELD), Role::TextField, Rect::new(10.0, 100.0, 200.0, 20.0));
    t.set_name(f, "Title");
    t.set_value(f, text);
    t.set_states(f, state::ENABLED | state::VISIBLE | state::EDITABLE | state::FOCUSABLE);
    t.set_selection(f, caret, caret);
    t.end();
}

#[test]
fn atspi_bus_walk_actions_and_events() {
    let started = Instant::now();
    let Some((session, session_address)) = private_bus() else {
        println!("RAYS_TEST_SKIP: pxui_atspi: dbus-daemon is not installed");
        return;
    };
    // The accessibility bus: at-spi-bus-launcher on the private session bus when installed.
    let launcher_bin = ["/usr/libexec/at-spi-bus-launcher", "/usr/lib/at-spi2-core/at-spi-bus-launcher"].into_iter().find(|p| std::path::Path::new(p).exists());
    let launcher = launcher_bin.and_then(|bin| {
        Command::new(bin).arg("--launch-immediately").env("DBUS_SESSION_BUS_ADDRESS", &session_address).stdout(Stdio::null()).stderr(Stdio::null()).spawn().ok().map(Daemon)
    });
    let address = launcher.as_ref().and_then(|_| {
        let deadline = Instant::now() + Duration::from_secs(4);
        loop {
            match a11y_bus_address(Some(&session_address)) {
                Ok(a) => return Some(a),
                Err(_) if Instant::now() < deadline => std::thread::sleep(Duration::from_millis(50)),
                Err(e) => {
                    println!("pxui_atspi: at-spi-bus-launcher gave no address ({e}); testing on a plain bus");
                    return None;
                }
            }
        }
    });
    let registry = address.is_some();
    let address = address.unwrap_or_else(|| {
        println!("pxui_atspi: no at-spi-bus-launcher: the private bus stands in for the accessibility bus; registry embedding is not exercised");
        session_address.clone()
    });

    let bridge = Bridge::connect_to(&address, "pxui test").unwrap_or_else(|e| panic!("{e}"));
    assert_eq!(bridge.embedded(), registry, "Socket.Embed answered exactly when a registry runs");
    assert!(bridge.fd() >= 0);
    let mut t = A11yTree::new();
    tree(&mut t, "Apply", false, "hello", 5);
    let conn = Conn::open(Some(&address)).unwrap();
    conn.add_match("type='signal',interface='org.a11y.atspi.Event.Object'").unwrap();
    conn.add_match("type='signal',interface='org.a11y.atspi.Event.Focus'").unwrap();
    let mut c = Client { conn, bridge, actions: Vec::new(), signals: Vec::new() };
    c.bridge.update(&t);
    c.drain_signals(50);
    c.signals.clear();

    if registry {
        // The registry lists the application under the desktop.
        let mut r = c.call_to("org.a11y.atspi.Registry", ROOT_PATH, ACCESSIBLE, "GetChildren", |_| {});
        assert_eq!(r.kind(), METHOD_RETURN);
        let mut apps = Vec::new();
        r.enter();
        while r.arg() == b'r' {
            r.enter();
            apps.push((r.read_str().unwrap().to_owned(), r.read_str().unwrap().to_owned()));
            r.leave();
        }
        assert!(apps.contains(&(c.bridge.bus_name().to_owned(), ROOT_PATH.to_owned())), "registry children {apps:?}");
        let mut parent = c.prop(ROOT_PATH, ACCESSIBLE, "Parent");
        parent.enter();
        parent.read_str();
        assert_eq!(parent.read_str(), Some(ROOT_PATH), "the app's parent is the registry root");
    }

    // Walk: application -> window -> panel -> controls.
    assert_eq!(c.name(ROOT_PATH), "pxui test");
    assert_eq!(c.role(ROOT_PATH), 75);
    assert_eq!(c.prop(ROOT_PATH, "org.a11y.atspi.Application", "ToolkitName").read_str(), Some("PXUI"));
    let windows = c.children(ROOT_PATH);
    assert_eq!(windows, vec![path(node_id(WINDOW as usize, 1))]);
    let window = windows[0].clone();
    assert_eq!((c.name(&window), c.role(&window)), ("Demo window".to_owned(), 23));
    let panel = c.children(&window).remove(0);
    let controls = c.children(&panel);
    assert_eq!(controls.len(), 4);
    let seen: Vec<(String, u32)> = controls.iter().map(|p| (c.name(p), c.role(p))).collect();
    assert_eq!(seen, vec![("Apply".into(), 43), ("Snap".into(), 62), ("Opacity".into(), 51), ("Title".into(), 79)]);
    let (button, toggle, slider, field) = (controls[0].clone(), controls[1].clone(), controls[2].clone(), controls[3].clone());
    assert_eq!(c.prop(&panel, ACCESSIBLE, "ChildCount").read_i32(), Some(4));
    assert_eq!(c.call(&toggle, ACCESSIBLE, "GetIndexInParent", |_| {}).read_i32(), Some(1));
    let mut parent = c.prop(&toggle, ACCESSIBLE, "Parent");
    parent.enter();
    parent.read_str();
    assert_eq!(parent.read_str(), Some(panel.as_str()));
    let mut child = c.call(&panel, ACCESSIBLE, "GetChildAtIndex", |m| {
        m.i(2);
    });
    child.enter();
    child.read_str();
    assert_eq!(child.read_str(), Some(slider.as_str()));
    assert_eq!(c.call(&button, ACCESSIBLE, "GetRoleName", |_| {}).read_str(), Some("push button"));

    // States: two words of AtspiStateType bits.
    let mut st = c.call(&button, ACCESSIBLE, "GetState", |_| {});
    st.enter();
    let (lo, hi) = (st.read_u32().unwrap(), st.read_u32().unwrap());
    assert_eq!(lo & (1 << 11 | 1 << 8 | 1 << 24 | 1 << 30 | 1 << 25), 1 << 11 | 1 << 8 | 1 << 24 | 1 << 30 | 1 << 25, "focusable enabled sensitive visible showing");
    assert_eq!(lo & (1 << 12 | 1 << 4), 0, "not focused, not checked");
    assert_eq!(hi, 0);

    let mut ifaces = c.call(&slider, ACCESSIBLE, "GetInterfaces", |_| {});
    ifaces.enter();
    let mut names = Vec::new();
    while let Some(s) = ifaces.read_str() {
        names.push(s.to_owned());
    }
    assert_eq!(names, ["org.a11y.atspi.Accessible", "org.a11y.atspi.Component", "org.a11y.atspi.Value"]);

    // Component.
    let mut ext = c.call(&button, "org.a11y.atspi.Component", "GetExtents", |m| {
        m.u(1);
    });
    ext.enter();
    let e: Vec<i32> = (0..4).map(|_| ext.read_i32().unwrap()).collect();
    assert_eq!(e, [10, 10, 80, 20]);
    let mut at = c.call(&window, "org.a11y.atspi.Component", "GetAccessibleAtPoint", |m| {
        m.i(15).i(45).u(1);
    });
    at.enter();
    at.read_str();
    assert_eq!(at.read_str(), Some(toggle.as_str()));
    assert_eq!(c.call(&button, "org.a11y.atspi.Component", "Contains", |m| {
        m.i(5).i(5).u(1);
    }).read_bool(), Some(false));

    // Value and Text.
    assert_eq!(c.prop(&slider, "org.a11y.atspi.Value", "CurrentValue").read_f64(), Some(0.5));
    assert_eq!(c.prop(&slider, "org.a11y.atspi.Value", "MaximumValue").read_f64(), Some(1.0));
    assert_eq!(c.call(&field, "org.a11y.atspi.Text", "GetText", |m| {
        m.i(0).i(-1);
    }).read_str(), Some("hello"));
    assert_eq!(c.prop(&field, "org.a11y.atspi.Text", "CharacterCount").read_i32(), Some(5));
    assert_eq!(c.prop(&field, "org.a11y.atspi.Text", "CaretOffset").read_i32(), Some(5));

    // Actions come back as A11yActions.
    assert_eq!(c.prop(&button, "org.a11y.atspi.Action", "NActions").read_i32(), Some(1));
    assert_eq!(c.call(&button, "org.a11y.atspi.Action", "GetName", |m| {
        m.i(0);
    }).read_str(), Some("click"));
    assert_eq!(c.call(&button, "org.a11y.atspi.Action", "DoAction", |m| {
        m.i(0);
    }).read_bool(), Some(true));
    assert_eq!(c.call(&button, "org.a11y.atspi.Component", "GrabFocus", |_| {}).read_bool(), Some(true));
    c.call(&slider, PROPS, "Set", |m| {
        m.s("org.a11y.atspi.Value").s("CurrentValue").open(b'v', "d").d(0.8).close();
    });
    let id = |slot: u64| node_id(slot as usize, 1);
    assert_eq!(c.actions, vec![
        A11yAction { id: id(BUTTON), action: Action::Press, payload: 0.0 },
        A11yAction { id: id(BUTTON), action: Action::Focus, payload: 0.0 },
        A11yAction { id: id(SLIDER), action: Action::SetValue, payload: 0.8 },
    ]);
    let dest = c.bridge.bus_name().to_owned();
    let unknown = c.call_to(&dest, &path(999), ACCESSIBLE, "GetRole", |_| {});
    assert_eq!((unknown.kind(), unknown.error_name()), (ERROR, "org.freedesktop.DBus.Error.UnknownObject"));

    // Change events from the diff.
    c.signals.clear();
    tree(&mut t, "Apply all", true, "hello!", 6);
    c.bridge.update(&t);
    c.drain_signals(150);
    let has = |c: &Client, p: &str, member: &str, detail: &str, d1: i32, any: &str| {
        c.signals.iter().any(|s| s.0 == p && s.1 == member && s.2 == detail && s.3 == d1 && s.4 == any)
    };
    assert!(has(&c, &toggle, "StateChanged", "checked", 1, ""), "{:?}", c.signals);
    assert!(has(&c, &button, "PropertyChange", "accessible-name", 0, "Apply all"), "{:?}", c.signals);
    assert!(has(&c, &field, "TextChanged", "insert", 0, "hello!"), "{:?}", c.signals);
    assert!(has(&c, &field, "TextCaretMoved", "", 6, ""), "{:?}", c.signals);
    let n = c.signals.len();
    assert_eq!(n, 5, "checked, name, text delete + insert, caret: {:?}", c.signals);

    // An unchanged revision sends nothing; a removed node is announced on its parent.
    c.bridge.update(&t);
    c.drain_signals(50);
    assert_eq!(c.signals.len(), n);
    t.begin();
    let w = t.push(NO_PARENT, id(WINDOW), Role::Window, Rect::new(0.0, 0.0, 400.0, 300.0));
    t.set_name(w, "Demo window");
    t.set_states(w, state::ENABLED | state::VISIBLE | state::ACTIVE);
    t.end();
    c.bridge.update(&t);
    c.drain_signals(150);
    assert!(has(&c, &window, "ChildrenChanged", "remove", 0, ""), "{:?}", c.signals);
    assert_eq!(c.signals.len(), n + 1, "only the subtree's top is announced");
    tree(&mut t, "Apply all", true, "hello!", 6);
    c.bridge.update(&t);

    atspi_python_walk(&mut c, &address);
    drop(c);
    drop(launcher);
    drop(session);
    println!("pxui_atspi: registry {}; {:.2}s", if registry { "embedded" } else { "not exercised" }, started.elapsed().as_secs_f64());
}

/// libatspi's own client walk, when python3 has gi and the Atspi typelib (GI_TYPELIB_PATH may point at an
/// unpacked gir1.2-atspi-2.0).
fn atspi_python_walk(c: &mut Client, address: &str) {
    const SCRIPT: &str = r#"
import gi
gi.require_version("Atspi", "2.0")
from gi.repository import Atspi
desktop = Atspi.get_desktop(0)
def walk(node, depth):
    print("  " * depth + node.get_role_name() + ": " + (node.get_name() or ""))
    for i in range(node.get_child_count()):
        walk(node.get_child_at_index(i), depth + 1)
apps = [desktop.get_child_at_index(i) for i in range(desktop.get_child_count())]
app = [a for a in apps if a and a.get_name() == "pxui test"]
if not app:
    print("NOAPP", [a.get_name() for a in apps if a])
else:
    walk(app[0], 0)
    button = app[0].get_child_at_index(0).get_child_at_index(0).get_child_at_index(0)
    print("ACTION", button.get_action_name(0), button.do_action(0))
"#;
    let python = std::env::var("RAYS_PYTHON").unwrap_or_else(|_| "/usr/bin/python3".into());
    let probe = Command::new(&python).args(["-c", "import gi; gi.require_version('Atspi', '2.0')"]).output();
    if !probe.is_ok_and(|o| o.status.success()) {
        println!("pxui_atspi: no python3 gi + Atspi typelib, libatspi cross-check skipped");
        return;
    }
    let mut child = Command::new(&python).args(["-c", SCRIPT]).env("AT_SPI_BUS_ADDRESS", address).stdout(Stdio::piped()).stderr(Stdio::piped()).spawn().unwrap();
    let deadline = Instant::now() + Duration::from_secs(20);
    while child.try_wait().unwrap().is_none() {
        assert!(Instant::now() < deadline, "libatspi walk timed out");
        c.drain_signals(5);
    }
    let out = child.wait_with_output().unwrap();
    let text = String::from_utf8_lossy(&out.stdout);
    println!("pxui_atspi: libatspi walk:\n{text}{}", String::from_utf8_lossy(&out.stderr));
    if text.contains("NOAPP") {
        println!("pxui_atspi: libatspi saw no registry application (plain bus), walk not checked");
        return;
    }
    assert!(text.contains("application: pxui test") && text.contains("push button: Apply all") && text.contains("entry: Title"), "{text}");
    assert!(text.contains("ACTION click True"), "{text}");
    assert_eq!(c.actions.last().map(|a| (a.id, a.action)), Some((node_id(BUTTON as usize, 1), Action::Press)));
}
