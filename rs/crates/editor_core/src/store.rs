//! Port of lib/editor_core/store.ml: atomic file writes and the s-expression state stored beside
//! documents (a sketch's or the editor's saved settings, and the viewport of a preset).

use flow_syntax::syntax::{make as make_node, Node, Syntax};
use flow_syntax::lisp;
use input::Platform;
use rays::{camera, easy_camera};
use rays_math::vec3;

/// Write atomically through the shared filesystem helper; creates the directory.
pub fn write_text(filename: &str, text: &str) -> Result<(), String> {
    runtime_resources::write_text(filename, text)
}

pub fn read_text(filename: &str) -> Result<String, String> {
    let bytes = std::fs::read(filename).map_err(|e| e.to_string())?;
    String::from_utf8(bytes).map_err(|e| e.to_string())
}

/// `home/.rays` on Apple, and the platform's preference folder `pref` (SDL's) elsewhere, when
/// SDL names one.
pub fn user_folder(platform: Platform, pref: Option<&str>, home: &str) -> String {
    match (platform, pref) {
        (Platform::Other, Some(folder)) => folder.to_string(),
        (Platform::Apple, _) | (Platform::Other, None) => concat(home, ".rays"),
    }
}

/// OCaml's `Filename.concat`: a separator only when the directory does not end in one.
fn concat(dir: &str, file: &str) -> String {
    let (sep, ends) = if cfg!(windows) {
        ("\\", dir.is_empty() || dir.ends_with('/') || dir.ends_with('\\'))
    } else {
        ("/", dir.is_empty() || dir.ends_with('/'))
    };
    if ends { format!("{}{}", dir, file) } else { format!("{}{}{}", dir, sep, file) }
}

// ---- the forms stored beside the documents: all s-expressions ----

fn kw(name: &str) -> Syntax {
    make_node(Node::Kw(name.to_string()))
}

fn number(x: f64) -> Syntax {
    make_node(Node::Num(lisp::float(x)))
}

fn vec_of_numbers(xs: &[f64]) -> Syntax {
    make_node(Node::Vec(xs.iter().map(|x| number(*x)).collect()))
}

fn map(pairs: Vec<(&str, Syntax)>) -> Syntax {
    let mut items = Vec::with_capacity(pairs.len() * 2);
    for (k, v) in pairs {
        items.push(kw(k));
        items.push(v);
    }
    make_node(Node::Map(items.into()))
}

fn print(form: &Syntax) -> Result<String, String> {
    lisp::print(std::slice::from_ref(form)).map(|(text, _)| text).map_err(|error| rays_diagnostic::to_string(&error))
}

/// OCaml's `float_of_string_opt` on a number form, finite only.
fn num_of(form: &Syntax) -> Option<f64> {
    match &form.node {
        Node::Num(n) => cfmt::parse_ocaml_float(n).filter(|v| v.is_finite()),
        _ => None,
    }
}

fn vec_values(form: &Syntax) -> Option<Vec<f64>> {
    match &form.node {
        Node::Vec(xs) => {
            let values: Vec<f64> = xs.iter().filter_map(num_of).collect();
            if values.len() == xs.len() { Some(values) } else { None }
        }
        _ => None,
    }
}

/// The value after keyword `name` in a map form.
fn field(form: &Syntax, name: &str) -> Option<Syntax> {
    match &form.node {
        Node::Map(items) => {
            let mut i = 0;
            while i + 1 < items.len() {
                if let Node::Kw(k) = &items[i].node {
                    if k == name {
                        return Some(items[i + 1].clone());
                    }
                }
                i += 2;
            }
            None
        }
        _ => None,
    }
}

/// The environment's view state in a preset: the viewport camera.
pub mod viewport {
    use super::*;

    pub fn encode(easy: &easy_camera::t, look_through: bool) -> Result<Syntax, String> {
        let camera = easy_camera::camera(easy)?;
        let vector = |v: vec3::T| vec_of_numbers(&[v.x, v.y, v.z]);
        Ok(map(vec![
            ("eye", vector(camera::position(&camera))),
            ("target", vector(camera::target(&camera))),
            ("fov", number(easy_camera::fov_y(easy))),
            ("look_through", make_node(Node::Sym(look_through.to_string()))),
        ]))
    }

    pub fn decode(easy: easy_camera::t, form: &Syntax) -> (easy_camera::t, bool) {
        let vector = |name: &str| match field(form, name).as_ref().and_then(vec_values) {
            Some(values) if values.len() == 3 => {
                Some(vec3::create(values[0], values[1], values[2]))
            }
            _ => None,
        };
        let easy = match (vector("eye"), vector("target")) {
            (Some(eye), Some(target)) => easy_camera::of_view(eye, target, &easy),
            _ => easy,
        };
        let easy = match field(form, "fov").as_ref().and_then(num_of) {
            // the range guard makes with_fov_y's own validation unreachable here
            Some(fov) if fov > 0. && fov < std::f64::consts::PI => {
                easy_camera::with_fov_y(fov, &easy).unwrap_or_else(|_| easy.clone())
            }
            _ => easy,
        };
        let look_through = matches!(
            field(form, "look_through"),
            Some(Syntax { node: Node::Sym(s), .. }) if s == "true"
        );
        (easy, look_through)
    }
}

/// A sketch's or the editor's saved settings: one `(settings :sketch "name" :key value ...)` form.
pub mod settings {
    use super::*;

    #[derive(Clone, Debug, PartialEq)]
    pub enum Value {
        Bool(bool),
        Float(f64),
        Int(i64),
        Text(String),
        Choice(String),
        Pair(f64, f64),
    }

    pub type t = Vec<(String, Value)>;

    fn encode(value: &Value) -> Syntax {
        match value {
            Value::Bool(v) => make_node(Node::Sym(v.to_string())),
            Value::Float(v) => number(*v),
            Value::Int(v) => make_node(Node::Num(v.to_string())),
            Value::Text(v) => make_node(Node::Str(v.clone().into_bytes())),
            Value::Choice(v) => make_node(Node::List(vec![
                make_node(Node::Sym("choice".into())),
                make_node(Node::Str(v.clone().into_bytes())),
            ].into())),
            Value::Pair(x, y) => vec_of_numbers(&[*x, *y]),
        }
    }

    /// OCaml's `int_of_string_opt` for a Lisp number: optional sign, 0x/0o/0b prefix, digits with
    /// underscores, in the 63-bit range that `int` has.
    // ponytail: prefixed literals accept the unsigned 63-bit range without the 0u form.
    fn int_of_string(text: &str) -> Option<i64> {
        let (negative, body) = match text.as_bytes().first() {
            Some(b'-') => (true, &text[1..]),
            Some(b'+') => (false, &text[1..]),
            _ => (false, text),
        };
        let (radix, digits) = match body.as_bytes() {
            [b'0', b'x' | b'X', ..] => (16, &body[2..]),
            [b'0', b'o' | b'O', ..] => (8, &body[2..]),
            [b'0', b'b' | b'B', ..] => (2, &body[2..]),
            _ => (10, body),
        };
        if digits.is_empty() || digits.starts_with('_') || digits.ends_with('_') {
            return None;
        }
        let mut magnitude: i128 = 0;
        for c in digits.chars() {
            if c == '_' {
                continue;
            }
            let d = c.to_digit(radix)? as i128;
            magnitude = magnitude * radix as i128 + d;
            if magnitude > (1i128 << 64) {
                return None;
            }
        }
        let value = if negative { -magnitude } else { magnitude };
        let (low, high) = if radix == 10 {
            (-(1i128 << 62), (1i128 << 62) - 1)
        } else {
            (-(1i128 << 63), (1i128 << 63) - 1)
        };
        if value < low || value > high {
            return None;
        }
        // a prefixed literal above max_int wraps into the negative 63-bit range
        let wrapped = if value >= (1i128 << 62) { value - (1i128 << 63) } else { value };
        Some(wrapped as i64)
    }

    fn decode(form: &Syntax) -> Result<Value, String> {
        match &form.node {
            Node::Sym(s) if s == "true" => Ok(Value::Bool(true)),
            Node::Sym(s) if s == "false" => Ok(Value::Bool(false)),
            Node::Num(n) if int_of_string(n).is_some() => {
                Ok(Value::Int(int_of_string(n).unwrap_or_default()))
            }
            Node::Num(_) => match num_of(form) {
                Some(v) => Ok(Value::Float(v)),
                None => Err("invalid settings number".into()),
            },
            Node::Str(v) => match String::from_utf8(v.clone()) {
                Ok(text) => Ok(Value::Text(text)),
                Err(_) => Err("invalid settings text".into()),
            },
            Node::List(items) => match items.as_slice() {
                [Syntax { node: Node::Sym(s), .. }, Syntax { node: Node::Str(v), .. }]
                    if s == "choice" =>
                {
                    match String::from_utf8(v.clone()) {
                        Ok(text) => Ok(Value::Choice(text)),
                        Err(_) => Err("invalid settings text".into()),
                    }
                }
                _ => Err("invalid settings value".into()),
            },
            Node::Vec(_) => match vec_values(form) {
                Some(values) if values.len() == 2 => Ok(Value::Pair(values[0], values[1])),
                _ => Err("invalid settings pair".into()),
            },
            _ => Err("invalid settings value".into()),
        }
    }

    /// One `(settings :sketch "name" :key value ...)` form, written atomically.
    pub fn save(sketch: &str, filename: &str, values: &[(String, Value)]) -> Result<(), String> {
        let finite = values.iter().all(|(_, value)| match value {
            Value::Float(x) => x.is_finite(),
            Value::Pair(x, y) => x.is_finite() && y.is_finite(),
            _ => true,
        });
        if !finite {
            return Err("non-finite settings value".into());
        }
        let mut items = vec![
            make_node(Node::Sym("settings".into())),
            kw("sketch"),
            make_node(Node::Str(sketch.as_bytes().to_vec())),
        ];
        for (name, value) in values {
            items.push(kw(name));
            items.push(encode(value));
        }
        let form = make_node(Node::List(items.into()));
        write_text(filename, &print(&form)?)
    }

    pub fn load(sketch: &str, filename: &str) -> Result<t, String> {
        let text = read_text(filename)?;
        let forms = flow_syntax::syntax::parse(&text).map_err(|d| rays_diagnostic::to_string(&d))?;
        let items = match forms.as_slice() {
            [Syntax { node: Node::List(items), .. }] => match items.split_first() {
                Some((Syntax { node: Node::Sym(s), .. }, rest)) if s == "settings" => rest,
                _ => return Err("a settings file is one (settings ...) form".into()),
            },
            _ => return Err("a settings file is one (settings ...) form".into()),
        };
        let mut acc: t = Vec::new();
        let mut rest = items;
        loop {
            match rest {
                [] => return Ok(acc),
                [Syntax { node: Node::Kw(k), .. }, Syntax { node: Node::Str(name), .. }, tail @ ..]
                    if k == "sketch" && name.as_slice() == sketch.as_bytes() =>
                {
                    rest = tail;
                }
                [Syntax { node: Node::Kw(k), .. }, _, ..] if k == "sketch" => {
                    return Err("settings belong to another sketch".into());
                }
                [Syntax { node: Node::Kw(k), .. }, v, tail @ ..] => {
                    let value = decode(v)?;
                    acc.push((k.clone(), value));
                    rest = tail;
                }
                _ => return Err("invalid settings entry".into()),
            }
        }
    }

    /// The first entry named `name` (OCaml's `List.assoc_opt`), when it has the right kind.
    fn first_named(settings: &t, name: &str) -> Option<Value> {
        settings.iter().find(|(key, _)| key == name).map(|(_, value)| value.clone())
    }

    pub fn bool(settings: &t, name: &str) -> Option<bool> {
        match first_named(settings, name)? {
            Value::Bool(v) => Some(v),
            _ => None,
        }
    }

    pub fn float(settings: &t, name: &str) -> Option<f64> {
        match first_named(settings, name)? {
            Value::Float(v) => Some(v),
            _ => None,
        }
    }

    pub fn int(settings: &t, name: &str) -> Option<i64> {
        match first_named(settings, name)? {
            Value::Int(v) => Some(v),
            _ => None,
        }
    }
}
