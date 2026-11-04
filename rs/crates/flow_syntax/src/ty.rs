use crate::syntax::{self, Node, Syntax};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Ty {
    Named(String),
    Float,
    Int,
    Bool,
    Vec2,
    Vec3,
    Vec4,
    Text,
    Color,
    List(Box<Ty>),
    Array(Box<Ty>),
    Record(Vec<(String, Ty)>),
    Fn(Option<Box<FnSignature>>),
    Any,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FnSignature {
    pub params: Vec<Ty>,
    pub result: Ty,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ColorKind {
    Geometry,
    Float,
    Int,
    Bool,
    Vec3,
    Text,
    Fn,
    Record,
    Output,
    Compound,
}

pub type T = Ty;
pub type Color = ColorKind;

#[derive(Clone)]
struct Descriptor {
    name: &'static str,
    shape: bool,
    color: ColorKind,
    default: Option<Syntax>,
}

pub fn geometry() -> Ty { Ty::Named("geometry".into()) }
pub fn image() -> Ty { Ty::Named("image".into()) }
pub fn drawing() -> Ty { Ty::Named("drawing".into()) }
pub fn scene() -> Ty { Ty::Named("scene".into()) }
pub fn world() -> Ty { Ty::Named("world".into()) }
pub fn settings() -> Ty { Ty::Named("settings".into()) }
pub fn panel() -> Ty { Ty::Named("panel".into()) }
pub fn editor() -> Ty { Ty::Named("editor".into()) }
pub fn material() -> Ty { Ty::Named("material".into()) }

fn form(node: Node) -> Syntax { syntax::make(node) }

fn descriptors() -> Vec<Descriptor> {
    vec![
        Descriptor { name: "geometry", shape: true, color: ColorKind::Geometry, default: Some(form(Node::Sym("nil".into()))) },
        Descriptor {
            name: "image", shape: true, color: ColorKind::Output,
            default: Some(form(Node::List(vec![
                form(Node::Sym("image/noise".into())),
                form(Node::Kw("width".into())), form(Node::Num("1".into())),
                form(Node::Kw("height".into())), form(Node::Num("1".into())),
            ].into()))),
        },
        Descriptor { name: "drawing", shape: true, color: ColorKind::Output, default: Some(form(Node::List(vec![form(Node::Sym("draw/merge".into()))].into()))) },
        Descriptor { name: "scene", shape: true, color: ColorKind::Output, default: None },
        Descriptor { name: "world", shape: true, color: ColorKind::Output, default: None },
        Descriptor { name: "settings", shape: false, color: ColorKind::Output, default: None },
        Descriptor { name: "panel", shape: true, color: ColorKind::Output, default: None },
        Descriptor { name: "editor", shape: true, color: ColorKind::Output, default: None },
        Descriptor { name: "material", shape: false, color: ColorKind::Output, default: None },
        Descriptor { name: "effect", shape: true, color: ColorKind::Output, default: None },
        Descriptor { name: "sample", shape: false, color: ColorKind::Output, default: None },
    ]
}

fn structural() -> Vec<(&'static str, Ty)> {
    vec![
        ("float", Ty::Float), ("int", Ty::Int), ("bool", Ty::Bool),
        ("vec2", Ty::Vec2), ("vec3", Ty::Vec3), ("vec4", Ty::Vec4),
        ("text", Ty::Text), ("color", Ty::Color), ("fn", Ty::Fn(None)), ("any", Ty::Any),
    ]
}

pub fn names() -> Vec<(String, Ty)> {
    let mut result: Vec<_> = structural().into_iter().map(|(name, ty)| (name.to_owned(), ty)).collect();
    result.extend(descriptors().into_iter().map(|d| (d.name.to_owned(), Ty::Named(d.name.to_owned()))));
    result
}

pub fn is_geometry(ty: &Ty) -> bool { *ty == geometry() }
pub fn is_cooked(ty: &Ty) -> bool { *ty == geometry() || *ty == image() }

pub fn shape(ty: &Ty) -> bool {
    match ty {
        Ty::Named(name) => descriptors().into_iter().find(|d| d.name == name).map_or(true, |d| d.shape),
        _ => false,
    }
}

pub fn color(ty: &Ty) -> ColorKind {
    match ty {
        Ty::Named(name) => descriptors().into_iter().find(|d| d.name == name).map_or(ColorKind::Output, |d| d.color),
        _ => ColorKind::Output,
    }
}

pub fn default(ty: &Ty) -> Option<Syntax> {
    match ty {
        Ty::Named(name) => descriptors().into_iter().find(|d| d.name == name).and_then(|d| d.default),
        _ => None,
    }
}

pub fn to_string(ty: &Ty) -> String {
    match ty {
        Ty::Named(name) => name.clone(),
        Ty::List(element) => format!("list:{}", to_string(element)),
        Ty::Array(element) => format!("array:{}", to_string(element)),
        Ty::Record(fields) => format!("rec{{{}}}", fields.iter().map(|(name, ty)| format!("{name}:{}", to_string(ty))).collect::<Vec<_>>().join(",")),
        Ty::Fn(Some(signature)) => format!("fn({})->{}", signature.params.iter().map(to_string).collect::<Vec<_>>().join(","), to_string(&signature.result)),
        Ty::Fn(None) => "fn".into(),
        Ty::Float => "float".into(), Ty::Int => "int".into(), Ty::Bool => "bool".into(),
        Ty::Vec2 => "vec2".into(), Ty::Vec3 => "vec3".into(), Ty::Vec4 => "vec4".into(),
        Ty::Text => "text".into(), Ty::Color => "color".into(), Ty::Any => "any".into(),
    }
}

fn starts(source: &str, at: usize, text: &str) -> bool { source.get(at..).is_some_and(|tail| tail.starts_with(text)) }
fn eat(source: &str, at: &mut usize, text: &str) -> bool {
    if starts(source, *at, text) { *at += text.len(); true } else { false }
}
fn parse_type(source: &str, at: &mut usize) -> Option<Ty> {
    if eat(source, at, "list:") { return Some(Ty::List(Box::new(parse_type(source, at)?))); }
    if eat(source, at, "array:") {
        let element = parse_type(source, at)?;
        return matches!(element, Ty::Int | Ty::Float | Ty::Vec2 | Ty::Vec3 | Ty::Vec4 | Ty::Text | Ty::Any).then(|| Ty::Array(Box::new(element)));
    }
    if eat(source, at, "rec{") { return parse_fields(source, at); }
    if eat(source, at, "fn(") { return parse_parameters(source, at, Vec::new()); }
    let start = *at;
    while let Some(byte) = source.as_bytes().get(*at) {
        if matches!(*byte, b',' | b'}' | b')') { break; }
        *at += 1;
    }
    let name = source.get(start..*at)?;
    structural().into_iter().find(|(candidate, _)| *candidate == name).map(|(_, ty)| ty)
        .or_else(|| descriptors().into_iter().find(|d| d.name == name).map(|d| Ty::Named(name.to_owned())))
}
fn parse_fields(source: &str, at: &mut usize) -> Option<Ty> {
    if eat(source, at, "}") { return Some(Ty::Record(Vec::new())); }
    let mut fields = Vec::new();
    loop {
        let colon = source.get(*at..)?.find(':')? + *at;
        let name = source.get(*at..colon)?.to_owned();
        *at = colon + 1;
        fields.push((name, parse_type(source, at)?));
        if eat(source, at, "}") { return Some(Ty::Record(fields)); }
        if !eat(source, at, ",") { return None; }
    }
}
fn parse_parameters(source: &str, at: &mut usize, mut params: Vec<Ty>) -> Option<Ty> {
    if eat(source, at, ")->") {
        return Some(Ty::Fn(Some(Box::new(FnSignature { params, result: parse_type(source, at)? }))));
    }
    params.push(parse_type(source, at)?);
    if eat(source, at, ",") { return if starts(source, *at, ")->") { None } else { parse_parameters(source, at, params) }; }
    if starts(source, *at, ")->") { return parse_parameters(source, at, params); }
    None
}

pub fn of_string(source: &str) -> Option<Ty> {
    let mut at = 0;
    let ty = parse_type(source, &mut at)?;
    (at == source.len()).then_some(ty)
}

fn valid_field_name(name: &str) -> bool {
    let mut bytes = name.bytes();
    matches!(bytes.next(), Some(b'a'..=b'z')) && bytes.all(|b| matches!(b, b'a'..=b'z' | b'0'..=b'9' | b'_' | b'-'))
}

pub fn of_syntax(form: &Syntax) -> Option<Ty> {
    match &form.node {
        Node::Sym(name) => names().into_iter().find(|(candidate, _)| candidate == name).and_then(|(_, ty)| if matches!(ty, Ty::Any | Ty::Color) { None } else { Some(ty) }),
        Node::Map(items) => {
            let mut fields = Vec::new();
            let mut at = 0;
            while at < items.len() {
                let Node::Kw(name) = &items[at].node else { return None; };
                if !valid_field_name(name) || fields.iter().any(|(seen, _): &(String, Ty)| seen == name) { return None; }
                let value = items.get(at + 1)?;
                fields.push((name.clone(), of_syntax(value)?));
                at += 2;
            }
            Some(Ty::Record(fields))
        }
        Node::List(items) if items.len() == 2 && matches!(&items[0].node, Node::Sym(name) if name == "list") => {
            Some(Ty::List(Box::new(of_syntax(&items[1])?)))
        }
        Node::List(items) if items.len() == 2 && matches!(&items[0].node, Node::Sym(name) if name == "array") => {
            let inner = of_syntax(&items[1])?;
            matches!(inner, Ty::Int | Ty::Float | Ty::Vec2 | Ty::Vec3 | Ty::Vec4 | Ty::Text).then(|| Ty::Array(Box::new(inner)))
        }
        Node::List(items) if items.len() == 3 && matches!(&items[0].node, Node::Sym(name) if name == "fn") => {
            let Node::Vec(params) = &items[1].node else { return None; };
            let params = params.iter().map(of_syntax).collect::<Option<Vec<_>>>()?;
            Some(Ty::Fn(Some(Box::new(FnSignature { params, result: of_syntax(&items[2])? }))))
        }
        _ => None,
    }
}

pub fn elem(ty: &Ty) -> Option<Ty> { match ty { Ty::List(element) | Ty::Array(element) => Some((**element).clone()), _ => None } }

pub fn has_fn(ty: &Ty) -> bool {
    match ty {
        Ty::Fn(_) => true,
        Ty::List(element) | Ty::Array(element) => has_fn(element),
        Ty::Record(fields) => fields.iter().any(|(_, ty)| has_fn(ty)),
        _ => false,
    }
}

fn num(ty: &Ty) -> bool { matches!(ty, Ty::Int | Ty::Float) }
fn vector(ty: &Ty) -> bool { matches!(ty, Ty::Vec2 | Ty::Vec3 | Ty::Vec4) }

pub fn fits(have: &Ty, want: &Ty) -> bool {
    if matches!(have, Ty::Any) || matches!(want, Ty::Any) || have == want { return true; }
    if (num(have) && (num(want) || matches!(want, Ty::Bool))) || (matches!(have, Ty::Bool) && num(want)) { return true; }
    if matches!(want, Ty::Color) && (matches!(have, Ty::Text | Ty::Vec3) || matches!(have, Ty::List(element) if **element == Ty::Float || **element == Ty::Int)) { return true; }
    if vector(want) && num(have) { return true; }
    match (have, want) {
        (Ty::List(h), Ty::List(w)) => fits(h, w),
        (Ty::Array(h), Ty::Array(w)) => h == w || matches!(h.as_ref(), Ty::Any) || matches!(w.as_ref(), Ty::Any),
        (Ty::Record(hs), Ty::Record(ws)) => ws.iter().all(|(name, want)| hs.iter().find(|(field, _)| field == name).is_some_and(|(_, have)| fits(have, want))),
        (Ty::Fn(_), Ty::Fn(None)) => true,
        (Ty::Fn(Some(h)), Ty::Fn(Some(w))) => h.params == w.params && fits(&h.result, &w.result),
        _ => false,
    }
}

pub fn coerce(have: &Ty, want: &Ty) -> Ty {
    if matches!(want, Ty::Any) || matches!(have, Ty::Any) || have == want { return have.clone(); }
    match (have, want) {
        (Ty::List(h), Ty::List(w)) => Ty::List(Box::new(coerce(h, w))),
        (Ty::Array(h), Ty::Array(w)) => Ty::Array(Box::new(if matches!(w.as_ref(), Ty::Any) { (**h).clone() } else { (**w).clone() })),
        (Ty::Record(hs), Ty::Record(ws)) => Ty::Record(hs.iter().map(|(name, ty)| (name.clone(), ws.iter().find(|(field, _)| field == name).map_or_else(|| ty.clone(), |(_, want)| coerce(ty, want)))).collect()),
        _ if (num(have) || matches!(have, Ty::Bool)) && (num(want) || matches!(want, Ty::Bool) || vector(want) && !matches!(have, Ty::Bool)) => want.clone(),
        _ => have.clone(),
    }
}

pub fn join(a: &Ty, b: &Ty) -> Option<Ty> {
    if a == b { return Some(a.clone()); }
    if matches!(a, Ty::Any) { return Some(b.clone()); }
    if matches!(b, Ty::Any) { return Some(a.clone()); }
    match (a, b) {
        (Ty::Int | Ty::Float, Ty::Int | Ty::Float) => Some(Ty::Float),
        (Ty::List(x), Ty::List(y)) => join(x, y).map(|t| Ty::List(Box::new(t))),
        (Ty::Array(x), Ty::Array(y)) => join(x, y).map(|t| Ty::Array(Box::new(t))),
        (Ty::Record(xs), Ty::Record(ys)) if xs.len() == ys.len() && xs.iter().all(|(name, _)| ys.iter().any(|(other, _)| other == name)) => {
            xs.iter().map(|(name, x)| Some((name.clone(), join(x, &ys.iter().find(|(other, _)| other == name)?.1)?))).collect::<Option<Vec<_>>>().map(Ty::Record)
        }
        _ => None,
    }
}

pub fn unify(a: &Ty, b: &Ty) -> Option<Ty> { join(a, b).or_else(|| if fits(a, b) && fits(b, a) { Some(a.clone()) } else { None }) }

pub fn numeric_record(ty: &Ty) -> bool {
    match ty {
        Ty::Record(fields) => !fields.is_empty() && fields.iter().all(|(_, ty)| match ty { Ty::Float | Ty::Int | Ty::Bool | Ty::Vec2 | Ty::Vec3 | Ty::Vec4 => true, Ty::Record(_) => numeric_record(ty), _ => false }),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax;

    fn annotated(source: &str) -> Option<Ty> {
        let forms = syntax::parse(source).ok()?;
        if forms.len() == 1 { of_syntax(&forms[0]) } else { None }
    }

    #[test]
    fn annotation_and_spelling_round_trips() {
        assert_eq!(annotated("float"), Some(Ty::Float));
        assert_eq!(annotated("fn"), Some(Ty::Fn(None)));
        assert_eq!(annotated("geometry"), Some(geometry()));
        assert_eq!(annotated("(list int)"), Some(Ty::List(Box::new(Ty::Int))));
        assert_eq!(annotated("(array int)"), Some(Ty::Array(Box::new(Ty::Int))));
        assert_eq!(of_string("array:int"), Some(Ty::Array(Box::new(Ty::Int))));
        assert_eq!(annotated("(list (list vec3))"), Some(Ty::List(Box::new(Ty::List(Box::new(Ty::Vec3))))));
        let record = Ty::Record(vec![("shape".into(), geometry()), ("y".into(), Ty::Float)]);
        assert_eq!(annotated("{:shape geometry :y float}"), Some(record.clone()));
        assert_eq!(annotated("{}"), Some(Ty::Record(vec![])));
        for invalid in ["any", "color", "flot", "(list)", "(list int int)", "{:a}", "{:a int :a int}", "{:A int}", "{a int}", "(vec int)", "3"] {
            assert_eq!(annotated(invalid), None, "{invalid}");
        }
        assert_eq!(to_string(&record), "rec{shape:geometry,y:float}");
        for ty in [geometry(), Ty::Float, Ty::Int, Ty::Bool, Ty::Vec2, Ty::Vec3, Ty::Vec4, Ty::Array(Box::new(Ty::Vec2)), Ty::Array(Box::new(Ty::Vec4)), Ty::Text, Ty::Color, Ty::Fn(None), Ty::Any, scene(), world(), settings(), panel(), editor(), Ty::List(Box::new(Ty::Int)), record, Ty::Record(vec![])] {
            assert_eq!(of_string(&to_string(&ty)), Some(ty));
        }
        for invalid in ["nope", "list:", "rec{a:int", "int,"] { assert_eq!(of_string(invalid), None); }
    }

    #[test]
    fn function_types_and_compatibility() {
        let sig = FnSignature { params: vec![Ty::Record(vec![("p".into(), Ty::Vec3)]), Ty::List(Box::new(Ty::Text)), Ty::Array(Box::new(Ty::Float))], result: Ty::Float };
        let typed = Ty::Fn(Some(Box::new(sig.clone())));
        for ty in [typed.clone(), Ty::Fn(Some(Box::new(FnSignature { params: vec![], result: typed.clone() }))), Ty::Record(vec![("f".into(), typed.clone())]), Ty::List(Box::new(typed.clone()))] {
            assert_eq!(of_string(&to_string(&ty)), Some(ty));
        }
        assert!(fits(&typed, &Ty::Fn(None)) && !fits(&Ty::Fn(None), &typed));
        assert!(has_fn(&typed));
        assert!(!fits(&typed, &Ty::Fn(Some(Box::new(FnSignature { params: vec![Ty::Vec3], result: Ty::Float })))));
        for invalid in ["fn(", "fn(float)", "fn(float)->", "fn(nope)->float", "fn(float,)->float"] { assert_eq!(of_string(invalid), None); }
        assert!(fits(&Ty::Int, &Ty::Float) && fits(&Ty::Float, &Ty::Int) && fits(&Ty::Int, &Ty::Bool) && fits(&Ty::Bool, &Ty::Float));
        assert!(fits(&Ty::Int, &Ty::Vec3) && !fits(&Ty::Bool, &Ty::Vec3) && !fits(&Ty::Vec3, &Ty::Float));
        assert!(fits(&Ty::Text, &Ty::Color) && fits(&Ty::Vec3, &Ty::Color) && !fits(&Ty::Float, &Ty::Color));
        assert!(fits(&Ty::Any, &geometry()) && fits(&geometry(), &Ty::Any) && !fits(&geometry(), &Ty::Float));
        assert!(fits(&Ty::List(Box::new(Ty::Int)), &Ty::List(Box::new(Ty::Float))));
        assert!(!fits(&Ty::List(Box::new(geometry())), &Ty::List(Box::new(Ty::Float))));
        assert!(fits(&Ty::List(Box::new(Ty::Any)), &Ty::List(Box::new(Ty::Int))));
        assert!(fits(&Ty::Record(vec![("a".into(), Ty::Int), ("b".into(), Ty::Text)]), &Ty::Record(vec![("a".into(), Ty::Float)])));
        assert!(!fits(&Ty::Record(vec![("a".into(), Ty::Int)]), &Ty::Record(vec![("a".into(), Ty::Int), ("b".into(), Ty::Int)])));
        assert!(!fits(&Ty::List(Box::new(Ty::Int)), &Ty::Int) && !fits(&Ty::Int, &Ty::List(Box::new(Ty::Int))));
        assert_eq!(coerce(&Ty::Int, &Ty::Float), Ty::Float);
        assert_eq!(coerce(&Ty::Float, &Ty::Int), Ty::Int);
        assert_eq!(coerce(&Ty::Float, &Ty::Bool), Ty::Bool);
        assert_eq!(coerce(&Ty::Int, &Ty::Vec3), Ty::Vec3);
        assert_eq!(coerce(&Ty::Bool, &Ty::Vec3), Ty::Bool);
        assert_eq!(coerce(&Ty::List(Box::new(Ty::Int)), &Ty::List(Box::new(Ty::Float))), Ty::List(Box::new(Ty::Float)));
        assert_eq!(coerce(&Ty::Record(vec![("a".into(), Ty::Int), ("b".into(), Ty::Text)]), &Ty::Record(vec![("a".into(), Ty::Float)])), Ty::Record(vec![("a".into(), Ty::Float), ("b".into(), Ty::Text)]));
        assert_eq!(coerce(&Ty::Text, &Ty::Any), Ty::Text);
        assert_eq!(coerce(&Ty::Any, &Ty::Float), Ty::Any);
        assert_eq!(coerce(&geometry(), &Ty::Float), geometry());
        assert_eq!(join(&Ty::Int, &Ty::Int), Some(Ty::Int));
        assert_eq!(join(&Ty::Int, &Ty::Float), Some(Ty::Float));
        assert_eq!(join(&Ty::Any, &Ty::Text), Some(Ty::Text));
        assert_eq!(join(&Ty::Vec3, &Ty::Any), Some(Ty::Vec3));
        assert_eq!(join(&Ty::Text, &geometry()), None);
        assert_eq!(join(&Ty::Int, &Ty::Bool), None);
        assert_eq!(join(&Ty::List(Box::new(Ty::Int)), &Ty::List(Box::new(Ty::Float))), Some(Ty::List(Box::new(Ty::Float))));
        assert_eq!(unify(&Ty::Int, &Ty::Bool), Some(Ty::Int));
        assert_eq!(unify(&geometry(), &Ty::Text), None);
        assert_eq!(elem(&Ty::List(Box::new(Ty::Int))), Some(Ty::Int));
        assert_eq!(elem(&Ty::Int), None);
    }
    #[test]
    fn syntax_signatures_vectors_and_nominals() {
        let sym = |name: &str| syntax::make(syntax::Node::Sym(name.into()));
        let params = syntax::make(syntax::Node::Vec(vec![sym("vec3"), sym("text")].into()));
        let form = syntax::make(syntax::Node::List(vec![sym("fn"), params, sym("float")].into()));
        assert_eq!(of_syntax(&form), Some(Ty::Fn(Some(Box::new(FnSignature { params: vec![Ty::Vec3, Ty::Text], result: Ty::Float })))));
        assert!(has_fn(&Ty::List(Box::new(Ty::Fn(None)))));
        assert!(has_fn(&Ty::Record(vec![("f".into(), Ty::Fn(None))])));
        assert!(!has_fn(&Ty::List(Box::new(Ty::Int))));
        for vector in [Ty::Vec2, Ty::Vec4] {
            let spelling = to_string(&vector);
            assert_eq!(of_string(&spelling), Some(vector.clone()));
            assert_eq!(annotated(&format!("(array {spelling})")), Some(Ty::Array(Box::new(vector.clone()))));
            assert!(fits(&Ty::Int, &vector) && fits(&Ty::Float, &vector) && !fits(&Ty::Bool, &vector));
            assert_eq!(coerce(&Ty::Int, &vector), vector);
            assert_eq!(coerce(&Ty::Float, &vector), vector);
            assert_eq!(join(&vector, &Ty::Any), Some(vector.clone()));
            assert_eq!(join(&vector, &vector), Some(vector.clone()));
            for other in [Ty::Vec2, Ty::Vec3, Ty::Vec4] {
                if vector != other {
                    assert!(!fits(&vector, &other));
                    assert!(!fits(&Ty::Array(Box::new(vector.clone())), &Ty::Array(Box::new(other.clone()))));
                    assert_eq!(coerce(&vector, &other), vector);
                    assert_eq!(join(&vector, &other), None);
                }
            }
        }
        assert!(numeric_record(&Ty::Record(vec![("v".into(), Ty::Vec3), ("nested".into(), Ty::Record(vec![("x".into(), Ty::Int)]))])));
        assert!(!numeric_record(&Ty::Record(vec![])));
        assert!(!numeric_record(&Ty::Record(vec![("text".into(), Ty::Text)])));
        assert!(shape(&geometry()));
        assert!(!shape(&settings()));
        assert_eq!(color(&geometry()), ColorKind::Geometry);
        assert!(default(&image()).is_some());
    }
}
