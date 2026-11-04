use crate::catalog::{self, KernelElements, Slot};
use crate::port_type::PortType;
use flow_syntax::context;
use flow_syntax::macros;
use flow_syntax::symbol;
use flow_syntax::syntax::{Node, Syntax};
use flow_syntax::ty::{self, Ty};
use param::{self, FieldView, Impact, KindView, Value};
use rays_diagnostic::{self, Diagnostic};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Validation {
    pub predicate: Syntax,
    pub code: String,
    pub message: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Defnode {
    pub kind: catalog::Kind,
    pub slots: Vec<Slot>,
    pub keyword_inputs: Vec<String>,
    pub fields: Vec<FieldView>,
    pub label: String,
    pub operation: String,
    pub version: i64,
    pub category: Vec<String>,
    pub description: Option<String>,
    pub inputs: Vec<(String, Ty, Option<Syntax>)>,
    pub body: Syntax,
    pub validations: Vec<Validation>,
    pub normalizers: Vec<(String, Syntax)>,
    pub form: Syntax,
}

fn bad(form: &Syntax, message: &str) -> Diagnostic {
    rays_diagnostic::error("E_SHAPE".into(), message.into(), Some(form.span.clone()), None, None, Vec::new(), Vec::new())
}

fn metadata_string(bytes: &[u8]) -> String {
    if let Ok(text) = std::str::from_utf8(bytes) {
        return text.to_owned();
    }
    let quoted = flow_syntax::lisp::flat(&flow_syntax::syntax::make(Node::Str(bytes.to_vec())));
    quoted.strip_prefix('"').and_then(|text| text.strip_suffix('"')).unwrap_or(&quoted).to_owned()
}

fn text(form: &Syntax) -> Result<String, Diagnostic> {
    match &form.node {
        Node::Str(bytes) => Ok(metadata_string(bytes)),
        _ => Err(bad(form, "Metadata text must be a string.")),
    }
}

fn byte_text(form: &Syntax) -> Result<Vec<u8>, Diagnostic> {
    match &form.node {
        Node::Str(bytes) => Ok(bytes.clone()),
        _ => Err(bad(form, "Expected a string value.")),
    }
}

fn word(form: &Syntax) -> Result<String, Diagnostic> {
    match &form.node {
        Node::Sym(value) | Node::Kw(value) => Ok(value.clone()),
        Node::Str(bytes) => Ok(metadata_string(bytes)),
        _ => Err(bad(form, "Expected a metadata name.")),
    }
}

fn boolean(form: &Syntax) -> Result<bool, Diagnostic> {
    match &form.node {
        Node::Sym(value) if value == "true" => Ok(true),
        Node::Sym(value) if value == "false" => Ok(false),
        _ => Err(bad(form, "Expected true or false.")),
    }
}

fn number(form: &Syntax) -> Result<f64, Diagnostic> {
    match &form.node {
        Node::Num(value) => match flow_syntax::syntax::number_value(value) {
            Some(number) if number.is_finite() => Ok(number),
            _ => Err(bad(form, "Expected a finite number.")),
        },
        _ => Err(bad(form, "Expected a finite number.")),
    }
}

fn integer(form: &Syntax) -> Result<i64, Diagnostic> {
    let n = number(form)?;
    const OCAML_MIN: f64 = -4_611_686_018_427_387_904.0;
    const OCAML_MAX_EXCLUSIVE: f64 = 4_611_686_018_427_387_904.0;
    if n < OCAML_MIN || n >= OCAML_MAX_EXCLUSIVE || n.fract() != 0.0 { return Err(bad(form, "Expected an integer.")); }
    Ok(n as i64)
}

fn version(form: &Syntax) -> Result<i64, Diagnostic> {
    match &form.node {
        Node::Num(value) => match value.parse::<i64>() {
            Ok(number) if (0..=4_611_686_018_427_387_903).contains(&number) => Ok(number),
            _ => Err(bad(form, "Node version must be a nonnegative integer literal.")),
        },
        _ => Err(bad(form, "Node version must be a nonnegative integer literal.")),
    }
}

fn strings(form: &Syntax) -> Result<Vec<String>, Diagnostic> {
    match &form.node {
        Node::Str(_) => Ok(vec![text(form)?]),
        Node::Vec(values) => values.iter().map(text).collect(),
        _ => Err(bad(form, "Expected a string or a vector of strings.")),
    }
}

fn options(form: &Syntax, allowed: &[&str], values: &[Syntax]) -> Result<Vec<(String, Syntax)>, Diagnostic> {
    let mut result = Vec::new();
    let mut at = 0;
    while at < values.len() {
        if at + 1 >= values.len() { return Err(bad(form, "Metadata consists of :key value pairs.")); }
        let Node::Kw(key) = &values[at].node else { return Err(bad(form, "Metadata consists of :key value pairs.")); };
        if !allowed.contains(&key.as_str()) { return Err(bad(&values[at], &format!("Unknown metadata :{key}."))); }
        if result.iter().any(|(seen, _): &(String, Syntax)| seen == key) { return Err(bad(&values[at], &format!("Duplicate metadata :{key}."))); }
        result.push((key.clone(), values[at + 1].clone())); at += 2;
    }
    Ok(result)
}

trait Options { fn opt(&self, key: &'static str) -> Option<&Syntax>; }
impl Options for [(String, Syntax)] {
    fn opt(&self, key: &'static str) -> Option<&Syntax> {
    self.iter().find(|(name, _)| name == key).map(|(_, value)| value)
    }
}

fn get_text(options: &[(String, Syntax)], key: &'static str, default: &str) -> Result<String, Diagnostic> {
    options.opt(key).map(text).unwrap_or_else(|| Ok(default.into()))
}

fn get_bool(options: &[(String, Syntax)], key: &'static str, default: bool) -> Result<bool, Diagnostic> {
    options.opt(key).map(boolean).unwrap_or(Ok(default))
}

fn title(name: &str) -> String {
    let mut chars = name.chars().map(|ch| if ch == '_' { ' ' } else { ch });
    match chars.next() {
        Some(first) => {
            let mut result = String::new();
            result.push(first.to_ascii_uppercase());
            result.extend(chars);
            result
        }
        None => String::new(),
    }
}

fn validation(options: &[(String, Syntax)]) -> Result<Vec<Validation>, Diagnostic> {
    let code = get_text(options, "code", "E_PARAM")?;
    let message = get_text(options, "message", "Node parameter validation failed.")?;
    for key in ["code", "message"] {
        if let Some(form) = options.opt(key) {
            if text(form)?.trim().is_empty() { return Err(bad(form, "Validation code and message must be nonempty.")); }
        }
    }
    Ok(options.opt("validate").map(|predicate| vec![Validation { predicate: predicate.clone(), code, message }]).unwrap_or_default())
}

fn facts(form: &Syntax) -> Result<catalog::KernelFacts, Diagnostic> {
    let Node::Map(entries) = &form.node else { return Err(bad(form, ":facts is a metadata map.")); };
    let options = options(form, &["elementwise", "reads", "writes", "topology", "preserves_topology", "exact", "cook-mode"], entries)?;
    let elementwise = match options.opt("elementwise").map(word).transpose()?.as_deref() {
        None | Some("none") | Some("irregular") => KernelElements::Irregular,
        Some("points") => KernelElements::Points,
        Some("primitives") => KernelElements::Primitives,
        _ => return Err(bad(options.opt("elementwise").unwrap_or(form), "Elementwise is points, primitives or irregular.")),
    };
    if let Some(mode) = options.opt("cook-mode") {
        match word(mode)?.as_str() {
            "generator" | "generic" | "modifier" | "filter" | "analysis" | "irregular" => (),
            _ => return Err(bad(mode, "Invalid cook mode.")),
        }
    }
    let topology = options.opt("topology").map(|value| match word(value)?.as_str() {
        "preserved" | "unchanged" => Ok(true), "changed" => Ok(false),
        _ => Err(bad(value, "Topology is preserved or changed.")),
    }).transpose()?;
    let preserves_topology = options.opt("preserves_topology").map(boolean).transpose()?.or(topology).unwrap_or(false);
    Ok(catalog::KernelFacts {
        elementwise,
        reads: options.opt("reads").map(strings).transpose()?.unwrap_or_default(),
        writes: options.opt("writes").map(strings).transpose()?.unwrap_or_default(),
        preserves_topology,
        exact: get_bool(&options, "exact", true)?,
    })
}

const PARAMETER_KEYS: &[&str] = &[
    "label", "description", "folder", "min", "max", "hard_min", "hard_max", "hard-min", "hard-max",
    "primary", "unit", "kind", "choices", "impact", "vec3", "required", "rest", "keyword",
    "validate", "code", "message", "normalize",
];

fn range_value(options: &[(String, Syntax)], first: &'static str, alias: &'static str) -> Result<Option<f64>, Diagnostic> {
    match (options.opt(first), options.opt(alias)) {
        (Some(value), Some(_)) => Err(bad(value, &format!("Duplicate metadata :{first}."))),
        (Some(value), _) | (_, Some(value)) => number(value).map(Some),
        _ => Ok(None),
    }
}
fn range_integer(options: &[(String, Syntax)], first: &'static str, alias: &'static str) -> Result<Option<i64>, Diagnostic> {
    match (options.opt(first), options.opt(alias)) {
        (Some(value), Some(_)) => Err(bad(value, &format!("Duplicate metadata :{first}."))),
        (Some(value), _) | (_, Some(value)) => integer(value).map(Some),
        _ => Ok(None),
    }
}

fn field_value(
    name: String, label: String, description: Option<String>, folder: Vec<String>, impact: Impact,
    primary: bool, unit: Option<String>, vec3: Option<(String, i64)>, kind: KindView, value: Value,
) -> FieldView {
    FieldView { name, label, description, folder, impact, primary, unit, vec3, kind, default: value.clone(), current: value }
}

fn parameter(
    form: &Syntax, pn: &str, ty: &Ty, default_form: &Syntax, options: &[(String, Syntax)],
    fields: &mut Vec<FieldView>, normalizers: &mut Vec<(String, Syntax)>,
) -> Result<(), Diagnostic> {
    let label = get_text(options, "label", &title(pn))?;
    let description = options.opt("description").map(text).transpose()?;
    let folder = options.opt("folder").map(strings).transpose()?.unwrap_or_default();
    let primary = get_bool(options, "primary", false)?;
    let unit = options.opt("unit").map(text).transpose()?;
    let impact = match options.opt("impact").map(word).transpose()?.as_deref() {
        None | Some("cook") => Impact::Cook, Some("view") => Impact::View, Some("export") => Impact::Export,
        _ => return Err(bad(options.opt("impact").unwrap_or(form), "Impact is cook, view or export.")),
    };
    let vec3 = match options.opt("vec3") {
        None => None,
        Some(form) => match &form.node {
            Node::Vec(pair) if pair.len() == 2 => {
                let group = text(&pair[0])?; let index = integer(&pair[1])?;
                if !(0..=2).contains(&index) { return Err(bad(form, "A vec3 component is 0, 1 or 2.")); }
                Some((group, index))
            }
            _ => return Err(bad(form, ":vec3 is [\"group\" component].")),
        },
    };
    let has_ranges = ["min", "max", "hard_min", "hard_max", "hard-min", "hard-max"].iter().any(|key| options.opt(key).is_some());
    if !matches!(ty, Ty::Float | Ty::Int | Ty::Vec3) && has_ranges { return Err(bad(form, "Ranges require a numeric parameter.")); }
    let choices = match options.opt("choices") {
        None => Vec::new(),
        Some(value) => match &value.node {
            Node::Vec(values) => {
                let labels: Vec<String> = values.iter().map(text).collect::<Result<_, _>>()?;
                let mut unique = labels.clone(); unique.sort(); unique.dedup();
                if labels.is_empty() || labels.len() != unique.len() { return Err(bad(value, "Choices must be nonempty and unique.")); }
                labels
            }
            _ => return Err(bad(value, "Choices are a vector of strings.")),
        },
    };
    let kind_name = match options.opt("kind") { Some(v) => word(v)?, None => if choices.is_empty() { ty::to_string(ty) } else { "choice".to_owned() } };
    match (kind_name.as_str(), options.opt("normalize")) {
        ("encoded", Some(fn_form)) if ty == &Ty::Text => normalizers.push((pn.into(), fn_form.clone())),
        ("encoded", None) => return Err(bad(form, "An encoded parameter requires a pure :normalize text-to-text function.")),
        (_, Some(_)) => return Err(bad(form, "Only encoded text parameters have :normalize.")),
        _ => (),
    }
    let soft_min_float = options.opt("min").map(number).transpose()?.unwrap_or(0.0);
    let soft_max_float = options.opt("max").map(number).transpose()?.unwrap_or(1.0);
    let integer_soft = if *ty == Ty::Int {
        Some((options.opt("min").map(integer).transpose()?.unwrap_or(0), options.opt("max").map(integer).transpose()?.unwrap_or(1)))
    } else { None };
    if integer_soft.is_some_and(|(min, max)| min > max) || (integer_soft.is_none() && soft_min_float > soft_max_float) {
        return Err(bad(form, "Soft minimum exceeds maximum."));
    }
    let hard_min_float = range_value(options, "hard_min", "hard-min")?;
    let hard_max_float = range_value(options, "hard_max", "hard-max")?;
    let integer_hard = if *ty == Ty::Int {
        Some((range_integer(options, "hard_min", "hard-min")?, range_integer(options, "hard_max", "hard-max")?))
    } else { None };
    if integer_hard.is_some_and(|(min, max)| min.zip(max).is_some_and(|(min, max)| min > max))
        || (integer_hard.is_none() && hard_min_float.zip(hard_max_float).is_some_and(|(min, max)| min > max))
    { return Err(bad(form, "Hard minimum exceeds maximum.")); }
    let bounded = |value: f64| -> Result<(), Diagnostic> {
        if !value.is_finite() || hard_min_float.is_some_and(|min| value < min) || hard_max_float.is_some_and(|max| value > max) {
            Err(bad(default_form, "Default is outside the hard range."))
        } else { Ok(()) }
    };
    let (kind, value) = match (kind_name.as_str(), ty) {
        ("bool" | "toggle", Ty::Bool) => (KindView::Toggle, Value::Bool(boolean(default_form)?)),
        ("float" | "floating", Ty::Float | Ty::Vec3) => {
            let value = if *ty == Ty::Vec3 { 0.0 } else { number(default_form)? };
            if *ty != Ty::Vec3 { bounded(value)?; }
            (KindView::Floating(param::FloatRange { soft_min: soft_min_float, soft_max: soft_max_float, hard_min: hard_min_float, hard_max: hard_max_float }), Value::Float(value))
        }
        ("int" | "integer", Ty::Int) => {
            let (lo, hi) = integer_soft.unwrap_or((0, 1));
            let (hard_min, hard_max) = integer_hard.unwrap_or((None, None));
            let value = integer(default_form)?; bounded(value as f64)?;
            (KindView::Integer(param::IntRange { soft_min: lo, soft_max: hi, hard_min, hard_max }), Value::Int(value))
        }
        ("text" | "encoded" | "ramp", Ty::Text) => (KindView::Text, Value::Text(byte_text(default_form)?)),
        ("choice", Ty::Text) => {
            let value = text(default_form)?;
            if !choices.contains(&value) { return Err(bad(default_form, "Default is not one of the choices.")); }
            (KindView::Choice(choices.clone()), Value::Choice(value))
        }
        ("vec3", Ty::Vec3) => (KindView::Floating(param::FloatRange { soft_min: soft_min_float, soft_max: soft_max_float, hard_min: hard_min_float, hard_max: hard_max_float }), Value::Float(0.0)),
        _ => return Err(bad(form, "Parameter kind does not match its type.")),
    };
    if !choices.is_empty() && kind_name != "choice" { return Err(bad(form, ":choices requires kind choice.")); }
    if *ty == Ty::Vec3 {
        match &default_form.node {
            Node::Vec(values) if values.len() == 3 => {
                for (index, value_form) in values.iter().enumerate() {
                    let suffix = ["x", "y", "z"][index];
                    let value = number(value_form)?; bounded(value)?;
                    fields.push(field_value(format!("{pn}_{suffix}"), format!("{label} {}", suffix.to_ascii_uppercase()), description.clone(), folder.clone(), impact, primary, unit.clone(), Some((pn.into(), index as i64)), kind.clone(), Value::Float(value)));
                }
            }
            _ => return Err(bad(default_form, "A vec3 default contains three finite numbers.")),
        }
    } else {
        if let Value::Float(value) = &value { bounded(*value)?; }
        fields.push(field_value(pn.into(), label, description, folder, impact, primary, unit, vec3, kind, value));
    }
    Ok(())
}

pub fn valid_name(name: &str) -> bool {
    name.strip_prefix("sop/").is_some_and(|key| !key.contains('/') && symbol::valid_name(key))
}

pub fn parse(form: &Syntax) -> Result<Defnode, Diagnostic> {
    let (name, rest) = match &form.node {
        Node::List(items) if items.len() >= 2 && matches!(&items[0].node, Node::Sym(head) if head == "defnode") && matches!(&items[1].node, Node::Sym(name) if valid_name(name)) => {
            let Node::Sym(name) = &items[1].node else { return Err(bad(form, "A node is (defnode sop/name :label \"Name\" :category [\"Folder\"] [parameters] body).")); };
            (name.clone(), &items[2..])
        }
        _ => return Err(bad(form, "A node is (defnode sop/name :label \"Name\" :category [\"Folder\"] [parameters] body).")),
    };
    let mut header_values = Vec::new();
    let mut at = 0;
    while at < rest.len() {
        if matches!(&rest[at].node, Node::Kw(_)) && at + 1 < rest.len() {
            header_values.push(rest[at].clone()); header_values.push(rest[at + 1].clone()); at += 2;
        } else { break; }
    }
    if rest.len() != at + 2 { return Err(bad(form, "A defnode needs metadata, a parameter vector and exactly one body.")); }
    let Node::Vec(parameters_form) = &rest[at].node else { return Err(bad(form, "A defnode needs metadata, a parameter vector and exactly one body.")); };
    let body = rest[at + 1].clone();
    let hs = options(form, &["label", "operation", "version", "category", "description", "facts", "validate", "code", "message"], &header_values)?;
    let short = name.rsplit('/').next().unwrap_or(&name);
    let operation = get_text(&hs, "operation", short)?;
    if !symbol::valid_name(&operation) { return Err(bad(form, ":operation is a nonempty stable operation name.")); }
    let mut used = Vec::<String>::new();
    let mut fields = Vec::new(); let mut slots = Vec::new(); let mut keywords = Vec::new(); let mut normalizers = Vec::new();
    let mut validations = validation(&hs)?;
    let mut inputs = Vec::new();
    for parameter_form in parameters_form {
        let values = match &parameter_form.node { Node::List(values) => values, _ => return Err(bad(parameter_form, "Each unique input is (name : type default? :metadata value ...).")) };
        if values.len() < 3 || !matches!(&values[1].node, Node::Sym(colon) if colon == ":") { return Err(bad(parameter_form, "Each unique input is (name : type default? :metadata value ...).")); }
        let Node::Sym(pn) = &values[0].node else { return Err(bad(parameter_form, "Each unique input is (name : type default? :metadata value ...).")); };
        if !macros::valid_name(pn) || symbol::reserved(pn) || used.iter().any(|seen| seen == pn) { return Err(bad(parameter_form, "Each unique input is (name : type default? :metadata value ...).")); }
        let ty = ty::of_syntax(&values[2]).ok_or_else(|| bad(parameter_form, "Unknown parameter type."))?;
        used.push(pn.clone());
        let mut tail = &values[3..];
        let default = if tail.first().is_some_and(|value| matches!(&value.node, Node::Kw(_))) || tail.is_empty() { None } else { let result = Some(tail[0].clone()); tail = &tail[1..]; result };
        let opts = options(parameter_form, PARAMETER_KEYS, tail)?;
        validations.extend(validation(&opts)?);
        let slot = default.is_none() || ty == ty::geometry() || ty == ty::image() || matches!(ty, Ty::Fn(_));
        if slot {
            if opts.opt("normalize").is_some() { return Err(bad(parameter_form, "Only encoded text parameters have :normalize.")); }
            if default.is_some() { return Err(bad(parameter_form, "Typed slots have no default; use :required false for an optional slot.")); }
            if ["min", "max", "hard_min", "hard_max", "hard-min", "hard-max", "choices", "kind", "vec3"].iter().any(|key| opts.opt(key).is_some()) { return Err(bad(parameter_form, "Range and kind metadata belongs to parameters with defaults.")); }
            let required = get_bool(&opts, "required", true)?;
            let rest = get_bool(&opts, "rest", false)?;
            slots.push(Slot { name: pn.clone(), required, rest, ty: if ty == ty::geometry() { None } else { Some(ty.clone()) } });
            if get_bool(&opts, "keyword", false)? {
                let port = match &ty {
                    Ty::Fn(Some(signature)) => PortType::Fn((**signature).clone()),
                    Ty::Named(name) if name == "image" => PortType::Image,
                    _ => return Err(bad(parameter_form, "Keyword slots must have image or typed fn types.")),
                };
                if !required || rest { return Err(bad(parameter_form, "Keyword slots must be required and cannot repeat.")); }
                keywords.push(catalog::Parameter { name: pn.clone(), label: pn.clone(), ty: Some(port), fields: Vec::new(), folder: Vec::new(), primary: true, unit: None });
            }
        } else {
            if ["required", "rest", "keyword"].iter().any(|key| opts.opt(key).is_some()) { return Err(bad(parameter_form, "Only typed slots have :required, :rest and :keyword.")); }
            let default_form = default.as_ref().ok_or_else(|| bad(parameter_form, "Each unique input is (name : type default? :metadata value ...)."))?;
            parameter(parameter_form, pn, &ty, default_form, &opts, &mut fields, &mut normalizers)?;
        }
        inputs.push((pn.clone(), if slot && get_bool(&opts, "rest", false)? { Ty::List(Box::new(ty)) } else { ty }, default));
    }
    for (index, slot) in slots.iter().enumerate() {
        if slot.rest && index + 1 != slots.len() { return Err(bad(form, "A rest slot must be the final slot.")); }
    }
    let mut field_names: Vec<&str> = fields.iter().map(|field| field.name.as_str()).collect();
    field_names.sort(); field_names.dedup();
    if field_names.len() != fields.len() { return Err(bad(form, "Overlapping parameter field names.")); }
    let parameters = catalog::parameters_of_fields(&fields).map_err(|_| bad(form, "A vec3 group contains three contiguous float fields in the same folder."))?;
    let keyword_inputs: Vec<String> = keywords.iter().map(|parameter| parameter.name.clone()).collect();
    let mut kind_slots = slots.clone(); kind_slots.retain(|slot| !keyword_inputs.contains(&slot.name));
    let mut kind_parameters = parameters; kind_parameters.extend(keywords);
    let kind = catalog::Kind {
        qualified: name.clone(), context: context::sop(), slots: kind_slots, parameters: kind_parameters,
        outputs: vec![("geo".into(), PortType::Geometry)], facts: hs.opt("facts").map(facts).transpose()?,
    };
    Ok(Defnode {
        kind, slots, keyword_inputs, fields, label: get_text(&hs, "label", &title(short))?, operation,
        version: hs.opt("version").map(version).transpose()?.unwrap_or(1),
        category: hs.opt("category").map(strings).transpose()?.unwrap_or_default(),
        description: hs.opt("description").map(text).transpose()?, inputs, body, validations, normalizers, form: form.clone(),
    })
}
#[cfg(test)]
mod tests {
    use super::*;
    use flow_syntax::syntax;

    #[test]
    fn parses_slots_parameters_and_version() {
        let source = "(defnode sop/test_node :label \"Test\" :version 2 [(geo : geometry) (size : float 1 :min 0 :max 2)] geo)";
        let forms = syntax::parse(source).expect("valid syntax");
        let node = parse(&forms[0]).expect("valid defnode");
        assert_eq!(node.kind.qualified, "sop/test_node");
        assert_eq!(node.kind.slots, vec![Slot { name: "geo".into(), required: true, rest: false, ty: None }]);
        assert_eq!(node.kind.parameters[0].name, "size");
        assert_eq!(node.version, 2);
        let malformed = syntax::parse("(defnode sop/test_node :version 2.0 [] nil)").expect("valid syntax");
        assert_eq!(parse(&malformed[0]).unwrap_err().code, "E_SHAPE");
    }
    #[test]
    fn parses_vec3_fields_and_rejects_bad_metadata() {
        let source = r#"(defnode sop/scale [(geo : geometry) (scale_x : float 1 :vec3 ["scale" 0]) (scale_y : float 2 :vec3 ["scale" 1]) (scale_z : float 3 :vec3 ["scale" 2])] geo)"#;
        let forms = syntax::parse(source).expect("valid syntax");
        let node = parse(&forms[0]).expect("valid defnode");
        assert_eq!(node.fields.len(), 3);
        assert_eq!(node.kind.parameters.len(), 1);
        assert_eq!(node.kind.parameters[0].name, "scale");
        let malformed = syntax::parse("(defnode sop/test [(geo : geometry) (x : float 1 :unknown 2)] geo)").expect("valid syntax");
        assert_eq!(parse(&malformed[0]).unwrap_err().code, "E_SHAPE");
    }
    #[test]
    fn preserves_text_default_bytes() {
        let source = r#"(defnode sop/byte_text [(geo : geometry) (caption : text "\255")] geo)"#;
        let forms = syntax::parse(source).expect("valid syntax");
        let node = parse(&forms[0]).expect("valid defnode");
        assert_eq!(node.fields[0].default, Value::Text(vec![255]));
    }
}

// Port of lib/flow/test_defnode.ml.
#[cfg(test)]
mod test_defnode {
    use crate::{catalog, op, workspace as W};
    use flow_syntax::{context, syntax, ty::Ty};
    use rays_diagnostic::Diagnostic;

    fn catalog() -> catalog::Catalog {
        let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../../lib/flow_sop/flow_manifest.sexp");
        let text = std::fs::read_to_string(path).expect("flow_manifest.sexp");
        catalog::catalog_of_manifest(&text).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d))).0
    }
    fn parse(text: &str) -> Vec<syntax::Syntax> { syntax::parse(text).unwrap() }
    fn source(node: &str, call: &str) -> String { format!("(workspace w {node} (graph g :context sop {call}))") }
    fn lines(ds: &[Diagnostic]) -> String { ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n") }
    fn good(c: &catalog::Catalog, text: &str) -> W::Workspace {
        match W::check(&[], false, false, None, c, &parse(text)) { (Some(w), _) => w, (_, ds) => panic!("{}
{text}", lines(&ds)) }
    }
    fn bad(c: &catalog::Catalog, code: &str, text: &str) {
        match W::check(&[], false, false, None, c, &parse(text)) {
            (None, ds) if ds.iter().any(|d| d.code == code) => (),
            (_, ds) => panic!("Expected {code}: {}\n{text}", lines(&ds)),
        }
    }
    fn node(ps: &str) -> String { format!("(defnode sop/test_node :label \"Test\" :category [\"Deform\" \"Test\"] {ps} geo)") }

    #[test]
    fn context_seed_operator_is_refused_inside_node_bodies() {
        let c = catalog();
        let mut seed = op::find(&[], "length", &context::value()).unwrap();
        seed.name = "sop/context_seed".into();
        seed.signature = op::Signature { pos: vec![("namespace".into(), Ty::Text)], opt: vec![], rest: None, kw: vec![] };
        seed.out = std::sync::Arc::new(|_| Ok(Ty::Int));
        seed.any_num = false; seed.arithmetic = None; seed.packed_extension = None;
        let ops = vec![seed];
        let check = |text: &str| W::check(&ops, false, false, None, &c, &parse(text));
        let context_bad = |code: &str, text: &str| match check(text) {
            (None, ds) if ds.iter().any(|d| d.code == code) => (),
            (_, ds) => panic!("Expected {code}: {}", lines(&ds)),
        };
        context_bad("E_NODE_CONTEXT", "(workspace w (graph g :context value (sop/context_seed \"fixture\")))");
        for declaration in [
            "(defnode sop/context_fixture :validate (= (sop/context_seed \"fixture\") 0) [(geo : geometry)] geo)",
            "(defn seeded :context value [] (sop/context_seed \"fixture\"))\n\n      (defnode sop/context_fixture :validate (= (seeded) 0) [(geo : geometry)] geo)",
        ] { context_bad("E_NODE_CONTEXT", &source(declaration, "nil")); }
        for namespace in ["namespace", "\" \""] {
            context_bad("E_SEED_NAMESPACE", &source(&format!("(defnode sop/context_fixture [(geo : geometry) (namespace : text \"fixture\")]\n(let* [seed (sop/context_seed {namespace})] geo))"), "(sop/context_fixture nil)"));
        }
        match check(&source("(defnode sop/context_fixture [(geo : geometry)]\n\n    (let* [seed (sop/context_seed \"fixture\")] geo))", "(sop/context_fixture nil)")) {
            (Some(_), _) => (),
            (_, ds) => panic!("{}", lines(&ds)),
        }
    }

    #[test]
    fn typed_slots_metadata_validations_and_malformed_declarations() {
        let c = catalog();
        let w = good(&c, &source("(defnode sop/test_node :label \"Test\" :category [\"Deform\" \"Test\"] :description \"A checked body\"\n    :facts {:elementwise points :reads [\"P\" \"N\"] :writes [\"P\"] :topology preserved :exact true}\n    :validate (> count 0) :code \"E_COUNT\" :message \"Count must be positive\"\n    [(geo : geometry :required false)\n     (distance : float 0.1 :label \"Distance\" :description \"Displacement\" :folder [\"Shape\"]\n       :min -10 :max 10 :hard_min -20 :hard_max 20 :primary true :unit \"m\" :impact cook\n       :validate (< distance 20) :code \"E_DISTANCE\" :message \"Distance is too large\")\n     (count : int 2 :min 1 :max 16 :hard-min 1 :hard-max 100)\n     (active : bool true :kind toggle)\n     (name : text \"abc\" :kind text)\n     (ramp : text \"0:0,1:1\" :kind ramp)\n     (mode : text \"First\" :kind choice :choices [\"First\" \"Second\"])\n     (scale : vec3 [1 2 3] :min 0 :max 10)] geo)", "(sop/test_node nil :distance 2 :count 3)"));
        let d = &w.nodes[0];
        assert!(d.label == "Test" && d.category == vec!["Deform", "Test"] && d.description.as_deref() == Some("A checked body"));
        assert!(d.fields.len() == 9 && d.validations.len() == 2, "{} {}", d.fields.len(), d.validations.len());
        let distance = &d.fields[0];
        assert!(distance.label == "Distance" && distance.description.as_deref() == Some("Displacement") && distance.folder == vec!["Shape"] && distance.primary && distance.unit.as_deref() == Some("m") && distance.impact == param::Impact::Cook);
        assert!(d.kind.facts.as_ref().unwrap().preserves_topology);
        assert!(d.kind.slots[0].ty.is_none() && !d.kind.slots[0].required);
        assert!(matches!(&w.graphs[0].body.node, W::Node::Call { kind, .. } if kind == "sop/test_node"));
        good(&c, &source("(defnode sop/test_node [(geo : geometry :rest true)] (sop/merge geo))", "(sop/test_node nil nil)"));
        let grouped = good(&c, &source("(defnode sop/test_node [(geo : geometry)\n    (scale_x : float 1 :vec3 [\"scale\" 0]) (scale_y : float 2 :vec3 [\"scale\" 1])\n    (scale_z : float 3 :vec3 [\"scale\" 2])] geo)", "(sop/test_node nil :scale [4 5 6])"));
        assert_eq!(grouped.nodes[0].kind.parameters.iter().map(|p| p.name.clone()).collect::<Vec<_>>(), vec!["scale"]);
        good(&c, &source("(defnode sop/typed_slots [(geo : geometry) (image : image) (field : (fn [vec3] float))] geo)", "(sop/typed_slots nil (image/noise :width 1 :height 1) (fn [(p : vec3)] p.x))"));
        let keyword_slots = good(&c, &source("(defnode sop/keyword_slots [(geo : geometry)\n    (image : image :keyword true) (field : (fn [vec3] float) :keyword true)] geo)", "(sop/keyword_slots nil :image (image/noise :width 1 :height 1) :field (fn [(p : vec3)] p.x))"));
        let k = &keyword_slots.nodes[0];
        assert!(k.slots.len() == 3 && k.kind.slots.len() == 1);
        assert_eq!(k.keyword_inputs, vec!["image", "field"]);
        assert!(k.kind.parameters.iter().all(|p| p.fields.is_empty() && p.primary));
        let typed_map = good(&c, &source("(defnode sop/typed_map [(geo : geometry) (field : (fn [vec3] vec3))]\n    (let* [mapped (map field (array/vec3 1))] geo))", "nil"));
        match &typed_map.nodes[0].body.body.node {
            W::Node::Let(bindings, _) if bindings.len() == 1 => assert_eq!(bindings[0].1.ty, Ty::Array(Box::new(Ty::Vec3))),
            other => panic!("{other:?}"),
        }
        for call in ["(let* [field (fn [p] (+ p [1 0 0]))] (sop/rest_fields nil field field))", "(sop/rest_fields nil (list (fn [p] (+ p [1 0 0])) (fn [p] (* p 2))))"] {
            good(&c, &source("(defnode sop/rest_fields [(geo : geometry) (fields : (fn [vec3] vec3) :rest true)] geo)", call));
        }
        for (call, code) in [("(field [0 0 0] [1 1 1])", "E_ARITY"), ("(field \"bad\")", "E_TYPE")] {
            bad(&c, code, &source(&format!("(defnode sop/typed_call [(geo : geometry) (field : (fn [vec3] vec3))]\n(let* [value {call}] geo))"), "nil"));
        }
        for ps in [
            "[(geo : geometry) (x : float 0 :unknown 1)]", "[(geo : geometry) (x : float 0 :min 1 :min 2)]", "[(geo : geometry) (x : float 0 :min)]",
            "[(geo : geometry) (x : not_a_type 0)]", "[(geo : geometry) (x : float)] bad", "[(geo : geometry) (geo : float 0)]",
            "[(geo : geometry) (x : float 0 :label false)]", "[(geo : geometry) (x : float 0 :description 1)]", "[(geo : geometry) (x : float 0 :folder [1])]",
            "[(geo : geometry) (x : float 0 :primary 1)]", "[(geo : geometry) (x : float 0 :unit true)]", "[(geo : geometry) (x : float 0 :impact invalid)]",
            "[(geo : geometry) (x : float 0 :vec3 [\"a\" 3])]", "[(geo : geometry) (x : float 0 :vec3 [\"a\" 0])]", "[(geo : geometry) (x : float 0 :kind bool)]",
            "[(geo : geometry) (x : float 0 :min 2 :max 1)]", "[(geo : geometry) (x : float 0 :hard_min 2 :hard_max 1)]", "[(geo : geometry) (x : float 0 :hard_min 1)]",
            "[(geo : geometry) (x : int 0.5)]", "[(geo : geometry) (x : bool 0)]", "[(geo : geometry) (x : text 0)]", "[(geo : geometry) (x : text \"A\" :choices [])]",
            "[(geo : geometry) (x : text \"A\" :choices [\"A\" \"A\"])]", "[(geo : geometry) (x : text \"B\" :choices [\"A\"])]", "[(geo : geometry) (x : float 0 :choices [\"A\"])]",
            "[(geo : geometry) (x : vec3 [1 2])]", "[(geo : geometry 0)]", "[(geo : geometry :rest true) (other : geometry)]", "[(geo : geometry :required 0)]",
            "[(geo : geometry) (x : float 0 :rest true)]", "[(geo : geometry :keyword true)]", "[(geo : geometry) (image : image :keyword true :required false)]",
            "[(geo : geometry) (field : (fn [vec3] float) :keyword true :rest true)]", "[(geo : geometry) (field : (fn [vec3] float) :keyword 1)]", "[(geo : geometry) (x : float 0 :keyword true)]",
        ] { bad(&c, "E_SHAPE", &source(&node(ps), "(sop/test_node nil)")); }
        for definition in [
            "(defnode sop/test_node)", "(defnode sop/test_node [] nil nil)", "(defnode sop/test_node :facts [] [] nil)", "(defnode sop/test_node :facts {:elementwise vertices} [] nil)",
            "(defnode sop/test_node :facts {:reads [1]} [] nil)", "(defnode sop/test_node :facts {:topology false} [] nil)", "(defnode sop/test_node :facts {:exact 1} [] nil)",
            "(defnode sop/test_node :facts {:other 1} [] nil)", "(defnode sop/test_node :category [1] [] nil)",
        ] { bad(&c, "E_SHAPE", &source(definition, "nil")); }
        bad(&c, "E_NAME", &source("(defnode sop/box [] nil)", "nil"));
        bad(&c, "E_NAME", &source("(defnode test_node [] nil)", "nil"));
        bad(&c, "E_TYPE", &source("(defnode sop/test_node [] 1)", "nil"));
        bad(&c, "E_TYPE", &source("(defnode sop/test_node :validate 1 [] nil)", "nil"));
        bad(&c, "E_RECURSION", &source("(defnode sop/test_node [(geo : geometry)] (sop/test_node geo))", "nil"));
        bad(&c, "E_RECURSION", &source("(defnode sop/test_node [(geo : geometry)] (sop/other_node geo))\n    (defnode sop/other_node [(geo : geometry)] (sop/test_node geo))", "nil"));
        bad(&c, "E_TYPE", &source(&node("[(geo : geometry)]"), "(sop/test_node 1)"));
        bad(&c, "E_HARD_RANGE", &source(&node("[(geo : geometry) (x : float 0 :hard_min -1 :hard_max 1)]"), "(sop/test_node nil :x 2)"));
        bad(&c, "E_UNKNOWN_PARAM", &source(&node("[(geo : geometry)]"), "(sop/test_node nil :bad 1)"));
    }

    #[test]
    fn version_metadata_is_an_exact_nonnegative_integer() {
        let c = catalog();
        let declaration = |metadata: &str| format!("(defnode sop/versioned {metadata} [(geo : geometry)] geo)");
        for (metadata, expected) in [("".to_owned(), 1i64), (":version 0".into(), 0), (":version 2".into(), 2), (format!(":version {}", (1i64 << 62) - 1), (1i64 << 62) - 1)] {
            let form = parse(&declaration(&metadata)).remove(0);
            let parsed = crate::defnode::parse(&form).unwrap();
            let checked = good(&c, &source(&declaration(&metadata), "(sop/versioned nil)"));
            assert!(parsed.version == expected && checked.nodes[0].version == expected, "{metadata}");
            let restored = good(&c, &flow_syntax::lisp::flat(&checked.source[0]));
            assert_eq!(restored.nodes[0].version, expected);
        }
        for metadata in [":version -1", ":version 2.5", ":version 2.0", ":version true", ":version \"2\"", ":version 9223372036854775808", ":version 2 :version 3"] {
            bad(&c, "E_SHAPE", &source(&declaration(metadata), "(sop/versioned nil)"));
        }
    }
}
