use crate::port_type::{self, PortType};
use flow_syntax::context::{self, Context};
use flow_syntax::syntax::{Node, Syntax};
use flow_syntax::ty::{self, Ty};
use param::{self, FieldView, KindView, Value};
use rays_diagnostic::{self, Diagnostic, Severity};

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub label: String,
    pub ty: Option<PortType>,
    pub fields: Vec<(String, KindView, Value)>,
    pub folder: Vec<String>,
    pub primary: bool,
    pub unit: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Slot {
    pub name: String,
    pub required: bool,
    pub rest: bool,
    pub ty: Option<Ty>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum KernelElements {
    Points,
    Primitives,
    Irregular,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KernelFacts {
    pub elementwise: KernelElements,
    pub reads: Vec<String>,
    pub writes: Vec<String>,
    pub preserves_topology: bool,
    pub exact: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Kind {
    pub qualified: String,
    pub context: Context,
    pub slots: Vec<Slot>,
    pub parameters: Vec<Parameter>,
    pub outputs: Vec<(String, PortType)>,
    pub facts: Option<KernelFacts>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Catalog {
    pub version: i64,
    pub kinds: Vec<Kind>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Term {
    pub node: TermNode,
    pub ty: Option<PortType>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TermNode {
    Literal(Value),
    Nil,
    Vector(Vec<Term>),
    Reference(String, String),
}

fn trim_ascii(value: &str) -> &str {
    value.trim_matches(|ch| matches!(ch, ' ' | '\t' | '\n' | '\r' | '\u{000c}'))
}

fn format_g(value: f64) -> String {
    if value == 0.0 {
        return if value.is_sign_negative() { "-0".into() } else { "0".into() };
    }
    let scientific = format!("{value:.5e}");
    let Some((mantissa, exponent)) = scientific.split_once('e') else { return scientific; };
    let exponent = exponent.parse::<i32>().unwrap_or(0);
    let mantissa = mantissa.trim_end_matches('0').trim_end_matches('.');
    if (-4..6).contains(&exponent) {
        let decimals = (5 - exponent).max(0) as usize;
        let fixed = format!("{:.*}", decimals, value);
        if fixed.contains('.') { fixed.trim_end_matches('0').trim_end_matches('.').to_owned() } else { fixed }
    } else {
        let sign = if exponent < 0 { "-" } else { "+" };
        format!("{mantissa}e{sign}{:02}", exponent.abs())
    }
}

fn symbol(form: &Syntax) -> Option<&str> {
    match &form.node {
        Node::Sym(name) => Some(name),
        _ => None,
    }
}

fn list(form: &Syntax) -> Option<&[Syntax]> {
    match &form.node {
        Node::List(items) => Some(items),
        _ => None,
    }
}

fn bad(form: &Syntax, message: &str) -> Diagnostic {
    rays_diagnostic::error(
        "E_CATALOG".into(), message.into(), Some(form.span.clone()), None, None, Vec::new(), Vec::new(),
    )
}

// methods, not free fns: a method's output borrows from `self` without a lifetime parameter
trait Tags {
    fn tagged(&self, tag: &'static str) -> Result<&[Syntax], Diagnostic>;
    fn one(&self, tag: &'static str) -> Result<&Syntax, Diagnostic>;
}
impl Tags for Syntax {
    fn tagged(&self, tag: &'static str) -> Result<&[Syntax], Diagnostic> {
        let form = self;
    match list(form) {
        Some(items) if items.first().and_then(symbol) == Some(tag) => Ok(&items[1..]),
        _ => Err(bad(form, &format!("Expected ({tag} …) in Flow manifest"))),
    }
    }
    fn one(&self, tag: &'static str) -> Result<&Syntax, Diagnostic> {
        let form = self;
        let fields = form.tagged(tag)?;
        if fields.len() == 1 { Ok(&fields[0]) } else { Err(bad(form, &format!("{tag} needs one value"))) }
    }
}

fn metadata_string(bytes: &[u8]) -> String {
    if let Ok(text) = std::str::from_utf8(bytes) {
        return text.to_owned();
    }
    let quoted = flow_syntax::lisp::flat(&flow_syntax::syntax::make(Node::Str(bytes.to_vec())));
    quoted.strip_prefix('"').and_then(|text| text.strip_suffix('"')).unwrap_or(&quoted).to_owned()
}

fn string(form: &Syntax) -> Result<String, Diagnostic> {
    match &form.node {
        Node::Str(bytes) => Ok(metadata_string(bytes)),
        _ => Err(bad(form, "Expected a quoted string in Flow manifest")),
    }
}
fn byte_string(form: &Syntax) -> Result<Vec<u8>, Diagnostic> {
    match &form.node {
        Node::Str(bytes) => Ok(bytes.clone()),
        _ => Err(bad(form, "Expected a quoted string in Flow manifest")),
    }
}

fn word(form: &Syntax) -> Result<String, Diagnostic> {
    match &form.node {
        Node::Sym(text) | Node::Num(text) => Ok(text.clone()),
        _ => Err(bad(form, "Expected an atom in Flow manifest")),
    }
}

fn integer(form: &Syntax) -> Result<i64, Diagnostic> {
    let value = word(form)?.parse::<i64>().map_err(|_| bad(form, "Invalid number in Flow manifest"))?;
    const MIN: i64 = -(1i64 << 62);
    const MAX: i64 = (1i64 << 62) - 1;
    if value < MIN || value > MAX { Err(bad(form, "Invalid number in Flow manifest")) } else { Ok(value) }
}

fn floating(form: &Syntax) -> Result<f64, Diagnostic> {
    let value = word(form)?.parse::<f64>().map_err(|_| bad(form, "Invalid number in Flow manifest"))?;
    if value.is_finite() { Ok(value) } else { Err(bad(form, "Non-finite number in Flow manifest")) }
}

fn boolean(form: &Syntax) -> Result<bool, Diagnostic> {
    match word(form)?.as_str() {
        "true" => Ok(true),
        "false" => Ok(false),
        _ => Err(bad(form, "Expected true or false in Flow manifest")),
    }
}

fn optional_integer(form: &Syntax) -> Result<Option<i64>, Diagnostic> {
    if word(form)? == "nil" { Ok(None) } else { integer(form).map(Some) }
}

fn optional_float(form: &Syntax) -> Result<Option<f64>, Diagnostic> {
    if word(form)? == "nil" { Ok(None) } else { floating(form).map(Some) }
}

fn strings(form: &Syntax, tag: &'static str) -> Result<Vec<String>, Diagnostic> {
    form.tagged(tag)?.iter().map(string).collect()
}

fn property(fields: &[Syntax], parent: &Syntax, tag: &'static str) -> Result<Syntax, Diagnostic> {
    match fields.iter().find(|form| list(form).and_then(|items| items.first()).and_then(symbol) == Some(tag)) {
        Some(form) => Ok(form.clone()),
        None => Err(bad(parent, &format!("Missing {tag} in Flow manifest"))),
    }
}

fn literal(form: &Syntax) -> Result<Value, Diagnostic> {
    match list(form) {
        Some(items) if items.len() == 2 => match symbol(&items[0]) {
            Some("bool") => boolean(&items[1]).map(Value::Bool),
            Some("int") => integer(&items[1]).map(Value::Int),
            Some("float") => floating(&items[1]).map(Value::Float),
            Some("text") => byte_string(&items[1]).map(Value::Text),
            Some("choice") => string(&items[1]).map(Value::Choice),
            _ => Err(bad(form, "Unknown literal kind in Flow manifest")),
        },
        _ => Err(bad(form, "Malformed literal in Flow manifest")),
    }
}

fn kind_view(form: &Syntax) -> Result<KindView, Diagnostic> {
    let Some(items) = list(form) else { return Err(bad(form, "Unknown field kind in Flow manifest")); };
    match items {
        [head] if symbol(head) == Some("bool") => Ok(KindView::Toggle),
        [head] if symbol(head) == Some("text") => Ok(KindView::Text),
        [head, soft, hard] if symbol(head) == Some("int") || symbol(head) == Some("float") => {
            let softs = soft.tagged("soft")?;
            let hards = hard.tagged("hard")?;
            if softs.len() != 2 || hards.len() != 2 { return Err(bad(form, "Malformed numeric range in Flow manifest")); }
            if symbol(head) == Some("int") {
                Ok(KindView::Integer(param::IntRange {
                    soft_min: integer(&softs[0])?, soft_max: integer(&softs[1])?,
                    hard_min: optional_integer(&hards[0])?, hard_max: optional_integer(&hards[1])?,
                }))
            } else {
                Ok(KindView::Floating(param::FloatRange {
                    soft_min: floating(&softs[0])?, soft_max: floating(&softs[1])?,
                    hard_min: optional_float(&hards[0])?, hard_max: optional_float(&hards[1])?,
                }))
            }
        }
        [head, labels @ ..] if symbol(head) == Some("choice") => Ok(KindView::Choice(labels.iter().map(string).collect::<Result<Vec<_>, _>>()?)),
        _ => Err(bad(form, "Unknown field kind in Flow manifest")),
    }
}

fn field(form: &Syntax) -> Result<FieldView, Diagnostic> {
    let values = form.tagged("field")?;
    if values.len() < 7 { return Err(bad(form, "Malformed field in Flow manifest")); }
    let (name, label, folder, kind_form, default_form, primary_form, vec3_form) =
        (&values[0], &values[1], &values[2], &values[3], &values[4], &values[5], &values[6]);
    let mut unit = None;
    let mut description = None;
    let mut impact = param::Impact::Cook;
    let mut seen = Vec::<String>::new();
    for metadata in &values[7..] {
        match list(metadata) {
            Some(items) if items.len() == 2 => match symbol(&items[0]) {
                Some(key @ ("unit" | "description" | "impact")) if !seen.iter().any(|value| value == key) => {
                    seen.push(key.into());
                    match key {
                        "unit" => unit = Some(string(&items[1])?),
                        "description" => description = Some(string(&items[1])?),
                        _ => impact = match word(&items[1])?.as_str() {
                            "cook" => param::Impact::Cook,
                            "view" => param::Impact::View,
                            "export" => param::Impact::Export,
                            _ => return Err(bad(&items[1], "Unknown field impact in Flow manifest")),
                        },
                    }
                }
                _ => return Err(bad(metadata, "Malformed or duplicate field metadata in Flow manifest")),
            },
            _ => return Err(bad(metadata, "Malformed or duplicate field metadata in Flow manifest")),
        }
    }
    let default = literal(default_form)?;
    let vec3 = match vec3_form.tagged("vec3")? {
        [] => None,
        [name, index] => Some((string(name)?, integer(index)?)),
        _ => return Err(bad(vec3_form, "Malformed Vec3 group in Flow manifest")),
    };
    Ok(FieldView {
        name: string(name)?, label: string(label)?, description, folder: strings(folder, "folder")?, impact,
        primary: boolean(primary_form.one("primary")?)?, unit, vec3, kind: kind_view(kind_form)?,
        default: default.clone(), current: default,
    })
}

fn type_name(form: &Syntax) -> Result<String, Diagnostic> {
    match &form.node { Node::Str(_) => string(form), _ => word(form) }
}

fn port_type(form: &Syntax) -> Result<PortType, Diagnostic> {
    match type_name(form)?.as_str() {
        "geometry" => Ok(PortType::Geometry), "float" => Ok(PortType::Float), "int" => Ok(PortType::Int),
        "bool" => Ok(PortType::Bool), "vec3" => Ok(PortType::Vec3), "image" => Ok(PortType::Image),
        name => match ty::of_string(name) {
            Some(Ty::Fn(Some(signature))) => Ok(PortType::Fn(*signature)),
            _ => Err(bad(form, "Unknown port type in Flow manifest")),
        },
    }
}

fn slot(form: &Syntax) -> Result<Slot, Diagnostic> {
    let values = form.tagged("slot")?;
    if values.len() < 2 || values.len() > 3 { return Err(bad(form, "Malformed slot in Flow manifest")); }
    let ty = if values.len() == 2 { None } else {
        match ty::of_string(&type_name(&values[2])?) {
            Some(value @ (Ty::Named(_) | Ty::Fn(Some(_)))) => Some(value),
            _ => return Err(bad(&values[2], "Unknown slot type")),
        }
    };
    let (required, rest) = match word(&values[1])?.as_str() {
        "required" => (true, false), "optional" => (false, false), "rest" => (true, true),
        "optional-rest" => (false, true), _ => return Err(bad(&values[1], "Unknown slot requirement")),
    };
    Ok(Slot { name: string(&values[0])?, required, rest, ty })
}

fn facts(form: &Syntax) -> Result<KernelFacts, Diagnostic> {
    let fields = form.tagged("facts")?;
    let mut names = Vec::with_capacity(fields.len());
    for field in fields {
        match list(field).and_then(|items| items.first()) {
            Some(head) => names.push(word(head)?),
            None => return Err(bad(field, "Malformed kernel facts")),
        }
    }
    names.sort();
    let required = ["cook-mode", "elementwise", "exact", "reads", "topology", "writes"].map(str::to_owned);
    if names != required {
        return Err(bad(form, "Kernel facts require cook-mode, elementwise, exact, reads, topology and writes exactly once"));
    }
    let cook = property(fields, form, "cook-mode")?;
    match cook.tagged("cook-mode")? {
        [mode] if ["generator", "generic"].contains(&word(mode)?.as_str()) => (),
        [mode, index] if ["duplicate-input", "in-place", "instance-input", "passthrough"].contains(&word(mode)?.as_str()) && integer(index)? >= 0 => (),
        _ => return Err(bad(form, "Invalid cook mode in kernel facts")),
    }
    let elements_form = property(fields, form, "elementwise")?;
    let elements = word(elements_form.one("elementwise")?)?;
    let elementwise = match elements.as_str() {
        "points" => KernelElements::Points, "primitives" => KernelElements::Primitives,
        "none" => KernelElements::Irregular, _ => return Err(bad(form, "Invalid elementwise class in kernel facts")),
    };
    let topology_form = property(fields, form, "topology")?;
    let topology = word(topology_form.one("topology")?)?;
    let preserves_topology = match topology.as_str() {
        "preserved" => true, "changed" => false, _ => return Err(bad(form, "Invalid topology class in kernel facts")),
    };
    let reads_form = property(fields, form, "reads")?;
    let writes_form = property(fields, form, "writes")?;
    let reads = strings(&reads_form, "reads")?;
    let writes = strings(&writes_form, "writes")?;
    if reads.iter().chain(&writes).any(|name| trim_ascii(name).is_empty()) {
        return Err(bad(form, "Blank component name in kernel facts"));
    }
    let exact_form = property(fields, form, "exact")?;
    Ok(KernelFacts {
        elementwise, reads, writes, preserves_topology,
        exact: boolean(exact_form.one("exact")?)?,
    })
}

fn kind(form: &Syntax) -> Result<Kind, Diagnostic> {
    let values = form.tagged("kind")?;
    if values.is_empty() { return Err(bad(form, "Malformed kind in Flow manifest")); }
    let qualified = string(&values[0])?;
    let properties = &values[1..];
    let get = |tag: &'static str| property(properties, form, tag);
    let key_form = get("key")?;
    let key = string(key_form.one("key")?)?;
    let operation_form = get("operation")?;
    let _operation = string(operation_form.one("operation")?)?;
    let label_form = get("label")?;
    let _label = string(label_form.one("label")?)?;
    let category_form = get("category")?;
    let _category = strings(&category_form, "category")?;
    let facts_forms: Vec<&Syntax> = properties.iter().filter(|item| list(item).and_then(|xs| xs.first()).and_then(symbol) == Some("facts")).collect();
    let facts = match facts_forms.as_slice() {
        [] => None, [value] => Some(facts(value)?), _ => return Err(bad(form, "Duplicate kernel facts")),
    };
    let slots_form = get("slots")?;
    let slots: Vec<Slot> = slots_form.tagged("slots")?.iter().map(slot).collect::<Result<_, _>>()?;
    let fields_form = get("fields")?;
    let fields: Vec<FieldView> = fields_form.tagged("fields")?.iter().map(field).collect::<Result<_, _>>()?;
    let keyword_forms: Vec<&Syntax> = properties.iter().filter(|item| list(item).and_then(|xs| xs.first()).and_then(symbol) == Some("keyword-inputs")).collect();
    let keywords = match keyword_forms.as_slice() {
        [] => Vec::new(), [value] => strings(value, "keyword-inputs")?, _ => return Err(bad(form, "Duplicate keyword inputs")),
    };
    let mut uniq = keywords.clone(); uniq.sort(); uniq.dedup();
    if uniq.len() != keywords.len() { return Err(bad(form, "Duplicate keyword input name")); }
    let mut keyword_parameters = Vec::new();
    for name in &keywords {
        match slots.iter().find(|slot| slot.name == *name) {
            Some(Slot { required: true, rest: false, ty: Some(Ty::Fn(Some(signature))), .. }) => keyword_parameters.push(Parameter {
                name: name.clone(), label: name.clone(), ty: Some(PortType::Fn((**signature).clone())), fields: Vec::new(), folder: Vec::new(), primary: true, unit: None,
            }),
            Some(Slot { required: true, rest: false, ty: Some(Ty::Named(ty_name)), .. }) if ty_name == "image" => keyword_parameters.push(Parameter {
                name: name.clone(), label: name.clone(), ty: Some(PortType::Image), fields: Vec::new(), folder: Vec::new(), primary: true, unit: None,
            }),
            _ => return Err(bad(form, "Keyword inputs must name required slots")),
        }
    }
    let slots: Vec<Slot> = slots.into_iter().filter(|slot| !keywords.contains(&slot.name)).collect();
    let mut parameters = parameters_of_fields(&fields).map_err(|message| bad(form, &message))?;
    if parameters.iter().any(|parameter| keywords.contains(&parameter.name)) { return Err(bad(form, "Keyword input clashes with a scalar parameter")); }
    let mut outputs = Vec::new();
    let outputs_form = get("outputs")?;
    for output in outputs_form.tagged("outputs")? {
        match output.tagged("output")? {
            [name, ty] => outputs.push((string(name)?, port_type(ty)?)),
            _ => return Err(bad(output, "Malformed output in Flow manifest")),
        }
    }
    let context = context::of_qualified(&qualified).ok_or_else(|| bad(form, "Unknown kind namespace in Flow manifest"))?;
    if !qualified.as_bytes().strip_suffix(key.as_bytes()).is_some_and(|prefix| prefix.last() == Some(&b'/')) {
        return Err(bad(form, "Kind key differs from its qualified name"));
    }
    parameters.append(&mut keyword_parameters);
    Ok(Kind { qualified, context, slots, parameters, outputs, facts })
}

fn raw(source: &str, form: &Syntax) -> Result<String, Diagnostic> {
    let start = usize::try_from(form.span.start).map_err(|_| bad(form, "Invalid Flow manifest span"))?;
    let finish = usize::try_from(form.span.finish).map_err(|_| bad(form, "Invalid Flow manifest span"))?;
    source.get(start..finish).map(str::to_owned).ok_or_else(|| bad(form, "Invalid Flow manifest span"))
}

pub fn catalog_of_manifest(source: &str) -> Result<(Catalog, String), Diagnostic> {
    let forms = flow_syntax::syntax::parse(source)?;
    if forms.len() != 1 { return Err(rays_diagnostic::error("E_CATALOG".into(), "Flow manifest needs one top-level form".into(), None, None, None, Vec::new(), Vec::new())); }
    let root = &forms[0];
    let values = root.tagged("flow_manifest")?;
    if values.len() != 3 { return Err(bad(root, "Flow manifest needs version, digest and kinds")); }
    let version = integer(values[0].one("version")?)?;
    let digest_form = values[1].one("digest")?;
    let digest = string(digest_form)?;
    if version < 0 { return Err(bad(root, "Negative Flow catalog version")); }
    if digest.len() != 32 || !digest.bytes().all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte)) {
        return Err(bad(root, "Flow manifest digest must be 32 lowercase hex digits"));
    }
    let kind_forms = values[2].tagged("kinds")?;
    let mut linked = Vec::new();
    for form in kind_forms {
        let fields = form.tagged("kind")?;
        if let Some(name) = fields.first() {
            if string(name)?.starts_with("sop/") { linked.push(raw(source, form)?); }
        }
    }
    let payload = format!("{}\n(kinds{})", raw(source, &values[0])?, if linked.is_empty() { String::new() } else { format!(" {}", linked.join(" ")) });
    if md5_hex(payload.as_bytes()) != digest { return Err(bad(digest_form, "Flow manifest digest does not match its contents")); }
    let mut kinds = Vec::with_capacity(kind_forms.len());
    for form in kind_forms { kinds.push(kind(form)?); }
    Ok((Catalog { version, kinds }, digest))
}

pub fn resolve_component_names(lookup: &dyn Fn(&str) -> Option<String>, names: &[String]) -> Vec<String> {
    let mut result = Vec::new();
    for name in names {
        if !name.starts_with('$') { result.push(name.clone()); continue; }
        let body = &name[1..];
        if let Some((parameter, fallback)) = body.split_once('|') {
            result.push(lookup(parameter).map(|value| trim_ascii(&value).to_owned()).filter(|value| !value.is_empty()).unwrap_or_else(|| fallback.to_owned()));
        } else if let Some((parameter, prefix)) = body.split_once('~') {
            if let Some(value) = lookup(parameter).map(|value| trim_ascii(&value).to_owned()).filter(|value| value.starts_with(prefix)) {
                let rest = trim_ascii(&value[prefix.len()..]).to_owned(); if !rest.is_empty() { result.push(rest); }
            }
        } else if let Some(value) = lookup(body).map(|value| trim_ascii(&value).to_owned()).filter(|value| !value.is_empty()) {
            result.push(value);
        }
    }
    result
}

pub fn short(qualified: &str) -> String {
    short_name(qualified).to_owned()
}
fn short_name(qualified: &str) -> &str {
    match qualified.split_once('/') {
        Some((_, name)) if !name.contains('/') => name,
        _ => qualified,
    }
}


fn distance(left: &str, right: &str) -> usize {
    let mut row: Vec<usize> = (0..=right.len()).collect();
    for (i, a) in left.bytes().enumerate() {
        let mut diagonal = row[0]; row[0] = i + 1;
        for (j, b) in right.bytes().enumerate() {
            let above = row[j + 1];
            row[j + 1] = (row[j + 1] + 1).min(row[j] + 1).min(diagonal + usize::from(a != b));
            diagonal = above;
        }
    }
    row[right.len()]
}

pub fn suggestion(word: &str, candidates: &[String]) -> String {
    let mut best: Option<(usize, &str)> = None;
    for candidate in candidates {
        let candidate = candidate.as_str();
        let score = distance(word, candidate);
        if score <= 2 && best.as_ref().is_none_or(|current| (score, candidate) < (current.0, current.1)) {
            best = Some((score, candidate));
        }
    }
    best.map_or_else(String::new, |(_, candidate)| format!(" Did you mean {candidate}?"))
}
fn suggestion_catalog(word: &str, catalog: &Catalog) -> String {
    let mut best: Option<(usize, &str)> = None;
    for kind in &catalog.kinds {
        let candidate = short_name(&kind.qualified);
        let score = distance(word, candidate);
        if score <= 2 && best.as_ref().is_none_or(|current| (score, candidate) < (current.0, current.1)) {
            best = Some((score, candidate));
        }
    }
    best.map_or_else(String::new, |(_, candidate)| format!(" Did you mean {candidate}?"))
}

pub fn parameters_of_fields(fields: &[FieldView]) -> Result<Vec<Parameter>, String> {
    let groups = param::group_vec3(fields, false)?;
    let mut result = Vec::with_capacity(groups.len());
    for group in groups {
        let Some(first) = group.first() else { return Err("Empty parameter field group".into()); };
        let (name, ty, unit) = match &first.vec3 {
            Some((group, _)) => (group.clone(), Some(PortType::Vec3), None),
            None => (first.name.clone(), port_type::of_field_kind(&first.kind), first.unit.clone()),
        };
        result.push(Parameter {
            name, label: first.label.clone(), ty,
            fields: group.iter().map(|field| (field.name.clone(), field.kind.clone(), field.default.clone())).collect(),
            folder: first.folder.clone(), primary: first.primary, unit,
        });
    }
    Ok(result)
}

fn known_prefix(catalog: &Catalog, name: &str) -> bool {
    name == "user" || context::all().iter().any(|context| {
        let descriptor = context::descriptor(*context);
        descriptor.name == name || descriptor.catalog_prefix.as_deref() == Some(name)
    }) || catalog.kinds.iter().any(|kind| kind.qualified.strip_prefix(name).is_some_and(|suffix| suffix.starts_with('/')))
}

fn allowed(context: &Context, kind: &Kind) -> bool { kind.context == context::value() || kind.context == *context }

fn resolve_report(catalog: &Catalog, context: &Context, head: &str, report: &mut dyn FnMut(&str, String)) -> Option<Kind> {
    let mut candidates: Vec<&Kind> = catalog.kinds.iter().filter(|kind| {
        if head.contains('/') { kind.qualified == head } else { short_name(&kind.qualified) == head }
    }).collect();
    candidates.sort_by(|left, right| left.qualified.cmp(&right.qualified));
    candidates.dedup_by(|left, right| left.qualified == right.qualified);
    match candidates.as_slice() {
        [kind] if allowed(context, kind) => Some((*kind).clone()),
        [kind] => { report("E_WRONG_CONTEXT", format!("{} is a {} node and cannot appear in a {} graph", kind.qualified, context::name(kind.context), context::name(*context))); None }
        [_, _, ..] => { report("E_AMBIGUOUS", format!("{head} is ambiguous: {}. Write the namespace to choose", candidates.iter().map(|kind| kind.qualified.as_str()).collect::<Vec<_>>().join(" or "))); None }
        [] => {
            match head.split('/').collect::<Vec<_>>().as_slice() {
                [prefix, _, ..] if !known_prefix(catalog, prefix) => report("E_NAMESPACE", format!("Unknown namespace {prefix}.")),
                _ => report("E_UNKNOWN_KIND", format!("Unknown node {head}.{}", suggestion_catalog(short_name(head), catalog))),
            }
            None
        }
    }
}

pub fn resolve_kind(catalog: &Catalog, context: &Context, head: &str) -> Result<Kind, (String, String)> {
    let mut failure = None;
    match resolve_report(catalog, context, head, &mut |code, message| failure = Some((code.into(), message))) {
        Some(kind) => Ok(kind), None => Err(failure.unwrap_or_else(|| ("E_UNKNOWN_KIND".into(), format!("Unknown node {head}")))),
    }
}

fn numeric_value(value: &Value) -> Option<f64> {
    match value { Value::Int(n) => Some(*n as f64), Value::Float(n) => Some(*n), Value::Bool(value) => Some(if *value { 1.0 } else { 0.0 }), Value::Text(_) | Value::Choice(_) => None }
}

fn validate_range(report: &mut dyn FnMut(Severity, &str, &str), parameter: &Parameter, field: &(String, KindView, Value), value: &Value) {
    match (&field.1, numeric_value(value)) {
        (KindView::Integer(range), Some(number)) => {
            if number.fract() == 0.0 {
                if range.hard_min.is_some_and(|bound| number < bound as f64) || range.hard_max.is_some_and(|bound| number > bound as f64) {
                    report(Severity::ErrorSeverity, "E_HARD_RANGE", &format!(":{} is outside its hard range", parameter.name));
                } else if number < range.soft_min as f64 || number > range.soft_max as f64 {
                    report(Severity::Warning, "W_SOFT_RANGE", &format!(":{} {} is outside the slider range {}–{}. Allowed, but check it", parameter.name, format_g(number), range.soft_min, range.soft_max));
                }
            }
        }
        (KindView::Floating(range), Some(number)) => {
            if !number.is_finite() { report(Severity::ErrorSeverity, "E_NONFINITE", &format!(":{} must be finite", parameter.name)); }
            else if range.hard_min.is_some_and(|bound| number < bound) || range.hard_max.is_some_and(|bound| number > bound) {
                report(Severity::ErrorSeverity, "E_HARD_RANGE", &format!(":{} is outside its hard range", parameter.name));
            } else if number < range.soft_min || number > range.soft_max {
                report(Severity::Warning, "W_SOFT_RANGE", &format!(":{} {} is outside the slider range {}–{}. Allowed, but check it", parameter.name, format_g(number), format_g(range.soft_min), format_g(range.soft_max)));
            }
        }
        _ => (),
    }
}

pub fn validate_parameter(report: &mut dyn FnMut(Severity, &str, &str), parameter: &Parameter, term: &Term) {
    let good = match (&parameter.ty, &term.node) {
        (None, TermNode::Literal(Value::Text(_) | Value::Choice(_))) => true,
        (None, _) => false,
        (Some(PortType::Vec3), TermNode::Vector(components)) => components.iter().all(|component| component.ty.as_ref().is_some_and(|source| port_type::can_connect(source, &PortType::Float))),
        (Some(PortType::Vec3), TermNode::Literal(_)) => false,
        (Some(target), _) => term.ty.as_ref().is_some_and(|source| port_type::can_connect(source, target)),
    };
    if !good {
        report(Severity::ErrorSeverity, "E_TYPE", &format!(":{} takes {}, but this is {}", parameter.name, parameter.ty.as_ref().map_or("text".into(), port_type::name), term.ty.as_ref().map_or("nil or text".into(), port_type::name)));
    }
    if let (Some(PortType::Int), TermNode::Literal(Value::Float(number))) = (&parameter.ty, &term.node) {
        if number.fract() != 0.0 { report(Severity::ErrorSeverity, "E_INT_LITERAL", &format!(":{} is an integer, not {}", parameter.name, format_g(*number))); }
        else if *number >= 4_611_686_018_427_387_904.0 || *number < -4_611_686_018_427_387_904.0 { report(Severity::ErrorSeverity, "E_HARD_RANGE", &format!(":{} is outside the integer range", parameter.name)); }
    }
    if let ([(_, KindView::Choice(options), _)], TermNode::Literal(value)) =
        (parameter.fields.as_slice(), &term.node)
    {
        let valid = match value {
            Value::Choice(label) => options.iter().any(|option| option == label),
            Value::Text(bytes) => std::str::from_utf8(bytes)
                .ok()
                .is_some_and(|label| options.iter().any(|option| option == label)),
            _ => true,
        };
        if !valid {
            report(Severity::ErrorSeverity, "E_TYPE", &format!(":{} must be one of {}", parameter.name, options.join(", ")));
        }
    }
    if good {
        match (&term.node, parameter.fields.as_slice()) {
            (TermNode::Literal(value), [field, ..]) => validate_range(report, parameter, field, value),
            (TermNode::Vector(components), fields) => {
                    for (component, field) in components.iter().zip(fields) {
                        if let TermNode::Literal(value) = &component.node {
                            validate_range(report, parameter, field, value);
                        }
                    }
                }
            _ => (),
        }
    }
}

fn md5_hex(input: &[u8]) -> String {
    const S: [u32; 64] = [7,12,17,22,7,12,17,22,7,12,17,22,7,12,17,22,5,9,14,20,5,9,14,20,5,9,14,20,5,9,14,20,4,11,16,23,4,11,16,23,4,11,16,23,4,11,16,23,6,10,15,21,6,10,15,21,6,10,15,21,6,10,15,21];
    const K: [u32; 64] = [0xd76aa478,0xe8c7b756,0x242070db,0xc1bdceee,0xf57c0faf,0x4787c62a,0xa8304613,0xfd469501,0x698098d8,0x8b44f7af,0xffff5bb1,0x895cd7be,0x6b901122,0xfd987193,0xa679438e,0x49b40821,0xf61e2562,0xc040b340,0x265e5a51,0xe9b6c7aa,0xd62f105d,0x02441453,0xd8a1e681,0xe7d3fbc8,0x21e1cde6,0xc33707d6,0xf4d50d87,0x455a14ed,0xa9e3e905,0xfcefa3f8,0x676f02d9,0x8d2a4c8a,0xfffa3942,0x8771f681,0x6d9d6122,0xfde5380c,0xa4beea44,0x4bdecfa9,0xf6bb4b60,0xbebfbc70,0x289b7ec6,0xeaa127fa,0xd4ef3085,0x04881d05,0xd9d4d039,0xe6db99e5,0x1fa27cf8,0xc4ac5665,0xf4292244,0x432aff97,0xab9423a7,0xfc93a039,0x655b59c3,0x8f0ccc92,0xffeff47d,0x85845dd1,0x6fa87e4f,0xfe2ce6e0,0xa3014314,0x4e0811a1,0xf7537e82,0xbd3af235,0x2ad7d2bb,0xeb86d391];
    let mut data = input.to_vec(); let bit_len = (data.len() as u64).wrapping_mul(8); data.push(0x80);
    while data.len() % 64 != 56 { data.push(0); }
    data.extend_from_slice(&bit_len.to_le_bytes());
    let (mut a0, mut b0, mut c0, mut d0) = (0x67452301u32, 0xefcdab89u32, 0x98badcfeu32, 0x10325476u32);
    for chunk in data.chunks_exact(64) {
        let mut words = [0u32; 16];
        for (index, bytes) in chunk.chunks_exact(4).enumerate() { words[index] = u32::from_le_bytes([bytes[0],bytes[1],bytes[2],bytes[3]]); }
        let (mut a, mut b, mut c, mut d) = (a0,b0,c0,d0);
        for i in 0..64 {
            let (f, g) = if i < 16 { ((b & c) | (!b & d), i) } else if i < 32 { ((d & b) | (!d & c), (5*i+1)%16) } else if i < 48 { (b ^ c ^ d, (3*i+5)%16) } else { (c ^ (b | !d), (7*i)%16) };
            let next = b.wrapping_add(a.wrapping_add(f).wrapping_add(K[i]).wrapping_add(words[g]).rotate_left(S[i]));
            (a,b,c,d) = (d,next,b,c);
        }
        a0=a0.wrapping_add(a); b0=b0.wrapping_add(b); c0=c0.wrapping_add(c); d0=d0.wrapping_add(d);
    }
    let mut output = String::with_capacity(32);
    const HEX: &[u8; 16] = b"0123456789abcdef";
    for word in [a0, b0, c0, d0] {
        for byte in word.to_le_bytes() {
            output.push(HEX[(byte >> 4) as usize] as char);
            output.push(HEX[(byte & 0xf) as usize] as char);
        }
    }
    output
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verifies_md5_and_manifest_digest() {
        assert_eq!(md5_hex(b""), "d41d8cd98f00b204e9800998ecf8427e");
        assert_eq!(md5_hex(b"abc"), "900150983cd24fb0d6963f7d28e17f72");
        let kind = "(kind \"sop/box\" (key \"box\") (operation \"box\") (label \"Box\") (category) (slots) (fields) (outputs (output \"geo\" geometry)))";
        let payload = format!("(version 1)\n(kinds {kind})");
        let digest = md5_hex(payload.as_bytes());
        let manifest = format!("(flow_manifest (version 1) (digest \"{digest}\") (kinds {kind}))");
        let (catalog, actual) = catalog_of_manifest(&manifest).expect("valid manifest");
        assert_eq!(actual, digest);
        assert_eq!(catalog.kinds[0].qualified, "sop/box");
        assert_eq!(catalog.kinds[0].context, context::sop());
        assert!(catalog_of_manifest(&manifest.replace(&digest, "00000000000000000000000000000000")).is_err());
    }

    #[test]
    fn resolves_and_expands_components() {
        let kind = Kind { qualified: "sop/box".into(), context: context::sop(), slots: Vec::new(), parameters: Vec::new(), outputs: Vec::new(), facts: None };
        let catalog = Catalog { version: 1, kinds: vec![kind] };
        assert_eq!(resolve_kind(&catalog, &context::sop(), "box").map(|kind| kind.qualified), Ok("sop/box".into()));
        assert_eq!(resolve_kind(&catalog, &context::scene(), "box").unwrap_err().0, "E_WRONG_CONTEXT");
        let names = vec!["P".into(), "$name".into(), "$missing|N".into(), "$attribute~attr:".into()];
        let resolve = |name: &str| match name { "name" => Some("  N  ".into()), "attribute" => Some("attr: position".into()), _ => None };
        assert_eq!(resolve_component_names(&resolve, &names), vec!["P".to_owned(), "N".to_owned(), "N".to_owned(), "position".to_owned()]);
        assert_eq!(suggestion("boz", &["box".into()]), " Did you mean box?");
    }
    #[test]
    fn groups_vec3_and_checks_parameter_ranges() {
        let range = param::FloatRange { soft_min: 0.0, soft_max: 2.0, hard_min: Some(0.0), hard_max: Some(4.0) };
        let fields: Vec<FieldView> = (0..3).map(|index| {
            let name = ["scale_x", "scale_y", "scale_z"][index as usize];
            FieldView {
                name: name.into(), label: name.into(), description: None, folder: Vec::new(),
                impact: param::Impact::Cook, primary: false, unit: None, vec3: Some(("scale".into(), index)),
                kind: KindView::Floating(range), default: Value::Float(1.0), current: Value::Float(1.0),
            }
        }).collect();
        let grouped = parameters_of_fields(&fields).expect("valid vec3 group");
        assert_eq!(grouped.len(), 1);
        assert_eq!(grouped[0].ty, Some(PortType::Vec3));
        let parameter = Parameter {
            name: "size".into(), label: "Size".into(), ty: Some(PortType::Float),
            fields: vec![("size".into(), KindView::Floating(range), Value::Float(1.0))],
            folder: Vec::new(), primary: false, unit: None,
        };
        let term = Term { node: TermNode::Literal(Value::Float(5.0)), ty: Some(PortType::Float) };
        let mut codes = Vec::new();
        validate_parameter(&mut |_, code, _| codes.push(code.to_owned()), &parameter, &term);
        assert_eq!(codes, vec!["E_HARD_RANGE".to_owned()]);
    }
    #[test]
    fn manifest_text_defaults_preserve_bytes() {
        let kind = r#"(kind "sop/text" (key "text") (operation "text") (label "Text") (category) (slots) (fields (field "caption" "Caption" (folder) (text) (text "\255") (primary false) (vec3))) (outputs))"#;
        let digest = md5_hex(format!("(version 1)\n(kinds {kind})").as_bytes());
        let manifest = format!("(flow_manifest (version 1) (digest \"{digest}\") (kinds {kind}))");
        let (catalog, _) = catalog_of_manifest(&manifest).expect("valid byte-backed text");
        assert_eq!(catalog.kinds[0].parameters[0].fields[0].2, Value::Text(vec![255]));
    }
}

// Port of lib/flow/test_catalog.ml.
#[cfg(test)]
mod test_catalog {
    use super::*;
    use flow_syntax::ty::FnSignature;

    fn param_of(name: &str, label: &str, ty: Option<PortType>, fields: Vec<(String, KindView, Value)>) -> Parameter {
        Parameter { name: name.into(), label: label.into(), ty, fields, folder: vec![], primary: false, unit: None }
    }
    fn size() -> Parameter {
        let range = param::FloatRange { soft_min: 0., soft_max: 2., hard_min: Some(0.), hard_max: Some(4.) };
        param_of("size", "Size", Some(PortType::Float), vec![("size".into(), KindView::Floating(range), Value::Float(1.))])
    }
    fn rows() -> Parameter {
        let range = param::IntRange { soft_min: 2, soft_max: 40, hard_min: Some(1), hard_max: Some(100) };
        param_of("rows", "Rows", Some(PortType::Int), vec![("rows".into(), KindView::Integer(range), Value::Int(10))])
    }
    fn mode() -> Parameter {
        param_of("mode", "Mode", None, vec![("mode".into(), KindView::Choice(vec!["solid".into(), "wire".into()]), Value::Choice("solid".into()))])
    }
    fn boxk() -> Kind {
        Kind { qualified: "sop/box".into(), context: context::sop(), slots: vec![], parameters: vec![size(), rows(), mode()], outputs: vec![("geo".into(), PortType::Geometry)], facts: None }
    }
    fn transform() -> Kind {
        Kind { qualified: "sop/transform".into(), context: context::sop(), slots: vec![Slot { name: "in0".into(), required: true, rest: false, ty: None }], parameters: vec![], outputs: vec![("geo".into(), PortType::Geometry)], facts: None }
    }
    fn catalog() -> Catalog { Catalog { version: 202609, kinds: vec![boxk(), transform()] } }
    fn code(r: &Result<Kind, (String, String)>) -> String { match r { Err((c, _)) => c.clone(), Ok(_) => "ok".into() } }

    #[test]
    fn parameter_validation_of_function_and_image_ports() {
        let signature = FnSignature { params: vec![Ty::Record(vec![("p".into(), Ty::Vec3)])], result: Ty::Float };
        let mut field = size();
        field.ty = Some(PortType::Fn(signature.clone()));
        field.fields = vec![];
        let check = |parameter: &Parameter, ty: PortType| {
            let mut diagnostics = Vec::new();
            validate_parameter(&mut |_, code, _| diagnostics.insert(0, code.to_owned()), parameter, &Term { node: TermNode::Reference("argument".into(), "".into()), ty: Some(ty) });
            diagnostics
        };
        assert!(check(&field, PortType::Fn(signature.clone())).is_empty());
        assert_eq!(check(&field, PortType::Float), vec!["E_TYPE"]);
        assert_eq!(check(&field, PortType::Fn(FnSignature { params: vec![], ..signature.clone() })), vec!["E_TYPE"]);
        let mut image = field.clone(); image.ty = Some(PortType::Image);
        assert!(check(&image, PortType::Image).is_empty());
        assert_eq!(check(&size(), PortType::Image), vec!["E_TYPE"]);
        for target in [PortType::Image, PortType::Fn(signature)] {
            assert!(port_type::coerce(&target, port_type::PortValue::Float(1.)).is_err());
        }
    }

    #[test]
    fn kind_resolution_and_manifest_reader() {
        let catalog = catalog();
        let resolve = |c: Context, name: &str| resolve_kind(&catalog, &c, name);
        assert_eq!(resolve(context::sop(), "box").map(|k| k.qualified), Ok("sop/box".into()));
        assert!(resolve(context::sop(), "sop/transform").is_ok());
        assert_eq!(code(&resolve(context::sop(), "boxx")), "E_UNKNOWN_KIND");
        assert_eq!(code(&resolve(context::sop(), "nope/box")), "E_NAMESPACE");
        assert_eq!(code(&resolve(context::scene(), "sop/box")), "E_WRONG_CONTEXT");
        let mut scene_box = boxk(); scene_box.qualified = "scene/box".into(); scene_box.context = context::scene();
        let ambiguous = Catalog { kinds: vec![boxk(), transform(), scene_box], ..catalog.clone() };
        assert_eq!(code(&resolve_kind(&ambiguous, &context::sop(), "box")), "E_AMBIGUOUS");
        assert!(resolve_kind(&ambiguous, &context::sop(), "sop/box").is_ok());
        match catalog_of_manifest("(flow_manifest (version 1) (digest \"wrong\") (kinds))") {
            Err(d) => assert!(d.code == "E_CATALOG" && d.message == "Flow manifest digest must be 32 lowercase hex digits", "{d:?}"),
            Ok(_) => panic!("accepted malformed Flow manifest digest"),
        }
        let manifest = |facts: &str| {
            let kind = format!("(kind \"sop/box\" (key \"box\") (operation \"box\")\n(label \"Box\") (category \"Create\") (slots) (fields) (outputs) {facts})");
            let digest = md5_hex(format!("(version 1)\n(kinds {kind})").as_bytes());
            format!("(flow_manifest (version 1) (digest \"{digest}\") (kinds {kind}))")
        };
        let facts = |elementwise: &str, reads: &str, mode: &str, tail: &str| format!("(facts (cook-mode {mode}) (elementwise {elementwise}) (reads {reads}) (writes \"P\") (topology preserved) {tail})");
        let valid = facts("points", "\"P\"", "duplicate-input 0", "(exact false)");
        let (parsed, _) = catalog_of_manifest(&manifest(&valid)).unwrap();
        assert_eq!(parsed.kinds[0].facts, Some(KernelFacts { elementwise: KernelElements::Points, reads: vec!["P".into()], writes: vec!["P".into()], preserves_topology: true, exact: false }));
        for declaration in [
            facts("pixels", "\"P\"", "generic", "(exact false)"),
            facts("points", "\"\"", "generic", "(exact false)"),
            facts("points", "\"P\"", "duplicate-input -1", "(exact false)"),
            facts("points", "\"P\"", "generic", "(exact false) (exact true)"),
            facts("points", "\"P\"", "generic", ""),
            format!("{valid} {valid}"),
        ] {
            match catalog_of_manifest(&manifest(&declaration)) { Err(d) => assert_eq!(d.code, "E_CATALOG", "{declaration}"), Ok(_) => panic!("accepted invalid kernel facts: {declaration}") }
        }
        let field_manifest = |metadata: &str| {
            let kind = format!("(kind \"sop/test\" (key \"test\") (operation \"test\") (label \"Test\") (category)\n      (slots) (fields (field \"amount\" \"Amount\" (folder) (float (soft 0 1) (hard nil nil))\n        (float 0) (primary false) (vec3) {metadata})) (outputs))");
            let kind = flow_syntax::lisp::flat(&flow_syntax::syntax::parse(&kind).unwrap()[0]);
            format!("(flow_manifest (version 1) (digest \"{}\") (kinds {kind}))", md5_hex(format!("(version 1)\n(kinds {kind})").as_bytes()))
        };
        let (parsed, _) = catalog_of_manifest(&field_manifest("(description \"Explanation\") (impact view) (unit \"m\")")).unwrap();
        assert_eq!(parsed.kinds[0].parameters[0].unit, Some("m".into()));
        for metadata in ["(unit \"m\") (unit \"s\")", "(description \"a\") (description \"b\")", "(impact view) (impact cook)", "(impact invalid)", "(description true)", "(unknown \"x\")"] {
            match catalog_of_manifest(&field_manifest(metadata)) { Err(d) => assert_eq!(d.code, "E_CATALOG", "{metadata}"), Ok(_) => panic!("Accepted malformed parameter metadata: {metadata}") }
        }
    }
}
