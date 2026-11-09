use std::fmt::Write as _;
use std::io::Write as _;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Impact {
    Cook,
    View,
    Export,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Effects {
    pub cook: bool,
    pub view: bool,
    pub export: bool,
}

pub const NO_EFFECTS: Effects = Effects {
    cook: false,
    view: false,
    export: false,
};

pub fn add_impact(impact: Impact, mut effects: Effects) -> Effects {
    match impact {
        Impact::Cook => effects.cook = true,
        Impact::View => effects.view = true,
        Impact::Export => effects.export = true,
    }
    effects
}

pub fn union_effects(left: Effects, right: Effects) -> Effects {
    Effects {
        cook: left.cook || right.cook,
        view: left.view || right.view,
        export: left.export || right.export,
    }
}

pub fn has_effects(value: Effects) -> bool {
    value.cook || value.view || value.export
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IntRange {
    pub soft_min: i64,
    pub soft_max: i64,
    pub hard_min: Option<i64>,
    pub hard_max: Option<i64>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FloatRange {
    pub soft_min: f64,
    pub soft_max: f64,
    pub hard_min: Option<f64>,
    pub hard_max: Option<f64>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Float(f64),
    /// Raw OCaml string bytes; not restricted to UTF-8.
    Text(Vec<u8>),
    Choice(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum KindView {
    Toggle,
    Integer(IntRange),
    Floating(FloatRange),
    Text,
    Choice(Vec<String>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FieldView {
    pub name: String,
    pub label: String,
    pub description: Option<String>,
    pub folder: Vec<String>,
    pub impact: Impact,
    pub primary: bool,
    pub unit: Option<String>,
    pub vec3: Option<(String, i64)>,
    pub kind: KindView,
    pub default: Value,
    pub current: Value,
}

/// Concrete runtime kinds. Custom choice values cross this crate as UTF-8 labels; encoded values
/// use arbitrary bytes, and adapters keep domain types outside the schema.
pub enum Kind {
    Toggle,
    Integer(IntRange),
    Floating(FloatRange),
    Text,
    Choice {
        options: Vec<String>,
        equal: Box<dyn Fn(&str, &str) -> bool + Send + Sync>,
    },
    IndexChoice(Vec<String>),
    Encoded {
        equal: Box<dyn Fn(&[u8], &[u8]) -> bool + Send + Sync>,
        normalize: Box<dyn Fn(&[u8]) -> Result<Vec<u8>, String> + Send + Sync>,
    },
}

pub fn toggle() -> Kind {
    Kind::Toggle
}

pub fn integer(min: i64, max: i64, hard_min: Option<i64>, hard_max: Option<i64>) -> Result<Kind, String> {
    let range = IntRange {
        soft_min: min,
        soft_max: max,
        hard_min,
        hard_max,
    };
    validate_int_range(range)?;
    Ok(Kind::Integer(range))
}

pub fn floating(min: f64, max: f64, hard_min: Option<f64>, hard_max: Option<f64>) -> Result<Kind, String> {
    let range = FloatRange {
        soft_min: min,
        soft_max: max,
        hard_min,
        hard_max,
    };
    validate_float_range(range)?;
    Ok(Kind::Floating(range))
}

pub fn text() -> Kind {
    Kind::Text
}

pub fn choice(labels: Vec<String>) -> Result<Kind, String> {
    choice_with(labels, Box::new(|left, right| left == right))
}

pub fn choice_with(
    labels: Vec<String>,
    equal: Box<dyn Fn(&str, &str) -> bool + Send + Sync>,
) -> Result<Kind, String> {
    validate_choice_labels(&labels)?;
    Ok(Kind::Choice {
        options: labels,
        equal,
    })
}

pub fn index_choice(labels: Vec<String>) -> Result<Kind, String> {
    validate_choice_labels(&labels)?;
    Ok(Kind::IndexChoice(labels))
}

pub fn encoded() -> Kind {
    encoded_with(
        Box::new(|left, right| left == right),
        Box::new(|text| Ok(text.to_owned())),
    )
}

/// Decodes an encoded byte string and returns its canonical form.
pub fn encoded_with(
    equal: Box<dyn Fn(&[u8], &[u8]) -> bool + Send + Sync>,
    normalize: Box<dyn Fn(&[u8]) -> Result<Vec<u8>, String> + Send + Sync>,
) -> Kind {
    Kind::Encoded { equal, normalize }
}

impl Kind {
    fn view(&self) -> KindView {
        match self {
            Kind::Toggle => KindView::Toggle,
            Kind::Integer(range) => KindView::Integer(*range),
            Kind::Floating(range) => KindView::Floating(*range),
            Kind::Text | Kind::Encoded { .. } => KindView::Text,
            Kind::Choice { options, .. } | Kind::IndexChoice(options) => {
                KindView::Choice(options.clone())
            }
        }
    }

    fn validate_type(&self, value: &Value) -> bool {
        matches!(
            (self, value),
            (Kind::Toggle, Value::Bool(_))
                | (Kind::Integer(_), Value::Int(_))
                | (Kind::Floating(_), Value::Float(_))
                | (Kind::Text, Value::Text(_))
                | (Kind::Choice { .. }, Value::Choice(_))
                | (Kind::IndexChoice(_), Value::Int(_))
                | (Kind::Encoded { .. }, Value::Text(_))
        )
    }

    fn equal_value(&self, left: &Value, right: &Value) -> bool {
        match (self, left, right) {
            (Kind::Choice { equal, .. }, Value::Choice(left), Value::Choice(right)) => {
                equal(left, right)
            }
            (Kind::Encoded { equal, .. }, Value::Text(left), Value::Text(right)) => {
                equal(left, right)
            }
            (Kind::Floating(_), Value::Float(left), Value::Float(right)) => left == right,
            _ => left == right,
        }
    }
}
fn display_value(kind: &Kind, value: &Value) -> Result<Value, String> {
    match (kind, value) {
        (Kind::Choice { options, equal }, Value::Choice(value)) => options
            .iter()
            .find(|option| equal(option, value))
            .cloned()
            .map(Value::Choice)
            .ok_or_else(|| "Param.view: choice value is not an option".to_owned()),
        (Kind::Encoded { normalize, .. }, Value::Text(value)) => {
            normalize(value).map(Value::Text)
        }
        _ => Ok(value.clone()),
    }
}

fn ocaml_float_max(left: f64, right: f64) -> f64 {
    if left.is_nan() || right.is_nan() {
        f64::NAN
    } else if left == 0.0 && right == 0.0 {
        if left.is_sign_positive() || right.is_sign_positive() { 0.0 } else { -0.0 }
    } else if left > right {
        left
    } else {
        right
    }
}

fn ocaml_float_min(left: f64, right: f64) -> f64 {
    if left.is_nan() || right.is_nan() {
        f64::NAN
    } else if left == 0.0 && right == 0.0 {
        if left.is_sign_negative() || right.is_sign_negative() { -0.0 } else { 0.0 }
    } else if left < right {
        left
    } else {
        right
    }
}

pub fn normalize_value(kind: &Kind, value: &Value) -> Result<Value, String> {
    if !kind.validate_type(value) {
        return Err("parameter value has the wrong type".to_owned());
    }
    match (kind, value) {
        (Kind::Toggle, Value::Bool(value)) => Ok(Value::Bool(*value)),
        (Kind::Text, Value::Text(value)) => Ok(Value::Text(value.clone())),
        (Kind::Encoded { normalize, .. }, Value::Text(value)) => normalize(value).map(Value::Text),
        (Kind::Integer(range), Value::Int(value)) => {
            let value = range.hard_min.map_or(*value, |low| low.max(*value));
            Ok(Value::Int(range.hard_max.map_or(value, |high| high.min(value))))
        }
        (Kind::Floating(range), Value::Float(value)) => {
            if !value.is_finite() {
                return Err("float parameter must be finite".to_owned());
            }
            let value = range.hard_min.map_or(*value, |low| ocaml_float_max(low, *value));
            Ok(Value::Float(range.hard_max.map_or(value, |high| ocaml_float_min(high, value))))
        }
        (Kind::Choice { options, equal }, Value::Choice(value)) => {
            if options.iter().any(|option| equal(option, value)) {
                Ok(Value::Choice(value.clone()))
            } else {
                Err("choice parameter value is not an option".to_owned())
            }
        }
        (Kind::IndexChoice(labels), Value::Int(value)) => {
            if *value >= 0 && *value < labels.len() as i64 {
                Ok(Value::Int(*value))
            } else {
                Err("choice parameter index is out of bounds".to_owned())
            }
        }
        _ => Err("parameter value has the wrong type".to_owned()),
    }
}

fn validate_normalizable(kind: &Kind, value: &Value) -> Result<(), String> {
    if !kind.validate_type(value) {
        return Err("parameter value has the wrong type".to_owned());
    }
    match (kind, value) {
        (Kind::Floating(_), Value::Float(value)) if !value.is_finite() => {
            Err("float parameter must be finite".to_owned())
        }
        (Kind::Choice { options, equal }, Value::Choice(value))
            if !options.iter().any(|option| equal(option, value)) =>
        {
            Err("choice parameter value is not an option".to_owned())
        }
        (Kind::IndexChoice(labels), Value::Int(value))
            if *value < 0 || *value >= labels.len() as i64 =>
        {
            Err("choice parameter index is out of bounds".to_owned())
        }
        _ => Ok(()),
    }
}

fn normalize_in_place(kind: &Kind, value: &mut Value) -> bool {
    match (kind, &mut *value) {
        (Kind::Integer(range), Value::Int(value)) => {
            let normalized = range
                .hard_max
                .map_or(range.hard_min.map_or(*value, |low| low.max(*value)), |high| {
                    high.min(range.hard_min.map_or(*value, |low| low.max(*value)))
                });
            let changed = *value != normalized;
            *value = normalized;
            changed
        }
        (Kind::Floating(range), Value::Float(value)) => {
            let min_clamped = range.hard_min.map_or(*value, |low| ocaml_float_max(low, *value));
            let normalized = range.hard_max.map_or(min_clamped, |high| ocaml_float_min(high, min_clamped));
            let changed =
                *value != normalized || (*value == 0.0 && value.to_bits() != normalized.to_bits());
            if changed {
                *value = normalized;
            }
            changed
        }
        _ => !kind.equal_value(&*value, &*value),
    }
}
#[derive(Clone)]
struct FieldMeta {
    name: String,
    label: String,
    description: Option<String>,
    folder: Vec<String>,
    impact: Impact,
    primary: bool,
    unit: Option<String>,
    vec3: Option<(String, i64)>,
}

pub struct Field {
    meta: FieldMeta,
    kind: Kind,
    default: Value,
}

pub fn field(name: String, kind: Kind, default: Value) -> Result<Field, String> {
    nonblank("Param.field name", &name)?;
    let default = normalize_value(&kind, &default)
        .map_err(|message| format!("Param.field {name}: {message}"))?;
    Ok(Field {
        meta: FieldMeta {
            label: default_label(&name),
            name,
            description: None,
            folder: Vec::new(),
            impact: Impact::Cook,
            primary: false,
            unit: None,
            vec3: None,
        },
        kind,
        default,
    })
}

impl Field {
    pub fn name(&self) -> &str {
        &self.meta.name
    }

    pub fn kind_view(&self) -> KindView {
        self.kind.view()
    }

    pub fn default_value(&self) -> &Value {
        &self.default
    }

    pub fn label(mut self, label: String) -> Result<Self, String> {
        nonblank("Param.field label", &label)?;
        self.meta.label = label;
        Ok(self)
    }

    pub fn description(mut self, description: String) -> Self {
        self.meta.description = Some(description);
        self
    }

    pub fn folder(mut self, folder: Vec<String>) -> Result<Self, String> {
        for value in &folder {
            nonblank("Param.field folder", value)?;
        }
        self.meta.folder = folder;
        Ok(self)
    }

    pub fn impact(mut self, impact: Impact) -> Self {
        self.meta.impact = impact;
        self
    }

    pub fn primary(mut self, primary: bool) -> Self {
        self.meta.primary = primary;
        self
    }

    pub fn unit(mut self, unit: String) -> Self {
        self.meta.unit = Some(unit);
        self
    }

    pub fn vec3(mut self, group: String, component: i64) -> Result<Self, String> {
        nonblank("Param.field vec3 group", &group)?;
        if !(0..=2).contains(&component) {
            return Err("Param.field: vec3 component must be 0, 1 or 2".to_owned());
        }
        if !matches!(&self.kind, Kind::Floating(_)) {
            return Err("Param.field: vec3 components must be float fields".to_owned());
        }
        self.meta.vec3 = Some((group, component));
        Ok(self)
    }

    fn view(&self, current: &Value) -> Result<FieldView, String> {
        Ok(FieldView {
            name: self.meta.name.clone(),
            label: self.meta.label.clone(),
            description: self.meta.description.clone(),
            folder: self.meta.folder.clone(),
            impact: self.meta.impact,
            primary: self.meta.primary,
            unit: self.meta.unit.clone(),
            vec3: self.meta.vec3.clone(),
            kind: self.kind.view(),
            default: display_value(&self.kind, &self.default)?,
            current: display_value(&self.kind, current)?,
        })
    }
}

pub struct Schema {
    name: String,
    default: Vec<Value>,
    fields: Vec<Field>,
    by_name: std::collections::HashMap<String, usize>,
}

pub fn schema(name: String, fields: Vec<Field>) -> Result<Schema, String> {
    nonblank("Param.schema name", &name)?;
    let mut by_name = std::collections::HashMap::with_capacity(fields.len());
    let mut default = Vec::with_capacity(fields.len());
    for (index, field) in fields.iter().enumerate() {
        if by_name.insert(field.meta.name.clone(), index).is_some() {
            return Err(format!("Param.schema: duplicate field {}", quote(&field.meta.name)));
        }
        let value = normalize_value(&field.kind, &field.default)
            .map_err(|message| format!("Param.schema {name}: {message}"))?;
        default.push(value);
    }
    Ok(Schema {
        name,
        default,
        fields,
        by_name,
    })
}

impl Schema {
    pub fn default(&self) -> &[Value] {
        &self.default
    }

    pub fn fields(&self) -> &[Field] {
        &self.fields
    }

    fn validate_record(&self, record: &[Value]) -> Result<(), String> {
        if record.len() != self.fields.len() {
            return Err("parameter record has the wrong number of fields".to_owned());
        }
        for (field, value) in self.fields.iter().zip(record) {
            if !field.kind.validate_type(value) {
                return Err(format!(
                    "parameter {} received the wrong type",
                    quote(&field.meta.name)
                ));
            }
        }
        Ok(())
    }

    pub fn view(&self, record: &[Value]) -> Result<Vec<FieldView>, String> {
        self.validate_record(record)?;
        self.fields
            .iter()
            .zip(record)
            .map(|(field, current)| field.view(current))
            .collect()
    }
    pub fn key(&self, record: &[Value]) -> Result<Vec<u8>, String> {
        self.encode_key(record, false)
    }

    pub fn cook_key(&self, record: &[Value]) -> Result<Vec<u8>, String> {
        self.encode_key(record, true)
    }

    fn encode_key(&self, record: &[Value], cook_only: bool) -> Result<Vec<u8>, String> {
        self.validate_record(record)?;
        let mut out = Vec::with_capacity(128);
        append_token(&mut out, self.name.as_bytes());
        for (field, value) in self.fields.iter().zip(record) {
            if cook_only && field.meta.impact != Impact::Cook {
                continue;
            }
            append_token(&mut out, field.meta.name.as_bytes());
            append_key_value(&mut out, &field.kind, value, &field.meta.name)?;
        }
        Ok(out)
    }

    pub fn cook_text(&self, record: &[Value]) -> Result<Vec<u8>, String> {
        self.validate_record(record)?;
        let mut out = Vec::new();
        let mut first = true;
        for (field, value) in self.fields.iter().zip(record) {
            if field.meta.impact != Impact::Cook {
                continue;
            }
            if !first {
                out.push(b';');
            }
            first = false;
            out.extend_from_slice(field.meta.name.as_bytes());
            out.push(b'=');
            append_cook_value(&mut out, &field.kind, value, &field.meta.name)?;
        }
        Ok(out)
    }
    pub fn apply_all(
        &self,
        record: &[Value],
        changes: &[(String, Value)],
    ) -> Result<(Vec<Value>, Effects), String> {
        self.validate_record(record)?;
        let mut values = record.to_vec();
        for (field, value) in self.fields.iter().zip(values.iter_mut()) {
            if matches!(&field.kind, Kind::Encoded { .. }) {
                *value = normalize_value(&field.kind, value)?;
            }
        }
        let mut effects = NO_EFFECTS;
        for (name, candidate) in changes {
            let Some(&index) = self.by_name.get(name) else {
                continue;
            };
            let field = &self.fields[index];
            let old = &values[index];
            let value = apply_value(field, old, candidate)?;
            if !field.kind.equal_value(old, &value) {
                values[index] = value;
                effects = add_impact(field.meta.impact, effects);
            }
        }
        Ok((values, effects))
    }
    pub fn normalize(&self, record: &mut [Value]) -> Result<bool, String> {
        self.validate_record(record)?;
        let encoded_count = self
            .fields
            .iter()
            .filter(|field| matches!(&field.kind, Kind::Encoded { .. }))
            .count();
        let mut encoded = Vec::with_capacity(encoded_count);
        for (field, value) in self.fields.iter().zip(record.iter()) {
            validate_normalizable(&field.kind, value)?;
            if let (Kind::Encoded { normalize, .. }, Value::Text(value)) = (&field.kind, value) {
                encoded.push(normalize(value)?);
            }
        }
        if encoded.len() != encoded_count {
            return Err("encoded normalization values are incomplete".to_owned());
        }
        let mut encoded = encoded.into_iter();
        let mut changed = false;
        for (field, value) in self.fields.iter().zip(record.iter_mut()) {
            if matches!(&field.kind, Kind::Encoded { .. }) {
                let Some(normalized) = encoded.next() else {
                    return Err("encoded normalization values are incomplete".to_owned());
                };
                let Value::Text(current) = value else {
                    return Err("encoded parameter value has the wrong type".to_owned());
                };
                if current != &normalized {
                    *value = Value::Text(normalized);
                    changed = true;
                }
            } else {
                changed |= normalize_in_place(&field.kind, value);
            }
        }
        Ok(changed)
    }
}


fn apply_value(field: &Field, old: &Value, candidate: &Value) -> Result<Value, String> {
    let value = match (&field.kind, candidate) {
        (Kind::Choice { options, .. }, Value::Choice(label)) => {
            if !options.iter().any(|option| option == label) {
                return Err(format!(
                    "parameter {} has no choice {}",
                    quote(&field.meta.name),
                    quote(label)
                ));
            }
            candidate.clone()
        }
        _ => normalize_value(&field.kind, candidate).map_err(|message| {
            if message == "parameter value has the wrong type" {
                format!(
                    "parameter {} received the wrong type",
                    quote(&field.meta.name)
                )
            } else {
                message
            }
        })?,
    };
    if !field.kind.validate_type(old) {
        return Err(format!(
            "parameter {} received the wrong type",
            quote(&field.meta.name)
        ));
    }
    let _ = old;
    Ok(value)
}

fn append_key_value(out: &mut Vec<u8>, kind: &Kind, value: &Value, name: &str) -> Result<(), String> {
    match (kind, value) {
        (Kind::Toggle, Value::Bool(value)) => out.extend_from_slice(if *value { b"b1" } else { b"b0" }),
        (Kind::Integer(_), Value::Int(value)) | (Kind::IndexChoice(_), Value::Int(value)) => {
            write!(out, "i{value};").unwrap();
        }
        (Kind::Floating(_), Value::Float(value)) => {
            out.push(b'f');
            out.extend_from_slice(float_text(*value).as_bytes());
            out.push(b';');
        }
        (Kind::Text, Value::Text(value)) => {
            out.push(b's');
            append_token(out, value);
        }
        (Kind::Encoded { normalize, .. }, Value::Text(value)) => {
            out.push(b'e');
            append_token(out, &normalize(value)?);
        }
        (Kind::Choice { options, equal }, Value::Choice(value)) => {
            let label = options
                .iter()
                .find(|label| equal(label, value))
                .ok_or_else(|| "Param.view: choice value is not an option".to_owned())?;
            out.push(b'c');
            append_token(out, label.as_bytes());
        }
        _ => {
            return Err(format!(
                "parameter {} received the wrong type",
                quote(name)
            ));
        }
    }
    Ok(())
}

fn append_cook_value(out: &mut Vec<u8>, kind: &Kind, value: &Value, name: &str) -> Result<(), String> {
    match (kind, value) {
        (Kind::Toggle, Value::Bool(value)) => {
            out.extend_from_slice(if *value { b"true" } else { b"false" });
        }
        (Kind::Integer(_), Value::Int(value)) | (Kind::IndexChoice(_), Value::Int(value)) => {
            write!(out, "{value}").unwrap();
        }
        (Kind::Floating(_), Value::Float(value)) => {
            out.extend_from_slice(float_text(*value).as_bytes());
        }
        (Kind::Text, Value::Text(value)) => out.extend_from_slice(value),
        (Kind::Encoded { normalize, .. }, Value::Text(value)) => {
            out.extend_from_slice(&normalize(value)?);
        }
        (Kind::Choice { options, equal }, Value::Choice(value)) => {
            let label = options
                .iter()
                .find(|label| equal(label, value))
                .ok_or_else(|| "Param.view: choice value is not an option".to_owned())?;
            out.extend(label.bytes().map(|character| match character {
                b'a'..=b'z' | b'0'..=b'9' => character,
                b'A'..=b'Z' => character.to_ascii_lowercase(),
                _ => b'_',
            }));
        }
        _ => {
            return Err(format!(
                "parameter {} received the wrong type",
                quote(name)
            ));
        }
    }
    Ok(())
}


pub fn float_text(value: f64) -> String {
    float_text_with_decimal(value, false)
}

pub fn float_text_with_decimal(value: f64, decimal: bool) -> String {
    cfmt::ocaml_float(value, decimal)
}

pub fn group_vec3(fields: &[FieldView], infer: bool) -> Result<Vec<Vec<FieldView>>, String> {
    let names: Vec<&str> = fields.iter().map(|field| field.name.as_str()).collect();
    let mut fields = fields.to_vec();
    if infer {
        let mut index = 0;
        while index + 2 < fields.len() {
            let x = &fields[index];
            let y = &fields[index + 1];
            let z = &fields[index + 2];
            if is_float(&x.kind)
                && is_float(&y.kind)
                && is_float(&z.kind)
                && x.vec3.is_none()
                && y.vec3.is_none()
                && z.vec3.is_none()
                && x.folder == y.folder
                && x.folder == z.folder
            {
                let group = [("_x", "_y", "_z"), ("_r", "_g", "_b")]
                    .into_iter()
                    .find_map(|(sx, sy, sz)| {
                        if !x.name.ends_with(sx) {
                            return None;
                        }
                        let stem = &x.name[..x.name.len() - 2];
                        (y.name.strip_prefix(stem) == Some(sy)
                            && z.name.strip_prefix(stem) == Some(sz))
                            .then(|| {
                                if names.contains(&stem) {
                                    format!("{stem}_color")
                                } else {
                                    stem.to_owned()
                                }
                            })
                    });
                if let Some(group) = group {
                    fields[index].vec3 = Some((group.clone(), 0));
                    fields[index + 1].vec3 = Some((group.clone(), 1));
                    fields[index + 2].vec3 = Some((group, 2));
                    index += 3;
                    continue;
                }
            }
            index += 1;
        }
    }
    let mut seen = Vec::<String>::new();
    let mut result = Vec::new();
    let mut index = 0;
    while index < fields.len() {
        let field = &fields[index];
        match &field.vec3 {
            None => {
                result.push(vec![field.clone()]);
                index += 1;
            }
            Some((name, 0)) if !names.contains(&name.as_str()) && !seen.contains(name) => {
                if index + 2 >= fields.len() {
                    return Err(format!("Invalid vec3 group {name}"));
                }
                let y = &fields[index + 1];
                let z = &fields[index + 2];
                if y.vec3.as_ref().is_some_and(|(group, component)| group == name && *component == 1)
                    && z.vec3.as_ref().is_some_and(|(group, component)| group == name && *component == 2)
                    && field.folder == y.folder
                    && field.folder == z.folder
                    && is_float(&field.kind)
                    && is_float(&y.kind)
                    && is_float(&z.kind)
                {
                    result.push(vec![field.clone(), y.clone(), z.clone()]);
                    seen.push(name.clone());
                    index += 3;
                } else {
                    return Err(format!("Invalid vec3 group {name}"));
                }
            }
            Some((name, _)) => {
                return Err(format!("Invalid or overlapping vec3 group {name}"));
            }
        }
    }
    Ok(result)
}

fn is_float(kind: &KindView) -> bool {
    matches!(kind, KindView::Floating(_))
}

pub mod string_table {
    pub fn encode(rows: &[Vec<Vec<u8>>]) -> Vec<u8> {
        let mut out = Vec::new();
        for (row_index, row) in rows.iter().enumerate() {
            if row_index > 0 {
                out.push(b'\n');
            }
            for (cell_index, cell) in row.iter().enumerate() {
                if cell_index > 0 {
                    out.push(b'\t');
                }
                encode_cell(&mut out, cell);
            }
        }
        out
    }

    fn encode_cell(out: &mut Vec<u8>, value: &[u8]) {
        for &byte in value {
            match byte {
                b'\\' => out.extend_from_slice(b"\\\\"),
                b'\t' => out.extend_from_slice(b"\\t"),
                b'\n' => out.extend_from_slice(b"\\n"),
                byte => out.push(byte),
            }
        }
    }

    pub fn decode(text: &[u8]) -> Result<Vec<Vec<Vec<u8>>>, String> {
        if text.is_empty() {
            return Ok(Vec::new());
        }
        let mut rows = Vec::new();
        let mut row = Vec::new();
        let mut cell = Vec::new();
        let mut escaped = false;
        for &byte in text {
            if escaped {
                escaped = false;
                match byte {
                    b'\\' => cell.push(b'\\'),
                    b't' => cell.push(b'\t'),
                    b'n' => cell.push(b'\n'),
                    byte => return Err(if byte.is_ascii() {
                        format!("unsupported table escape \\{}", byte as char)
                    } else {
                        format!("unsupported table escape \\x{byte:02x}")
                    }),
                }
            } else {
                match byte {
                    b'\\' => escaped = true,
                    b'\t' => row.push(std::mem::take(&mut cell)),
                    b'\n' => {
                        row.push(std::mem::take(&mut cell));
                        rows.push(std::mem::take(&mut row));
                    }
                    byte => cell.push(byte),
                }
            }
        }
        if escaped {
            return Err("trailing table escape".to_owned());
        }
        row.push(cell);
        rows.push(row);
        Ok(rows)
    }
}

pub mod number {
    use super::{ocaml_float_max, ocaml_float_min};

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Kind {
        Int,
        Float,
    }

    pub fn valid(kind: Kind, text: &str) -> bool {
        match kind {
            Kind::Int => parse_int(text).is_some(),
            Kind::Float => parse_float(text).is_some_and(f64::is_finite),
        }
    }

    pub fn fraction(range: (f64, f64), text: &str) -> Option<f64> {
        let (low, high) = range;
        parse_float(text)
            .filter(|_| high > low)
            .map(|value| ocaml_float_max(0.0, ocaml_float_min(1.0, (value - low) / (high - low))))
    }

    pub fn show(value: f64) -> String {
        super::float_text_with_decimal(value, true)
    }

    // the digits a text writes after its point
    fn decimals(text: &str) -> usize {
        match text.find('.') {
            Some(index) if !text.contains('e') && !text.contains('E') => (text.len() - index - 1).min(6),
            _ => 0,
        }
    }

    /// Pointer drag in points → rounded change in integer units (one unit per six points).
    pub fn int_scrub_delta(dx: f64) -> f64 {
        (dx / 6.0).round()
    }

    pub fn scrub(kind: Kind, range: Option<(f64, f64)>, origin: &str, dx: f64, fine: bool) -> String {
        let Some(value) = parse_float(origin) else {
            return origin.to_owned();
        };
        // a drag stays inside the soft range; a value typed outside it is not pulled in
        let held = |v: f64, moved: f64| match range {
            Some((low, high)) if high > low => {
                ocaml_float_max(ocaml_float_min(low, v), ocaml_float_min(ocaml_float_max(high, v), moved))
            }
            _ => moved,
        };
        match kind {
            Kind::Int => {
                let n = int_scrub_delta(dx);
                if n == 0.0 {
                    origin.to_owned()
                } else {
                    (held(value, value.round() + n) as i64).to_string()
                }
            }
            Kind::Float => {
                let step = match range {
                    Some((low, high)) if high > low => (high - low) / 200.0,
                    _ => 0.05,
                } / if fine { 10.0 } else { 1.0 };
                // the result is rounded to the step's digits, or the origin's when it wrote more
                let digits = decimals(origin).max(((-step.log10()).ceil() as i64).clamp(0, 6) as usize);
                let unit = 10.0f64.powf(digits as f64);
                let delta = (dx * step * unit).round() / unit;
                if delta == 0.0 {
                    origin.to_owned()
                } else {
                    super::float_text_with_decimal(held(value, ((value + delta) * unit).round() / unit), true)
                }
            }
        }
    }

    // OCaml int_of_string: an optional sign, then 0x/0o/0b/0u (unsigned) or decimal digits, with
    // '_' allowed after the first digit. Decimal signed magnitudes stop at 2^62 (negative) and
    // 2^62 - 1 (positive); other forms at 2^63 - 1, and the value wraps to 63 bits.
    fn parse_int(text: &str) -> Option<i64> {
        let bytes = text.as_bytes();
        let (negative, mut index) = match bytes.first() {
            Some(b'-') => (true, 1),
            Some(b'+') => (false, 1),
            _ => (false, 0),
        };
        let (base, signed) = match (bytes.get(index), bytes.get(index + 1)) {
            (Some(b'0'), Some(b'x' | b'X')) => (16, false),
            (Some(b'0'), Some(b'o' | b'O')) => (8, false),
            (Some(b'0'), Some(b'b' | b'B')) => (2, false),
            (Some(b'0'), Some(b'u' | b'U')) => (10, false),
            _ => (10, true),
        };
        if matches!(bytes.get(index), Some(b'0')) && !signed {
            index += 2;
        }
        let digits = &bytes[index..];
        if !digits.first().is_some_and(u8::is_ascii_alphanumeric) {
            return None;
        }
        let mut magnitude: u64 = 0;
        for &byte in digits {
            if byte == b'_' {
                continue;
            }
            let digit = match byte {
                b'0'..=b'9' => u64::from(byte - b'0'),
                b'a'..=b'z' => u64::from(byte - b'a') + 10,
                b'A'..=b'Z' => u64::from(byte - b'A') + 10,
                _ => return None,
            };
            if digit >= base {
                return None;
            }
            magnitude = magnitude.checked_mul(base)?.checked_add(digit)?;
        }
        let limit = match (signed, negative) {
            (true, true) => 1 << 63,
            (true, false) => (1 << 63) - 1,
            (false, _) => (1 << 63) - 1,
        };
        if magnitude > limit {
            return None;
        }
        Some(if negative { magnitude.wrapping_neg() } else { magnitude } as i64)
    }

    fn parse_float(text: &str) -> Option<f64> {
        cfmt::parse_ocaml_float(text)
    }
}

fn validate_int_range(range: IntRange) -> Result<(), String> {
    if range.soft_max <= range.soft_min {
        return Err("Param.integer: soft max must be greater than soft min".to_owned());
    }
    if matches!((range.hard_min, range.hard_max), (Some(low), Some(high)) if high < low) {
        return Err("Param.integer: hard max must be at least hard min".to_owned());
    }
    if range.hard_min.is_some_and(|low| range.soft_min < low) {
        return Err("Param.integer: soft min must not be below hard min".to_owned());
    }
    if range.hard_max.is_some_and(|high| range.soft_max > high) {
        return Err("Param.integer: soft max must not exceed hard max".to_owned());
    }
    Ok(())
}

fn validate_float_range(range: FloatRange) -> Result<(), String> {
    if !(range.soft_min.is_finite() && range.soft_max.is_finite()) || range.soft_max <= range.soft_min {
        return Err("Param.floating: soft bounds must be finite and increasing".to_owned());
    }
    if range.hard_min.is_some_and(|value| !value.is_finite())
        || range.hard_max.is_some_and(|value| !value.is_finite())
    {
        return Err("Param.floating: hard bounds must be finite".to_owned());
    }
    if matches!((range.hard_min, range.hard_max), (Some(low), Some(high)) if high < low) {
        return Err("Param.floating: hard max must be at least hard min".to_owned());
    }
    if range.hard_min.is_some_and(|low| range.soft_min < low) {
        return Err("Param.floating: soft min must not be below hard min".to_owned());
    }
    if range.hard_max.is_some_and(|high| range.soft_max > high) {
        return Err("Param.floating: soft max must not exceed hard max".to_owned());
    }
    Ok(())
}

fn validate_choice_labels(labels: &[String]) -> Result<(), String> {
    if labels.is_empty() {
        return Err("Param.choice: options must not be empty".to_owned());
    }
    let mut seen = std::collections::HashSet::<&str>::with_capacity(labels.len());
    for label in labels {
        nonblank("Param.choice label", label)?;
        if !seen.insert(label) {
            return Err(format!("Param.choice: duplicate label {}", quote(label)));
        }
    }
    Ok(())
}

fn nonblank(what: &str, value: &str) -> Result<(), String> {
    if value.trim_matches(|character| character == ' ' || matches!(character as u32, 9 | 10 | 12 | 13)).is_empty() {
        Err(format!("{what} must not be blank"))
    } else {
        Ok(())
    }
}

fn default_label(name: &str) -> String {
    let mut label = name.replace('_', " ").replace('-', " ");
    if let Some(first) = label.get_mut(0..1) {
        first.make_ascii_uppercase();
    }
    label
}

fn append_token(out: &mut Vec<u8>, token: &[u8]) {
    write!(out, "{}:", token.len()).unwrap();
    out.extend_from_slice(token);
}

fn quote(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('"');
    for byte in value.bytes() {
        match byte {
            b'"' => out.push_str("\\\""),
            b'\\' => out.push_str("\\\\"),
            b'\n' => out.push_str("\\n"),
            b'\r' => out.push_str("\\r"),
            b'\t' => out.push_str("\\t"),
            b'\x08' => out.push_str("\\b"),
            32..=126 => out.push(byte as char),
            byte => {
                let _ = write!(out, "\\{byte:03}");
            }
        }
    }
    out.push('"');
    out
}


#[cfg(test)]
mod tests {
    use super::*;


    fn field_ok(name: &str, kind: Kind, default: Value) -> Field {
        field(name.to_owned(), kind, default).unwrap()
    }

    fn schema_ok(name: &str, fields: Vec<Field>) -> Schema {
        schema(name.to_owned(), fields).unwrap()
    }
    fn text_value(value: &str) -> Value {
        Value::Text(value.as_bytes().to_vec())
    }

    fn integer_ok(min: i64, max: i64, hard_min: Option<i64>, hard_max: Option<i64>) -> Kind {
        integer(min, max, hard_min, hard_max).unwrap()
    }

    fn floating_ok(min: f64, max: f64, hard_min: Option<f64>, hard_max: Option<f64>) -> Kind {
        floating(min, max, hard_min, hard_max).unwrap()
    }

    fn index_choice_ok(labels: Vec<String>) -> Kind {
        index_choice(labels).unwrap()
    }

    fn float_field(name: &str, value: f64) -> Field {
        field_ok(
            name,
            floating_ok(-10.0, 10.0, None, None),
            Value::Float(value),
        )
    }

    #[test]
    fn translated_param_assertions() {
        for value in [
            0.0,
            -0.0,
            f64::from_bits(1.0f64.to_bits() + 1),
            0.1 + 0.2,
            std::f64::consts::PI,
            f64::MAX,
            f64::MIN_POSITIVE,
            f64::from_bits(1),
            -1.2345678901234567,
        ] {
            assert_eq!(float_text(value).parse::<f64>().unwrap().to_bits(), value.to_bits());
        }
        assert_eq!(float_text(1.234567890123456), "1.234567890123456");
        assert_eq!(float_text(std::f64::consts::PI), "3.141592653589793");
        assert_eq!(float_text(10.0), "10");
        assert_eq!(float_text(100.0), "100");
        assert_eq!(float_text(1000.0), "1e3");
        assert_eq!(float_text(0.0001), "1e-4");
        assert_eq!(float_text_with_decimal(-0.0, true), "-0.0");
        for i in 0..=2047u64 {
            let bits = i.wrapping_mul(6364136223846793005).wrapping_add(1);
            let value = f64::from_bits(bits);
            if value.is_finite() {
                let text = float_text(value);
                assert_eq!(text.parse::<f64>().unwrap().to_bits(), bits);
                assert_eq!(float_text_with_decimal(value, true).parse::<f64>().unwrap().to_bits(), bits);
            }
        }

        let plain = schema_ok("position", vec![float_field("x", 1.0)]);
        let grouped = schema_ok(
            "position",
            vec![float_field("x", 1.0).primary(true).vec3("center".into(), 0).unwrap()],
        );
        let plain_record = vec![Value::Float(2.0)];
        let grouped_record = plain_record.clone();
        let p = plain.view(&plain_record).unwrap()[0].clone();
        let g = grouped.view(&grouped_record).unwrap()[0].clone();
        assert!(plain.view(&[]).is_err());
        assert!(plain.view(&[Value::Bool(false)]).is_err());
        assert!(plain.key(&[]).is_err());
        assert!(plain.cook_text(&[]).is_err());
        assert!(plain.apply_all(&[], &[]).is_err());
        assert!(!p.primary && p.vec3.is_none());
        assert!(g.primary && g.vec3 == Some(("center".into(), 0)) && g.current == Value::Float(2.0));
        let component = |index| {
            let mut field = g.clone();
            field.name = format!("center_{index}");
            field.vec3 = Some(("center".into(), index));
            field
        };
        let x = component(0);
        let y = component(1);
        let z = component(2);
        assert_eq!(
            group_vec3(&[p.clone(), x.clone(), y.clone(), z.clone()], false),
            Ok(vec![vec![p.clone()], vec![x.clone(), y.clone(), z.clone()]])
        );
        for fields in [
            vec![x.clone(), y.clone()],
            vec![y.clone(), z.clone(), x.clone()],
            vec![x.clone(), y.clone(), FieldView { folder: vec!["other".into()], ..z.clone() }],
            vec![x.clone(), y.clone(), FieldView { kind: KindView::Text, ..z.clone() }],
            vec![x.clone(), y.clone(), z.clone(), x.clone(), y.clone(), z.clone()],
            vec![FieldView { name: "center".into(), ..p.clone() }, x.clone(), y.clone(), z.clone()],
        ] {
            assert!(group_vec3(&fields, false).is_err());
        }
        let ungrouped: Vec<_> = ["x", "y", "z"]
            .into_iter()
            .enumerate()
            .map(|(index, axis)| FieldView {
                name: format!("p_{axis}"),
                current: Value::Float(index as f64),
                ..p.clone()
            })
            .collect();
        assert_eq!(
            group_vec3(&ungrouped, false),
            Ok(ungrouped.iter().cloned().map(|field| vec![field]).collect())
        );
        let inferred = group_vec3(&ungrouped, true).unwrap().remove(0);
        assert_eq!(
            inferred.iter().map(|field| field.vec3.clone()).collect::<Vec<_>>(),
            vec![Some(("p".into(), 0)), Some(("p".into(), 1)), Some(("p".into(), 2))]
        );
        let mut clashing = ungrouped.clone();
        clashing.insert(0, FieldView { name: "p".into(), ..p.clone() });
        let clash = &group_vec3(&clashing, true).unwrap()[1];
        assert_eq!(
            clash.iter().map(|field| field.vec3.clone()).collect::<Vec<_>>(),
            vec![Some(("p_color".into(), 0)), Some(("p_color".into(), 1)), Some(("p_color".into(), 2))]
        );
        assert_eq!(plain.key(&plain_record).unwrap(), grouped.key(&grouped_record).unwrap());
        assert_eq!(plain.cook_key(&plain_record).unwrap(), grouped.cook_key(&grouped_record).unwrap());
        let cook_texts: Vec<Vec<u8>> = [1.5, 0.1, 0.1 + 0.2, -0.0]
            .into_iter()
            .map(|value| plain.cook_text(&[Value::Float(value)]).unwrap())
            .collect();
        assert_eq!(cook_texts, [b"x=1.5".to_vec(), b"x=0.1".to_vec(), b"x=0.30000000000000004".to_vec(), b"x=-0".to_vec()]);
        let (grouped_value, effects) = grouped
            .apply_all(&[Value::Float(1.0)], &[("x".into(), Value::Float(3.0))])
            .unwrap();
        assert_eq!(grouped_value, vec![Value::Float(3.0)]);
        assert!(effects.cook);
        assert!(float_field("x", 1.0).vec3("".into(), 0).is_err());
        assert!(float_field("x", 1.0).vec3("center".into(), 3).is_err());
        assert!(field_ok("x", toggle(), Value::Bool(false)).vec3("center".into(), 0).is_err());

        let choice_schema = |labels: &[&str]| {
            schema_ok(
                "switch",
                vec![field_ok(
                    "input",
                    index_choice_ok(labels.iter().map(|label| (*label).to_owned()).collect()),
                    Value::Int(0),
                )],
            )
        };
        let initial = choice_schema(&["cube", "grid"]);
        let renamed = choice_schema(&["box", "replacement"]);
        let initial_record = vec![Value::Int(1)];
        let renamed_record = initial_record.clone();
        assert_eq!(initial.cook_key(&initial_record).unwrap(), renamed.cook_key(&renamed_record).unwrap());
        assert_eq!(initial.view(&initial_record).unwrap()[0].current, Value::Int(1));
        let (selected, effects) = initial
            .apply_all(&[Value::Int(0)], &[("input".into(), Value::Int(1))])
            .unwrap();
        assert_eq!(selected, vec![Value::Int(1)]);
        assert!(effects.cook);
        assert!(initial.apply_all(&[Value::Int(0)], &[("input".into(), Value::Int(-1))]).is_err());
        assert!(initial.apply_all(&[Value::Int(0)], &[("input".into(), Value::Int(2))]).is_err());
        assert!(index_choice(Vec::new()).is_err());
        assert!(index_choice(vec!["duplicate".into(), "duplicate".into()]).is_err());
    }

    #[test]
    fn translated_normalization_assertions() {
        let make_fields = |count: i64, amount: f64| {
            vec![
                field_ok("count", integer_ok(0, 10, Some(0), Some(10)), Value::Int(count)),
                field_ok("amount", floating_ok(0.0, 10.0, Some(0.0), Some(10.0)), Value::Float(amount)),
                field_ok("title", text(), text_value("ok")),
            ]
        };
        let original = vec![Value::Int(3), Value::Float(2.0), text_value("ok")];
        let template = schema_ok("normalization", make_fields(3, 2.0));
        assert_eq!(template.default(), original.as_slice());
        let mut unchanged = original.clone();
        assert!(!template.normalize(&mut unchanged).unwrap());
        assert_eq!(unchanged, original);
        let outside = vec![Value::Int(20), Value::Float(-2.0), text_value("ok")];
        let expected = vec![Value::Int(10), Value::Float(0.0), text_value("ok")];
        let mut normalized = outside.clone();
        assert!(template.normalize(&mut normalized).unwrap());
        assert_eq!(normalized, expected);
        let clamped = schema_ok("clamped", make_fields(20, -2.0));
        assert_eq!(clamped.default(), expected.as_slice());
        assert_eq!(outside[0], Value::Int(20));
        assert_eq!(outside[1], Value::Float(-2.0));
        let mut signed_zero = vec![Value::Int(3), Value::Float(-0.0), text_value("ok")];
        let amount_kind = floating_ok(0.0, 10.0, Some(0.0), Some(10.0));
        assert!(template.normalize(&mut signed_zero).unwrap());
        let expected_zero = normalize_value(&amount_kind, &Value::Float(-0.0)).unwrap();
        let Value::Float(expected_zero) = expected_zero else { unreachable!() };
        let Value::Float(normalized_zero) = &signed_zero[1] else { unreachable!() };
        assert_eq!(normalized_zero.to_bits(), expected_zero.to_bits());
        let mut nan = vec![Value::Int(3), Value::Float(f64::NAN), text_value("ok")];
        let before_nan = nan.clone();
        assert!(template.normalize(&mut nan).is_err());
        assert_eq!(nan[0], before_nan[0]);
        assert!(matches!(
            (&nan[1], &before_nan[1]),
            (Value::Float(left), Value::Float(right)) if left.to_bits() == right.to_bits()
        ));
        assert_eq!(nan[2], before_nan[2]);
        let mut infinity = vec![Value::Int(3), Value::Float(f64::INFINITY), text_value("ok")];
        let before_infinity = infinity.clone();
        assert!(template.normalize(&mut infinity).is_err());
        assert_eq!(infinity, before_infinity);
    }

    #[test]
    fn translated_string_table_assertions() {
        use string_table::{decode, encode};
        assert_eq!(encode(&[]), Vec::<u8>::new());
        assert_eq!(decode(b""), Ok(vec![]));
        let rows = vec![
            vec![b"".to_vec(), "雪".as_bytes().to_vec(), b"tab\tline\nslash\\".to_vec()],
            vec![b"last".to_vec(), b"".to_vec()],
            vec![vec![0xff, 0, 0x80]],
        ];
        let mut expected = "\t雪\ttab\\tline\\nslash\\\\\nlast\t\n".as_bytes().to_vec();
        expected.extend_from_slice(&[0xff, 0, 0x80]);
        assert_eq!(encode(&rows), expected);
        assert_eq!(decode(&encode(&rows)), Ok(rows));
        assert_eq!(decode(b"a\t\n"), Ok(vec![vec![b"a".to_vec(), b"".to_vec()], vec![b"".to_vec()]]));
        assert_eq!(decode(b"\\q"), Err("unsupported table escape \\q".into()));
        assert_eq!(decode(b"\\\xff"), Err("unsupported table escape \\xff".into()));
        assert_eq!(decode(b"trailing\\"), Err("trailing table escape".into()));
    }

    #[test]
    fn text_values_preserve_arbitrary_bytes() {
        let original = vec![0xff, b'a', b'\t', b'\n', 0];
        let replacement = vec![0x80, b'b', 0];
        let schema = schema_ok(
            "text",
            vec![field_ok(
                "value",
                text(),
                Value::Text(original.clone()),
            )],
        );
        assert_eq!(schema.default(), &[Value::Text(original.clone())]);
        assert_eq!(
            schema.view(schema.default()).unwrap()[0].current,
            Value::Text(original.clone())
        );
        let mut normalized = schema.default().to_vec();
        assert!(!schema.normalize(&mut normalized).unwrap());
        assert_eq!(normalized, vec![Value::Text(original.clone())]);
        let expected_key = b"4:text5:values5:\xffa\t\n\0".to_vec();
        assert_eq!(schema.key(schema.default()).unwrap(), expected_key);
        assert_eq!(schema.cook_key(schema.default()).unwrap(), expected_key);
        assert_eq!(schema.cook_text(schema.default()).unwrap(), b"value=\xffa\t\n\0".to_vec());

        let (applied, effects) = schema
            .apply_all(
                schema.default(),
                &[("value".into(), Value::Text(replacement.clone()))],
            )
            .unwrap();
        assert_eq!(applied, vec![Value::Text(replacement)]);
        assert!(effects.cook);
        let encoded = schema_ok(
            "encoded",
            vec![field_ok(
                "payload",
                encoded(),
                Value::Text(original.clone()),
            )],
        );
        assert_eq!(encoded.default(), &[Value::Text(original.clone())]);
        assert_eq!(
            encoded.view(encoded.default()).unwrap()[0].current,
            Value::Text(original.clone())
        );
        let mut encoded_normalized = encoded.default().to_vec();
        assert!(!encoded.normalize(&mut encoded_normalized).unwrap());
        assert_eq!(encoded_normalized, vec![Value::Text(original)]);
    }

    #[test]
    fn typed_choice_and_encoding_adapters() {
        let labels = vec!["Warm".to_owned(), "Cool".to_owned()];
        let tone_schema = schema_ok(
            "tones",
            vec![field_ok(
                "tone",
                choice_with(
                    labels,
                    Box::new(|left, right| {
                        let tone = |label: &str| match label {
                            "Warm" => Some(1),
                            "Cool" => Some(2),
                            _ => None,
                        };
                        tone(left) == tone(right)
                    }),
                )
                .unwrap(),
                Value::Choice("Warm".into()),
            )],
        );
        let (tone, effects) = tone_schema
            .apply_all(
                &[Value::Choice("Warm".into())],
                &[("tone".into(), Value::Choice("Cool".into()))],
            )
            .unwrap();
        assert_eq!(tone[0], Value::Choice("Cool".into()));
        assert!(effects.cook);

        let encoded_schema = schema_ok(
            "lists",
            vec![field_ok(
                "items",
                encoded_with(
                    Box::new(|left, right| decode_ints(left) == decode_ints(right)),
                    Box::new(encode_ints),
                ),
                text_value("1,2"),
            )],
        );
        assert_eq!(encoded_schema.view(encoded_schema.default()).unwrap()[0].current, text_value("1,2"));
        let (list, effects) = encoded_schema
            .apply_all(
                &[text_value("1,2")],
                &[("items".into(), text_value("3,4"))],
            )
            .unwrap();
        assert_eq!(list[0], text_value("3,4"));
        assert!(effects.cook);
    }

    #[test]
    fn encoded_values_are_validated_and_canonicalized() {
        let encoded = || {
            encoded_with(
                Box::new(|left, right| decode_ints(left) == decode_ints(right)),
                Box::new(encode_ints),
            )
        };
        let schema = schema_ok(
            "lists",
            vec![field_ok(
                "items",
                encoded(),
                text_value("01,2"),
            )],
        );
        assert_eq!(schema.default(), &[text_value("1,2")][..]);
        let (value, effects) = schema
            .apply_all(
                schema.default(),
                &[("items".into(), text_value("03,4"))],
            )
            .unwrap();
        assert_eq!(value, vec![text_value("3,4")]);
        assert!(effects.cook);
        let (equivalent, effects) = schema
            .apply_all(
                &value,
                &[("items".into(), text_value("03,4"))],
            )
            .unwrap();
        assert_eq!(equivalent, value);
        assert!(!has_effects(effects));
        let raw = vec![text_value("03,4")];
        assert_eq!(
            schema.view(&raw).unwrap()[0].current,
            text_value("3,4")
        );
        assert_eq!(schema.key(&raw).unwrap(), schema.key(&value).unwrap());
        assert_eq!(
            schema.cook_key(&raw).unwrap(),
            schema.cook_key(&value).unwrap()
        );
        assert_eq!(schema.cook_text(&raw).unwrap(), b"items=3,4");
        let (canonical, effects) = schema
            .apply_all(&raw, &[("items".into(), text_value("3,4"))])
            .unwrap();
        assert_eq!(canonical, value);
        assert!(!has_effects(effects));
        let mut normalized = raw.clone();
        assert!(schema.normalize(&mut normalized).unwrap());
        assert_eq!(normalized, value);

        let transactional = schema_ok(
            "transactional",
            vec![
                field_ok(
                    "count",
                    integer(0, 10, Some(0), Some(10)).unwrap(),
                    Value::Int(1),
                ),
                field_ok("items", encoded(), text_value("1")),
            ],
        );
        let mut invalid = vec![Value::Int(100), text_value("bad")];
        let before = invalid.clone();
        assert!(transactional.normalize(&mut invalid).is_err());
        assert_eq!(invalid, before);
        assert!(field(
            "items".into(),
            encoded_with(
                Box::new(|left, right| left == right),
                Box::new(encode_ints),
            ),
            text_value("invalid"),
        )
        .is_err());
        assert!(field("items".into(), encoded(), text_value("invalid")).is_err());
        assert!(schema
            .apply_all(
                &value,
                &[("items".into(), text_value("invalid"))],
            )
            .is_err());
    }

    #[test]
    fn number_field_behavior() {
        assert!(number::valid(number::Kind::Int, "9223372036854775807"));
        assert!(number::valid(number::Kind::Int, "-9223372036854775808"));
        assert!(!number::valid(number::Kind::Int, "9223372036854775808"));
        assert!(!number::valid(number::Kind::Int, "-9223372036854775809"));
        assert!(number::valid(number::Kind::Int, "0x7fffffffffffffff"));
        assert!(!number::valid(number::Kind::Int, "0x8000000000000000"));
        assert!(number::valid(number::Kind::Int, "0xff"));
        assert!(number::valid(number::Kind::Float, "0x1.8p1"));
        assert!(number::valid(number::Kind::Float, "0x1p-1074"));
        assert_eq!(number::fraction((0.0, 10.0), "5"), Some(0.5));
        assert_eq!(number::show(10.0), "10.0");
        assert_eq!(
            number::scrub(number::Kind::Float, None, "1", 10.0, false),
            "1.5"
        );
    }

    // expected values: lib/param/param.ml run under `ocaml` (Number.scrub, show, fraction, valid)
    #[test]
    fn number_scrub_and_parse_match_ocaml() {
        use number::Kind::{Float, Int};
        let s = number::scrub;
        assert_eq!(s(Float, None, "1", 10.0, false), "1.5");
        assert_eq!(s(Float, None, "1", 0.04, false), "1");
        assert_eq!(s(Float, None, "1234567.89", 2.0, false), "1234567.99");
        assert_eq!(s(Float, None, "1.5", 0.5, false), "1.53");
        assert_eq!(s(Float, None, "2", 0.3, true), "2.002");
        assert_eq!(s(Float, None, "1", 1.0, true), "1.005");
        assert_eq!(s(Float, None, "1e3", 10.0, false), "1000.5");
        assert_eq!(s(Float, None, "nan", 10.0, false), "NaN");
        assert_eq!(s(Float, None, "abc", 10.0, false), "abc");
        assert_eq!(s(Float, Some((5.0, 5.0)), "1", 10.0, false), "1.5");
        // the range holds the rounded value, not the value scaled by 10^digits
        assert_eq!(s(Float, Some((0.0, 2.0)), "1", 500.0, false), "2.0");
        assert_eq!(s(Float, Some((0.0, 2.0)), "1", 1000.0, true), "2.0");
        assert_eq!(s(Float, Some((0.0, 2.0)), "1", 1_000_000.0, false), "2.0");
        assert_eq!(s(Float, Some((0.0, 2.0)), "1", -500.0, false), "0.0");
        assert_eq!(s(Float, Some((0.0, 2.0)), "1.9", 3.0, false), "1.93");
        // a value typed outside the range is not pulled in by a drag
        assert_eq!(s(Float, Some((0.0, 2.0)), "3", -3.0, false), "2.97");
        assert_eq!(s(Float, Some((0.0, 2.0)), "3", 0.5, false), "3.0");
        assert_eq!(s(Float, Some((0.0, 2.0)), "0.5", 0.7, false), "0.51");
        assert_eq!(s(Float, Some((0.0, 2.0)), "0.1234567", 7.0, false), "0.193457");
        assert_eq!(s(Int, None, "3", 12.0, false), "5");
        assert_eq!(s(Int, None, "2.5", 60.0, false), "13");
        assert_eq!(s(Int, None, "-2.5", 60.0, false), "7");
        assert_eq!(s(Int, None, "3", -18.0, false), "0");
        assert_eq!(s(Int, Some((0.0, 5.0)), "9", 6.0, false), "9");
        assert_eq!(s(Int, Some((0.0, 5.0)), "2", 1000.0, false), "5");
        assert_eq!(number::show(1e-7), "0.0000001");
        assert_eq!(number::show(1.0), "1.0");
        assert_eq!(number::show(123_456_789.123), "123456789.123");
        assert_eq!(number::show(-0.0), "-0.0");
        assert_eq!(number::show(f64::NAN), "NaN");
        // fraction propagates NaN (OCaml Float.min) and clamps finite values
        assert!(number::fraction((0.0, 10.0), "nan").is_some_and(f64::is_nan));
        assert_eq!(number::fraction((0.0, 10.0), "-5"), Some(0.0));
        assert_eq!(number::fraction((0.0, 10.0), "50"), Some(1.0));
        assert_eq!(number::fraction((0.0, 10.0), "x"), None);
        assert_eq!(number::fraction((10.0, 0.0), "5"), None);
        // int_of_string and float_of_string acceptance
        let int = |text| number::valid(Int, text);
        assert!(int("1_000") && int("1__0") && int("1_") && int("0u5") && int("+5") && int("-0x10"));
        assert!(int("0o777777777777777777777") && int("-0x4000000000000001") && int("00012"));
        assert!(!int("0x_1") && !int(" 5") && !int("1 ") && !int("_1") && !int("0x") && !int(""));
        assert!(!int("-") && !int("0u") && !int("0b1000000000000000000000000000000000000000000000000000000000000000"));
        assert!(!int("-0x8000000000000000") && !int("0u18446744073709551615") && !int("1e3"));
        let float = |text| number::valid(Float, text);
        assert!(float("1_000.5") && float(" 1") && float("1.") && float(".5") && float("+.5"));
        assert!(float("0x_1p0") && float("0x1p-1074") && float("1.5e+3"));
        assert!(!float(" 5 ") && !float("1e") && !float("1e400") && !float("infinity") && !float("1d"));
    }

    #[test]
    fn invalid_configuration_returns_ocaml_errors() {
        assert_eq!(
            integer(0, 0, None, None).err().as_deref(),
            Some("Param.integer: soft max must be greater than soft min")
        );
        assert_eq!(
            floating(0.0, 0.0, None, None).err().as_deref(),
            Some("Param.floating: soft bounds must be finite and increasing")
        );
        assert_eq!(
            choice(Vec::new()).err().as_deref(),
            Some("Param.choice: options must not be empty")
        );
        assert_eq!(
            choice(vec!["x".into(), "x".into()]).err().as_deref(),
            Some("Param.choice: duplicate label \"x\"")
        );
        assert_eq!(
            field(" ".into(), text(), text_value("x")).err().as_deref(),
            Some("Param.field name must not be blank")
        );
        assert_eq!(
            field("x".into(), toggle(), text_value("x")).err().as_deref(),
            Some("Param.field x: parameter value has the wrong type")
        );
        assert_eq!(
            schema(" ".into(), Vec::new()).err().as_deref(),
            Some("Param.schema name must not be blank")
        );
        assert_eq!(
            schema(
                "schema".into(),
                vec![
                    field_ok("x", text(), text_value("")),
                    field_ok("x", text(), text_value("")),
                ],
            )
            .err()
            .as_deref(),
            Some("Param.schema: duplicate field \"x\"")
        );
        assert_eq!(
            field_ok("x", text(), text_value(""))
                .label(" ".into())
                .err()
                .as_deref(),
            Some("Param.field label must not be blank")
        );
        assert_eq!(
            field_ok("x", text(), text_value(""))
                .folder(vec![" ".into()])
                .err()
                .as_deref(),
            Some("Param.field folder must not be blank")
        );
        assert_eq!(
            float_field("x", 1.0).vec3("".into(), 0).err().as_deref(),
            Some("Param.field vec3 group must not be blank")
        );
        assert_eq!(
            float_field("x", 1.0).vec3("center".into(), -1).err().as_deref(),
            Some("Param.field: vec3 component must be 0, 1 or 2")
        );
        assert_eq!(
            field_ok("x", toggle(), Value::Bool(false))
                .vec3("center".into(), 0)
                .err()
                .as_deref(),
            Some("Param.field: vec3 components must be float fields")
        );
    }

    fn decode_ints(text: &[u8]) -> Result<Vec<i64>, String> {
        std::str::from_utf8(text)
            .map_err(|error| error.to_string())?
            .split(',')
            .map(str::parse)
            .collect::<Result<Vec<i64>, _>>()
            .map_err(|error| error.to_string())
    }
    fn encode_ints(text: &[u8]) -> Result<Vec<u8>, String> {
        decode_ints(text).map(|values| {
            values
                .iter()
                .map(i64::to_string)
                .collect::<Vec<_>>()
                .join(",")
                .into_bytes()
        })
    }

}

#[cfg(test)]
mod test_param;
