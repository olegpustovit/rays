use std::{collections::BTreeSet, env, fs, path::PathBuf};

#[derive(Clone, Debug)]
enum Token { Word(String), Text(String), Number(String), Symbol(char) }
#[derive(Clone, Debug)]
enum Ty { Scalar(String), Enum(String), Text, Object(String), OptionalObject(String), Record(String), Tuple(String) }
#[derive(Clone, Debug)]
enum Body { Send, Read(String), Write(String) }
#[derive(Clone, Debug)]
struct Call {
    name: String, receiver: String, objc: String, selector: String, args: Vec<Ty>, result: Option<Ty>,
    error: bool, pool: bool, since: Option<(u32, u32)>, feature: String, body: Body,
}
#[derive(Clone, Debug)]
struct EnumCase { name: String, symbol: String, value: i64, since: (u32, u32, u32) }
#[derive(Clone, Debug)]
struct EnumDef { name: String, feature: String, cases: Vec<EnumCase> }
#[derive(Clone, Debug)]
struct RecordDef { name: String, sdk: String, feature: String, fields: Vec<(String, String)> }
#[derive(Clone, Debug)]
struct NativeDef { name: String, signature: String, primitives: Vec<String>, reason: String }

fn lex(source: &str) -> Vec<Token> {
    let bytes = source.as_bytes();
    let mut tokens = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i].is_ascii_whitespace() { i += 1; continue; }
        if bytes.get(i..i + 2) == Some(b"(*") {
            i += 2;
            let mut depth = 1;
            while i < bytes.len() && depth > 0 {
                if bytes.get(i..i + 2) == Some(b"(*") { depth += 1; i += 2; }
                else if bytes.get(i..i + 2) == Some(b"*)") { depth -= 1; i += 2; }
                else { i += 1; }
            }
            continue;
        }
        match bytes[i] {
            b'"' => {
                i += 1;
                let mut text = String::new();
                while i < bytes.len() && bytes[i] != b'"' {
                    if bytes[i] == b'\\' && i + 1 < bytes.len() {
                        i += 1;
                        text.push(match bytes[i] { b'n' => '\n', b'r' => '\r', b't' => '\t', other => other as char });
                        i += 1;
                    } else {
                        let ch = source[i..].chars().next().unwrap();
                        text.push(ch);
                        i += ch.len_utf8();
                    }
                }
                if i < bytes.len() { i += 1; }
                tokens.push(Token::Text(text));
            }
            b'0'..=b'9' => {
                let start = i;
                i += 1;
                while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') { i += 1; }
                tokens.push(Token::Number(source[start..i].to_owned()));
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let start = i;
                i += 1;
                while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') { i += 1; }
                tokens.push(Token::Word(source[start..i].to_owned()));
            }
            other => { tokens.push(Token::Symbol(other as char)); i += 1; }
        }
    }
    tokens
}

fn is_sym(token: &Token, ch: char) -> bool { matches!(token, Token::Symbol(found) if *found == ch) }
fn word(token: &Token) -> Option<&str> { if let Token::Word(value) = token { Some(value) } else { None } }
fn text(token: &Token) -> Option<&str> { if let Token::Text(value) = token { Some(value) } else { None } }
fn name(tokens: &[Token]) -> String { tokens.iter().rev().find_map(word).unwrap_or("").to_owned() }
fn string(tokens: &[Token]) -> String { tokens.iter().find_map(text).unwrap_or("").to_owned() }
fn enclosed(tokens: &[Token], open: char, close: char) -> &[Token] {
    if tokens.len() >= 2 && is_sym(&tokens[0], open) && is_sym(tokens.last().unwrap(), close) { &tokens[1..tokens.len()-1] } else { tokens }
}
fn split_top(tokens: &[Token], separator: char) -> Vec<&[Token]> {
    let mut parts = Vec::new();
    let mut start = 0;
    let mut depth = 0i32;
    for (index, token) in tokens.iter().enumerate() {
        match token {
            Token::Symbol('(' | '[' | '{') => depth += 1,
            Token::Symbol(')' | ']' | '}') => depth -= 1,
            Token::Symbol(found) if *found == separator && depth == 0 => { parts.push(&tokens[start..index]); start = index + 1; }
            _ => {}
        }
    }
    if start < tokens.len() { parts.push(&tokens[start..]); }
    parts
}
fn list(tokens: &[Token]) -> Vec<&[Token]> { split_top(enclosed(tokens, '[', ']'), ';') }
fn number(token: &Token) -> i64 {
    let Token::Number(value) = token else { return 0 };
    let value = value.trim_end_matches('L').replace('_', "");
    value.strip_prefix("0x").or_else(|| value.strip_prefix("0X")).map_or_else(|| value.parse().unwrap_or(0), |hex| i64::from_str_radix(hex, 16).unwrap_or(0))
}
fn fields(tokens: &[Token]) -> Vec<(String, Vec<Token>)> {
    split_top(enclosed(tokens, '{', '}'), ';').into_iter().filter_map(|field| {
        let eq = field.iter().position(|token| is_sym(token, '='))?;
        Some((word(field.first()?)?.to_owned(), field[eq+1..].to_vec()))
    }).collect()
}
fn get<'a>(fields: &'a [(String, Vec<Token>)], key: &str) -> &'a [Token] {
    fields.iter().find(|(field, _)| field == key).map(|(_, value)| value.as_slice()).unwrap_or(&[])
}
fn parse_ty(tokens: &[Token]) -> Ty {
    match name(tokens).as_str() {
        "Bool" | "Int" | "Nsuint" | "Nsuint_int" | "Nsint" | "Float" | "Double" => Ty::Scalar(name(tokens)),
        "Enum_of" => Ty::Enum(string(tokens)), "Str" => Ty::Text,
        "Obj" => Ty::Object(string(tokens)), "Opt_obj" => Ty::OptionalObject(string(tokens)),
        "Rec" => Ty::Record(string(tokens)), "Tuple" => Ty::Tuple(string(tokens)),
        other => panic!("unhandled Metal registry type {other}: {tokens:?}"),
    }
}
fn parse_ty_list(tokens: &[Token]) -> Vec<Ty> { list(tokens).into_iter().filter(|part| !part.is_empty()).map(parse_ty).collect() }
fn parse_result(tokens: &[Token]) -> Option<Ty> {
    if word(tokens.first()?) == Some("None") { None } else { Some(parse_ty(&tokens[1..])) }
}
fn parse_since(tokens: &[Token]) -> Option<(u32, u32)> {
    if word(tokens.first()?) != Some("Some") { return None; }
    let parts = split_top(enclosed(&tokens[1..], '(', ')'), ',');
    Some((number(parts[0].first()?) as u32, number(parts[1].first()?) as u32))
}
fn parse_enum_cases(tokens: &[Token]) -> Vec<EnumCase> {
    list(tokens).into_iter().filter(|part| !part.is_empty()).map(|item| {
        let values = split_top(enclosed(item, '(', ')'), ',');
        let version = split_top(enclosed(values[3], '(', ')'), ',');
        EnumCase {
            name: string(values[0]), symbol: string(values[1]), value: number(values[2].first().unwrap()),
            since: (number(version[0].first().unwrap()) as u32, number(version[1].first().unwrap()) as u32, number(version[2].first().unwrap()) as u32),
        }
    }).collect()
}
fn parse_record_fields(tokens: &[Token]) -> Vec<(String, String)> {
    list(tokens).into_iter().filter(|part| !part.is_empty()).map(|item| {
        let values = split_top(enclosed(item, '(', ')'), ',');
        (string(values[0]), name(values[1]))
    }).collect()
}
fn feature(fields: &[(String, Vec<Token>)]) -> String { name(get(fields, "feature")) }

fn parse_registry(source: &str) -> (Vec<Call>, Vec<EnumDef>, Vec<RecordDef>, Vec<NativeDef>) {
    let tokens = lex(source);
    let marker = tokens.windows(2).position(|pair| word(&pair[0]) == Some("let") && word(&pair[1]) == Some("entries")).expect("Metal registry entries");
    let list_start = marker + 2 + tokens[marker+2..].iter().position(|token| is_sym(token, '[')).expect("Metal registry list") + 1;
    let mut index = list_start;
    let mut calls = Vec::new();
    let mut enums = Vec::new();
    let mut records = Vec::new();
    let mut natives = Vec::new();
    while index < tokens.len() && !is_sym(&tokens[index], ']') {
        let Some(kind) = word(&tokens[index]).map(str::to_owned) else { index += 1; continue };
        if !matches!(kind.as_str(), "Method" | "Class_method" | "Property" | "Enum" | "Record" | "Native") { index += 1; continue; }
        index += 1;
        while index < tokens.len() && !is_sym(&tokens[index], '{') { index += 1; }
        let start = index;
        let mut depth = 0i32;
        while index < tokens.len() {
            if is_sym(&tokens[index], '{') { depth += 1; }
            if is_sym(&tokens[index], '}') { depth -= 1; if depth == 0 { index += 1; break; } }
            index += 1;
        }
        let entry = fields(&tokens[start..index]);
        let cap = feature(&entry);
        match kind.as_str() {
            "Method" | "Class_method" => {
                let class = kind == "Class_method";
                calls.push(Call {
                    name: string(get(&entry, "ocaml")),
                    receiver: if class { "Class".to_owned() } else { string(get(&entry, "recv")) },
                    objc: string(get(&entry, "objc")), selector: string(get(&entry, "sel")),
                    args: parse_ty_list(get(&entry, "args")), result: parse_result(get(&entry, "ret")),
                    error: name(get(&entry, "error")) == "true", pool: class || name(get(&entry, "pool")) == "true",
                    since: parse_since(get(&entry, "since")), feature: cap, body: Body::Send,
                });
            }
            "Property" => {
                let receiver = string(get(&entry, "recv"));
                let objc = string(get(&entry, "objc"));
                let property = string(get(&entry, "name"));
                let binding = string(get(&entry, "ocaml"));
                let ty = parse_ty(get(&entry, "ty"));
                let access = name(get(&entry, "access"));
                let since = parse_since(get(&entry, "since"));
                if access != "Set" { calls.push(Call { name: binding.clone(), receiver: receiver.clone(), objc: objc.clone(), selector: property.clone(), args: vec![], result: Some(ty.clone()), error: false, pool: true, since, feature: cap.clone(), body: Body::Read(property.clone()) }); }
                if access != "Get" { calls.push(Call { name: format!("set_{binding}"), receiver, objc, selector: format!("{property}="), args: vec![ty], result: None, error: false, pool: true, since, feature: cap, body: Body::Write(property) }); }
            }
            "Enum" => enums.push(EnumDef { name: string(get(&entry, "ocaml")), feature: cap, cases: parse_enum_cases(get(&entry, "cases")) }),
            "Record" => records.push(RecordDef { name: string(get(&entry, "ocaml")), sdk: string(get(&entry, "sdk")), feature: cap, fields: parse_record_fields(get(&entry, "fields")) }),
            "Native" => natives.push(NativeDef { name: string(get(&entry, "ocaml")), signature: string(get(&entry, "signature")), primitives: list(get(&entry, "primitives")).into_iter().map(string).collect(), reason: string(get(&entry, "reason")) }),
            _ => unreachable!(),
        }
    }
    (calls, enums, records, natives)
}

fn feature_name(name: &str) -> &'static str {
    match name {
        "Buffer" => "Buffer", "Texture" => "Texture", "Sampler" => "Sampler", "Compute_pipeline" => "ComputePipeline",
        "Render_pipeline" => "RenderPipeline", "Queue" => "Queue", "Surface" => "Surface", "Ray_tracing" => "RayTracing",
        "Ray_tracing_curves" => "RayTracingCurves", "Ray_tracing_motion" => "RayTracingMotion", "Ray_tracing_refit" => "RayTracingRefit",
        "Memoryless_attachments" => "MemorylessAttachments", other => panic!("unmapped Metal capability {other}"),
    }
}
fn ident(value: &str) -> String { value.replace('-', "_") }
fn kind(value: &str) -> String { value.to_ascii_uppercase() }
fn rust_ty(ty: &Ty) -> String {
    match ty {
        Ty::Scalar(v) => match v.as_str() { "Bool" => "bool", "Int" => "i32", "Nsuint" => "u64", "Nsuint_int" => "usize", "Nsint" => "isize", "Float" => "f32", "Double" => "f64", _ => unreachable!() }.to_owned(),
        Ty::Enum(_) => "i64".to_owned(), Ty::Text => "&str".to_owned(), Ty::Object(_) => "&Handle".to_owned(), Ty::OptionalObject(_) => "Option<&Handle>".to_owned(),
        Ty::Record(v) | Ty::Tuple(v) => format!("Record::{}", ident(v)),
    }
}
fn rust_result_ty(ty: &Ty) -> String {
    match ty {
        Ty::Text => "String".to_owned(),
        Ty::Object(_) => "Handle".to_owned(),
        Ty::OptionalObject(_) => "Option<Handle>".to_owned(),
        _ => rust_ty(ty),
    }
}
fn rust_ffi_ty(ty: &Ty) -> String {
    match ty {
        Ty::Scalar(v) => match v.as_str() { "Bool" => "u8", "Int" => "i32", "Nsuint" => "u64", "Nsuint_int" => "usize", "Nsint" => "isize", "Float" => "f32", "Double" => "f64", _ => unreachable!() }.to_owned(),
        Ty::Enum(_) => "i64".to_owned(), Ty::Text => "*const std::ffi::c_char".to_owned(),
        Ty::Object(_) | Ty::OptionalObject(_) => "*mut super::NativeHandle".to_owned(),
        Ty::Record(v) | Ty::Tuple(v) => format!("Record::{}", ident(v)),
    }
}
fn cpp_ffi_ty(ty: &Ty, records: &[RecordDef]) -> String {
    match ty {
        Ty::Scalar(v) => match v.as_str() { "Bool" => "uint8_t", "Int" => "int32_t", "Nsuint" => "uint64_t", "Nsuint_int" => "uintptr_t", "Nsint" => "intptr_t", "Float" => "float", "Double" => "double", _ => unreachable!() }.to_owned(),
        Ty::Enum(_) => "int64_t".to_owned(), Ty::Text => "const char *".to_owned(), Ty::Object(_) | Ty::OptionalObject(_) => "MetalHandle *".to_owned(),
        Ty::Record(v) | Ty::Tuple(v) => records.iter().find(|record| record.name == *v).expect("registry record").sdk.clone(),
    }
}
fn cpp_arg(i: usize, ty: &Ty, records: &[RecordDef]) -> String {
    match ty {
        Ty::Scalar(v) => match v.as_str() { "Bool" => format!("static_cast<BOOL>(arg{i} != 0)"), "Int" => format!("static_cast<int>(arg{i})"), "Nsuint" | "Nsuint_int" => format!("static_cast<NSUInteger>(arg{i})"), "Nsint" => format!("static_cast<NSInteger>(arg{i})"), "Float" => format!("static_cast<float>(arg{i})"), "Double" => format!("arg{i}"), _ => unreachable!() },
        Ty::Enum(v) => format!("static_cast<{v}>(arg{i})"), Ty::Text => format!("text{i}"),
        Ty::Object(v) => format!("checked_object(arg{i}, KIND_{})", kind(v)),
        Ty::OptionalObject(v) => format!("(arg{i} == nullptr ? nil : checked_object(arg{i}, KIND_{}))", kind(v)),
        Ty::Record(v) | Ty::Tuple(v) => { let record = records.iter().find(|record| record.name == *v).expect("registry record"); format!("{}{{{}}}", record.sdk, record.fields.iter().map(|(field, _)| format!("arg{i}.{field}")).collect::<Vec<_>>().join(", ")) },
    }
}
fn cpp_native_ty(ty: &Ty, records: &[RecordDef]) -> String {
    match ty {
        Ty::Scalar(v) => match v.as_str() { "Bool" => "BOOL", "Int" => "int", "Nsuint" | "Nsuint_int" => "NSUInteger", "Nsint" => "NSInteger", "Float" => "float", "Double" => "double", _ => unreachable!() }.to_owned(),
        Ty::Enum(v) => v.clone(), Ty::Text => "NSString *".to_owned(), Ty::Object(_) | Ty::OptionalObject(_) => "id".to_owned(),
        Ty::Record(v) | Ty::Tuple(v) => records.iter().find(|record| record.name == *v).expect("registry record").sdk.clone(),
    }
}
fn cpp_out_ty(ty: &Ty, records: &[RecordDef]) -> String { cpp_ffi_ty(ty, records) }
fn cpp_out_value(ty: &Ty) -> String {
    match ty {
        Ty::Scalar(v) if v == "Bool" => "*out = native_result ? 1 : 0;".to_owned(),
        Ty::Scalar(v) if v == "Nsuint" => "*out = static_cast<uint64_t>(native_result);".to_owned(),
        Ty::Scalar(v) if v == "Nsuint_int" => "*out = static_cast<uintptr_t>(native_result);".to_owned(),
        Ty::Scalar(v) if v == "Nsint" => "*out = static_cast<intptr_t>(native_result);".to_owned(),
        Ty::Scalar(_) => "*out = native_result;".to_owned(),
        Ty::Enum(_) => "*out = static_cast<int64_t>(native_result);".to_owned(),
        Ty::Text => "text_buffer = native_result.UTF8String ?: \"\"; *out = text_buffer.c_str();".to_owned(),
        Ty::Object(v) => format!("if (native_result == nil) return set_error(message, \"Metal returned nil\"); *out = make_handle(native_result, KIND_{});", kind(v)),
        Ty::OptionalObject(v) => format!("*out = native_result == nil ? nullptr : make_handle(native_result, KIND_{});", kind(v)),
        Ty::Record(_) | Ty::Tuple(_) => "*out = native_result;".to_owned(),
    }
}
fn cpp_call(call: &Call, records: &[RecordDef]) -> String {
    let symbol = format!("rays_metal_{}", call.name);
    let mut params = Vec::new();
    if call.receiver != "Class" { params.push("MetalHandle *receiver".to_owned()); }
    for (i, ty) in call.args.iter().enumerate() { params.push(format!("{} arg{i}", cpp_ffi_ty(ty, records))); }
    if let Some(result) = &call.result { params.push(format!("{} *out", cpp_out_ty(result, records))); }
    params.push("const char **message".to_owned());
    let mut out = format!("extern \"C\" int32_t {symbol}({}) {{\n  if (message) *message = nullptr;\n  @try {{\n    @autoreleasepool {{\n", params.join(", "));
    for (i, ty) in call.args.iter().enumerate() { if matches!(ty, Ty::Text) { out.push_str(&format!("      if (arg{i} == nullptr) return set_error(message, \"null UTF-8 input\"); NSString *text{i} = [NSString stringWithUTF8String:arg{i}]; if (text{i} == nil) return set_error(message, \"invalid UTF-8 input\");\n")); } }
    if call.receiver != "Class" { out.push_str(&format!("      {} object = checked_object(receiver, KIND_{});\n", call.objc, kind(&call.receiver))); }
    let available_indent = if let Some((major, minor)) = call.since { out.push_str(&format!("      if (@available(macOS {major}.{minor}, *)) {{\n")); "        " } else { "      " };
    let args = call.args.iter().enumerate().map(|(i, ty)| cpp_arg(i, ty, records)).collect::<Vec<_>>();
    let expression = match &call.body {
        Body::Read(property) => format!("object.{property}"),
        Body::Write(property) => format!("object.{property} = {}", args[0]),
        Body::Send => {
            let pieces = call.selector.split(':').filter(|part| !part.is_empty()).collect::<Vec<_>>();
            let mut selector = if args.is_empty() { pieces.first().copied().unwrap_or(&call.selector).to_owned() } else { String::new() };
            for (piece, argument) in pieces.iter().zip(&args) { selector.push_str(&format!("{}{piece}:{argument}", if selector.is_empty() { "" } else { " " })); }
            format!("[{} {selector}]", if call.receiver == "Class" { call.objc.as_str() } else { "object" })
        }
    };
    let expression = if call.error {
        let pieces = call.selector.split(':').filter(|part| !part.is_empty()).collect::<Vec<_>>();
        let mut selector = String::new();
        for (piece, argument) in pieces.iter().zip(args.iter().chain(std::iter::once(&"&native_error".to_owned()))) { selector.push_str(&format!("{}{piece}:{argument}", if selector.is_empty() { "" } else { " " })); }
        format!("[object {selector}]")
    } else { expression };
    if call.error { out.push_str(&format!("{available_indent}NSError *native_error = nil;\n")); }
    if let Some(result) = &call.result {
        out.push_str(&format!("{available_indent}const {} native_result = {expression};\n", cpp_native_ty(result, records)));
        if call.error { out.push_str(&format!("{available_indent}if (native_error != nil) return set_error(message, native_error.localizedDescription.UTF8String);\n")); }
        out.push_str(&format!("{available_indent}{}\n", cpp_out_value(result)));
    } else if call.error {
        out.push_str(&format!("{available_indent}const BOOL native_success = {expression};\n"));
        out.push_str(&format!("{available_indent}if (native_error != nil) return set_error(message, native_error.localizedDescription.UTF8String);\n"));
        out.push_str(&format!("{available_indent}if (!native_success) return set_error(message, \"Metal operation failed\");\n"));
    } else {
        out.push_str(&format!("{available_indent}{expression};\n"));
    }
    out.push_str(&format!("{available_indent}return 0;\n"));
    if call.since.is_some() { out.push_str("      } else { return set_unavailable(message); }\n"); }
    out.push_str("    }\n  } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); }\n}\n");
    out
}
fn emit_record(record: &RecordDef) -> String {
    let mut out = format!("#[repr(C)]\n#[derive(Clone, Copy, Debug, Default, PartialEq)]\npub struct {} {{\n", ident(&record.name));
    for (field, ty) in &record.fields {
        let ty = match ty.as_str() { "Bool" => "u8", "Int" => "i32", "Nsuint" => "u64", "Nsuint_int" => "usize", "Nsint" => "isize", "Float" => "f32", "Double" => "f64", _ => panic!("unknown Metal record scalar {ty}") };
        out.push_str(&format!("    pub {field}: {ty},\n"));
    }
    out.push_str("}\n"); out
}
fn emit_call(call: &Call) -> String {
    let symbol = format!("rays_metal_{}", call.name);
    let mut params = Vec::new();
    if call.receiver != "Class" { params.push("receiver: &Handle".to_owned()); }
    for (i, ty) in call.args.iter().enumerate() { params.push(format!("arg{i}: {}", rust_ty(ty))); }
    let result = call.result.as_ref().map(rust_result_ty).unwrap_or_else(|| "()".to_owned());
    let mut out = format!("pub fn {}({}) -> Result<{result}, Error> {{\n", ident(&call.name), params.join(", "));
    if call.receiver != "Class" { out.push_str("    let receiver_ptr = receiver.as_native_ptr()?;\n"); }
    let mut args = Vec::new();
    for (i, ty) in call.args.iter().enumerate() {
        match ty {
            Ty::Text => { out.push_str(&format!("    let arg{i}_cstring = CString::new(arg{i}).map_err(|_| Error::InvalidArgument(\"Metal strings must not contain NUL\"))?;\n")); args.push(format!("arg{i}_cstring.as_ptr()")); }
            Ty::Object(_) => args.push(format!("arg{i}.as_native_ptr()?")),
            Ty::OptionalObject(_) => args.push(format!("match arg{i} {{ Some(value) => value.as_native_ptr()?, None => std::ptr::null_mut() }}")),
            Ty::Scalar(v) if v == "Bool" => args.push(format!("u8::from(arg{i})")),
            _ => args.push(format!("arg{i}")),
        }
    }
    out.push_str("    let mut message = std::ptr::null();\n");
    if let Some(ty) = &call.result { out.push_str(&format!("    let mut output: MaybeUninit<{}> = MaybeUninit::uninit();\n", rust_ffi_ty(ty))); }
    let mut ffi_args = Vec::new();
    if call.receiver != "Class" { ffi_args.push("receiver_ptr".to_owned()); }
    ffi_args.extend(args);
    if call.result.is_some() { ffi_args.push("output.as_mut_ptr()".to_owned()); }
    ffi_args.push("&mut message".to_owned());
    out.push_str(&format!("    #[cfg(target_vendor = \"apple\")] let status = unsafe {{ ffi_{symbol}({}) }};\n", ffi_args.join(", ")));
    out.push_str("    #[cfg(not(target_vendor = \"apple\"))] let status = 2;\n    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }\n");
    if let Some(ty) = &call.result {
        let value = match ty {
            Ty::Scalar(v) if v == "Bool" => "unsafe { output.assume_init() != 0 }".to_owned(),
            Ty::Text => "unsafe { CStr::from_ptr(output.assume_init()).to_string_lossy().into_owned() }".to_owned(),
            Ty::Object(_) => "unsafe { Handle::from_native(output.assume_init())? }".to_owned(),
            Ty::OptionalObject(_) => "unsafe { let pointer = output.assume_init(); if pointer.is_null() { None } else { Some(Handle::from_native(pointer)?) } }".to_owned(),
            _ => "unsafe { output.assume_init() }".to_owned(),
        };
        out.push_str(&format!("    Ok({value})\n"));
    } else { out.push_str("    Ok(())\n"); }
    out.push_str("}\n");
    let mut ffi_params = Vec::new();
    if call.receiver != "Class" { ffi_params.push("receiver: *mut super::NativeHandle".to_owned()); }
    for (i, ty) in call.args.iter().enumerate() { ffi_params.push(format!("arg{i}: {}", rust_ffi_ty(ty))); }
    if let Some(ty) = &call.result { ffi_params.push(format!("output: *mut {}", rust_ffi_ty(ty))); }
    ffi_params.push("message: *mut *const std::ffi::c_char".to_owned());
    out.push_str(&format!("#[cfg(target_vendor = \"apple\")] unsafe extern \"C\" {{ #[link_name = \"{symbol}\"] fn ffi_{symbol}({}) -> i32; }}\n", ffi_params.join(", ")));
    out
}
fn emit_rust(calls: &[Call], enums: &[EnumDef], records: &[RecordDef], natives: &[NativeDef], kinds: &BTreeSet<String>) -> String {
    let mut out = String::from("// Generated from lib/metal/gen/registry.ml. Do not edit.\nuse super::{Error, Handle, NativeHandle};\nuse std::ffi::{CStr, CString};\nuse std::mem::MaybeUninit;\n\n#[allow(non_snake_case)] pub mod Record {\n");
    for record in records { for line in emit_record(record).lines() { out.push_str("    "); out.push_str(line); out.push('\n'); } }
    out.push_str("}\n#[derive(Clone, Copy, Debug, Eq, PartialEq)] pub enum Feature { Buffer, Texture, Sampler, ComputePipeline, RenderPipeline, Queue, Surface, RayTracing, RayTracingCurves, RayTracingMotion, RayTracingRefit, MemorylessAttachments }\n#[derive(Clone, Copy, Debug)] pub struct BindingMetadata { pub name: &'static str, pub receiver: &'static str, pub selector: &'static str, pub feature: Feature, pub since: Option<(u16,u16)>, pub autorelease_pool: bool }\npub const BINDINGS: &[BindingMetadata] = &[\n");
    for call in calls {
        let since = call.since.map(|(a,b)| format!("Some(({a},{b}))")).unwrap_or_else(|| "None".to_owned());
        out.push_str(&format!("BindingMetadata {{ name: {:?}, receiver: {:?}, selector: {:?}, feature: Feature::{}, since: {}, autorelease_pool: {} }},\n", call.name, call.receiver, call.selector, feature_name(&call.feature), since, call.pool));
    }
    out.push_str("];\n#[derive(Clone, Copy, Debug)] pub struct NativeMetadata { pub name: &'static str, pub signature: &'static str, pub primitives: &'static [&'static str], pub reason: &'static str }\npub const NATIVE_BINDINGS: &[NativeMetadata] = &[\n");
    for native in natives { out.push_str(&format!("NativeMetadata {{ name: {:?}, signature: {:?}, primitives: &[{}], reason: {:?} }},\n", native.name, native.signature, native.primitives.iter().map(|value| format!("{value:?}")).collect::<Vec<_>>().join(","), native.reason)); }
    out.push_str("];\n#[derive(Clone, Copy, Debug)] pub struct EnumCase { pub name: &'static str, pub symbol: &'static str, pub value: i64, pub since: (u32,u32,u32) }\n#[derive(Clone, Copy, Debug)] pub struct EnumMetadata { pub name: &'static str, pub feature: Feature, pub cases: &'static [EnumCase] }\n#[allow(non_snake_case, non_upper_case_globals)]\n");
    for en in enums {
        out.push_str(&format!("pub mod {} {{ pub type Value = i64;\n", ident(&en.name)));
        for case in &en.cases { out.push_str(&format!("pub const {}: Value = {}i64;\n", ident(&case.name), case.value)); }
        out.push_str("pub fn to_int64(value: Value) -> i64 { value }\npub fn of_int64(value: i64) -> Option<Value> { match value {\n");
        let unique: BTreeSet<i64> = en.cases.iter().map(|case| case.value).collect();
        for value in unique { out.push_str(&format!("{value} => Some(value),\n")); }
        out.push_str("_ => None, } }\n}\n");
    }
    out.push_str("pub const ENUMS: &[EnumMetadata] = &[\n");
    for en in enums {
        out.push_str(&format!("EnumMetadata {{ name: {:?}, feature: Feature::{}, cases: &[\n", en.name, feature_name(&en.feature)));
        for case in &en.cases { out.push_str(&format!("EnumCase {{ name: {:?}, symbol: {:?}, value: {}i64, since: ({},{},{}) }},\n", case.name, case.symbol, case.value, case.since.0, case.since.1, case.since.2)); }
        out.push_str("] },\n");
    }
    out.push_str("];\n#[derive(Clone, Copy, Debug)] pub struct RecordMetadata { pub name: &'static str, pub sdk: &'static str, pub feature: Feature }\npub const RECORDS: &[RecordMetadata] = &[\n");
    for record in records { out.push_str(&format!("RecordMetadata {{ name: {:?}, sdk: {:?}, feature: Feature::{} }},\n", record.name, record.sdk, feature_name(&record.feature))); }
    out.push_str("];\npub const HANDLE_KINDS: &[(&str,u32)] = &[\n");
    for (index, value) in kinds.iter().enumerate() { out.push_str(&format!("({value:?}, {}),\n", index + 1)); }
    out.push_str("];\n");
    for (index, value) in kinds.iter().enumerate() { out.push_str(&format!("pub const KIND_{}: u32 = {};\n", kind(value), index + 1)); }
    for call in calls { out.push_str(&emit_call(call)); }
    out
}
fn emit_cpp(calls: &[Call], enums: &[EnumDef], records: &[RecordDef]) -> String {
    let mut out = String::from("#include <Foundation/Foundation.h>\n#include <Metal/Metal.h>\n#include <QuartzCore/CAMetalLayer.h>\n#include <cstddef>\n#include <cstdint>\n#include <string>\n#include <new>\n#include <type_traits>\n#include \"metal_handle.h\"\n#include \"metal_kinds.h\"\nstruct MetalHandle { void *object; uint32_t kind; };\nstatic thread_local std::string error_text;\nstatic thread_local std::string text_buffer;\nextern \"C\" int32_t rays_metal_set_error(const char **out, const char *message) { error_text = message ? message : \"Metal operation failed\"; if (out) *out = error_text.c_str(); return 1; }\nextern \"C\" int32_t rays_metal_set_unavailable(const char **out, const char *message) { error_text = message ? message : \"Metal API is unavailable on this macOS version\"; if (out) *out = error_text.c_str(); return 2; }\nextern \"C\" void *rays_metal_handle_object(MetalHandle *handle, uint32_t expected_kind) { if (handle == nullptr || handle->object == nullptr || handle->kind != expected_kind) return nullptr; return handle->object; }\nextern \"C\" MetalHandle *rays_metal_handle_new(void *pointer, uint32_t kind) { if (pointer == nullptr) return nullptr; id object = (__bridge id)pointer; return new (std::nothrow) MetalHandle{(__bridge_retained void *)object, kind}; }\nstatic int32_t set_error(const char **out, const char *message) { return rays_metal_set_error(out, message); }\nstatic int32_t set_unavailable(const char **out) { return rays_metal_set_unavailable(out, nullptr); }\nstatic MetalHandle *make_handle(id object, uint32_t kind) { return object == nil ? nullptr : rays_metal_handle_new((__bridge void *)object, kind); }\nstatic id checked_object(MetalHandle *handle, uint32_t kind) { void *pointer = rays_metal_handle_object(handle, kind); if (pointer == nullptr) @throw [NSException exceptionWithName:@\"MetalHandleError\" reason:@\"invalid Metal handle\" userInfo:nil]; return (__bridge id)pointer; }\nextern \"C\" void rays_metal_handle_destroy(MetalHandle *handle) { if (handle != nullptr) { if (handle->object != nullptr) (void)CFBridgingRelease(handle->object); delete handle; } }\n");
    out.push_str("extern \"C\" int32_t rays_metal_default_device(MetalHandle **out, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLDevice> device = MTLCreateSystemDefaultDevice(); if (device == nil) return set_error(message, \"no default Metal device\"); *out = make_handle(device, KIND_DEVICE); return *out ? 0 : set_error(message, \"unable to allocate Metal handle\"); } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }\n");
    out.push_str("extern \"C\" int32_t caml_rays_metal_device_supports_sampler_reduction(MetalHandle *handle, uint8_t *output, const char **message) { if (message) *message = nullptr; if (output == nullptr) return set_error(message, \"invalid sampler capability output\"); @try { @autoreleasepool { id<MTLDevice> device = checked_object(handle, KIND_DEVICE); BOOL supported = NO; if (@available(macOS 26.0, *)) { supported = [device supportsFamily:MTLGPUFamilyApple10] && [MTLSamplerDescriptor instancesRespondToSelector:@selector(reductionMode)] && [MTLSamplerDescriptor instancesRespondToSelector:@selector(setReductionMode:)] && [MTLSamplerDescriptor instancesRespondToSelector:@selector(lodBias)] && [MTLSamplerDescriptor instancesRespondToSelector:@selector(setLodBias:)]; } *output = supported ? 1 : 0; return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }\n");
    out.push_str("extern \"C\" int pthread_main_np(void);\nextern \"C\" int32_t rays_metal_is_main_thread(void) { return pthread_main_np(); }\n");
    out.push_str("extern \"C\" int32_t rays_metal_buffer_copy(MetalHandle *handle, uint64_t offset, uint8_t *bytes, uintptr_t length, uint8_t write, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLBuffer> buffer = checked_object(handle, KIND_BUFFER); if (offset > buffer.length || length > buffer.length - offset) return set_error(message, \"buffer range is out of bounds\"); void *contents = buffer.contents; if (contents == nullptr) return set_error(message, \"buffer has no CPU mapping\"); if (length != 0) { if (write) __builtin_memcpy(static_cast<uint8_t *>(contents) + offset, bytes, length); else __builtin_memcpy(bytes, static_cast<uint8_t *>(contents) + offset, length); } if (write && length != 0 && buffer.storageMode == MTLStorageModeManaged) [buffer didModifyRange:NSMakeRange(static_cast<NSUInteger>(offset), static_cast<NSUInteger>(length))]; return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }\n");
    out.push_str("#include <unistd.h>\n");
    out.push_str("extern \"C\" int32_t rays_metal_buffer_create_mapped(MetalHandle *device_handle, uint64_t length, int64_t options, MetalHandle **out, float **mapping, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLDevice> device = checked_object(device_handle, KIND_DEVICE); if (length == 0 || (length & 3) != 0) return set_error(message, \"mapped buffer length must be a positive multiple of four\"); const uint64_t page = static_cast<uint64_t>(getpagesize()); if (page == 0 || length > UINT64_MAX - (page - 1)) return set_error(message, \"mapped buffer rounded length overflows\"); const uint64_t rounded = ((length + page - 1) / page) * page; if (rounded > std::numeric_limits<NSUInteger>::max()) return set_error(message, \"mapped buffer exceeds Metal length bounds\"); id<MTLBuffer> buffer = [device newBufferWithLength:static_cast<NSUInteger>(rounded) options:static_cast<MTLResourceOptions>(options)]; if (buffer == nil || buffer.contents == nullptr) return set_error(message, \"unable to allocate mapped Metal buffer\"); __builtin_memset(buffer.contents, 0, static_cast<size_t>(rounded)); *out = make_handle(buffer, KIND_BUFFER); if (*out == nullptr) return set_error(message, \"unable to allocate Metal handle\"); *mapping = static_cast<float *>(buffer.contents); return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }\n");
    out.push_str("struct RaysTextureDescriptor { int32_t texture_type; int32_t pixel_format; uint64_t width; uint64_t height; uint64_t depth; uint64_t mip_levels; uint64_t sample_count; uint64_t array_length; int32_t storage_mode; int32_t cpu_cache_mode; int32_t hazard_tracking_mode; int32_t usage; uint8_t allow_gpu_optimized_contents; int32_t compression_type; int32_t swizzle_red; int32_t swizzle_green; int32_t swizzle_blue; int32_t swizzle_alpha; };\nextern \"C\" int32_t rays_metal_texture_create(MetalHandle *device_handle, const RaysTextureDescriptor *source, const char *label, MetalHandle **out, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLDevice> device = checked_object(device_handle, KIND_DEVICE); MTLTextureDescriptor *descriptor = [[MTLTextureDescriptor alloc] init]; descriptor.textureType = static_cast<MTLTextureType>(source->texture_type); descriptor.pixelFormat = static_cast<MTLPixelFormat>(source->pixel_format); descriptor.width = static_cast<NSUInteger>(source->width); descriptor.height = static_cast<NSUInteger>(source->height); descriptor.depth = static_cast<NSUInteger>(source->depth); descriptor.mipmapLevelCount = static_cast<NSUInteger>(source->mip_levels); descriptor.sampleCount = static_cast<NSUInteger>(source->sample_count); descriptor.arrayLength = static_cast<NSUInteger>(source->array_length); descriptor.storageMode = static_cast<MTLStorageMode>(source->storage_mode); descriptor.cpuCacheMode = static_cast<MTLCPUCacheMode>(source->cpu_cache_mode); descriptor.hazardTrackingMode = static_cast<MTLHazardTrackingMode>(source->hazard_tracking_mode); descriptor.usage = static_cast<MTLTextureUsage>(source->usage); descriptor.allowGPUOptimizedContents = source->allow_gpu_optimized_contents != 0; if (@available(macOS 15.0, *)) descriptor.compressionType = static_cast<MTLTextureCompressionType>(source->compression_type); else if (source->compression_type != 0) return set_unavailable(message); if (@available(macOS 13.0, *)) descriptor.swizzle = MTLTextureSwizzleChannelsMake(static_cast<MTLTextureSwizzle>(source->swizzle_red), static_cast<MTLTextureSwizzle>(source->swizzle_green), static_cast<MTLTextureSwizzle>(source->swizzle_blue), static_cast<MTLTextureSwizzle>(source->swizzle_alpha)); else if (source->swizzle_red != 2 || source->swizzle_green != 3 || source->swizzle_blue != 4 || source->swizzle_alpha != 5) return set_unavailable(message); NSString *text = nil; if (label != nullptr) { text = [NSString stringWithUTF8String:label]; if (text == nil) return set_error(message, \"invalid UTF-8 texture label\"); } id<MTLTexture> texture = [device newTextureWithDescriptor:descriptor]; if (texture == nil) return set_error(message, \"Metal returned no texture\"); if (text != nil) texture.label = text; *out = make_handle(texture, KIND_TEXTURE); return *out ? 0 : set_error(message, \"unable to allocate Metal handle\"); } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }\nextern \"C\" int32_t rays_metal_texture_read(MetalHandle *handle, uint8_t *destination, uintptr_t bytes_per_row, uintptr_t bytes_per_image, uint64_t x, uint64_t y, uint64_t z, uint64_t width, uint64_t height, uint64_t depth, uint64_t mip_level, uint64_t slice, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLTexture> texture = checked_object(handle, KIND_TEXTURE); MTLRegion region = MTLRegionMake3D(static_cast<NSUInteger>(x), static_cast<NSUInteger>(y), static_cast<NSUInteger>(z), static_cast<NSUInteger>(width), static_cast<NSUInteger>(height), static_cast<NSUInteger>(depth)); [texture getBytes:destination bytesPerRow:bytes_per_row bytesPerImage:bytes_per_image fromRegion:region mipmapLevel:static_cast<NSUInteger>(mip_level) slice:static_cast<NSUInteger>(slice)]; return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }\n");
    out.push_str("extern \"C\" int32_t rays_metal_command_buffer_wait(MetalHandle *handle, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLCommandBuffer> buffer = checked_object(handle, KIND_COMMAND_BUFFER); [buffer waitUntilCompleted]; return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }\nextern \"C\" int32_t rays_metal_command_buffer_error(MetalHandle *handle, const char **output) { @try { @autoreleasepool { id<MTLCommandBuffer> buffer = checked_object(handle, KIND_COMMAND_BUFFER); NSError *error = buffer.error; if (error == nil) { *output = nullptr; return 0; } text_buffer = error.localizedDescription.UTF8String ?: \"Metal command buffer failed\"; *output = text_buffer.c_str(); return 1; } } @catch (NSException *exception) { text_buffer = exception.reason.UTF8String ?: \"Metal command buffer failed\"; *output = text_buffer.c_str(); return 1; } }\nextern \"C\" int32_t rays_metal_compute_encoder_set_bytes(MetalHandle *handle, const uint8_t *bytes, uintptr_t length, uint64_t index, const char **message) { if (message) *message = nullptr; if (length == 0) return set_error(message, \"compute byte binding must not be empty\"); @try { @autoreleasepool { id<MTLComputeCommandEncoder> encoder = checked_object(handle, KIND_COMPUTE_ENCODER); [encoder setBytes:bytes length:length atIndex:static_cast<NSUInteger>(index)]; return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }\nextern \"C\" int32_t rays_metal_compute_encoder_use_accelerations(MetalHandle *handle, MetalHandle **structures, uintptr_t count, const char **message) { if (message) *message = nullptr; if (count == 0) return set_error(message, \"acceleration structure list must not be empty\"); @try { @autoreleasepool { id<MTLComputeCommandEncoder> encoder = checked_object(handle, KIND_COMPUTE_ENCODER); for (uintptr_t index = 0; index < count; ++index) { id<MTLAccelerationStructure> structure = checked_object(structures[index], KIND_ACCELERATION_STRUCTURE); [encoder useResource:structure usage:MTLResourceUsageRead]; } return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }\n");
    out.push_str("struct RaysPresentationCommandSnapshot { uint64_t queue_device_id; uint64_t device_id; uint64_t error_options; double gpu_start; double gpu_end; double kernel_start; double kernel_end; uint8_t retained_references; };\nextern \"C\" int32_t caml_rays_metal_presentation_command_snapshot(MetalHandle *handle, RaysPresentationCommandSnapshot *out, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLCommandBuffer> buffer = checked_object(handle, KIND_COMMAND_BUFFER); out->queue_device_id = buffer.commandQueue.device.registryID; out->device_id = buffer.device.registryID; out->error_options = static_cast<uint64_t>(buffer.errorOptions); out->gpu_start = buffer.GPUStartTime; out->gpu_end = buffer.GPUEndTime; out->kernel_start = buffer.kernelStartTime; out->kernel_end = buffer.kernelEndTime; out->retained_references = buffer.retainedReferences ? 1 : 0; return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }\n");
    out.push_str("extern \"C\" int32_t caml_rays_metal_default_device(MetalHandle **out, const char **message) { return rays_metal_default_device(out, message); }\nextern \"C\" int32_t caml_rays_metal_is_main_thread(void) { return rays_metal_is_main_thread(); }\nextern \"C\" int32_t caml_rays_metal_buffer_write(MetalHandle *handle, uint64_t destination_offset, const uint8_t *bytes, uintptr_t source_offset, uintptr_t length, const char **message) { if (source_offset > UINTPTR_MAX - length) return set_error(message, \"buffer source range overflow\"); return rays_metal_buffer_copy(handle, destination_offset, const_cast<uint8_t *>(bytes + source_offset), length, 1, message); }\nextern \"C\" int32_t caml_rays_metal_buffer_read(MetalHandle *handle, uint64_t offset, uintptr_t length, uint8_t *destination, const char **message) { return rays_metal_buffer_copy(handle, offset, destination, length, 0, message); }\nextern \"C\" int32_t caml_rays_metal_buffer_create_mapped(MetalHandle *device, uint64_t length, int64_t options, MetalHandle **out, float **mapping, const char **message) { return rays_metal_buffer_create_mapped(device, length, options, out, mapping, message); }\nextern \"C\" int32_t caml_rays_metal_texture_create(MetalHandle *device, const RaysTextureDescriptor *descriptor, const char *label, MetalHandle **out, const char **message) { return rays_metal_texture_create(device, descriptor, label, out, message); }\nextern \"C\" int32_t caml_rays_metal_texture_read(MetalHandle *texture, uint8_t *destination, uintptr_t row, uintptr_t image, uint64_t x, uint64_t y, uint64_t z, uint64_t width, uint64_t height, uint64_t depth, uint64_t mip, uint64_t slice, const char **message) { return rays_metal_texture_read(texture, destination, row, image, x, y, z, width, height, depth, mip, slice, message); }\nextern \"C\" int32_t caml_rays_metal_texture_read_into(MetalHandle *texture, uint8_t *destination, uintptr_t row, uintptr_t image, uint64_t x, uint64_t y, uint64_t z, uint64_t width, uint64_t height, uint64_t depth, uint64_t mip, uint64_t slice, const char **message) { return rays_metal_texture_read(texture, destination, row, image, x, y, z, width, height, depth, mip, slice, message); }\nextern \"C\" int32_t caml_rays_metal_command_buffer_wait(MetalHandle *buffer, const char **message) { return rays_metal_command_buffer_wait(buffer, message); }\nextern \"C\" int32_t caml_rays_metal_command_buffer_error(MetalHandle *buffer, const char **output) { return rays_metal_command_buffer_error(buffer, output); }\nextern \"C\" int32_t caml_rays_metal_compute35_bytes_plain(MetalHandle *encoder, const uint8_t *bytes, uintptr_t length, uint64_t index, const char **message) { return rays_metal_compute_encoder_set_bytes(encoder, bytes, length, index, message); }\nextern \"C\" int32_t caml_rays_metal_compute35_accelerations(MetalHandle *encoder, MetalHandle **structures, uintptr_t count, const char **message) { return rays_metal_compute_encoder_use_accelerations(encoder, structures, count, message); }\n");
    out.push_str("extern \"C\" int32_t caml_rays_metal_texture_info(MetalHandle *handle, int64_t *values, const char **message) { if (message) *message = nullptr; if (values == nullptr) return set_error(message, \"invalid texture-info output\"); @try { @autoreleasepool { id<MTLTexture> texture = checked_object(handle, KIND_TEXTURE); MTLTextureSwizzleChannels swizzle = texture.swizzle; values[0] = static_cast<int64_t>(texture.textureType); values[1] = static_cast<int64_t>(texture.pixelFormat); values[2] = static_cast<int64_t>(texture.width); values[3] = static_cast<int64_t>(texture.height); values[4] = static_cast<int64_t>(texture.depth); values[5] = static_cast<int64_t>(texture.mipmapLevelCount); values[6] = static_cast<int64_t>(texture.sampleCount); values[7] = static_cast<int64_t>(texture.arrayLength); values[8] = static_cast<int64_t>(texture.usage); values[9] = static_cast<int64_t>(texture.storageMode); values[10] = static_cast<int64_t>(texture.cpuCacheMode); values[11] = static_cast<int64_t>(texture.hazardTrackingMode); values[12] = texture.allowGPUOptimizedContents ? 1 : 0; values[13] = static_cast<int64_t>(texture.compressionType); values[14] = static_cast<int64_t>(swizzle.red); values[15] = static_cast<int64_t>(swizzle.green); values[16] = static_cast<int64_t>(swizzle.blue); values[17] = static_cast<int64_t>(swizzle.alpha); return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }\n");
    out.push_str("#include <cstring>\n");
    out.push_str("extern \"C\" int32_t caml_rays_metal_library_compile(MetalHandle *device_handle, const char *source_bytes, const char *label_bytes, MetalHandle **out, const char **message) { if (message) *message = nullptr; if (source_bytes == nullptr) return set_error(message, \"null Metal source\"); @try { @autoreleasepool { id<MTLDevice> device = checked_object(device_handle, KIND_DEVICE); NSString *source = [NSString stringWithUTF8String:source_bytes]; if (source == nil) return set_error(message, \"Metal source is not valid UTF-8\"); NSString *label = label_bytes ? [NSString stringWithUTF8String:label_bytes] : nil; if (label_bytes && label == nil) return set_error(message, \"Metal library label is not valid UTF-8\"); MTLCompileOptions *options = [[MTLCompileOptions alloc] init]; options.fastMathEnabled = NO; NSError *error = nil; id<MTLLibrary> library = [device newLibraryWithSource:source options:options error:&error]; if (library == nil) return set_error(message, error.localizedDescription.UTF8String ?: \"Metal source compilation failed without NSError\"); library.label = label; if (library.device.registryID != device.registryID || ((label == nil) != (library.label == nil)) || (label && ![library.label isEqualToString:label])) return set_error(message, \"Metal changed checked library properties\"); *out = make_handle(library, KIND_LIBRARY); return *out ? 0 : set_error(message, \"unable to allocate Metal handle\"); } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }\nextern \"C\" int32_t caml_rays_metal_library_function_names(MetalHandle *handle, char **output, uintptr_t *length, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLLibrary> library = checked_object(handle, KIND_LIBRARY); std::string packed; for (NSString *name in library.functionNames) { const char *utf8 = name.UTF8String ?: \"\"; packed.append(utf8); packed.push_back('\\0'); } if (packed.empty()) packed.push_back('\\0'); char *bytes = new (std::nothrow) char[packed.size()]; if (bytes == nullptr) return set_error(message, \"unable to allocate function-name buffer\"); std::memcpy(bytes, packed.data(), packed.size()); *output = bytes; *length = packed.size(); return 0; } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }\nextern \"C\" void caml_rays_metal_free_bytes(char *bytes) { delete[] bytes; }\nextern \"C\" int32_t caml_rays_metal_function_find(MetalHandle *library_handle, const char *name_bytes, MetalHandle **out, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLLibrary> library = checked_object(library_handle, KIND_LIBRARY); NSString *name = name_bytes ? [NSString stringWithUTF8String:name_bytes] : nil; if (name == nil) return set_error(message, \"Metal function name is not valid UTF-8\"); id<MTLFunction> function = [library newFunctionWithName:name]; if (function == nil) return set_error(message, \"Metal library has no function with the requested name\"); if (function.device.registryID != library.device.registryID || ![function.name isEqualToString:name]) return set_error(message, \"Metal changed checked function lookup properties\"); *out = make_handle(function, KIND_FUNCTION); return *out ? 0 : set_error(message, \"unable to allocate Metal handle\"); } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }\nstruct RaysFunctionConstantValue { const char *name; int32_t tag; int64_t integral; double floating; };\nextern \"C\" int32_t caml_rays_metal_function_specialize(MetalHandle *library_handle, const char *name_bytes, const RaysFunctionConstantValue *constants, uintptr_t count, const char *label_bytes, MetalHandle **out, const char **message) { if (message) *message = nullptr; @try { @autoreleasepool { id<MTLLibrary> library = checked_object(library_handle, KIND_LIBRARY); NSString *name = name_bytes ? [NSString stringWithUTF8String:name_bytes] : nil; NSString *label = label_bytes ? [NSString stringWithUTF8String:label_bytes] : nil; if (name == nil || (label_bytes && label == nil)) return set_error(message, \"Metal function name or label is not valid UTF-8\"); MTLFunctionConstantValues *values = [[MTLFunctionConstantValues alloc] init]; NSMutableSet<NSString *> *names = [NSMutableSet set]; for (uintptr_t index = 0; index < count; ++index) { const RaysFunctionConstantValue &constant = constants[index]; NSString *constant_name = constant.name ? [NSString stringWithUTF8String:constant.name] : nil; if (constant_name == nil || [names containsObject:constant_name]) return set_error(message, \"function-constant names must be valid and unique\"); [names addObject:constant_name]; switch (constant.tag) { case 0: { const bool value = constant.integral != 0; [values setConstantValue:&value type:MTLDataTypeBool withName:constant_name]; break; } case 1: { const int8_t value = static_cast<int8_t>(constant.integral); [values setConstantValue:&value type:MTLDataTypeChar withName:constant_name]; break; } case 2: { const uint8_t value = static_cast<uint8_t>(constant.integral); [values setConstantValue:&value type:MTLDataTypeUChar withName:constant_name]; break; } case 3: { const int16_t value = static_cast<int16_t>(constant.integral); [values setConstantValue:&value type:MTLDataTypeShort withName:constant_name]; break; } case 4: { const uint16_t value = static_cast<uint16_t>(constant.integral); [values setConstantValue:&value type:MTLDataTypeUShort withName:constant_name]; break; } case 5: { const int32_t value = static_cast<int32_t>(constant.integral); [values setConstantValue:&value type:MTLDataTypeInt withName:constant_name]; break; } case 6: { const uint32_t value = static_cast<uint32_t>(constant.integral); [values setConstantValue:&value type:MTLDataTypeUInt withName:constant_name]; break; } case 7: { const int64_t value = constant.integral; [values setConstantValue:&value type:MTLDataTypeLong withName:constant_name]; break; } case 8: { const uint64_t value = static_cast<uint64_t>(constant.integral); [values setConstantValue:&value type:MTLDataTypeULong withName:constant_name]; break; } case 9: { const _Float16 value = static_cast<_Float16>(constant.floating); [values setConstantValue:&value type:MTLDataTypeHalf withName:constant_name]; break; } case 10: { const float value = static_cast<float>(constant.floating); [values setConstantValue:&value type:MTLDataTypeFloat withName:constant_name]; break; } default: return set_error(message, \"invalid function-constant type tag\"); } } NSError *error = nil; id<MTLFunction> function = [library newFunctionWithName:name constantValues:values error:&error]; if (function == nil) return set_error(message, error.localizedDescription.UTF8String ?: \"Metal function specialization failed without NSError\"); function.label = label; if (function.device.registryID != library.device.registryID || ![function.name isEqualToString:name] || ((label == nil) != (function.label == nil)) || (label && ![function.label isEqualToString:label])) return set_error(message, \"Metal changed checked function specialization properties\"); *out = make_handle(function, KIND_FUNCTION); return *out ? 0 : set_error(message, \"unable to allocate Metal handle\"); } } @catch (NSException *exception) { return set_error(message, exception.reason.UTF8String); } }\n");
    for record in records { out.push_str(&format!("static_assert(std::is_standard_layout_v<{}>);\n", record.sdk)); }
    for en in enums {
        for case in &en.cases {
            let sdk = (case.since.0 * 10000) + (case.since.1 * 100);
            out.push_str(&format!("#if __MAC_OS_X_VERSION_MAX_ALLOWED >= {sdk}\nstatic_assert(static_cast<unsigned long long>({}) == 0x{:016x}ULL);\n#endif\n", case.symbol, case.value as u64));
        }
    }
    for call in calls { out.push_str(&cpp_call(call, records)); }
    out
}
fn emit_kinds_header(kinds: &BTreeSet<String>) -> String {
    let mut out = String::from("#pragma once\n#include <cstdint>\n");
    for (index, value) in kinds.iter().enumerate() {
        out.push_str(&format!("static constexpr uint32_t KIND_{} = {};\n", kind(value), index + 1));
    }
    out
}
fn parse_handle_kinds(source: &str) -> BTreeSet<String> {
    let start = source.find("enum class Handle_kind").expect("Metal handle kind enum");
    let body_start = start + source[start..].find('{').expect("Metal handle kind opener") + 1;
    let end = body_start + source[body_start..].find("};").expect("Metal handle kind closer");
    source[body_start..end]
        .lines()
        .filter_map(|line| {
            let value = line.split("//").next()?.trim().trim_end_matches(',').trim();
            if value.is_empty() { return None; }
            let name = value.split('=').next()?.trim();
            name.chars().all(|character| character.is_ascii_alphanumeric() || character == '_')
                .then(|| name.to_owned())
        })
        .collect()
}

fn main() {
    let manifest = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap());
    let root = manifest.join("../../..");
    let registry_path = root.join("lib/metal/gen/registry.ml");
    println!("cargo:rerun-if-changed={}", registry_path.display());
    let source = fs::read_to_string(&registry_path).expect("read Metal registry");
    let (calls, enums, records, natives) = parse_registry(&source);
    let mut kinds = BTreeSet::new();
    let handle_kinds_path = root.join("lib/metal/metal_bridge.mm");
    println!("cargo:rerun-if-changed={}", handle_kinds_path.display());
    let handle_kinds_source = fs::read_to_string(&handle_kinds_path).expect("read Metal handle kinds");
    kinds.extend(parse_handle_kinds(&handle_kinds_source));
    for call in &calls {
        if call.receiver != "Class" { kinds.insert(call.receiver.clone()); }
        for ty in call.args.iter().chain(call.result.iter()) {
            match ty {
                Ty::Object(value) | Ty::OptionalObject(value) => { kinds.insert(value.clone()); }
                _ => {}
            }
        }
    }
    let output = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    fs::write(output.join("metal_registry.rs"), emit_rust(&calls, &enums, &records, &natives, &kinds)).unwrap();
    fs::write(output.join("metal_kinds.h"), emit_kinds_header(&kinds)).unwrap();
    if env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("macos") {
        let cpp = output.join("generated_metal.mm");
        fs::write(&cpp, emit_cpp(&calls, &enums, &records)).unwrap();
        let native_dir = manifest.join("src/native");
        println!("cargo:rerun-if-changed={}", native_dir.join("metal_handle.h").display());
        let mut build = cc::Build::new();
        build.cpp(true).include(&native_dir).include(&output).file(cpp)
            .flag("-std=c++17").flag("-fobjc-arc").flag("-fblocks").warnings(true);
        if let Ok(entries) = fs::read_dir(&native_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.extension().is_some_and(|extension| extension == "mm") {
                    println!("cargo:rerun-if-changed={}", path.display());
                    build.file(path);
                }
            }
        }
        build.compile("metal_generated");
        println!("cargo:rustc-link-lib=framework=Foundation");
        println!("cargo:rustc-link-lib=framework=Metal");
        println!("cargo:rustc-link-lib=framework=QuartzCore");
    }
}
