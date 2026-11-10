use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

#[derive(Clone, Debug)]
struct Token {
    text: String,
    line: usize,
}

fn advance_lines(bytes: &[u8], first: usize, last: usize, line: &mut usize) {
    *line += bytes[first..last].iter().filter(|&&byte| byte == b'\n').count();
}

fn raw_string_end(bytes: &[u8], start: usize) -> Option<usize> {
    let mut quote = start;
    if bytes.get(quote) == Some(&b'b') || bytes.get(quote) == Some(&b'c') {
        quote += 1;
    }
    if bytes.get(quote) != Some(&b'r') {
        return None;
    }
    quote += 1;
    while bytes.get(quote) == Some(&b'#') {
        quote += 1;
    }
    if bytes.get(quote) != Some(&b'"') {
        return None;
    }
    let hashes = quote - start - if bytes.get(start) == Some(&b'b') || bytes.get(start) == Some(&b'c') { 2 } else { 1 };
    let mut cursor = quote + 1;
    while cursor < bytes.len() {
        if bytes[cursor] == b'"'
            && bytes.get(cursor + 1..cursor + 1 + hashes).is_some_and(|ending| ending.iter().all(|&byte| byte == b'#'))
        {
            return Some(cursor + 1 + hashes);
        }
        cursor += 1;
    }
    Some(bytes.len())
}

fn lex(source: &str) -> Vec<Token> {
    let bytes = source.as_bytes();
    let mut tokens = Vec::new();
    let (mut i, mut line) = (0, 1);
    while i < bytes.len() {
        let byte = bytes[i];
        if byte.is_ascii_whitespace() {
            if byte == b'\n' {
                line += 1;
            }
            i += 1;
            continue;
        }
        if bytes.get(i..i + 2) == Some(&b"//"[..]) {
            while i < bytes.len() && bytes[i] != b'\n' {
                i += 1;
            }
            continue;
        }
        if bytes.get(i..i + 2) == Some(&b"/*"[..]) {
            let start = i;
            i += 2;
            let mut depth = 1usize;
            while i < bytes.len() && depth > 0 {
                match bytes.get(i..i + 2) {
                    Some(pair) if pair == &b"/*"[..] => {
                        depth += 1;
                        i += 2;
                    }
                    Some(pair) if pair == &b"*/"[..] => {
                        depth -= 1;
                        i += 2;
                    }
                    _ => i += 1,
                }
            }
            advance_lines(bytes, start, i, &mut line);
            continue;
        }
        if let Some(end) = raw_string_end(bytes, i) {
            advance_lines(bytes, i, end, &mut line);
            i = end;
            continue;
        }
        if byte == b'"' {
            let start = i;
            i += 1;
            while i < bytes.len() {
                if bytes[i] == b'\\' {
                    i = (i + 2).min(bytes.len());
                } else if bytes[i] == b'"' {
                    i += 1;
                    break;
                } else {
                    i += 1;
                }
            }
            advance_lines(bytes, start, i, &mut line);
            continue;
        }
        if byte == b'\'' {
            let mut end = i + 1;
            if end < bytes.len() && bytes[end] == b'\\' {
                end = (end + 2).min(bytes.len());
            } else if end < bytes.len() {
                end += if bytes[end].is_ascii() { 1 } else { source[end..].chars().next().map_or(1, char::len_utf8) };
            }
            if bytes.get(end) == Some(&b'\'') {
                end += 1;
                advance_lines(bytes, i, end, &mut line);
                i = end;
                continue;
            }
        }
        if byte.is_ascii_alphabetic() || byte == b'_' || byte >= 0x80 {
            let start = i;
            i += 1;
            while i < bytes.len()
                && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_' || bytes[i] >= 0x80)
            {
                i += 1;
            }
            tokens.push(Token { text: source[start..i].to_owned(), line });
            continue;
        }
        let start = i;
        let pair = bytes.get(i..i + 2);
        if pair == Some(&b"::"[..]) || pair == Some(&b"->"[..]) || pair == Some(&b"=>"[..]) {
            i += 2;
        } else {
            i += 1;
        }
        tokens.push(Token { text: source[start..i].to_owned(), line });
    }
    tokens
}

fn matching(tokens: &[Token], start: usize, open: &str, close: &str) -> Option<usize> {
    let mut depth = 0usize;
    for (index, token) in tokens.iter().enumerate().skip(start) {
        if token.text == open {
            depth += 1;
        } else if token.text == close {
            depth = depth.checked_sub(1)?;
            if depth == 0 {
                return Some(index);
            }
        }
    }
    None
}

fn generic_end(tokens: &[Token], start: usize) -> Option<usize> {
    let mut depth = 0usize;
    for (index, token) in tokens.iter().enumerate().skip(start) {
        match token.text.as_str() {
            "<" => depth += 1,
            ">" => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    return Some(index);
                }
            }
            ";" | "{" if depth == 1 => return None,
            _ => (),
        }
    }
    None
}

fn issue(issues: &mut Vec<String>, path: &str, line: usize, message: impl AsRef<str>) {
    issues.push(format!("{path}:{line}: {}", message.as_ref()));
}

fn shape_source(crate_name: &str, path: &str, source: &str) -> Vec<String> {
    let tokens = lex(source);
    // Whole test files (tests/, src/tests.rs, src/test_*.rs) count as one test region.
    let file = path.rsplit('/').next().unwrap_or(path);
    let test_file = path.contains("/tests/") || file == "tests.rs" || file.starts_with("test_");
    let test_ranges = if test_file { vec![(0, tokens.len())] } else { test_regions(&tokens) };
    // Only the value-returning worker-context helper may use this generic signature.
    // Keep the exception in its header; sibling and nested generic items still fail.
    let domain_header = if crate_name == "parallel" && matches!(path, "parallel/src/lib.rs" | "crates/parallel/src/lib.rs") {
        let signature = lex("fn with_domains<R>(domains: i64, work: impl FnOnce() -> R) -> R");
        tokens.iter().enumerate().find_map(|(index, token)| {
            if token.text != "fn" { return None; }
            let (body, _) = function_body(&tokens, index)?;
            tokens[index..body].iter().map(|token| &token.text).eq(signature.iter().map(|token| &token.text)).then_some((index, body))
        })
    } else { None };
    // Per-element kernel entry points take `impl Fn` so the body inlines into the loop (Z3);
    // allowlisted by path and name, exactly like with_domains.
    const KERNEL_ENTRY: &[(&str, &str)] = &[
        ("rdk_core/src/support.rs", "parallel_for"),
        ("rdk_core/src/support.rs", "par_fill"),
        ("rdk_core/src/group.rs", "init"),
        ("rdk_core/src/edge_group.rs", "init"),
        ("flow_sop/src/attribute_kernel.rs", "ranges"),
    ];
    let kernel_headers: Vec<(usize, usize)> = tokens.iter().enumerate().filter_map(|(index, token)| {
        if token.text != "fn" { return None; }
        let name = tokens.get(index + 1)?;
        KERNEL_ENTRY.iter().any(|(file, entry)| path.ends_with(file) && name.text == *entry)
            .then(|| function_body(&tokens, index).map(|(body, _)| (index, body)))?
    }).collect();
    let unsafe_allowed = crate_name.ends_with("_sys") || crate_name == "parallel";
    let mut issues = Vec::new();

    for (index, token) in tokens.iter().enumerate() {
        if token.text == "unsafe" && !unsafe_allowed {
            issue(&mut issues, path, token.line, "unsafe is restricted to *_sys crates and parallel");
        }
        if matches!(token.text.as_str(), "async" | "await") {
            issue(&mut issues, path, token.line, "async/await is not allowed in product code");
        }
        if token.text == "catch_unwind" && crate_name != "parallel" {
            issue(&mut issues, path, token.line, "catch_unwind is restricted to parallel worker isolation and the bin/rays crash reporter");
        }
        if token.text == "macro_rules"
            && tokens.get(index + 1).is_some_and(|next| next.text == "!")
        {
            issue(&mut issues, path, token.line, "macro_rules! is not allowed in product crates");
        }
        // unreachable! is OCaml's `assert false` (an impossible case, never control flow) and is allowed.
        if matches!(token.text.as_str(), "panic" | "todo" | "unimplemented")
            && tokens.get(index + 1).is_some_and(|next| next.text == "!")
            && !test_ranges.iter().any(|(start, end)| *start <= index && index <= *end)
        {
            issue(&mut issues, path, token.line, "panic macros must not be used for control flow");
        }
        if token.text == "impl" {
            let previous = index.checked_sub(1).and_then(|previous| tokens.get(previous)).map(|token| token.text.as_str());
            let lifetime_reference = index >= 3 && tokens[index - 2].text == "'" && tokens[index - 3].text == "&";
            if (matches!(previous, Some(":") | Some("->") | Some("=") | Some("<") | Some(",") | Some("(") | Some("&") | Some("mut") | Some(">"))
                || lifetime_reference)
                && !domain_header.is_some_and(|(start, body)| start <= index && index < body)
                && !kernel_headers.iter().any(|(start, body)| *start <= index && index < *body)
            {
                issue(&mut issues, path, token.line, "impl Trait is not allowed in product crates");
            }
        }
        if token.text == "proc_macro" || token.text == "proc_macro_derive" || token.text == "proc_macro_attribute" {
            issue(&mut issues, path, token.line, "procedural macros are not allowed in product crates");
        }
    }

    for index in 0..tokens.len() {
        let token = &tokens[index];
        let item_name = match token.text.as_str() {
            "fn" | "struct" | "enum" | "union" | "trait" | "type" => {
                tokens.get(index + 1).filter(|name| is_identifier(&name.text)).map(|_| index + 1)
            }
            "impl" => Some(index),
            _ => None,
        };
        if let Some(name_index) = item_name {
            let generic_index = if token.text == "impl" { index + 1 } else { name_index + 1 };
            if tokens.get(generic_index).is_some_and(|next| next.text == "<") {
                // Borrow lifetimes do not monomorphize functions or impls; only type parameters do.
                let borrow_lifetimes_only = matches!(token.text.as_str(), "fn" | "impl")
                    && generic_end(&tokens, generic_index).is_some_and(|end| {
                        let parameters = &tokens[generic_index + 1..end];
                        !parameters.is_empty() && parameters.iter().enumerate().all(|(at, parameter)| {
                            matches!(parameter.text.as_str(), "'" | ":" | "+" | ",")
                                || (at > 0 && parameters[at - 1].text == "'" && is_identifier(&parameter.text))
                        })
                    });
                if !borrow_lifetimes_only && !domain_header.is_some_and(|(start, _)| token.text == "fn" && start == index)
                    && !kernel_headers.iter().any(|(start, _)| token.text == "fn" && *start == index) {
                    issue(&mut issues, path, token.line, "generic items are not allowed in product crates");
                }
                if matches!(token.text.as_str(), "struct" | "enum") {
                    if let Some(end) = generic_end(&tokens, generic_index) {
                        if tokens[generic_index + 1..end].iter().any(|parameter| parameter.text == "'") {
                            issue(&mut issues, path, token.line, "struct and enum lifetime parameters are not allowed");
                        }
                    }
                }
            }
        }

        if token.text == "#" {
            let bracket = if tokens.get(index + 1).is_some_and(|next| next.text == "!") { index + 2 } else { index + 1 };
            if tokens.get(bracket).is_some_and(|next| next.text == "[") {
                if let Some(end) = matching(&tokens, bracket, "[", "]") {
                    let attribute = &tokens[bracket + 1..end];
                    if attribute.first().is_some_and(|part| part.text == "ignore")
                        || attribute.first().is_some_and(|part| part.text == "cfg_attr") && attribute.iter().any(|part| part.text == "ignore") {
                        issue(&mut issues, path, token.line, "ignored tests are forbidden; fix, shrink or delete the case");
                    }
                    for (offset, part) in attribute.iter().enumerate() {
                        if part.text == "derive" && attribute.get(offset + 1).is_some_and(|next| next.text == "(") {
                            if let Some(derive_end) = matching(attribute, offset + 1, "(", ")") {
                                for derive in &attribute[offset + 2..derive_end] {
                                    if is_identifier(&derive.text)
                                        && !matches!(derive.text.as_str(), "Clone" | "Copy" | "Debug" | "PartialEq" | "Eq" | "Hash" | "Default")
                                    {
                                        issue(&mut issues, path, derive.line, format!("derive `{}` is not allowed", derive.text));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    issues.extend(hashmap_output_issues(path, &tokens));
    issues
}

fn is_identifier(value: &str) -> bool {
    value.chars().next().is_some_and(|first| first.is_ascii_alphabetic() || first == '_' || !first.is_ascii())
}

fn hash_map_names(tokens: &[Token]) -> HashSet<String> {
    let mut names = HashSet::new();
    for (index, token) in tokens.iter().enumerate() {
        if token.text != "HashMap" {
            continue;
        }
        for cursor in (index.saturating_sub(12)..index).rev() {
            match tokens[cursor].text.as_str() {
                ";" | "{" | "}" | "," => break,
                ":" => {
                    if let Some(name) = cursor.checked_sub(1).and_then(|name| tokens.get(name)) {
                        if is_identifier(&name.text) {
                            names.insert(name.text.clone());
                        }
                    }
                    break;
                }
                "let" => {
                    let name_index = if cursor > 0 && tokens[cursor - 1].text == "mut" { cursor - 2 } else { cursor - 1 };
                    if let Some(name) = tokens.get(name_index) {
                        if is_identifier(&name.text) {
                            names.insert(name.text.clone());
                        }
                    }
                    break;
                }
                _ => (),
            }
        }
    }
    names
}

fn function_body(tokens: &[Token], start: usize) -> Option<(usize, usize)> {
    if !tokens.get(start + 1).is_some_and(|name| is_identifier(&name.text)) {
        return None;
    }
    let mut cursor = start + 2;
    while cursor < tokens.len() {
        match tokens[cursor].text.as_str() {
            ";" => return None,
            "{" => return matching(tokens, cursor, "{", "}").map(|end| (cursor, end)),
            "(" => cursor = matching(tokens, cursor, "(", ")")? + 1,
            "[" => cursor = matching(tokens, cursor, "[", "]")? + 1,
            _ => cursor += 1,
        }
    }
    None
}

fn test_regions(tokens: &[Token]) -> Vec<(usize, usize)> {
    let mut ranges = Vec::new();
    for (index, token) in tokens.iter().enumerate() {
        if token.text != "#" || !tokens.get(index + 1).is_some_and(|next| next.text == "[") {
            continue;
        }
        let Some(attribute_end) = matching(tokens, index + 1, "[", "]") else { continue };
        let attribute = &tokens[index + 2..attribute_end];
        let test_fn = attribute.len() == 1 && attribute[0].text == "test";
        let test_module = attribute.iter().any(|part| part.text == "cfg")
            && attribute.iter().any(|part| part.text == "test")
            && !attribute.iter().any(|part| part.text == "not");
        if !test_fn && !test_module {
            continue;
        }
        let item_start = attribute_end + 1;
        if test_fn {
            if let Some(function) = (item_start..tokens.len().min(item_start + 12)).find(|&cursor| tokens[cursor].text == "fn") {
                if let Some((start, end)) = function_body(tokens, function) {
                    ranges.push((start, end));
                }
            }
        } else if let Some(module) = (item_start..tokens.len().min(item_start + 12)).find(|&cursor| tokens[cursor].text == "mod") {
            if let Some(open) = (module + 1..tokens.len().min(module + 12)).find(|&cursor| tokens[cursor].text == "{") {
                if let Some(end) = matching(tokens, open, "{", "}") {
                    ranges.push((open, end));
                }
            }
        }
    }
    ranges
}


fn emits_text(name: &str, signature_and_body: &[Token]) -> bool {
    let lower = name.to_ascii_lowercase();
    if ["emit", "print", "write", "serialize", "encode", "format", "to_string", "display", "render"]
        .iter().any(|word| lower.contains(word))
    {
        return true;
    }
    signature_and_body.windows(2).any(|pair| {
        matches!(pair[0].text.as_str(), "format" | "write" | "writeln" | "print" | "println") && pair[1].text == "!"
    }) || signature_and_body.windows(2).any(|pair| pair[0].text == "String" && pair[1].text == "::")
        || signature_and_body.iter().any(|token| {
            matches!(token.text.as_str(), "push_str" | "write_str" | "write_fmt" | "to_string")
        })
}

// ponytail: output detection is token-based; switch to syntax-aware provenance if new emission idioms appear.
fn hashmap_output_issues(path: &str, tokens: &[Token]) -> Vec<String> {
    let names = hash_map_names(tokens);
    if names.is_empty() {
        return Vec::new();
    }
    let mut issues = Vec::new();
    for (index, token) in tokens.iter().enumerate() {
        if token.text != "fn" {
            continue;
        }
        let Some(name) = tokens.get(index + 1).map(|name| name.text.as_str()) else { continue };
        let Some((body_start, body_end)) = function_body(tokens, index) else { continue };
        let signature = &tokens[index..=body_end];
        let output = emits_text(name, signature);
        if !output {
            continue;
        }
        let iterates_map = (body_start..body_end).any(|cursor| {
            if matches!(tokens[cursor].text.as_str(), "iter" | "keys" | "values" | "into_iter" | "into_keys" | "into_values" | "drain")
                && tokens.get(cursor + 1).is_some_and(|next| next.text == "(")
                && cursor >= 2
                && tokens[cursor - 1].text == "."
            {
                let receiver = &tokens[cursor - 2].text;
                if names.contains(receiver) {
                    return true;
                }
                if cursor >= 4 && tokens[cursor - 3].text == "." && tokens[cursor - 4].text == "self" {
                    return names.contains(receiver);
                }
            }
            if tokens[cursor].text == "in" {
                let mut value = cursor + 1;
                while value < body_end && matches!(tokens[value].text.as_str(), "&" | "*" | "(") {
                    value += 1;
                }
                if value < body_end && names.contains(&tokens[value].text) {
                    return true;
                }
            }
            false
        });
        if iterates_map {
            issue(&mut issues, path, token.line, "HashMap iteration must not determine emitted output");
        }
    }
    issues
}

fn rust_sources(dir: &Path, files: &mut Vec<PathBuf>) -> std::io::Result<()> {
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            let name = entry.file_name();
            if matches!(name.to_str(), Some("target" | "_build" | ".git")) {
                continue;
            }
            rust_sources(&path, files)?;
        } else if path.extension().is_some_and(|extension| extension == "rs") {
            files.push(path);
        }
    }
    Ok(())
}

fn lib_unit_tests_disabled(workspace: &Path) -> Result<std::collections::HashSet<String>, String> {
    let mut manifests = Vec::new();
    cargo_manifests(&workspace.join("crates"), &mut manifests)?;
    let mut disabled = std::collections::HashSet::new();
    for manifest in manifests {
        let parsed = parse_manifest(&manifest)?;
        let Some(package_name) = parsed.package_name.as_deref() else { continue };
        if !parsed.lib_unit_tests {
            disabled.insert(crate_name(package_name, parsed.lib_name.as_deref()));
        }
    }
    Ok(disabled)
}

fn shape(workspace: &Path) -> Result<Vec<String>, String> {
    let crates = workspace.join("crates");
    let disabled_lib_tests = lib_unit_tests_disabled(workspace)?;
    let mut files = Vec::new();
    rust_sources(&crates, &mut files).map_err(|error| format!("cannot scan {}: {error}", crates.display()))?;
    files.sort();
    let mut issues = Vec::new();
    for file in files {
        let relative = file.strip_prefix(workspace).unwrap_or(&file).to_string_lossy().replace('\\', "/");
        let crate_name = file
            .strip_prefix(&crates)
            .ok()
            .and_then(|relative| relative.components().next())
            .map(|component| component.as_os_str().to_string_lossy().into_owned())
            .unwrap_or_default();
        let source = fs::read_to_string(&file).map_err(|error| format!("cannot read {}: {error}", file.display()))?;
        if disabled_lib_tests.contains(&crate_name) {
            let file_name = file.file_name().and_then(|name| name.to_str()).unwrap_or("");
            let in_lib_src = relative.starts_with(&format!("crates/{crate_name}/src/"));
            if in_lib_src && !file_name.starts_with("test_") && source.contains("#[test]") {
                issues.push(format!(
                    "{relative}: unit tests in library sources require `[lib] test` to stay enabled"
                ));
            }
        }
        issues.extend(shape_source(&crate_name, &relative, &source));
    }
    issues.sort();
    apply_shape_baseline(workspace, issues)
}

fn apply_shape_baseline(workspace: &Path, issues: Vec<String>) -> Result<Vec<String>, String> {
    // Explicit legacy exceptions are pinned to the offending source, never a whole crate or rule.
    // A multiset keeps another copy of an existing exception from silently passing.
    let mut allowed = HashMap::<(String, String, String), usize>::new();
    for row in include_str!("../shape-allowlist.tsv").lines().filter(|row| !row.starts_with('#') && !row.is_empty()) {
        let columns: Vec<_> = row.split('\t').collect();
        if columns.len() != 4 || columns[3].trim().is_empty() { return Err("invalid shape allowlist entry; each exception needs a reason".into()); }
        *allowed.entry((columns[0].into(), columns[1].into(), columns[2].into())).or_default() += 1;
    }
    let mut remaining = Vec::new();
    for issue in issues {
        let admitted = issue.split_once(": ").and_then(|(location, message)| {
            let (path, line) = location.rsplit_once(':')?;
            let line = line.parse::<usize>().ok()?;
            let source = fs::read_to_string(workspace.join(path)).ok()?;
            let snippet = source.lines().nth(line.checked_sub(1)?)?.split_whitespace().collect::<Vec<_>>().join(" ");
            let count = allowed.get_mut(&(path.into(), message.into(), snippet))?;
            if *count == 0 { return None; }
            *count -= 1;
            Some(())
        }).is_some();
        if !admitted { remaining.push(issue); }
    }
    Ok(remaining)
}

#[derive(Clone, Debug, Default)]
struct DepSpec {
    alias: String,
    package: Option<String>,
    path: Option<String>,
    workspace: bool,
    optional: bool,
    kind: String,
    target: Option<String>,
    line: usize,
}

#[derive(Clone, Debug)]
struct Package {
    manifest: PathBuf,
    package_name: String,
    crate_name: String,
    product: bool,
    deps: Vec<DepSpec>,
    has_binary: bool,
    has_gpu_dev_targets: bool,
}

#[derive(Default)]
struct ParsedManifest {
    disable_auto_tests: bool,
    package_name: Option<String>,
    lib_name: Option<String>,
    lib_unit_tests: bool,
    has_binary: bool,
    has_gpu_dev_targets: bool,
    binary_names: Vec<String>,
    deps: Vec<DepSpec>,
    workspace_deps: Vec<DepSpec>,
}

fn without_toml_comment(line: &str) -> &str {
    let (mut quote, mut escaped) = (None, false);
    for (index, character) in line.char_indices() {
        if let Some(active) = quote {
            if active == '"' && character == '\\' && !escaped {
                escaped = true;
                continue;
            }
            if character == active && !escaped {
                quote = None;
            }
            escaped = false;
        } else if character == '"' || character == '\'' {
            quote = Some(character);
        } else if character == '#' {
            return &line[..index];
        }
    }
    line
}

fn split_toml_path(section: &str) -> Vec<String> {
    let (mut parts, mut start, mut quote) = (Vec::new(), 0, None);
    for (index, character) in section.char_indices() {
        if let Some(active) = quote {
            if character == active {
                quote = None;
            }
        } else if character == '"' || character == '\'' {
            quote = Some(character);
        } else if character == '.' {
            parts.push(section[start..index].trim().trim_matches(|character| character == '"' || character == '\'').to_owned());
            start = index + 1;
        }
    }
    parts.push(section[start..].trim().trim_matches(|character| character == '"' || character == '\'').to_owned());
    parts
}

fn toml_string(value: &str) -> Option<String> {
    let value = value.trim();
    if value.len() >= 2 && ((value.starts_with('"') && value.ends_with('"')) || (value.starts_with('\'') && value.ends_with('\''))) {
        Some(value[1..value.len() - 1].to_owned())
    } else {
        None
    }
}

fn assignment_fields(value: &str) -> HashMap<String, String> {
    let mut fields = HashMap::new();
    let value = value.trim().strip_prefix('{').and_then(|value| value.strip_suffix('}')).unwrap_or(value);
    let (mut start, mut depth, mut quote, mut escaped) = (0, 0isize, None, false);
    let mut pieces = Vec::new();
    for (index, character) in value.char_indices() {
        if let Some(active) = quote {
            if active == '"' && character == '\\' && !escaped {
                escaped = true;
                continue;
            }
            if character == active && !escaped {
                quote = None;
            }
            escaped = false;
        } else if character == '"' || character == '\'' {
            quote = Some(character);
        } else if character == '{' || character == '[' {
            depth += 1;
        } else if character == '}' || character == ']' {
            depth -= 1;
        } else if character == ',' && depth == 0 {
            pieces.push(&value[start..index]);
            start = index + 1;
        }
    }
    pieces.push(&value[start..]);
    for piece in pieces {
        if let Some((key, value)) = piece.split_once('=') {
            fields.insert(key.trim().trim_matches(|character| character == '"' || character == '\'').to_owned(), value.trim().to_owned());
        }
    }
    fields
}

fn toml_value_complete(value: &str) -> bool {
    let mut depth = 0isize;
    let (mut quote, mut escaped) = (None, false);
    for character in value.chars() {
        if let Some(active) = quote {
            if active == '"' && character == '\\' && !escaped {
                escaped = true;
                continue;
            }
            if character == active && !escaped {
                quote = None;
            }
            escaped = false;
        } else if character == '"' || character == '\'' {
            quote = Some(character);
        } else if character == '{' || character == '[' {
            depth += 1;
        } else if character == '}' || character == ']' {
            depth -= 1;
        }
    }
    depth <= 0 && quote.is_none()
}

fn dependency(section: &[String], key: Option<&str>, value: Option<&str>, line: usize, workspace: bool) -> Option<DepSpec> {
    let dependency_sections = ["dependencies", "dev-dependencies", "build-dependencies"];
    let dep_index = section.iter().position(|part| dependency_sections.contains(&part.as_str()));
    let workspace_index = section.windows(2).position(|parts| parts[0] == "workspace" && parts[1] == "dependencies");
    let (alias, kind) = if let Some(dep_index) = dep_index {
        let alias = if let Some(key) = key {
            if section.len() != dep_index + 1 {
                return None;
            }
            key.to_owned()
        } else {
            section.get(dep_index + 1)?.clone()
        };
        (alias, section[dep_index].clone())
    } else if let Some(workspace_index) = workspace_index {
        let alias = if let Some(key) = key {
            if section.len() != workspace_index + 2 {
                return None;
            }
            key.to_owned()
        } else {
            section.get(workspace_index + 2)?.clone()
        };
        (alias, "workspace".to_owned())
    } else {
        return None;
    };
    let fields = value.map(assignment_fields).unwrap_or_default();
    Some(DepSpec {
        alias,
        package: fields.get("package").and_then(|field| toml_string(field)),
        path: fields.get("path").and_then(|field| toml_string(field)),
        workspace: workspace || fields.get("workspace").is_some_and(|field| field == "true"),
        optional: fields.get("optional").is_some_and(|field| field == "true"),
        kind,
        target: section.first().is_some_and(|part| part == "target").then(|| section[1].clone()),
        line,
    })
}

fn parse_manifest(path: &Path) -> Result<ParsedManifest, String> {
    let source = fs::read_to_string(path).map_err(|error| format!("cannot read {}: {error}", path.display()))?;
    let mut sections: BTreeMap<Vec<String>, HashMap<String, (String, usize)>> = BTreeMap::new();
    let (mut has_binary, mut has_gpu_dev_targets) = (false, false);
    let mut binary_names = Vec::new();
    let (mut section, mut pending, mut pending_line) = (Vec::new(), None::<(String, String)>, 0usize);
    for (zero_line, raw_line) in source.lines().enumerate() {
        let line_number = zero_line + 1;
        let line = without_toml_comment(raw_line).trim();
        if line.is_empty() {
            continue;
        }
        if let Some((key, value)) = pending.as_mut() {
            value.push(' ');
            value.push_str(line);
            if toml_value_complete(value) {
                sections.entry(section.clone()).or_default().insert(key.clone(), (value.clone(), pending_line));
                pending = None;
            }
            continue;
        }
        if line.starts_with("[[") && line.ends_with("]]") {
            section = split_toml_path(&line[2..line.len() - 2]);
            has_binary |= section.first().is_some_and(|part| part == "bin");
            has_gpu_dev_targets |= section.first().is_some_and(|part| ["example", "bench"].contains(&part.as_str()));
            continue;
        }
        if line.starts_with('[') && line.ends_with(']') {
            section = split_toml_path(&line[1..line.len() - 1]);
            continue;
        }
        if let Some((key, value)) = line.split_once('=') {
            let key = key.trim().trim_matches(|character| character == '"' || character == '\'').to_owned();
            let value = value.trim().to_owned();
            if key == "name" {
                if let Some(name) = toml_string(&value) {
                    if section == ["test"] {
                        has_gpu_dev_targets |= name.starts_with("gpu_") || name.starts_with("window_");
                    } else if section == ["bin"] {
                        binary_names.push(name);
                    }
                }
            }
            if toml_value_complete(&value) {
                sections.entry(section.clone()).or_default().insert(key, (value, line_number));
            } else {
                pending = Some((key, value));
                pending_line = line_number;
            }
        }
    }

    let field = |table: &[&str], name: &str| -> Option<String> {
        let key = table.iter().map(|part| (*part).to_owned()).collect::<Vec<_>>();
        sections.get(&key)?.get(name).and_then(|(value, _)| toml_string(value))
    };
    let lib_unit_tests = sections
        .get(&vec!["lib".into()])
        .and_then(|entries| entries.get("test"))
        .is_none_or(|(value, _)| value != "false");
    let mut parsed = ParsedManifest {
        disable_auto_tests: sections.get(&vec!["package".into()]).and_then(|entries| entries.get("autotests")).is_some_and(|(value, _)| value == "false"),
        package_name: field(&["package"], "name"),
        lib_name: field(&["lib"], "name"),
        lib_unit_tests,
        has_binary,
        has_gpu_dev_targets,
        binary_names,
        ..ParsedManifest::default()
    };
    for (section, entries) in &sections {
        if section.len() >= 2 && section[0] == "workspace" && section[1] == "dependencies" {
            if section.len() == 2 {
                for (alias, (value, line)) in entries {
                    if let Some(dependency) = dependency(section, Some(alias), Some(value), *line, false) {
                        parsed.workspace_deps.push(dependency);
                    }
                }
            } else if let Some(mut dependency) = dependency(section, None, Some("{}"), entries.values().map(|(_, line)| *line).min().unwrap_or(1), false) {
                let fields: HashMap<String, String> = entries.iter().map(|(key, (value, _))| (key.clone(), value.clone())).collect();
                dependency.package = fields.get("package").and_then(|field| toml_string(field));
                dependency.path = fields.get("path").and_then(|field| toml_string(field));
                dependency.optional = fields.get("optional").is_some_and(|field| field == "true");
                parsed.workspace_deps.push(dependency);
            }
            continue;
        }
        if section.iter().any(|part| ["dependencies", "dev-dependencies", "build-dependencies"].contains(&part.as_str())) {
            if section.last().is_some_and(|part| ["dependencies", "dev-dependencies", "build-dependencies"].contains(&part.as_str())) {
                for (alias, (value, line)) in entries {
                    if let Some(dependency) = dependency(section, Some(alias), Some(value), *line, false) {
                        parsed.deps.push(dependency);
                    }
                }
            } else if let Some(mut dependency) = dependency(section, None, Some("{}"), entries.values().map(|(_, line)| *line).min().unwrap_or(1), false) {
                let fields: HashMap<String, String> = entries.iter().map(|(key, (value, _))| (key.clone(), value.clone())).collect();
                dependency.package = fields.get("package").and_then(|field| toml_string(field));
                dependency.path = fields.get("path").and_then(|field| toml_string(field));
                dependency.workspace = fields.get("workspace").is_some_and(|field| field == "true");
                dependency.optional = fields.get("optional").is_some_and(|field| field == "true");
                parsed.deps.push(dependency);
            }
        }
    }
    Ok(parsed)
}

fn cargo_manifests(dir: &Path, files: &mut Vec<PathBuf>) -> Result<(), String> {
    if !dir.exists() {
        return Ok(());
    }
    for entry in fs::read_dir(dir).map_err(|error| format!("cannot scan {}: {error}", dir.display()))? {
        let entry = entry.map_err(|error| format!("cannot scan {}: {error}", dir.display()))?;
        let path = entry.path().join("Cargo.toml");
        if path.is_file() {
            files.push(path);
        }
    }
    Ok(())
}

fn package_targets(dir: &Path, manifest: &ParsedManifest) -> Result<(bool, bool), String> {
    let mut files = Vec::new();
    rust_sources(dir, &mut files).map_err(|error| format!("cannot scan {}: {error}", dir.display()))?;
    // print_shader only writes generated source/SPIR-V; ogpu_core supplies shader metadata.
    // Any additional executable in this crate still needs the ordinary startup check.
    let shader_printer = manifest.package_name.as_deref() == Some("flow_shader")
        && manifest.binary_names == ["print_shader"];
    let mut has_binary = (manifest.has_binary && !shader_printer) || dir.join("src/main.rs").is_file();
    let mut has_gpu_dev_targets = manifest.has_gpu_dev_targets;
    for file in files {
        let relative = file.strip_prefix(dir).unwrap_or(&file);
        let mut components = relative.components();
        let first = components.next().and_then(|part| part.as_os_str().to_str());
        let second = components.next().and_then(|part| part.as_os_str().to_str());
        has_binary |= first == Some("src") && second == Some("bin");
        // Manifest regeneration evaluates Lisp and writes a text fixture; it never starts a GPU.
        let manifest_writer = manifest.package_name.as_deref() == Some("web_export")
            && relative == Path::new("examples/refresh_manifest.rs");
        has_gpu_dev_targets |= matches!(first, Some("examples" | "benches")) && !manifest_writer;
        // Same native target convention as rays-t; ordinary unit/e2e tests may use GPU types
        // without creating a device. Explicit targets are recorded while parsing the manifest.
        has_gpu_dev_targets |= !manifest.disable_auto_tests && first == Some("tests") && components.next().is_none()
            && file.file_stem().and_then(|name| name.to_str())
                .is_some_and(|name| name.starts_with("gpu_") || name.starts_with("window_"));
    }
    Ok((has_binary, has_gpu_dev_targets))
}

fn crate_name(package_name: &str, lib_name: Option<&str>) -> String {
    lib_name.unwrap_or(package_name).replace('-', "_")
}

/// Rust-only helper leaves that stand in for OCaml stdlib, printf and int semantics: any product crate may name them.
const HELPER_LEAVES: [&str; 1] = ["cfmt"];
/// OCaml leaves a crate may name directly when its allowed dependencies reach them (flow reaches
/// rays_diagnostic and input; lru reaches mem).
// param: OCaml flow depends on it, so anything reaching flow reaches param.
const OCAML_LEAVES: [&str; 4] = ["rays_diagnostic", "input", "mem", "param"];

/// Direct production edges in the Rust architecture. Sys crates own their native ABI bridges.
fn direct_allowlist() -> HashMap<&'static str, &'static [&'static str]> {
    HashMap::from([
        ("cfmt", &[][..]),
        ("rays_diagnostic", &[][..]),
        ("native_layer_token_sys", &[][..]),
        ("mem_sys", &[][..]),
        ("mem", &["mem_sys"][..]),
        ("lru", &["mem"][..]),
        ("param", &["cfmt"][..]),
        ("input", &[][..]),
        ("rays_math_sys", &[][..]),
        ("rays_math", &["rays_math_sys"][..]),
        ("scene_command", &["rays_math"][..]),
        ("parallel", &[][..]),
        ("sdl3_sys", &["native_layer_token_sys"][..]),
        ("metal_sys", &["ogpu_core"][..]),
        ("vulkan_sys", &["ogpu_core"][..]),
        ("rdk_core", &["mem", "parallel", "rays_math"][..]),
        ("rdk_geom", &["rdk_core", "rays_math", "parallel"][..]),
        ("rdk_ops", &["rdk_core", "rdk_geom", "rays_math", "parallel"][..]),
        ("rdk_boolean", &["rdk_core", "rdk_geom", "rays_math", "parallel"][..]),
        ("rdk_rays", &["rdk_core", "rdk_geom", "rdk_ops", "rays", "rays_math", "param"][..]),
        ("scene_core", &["rays_math", "lru", "parallel"][..]),
        ("rays", &[
            "input", "rays_math", "lru", "runtime_resources", "runtime", "scene_command", "renderer",
            "parallel", "ogpu_core", "scene_core",
        ][..]),
        ("runtime", &["sdl3", "ogpu_core", "input"][..]),
        ("runtime_resources", &["sdl3", "ogpu_core"][..]),
        ("renderer", &["ogpu_core", "scene_command", "runtime", "lru", "rays_math", "runtime_resources", "scene_core"][..]),
        ("flow_syntax", &["param", "rays_diagnostic", "input", "rays_math"][..]),
        ("flow_check", &["flow_syntax", "param", "rays_diagnostic", "input", "rays_math", "lru"][..]),
        ("flow_eval", &["flow_syntax", "flow_check", "param", "rays_diagnostic", "input", "rays_math", "mem_sys"][..]),
        ("flow_graph", &["flow_syntax", "flow_check", "flow_eval", "param"][..]),
        ("flow_ir", &[
            "flow_syntax", "flow_check", "flow_eval", "param", "rays_math", "scene_command", "lru", "parallel",
        ][..]),
        ("flow_shader", &["flow_syntax", "flow_check", "flow_eval", "flow_ir", "ogpu_core", "rays_math"][..]),
        ("flow_gpu", &[
            "flow_syntax", "flow_check", "flow_eval", "flow_ir", "flow_shader", "param", "rays_math", "ogpu_core",
            "renderer", "lru", "mem", "parallel",
        ][..]),
        ("flow_web", &[
            "flow_syntax", "flow_check", "flow_eval", "flow_ir", "param", "rays_math", "scene_command", "parallel",
        ][..]),
        ("sop", &["param", "input", "rays_diagnostic", "rays_math", "rdk_core", "rdk_geom", "rdk_ops", "rdk_boolean", "lru", "mem", "parallel"][..]),
        ("flow_sop", &[
            "flow_syntax", "flow_check", "flow_eval", "flow_ir", "lru", "param", "sop", "rays_diagnostic", "rays_math",
            "parallel", "rdk_core", "rdk_geom", "rdk_ops", "flow_sop_catalog",
        ][..]),
        ("flow_sop_catalog", &["flow_syntax", "flow_check", "flow_eval", "flow_ir", "param", "sop", "rays_math"][..]),
        ("flow_native", &[
            "editor_document", "rays", "rays_math", "input", "flow_syntax", "flow_check", "flow_eval",
            "flow_graph", "flow_ir", "flow_gpu", "flow_sop", "scene_command", "renderer", "lru", "runtime_resources",
            "parallel", "sop", "flow_shader", "ogpu_core", "rdk_core",
        ][..]),
        ("ogpu_core", &["native_layer_token_sys"][..]),
        ("ogpu_mock", &["ogpu_core"][..]),
        // Rust merges OCaml's ogpu_metal_native into ogpu_metal.
        ("ogpu_metal", &["ogpu_core", "metal_sys", "native_layer_token_sys"][..]),
        ("ogpu_vulkan", &["ogpu_core", "vulkan_sys", "native_layer_token_sys", "rays_math"][..]),
        // PXUI is headed for its own repository: rays_math and scene_command leave in P2 with the draw list.
        ("pxui", &["input", "cfmt", "scene_command"][..]),
        ("pxui_text", &[][..]),
        // The UI painter: only the portable GPU API, so it moves to the PXUI repository unchanged.
        ("pxui_render", &["ogpu_core"][..]),
        ("rays_shell", &["rays", "rays_math", "editor_core", "pxui", "mem"][..]),
        ("rays_graph", &[
            "rays", "rays_math", "flow_syntax", "flow_check", "flow_eval", "flow_graph", "pxui", "editor_core",
        ][..]),
        ("editor_core", &["param", "rays", "rays_math", "flow_syntax", "flow_check", "flow_eval", "runtime_resources"][..]),
        ("web_export", &[
            "flow_syntax", "flow_check", "flow_eval", "flow_ir", "flow_web", "flow_sop", "editor_document", "input",
            "param", "scene_command", "flow_shader", "rays_math", "sop",
        ][..]),
        ("rays_pathtracer", &[
            "rays", "ogpu_core", "renderer", "flow_syntax", "flow_check", "flow_eval", "flow_ir", "flow_gpu", "input",
            "runtime_resources", "rays_math", "flow_shader",
        ][..]),
        ("rays_bin", &["editor_document", "rays_editor", "web_export", "flow_syntax", "flow_check", "flow_eval", "flow_sop", "sop", "runtime", "sdl3", "ogpu_core", "ogpu_metal", "ogpu_vulkan"][..]),
    ])
}

fn local_workspace_only() -> HashSet<&'static str> {
    HashSet::from([
        "sdl3", "editor_document", "rays_editor",
    ])
}

/// OCaml leaves that `start` reaches through its allowed dependencies (transitively, over the policy).
fn ocaml_leaves_reached(start: &str, allowlist: &HashMap<&'static str, &'static [&'static str]>) -> HashSet<&'static str> {
    let mut reached = HashSet::new();
    let mut seen = HashSet::new();
    let mut pending: Vec<&'static str> = allowlist.get(start).map(|allowed| allowed.to_vec()).unwrap_or_default();
    while let Some(name) = pending.pop() {
        if !seen.insert(name) {
            continue;
        }
        if OCAML_LEAVES.contains(&name) {
            reached.insert(name);
        }
        if let Some(next) = allowlist.get(name) {
            pending.extend(next.iter().copied());
        }
    }
    reached
}

fn forbidden_reaches() -> HashMap<String, HashSet<String>> {
    let mut rules: HashMap<String, HashSet<String>> = HashMap::new();
    let upper = [
        "param", "flow_ir", "flow_web", "flow_shader", "flow_gpu", "flow_graph", "flow_sop",
        "flow_native", "rays", "editor_core", "editor_document", "pxui", "rays_shell", "rays_graph",
        "sop", "rdk_core", "rdk_geom", "rdk_ops", "rdk_boolean", "rdk_rays", "rays_pathtracer",
        "web_export", "rays_editor", "rays_bin", "flow_sop_catalog",
    ];
    let gpu = [
        "sdl3", "metal_sys", "vulkan_sys", "ogpu_core",
        "ogpu_mock", "ogpu_metal", "ogpu_vulkan", "runtime",
        "runtime_resources", "renderer",
    ];
    let mut upper_no_gpu = upper.to_vec();
    upper_no_gpu.extend(["runtime", "runtime_resources", "renderer"]);
    let foundational = [
        "cfmt",
        "rays_diagnostic", "native_layer_token_sys", "mem", "mem_sys",
        "lru", "param", "input", "rays_math", "rays_math_sys", "parallel", "scene_command", "sdl3",
        "metal_sys", "vulkan_sys", "ogpu_core",
        "rdk_core",
    ];
    for library in foundational {
        rules.entry(library.to_owned()).or_default().extend(upper_no_gpu.iter().map(|name| (*name).to_owned()));
    }
    let mut forbid = |library: &str, names: &[&str]| {
        rules.entry(library.to_owned()).or_default().extend(names.iter().map(|name| (*name).to_owned()));
    };
    for library in ["runtime", "renderer", "rays"] {
        forbid(library, &["metal_sys", "ogpu_metal", "vulkan_sys", "ogpu_vulkan"]);
    }
    for library in ["ogpu_metal", "vulkan_sys", "ogpu_vulkan"] {
        forbid(library, &["sdl3", "runtime", "rays", "renderer"]);
    }
    forbid("runtime", &upper);
    forbid("flow_native", &["pxui", "rays_shell", "rays_graph", "rays_editor"]);
    forbid("flow_ir", &[
        "rays", "pxui", "rays_shell", "rays_graph", "sop", "rdk_core", "rdk_geom", "rdk_ops",
        "rdk_boolean", "rdk_rays", "rays_pathtracer", "rays_editor",
    ]);
    forbid("flow_shader", &[
        "runtime", "renderer", "ogpu_mock", "ogpu_metal", "ogpu_vulkan",
        "rays", "rdk_core", "rdk_geom", "rdk_ops", "rdk_boolean", "rdk_rays", "rays_pathtracer", "flow_sop",
        "sop", "editor_core", "pxui", "rays_shell", "rays_graph", "rays_editor",
    ]);
    forbid("flow_gpu", &[
        "sop", "rdk_core", "rdk_geom", "rdk_ops", "rdk_boolean", "rdk_rays", "rays_pathtracer", "flow_sop",
        "editor_core", "pxui", "rays_shell", "rays_graph", "rays_editor",
    ]);
    forbid("flow_web", &[
        "rays", "sop", "rdk_core", "rdk_geom", "rdk_ops", "rdk_boolean", "rdk_rays", "rays_pathtracer",
        "flow_sop", "flow_gpu", "editor_core", "pxui", "rays_shell", "rays_graph", "rays_editor",
    ]);
    forbid("flow_web", &gpu);
    // G6: the web target needs no SDL, runtime or renderer, directly or transitively.
    forbid("web_export", &["pxui", "rays_shell", "rays_graph", "rays_editor", "rays", "runtime", "runtime_resources", "renderer", "sdl3"]);
    forbid("rays", &[
        "pxui", "rays_shell", "rays_graph", "sop", "rdk_core", "rdk_geom", "rdk_ops", "rdk_boolean",
        "rdk_rays", "rays_pathtracer", "rays_editor",
    ]);
    forbid("rays_math", &["rdk_core", "rdk_geom", "rdk_ops", "rdk_boolean", "rdk_rays", "sop"]);
    forbid("rays_math", &gpu);
    forbid("rdk_core", &["rdk_geom", "rdk_ops", "rdk_boolean", "rays", "rdk_rays", "sop"]);
    forbid("rdk_core", &gpu);
    forbid("rdk_geom", &["rdk_ops", "rdk_boolean", "rays", "rdk_rays", "sop"]);
    forbid("rdk_geom", &gpu);
    forbid("rdk_ops", &["rdk_boolean", "rays", "rdk_rays", "sop"]);
    forbid("rdk_ops", &gpu);
    forbid("rdk_boolean", &["rdk_ops", "rays", "rdk_rays", "sop"]);
    forbid("rdk_boolean", &gpu);
    forbid("sop", &["rays", "rdk_rays", "pxui", "rays_shell", "rays_graph", "rays_editor"]);
    forbid("sop", &gpu);
    forbid("editor_core", &["pxui", "rays_shell", "rays_graph", "rays_editor", "sop"]);
    forbid("editor_document", &["pxui", "rays_shell", "rays_graph", "rays_editor"]);
    forbid("pxui", &["rays", "param", "mem", "lru", "runtime", "runtime_resources", "renderer", "sdl3", "editor_core", "rays_shell", "sop", "rays_graph", "rays_editor"]);
    forbid("rays_shell", &["sop", "rays_graph", "rays_editor"]);
    forbid("rays_graph", &["sop", "rays_shell", "rays_editor"]);
    forbid("pxui_render", &upper_no_gpu);
    forbid("pxui_render", &["scene_command", "scene_core", "rays_math", "mem", "mem_sys", "lru", "input", "sdl3", "metal_sys", "vulkan_sys", "ogpu_metal", "ogpu_vulkan", "ogpu_mock"]);
    forbid("rays_pathtracer", &[
        "metal_sys", "ogpu_metal", "ogpu_mock", "rdk_core", "sop", "flow_sop", "editor_core",
        "editor_document", "pxui", "rays_shell", "rays_graph", "rays_editor",
    ]);
    forbid("rdk_rays", &[
        "metal_sys", "ogpu_metal", "rdk_boolean", "sop", "editor_core",
        "editor_document", "pxui", "rays_shell", "rays_graph", "rays_pathtracer", "rays_editor",
    ]);
    // Flow is UI-free and GPU-free, but its three crates may reach the OCaml flow's leaves (param, input, diagnostic).
    for library in ["flow_syntax", "flow_check", "flow_eval"] {
        forbid(library, &[
            "flow_ir", "flow_web", "flow_shader", "flow_gpu", "flow_graph", "flow_sop", "flow_native", "rays",
            "editor_core", "pxui", "rays_shell", "rays_graph", "sop", "rays_editor",
        ]);
        forbid(library, &gpu);
    }
    forbid("flow_sop_catalog", &[
        "flow_sop", "flow_native", "rays", "editor_core", "editor_document", "pxui", "rays_shell", "rays_graph", "rays_editor", "rays_bin",
    ]);
    forbid("flow_sop_catalog", &gpu);
    rules
}

fn dependency_path(start: &str, goal: &str, graph: &HashMap<String, Vec<String>>) -> Option<Vec<String>> {
    let mut pending = vec![(start.to_owned(), vec![start.to_owned()])];
    let mut seen = HashSet::new();
    while let Some((current, path)) = pending.pop() {
        if !seen.insert(current.clone()) {
            continue;
        }
        if current == goal {
            return Some(path);
        }
        if let Some(next) = graph.get(&current) {
            for dependency in next.iter().rev() {
                let mut path = path.clone();
                path.push(dependency.clone());
                pending.push((dependency.clone(), path));
            }
        }
    }
    None
}
fn target_platform(target: &str) -> Option<&'static str> {
    let target = target.to_ascii_lowercase();
    if target.contains("not") || target.contains('!') {
        return None;
    }
    let apple = target.contains("apple") || target.contains("macos") || target.contains("ios");
    let windows = target.contains("windows");
    match (apple, windows) {
        (true, false) => Some("apple"),
        (false, true) => Some("windows"),
        _ => None,
    }
}

fn ogpu_selection_errors(
    packages: &[Package],
    graph: &HashMap<String, Vec<String>>,
    direct: &HashMap<String, Vec<String>>,
    workspace: &Path,
) -> Vec<String> {
    let mut errors = Vec::new();
    for package in packages {
        let all_selected = direct.get(&package.crate_name).map(Vec::as_slice).unwrap_or_default();
        let path = package.manifest.strip_prefix(workspace).unwrap_or(&package.manifest).to_string_lossy().replace('\\', "/");
        // Production startup cannot be satisfied by a dev dependency. Tests may explicitly
        // select mock in one target and the platform-native implementation in another.
        for tests in [false, true] {
            if !(if tests { package.has_gpu_dev_targets } else { package.has_binary }) {
                continue;
            }
            let declarations: Vec<_> = package.deps.iter().filter(|dependency| {
                dependency.kind == "dependencies" || (tests && dependency.kind == "dev-dependencies")
            }).collect();
            for dependency in &declarations {
                let name = dependency.package.as_deref().unwrap_or(&dependency.alias).replace('-', "_");
                let expected = match name.as_str() {
                    "ogpu_metal" => "apple",
                    "ogpu_vulkan" => "windows",
                    _ => continue,
                };
                if dependency.optional || dependency.target.as_deref().and_then(target_platform) != Some(expected) {
                    errors.push(format!("{path}: {} backend {} must be nonoptional and target {expected}", package.crate_name, dependency.alias));
                }
            }
            for (platform, native) in [("apple", "ogpu_metal"), ("windows", "ogpu_vulkan")] {
                // An implementation's native tests run only on its own platform.
                if tests && matches!(package.crate_name.as_str(), "ogpu_metal" | "ogpu_vulkan")
                    && package.crate_name != native
                {
                    continue;
                }
                let selected: Vec<_> = all_selected.iter().filter(|name| {
                    declarations.iter().any(|dependency| {
                        dependency.package.as_deref().unwrap_or(&dependency.alias).replace('-', "_") == **name
                            && !dependency.optional
                            && dependency.target.as_deref().and_then(target_platform).is_none_or(|target| target == platform)
                    })
                }).map(String::as_str).collect();
                let reaches_ogpu = selected.iter().any(|dependency| {
                    *dependency == "ogpu_core" || dependency_path(dependency, "ogpu_core", graph).is_some()
                });
                if !reaches_ogpu {
                    continue;
                }
                let native_count = declarations.iter().filter(|dependency| {
                    dependency.package.as_deref().unwrap_or(&dependency.alias).replace('-', "_") == native
                        && !dependency.optional
                        && dependency.target.as_deref().and_then(target_platform) == Some(platform)
                }).count() + usize::from(tests && package.crate_name == native);
                let mock = selected.contains(&"ogpu_mock") || (tests && package.crate_name == "ogpu_mock");
                if native_count <= 1 && (native_count == 1 || (tests && mock)) && (tests || !mock) {
                    continue;
                }
                let role = if tests { "GPU/window tests or dev executable" } else { "native executable" };
                errors.push(format!(
                    "{path}: {role} {} must select one nonoptional {native} {}on {platform}{}",
                    package.crate_name,
                    if tests { "" } else { "normal dependency " },
                    if tests { " (mock may also be selected)" } else { "" },
                ));
            }
        }
    }
    errors
}

fn deps(workspace: &Path) -> Result<Vec<String>, String> {
    let workspace_path = fs::canonicalize(workspace).map_err(|error| format!("cannot locate workspace {}: {error}", workspace.display()))?;
    let workspace = workspace_path.as_path();
    let root_manifest = parse_manifest(&workspace.join("Cargo.toml"))?;
    let workspace_deps: HashMap<String, DepSpec> = root_manifest.workspace_deps.into_iter().map(|dependency| (dependency.alias.clone(), dependency)).collect();
    let mut manifests = Vec::new();
    cargo_manifests(&workspace.join("crates"), &mut manifests)?;
    cargo_manifests(&workspace.join("tools"), &mut manifests)?;
    cargo_manifests(&workspace.join("bin"), &mut manifests)?;
    manifests.sort();

    let mut packages = Vec::new();
    let mut errors = Vec::new();
    for manifest_path in manifests {
        let manifest_path = fs::canonicalize(&manifest_path).map_err(|error| format!("cannot resolve {}: {error}", manifest_path.display()))?;
        let parsed = parse_manifest(&manifest_path)?;
        let Some(package_name) = parsed.package_name.as_deref() else { continue };
        let relative = manifest_path.strip_prefix(workspace).unwrap_or(&manifest_path).to_string_lossy().replace('\\', "/");
        let is_product = relative.starts_with("crates/") || relative.starts_with("bin/");
        let crate_name = crate_name(package_name, parsed.lib_name.as_deref());
        let (has_binary, has_gpu_dev_targets) = package_targets(manifest_path.parent().unwrap_or(workspace), &parsed)?;
        let mut dependencies = Vec::new();
        for mut dependency in parsed.deps {
            if dependency.workspace {
                if let Some(inherited) = workspace_deps.get(&dependency.alias) {
                    if dependency.package.is_none() { dependency.package = inherited.package.clone(); }
                    if dependency.path.is_none() {
                        dependency.path = inherited.path.as_ref().map(|path| workspace.join(path).to_string_lossy().into_owned());
                    }
                    dependency.workspace = inherited.workspace;
                }
            }
            dependencies.push(dependency);
        }
        packages.push(Package {
            manifest: manifest_path,
            package_name: package_name.to_owned(),
            crate_name,
            product: is_product,
            deps: dependencies,
            has_binary,
            has_gpu_dev_targets,
        });
    }

    let mut by_manifest = HashMap::new();
    let mut by_package = HashMap::new();
    let mut by_crate = HashMap::new();
    for (index, package) in packages.iter().enumerate() {
        by_manifest.insert(package.manifest.clone(), index);
        if by_package.insert(package.package_name.clone(), index).is_some() {
            errors.push(format!("{}: duplicate workspace package `{}`", package.manifest.display(), package.package_name));
        }
        if by_crate.insert(package.crate_name.clone(), index).is_some() {
            errors.push(format!("{}: duplicate crate name `{}`", package.manifest.display(), package.crate_name));
        }
    }
    let mut graph: HashMap<String, Vec<String>> = HashMap::new();
    let mut runtime_graph: HashMap<String, Vec<String>> = HashMap::new();
    let mut selection_direct: HashMap<String, Vec<String>> = HashMap::new();
    let allowlist = direct_allowlist();
    let local_policy = local_workspace_only();

    for package in &packages {
        let manifest_dir = package.manifest.parent().unwrap_or(workspace);
        let path = package.manifest.strip_prefix(workspace).unwrap_or(&package.manifest).to_string_lossy().replace('\\', "/");
        if package.product
            && !allowlist.contains_key(package.crate_name.as_str())
            && !local_policy.contains(package.crate_name.as_str())
        {
            errors.push(format!("{path}: no dependency policy for product crate `{}`", package.crate_name));
        }
        let mut direct = BTreeSet::new();
        let mut selection = BTreeSet::new();
        let mut runtime_direct = BTreeSet::new();
        for dependency in &package.deps {
            let dependency_package = dependency.package.as_deref().unwrap_or(&dependency.alias);
            let resolved = if let Some(local_path) = &dependency.path {
                let dep_manifest = manifest_dir.join(local_path).join("Cargo.toml");
                let dep_manifest = fs::canonicalize(&dep_manifest).unwrap_or(dep_manifest);
                if !dep_manifest.starts_with(workspace) {
                    errors.push(format!("{path}:{}: dependency `{}` is not a workspace crate", dependency.line, dependency.alias));
                    continue;
                }
                match by_manifest.get(&dep_manifest).and_then(|index| packages.get(*index)) {
                    Some(target) if target.package_name == dependency_package => Some(target),
                    Some(target) => {
                        errors.push(format!("{path}:{}: dependency `{}` names package `{dependency_package}` but its path is `{}`", dependency.line, dependency.alias, target.package_name));
                        Some(target)
                    }
                    None => {
                        errors.push(format!("{path}:{}: dependency `{}` does not resolve to a workspace crate", dependency.line, dependency.alias));
                        None
                    }
                }
            } else if (dependency_package == "cc" && dependency.kind == "build-dependencies")
                || (package.package_name == "pxui_text" && dependency_package == "cosmic-text" && dependency.kind == "dependencies")
            {
                None
            } else {
                errors.push(format!("{path}:{}: external dependency `{dependency_package}` is not allowed; use a local workspace crate (cc is the sole exception)", dependency.line));
                None
            };
            if let Some(target) = resolved {
                if package.crate_name == "rays"
                    && dependency.kind != "dev-dependencies"
                    && ["sdl3"].contains(&target.crate_name.as_str())
                {
                    errors.push(format!(
                        "{path}:{}: rays depends directly on {} (SDL services belong to runtime)",
                        dependency.line, target.crate_name
                    ));
                }
                if dependency.kind != "dev-dependencies" {
                    direct.insert(target.crate_name.clone());
                }
                if dependency.kind != "build-dependencies" {
                    selection.insert(target.crate_name.clone());
                }
                if dependency.kind == "dependencies" {
                    runtime_direct.insert(target.crate_name.clone());
                }
                if package.product && !target.product {
                    errors.push(format!("{path}:{}: product crate `{}` must not depend on tool crate `{}`", dependency.line, package.crate_name, target.crate_name));
                }
            }
        }
        let direct: Vec<String> = direct.into_iter().collect();
        graph.insert(package.crate_name.clone(), direct.clone());
        let runtime_direct: Vec<String> = runtime_direct.into_iter().collect();
        runtime_graph.insert(package.crate_name.clone(), runtime_direct);
        let selection: Vec<String> = selection.into_iter().collect();
        selection_direct.insert(package.crate_name.clone(), selection);
        if let Some(allowed) = allowlist.get(package.crate_name.as_str()) {
            let leaves = ocaml_leaves_reached(&package.crate_name, &allowlist);
            for dependency in &direct {
                let name = dependency.as_str();
                if !allowed.contains(&name) && !HELPER_LEAVES.contains(&name) && !leaves.contains(name) {
                    errors.push(format!("{path}: `{}` may not directly depend on `{dependency}`", package.crate_name));
                }
            }
        }
    }

    let reach_rules = forbidden_reaches();
    for (library, forbidden) in reach_rules {
        if !by_crate.contains_key(&library) {
            continue;
        }
        for target in forbidden {
            if target == library {
                continue;
            }
            if let Some(path) = dependency_path(&library, &target, &graph) {
                errors.push(format!("dependency boundary: {} must not reach `{target}` (via {})", library, path.join(" -> ")));
            }
        }
    }
    errors.extend(ogpu_selection_errors(&packages, &runtime_graph, &selection_direct, workspace));
    errors.sort();
    Ok(errors)
}


fn docs(workspace: &Path) -> Result<Vec<String>, String> {
    // rays-t bounds setup/build, each case and the test run. Gate checks never refresh fixtures.
    for (package, filter, target) in [
        ("flow_check", "examples_check", None),
        ("rays_bin", "cli_help_errors_json_and_documentation", Some("cli")),
        ("rays_bin", "registry_metadata_contracts", Some("cli")),
    ] {
        let mut command = std::process::Command::new("cargo");
        command.current_dir(workspace).args(["t", "--profile", "default", "-p", package])
            .env_remove("RAYS_UPDATE_FIXTURES").env_remove("RAYS_ALLOW_SKIP");
        if let Some(target) = target { command.args(["--test", target]); }
        let status = command.arg(filter).status().map_err(|error| format!("cannot run registry check: {error}"))?;
        if !status.success() {
            return Ok(vec![format!("docs gate: {package}::{filter} failed ({status}); examples, descriptions, angle units and docs/lisp.md must match the registry")]);
        }
    }
    Ok(Vec::new())
}


fn workspace_root() -> Result<PathBuf, String> {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(2)
        .map(Path::to_path_buf)
        .ok_or_else(|| "cannot locate Rust workspace root".to_owned())
}

fn main() {
    let mut args = env::args().skip(1);
    let command = args.next().unwrap_or_else(|| "all".into());
    if args.next().is_some() || !matches!(command.as_str(), "shape" | "deps" | "docs" | "all") {
        eprintln!("usage: cargo run -p gate -- <shape|deps|docs|all>");
        process::exit(2);
    }
    let workspace = match workspace_root() {
        Ok(workspace) => workspace,
        Err(error) => {
            eprintln!("gate: {error}");
            process::exit(2);
        }
    };
    let result = match command.as_str() {
        "shape" => shape(&workspace),
        "deps" => deps(&workspace),
        "docs" => docs(&workspace),
        _ => shape(&workspace).and_then(|mut errors| {
            errors.extend(deps(&workspace)?);
            if errors.is_empty() { errors.extend(docs(&workspace)?); }
            Ok(errors)
        }),
    };
    match result {
        Ok(issues) if issues.is_empty() => println!("{} gate passed", command),
        Ok(issues) => {
            for issue in issues {
                eprintln!("{issue}");
            }
            process::exit(1);
        }
        Err(error) => {
            eprintln!("gate: {error}");
            process::exit(2);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{assignment_fields, dependency_path, deps, ogpu_selection_errors, shape_source, target_platform, toml_string, DepSpec, Package};
    use std::collections::HashMap;
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[test]
    fn lib_unit_tests_disabled_includes_integration_only_libs() {
        let workspace = std::env::current_dir().expect("cwd");
        let root = if workspace.ends_with("gate") {
            workspace.parent().and_then(|p| p.parent()).expect("workspace root")
        } else {
            workspace.as_path()
        };
        let disabled = super::lib_unit_tests_disabled(root).expect("manifest scan");
        assert!(disabled.contains("pxui") && disabled.contains("flow_shader"));
    }

    #[test]
    fn ignored_cases_are_rejected_even_in_test_regions() {
        for source in ["#[test] #[ignore] fn case() {}", "#[cfg_attr(unix, ignore = \"slow\")] fn case() {}"] {
            assert!(shape_source("sample", "sample/tests/cases.rs", source).iter().any(|issue| issue.contains("ignored tests are forbidden")));
        }
    }

    #[test]
    fn shape_gate_restricts_types_and_control_flow() {
        let source = "#[derive(Clone, Ord)]\npub struct Point<'a, T>(&'a T);\npub fn make<T>() -> impl Clone { todo!() }\n";
        let issues = shape_source("geometry", "geometry/src/lib.rs", source);
        assert!(issues.iter().any(|issue| issue.contains("derive `Ord`")));
        assert!(issues.iter().any(|issue| issue.contains("generic items")));
        assert!(issues.iter().any(|issue| issue.contains("lifetime parameters")));
        assert!(issues.iter().any(|issue| issue.contains("impl Trait")));
        assert!(issues.iter().any(|issue| issue.contains("panic macros")));
        assert!(shape_source("geometry", "geometry/src/lib.rs", "macro_rules! extra { () => {} }\n").iter().any(|issue| issue.contains("macro_rules!")));
        assert!(shape_source("geometry", "geometry/src/lib.rs", "unsafe fn raw() {}\n").iter().any(|issue| issue.contains("unsafe")));
        assert!(shape_source("parallel", "parallel/src/lib.rs", "unsafe fn raw() {}\n").is_empty());
        let async_issues = shape_source("rays", "rays/src/lib.rs", "async fn tick() { let _ = async {}.await; }\n");
        assert_eq!(async_issues.iter().filter(|issue| issue.contains("async/await")).count(), 3);
        let unwind_issues = shape_source("rays", "rays/src/lib.rs", "std::panic::catch_unwind(|| {});\n");
        assert!(unwind_issues.iter().any(|issue| issue.contains("catch_unwind")));
        let worker_unwind =
            shape_source("parallel", "parallel/src/lib.rs", "std::panic::catch_unwind(|| {});\n");
        assert!(worker_unwind.is_empty());
        let impl_trait = shape_source(
            "rays",
            "rays/src/lib.rs",
            "pub fn borrow(value: &impl Debug) {}\npub fn borrow_named(value: &'a impl Debug) {}\n",
        );
        assert_eq!(impl_trait.iter().filter(|issue| issue.contains("impl Trait")).count(), 2);
        let emitted = shape_source(
            "flow",
            "flow/src/lib.rs",
            "use std::collections::HashMap;\nfn emit(map: HashMap<u8, u8>) -> String { map.iter().map(|(key, value)| format!(\"{key}{value}\")).collect() }\n",
        );
        assert!(emitted.iter().any(|issue| issue.contains("HashMap iteration")));
        let test_only = shape_source("flow", "flow/src/lib.rs", "#[cfg(test)] mod tests { fn sentinel() { panic!(\"test\"); } }\n");
        assert!(!test_only.iter().any(|issue| issue.contains("panic macros")));
        let fields = assignment_fields(r#"{ path = "../mem_sys", package = "mem_sys", features = ["a", "b"] }"#);
        assert_eq!(fields.get("path").and_then(|value| toml_string(value)).as_deref(), Some("../mem_sys"));
        assert_eq!(fields.get("package").and_then(|value| toml_string(value)).as_deref(), Some("mem_sys"));
        let graph = HashMap::from([
            ("base".to_owned(), vec!["middle".to_owned()]),
            ("middle".to_owned(), vec!["upper".to_owned()]),
        ]);
        assert_eq!(
            dependency_path("base", "upper", &graph),
            Some(vec!["base".to_owned(), "middle".to_owned(), "upper".to_owned()])
        );
    }

    #[test]
    fn shape_gate_distinguishes_borrow_lifetimes_from_type_parameters() {
        for source in [
            "fn borrow<'a>(value: &'a str) -> &'a str { value }",
            "fn borrow<'a, 'b: 'a>(a: &'a str, b: &'b str) -> &'a str { a }",
            "impl<'a> IntoIterator for &'a Rows {}",
        ] {
            assert!(shape_source("other", "other/src/lib.rs", source).is_empty(), "{source}");
        }
        let typed = shape_source("other", "other/src/lib.rs", "fn generic<'a, T>(value: &'a T) {}");
        assert!(typed.iter().any(|issue| issue.contains("generic items")));
        let borrowed = shape_source("other", "other/src/lib.rs", "struct View<'a>(&'a str);");
        assert!(borrowed.iter().any(|issue| issue.contains("lifetime parameters")));
    }

    #[test]
    fn shape_gate_allows_only_the_requested_domain_helper() {
        let helper = "pub fn with_domains<R>(domains: i64, work: impl FnOnce() -> R) -> R { work() }";
        let path = "crates/parallel/src/lib.rs";
        assert!(shape_source("parallel", path, helper).is_empty());
        assert!(shape_source("parallel", "parallel/src/lib.rs", helper).is_empty());
        for (name, file, source) in [
            ("parallel", path, "pub fn sibling<R>(work: impl FnOnce() -> R) -> R { work() }"),
            ("other", path, helper),
            ("parallel", "crates/parallel/src/other.rs", helper),
            ("parallel", path, "pub fn with_domains<R>(domains: i64, work: impl FnOnce() -> R) -> R { fn nested<T>() {} work() }"),
            ("parallel", path, "struct with_domains<T> { value: T }"),
            ("parallel", path, "pub fn with_domains<R, S>(domains: i64, work: impl FnOnce(S) -> R) -> R { work() }"),
            ("parallel", path, "pub fn with_domains<R>(domains: i64, work: impl FnMut() -> R) -> R { work() }"),
        ] {
            assert!(!shape_source(name, file, source).is_empty(), "{source}");
        }
    }

    #[test]
    fn shape_gate_allows_unreachable_but_not_panic() {
        // unreachable! is OCaml's `assert false`, allowed anywhere; panic!/todo! never are.
        assert!(shape_source("other", "crates/other/src/lib.rs", "unreachable!(\"cache state\");\n").is_empty());
        for (crate_name, path, source) in [
            ("lru", "crates/lru/src/lib.rs", "panic!(\"not an invariant\");"),
            ("other", "crates/other/src/lib.rs", "todo!();"),
        ] {
            assert!(shape_source(crate_name, path, source)
                .iter()
                .any(|issue| issue.contains("panic macros")));
        }
    }


    #[test]
    fn dependency_gate_resolves_workspace_dependencies_and_rejects_prohibited_edges() {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let root = std::env::temp_dir().join(format!(
            "gate-workspace-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        for path in ["crates/mem/src", "crates/lru/src", "crates/rays/src", "crates/sdl3/src"] {
            fs::create_dir_all(root.join(path)).unwrap();
        }
        fs::write(
            root.join("Cargo.toml"),
            "[workspace]\nmembers = [\"crates/mem\", \"crates/lru\", \"crates/rays\", \"crates/sdl3\"]\n[workspace.dependencies]\nmem = { path = \"crates/mem\" }\n",
        )
        .unwrap();
        for package in ["mem", "sdl3", "rays"] {
            fs::write(
                root.join(format!("crates/{package}/Cargo.toml")),
                format!("[package]\nname = \"{package}\"\nversion = \"0.1.0\"\n"),
            )
            .unwrap();
            fs::write(root.join(format!("crates/{package}/src/lib.rs")), "").unwrap();
        }
        fs::write(
            root.join("crates/lru/Cargo.toml"),
            "[package]\nname = \"lru\"\nversion = \"0.1.0\"\n[dependencies]\nmem = { workspace = true }\n",
        )
        .unwrap();
        fs::write(root.join("crates/lru/src/lib.rs"), "").unwrap();
        let allowed = deps(&root);
        fs::write(
            root.join("crates/rays/Cargo.toml"),
            "[package]\nname = \"rays\"\nversion = \"0.1.0\"\n[dependencies]\nsdl3 = { path = \"../sdl3\" }\n",
        )
        .unwrap();
        let sdl_edge = deps(&root);
        fs::write(
            root.join("crates/lru/Cargo.toml"),
            "[package]\nname = \"lru\"\nversion = \"0.1.0\"\n[dependencies]\nmem = { workspace = true }\n[dev-dependencies]\nserde = \"1\"\n",
        )
        .unwrap();
        let external_edge = deps(&root);
        fs::write(
            root.join("crates/lru/Cargo.toml"),
            "[package]\nname = \"lru\"\nversion = \"0.1.0\"\n[dependencies]\nmem = { workspace = true }\ncc = \"1\"\n",
        )
        .unwrap();
        let cc_dependency_edge = deps(&root);
        fs::write(
            root.join("crates/lru/Cargo.toml"),
            "[package]\nname = \"lru\"\nversion = \"0.1.0\"\n[dependencies]\nmem = { workspace = true }\n[build-dependencies]\ncc = \"1\"\n",
        )
        .unwrap();
        let cc_build_edge = deps(&root);
        for package in ["flow_ir", "pxui", "ogpu_metal", "metal_sys", "vulkan_sys", "ogpu_core"] {
            fs::create_dir_all(root.join(format!("crates/{package}/src"))).unwrap();
            fs::write(root.join(format!("crates/{package}/Cargo.toml")), format!("[package]\nname = \"{package}\"\n")).unwrap();
            fs::write(root.join(format!("crates/{package}/src/lib.rs")), "").unwrap();
        }
        fs::write(root.join("crates/flow_ir/Cargo.toml"), "[package]\nname = \"flow_ir\"\n[dependencies]\npxui = { path = \"../pxui\" }\n").unwrap();
        fs::write(root.join("crates/rays/Cargo.toml"), "[package]\nname = \"rays\"\n[dependencies]\nogpu_metal = { path = \"../ogpu_metal\" }\nmetal_sys = { path = \"../metal_sys\" }\n").unwrap();
        fs::write(root.join("crates/metal_sys/Cargo.toml"), "[package]\nname = \"metal_sys\"\n[dependencies]\nogpu_core = { path = \"../ogpu_core\" }\n").unwrap();
        fs::write(root.join("crates/vulkan_sys/Cargo.toml"), "[package]\nname = \"vulkan_sys\"\n[dependencies]\nogpu_core = { path = \"../ogpu_core\" }\n").unwrap();
        let boundaries = deps(&root).unwrap();
        assert!(boundaries.iter().any(|issue| issue.contains("flow_ir must not reach") && issue.contains("pxui")));
        assert!(boundaries.iter().any(|issue| issue.contains("rays must not reach") && issue.contains("ogpu_metal")));
        assert!(boundaries.iter().any(|issue| issue.contains("rays must not reach") && issue.contains("metal_sys")));
        assert!(!boundaries.iter().any(|issue| issue.contains("`metal_sys` may not directly depend on `ogpu_core`")));
        assert!(!boundaries.iter().any(|issue| issue.contains("`vulkan_sys` may not directly depend on `ogpu_core`")));
        let vulkan_manifest=root.join("crates/vulkan_sys/Cargo.toml");
        let admitted_vulkan=fs::read_to_string(&vulkan_manifest).unwrap();
        fs::write(&vulkan_manifest,format!("{admitted_vulkan}pxui = {{ path = \"../pxui\" }}\n")).unwrap();
        assert!(deps(&root).unwrap().iter().any(|issue| issue.contains("`vulkan_sys` may not directly depend on `pxui`")));
        fs::write(vulkan_manifest,admitted_vulkan).unwrap();
        let sys_manifest = root.join("crates/metal_sys/Cargo.toml");
        let admitted_sys = fs::read_to_string(&sys_manifest).unwrap();
        fs::write(&sys_manifest,format!("{admitted_sys}pxui = {{ path = \"../pxui\" }}\n")).unwrap();
        assert!(deps(&root).unwrap().iter().any(|issue| issue.contains("`metal_sys` may not directly depend on `pxui`")));
        fs::write(sys_manifest,admitted_sys).unwrap();
        for package in ["flow_ir", "rays"] {
            let path = root.join(format!("crates/{package}/Cargo.toml"));
            fs::write(&path, fs::read_to_string(&path).unwrap().replace("[dependencies]", "[dev-dependencies]")).unwrap();
        }
        assert!(deps(&root).unwrap().is_empty(), "dev-only tests do not change production boundaries");
        fs::remove_dir_all(&root).unwrap();
        assert_eq!(allowed.unwrap(), Vec::<String>::new());
        let sdl_edge = sdl_edge.unwrap();
        assert!(sdl_edge.iter().any(|issue| issue.contains("rays depends directly on sdl3")));
        let external_edge = external_edge.unwrap();
        assert!(external_edge.iter().any(|issue| issue.contains("external dependency `serde`")));
        assert!(!external_edge.iter().any(|issue| issue.contains("external dependency `mem`")));
        let cc_dependency_edge = cc_dependency_edge.unwrap();
        assert!(cc_dependency_edge
            .iter()
            .any(|issue| issue.contains("external dependency `cc`")));
        let cc_build_edge = cc_build_edge.unwrap();
        assert!(!cc_build_edge
            .iter()
            .any(|issue| issue.contains("external dependency `cc`")));
    }


    #[test]
    fn backend_selection_distinguishes_cpu_tests_dev_targets_and_native_startup() {
        let dependency = |name: &str, kind: &str, target: Option<&str>| DepSpec {
            alias: name.into(), kind: kind.into(), target: target.map(str::to_owned), ..Default::default()
        };
        let core = dependency("ogpu_core", "dependencies", None);
        let metal = dependency("ogpu_metal", "dependencies", Some("cfg(target_os = \"macos\")"));
        let vulkan = dependency("ogpu_vulkan", "dependencies", Some("cfg(windows)"));
        let mock = dependency("ogpu_mock", "dev-dependencies", None);
        let mut app = Package {
            manifest: PathBuf::from("crates/app/Cargo.toml"),
            package_name: "app".into(), crate_name: "app".into(), product: true,
            deps: vec![core.clone(), metal.clone(), vulkan.clone()],
            has_binary: true, has_gpu_dev_targets: false,
        };
        let graph = HashMap::from([
            ("ogpu_metal".into(), vec!["ogpu_core".into()]),
            ("ogpu_vulkan".into(), vec!["ogpu_core".into()]),
            ("ogpu_mock".into(), vec!["ogpu_core".into()]),
        ]);
        let check = |package: &Package| {
            let selected = HashMap::from([("app".into(), package.deps.iter().map(|d| d.alias.clone()).collect())]);
            ogpu_selection_errors(&[package.clone()], &graph, &selected, Path::new("rs"))
        };
        assert!(check(&app).is_empty(), "normal target-specific native startup");
        let valid = app.clone();
        app.deps = vec![core];
        assert_eq!(check(&app).len(), 2, "each platform needs its backend");
        app.has_binary = false;
        assert!(check(&app).is_empty(), "CPU unit/e2e tests may link portable GPU types");
        app.has_gpu_dev_targets = true;
        assert_eq!(check(&app).len(), 2, "GPU targets still require an implementation");
        app.deps.push(mock.clone());
        assert!(check(&app).is_empty(), "mock-only GPU contract tests");
        for mut native in [metal.clone(), vulkan] {
            native.kind = "dev-dependencies".into();
            app.deps.push(native);
        }
        assert!(check(&app).is_empty(), "examples and tests may select mock and one platform-native backend");
        app.has_binary = true;
        assert_eq!(check(&app).len(), 2, "dev dependencies cannot satisfy production startup");
        app = valid.clone();
        app.has_gpu_dev_targets = true;
        app.deps.push(mock.clone());
        assert!(check(&app).is_empty(), "test mock does not change production selection");
        app = valid.clone();
        app.deps[1].optional = true;
        assert!(!check(&app).is_empty(), "optional native edge");
        app = valid.clone();
        app.deps.push(metal);
        assert!(!check(&app).is_empty(), "duplicate native edges");
        app = valid.clone();
        app.deps[1].target = Some("cfg(windows)".into());
        assert!(!check(&app).is_empty(), "wrong platform");
        app = valid.clone();
        app.deps[1].target = None;
        assert!(!check(&app).is_empty(), "unconditional native edge");
        app = valid.clone();
        app.deps[1].kind = "build-dependencies".into();
        assert!(!check(&app).is_empty(), "build dependency is not a runtime backend");
        app = valid;
        app.deps.push(DepSpec { kind: "dependencies".into(), ..mock });
        assert!(!check(&app).is_empty(), "native applications cannot select the mock backend");
        assert_eq!(target_platform("cfg(target_os = \"windows\")"), Some("windows"));
    }

    #[test]
    fn target_roles_keep_cpu_shader_printer_and_dev_examples_out_of_native_binaries() {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let root = std::env::temp_dir().join(format!("gate-targets-{}-{}", std::process::id(), NEXT.fetch_add(1, Ordering::Relaxed)));
        fs::create_dir_all(root.join("src")).unwrap();
        let manifest = root.join("Cargo.toml");
        fs::write(&manifest, "[package]\nname = \"flow_shader\"\n[[bin]]\nname = \"print_shader\"\npath = \"src/print_shader.rs\"\n").unwrap();
        fs::write(root.join("src/print_shader.rs"), "fn main() {}").unwrap();
        let roles = || super::package_targets(&root, &super::parse_manifest(&manifest).unwrap()).unwrap();
        assert_eq!(roles(), (false, false), "code generation needs no driver");
        fs::write(root.join("src/lib.rs"), "#[test] fn cpu_metadata_test() {}").unwrap();
        assert_eq!(roles(), (false, false), "ordinary CPU tests do not initialize GPU");
        fs::create_dir_all(root.join("examples")).unwrap();
        fs::write(root.join("examples/orbit.rs"), "fn main() {}").unwrap();
        assert_eq!(roles(), (false, true), "Cargo examples use dev dependencies");
        fs::remove_dir_all(root.join("examples")).unwrap();
        fs::write(&manifest, "[package]\nname = \"flow_shader\"\n[[bin]]\nname = \"print_shader\"\n[[bin]]\nname = \"app\"\n").unwrap();
        assert_eq!(roles(), (true, false), "additional binaries retain strict startup checks");
        fs::write(&manifest, "[package]\nname = \"app\"\n[[test]]\nname = \"gpu_first\"\n[[test]]\nname = \"cpu_last\"\n").unwrap();
        assert_eq!(roles(), (false, true), "all explicit targets count, not just the last table");
        fs::write(&manifest, "[package]\nname = \"web_export\"\n").unwrap();
        fs::create_dir_all(root.join("examples")).unwrap();
        fs::write(root.join("examples/refresh_manifest.rs"), "fn main() {}").unwrap();
        assert_eq!(roles(), (false, false), "the text manifest generator requires no device");
        fs::write(root.join("examples/interactive.rs"), "fn main() {}").unwrap();
        assert_eq!(roles(), (false, true), "additional examples retain native backend requirements");
        fs::remove_dir_all(root.join("examples")).unwrap();
        fs::create_dir_all(root.join("tests")).unwrap();
        fs::write(root.join("tests/gpu_unused.rs"), "fn main() {}").unwrap();
        fs::write(&manifest, "[package]\nname = \"app\"\nautotests = false\n").unwrap();
        assert_eq!(roles(), (false, false), "disabled automatic test files are not targets");
        fs::write(&manifest, "[package]\nname = \"app\"\nautotests = false\n[[test]]\nname = \"gpu_actual\"\n").unwrap();
        assert_eq!(roles(), (false, true), "explicit tests remain active with autotests disabled");
        fs::remove_dir_all(&root).unwrap();
    }
}
