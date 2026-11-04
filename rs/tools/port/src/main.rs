use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::env;
use std::fs::{self, OpenOptions};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{self, Command};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

const HEADER: &str = "id\tocaml_file\trust_file\tcrate\tdepends_on\tlines\ttier\tstatus\towner";
const OCAMLDEP_BATCH: usize = 64;

#[derive(Clone, Debug, Eq, PartialEq)]
enum Sexp {
    Atom(String),
    List(Vec<Sexp>),
}

#[derive(Clone, Debug)]
struct DuneModule {
    name: String,
    ocaml_file: String,
    sources: Vec<String>,
}


#[derive(Clone, Debug)]
struct DuneLibrary {
    name: String,
    wrapped: bool,
    modules: Vec<DuneModule>,
    libraries: Vec<String>,
    implicit_modules: Vec<String>,
    crate_name: String,
    tier: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct Row {
    id: String,
    ocaml_file: String,
    rust_file: String,
    crate_name: String,
    depends_on: Vec<String>,
    lines: usize,
    tier: String,
    status: String,
    owner: String,
}

fn root() -> Result<PathBuf, String> {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    manifest
        .ancestors()
        .find(|path| path.join("lib").is_dir() && path.join("rs").is_dir())
        .map(Path::to_path_buf)
        .ok_or_else(|| format!("could not find repository root above {}", manifest.display()))
}

fn queue_path(root: &Path) -> PathBuf {
    root.join("rs/port/queue.tsv")
}

fn parse_sexps(input: &str) -> Result<Vec<Sexp>, String> {
    fn quoted(bytes: &[u8], at: &mut usize) -> Result<String, String> {
        *at += 1;
        let mut out = String::new();
        while *at < bytes.len() {
            match bytes[*at] {
                b'"' => {
                    *at += 1;
                    return Ok(out);
                }
                b'\\' => {
                    *at += 1;
                    let byte = *bytes.get(*at).ok_or("unterminated Dune string escape")?;
                    out.push(match byte {
                        b'n' => '\n',
                        b'r' => '\r',
                        b't' => '\t',
                        other => other as char,
                    });
                    *at += 1;
                }
                byte if byte.is_ascii() => {
                    out.push(byte as char);
                    *at += 1;
                }
                _ => {
                    let text = std::str::from_utf8(&bytes[*at..]).map_err(|e| e.to_string())?;
                    let ch = text.chars().next().ok_or("invalid Dune string")?;
                    out.push(ch);
                    *at += ch.len_utf8();
                }
            }
        }
        Err("unterminated Dune string".to_owned())
    }

    fn one(bytes: &[u8], at: &mut usize) -> Result<Sexp, String> {
        while *at < bytes.len() {
            match bytes[*at] {
                b' ' | b'\t' | b'\r' | b'\n' => *at += 1,
                b';' => {
                    while *at < bytes.len() && bytes[*at] != b'\n' {
                        *at += 1;
                    }
                }
                _ => break,
            }
        }
        match bytes.get(*at).copied() {
            Some(b'(') => {
                *at += 1;
                let mut items = Vec::new();
                loop {
                    while *at < bytes.len() {
                        match bytes[*at] {
                            b' ' | b'\t' | b'\r' | b'\n' => *at += 1,
                            b';' => {
                                while *at < bytes.len() && bytes[*at] != b'\n' {
                                    *at += 1;
                                }
                            }
                            _ => break,
                        }
                    }
                    match bytes.get(*at) {
                        Some(b')') => {
                            *at += 1;
                            return Ok(Sexp::List(items));
                        }
                        None => return Err("unterminated Dune list".to_owned()),
                        _ => items.push(one(bytes, at)?),
                    }
                }
            }
            Some(b')') => Err("unexpected ')' in Dune file".to_owned()),
            Some(b'"') => quoted(bytes, at).map(Sexp::Atom),
            Some(_) => {
                let start = *at;
                while *at < bytes.len()
                    && !matches!(bytes[*at], b' ' | b'\t' | b'\r' | b'\n' | b'(' | b')' | b';')
                {
                    *at += 1;
                }
                let atom = std::str::from_utf8(&bytes[start..*at])
                    .map_err(|e| e.to_string())?
                    .to_owned();
                Ok(Sexp::Atom(atom))
            }
            None => Err("unexpected end of Dune file".to_owned()),
        }
    }

    let bytes = input.as_bytes();
    let mut at = 0;
    let mut forms = Vec::new();
    loop {
        while at < bytes.len() {
            match bytes[at] {
                b' ' | b'\t' | b'\r' | b'\n' => at += 1,
                b';' => {
                    while at < bytes.len() && bytes[at] != b'\n' {
                        at += 1;
                    }
                }
                _ => break,
            }
        }
        if at == bytes.len() {
            return Ok(forms);
        }
        forms.push(one(bytes, &mut at)?);
    }
}

fn list_head(expr: &Sexp) -> Option<&str> {
    match expr {
        Sexp::List(items) => items.first().and_then(atom),
        Sexp::Atom(_) => None,
    }
}

fn atom(expr: &Sexp) -> Option<&str> {
    match expr {
        Sexp::Atom(value) => Some(value),
        Sexp::List(_) => None,
    }
}

fn field<'a>(stanza: &'a Sexp, key: &str) -> Option<&'a Sexp> {
    match stanza {
        Sexp::List(items) => items.iter().skip(1).find(|item| list_head(item) == Some(key)),
        _ => None,
    }
}

fn field_atom(stanza: &Sexp, key: &str) -> Option<String> {
    let Sexp::List(values) = field(stanza, key)? else {
        return None;
    };
    values.get(1).and_then(atom).map(str::to_owned)
}

fn atoms(expr: &Sexp, out: &mut Vec<String>) {
    match expr {
        Sexp::Atom(value) => out.push(value.clone()),
        Sexp::List(items) => items.iter().for_each(|item| atoms(item, out)),
    }
}

fn dune_module_name(name: &str) -> String {
    let mut module = name.to_owned();
    if let Some(first) = module.get_mut(0..1) {
        first.make_ascii_uppercase();
    }
    module
}

fn source_module_name(name: &str, available: &BTreeMap<String, Vec<String>>) -> String {
    if available.contains_key(name) {
        name.to_owned()
    } else {
        let module = dune_module_name(name);
        if available.contains_key(&module) { module } else { name.to_owned() }
    }
}

fn flow_module_crate(module: &str) -> Option<&'static str> {
    const SYNTAX: &[&str] = &[
        "phase_timer", "context", "symbol", "parinfer", "syntax", "text_lexer", "ty", "value",
        "packed_ops", "lisp", "macro",
    ];
    const CHECK: &[&str] = &[
        "port_type", "catalog", "defnode", "lisp_scan", "op", "ray_host", "workspace",
    ];
    if SYNTAX.iter().any(|name| module.eq_ignore_ascii_case(name)) {
        Some("flow_syntax")
    } else if CHECK.iter().any(|name| module.eq_ignore_ascii_case(name)) {
        Some("flow_check")
    } else if module.eq_ignore_ascii_case("eval") {
        Some("flow_eval")
    } else {
        None
    }
}

fn module_crate<'a>(library: &'a DuneLibrary, module: &str) -> &'a str {
    if library.name == "flow" {
        flow_module_crate(module).unwrap_or(&library.crate_name)
    } else {
        &library.crate_name
    }
}

fn module_id(library: &DuneLibrary, module: &str) -> String {
    format!("{}:{}", library.crate_name, module.to_ascii_lowercase())
}

fn rust_module_file(library: &DuneLibrary, module: &str) -> String {
    let file = if library.name == "flow" && module.eq_ignore_ascii_case("macro") {
        "macros"
    } else {
        module
    };
    format!("rs/crates/{}/src/{}.rs", module_crate(library, module), file.to_ascii_lowercase())
}



fn source_modules(
    dir: &Path,
    root: &Path,
    recursive: bool,
) -> Result<BTreeMap<String, Vec<String>>, String> {
    let mut modules: BTreeMap<String, Vec<String>> = BTreeMap::new();
    let entries = fs::read_dir(dir).map_err(|e| format!("{}: {e}", dir.display()))?;
    for entry in entries {
        let entry = entry.map_err(|e| format!("{}: {e}", dir.display()))?;
        let file_type = entry.file_type().map_err(|e| e.to_string())?;
        if file_type.is_dir() {
            if recursive {
                for (module, mut paths) in source_modules(&entry.path(), root, true)? {
                    modules.entry(module).or_default().append(&mut paths);
                }
            }
            continue;
        }
        if !file_type.is_file() {
            continue;
        }
        let path = entry.path();
        let Some(ext) = path.extension().and_then(|x| x.to_str()) else {
            continue;
        };
        if ext != "ml" && ext != "mli" {
            continue;
        }
        let Some(stem) = path.file_stem().and_then(|x| x.to_str()) else {
            continue;
        };
        let mut module = stem.to_owned();
        if let Some(first) = module.get_mut(0..1) {
            first.make_ascii_uppercase();
        }
        let relative = path
            .strip_prefix(root)
            .map_err(|e| e.to_string())?
            .to_string_lossy()
            .replace('\\', "/");
        modules.entry(module).or_default().push(relative);
    }
    for files in modules.values_mut() {
        files.sort();
    }
    Ok(modules)
}

fn module_spec(field: Option<&Sexp>, default_modules: &BTreeMap<String, Vec<String>>) -> Result<Vec<String>, String> {
    let mut names = BTreeSet::new();
    let mut excluded = BTreeSet::new();
    let mut standard = field.is_none();
    let mut excluding = false;
    if let Some(expr) = field {
        let Sexp::List(values) = expr else {
            return Err("modules field is not a list".to_owned());
        };
        for value in values.iter().skip(1) {
            match value {
                Sexp::Atom(name) if name == ":standard" => {
                    standard = true;
                    excluding = false;
                }
                Sexp::Atom(name) if name == "\\" => excluding = true,
                Sexp::Atom(name) if name.starts_with(':') || name.starts_with('%') => {
                    return Err(format!("unsupported modules selector {name}"));
                }
                Sexp::Atom(name) => {
                    let module = source_module_name(name, default_modules);
                    if excluding {
                        excluded.insert(module);
                    } else {
                        names.insert(module);
                    }
                }
                Sexp::List(_) => return Err("nested modules expression is unsupported".to_owned()),
            }
        }
    }
    if standard {
        names.extend(default_modules.keys().cloned());
    }
    names.retain(|name| !excluded.contains(name));
    Ok(names.into_iter().collect())
}

fn all_stanza_modules(stanza: &Sexp) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    for key in ["modules", "modules_without_implementation"] {
        if let Some(expr) = field(stanza, key) {
            let mut values = Vec::new();
            atoms(expr, &mut values);
            for value in values.into_iter().skip(1) {
                if value != ":standard" && value != "\\" && !value.starts_with(':') && !value.starts_with('%') {
                    names.insert(dune_module_name(&value));
                }
            }
        }
    }
    names
}

fn walk_dune(dir: &Path, files: &mut Vec<PathBuf>) -> Result<(), String> {
    for entry in fs::read_dir(dir).map_err(|e| format!("{}: {e}", dir.display()))? {
        let entry = entry.map_err(|e| format!("{}: {e}", dir.display()))?;
        let ty = entry.file_type().map_err(|e| e.to_string())?;
        if ty.is_dir() {
            walk_dune(&entry.path(), files)?;
        } else if ty.is_file() && entry.file_name() == "dune" {
            files.push(entry.path());
        }
    }
    Ok(())
}

fn seed_tiers(root: &Path) -> Result<BTreeMap<String, String>, String> {
    let content = fs::read_to_string(root.join("rs/port/progress.tsv")).map_err(|e| e.to_string())?;
    let mut tiers = BTreeMap::new();
    for line in content.lines().skip(1) {
        let columns: Vec<_> = line.split('\t').collect();
        if columns.len() < 4 || !columns[0].starts_with('W') {
            continue;
        }
        let tier = columns[0];
        if tier == "W1" {
            for crate_name in [
                "rays_diagnostic",
                "input",
                "native_layer_token",
                "rays_math",
                "scene_command",
                "param",
                "mem",
                "lru",
                "parallel",
                "rays_collections",
            ] {
                tiers.insert(crate_name.to_owned(), tier.to_owned());
            }
            continue;
        }
        for name in columns[3].split(',').map(str::trim) {
            if name == "SDL media bindings" {
                for crate_name in ["sdl3_image", "sdl3_ttf", "sdl3_mixer"] {
                    tiers.insert(crate_name.to_owned(), tier.to_owned());
                }
            } else if !name.is_empty() && !name.contains(' ') {
                let name = if name == "diagnostic" { "rays_diagnostic" } else { name };
                tiers.insert(name.to_owned(), tier.to_owned());
            }
        }
    }
    Ok(tiers)
}

fn initial_states(root: &Path) -> Result<BTreeMap<String, (String, String)>, String> {
    let content = fs::read_to_string(root.join("rs/port/progress.tsv")).map_err(|e| e.to_string())?;
    let mut states = BTreeMap::new();
    for line in content.lines().skip(1) {
        let columns: Vec<_> = line.split('\t').collect();
        if columns.len() < 4 || !columns[0].starts_with("unit: ") {
            continue;
        }
        states.insert(columns[0][6..].to_owned(), (columns[1].to_owned(), columns[2].to_owned()));
    }
    Ok(states)
}

fn rust_crate(library: &str) -> String {
    if library == "rays_diagnostic" {
        "rays_diagnostic".to_owned()
    } else {
        library.replace('-', "_")
    }
}

fn test_or_tool_library(name: &str) -> bool {
    matches!(
        name,
        "metal_build_config"
            | "sdl3_discovery"
            | "ogpu_conformance"
            | "golden_fixtures"
            | "scene_execution_fixtures"
            | "workspace_parity"
            | "lisp_sop"
            | "rdk_test_support"
            | "flow_ir_test_support"
            | "test_support"
    ) || name.ends_with("_test_support")
}

fn discover_libraries(root: &Path) -> Result<Vec<DuneLibrary>, String> {
    let tiers = seed_tiers(root)?;
    let mut dune_files = Vec::new();
    walk_dune(&root.join("lib"), &mut dune_files)?;
    dune_files.sort();
    let mut libraries = Vec::new();
    let mut unsupported = Vec::new();
    let mut unmapped = Vec::new();
    let mut warnings = Vec::new();

    for dune_file in dune_files {
        let text = fs::read_to_string(&dune_file)
            .map_err(|e| format!("{}: {e}", dune_file.display()))?;
        let forms = parse_sexps(&text).map_err(|e| format!("{}: {e}", dune_file.display()))?;
        let dir = dune_file.parent().ok_or("Dune file has no parent")?;
        let mut include_subdirs = false;
        for form in &forms {
            match list_head(form) {
                Some("include_subdirs") => {
                    if matches!(form, Sexp::List(items) if items.get(1).and_then(atom) == Some("unqualified")) {
                        include_subdirs = true;
                    } else {
                        unsupported.push(format!(
                            "{}: only `(include_subdirs unqualified)` is supported",
                            dune_file.strip_prefix(root).unwrap_or(&dune_file).display()
                        ));
                    }
                }
                Some("include") => unsupported.push(format!(
                    "{}: top-level (include) is unsupported",
                    dune_file.strip_prefix(root).unwrap_or(&dune_file).display()
                )),
                _ => {}
            }
        }
        let source_map = source_modules(dir, root, include_subdirs)?;
        let dir_relative = dir
            .strip_prefix(root)
            .map_err(|e| e.to_string())?
            .to_string_lossy()
            .replace('\\', "/");
        let stanzas: Vec<_> = forms
            .iter()
            .filter(|form| matches!(list_head(form), Some("library" | "test" | "tests" | "executable" | "executables")))
            .collect();
        let non_library_modules: BTreeSet<_> = stanzas
            .iter()
            .filter(|stanza| list_head(stanza) != Some("library"))
            .flat_map(|stanza| all_stanza_modules(stanza))
            .collect();


        for stanza in stanzas.into_iter().filter(|s| list_head(s) == Some("library")) {
            let Some(name) = field_atom(stanza, "name") else {
                unsupported.push(format!("{}: library stanza has no literal name", dune_file.display()));
                continue;
            };
            let crate_name = rust_crate(&name);
            let Some(tier) = tiers.get(&crate_name).cloned() else {
                if name != "ogpu_native" && !test_or_tool_library(&name) {
                    unmapped.push(format!("{} ({name})", dune_file.strip_prefix(root).unwrap_or(&dune_file).display()));
                }
                continue;
            };
            let wrapped = match field_atom(stanza, "wrapped").as_deref() {
                Some("true") => true,
                Some("false") => false,
                None if field(stanza, "wrapped").is_none() => true,
                _ => {
                    unsupported.push(format!("{} library {name}: wrapped must be a literal true or false", dune_file.strip_prefix(root).unwrap_or(&dune_file).display()));
                    true
                }
            };
            if field(stanza, "preprocess").is_some() {
                warnings.push(format!(
                    "{} library {name}: Dune preprocess is not applied; module edges use raw-source ocamldep output",
                    dune_file.strip_prefix(root).unwrap_or(&dune_file).display()
                ));
            }
            // Virtual and implementation fields affect linkage, not source module identity.
            let mut default_modules = source_map.clone();
            default_modules.retain(|module, _| !non_library_modules.contains(module));
            let mut module_names = module_spec(field(stanza, "modules"), &default_modules)
                .map_err(|e| format!("{} library {name}: {e}", dune_file.display()))?;
            if let Some(expr) = field(stanza, "modules_without_implementation") {
                let mut names = Vec::new();
                atoms(expr, &mut names);
                for module in names.into_iter().skip(1) {
                    if module.starts_with(':') || module.starts_with('%') {
                        return Err(format!("{} library {name}: unsupported modules_without_implementation selector {module}", dune_file.display()));
                    }
                    let module = source_module_name(&module, &source_map);
                    if !module_names.contains(&module) {
                        module_names.push(module);
                    }
                }
            }
            module_names.sort();
            module_names.dedup();
            let mut modules = Vec::new();
            for module in module_names {
                let mut sources = source_map.get(&module).cloned().unwrap_or_default();
                let expected_ml = format!("{dir_relative}/{}.ml", module.to_ascii_lowercase());
                if !sources.iter().any(|path| path.ends_with(".ml")) {
                    let generated = format!("_build/default/{expected_ml}");
                    if root.join(&generated).is_file() && !sources.contains(&generated) {
                        sources.push(generated);
                    }
                }
                if sources.is_empty() {
                    let expected_mli = expected_ml.trim_end_matches(".ml").to_owned() + ".mli";
                    let generated = format!("_build/default/{expected_mli}");
                    if root.join(&generated).is_file() {
                        sources.push(generated);
                    }
                }
                if sources.is_empty() {
                    eprintln!("port: {} library {name} module {module}: no checked-in or built .ml/.mli source (kept with 0 lines and no discovered dependencies)", dune_file.strip_prefix(root).unwrap_or(&dune_file).display());
                }
                let ocaml_file = source_map
                    .get(&module)
                    .and_then(|paths| paths.iter().find(|path| path.ends_with(".ml")).or_else(|| paths.first()))
                    .cloned()
                    .unwrap_or(expected_ml);
                modules.push(DuneModule { name: module, ocaml_file, sources });
            }
            let mut libraries_field = Vec::new();
            if let Some(expr) = field(stanza, "libraries") {
                atoms(expr, &mut libraries_field);
                libraries_field.drain(..1);
                if let Sexp::List(items) = expr {
                    if items.iter().skip(1).any(|item| matches!(item, Sexp::List(_))) {
                        unsupported.push(format!("{} library {name}: conditional/nested libraries expression", dune_file.strip_prefix(root).unwrap_or(&dune_file).display()));
                    }
                }
            }
            if let Some(expr) = field(stanza, "implements") {
                let mut implemented = Vec::new();
                atoms(expr, &mut implemented);
                if implemented.iter().skip(1).any(|value| value.starts_with(':') || value.starts_with('%')) {
                    unsupported.push(format!("{} library {name}: dynamic implements expression", dune_file.strip_prefix(root).unwrap_or(&dune_file).display()));
                }
                libraries_field.extend(implemented.into_iter().skip(1));
            }
            let mut implicit_modules = Vec::new();
            for key in ["flags", "ocamlc_flags", "ocamlopt_flags"] {
                if let Some(expr) = field(stanza, key) {
                    let mut flags = Vec::new();
                    atoms(expr, &mut flags);
                    let mut index = 1;
                    while index < flags.len() {
                        if flags[index] == "-open" {
                            match flags.get(index + 1) {
                                Some(module) if !module.starts_with('%') && !module.starts_with(':') => {
                                    implicit_modules.push(module.clone());
                                    index += 2;
                                }
                                _ => {
                                    unsupported.push(format!("{} library {name}: dynamic or missing module after -open", dune_file.strip_prefix(root).unwrap_or(&dune_file).display()));
                                    index += 1;
                                }
                            }
                        } else {
                            index += 1;
                        }
                    }
                }
            }
            implicit_modules.sort();
            implicit_modules.dedup();
            libraries.push(DuneLibrary {
                name,
                wrapped,
                modules,
                implicit_modules,
                libraries: libraries_field,
                crate_name,
                tier,
            });
        }
    }

    if !unmapped.is_empty() {
        unsupported.push(format!("product/library tier not present in rs/port/progress.tsv: {}", unmapped.join(", ")));
    }
    if !unsupported.is_empty() {
        return Err(format!("Dune constructs or libraries the planner cannot map:\n  {}", unsupported.join("\n  ")));
    }
    for warning in warnings {
        eprintln!("port: warning: {warning}");
    }
    libraries.sort_by(|a, b| (&a.tier, &a.crate_name, &a.name).cmp(&(&b.tier, &b.crate_name, &b.name)));
    Ok(libraries)
}

fn line_count(root: &Path, source: &str) -> Result<usize, String> {
    let text = fs::read_to_string(root.join(source)).map_err(|e| format!("{source}: {e}"))?;
    Ok(text.lines().count())
}

fn normalized_path(path: &str) -> String {
    let path = path.replace('\\', "/");
    let path = path.trim_start_matches("./");
    let path = path
        .rfind("/lib/")
        .map(|index| &path[index + 1..])
        .unwrap_or(path);
    path.to_ascii_lowercase()
}

fn source_key(path: &str) -> String {
    normalized_path(path)
}

fn run_ocamldep(root: &Path, files: &[String]) -> Result<HashMap<String, BTreeSet<String>>, String> {
    let mut output = HashMap::new();
    for batch in files.chunks(OCAMLDEP_BATCH) {
        let result = Command::new("ocamldep")
            .arg("-modules")
            .args(batch)
            .current_dir(root)
            .output()
            .map_err(|e| format!("could not execute `ocamldep -modules ...`: {e}"))?;
        if !result.status.success() {
            return Err(format!(
                "`ocamldep -modules {}` failed: {}",
                batch.join(" "),
                String::from_utf8_lossy(&result.stderr).trim()
            ));
        }
        let stdout = String::from_utf8(result.stdout).map_err(|e| e.to_string())?;
        for line in stdout.lines() {
            let (path, modules) = line
                .rsplit_once(':')
                .ok_or_else(|| format!("unrecognized ocamldep output: {line}"))?;
            let key = normalized_path(path.trim());
            let deps = modules.split_whitespace().map(str::to_owned).collect();
            output.insert(key, deps);
        }
    }
    Ok(output)
}

fn dependency_closure(name: &str, by_name: &HashMap<String, &DuneLibrary>) -> BTreeSet<String> {
    let mut seen = BTreeSet::new();
    let mut pending = vec![name.to_owned()];
    while let Some(current) = pending.pop() {
        if !seen.insert(current.clone()) {
            continue;
        }
        if let Some(lib) = by_name.get(&current) {
            pending.extend(lib.libraries.iter().cloned());
        }
    }
    seen
}

fn make_rows(root: &Path) -> Result<Vec<Row>, String> {
    let libraries = discover_libraries(root)?;
    let mut rows = Vec::new();
    let by_name: HashMap<_, _> = libraries.iter().map(|lib| (lib.name.clone(), lib)).collect();

    for lib in &libraries {
        for module in &lib.modules {
            let id = module_id(lib, &module.name);
            let source = module.ocaml_file.clone();
            let rust_file = rust_module_file(lib, &module.name);
            let mut lines = 0;
            for path in &module.sources {
                lines += line_count(root, path)?;
            }
            rows.push(Row {
                id,
                ocaml_file: source,
                rust_file,
                crate_name: module_crate(lib, &module.name).to_owned(),
                depends_on: Vec::new(),
                lines,
                tier: lib.tier.clone(),
                status: "pending".to_owned(),
                owner: String::new(),
            });
        }
    }
    rows.sort_by(|a, b| {
        let tier = |name: &str| name.strip_prefix('W').and_then(|n| n.parse::<u8>().ok()).unwrap_or(u8::MAX);
        (tier(&a.tier), &a.crate_name, &a.ocaml_file, &a.id)
            .cmp(&(tier(&b.tier), &b.crate_name, &b.ocaml_file, &b.id))
    });

    let mut source_paths: Vec<_> = libraries
        .iter()
        .flat_map(|lib| lib.modules.iter())
        .flat_map(|module| module.sources.iter().cloned())
        .collect();
    source_paths.sort();
    source_paths.dedup();
    let ocaml_deps = run_ocamldep(root, &source_paths)?;
    let row_by_id: HashMap<_, _> = rows.iter().enumerate().map(|(index, row)| (row.id.clone(), index)).collect();
    let mut bad = Vec::new();
    for lib in &libraries {
        let reachable = dependency_closure(&lib.name, &by_name);
        for module in &lib.modules {
            let id = module_id(lib, &module.name);
            let mut dependencies = BTreeSet::new();
            for source in &module.sources {
                let Some(names) = ocaml_deps.get(&source_key(source)) else {
                    bad.push(format!("ocamldep did not report {}", source));
                    continue;
                };
                let mut names = names.clone();
                names.extend(lib.implicit_modules.iter().cloned());
                for name in &names {
                    let mut candidates = Vec::new();
                    let mut wrapper_dependencies = BTreeSet::new();
                    for candidate_lib in &libraries {
                        if !reachable.contains(&candidate_lib.name) {
                            continue;
                        }
                        if let Some(candidate) = candidate_lib.modules.iter().find(|candidate| candidate.name.eq_ignore_ascii_case(name)) {
                            candidates.push(module_id(candidate_lib, &candidate.name));
                        } else if candidate_lib.wrapped && dune_module_name(&candidate_lib.name).as_str() == name.as_str() {
                            wrapper_dependencies.extend(
                                candidate_lib
                                    .modules
                                    .iter()
                                    .map(|candidate| module_id(candidate_lib, &candidate.name))
                                    .filter(|dependency| dependency != &id),
                            );
                        }
                    }
                    candidates.sort();
                    if candidates.is_empty() {
                        dependencies.extend(wrapper_dependencies);
                    }
                    candidates.dedup();
                    // ponytail: `ocamldep` omits scope, so depend on every reachable match to avoid a false mapping; use Dune's resolved graph if this over-orders the queue.
                    candidates.retain(|candidate| candidate != &id);
                    if candidates.len() > 1 {
                        eprintln!(
                            "port: warning: {id} references ambiguous module {name}; depending on all: {}",
                            candidates.join(", ")
                        );
                    }
                    dependencies.extend(candidates);
                }
            }
            if let Some(index) = row_by_id.get(&id) {
                rows[*index].depends_on = dependencies.into_iter().collect();
            }
        }
    }
    if !bad.is_empty() {
        return Err(format!("module dependency mapping failed:\n  {}", bad.join("\n  ")));
    }
    let states = initial_states(root)?;
    for row in &mut rows {
        if let Some((status, owner)) = states.get(&row.crate_name) {
            row.status = status.clone();
            row.owner = owner.clone();
        }
    }
    Ok(rows)
}

fn parse_queue(content: &str) -> Result<Vec<Row>, String> {
    let mut lines = content.lines();
    if lines.next() != Some(HEADER) {
        return Err("queue header does not match the required TSV columns".to_owned());
    }
    let mut rows = Vec::new();
    let mut ids = BTreeSet::new();
    for (line_number, line) in lines.enumerate() {
        if line.is_empty() {
            continue;
        }
        let columns: Vec<_> = line.split('\t').collect();
        if columns.len() != 9 {
            return Err(format!("queue line {} has {} columns, expected 9", line_number + 2, columns.len()));
        }
        if !ids.insert(columns[0].to_owned()) {
            return Err(format!("duplicate queue id {}", columns[0]));
        }
        let lines = columns[5]
            .parse::<usize>()
            .map_err(|e| format!("invalid line count for {}: {e}", columns[0]))?;
        rows.push(Row {
            id: columns[0].to_owned(),
            ocaml_file: columns[1].to_owned(),
            rust_file: columns[2].to_owned(),
            crate_name: columns[3].to_owned(),
            depends_on: if columns[4].is_empty() {
                Vec::new()
            } else {
                columns[4].split(',').map(str::to_owned).collect()
            },
            lines,
            tier: columns[6].to_owned(),
            status: columns[7].to_owned(),
            owner: columns[8].to_owned(),
        });
    }
    Ok(rows)
}

fn render(rows: &[Row]) -> String {
    let mut result = String::from(HEADER);
    result.push('\n');
    for row in rows {
        result.push_str(&[
            row.id.as_str(),
            row.ocaml_file.as_str(),
            row.rust_file.as_str(),
            row.crate_name.as_str(),
        ].join("\t"));
        result.push('\t');
        result.push_str(&row.depends_on.join(","));
        result.push('\t');
        result.push_str(&row.lines.to_string());
        result.push('\t');
        result.push_str(&row.tier);
        result.push('\t');
        result.push_str(&row.status);
        result.push('\t');
        result.push_str(&row.owner);
        result.push('\n');
    }
    result
}

struct QueueLock(PathBuf);

impl QueueLock {
    fn acquire(queue: &Path) -> Result<Self, String> {
        let lock = queue.with_extension("tsv.lock");
        let deadline = Instant::now() + Duration::from_secs(60);
        loop {
            match fs::create_dir(&lock) {
                Ok(()) => return Ok(Self(lock)),
                Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {
                    if Instant::now() >= deadline {
                        return Err(format!("timed out waiting for {}; remove it only if no port command is running", lock.display()));
                    }
                    thread::sleep(Duration::from_millis(50));
                }
                Err(error) => return Err(format!("{}: {error}", lock.display())),
            }
        }
    }
}

impl Drop for QueueLock {
    fn drop(&mut self) {
        let _ = fs::remove_dir(&self.0);
    }
}

fn atomic_write(queue: &Path, contents: &str) -> Result<(), String> {
    let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap_or_default().as_nanos();
    let temp = queue.with_extension(format!("tsv.tmp-{}-{now}", process::id()));
    let result = (|| {
        let mut file = OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&temp)
            .map_err(|e| format!("{}: {e}", temp.display()))?;
        file.write_all(contents.as_bytes()).map_err(|e| e.to_string())?;
        file.sync_all().map_err(|e| e.to_string())?;
        drop(file);
        fs::rename(&temp, queue).map_err(|e| format!("rename {} to {}: {e}", temp.display(), queue.display()))
    })();
    if result.is_err() {
        let _ = fs::remove_file(temp);
    }
    result
}

fn with_queue<T>(root: &Path, f: impl FnOnce(&Path) -> Result<T, String>) -> Result<T, String> {
    let queue = queue_path(root);
    fs::create_dir_all(queue.parent().ok_or("queue has no parent")?).map_err(|e| e.to_string())?;
    let _lock = QueueLock::acquire(&queue)?;
    f(&queue)
}

fn plan(root: &Path) -> Result<(), String> {
    let mut rows = make_rows(root)?;
    with_queue(root, |queue| {
        if queue.exists() {
            let existing = parse_queue(&fs::read_to_string(queue).map_err(|e| e.to_string())?)?;
            let state: HashMap<_, _> = existing
                .into_iter()
                .map(|row| (row.id, (row.status, row.owner)))
                .collect();
            for row in &mut rows {
                if let Some((status, owner)) = state.get(&row.id) {
                    row.status = status.clone();
                    row.owner = owner.clone();
                }
            }
        }
        atomic_write(queue, &render(&rows))
    })?;
    print!("{}", render(&rows));
    Ok(())
}

fn state_is_active(status: &str) -> bool {
    matches!(status, "claimed" | "in_progress" | "in_review")
}
fn module_source_available(root: &Path, row: &Row) -> bool {
    root.join(&row.ocaml_file).is_file()
        || root.join("_build/default").join(&row.ocaml_file).is_file()
}

fn blocked_dependency<'a>(row: &'a Row, by_id: &HashMap<&str, &Row>) -> Option<&'a str> {
    row.depends_on
        .iter()
        .find(|dependency| !by_id.get(dependency.as_str()).is_some_and(|entry| entry.status == "done"))
        .map(String::as_str)
}

fn next(root: &Path) -> Result<(), String> {
    with_queue(root, |queue| {
        if !queue.exists() {
            return Err("queue.tsv does not exist; run `cargo run -p port -- plan` first".to_owned());
        }
        let rows = parse_queue(&fs::read_to_string(queue).map_err(|e| e.to_string())?)?;
        let by_id: HashMap<_, _> = rows.iter().map(|row| (row.id.as_str(), row)).collect();
        if let Some(row) = rows.iter().find(|row| {
            row.status == "pending"
                && module_source_available(root, row)
                && blocked_dependency(row, &by_id).is_none()
        }) {
            println!("{}", render(std::slice::from_ref(row)).lines().nth(1).unwrap_or_default());
        } else {
            println!("no ready modules");
        }
        Ok(())
    })
}

fn update(root: &Path, action: &str, id: &str, owner: &str, reason: Option<String>) -> Result<(), String> {
    if owner.is_empty() || owner.chars().any(|ch| matches!(ch, '\t' | '\r' | '\n')) {
        return Err("owner must be nonempty and contain no tabs or line breaks".to_owned());
    }
    with_queue(root, |queue| {
        if !queue.exists() {
            return Err("queue.tsv does not exist; run `cargo run -p port -- plan` first".to_owned());
        }
        let mut rows = parse_queue(&fs::read_to_string(queue).map_err(|e| e.to_string())?)?;
        let index = rows.iter().position(|row| row.id == id).ok_or_else(|| format!("unknown module id {id}"))?;
        let row = &rows[index];
        match action {
            "claim" => {
                if row.status != "pending" || !row.owner.is_empty() {
                    return Err(format!("cannot claim {id}: state is {} with owner {:?}", row.status, row.owner));
                }
                if !module_source_available(root, row) {
                    return Err(format!("cannot claim {id}: source {} is unavailable", row.ocaml_file));
                }
                let by_id: HashMap<_, _> = rows.iter().map(|row| (row.id.as_str(), row)).collect();
                if let Some(blocked) = blocked_dependency(row, &by_id) {
                    return Err(format!("cannot claim {id}: dependency {blocked} is not done"));
                }
                rows[index].status = "claimed".to_owned();
                rows[index].owner = owner.to_owned();
            }
            "done" | "fail" => {
                if !state_is_active(&row.status) || row.owner != owner {
                    return Err(format!("cannot {action} {id}: state is {} and owner is {:?}", row.status, row.owner));
                }
                if action == "done" {
                    rows[index].status = "done".to_owned();
                } else {
                    let reason = reason.unwrap_or_default();
                    let reason: String = reason
                        .chars()
                        .map(|ch| if matches!(ch, '\t' | '\r' | '\n') { ' ' } else { ch })
                        .collect();
                    if reason.trim().is_empty() {
                        return Err("fail requires a nonempty reason".to_owned());
                    }
                    rows[index].status = format!("failed: {}", reason.trim());
                }
            }
            _ => return Err(format!("unknown update action {action}")),
        }
        atomic_write(queue, &render(&rows))
    })
}

fn usage() -> &'static str {
    "usage: cargo run -p port -- plan | next | claim <id> <owner> | done <id> <owner> | fail <id> <owner> <reason>"
}

fn run() -> Result<(), String> {
    let mut args = env::args().skip(1);
    let action = args.next().ok_or_else(|| usage().to_owned())?;
    let root = root()?;
    match action.as_str() {
        "plan" if args.next().is_none() => plan(&root),
        "next" if args.next().is_none() => next(&root),
        "claim" | "done" => {
            let id = args.next().ok_or_else(|| usage().to_owned())?;
            let owner = args.next().ok_or_else(|| usage().to_owned())?;
            if args.next().is_some() {
                return Err(usage().to_owned());
            }
            update(&root, &action, &id, &owner, None)
        }
        "fail" => {
            let id = args.next().ok_or_else(|| usage().to_owned())?;
            let owner = args.next().ok_or_else(|| usage().to_owned())?;
            let reason = args.collect::<Vec<_>>().join(" ");
            update(&root, &action, &id, &owner, Some(reason))
        }
        _ => Err(usage().to_owned()),
    }
}

fn main() {
    if let Err(error) = run() {
        eprintln!("port: {error}");
        process::exit(2);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dune_module_selection_expands_standard_and_exclusions() {
        let default = BTreeMap::from([
            ("Foo".to_owned(), Vec::new()),
            ("Test_main".to_owned(), Vec::new()),
        ]);
        let expr = Sexp::List(vec![
            Sexp::Atom("modules".to_owned()),
            Sexp::Atom(":standard".to_owned()),
            Sexp::Atom("Extra".to_owned()),
            Sexp::Atom("\\".to_owned()),
            Sexp::Atom("Test_main".to_owned()),
        ]);
        assert_eq!(module_spec(Some(&expr), &default).unwrap(), vec![String::from("Extra"), String::from("Foo")]);
    }

    #[test]
    fn unqualified_subdir_scan_discovers_nested_modules() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let root = env::temp_dir().join(format!(
            "rays-port-source-modules-{}-{unique}",
            std::process::id()
        ));
        let nested = root.join("nested");
        std::fs::create_dir_all(&nested).unwrap();
        std::fs::write(nested.join("found.ml"), "let value = 1\n").unwrap();
        assert!(source_modules(&root, &root, false).unwrap().is_empty());
        let modules = source_modules(&root, &root, true);
        std::fs::remove_dir_all(&root).unwrap();
        assert_eq!(
            modules.unwrap().get("Found").unwrap(),
            &vec!["nested/found.ml".to_owned()]
        );
    }


    #[test]
    fn flow_split_dependencies_block_until_upstream_rows_are_done() {
        let flow = DuneLibrary {
            name: "flow".to_owned(),
            wrapped: true,
            modules: Vec::new(),
            libraries: Vec::new(),
            implicit_modules: Vec::new(),
            crate_name: "flow".to_owned(),
            tier: "W2".to_owned(),
        };

        let defaults = BTreeMap::from([("Ty".to_owned(), Vec::new())]);
        let selection = Sexp::List(vec![
            Sexp::Atom("modules".to_owned()),
            Sexp::Atom("ty".to_owned()),
        ]);
        assert_eq!(module_spec(Some(&selection), &defaults).unwrap(), vec![String::from("Ty")]);
        let row = |module: &str, depends_on: Vec<String>| Row {
            id: module_id(&flow, module),
            ocaml_file: format!("lib/flow/{}.ml", module.to_ascii_lowercase()),
            rust_file: rust_module_file(&flow, module),
            crate_name: module_crate(&flow, module).to_owned(),
            depends_on,
            lines: 0,
            tier: "W2".to_owned(),
            status: "pending".to_owned(),
            owner: String::new(),
        };
        let mut syntax = row("Ty", Vec::new());
        let mut check = row("Workspace", vec![syntax.id.clone()]);
        let eval = row("Eval", vec![check.id.clone()]);

        assert_eq!(syntax.crate_name, "flow_syntax");
        assert_eq!(syntax.rust_file, "rs/crates/flow_syntax/src/ty.rs");
        assert_eq!(check.crate_name, "flow_check");
        assert_eq!(check.rust_file, "rs/crates/flow_check/src/workspace.rs");
        assert_eq!(eval.crate_name, "flow_eval");
        assert_eq!(eval.rust_file, "rs/crates/flow_eval/src/eval.rs");
        assert_eq!(rust_module_file(&flow, "Macro"), "rs/crates/flow_syntax/src/macros.rs");

        {
            let by_id = HashMap::from([
                (syntax.id.as_str(), &syntax),
                (check.id.as_str(), &check),
                (eval.id.as_str(), &eval),
            ]);
            assert_eq!(blocked_dependency(&check, &by_id), Some("flow:ty"));
            assert_eq!(blocked_dependency(&eval, &by_id), Some("flow:workspace"));
        }
        syntax.status = "done".to_owned();
        {
            let by_id = HashMap::from([
                (syntax.id.as_str(), &syntax),
                (check.id.as_str(), &check),
                (eval.id.as_str(), &eval),
            ]);
            assert_eq!(blocked_dependency(&check, &by_id), None);
            assert_eq!(blocked_dependency(&eval, &by_id), Some("flow:workspace"));
        }
        check.status = "done".to_owned();
        let by_id = HashMap::from([
            (syntax.id.as_str(), &syntax),
            (check.id.as_str(), &check),
            (eval.id.as_str(), &eval),
        ]);
        assert_eq!(blocked_dependency(&eval, &by_id), None);
    }

    #[test]
    fn queue_rows_round_trip_and_require_exact_header() {
        let row = Row {
            id: "rays_math:vec3".to_owned(),
            ocaml_file: "lib/rays_math/vec3.ml".to_owned(),
            rust_file: "rs/crates/rays_math/src/vec3.rs".to_owned(),
            crate_name: "rays_math".to_owned(),
            depends_on: vec!["rays_math:vec2".to_owned()],
            lines: 12,
            tier: "W1".to_owned(),
            status: "pending".to_owned(),
            owner: String::new(),
        };
        let text = render(std::slice::from_ref(&row));
        assert_eq!(parse_queue(&text).unwrap(), vec![row]);
        assert!(parse_queue("id\tstatus\n").is_err());
    }
}
