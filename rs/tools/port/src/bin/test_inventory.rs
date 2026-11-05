//! Reproducible census, not a test-coverage oracle. Run from any directory.
use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
};

fn files(dir: &Path, extension: &str, out: &mut Vec<PathBuf>) -> std::io::Result<()> {
    for entry in fs::read_dir(dir)? {
        let path = entry?.path();
        if path.is_dir() {
            files(&path, extension, out)?;
        } else if path.extension().is_some_and(|e| e == extension) {
            out.push(path);
        }
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .find(|p| p.join("lib").is_dir() && p.join("rs").is_dir())
        .unwrap();
    let inventory = fs::read_to_string(root.join("rs/port/test_inventory.md"))?;
    let mut declared = BTreeMap::new();
    let mut section = "";
    for line in inventory.lines() {
        if line.starts_with("## MISSING (") {
            section = "MISSING";
        } else if line.starts_with("## PARTIAL (") {
            section = "PARTIAL";
        } else if line.starts_with("## ") {
            section = "";
        }
        if !section.is_empty() {
            if let Some(path) = line.split('`').nth(1).filter(|s| s.starts_with("lib/")) {
                assert!(
                    declared.insert(path.to_owned(), section).is_none(),
                    "duplicate {path}"
                );
            }
        }
    }
    let mut ocaml = Vec::new();
    files(&root.join("lib"), "ml", &mut ocaml)?;
    ocaml.retain(|p| {
        p.file_name()
            .unwrap()
            .to_string_lossy()
            .starts_with("test_")
    });
    ocaml.sort();
    let mut rust = Vec::new();
    files(&root.join("rs/crates"), "rs", &mut rust)?;
    rust.sort();
    let rust: Vec<_> = rust
        .iter()
        .map(|p| Ok((p, fs::read_to_string(p)?)))
        .collect::<Result<_, std::io::Error>>()?;
    let mut output = String::from("ocaml_file\tdeclared_status\taudit_status\trust_candidates\n");
    let mut totals = BTreeMap::new();
    for path in &ocaml {
        let relative = path
            .strip_prefix(root)?
            .to_string_lossy()
            .replace('\\', "/");
        let name = path.file_name().unwrap().to_str().unwrap();
        let stem = path.file_stem().unwrap().to_str().unwrap();
        let short = stem.strip_prefix("test_").unwrap();
        // Candidates only: a matching name or comment proves neither registration nor assertion parity.
        let candidates: Vec<_> = rust
            .iter()
            .filter(|(p, text)| {
                let rs_stem = p.file_stem().unwrap().to_str().unwrap();
                rs_stem == stem || rs_stem == short || text.contains(name)
            })
            .map(|(p, _)| {
                p.strip_prefix(root)
                    .unwrap()
                    .to_string_lossy()
                    .replace('\\', "/")
            })
            .collect();
        let status = declared.get(&relative).copied().unwrap_or("UNVERIFIED");
        *totals.entry(status).or_insert(0usize) += 1;
        output.push_str(&format!(
            "{relative}\t{status}\tNEEDS_ASSERTION_REVIEW\t{}\n",
            candidates.join(";")
        ));
    }
    for path in declared.keys() {
        assert!(
            ocaml.iter().any(|p| p
                .strip_prefix(root)
                .unwrap()
                .to_string_lossy()
                .replace('\\', "/")
                == *path),
            "stale inventory path: {path}"
        );
    }
    let destination = root.join("rs/port/test_inventory.tsv");
    if std::env::args().any(|a| a == "--check") {
        assert_eq!(
            fs::read_to_string(&destination)?,
            output,
            "test inventory is stale; run cargo run -p port --bin test_inventory"
        );
    } else {
        fs::write(destination, output)?;
    }
    println!(
        "{} OCaml files: {totals:?}; candidates are not coverage evidence",
        ocaml.len()
    );
    Ok(())
}
