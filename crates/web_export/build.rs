use std::{env, fs, path::{Path, PathBuf}};

fn sources(root: &Path, directory: &Path, files: &mut Vec<String>) {
    println!("cargo:rerun-if-changed={}", directory.display());
    for entry in fs::read_dir(directory).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() { sources(root, &path, files); }
        else if path.extension().is_some_and(|extension| extension == "rays") {
            files.push(path.strip_prefix(root).unwrap().to_str().unwrap().replace('\\', "/"));
        }
    }
}

fn main() {
    let root = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("../..");
    let mut files = Vec::new();
    for directory in ["crates", "sketches", "specification"] { sources(&root, &root.join(directory), &mut files); }
    // Source Atlas reads a table an indexer generates outside Git: its check differs by checkout.
    files.retain(|file| file != "sketches/source_atlas.rays");
    files.sort();
    let mut generated = format!("const FILES: &[&str] = &{:?};\n", files);
    for file in files {
        let name: String = file.chars().map(|c| if c.is_ascii_alphanumeric() { c } else { '_' }).collect();
        generated.push_str(&format!("#[test] fn {name}() {{ golden({file:?}) }}\n"));
    }
    fs::write(PathBuf::from(env::var("OUT_DIR").unwrap()).join("e2e_sources.rs"), generated).unwrap();
}
