use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

// Locate SDL3: $SDL3_DIR (include/ and lib/ beneath it), then pkg-config, then the MSYS2 prefix.
fn run_pkg_config(argument: &str) -> String {
    println!("cargo:rerun-if-env-changed=SDL3_DIR");
    println!("cargo:rerun-if-env-changed=PKG_CONFIG");
    let prefix = |dir: &str| {
        if argument == "--cflags" {
            format!("-I{dir}/include")
        } else {
            format!("-L{dir}/lib -lSDL3")
        }
    };
    if let Some(dir) = env::var_os("SDL3_DIR") {
        return prefix(&dir.to_string_lossy().replace('\\', "/"));
    }
    let program = env::var_os("PKG_CONFIG").unwrap_or_else(|| "pkg-config".into());
    if let Ok(output) = Command::new(program).arg(argument).arg("sdl3").output() {
        if output.status.success() {
            return String::from_utf8(output.stdout).expect("pkg-config output must be UTF-8");
        }
    }
    let msys = ["C:/msys64/mingw64", "C:/msys64/ucrt64"]
        .into_iter()
        .find(|dir| std::path::Path::new(dir).join("include/SDL3/SDL.h").exists());
    match msys {
        Some(dir) => prefix(dir),
        None => build_failure(format!("SDL3 not found: set SDL3_DIR, or install pkg-config with the SDL3 development package")),
    }
}

fn words(value: &str) -> Vec<&str> {
    value.split_whitespace().collect()
}

fn main() {
    let root = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap());
    let out = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let lock_path = root.join("../../packaging/sdl3.lock");
    let abi_path = root.join("abi.sexp");
    println!("cargo:rerun-if-changed={}", lock_path.display());
    println!("cargo:rerun-if-changed={}", abi_path.display());
    println!("cargo:rerun-if-changed=src/bridge.c");
    println!("cargo:rerun-if-changed=src/dialog.c");
    println!("cargo:rerun-if-changed=src/dialog.h");
    println!("cargo:rerun-if-changed=src/test_helpers.c");
    println!("cargo:rerun-if-changed=src/platform.c");

    let lock = fs::read_to_string(&lock_path).expect("read packaging/sdl3.lock");
    let entry = lock.lines()
        .map(str::trim)
        .find(|line| line.starts_with("((sdl3 "))
        .expect("sdl3 entry in packaging/sdl3.lock");
    let lock_words: Vec<_> = words(entry);
    let floor = lock_words
        .windows(2)
        .find(|pair| pair[0] == "(floor")
        .and_then(|pair| pair[1].strip_suffix(')'))
        .expect("SDL3 floor in packaging/sdl3.lock");
    let version: Vec<u32> = floor
        .split('.')
        .map(|part| part.parse().expect("numeric SDL3 floor"))
        .collect();
    assert_eq!(version.len(), 3, "SDL3 floor must be major.minor.patch");

    let abi = fs::read_to_string(&abi_path).expect("read SDL3 ABI manifest");
    let mut header = format!(
        "#include <SDL3/SDL.h>\n#include <stddef.h>\n\
         _Static_assert(SDL_MAJOR_VERSION == {major}, \"SDL3 major version changed\");\n\
         _Static_assert(SDL_VERSION >= SDL_VERSIONNUM({major}, {minor}, {patch}), \"SDL3 headers are below packaging/sdl3.lock floor\");\n",
        major = version[0],
        minor = version[1],
        patch = version[2]
    );
    for line in abi.lines().map(str::trim).filter(|line| !line.starts_with(';')) {
        let fields: Vec<_> = line
            .split(|character: char| character == '(' || character == ')' || character.is_whitespace())
            .filter(|field| !field.is_empty())
            .collect();
        if fields.first().is_none_or(|name| !name.starts_with("SDL_")) {
            continue;
        }
        assert!(fields.len() >= 3, "malformed SDL ABI entry: {line}");
        let name = fields[0];
        let size: usize = fields[1].parse().expect("SDL ABI size");
        let align: usize = fields[2].parse().expect("SDL ABI alignment");
        header.push_str(&format!(
            "_Static_assert(sizeof({name}) == {size}, \"{name} size changed\");\n\
             _Static_assert(_Alignof({name}) == {align}, \"{name} alignment changed\");\n"
        ));
        for pair in fields[3..].chunks_exact(2) {
            let offset: usize = pair[1].parse().expect("SDL ABI field offset");
            header.push_str(&format!(
                "_Static_assert(offsetof({name}, {field}) == {offset}, \"{name}.{field} moved\");\n",
                field = pair[0]
            ));
        }
    }
    fs::write(out.join("sdl3_abi.h"), header).expect("write SDL3 ABI assertions");

    let target = env::var("CARGO_CFG_TARGET_OS").unwrap();
    let mut c = cc::Build::new();
    c.files(["src/bridge.c", "src/dialog.c", "src/platform.c", "src/test_helpers.c"])
        .include("src")
        .include(&out)
        .std("c11")
        .warnings(true)
        .flag_if_supported("-Werror");
    let cflags = run_pkg_config("--cflags");
    for flag in words(&cflags) {
        if let Some(path) = flag.strip_prefix("-I") {
            c.include(path);
        } else if let Some(definition) = flag.strip_prefix("-D") {
            c.define(definition.split('=').next().unwrap(), definition.split_once('=').map(|(_, value)| value));
        } else {
            c.flag(flag);
        }
    }
    if target == "macos" {
        c.define("RAYS_SDL3_MACOS", None).flag_if_supported("-fblocks");
    }
    c.compile("rays_sdl3_sys");

    let libs = run_pkg_config("--libs");
    let library_flags: Vec<_> = words(&libs);
    let mut index = 0;
    while index < library_flags.len() {
        let flag = library_flags[index];
        if let Some(path) = flag.strip_prefix("-L") {
            println!("cargo:rustc-link-search=native={path}");
        } else if let Some(path) = flag.strip_prefix("-F") {
            println!("cargo:rustc-link-search=framework={path}");
        } else if let Some(library) = flag.strip_prefix("-l") {
            println!("cargo:rustc-link-lib={library}");
        } else if flag == "-pthread" {
            println!("cargo:rustc-link-arg=-pthread");
        } else if flag == "-framework" {
            index += 1;
            println!("cargo:rustc-link-lib=framework={}", library_flags[index]);
        } else if let Some(framework) = flag.strip_prefix("-framework=") {
            println!("cargo:rustc-link-lib=framework={framework}");
        } else {
            println!("cargo:rustc-link-arg={flag}");
        }
        index += 1;
    }
    if target == "macos" {
        println!("cargo:rustc-link-lib=objc");
        println!("cargo:rustc-link-lib=framework=CoreFoundation");
        println!("cargo:rustc-link-lib=framework=ApplicationServices");
    }
}

fn build_failure(message: String) -> ! {
    eprintln!("{message}");
    std::process::exit(1)
}
