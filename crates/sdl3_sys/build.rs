use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

// Locate SDL3 or one of its satellites (`pkg` is the pkg-config name, `lib` the library, e.g. SDL3_image):
// $SDL3_DIR (include/ and lib/ beneath it), then pkg-config, then the MSYS2 prefix.
fn run_pkg_config(pkg: &str, lib: &str, argument: &str) -> String {
    println!("cargo:rerun-if-env-changed=SDL3_DIR");
    println!("cargo:rerun-if-env-changed=PKG_CONFIG");
    let prefix = |dir: &str| {
        if argument == "--cflags" {
            format!("-I{dir}/include")
        } else if lib == "SDL3" {
            format!("-L{dir}/lib -lSDL3")
        } else {
            format!("-L{dir}/lib -l{lib} -lSDL3")
        }
    };
    if let Some(dir) = env::var_os("SDL3_DIR") {
        return prefix(&dir.to_string_lossy().replace('\\', "/"));
    }
    let program = env::var_os("PKG_CONFIG").unwrap_or_else(|| "pkg-config".into());
    if let Ok(output) = Command::new(program).arg(argument).arg(pkg).output() {
        if output.status.success() {
            return String::from_utf8(output.stdout).expect("pkg-config output must be UTF-8");
        }
    }
    let msys = ["C:/msys64/mingw64", "C:/msys64/ucrt64"]
        .into_iter()
        .find(|dir| std::path::Path::new(dir).join("include/SDL3/SDL.h").exists());
    match msys {
        Some(dir) => prefix(dir),
        None => build_failure(format!("{pkg} not found: set SDL3_DIR, or install pkg-config with its development package")),
    }
}

fn words(value: &str) -> Vec<&str> {
    value.split_whitespace().collect()
}

fn apply_cflags(c: &mut cc::Build, cflags: &str) {
    for flag in words(cflags) {
        if let Some(path) = flag.strip_prefix("-I") {
            c.include(path);
        } else if let Some(definition) = flag.strip_prefix("-D") {
            c.define(definition.split('=').next().unwrap(), definition.split_once('=').map(|(_, value)| value));
        } else {
            c.flag(flag);
        }
    }
}

fn emit_link_flags(libs: &str) {
    let library_flags: Vec<_> = words(libs);
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
}

// One bridge per satellite (`{key}` of the lock entry); the lock floor becomes a compile-time assertion on its headers.
fn satellite(root: &std::path::Path, out: &std::path::Path, lock: &str, key: &str, pkg: &str, lib: &str) {
    let source = format!("src/{}_bridge.c", key.trim_start_matches("sdl3_"));
    println!("cargo:rerun-if-changed={source}");
    let entry = lock
        .lines()
        .map(|line| line.trim().trim_start_matches('('))
        .find(|line| line.starts_with(&format!("{key} ")))
        .unwrap_or_else(|| build_failure(format!("{key} entry in packaging/sdl3.lock")));
    let floor = entry
        .split("(floor ")
        .nth(1)
        .and_then(|rest| rest.split(')').next())
        .expect("floor in packaging/sdl3.lock");
    let v: Vec<u32> = floor.split('.').map(|part| part.parse().expect("numeric floor")).collect();
    assert_eq!(v.len(), 3, "floor must be major.minor.patch");
    let dir = out.join(key);
    fs::create_dir_all(&dir).expect("create satellite out dir");
    fs::write(
        dir.join("floor.h"),
        format!(
            "#define RAYS_FLOOR_MAJOR {}\n#define RAYS_FLOOR_MINOR {}\n#define RAYS_FLOOR_PATCH {}\n",
            v[0], v[1], v[2]
        ),
    )
    .expect("write floor.h");
    let mut c = cc::Build::new();
    c.file(root.join(&source)).include(&dir).std("c11").warnings(true).flag_if_supported("-Werror");
    apply_cflags(&mut c, &run_pkg_config(pkg, lib, "--cflags"));
    c.compile(&format!("rays_{key}_sys"));
    emit_link_flags(&run_pkg_config(pkg, lib, "--libs"));
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
    apply_cflags(&mut c, &run_pkg_config("sdl3", "SDL3", "--cflags"));
    if target == "macos" {
        c.define("RAYS_SDL3_MACOS", None).flag_if_supported("-fblocks");
    }
    c.compile("rays_sdl3_sys");

    emit_link_flags(&run_pkg_config("sdl3", "SDL3", "--libs"));
    for (key, pkg, lib) in [("sdl3_image", "sdl3-image", "SDL3_image"), ("sdl3_ttf", "sdl3-ttf", "SDL3_ttf"), ("sdl3_mixer", "sdl3-mixer", "SDL3_mixer")] {
        satellite(&root, &out, &lock, key, pkg, lib);
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
