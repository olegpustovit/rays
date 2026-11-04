use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

const KEY: &str = "sdl3_image";
const PKG: &str = "sdl3-image";
const LIB: &str = "SDL3_image";

// Locate SDL3 and its satellite: $SDL3_DIR (include/ and lib/ beneath it), then pkg-config, then the MSYS2 prefix.
fn flags(argument: &str) -> String {
    println!("cargo:rerun-if-env-changed=SDL3_DIR");
    println!("cargo:rerun-if-env-changed=PKG_CONFIG");
    let prefix = |dir: &str| {
        if argument == "--cflags" { format!("-I{dir}/include") } else { format!("-L{dir}/lib -l{LIB} -lSDL3") }
    };
    if let Some(dir) = env::var_os("SDL3_DIR") {
        return prefix(&dir.to_string_lossy().replace(char::from(92u8), "/"));
    }
    let program = env::var_os("PKG_CONFIG").unwrap_or_else(|| "pkg-config".into());
    if let Ok(output) = Command::new(program).arg(argument).arg(PKG).output() {
        if output.status.success() {
            return String::from_utf8(output.stdout).expect("pkg-config output must be UTF-8");
        }
    }
    let msys = ["C:/msys64/mingw64", "C:/msys64/ucrt64"]
        .into_iter()
        .find(|dir| std::path::Path::new(dir).join("include/SDL3/SDL.h").exists());
    match msys {
        Some(dir) => prefix(dir),
        None => build_failure(format!("{PKG} not found: set SDL3_DIR, or install pkg-config with its development package")),
    }
}

fn main() {
    let root = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap());
    let out = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let lock_path = root.join("../../../packaging/sdl3.lock");
    println!("cargo:rerun-if-changed={}", lock_path.display());
    println!("cargo:rerun-if-changed=src/bridge.c");

    // The lock floor becomes a compile-time assertion on the headers.
    let lock = fs::read_to_string(&lock_path).expect("read packaging/sdl3.lock");
    let entry = lock
        .lines()
        .map(|line| line.trim().trim_start_matches('('))
        .find(|line| line.starts_with(&format!("{KEY} ")))
        .unwrap_or_else(|| build_failure(format!("{KEY} entry in packaging/sdl3.lock")));
    let floor = entry
        .split("(floor ")
        .nth(1)
        .and_then(|rest| rest.split(')').next())
        .expect("floor in packaging/sdl3.lock");
    let v: Vec<u32> = floor.split('.').map(|part| part.parse().expect("numeric floor")).collect();
    assert_eq!(v.len(), 3, "floor must be major.minor.patch");
    fs::write(
        out.join("floor.h"),
        format!(
            "#define RAYS_FLOOR_MAJOR {}\n#define RAYS_FLOOR_MINOR {}\n#define RAYS_FLOOR_PATCH {}\n",
            v[0], v[1], v[2]
        ),
    )
    .expect("write floor.h");

    let mut c = cc::Build::new();
    c.file("src/bridge.c").include(&out).std("c11").warnings(true).flag_if_supported("-Werror");
    for flag in flags("--cflags").split_whitespace() {
        if let Some(path) = flag.strip_prefix("-I") {
            c.include(path);
        } else if let Some(definition) = flag.strip_prefix("-D") {
            c.define(definition.split('=').next().unwrap(), definition.split_once('=').map(|(_, value)| value));
        } else {
            c.flag(flag);
        }
    }
    c.compile("rays_sdl3_image_sys");

    for flag in flags("--libs").split_whitespace() {
        if let Some(path) = flag.strip_prefix("-L") {
            println!("cargo:rustc-link-search=native={path}");
        } else if let Some(library) = flag.strip_prefix("-l") {
            println!("cargo:rustc-link-lib={library}");
        } else if flag == "-pthread" {
            println!("cargo:rustc-link-arg=-pthread");
        } else {
            println!("cargo:rustc-link-arg={flag}");
        }
    }
}

fn build_failure(message: String) -> ! {
    eprintln!("{message}");
    std::process::exit(1)
}
