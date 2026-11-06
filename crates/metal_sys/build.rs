use std::{env, fs, path::PathBuf};
mod build_config;
fn build_failure(message: impl std::fmt::Display) -> ! { panic!("{message}") }
fn main() {
    let manifest = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap());
    if env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("macos") {
        let sdk = std::process::Command::new("/usr/bin/xcrun")
            .args(["--sdk", "macosx", "--show-sdk-version"])
            .output().expect("xcrun could not report the macOS SDK version");
        assert!(sdk.status.success(), "xcrun could not report the macOS SDK version: {}", String::from_utf8_lossy(&sdk.stderr));
        build_config::check_sdk_version(std::str::from_utf8(&sdk.stdout).expect("non-UTF-8 macOS SDK version")).unwrap_or_else(|message| build_failure(message));
        println!("cargo:rerun-if-env-changed=RAYS_METAL_SANITIZERS");
        let sanitizers = build_config::parse_sanitizers(&env::var("RAYS_METAL_SANITIZERS").unwrap_or_default()).unwrap_or_else(|message| build_failure(message));
        let native_dir = manifest.join("src/native");
        println!("cargo:rerun-if-changed={}", native_dir.display());
        let mut build = cc::Build::new();
        build.cpp(true).include(&native_dir)
            .flag("-std=c++17").flag("-fobjc-arc").flag("-fblocks").warnings(true);
        build.flags(build_config::compile_flags(&sanitizers));
        build.flags(build_config::profile_compile_flags(&env::var("PROFILE").unwrap_or_default()));
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
        for framework in build_config::FRAMEWORKS {
            println!("cargo:rustc-link-lib=framework={framework}");
        }
        for flag in build_config::link_flags(&sanitizers) {
            println!("cargo:rustc-link-arg={flag}");
        }
    }
}
