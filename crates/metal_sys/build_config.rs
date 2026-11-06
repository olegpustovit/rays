// Rust bridge build contract, ported from lib/metal/metal_build_config.ml.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Sanitizer { Address, Undefined, Thread }

impl Sanitizer {
    fn name(self) -> &'static str {
        match self { Self::Address => "address", Self::Undefined => "undefined", Self::Thread => "thread" }
    }
}

pub fn parse_sanitizers(value: &str) -> Result<Vec<Sanitizer>, String> {
    let value = value.trim();
    if value.is_empty() || value == "none" { return Ok(vec![]); }
    let mut sanitizers = value.split(',').map(|name| match name.trim() {
        "address" => Ok(Sanitizer::Address),
        "undefined" => Ok(Sanitizer::Undefined),
        "thread" => Ok(Sanitizer::Thread),
        invalid => Err(format!("unknown sanitizer {invalid:?}")),
    }).collect::<Result<Vec<_>, _>>()?;
    sanitizers.sort_unstable();
    sanitizers.dedup();
    if sanitizers.contains(&Sanitizer::Thread) && sanitizers.len() != 1 {
        return Err("thread sanitizer cannot be combined with address or undefined".to_owned());
    }
    Ok(sanitizers)
}

pub fn link_flags(sanitizers: &[Sanitizer]) -> Vec<String> {
    if sanitizers.is_empty() { return vec![]; }
    let mut flags = vec![format!("-fsanitize={}", sanitizers.iter().map(|value| value.name()).collect::<Vec<_>>().join(","))];
    if sanitizers.contains(&Sanitizer::Undefined) { flags.push("-fno-sanitize-recover=undefined".to_owned()); }
    flags
}

pub fn compile_flags(sanitizers: &[Sanitizer]) -> Vec<String> {
    let mut flags = link_flags(sanitizers);
    if !flags.is_empty() { flags.insert(1, "-fno-omit-frame-pointer".to_owned()); }
    flags
}

pub fn profile_compile_flags(profile: &str) -> &'static [&'static str] {
    match profile { "debug" | "dev" => &["-O0", "-g"], "release" => &["-O3", "-DNDEBUG"], _ => &["-O2"] }
}

pub fn check_sdk_version(version: &str) -> Result<(), String> {
    let invalid = || format!("invalid macOS SDK version {version:?}");
    let number = |part: &str| {
        if part.is_empty() || !part.bytes().all(|byte| byte.is_ascii_digit()) { return Err(invalid()); }
        part.parse::<usize>().map_err(|_| invalid())
    };
    let mut parts = version.trim().split('.');
    let major = number(parts.next().ok_or_else(invalid)?)?;
    number(parts.next().ok_or_else(invalid)?)?;
    if let Some(patch) = parts.next() { number(patch)?; }
    if parts.next().is_some() { return Err(invalid()); }
    if major < 26 { return Err(format!("Metal requires macOS SDK 26.0 or newer (found {version})")); }
    Ok(())
}

pub const FRAMEWORKS: &[&str] = &["Foundation", "Metal", "QuartzCore", "CoreGraphics", "IOSurface"];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_build_config_contract() {
        assert_eq!(parse_sanitizers("").unwrap(), []);
        assert_eq!(parse_sanitizers("none").unwrap(), []);
        let combined = parse_sanitizers("undefined,address,address").unwrap();
        assert_eq!(combined, [Sanitizer::Address, Sanitizer::Undefined]);
        assert_eq!(compile_flags(&combined), ["-fsanitize=address,undefined", "-fno-omit-frame-pointer", "-fno-sanitize-recover=undefined"]);
        assert_eq!(link_flags(&combined), ["-fsanitize=address,undefined", "-fno-sanitize-recover=undefined"]);
        assert_eq!(profile_compile_flags("release"), ["-O3", "-DNDEBUG"]);
        assert_eq!(profile_compile_flags("debug"), ["-O0", "-g"]);
        assert_eq!(profile_compile_flags("other"), ["-O2"]);
        for version in ["26.0", "26.0.1", "26.5", "27.0"] { assert!(check_sdk_version(version).is_ok(), "supported SDK {version} was rejected"); }
        for version in ["", "25.9", "26", "26.beta", "26.0.bad", "27.0junk"] { assert!(check_sdk_version(version).is_err(), "unsupported SDK {version:?} was accepted"); }
        assert!(FRAMEWORKS.contains(&"IOSurface"));
        assert!(parse_sanitizers("thread,address").is_err());
        assert!(parse_sanitizers("python").is_err());
        assert_eq!(parse_sanitizers(" thread,thread ").unwrap(), [Sanitizer::Thread]);
    }
}
