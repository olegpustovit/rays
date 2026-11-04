//! `print_shader OWNER`: prints OWNER's shaders from its `shaders/NAME.rays` into the current
//! directory (port of lib/flow_shader/print_shader.ml): NAME.metal and the Vulkan modules
//! NAME.{vert,frag,comp}.spv.
use std::fs;

fn main() {
    let owner = std::env::args().nth(1).unwrap_or_else(|| {
        eprintln!("usage: print_shader OWNER");
        std::process::exit(2);
    });
    let source = |name: &str| fs::read_to_string(format!("shaders/{name}.rays")).unwrap_or_else(|e| panic!("shaders/{name}.rays: {e}"));
    match flow_shader::print::print(&owner, &source) {
        Ok(files) => {
            for (name, bytes) in files {
                fs::write(&name, bytes).unwrap_or_else(|e| panic!("{name}: {e}"));
            }
        }
        Err(message) => {
            eprintln!("{message}");
            std::process::exit(1);
        }
    }
}
