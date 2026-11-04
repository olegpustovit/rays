use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=src/vulkan_sys.cpp");
    println!("cargo:rerun-if-changed=../../../lib/vulkan/vendor/volk.c");
    println!("cargo:rerun-if-changed=../../../lib/vulkan/vendor/volk.h");
    println!("cargo:rerun-if-changed=../../../lib/vulkan/vendor/vk_mem_alloc.h");
    if !std::env::var("CARGO_CFG_TARGET_OS").is_ok_and(|target| target == "windows") {
        return;
    }

    let root = PathBuf::from(std::env::var_os("CARGO_MANIFEST_DIR").unwrap()).join("../../..");
    let vulkan = root.join("lib/vulkan");
    cc::Build::new()
        .cpp(true)
        .file("src/vulkan_sys.cpp")
        .include(&vulkan)
        .flag_if_supported("-std=c++17")
        .warnings(true)
        .compile("rays_vulkan_sys");
    cc::Build::new()
        .file(vulkan.join("volk_unit.c"))
        .include(&vulkan)
        .warnings(true)
        .compile("rays_vulkan_volk");
    cc::Build::new()
        .cpp(true)
        .file(vulkan.join("vma.cpp"))
        .include(&vulkan)
        .flag_if_supported("-std=c++17")
        .warnings(false)
        .compile("rays_vulkan_vma");
}
