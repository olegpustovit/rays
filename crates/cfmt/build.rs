fn main() {
    println!("cargo:rerun-if-changed=src/cfmt_sys.c");
    cc::Build::new()
        .file("src/cfmt_sys.c")
        .warnings(true)
        .compile("cfmt_sys");
}
