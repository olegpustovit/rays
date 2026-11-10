//! RAYS_UPDATE_FIXTURES=1 cargo run -p web_export --example refresh_manifest
fn main() {
    assert_eq!(std::env::var("RAYS_UPDATE_FIXTURES").as_deref(), Ok("1"), "set RAYS_UPDATE_FIXTURES=1 to replace the manifest");
    std::thread::Builder::new().stack_size(256 << 20).spawn(|| {
        let (manifest, _) = flow_sop::manifest::generate(&editor_document::kinds::descriptors(), None, &sop::nodes::factories())
            .expect("manifest generation");
        std::fs::write(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../flow_sop/flow_manifest.sexp"), manifest).unwrap();
    }).unwrap().join().unwrap();
}
