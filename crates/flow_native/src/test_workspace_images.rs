//! The workspace host owns and closes its services. (The image owner's tests need the process main
//! thread and live in tests/gpu_workspace_images.rs.)
use crate::{workspace_host, workspace_resources};

#[test]
fn image_keys_keep_authored_paths_and_exact_versions_distinct() {
    use crate::workspace_images::ImageKey;
    let site = (7, (vec!["image".into(), "a/b".into()], vec![2, 3]));
    let split = (7, (vec!["image".into(), "a".into(), "b".into()], vec![2, 3]));
    let version = (11, 13, 17, 32, 24);
    let mut keys = vec![
        ImageKey::Map(site.clone()), ImageKey::Render(site.clone()),
        ImageKey::Map(split), ImageKey::Exact(site.clone(), version),
        ImageKey::Load("image/a/b".into()), ImageKey::Noise("image/a/b".into()),
        ImageKey::NoiseLive(11, 13),
    ];
    for field in 0..5 {
        let mut changed = [version.0, version.1, version.2, version.3, version.4];
        changed[field] += 1;
        keys.push(ImageKey::Exact(site.clone(), (changed[0], changed[1], changed[2], changed[3], changed[4])));
    }
    keys.push(ImageKey::Map((8, site.1.clone())));
    keys.push(ImageKey::Map((7, (site.1.0.clone(), vec![2, 4]))));
    for (index, key) in keys.iter().enumerate() {
        assert!(!keys[..index].contains(key), "every producer, site, iteration and exact version field participates");
    }
    mem_sys::local_reset();
    for _ in 0..1024 {
        for key in &keys { assert!(std::hint::black_box(&keys).contains(key)); }
    }
    assert_eq!(mem_sys::local_alloc_count(), 0, "image identity lookup must not format authored paths");
}

#[test]
fn a_host_owns_and_closes_its_services() {
    let mut host = workspace_host::create(workspace_host::CreateOptions::default());
    assert!(host.prepared.is_none() && !host.quit_requested && host.fired == 0);
    assert!(workspace_host::take_events(&mut host).is_empty());
    workspace_host::close(&mut host);
    assert!(workspace_resources::closed(&host.resources));
}
