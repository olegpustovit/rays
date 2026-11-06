//! The workspace host owns and closes its services. (The image owner's tests need the process main
//! thread and live in tests/gpu_workspace_images.rs.)
use crate::{workspace_host, workspace_resources};

#[test]
fn a_host_owns_and_closes_its_services() {
    let mut host = workspace_host::create(workspace_host::CreateOptions::default());
    assert!(host.prepared.is_none() && !host.quit_requested && host.fired == 0);
    assert!(workspace_host::take_events(&mut host).is_empty());
    workspace_host::close(&mut host);
    assert!(workspace_resources::closed(&host.resources));
}
