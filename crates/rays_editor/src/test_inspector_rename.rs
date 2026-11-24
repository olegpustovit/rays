use super::object_rename;

#[test]
fn b26_object_rename_trims_and_ignores_unchanged_names() {
    assert_eq!(object_rename("before", " cam ").as_deref(), Some("cam"));
    assert_eq!(object_rename("cam", " cam "), None);
    assert_eq!(object_rename("cam", "  "), None);
}
