//! Native system clipboard text. Calls must run on the initial thread.
pub fn set_text(text: &str) -> Result<(), String> {
    runtime::clipboard_set_text(text)
}

pub fn get_text() -> Result<String, String> {
    runtime::clipboard_get_text()
}
