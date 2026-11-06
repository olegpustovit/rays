//! Native system clipboard text. Calls must run on the initial thread.
pub fn set_text(text: &str) -> Result<(), String> {
    runtime::clipboard_set_text(text).map_err(|e| ogpu::error::to_string(&e))
}

pub fn get_text() -> Result<String, String> {
    runtime::clipboard_get_text().map_err(|e| ogpu::error::to_string(&e))
}
