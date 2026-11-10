//! every time work meant for the device runs on the CPU instead,
//! here, with why. Bounded to 64 distinct (site, reason) pairs; a measured choice of the CPU is not
//! a fallback.
use std::collections::HashMap;
use std::sync::{LazyLock, Mutex};

const CAPACITY: usize = 64;
static ENTRIES: LazyLock<Mutex<HashMap<(String, String), i64>>> = LazyLock::new(|| Mutex::new(HashMap::new()));

pub fn note(site: &str, reason: &str) {
    let mut entries = ENTRIES.lock().unwrap();
    let key = (site.to_owned(), reason.to_owned());
    if let Some(count) = entries.get_mut(&key) {
        *count += 1;
    } else if entries.len() < CAPACITY {
        entries.insert(key, 1);
    }
}

/// Each (site, reason) and how many times, sorted.
pub fn seen() -> Vec<(String, String, i64)> {
    let entries = ENTRIES.lock().unwrap();
    let mut all: Vec<(String, String, i64)> = entries.iter().map(|((s, r), c)| (s.clone(), r.clone(), *c)).collect();
    all.sort_by(|a, b| (&a.0, &a.1).cmp(&(&b.0, &b.1)));
    all
}

pub fn reset() {
    ENTRIES.lock().unwrap().clear();
}

/// The first entry and how many more, for one status line.
pub fn summary() -> Option<String> {
    let all = seen();
    let (site, reason, _) = all.first()?;
    let more = if all.len() == 1 { String::new() } else { format!(" (+{})", all.len() - 1) };
    Some(format!("off GPU: {}: {}{}", site, reason, more))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn notes_count_sort_and_summarise() {
        // one test owns the global table
        reset();
        assert_eq!(summary(), None);
        note("b", "why");
        note("a", "x");
        note("b", "why");
        assert_eq!(seen(), vec![("a".into(), "x".into(), 1), ("b".into(), "why".into(), 2)]);
        assert_eq!(summary().unwrap(), "off GPU: a: x (+1)");
        for i in 0..100 {
            note("s", &i.to_string());
        }
        assert_eq!(seen().len(), CAPACITY);
        reset();
        assert!(seen().is_empty());
    }
}
