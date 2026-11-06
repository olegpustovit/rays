use std::sync::LazyLock;
use std::time::Instant;

static ORIGIN: LazyLock<Instant> = LazyLock::new(Instant::now);

pub fn now() -> f64 {
    ORIGIN.elapsed().as_secs_f64()
}
