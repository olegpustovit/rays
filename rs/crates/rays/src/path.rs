//! Path construction and tessellation: the shared `scene_command` path under the sketch names.
pub use scene_command::path::{Cap, Command, Error, FillRule, Join, Mesh, Path as t, Point};
use scene_command::path as p;

pub fn of_commands(commands: &[Command]) -> t {
    p::of_commands(commands)
}

pub fn tessellate(tolerance: f64, fill_rule: FillRule, path: &t) -> Result<Mesh, Error> {
    p::tessellate(tolerance, fill_rule, path)
}

pub fn stroke(tolerance: f64, width: f64, cap: Cap, join: Join, miter_limit: f64, path: &t) -> Result<Mesh, Error> {
    p::stroke(tolerance, width, cap, join, miter_limit, path)
}
