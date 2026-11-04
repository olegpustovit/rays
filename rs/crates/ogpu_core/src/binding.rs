use crate::caps::Caps;
use crate::error::{self, Error, ErrorKind};
use crate::handle::Device;
use crate::validation;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stage {
    Vertex,
    Fragment,
    Compute,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    Buffer,
    Texture,
    Sampler,
    AccelerationStructure,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LayoutEntry {
    pub binding: i64,
    pub kind: Kind,
    pub visibility: Vec<Stage>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Layout(Vec<LayoutEntry>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PipelineLayout {
    groups: Vec<(i64, Layout)>,
}

pub fn create_layout(entries: &[LayoutEntry]) -> Result<Layout, Error> {
    let mut sorted = entries.to_vec();
    sorted.sort_by_key(|entry| entry.binding);
    let checked: Vec<_> = sorted
        .iter()
        .map(|entry| {
            let visibility = entry
                .visibility
                .iter()
                .map(|stage| match stage {
                    Stage::Vertex => 0,
                    Stage::Fragment => 1,
                    Stage::Compute => 2,
                })
                .collect();
            (entry.binding, visibility)
        })
        .collect();
    validation::validate_layout("Ogpu.Binding.create_layout", &checked)?;
    Ok(Layout(sorted))
}

pub fn create_pipeline_layout(
    device: &Device,
    capabilities: &Caps,
    layouts: &[(i64, Layout)],
) -> Result<PipelineLayout, Error> {
    if crate::handle::device_destroyed(device) {
        return Err(error::make(
            "Ogpu.Binding.create_pipeline_layout",
            ErrorKind::StaleHandle,
            "device is destroyed",
        ));
    }
    let mut groups = layouts.to_vec();
    groups.sort_by_key(|(group, _)| *group);
    let group_ids: Vec<_> = groups.iter().map(|(group, _)| *group).collect();
    validation::validate_groups(
        "Ogpu.Binding.create_pipeline_layout",
        capabilities.limits.max_bind_groups,
        &group_ids,
    )?;
    Ok(PipelineLayout { groups })
}

pub fn pipeline_layouts(layout: &PipelineLayout) -> Vec<(i64, Vec<LayoutEntry>)> {
    layout
        .groups
        .iter()
        .map(|(group, entries)| (*group, entries.0.clone()))
        .collect()
}
