//! Interpolate source attributes onto destination elements (primitive/UVW or weighted drivers).
//!
//! The OCaml version mutates shared output planes from parallel ranges. Here every range works on
//! per-destination `Cell`s gathered from the job outputs and the cells are scattered back in range
//! order, which is byte-identical to the sequential path for any domain count.
use std::collections::HashSet;
use std::sync::Arc;
use rays_math::text_array::TextArray;

use rdk_core::{
    attribute, cancel, error, geometry, group, packed, support, topology,
};
use rdk_core::error::Code;

use crate::attribute_ops::{make_error, ocaml_quote};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Spec {
    pub source_owner: attribute::Owner,
    pub source_name: String,
    pub target_name: String,
}

#[derive(Clone)]
pub struct GroupSpec {
    pub group_source_owner: attribute::Owner,
    pub group_source: group::T,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Unmatched {
    KeepTarget,
    DefaultValue,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WeightedSource {
    Points,
    Vertices,
    Primitives,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Computed {
    pub computed_owner: WeightedSource,
    pub computed_numbers_attribute: String,
    pub computed_weights_attribute: String,
}

#[derive(Clone)]
pub struct PrimitiveParams {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub selection: Option<group::T>,
    pub compute: Option<Computed>,
    pub primitive_attribute: Option<String>,
    pub uvw_attribute: Option<String>,
    pub pre_scale: Option<f64>,
    pub blend: Option<f64>,
    pub unmatched: Option<Unmatched>,
    pub target_owner: attribute::Owner,
    pub attributes: Vec<Spec>,
    pub groups: Vec<GroupSpec>,
    pub source: geometry::T,
    pub target: geometry::T,
}

#[derive(Clone)]
pub struct WeightedParams {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub selection: Option<group::T>,
    pub numbers_attribute: Option<String>,
    pub weights_attribute: Option<String>,
    pub pre_scale: Option<f64>,
    pub normalize_weights: Option<bool>,
    pub threshold: Option<f64>,
    pub blend: Option<f64>,
    pub unmatched: Option<Unmatched>,
    pub weighted_owner: WeightedSource,
    pub target_owner: attribute::Owner,
    pub attributes: Vec<Spec>,
    pub groups: Vec<GroupSpec>,
    pub source: geometry::T,
    pub target: geometry::T,
}

const OPERATION: &str = "attribute_interpolate";
const MAX_INT: i64 = (1_i64 << 62) - 1;

/// An `Error msg` of the OCaml API (the caller codes it `invalid_interpolate`).
fn fail(message: &str) -> error::T {
    make_error(OPERATION, Code::InvalidInterpolate, message)
}

/// An `invalid_arg msg` of the OCaml API (the caller codes it `invalid_parameter`).
fn invalid(message: &str) -> error::T {
    make_error(OPERATION, Code::InvalidParameter, message)
}

fn is_blank(text: &str) -> bool {
    crate::attribute_ops::is_blank(text)
}

enum Data {
    F(Vec<Arc<Vec<f64>>>),
    I(Arc<Vec<i64>>),
    T(Arc<TextArray>),
    IntArray(packed::IntArray::View::View),
    FloatArray(packed::FloatArray::View::View),
}

#[derive(Default)]
struct Out {
    f: Vec<Vec<f64>>,
    i: Vec<i64>,
    /// Random-access scratch rows; packed into one `TextArray` when stored.
    t: Vec<Vec<u8>>,
    choices: Vec<i64>,
}

struct Job {
    source_owner: attribute::Owner,
    target_owner: attribute::Owner,
    target_name: String,
    position: bool,
    normalize: bool,
    source: Data,
    existing: Option<Data>,
    out: Out,
}

struct GroupJob {
    group_source_owner: attribute::Owner,
    group_source_bits: Vec<u8>,
    group_target_owner: group::Owner,
    group_target_name: String,
    group_existing_bits: Option<Vec<u8>>,
    group_output_bits: Vec<u8>,
    group_target_count: i64,
}

#[derive(Clone, Default)]
struct Cell {
    f: [f64; 4],
    i: i64,
    t: Vec<u8>,
    choice: i64,
}

fn owner_count(geometry_value: &geometry::T, owner: attribute::Owner) -> i64 {
    match owner {
        attribute::Owner::Point => geometry::point_count(geometry_value),
        attribute::Owner::Vertex => geometry::vertex_count(geometry_value),
        attribute::Owner::Primitive => geometry::primitive_count(geometry_value),
        attribute::Owner::Detail => 1,
    }
}

fn owner_name(owner: attribute::Owner) -> &'static str {
    match owner {
        attribute::Owner::Point => "point",
        attribute::Owner::Vertex => "vertex",
        attribute::Owner::Primitive => "primitive",
        attribute::Owner::Detail => "detail",
    }
}

fn group_owner(owner: attribute::Owner) -> Option<group::Owner> {
    match owner {
        attribute::Owner::Point => Some(group::Owner::Point),
        attribute::Owner::Vertex => Some(group::Owner::Vertex),
        attribute::Owner::Primitive => Some(group::Owner::Primitive),
        attribute::Owner::Detail => None,
    }
}

fn attribute_owner_of_group(owner: group::Owner) -> attribute::Owner {
    match owner {
        group::Owner::Point => attribute::Owner::Point,
        group::Owner::Vertex => attribute::Owner::Vertex,
        group::Owner::Primitive => attribute::Owner::Primitive,
    }
}

fn bit_set(bits: &mut [u8], index: usize, member: bool) {
    let byte = index >> 3;
    let mask = 1_u8 << (index & 7);
    if member {
        bits[byte] |= mask;
    } else {
        bits[byte] &= !mask;
    }
}

fn bit_mem(bits: &[u8], index: i64) -> bool {
    let index = index as usize;
    bits[index >> 3] & (1 << (index & 7)) != 0
}

fn create_group_jobs(
    target_owner: attribute::Owner,
    target: &geometry::T,
    groups: &[GroupSpec],
) -> Result<Vec<GroupJob>, error::T> {
    let Some(target_group_owner) = group_owner(target_owner) else {
        if !groups.is_empty() {
            return Err(fail("Attribute Interpolate: detail destinations cannot receive groups"));
        }
        return Ok(Vec::new());
    };
    let target_count = owner_count(target, target_owner);
    let mut seen: HashSet<String> = HashSet::new();
    let mut output = Vec::new();
    for spec in groups {
        let source = &spec.group_source;
        if attribute_owner_of_group(group::owner(source)) != spec.group_source_owner {
            return Err(fail("Attribute Interpolate: internal source-group ownership mismatch"));
        }
        if !seen.insert(group::name(source).to_owned()) {
            return Err(fail(&format!(
                "Attribute Interpolate: duplicate target group {}",
                ocaml_quote(group::name(source))
            )));
        }
        let existing = geometry::find_group(target_group_owner, group::name(source), target);
        let existing_bits = existing.as_ref().map(|value| group::View::bits_view(value).to_vec());
        let output_bits = match &existing_bits {
            None => vec![0_u8; ((target_count + 7) / 8) as usize],
            Some(bits) => bits.clone(),
        };
        output.push(GroupJob {
            group_source_owner: spec.group_source_owner,
            group_source_bits: group::View::bits_view(source).to_vec(),
            group_target_owner: target_group_owner,
            group_target_name: group::name(source).to_owned(),
            group_existing_bits: existing_bits,
            group_output_bits: output_bits,
            group_target_count: target_count,
        });
    }
    Ok(output)
}

/// The selected destination elements in group order (`Group.iter_ordered`), or all of them.
struct Selected {
    count: i64,
    elements: Option<Vec<i64>>,
}

impl Selected {
    fn element(&self, rank: i64) -> i64 {
        match &self.elements {
            None => rank,
            Some(elements) => elements[rank as usize],
        }
    }
}

fn selection_elements(
    owner: attribute::Owner,
    selection: Option<&group::T>,
    geometry_value: &geometry::T,
) -> Result<Selected, error::T> {
    let count = owner_count(geometry_value, owner);
    let Some(value) = selection else {
        return Ok(Selected { count, elements: None });
    };
    match group_owner(owner) {
        None => Err(fail("Attribute Interpolate: detail attributes do not accept a group")),
        Some(expected) if group::owner(value) != expected || group::length(value) != count => Err(fail(&format!(
            "Attribute Interpolate: selection must be a matching {} group",
            owner_name(owner)
        ))),
        Some(_) => {
            let elements = match group::ordered_elements(value) {
                Some(order) => order,
                None => (0..group::length(value)).filter(|&element| group::mem(element, value)).collect(),
            };
            Ok(Selected { count: elements.len() as i64, elements: Some(elements) })
        }
    }
}

fn data_of(storage: &attribute::Storage) -> Result<Data, error::T> {
    Ok(match storage {
        attribute::Storage::Float(values) => Data::F(vec![values.clone()]),
        attribute::Storage::Float2(values) => {
            let view = packed::Float2::View::view(values);
            Data::F(vec![view.x, view.y])
        }
        attribute::Storage::Float3(values) => {
            let view = packed::Float3::View::view(values)?;
            Data::F(vec![view.x, view.y, view.z])
        }
        attribute::Storage::Float4(values) => {
            let view = packed::Float4::View::view(values);
            Data::F(vec![view.x, view.y, view.z, view.w])
        }
        attribute::Storage::Int(values) => Data::I(values.clone()),
        attribute::Storage::Text(values) => Data::T(values.clone()),
        attribute::Storage::IntArray(values) => Data::IntArray(packed::IntArray::View::view(values)),
        attribute::Storage::FloatArray(values) => Data::FloatArray(packed::FloatArray::View::view(values)),
    })
}

fn same_kind(source: &Data, target: &Data) -> bool {
    match (source, target) {
        (Data::F(left), Data::F(right)) => left.len() == right.len(),
        (Data::I(_), Data::I(_))
        | (Data::T(_), Data::T(_))
        | (Data::IntArray(_), Data::IntArray(_))
        | (Data::FloatArray(_), Data::FloatArray(_)) => true,
        _ => false,
    }
}

fn source_data(
    source_owner: attribute::Owner,
    source_name: &str,
    source: &geometry::T,
) -> Result<Data, error::T> {
    if source_owner == attribute::Owner::Point && source_name == "P" {
        return data_of(&attribute::Storage::Float3(geometry::positions(source).clone()));
    }
    match geometry::find_attribute(source_owner, source_name, source) {
        None => Err(fail(&format!(
            "Attribute Interpolate: source is missing {} attribute {}",
            owner_name(source_owner),
            ocaml_quote(source_name)
        ))),
        Some(found) => data_of(attribute::View::storage(&found)?),
    }
}

fn target_data(
    target_owner: attribute::Owner,
    target_name: &str,
    target: &geometry::T,
) -> Result<Option<Data>, error::T> {
    if target_owner == attribute::Owner::Point && target_name == "P" {
        return data_of(&attribute::Storage::Float3(geometry::positions(target).clone())).map(Some);
    }
    match geometry::find_attribute(target_owner, target_name, target) {
        None => Ok(None),
        Some(found) => data_of(attribute::View::storage(&found)?).map(Some),
    }
}

fn create_job(
    target_owner: attribute::Owner,
    source: &geometry::T,
    target: &geometry::T,
    spec: &Spec,
) -> Result<Job, error::T> {
    if is_blank(&spec.source_name) || is_blank(&spec.target_name) {
        return Err(fail("Attribute Interpolate: attribute names must not be empty"));
    }
    if spec.source_name == "P" && spec.source_owner != attribute::Owner::Point {
        return Err(fail("Attribute Interpolate: canonical source P must be point owned"));
    }
    if spec.target_name == "P" && target_owner != attribute::Owner::Point {
        return Err(fail("Attribute Interpolate: canonical target P must be point owned"));
    }
    let source_value = source_data(spec.source_owner, &spec.source_name, source)?;
    let existing = target_data(target_owner, &spec.target_name, target)?;
    if let Some(existing) = &existing {
        if !same_kind(&source_value, existing) {
            return Err(fail(&format!(
                "Attribute Interpolate: target attribute {} has incompatible storage",
                ocaml_quote(&spec.target_name)
            )));
        }
    }
    let count = owner_count(target, target_owner) as usize;
    let position = target_owner == attribute::Owner::Point && spec.target_name == "P";
    let normalize = spec.target_name == "N";
    let mut out = Out::default();
    match (&source_value, &existing) {
        (Data::F(planes), Some(Data::F(old))) => {
            out.f = (0..planes.len()).map(|k| old[k].to_vec()).collect();
        }
        (Data::F(planes), _) => out.f = (0..planes.len()).map(|_| vec![0.0; count]).collect(),
        (Data::I(_), Some(Data::I(old))) => out.i = old.to_vec(),
        (Data::I(_), _) => out.i = vec![0; count],
        (Data::T(_), Some(Data::T(old))) => out.t = old.to_rows(),
        (Data::T(_), _) => out.t = vec![Vec::new(); count],
        (Data::IntArray(_) | Data::FloatArray(_), _) => out.choices = vec![-1; count],
    }
    Ok(Job {
        source_owner: spec.source_owner,
        target_owner,
        target_name: spec.target_name.clone(),
        position,
        normalize,
        source: source_value,
        existing,
        out,
    })
}

fn polygon_weight(size: i64, u: f64, v: f64, local: i64) -> f64 {
    if size == 3 {
        if local == 0 {
            1.0 - u - v
        } else if local == 1 {
            u
        } else {
            v
        }
    } else if size == 4 {
        if local == 0 {
            (1.0 - u) * (1.0 - v)
        } else if local == 1 {
            (1.0 - u) * v
        } else if local == 2 {
            u * v
        } else {
            u * (1.0 - v)
        }
    } else {
        let scaled = u * size as f64;
        let base = scaled.floor() as i64;
        let edge = ((base % size) + size) % size;
        let next = if edge + 1 == size { 0 } else { edge + 1 };
        let t = scaled - scaled.floor();
        let boundary = if local == edge {
            1.0 - t
        } else if local == next {
            t
        } else {
            0.0
        };
        ((1.0 - v) * boundary) + (v / size as f64)
    }
}

fn curve_weight(closed: bool, size: i64, u: f64, local: i64) -> f64 {
    let edges = if closed { size } else { size - 1 };
    let scaled = u * edges as f64;
    let base = scaled.floor() as i64;
    let t = scaled - scaled.floor();
    if closed {
        let edge = ((base % size) + size) % size;
        let next = if edge + 1 == size { 0 } else { edge + 1 };
        if local == edge {
            1.0 - t
        } else if local == next {
            t
        } else {
            0.0
        }
    } else if base < 0 {
        if local == 0 {
            1.0 - scaled
        } else if local == 1 {
            scaled
        } else {
            0.0
        }
    } else if base >= size - 1 {
        let t = scaled - (size - 2) as f64;
        if local == size - 2 {
            1.0 - t
        } else if local == size - 1 {
            t
        } else {
            0.0
        }
    } else if local == base {
        1.0 - t
    } else if local == base + 1 {
        t
    } else {
        0.0
    }
}

fn kind_weight(kind: u8, size: i64, u: f64, v: f64, local: i64) -> f64 {
    match kind {
        0 => polygon_weight(size, u, v, local),
        1 => curve_weight(false, size, u, local),
        2 => curve_weight(true, size, u, local),
        _ => 0.0,
    }
}

fn is_numeric(job: &Job) -> bool {
    matches!(job.source, Data::F(_))
}

fn load_cell(job: &Job, destination: usize) -> Cell {
    let mut cell = Cell::default();
    match &job.source {
        Data::F(_) => {
            for (k, plane) in job.out.f.iter().enumerate() {
                cell.f[k] = plane[destination];
            }
        }
        Data::I(_) => cell.i = job.out.i[destination],
        Data::T(_) => cell.t = job.out.t[destination].clone(),
        Data::IntArray(_) | Data::FloatArray(_) => cell.choice = job.out.choices[destination],
    }
    cell
}

fn store_cell(job: &mut Job, destination: usize, cell: Cell) {
    match &job.source {
        Data::F(_) => {
            for k in 0..job.out.f.len() {
                job.out.f[k][destination] = cell.f[k];
            }
        }
        Data::I(_) => job.out.i[destination] = cell.i,
        Data::T(_) => job.out.t[destination] = cell.t,
        Data::IntArray(_) | Data::FloatArray(_) => job.out.choices[destination] = cell.choice,
    }
}

fn zero_numeric(job: &Job, cell: &mut Cell) {
    if let Data::F(planes) = &job.source {
        for k in 0..planes.len() {
            cell.f[k] = 0.0;
        }
    }
}

fn add_numeric(job: &Job, cell: &mut Cell, source: i64, weight: f64) {
    if let Data::F(planes) = &job.source {
        for (k, plane) in planes.iter().enumerate() {
            cell.f[k] += weight * plane[source as usize];
        }
    }
}

fn set_numeric(job: &Job, cell: &mut Cell, source: i64) {
    if let Data::F(planes) = &job.source {
        for (k, plane) in planes.iter().enumerate() {
            cell.f[k] = plane[source as usize];
        }
    }
}

fn set_discrete(job: &Job, cell: &mut Cell, source: i64) {
    match &job.source {
        Data::I(values) => cell.i = values[source as usize],
        Data::T(values) => cell.t = values[source as usize].to_vec(),
        Data::IntArray(_) | Data::FloatArray(_) => cell.choice = source,
        Data::F(_) => {}
    }
}

fn blend_job(job: &Job, destination: usize, cell: &mut Cell, blend: f64) {
    match (&job.source, &job.existing) {
        (Data::F(planes), existing) => {
            let width = planes.len();
            let mut old = [0.0_f64; 4];
            if let Some(Data::F(old_planes)) = existing {
                for k in 0..width {
                    old[k] = old_planes[k][destination];
                }
            }
            for k in 0..width {
                cell.f[k] = old[k] + (blend * (cell.f[k] - old[k]));
            }
            if width == 3 && job.normalize && blend > 0.0 {
                let (x, y, z) = (cell.f[0], cell.f[1], cell.f[2]);
                let length = ((x * x) + (y * y) + (z * z)).sqrt();
                if length > 1e-20 {
                    cell.f[0] = x / length;
                    cell.f[1] = y / length;
                    cell.f[2] = z / length;
                }
            }
        }
        (Data::I(_), existing) => {
            if blend < 0.5 {
                cell.i = match existing {
                    Some(Data::I(old)) => old[destination],
                    _ => 0,
                };
            }
        }
        (Data::T(_), existing) => {
            if blend < 0.5 {
                cell.t = match existing {
                    Some(Data::T(old)) => old[destination].to_vec(),
                    _ => Vec::new(),
                };
            }
        }
        (Data::IntArray(_) | Data::FloatArray(_), _) => {
            if blend < 0.5 {
                cell.choice = -1;
            }
        }
    }
}

fn default_job(job: &Job, cell: &mut Cell) {
    match &job.source {
        Data::F(planes) => {
            for k in 0..planes.len() {
                cell.f[k] = 0.0;
            }
        }
        Data::I(_) => cell.i = 0,
        Data::T(_) => cell.t = Vec::new(),
        Data::IntArray(_) | Data::FloatArray(_) => cell.choice = -2,
    }
}

/// Runs `work(rank, cells)` for every selected rank in disjoint ranges of `grain`, where `cells[j]`
/// is job `j`'s state at that destination. Outputs are computed per rank, then stored in rank order.
fn run_cells(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    selected: &Selected,
    jobs: &mut Vec<Job>,
    work: &(dyn Fn(i64, i64, &[Job], &mut [Cell]) -> Result<(), error::T> + Sync),
) -> Result<(), error::T> {
    let count = selected.count;
    if count <= 0 {
        return Ok(());
    }
    // ponytail: destinations come from a group order (arbitrary scatter), so results land per rank first and are stored sequentially.
    let mut per_job: Vec<Vec<Cell>> = jobs.iter().map(|_| vec![Cell::default(); count as usize]).collect();
    {
        let shared: &Vec<Job> = jobs;
        let windows: Vec<support::Windows<Cell>> =
            per_job.iter_mut().map(|cells| support::Windows::even(cells, grain)).collect();
        support::run_indexed_ranges(grain, count, &|range, first, last| {
            let mut out: Vec<&mut [Cell]> = windows.iter().map(|window| window.take(range)).collect();
            let mut cells: Vec<Cell> = Vec::with_capacity(shared.len());
            for rank in first..last {
                cancel::poll(cancel_token, rank, 4095)?;
                let destination = selected.element(rank) as usize;
                cells.clear();
                for job in shared.iter() {
                    cells.push(load_cell(job, destination));
                }
                work(rank, destination as i64, shared, &mut cells)?;
                for (job_index, cell) in cells.drain(..).enumerate() {
                    out[job_index][(rank - first) as usize] = cell;
                }
            }
            Ok(())
        })?;
    }
    for (job_index, cells) in per_job.into_iter().enumerate() {
        for (rank, cell) in cells.into_iter().enumerate() {
            store_cell(&mut jobs[job_index], selected.element(rank as i64) as usize, cell);
        }
    }
    Ok(())
}

fn array_offsets(
    cancel_token: Option<&cancel::T>,
    operation: &str,
    source_offsets: &[i64],
    existing_offsets: Option<&[i64]>,
    choices: &[i64],
) -> Result<Vec<i64>, error::T> {
    let count = choices.len();
    let source_count = source_offsets.len() as i64 - 1;
    let mut offsets = vec![0_i64; count + 1];
    for destination in 0..count {
        cancel::poll(cancel_token, destination as i64, 4095)?;
        let choice = choices[destination];
        let length = if choice >= 0 {
            if choice >= source_count {
                return Err(invalid(&format!("{operation}: internal source row is out of bounds")));
            }
            source_offsets[choice as usize + 1] - source_offsets[choice as usize]
        } else if choice == -1 {
            match existing_offsets {
                None => 0,
                Some(old) => old[destination + 1] - old[destination],
            }
        } else if choice == -2 {
            0
        } else {
            return Err(invalid(&format!("{operation}: invalid internal row choice")));
        };
        if length > MAX_INT - offsets[destination] {
            return Err(invalid(&format!("{operation}: packed value count overflow")));
        }
        offsets[destination + 1] = offsets[destination] + length;
    }
    Ok(offsets)
}

fn materialize_int_array(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    source: &packed::IntArray::View::View,
    existing: Option<&packed::IntArray::View::View>,
    choices: &[i64],
) -> Result<packed::IntArray::T, error::T> {
    let offsets = array_offsets(
        cancel_token,
        "Attribute Interpolate int array",
        &source.offsets,
        existing.map(|view| view.offsets.as_slice()),
        choices,
    )?;
    let count = choices.len() as i64;
    let ranges = support::range_count(count, grain);
    let mut values = vec![0_i64; offsets[count as usize] as usize];
    {
        let windows = support::Windows::new(&mut values, ranges, |range| offsets[(range * grain).min(count) as usize] as usize);
        support::run_indexed_ranges(grain, count, &|range, first, last| {
            let base = offsets[first as usize];
            let local = windows.take(range);
            for destination in first..last {
                cancel::poll(cancel_token, destination, 4095)?;
                let d = destination as usize;
                let choice = choices[d];
                let at = (offsets[d] - base) as usize;
                let length = (offsets[d + 1] - offsets[d]) as usize;
                if choice >= 0 {
                    let from = source.offsets[choice as usize] as usize;
                    local[at..at + length].copy_from_slice(&source.values[from..from + length]);
                } else if choice == -1 {
                    if let Some(old) = existing {
                        let from = old.offsets[d] as usize;
                        local[at..at + length].copy_from_slice(&old.values[from..from + length]);
                    }
                }
            }
            Ok(())
        })?;
    }
    Ok(packed::IntArray::View::create_validated_owned(offsets, values))
}

fn materialize_float_array(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    source: &packed::FloatArray::View::View,
    existing: Option<&packed::FloatArray::View::View>,
    choices: &[i64],
) -> Result<packed::FloatArray::T, error::T> {
    let offsets = array_offsets(
        cancel_token,
        "Attribute Interpolate float array",
        &source.offsets,
        existing.map(|view| view.offsets.as_slice()),
        choices,
    )?;
    let count = choices.len() as i64;
    let ranges = support::range_count(count, grain);
    let mut values = vec![0.0_f64; offsets[count as usize] as usize];
    {
        let windows = support::Windows::new(&mut values, ranges, |range| offsets[(range * grain).min(count) as usize] as usize);
        support::run_indexed_ranges(grain, count, &|range, first, last| {
            let base = offsets[first as usize];
            let local = windows.take(range);
            for destination in first..last {
                cancel::poll(cancel_token, destination, 4095)?;
                let d = destination as usize;
                let choice = choices[d];
                let at = (offsets[d] - base) as usize;
                let length = (offsets[d + 1] - offsets[d]) as usize;
                if choice >= 0 {
                    let from = source.offsets[choice as usize] as usize;
                    local[at..at + length].copy_from_slice(&source.values[from..from + length]);
                } else if choice == -1 {
                    if let Some(old) = existing {
                        let from = old.offsets[d] as usize;
                        local[at..at + length].copy_from_slice(&old.values[from..from + length]);
                    }
                }
            }
            Ok(())
        })?;
    }
    Ok(packed::FloatArray::View::create_validated_owned(offsets, values))
}

fn storage_of_job(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    job: &Job,
) -> Result<attribute::Storage, error::T> {
    Ok(match &job.source {
        Data::F(planes) => {
            let mut planes_out = job.out.f.iter().cloned();
            match planes.len() {
                1 => attribute::Storage::Float(Arc::new(planes_out.next().unwrap_or_default())),
                2 => attribute::Storage::Float2(packed::Float2::of_owned(
                    planes_out.next().unwrap_or_default(),
                    planes_out.next().unwrap_or_default(),
                )?),
                3 => attribute::Storage::Float3(packed::Float3::View::of_owned_exn(
                    planes_out.next().unwrap_or_default(),
                    planes_out.next().unwrap_or_default(),
                    planes_out.next().unwrap_or_default(),
                )?),
                _ => attribute::Storage::Float4(packed::Float4::of_owned(
                    planes_out.next().unwrap_or_default(),
                    planes_out.next().unwrap_or_default(),
                    planes_out.next().unwrap_or_default(),
                    planes_out.next().unwrap_or_default(),
                )?),
            }
        }
        Data::I(_) => attribute::Storage::Int(Arc::new(job.out.i.clone())),
        Data::T(_) => attribute::Storage::Text(Arc::new(job.out.t.iter().collect())),
        Data::IntArray(source) => {
            let existing = match &job.existing {
                Some(Data::IntArray(view)) => Some(view),
                _ => None,
            };
            attribute::Storage::IntArray(materialize_int_array(
                cancel_token,
                grain,
                source,
                existing,
                &job.out.choices,
            )?)
        }
        Data::FloatArray(source) => {
            let existing = match &job.existing {
                Some(Data::FloatArray(view)) => Some(view),
                _ => None,
            };
            attribute::Storage::FloatArray(materialize_float_array(
                cancel_token,
                grain,
                source,
                existing,
                &job.out.choices,
            )?)
        }
    })
}

fn groups_of_jobs(jobs: &[GroupJob]) -> Result<Vec<group::T>, error::T> {
    jobs.iter()
        .map(|job| {
            group::View::of_owned_bits(
                job.group_target_owner,
                &job.group_target_name,
                job.group_target_count,
                job.group_output_bits.clone(),
            )
        })
        .collect()
}

fn existing_group_value(job: &GroupJob, destination: i64) -> f64 {
    match &job.group_existing_bits {
        Some(bits) if bit_mem(bits, destination) => 1.0,
        _ => 0.0,
    }
}

fn commit_group_score(job: &GroupJob, local: &mut [u8], base_byte: usize, destination: i64, blend: f64, score: f64) {
    let old = existing_group_value(job, destination);
    let member = old + (blend * (score - old)) >= 0.5;
    let index = destination as usize;
    let byte = (index >> 3) - base_byte;
    let mask = 1_u8 << (index & 7);
    if member {
        local[byte] |= mask;
    } else {
        local[byte] &= !mask;
    }
}

/// Runs `work(destination, locals)` for every destination, with `locals[j]` the bytes of job `j`'s
/// output bits for the byte range being processed (`base_byte` is its first byte).
fn run_group_bytes(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    selection: Option<&group::T>,
    jobs: &mut Vec<GroupJob>,
    work: &(dyn Fn(i64, &[GroupJob], &mut [&mut [u8]], usize) -> Result<(), error::T> + Sync),
) -> Result<(), error::T> {
    if jobs.is_empty() {
        return Ok(());
    }
    let target_count = jobs[0].group_target_count;
    let byte_count = (target_count + 7) / 8;
    if byte_count <= 0 {
        return Ok(());
    }
    let chunk = (grain / 8).max(1);
    // Detach the output bits so the shared job view and the disjoint output windows never alias.
    let mut outputs: Vec<Vec<u8>> = jobs.iter_mut().map(|job| std::mem::take(&mut job.group_output_bits)).collect();
    let result = {
        let shared: &Vec<GroupJob> = jobs;
        let windows: Vec<support::Windows<u8>> =
            outputs.iter_mut().map(|bits| support::Windows::even(&mut bits[..byte_count as usize], chunk)).collect();
        support::run_indexed_ranges(chunk, byte_count, &|range, first_byte, last_byte| {
            let mut locals: Vec<&mut [u8]> = windows.iter().map(|window| window.take(range)).collect();
            for byte in first_byte..last_byte {
                cancel::poll(cancel_token, byte, 511)?;
                let first_destination = byte << 3;
                let last_destination = target_count.min(first_destination + 8);
                for destination in first_destination..last_destination {
                    let selected = match selection {
                        None => true,
                        Some(value) => group::mem(destination, value),
                    };
                    if selected {
                        work(destination, shared, &mut locals, first_byte as usize)?;
                    }
                }
            }
            Ok(())
        })
    };
    for (job, bits) in jobs.iter_mut().zip(outputs) {
        job.group_output_bits = bits;
    }
    result
}

fn clear_group_bits(locals: &mut [&mut [u8]], base_byte: usize, destination: i64) {
    let index = destination as usize;
    for local in locals.iter_mut() {
        bit_set(local, index - (base_byte << 3), false);
    }
}

#[allow(clippy::too_many_arguments)]
fn interpolate_primitive_groups(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    selection: Option<&group::T>,
    unmatched: Unmatched,
    blend: f64,
    pre_scale: f64,
    primitive_numbers: &[i64],
    uvw: &packed::Float3::View::View,
    topology_view: &topology::View::View,
    primitive_count: i64,
    jobs: &mut Vec<GroupJob>,
) -> Result<(), error::T> {
    run_group_bytes(cancel_token, grain, selection, jobs, &|destination, shared, locals, base_byte| {
        let primitive = primitive_numbers[destination as usize];
        if primitive < 0 || primitive >= primitive_count {
            if unmatched == Unmatched::DefaultValue {
                clear_group_bits(locals, base_byte, destination);
            }
            return Ok(());
        }
        let first = topology_view.primitive_offsets[primitive as usize];
        let last = topology_view.primitive_offsets[primitive as usize + 1];
        let size = last - first;
        let u = uvw.x[destination as usize] * pre_scale;
        let v = uvw.y[destination as usize] * pre_scale;
        let kind = topology_view.primitive_kinds[primitive as usize];
        for (index, job) in shared.iter().enumerate() {
            let score = match job.group_source_owner {
                attribute::Owner::Primitive => {
                    if bit_mem(&job.group_source_bits, primitive) {
                        1.0
                    } else {
                        0.0
                    }
                }
                attribute::Owner::Point | attribute::Owner::Vertex => {
                    let mut score = 0.0_f64;
                    for local in 0..size {
                        let weight = kind_weight(kind, size, u, v, local);
                        let vertex = first + local;
                        let source_element = if job.group_source_owner == attribute::Owner::Point {
                            topology_view.vertex_points[vertex as usize]
                        } else {
                            vertex
                        };
                        if bit_mem(&job.group_source_bits, source_element) {
                            score += weight;
                        }
                    }
                    score
                }
                attribute::Owner::Detail => 0.0,
            };
            commit_group_score(job, &mut locals[index], base_byte, destination, blend, score);
        }
        Ok(())
    })
}

#[allow(clippy::too_many_arguments)]
fn compute_primitive_weights(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    selection: Option<&group::T>,
    unmatched: Unmatched,
    target_owner: attribute::Owner,
    computed: &Computed,
    primitive_numbers: &[i64],
    uvw: &packed::Float3::View::View,
    pre_scale: f64,
    source: &geometry::T,
    target: &geometry::T,
) -> Result<Vec<attribute::T>, error::T> {
    let numbers_name = &computed.computed_numbers_attribute;
    let weights_name = &computed.computed_weights_attribute;
    if computed.computed_owner == WeightedSource::Primitives {
        return Err(fail("Attribute Interpolate: computed weights support point or vertex numbers"));
    }
    if is_blank(numbers_name) || is_blank(weights_name) {
        return Err(fail("Attribute Interpolate: computed attribute names must not be empty"));
    }
    if numbers_name == weights_name {
        return Err(fail("Attribute Interpolate: computed number and weight attributes must have different names"));
    }
    if target_owner == attribute::Owner::Point && (numbers_name == "P" || weights_name == "P") {
        return Err(fail("Attribute Interpolate: canonical P cannot store computed arrays"));
    }
    let existing_numbers = geometry::find_attribute(target_owner, numbers_name, target);
    let existing_weights = geometry::find_attribute(target_owner, weights_name, target);
    let existing: Option<(packed::IntArray::View::View, packed::FloatArray::View::View)> =
        match (existing_numbers, existing_weights) {
            (None, None) => None,
            (Some(numbers), Some(weights)) => {
                match (attribute::View::storage(&numbers)?, attribute::View::storage(&weights)?) {
                    (attribute::Storage::IntArray(numbers), attribute::Storage::FloatArray(weights)) => {
                        let numbers = packed::IntArray::View::view(numbers);
                        let weights = packed::FloatArray::View::view(weights);
                        if *numbers.offsets == *weights.offsets {
                            Some((numbers, weights))
                        } else {
                            return Err(fail("Attribute Interpolate: existing computed array row layouts differ"));
                        }
                    }
                    _ => {
                        return Err(fail(
                            "Attribute Interpolate: existing computed attributes must be int-array and float-array",
                        ))
                    }
                }
            }
            _ => {
                return Err(fail(
                    "Attribute Interpolate: existing computed number and weight attributes must occur together",
                ))
            }
        };
    let topology_view = topology::View::view(geometry::topology(source))?;
    let primitive_count = geometry::primitive_count(source);
    let target_count = owner_count(target, target_owner);
    let selected = |destination: i64| match selection {
        None => true,
        Some(value) => group::mem(destination, value),
    };
    let preserve = |destination: i64, primitive: i64| {
        !selected(destination)
            || (unmatched == Unmatched::KeepTarget && (primitive < 0 || primitive >= primitive_count))
    };
    let row_length = |destination: i64| -> i64 {
        let primitive = primitive_numbers[destination as usize];
        if preserve(destination, primitive) {
            match &existing {
                None => 0,
                Some((numbers, _)) => numbers.offsets[destination as usize + 1] - numbers.offsets[destination as usize],
            }
        } else if primitive < 0 || primitive >= primitive_count {
            0
        } else {
            topology_view.primitive_offsets[primitive as usize + 1] - topology_view.primitive_offsets[primitive as usize]
        }
    };
    let zeros = vec![0_i64; target_count as usize];
    let lengths = crate::attribute_ops::map_ints(cancel_token, grain, &zeros, &|destination, slot| {
        *slot = row_length(destination);
        Ok(())
    })?;
    let mut offsets = vec![0_i64; target_count as usize + 1];
    for destination in 0..target_count as usize {
        cancel::poll(cancel_token, destination as i64, 4095)?;
        if lengths[destination] > MAX_INT - offsets[destination] {
            return Err(invalid("Attribute Interpolate: computed weight cardinality overflow"));
        }
        offsets[destination + 1] = offsets[destination] + lengths[destination];
    }
    let ranges = support::range_count(target_count, grain);
    let bound = |range: i64| offsets[(range * grain).min(target_count) as usize] as usize;
    let total_values = offsets[target_count as usize] as usize;
    let (mut numbers, mut weights) = (vec![0_i64; total_values], vec![0.0_f64; total_values]);
    {
        let (number_windows, weight_windows) =
            (support::Windows::new(&mut numbers, ranges, bound), support::Windows::new(&mut weights, ranges, bound));
        support::run_indexed_ranges(grain, target_count, &|range, first_destination, last_destination| {
            let base = offsets[first_destination as usize];
            let (numbers, weights) = (number_windows.take(range), weight_windows.take(range));
            for destination in first_destination..last_destination {
                cancel::poll(cancel_token, destination, 4095)?;
                let d = destination as usize;
                let primitive = primitive_numbers[d];
                let output_first = (offsets[d] - base) as usize;
                if preserve(destination, primitive) {
                    if let Some((old_numbers, old_weights)) = &existing {
                        let old_first = old_numbers.offsets[d] as usize;
                        let length = lengths[d] as usize;
                        numbers[output_first..output_first + length]
                            .copy_from_slice(&old_numbers.values[old_first..old_first + length]);
                        weights[output_first..output_first + length]
                            .copy_from_slice(&old_weights.values[old_first..old_first + length]);
                    }
                } else if primitive >= 0 && primitive < primitive_count {
                    let first = topology_view.primitive_offsets[primitive as usize];
                    let last = topology_view.primitive_offsets[primitive as usize + 1];
                    let size = last - first;
                    let u = uvw.x[d] * pre_scale;
                    let v = uvw.y[d] * pre_scale;
                    let kind = topology_view.primitive_kinds[primitive as usize];
                    for local in 0..size {
                        let vertex = first + local;
                        numbers[output_first + local as usize] = if computed.computed_owner == WeightedSource::Points {
                            topology_view.vertex_points[vertex as usize]
                        } else {
                            vertex
                        };
                        weights[output_first + local as usize] = kind_weight(kind, size, u, v, local);
                    }
                }
            }
            Ok(())
        })?;
    }
    let numbers = packed::IntArray::View::create_validated_owned(offsets.clone(), numbers);
    let weights = packed::FloatArray::View::create_validated_owned(offsets, weights);
    Ok(vec![
        attribute::create_owned(numbers_name, target_owner, attribute::Storage::IntArray(numbers))?,
        attribute::create_owned(weights_name, target_owner, attribute::Storage::FloatArray(weights))?,
    ])
}

fn install(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    extra: Vec<attribute::T>,
    groups: Vec<group::T>,
    jobs: &[Job],
    target: &geometry::T,
) -> Result<geometry::T, error::T> {
    let mut position: Option<packed::Float3::T> = None;
    let mut attributes: Vec<attribute::T> = Vec::new();
    for job in jobs {
        let storage = storage_of_job(cancel_token, grain, job)?;
        if job.position {
            match storage {
                attribute::Storage::Float3(positions) => position = Some(positions),
                _ => return Err(fail("Attribute Interpolate: P requires float3 storage")),
            }
        } else {
            attributes.push(attribute::create_owned(&job.target_name, job.target_owner, storage)?);
        }
    }
    attributes.extend(extra);
    geometry::View::with_merged_owned(position, attributes, groups, target)
}

fn collect_jobs(
    attributes: &[Spec],
    target_owner: attribute::Owner,
    source: &geometry::T,
    target: &geometry::T,
    supported: &dyn Fn(&Spec) -> Option<error::T>,
) -> Result<(Vec<Job>, HashSet<String>), error::T> {
    let mut seen: HashSet<String> = HashSet::new();
    let mut jobs = Vec::new();
    for spec in attributes {
        if let Some(failure) = supported(spec) {
            return Err(failure);
        }
        if !seen.insert(spec.target_name.clone()) {
            return Err(fail(&format!(
                "Attribute Interpolate: duplicate target attribute {}",
                ocaml_quote(&spec.target_name)
            )));
        }
        jobs.push(create_job(target_owner, source, target, spec)?);
    }
    Ok((jobs, seen))
}

fn indices_of(jobs: &[Job], owner: attribute::Owner, numeric: bool) -> Vec<usize> {
    (0..jobs.len()).filter(|&index| jobs[index].source_owner == owner && is_numeric(&jobs[index]) == numeric).collect()
}

pub fn interpolate_primitive(params: &PrimitiveParams) -> Result<geometry::T, error::T> {
    let cancel_token = params.cancel.as_deref();
    let grain = params.grain.unwrap_or(16_384);
    let pre_scale = params.pre_scale.unwrap_or(1.0);
    let blend = params.blend.unwrap_or(1.0);
    let unmatched = params.unmatched.unwrap_or(Unmatched::KeepTarget);
    let primitive_attribute = params.primitive_attribute.clone().unwrap_or_else(|| "source_primitive".to_owned());
    let uvw_attribute = params.uvw_attribute.clone().unwrap_or_else(|| "source_uvw".to_owned());
    let target_owner = params.target_owner;
    let (source, target) = (&params.source, &params.target);
    if grain <= 0 {
        return Err(invalid("Attribute Interpolate: grain must be positive"));
    }
    cancel::check_opt(cancel_token)?;
    if !pre_scale.is_finite() {
        return Err(fail("Attribute Interpolate: pre_scale must be finite"));
    }
    if !blend.is_finite() || !(0.0..=1.0).contains(&blend) {
        return Err(fail("Attribute Interpolate: blend must be finite and in [0,1]"));
    }
    if is_blank(&primitive_attribute) || is_blank(&uvw_attribute) {
        return Err(fail("Attribute Interpolate: driver attribute names must not be empty"));
    }
    let primitive_driver = geometry::find_attribute(target_owner, &primitive_attribute, target);
    let uvw_driver = geometry::find_attribute(target_owner, &uvw_attribute, target);
    let Some(primitive_driver) = primitive_driver else {
        return Err(fail(&format!(
            "Attribute Interpolate: missing destination integer attribute {}",
            ocaml_quote(&primitive_attribute)
        )));
    };
    let Some(uvw_driver) = uvw_driver else {
        return Err(fail(&format!(
            "Attribute Interpolate: missing destination float3 attribute {}",
            ocaml_quote(&uvw_attribute)
        )));
    };
    let (primitive_numbers, uvw) = match (attribute::View::storage(&primitive_driver)?, attribute::View::storage(&uvw_driver)?) {
        (attribute::Storage::Int(numbers), attribute::Storage::Float3(uvw)) => {
            (numbers.clone(), packed::Float3::View::view(uvw)?)
        }
        _ => return Err(fail("Attribute Interpolate: primitive driver must be int and UVW driver float3")),
    };
    let selected = selection_elements(target_owner, params.selection.as_ref(), target)?;
    let mut group_jobs = create_group_jobs(target_owner, target, &params.groups)?;
    let (mut jobs, seen) = collect_jobs(&params.attributes, target_owner, source, target, &|_| None)?;
    if let Some(computed) = &params.compute {
        let numbers = &computed.computed_numbers_attribute;
        let weights = &computed.computed_weights_attribute;
        if *numbers == primitive_attribute
            || *numbers == uvw_attribute
            || *weights == primitive_attribute
            || *weights == uvw_attribute
        {
            return Err(fail("Attribute Interpolate: computed attributes must not replace driver attributes"));
        }
        if seen.contains(numbers) || seen.contains(weights) {
            return Err(fail("Attribute Interpolate: computed attributes must not replace interpolated outputs"));
        }
    }
    if (jobs.is_empty() && group_jobs.is_empty() && params.compute.is_none()) || selected.count == 0 {
        return Ok(target.clone());
    }
    for rank in 0..selected.count {
        cancel::poll(cancel_token, rank, 4095)?;
        let element = selected.element(rank) as usize;
        if !(uvw.x[element].is_finite() && uvw.y[element].is_finite() && uvw.z[element].is_finite()) {
            return Err(invalid("Attribute Interpolate: UVW contains a non-finite value"));
        }
    }
    let topology_view = topology::View::view(geometry::topology(source))?;
    let primitive_count = geometry::primitive_count(source);
    let point_numeric = indices_of(&jobs, attribute::Owner::Point, true);
    let vertex_numeric = indices_of(&jobs, attribute::Owner::Vertex, true);
    let primitive_numeric = indices_of(&jobs, attribute::Owner::Primitive, true);
    let detail_numeric = indices_of(&jobs, attribute::Owner::Detail, true);
    let point_discrete = indices_of(&jobs, attribute::Owner::Point, false);
    let vertex_discrete = indices_of(&jobs, attribute::Owner::Vertex, false);
    let primitive_discrete = indices_of(&jobs, attribute::Owner::Primitive, false);
    let detail_discrete = indices_of(&jobs, attribute::Owner::Detail, false);
    let numeric: Vec<usize> = [&point_numeric, &vertex_numeric, &primitive_numeric, &detail_numeric]
        .iter()
        .flat_map(|list| list.iter().copied())
        .collect();
    run_cells(cancel_token, grain, &selected, &mut jobs, &|_, destination, shared, cells| {
        let primitive = primitive_numbers[destination as usize];
        if primitive < 0 || primitive >= primitive_count {
            if unmatched == Unmatched::DefaultValue {
                for (index, job) in shared.iter().enumerate() {
                    default_job(job, &mut cells[index]);
                }
            }
            return Ok(());
        }
        let first = topology_view.primitive_offsets[primitive as usize];
        let last = topology_view.primitive_offsets[primitive as usize + 1];
        let size = last - first;
        let u = uvw.x[destination as usize] * pre_scale;
        let v = uvw.y[destination as usize] * pre_scale;
        let kind = topology_view.primitive_kinds[primitive as usize];
        for &index in &numeric {
            zero_numeric(&shared[index], &mut cells[index]);
        }
        let mut best_local = 0_i64;
        let mut best_weight = f64::NEG_INFINITY;
        for local in 0..size {
            let weight = kind_weight(kind, size, u, v, local);
            if weight > best_weight {
                best_weight = weight;
                best_local = local;
            }
            let vertex = first + local;
            let point = topology_view.vertex_points[vertex as usize];
            for &index in &point_numeric {
                add_numeric(&shared[index], &mut cells[index], point, weight);
            }
            for &index in &vertex_numeric {
                add_numeric(&shared[index], &mut cells[index], vertex, weight);
            }
        }
        let best_vertex = first + best_local;
        let best_point = topology_view.vertex_points[best_vertex as usize];
        for &index in &primitive_numeric {
            set_numeric(&shared[index], &mut cells[index], primitive);
        }
        for &index in &detail_numeric {
            set_numeric(&shared[index], &mut cells[index], 0);
        }
        for &index in &point_discrete {
            set_discrete(&shared[index], &mut cells[index], best_point);
        }
        for &index in &vertex_discrete {
            set_discrete(&shared[index], &mut cells[index], best_vertex);
        }
        for &index in &primitive_discrete {
            set_discrete(&shared[index], &mut cells[index], primitive);
        }
        for &index in &detail_discrete {
            set_discrete(&shared[index], &mut cells[index], 0);
        }
        for (index, job) in shared.iter().enumerate() {
            blend_job(job, destination as usize, &mut cells[index], blend);
        }
        Ok(())
    })?;
    interpolate_primitive_groups(
        cancel_token,
        grain,
        params.selection.as_ref(),
        unmatched,
        blend,
        pre_scale,
        &primitive_numbers,
        &uvw,
        &topology_view,
        primitive_count,
        &mut group_jobs,
    )?;
    let extra = match &params.compute {
        None => Vec::new(),
        Some(computed) => compute_primitive_weights(
            cancel_token,
            grain,
            params.selection.as_ref(),
            unmatched,
            target_owner,
            computed,
            &primitive_numbers,
            &uvw,
            pre_scale,
            source,
            target,
        )?,
    };
    install(cancel_token, grain, extra, groups_of_jobs(&group_jobs)?, &jobs, target)
}

fn weighted_owner_name(owner: WeightedSource) -> &'static str {
    match owner {
        WeightedSource::Points => "point",
        WeightedSource::Vertices => "vertex",
        WeightedSource::Primitives => "primitive",
    }
}

fn weighted_source_count(source: &geometry::T, owner: WeightedSource) -> i64 {
    match owner {
        WeightedSource::Points => geometry::point_count(source),
        WeightedSource::Vertices => geometry::vertex_count(source),
        WeightedSource::Primitives => geometry::primitive_count(source),
    }
}

fn weighted_owner_supported(weighted_owner: WeightedSource, source_owner: attribute::Owner) -> bool {
    match weighted_owner {
        WeightedSource::Points => matches!(source_owner, attribute::Owner::Point | attribute::Owner::Detail),
        WeightedSource::Vertices => true,
        WeightedSource::Primitives => matches!(source_owner, attribute::Owner::Primitive | attribute::Owner::Detail),
    }
}

fn weighted_source_index(
    weighted_owner: WeightedSource,
    source_owner: attribute::Owner,
    topology_view: &topology::View::View,
    primitive_of_vertex: &[i64],
    source_element: i64,
) -> i64 {
    match (weighted_owner, source_owner) {
        (_, attribute::Owner::Detail) => 0,
        (WeightedSource::Points, attribute::Owner::Point) => source_element,
        (WeightedSource::Vertices, attribute::Owner::Point) => topology_view.vertex_points[source_element as usize],
        (WeightedSource::Vertices, attribute::Owner::Vertex) => source_element,
        (WeightedSource::Vertices, attribute::Owner::Primitive) => primitive_of_vertex[source_element as usize],
        (WeightedSource::Primitives, attribute::Owner::Primitive) => source_element,
        _ => 0,
    }
}

#[allow(clippy::too_many_arguments)]
fn interpolate_weighted_groups(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    selection: Option<&group::T>,
    unmatched: Unmatched,
    blend: f64,
    pre_scale: f64,
    normalize_weights: bool,
    threshold: f64,
    weighted_owner: WeightedSource,
    numbers: &packed::IntArray::View::View,
    weights: &packed::FloatArray::View::View,
    topology_view: &topology::View::View,
    primitive_of_vertex: &[i64],
    jobs: &mut Vec<GroupJob>,
) -> Result<(), error::T> {
    run_group_bytes(cancel_token, grain, selection, jobs, &|destination, shared, locals, base_byte| {
        let first = numbers.offsets[destination as usize];
        let last = numbers.offsets[destination as usize + 1];
        if first == last {
            if unmatched == Unmatched::DefaultValue {
                clear_group_bits(locals, base_byte, destination);
            }
            return Ok(());
        }
        let mut raw_sum = 0.0_f64;
        for slot in first..last {
            raw_sum += weights.values[slot as usize] * pre_scale;
        }
        let (normalization, influence) = if normalize_weights {
            let magnitude = raw_sum.abs();
            (if magnitude > 1e-20 { 1.0 / raw_sum } else { 0.0 }, (magnitude / threshold).min(1.0))
        } else {
            (1.0, 1.0)
        };
        let effective_blend = blend * influence;
        for (index, job) in shared.iter().enumerate() {
            let mut score = 0.0_f64;
            for slot in first..last {
                let source_element = weighted_source_index(
                    weighted_owner,
                    job.group_source_owner,
                    topology_view,
                    primitive_of_vertex,
                    numbers.values[slot as usize],
                );
                if bit_mem(&job.group_source_bits, source_element) {
                    score += weights.values[slot as usize] * pre_scale * normalization;
                }
            }
            commit_group_score(job, &mut locals[index], base_byte, destination, effective_blend, score);
        }
        Ok(())
    })
}

pub fn interpolate_weighted(params: &WeightedParams) -> Result<geometry::T, error::T> {
    let cancel_token = params.cancel.as_deref();
    let grain = params.grain.unwrap_or(16_384);
    let numbers_attribute = params.numbers_attribute.clone().unwrap_or_else(|| "source_elements".to_owned());
    let weights_attribute = params.weights_attribute.clone().unwrap_or_else(|| "source_weights".to_owned());
    let pre_scale = params.pre_scale.unwrap_or(1.0);
    let normalize_weights = params.normalize_weights.unwrap_or(false);
    let threshold = params.threshold.unwrap_or(1e-6);
    let blend = params.blend.unwrap_or(1.0);
    let unmatched = params.unmatched.unwrap_or(Unmatched::KeepTarget);
    let weighted_owner = params.weighted_owner;
    let target_owner = params.target_owner;
    let (source, target) = (&params.source, &params.target);
    if grain <= 0 {
        return Err(invalid("Attribute Interpolate: grain must be positive"));
    }
    cancel::check_opt(cancel_token)?;
    if !pre_scale.is_finite() {
        return Err(fail("Attribute Interpolate: pre_scale must be finite"));
    }
    if !blend.is_finite() || !(0.0..=1.0).contains(&blend) {
        return Err(fail("Attribute Interpolate: blend must be finite and in [0,1]"));
    }
    if !threshold.is_finite() || threshold <= 0.0 {
        return Err(fail("Attribute Interpolate: threshold must be finite and positive"));
    }
    if is_blank(&numbers_attribute) || is_blank(&weights_attribute) {
        return Err(fail("Attribute Interpolate: driver attribute names must not be empty"));
    }
    let numbers_driver = geometry::find_attribute(target_owner, &numbers_attribute, target);
    let weights_driver = geometry::find_attribute(target_owner, &weights_attribute, target);
    let Some(numbers_driver) = numbers_driver else {
        return Err(fail(&format!(
            "Attribute Interpolate: missing destination integer-array attribute {}",
            ocaml_quote(&numbers_attribute)
        )));
    };
    let Some(weights_driver) = weights_driver else {
        return Err(fail(&format!(
            "Attribute Interpolate: missing destination float-array attribute {}",
            ocaml_quote(&weights_attribute)
        )));
    };
    let (numbers, weights) = match (attribute::View::storage(&numbers_driver)?, attribute::View::storage(&weights_driver)?) {
        (attribute::Storage::IntArray(numbers), attribute::Storage::FloatArray(weights)) => {
            (packed::IntArray::View::view(numbers), packed::FloatArray::View::view(weights))
        }
        _ => return Err(fail("Attribute Interpolate: weighted drivers must be int-array and float-array")),
    };
    if *numbers.offsets != *weights.offsets {
        return Err(fail("Attribute Interpolate: number and weight row layouts must match"));
    }
    let selected = selection_elements(target_owner, params.selection.as_ref(), target)?;
    if let Some(spec) = params.groups.iter().find(|spec| !weighted_owner_supported(weighted_owner, spec.group_source_owner)) {
        return Err(fail(&format!(
            "Attribute Interpolate: {}-number mode cannot interpolate {} group {}",
            weighted_owner_name(weighted_owner),
            owner_name(spec.group_source_owner),
            ocaml_quote(group::name(&spec.group_source))
        )));
    }
    let mut group_jobs = create_group_jobs(target_owner, target, &params.groups)?;
    let (mut jobs, _) = collect_jobs(&params.attributes, target_owner, source, target, &|spec| {
        if weighted_owner_supported(weighted_owner, spec.source_owner) {
            None
        } else {
            Some(fail(&format!(
                "Attribute Interpolate: {}-number mode cannot interpolate {} attribute {}",
                weighted_owner_name(weighted_owner),
                owner_name(spec.source_owner),
                ocaml_quote(&spec.source_name)
            )))
        }
    })?;
    if (jobs.is_empty() && group_jobs.is_empty()) || selected.count == 0 {
        return Ok(target.clone());
    }
    let source_count = weighted_source_count(source, weighted_owner);
    for rank in 0..selected.count {
        cancel::poll(cancel_token, rank, 4095)?;
        let destination = selected.element(rank);
        let first = numbers.offsets[destination as usize];
        let last = numbers.offsets[destination as usize + 1];
        for slot in first..last {
            let source_element = numbers.values[slot as usize];
            let weight = weights.values[slot as usize];
            if source_element < 0 || source_element >= source_count {
                return Err(fail(&format!(
                    "Attribute Interpolate: {} number {} is out of bounds at destination {}, weight {}",
                    weighted_owner_name(weighted_owner),
                    source_element,
                    destination,
                    slot - first
                )));
            } else if !weight.is_finite() {
                return Err(fail(&format!(
                    "Attribute Interpolate: weight is non-finite at destination {}, weight {}",
                    destination,
                    slot - first
                )));
            }
        }
    }
    let topology_view = topology::View::view(geometry::topology(source))?;
    let needs_primitive_map = weighted_owner == WeightedSource::Vertices
        && (jobs.iter().any(|job| job.source_owner == attribute::Owner::Primitive)
            || group_jobs.iter().any(|job| job.group_source_owner == attribute::Owner::Primitive));
    let primitive_of_vertex: Vec<i64> = if needs_primitive_map {
        let mut output = vec![-1_i64; geometry::vertex_count(source) as usize];
        for primitive in 0..geometry::primitive_count(source) {
            for vertex in topology_view.primitive_offsets[primitive as usize]..topology_view.primitive_offsets[primitive as usize + 1] {
                output[vertex as usize] = primitive;
            }
        }
        output
    } else {
        Vec::new()
    };
    let numeric: Vec<usize> = (0..jobs.len()).filter(|&index| is_numeric(&jobs[index])).collect();
    let discrete: Vec<usize> = (0..jobs.len()).filter(|&index| !is_numeric(&jobs[index])).collect();
    run_cells(cancel_token, grain, &selected, &mut jobs, &|_, destination, shared, cells| {
        let first = numbers.offsets[destination as usize];
        let last = numbers.offsets[destination as usize + 1];
        if first == last {
            if unmatched == Unmatched::DefaultValue {
                for (index, job) in shared.iter().enumerate() {
                    default_job(job, &mut cells[index]);
                }
            }
            return Ok(());
        }
        let mut raw_sum = 0.0_f64;
        for slot in first..last {
            raw_sum += weights.values[slot as usize] * pre_scale;
        }
        let (normalization, influence) = if normalize_weights {
            let magnitude = raw_sum.abs();
            (if magnitude > 1e-20 { 1.0 / raw_sum } else { 0.0 }, (magnitude / threshold).min(1.0))
        } else {
            (1.0, 1.0)
        };
        for &index in &numeric {
            zero_numeric(&shared[index], &mut cells[index]);
        }
        let mut best_source = numbers.values[first as usize];
        let mut best_weight = f64::NEG_INFINITY;
        for slot in first..last {
            let source_element = numbers.values[slot as usize];
            let weight = weights.values[slot as usize] * pre_scale * normalization;
            if weight > best_weight {
                best_weight = weight;
                best_source = source_element;
            }
            for &index in &numeric {
                let job = &shared[index];
                let mapped =
                    weighted_source_index(weighted_owner, job.source_owner, &topology_view, &primitive_of_vertex, source_element);
                add_numeric(job, &mut cells[index], mapped, weight);
            }
        }
        for &index in &discrete {
            let job = &shared[index];
            let mapped =
                weighted_source_index(weighted_owner, job.source_owner, &topology_view, &primitive_of_vertex, best_source);
            set_discrete(job, &mut cells[index], mapped);
        }
        let effective_blend = blend * influence;
        for (index, job) in shared.iter().enumerate() {
            blend_job(job, destination as usize, &mut cells[index], effective_blend);
        }
        Ok(())
    })?;
    interpolate_weighted_groups(
        cancel_token,
        grain,
        params.selection.as_ref(),
        unmatched,
        blend,
        pre_scale,
        normalize_weights,
        threshold,
        weighted_owner,
        &numbers,
        &weights,
        &topology_view,
        &primitive_of_vertex,
        &mut group_jobs,
    )?;
    install(cancel_token, grain, Vec::new(), groups_of_jobs(&group_jobs)?, &jobs, target)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::attribute_ops::{
        self, InterpolateAttribute, InterpolateComputed, InterpolateDriver, Params,
        fixtures::*, fixtures2::*,
    };
    use crate::attribute_transfer::Unmatched as TransferUnmatched;
    use attribute::Owner::{Detail, Point, Primitive, Vertex};
    use rdk_core::topology::PrimitiveKind;

    fn blank_points(count: i64) -> geometry::T {
        points(&vec![(0.0, 0.0, 0.0); count as usize])
    }

    fn add_group_ordered(owner: group::Owner, name: &str, elements: &[i64], g: &geometry::T) -> geometry::T {
        let length = match owner {
            group::Owner::Point => geometry::point_count(g),
            group::Owner::Vertex => geometry::vertex_count(g),
            group::Owner::Primitive => geometry::primitive_count(g),
        };
        geometry::with_group(group::ordered(owner, name, length, elements).unwrap(), g).unwrap()
    }

    fn group_values(owner: group::Owner, name: &str, g: &geometry::T) -> Vec<bool> {
        let value = geometry::find_group(owner, name, g).unwrap_or_else(|| panic!("missing group {name}"));
        (0..group::length(&value)).map(|element| group::mem(element, &value)).collect()
    }

    fn float3_planes(values: &[(f64, f64, f64)]) -> (Vec<f64>, Vec<f64>, Vec<f64>) {
        (
            values.iter().map(|v| v.0).collect(),
            values.iter().map(|v| v.1).collect(),
            values.iter().map(|v| v.2).collect(),
        )
    }

    fn add_drivers(owner: attribute::Owner, primitives: Vec<i64>, uvw: &[(f64, f64, f64)], g: &geometry::T) -> geometry::T {
        let (x, y, z) = float3_planes(uvw);
        add_float3s(owner, "source_uvw", x, y, z, &add_ints(owner, "source_primitive", primitives, g))
    }

    fn int_rows(rows: &[Vec<i64>]) -> packed::IntArray::T {
        let mut offsets = vec![0_i64];
        let mut values = Vec::new();
        for row in rows {
            values.extend(row);
            offsets.push(values.len() as i64);
        }
        packed::IntArray::create_owned(offsets, values).unwrap()
    }

    fn float_rows(rows: &[Vec<f64>]) -> packed::FloatArray::T {
        let mut offsets = vec![0_i64];
        let mut values = Vec::new();
        for row in rows {
            values.extend(row);
            offsets.push(values.len() as i64);
        }
        packed::FloatArray::create_owned(offsets, values).unwrap()
    }

    fn add_weight_drivers(owner: attribute::Owner, numbers: &[Vec<i64>], weights: &[Vec<f64>], g: &geometry::T) -> geometry::T {
        let g = add_storage(owner, "source_elements", attribute::Storage::IntArray(int_rows(numbers)), g);
        add_storage(owner, "source_weights", attribute::Storage::FloatArray(float_rows(weights)), &g)
    }

    fn spec(owner: attribute::Owner, name: &str) -> InterpolateAttribute {
        attribute_ops::interpolate_attribute(None, owner, name).unwrap()
    }

    fn spec_into(owner: attribute::Owner, name: &str, into: &str) -> InterpolateAttribute {
        attribute_ops::interpolate_attribute(Some(into), owner, name).unwrap()
    }

    fn base(source: &geometry::T, target: &geometry::T, target_owner: attribute::Owner, attributes: Vec<InterpolateAttribute>) -> Params {
        Params {
            cancel: None,
            grain: None,
            selection: None,
            driver: None,
            compute_weights: None,
            point_pattern: None,
            vertex_pattern: None,
            primitive_pattern: None,
            detail_pattern: None,
            match_groups: false,
            pre_scale: None,
            normalize_weights: None,
            threshold: None,
            blend: None,
            unmatched: TransferUnmatched::KeepTarget,
            target_owner,
            attributes,
            source: source.clone(),
            target: target.clone(),
        }
    }

    fn interpolate(source: &geometry::T, target: &geometry::T, target_owner: attribute::Owner, attributes: Vec<InterpolateAttribute>) -> geometry::T {
        attribute_ops::interpolate(&base(source, target, target_owner, attributes)).unwrap()
    }

    fn point_weights() -> Option<InterpolateDriver> {
        Some(InterpolateDriver::PointWeights {
            numbers_attribute: "source_elements".to_owned(),
            weights_attribute: "source_weights".to_owned(),
        })
    }

    fn triangle_source() -> geometry::T {
        let g = polygons(&[(0.0, 0.0, 0.0), (2.0, 0.0, 0.0), (0.0, 2.0, 0.0)], &[vec![0, 1, 2]]);
        let g = add_values(Point, "weight", vec![0.0, 10.0, 20.0], &g);
        let g = add_ints(Point, "id", vec![10, 20, 30], &g);
        let g = add_storage(Point, "label", attribute::Storage::Text(std::sync::Arc::new(vec![b"a".to_vec(), b"b".to_vec(), b"c".to_vec()].into())), &g);
        let g = add_storage(
            Point,
            "uv",
            attribute::Storage::Float2(packed::Float2::of_owned(vec![0.0, 2.0, 0.0], vec![0.0, 0.0, 2.0]).unwrap()),
            &g,
        );
        let g = add_float3s(Point, "N", vec![2.0, 0.0, 0.0], vec![0.0, 2.0, 0.0], vec![0.0, 0.0, 2.0], &g);
        let g = add_storage(
            Point,
            "tuple4",
            attribute::Storage::Float4(
                packed::Float4::of_owned(vec![0.0, 1.0, 2.0], vec![3.0, 4.0, 5.0], vec![6.0, 7.0, 8.0], vec![9.0, 10.0, 11.0]).unwrap(),
            ),
            &g,
        );
        let g = add_values(Vertex, "vertex_weight", vec![100.0, 200.0, 300.0], &g);
        let g = add_values(Primitive, "primitive_weight", vec![42.0], &g);
        add_values(Detail, "detail_weight", vec![7.0], &g)
    }

    #[test]
    fn triangle_mixed_storage_and_position() {
        let source = triangle_source();
        let target = add_drivers(
            Point,
            vec![0, 0, 0, -1],
            &[(0.0, 0.0, 0.0), (0.25, 0.25, 0.0), (1.0, 0.0, 0.0), (0.0, 0.0, 0.0)],
            &blank_points(4),
        );
        let result = interpolate(
            &source,
            &target,
            Point,
            vec![
                spec(Point, "P"),
                spec(Point, "weight"),
                spec(Point, "id"),
                spec(Point, "label"),
                spec(Point, "uv"),
                spec(Point, "N"),
                spec(Point, "tuple4"),
                spec(Vertex, "vertex_weight"),
                spec(Primitive, "primitive_weight"),
                spec(Detail, "detail_weight"),
            ],
        );
        let (px, py, _) = positions(&result);
        assert_eq!(px, vec![0.0, 0.5, 2.0, 0.0]);
        assert_eq!(py, vec![0.0, 0.5, 0.0, 0.0]);
        assert_eq!(floats(Point, "weight", &result), vec![0.0, 7.5, 10.0, 0.0]);
        assert_eq!(ints(Point, "id", &result), vec![10, 10, 20, 0]);
        assert_eq!(texts(Point, "label", &result), vec![b"a".to_vec(), b"a".to_vec(), b"b".to_vec(), Vec::new()]);
        let uv = match attribute::storage(&find(Point, "uv", &result)).unwrap() {
            attribute::Storage::Float2(values) => packed::Float2::View::view(&values),
            _ => panic!("uv"),
        };
        assert_eq!(&*uv.x, &[0.0, 0.5, 2.0, 0.0]);
        assert_eq!(&*uv.y, &[0.0, 0.5, 0.0, 0.0]);
        let tuple4 = match attribute::storage(&find(Point, "tuple4", &result)).unwrap() {
            attribute::Storage::Float4(values) => packed::Float4::View::view(&values),
            _ => panic!("tuple4"),
        };
        assert_eq!(&*tuple4.x, &[0.0, 0.75, 1.0, 0.0]);
        assert_eq!(floats(Point, "vertex_weight", &result), vec![100.0, 175.0, 200.0, 0.0]);
        assert_eq!(floats(Point, "primitive_weight", &result), vec![42.0, 42.0, 42.0, 0.0]);
        assert_eq!(floats(Point, "detail_weight", &result), vec![7.0, 7.0, 7.0, 0.0]);
        let (nx, ny, nz) = float3(Point, "N", &result);
        assert!(near(nx[1], 2.0 / 6.0_f64.sqrt()) && near(ny[1], 1.0 / 6.0_f64.sqrt()) && near(nz[1], 1.0 / 6.0_f64.sqrt()));
    }

    #[test]
    fn quad_ngon_and_curve_parameter_spaces() {
        let quad = polygons(&[(0.0, 0.0, 0.0), (0.0, 1.0, 0.0), (1.0, 1.0, 0.0), (1.0, 0.0, 0.0)], &[vec![0, 1, 2, 3]]);
        let quad = add_values(Point, "value", vec![0.0, 10.0, 20.0, 30.0], &quad);
        let target = add_drivers(
            Point,
            vec![0; 5],
            &[(0.0, 0.0, 0.0), (0.0, 1.0, 0.0), (1.0, 1.0, 0.0), (1.0, 0.0, 0.0), (0.5, 0.5, 0.0)],
            &blank_points(5),
        );
        let result = interpolate(&quad, &target, Point, vec![spec(Point, "value")]);
        assert_eq!(floats(Point, "value", &result), vec![0.0, 10.0, 20.0, 30.0, 15.0]);

        let ngon = polygons(
            &[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.5, 1.0, 0.0), (0.5, 2.0, 0.0), (-0.5, 1.0, 0.0)],
            &[vec![0, 1, 2, 3, 4]],
        );
        let ngon = add_values(Point, "value", vec![0.0, 10.0, 20.0, 30.0, 40.0], &ngon);
        let target = add_drivers(Point, vec![0; 3], &[(0.1, 0.0, 0.0), (0.7, 1.0, 0.0), (0.3, 0.5, 0.0)], &blank_points(3));
        let result = interpolate(&ngon, &target, Point, vec![spec(Point, "value")]);
        assert_eq!(floats(Point, "value", &result), vec![5.0, 20.0, 17.5]);

        let curves = mesh(
            &[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (2.0, 0.0, 0.0)],
            &[(PrimitiveKind::OpenPolyline, vec![0, 1, 2]), (PrimitiveKind::ClosedPolyline, vec![0, 1, 2])],
        );
        let curves = add_values(Point, "value", vec![0.0, 10.0, 20.0], &curves);
        let target = add_drivers(
            Point,
            vec![0, 0, 0, 0, 1, 1, 1],
            &[
                (0.0, 0.0, 0.0),
                (0.25, 0.0, 0.0),
                (0.5, 0.0, 0.0),
                (1.0, 0.0, 0.0),
                (1.0 / 6.0, 0.0, 0.0),
                (0.5, 0.0, 0.0),
                (5.0 / 6.0, 0.0, 0.0),
            ],
            &blank_points(7),
        );
        let result = interpolate(&curves, &target, Point, vec![spec(Point, "value")]);
        let values = floats(Point, "value", &result);
        assert!(near_all(&[0.0, 5.0, 10.0, 20.0, 5.0, 15.0, 10.0], &values), "{values:?}");
    }

    fn target_topology() -> geometry::T {
        polygons(&[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0)], &[vec![0, 1, 2], vec![0, 2, 3]])
    }

    #[test]
    fn destination_owners_group_blend_and_misses() {
        let source = triangle_source();
        let vertex_target = add_drivers(Vertex, vec![0; 6], &vec![(0.25, 0.25, 0.0); 6], &target_topology());
        let vertex = interpolate(&source, &vertex_target, Vertex, vec![spec(Point, "weight")]);
        assert_eq!(floats(Vertex, "weight", &vertex), vec![7.5; 6]);
        let primitive_target = add_drivers(Primitive, vec![0, 0], &[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0)], &target_topology());
        let primitive = interpolate(&source, &primitive_target, Primitive, vec![spec(Point, "weight")]);
        assert_eq!(floats(Primitive, "weight", &primitive), vec![0.0, 10.0]);
        let detail_target = add_drivers(Detail, vec![0], &[(0.25, 0.25, 0.0)], &blank_points(1));
        let detail = interpolate(&source, &detail_target, Detail, vec![spec(Point, "weight")]);
        assert_eq!(floats(Detail, "weight", &detail), vec![7.5]);

        let target = add_drivers(
            Point,
            vec![0, 0, -1, 0],
            &[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (0.0, 0.0, 0.0), (0.0, 1.0, 0.0)],
            &blank_points(4),
        );
        let target = add_values(Point, "weight", vec![100.0; 4], &target);
        let selection = group::ordered(group::Owner::Point, "selected", 4, &[1, 2, 3]).unwrap();
        let mut kept = base(&source, &target, Point, vec![spec(Point, "weight")]);
        kept.selection = Some(selection.clone());
        kept.blend = Some(0.5);
        assert_eq!(floats(Point, "weight", &attribute_ops::interpolate(&kept).unwrap()), vec![100.0, 55.0, 100.0, 60.0]);
        let mut defaulted = base(&source, &target, Point, vec![spec(Point, "weight")]);
        defaulted.selection = Some(selection);
        defaulted.unmatched = TransferUnmatched::DefaultValue;
        assert_eq!(floats(Point, "weight", &attribute_ops::interpolate(&defaulted).unwrap()), vec![100.0, 10.0, 0.0, 20.0]);
        let mut prescaled = base(
            &source,
            &add_drivers(Point, vec![0], &[(1.0, 0.0, 0.0)], &blank_points(1)),
            Point,
            vec![spec(Point, "weight")],
        );
        prescaled.pre_scale = Some(0.5);
        assert_eq!(floats(Point, "weight", &attribute_ops::interpolate(&prescaled).unwrap()), vec![5.0]);
    }

    #[test]
    fn explicit_weight_modes() {
        let source = triangle_source();
        let point_target = add_weight_drivers(
            Point,
            &[vec![0, 1], vec![1, 2], vec![]],
            &[vec![0.25, 0.75], vec![0.5, 0.5], vec![]],
            &blank_points(3),
        );
        let mut p = base(&source, &point_target, Point, vec![spec(Point, "weight"), spec(Point, "id"), spec(Detail, "detail_weight")]);
        p.driver = point_weights();
        let point = attribute_ops::interpolate(&p).unwrap();
        assert_eq!(floats(Point, "weight", &point), vec![7.5, 15.0, 0.0]);
        assert_eq!(ints(Point, "id", &point), vec![20, 20, 0]);
        assert_eq!(floats(Point, "detail_weight", &point), vec![7.0, 7.0, 0.0]);

        let normalized_target = add_values(
            Point,
            "detail_weight",
            vec![100.0],
            &add_values(Point, "weight", vec![100.0], &add_weight_drivers(Point, &[vec![0, 1]], &[vec![0.1, 0.1]], &blank_points(1))),
        );
        let mut n = base(&source, &normalized_target, Point, vec![spec(Point, "weight"), spec(Detail, "detail_weight")]);
        n.driver = point_weights();
        n.pre_scale = Some(2.0);
        n.normalize_weights = Some(true);
        n.threshold = Some(0.8);
        let normalized = attribute_ops::interpolate(&n).unwrap();
        assert!(near(floats(Point, "weight", &normalized)[0], 52.5));
        assert!(near(floats(Point, "detail_weight", &normalized)[0], 53.5));

        let vertex_target = add_weight_drivers(Point, &[vec![0, 2]], &[vec![0.25, 0.75]], &blank_points(1));
        let mut v = base(
            &source,
            &vertex_target,
            Point,
            vec![spec(Point, "weight"), spec(Vertex, "vertex_weight"), spec(Primitive, "primitive_weight"), spec(Detail, "detail_weight")],
        );
        v.driver = Some(InterpolateDriver::VertexWeights {
            numbers_attribute: "source_elements".to_owned(),
            weights_attribute: "source_weights".to_owned(),
        });
        let vertex = attribute_ops::interpolate(&v).unwrap();
        assert_eq!(floats(Point, "weight", &vertex), vec![15.0]);
        assert_eq!(floats(Point, "vertex_weight", &vertex), vec![250.0]);
        assert_eq!(floats(Point, "primitive_weight", &vertex), vec![42.0]);
        assert_eq!(floats(Point, "detail_weight", &vertex), vec![7.0]);

        let primitive_source = polygons(
            &[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (2.0, 0.0, 0.0), (3.0, 0.0, 0.0), (2.0, 1.0, 0.0)],
            &[vec![0, 1, 2], vec![3, 4, 5]],
        );
        let primitive_source = add_values(Detail, "detail", vec![2.0], &add_values(Primitive, "piece", vec![10.0, 30.0], &primitive_source));
        let primitive_target = add_weight_drivers(Point, &[vec![0, 1]], &[vec![0.25, 0.75]], &blank_points(1));
        let mut q = base(&primitive_source, &primitive_target, Point, vec![spec(Primitive, "piece"), spec(Detail, "detail")]);
        q.driver = Some(InterpolateDriver::PrimitiveWeights {
            numbers_attribute: "source_elements".to_owned(),
            weights_attribute: "source_weights".to_owned(),
        });
        let primitive = attribute_ops::interpolate(&q).unwrap();
        assert_eq!(floats(Point, "piece", &primitive), vec![25.0]);
        assert_eq!(floats(Point, "detail", &primitive), vec![2.0]);
    }

    #[test]
    fn signed_normalization_and_group_threshold() {
        let source = add_group_ordered(group::Owner::Point, "hot", &[1], &triangle_source());
        let target = add_weight_drivers(
            Point,
            &[vec![0, 1], vec![0, 1], vec![0, 1]],
            &[vec![-1.0, -1.0], vec![2.0, -1.0], vec![1.0, -1.0]],
            &blank_points(3),
        );
        let target = add_ints(Point, "id", vec![99; 3], &add_values(Point, "weight", vec![100.0; 3], &target));
        let target = add_group_ordered(group::Owner::Point, "hot", &[2], &target);
        let mut p = base(&source, &target, Point, vec![spec(Point, "weight"), spec(Point, "id")]);
        p.driver = point_weights();
        p.normalize_weights = Some(true);
        p.threshold = Some(0.5);
        p.match_groups = true;
        p.point_pattern = Some("hot".to_owned());
        let result = attribute_ops::interpolate(&p).unwrap();
        assert_eq!(floats(Point, "weight", &result), vec![5.0, -10.0, 100.0]);
        assert_eq!(ints(Point, "id", &result), vec![10, 10, 99]);
        assert_eq!(group_values(group::Owner::Point, "hot", &result), vec![true, false, true]);
    }

    #[test]
    fn array_field_row_selection() {
        let source = add_storage(
            Point,
            "samples",
            attribute::Storage::FloatArray(float_rows(&[vec![0.1], vec![0.2, 0.3], vec![0.4]])),
            &add_storage(Point, "neighbors", attribute::Storage::IntArray(int_rows(&[vec![1, 2], vec![3], vec![]])), &triangle_source()),
        );
        let attributes = vec![spec(Point, "neighbors"), spec(Point, "samples")];
        let rows = |g: &geometry::T, name: &str| -> Vec<Vec<i64>> {
            match attribute::storage(&find(Point, name, g)).unwrap() {
                attribute::Storage::IntArray(values) => (0..packed::IntArray::length(&values))
                    .map(|row| packed::IntArray::get(&values, row).unwrap())
                    .collect(),
                _ => panic!("int array"),
            }
        };
        let frows = |g: &geometry::T, name: &str| -> Vec<Vec<f64>> {
            match attribute::storage(&find(Point, name, g)).unwrap() {
                attribute::Storage::FloatArray(values) => (0..packed::FloatArray::length(&values))
                    .map(|row| packed::FloatArray::get(&values, row).unwrap())
                    .collect(),
                _ => panic!("float array"),
            }
        };
        let primitive_target = add_drivers(Point, vec![0; 3], &[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (0.0, 1.0, 0.0)], &blank_points(3));
        let primitive = interpolate(&source, &primitive_target, Point, attributes.clone());
        assert_eq!(rows(&primitive, "neighbors"), vec![vec![1, 2], vec![3], vec![]]);
        assert_eq!(frows(&primitive, "samples"), vec![vec![0.1], vec![0.2, 0.3], vec![0.4]]);

        let weighted_target = add_weight_drivers(
            Point,
            &[vec![0, 1], vec![0, 1], vec![0, 2]],
            &[vec![0.25, 0.75], vec![0.5, 0.5], vec![1.0, -1.0]],
            &blank_points(3),
        );
        let weighted_target = add_storage(
            Point,
            "samples",
            attribute::Storage::FloatArray(float_rows(&[vec![9.0], vec![8.0], vec![7.0]])),
            &add_storage(Point, "neighbors", attribute::Storage::IntArray(int_rows(&[vec![9], vec![8], vec![7]])), &weighted_target),
        );
        let mut w = base(&source, &weighted_target, Point, attributes.clone());
        w.driver = point_weights();
        w.normalize_weights = Some(true);
        w.threshold = Some(0.1);
        let weighted = attribute_ops::interpolate(&w).unwrap();
        assert_eq!(rows(&weighted, "neighbors"), vec![vec![3], vec![1, 2], vec![7]]);
        assert_eq!(frows(&weighted, "samples"), vec![vec![0.2, 0.3], vec![0.1], vec![7.0]]);

        let blend_target = add_storage(
            Point,
            "neighbors",
            attribute::Storage::IntArray(int_rows(&[vec![9], vec![8]])),
            &add_weight_drivers(Point, &[vec![1], vec![1]], &[vec![1.0], vec![1.0]], &blank_points(2)),
        );
        let run_blend = |blend: f64| {
            let mut p = base(&source, &blend_target, Point, vec![spec(Point, "neighbors")]);
            p.driver = point_weights();
            p.blend = Some(blend);
            attribute_ops::interpolate(&p).unwrap()
        };
        assert_eq!(rows(&run_blend(0.49), "neighbors")[0], vec![9]);
        assert_eq!(rows(&run_blend(0.5), "neighbors")[0], vec![3]);

        let missed = add_storage(
            Point,
            "neighbors",
            attribute::Storage::IntArray(int_rows(&[vec![9]])),
            &add_drivers(Point, vec![-1], &[(0.0, 0.0, 0.0)], &blank_points(1)),
        );
        let mut d = base(&source, &missed, Point, vec![spec(Point, "neighbors")]);
        d.unmatched = TransferUnmatched::DefaultValue;
        assert_eq!(rows(&attribute_ops::interpolate(&d).unwrap(), "neighbors")[0], Vec::<i64>::new());
    }

    #[test]
    fn computed_weights_are_equivalent_to_primitive_uvw() {
        let source = triangle_source();
        let target = add_drivers(Point, vec![0, 0, -1], &[(0.0, 0.0, 0.0), (0.25, 0.25, 0.0), (0.0, 0.0, 0.0)], &blank_points(3));
        let compute = InterpolateComputed {
            computed_owner: Point,
            computed_numbers_attribute: "computed_points".to_owned(),
            computed_weights_attribute: "computed_weights".to_owned(),
        };
        let numbers = |g: &geometry::T| -> Vec<Vec<i64>> {
            match attribute::storage(&find(Point, "computed_points", g)).unwrap() {
                attribute::Storage::IntArray(values) => (0..packed::IntArray::length(&values))
                    .map(|row| packed::IntArray::get(&values, row).unwrap())
                    .collect(),
                _ => panic!("computed_points"),
            }
        };
        let mut only = base(&source, &target, Point, vec![]);
        only.compute_weights = Some(compute.clone());
        assert_eq!(numbers(&attribute_ops::interpolate(&only).unwrap()), vec![vec![0, 1, 2], vec![0, 1, 2], vec![]]);
        let mut with = base(&source, &target, Point, vec![spec(Point, "weight"), spec(Point, "id")]);
        with.compute_weights = Some(compute);
        let primitive = attribute_ops::interpolate(&with).unwrap();
        assert_eq!(numbers(&primitive), vec![vec![0, 1, 2], vec![0, 1, 2], vec![]]);
        let mut again = base(&source, &primitive, Point, vec![spec(Point, "weight"), spec(Point, "id")]);
        again.driver = Some(InterpolateDriver::PointWeights {
            numbers_attribute: "computed_points".to_owned(),
            weights_attribute: "computed_weights".to_owned(),
        });
        let weighted = attribute_ops::interpolate(&again).unwrap();
        assert_eq!(floats(Point, "weight", &weighted), floats(Point, "weight", &primitive));
        assert_eq!(ints(Point, "id", &weighted), ints(Point, "id", &primitive));
    }

    #[test]
    fn attribute_pattern_expansion() {
        let source = triangle_source();
        let target = add_drivers(Point, vec![0], &[(0.25, 0.25, 0.0)], &blank_points(1));
        let mut p = base(&source, &target, Point, vec![]);
        p.point_pattern = Some("P weight ^id".to_owned());
        p.vertex_pattern = Some("vertex_*".to_owned());
        p.primitive_pattern = Some("primitive_*".to_owned());
        p.detail_pattern = Some("detail_*".to_owned());
        let result = attribute_ops::interpolate(&p).unwrap();
        let (px, py, _) = positions(&result);
        assert_eq!((px, py), (vec![0.5], vec![0.5]));
        assert_eq!(floats(Point, "weight", &result), vec![7.5]);
        assert_eq!(floats(Point, "vertex_weight", &result), vec![175.0]);
        assert_eq!(floats(Point, "primitive_weight", &result), vec![42.0]);
        assert_eq!(floats(Point, "detail_weight", &result), vec![7.0]);
        assert!(!has(Point, "id", &result));
        let mut bad = base(&source, &target, Point, vec![]);
        bad.point_pattern = Some("[bad".to_owned());
        assert_eq!(err_code(attribute_ops::interpolate(&bad)), "invalid_interpolate");
        let mut duplicate = base(&source, &target, Point, vec![spec(Point, "weight")]);
        duplicate.point_pattern = Some("weight".to_owned());
        assert_eq!(err_code(attribute_ops::interpolate(&duplicate)), "invalid_interpolate");
    }

    #[test]
    fn source_group_matching() {
        let source = add_group_ordered(
            group::Owner::Primitive,
            "face_hot",
            &[0],
            &add_group_ordered(
                group::Owner::Vertex,
                "corner_hot",
                &[2],
                &add_group_ordered(group::Owner::Point, "hot", &[1], &triangle_source()),
            ),
        );
        let target = add_drivers(Point, vec![0; 3], &[(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (0.0, 1.0, 0.0)], &blank_points(3));
        let target = add_group_ordered(group::Owner::Point, "corner_hot", &[2], &add_group_ordered(group::Owner::Point, "hot", &[0, 2], &target));
        let mut p = base(&source, &target, Point, vec![]);
        p.selection = Some(group::ordered(group::Owner::Point, "selected", 3, &[0, 1]).unwrap());
        p.match_groups = true;
        p.point_pattern = Some("hot".to_owned());
        p.vertex_pattern = Some("corner_*".to_owned());
        p.primitive_pattern = Some("face_*".to_owned());
        let result = attribute_ops::interpolate(&p).unwrap();
        assert_eq!(group_values(group::Owner::Point, "hot", &result), vec![false, true, true]);
        assert_eq!(group_values(group::Owner::Point, "corner_hot", &result), vec![false, false, true]);
        assert_eq!(group_values(group::Owner::Point, "face_hot", &result), vec![true, true, false]);

        let blend_target = add_group_ordered(
            group::Owner::Point,
            "hot",
            &[1],
            &add_drivers(Point, vec![0, 0], &[(1.0, 0.0, 0.0), (0.0, 0.0, 0.0)], &blank_points(2)),
        );
        let mut b = base(&source, &blend_target, Point, vec![]);
        b.blend = Some(0.5);
        b.match_groups = true;
        b.point_pattern = Some("hot".to_owned());
        assert_eq!(group_values(group::Owner::Point, "hot", &attribute_ops::interpolate(&b).unwrap()), vec![true, true]);

        let weighted_target = add_weight_drivers(Point, &[vec![0, 2], vec![0, 1]], &[vec![0.25, 0.75], vec![0.8, 0.2]], &blank_points(2));
        let mut w = base(&source, &weighted_target, Point, vec![]);
        w.driver = Some(InterpolateDriver::VertexWeights {
            numbers_attribute: "source_elements".to_owned(),
            weights_attribute: "source_weights".to_owned(),
        });
        w.match_groups = true;
        w.point_pattern = Some("hot".to_owned());
        w.vertex_pattern = Some("corner_*".to_owned());
        w.primitive_pattern = Some("face_*".to_owned());
        let weighted = attribute_ops::interpolate(&w).unwrap();
        assert_eq!(group_values(group::Owner::Point, "hot", &weighted), vec![false, false]);
        assert_eq!(group_values(group::Owner::Point, "corner_hot", &weighted), vec![true, false]);
        assert_eq!(group_values(group::Owner::Point, "face_hot", &weighted), vec![true, true]);

        let normalized_target = add_weight_drivers(Point, &[vec![1]], &[vec![0.1]], &blank_points(1));
        let mut n = base(&source, &normalized_target, Point, vec![]);
        n.driver = point_weights();
        n.normalize_weights = Some(true);
        n.threshold = Some(0.4);
        n.match_groups = true;
        n.point_pattern = Some("hot".to_owned());
        assert_eq!(group_values(group::Owner::Point, "hot", &attribute_ops::interpolate(&n).unwrap()), vec![false]);

        let duplicate_source = add_group_ordered(group::Owner::Primitive, "hot", &[0], &source);
        let mut dup = base(&duplicate_source, &target, Point, vec![]);
        dup.match_groups = true;
        dup.point_pattern = Some("hot".to_owned());
        dup.primitive_pattern = Some("hot".to_owned());
        assert_eq!(err_code(attribute_ops::interpolate(&dup)), "invalid_interpolate");
        let mut mismatch = base(&source, &weighted_target, Point, vec![]);
        mismatch.driver = point_weights();
        mismatch.match_groups = true;
        mismatch.primitive_pattern = Some("face_*".to_owned());
        assert_eq!(err_code(attribute_ops::interpolate(&mismatch)), "invalid_interpolate");
    }

    #[test]
    fn errors_and_cancellation() {
        let source = triangle_source();
        let target = blank_points(1);
        let weight_spec = || vec![spec(Point, "weight")];
        assert_eq!(err_code(attribute_ops::interpolate(&base(&source, &target, Point, weight_spec()))), "invalid_interpolate");
        let wrong_driver = add_float3s(
            Point,
            "source_uvw",
            vec![0.0],
            vec![0.0],
            vec![0.0],
            &add_values(Point, "source_primitive", vec![0.0], &target),
        );
        assert_eq!(err_code(attribute_ops::interpolate(&base(&source, &wrong_driver, Point, weight_spec()))), "invalid_interpolate");
        let incompatible = add_storage(
            Point,
            "weight",
            attribute::Storage::Text(std::sync::Arc::new(vec![b"bad".to_vec()].into())),
            &add_drivers(Point, vec![0], &[(0.0, 0.0, 0.0)], &blank_points(1)),
        );
        assert_eq!(err_code(attribute_ops::interpolate(&base(&source, &incompatible, Point, weight_spec()))), "invalid_interpolate");
        let nan = add_drivers(Point, vec![0], &[(f64::NAN, 0.0, 0.0)], &blank_points(1));
        assert_eq!(err_code(attribute_ops::interpolate(&base(&source, &nan, Point, weight_spec()))), "invalid_parameter");
        let driven = add_drivers(Point, vec![0], &[(0.0, 0.0, 0.0)], &blank_points(1));
        let duplicate = vec![spec(Point, "weight"), spec_into(Point, "id", "weight")];
        assert_eq!(err_code(attribute_ops::interpolate(&base(&source, &driven, Point, duplicate))), "invalid_interpolate");
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        let mut cancelled = base(&source, &driven, Point, weight_spec());
        cancelled.cancel = Some(token.clone());
        assert_eq!(err_code(attribute_ops::interpolate(&cancelled)), "cancelled");

        let weighted = |target: &geometry::T, attributes: Vec<InterpolateAttribute>| {
            let mut p = base(&source, target, Point, attributes);
            p.driver = point_weights();
            attribute_ops::interpolate(&p)
        };
        let bad_layout = add_storage(
            Point,
            "source_weights",
            attribute::Storage::FloatArray(float_rows(&[vec![1.0, 0.0], vec![]])),
            &add_storage(Point, "source_elements", attribute::Storage::IntArray(int_rows(&[vec![0], vec![1]])), &blank_points(2)),
        );
        assert_eq!(err_code(weighted(&bad_layout, weight_spec())), "invalid_interpolate");
        let bad_number = add_weight_drivers(Point, &[vec![99]], &[vec![1.0]], &blank_points(1));
        assert_eq!(err_code(weighted(&bad_number, weight_spec())), "invalid_interpolate");
        let bad_weight = add_weight_drivers(Point, &[vec![0]], &[vec![f64::NAN]], &blank_points(1));
        assert_eq!(err_code(weighted(&bad_weight, weight_spec())), "invalid_interpolate");
        let point_weighted = add_weight_drivers(Point, &[vec![0]], &[vec![1.0]], &blank_points(1));
        assert_eq!(err_code(weighted(&point_weighted, vec![spec(Vertex, "vertex_weight")])), "invalid_interpolate");
        let mut computed = base(&source, &point_weighted, Point, weight_spec());
        computed.driver = point_weights();
        computed.compute_weights = Some(InterpolateComputed {
            computed_owner: Point,
            computed_numbers_attribute: "computed_numbers".to_owned(),
            computed_weights_attribute: "computed_weights".to_owned(),
        });
        assert_eq!(err_code(attribute_ops::interpolate(&computed)), "invalid_interpolate");
        let mut cancelled_weighted = base(&source, &point_weighted, Point, weight_spec());
        cancelled_weighted.driver = point_weights();
        cancelled_weighted.cancel = Some(token);
        assert_eq!(err_code(attribute_ops::interpolate(&cancelled_weighted)), "cancelled");
    }

    #[test]
    fn parallel_exactness_at_scale() {
        let count = 200_003_i64;
        let source = polygons(&[(0.0, 0.0, 0.0), (0.0, 1.0, 0.0), (1.0, 1.0, 0.0), (1.0, 0.0, 0.0)], &[vec![0, 1, 2, 3]]);
        let source = add_storage(Point, "weight", attribute::Storage::Float(Arc::new(vec![0.0, 1.0, 2.0, 3.0])), &source);
        let source = add_ints(Point, "id", vec![3, 5, 7, 11], &source);
        let source = add_storage(Point, "label", attribute::Storage::Text(std::sync::Arc::new(vec![b"a".to_vec(), b"b".to_vec(), b"c".to_vec(), b"d".to_vec()].into())), &source);
        let source = add_storage(
            Point,
            "tuple4",
            attribute::Storage::Float4(
                packed::Float4::of_owned(
                    vec![0.0, 1.0, 2.0, 3.0],
                    vec![4.0, 5.0, 6.0, 7.0],
                    vec![8.0, 9.0, 10.0, 11.0],
                    vec![12.0, 13.0, 14.0, 15.0],
                )
                .unwrap(),
            ),
            &source,
        );
        let source = add_storage(
            Point,
            "neighbors",
            attribute::Storage::IntArray(int_rows(&[vec![0], vec![1, 2], vec![], vec![3, 4, 5]])),
            &source,
        );
        let source = add_storage(
            Point,
            "samples",
            attribute::Storage::FloatArray(float_rows(&[vec![0.25], vec![1.0, 2.0], vec![], vec![3.0, 4.0, 5.0]])),
            &source,
        );
        let uvw: Vec<(f64, f64, f64)> =
            (0..count).map(|i| ((i & 255) as f64 / 255.0, ((i * 17) & 255) as f64 / 255.0, 0.0)).collect();
        let target = add_drivers(Point, vec![0; count as usize], &uvw, &blank_points(count));
        let attributes = vec![
            spec(Point, "P"),
            spec(Point, "weight"),
            spec(Point, "id"),
            spec(Point, "label"),
            spec(Point, "tuple4"),
            spec(Point, "neighbors"),
            spec(Point, "samples"),
        ];
        let compute = InterpolateComputed {
            computed_owner: Point,
            computed_numbers_attribute: "computed_points".to_owned(),
            computed_weights_attribute: "computed_weights".to_owned(),
        };
        let run = |domains: i64| {
            on_domains(domains, &|| {
                let mut p = base(&source, &target, Point, attributes.clone());
                p.grain = Some(257);
                p.compute_weights = Some(compute.clone());
                attribute_ops::interpolate(&p).unwrap()
            })
        };
        let (one, four) = (run(1), run(4));
        assert_eq!(signature(&one), signature(&four));
        let weighted_run = |domains: i64| {
            on_domains(domains, &|| {
                let mut p = base(&source, &one, Point, attributes.clone());
                p.grain = Some(257);
                p.driver = Some(InterpolateDriver::PointWeights {
                    numbers_attribute: "computed_points".to_owned(),
                    weights_attribute: "computed_weights".to_owned(),
                });
                attribute_ops::interpolate(&p).unwrap()
            })
        };
        let (weighted_one, weighted_four) = (weighted_run(1), weighted_run(4));
        assert_eq!(signature(&weighted_one), signature(&weighted_four));
    }
}
