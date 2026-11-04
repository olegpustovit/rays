use crate::side_lru::{self, Item, TableKey};
use runtime_resources::{Canvas, Image, Text};
use scene_command::render_ir::{Command, RenderIr};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// A resolved resource id. `Text` is shared by `Rc`: a stamp compares its `Text::identity`.
#[derive(Clone)]
pub enum resource {
    Image(Image::t),
    Text(Rc<Text::t>),
    Canvas(Canvas::t),
}

/// The id a command names, the resource's identity (the OCaml physical equality) and its generation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum resource_stamp {
    Image_stamp(i64, i64, i64),
    Text_stamp(i64, i64, i64),
    Canvas_stamp(i64, i64, i64),
}

pub struct plan {
    pub plan_ir_id: i64,
    pub plan_command_count: i64,
    pub plan_source_bytes: i64,
    pub plan_density: i64,
    pub plan_extent: (i64, i64, i64, i64),
    pub plan_resources: Vec<resource_stamp>,
    pub plan_ir: Rc<RenderIr>,
    pub plan_draws: Vec<crate::scene_execution::draw>,
}

pub type plan_ref = Rc<RefCell<plan>>;

pub struct segment {
    pub segment_version: i64,
    pub segment_density: i64,
    pub segment_width: i64,
    pub segment_height: i64,
    pub segment_draws: Vec<crate::scene_execution::draw>,
}

pub struct t {
    by_fingerprint: side_lru::Table,
    by_ir: Rc<RefCell<HashMap<i64, plan_ref>>>,
    segments: side_lru::Table,
}

fn plan_of(item: &Item) -> plan_ref {
    item.clone().downcast::<RefCell<plan>>().unwrap_or_else(|_| panic!("plan table item"))
}

pub fn create() -> t {
    let by_ir: Rc<RefCell<HashMap<i64, plan_ref>>> = Rc::new(RefCell::new(HashMap::new()));
    let released = by_ir.clone();
    let by_fingerprint = side_lru::create(
        lru::cache("renderer.plans"),
        true,
        64 * 1024 * 1024,
        Box::new(move |_, item| {
            let released_plan = plan_of(&item);
            let id = released_plan.borrow().plan_ir_id;
            let current = released.borrow().get(&id).cloned();
            if let Some(current) = current {
                if Rc::ptr_eq(&current, &released_plan) {
                    released.borrow_mut().remove(&id);
                }
            }
        }),
        16,
    );
    t {
        by_fingerprint,
        by_ir,
        segments: side_lru::create(lru::cache("renderer.segments"), true, 64 * 1024 * 1024, Box::new(|_, _| {}), 256),
    }
}

pub fn find(value: &mut t, fingerprint: i64) -> Option<plan_ref> {
    side_lru::find(&mut value.by_fingerprint, &TableKey::Int(fingerprint)).map(|item| plan_of(&item))
}

pub fn find_ir(value: &t, ir_id: i64) -> Option<plan_ref> {
    value.by_ir.borrow().get(&ir_id).cloned()
}

pub fn add(value: &mut t, fingerprint: i64, plan_: plan_ref) {
    let (bytes, ir_id) = {
        let p = plan_.borrow();
        (p.plan_source_bytes, p.plan_ir_id)
    };
    let item: Item = plan_.clone();
    side_lru::add(&mut value.by_fingerprint, bytes, TableKey::Int(fingerprint), item);
    value.by_ir.borrow_mut().insert(ir_id, plan_);
}

pub fn rebind(value: &t, plan_: &plan_ref, ir_id: i64, ir: &Rc<RenderIr>) {
    let old_id = plan_.borrow().plan_ir_id;
    if old_id != ir_id {
        let current = value.by_ir.borrow().get(&old_id).cloned();
        if let Some(current) = current {
            if Rc::ptr_eq(&current, plan_) {
                value.by_ir.borrow_mut().remove(&old_id);
            }
        }
        let mut p = plan_.borrow_mut();
        p.plan_ir_id = ir_id;
        p.plan_ir = ir.clone();
        drop(p);
        value.by_ir.borrow_mut().insert(ir_id, plan_.clone());
    }
}

pub fn find_segment(value: &mut t, identity: i64) -> Option<Rc<segment>> {
    side_lru::find(&mut value.segments, &TableKey::Int(identity)).map(|item| item.downcast::<segment>().unwrap_or_else(|_| panic!("segment table item")))
}

pub fn add_segment(value: &mut t, identity: i64, bytes: i64, segment_: segment) {
    let item: Item = Rc::new(segment_);
    side_lru::add(&mut value.segments, bytes, TableKey::Int(identity), item);
}

pub fn segment_count(value: &mut t) -> i64 {
    side_lru::length(&mut value.segments)
}

pub fn clear(value: &mut t) {
    side_lru::clear(&mut value.by_fingerprint);
    value.by_ir.borrow_mut().clear();
    side_lru::clear(&mut value.segments);
}

pub fn plan_source_bytes(commands: &[Command]) -> i64 {
    commands
        .iter()
        .map(|command| match command {
            Command::Geometry(geometry) => (geometry.vertices.len() * 8 + geometry.indices.len() * 8) as i64,
            Command::Glyphs(glyphs) => glyphs.glyphs.len() as i64 * 24,
            Command::Shapes(shapes) => shapes.instances().len() as i64,
            _ => 32,
        })
        .sum()
}

pub fn same_resources(left: &[resource_stamp], right: &[resource_stamp]) -> bool {
    left == right
}

/// Whether every resource the commands name is live, and the stamps of those that are.
pub fn resource_stamps(resolve: &dyn Fn(i64) -> Option<resource>, commands: &[Command]) -> (bool, Vec<resource_stamp>) {
    let mut ids: Vec<i64> = Vec::new();
    let mut missing = false;
    for command in commands {
        match command {
            Command::Image(image) => ids.push(image.resource_id),
            Command::Glyphs(glyphs) => ids.push(glyphs.resource_id),
            _ => {}
        }
    }
    ids.sort();
    ids.dedup();
    let mut stamps = Vec::new();
    for id in ids {
        match resolve(id) {
            None => missing = true,
            Some(resource::Image(image)) => {
                if Image::gpu_snapshot(&image).is_err() {
                    missing = true;
                } else {
                    stamps.push(resource_stamp::Image_stamp(id, Image::identity(&image), Image::generation(&image)));
                }
            }
            Some(resource::Text(text)) => {
                if Text::destroyed(&text) {
                    missing = true;
                } else {
                    stamps.push(resource_stamp::Text_stamp(id, Text::identity(&text), Text::generation(&text)));
                }
            }
            Some(resource::Canvas(canvas)) => {
                if Canvas::destroyed(&canvas) {
                    missing = true;
                } else {
                    stamps.push(resource_stamp::Canvas_stamp(id, Canvas::identity(&canvas), Canvas::generation(&canvas)));
                }
            }
        }
    }
    (!missing, stamps)
}
