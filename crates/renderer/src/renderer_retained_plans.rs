use crate::side_lru::{self, Item, TableKey};
use runtime_resources::{canvas, image, text};
use scene_command::render_ir::{Command, RenderIr};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// A resolved resource id. `Text` is shared by `Rc`: a stamp compares its `Text::identity`.
#[derive(Clone)]
pub enum Resource {
    Image(image::Image),
    Text(Rc<text::Text>),
    Canvas(canvas::Canvas),
}

/// The id a command names, the resource's identity (the OCaml physical equality) and its generation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ResourceStamp {
    ImageStamp(i64, i64, i64),
    TextStamp(i64, i64, i64),
    CanvasStamp(i64, i64, i64),
}

pub struct Plan {
    pub ir_id: i64,
    pub command_count: i64,
    pub source_bytes: i64,
    pub density: i64,
    pub extent: (i64, i64, i64, i64),
    pub resources: Vec<ResourceStamp>,
    pub ir: Rc<RenderIr>,
    pub draws: Vec<crate::scene_execution::Draw>,
}

pub type PlanRef = Rc<RefCell<Plan>>;

pub struct Segment {
    pub version: i64,
    pub density: i64,
    pub width: i64,
    pub height: i64,
    pub draws: Vec<crate::scene_execution::Draw>,
}

pub struct RetainedPlans {
    by_fingerprint: side_lru::Table,
    by_ir: Rc<RefCell<HashMap<i64, PlanRef>>>,
    segments: side_lru::Table,
}

fn plan_of(item: &Item) -> PlanRef {
    item.clone().downcast::<RefCell<Plan>>().unwrap_or_else(|_| unreachable!("plan table item"))
}

pub fn create() -> RetainedPlans {
    let by_ir: Rc<RefCell<HashMap<i64, PlanRef>>> = Rc::new(RefCell::new(HashMap::new()));
    let released = by_ir.clone();
    let by_fingerprint = side_lru::create(
        lru::cache("renderer.plans"),
        true,
        64 * 1024 * 1024,
        Box::new(move |_, item| {
            let released_plan = plan_of(&item);
            let id = released_plan.borrow().ir_id;
            let current = released.borrow().get(&id).cloned();
            if let Some(current) = current
                && Rc::ptr_eq(&current, &released_plan) {
                    released.borrow_mut().remove(&id);
                }
        }),
        16,
    );
    RetainedPlans {
        by_fingerprint,
        by_ir,
        segments: side_lru::create(lru::cache("renderer.segments"), true, 64 * 1024 * 1024, Box::new(|_, _| {}), 256),
    }
}

pub fn find(value: &mut RetainedPlans, fingerprint: i64) -> Option<PlanRef> {
    side_lru::find(&mut value.by_fingerprint, &TableKey::Int(fingerprint)).map(|item| plan_of(&item))
}

pub fn find_ir(value: &RetainedPlans, ir_id: i64) -> Option<PlanRef> {
    value.by_ir.borrow().get(&ir_id).cloned()
}

pub fn add(value: &mut RetainedPlans, fingerprint: i64, plan_: PlanRef) {
    let (bytes, ir_id) = {
        let p = plan_.borrow();
        (p.source_bytes, p.ir_id)
    };
    let item: Item = plan_.clone();
    side_lru::add(&mut value.by_fingerprint, bytes, TableKey::Int(fingerprint), item);
    value.by_ir.borrow_mut().insert(ir_id, plan_);
}

pub fn rebind(value: &RetainedPlans, plan_: &PlanRef, ir_id: i64, ir: &Rc<RenderIr>) {
    let old_id = plan_.borrow().ir_id;
    if old_id != ir_id {
        let current = value.by_ir.borrow().get(&old_id).cloned();
        if let Some(current) = current
            && Rc::ptr_eq(&current, plan_) {
                value.by_ir.borrow_mut().remove(&old_id);
            }
        let mut p = plan_.borrow_mut();
        p.ir_id = ir_id;
        p.ir = ir.clone();
        drop(p);
        value.by_ir.borrow_mut().insert(ir_id, plan_.clone());
    }
}

pub fn find_segment(value: &mut RetainedPlans, identity: i64) -> Option<Rc<Segment>> {
    side_lru::find(&mut value.segments, &TableKey::Int(identity)).map(|item| item.downcast::<Segment>().unwrap_or_else(|_| unreachable!("segment table item")))
}

pub fn add_segment(value: &mut RetainedPlans, identity: i64, bytes: i64, segment_: Segment) {
    let item: Item = Rc::new(segment_);
    side_lru::add(&mut value.segments, bytes, TableKey::Int(identity), item);
}

pub fn segment_count(value: &mut RetainedPlans) -> i64 {
    side_lru::length(&mut value.segments)
}

pub fn clear(value: &mut RetainedPlans) {
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

pub fn same_resources(left: &[ResourceStamp], right: &[ResourceStamp]) -> bool {
    left == right
}

/// Whether every resource the commands name is live, and the stamps of those that are.
pub fn resource_stamps(resolve: &dyn Fn(i64) -> Option<Resource>, commands: &[Command]) -> (bool, Vec<ResourceStamp>) {
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
            Some(Resource::Image(image)) => {
                if image::gpu_snapshot(&image).is_err() {
                    missing = true;
                } else {
                    stamps.push(ResourceStamp::ImageStamp(id, image::identity(&image), image::generation(&image)));
                }
            }
            Some(Resource::Text(text)) => {
                if text::destroyed(&text) {
                    missing = true;
                } else {
                    stamps.push(ResourceStamp::TextStamp(id, text::identity(&text), text::generation(&text)));
                }
            }
            Some(Resource::Canvas(canvas)) => {
                if canvas::destroyed(&canvas) {
                    missing = true;
                } else {
                    stamps.push(ResourceStamp::CanvasStamp(id, canvas::identity(&canvas), canvas::generation(&canvas)));
                }
            }
        }
    }
    (!missing, stamps)
}
