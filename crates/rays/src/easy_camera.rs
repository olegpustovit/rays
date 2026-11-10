//! Functional pointer-controlled orbit cameras use logical coordinates and return validation errors as Results.
use crate::camera;
use crate::frame;
use crate::num::{fmax, fmin};
use input::{Event, Key, MouseButton};
use rays_math::vec3;
use std::cell::RefCell;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Interaction {
    Orbit,
    Pan,
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Settings {
    target: vec3::T,
    distance: f64,
    azimuth: f64,
    elevation: f64,
}

#[derive(Clone, Copy, Debug)]
struct Motion {
    interaction: Interaction,
    dx: f64,
    dy: f64,
}

#[derive(Clone)]
struct CameraSnapshot {
    snapshot_target: vec3::T,
    snapshot_distance: f64,
    snapshot_azimuth: f64,
    snapshot_elevation: f64,
    snapshot_fov_y: f64,
    snapshot_near: f64,
    snapshot_far: f64,
    snapshot_camera: camera::Camera,
}

#[derive(Clone)]
pub struct EasyCamera {
    target: vec3::T,
    distance: f64,
    azimuth: f64,
    elevation: f64,
    fov_y: f64,
    near: f64,
    far: f64,
    control_area: Option<(i64, i64, i64, i64)>,
    inertia: bool,
    drag: Option<(MouseButton, Interaction, (f64, f64))>,
    velocity: Option<Motion>,
    last_press: Option<(MouseButton, (f64, f64), f64)>,
    pointer: Option<(f64, f64)>,
    initial: Settings,
    camera_cache: RefCell<Option<CameraSnapshot>>,
}

fn validate_distance(distance: f64) -> Result<(), String> {
    if !distance.is_finite() || distance <= 0. {
        return Err("Easy_camera: distance must be finite and positive".to_string());
    }
    Ok(())
}

fn validate_control_area(area: Option<(i64, i64, i64, i64)>) -> Result<(), String> {
    if let Some((_, _, width, height)) = area
        && (width < 0 || height < 0) {
            return Err("Easy_camera: control-area dimensions must be non-negative".to_string());
        }
    Ok(())
}

const ROTATION_SENSITIVITY: f64 = 0.01;
const DOLLY_SENSITIVITY: f64 = 0.01;
const DRAG_COEFFICIENT: f64 = 0.9;

fn clamp_elevation(elevation: f64) -> f64 {
    let limit = (std::f64::consts::PI / 2.) - 1e-4;
    fmax(-limit, fmin(limit, elevation))
}

pub fn create(
    target: Option<vec3::T>,
    distance: Option<f64>,
    azimuth: Option<f64>,
    elevation: Option<f64>,
    fov_y: Option<f64>,
    inertia: Option<bool>,
) -> Result<EasyCamera, String> {
    let target = target.unwrap_or_else(vec3::zero);
    let distance = distance.unwrap_or(10.);
    let azimuth = azimuth.unwrap_or(0.);
    let elevation = elevation.unwrap_or(0.);
    let fov_y = fov_y.unwrap_or(std::f64::consts::PI / 3.);
    let inertia = inertia.unwrap_or(true);
    let near = 0.1;
    let far = 1000.;
    validate_distance(distance)?;
    camera::perspective(Some(fov_y), Some(near), Some(far), vec3::create(0., 0., distance), target)?;
    let elevation = clamp_elevation(elevation);
    Ok(EasyCamera {
        target,
        distance,
        azimuth,
        elevation,
        fov_y,
        near,
        far,
        control_area: None,
        inertia,
        drag: None,
        velocity: None,
        last_press: None,
        pointer: None,
        initial: Settings { target, distance, azimuth, elevation },
        camera_cache: RefCell::new(None),
    })
}

fn orbit_basis(up: vec3::T) -> (vec3::T, vec3::T) {
    let candidate = if vec3::dot(up, vec3::unit_z()).abs() < 0.999 { vec3::unit_z() } else { vec3::unit_x() };
    let forward = vec3::normalize(vec3::sub(candidate, vec3::scale(up, vec3::dot(candidate, up))));
    let right = vec3::normalize(vec3::cross(up, forward));
    (right, forward)
}

pub fn camera(value: &EasyCamera) -> Result<camera::Camera, String> {
    if let Some(cached) = value.camera_cache.borrow().as_ref()
        && cached.snapshot_target == value.target
        && cached.snapshot_distance == value.distance
        && cached.snapshot_azimuth == value.azimuth
        && cached.snapshot_elevation == value.elevation
        && cached.snapshot_fov_y == value.fov_y
        && cached.snapshot_near == value.near
        && cached.snapshot_far == value.far
    {
        return Ok(cached.snapshot_camera.clone());
    }
    let up = vec3::unit_y();
    let (right, forward) = orbit_basis(up);
    let cosine = value.elevation.cos();
    let horizontal = vec3::add(vec3::scale(right, cosine * value.azimuth.sin()), vec3::scale(forward, cosine * value.azimuth.cos()));
    let offset = vec3::scale(vec3::add(horizontal, vec3::scale(up, value.elevation.sin())), value.distance);
    let cam = camera::perspective(Some(value.fov_y), Some(value.near), Some(value.far), vec3::add(value.target, offset), value.target)?;
    let cam = camera::with_up(up, &cam)?;
    *value.camera_cache.borrow_mut() = Some(CameraSnapshot {
        snapshot_target: value.target,
        snapshot_distance: value.distance,
        snapshot_azimuth: value.azimuth,
        snapshot_elevation: value.elevation,
        snapshot_fov_y: value.fov_y,
        snapshot_near: value.near,
        snapshot_far: value.far,
        snapshot_camera: cam.clone(),
    });
    Ok(cam)
}

pub fn target(value: &EasyCamera) -> vec3::T {
    value.target
}

pub fn distance(value: &EasyCamera) -> f64 {
    value.distance
}

pub fn fov_y(value: &EasyCamera) -> f64 {
    value.fov_y
}

pub fn near(value: &EasyCamera) -> f64 {
    value.near
}

pub fn far(value: &EasyCamera) -> f64 {
    value.far
}

pub fn control_area(value: &EasyCamera) -> Option<(i64, i64, i64, i64)> {
    value.control_area
}

pub fn inertia(value: &EasyCamera) -> bool {
    value.inertia
}

/// Orbit to look from `eye` at `target` about the current up axis (elevation clamps just short of
/// the poles). Lens, clipping, and input settings are kept; inertia stops. Inverse of `camera`'s
/// orbit placement.
pub fn of_view(eye: vec3::T, target: vec3::T, value: &EasyCamera) -> EasyCamera {
    let offset = vec3::sub(eye, target);
    let distance = vec3::length(offset);
    if !distance.is_finite() || distance <= 1e-9 {
        return EasyCamera { target, drag: None, velocity: None, ..value.clone() };
    }
    let unit = vec3::scale(offset, 1. / distance);
    let (right, forward) = orbit_basis(vec3::unit_y());
    let height = fmax(-1., fmin(1., vec3::dot(unit, vec3::unit_y())));
    EasyCamera {
        target,
        distance,
        elevation: clamp_elevation(height.asin()),
        azimuth: vec3::dot(unit, right).atan2(vec3::dot(unit, forward)),
        drag: None,
        velocity: None,
        ..value.clone()
    }
}

/// Target the box center at `radius / tan (fov_y / 2) * 1.2` (radius: half the box diagonal),
/// keeping the viewing direction.
pub fn frame_bounds(min: vec3::T, max: vec3::T, value: &EasyCamera) -> EasyCamera {
    let center = vec3::scale(vec3::add(min, max), 0.5);
    let radius = 0.5 * vec3::length(vec3::sub(max, min));
    let distance = fmax(1e-3, radius / (value.fov_y / 2.).tan() * 1.2);
    EasyCamera { target: center, distance, drag: None, velocity: None, ..value.clone() }
}

pub fn with_distance(distance: f64, value: &EasyCamera) -> Result<EasyCamera, String> {
    validate_distance(distance)?;
    if distance == value.distance { Ok(value.clone()) } else { Ok(EasyCamera { distance, ..value.clone() }) }
}

pub fn with_fov_y(fov_y: f64, value: &EasyCamera) -> Result<EasyCamera, String> {
    if fov_y == value.fov_y {
        return Ok(value.clone());
    }
    camera::perspective(Some(fov_y), Some(value.near), Some(value.far), vec3::create(0., 0., value.distance), vec3::zero())?;
    Ok(EasyCamera { fov_y, ..value.clone() })
}

pub fn with_clip(near: f64, far: f64, value: &EasyCamera) -> Result<EasyCamera, String> {
    if near == value.near && far == value.far {
        return Ok(value.clone());
    }
    camera::perspective(Some(value.fov_y), Some(near), Some(far), vec3::create(0., 0., value.distance), vec3::zero())?;
    Ok(EasyCamera { near, far, ..value.clone() })
}

pub fn with_control_area(area: Option<(i64, i64, i64, i64)>, value: &EasyCamera) -> Result<EasyCamera, String> {
    validate_control_area(area)?;
    if area == value.control_area { Ok(value.clone()) } else { Ok(EasyCamera { control_area: area, drag: None, ..value.clone() }) }
}

pub fn with_inertia(inertia: bool, value: &EasyCamera) -> EasyCamera {
    if inertia == value.inertia {
        value.clone()
    } else {
        EasyCamera { inertia, velocity: if inertia { value.velocity } else { None }, ..value.clone() }
    }
}

pub fn reset(value: &EasyCamera) -> EasyCamera {
    EasyCamera {
        target: value.initial.target,
        distance: value.initial.distance,
        azimuth: value.initial.azimuth,
        elevation: value.initial.elevation,
        drag: None,
        velocity: None,
        ..value.clone()
    }
}

fn contains(value: &EasyCamera, (x, y): (f64, f64)) -> bool {
    match value.control_area {
        None => true,
        Some((left, top, width, height)) => x >= left as f64 && y >= top as f64 && x < (left + width) as f64 && y < (top + height) as f64,
    }
}

fn pan_axes(value: &EasyCamera) -> Result<(vec3::T, vec3::T), String> {
    let cam = camera(value)?;
    let forward = vec3::normalize(vec3::sub(camera::target(&cam), camera::position(&cam)));
    let up = camera::up(&cam);
    let right = vec3::normalize(vec3::cross(forward, up));
    let up = vec3::normalize(vec3::cross(right, forward));
    Ok((right, up))
}

fn apply_delta(frame_: &frame::Frame, value: &EasyCamera, interaction_: Interaction, dx: f64, dy: f64) -> Result<EasyCamera, String> {
    match interaction_ {
        Interaction::Orbit => Ok(EasyCamera {
            azimuth: value.azimuth - (dx * ROTATION_SENSITIVITY),
            elevation: clamp_elevation(value.elevation + (dy * ROTATION_SENSITIVITY)),
            ..value.clone()
        }),
        Interaction::Pan => {
            let (right, up) = pan_axes(value)?;
            let height = std::cmp::max(1, frame_.input.size.1);
            let units_per_pixel = 2. * value.distance * (value.fov_y / 2.).tan() / height as f64;
            let delta = vec3::add(vec3::scale(right, -dx * units_per_pixel), vec3::scale(up, dy * units_per_pixel));
            Ok(EasyCamera { target: vec3::add(value.target, delta), ..value.clone() })
        }
    }
}

fn interaction_for_button(button: MouseButton) -> Option<Interaction> {
    match button {
        MouseButton::LeftButton => Some(Interaction::Orbit),
        MouseButton::MiddleButton | MouseButton::RightButton => Some(Interaction::Pan),
        MouseButton::MouseX1 | MouseButton::MouseX2 => None,
    }
}

fn double_click(value: &EasyCamera, button: MouseButton, point: (f64, f64), time: f64) -> bool {
    match value.last_press {
        Some((previous_button, (previous_x, previous_y), previous_time)) => {
            let (x, y) = point;
            previous_button == button
                && time >= previous_time
                && time - previous_time <= 0.3
                && ((x - previous_x) * (x - previous_x)) + ((y - previous_y) * (y - previous_y)) <= 25.
        }
        None => false,
    }
}

fn apply_inertia(frame_: &frame::Frame, value: EasyCamera) -> Result<EasyCamera, String> {
    match (value.drag, value.velocity) {
        (None, Some(m)) if value.inertia && frame_.input.dt > 0. => {
            let frames = frame_.input.dt * 60.;
            let decay = DRAG_COEFFICIENT.powf(frames);
            let value = apply_delta(frame_, &value, m.interaction, m.dx * decay, m.dy * decay)?;
            if (m.dx * decay).abs() < 1e-4 && (m.dy * decay).abs() < 1e-4 {
                Ok(EasyCamera { velocity: None, ..value })
            } else {
                Ok(EasyCamera { velocity: Some(Motion { dx: m.dx * decay, dy: m.dy * decay, ..m }), ..value })
            }
        }
        (None, Some(_)) => Ok(EasyCamera { velocity: None, ..value }),
        _ => Ok(value),
    }
}

/// Consume the ordered pointer facts in one frame. Captured drags continue outside the control
/// area. Double-clicking resets the original target, distance, and orientation. Vertical
/// wheel/trackpad motion dollies and horizontal wheel/trackpad motion is ignored. Left drags
/// orbit and middle/right drags pan.
pub fn update(value: &EasyCamera, frame_: &frame::Frame) -> Result<EasyCamera, String> {
    if frame_.input.events.is_empty() {
        return apply_inertia(frame_, value.clone());
    }
    let has_pointer = frame_
        .input
        .events
        .iter()
        .any(|e| matches!(e, Event::PointerPressed(..) | Event::PointerMoved(_) | Event::PointerReleased(..)));
    let mut pointer = if has_pointer { value.pointer.unwrap_or(frame_.input.pointer) } else { frame_.input.pointer };
    let mut value = value.clone();
    for event in &frame_.input.events {
        value = match event {
            Event::PointerPressed(button, point) => {
                pointer = *point;
                if !contains(&value, *point) {
                    value
                } else if double_click(&value, *button, *point, frame_.input.t) {
                    let reset_ = reset(&value);
                    EasyCamera { last_press: Some((*button, *point, frame_.input.t)), ..reset_ }
                } else {
                    match interaction_for_button(*button) {
                        None => EasyCamera { last_press: Some((*button, *point, frame_.input.t)), ..value },
                        Some(interaction_) => EasyCamera {
                            drag: Some((*button, interaction_, *point)),
                            velocity: None,
                            last_press: Some((*button, *point, frame_.input.t)),
                            ..value
                        },
                    }
                }
            }
            Event::PointerMoved((x, y)) => {
                let point = (*x, *y);
                pointer = point;
                match value.drag {
                    None => value,
                    Some((button, interaction_, (previous_x, previous_y))) => {
                        let dx = x - previous_x;
                        let dy = y - previous_y;
                        let moved = apply_delta(frame_, &value, interaction_, dx, dy)?;
                        EasyCamera { drag: Some((button, interaction_, point)), velocity: Some(Motion { interaction: interaction_, dx, dy }), ..moved }
                    }
                }
            }
            Event::PointerReleased(button, point) => {
                pointer = *point;
                match value.drag {
                    Some((captured, _, _)) if captured == *button => {
                        let velocity = if value.inertia { value.velocity } else { None };
                        EasyCamera { drag: None, velocity, ..value }
                    }
                    _ => value,
                }
            }
            Event::Scrolled((_horizontal, vertical)) if contains(&value, pointer) => EasyCamera {
                distance: fmax(1e-4, value.distance * (-vertical * DOLLY_SENSITIVITY * 12.).exp()),
                velocity: None,
                pointer: Some(pointer),
                ..value
            },
            Event::PointerCancelled(button) => match value.drag {
                Some((captured, _, _)) if captured == *button => EasyCamera { drag: None, velocity: None, ..value },
                _ => value,
            },
            Event::FocusLost => EasyCamera { drag: None, velocity: None, last_press: None, ..value },
            _ => value,
        };
    }
    if has_pointer {
        match value.pointer {
            Some(previous) if previous == pointer => {}
            _ => value = EasyCamera { pointer: Some(pointer), ..value },
        }
    }
    apply_inertia(frame_, value)
}

/// One fly-camera step: held W/S/A/D/Q/E move along view forward, right, and the up axis at
/// `speed` units/s (Shift x4); pointer motion yaws about the up axis and pitches within +-89
/// degrees; each wheel step scales the speed by 1.2. The orbit target stays `distance` ahead.
/// The result is an ordinary orbit camera, so orbiting resumes seamlessly.
pub fn fly(speed: f64, value: &EasyCamera, frame_: &frame::Frame) -> Result<(EasyCamera, f64), String> {
    let view = camera(value)?;
    let up = vec3::unit_y();
    let eye = camera::position(&view);
    let forward = vec3::normalize(vec3::sub(camera::target(&view), eye));
    let (dx, dy) = frame_.mouse_delta;
    let yaw = -0.003 * dx;
    let pitch = -0.003 * dy;
    let along = vec3::dot(up, forward);
    let yawed = vec3::add(
        vec3::add(vec3::scale(forward, yaw.cos()), vec3::scale(vec3::cross(up, forward), yaw.sin())),
        vec3::scale(up, along * (1. - yaw.cos())),
    );
    let limit = 89. * std::f64::consts::PI / 180.;
    let elevation = fmax(-limit, fmin(limit, fmax(-1., fmin(1., vec3::dot(yawed, up))).asin() + pitch));
    let horizontal = vec3::normalize(vec3::sub(yawed, vec3::scale(up, vec3::dot(yawed, up))));
    let forward = vec3::add(vec3::scale(horizontal, elevation.cos()), vec3::scale(up, elevation.sin()));
    let right = vec3::normalize(vec3::cross(forward, up));
    let held = |key: char| frame_.input.keys.contains(&Key::KeyChar(key));
    let axis = |positive: char, negative: char| (if held(positive) { 1. } else { 0. }) - if held(negative) { 1. } else { 0. };
    let move_ = vec3::add(
        vec3::add(vec3::scale(forward, axis('w', 's')), vec3::scale(right, axis('d', 'a'))),
        vec3::scale(up, axis('q', 'e')),
    );
    let step = speed * frame_.input.dt * if frame_.input.keys.contains(&Key::Shift) { 4. } else { 1. };
    let eye = if step.is_finite() { vec3::add(eye, vec3::scale(move_, step)) } else { eye };
    let wheel: f64 = frame_.input.events.iter().fold(0., |total, e| match e {
        Event::Scrolled((_, steps)) => total + steps,
        _ => total,
    });
    let speed = fmax(0.01, fmin(1e4, speed * 1.2_f64.powf(wheel)));
    let target = vec3::add(eye, vec3::scale(forward, distance(value)));
    Ok((of_view(eye, target, value), speed))
}
