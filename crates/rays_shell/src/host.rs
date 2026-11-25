//! PXUI's host services from Rays: the kit font and GPU images in, scene layers and platform
//! requests out.
//!
//! Rays owns the GPU. Each frame that has UI layers, the scene renderer calls [`begin`] before it records its first pass: PXUI's
//! `Renderer` (made on Rays' own device, adopted through its native handles) records the glyph uploads into the frame's command
//! buffer. Each UI layer is then drawn by [`draw_layer`] into Rays' open pass, between the scene nodes around it; PXUI never
//! submits. A layer with a backdrop blur splits the pass through `UiPass::split`.
use input::{frame::Frame, Event};
use ogpu_core::backend::{self, Device, Texture};
use ogpu_core::error::{self, Error, ErrorKind};
use ogpu_core::types::{Extent, Origin, TextureDescriptor, TextureFormat, TextureUsage};
use pxui::{Bytes, Commands, Gpu, Image, Pass, Renderer, Str, TargetDesc, Ui};
use rays::{clipboard, image, scene, sketch};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::{Rc, Weak};

/// The bytes of the kit's font face: the file at `path` (`RAYS_UI_FONT`), else the bundled Pragmasevka.
/// Fonts come only from these bytes; nothing scans the system.
pub fn font(path: Option<&str>) -> Vec<u8> {
    path.and_then(|p| std::fs::read(p).ok()).unwrap_or_else(|| pxui::pragmasevka().to_vec())
}

/// A UI on `platform` with the kit font at `path` (see [`font`]).
pub fn create(platform: input::Platform, font_size: Option<i64>, path: Option<&str>) -> Ui {
    let platform = if platform == input::Platform::Apple { pxui::Platform::Apple } else { pxui::Platform::Other };
    // libpxui allocates in its own allocator: its counters join this process's
    mem::set_peer(pxui::library_counter);
    let bytes = font(path);
    let fonts = [pxui::FontData(&bytes)];
    let desc = pxui::UiDesc::new(platform).fonts(&fonts);
    let desc = font_size.map_or(desc, |size| desc.font_size(size as f32));
    Ui::new(&desc).expect("PXUI creation")
}

/// The frame's input as the UI routed it to `owner` (a viewport's root key; `None`: the app's).
pub fn input(ui: &mut Ui, owner: Option<i64>) -> Frame {
    let routed = ui.routed_input(owner).expect("completed PXUI frame");
    let raw = routed.input();
    let events = routed.events().enumerate().filter_map(|(index, event)| {
        let touch = (event.kind == pxui::Event::TOUCH).then(|| routed.touch(index)).flatten().and_then(|touch| Some(input::Touch {
            device: touch.device, finger: touch.finger, point: (touch.x, touch.y),
            time: touch.time, pressure: touch.pressure, phase: input::TouchPhase::from_code(touch.phase)?,
        }));
        Event::from_raw(&input::wire::Raw {
            kind: event.kind, code: event.code, x: event.x, y: event.y, value: event.value,
            text: Some(pxui::event_text(event)), touch: touch.as_ref(),
        })
    }).collect();
    Frame {
        input: input::Frame {
            t: raw.time, dt: raw.dt, frame: raw.frame, tick: raw.tick, size: (raw.logical_size[0], raw.logical_size[1]), pointer: (raw.pointer[0], raw.pointer[1]),
            buttons: routed.buttons().iter().filter_map(|b| input::MouseButton::from_code(*b)).collect(),
            keys: routed.keys().iter().filter_map(|k| input::Key::from_code(*k)).collect(),
            events,
        },
        pixel_scale: (raw.pixel_scale[0], raw.pixel_scale[1]), fps: raw.fps, mouse_delta: (raw.mouse_delta[0], raw.mouse_delta[1]),
    }
}

/// The frame gate: build only for input or a due repaint.
pub fn wants_frame(ui: &Ui, events: &[Event], now: f64) -> bool {
    !events.is_empty() || ui.wants_frame(&[], now)
}

/// The last root pressed by a pointer press among the frame's events, by the previous hit tree.
pub fn last_press_within(ui: &Ui, frame: &Frame, roots: &[pxui::Key]) -> Option<i64> {
    frame.input.events.iter().rev().find_map(|event| {
        let Event::PointerPressed(_, (x, y)) = event else { return None };
        match ui.root_at(pxui::Point { x: *x as f32, y: *y as f32 }, roots) {
            Some(pxui::Key::Resolved(key)) => Some(key),
            _ => None,
        }
    })
}

/// The platform whose editing conventions the UI follows, as `input` names it.
pub fn platform(ui: &Ui) -> input::Platform {
    if ui.platform() == pxui::Platform::Apple { input::Platform::Apple } else { input::Platform::Other }
}

/// What one frame's input takes on the wire: the events, held keys and buttons as the C ABI numbers them. Kept between frames so a
/// settled frame allocates nothing.
#[derive(Default)]
struct Encoded {
    events: Vec<pxui::Event>,
    text: Vec<String>,
    touches: Vec<pxui::Touch>,
    keys: Vec<u32>,
    buttons: Vec<u32>,
}

thread_local! {
    static ENCODED: Cell<Encoded> = Cell::new(Encoded::default());
    /// What the ledger holds for each live UI (by `Ui::instance_id`).
    static LEDGER: RefCell<HashMap<usize, Bytes>> = RefCell::new(HashMap::new());
}

fn wire_event(event: &Event, scratch: &mut String) -> pxui::Event {
    let raw = event.raw(scratch);
    let text = raw.text.unwrap_or("");
    pxui::Event {
        code: raw.code, x: raw.x, y: raw.y, value: raw.value,
        text: Str { ptr: text.as_ptr(), len: text.len() },
        ..pxui::Event::new(raw.kind)
    }
}

/// Run a frame as the Rays host does: the clipboard text the last frame asked for arrives first,
/// and the clipboard and window-ground requests apply after.
pub fn frame(ui: &mut Ui, frame: &Frame, f: &mut dyn FnMut(&mut Ui)) {
    let pasted = if ui.paste_requested() { clipboard::get_text().ok().map(Event::Paste) } else { None };
    build_with(ui, frame, pasted, f);
    if let Some(text) = ui.clipboard_request() {
        let _ = clipboard::set_text(text);
    }
    // no window, no effect
    if let Some(ground) = ui.background_request() {
        let _ = sketch::set_window_background(rays_math::color::rgba(ground.r as i64, ground.g as i64, ground.b as i64, ground.a as i64));
    }
    charge(ui);
}

/// Build one frame of `frame`'s input with no host effect (no clipboard, no window ground, no ledger): what a case that only
/// looks at the UI runs.
pub fn build(ui: &mut Ui, frame: &Frame, f: &mut dyn FnMut(&mut Ui)) {
    build_with(ui, frame, None, f);
}

fn build_with(ui: &mut Ui, frame: &Frame, pasted: Option<Event>, f: &mut dyn FnMut(&mut Ui)) {
    let mut enc = ENCODED.take();
    let events = pasted.iter().chain(&frame.input.events);
    let count = events.clone().count();
    enc.events.clear();
    enc.text.resize(count, String::new());
    enc.touches.clear();
    enc.touches.reserve(events.clone().filter(|event| matches!(event, Event::Touch(_))).count());
    for (event, text) in events.zip(enc.text.iter_mut()) {
        if let Event::Touch(touch) = event {
            enc.touches.push(pxui::Touch::new(
                touch.device, touch.finger, [touch.point.0, touch.point.1],
                touch.time, touch.pressure, touch.phase.code(),
            ));
            let touch = enc.touches.last().expect("encoded touch");
            enc.events.push(pxui::Event {
                text: Str { ptr: std::ptr::from_ref(touch).cast(), len: std::mem::size_of::<pxui::Touch>() },
                ..pxui::Event::new(pxui::Event::TOUCH)
            });
        } else {
            enc.events.push(wire_event(event, text));
        }
    }
    enc.keys.clear();
    enc.keys.extend(frame.input.keys.iter().map(|key| key.code()));
    enc.buttons.clear();
    enc.buttons.extend(frame.input.buttons.iter().map(|button| button.code()));
    let (width, height) = frame.input.size;
    let input = pxui::FrameIn::new(width as f32, height as f32)
        .time(frame.input.t)
        .scale(frame.pixel_scale.0 as f32)
        .events(&enc.events);
    let snapshot = pxui::InputSnapshot {
        keys: &enc.keys,
        buttons: &enc.buttons,
        pointer: [frame.input.pointer.0, frame.input.pointer.1],
        mouse_delta: [frame.mouse_delta.0, frame.mouse_delta.1],
        pixel_scale: [frame.pixel_scale.0, frame.pixel_scale.1],
        dt: frame.input.dt,
        frame: frame.input.frame,
        tick: frame.input.tick,
        fps: frame.fps,
    };
    let built = ui.frame_with_snapshot(&input, &snapshot, f).map(|_| ());
    ENCODED.set(enc);
    built.map_err(|code| format!("PXUI frame failed ({code:?}): {}", ui.last_error())).expect("PXUI frame");
}

/// Take what the UI held off the memory ledger and let go of its renderer (the UI itself goes when its owner drops it).
pub fn destroy(ui: &mut Ui) {
    let id = ui.instance_id();
    // its renderer goes now, while the device lives: the scene's cached layers still hold the paint closure (and through it the
    // renderer's cell), so dropping the entry alone would keep the renderer, and its lease on the device, past the device's end
    ENTRIES.with(|entries| entries.borrow_mut().retain(|e| keep_or_end(e, e.id != id)));
    LEDGER.with(|ledger| ledger.borrow_mut().remove(&id));
    let was = ui.bytes();
    settle_ledger(was, Bytes::default());
}

/// The ledger follows what the UI reports holding (after every `frame`).
pub fn charge(ui: &mut Ui) {
    let now = ui.bytes();
    let was = LEDGER.with(|ledger| ledger.borrow_mut().insert(ui.instance_id(), now)).unwrap_or_default();
    settle_ledger(was, now);
}

fn settle_ledger(was: Bytes, now: Bytes) {
    use std::sync::OnceLock;
    static ATLAS: OnceLock<mem::Owner> = OnceLock::new();
    static TEXT_UNDO: OnceLock<mem::Owner> = OnceLock::new();
    for (owner, was, now) in [
        (ATLAS.get_or_init(|| mem::plan("pxui.atlas")), was.atlas, now.atlas),
        (TEXT_UNDO.get_or_init(|| mem::document("pxui.text_undo")), was.text_undo, now.text_undo),
    ] {
        mem::charge(owner, now - was);
        mem::release(owner, was - now);
    }
}

/// The last frame as scene nodes: each layer after what `under` puts beneath its floating root
/// (by box key), then the focused text's input-method area. The renderer draws each layer, borrowed
/// from the UI, in its place in the frame's pass.
pub fn scene(ui: &Rc<RefCell<Ui>>, under: Option<&dyn Fn(i64) -> Vec<scene::Node>>) -> Vec<scene::Node> {
    let paint = painter(ui);
    let ui = ui.borrow();
    let mut nodes = Vec::new();
    for (index, layer) in ui.layers().enumerate() {
        if let Some(under) = under {
            nodes.extend(under(layer.key));
        }
        if !layer.instances.is_empty() {
            nodes.push(scene::layer::ui(paint.clone(), index, ui.draw_revision() as i64));
        }
    }
    if let Some(ime) = ui.ime() {
        let pxui::Rect { x, y, w, h } = ime.rect;
        nodes.push(scene::text_input_region(
            (x.round() as i64, y.round() as i64),
            w.round() as i64, h.round() as i64, Some(true), Some(ime.caret.round() as i64),
        ).expect("PXUI input area"));
    }
    nodes
}

/// One live UI the scene draws: its paint closure (one per UI, so equal frames give equal scene nodes) and its renderer, made on
/// the first frame that draws it.
struct Entry {
    ui: Weak<RefCell<Ui>>,
    id: usize,
    paint: scene::UiPaint,
    renderer: Rc<RefCell<Option<Renderer>>>,
    blur_extent: Cell<Option<[u32; 2]>>,
}

thread_local! {
    static ENTRIES: RefCell<Vec<Entry>> = const { RefCell::new(Vec::new()) };
    /// Rays' device, the textures of UI images sit on it
    static DEVICE: RefCell<Option<Device>> = const { RefCell::new(None) };
    /// serial of the last frame prepared, and the textures dropped since, each waiting for the frames that might sample it
    static LAST_SERIAL: Cell<u64> = const { Cell::new(0) };
    static GRAVE: RefCell<Vec<(Texture, u64)>> = const { RefCell::new(Vec::new()) };
    static IMAGES: RefCell<Vec<(i64, i64, Weak<Owned>, Image)>> = const { RefCell::new(Vec::new()) };
}

/// The renderer cell of the UI's scene entry, for the regression that it is emptied by `destroy` however long a cached paint closure lives.
pub fn renderer_probe(ui: &Ui) -> Option<Rc<RefCell<Option<Renderer>>>> {
    let id = ui.instance_id();
    ENTRIES.with(|entries| entries.borrow().iter().find(|e| e.id == id).map(|e| e.renderer.clone()))
}

/// `retain`'s answer for an entry; one that goes has its renderer destroyed first.
fn keep_or_end(entry: &Entry, keep: bool) -> bool {
    if !keep {
        entry.renderer.borrow_mut().take();
    }
    keep
}

fn painter(ui: &Rc<RefCell<Ui>>) -> scene::UiPaint {
    scene::on_frame_begin(begin);
    scene::on_device_release(release);
    ENTRIES.with(|entries| {
        let mut entries = entries.borrow_mut();
        let weak = Rc::downgrade(ui);
        if let Some(e) = entries.iter().find(|e| e.ui.ptr_eq(&weak)) {
            return e.paint.clone();
        }
        entries.retain(|e| keep_or_end(e, e.ui.strong_count() > 0));
        let renderer: Rc<RefCell<Option<Renderer>>> = Rc::new(RefCell::new(None));
        let (held, drawn) = (weak.clone(), renderer.clone());
        let paint: scene::UiPaint = Rc::new(move |pass: &scene::UiPass, layer: usize| draw_layer(&held, &drawn, pass, layer));
        entries.push(Entry { ui: weak, id: ui.borrow().instance_id(), paint: paint.clone(), renderer, blur_extent: Cell::new(None) });
        paint
    })
}

/// Prepare backing textures for a UI that requests backdrop blur. Call before rendering its layers.
pub fn enable_backdrop_blur(ui: &Rc<RefCell<Ui>>) {
    let paint = painter(ui);
    ENTRIES.with(|entries| {
        if let Some(entry) = entries.borrow().iter().find(|entry| Rc::ptr_eq(&entry.paint, &paint))
            && entry.blur_extent.get().is_none() {
            entry.blur_extent.set(Some([0; 2]));
        }
    });
}

/// A PXUI failure as an OGPU error, with the UI's own message.
fn failure(ui: &Weak<RefCell<Ui>>, what: &str, cause: pxui::Error) -> Error {
    let message = ui.upgrade().and_then(|ui| ui.try_borrow().ok().map(|ui| ui.last_error().to_owned())).unwrap_or_default();
    error::make("Pxui.paint", ErrorKind::InvalidState, &format!("{what} failed ({cause:?}): {message}"))
}

/// Before a frame with UI layers records its first pass: the renderer of each UI it draws records the glyph rectangles rasterised
/// since the last frame (and makes itself on first use).
fn begin(frame: &scene::FrameStart) -> Result<(), Error> {
    DEVICE.with(|d| *d.borrow_mut() = Some(frame.device.clone()));
    LAST_SERIAL.set(frame.serial);
    GRAVE.with(|g| g.borrow_mut().retain(|(texture, serial)| {
        let gone = *serial <= frame.completed;
        if gone {
            let _ = backend::destroy_texture(texture);
        }
        !gone
    }));
    let native = backend::native_commands(&frame.commands)?;
    let commands = Commands { vk_command_buffer: native.vk_command_buffer, mtl_command_buffer: native.mtl_command_buffer, ..Commands::default() };
    for paint in &frame.painters {
        let found = ENTRIES.with(|entries| entries.borrow().iter().find(|e| Rc::ptr_eq(&e.paint, paint)).map(|e| (e.ui.clone(), e.renderer.clone(), e.blur_extent.get())));
        let Some((ui, cell, blur_extent)) = found else { continue };
        let Some(ui_rc) = ui.upgrade() else { continue };
        let mut renderer = cell.borrow_mut();
        let creating = renderer.is_none();
        if creating {
            let ui = ui_rc.try_borrow().map_err(|_| error::make("Pxui.paint", ErrorKind::InvalidState, "the UI is being changed while its frame renders"))?;
            *renderer = Some(make_renderer(&ui, &frame.device)?);
        }
        let renderer = renderer.as_mut().expect("renderer");
        if blur_extent.is_some_and(|extent| creating || extent != frame.size_px) {
            let [width, height] = frame.size_px;
            renderer.resize(width, height).map_err(|code| failure(&ui, "prepare backdrop blur", code))?;
            ENTRIES.with(|entries| {
                if let Some(entry) = entries.borrow().iter().find(|entry| Rc::ptr_eq(&entry.paint, paint)) {
                    entry.blur_extent.set(Some(frame.size_px));
                }
            });
        }
        let prepared = renderer.prepare(&commands, frame.serial, frame.completed);
        prepared.map_err(|code| failure(&ui, "renderer prepare", code))?;
    }
    Ok(())
}

/// A renderer for `ui` on Rays' device: it adopts the device's native handles (one lease, released with the renderer).
fn make_renderer(ui: &Ui, device: &Device) -> Result<Renderer, Error> {
    let native = backend::native_leased(device)?;
    let gpu = Gpu {
        size: native.size, backend: native.backend, instance: native.instance, physical_device: native.physical_device,
        device: native.device, queue: native.queue, queue_family: native.queue_family, api_version: native.api_version,
        features: native.features, get_instance_proc_addr: native.get_instance_proc_addr, pipeline_cache: native.pipeline_cache,
        mtl_device: native.mtl_device, wl_display: native.wl_display, wl_surface: native.wl_surface, metal_layer: native.metal_layer,
        hwnd: native.hwnd, owner_token: native.owner_token, release_owner: native.release_owner,
    };
    // the scene's UI pass is RGBA8, one sample, no depth
    Renderer::new(ui, gpu, TargetDesc { format: 1, samples: 1 }).map_err(|cause| error::make("Pxui.paint", ErrorKind::InvalidState, &format!("renderer creation failed ({cause:?}): {}", ui.last_error())))
}

/// Draws layer `layer` of the UI into the scene's open pass.
fn draw_layer(ui: &Weak<RefCell<Ui>>, renderer: &Rc<RefCell<Option<Renderer>>>, pass: &scene::UiPass, layer: usize) -> Result<(), Error> {
    if ui.strong_count() == 0 {
        return Ok(());
    }
    let mut renderer = renderer.borrow_mut();
    let Some(renderer) = renderer.as_mut() else {
        return Err(error::make("Pxui.paint", ErrorKind::InvalidState, "the UI has no renderer: its frame was not prepared"));
    };
    let commands = backend::native_commands(pass.commands())?;
    let open = backend::native_pass(pass.encoder)?;
    let ui_pass = Pass {
        vk_command_buffer: open.vk_command_buffer, mtl_render_encoder: open.mtl_render_encoder,
        width_px: pass.size_px[0], height_px: pass.size_px[1], scale: pass.scale, mtl_command_buffer: commands.mtl_command_buffer,
        ..Pass::default()
    };
    let mut split_error = None;
    let drawn = renderer.draw_layer(&ui_pass, layer as u32, &mut |record| {
        let reopened = pass.split(&mut |_, target| {
            let native = backend::native_texture(target)?;
            let image = Image { vk_image_view: native.vk_image_view, mtl_texture: native.mtl_texture, width: native.width, height: native.height, format: native.format, ..Image::default() };
            record(&image).map_err(|code| error::make("Pxui.paint", ErrorKind::InvalidState, &format!("blur recording failed ({code:?})")))
        });
        let next = reopened.and_then(|encoder| backend::native_pass(&encoder));
        match next {
            Ok(next) => Ok(Pass { vk_command_buffer: next.vk_command_buffer, mtl_render_encoder: next.mtl_render_encoder, ..ui_pass }),
            Err(e) => {
                split_error = Some(e);
                Err(pxui::Error::Gpu)
            }
        }
    });
    if let Err(code) = drawn {
        return Err(split_error.unwrap_or_else(|| failure(ui, "renderer draw", code)));
    }
    // PXUI leaves its own pipeline and bindings in the pass: Rays forgets what it cached before it draws more
    backend::invalidate_state(&pass.open_pass())
}

/// Free the UI renderers (they adopted the device) and the image textures; the scene renderer calls this before it destroys a
/// device. The next frame makes them again.
fn release() {
    ENTRIES.with(|entries| entries.borrow().iter().for_each(|e| drop(e.renderer.borrow_mut().take())));
    IMAGES.with(|images| images.borrow_mut().clear());
    GRAVE.with(|grave| grave.borrow_mut().drain(..).for_each(|(texture, _)| {
        let _ = backend::destroy_texture(&texture);
    }));
    DEVICE.with(|d| d.borrow_mut().take());
}

/// Draw calls the thread's UI renderers have issued.
pub fn draw_calls() -> u64 {
    ENTRIES.with(|entries| entries.borrow().iter().map(|e| e.renderer.borrow().as_ref().map_or(0, Renderer::draw_calls)).sum())
}

/// A GPU texture holding a Rays image's pixels, owned by what shows it: it goes when the last holder drops, after the frames that
/// might sample it have finished.
struct Owned {
    texture: Texture,
}

impl Drop for Owned {
    fn drop(&mut self) {
        GRAVE.with(|grave| grave.borrow_mut().push((self.texture.clone(), LAST_SERIAL.get())));
    }
}

/// A Rays image as PXUI's handle, for `Paint::image`: it keeps the texture alive for as long as it is held.
pub struct HostImage {
    image: Image,
    _texture: Rc<Owned>,
}

impl std::ops::Deref for HostImage {
    type Target = Image;

    fn deref(&self) -> &Image {
        &self.image
    }
}

/// Whether a frame has run, so the device the images sit on is known (`image` is `None` until then).
pub fn ready() -> bool { DEVICE.with(|d| d.borrow().is_some()) }

/// The image on Rays' device as a handle PXUI draws; `None` until a frame has run and the device is known (`ready`), and when the
/// image cannot be made a texture. A caller that wants it shown asks for the next frame while not `ready`.
pub fn image(i: &image::Image) -> Option<HostImage> {
    let (id, generation) = (image::identity(i), image::generation(i));
    let held = IMAGES.with(|images| images.borrow().iter().find(|(key, at, ..)| *key == id && *at == generation).and_then(|(_, _, owner, image)| owner.upgrade().map(|o| (o, *image))));
    if let Some((owner, handle)) = held {
        return Some(HostImage { image: handle, _texture: owner });
    }
    let device = DEVICE.with(|d| d.borrow().clone())?;
    let (width, height) = image::dimensions(i);
    let pixels = image::pixels(i).ok()?;
    let texture = backend::create_texture(&device, TextureDescriptor {
        label: Some("pxui.image".into()), width, height, depth: 1, mip_levels: 1, sample_count: 1,
        format: TextureFormat::Rgba8Unorm, usage: vec![TextureUsage::TextureBinding, TextureUsage::TextureCopyDst],
    }).ok()?;
    backend::write_texture(&texture, None, Origin { x: 0, y: 0, z: 0 }, Extent { width, height, depth: 1 }, width * 4, &pixels).ok()?;
    let native = backend::native_texture(&texture).ok()?;
    let handle = Image { vk_image_view: native.vk_image_view, mtl_texture: native.mtl_texture, width: native.width, height: native.height, format: native.format, ..Image::default() };
    let owner = Rc::new(Owned { texture });
    IMAGES.with(|images| {
        let mut images = images.borrow_mut();
        images.retain(|(key, _, owner, _)| *key != id && owner.strong_count() > 0);
        images.push((id, generation, Rc::downgrade(&owner), handle));
    });
    Some(HostImage { image: handle, _texture: owner })
}
