//! Port of lib/flow_native/workspace_resources.ml: the audio samples and images a workspace owns
//! (at most 64 of each), released together on `close`.
use rays::audio::{self, sample};
use rays::image;
use rays_diagnostic::Diagnostic;
use std::cell::RefCell;
use std::rc::Rc;

pub struct data {
    pub samples: Vec<(String, Rc<RefCell<sample::Sample>>)>,
    pub images: Vec<(crate::workspace_images::ImageKey, image::Image)>,
    pub audio: Option<Rc<audio::Audio>>,
    pub closed: bool,
    pub samples_created: i64,
    pub samples_destroyed: i64,
    pub images_created: i64,
    pub images_destroyed: i64,
}

/// Shared by the host and the image owner (handle semantics, as the OCaml mutable record).
#[derive(Clone)]
pub struct WorkspaceResources(pub Rc<RefCell<data>>);

pub fn create() -> WorkspaceResources {
    WorkspaceResources(Rc::new(RefCell::new(data {
        samples: Vec::new(),
        images: Vec::new(),
        audio: None,
        closed: false,
        samples_created: 0,
        samples_destroyed: 0,
        images_created: 0,
        images_destroyed: 0,
    })))
}

fn error(code: &str, message: &str) -> Diagnostic {
    flow_syntax::value::fail(code, message)
}

pub fn closed(resources: &WorkspaceResources) -> bool {
    resources.0.borrow().closed
}

/// The sample under `key`, made (and the audio device opened) on first use.
pub fn sample(resources: &WorkspaceResources, key: &str, make: &dyn Fn(&audio::Audio) -> Result<sample::Sample, String>) -> Result<Rc<RefCell<sample::Sample>>, Diagnostic> {
    {
        let r = resources.0.borrow();
        if r.closed {
            return Err(error("E_AUDIO", "Workspace resources are closed."));
        }
        if let Some((_, sample)) = r.samples.iter().find(|(k, _)| k == key) {
            return Ok(sample.clone());
        }
        if r.samples.len() >= 64 {
            return Err(error("E_AUDIO", "A workspace owns at most 64 audio samples."));
        }
    }
    let existing = resources.0.borrow().audio.clone();
    let initialized = match existing {
        Some(audio) => audio,
        None => {
            let audio = Rc::new(audio::init().map_err(|m| error("E_AUDIO", &m))?);
            resources.0.borrow_mut().audio = Some(audio.clone());
            audio
        }
    };
    let sample = Rc::new(RefCell::new(make(&initialized).map_err(|m| error("E_AUDIO", &m))?));
    let mut r = resources.0.borrow_mut();
    r.samples.insert(0, (key.to_owned(), sample.clone()));
    r.samples_created += 1;
    Ok(sample)
}

/// The image under `key`, made on first use.
pub fn image(resources: &WorkspaceResources, key: &crate::workspace_images::ImageKey, make: &dyn Fn() -> Result<image::Image, String>) -> Result<image::Image, Diagnostic> {
    {
        let r = resources.0.borrow();
        if r.closed {
            return Err(error("E_IMAGE", "Workspace resources are closed."));
        }
        if let Some((_, image)) = r.images.iter().find(|(k, _)| k == key) {
            return Ok(image.clone());
        }
        if r.images.len() >= 64 {
            return Err(error("E_IMAGE", "A workspace owns at most 64 images."));
        }
    }
    let image = make().map_err(|m| error("E_IMAGE", &m))?;
    let mut r = resources.0.borrow_mut();
    r.images.insert(0, (key.to_owned(), image.clone()));
    r.images_created += 1;
    Ok(image)
}

pub fn close(resources: &WorkspaceResources) {
    let (samples, images, audio) = {
        let mut r = resources.0.borrow_mut();
        if r.closed {
            return;
        }
        (std::mem::take(&mut r.samples), std::mem::take(&mut r.images), r.audio.take())
    };
    for (_, sample) in &samples {
        sample::destroy(&mut sample.borrow_mut());
        resources.0.borrow_mut().samples_destroyed += 1;
    }
    for (_, image) in &images {
        let _ = image::destroy(image);
        resources.0.borrow_mut().images_destroyed += 1;
    }
    if let Some(audio) = audio {
        let _ = audio::shutdown(&audio);
    }
    resources.0.borrow_mut().closed = true;
}
