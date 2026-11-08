use std::fmt;
use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};
use std::sync::{Arc, Mutex};

use crate::Version;
use sdl3_sys::mixer as sys;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErrorKind {
    MixerError,
    WrongDomain,
    Destroyed,
    ParentHasDependents,
    InvalidArgument,
    IncompatibleVersion,
    NotInitialized,
    HandlesStillOpen,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    operation: String,
    kind: ErrorKind,
    message: String,
}

impl Error {
    pub fn operation(&self) -> &str { &self.operation }
    pub fn kind(&self) -> ErrorKind { self.kind }
    pub fn message(&self) -> &str { &self.message }
}


impl fmt::Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}: {}", self.operation, self.message)
    }
}

impl std::error::Error for Error {}

fn error(operation: &str, kind: ErrorKind, message: &str) -> Error {
    Error { operation: operation.to_owned(), kind, message: message.to_string() }
}

fn version(number: i32) -> Version {
    Version { major: number / 1_000_000, minor: number / 1_000 % 1_000, patch: number % 1_000 }
}

pub fn linked_version() -> Version {
    version(sys::linked_version())
}

/// The headers' own version macro, not a checked-in constant.
pub fn compiled_version() -> Version {
    version(sys::compiled_version())
}

pub fn check_version(release: bool) -> std::result::Result<(), Error> {
    crate::validate_version_for("SDL3_mixer", compiled_version(), release, linked_version())
        .map_err(|source| error("SDL3_mixer.check_version", ErrorKind::IncompatibleVersion, source.message()))
}

static LIVE_MIXERS: AtomicI64 = AtomicI64::new(0);
static LIVE_AUDIOS: AtomicI64 = AtomicI64::new(0);
static LIVE_TRACKS: AtomicI64 = AtomicI64::new(0);
static INIT_COUNT: AtomicI64 = AtomicI64::new(0);

// Handles dropped off the main thread wait here, unbounded, until the next main-thread operation.
static MIXER_QUEUE: Mutex<Vec<usize>> = Mutex::new(Vec::new());
static AUDIO_QUEUE: Mutex<Vec<usize>> = Mutex::new(Vec::new());
static TRACK_QUEUE: Mutex<Vec<usize>> = Mutex::new(Vec::new());

fn drain() {
    let tracks = std::mem::take(&mut *TRACK_QUEUE.lock().unwrap());
    let audios = std::mem::take(&mut *AUDIO_QUEUE.lock().unwrap());
    let mixers = std::mem::take(&mut *MIXER_QUEUE.lock().unwrap());
    tracks.into_iter().for_each(sys::destroy_track);
    audios.into_iter().for_each(sys::destroy_audio);
    mixers.into_iter().for_each(sys::destroy_mixer);
}

fn require_main(operation: &str) -> std::result::Result<(), Error> {
    if !main_thread() {
        Err(error(operation, ErrorKind::WrongDomain, "SDL3_mixer operation must run on the initial thread and SDL main thread"))
    } else {
        Ok(())
    }
}

fn enter_main(operation: &str) -> std::result::Result<(), Error> {
    require_main(operation)?;
    drain();
    Ok(())
}

fn int(value: i64) -> i32 {
    value.clamp(i32::MIN.into(), i32::MAX.into()) as i32
}

pub mod init {
    use super::*;

    pub fn init() -> std::result::Result<(), Error> {
        enter_main("SDL3_mixer.Init.init")?;
            check_version(true)?;
            sys::init().map_err(|message| error("SDL3_mixer.Init.init", ErrorKind::MixerError, &message))?;
            INIT_COUNT.fetch_add(1, Ordering::SeqCst);
            Ok(())
    }

    pub fn initialized() -> std::result::Result<bool, Error> {
        enter_main("SDL3_mixer.Init.initialized")?;
        Ok(INIT_COUNT.load(Ordering::SeqCst) > 0)
    }

    pub fn quit() -> std::result::Result<(), Error> {
        enter_main("SDL3_mixer.Init.quit")?;
            let handles = LIVE_MIXERS.load(Ordering::SeqCst) + LIVE_AUDIOS.load(Ordering::SeqCst) + LIVE_TRACKS.load(Ordering::SeqCst);
            if handles != 0 {
                Err(error("SDL3_mixer.Init.quit", ErrorKind::HandlesStillOpen, &format!("{handles} mixer/audio/track handle(s) are still open")))
            } else if INIT_COUNT.load(Ordering::SeqCst) == 0 {
                Ok(())
            } else {
                INIT_COUNT.fetch_sub(1, Ordering::SeqCst);
                sys::quit();
                Ok(())
            }
    }
}

fn require_initialized(operation: &str) -> std::result::Result<(), Error> {
    if init::initialized()? {
        Ok(())
    } else {
        Err(error(operation, ErrorKind::NotInitialized, "SDL3_mixer is not initialized"))
    }
}

pub mod mixer {
    use super::*;

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Mode {
        Device,
        Memory,
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct Format {
        pub sample_rate: i64,
        pub channels: i64,
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct Generated {
        pub mixed_bytes: i64,
        pub pcm_f32: Vec<u8>,
    }

    pub(crate) struct Inner {
        pub(crate) raw: usize,
        pub(crate) mode: Mode,
        pub(crate) channels: i64,
        pub(crate) tracks: AtomicI64,
        pub(crate) audios: AtomicI64,
        pub(crate) destroyed: AtomicBool,
    }

    impl Drop for Inner {
        fn drop(&mut self) {
            if !self.destroyed.swap(true, Ordering::SeqCst) {
                LIVE_MIXERS.fetch_sub(1, Ordering::SeqCst);
                MIXER_QUEUE.lock().unwrap().push(self.raw);
            }
        }
    }

    #[derive(Clone)]
    pub struct Mixer {
        pub(crate) inner: Arc<Inner>,
    }

    impl Mixer {
        pub(crate) fn raw(&self, operation: &str) -> std::result::Result<usize, Error> {
            enter_main(operation)?;
                if self.inner.destroyed.load(Ordering::SeqCst) {
                    return Err(error(operation, ErrorKind::Destroyed, "mixer is destroyed"));
                }
                require_initialized(operation)?;
                Ok(self.inner.raw)
        }

        fn finish_create(operation: &str, mode: Mode, raw: std::result::Result<usize, String>) -> std::result::Result<Mixer, Error> {
            let raw = raw.map_err(|message| error(operation, ErrorKind::MixerError, &message))?;
            match sys::format(raw) {
                Ok((_sample_rate, channels)) => {
                    LIVE_MIXERS.fetch_add(1, Ordering::SeqCst);
                    Ok(Mixer {
                        inner: Arc::new(Inner {
                            raw,
                            mode,
                            channels: channels.into(),
                            tracks: AtomicI64::new(0),
                            audios: AtomicI64::new(0),
                            destroyed: AtomicBool::new(false),
                        }),
                    })
                }
                Err(message) => {
                    sys::destroy_mixer(raw);
                    Err(error(operation, ErrorKind::MixerError, &message))
                }
            }
        }

        pub fn create_device() -> std::result::Result<Mixer, Error> {
            let operation = "SDL3_mixer.Mixer.create_device";
            enter_main(operation)?;
                require_initialized(operation)?;
                Mixer::finish_create(operation, Mode::Device, sys::create_device())
        }

        pub fn create_memory(sample_rate: i64, channels: i64) -> std::result::Result<Mixer, Error> {
            let operation = "SDL3_mixer.Mixer.create_memory";
            if !(8_000..=384_000).contains(&sample_rate) {
                Err(error(operation, ErrorKind::InvalidArgument, "sample rate must be in 8000..384000"))
            } else if channels <= 0 || channels > 8 {
                Err(error(operation, ErrorKind::InvalidArgument, "channel count must be in 1..8"))
            } else {
                enter_main(operation)?;
                    require_initialized(operation)?;
                    Mixer::finish_create(operation, Mode::Memory, sys::create_memory(sample_rate as i32, channels as i32))
            }
        }

        /// Generate exactly `frames` of interleaved native-endian float32 PCM from a memory
        /// mixer. `mixed_bytes` excludes silence appended after tracks finish, while `pcm_f32`
        /// always has the requested length.
        pub fn generate(&self, frames: i64) -> std::result::Result<Generated, Error> {
            let operation = "SDL3_mixer.Mixer.generate";
            let channels = self.inner.channels;
            if self.inner.mode != Mode::Memory {
                Err(error(operation, ErrorKind::InvalidArgument, "audio can only be generated by a memory mixer"))
            } else if frames <= 0 {
                Err(error(operation, ErrorKind::InvalidArgument, "frame count must be positive"))
            } else if channels > i64::MAX / 4 || frames > i64::MAX / (channels * 4) {
                Err(error(operation, ErrorKind::InvalidArgument, "generated PCM byte count overflows"))
            } else {
                let raw = self.raw(operation)?;
                    let mut pcm_f32 = vec![0u8; (frames * channels * 4) as usize];
                    let mixed_bytes = sys::generate(raw, &mut pcm_f32).map_err(|message| error(operation, ErrorKind::MixerError, &message))?;
                    Ok(Generated { mixed_bytes: mixed_bytes.into(), pcm_f32 })
            }
        }

        pub fn destroy(&self) -> std::result::Result<(), Error> {
            enter_main("SDL3_mixer.Mixer.destroy")?;
                if self.inner.destroyed.load(Ordering::SeqCst) {
                    return Ok(());
                }
                let (tracks, audios) = (self.inner.tracks.load(Ordering::SeqCst), self.inner.audios.load(Ordering::SeqCst));
                if tracks != 0 || audios != 0 {
                    Err(error(
                        "SDL3_mixer.Mixer.destroy",
                        ErrorKind::ParentHasDependents,
                        &format!("mixer still owns {tracks} track(s) and {audios} audio handle(s)"),
                    ))
                } else {
                    self.inner.destroyed.store(true, Ordering::SeqCst);
                    LIVE_MIXERS.fetch_sub(1, Ordering::SeqCst);
                    sys::destroy_mixer(self.inner.raw);
                    Ok(())
                }
        }
    }
}

pub mod audio {
    use super::mixer::{Inner, Mixer};
    use super::*;

    pub struct Audio {
        pub(crate) raw: usize,
        pub(crate) mixer: Arc<Inner>,
        pub(crate) destroyed: AtomicBool,
    }

    impl Audio {
        pub fn load_bytes(mixer: &Mixer, bytes: &[u8]) -> std::result::Result<Audio, Error> {
            let operation = "SDL3_mixer.Audio.load_bytes";
            if bytes.is_empty() {
                return Err(error(operation, ErrorKind::InvalidArgument, "encoded audio buffer must be non-empty"));
            }
            let raw = mixer.raw(operation)?;
                let audio = sys::load_bytes(raw, bytes).map_err(|message| error(operation, ErrorKind::MixerError, &message))?;
                LIVE_AUDIOS.fetch_add(1, Ordering::SeqCst);
                mixer.inner.audios.fetch_add(1, Ordering::SeqCst);
                Ok(Audio { raw: audio, mixer: Arc::clone(&mixer.inner), destroyed: AtomicBool::new(false) })
        }

        pub fn destroy(&self) -> std::result::Result<(), Error> {
            enter_main("SDL3_mixer.Audio.destroy")?;
                if !self.destroyed.swap(true, Ordering::SeqCst) {
                    LIVE_AUDIOS.fetch_sub(1, Ordering::SeqCst);
                    self.mixer.audios.fetch_sub(1, Ordering::SeqCst);
                    sys::destroy_audio(self.raw);
                }
                Ok(())
        }
    }

    impl Drop for Audio {
        fn drop(&mut self) {
            if !self.destroyed.swap(true, Ordering::SeqCst) {
                LIVE_AUDIOS.fetch_sub(1, Ordering::SeqCst);
                self.mixer.audios.fetch_sub(1, Ordering::SeqCst);
                AUDIO_QUEUE.lock().unwrap().push(self.raw);
            }
        }
    }
}

pub mod track {
    use super::audio::Audio;
    use super::mixer::{Inner, Mixer};
    use super::*;

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Status {
        Stopped,
        Playing,
        Paused,
    }

    pub struct Track {
        raw: usize,
        mixer: Arc<Inner>,
        destroyed: AtomicBool,
    }

    impl Track {
        fn raw(&self, operation: &str) -> std::result::Result<usize, Error> {
            enter_main(operation)?;
                if self.destroyed.load(Ordering::SeqCst) {
                    Err(error(operation, ErrorKind::Destroyed, "track is destroyed"))
                } else if self.mixer.destroyed.load(Ordering::SeqCst) {
                    Err(error(operation, ErrorKind::Destroyed, "track's mixer is destroyed"))
                } else {
                    require_initialized(operation)?;
                    Ok(self.raw)
                }
        }

        fn mutate(&self, operation: &str, call: &dyn Fn(usize) -> std::result::Result<(), String>) -> std::result::Result<(), Error> {
            let raw = self.raw(operation)?;
            call(raw).map_err(|message| error(operation, ErrorKind::MixerError, &message))
        }

        pub fn create(mixer: &Mixer) -> std::result::Result<Track, Error> {
            let operation = "SDL3_mixer.Track.create";
            let raw = mixer.raw(operation)?;
                let track = sys::create_track(raw).map_err(|message| error(operation, ErrorKind::MixerError, &message))?;
                LIVE_TRACKS.fetch_add(1, Ordering::SeqCst);
                mixer.inner.tracks.fetch_add(1, Ordering::SeqCst);
                Ok(Track { raw: track, mixer: Arc::clone(&mixer.inner), destroyed: AtomicBool::new(false) })
        }

        pub fn set_audio(&self, audio: &Audio) -> std::result::Result<(), Error> {
            let operation = "SDL3_mixer.Track.set_audio";
            if audio.destroyed.load(Ordering::SeqCst) {
                Err(error(operation, ErrorKind::Destroyed, "audio handle is destroyed"))
            } else if !Arc::ptr_eq(&self.mixer, &audio.mixer) {
                Err(error(operation, ErrorKind::InvalidArgument, "track and audio belong to different mixers"))
            } else {
                self.mutate(operation, &|raw| sys::set_track_audio(raw, audio.raw))
            }
        }

        pub fn set_gain(&self, gain: f64) -> std::result::Result<(), Error> {
            let operation = "SDL3_mixer.Track.set_gain";
            if !gain.is_finite() || gain < 0.0 {
                Err(error(operation, ErrorKind::InvalidArgument, "track gain must be finite and non-negative"))
            } else {
                self.mutate(operation, &|raw| sys::set_track_gain(raw, gain))
            }
        }

        /// `loops` is -1 (forever) or non-negative; `fade_in_ms` is non-negative.
        pub fn play(&self, loops: i64, fade_in_ms: i64) -> std::result::Result<(), Error> {
            let operation = "SDL3_mixer.Track.play";
            if loops < -1 {
                Err(error(operation, ErrorKind::InvalidArgument, "loop count must be -1 or non-negative"))
            } else if fade_in_ms < 0 {
                Err(error(operation, ErrorKind::InvalidArgument, "fade duration must be non-negative"))
            } else {
                self.mutate(operation, &|raw| sys::play_track(raw, int(loops), int(fade_in_ms)))
            }
        }

        pub fn stop(&self, fade_out_ms: i64) -> std::result::Result<(), Error> {
            let operation = "SDL3_mixer.Track.stop";
            if fade_out_ms < 0 {
                Err(error(operation, ErrorKind::InvalidArgument, "fade duration must be non-negative"))
            } else {
                self.mutate(operation, &|raw| sys::stop_track(raw, int(fade_out_ms)))
            }
        }

        pub fn playing(&self) -> std::result::Result<bool, Error> {
            let raw = self.raw("SDL3_mixer.Track.playing")?;
            Ok(sys::track_playing(raw))
        }

        pub fn destroy(&self) -> std::result::Result<(), Error> {
            enter_main("SDL3_mixer.Track.destroy")?;
                if !self.destroyed.swap(true, Ordering::SeqCst) {
                    LIVE_TRACKS.fetch_sub(1, Ordering::SeqCst);
                    self.mixer.tracks.fetch_sub(1, Ordering::SeqCst);
                    sys::destroy_track(self.raw);
                }
                Ok(())
        }
    }

    impl Drop for Track {
        fn drop(&mut self) {
            if !self.destroyed.swap(true, Ordering::SeqCst) {
                LIVE_TRACKS.fetch_sub(1, Ordering::SeqCst);
                self.mixer.tracks.fetch_sub(1, Ordering::SeqCst);
                TRACK_QUEUE.lock().unwrap().push(self.raw);
            }
        }
    }
}

/// Bounded SDL_mixer-2-style channel allocation implemented with owned SDL3_mixer tracks. Channel
/// and group numbers are stable array indices; no callback or global mutable mixer state is exposed.
pub mod channels {
    use super::audio::Audio;
    use super::mixer::Mixer;
    use super::track::Track;
    use super::*;

    pub type Channel = i64;

    struct Slot {
        track: Track,
        volume: Mutex<f64>,
    }

    pub struct Channels {
        mixer: Mixer,
        slots: Vec<Slot>,
        destroyed: AtomicBool,
    }

    fn operation(name: &str) -> String {
        format!("SDL3_mixer.Channels.{name}")
    }

    impl Channels {
        fn enter(&self, name: &str) -> std::result::Result<(), Error> {
            let op = operation(name);
            if self.destroyed.load(Ordering::SeqCst) {
                Err(error(&op, ErrorKind::Destroyed, "channel pool is destroyed"))
            } else if self.mixer.inner.destroyed.load(Ordering::SeqCst) {
                Err(error(&op, ErrorKind::Destroyed, "channel pool mixer is destroyed"))
            } else {
                enter_main(&op)
            }
        }

        fn slot(&self, name: &str, channel: Channel) -> std::result::Result<&Slot, Error> {
            self.enter(name)?;
            match usize::try_from(channel).ok().and_then(|index| self.slots.get(index)) {
                Some(slot) => Ok(slot),
                None => Err(error(&operation(name), ErrorKind::InvalidArgument, "channel index is out of range")),
            }
        }

        pub fn create(mixer: &Mixer, count: i64) -> std::result::Result<Channels, Error> {
            let op = operation("create");
            if count <= 0 || count > 4096 {
                return Err(error(&op, ErrorKind::InvalidArgument, "channel count must be in 1..4096"));
            }
            let _raw = mixer.raw(&op)?;
                let mut slots: Vec<Slot> = Vec::with_capacity(count as usize);
                for _ in 0..count {
                    match Track::create(mixer) {
                        Ok(track) => slots.push(Slot { track, volume: Mutex::new(1.0) }),
                        Err(failure) => {
                            for slot in &slots {
                                let _ = slot.track.destroy();
                            }
                            return Err(failure);
                        }
                    }
                }
                Ok(Channels { mixer: mixer.clone(), slots, destroyed: AtomicBool::new(false) })
        }

        fn allocate(&self) -> std::result::Result<Channel, Error> {
            self.enter("allocate")?;
                for (index, slot) in self.slots.iter().enumerate() {
                    if !slot.track.playing()? {
                        return Ok(index as Channel);
                    }
                }
                Err(error(&operation("allocate"), ErrorKind::MixerError, "no free channel"))
        }

        pub fn play(&self, channel: Option<Channel>, loops: i64, fade_in_ms: i64, audio: &Audio) -> std::result::Result<Channel, Error> {
            let channel = match channel {
                Some(channel) => channel,
                None => self.allocate()?,
            };
            let selected = self.slot("play", channel)?;
                selected.track.set_audio(audio)?;
                selected.track.set_gain(*selected.volume.lock().unwrap())?;
                selected.track.play(loops, fade_in_ms)?;
                Ok(channel)
        }

        pub fn set_volume(&self, channel: Channel, volume: f64) -> std::result::Result<(), Error> {
            if !volume.is_finite() || volume < 0.0 {
                return Err(error(&operation("set_volume"), ErrorKind::InvalidArgument, "channel volume must be finite and non-negative"));
            }
            let selected = self.slot("set_volume", channel)?;
                selected.track.set_gain(volume)?;
                *selected.volume.lock().unwrap() = volume;
                Ok(())
        }

        pub fn destroy(&self) -> std::result::Result<(), Error> {
            enter_main(&operation("destroy"))?;
                if self.destroyed.load(Ordering::SeqCst) {
                    return Ok(());
                }
                let mut failure = None;
                for slot in &self.slots {
                    if let Err(failed) = slot.track.destroy() {
                        failure.get_or_insert(failed);
                    }
                }
                match failure {
                    Some(failed) => Err(failed),
                    None => {
                        self.destroyed.store(true, Ordering::SeqCst);
                        Ok(())
                    }
                }
        }
    }
}

/// One music-style track with explicit ownership and fade/loop semantics.
pub mod music {
    pub use super::track::Status;
    pub type Music = super::track::Track;
}

#[cfg(test)]
mod tests;

// The test harness never runs a test on the process main thread, so a test claims that role.
#[cfg(not(test))]
fn main_thread() -> bool {
    crate::thread::is_initial_domain() && crate::thread::is_sdl_main_thread()
}

#[cfg(test)]
fn main_thread() -> bool {
    tests::is_main()
}
