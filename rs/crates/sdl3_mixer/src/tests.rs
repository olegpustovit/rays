use crate::audio::Audio;
use crate::channels::Channels;
use crate::mixer::Mixer;
use crate::music::Music;
use crate::{ErrorKind, check_version, compiled_version, init, linked_version};

fn get<T>(result: crate::Result<T>) -> T {
    result.unwrap_or_else(|error| panic!("{error}"))
}

fn kind<T>(result: crate::Result<T>) -> Option<ErrorKind> {
    result.err().map(|error| error.kind())
}

/// Reads `(floor ...)` for `key` from packaging/sdl3.lock.
fn lock_floor(key: &str) -> (i32, i32, i32) {
    let path = format!("{}/../../../packaging/sdl3.lock", env!("CARGO_MANIFEST_DIR"));
    let text = std::fs::read_to_string(path).expect("read sdl3.lock");
    let line = text
        .lines()
        .map(|line| line.trim().trim_start_matches('('))
        .find(|line| line.starts_with(&format!("{key} ")))
        .expect("lock entry");
    let floor = line.split("(floor ").nth(1).and_then(|rest| rest.split(')').next()).expect("floor");
    let parts: Vec<i32> = floor.split('.').map(|part| part.parse().unwrap()).collect();
    (parts[0], parts[1], parts[2])
}

fn put(bytes: &mut [u8], offset: usize, value: u32, width: usize) {
    bytes[offset..offset + width].copy_from_slice(&value.to_le_bytes()[..width]);
}

fn tiny_wav() -> Vec<u8> {
    let samples = 80usize;
    let data_bytes = samples * 2;
    let mut bytes = vec![0u8; 44 + data_bytes];
    bytes[0..4].copy_from_slice(b"RIFF");
    put(&mut bytes, 4, (36 + data_bytes) as u32, 4);
    bytes[8..16].copy_from_slice(b"WAVEfmt ");
    put(&mut bytes, 16, 16, 4);
    put(&mut bytes, 20, 1, 2);
    put(&mut bytes, 22, 1, 2);
    put(&mut bytes, 24, 8_000, 4);
    put(&mut bytes, 28, 16_000, 4);
    put(&mut bytes, 32, 2, 2);
    put(&mut bytes, 34, 16, 2);
    bytes[36..40].copy_from_slice(b"data");
    put(&mut bytes, 40, data_bytes as u32, 4);
    for sample in 0..samples {
        let value: i32 = if sample % 16 < 8 { 8_000 } else { -8_000 };
        put(&mut bytes, 44 + sample * 2, value as u32, 2);
    }
    bytes
}

#[test]
fn track_device_and_memory_conformance() {
    claim_main();
    if std::env::var("SDL_AUDIODRIVER").ok().as_deref() != Some("dummy") {
        return;
    }
    let triple = |v: sdl3::Version| (v.major, v.minor, v.patch);
    let floor = lock_floor("sdl3_mixer");
    assert!(triple(compiled_version()) >= floor && triple(linked_version()) >= floor, "installed SDL3_mixer is below the lock floor");
    get(check_version(true));
    assert_eq!(kind(Mixer::create_memory(48_000, 2)), Some(ErrorKind::NotInitialized), "pre-init mixer");
    assert_eq!(kind(std::thread::spawn(init::init).join().unwrap()), Some(ErrorKind::WrongDomain));
    get(init::init());
    assert_eq!(kind(Mixer::create_memory(7_999, 2)), Some(ErrorKind::InvalidArgument), "invalid sample rate");
    assert_eq!(kind(Mixer::create_memory(48_000, 0)), Some(ErrorKind::InvalidArgument), "invalid channels");

    let mixer = get(Mixer::create_memory(48_000, 2));

    let mut encoded = tiny_wav();
    let audio = get(Audio::load_bytes(&mixer, &encoded));
    // the audio owns a copy: scribbling on the source changes nothing
    encoded.fill(0);
    assert_eq!(kind(Audio::load_bytes(&mixer, &[])), Some(ErrorKind::InvalidArgument), "empty audio bytes");
    match Audio::load_bytes(&mixer, b"bad audio") {
        Err(captured) if captured.kind() == ErrorKind::MixerError && !captured.message().is_empty() => {
            let original = captured.message().to_owned();
            let _ = linked_version();
            assert_eq!(captured.message(), original, "mixer error text changed after a subsequent native call");
        }
        _ => panic!("malformed audio returned the wrong result"),
    }
    let track = get(Music::create(&mixer));
    get(track.set_audio(&audio));
    get(track.play(1, 5));
    let generated = get(mixer.generate(256));
    assert!(
        generated.pcm_f32.len() == 256 * 2 * 4 && generated.mixed_bytes > 0 && generated.pcm_f32.iter().any(|&byte| byte != 0),
        "memory mixer did not generate finite non-silent float32 PCM"
    );
    get(track.stop(2));

    let channels = get(Channels::create(&mixer, 32));
    let selected = get(channels.play(None, 1, 1, &audio));
    get(channels.set_volume(selected, 0.4));
    assert_eq!(kind(channels.set_volume(0, f64::NAN)), Some(ErrorKind::InvalidArgument), "NaN channel volume");
    let misuse = std::thread::scope(|scope| scope.spawn(|| channels.play(None, 0, 0, &audio)).join().unwrap());
    assert_eq!(kind(misuse), Some(ErrorKind::WrongDomain), "concurrent channel misuse");
    for index in 0..32 {
        get(channels.play(Some(index), index & 1, 0, &audio));
    }
    assert_eq!(kind(channels.play(None, 0, 0, &audio)), Some(ErrorKind::MixerError), "full channel bank");
    get(channels.destroy());
    get(channels.destroy());

    for _ in 0..1_000 {
        let transient = get(Music::create(&mixer));
        get(transient.destroy());
    }

    let other = get(Mixer::create_memory(48_000, 2));
    let foreign = get(Music::create(&other));
    assert_eq!(kind(foreign.set_audio(&audio)), Some(ErrorKind::InvalidArgument), "cross-mixer audio attachment");
    get(foreign.destroy());
    get(other.destroy());

    assert_eq!(kind(mixer.destroy()), Some(ErrorKind::ParentHasDependents), "mixer teardown ignored child handles");
    let wrong = std::thread::scope(|scope| scope.spawn(|| mixer.generate(1)).join().unwrap());
    assert_eq!(kind(wrong), Some(ErrorKind::WrongDomain), "wrong-domain mixer access");
    get(track.destroy());
    get(track.destroy());
    get(audio.destroy());
    assert_eq!(kind(track.stop(0)), Some(ErrorKind::Destroyed), "stale track access");
    get(mixer.destroy());
    get(mixer.destroy());
    assert_eq!(kind(Music::create(&mixer)), Some(ErrorKind::Destroyed), "track used destroyed mixer");

    let device = get(Mixer::create_device());
    let device_audio = get(Audio::load_bytes(&device, &tiny_wav()));
    let device_track = get(Music::create(&device));
    get(device_track.set_audio(&device_audio));
    get(device_track.play(0, 0));
    get(device_track.stop(0));
    get(device_track.destroy());
    get(device_audio.destroy());
    get(device.destroy());

    get(init::quit());
    get(init::quit());
}

/// Runs the body of `typed_device_failure` in a child process that names a missing audio driver.
#[test]
fn typed_device_failure_with_missing_audio_driver() {
    if std::env::var("RAYS_MIXER_DEVICE_FAILURE_CHILD").is_ok() {
        return;
    }
    let status = std::process::Command::new(std::env::current_exe().unwrap())
        .args(["--exact", "tests::typed_device_failure", "--test-threads=1"])
        .env("RAYS_MIXER_DEVICE_FAILURE_CHILD", "1")
        .env("SDL_AUDIODRIVER", "rays_missing_audio_driver")
        .status()
        .unwrap();
    assert!(status.success(), "device-failure child failed");
}

#[test]
fn typed_device_failure() {
    if std::env::var("RAYS_MIXER_DEVICE_FAILURE_CHILD").is_err() {
        return;
    }
    claim_main();
    get(init::init());
    match Mixer::create_device() {
        Err(error) if error.kind() == ErrorKind::MixerError && !error.message().is_empty() => {}
        Ok(mixer) => {
            get(mixer.destroy());
            panic!("invalid SDL audio driver unexpectedly opened a device mixer");
        }
        Err(_) => panic!("device failure returned the wrong typed error"),
    }
    get(init::quit());
}

static MAIN: std::sync::OnceLock<std::thread::ThreadId> = std::sync::OnceLock::new();

/// The calling test thread becomes the thread the library treats as the SDL main thread.
fn claim_main() {
    MAIN.get_or_init(|| std::thread::current().id());
}

pub(crate) fn is_main() -> bool {
    MAIN.get() == Some(&std::thread::current().id())
}
