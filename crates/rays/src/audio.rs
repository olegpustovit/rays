//! SDL3_mixer-backed audio: a default playback device and decoded samples.
use runtime_resources::Audio as Resource;

pub struct t {
    audio: Resource::t,
}

fn message(operation: &str, error: &runtime_resources::error) -> String {
    format!("{operation}: {}", ogpu_core::error::to_string(error))
}

/// Opens the default playback device.
pub fn init() -> Result<t, String> {
    match Resource::create_device(32) {
        Ok(audio) => Ok(t { audio }),
        Err(error) => Err(message("Audio.init", &error)),
    }
}

pub fn shutdown(value: &t) -> Result<(), String> {
    Resource::destroy(&value.audio).map_err(|e| message("Audio.shutdown", &e))
}

pub mod Sample {
    use super::*;

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum waveform {
        Sine,
        Square,
        Saw,
        Triangle,
    }

    pub struct t {
        resource: Resource::sample,
        destroyed: bool,
        volume: f64,
    }

    pub fn load(audio: &super::t, path: &str) -> Result<t, String> {
        let bytes = std::fs::read(path).map_err(|e| e.to_string())?;
        match Resource::load_sample_bytes(&audio.audio, &bytes) {
            Ok(resource) => Ok(t { resource, destroyed: false, volume: 1. }),
            Err(error) => Err(message("Audio.Sample.load", &error)),
        }
    }

    fn p16(bytes: &mut [u8], o: usize, v: i64) {
        bytes[o] = (v & 255) as u8;
        bytes[o + 1] = ((v >> 8) & 255) as u8;
    }

    fn p32(bytes: &mut [u8], o: usize, v: i64) {
        p16(bytes, o, v);
        p16(bytes, o + 2, v >> 16);
    }

    /// The 16-bit mono WAV file of a synthesized tone.
    pub fn wav(volume: f64, waveform: waveform, frequency: f64, duration: f64) -> Result<Vec<u8>, String> {
        let sample_rate = 48000_i64;
        if frequency <= 0. || duration <= 0. {
            return Err("Audio.Sample.synth: invalid frequency or duration".to_string());
        }
        let frames = std::cmp::max(1, (duration * sample_rate as f64) as i64);
        let mut bytes = vec![0u8; (44 + frames * 2) as usize];
        bytes[0..4].copy_from_slice(b"RIFF");
        p32(&mut bytes, 4, 36 + frames * 2);
        bytes[8..16].copy_from_slice(b"WAVEfmt ");
        p32(&mut bytes, 16, 16);
        p16(&mut bytes, 20, 1);
        p16(&mut bytes, 22, 1);
        p32(&mut bytes, 24, sample_rate);
        p32(&mut bytes, 28, sample_rate * 2);
        p16(&mut bytes, 32, 2);
        p16(&mut bytes, 34, 16);
        bytes[36..40].copy_from_slice(b"data");
        p32(&mut bytes, 40, frames * 2);
        let pi = 4. * 1.0_f64.atan();
        for i in 0..frames {
            let phase = frequency * i as f64 / sample_rate as f64;
            let x = match waveform {
                waveform::Sine => (2. * pi * phase).sin(),
                waveform::Square => {
                    if (2. * pi * phase).sin() >= 0. {
                        1.
                    } else {
                        -1.
                    }
                }
                waveform::Saw => 2. * (phase - (phase + 0.5).floor()),
                waveform::Triangle => (2. * (2. * (phase - (phase + 0.5).floor())).abs()) - 1.,
            };
            let scaled = (-1.0_f64).max(1.0_f64.min(volume * x));
            p16(&mut bytes, (44 + i * 2) as usize, ((scaled * 32767.) as i64) & 0xffff);
        }
        Ok(bytes)
    }

    pub fn synth(audio: &super::t, volume: Option<f64>, waveform: waveform, frequency: f64, duration: f64) -> Result<t, String> {
        let bytes = wav(volume.unwrap_or(1.), waveform, frequency, duration)?;
        match Resource::load_sample_bytes(&audio.audio, &bytes) {
            Ok(resource) => Ok(t { resource, destroyed: false, volume: 1. }),
            Err(error) => Err(message("Audio.Sample.synth", &error)),
        }
    }

    pub fn play(audio: &super::t, loops: Option<i64>, volume: Option<f64>, sample: &t) -> Result<i64, String> {
        Resource::play_sample(&audio.audio, loops, Some(volume.unwrap_or(sample.volume)), &sample.resource)
            .map_err(|e| message("Audio.Sample.play", &e))
    }

    pub fn destroy(sample: &mut t) {
        if !sample.destroyed {
            let _ = Resource::destroy_sample(&mut sample.resource);
            sample.destroyed = true;
        }
    }
}
