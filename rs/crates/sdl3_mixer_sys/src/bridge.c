#include <limits.h>
#include <stdbool.h>
#include <stdint.h>

#include <SDL3/SDL.h>
#include <SDL3_mixer/SDL_mixer.h>

#include "floor.h"

_Static_assert(SDL_MIXER_VERSION >= SDL_VERSIONNUM(RAYS_FLOOR_MAJOR, RAYS_FLOOR_MINOR, RAYS_FLOOR_PATCH),
    "SDL3_mixer headers are below packaging/sdl3.lock floor");

const char *rays_mix_get_error(void) { return SDL_GetError(); }

int rays_mixer_version(void) { return MIX_Version(); }
int rays_mixer_compiled_version(void) { return SDL_MIXER_VERSION; }
bool rays_mixer_init(void) { return MIX_Init(); }
void rays_mixer_quit(void) { MIX_Quit(); }
void *rays_mixer_create_device(void) { return MIX_CreateMixerDevice(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, NULL); }

void *rays_mixer_create_memory(int sample_rate, int channels)
{
  SDL_AudioSpec spec;
  SDL_zero(spec);
  spec.format = SDL_AUDIO_F32;
  spec.channels = channels;
  spec.freq = sample_rate;
  return MIX_CreateMixer(&spec);
}

void rays_mixer_destroy_mixer(void *mixer) { MIX_DestroyMixer((MIX_Mixer *)mixer); }

bool rays_mixer_format(void *mixer, int *out)
{
  SDL_AudioSpec spec;
  SDL_zero(spec);
  if (!MIX_GetMixerFormat((MIX_Mixer *)mixer, &spec)) return false;
  out[0] = spec.freq;
  out[1] = spec.channels;
  return true;
}

/* Returns the mixed byte count, or -1 with SDL's error set. */
int rays_mixer_generate(void *mixer, uint8_t *buffer, size_t length)
{
  if (length > INT_MAX) {
    SDL_SetError("PCM output buffer exceeds the signed length of MIX_Generate");
    return -1;
  }
  return MIX_Generate((MIX_Mixer *)mixer, buffer, (int)length);
}

void *rays_mixer_load_bytes(void *mixer, const void *data, size_t length)
{
  SDL_IOStream *stream = SDL_IOFromConstMem(data, length);
  if (stream == NULL) return NULL;
  return MIX_LoadAudio_IO((MIX_Mixer *)mixer, stream, true, true);
}

void rays_mixer_destroy_audio(void *audio) { MIX_DestroyAudio((MIX_Audio *)audio); }
void *rays_mixer_create_track(void *mixer) { return MIX_CreateTrack((MIX_Mixer *)mixer); }
void rays_mixer_destroy_track(void *track) { MIX_DestroyTrack((MIX_Track *)track); }
bool rays_mixer_set_track_audio(void *track, void *audio) { return MIX_SetTrackAudio((MIX_Track *)track, (MIX_Audio *)audio); }
bool rays_mixer_set_track_gain(void *track, double gain) { return MIX_SetTrackGain((MIX_Track *)track, (float)gain); }

bool rays_mixer_play_track(void *track, int loops, int fade_in_ms)
{
  bool success;
  SDL_PropertiesID properties = SDL_CreateProperties();
  if (properties == 0) return false;
  success = SDL_SetNumberProperty(properties, MIX_PROP_PLAY_LOOPS_NUMBER, loops);
  if (success && fade_in_ms > 0) {
    success = SDL_SetNumberProperty(properties, MIX_PROP_PLAY_FADE_IN_MILLISECONDS_NUMBER, fade_in_ms);
  }
  if (success) success = MIX_PlayTrack((MIX_Track *)track, properties);
  SDL_DestroyProperties(properties);
  return success;
}

bool rays_mixer_stop_track(void *track, int fade_out_ms)
{
  Sint64 frames = MIX_TrackMSToFrames((MIX_Track *)track, fade_out_ms);
  if (frames < 0) return false;
  return MIX_StopTrack((MIX_Track *)track, frames);
}

bool rays_mixer_track_playing(void *track) { return MIX_TrackPlaying((MIX_Track *)track); }
