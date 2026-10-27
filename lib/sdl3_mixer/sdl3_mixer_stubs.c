#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <limits.h>

#include <SDL3/SDL.h>
#include <SDL3_mixer/SDL_mixer.h>

#include "generated_abi.h"

static MIX_Mixer *mixer_of_value(value raw)
{
  return (MIX_Mixer *)(intnat)Nativeint_val(raw);
}

static MIX_Audio *audio_of_value(value raw)
{
  return (MIX_Audio *)(intnat)Nativeint_val(raw);
}

static MIX_Track *track_of_value(value raw)
{
  return (MIX_Track *)(intnat)Nativeint_val(raw);
}

static value string_error(void)
{
  const char *message = SDL_GetError();
  CAMLparam0();
  CAMLlocal2(copy, result);
  copy = caml_copy_string(
      message != NULL && message[0] != '\0' ? message : "SDL3_mixer call failed");
  result = caml_alloc(1, 1);
  Store_field(result, 0, copy);
  CAMLreturn(result);
}

static value unit_success(void)
{
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_alloc(1, 0);
  Store_field(result, 0, Val_unit);
  CAMLreturn(result);
}

static value native_success(void *pointer)
{
  CAMLparam0();
  CAMLlocal2(raw, result);
  raw = caml_copy_nativeint((intnat)pointer);
  result = caml_alloc(1, 0);
  Store_field(result, 0, raw);
  CAMLreturn(result);
}

static value int_success(int number)
{
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_alloc(1, 0);
  Store_field(result, 0, Val_int(number));
  CAMLreturn(result);
}

CAMLprim value caml_sdl3_mixer_version(value unit)
{
  (void)unit;
  return Val_int(MIX_Version());
}

CAMLprim value caml_sdl3_mixer_compiled_version(value unit)
{
  (void)unit;
  return Val_int(SDL_MIXER_VERSION);
}

CAMLprim value caml_sdl3_mixer_init(value unit)
{
  (void)unit;
  if (!MIX_Init()) {
    return string_error();
  }
  return unit_success();
}

CAMLprim value caml_sdl3_mixer_quit(value unit)
{
  (void)unit;
  MIX_Quit();
  return Val_unit;
}

CAMLprim value caml_sdl3_mixer_create_device(value unit)
{
  MIX_Mixer *mixer;
  (void)unit;
  mixer = MIX_CreateMixerDevice(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, NULL);
  return mixer != NULL ? native_success(mixer) : string_error();
}

CAMLprim value caml_sdl3_mixer_create_memory(value sample_rate, value channels)
{
  SDL_AudioSpec spec;
  MIX_Mixer *mixer;
  CAMLparam2(sample_rate, channels);
  SDL_zero(spec);
  spec.format = SDL_AUDIO_F32;
  spec.channels = Int_val(channels);
  spec.freq = Int_val(sample_rate);
  mixer = MIX_CreateMixer(&spec);
  if (mixer == NULL) {
    CAMLreturn(string_error());
  }
  CAMLreturn(native_success(mixer));
}

CAMLprim value caml_sdl3_mixer_destroy_mixer(value raw)
{
  MIX_DestroyMixer(mixer_of_value(raw));
  return Val_unit;
}

CAMLprim value caml_sdl3_mixer_format(value raw)
{
  SDL_AudioSpec spec;
  CAMLparam1(raw);
  CAMLlocal3(pair, result, failure);
  SDL_zero(spec);
  if (!MIX_GetMixerFormat(mixer_of_value(raw), &spec)) {
    failure = string_error();
    CAMLreturn(failure);
  }
  pair = caml_alloc_tuple(2);
  Store_field(pair, 0, Val_int(spec.freq));
  Store_field(pair, 1, Val_int(spec.channels));
  result = caml_alloc(1, 0);
  Store_field(result, 0, pair);
  CAMLreturn(result);
}

CAMLprim value caml_sdl3_mixer_generate(value raw, value bytes)
{
  mlsize_t length;
  int mixed;
  CAMLparam2(raw, bytes);
  length = caml_string_length(bytes);
  if (length > INT_MAX) {
    SDL_SetError("PCM output buffer exceeds MIX_Generate's signed length");
    CAMLreturn(string_error());
  }
  mixed = MIX_Generate(mixer_of_value(raw), Bytes_val(bytes), (int)length);
  if (mixed < 0) {
    CAMLreturn(string_error());
  }
  CAMLreturn(int_success(mixed));
}

CAMLprim value caml_sdl3_mixer_load_bytes(value raw_mixer, value bytes)
{
  SDL_IOStream *stream;
  MIX_Audio *audio;
  CAMLparam2(raw_mixer, bytes);
  stream = SDL_IOFromConstMem(Bytes_val(bytes), caml_string_length(bytes));
  if (stream == NULL) {
    CAMLreturn(string_error());
  }
  audio = MIX_LoadAudio_IO(mixer_of_value(raw_mixer), stream, true, true);
  if (audio == NULL) {
    CAMLreturn(string_error());
  }
  CAMLreturn(native_success(audio));
}

CAMLprim value caml_sdl3_mixer_destroy_audio(value raw)
{
  MIX_DestroyAudio(audio_of_value(raw));
  return Val_unit;
}

CAMLprim value caml_sdl3_mixer_create_track(value raw_mixer)
{
  MIX_Track *track;
  CAMLparam1(raw_mixer);
  track = MIX_CreateTrack(mixer_of_value(raw_mixer));
  if (track == NULL) {
    CAMLreturn(string_error());
  }
  CAMLreturn(native_success(track));
}

CAMLprim value caml_sdl3_mixer_destroy_track(value raw)
{
  MIX_DestroyTrack(track_of_value(raw));
  return Val_unit;
}

CAMLprim value caml_sdl3_mixer_set_track_audio(value raw_track, value raw_audio)
{
  CAMLparam2(raw_track, raw_audio);
  if (!MIX_SetTrackAudio(track_of_value(raw_track), audio_of_value(raw_audio))) {
    CAMLreturn(string_error());
  }
  CAMLreturn(unit_success());
}

CAMLprim value caml_sdl3_mixer_set_track_gain(value raw, value gain)
{
  CAMLparam2(raw, gain);
  if (!MIX_SetTrackGain(track_of_value(raw), Double_val(gain))) {
    CAMLreturn(string_error());
  }
  CAMLreturn(unit_success());
}

CAMLprim value caml_sdl3_mixer_play_track(
    value raw, value loops, value fade_in_ms)
{
  SDL_PropertiesID properties;
  bool success;
  CAMLparam3(raw, loops, fade_in_ms);
  CAMLlocal1(failure);
  properties = SDL_CreateProperties();
  if (properties == 0) {
    CAMLreturn(string_error());
  }
  success = SDL_SetNumberProperty(properties, MIX_PROP_PLAY_LOOPS_NUMBER,
      Int_val(loops));
  if (success && Int_val(fade_in_ms) > 0) {
    success = SDL_SetNumberProperty(properties,
        MIX_PROP_PLAY_FADE_IN_MILLISECONDS_NUMBER, Int_val(fade_in_ms));
  }
  if (success) {
    success = MIX_PlayTrack(track_of_value(raw), properties);
  }
  if (!success) {
    failure = string_error();
    SDL_DestroyProperties(properties);
    CAMLreturn(failure);
  }
  SDL_DestroyProperties(properties);
  CAMLreturn(unit_success());
}

CAMLprim value caml_sdl3_mixer_stop_track(value raw, value fade_out_ms)
{
  Sint64 frames;
  CAMLparam2(raw, fade_out_ms);
  frames = MIX_TrackMSToFrames(track_of_value(raw), Int_val(fade_out_ms));
  if (frames < 0) {
    CAMLreturn(string_error());
  }
  if (!MIX_StopTrack(track_of_value(raw), frames)) {
    CAMLreturn(string_error());
  }
  CAMLreturn(unit_success());
}

CAMLprim value caml_sdl3_mixer_track_playing(value raw)
{
  return Val_bool(MIX_TrackPlaying(track_of_value(raw)));
}
