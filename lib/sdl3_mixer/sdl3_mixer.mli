(** Ownership-aware binding to SDL3_mixer's mixer/audio/track model. *)

type error_kind =
  | Mixer_error
  | Wrong_domain
  | Destroyed
  | Parent_has_dependents
  | Invalid_argument
  | Incompatible_version
  | Not_initialized
  | Handles_still_open

type error = private {
  operation : string;
  kind : error_kind;
  message : string;
}

val pp_error : Format.formatter -> error -> unit

(** The version macro of the headers compiled against, and the library
    linked; both are checked against packaging/sdl3.lock by the tests. *)
val compiled_version : Sdl3.version
val linked_version : unit -> Sdl3.version
val check_version : ?release:bool -> unit -> (unit, error) result

module Init : sig
  val init : unit -> (unit, error) result
  val quit : unit -> (unit, error) result
end

module Mixer : sig
  type t
  type mode = Device | Memory
  type format = { sample_rate : int; channels : int }
  type generated = private { mixed_bytes : int; pcm_f32 : bytes }

  val create_device : unit -> (t, error) result
  val create_memory : sample_rate:int -> channels:int -> (t, error) result

  (** Generate exactly [frames] of interleaved native-endian float32 PCM from
      a memory mixer. [mixed_bytes] excludes silence appended after tracks
      finish, while [pcm_f32] always has the requested length. *)
  val generate : t -> frames:int -> (generated, error) result

  val destroy : t -> (unit, error) result
end

module Audio : sig
  type t

  val load_bytes : Mixer.t -> bytes -> (t, error) result
  val destroy : t -> (unit, error) result
end

(** Bounded SDL_mixer-2-style channel allocation implemented with owned
    SDL3_mixer tracks. Channel and group numbers are stable array indices;
    no callback or global mutable mixer state is exposed. *)
module Channels : sig
  type t
  type channel = int

  val create : Mixer.t -> count:int -> (t, error) result
  val play :
    t -> ?channel:channel -> ?loops:int -> ?fade_in_ms:int -> Audio.t ->
    (channel, error) result
  val set_volume : t -> channel -> float -> (unit, error) result
  val destroy : t -> (unit, error) result
end

(** One music-style track with explicit ownership and fade/loop semantics. *)
module Music : sig
  type t
  type status = Stopped | Playing | Paused
  val create : Mixer.t -> (t, error) result
  val set_audio : t -> Audio.t -> (unit, error) result
  val play : t -> ?loops:int -> ?fade_in_ms:int -> unit -> (unit, error) result
  val stop : t -> ?fade_out_ms:int -> unit -> (unit, error) result
  val destroy : t -> (unit, error) result
end
