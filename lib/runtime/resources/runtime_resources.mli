(** Target-neutral owned resource snapshots for the SDL3 migration. *)

type error = Ogpu.Error.t
val pp_error : Format.formatter -> error -> unit

val ensure_directory : string -> (unit, string) result
(** Create a directory and its parents, or return the filesystem error. *)

val write_atomic : filename:string -> (string -> (unit, string) result) -> (unit, string) result
(** Let a writer fill a temporary file beside [filename], then sync and atomically replace it. *)

val write_text : filename:string -> string -> (unit, string) result
(** Write text through a synced temporary file and atomic rename. *)

module Image : sig
  type t
  val create : width:int -> height:int -> rgba:bytes -> (t,error) result
  val load_file : string -> (t,error) result
  val identity : t -> int
  val generation : t -> int
  val destroyed : t -> bool
  val size : t -> ((int*int),error) result
  val pixels : t -> (bytes,error) result
  module Private : sig
    val of_gpu : width:int -> height:int -> source:(unit -> Ogpu.Backend.texture option) -> (t,error) result
    val replace_gpu_source : t -> width:int -> height:int ->
      source:(unit -> Ogpu.Backend.texture option) -> (unit,error) result
    (** Publish a borrowed, generation-validated RGBA8 source without CPU pixel
        storage or readback. Replacement preserves identity and may resize.
        The callback must return the published texture until invalidated;
        substitution or expiration is an error, never a CPU fallback.
        Destroying the image does not destroy its source texture. *)

    val replace_gpu : t -> Ogpu.Backend.texture -> (unit,error) result
    (** Same-shape publication for owners that already manage raw texture lifetime. *)

    val gpu_snapshot : t -> ((int * int * int * Ogpu.Backend.texture) option,error) result
    val readbacks : t -> int
    (** Successful GPU pixel reads through this image; publication does not read. *)

    val cpu_storage_bytes : t -> int
    (** Current CPU bank and spare, excluding separately leased snapshots. *)

    type lease
    (* The bytes remain stable until the lease is released, including across
       image destruction. Mutations use a second bounded buffer rather than
       changing leased storage; release is idempotent. *)
    val borrow_snapshot : t ->
      ((int * int * int * bytes * lease),error) result
    val release_snapshot : lease -> unit
  end
  val replace : t -> width:int -> height:int -> rgba:bytes -> (unit,error) result
  (* Atomically transfer the source's owned pixel storage. On success the
     source is destroyed and the target retains its identity. A source with an
     active private snapshot lease is rejected without mutation. *)
  val replace_owned : t -> t -> (unit,error) result
  val destroy : t -> (unit,error) result
end

module Canvas : sig
  type t
  val create : width:int -> height:int -> (t,error) result
  (** Adopt a packed RGBA byte buffer without copying it. The caller must not
      read or mutate [rgba] after a successful call. *)
  val of_rgba_bytes : width:int -> height:int -> rgba:bytes -> (t,error) result
  val generation : t -> int
  val destroyed : t -> bool
  val size : t -> ((int*int),error) result
  val set_pixel : t -> x:int -> y:int -> int32 -> (unit,error) result
  (* Atomically replace [image] with the current canvas pixels while retaining
     the image identity. Equal extents reuse the image's owned storage. *)
  val snapshot : t -> ((int * int * int * bytes),error) result
  val capture : t -> (Image.t,error) result
  val save_png : t -> string -> (unit,error) result
  module Private : sig
    (** [publish_gpu] adopts the canvas execution's completed frame texture
        (borrowed; the execution owns it) as the authoritative pixels and
        bumps the generation. CPU readers read it back lazily; CPU writers
        forget it. *)
    val publish_gpu : t -> Ogpu.Backend.texture -> (unit,error) result
    val gpu_snapshot : t -> (int * int * int * Ogpu.Backend.texture) option
    val pixel_stats : t -> int * int
    (** Successful captures and actual successful GPU pixel readbacks. *)

    val identity : t -> int
  end
  val destroy : t -> (unit,error) result
end

module Text : sig
  type t
  val generation : t -> int
  val destroyed : t -> bool
  val size : t -> ((int*int),error) result
  val pixels : t -> (bytes,error) result
  val destroy : t -> (unit,error) result
  module Private : sig
    val identity : t -> int
    (* The owned RGBA storage, borrowed without a copy. Do not mutate or retain
       it: [into_image] may later hand it to an image. *)
    val borrow_pixels : t -> (bytes,error) result
    (* Consume the text and transfer its owned RGBA storage to an image. *)
    val into_image : t -> (Image.t,error) result
  end
end

module Font : sig
  type t
  type hinting = Normal_hinting | Light_hinting | Mono_hinting
    | None_hinting | Light_subpixel_hinting
  type glyph_metrics = {     advance:int }
  type alignment = Left | Center | Right
  type metrics = {  ascent:int;  }
  val open_file : path:string -> size:float -> (t,error) result
  val open_system : size:float -> (t,error) result
  val set_hinting : t -> hinting -> (unit,error) result
  val metrics : t -> (metrics,error) result
  val glyph_metrics_at : t -> density:int -> int -> (glyph_metrics,error) result
  val render : t -> ?wrap_width:int -> ?align:alignment -> density:int ->
    color:int*int*int*int -> string -> (Text.t option,error) result
  val destroy : t -> (unit,error) result
end

module Audio : sig
  type t
  type sample
  (* Mixes into memory only; drive it with [generate]. *)
  val create_device : max_channels:int -> (t,error) result
  (* Mixes to the default playback device. *)
  val load_sample_bytes : t -> bytes -> (sample,error) result
  val play_sample : t -> ?loops:int -> ?volume:float -> sample -> (int,error) result
  val destroy_sample : sample -> (unit,error) result
  val destroy : t -> (unit,error) result
end

module Assets : sig
  type t
  val create : unit -> t
  (* Register an owner hook and return the same borrowed value. *)
  val destroy : t -> (unit,error) result
end
