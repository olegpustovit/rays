(** Immutable CPU-readable textures for 3D mesh sampling. *)

type t
type filter = Nearest | Bilinear | Trilinear
type wrap = Clamp | Repeat | Mirror

val init : width:int -> height:int -> (x:int -> y:int -> Rays_math.Color.t) -> t
val size : t -> int * int
val pixels : t -> Rays_math.Color.t list
val generate_mipmaps : t -> t
val subsection : x:int -> y:int -> width:int -> height:int -> t -> (t, string) result

val sample : t -> u:float -> v:float -> Rays_math.Color.t
(** Sample normalized texture coordinates. [(0, 0)] is the upper-left texel. *)

val of_image : Runtime_resources.Image.t -> (t, string) result

(* Borrow an existing GPU image without CPU storage or texture ownership.
      Identity is stable; dimensions follow publication/resize. CPU pixel,
      sampling and mipmap operations require an explicit immutable snapshot.
      Creation validates the live GPU source on the initial domain. *)
(* Zero-copy construction for fresh immutable pixel arrays. The supplied
      array becomes backing storage and must not be mutated afterward. *)
val create_owned : width:int -> height:int -> Rays_math.Color.t array -> (t, string) result

module Private : sig
  val image : t -> Runtime_resources.Image.t option

  (* Allocation-free packed RGBA sampling for renderer-independent image
     processing and deterministic texture queries. *)
  val levels : t -> (int * int * Rays_math.Color.t array) array

  val identity : t -> int
  (** Process-local identity: equal only for the same immutable texture value, so renderers can key
      uploads without hashing pixels. *)
end
