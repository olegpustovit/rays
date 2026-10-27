(** Immutable layout data shared by the Metal and WGSL instruction printers. *)
type t = {
  view : Packed.Private.view;
  used : bool array;
  finite_checks : bool array;
  uniforms : int array;
  frames : int array;
  image_width : int option;
  image_height : int option;
  table_seeds : int array;
  gathers : (int * (int * int)) list;
  gather_lengths : int array;
  input_used : bool array;
  input_count : int;
  output_count : int;
  uniform_layout : (string * int * int) list;
  uniform_bytes : int;
  uniform_binding : int;
  status_binding : int;
  table_binding : int option;
  gather_base : int;
  count_base : int;
  counts : int option;
  scene_binding : int option;
      (** [scene/trace]: the acceleration structure's buffer index; the triangle vertices follow. *)
  scene_spheres : int option;  (** uniform word holding the procedural sphere count *)
}

(** [fast] includes gather lengths and the count record; [image] reserves the two
    dimensions used by WGSL image kernels and has no input bindings. *)
val plan : ?fast:bool -> ?image:bool -> Packed.t -> t
val uniform_words : t -> int

val iter : t -> instruction:(int -> Packed.instruction -> unit) ->
  loop:(int -> Packed.loop -> next:int array -> stop:int option -> body:(unit -> unit) -> unit) -> unit
(** Visit validated control scopes in authored order. The loop printer owns its
    carried declarations, simultaneous update and optional early exit; [body]
    visits only its body. *)
