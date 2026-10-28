(** Immutable layout data shared by the Metal and WGSL instruction printers. *)
type t = {
  view : Packed.Private.view;
  used : bool array;
  finite_checks : bool array;
      (** per-instruction finite checks: a fast kernel checks only its inputs, and its outputs *)
  output_checks : bool;
      (** fast: the approximate tier guarantees valid outputs (specification/gpu-codegen.md) *)
  narrow : bool array;
      (** fast: Int registers printed as 32-bit unsigned words, with the CPU's bits: each always lies
          in [0, 2^32) or is read only through its low 32 bits ([Loop_result] follows its carry) *)
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
  scene_instances : int option;
      (** an instanced scene (a static fact): the instance table's buffer index, after the spheres *)
  scene_motion : bool;
      (** a moving scene (a static fact): the query's time selects instance transforms (instanced) or vertex
          keyframes (flat) *)
  scene_vertices_end : int option;  (** flat motion: the shutter-end triangle vertices' buffer index *)
  scene_strands : int option;
      (** a stranded scene (a static fact): the strand segments' buffer index, after every other scene buffer *)
  scene_strand_count : int option;  (** uniform word holding the strand segment count *)
}

(** [fast] includes gather lengths and the count record; [image] reserves the two
    dimensions used by WGSL image kernels and has no input bindings. *)
val plan : ?fast:bool -> ?image:bool -> ?instanced:bool -> ?motion:bool -> ?curves:bool -> Packed.t -> t
val uniform_words : t -> int

val iter : t -> instruction:(int -> Packed.instruction -> unit) ->
  loop:(int -> Packed.loop -> next:int array -> stop:int option -> body:(unit -> unit) -> unit) -> unit
(** Visit validated control scopes in authored order. The loop printer owns its
    carried declarations, simultaneous update and optional early exit; [body]
    visits only its body. *)

(** A printed scope (the kernel body or one loop body) of the text printers: its registers are
    declared first ([decls]), then its code runs, where consecutive instructions under the same
    guards share one nest of blocks ([opened], outermost first) instead of testing their
    conditions instruction by instruction. *)
module Scope : sig
  type t = { id : int; decls : Buffer.t; code : Buffer.t; mutable opened : (int * bool) list;
             mutable blocks : int list; outer : int list; next : int ref }
  (** [blocks] numbers the open blocks of [opened] (from [next], shared by a kernel's scopes); [outer] is
      the block stack around a loop body. *)

  val create : ?outer:int list -> ?next:int ref -> int -> t
  val indent : t -> string
  val stack : t -> int list
  (** The blocks around the scope's current position, outermost first: a local declared there is
      visible wherever the stack extends this one. *)

  val enter : t -> opening:(int -> bool -> string) -> (int * bool) list -> unit
  (** Close the blocks [guards] leave and open, with [opening condition selected], the ones it adds. *)
end
