(** The scene's objects as plain kinds ([scene/geometry], [scene/camera], [scene/light],
    [scene/root]): typed schemas, and the reads and edits of a {!Kind_graph.t} of them. *)

module Geometry :
  sig
    type parameters = {
      translate_x : float;
      translate_y : float;
      translate_z : float;
      rotate_x : float;
      rotate_y : float;
      rotate_z : float;
      scale_x : float;
      scale_y : float;
      scale_z : float;
      visible : bool;
      render : bool;
    }
    val parameters_default : parameters
    val parameters_schema : parameters Param.schema
    val kind : Kind_graph.Kind.t
  end
module Camera :
  sig
    type parameters = {
      fov : float;
      near : float;
      far : float;
      aperture : float;
      focus_distance : float;
      eye_x : float;
      eye_y : float;
      eye_z : float;
      target_x : float;
      target_y : float;
      target_z : float;
      up_x : float;
      up_y : float;
      up_z : float;
      follow_viewport : bool;
    }
    val parameters_default : parameters
    val parameters_schema : parameters Param.schema
    val kind : Kind_graph.Kind.t
    val of_node : Kind_graph.node -> (Rays.Camera.t * bool) option
    val lens_values : Rays.Camera.lens -> (string * Param.value) list
    val to_values :
      eye:Rays_math.Vec3.t ->
      target:Rays_math.Vec3.t -> fov_y:float -> (string * Param.value) list
  end
module Root :
  sig
    type renderer = Raster | Wireframe | Path_traced
    val renderer_parameter : renderer Param.kind
    type parameters = {
      renderer : renderer;
      width : int;
      height : int;
      max_spp : int;
      bounces : int;
      round_samples : int;
    }
    val parameters_default : parameters
    val parameters_schema : parameters Param.schema
    val kind : Kind_graph.Kind.t
    val default : parameters
    type render = { width : int; height : int; max_spp : int; }
    val render : parameters -> render
    val fields : parameters -> Param.field_view list
  end
module Light :
  sig
    type shape = Point | Spot | Area | Directional
    val shape_parameter : shape Param.kind
    type parameters = {
      shape : shape;
      translate_x : float;
      translate_y : float;
      translate_z : float;
      target_x : float;
      target_y : float;
      target_z : float;
      color_r : float;
      color_g : float;
      color_b : float;
      intensity : float;
      width : float;
      height : float;
      cone : float;
      visible : bool;
      render : bool;
    }
    val parameters_default : parameters
    val parameters_schema : parameters Param.schema
    val kind : Kind_graph.Kind.t
  end
val read : 'a Param.schema -> 'a -> Kind_graph.node -> 'a option
val geometry : Kind_graph.node -> Geometry.parameters option
val light : Kind_graph.node -> Light.parameters option
val parent : Kind_graph.t -> int -> int option
val within : Kind_graph.t -> int -> int -> bool
(** [within scene id above]: [above] is [id] or below it, so [id] cannot be parented under it. *)
val parent_world : Kind_graph.t -> int -> Rays_math.Mat4.t
val world : Kind_graph.t -> int -> Rays_math.Mat4.t
val visible : Kind_graph.node -> bool
val flag : string -> Kind_graph.node -> bool
val has_flag : string -> Kind_graph.node -> bool
val ids : string -> Kind_graph.t -> int list
val lights : ?render:bool -> Kind_graph.t -> Rays.Light.t list
val light_values : Rays.Light.t -> (string * Param.value) list
val kinds : Kind_graph.Kind.t list
(** The object kinds a scene adds: geometry, light, camera. *)
val reparent : Kind_graph.t -> int list -> int option -> Kind_graph.t
val outdent : Kind_graph.t -> int list -> Kind_graph.t
