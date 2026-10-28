(** The scene's objects as scene-graph kinds ([scene/geometry], [scene/camera], [scene/light],
    [scene/root]) and the reads and edits of a network of them. *)

val empty : Rdk_core.Geometry.t
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
    val parameters_factory :
      (label:string ->
       inputs:Sop.Node.t option list -> parameters -> Sop.Node.t) ->
      Sop.Edit_graph.factory
    val parameters_build :
      ?schema:(parameters -> Sop.Node.t -> parameters Param.schema) ->
      (label:string -> parameters -> Sop.Node.t option -> Sop.Node.t) ->
      label:string ->
      inputs:Sop.Node.t option list -> parameters -> Sop.Node.t
    val build :
      label:string ->
      inputs:Sop.Node.t option list -> parameters -> Sop.Node.t
    val factory : Sop.Edit_graph.factory
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
    val parameters_factory :
      (label:string -> inputs:Sop.Node.t list -> parameters -> Sop.Node.t) ->
      Sop.Edit_graph.factory
    val parameters_build :
      ?schema:(parameters -> Sop.Node.t -> parameters Param.schema) ->
      (label:string -> parameters -> Sop.Node.t) ->
      label:string -> inputs:Sop.Node.t list -> parameters -> Sop.Node.t
    val build :
      label:string -> inputs:Sop.Node.t list -> parameters -> Sop.Node.t
    val factory : Sop.Edit_graph.factory
    val of_node : Sop.Node.t -> (Rays.Camera.t * bool) option
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
    val parameters_factory :
      (label:string -> inputs:Sop.Node.t list -> parameters -> Sop.Node.t) ->
      Sop.Edit_graph.factory
    val parameters_build :
      ?schema:(parameters -> Sop.Node.t -> parameters Param.schema) ->
      (label:string -> parameters -> Sop.Node.t) ->
      label:string -> inputs:Sop.Node.t list -> parameters -> Sop.Node.t
    val build :
      label:string -> inputs:Sop.Node.t list -> parameters -> Sop.Node.t
    val factory : Sop.Edit_graph.factory
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
    val parameters_factory :
      (label:string ->
       inputs:Sop.Node.t option list -> parameters -> Sop.Node.t) ->
      Sop.Edit_graph.factory
    val parameters_build :
      ?schema:(parameters -> Sop.Node.t -> parameters Param.schema) ->
      (label:string -> parameters -> Sop.Node.t option -> Sop.Node.t) ->
      label:string ->
      inputs:Sop.Node.t option list -> parameters -> Sop.Node.t
    val build :
      label:string ->
      inputs:Sop.Node.t option list -> parameters -> Sop.Node.t
    val factory : Sop.Edit_graph.factory
  end
val read : 'a Param.schema -> 'a -> Sop.Node.t -> 'a option
val geometry : Sop.Node.t -> Geometry.parameters option
val light : Sop.Node.t -> Light.parameters option
val parent : Sop.Edit_graph.t -> int -> int option
val parent_world : Sop.Edit_graph.t -> int -> Rays_math.Mat4.t
val world : Sop.Edit_graph.t -> int -> Rays_math.Mat4.t
val visible : Sop.Node.t -> bool
val flag : string -> Sop.Node.t -> bool
val has_flag : string -> Sop.Node.t -> bool
val ids : string -> Sop.Edit_graph.t -> int list
val lights : ?render:bool -> Sop.Edit_graph.t -> Rays.Light.t list
val light_values : Rays.Light.t -> (string * Param.value) list
val catalog : Sop.Edit_graph.factory list
val reparent : Sop.Edit_graph.t -> int list -> int option -> Sop.Edit_graph.t
val outdent : Sop.Edit_graph.t -> int list -> Sop.Edit_graph.t
