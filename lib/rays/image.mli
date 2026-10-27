type t
val load : string -> (t,string) result
val load_exn : string -> t
val create : width:int -> height:int -> ?color:Rays_math.Color.t -> unit -> (t,string) result
(* Create or update an RGBA image. Updating preserves the image identity. *)
val upload_rgba : ?into:t -> width:int -> height:int -> rgba:bytes -> unit -> (t,string) result
val destroy : t -> (unit,string) result
val get_width : t -> (int,string) result
val get_height : t -> (int,string) result
val get_size : t -> (int * int,string) result
module Private : sig
  val replace : t -> t -> (unit,string) result
  val identity : t -> int
  val pixels : t -> (bytes,string) result
  val of_resource : Runtime_resources.Image.t -> (t,string) result
  val resource : t -> Runtime_resources.Image.t
  val dimensions : t -> int * int
end
