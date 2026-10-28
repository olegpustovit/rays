type device
type 'kind t

val create_device : unit -> device
val device_id : device -> int64
val destroy_device : device -> unit
val device_destroyed : device -> bool
val create : device:device -> 'kind t
val id : 'kind t -> int64
val destroy : 'kind t -> unit
val destroyed : 'kind t -> bool
val validate_for : operation:string -> device -> 'kind t -> (unit, Error.t) result
