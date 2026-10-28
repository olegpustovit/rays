type kind =
  | Invalid_argument
  | Invalid_state
  | Unsupported
  | No_adapter
  | Capacity
  | Device_lost
  | Stale_handle
  | Cross_device
  | Wrong_domain
  | Destroyed
  | Decode
  | Io

type t = { operation : string; kind : kind; message : string }

val make : string -> kind -> string -> t
val pp : Format.formatter -> t -> unit
val to_string : t -> string
