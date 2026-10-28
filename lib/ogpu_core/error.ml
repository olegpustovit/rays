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

let make operation kind message = { operation; kind; message }
let pp formatter value = Format.fprintf formatter "%s: %s" value.operation value.message
let to_string value = Format.asprintf "%a" pp value
