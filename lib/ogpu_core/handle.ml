type device = { id : int64; mutable epoch : int64; mutable dead : bool }

type 'kind t = {
  id : int64;
  device : device;
  device_epoch : int64;
  mutable generation : int64;
  mutable dead : bool;
}

let next_device = Atomic.make 1
let next_handle = Atomic.make 1
let fresh counter = Int64.of_int (Atomic.fetch_and_add counter 1)
let create_device () = { id = fresh next_device; epoch = 1L; dead = false }
let device_id (value : device) = value.id

let destroy_device (value : device) =
  if not value.dead then (
    value.dead <- true;
    value.epoch <- Int64.succ value.epoch)

let device_destroyed (value : device) = value.dead

let create ~(device : device) =
  { id = fresh next_handle; device; device_epoch = device.epoch; generation = 1L; dead = false }

let id value = value.id

let destroy value =
  if not value.dead then (
    value.dead <- true;
    value.generation <- Int64.succ value.generation)

let destroyed value = value.dead

let validate ~operation value =
  if value.dead then Error (Error.make operation Error.Stale_handle "handle is destroyed")
  else if value.device.dead || value.device_epoch <> value.device.epoch then
    Error (Error.make operation Error.Stale_handle "owning device generation is stale")
  else Ok ()

let validate_for ~operation (device : device) value =
  if device.id <> value.device.id then
    Error (Error.make operation Error.Cross_device "handle belongs to another device")
  else validate ~operation value
