type t = { mutable leases : Runtime_resources.Image.lease list }
type checkpoint = Runtime_resources.Image.lease list
type policy = Copy_image_snapshots | Retain_image_snapshots of t

let create () = { leases = [] }
let checkpoint = function
  | Copy_image_snapshots -> None
  | Retain_image_snapshots value -> Some value.leases

let retain value lease = value.leases <- lease :: value.leases

let release value =
  List.iter Runtime_resources.Image.release_snapshot value.leases;
  value.leases <- []

let rollback policy checkpoint = match policy, checkpoint with
  | Copy_image_snapshots, _ -> ()
  | Retain_image_snapshots value, Some before ->
      let rec release_to = function
        | leases when leases == before -> ()
        | lease :: rest ->
            Runtime_resources.Image.release_snapshot lease;
            release_to rest
        | [] -> assert false
      in
      release_to value.leases;
      value.leases <- before
  | Retain_image_snapshots _, None -> assert false
