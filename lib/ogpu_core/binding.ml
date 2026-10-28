type stage = Vertex | Fragment | Compute
type kind = Buffer | Texture | Sampler | Acceleration_structure
type layout_entry = { binding : int; kind : kind; visibility : stage list }
type layout = layout_entry list
type pipeline_layout = { groups : (int * layout) list }

let stale operation message = Error (Error.make operation Error.Stale_handle message)

let compare_layout (left : layout_entry) (right : layout_entry) =
  Int.compare left.binding right.binding

let create_layout entries =
  let entries = List.sort compare_layout entries in
  let stage = function Vertex -> 0 | Fragment -> 1 | Compute -> 2 in
  Result.map
    (fun () -> entries)
    (Validation.validate_layout ~operation:"Ogpu.Binding.create_layout"
       (List.map
          (fun (value : layout_entry) -> (value.binding, List.map stage value.visibility))
          entries))

let create_pipeline_layout ~device ~capabilities groups =
  if Handle.device_destroyed device then
    stale "Ogpu.Binding.create_pipeline_layout" "device is destroyed"
  else
    let groups = List.sort (fun (left, _) (right, _) -> Int.compare left right) groups in
    Result.map
      (fun () -> { groups })
      (Validation.validate_groups ~operation:"Ogpu.Binding.create_pipeline_layout"
         ~max_groups:capabilities.Caps.limits.max_bind_groups (List.map fst groups))

let pipeline_layouts value = List.map (fun (group, layout) -> (group, layout)) value.groups
