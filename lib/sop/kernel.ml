type column = Floats of float array | Vec3s of float array
type runner = Context.t -> (column, Diagnostic.t) result
type t = {id : int; bytes : int; prepare : column list -> (runner, Diagnostic.t) result;
  geometry_capture : int -> Rdk.Geometry.t option}
let create ?(geometry_capture = fun _ -> None) ~payload_bytes prepare =
  if payload_bytes < 0 then invalid_arg "Kernel.create: negative payload size";
  {id = Rdk.Data_id.fresh (); bytes = payload_bytes; prepare; geometry_capture}
let data_id t = t.id
let payload_bytes t = t.bytes
let prepare t = t.prepare
  let geometry_capture t = t.geometry_capture
  let remap_geometry_captures ~sources t =
    if sources = [] then t else
      {t with geometry_capture = fun id ->
        Option.bind (List.find_opt (fun (_, mapped) -> id=mapped) sources)
          (fun (original, _) -> t.geometry_capture original)}
