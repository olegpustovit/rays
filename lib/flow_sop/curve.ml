module Edit = Sop.Edit_graph

let key = "flow.curve"
let parameter = "points"
let closed_parameter = "closed"
let filled_parameter = "filled"

let encode points =
  let buffer = Buffer.create (Array.length points * 40) in
  Array.iteri (fun i (x, y, z) ->
    if i > 0 then Buffer.add_char buffer ';';
    Buffer.add_string buffer (Printf.sprintf "%h,%h,%h" x y z)) points;
  Buffer.contents buffer

let decode text =
  if text = "" then Ok [||] else
  try Ok (Array.of_list (List.map (fun point ->
    match String.split_on_char ',' point with
    | [x; y; z] -> float_of_string x, float_of_string y, float_of_string z
    | _ -> failwith "point") (String.split_on_char ';' text)))
  with Failure _ -> Error "curve points: expected x,y,z;..."

type t = { points : (float * float * float) array; closed : bool; filled : bool }

let default = { points = [||]; closed = false; filled = false }

let schema = Param.schema ~name:"flow_curve" ~default [
  Param.field ~name:parameter ~label:"Points"
    ~kind:(Param.encoded ~equal:( = ) ~encode ~decode)
    ~default:default.points ~get:(fun curve -> curve.points)
    ~set:(fun points curve -> { curve with points }) ();
  Param.field ~name:closed_parameter ~label:"Closed"
    ~kind:Param.Toggle ~default:false ~get:(fun curve -> curve.closed)
    ~set:(fun closed curve -> { curve with closed }) ();
  Param.field ~name:filled_parameter ~label:"Filled"
    ~kind:Param.Toggle ~default:false ~get:(fun curve -> curve.filled)
    ~set:(fun filled curve -> { curve with filled }) () ]

(* A filled curve is one polygon primitive (a surface), not a closed polyline. *)
let polyline ~closed ~filled points =
  Result.bind (Rdk.Line_geometry.polyline ~closed:(closed || filled) (Array.copy points))
    (fun line ->
      if not filled then Ok line else
      let count = Array.length points in
      match Rdk.Topology.polygons_owned ~point_count:count
          ~vertex_points:(Array.init count Fun.id) ~primitive_offsets:[|0; count|] with
      | Error message -> Error (Rdk.Error.make ~operation:"polyline"
          ~code:"invalid_parameter" message)
      | Ok topology ->
          Result.map_error (Rdk.Error.make ~operation:"polyline" ~code:"invalid_parameter")
            (Rdk.Geometry.create ~positions:(Rdk.Geometry.positions line) ~topology ()))

let parameter_key {points; closed; filled} =
  let prefix = "flow.curve\000" in
  let bytes = Bytes.create (String.length prefix + 2 + (Array.length points * 24)) in
  Bytes.blit_string prefix 0 bytes 0 (String.length prefix);
  let offset = ref (String.length prefix) in
  Bytes.set bytes !offset (if closed then '\001' else '\000'); incr offset;
  Bytes.set bytes !offset (if filled then '\001' else '\000'); incr offset;
  let add_float value =
    let bits = Int64.bits_of_float value in
    for byte = 0 to 7 do
      let shifted = Int64.shift_right_logical bits (byte * 8) in
      Bytes.set bytes (!offset + byte) (Char.chr (Int64.to_int (Int64.logand shifted 0xffL)))
    done;
    offset := !offset + 8 in
  Array.iter (fun (x, y, z) -> add_float x; add_float y; add_float z) points;
  Bytes.to_string bytes

let node curve =
  let rec build ~label ~inputs parameters =
    let {points; closed; filled} = parameters in
    let parameters_key = Printf.sprintf "count=%d;closed=%b;filled=%b"
      (Array.length points) closed filled in
    let node = Sop.Node.Build.make_geometry ~label ~operation:"polyline" ~version:1
        ~parameters:parameters_key ~cook_mode:Sop.Node.Generator
        ~dependencies:Sop.Context.Dependencies.static ~inputs:(Array.of_list inputs)
        (fun ~node_id:_ _context _inputs ->
          match polyline ~closed ~filled points with
          | Ok geometry -> Ok Sop.Node.Build.{geometry; instances = None}
          | Error error -> Error (Diagnostic.error ~code:(Rdk.Error.code error)
              ~cause:(Rdk.Error.to_string error) ~hints:(Rdk.Error.hints error)
              (Rdk.Error.operation error ^ " could not produce valid geometry"))) in
    Sop.Node.Build.parameterize_with_key ~parameter_key:(parameter_key parameters)
      ~schema ~values:parameters ~rebuild:build node
  in
  Result.get_ok (build ~label:"Curve" ~inputs:[] curve)

let replace_points ~source ~closed ~filled points =
  Sop.Node.Build.adopt_identity ~source (node {points; closed; filled})

let factory = Edit.factory ~key ~label:"Curve" ~category:["Flow"] ~arity:0
  ~fields:(Sop.Node.parameter_fields (node default)) (fun _ -> Ok (node default))
