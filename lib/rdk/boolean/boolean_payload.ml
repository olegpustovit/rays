open Rays_math

type point_conflict = Reject | Promote_to_vertex

type attribute_pair = {
  name : string;
  source_owner : Attribute.owner;
  kind : int;
  mutable left : Attribute.t option;
  mutable right : Attribute.t option;
}

type group_pair = {
  group_name : string;
  source_group_owner : Group.owner;
  mutable left_group : Group.t option;
  mutable right_group : Group.t option;
}

type edge_group_pair = {
  edge_group_name : string;
  mutable left_edge_group : Edge_group.t option;
  mutable right_edge_group : Edge_group.t option;
}

exception Payload_error of string * string

let operation = "boolean_payload"
let error code message = Error (Error.make ~operation ~code message)

let storage_kind = function
  | Attribute.Float _ -> 0 | Attribute.Int _ -> 1
  | Attribute.Int_array _ -> 2 | Attribute.Float_array _ -> 3
  | Attribute.Float2 _ -> 4 | Attribute.Float3 _ -> 5
  | Attribute.Float4 _ -> 6 | Attribute.Text _ -> 7

let collect_attributes owner left right =
  let table = Hashtbl.create 16 and reversed = ref [] in
  let add side attribute =
    if Attribute.owner attribute = owner then begin
      let name = Attribute.name attribute
      and kind = storage_kind (Attribute.Private.storage attribute) in
      match Hashtbl.find_opt table name with
      | None ->
          let pair = { name; source_owner = owner; kind;
                       left = None; right = None } in
          if side = 0 then pair.left <- Some attribute else pair.right <- Some attribute;
          Hashtbl.add table name pair; reversed := pair :: !reversed
      | Some pair ->
          if pair.kind <> kind then raise (Payload_error
            ("attribute_storage_mismatch", Printf.sprintf
              "%s attribute %S has different storage on the Boolean operands"
              (match owner with Attribute.Point -> "point" | Attribute.Vertex -> "vertex" | _ -> "primitive") name));
          if side = 0 then pair.left <- Some attribute else pair.right <- Some attribute
    end in
  Array.iter (add 0) (Geometry.Private.attributes left);
  Array.iter (add 1) (Geometry.Private.attributes right);
  Array.of_list (List.rev !reversed)

let collect_groups owner left right =
  let table = Hashtbl.create 16 and reversed = ref [] in
  let add side group =
    if Group.owner group = owner then begin
      let name = Group.name group in
      match Hashtbl.find_opt table name with
      | None ->
          let pair = { group_name = name; source_group_owner = owner;
                       left_group = None; right_group = None } in
          if side = 0 then pair.left_group <- Some group else pair.right_group <- Some group;
          Hashtbl.add table name pair; reversed := pair :: !reversed
      | Some pair ->
          if side = 0 then pair.left_group <- Some group else pair.right_group <- Some group
    end in
  List.iter (add 0) (Geometry.groups left);
  List.iter (add 1) (Geometry.groups right);
  Array.of_list (List.rev !reversed)

let collect_edge_groups left right =
  let table = Hashtbl.create 16 and reversed = ref [] in
  let add side group =
    let name = Edge_group.name group in
    match Hashtbl.find_opt table name with
    | None ->
        let pair = { edge_group_name = name; left_edge_group = None;
                     right_edge_group = None } in
        if side = 0 then pair.left_edge_group <- Some group
        else pair.right_edge_group <- Some group;
        Hashtbl.add table name pair;
        reversed := pair :: !reversed
    | Some pair ->
        if side = 0 then pair.left_edge_group <- Some group
        else pair.right_edge_group <- Some group in
  List.iter (add 0) (Geometry.edge_groups left);
  List.iter (add 1) (Geometry.edge_groups right);
  Array.of_list (List.rev !reversed)

let selected view left right primitive =
  if Bytes.unsafe_get view.Boolean_extract.Private.primitive_sides primitive = '\000'
  then left else right

let source_index view owner primitive local = match owner with
  | Attribute.Point -> view.Boolean_extract.Private.primitive_source_points.
      ((primitive * 3) + local)
  | Attribute.Vertex -> view.Boolean_extract.Private.primitive_source_vertices.
      ((primitive * 3) + local)
  | _ -> assert false

let dominant_values a b c =
  if a >= b && a >= c then 0 else if b >= c then 1 else 2

let dominant_corner view corner = dominant_values
    view.Boolean_extract.Private.barycentric_a.(corner)
    view.Boolean_extract.Private.barycentric_b.(corner)
    view.Boolean_extract.Private.barycentric_c.(corner)

let float_equal tolerance left right =
  left = right
  || Int64.bits_of_float left = Int64.bits_of_float right
  || (Float.is_finite left && Float.is_finite right
      && abs_float (left -. right)
         <= tolerance *. Float.max 1. (Float.max (abs_float left) (abs_float right)))

let normalize3 x y z =
  let scale = Float.max (abs_float x) (Float.max (abs_float y) (abs_float z)) in
  if scale = 0. || not (Float.is_finite scale) then x, y, z
  else
    let sx = x /. scale and sy = y /. scale and sz = z /. scale in
    let length = sqrt ((sx *. sx) +. (sy *. sy) +. (sz *. sz)) in
    if length = 0. || not (Float.is_finite length) then x, y, z
    else sx /. length, sy /. length, sz /. length

let source_storage pair =
  Option.map Attribute.Private.storage pair.left,
  Option.map Attribute.Private.storage pair.right

let interpolate_fixed ?cancel ~grain ancestry view corner_count pair =
  let left, right = source_storage pair in
  match left, right with
  | (Some (Attribute.Float _) | None), (Some (Attribute.Float _) | None)
      when pair.kind = 0 ->
      let storage_view = function Attribute.Float value -> value | _ -> assert false in
      let left = Option.map storage_view left and right = Option.map storage_view right
      and output = Array.make corner_count 0. in
      Support.parallel_for ?cancel ~grain corner_count (fun corner ->
        let primitive = corner / 3 in
        match selected view left right primitive with
        | None -> ()
        | Some values ->
            let a = source_index view pair.source_owner primitive 0
            and b = source_index view pair.source_owner primitive 1
            and c = source_index view pair.source_owner primitive 2
            and wa = view.Boolean_extract.Private.barycentric_a.(corner)
            and wb = view.Boolean_extract.Private.barycentric_b.(corner)
            and wc = view.Boolean_extract.Private.barycentric_c.(corner) in
            output.(corner) <- (wa *. values.(a)) +. (wb *. values.(b))
                +. (wc *. values.(c)));
      Attribute.Float output
  | (Some (Attribute.Int _) | None), (Some (Attribute.Int _) | None)
      when pair.kind = 1 ->
      let storage_view = function Attribute.Int value -> value | _ -> assert false in
      let left = Option.map storage_view left and right = Option.map storage_view right
      and output = Array.make corner_count 0 in
      Support.parallel_for ?cancel ~grain corner_count (fun corner ->
        let primitive = corner / 3 in
        match selected view left right primitive with
        | None -> ()
        | Some values ->
            output.(corner) <- values.(source_index view pair.source_owner
              primitive (dominant_corner view corner)));
      Attribute.Int output
  | (Some (Attribute.Text _) | None), (Some (Attribute.Text _) | None)
      when pair.kind = 7 ->
      let storage_view = function Attribute.Text value -> value | _ -> assert false in
      let left = Option.map storage_view left and right = Option.map storage_view right
      and output = Array.make corner_count "" in
      Support.parallel_for ?cancel ~grain corner_count (fun corner ->
        let primitive = corner / 3 in
        match selected view left right primitive with
        | None -> ()
        | Some values ->
            output.(corner) <- values.(source_index view pair.source_owner
              primitive (dominant_corner view corner)));
      Attribute.Text output
  | (Some (Attribute.Float2 _) | None), (Some (Attribute.Float2 _) | None)
      when pair.kind = 4 ->
      let storage_view = function Attribute.Float2 value -> Packed.Float2.Private.view value
        | _ -> assert false in
      let left = Option.map storage_view left and right = Option.map storage_view right
      and x = Array.make corner_count 0. and y = Array.make corner_count 0. in
      Support.parallel_for ?cancel ~grain corner_count (fun corner ->
        let primitive = corner / 3 in
        match selected view left right primitive with
        | None -> ()
        | Some values ->
            let a = source_index view pair.source_owner primitive 0
            and b = source_index view pair.source_owner primitive 1
            and c = source_index view pair.source_owner primitive 2
            and wa = view.Boolean_extract.Private.barycentric_a.(corner)
            and wb = view.Boolean_extract.Private.barycentric_b.(corner)
            and wc = view.Boolean_extract.Private.barycentric_c.(corner) in
            x.(corner) <- wa *. values.x.(a) +. wb *. values.x.(b) +. wc *. values.x.(c);
            y.(corner) <- wa *. values.y.(a) +. wb *. values.y.(b) +. wc *. values.y.(c));
      let value = match Packed.Float2.of_owned ~x ~y with
        | Ok value -> value | Error message -> invalid_arg message in
      Attribute.Float2 value
  | (Some (Attribute.Float3 _) | None), (Some (Attribute.Float3 _) | None)
      when pair.kind = 5 ->
      let storage_view = function Attribute.Float3 value -> Packed.Float3.Private.view value
        | _ -> assert false in
      let left = Option.map storage_view left and right = Option.map storage_view right
      and x = Array.make corner_count 0. and y = Array.make corner_count 0.
      and z = Array.make corner_count 0. in
      Support.parallel_for ?cancel ~grain corner_count (fun corner ->
        let primitive = corner / 3 in
        match selected view left right primitive with
        | None -> ()
        | Some values ->
            let a = source_index view pair.source_owner primitive 0
            and b = source_index view pair.source_owner primitive 1
            and c = source_index view pair.source_owner primitive 2
            and wa = view.Boolean_extract.Private.barycentric_a.(corner)
            and wb = view.Boolean_extract.Private.barycentric_b.(corner)
            and wc = view.Boolean_extract.Private.barycentric_c.(corner) in
            let ox = wa *. values.x.(a) +. wb *. values.x.(b) +. wc *. values.x.(c)
            and oy = wa *. values.y.(a) +. wb *. values.y.(b) +. wc *. values.y.(c)
            and oz = wa *. values.z.(a) +. wb *. values.z.(b) +. wc *. values.z.(c) in
            let ox, oy, oz = if pair.name = "N" then
                let winding = float_of_int
                    (Boolean_extract.primitive_winding ancestry primitive) in
                normalize3 (winding *. ox) (winding *. oy) (winding *. oz)
              else ox, oy, oz in
            x.(corner) <- ox; y.(corner) <- oy; z.(corner) <- oz);
      Attribute.Float3 (Packed.Float3.Private.of_owned_exn ~x ~y ~z)
  | (Some (Attribute.Float4 _) | None), (Some (Attribute.Float4 _) | None)
      when pair.kind = 6 ->
      let storage_view = function Attribute.Float4 value -> Packed.Float4.Private.view value
        | _ -> assert false in
      let left = Option.map storage_view left and right = Option.map storage_view right
      and x = Array.make corner_count 0. and y = Array.make corner_count 0.
      and z = Array.make corner_count 0. and w = Array.make corner_count 0. in
      Support.parallel_for ?cancel ~grain corner_count (fun corner ->
        let primitive = corner / 3 in
        match selected view left right primitive with
        | None -> ()
        | Some values ->
            let a = source_index view pair.source_owner primitive 0
            and b = source_index view pair.source_owner primitive 1
            and c = source_index view pair.source_owner primitive 2
            and wa = view.Boolean_extract.Private.barycentric_a.(corner)
            and wb = view.Boolean_extract.Private.barycentric_b.(corner)
            and wc = view.Boolean_extract.Private.barycentric_c.(corner) in
            x.(corner) <- wa *. values.x.(a) +. wb *. values.x.(b) +. wc *. values.x.(c);
            y.(corner) <- wa *. values.y.(a) +. wb *. values.y.(b) +. wc *. values.y.(c);
            z.(corner) <- wa *. values.z.(a) +. wb *. values.z.(b) +. wc *. values.z.(c);
            w.(corner) <- wa *. values.w.(a) +. wb *. values.w.(b) +. wc *. values.w.(c));
      let value = match Packed.Float4.of_owned ~x ~y ~z ~w with
        | Ok value -> value | Error message -> invalid_arg message in
      Attribute.Float4 value
  | _ -> raise (Payload_error ("unsupported_storage", Printf.sprintf
      "%s attribute %S uses CSR storage not yet handled by the fixed-width interpolator"
      (if pair.source_owner = Attribute.Point then "point" else "vertex") pair.name))

let interpolate_csr ?cancel ~grain ancestry view corner_count pair =
  let left, right = source_storage pair in
  match left, right with
  | (Some (Attribute.Int_array _) | None), (Some (Attribute.Int_array _) | None)
      when pair.kind = 2 ->
      let storage_view = function Attribute.Int_array value -> Packed.Int_array.Private.view value
        | _ -> assert false in
      let left = Option.map storage_view left and right = Option.map storage_view right
      and lengths = Array.make corner_count 0 in
      Support.parallel_for ?cancel ~grain corner_count (fun corner ->
        let primitive = corner / 3 in
        match selected view left right primitive with
        | None -> ()
        | Some values ->
            let local = dominant_corner view corner in
            let source = source_index view pair.source_owner primitive local in
            lengths.(corner) <- values.offsets.(source + 1) - values.offsets.(source));
      let offsets = Array.make (corner_count + 1) 0 in
      for corner = 0 to corner_count - 1 do
        if offsets.(corner) > Sys.max_array_length - lengths.(corner) then
          invalid_arg "Boolean corner integer-array payload exceeds array limits";
        offsets.(corner + 1) <- offsets.(corner) + lengths.(corner)
      done;
      let values = Array.make offsets.(corner_count) 0 in
      Support.parallel_for ?cancel ~grain corner_count (fun corner ->
        let primitive = corner / 3 in
        match selected view left right primitive with
        | None -> ()
        | Some source_values ->
            let local = dominant_corner view corner in
            let source = source_index view pair.source_owner primitive local in
            Array.blit source_values.values source_values.offsets.(source)
              values offsets.(corner) lengths.(corner));
      Attribute.Int_array
        (Packed.Int_array.Private.create_validated_owned ~offsets ~values)
  | (Some (Attribute.Float_array _) | None), (Some (Attribute.Float_array _) | None)
      when pair.kind = 3 ->
      let storage_view = function Attribute.Float_array value -> Packed.Float_array.Private.view value
        | _ -> assert false in
      let left = Option.map storage_view left and right = Option.map storage_view right
      and lengths = Array.make corner_count 0 and interpolate = Bytes.make corner_count '\000' in
      Support.parallel_for ?cancel ~grain corner_count (fun corner ->
        let primitive = corner / 3 in
        match selected view left right primitive with
        | None -> ()
        | Some values ->
            let a = source_index view pair.source_owner primitive 0
            and b = source_index view pair.source_owner primitive 1
            and c = source_index view pair.source_owner primitive 2 in
            let al = values.offsets.(a + 1) - values.offsets.(a)
            and bl = values.offsets.(b + 1) - values.offsets.(b)
            and cl = values.offsets.(c + 1) - values.offsets.(c) in
            if al = bl && al = cl then begin
              lengths.(corner) <- al; Bytes.unsafe_set interpolate corner '\001'
            end else begin
              let local = dominant_corner view corner in
              let source = if local = 0 then a else if local = 1 then b else c in
              lengths.(corner) <- values.offsets.(source + 1) - values.offsets.(source)
            end);
      let offsets = Array.make (corner_count + 1) 0 in
      for corner = 0 to corner_count - 1 do
        if offsets.(corner) > Sys.max_array_length - lengths.(corner) then
          invalid_arg "Boolean corner float-array payload exceeds array limits";
        offsets.(corner + 1) <- offsets.(corner) + lengths.(corner)
      done;
      let values = Array.make offsets.(corner_count) 0. in
      Support.parallel_for ?cancel ~grain corner_count (fun corner ->
        let primitive = corner / 3 in
        match selected view left right primitive with
        | None -> ()
        | Some source_values ->
            let a = source_index view pair.source_owner primitive 0
            and b = source_index view pair.source_owner primitive 1
            and c = source_index view pair.source_owner primitive 2 in
            if Bytes.unsafe_get interpolate corner <> '\000' then begin
              let wa = view.Boolean_extract.Private.barycentric_a.(corner)
              and wb = view.Boolean_extract.Private.barycentric_b.(corner)
              and wc = view.Boolean_extract.Private.barycentric_c.(corner) in
              let ao = source_values.offsets.(a) and bo = source_values.offsets.(b)
              and co = source_values.offsets.(c) and output = offsets.(corner) in
              for component = 0 to lengths.(corner) - 1 do
                values.(output + component) <-
                  wa *. source_values.values.(ao + component)
                  +. wb *. source_values.values.(bo + component)
                  +. wc *. source_values.values.(co + component)
              done
            end else begin
              let local = dominant_corner view corner in
              let source = if local = 0 then a else if local = 1 then b else c in
              Array.blit source_values.values source_values.offsets.(source)
                values offsets.(corner) lengths.(corner)
            end);
      Attribute.Float_array
        (Packed.Float_array.Private.create_validated_owned ~offsets ~values)
  | _ -> interpolate_fixed ?cancel ~grain ancestry view corner_count pair

let first_corners topology point_count =
  let representatives = Array.make point_count (-1) in
  Array.iteri (fun corner point ->
    if representatives.(point) < 0 then representatives.(point) <- corner)
    topology.Topology.Private.vertex_points;
  representatives

let conflict name point = raise (Payload_error
  ("point_payload_conflict", Printf.sprintf
    "point payload %S disagrees across incident Boolean corners at output point %d; use Promote_to_vertex or split the schema"
    name point))

let consolidate_to_points ~tolerance name topology point_count representatives storage =
  let vertex_points = topology.Topology.Private.vertex_points in
  match storage with
  | Attribute.Float source ->
      let output = Array.init point_count (fun point -> source.(representatives.(point))) in
      Array.iteri (fun corner point ->
        if not (float_equal tolerance output.(point) source.(corner)) then conflict name point)
        vertex_points;
      Attribute.Float output
  | Attribute.Int source ->
      let output = Array.init point_count (fun point -> source.(representatives.(point))) in
      Array.iteri (fun corner point ->
        if output.(point) <> source.(corner) then conflict name point) vertex_points;
      Attribute.Int output
  | Attribute.Text source ->
      let output = Array.init point_count (fun point -> source.(representatives.(point))) in
      Array.iteri (fun corner point ->
        if not (String.equal output.(point) source.(corner)) then conflict name point)
        vertex_points;
      Attribute.Text output
  | Attribute.Float2 value ->
      let source = Packed.Float2.Private.view value in
      let x = Array.init point_count (fun point -> source.x.(representatives.(point)))
      and y = Array.init point_count (fun point -> source.y.(representatives.(point))) in
      Array.iteri (fun corner point ->
        if not (float_equal tolerance x.(point) source.x.(corner)
            && float_equal tolerance y.(point) source.y.(corner)) then conflict name point)
        vertex_points;
      let value = match Packed.Float2.of_owned ~x ~y with
        | Ok value -> value | Error message -> invalid_arg message in
      Attribute.Float2 value
  | Attribute.Float3 value ->
      let source = Packed.Float3.Private.view value in
      let x = Array.init point_count (fun point -> source.x.(representatives.(point)))
      and y = Array.init point_count (fun point -> source.y.(representatives.(point)))
      and z = Array.init point_count (fun point -> source.z.(representatives.(point))) in
      Array.iteri (fun corner point ->
        if not (float_equal tolerance x.(point) source.x.(corner)
            && float_equal tolerance y.(point) source.y.(corner)
            && float_equal tolerance z.(point) source.z.(corner)) then conflict name point)
        vertex_points;
      Attribute.Float3 (Packed.Float3.Private.of_owned_exn ~x ~y ~z)
  | Attribute.Float4 value ->
      let source = Packed.Float4.Private.view value in
      let x = Array.init point_count (fun point -> source.x.(representatives.(point)))
      and y = Array.init point_count (fun point -> source.y.(representatives.(point)))
      and z = Array.init point_count (fun point -> source.z.(representatives.(point)))
      and w = Array.init point_count (fun point -> source.w.(representatives.(point))) in
      Array.iteri (fun corner point ->
        if not (float_equal tolerance x.(point) source.x.(corner)
            && float_equal tolerance y.(point) source.y.(corner)
            && float_equal tolerance z.(point) source.z.(corner)
            && float_equal tolerance w.(point) source.w.(corner)) then conflict name point)
        vertex_points;
      let value = match Packed.Float4.of_owned ~x ~y ~z ~w with
        | Ok value -> value | Error message -> invalid_arg message in
      Attribute.Float4 value
  | Attribute.Int_array value ->
      let source = Packed.Int_array.Private.view value in
      let lengths = Array.init point_count (fun point ->
        let corner = representatives.(point) in
        source.offsets.(corner + 1) - source.offsets.(corner)) in
      Array.iteri (fun corner point ->
        let first = source.offsets.(corner) and last = source.offsets.(corner + 1)
        and representative = representatives.(point) in
        let other_first = source.offsets.(representative)
        and other_last = source.offsets.(representative + 1) in
        if last - first <> other_last - other_first then conflict name point;
        for slot = 0 to last - first - 1 do
          if source.values.(first + slot) <> source.values.(other_first + slot) then
            conflict name point
        done) vertex_points;
      let offsets = Array.make (point_count + 1) 0 in
      for point = 0 to point_count - 1 do
        offsets.(point + 1) <- offsets.(point) + lengths.(point)
      done;
      let values = Array.make offsets.(point_count) 0 in
      for point = 0 to point_count - 1 do
        let corner = representatives.(point) in
        Array.blit source.values source.offsets.(corner) values offsets.(point)
          lengths.(point)
      done;
      Attribute.Int_array
        (Packed.Int_array.Private.create_validated_owned ~offsets ~values)
  | Attribute.Float_array value ->
      let source = Packed.Float_array.Private.view value in
      let lengths = Array.init point_count (fun point ->
        let corner = representatives.(point) in
        source.offsets.(corner + 1) - source.offsets.(corner)) in
      Array.iteri (fun corner point ->
        let first = source.offsets.(corner) and last = source.offsets.(corner + 1)
        and representative = representatives.(point) in
        let other_first = source.offsets.(representative)
        and other_last = source.offsets.(representative + 1) in
        if last - first <> other_last - other_first then conflict name point;
        for slot = 0 to last - first - 1 do
          if not (float_equal tolerance source.values.(first + slot)
              source.values.(other_first + slot)) then conflict name point
        done) vertex_points;
      let offsets = Array.make (point_count + 1) 0 in
      for point = 0 to point_count - 1 do offsets.(point + 1) <- offsets.(point) + lengths.(point) done;
      let values = Array.make offsets.(point_count) 0. in
      for point = 0 to point_count - 1 do
        let corner = representatives.(point) in
        Array.blit source.values source.offsets.(corner) values offsets.(point)
          lengths.(point)
      done;
      Attribute.Float_array
        (Packed.Float_array.Private.create_validated_owned ~offsets ~values)

let create_attribute pair owner storage =
  match Attribute.create_owned ~name:pair.name ~owner storage with
  | Ok value -> value | Error message -> invalid_arg message

let group_source_index view owner primitive local = match owner with
  | Group.Point -> view.Boolean_extract.Private.primitive_source_points.
      ((primitive * 3) + local)
  | Group.Vertex -> view.Boolean_extract.Private.primitive_source_vertices.
      ((primitive * 3) + local)
  | Group.Primitive -> assert false

let copy_group ?cancel ~grain ancestry view topology point_count representatives
    ~point_conflict pair =
  let corner_count = Array.length topology.Topology.Private.vertex_points in
  let membership = Bytes.make corner_count '\000' in
  Support.parallel_for ?cancel ~grain corner_count (fun corner ->
    let primitive = corner / 3 and local = dominant_corner view corner in
    match selected view pair.left_group pair.right_group primitive with
    | None -> ()
    | Some source ->
        let source_index = group_source_index view pair.source_group_owner
            primitive local in
        if Group.mem source_index source then Bytes.unsafe_set membership corner '\001');
  let target_owner, target_count, target_membership, representative =
    match pair.source_group_owner, point_conflict with
    | Group.Point, Reject ->
        let points = Bytes.make point_count '\000' in
        Array.iteri (fun corner point ->
          let value = Bytes.unsafe_get membership corner in
          let representative = representatives.(point) in
          if corner = representative then Bytes.unsafe_set points point value
          else if value <> Bytes.unsafe_get points point then
            raise (Payload_error ("point_group_conflict", Printf.sprintf
              "point group %S disagrees across incident Boolean corners at output point %d; use Promote_to_vertex or split the group"
              pair.group_name point))) topology.Topology.Private.vertex_points;
        Group.Point, point_count, points, (fun point -> representatives.(point))
    | Group.Point, Promote_to_vertex
    | Group.Vertex, _ ->
        Group.Vertex, corner_count, membership, Fun.id
    | Group.Primitive, _ -> assert false in
  let group = Group.init ~grain ~owner:target_owner ~name:pair.group_name target_count
      (fun element -> Bytes.unsafe_get target_membership element <> '\000') in
  let is_ordered =
    Option.fold ~none:false ~some:Group.is_ordered pair.left_group
    || Option.fold ~none:false ~some:Group.is_ordered pair.right_group in
  if not is_ordered then group
  else begin
    let source_count geometry = match pair.source_group_owner with
      | Group.Point -> Geometry.point_count geometry
      | Group.Vertex -> Geometry.vertex_count geometry
      | Group.Primitive -> assert false in
    let ranks count source =
      let output = Array.make count max_int and rank = ref 0 in
      Option.iter (fun source -> Group.iter_ordered (fun element ->
        output.(element) <- !rank; incr rank) source) source;
      output in
    let left_geometry = Boolean_extract.Private.left_geometry ancestry
    and right_geometry = Boolean_extract.Private.right_geometry ancestry in
    let left_ranks = ranks (source_count left_geometry) pair.left_group
    and right_ranks = ranks (source_count right_geometry) pair.right_group
    and keyed = Array.make (Group.cardinality group) (0, 0, 0)
    and next = ref 0 in
    let key element =
      let corner = representative element in
      let primitive = corner / 3 and local = dominant_corner view corner in
      let source = group_source_index view pair.source_group_owner primitive local in
      if Bytes.unsafe_get view.Boolean_extract.Private.primitive_sides primitive = '\000'
      then 0, left_ranks.(source), element
      else 1, right_ranks.(source), element in
    Group.iter (fun element ->
      keyed.(!next) <- key element; incr next) group;
    Array.sort Stdlib.compare keyed;
    Group.Private.with_owned_order
      (Array.map (fun (_, _, element) -> element) keyed) group
  end

let assert_no_promotion_collisions point_conflict point_attributes vertex_attributes
    point_groups vertex_groups =
  if point_conflict = Promote_to_vertex then begin
    let attribute_names = Hashtbl.create (Array.length vertex_attributes) in
    Array.iter (fun pair -> Hashtbl.replace attribute_names pair.name ()) vertex_attributes;
    Array.iter (fun pair -> if Hashtbl.mem attribute_names pair.name then
      raise (Payload_error ("promotion_collision", Printf.sprintf
        "point attribute %S cannot be promoted because a vertex attribute has the same name"
        pair.name))) point_attributes;
    let group_names = Hashtbl.create (Array.length vertex_groups) in
    Array.iter (fun pair -> Hashtbl.replace group_names pair.group_name ()) vertex_groups;
    Array.iter (fun pair -> if Hashtbl.mem group_names pair.group_name then
      raise (Payload_error ("promotion_collision", Printf.sprintf
        "point group %S cannot be promoted because a vertex group has the same name"
        pair.group_name))) point_groups
  end

let copy_edge_group ?cancel ~grain view topology index pair =
  let output_index = Topology_index.Private.view index in
  let member edge =
    if edge land 4095 = 0 then Cancel.check_opt cancel;
    let first = output_index.edge_offsets.(edge)
    and last = output_index.edge_offsets.(edge + 1)
    and incident = ref 0 and found = ref false in
    incident := first;
    while not !found && !incident < last do
      let corner = output_index.edge_vertices.(!incident) in
      let source = view.Boolean_extract.Private.edge_source_offsets.(corner)
      and source_last = view.Boolean_extract.Private.edge_source_offsets.(corner + 1)
      and entry = ref 0 in
      entry := source;
      while not !found && !entry < source_last do
        let group =
          if Bytes.unsafe_get view.Boolean_extract.Private.edge_source_sides
              !entry = '\000'
          then pair.left_edge_group else pair.right_edge_group in
        (match group with
         | Some group when Edge_group.mem
             view.Boolean_extract.Private.edge_source_edges.(!entry) group ->
             found := true
         | None | Some _ -> ());
        incr entry
      done;
      incr incident
    done;
    !found in
  Edge_group.init ~grain ~topology ~index ~name:pair.edge_group_name member

let copy_points_and_vertices ?cancel ~grain ~point_conflict ~point_tolerance ancestry base =
  try
    if grain <= 0 then error "invalid_parameter" "grain must be positive"
    else if not (Float.is_finite point_tolerance) || point_tolerance < 0. then
      error "invalid_parameter" "point_tolerance must be finite and non-negative"
    else begin
      Cancel.check_opt cancel;
      let ancestry_base = Boolean_extract.geometry ancestry in
      if Geometry.positions base != Geometry.positions ancestry_base
          || Geometry.topology base != Geometry.topology ancestry_base then
        error "geometry_mismatch"
          "corner payload target does not share the Boolean ancestry topology"
      else begin
        let topology = Topology.Private.view (Geometry.topology base)
        and primitive_count = Geometry.primitive_count base
        and point_count = Geometry.point_count base
        and corner_count = Geometry.vertex_count base in
        if corner_count <> primitive_count * 3
            || Array.length topology.vertex_points <> corner_count then
          invalid_arg "Boolean corner payload requires triangular extraction topology";
        let representatives = first_corners topology point_count in
        if Array.exists (fun corner -> corner < 0) representatives then
          invalid_arg "Boolean corner payload found an unreferenced output point";
        let left = Boolean_extract.Private.left_geometry ancestry
        and right = Boolean_extract.Private.right_geometry ancestry in
        let view = Boolean_extract.Private.ancestry_view ancestry in
        let point_attributes = collect_attributes Attribute.Point left right
        and vertex_attributes = collect_attributes Attribute.Vertex left right
        and point_groups = collect_groups Group.Point left right
        and vertex_groups = collect_groups Group.Vertex left right
        and edge_groups = collect_edge_groups left right in
        assert_no_promotion_collisions point_conflict point_attributes vertex_attributes
          point_groups vertex_groups;
        let copy_attribute pair =
          let corner_storage = interpolate_csr ?cancel ~grain ancestry view
              corner_count pair in
          match pair.source_owner, point_conflict with
          | Attribute.Point, Reject ->
              create_attribute pair Attribute.Point
                (consolidate_to_points ~tolerance:point_tolerance pair.name topology
                   point_count representatives corner_storage)
          | Attribute.Point, Promote_to_vertex
          | Attribute.Vertex, _ -> create_attribute pair Attribute.Vertex corner_storage
          | _ -> assert false in
        let attributes = Array.append
            (Array.map copy_attribute point_attributes)
            (Array.map copy_attribute vertex_attributes)
        and groups = Array.append
            (Array.map (copy_group ?cancel ~grain ancestry view topology point_count
              representatives ~point_conflict) point_groups)
            (Array.map (copy_group ?cancel ~grain ancestry view topology point_count
              representatives ~point_conflict) vertex_groups) in
        let topology_value = Geometry.topology base in
        let topology_index = Topology_index.create ?cancel topology_value in
        let copied_edge_groups = Array.map
            (copy_edge_group ?cancel ~grain view topology_value topology_index)
            edge_groups in
        let copied_edge_names = Hashtbl.create (Array.length edge_groups) in
        Array.iter (fun pair -> Hashtbl.replace copied_edge_names
          pair.edge_group_name ()) edge_groups;
        let retained_attributes = List.filter (fun attribute ->
            match Attribute.owner attribute with
            | Attribute.Primitive | Attribute.Detail -> true
            | Attribute.Point | Attribute.Vertex -> false) (Geometry.attributes base)
        and retained_groups = List.filter (fun group ->
            Group.owner group = Group.Primitive) (Geometry.groups base)
        and retained_edge_groups = List.filter (fun group ->
            not (Hashtbl.mem copied_edge_names (Edge_group.name group)))
            (Geometry.edge_groups base) in
        match Geometry.create ~positions:(Geometry.positions base)
            ~topology:(Geometry.topology base)
            ~attributes:(retained_attributes @ Array.to_list attributes)
            ~groups:(retained_groups @ Array.to_list groups)
            ~edge_groups:(retained_edge_groups @ Array.to_list copied_edge_groups) () with
        | Ok geometry -> Ok geometry
        | Error message -> error "invalid_output" message
      end
    end
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean corner payload transfer was cancelled"
  | Payload_error (code, message) -> error code message
  | Invalid_argument message -> error "invalid_payload" message

let selected_primitive ancestry left right primitive =
  match Boolean_extract.primitive_side ancestry primitive with
  | Boolean_complex.Left -> left, Boolean_extract.primitive_face ancestry primitive
  | Boolean_complex.Right -> right, Boolean_extract.primitive_face ancestry primitive

let make_attribute pair storage =
  match Attribute.create_owned ~name:pair.name ~owner:Attribute.Primitive storage with
  | Ok value -> value
  | Error message -> invalid_arg message

let copy_attribute ?cancel ~grain ancestry primitive_count pair =
  let left, right = source_storage pair in
  match left, right with
  | (Some (Attribute.Float _) | None), (Some (Attribute.Float _) | None)
      when pair.kind = 0 ->
      let left = Option.map (function Attribute.Float value -> value | _ -> assert false) left
      and right = Option.map (function Attribute.Float value -> value | _ -> assert false) right
      and output = Array.make primitive_count 0. in
      Support.parallel_for ?cancel ~grain primitive_count (fun primitive ->
        let source, index = selected_primitive ancestry left right primitive in
        match source with None -> () | Some values -> output.(primitive) <- values.(index));
      make_attribute pair (Attribute.Float output)
  | (Some (Attribute.Int _) | None), (Some (Attribute.Int _) | None)
      when pair.kind = 1 ->
      let left = Option.map (function Attribute.Int value -> value | _ -> assert false) left
      and right = Option.map (function Attribute.Int value -> value | _ -> assert false) right
      and output = Array.make primitive_count 0 in
      Support.parallel_for ?cancel ~grain primitive_count (fun primitive ->
        let source, index = selected_primitive ancestry left right primitive in
        match source with None -> () | Some values -> output.(primitive) <- values.(index));
      make_attribute pair (Attribute.Int output)
  | (Some (Attribute.Text _) | None), (Some (Attribute.Text _) | None)
      when pair.kind = 7 ->
      let left = Option.map (function Attribute.Text value -> value | _ -> assert false) left
      and right = Option.map (function Attribute.Text value -> value | _ -> assert false) right
      and output = Array.make primitive_count "" in
      Support.parallel_for ?cancel ~grain primitive_count (fun primitive ->
        let source, index = selected_primitive ancestry left right primitive in
        match source with None -> () | Some values -> output.(primitive) <- values.(index));
      make_attribute pair (Attribute.Text output)
  | (Some (Attribute.Float2 _) | None), (Some (Attribute.Float2 _) | None)
      when pair.kind = 4 ->
      let view = function Attribute.Float2 value -> Packed.Float2.Private.view value
        | _ -> assert false in
      let left = Option.map view left and right = Option.map view right
      and x = Array.make primitive_count 0. and y = Array.make primitive_count 0. in
      Support.parallel_for ?cancel ~grain primitive_count (fun primitive ->
        let source, index = selected_primitive ancestry left right primitive in
        match source with None -> () | Some values ->
          x.(primitive) <- values.x.(index); y.(primitive) <- values.y.(index));
      let value = match Packed.Float2.of_owned ~x ~y with
        | Ok value -> value | Error message -> invalid_arg message in
      make_attribute pair (Attribute.Float2 value)
  | (Some (Attribute.Float3 _) | None), (Some (Attribute.Float3 _) | None)
      when pair.kind = 5 ->
      let view = function Attribute.Float3 value -> Packed.Float3.Private.view value
        | _ -> assert false in
      let left = Option.map view left and right = Option.map view right
      and x = Array.make primitive_count 0. and y = Array.make primitive_count 0.
      and z = Array.make primitive_count 0. in
      Support.parallel_for ?cancel ~grain primitive_count (fun primitive ->
        let source, index = selected_primitive ancestry left right primitive in
        match source with None -> () | Some values ->
          x.(primitive) <- values.x.(index); y.(primitive) <- values.y.(index);
          z.(primitive) <- values.z.(index));
      make_attribute pair (Attribute.Float3
        (Packed.Float3.Private.of_owned_exn ~x ~y ~z))
  | (Some (Attribute.Float4 _) | None), (Some (Attribute.Float4 _) | None)
      when pair.kind = 6 ->
      let view = function Attribute.Float4 value -> Packed.Float4.Private.view value
        | _ -> assert false in
      let left = Option.map view left and right = Option.map view right
      and x = Array.make primitive_count 0. and y = Array.make primitive_count 0.
      and z = Array.make primitive_count 0. and w = Array.make primitive_count 0. in
      Support.parallel_for ?cancel ~grain primitive_count (fun primitive ->
        let source, index = selected_primitive ancestry left right primitive in
        match source with None -> () | Some values ->
          x.(primitive) <- values.x.(index); y.(primitive) <- values.y.(index);
          z.(primitive) <- values.z.(index); w.(primitive) <- values.w.(index));
      let value = match Packed.Float4.of_owned ~x ~y ~z ~w with
        | Ok value -> value | Error message -> invalid_arg message in
      make_attribute pair (Attribute.Float4 value)
  | (Some (Attribute.Int_array _) | None), (Some (Attribute.Int_array _) | None)
      when pair.kind = 2 ->
      let view = function Attribute.Int_array value -> Packed.Int_array.Private.view value
        | _ -> assert false in
      let left = Option.map view left and right = Option.map view right
      and lengths = Array.make primitive_count 0 in
      Support.parallel_for ?cancel ~grain primitive_count (fun primitive ->
        let source, index = selected_primitive ancestry left right primitive in
        match source with None -> () | Some values ->
          lengths.(primitive) <- values.offsets.(index + 1) - values.offsets.(index));
      let offsets = Array.make (primitive_count + 1) 0 in
      for primitive = 0 to primitive_count - 1 do
        if offsets.(primitive) > Sys.max_array_length - lengths.(primitive) then
          invalid_arg "Boolean primitive integer-array payload exceeds array limits";
        offsets.(primitive + 1) <- offsets.(primitive) + lengths.(primitive)
      done;
      let values = Array.make offsets.(primitive_count) 0 in
      Support.parallel_for ?cancel ~grain primitive_count (fun primitive ->
        let source, index = selected_primitive ancestry left right primitive in
        match source with None -> () | Some source ->
          Array.blit source.values source.offsets.(index) values offsets.(primitive)
            lengths.(primitive));
      make_attribute pair (Attribute.Int_array
        (Packed.Int_array.Private.create_validated_owned ~offsets ~values))
  | (Some (Attribute.Float_array _) | None), (Some (Attribute.Float_array _) | None)
      when pair.kind = 3 ->
      let view = function Attribute.Float_array value -> Packed.Float_array.Private.view value
        | _ -> assert false in
      let left = Option.map view left and right = Option.map view right
      and lengths = Array.make primitive_count 0 in
      Support.parallel_for ?cancel ~grain primitive_count (fun primitive ->
        let source, index = selected_primitive ancestry left right primitive in
        match source with None -> () | Some values ->
          lengths.(primitive) <- values.offsets.(index + 1) - values.offsets.(index));
      let offsets = Array.make (primitive_count + 1) 0 in
      for primitive = 0 to primitive_count - 1 do
        if offsets.(primitive) > Sys.max_array_length - lengths.(primitive) then
          invalid_arg "Boolean primitive float-array payload exceeds array limits";
        offsets.(primitive + 1) <- offsets.(primitive) + lengths.(primitive)
      done;
      let values = Array.make offsets.(primitive_count) 0. in
      Support.parallel_for ?cancel ~grain primitive_count (fun primitive ->
        let source, index = selected_primitive ancestry left right primitive in
        match source with None -> () | Some source ->
          Array.blit source.values source.offsets.(index) values offsets.(primitive)
            lengths.(primitive));
      make_attribute pair (Attribute.Float_array
        (Packed.Float_array.Private.create_validated_owned ~offsets ~values))
  | _ -> invalid_arg (Printf.sprintf
      "primitive attribute %S has an inconsistent Boolean payload schema" pair.name)

let copy_primitives ?cancel ~grain ancestry =
  try
    if grain <= 0 then error "invalid_parameter" "grain must be positive"
    else begin
      Cancel.check_opt cancel;
      let left = Boolean_extract.Private.left_geometry ancestry
      and right = Boolean_extract.Private.right_geometry ancestry
      and base = Boolean_extract.geometry ancestry in
      let attribute_pairs = collect_attributes Attribute.Primitive left right
      and group_pairs = collect_groups Group.Primitive left right
      and primitive_count = Geometry.primitive_count base in
      let attributes = Array.map
          (copy_attribute ?cancel ~grain ancestry primitive_count) attribute_pairs in
      let groups = Array.map (fun pair ->
        let group = Group.init ~grain ~owner:Group.Primitive ~name:pair.group_name primitive_count
          (fun primitive ->
            if primitive land 4095 = 0 then Cancel.check_opt cancel;
            let source, index = selected_primitive ancestry pair.left_group pair.right_group primitive in
            match source with None -> false | Some group -> Group.mem index group) in
        let ordered =
          Option.fold ~none:false ~some:Group.is_ordered pair.left_group
          || Option.fold ~none:false ~some:Group.is_ordered pair.right_group in
        if not ordered then group
        else begin
          let ranks count source =
            let output = Array.make count max_int and rank = ref 0 in
            Option.iter (fun source -> Group.iter_ordered (fun element ->
              output.(element) <- !rank; incr rank) source) source;
            output in
          let left_ranks = ranks (Geometry.primitive_count left) pair.left_group
          and right_ranks = ranks (Geometry.primitive_count right) pair.right_group
          and keyed = Array.make (Group.cardinality group) (0, 0, 0)
          and next = ref 0 in
          let key primitive =
            let face = Boolean_extract.primitive_face ancestry primitive in
            match Boolean_extract.primitive_side ancestry primitive with
            | Boolean_complex.Left -> 0, left_ranks.(face), primitive
            | Boolean_complex.Right -> 1, right_ranks.(face), primitive in
          Group.iter (fun primitive ->
            keyed.(!next) <- key primitive; incr next) group;
          Array.sort Stdlib.compare keyed;
          Group.Private.with_owned_order
            (Array.map (fun (_, _, primitive) -> primitive) keyed) group
        end)
          group_pairs in
      let retained_attributes = List.filter
          (fun attribute -> Attribute.owner attribute <> Attribute.Primitive)
          (Geometry.attributes base)
      and retained_groups = List.filter
          (fun group -> Group.owner group <> Group.Primitive)
          (Geometry.groups base) in
      match Geometry.create ~positions:(Geometry.positions base)
          ~topology:(Geometry.topology base)
          ~attributes:(retained_attributes @ Array.to_list attributes)
          ~groups:(retained_groups @ Array.to_list groups)
          ~edge_groups:(Geometry.edge_groups base) () with
      | Ok geometry -> Ok geometry
      | Error message -> error "invalid_output" message
    end
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean primitive payload transfer was cancelled"
  | Payload_error (code, message) -> error code message
  | Invalid_argument message -> error "invalid_payload" message
