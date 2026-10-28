open Rays_math

let operation = "boolean_payload"
let error code message = Error (Error.make ~operation ~code message)

type attribute_pair = {
  name : string;
  kind : int;
  mutable left : Attribute.t option;
  mutable right : Attribute.t option;
}

type group_pair = {
  group_name : string;
  mutable left_group : Group.t option;
  mutable right_group : Group.t option;
}

let storage_kind = function
  | Attribute.Float _ -> 0
  | Attribute.Int _ -> 1
  | Attribute.Int_array _ -> 2
  | Attribute.Float_array _ -> 3
  | Attribute.Float2 _ -> 4
  | Attribute.Float3 _ -> 5
  | Attribute.Float4 _ -> 6
  | Attribute.Text _ -> 7

let collect_attributes left right =
  let table = Hashtbl.create 16 and reversed = ref [] in
  let add side attribute =
    if Attribute.owner attribute = Attribute.Primitive then begin
      let name = Attribute.name attribute
      and kind = storage_kind (Attribute.Private.storage attribute) in
      match Hashtbl.find_opt table name with
      | None ->
          let pair = { name; kind; left = None; right = None } in
          if side = 0 then pair.left <- Some attribute else pair.right <- Some attribute;
          Hashtbl.add table name pair;
          reversed := pair :: !reversed
      | Some pair ->
          if pair.kind <> kind then
            invalid_arg (Printf.sprintf
              "primitive attribute %S has different storage on the Boolean operands"
              name);
          if side = 0 then pair.left <- Some attribute else pair.right <- Some attribute
    end in
  Array.iter (add 0) (Geometry.Private.attributes left);
  Array.iter (add 1) (Geometry.Private.attributes right);
  Array.of_list (List.rev !reversed)

let collect_groups left right =
  let table = Hashtbl.create 16 and reversed = ref [] in
  let add side group =
    if Group.owner group = Group.Primitive then begin
      let name = Group.name group in
      match Hashtbl.find_opt table name with
      | None ->
          let pair = { group_name = name; left_group = None; right_group = None } in
          if side = 0 then pair.left_group <- Some group
          else pair.right_group <- Some group;
          Hashtbl.add table name pair;
          reversed := pair :: !reversed
      | Some pair ->
          if side = 0 then pair.left_group <- Some group
          else pair.right_group <- Some group
    end in
  List.iter (add 0) (Geometry.groups left);
  List.iter (add 1) (Geometry.groups right);
  Array.of_list (List.rev !reversed)

let selected ancestry left right primitive =
  match Boolean_extract.primitive_side ancestry primitive with
  | Boolean_complex.Left -> left, Boolean_extract.primitive_face ancestry primitive
  | Boolean_complex.Right -> right, Boolean_extract.primitive_face ancestry primitive

let source_storage attribute = Option.map Attribute.Private.storage attribute

let make_attribute pair storage =
  match Attribute.create_owned ~name:pair.name ~owner:Attribute.Primitive storage with
  | Ok value -> value
  | Error message -> invalid_arg message

let copy_attribute ?cancel ~grain ancestry primitive_count pair =
  let left = source_storage pair.left and right = source_storage pair.right in
  match left, right with
  | (Some (Attribute.Float _) | None), (Some (Attribute.Float _) | None)
      when pair.kind = 0 ->
      let left = Option.map (function Attribute.Float value -> value | _ -> assert false) left
      and right = Option.map (function Attribute.Float value -> value | _ -> assert false) right
      and output = Array.make primitive_count 0. in
      Support.parallel_for ?cancel ~grain primitive_count (fun primitive ->
        let source, index = selected ancestry left right primitive in
        match source with None -> () | Some values -> output.(primitive) <- values.(index));
      make_attribute pair (Attribute.Float output)
  | (Some (Attribute.Int _) | None), (Some (Attribute.Int _) | None)
      when pair.kind = 1 ->
      let left = Option.map (function Attribute.Int value -> value | _ -> assert false) left
      and right = Option.map (function Attribute.Int value -> value | _ -> assert false) right
      and output = Array.make primitive_count 0 in
      Support.parallel_for ?cancel ~grain primitive_count (fun primitive ->
        let source, index = selected ancestry left right primitive in
        match source with None -> () | Some values -> output.(primitive) <- values.(index));
      make_attribute pair (Attribute.Int output)
  | (Some (Attribute.Text _) | None), (Some (Attribute.Text _) | None)
      when pair.kind = 7 ->
      let left = Option.map (function Attribute.Text value -> value | _ -> assert false) left
      and right = Option.map (function Attribute.Text value -> value | _ -> assert false) right
      and output = Array.make primitive_count "" in
      Support.parallel_for ?cancel ~grain primitive_count (fun primitive ->
        let source, index = selected ancestry left right primitive in
        match source with None -> () | Some values -> output.(primitive) <- values.(index));
      make_attribute pair (Attribute.Text output)
  | (Some (Attribute.Float2 _) | None), (Some (Attribute.Float2 _) | None)
      when pair.kind = 4 ->
      let view = function Attribute.Float2 value -> Packed.Float2.Private.view value
        | _ -> assert false in
      let left = Option.map view left and right = Option.map view right
      and x = Array.make primitive_count 0. and y = Array.make primitive_count 0. in
      Support.parallel_for ?cancel ~grain primitive_count (fun primitive ->
        let source, index = selected ancestry left right primitive in
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
        let source, index = selected ancestry left right primitive in
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
        let source, index = selected ancestry left right primitive in
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
        let source, index = selected ancestry left right primitive in
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
        let source, index = selected ancestry left right primitive in
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
        let source, index = selected ancestry left right primitive in
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
        let source, index = selected ancestry left right primitive in
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
      let attribute_pairs = collect_attributes left right
      and group_pairs = collect_groups left right
      and primitive_count = Geometry.primitive_count base in
      let attributes = Array.map
          (copy_attribute ?cancel ~grain ancestry primitive_count) attribute_pairs in
      let groups = Array.map (fun pair ->
        let group = Group.init ~grain ~owner:Group.Primitive ~name:pair.group_name primitive_count
          (fun primitive ->
            if primitive land 4095 = 0 then Cancel.check_opt cancel;
            let source, index = selected ancestry pair.left_group pair.right_group primitive in
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
  | Invalid_argument message -> error "invalid_payload" message

type point_conflict = Boolean_corner_payload.point_conflict =
  | Reject
  | Promote_to_vertex

let copy_points_and_vertices = Boolean_corner_payload.copy
