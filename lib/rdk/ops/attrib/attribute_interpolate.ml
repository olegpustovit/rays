type spec = { source_owner : Attribute.owner; source_name : string; target_name : string }
type group_spec = { group_source_owner : Attribute.owner; group_source : Group.t }
type unmatched = Keep_target | Default_value
type weighted_source = Points | Vertices | Primitives

type computed = {
  computed_owner : weighted_source;
  computed_numbers_attribute : string;
  computed_weights_attribute : string;
}

type plane =
  | Float_plane of { source : float array; output : float array; existing : float array option }
  | Int_plane of { source : int array; output : int array; existing : int array option }
  | Text_plane of { source : string array; output : string array; existing : string array option }
  | Float2_plane of {
      source : Packed.Float2.View.view;
      output : Packed.Float2.View.view;
      existing : Packed.Float2.View.view option;
    }
  | Float3_plane of {
      source : Packed.Float3.View.view;
      output : Packed.Float3.View.view;
      existing : Packed.Float3.View.view option;
    }
  | Float4_plane of {
      source : Packed.Float4.View.view;
      output : Packed.Float4.View.view;
      existing : Packed.Float4.View.view option;
    }
  | Int_array_plane of {
      source : Packed.Int_array.View.view;
      existing : Packed.Int_array.View.view option;
      choices : int array;
    }
  | Float_array_plane of {
      source : Packed.Float_array.View.view;
      existing : Packed.Float_array.View.view option;
      choices : int array;
    }

type job = {
  source_owner : Attribute.owner;
  target_owner : Attribute.owner;
  target_name : string;
  position : bool;
  normalize : bool;
  plane : plane;
}

type group_job = {
  group_source_owner : Attribute.owner;
  group_source_bits : bytes;
  group_target_owner : Group.owner;
  group_target_name : string;
  group_existing_bits : bytes option;
  group_output_bits : bytes;
  group_target_count : int;
}

let owner_count geometry = function
  | Attribute.Point -> Geometry.point_count geometry
  | Attribute.Vertex -> Geometry.vertex_count geometry
  | Attribute.Primitive -> Geometry.primitive_count geometry
  | Attribute.Detail -> 1

let owner_name = function
  | Attribute.Point -> "point"
  | Attribute.Vertex -> "vertex"
  | Attribute.Primitive -> "primitive"
  | Attribute.Detail -> "detail"

let group_owner = function
  | Attribute.Point -> Some Group.Point
  | Attribute.Vertex -> Some Group.Vertex
  | Attribute.Primitive -> Some Group.Primitive
  | Attribute.Detail -> None

let attribute_owner_of_group = function
  | Group.Point -> Attribute.Point
  | Group.Vertex -> Attribute.Vertex
  | Group.Primitive -> Attribute.Primitive

let[@inline always] bit_set bits index member =
  let byte = index lsr 3 and mask = 1 lsl (index land 7) in
  let value = Char.code (Bytes.unsafe_get bits byte) in
  Bytes.unsafe_set bits byte
    (Char.chr (if member then value lor mask else value land (lnot mask land 255)))

let create_group_jobs ~target_owner ~target groups =
  match group_owner target_owner with
  | None when groups <> [] ->
      Error "Attribute Interpolate: detail destinations cannot receive groups"
  | None -> Ok [||]
  | Some target_group_owner ->
      let target_count = owner_count target target_owner
      and seen = Hashtbl.create (List.length groups) in
      let rec create output = function
        | [] -> Ok (Array.of_list (List.rev output))
        | spec :: rest ->
            let source = spec.group_source in
            if attribute_owner_of_group (Group.owner source) <> spec.group_source_owner then
              Error "Attribute Interpolate: internal source-group ownership mismatch"
            else if Hashtbl.mem seen (Group.name source) then
              Error
                (Printf.sprintf "Attribute Interpolate: duplicate target group %S"
                   (Group.name source))
            else begin
              Hashtbl.add seen (Group.name source) ();
              let existing =
                Geometry.find_group ~owner:target_group_owner (Group.name source) target
              in
              let existing_bits = Option.map Group.View.bits_view existing in
              let output_bits =
                match existing_bits with
                | None -> Bytes.make ((target_count + 7) / 8) '\000'
                | Some bits -> Bytes.copy bits
              in
              create
                ({
                   group_source_owner = spec.group_source_owner;
                   group_source_bits = Group.View.bits_view source;
                   group_target_owner = target_group_owner;
                   group_target_name = Group.name source;
                   group_existing_bits = existing_bits;
                   group_output_bits = output_bits;
                   group_target_count = target_count;
                 }
                :: output)
                rest
            end
      in
      create [] groups

let selection_elements owner selection geometry =
  let count = owner_count geometry owner in
  match selection with
  | None -> Ok (count, fun rank -> rank)
  | Some group -> (
      match group_owner owner with
      | None -> Error "Attribute Interpolate: detail attributes do not accept a group"
      | Some expected when Group.owner group <> expected || Group.length group <> count ->
          Error
            (Printf.sprintf "Attribute Interpolate: selection must be a matching %s group"
               (owner_name owner))
      | Some _ ->
          let elements = Array.make (Group.cardinality group) 0 and next = ref 0 in
          Group.iter_ordered
            (fun element ->
              elements.(!next) <- element;
              incr next)
            group;
          Ok (Array.length elements, fun rank -> elements.(rank)))

let source_storage source_owner source_name source =
  if source_owner = Attribute.Point && String.equal source_name "P" then
    Ok (Attribute.Float3 (Geometry.positions source))
  else
    match Geometry.find_attribute ~owner:source_owner source_name source with
    | None ->
        Error
          (Printf.sprintf "Attribute Interpolate: source is missing %s attribute %S"
             (owner_name source_owner) source_name)
    | Some attribute -> Ok (Attribute.View.storage attribute)

let target_storage target_owner target_name target =
  if target_owner = Attribute.Point && String.equal target_name "P" then
    Some (Attribute.Float3 (Geometry.positions target))
  else
    Option.map Attribute.View.storage
      (Geometry.find_attribute ~owner:target_owner target_name target)

let kind_matches source target =
  match (source, target) with
  | Attribute.Float _, Attribute.Float _
  | Attribute.Int _, Attribute.Int _
  | Attribute.Text _, Attribute.Text _
  | Attribute.Float2 _, Attribute.Float2 _
  | Attribute.Float3 _, Attribute.Float3 _
  | Attribute.Float4 _, Attribute.Float4 _
  | Attribute.Int_array _, Attribute.Int_array _
  | Attribute.Float_array _, Attribute.Float_array _ ->
      true
  | _ -> false

let copy_or_zero_float count = function
  | Some (Attribute.Float values) -> Array.copy values
  | None -> Array.make count 0.
  | Some _ -> assert false

let copy_or_zero_int count = function
  | Some (Attribute.Int values) -> Array.copy values
  | None -> Array.make count 0
  | Some _ -> assert false

let copy_or_empty_text count = function
  | Some (Attribute.Text values) -> Array.copy values
  | None -> Array.make count ""
  | Some _ -> assert false

let create_job ~target_owner ~source ~target spec =
  if String.trim spec.source_name = "" || String.trim spec.target_name = "" then
    Error "Attribute Interpolate: attribute names must not be empty"
  else if String.equal spec.source_name "P" && spec.source_owner <> Attribute.Point then
    Error "Attribute Interpolate: canonical source P must be point owned"
  else if String.equal spec.target_name "P" && target_owner <> Attribute.Point then
    Error "Attribute Interpolate: canonical target P must be point owned"
  else
    Result.bind (source_storage spec.source_owner spec.source_name source) (fun source_storage ->
        let existing = target_storage target_owner spec.target_name target in
        if
          match existing with
          | Some target -> not (kind_matches source_storage target)
          | None -> false
        then
          Error
            (Printf.sprintf "Attribute Interpolate: target attribute %S has incompatible storage"
               spec.target_name)
        else
          let count = owner_count target target_owner
          and position = target_owner = Attribute.Point && String.equal spec.target_name "P" in
          let normalize = String.equal spec.target_name "N" in
          let plane =
            match source_storage with
            | Attribute.Float source ->
                Float_plane
                  {
                    source;
                    output = copy_or_zero_float count existing;
                    existing =
                      Option.map
                        (function Attribute.Float values -> values | _ -> assert false)
                        existing;
                  }
            | Attribute.Int source ->
                Int_plane
                  {
                    source;
                    output = copy_or_zero_int count existing;
                    existing =
                      Option.map
                        (function Attribute.Int values -> values | _ -> assert false)
                        existing;
                  }
            | Attribute.Text source ->
                Text_plane
                  {
                    source;
                    output = copy_or_empty_text count existing;
                    existing =
                      Option.map
                        (function Attribute.Text values -> values | _ -> assert false)
                        existing;
                  }
            | Attribute.Float2 source ->
                let source = Packed.Float2.View.view source in
                let existing =
                  Option.map
                    (function
                      | Attribute.Float2 values -> Packed.Float2.View.view values
                      | _ -> assert false)
                    existing
                in
                let output =
                  Packed.Float2.View.view
                    (Packed.Float2.of_owned
                       ~x:
                         (match existing with
                         | Some values -> Array.copy values.x
                         | None -> Array.make count 0.)
                       ~y:
                         (match existing with
                         | Some values -> Array.copy values.y
                         | None -> Array.make count 0.)
                    |> Result.get_ok)
                in
                Float2_plane { source; output; existing }
            | Attribute.Float3 source ->
                let source = Packed.Float3.View.view source in
                let existing =
                  Option.map
                    (function
                      | Attribute.Float3 values -> Packed.Float3.View.view values
                      | _ -> assert false)
                    existing
                in
                let output =
                  Packed.Float3.View.view
                    (Packed.Float3.View.of_owned_exn
                       ~x:
                         (match existing with
                         | Some values -> Array.copy values.x
                         | None -> Array.make count 0.)
                       ~y:
                         (match existing with
                         | Some values -> Array.copy values.y
                         | None -> Array.make count 0.)
                       ~z:
                         (match existing with
                         | Some values -> Array.copy values.z
                         | None -> Array.make count 0.))
                in
                Float3_plane { source; output; existing }
            | Attribute.Float4 source ->
                let source = Packed.Float4.View.view source in
                let existing =
                  Option.map
                    (function
                      | Attribute.Float4 values -> Packed.Float4.View.view values
                      | _ -> assert false)
                    existing
                in
                let output =
                  Packed.Float4.View.view
                    (Packed.Float4.of_owned
                       ~x:
                         (match existing with
                         | Some values -> Array.copy values.x
                         | None -> Array.make count 0.)
                       ~y:
                         (match existing with
                         | Some values -> Array.copy values.y
                         | None -> Array.make count 0.)
                       ~z:
                         (match existing with
                         | Some values -> Array.copy values.z
                         | None -> Array.make count 0.)
                       ~w:
                         (match existing with
                         | Some values -> Array.copy values.w
                         | None -> Array.make count 0.)
                    |> Result.get_ok)
                in
                Float4_plane { source; output; existing }
            | Attribute.Int_array source ->
                let existing =
                  Option.map
                    (function
                      | Attribute.Int_array values -> Packed.Int_array.View.view values
                      | _ -> assert false)
                    existing
                in
                Int_array_plane
                  {
                    source = Packed.Int_array.View.view source;
                    existing;
                    choices = Array.make count (-1);
                  }
            | Attribute.Float_array source ->
                let existing =
                  Option.map
                    (function
                      | Attribute.Float_array values -> Packed.Float_array.View.view values
                      | _ -> assert false)
                    existing
                in
                Float_array_plane
                  {
                    source = Packed.Float_array.View.view source;
                    existing;
                    choices = Array.make count (-1);
                  }
          in
          Ok
            {
              source_owner = spec.source_owner;
              target_owner;
              target_name = spec.target_name;
              position;
              normalize;
              plane;
            })

let[@inline always] polygon_weight size u v local =
  if size = 3 then if local = 0 then 1. -. u -. v else if local = 1 then u else v
  else if size = 4 then
    if local = 0 then (1. -. u) *. (1. -. v)
    else if local = 1 then (1. -. u) *. v
    else if local = 2 then u *. v
    else u *. (1. -. v)
  else begin
    let scaled = u *. float_of_int size in
    let base = int_of_float (floor scaled) in
    let edge = ((base mod size) + size) mod size in
    let next = if edge + 1 = size then 0 else edge + 1 in
    let t = scaled -. floor scaled in
    let boundary = if local = edge then 1. -. t else if local = next then t else 0. in
    ((1. -. v) *. boundary) +. (v /. float_of_int size)
  end

let[@inline always] curve_weight ~closed size u local =
  let edges = if closed then size else size - 1 in
  let scaled = u *. float_of_int edges in
  let base = int_of_float (floor scaled) in
  let t = scaled -. floor scaled in
  if closed then begin
    let edge = ((base mod size) + size) mod size in
    let next = if edge + 1 = size then 0 else edge + 1 in
    if local = edge then 1. -. t else if local = next then t else 0.
  end
  else if base < 0 then if local = 0 then 1. -. scaled else if local = 1 then scaled else 0.
  else if base >= size - 1 then
    let t = scaled -. float_of_int (size - 2) in
    if local = size - 2 then 1. -. t else if local = size - 1 then t else 0.
  else if local = base then 1. -. t
  else if local = base + 1 then t
  else 0.

let[@inline always] zero_numeric job destination =
  match job.plane with
  | Float_plane values -> values.output.(destination) <- 0.
  | Float2_plane values ->
      values.output.x.(destination) <- 0.;
      values.output.y.(destination) <- 0.
  | Float3_plane values ->
      values.output.x.(destination) <- 0.;
      values.output.y.(destination) <- 0.;
      values.output.z.(destination) <- 0.
  | Float4_plane values ->
      values.output.x.(destination) <- 0.;
      values.output.y.(destination) <- 0.;
      values.output.z.(destination) <- 0.;
      values.output.w.(destination) <- 0.
  | Int_plane _ | Text_plane _ | Int_array_plane _ | Float_array_plane _ -> ()

let[@inline always] add_numeric job destination source weight =
  match job.plane with
  | Float_plane values ->
      values.output.(destination) <-
        values.output.(destination) +. (weight *. values.source.(source))
  | Float2_plane values ->
      values.output.x.(destination) <-
        values.output.x.(destination) +. (weight *. values.source.x.(source));
      values.output.y.(destination) <-
        values.output.y.(destination) +. (weight *. values.source.y.(source))
  | Float3_plane values ->
      values.output.x.(destination) <-
        values.output.x.(destination) +. (weight *. values.source.x.(source));
      values.output.y.(destination) <-
        values.output.y.(destination) +. (weight *. values.source.y.(source));
      values.output.z.(destination) <-
        values.output.z.(destination) +. (weight *. values.source.z.(source))
  | Float4_plane values ->
      values.output.x.(destination) <-
        values.output.x.(destination) +. (weight *. values.source.x.(source));
      values.output.y.(destination) <-
        values.output.y.(destination) +. (weight *. values.source.y.(source));
      values.output.z.(destination) <-
        values.output.z.(destination) +. (weight *. values.source.z.(source));
      values.output.w.(destination) <-
        values.output.w.(destination) +. (weight *. values.source.w.(source))
  | Int_plane _ | Text_plane _ | Int_array_plane _ | Float_array_plane _ -> ()

let[@inline always] set_discrete job destination source =
  match job.plane with
  | Int_plane values -> values.output.(destination) <- values.source.(source)
  | Text_plane values -> values.output.(destination) <- values.source.(source)
  | Int_array_plane values -> values.choices.(destination) <- source
  | Float_array_plane values -> values.choices.(destination) <- source
  | Float_plane _ | Float2_plane _ | Float3_plane _ | Float4_plane _ -> ()

let[@inline always] set_numeric job destination source =
  match job.plane with
  | Float_plane values -> values.output.(destination) <- values.source.(source)
  | Float2_plane values ->
      values.output.x.(destination) <- values.source.x.(source);
      values.output.y.(destination) <- values.source.y.(source)
  | Float3_plane values ->
      values.output.x.(destination) <- values.source.x.(source);
      values.output.y.(destination) <- values.source.y.(source);
      values.output.z.(destination) <- values.source.z.(source)
  | Float4_plane values ->
      values.output.x.(destination) <- values.source.x.(source);
      values.output.y.(destination) <- values.source.y.(source);
      values.output.z.(destination) <- values.source.z.(source);
      values.output.w.(destination) <- values.source.w.(source)
  | Int_plane _ | Text_plane _ | Int_array_plane _ | Float_array_plane _ -> ()

let is_numeric job =
  match job.plane with
  | Float_plane _ | Float2_plane _ | Float3_plane _ | Float4_plane _ -> true
  | Int_plane _ | Text_plane _ | Int_array_plane _ | Float_array_plane _ -> false

let select_jobs jobs owner numeric =
  Array.fold_right
    (fun job selected ->
      if job.source_owner = owner && Bool.equal (is_numeric job) numeric then job :: selected
      else selected)
    jobs []
  |> Array.of_list

let[@inline always] old_float existing destination =
  match existing with None -> 0. | Some values -> values.(destination)

let[@inline always] blend_job job destination blend =
  match job.plane with
  | Float_plane values ->
      let old = old_float values.existing destination in
      values.output.(destination) <- old +. (blend *. (values.output.(destination) -. old))
  | Float2_plane values ->
      let old_x = match values.existing with None -> 0. | Some old -> old.x.(destination) in
      let old_y = match values.existing with None -> 0. | Some old -> old.y.(destination) in
      values.output.x.(destination) <- old_x +. (blend *. (values.output.x.(destination) -. old_x));
      values.output.y.(destination) <- old_y +. (blend *. (values.output.y.(destination) -. old_y))
  | Float3_plane values ->
      let old_x = match values.existing with None -> 0. | Some old -> old.x.(destination) in
      let old_y = match values.existing with None -> 0. | Some old -> old.y.(destination) in
      let old_z = match values.existing with None -> 0. | Some old -> old.z.(destination) in
      values.output.x.(destination) <- old_x +. (blend *. (values.output.x.(destination) -. old_x));
      values.output.y.(destination) <- old_y +. (blend *. (values.output.y.(destination) -. old_y));
      values.output.z.(destination) <- old_z +. (blend *. (values.output.z.(destination) -. old_z));
      if job.normalize && blend > 0. then begin
        let x = values.output.x.(destination)
        and y = values.output.y.(destination)
        and z = values.output.z.(destination) in
        let length = sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
        if length > 1e-20 then begin
          values.output.x.(destination) <- x /. length;
          values.output.y.(destination) <- y /. length;
          values.output.z.(destination) <- z /. length
        end
      end
  | Float4_plane values ->
      let old_x = match values.existing with None -> 0. | Some old -> old.x.(destination) in
      let old_y = match values.existing with None -> 0. | Some old -> old.y.(destination) in
      let old_z = match values.existing with None -> 0. | Some old -> old.z.(destination) in
      let old_w = match values.existing with None -> 0. | Some old -> old.w.(destination) in
      values.output.x.(destination) <- old_x +. (blend *. (values.output.x.(destination) -. old_x));
      values.output.y.(destination) <- old_y +. (blend *. (values.output.y.(destination) -. old_y));
      values.output.z.(destination) <- old_z +. (blend *. (values.output.z.(destination) -. old_z));
      values.output.w.(destination) <- old_w +. (blend *. (values.output.w.(destination) -. old_w))
  | Int_plane values ->
      if blend < 0.5 then
        values.output.(destination) <-
          (match values.existing with None -> 0 | Some old -> old.(destination))
  | Text_plane values ->
      if blend < 0.5 then
        values.output.(destination) <-
          (match values.existing with None -> "" | Some old -> old.(destination))
  | Int_array_plane values -> if blend < 0.5 then values.choices.(destination) <- -1
  | Float_array_plane values -> if blend < 0.5 then values.choices.(destination) <- -1

let[@inline always] default_job job destination =
  match job.plane with
  | Float_plane values -> values.output.(destination) <- 0.
  | Int_plane values -> values.output.(destination) <- 0
  | Text_plane values -> values.output.(destination) <- ""
  | Float2_plane values ->
      values.output.x.(destination) <- 0.;
      values.output.y.(destination) <- 0.
  | Float3_plane values ->
      values.output.x.(destination) <- 0.;
      values.output.y.(destination) <- 0.;
      values.output.z.(destination) <- 0.
  | Float4_plane values ->
      values.output.x.(destination) <- 0.;
      values.output.y.(destination) <- 0.;
      values.output.z.(destination) <- 0.;
      values.output.w.(destination) <- 0.
  | Int_array_plane values -> values.choices.(destination) <- -2
  | Float_array_plane values -> values.choices.(destination) <- -2

let array_offsets ?cancel ~operation ~source_offsets ~existing_offsets choices =
  let count = Array.length choices and source_count = Array.length source_offsets - 1 in
  let offsets = Array.make (count + 1) 0 in
  for destination = 0 to count - 1 do
    Cancel.poll cancel destination 4095;
    let choice = choices.(destination) in
    let length =
      if choice >= 0 then begin
        if choice >= source_count then
          invalid_arg (operation ^ ": internal source row is out of bounds");
        source_offsets.(choice + 1) - source_offsets.(choice)
      end
      else if choice = -1 then
        match existing_offsets with
        | None -> 0
        | Some offsets -> offsets.(destination + 1) - offsets.(destination)
      else if choice = -2 then 0
      else invalid_arg (operation ^ ": invalid internal row choice")
    in
    if length > max_int - offsets.(destination) then
      invalid_arg (operation ^ ": packed value count overflow");
    offsets.(destination + 1) <- offsets.(destination) + length
  done;
  offsets

let materialize_int_array ?cancel ~grain source existing choices =
  let existing_offsets =
    Option.map (fun (view : Packed.Int_array.View.view) -> view.offsets) existing
  in
  let offsets =
    array_offsets ?cancel ~operation:"Attribute Interpolate int array"
      ~source_offsets:source.Packed.Int_array.View.offsets ~existing_offsets choices
  in
  let count = Array.length choices and values = Array.make offsets.(Array.length choices) 0 in
  if count > 0 then
    Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1) (fun destination ->
        Cancel.poll cancel destination 4095;
        let choice = choices.(destination) in
        if choice >= 0 then
          Array.blit source.values source.offsets.(choice) values offsets.(destination)
            (offsets.(destination + 1) - offsets.(destination))
        else if choice = -1 then
          match existing with
          | None -> ()
          | Some existing ->
              Array.blit existing.values existing.offsets.(destination) values offsets.(destination)
                (offsets.(destination + 1) - offsets.(destination))
        else ());
  Packed.Int_array.View.create_validated_owned ~offsets ~values

let materialize_float_array ?cancel ~grain source existing choices =
  let existing_offsets =
    Option.map (fun (view : Packed.Float_array.View.view) -> view.offsets) existing
  in
  let offsets =
    array_offsets ?cancel ~operation:"Attribute Interpolate float array"
      ~source_offsets:source.Packed.Float_array.View.offsets ~existing_offsets choices
  in
  let count = Array.length choices and values = Array.make offsets.(Array.length choices) 0. in
  if count > 0 then
    Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1) (fun destination ->
        Cancel.poll cancel destination 4095;
        let choice = choices.(destination) in
        if choice >= 0 then
          Array.blit source.values source.offsets.(choice) values offsets.(destination)
            (offsets.(destination + 1) - offsets.(destination))
        else if choice = -1 then
          match existing with
          | None -> ()
          | Some existing ->
              Array.blit existing.values existing.offsets.(destination) values offsets.(destination)
                (offsets.(destination + 1) - offsets.(destination))
        else ());
  Packed.Float_array.View.create_validated_owned ~offsets ~values

let storage_of_job ?cancel ~grain job =
  match job.plane with
  | Float_plane values -> Attribute.Float values.output
  | Int_plane values -> Attribute.Int values.output
  | Text_plane values -> Attribute.Text values.output
  | Float2_plane values ->
      Attribute.Float2
        (Packed.Float2.of_owned ~x:values.output.x ~y:values.output.y |> Result.get_ok)
  | Float3_plane values ->
      Attribute.Float3
        (Packed.Float3.View.of_owned_exn ~x:values.output.x ~y:values.output.y ~z:values.output.z)
  | Float4_plane values ->
      Attribute.Float4
        (Packed.Float4.of_owned ~x:values.output.x ~y:values.output.y ~z:values.output.z
           ~w:values.output.w
        |> Result.get_ok)
  | Int_array_plane values ->
      Attribute.Int_array
        (materialize_int_array ?cancel ~grain values.source values.existing values.choices)
  | Float_array_plane values ->
      Attribute.Float_array
        (materialize_float_array ?cancel ~grain values.source values.existing values.choices)

let groups_of_jobs jobs =
  Array.map
    (fun job ->
      Group.View.of_owned_bits ~owner:job.group_target_owner ~name:job.group_target_name
        ~length:job.group_target_count job.group_output_bits)
    jobs

let[@inline always] existing_group_value job destination =
  match job.group_existing_bits with
  | Some bits when Support.Bits.mem bits destination -> 1.
  | None | Some _ -> 0.

let[@inline always] commit_group_score job destination ~blend score =
  let old = existing_group_value job destination in
  bit_set job.group_output_bits destination (old +. (blend *. (score -. old)) >= 0.5)

let interpolate_primitive_groups ?cancel ?(grain = 16_384) ?selection ~unmatched ~blend ~pre_scale
    ~primitive_numbers ~uvw ~topology ~primitive_count jobs =
  let topology : Topology.View.view = topology and uvw : Packed.Float3.View.view = uvw in
  if Array.length jobs = 0 then ()
  else
    let target_count = jobs.(0).group_target_count in
    let byte_count = (target_count + 7) / 8 in
    if byte_count > 0 then
      Parallel.for_
        ~chunk_size:(max 1 (grain / 8))
        ~start:0 ~finish:(byte_count - 1)
        (fun byte ->
          Cancel.poll cancel byte 511;
          let first_destination = byte lsl 3 in
          let last_destination = min target_count (first_destination + 8) in
          for destination = first_destination to last_destination - 1 do
            if match selection with None -> true | Some group -> Group.mem destination group then begin
              let primitive = primitive_numbers.(destination) in
              if primitive < 0 || primitive >= primitive_count then begin
                if unmatched = Default_value then
                  for job_index = 0 to Array.length jobs - 1 do
                    bit_set jobs.(job_index).group_output_bits destination false
                  done
              end
              else begin
                let first = topology.primitive_offsets.(primitive)
                and last = topology.primitive_offsets.(primitive + 1) in
                let size = last - first
                and u = uvw.x.(destination) *. pre_scale
                and v = uvw.y.(destination) *. pre_scale
                and kind = Char.code (Bytes.unsafe_get topology.primitive_kinds primitive) in
                for job_index = 0 to Array.length jobs - 1 do
                  let job = jobs.(job_index) in
                  let score =
                    match job.group_source_owner with
                    | Attribute.Primitive ->
                        if Support.Bits.mem job.group_source_bits primitive then 1. else 0.
                    | Attribute.Point | Attribute.Vertex ->
                        let score = ref 0. in
                        for local = 0 to size - 1 do
                          let weight =
                            match kind with
                            | 0 -> polygon_weight size u v local
                            | 1 -> curve_weight ~closed:false size u local
                            | 2 -> curve_weight ~closed:true size u local
                            | _ -> assert false
                          in
                          let vertex = first + local in
                          let source_element =
                            if job.group_source_owner = Attribute.Point then
                              topology.vertex_points.(vertex)
                            else vertex
                          in
                          if Support.Bits.mem job.group_source_bits source_element then
                            score := !score +. weight
                        done;
                        !score
                    | Attribute.Detail -> assert false
                  in
                  commit_group_score job destination ~blend score
                done
              end
            end
          done)

let install ?cancel ~grain ?(extra = [||]) ?(groups = [||]) jobs target =
  let position = ref None and attributes = ref [] and failure = ref None in
  Array.iter
    (fun job ->
      if Option.is_none !failure then
        let storage = storage_of_job ?cancel ~grain job in
        if job.position then
          match storage with
          | Attribute.Float3 positions -> position := Some positions
          | _ -> failure := Some "Attribute Interpolate: P requires float3 storage"
        else
          match Attribute.create_owned ~owner:job.target_owner ~name:job.target_name storage with
          | Error message -> failure := Some message
          | Ok attribute -> attributes := attribute :: !attributes)
    jobs;
  match !failure with
  | Some message -> Error message
  | None ->
      let attributes = Array.append (Array.of_list (List.rev !attributes)) extra in
      Geometry.View.with_merged_owned ?positions:!position ~attributes ~groups target

let compute_primitive_weights ?cancel ?(grain = 16_384) ?selection ~unmatched ~target_owner
    ~computed ~primitive_numbers ~uvw ~pre_scale ~source ~target () =
  if computed.computed_owner = Primitives then
    Error "Attribute Interpolate: computed weights support point or vertex numbers"
  else if
    String.trim computed.computed_numbers_attribute = ""
    || String.trim computed.computed_weights_attribute = ""
  then Error "Attribute Interpolate: computed attribute names must not be empty"
  else if String.equal computed.computed_numbers_attribute computed.computed_weights_attribute then
    Error "Attribute Interpolate: computed number and weight attributes must have different names"
  else if
    target_owner = Attribute.Point
    && (String.equal computed.computed_numbers_attribute "P"
       || String.equal computed.computed_weights_attribute "P")
  then Error "Attribute Interpolate: canonical P cannot store computed arrays"
  else
    let existing_numbers =
      Geometry.find_attribute ~owner:target_owner computed.computed_numbers_attribute target
    and existing_weights =
      Geometry.find_attribute ~owner:target_owner computed.computed_weights_attribute target
    in
    let existing_result =
      match (existing_numbers, existing_weights) with
      | None, None -> Ok None
      | Some numbers, Some weights -> (
          match (Attribute.View.storage numbers, Attribute.View.storage weights) with
          | Attribute.Int_array numbers, Attribute.Float_array weights ->
              let numbers = Packed.Int_array.View.view numbers
              and weights = Packed.Float_array.View.view weights in
              if numbers.offsets = weights.offsets then Ok (Some (numbers, weights))
              else Error "Attribute Interpolate: existing computed array row layouts differ"
          | _ ->
              Error
                "Attribute Interpolate: existing computed attributes must be int-array and \
                 float-array")
      | Some _, None | None, Some _ ->
          Error
            "Attribute Interpolate: existing computed number and weight attributes must occur \
             together"
    in
    Result.bind existing_result (fun existing ->
        let uvw : Packed.Float3.View.view = uvw in
        let topology = Topology.View.view (Geometry.topology source)
        and primitive_count = Geometry.primitive_count source
        and target_count = owner_count target target_owner in
        let selected destination =
          match selection with None -> true | Some group -> Group.mem destination group
        in
        let preserve destination primitive =
          (not (selected destination))
          || (unmatched = Keep_target && (primitive < 0 || primitive >= primitive_count))
        in
        let row_length destination =
          let primitive = primitive_numbers.(destination) in
          if preserve destination primitive then
            match existing with
            | None -> 0
            | Some (numbers, _) -> numbers.offsets.(destination + 1) - numbers.offsets.(destination)
          else if primitive < 0 || primitive >= primitive_count then 0
          else topology.primitive_offsets.(primitive + 1) - topology.primitive_offsets.(primitive)
        in
        let lengths = Array.make target_count 0 in
        if target_count > 0 then
          Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(target_count - 1) (fun destination ->
              Cancel.poll cancel destination 4095;
              lengths.(destination) <- row_length destination);
        let offsets = Array.make (target_count + 1) 0 in
        for destination = 0 to target_count - 1 do
          Cancel.poll cancel destination 4095;
          if lengths.(destination) > max_int - offsets.(destination) then
            invalid_arg "Attribute Interpolate: computed weight cardinality overflow";
          offsets.(destination + 1) <- offsets.(destination) + lengths.(destination)
        done;
        let numbers = Array.make offsets.(target_count) 0
        and weights = Array.make offsets.(target_count) 0. in
        if target_count > 0 then
          Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(target_count - 1) (fun destination ->
              Cancel.poll cancel destination 4095;
              let primitive = primitive_numbers.(destination)
              and output_first = offsets.(destination) in
              if preserve destination primitive then (
                match existing with
                | None -> ()
                | Some (old_numbers, old_weights) ->
                    let old_first = old_numbers.offsets.(destination)
                    and length = lengths.(destination) in
                    Array.blit old_numbers.values old_first numbers output_first length;
                    Array.blit old_weights.values old_first weights output_first length)
              else if primitive >= 0 && primitive < primitive_count then begin
                let first = topology.primitive_offsets.(primitive)
                and last = topology.primitive_offsets.(primitive + 1) in
                let size = last - first
                and u = uvw.x.(destination) *. pre_scale
                and v = uvw.y.(destination) *. pre_scale
                and kind = Char.code (Bytes.unsafe_get topology.primitive_kinds primitive) in
                for local = 0 to size - 1 do
                  let vertex = first + local in
                  numbers.(output_first + local) <-
                    (if computed.computed_owner = Points then topology.vertex_points.(vertex)
                     else vertex);
                  weights.(output_first + local) <-
                    (match kind with
                    | 0 -> polygon_weight size u v local
                    | 1 -> curve_weight ~closed:false size u local
                    | 2 -> curve_weight ~closed:true size u local
                    | _ -> assert false)
                done
              end);
        let numbers = Packed.Int_array.View.create_validated_owned ~offsets ~values:numbers
        and weights =
          Packed.Float_array.View.create_validated_owned ~offsets:(Array.copy offsets)
            ~values:weights
        in
        Result.bind
          (Attribute.create_owned ~owner:target_owner ~name:computed.computed_numbers_attribute
             (Attribute.Int_array numbers)) (fun numbers ->
            Result.map
              (fun weights -> [| numbers; weights |])
              (Attribute.create_owned ~owner:target_owner ~name:computed.computed_weights_attribute
                 (Attribute.Float_array weights))))

let interpolate_primitive ?cancel ?(grain = 16_384) ?selection ?compute
    ?(primitive_attribute = "source_primitive") ?(uvw_attribute = "source_uvw") ?(pre_scale = 1.)
    ?(blend = 1.) ?(unmatched = Keep_target) ~target_owner ~attributes ~groups ~source ~target () =
  if grain <= 0 then invalid_arg "Attribute Interpolate: grain must be positive";
  Cancel.check_opt cancel;
  if not (Float.is_finite pre_scale) then Error "Attribute Interpolate: pre_scale must be finite"
  else if (not (Float.is_finite blend)) || blend < 0. || blend > 1. then
    Error "Attribute Interpolate: blend must be finite and in [0,1]"
  else if String.trim primitive_attribute = "" || String.trim uvw_attribute = "" then
    Error "Attribute Interpolate: driver attribute names must not be empty"
  else
    let primitive_driver = Geometry.find_attribute ~owner:target_owner primitive_attribute target
    and uvw_driver = Geometry.find_attribute ~owner:target_owner uvw_attribute target in
    match (primitive_driver, uvw_driver) with
    | None, _ ->
        Error
          (Printf.sprintf "Attribute Interpolate: missing destination integer attribute %S"
             primitive_attribute)
    | _, None ->
        Error
          (Printf.sprintf "Attribute Interpolate: missing destination float3 attribute %S"
             uvw_attribute)
    | Some primitive_driver, Some uvw_driver -> (
        match (Attribute.View.storage primitive_driver, Attribute.View.storage uvw_driver) with
        | Attribute.Int primitive_numbers, Attribute.Float3 uvw ->
            let uvw = Packed.Float3.View.view uvw in
            Result.bind (selection_elements target_owner selection target)
              (fun (selected_count, selected_element) ->
                Result.bind (create_group_jobs ~target_owner ~target groups) (fun group_jobs ->
                    let seen = Hashtbl.create (List.length attributes) in
                    let jobs_result =
                      List.fold_left
                        (fun result (spec : spec) ->
                          Result.bind result (fun jobs ->
                              let key = spec.target_name in
                              if Hashtbl.mem seen key then
                                Error
                                  (Printf.sprintf
                                     "Attribute Interpolate: duplicate target attribute %S" key)
                              else begin
                                Hashtbl.add seen key ();
                                Result.map
                                  (fun job -> job :: jobs)
                                  (create_job ~target_owner ~source ~target spec)
                              end))
                        (Ok []) attributes
                    in
                    Result.bind jobs_result (fun jobs ->
                        let jobs = Array.of_list (List.rev jobs) in
                        let compute_validation =
                          match compute with
                          | None -> Ok ()
                          | Some computed ->
                              let numbers = computed.computed_numbers_attribute
                              and weights = computed.computed_weights_attribute in
                              if
                                String.equal numbers primitive_attribute
                                || String.equal numbers uvw_attribute
                                || String.equal weights primitive_attribute
                                || String.equal weights uvw_attribute
                              then
                                Error
                                  "Attribute Interpolate: computed attributes must not replace \
                                   driver attributes"
                              else if Hashtbl.mem seen numbers || Hashtbl.mem seen weights then
                                Error
                                  "Attribute Interpolate: computed attributes must not replace \
                                   interpolated outputs"
                              else Ok ()
                        in
                        Result.bind compute_validation (fun () ->
                            if
                              Array.length jobs = 0
                              && Array.length group_jobs = 0
                              && Option.is_none compute
                              || selected_count = 0
                            then Ok target
                            else begin
                              for rank = 0 to selected_count - 1 do
                                Cancel.poll cancel rank 4095;
                                let element = selected_element rank in
                                if
                                  not
                                    (Float.is_finite uvw.x.(element)
                                    && Float.is_finite uvw.y.(element)
                                    && Float.is_finite uvw.z.(element))
                                then
                                  invalid_arg
                                    "Attribute Interpolate: UVW contains a non-finite value"
                              done;
                              let topology = Topology.View.view (Geometry.topology source)
                              and primitive_count = Geometry.primitive_count source
                              and point_numeric = select_jobs jobs Attribute.Point true
                              and vertex_numeric = select_jobs jobs Attribute.Vertex true
                              and primitive_numeric = select_jobs jobs Attribute.Primitive true
                              and detail_numeric = select_jobs jobs Attribute.Detail true
                              and point_discrete = select_jobs jobs Attribute.Point false
                              and vertex_discrete = select_jobs jobs Attribute.Vertex false
                              and primitive_discrete = select_jobs jobs Attribute.Primitive false
                              and detail_discrete = select_jobs jobs Attribute.Detail false in
                              let numeric_count =
                                Array.length point_numeric + Array.length vertex_numeric
                                + Array.length primitive_numeric + Array.length detail_numeric
                              in
                              let numeric =
                                if numeric_count = 0 then [||]
                                else Array.make numeric_count jobs.(0)
                              and at = ref 0 in
                              Array.iter
                                (fun selected ->
                                  Array.iter
                                    (fun job ->
                                      numeric.(!at) <- job;
                                      incr at)
                                    selected)
                                [|
                                  point_numeric; vertex_numeric; primitive_numeric; detail_numeric;
                                |];
                              Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(selected_count - 1)
                                (fun rank ->
                                  Cancel.poll cancel rank 4095;
                                  let destination = selected_element rank
                                  and primitive = primitive_numbers.(selected_element rank) in
                                  if primitive < 0 || primitive >= primitive_count then begin
                                    if unmatched = Default_value then
                                      for job_index = 0 to Array.length jobs - 1 do
                                        default_job jobs.(job_index) destination
                                      done
                                  end
                                  else begin
                                    let first = topology.primitive_offsets.(primitive)
                                    and last = topology.primitive_offsets.(primitive + 1) in
                                    let size = last - first
                                    and u = uvw.x.(destination) *. pre_scale
                                    and v = uvw.y.(destination) *. pre_scale
                                    and kind =
                                      Char.code
                                        (Bytes.unsafe_get topology.primitive_kinds primitive)
                                    in
                                    for job_index = 0 to Array.length numeric - 1 do
                                      zero_numeric numeric.(job_index) destination
                                    done;
                                    let best_local = ref 0 and best_weight = ref neg_infinity in
                                    for local = 0 to size - 1 do
                                      let weight =
                                        match kind with
                                        | 0 -> polygon_weight size u v local
                                        | 1 -> curve_weight ~closed:false size u local
                                        | 2 -> curve_weight ~closed:true size u local
                                        | _ -> assert false
                                      in
                                      if weight > !best_weight then begin
                                        best_weight := weight;
                                        best_local := local
                                      end;
                                      let vertex = first + local in
                                      let point = topology.vertex_points.(vertex) in
                                      for job_index = 0 to Array.length point_numeric - 1 do
                                        add_numeric point_numeric.(job_index) destination point
                                          weight
                                      done;
                                      for job_index = 0 to Array.length vertex_numeric - 1 do
                                        add_numeric vertex_numeric.(job_index) destination vertex
                                          weight
                                      done
                                    done;
                                    let best_vertex = first + !best_local in
                                    let best_point = topology.vertex_points.(best_vertex) in
                                    for job_index = 0 to Array.length primitive_numeric - 1 do
                                      set_numeric primitive_numeric.(job_index) destination
                                        primitive
                                    done;
                                    for job_index = 0 to Array.length detail_numeric - 1 do
                                      set_numeric detail_numeric.(job_index) destination 0
                                    done;
                                    for job_index = 0 to Array.length point_discrete - 1 do
                                      set_discrete point_discrete.(job_index) destination best_point
                                    done;
                                    for job_index = 0 to Array.length vertex_discrete - 1 do
                                      set_discrete vertex_discrete.(job_index) destination
                                        best_vertex
                                    done;
                                    for job_index = 0 to Array.length primitive_discrete - 1 do
                                      set_discrete primitive_discrete.(job_index) destination
                                        primitive
                                    done;
                                    for job_index = 0 to Array.length detail_discrete - 1 do
                                      set_discrete detail_discrete.(job_index) destination 0
                                    done;
                                    for job_index = 0 to Array.length jobs - 1 do
                                      blend_job jobs.(job_index) destination blend
                                    done
                                  end);
                              interpolate_primitive_groups ?cancel ~grain ?selection ~unmatched
                                ~blend ~pre_scale ~primitive_numbers ~uvw ~topology ~primitive_count
                                group_jobs;
                              Result.bind
                                (match compute with
                                | None -> Ok [||]
                                | Some computed ->
                                    compute_primitive_weights ?cancel ~grain ?selection ~unmatched
                                      ~target_owner ~computed ~primitive_numbers ~uvw ~pre_scale
                                      ~source ~target ())
                                (fun extra ->
                                  install ?cancel ~grain ~extra ~groups:(groups_of_jobs group_jobs)
                                    jobs target)
                            end))))
        | _ -> Error "Attribute Interpolate: primitive driver must be int and UVW driver float3")

let weighted_owner_name = function
  | Points -> "point"
  | Vertices -> "vertex"
  | Primitives -> "primitive"

let weighted_source_count source = function
  | Points -> Geometry.point_count source
  | Vertices -> Geometry.vertex_count source
  | Primitives -> Geometry.primitive_count source

let weighted_owner_supported weighted_owner source_owner =
  match weighted_owner with
  | Points -> source_owner = Attribute.Point || source_owner = Attribute.Detail
  | Vertices -> true
  | Primitives -> source_owner = Attribute.Primitive || source_owner = Attribute.Detail

let interpolate_weighted_groups ?cancel ?(grain = 16_384) ?selection ~unmatched ~blend ~pre_scale
    ~normalize_weights ~threshold ~weighted_owner ~numbers ~weights ~topology ~primitive_of_vertex
    jobs =
  let topology : Topology.View.view = topology
  and numbers : Packed.Int_array.View.view = numbers
  and weights : Packed.Float_array.View.view = weights in
  if Array.length jobs = 0 then ()
  else
    let target_count = jobs.(0).group_target_count in
    let source_index source_owner source_element =
      match (weighted_owner, source_owner) with
      | Points, Attribute.Point -> source_element
      | Vertices, Attribute.Point -> topology.vertex_points.(source_element)
      | Vertices, Attribute.Vertex -> source_element
      | Vertices, Attribute.Primitive -> primitive_of_vertex.(source_element)
      | Primitives, Attribute.Primitive -> source_element
      | _ -> assert false
    in
    let byte_count = (target_count + 7) / 8 in
    if byte_count > 0 then
      Parallel.for_
        ~chunk_size:(max 1 (grain / 8))
        ~start:0 ~finish:(byte_count - 1)
        (fun byte ->
          Cancel.poll cancel byte 511;
          let first_destination = byte lsl 3 in
          let last_destination = min target_count (first_destination + 8) in
          for destination = first_destination to last_destination - 1 do
            if match selection with None -> true | Some group -> Group.mem destination group then begin
              let first = numbers.offsets.(destination)
              and last = numbers.offsets.(destination + 1) in
              if first = last then begin
                if unmatched = Default_value then
                  for job_index = 0 to Array.length jobs - 1 do
                    bit_set jobs.(job_index).group_output_bits destination false
                  done
              end
              else begin
                let raw_sum = ref 0. in
                for slot = first to last - 1 do
                  raw_sum := !raw_sum +. (weights.values.(slot) *. pre_scale)
                done;
                let normalization, influence =
                  if normalize_weights then
                    let magnitude = Float.abs !raw_sum in
                    ( (if magnitude > 1e-20 then 1. /. !raw_sum else 0.),
                      Float.min 1. (magnitude /. threshold) )
                  else (1., 1.)
                in
                let effective_blend = blend *. influence in
                for job_index = 0 to Array.length jobs - 1 do
                  let job = jobs.(job_index) and score = ref 0. in
                  for slot = first to last - 1 do
                    let source_element =
                      source_index job.group_source_owner numbers.values.(slot)
                    in
                    if Support.Bits.mem job.group_source_bits source_element then
                      score := !score +. (weights.values.(slot) *. pre_scale *. normalization)
                  done;
                  commit_group_score job destination ~blend:effective_blend !score
                done
              end
            end
          done)

let interpolate_weighted ?cancel ?(grain = 16_384) ?selection
    ?(numbers_attribute = "source_elements") ?(weights_attribute = "source_weights")
    ?(pre_scale = 1.) ?(normalize_weights = false) ?(threshold = 1e-6) ?(blend = 1.)
    ?(unmatched = Keep_target) ~weighted_owner ~target_owner ~attributes ~groups ~source ~target ()
    =
  if grain <= 0 then invalid_arg "Attribute Interpolate: grain must be positive";
  Cancel.check_opt cancel;
  if not (Float.is_finite pre_scale) then Error "Attribute Interpolate: pre_scale must be finite"
  else if (not (Float.is_finite blend)) || blend < 0. || blend > 1. then
    Error "Attribute Interpolate: blend must be finite and in [0,1]"
  else if (not (Float.is_finite threshold)) || threshold <= 0. then
    Error "Attribute Interpolate: threshold must be finite and positive"
  else if String.trim numbers_attribute = "" || String.trim weights_attribute = "" then
    Error "Attribute Interpolate: driver attribute names must not be empty"
  else
    let numbers_driver = Geometry.find_attribute ~owner:target_owner numbers_attribute target
    and weights_driver = Geometry.find_attribute ~owner:target_owner weights_attribute target in
    match (numbers_driver, weights_driver) with
    | None, _ ->
        Error
          (Printf.sprintf "Attribute Interpolate: missing destination integer-array attribute %S"
             numbers_attribute)
    | _, None ->
        Error
          (Printf.sprintf "Attribute Interpolate: missing destination float-array attribute %S"
             weights_attribute)
    | Some numbers_driver, Some weights_driver -> (
        match (Attribute.View.storage numbers_driver, Attribute.View.storage weights_driver) with
        | Attribute.Int_array numbers, Attribute.Float_array weights ->
            let numbers = Packed.Int_array.View.view numbers
            and weights = Packed.Float_array.View.view weights in
            if numbers.offsets <> weights.offsets then
              Error "Attribute Interpolate: number and weight row layouts must match"
            else
              Result.bind (selection_elements target_owner selection target)
                (fun (selected_count, selected_element) ->
                  let incompatible_group =
                    List.find_opt
                      (fun (spec : group_spec) ->
                        not (weighted_owner_supported weighted_owner spec.group_source_owner))
                      groups
                  in
                  Result.bind
                    (match incompatible_group with
                    | None -> Ok ()
                    | Some (spec : group_spec) ->
                        Error
                          (Printf.sprintf
                             "Attribute Interpolate: %s-number mode cannot interpolate %s group %S"
                             (weighted_owner_name weighted_owner)
                             (owner_name spec.group_source_owner)
                             (Group.name spec.group_source)))
                    (fun () ->
                      Result.bind (create_group_jobs ~target_owner ~target groups)
                        (fun group_jobs ->
                          let seen = Hashtbl.create (List.length attributes) in
                          let jobs_result =
                            List.fold_left
                              (fun result (spec : spec) ->
                                Result.bind result (fun jobs ->
                                    if
                                      not
                                        (weighted_owner_supported weighted_owner spec.source_owner)
                                    then
                                      Error
                                        (Printf.sprintf
                                           "Attribute Interpolate: %s-number mode cannot \
                                            interpolate %s attribute %S"
                                           (weighted_owner_name weighted_owner)
                                           (owner_name spec.source_owner) spec.source_name)
                                    else if Hashtbl.mem seen spec.target_name then
                                      Error
                                        (Printf.sprintf
                                           "Attribute Interpolate: duplicate target attribute %S"
                                           spec.target_name)
                                    else begin
                                      Hashtbl.add seen spec.target_name ();
                                      Result.map
                                        (fun job -> job :: jobs)
                                        (create_job ~target_owner ~source ~target spec)
                                    end))
                              (Ok []) attributes
                          in
                          Result.bind jobs_result (fun jobs ->
                              let jobs = Array.of_list (List.rev jobs) in
                              if
                                (Array.length jobs = 0 && Array.length group_jobs = 0)
                                || selected_count = 0
                              then Ok target
                              else begin
                                let source_count = weighted_source_count source weighted_owner
                                and invalid = ref None in
                                for rank = 0 to selected_count - 1 do
                                  Cancel.poll cancel rank 4095;
                                  let destination = selected_element rank in
                                  let first = numbers.offsets.(destination)
                                  and last = numbers.offsets.(destination + 1) in
                                  let slot = ref first in
                                  while !slot < last && Option.is_none !invalid do
                                    let source_element = numbers.values.(!slot)
                                    and weight = weights.values.(!slot) in
                                    if source_element < 0 || source_element >= source_count then
                                      invalid :=
                                        Some
                                          (Printf.sprintf
                                             "Attribute Interpolate: %s number %d is out of bounds \
                                              at destination %d, weight %d"
                                             (weighted_owner_name weighted_owner)
                                             source_element destination (!slot - first))
                                    else if not (Float.is_finite weight) then
                                      invalid :=
                                        Some
                                          (Printf.sprintf
                                             "Attribute Interpolate: weight is non-finite at \
                                              destination %d, weight %d"
                                             destination (!slot - first));
                                    incr slot
                                  done
                                done;
                                match !invalid with
                                | Some message -> Error message
                                | None ->
                                    let topology = Topology.View.view (Geometry.topology source) in
                                    let needs_primitive_map =
                                      weighted_owner = Vertices
                                      && (Array.exists
                                            (fun job -> job.source_owner = Attribute.Primitive)
                                            jobs
                                         || Array.exists
                                              (fun job ->
                                                job.group_source_owner = Attribute.Primitive)
                                              group_jobs)
                                    in
                                    let primitive_of_vertex =
                                      if not needs_primitive_map then [||]
                                      else
                                        let output =
                                          Array.make (Geometry.vertex_count source) (-1)
                                        in
                                        for primitive = 0 to Geometry.primitive_count source - 1 do
                                          for
                                            vertex = topology.primitive_offsets.(primitive)
                                            to topology.primitive_offsets.(primitive + 1) - 1
                                          do
                                            output.(vertex) <- primitive
                                          done
                                        done;
                                        output
                                    in
                                    let numeric =
                                      Array.of_list (List.filter is_numeric (Array.to_list jobs))
                                    and discrete =
                                      Array.of_list
                                        (List.filter
                                           (fun job -> not (is_numeric job))
                                           (Array.to_list jobs))
                                    in
                                    let source_index job source_element =
                                      match (weighted_owner, job.source_owner) with
                                      | Points, Attribute.Point -> source_element
                                      | Vertices, Attribute.Point ->
                                          topology.vertex_points.(source_element)
                                      | Vertices, Attribute.Vertex -> source_element
                                      | Vertices, Attribute.Primitive ->
                                          primitive_of_vertex.(source_element)
                                      | Primitives, Attribute.Primitive -> source_element
                                      | (Points | Vertices | Primitives), Attribute.Detail -> 0
                                      | _ -> assert false
                                    in
                                    Parallel.for_ ~chunk_size:grain ~start:0
                                      ~finish:(selected_count - 1) (fun rank ->
                                        Cancel.poll cancel rank 4095;
                                        let destination = selected_element rank
                                        and first = numbers.offsets.(selected_element rank)
                                        and last = numbers.offsets.(selected_element rank + 1) in
                                        if first = last then begin
                                          if unmatched = Default_value then
                                            for job_index = 0 to Array.length jobs - 1 do
                                              default_job jobs.(job_index) destination
                                            done
                                        end
                                        else begin
                                          let raw_sum = ref 0. in
                                          for slot = first to last - 1 do
                                            raw_sum :=
                                              !raw_sum +. (weights.values.(slot) *. pre_scale)
                                          done;
                                          let normalization, influence =
                                            if normalize_weights then
                                              let magnitude = Float.abs !raw_sum in
                                              ( (if magnitude > 1e-20 then 1. /. !raw_sum else 0.),
                                                Float.min 1. (magnitude /. threshold) )
                                            else (1., 1.)
                                          in
                                          for job_index = 0 to Array.length numeric - 1 do
                                            zero_numeric numeric.(job_index) destination
                                          done;
                                          let best_source = ref numbers.values.(first)
                                          and best_weight = ref neg_infinity in
                                          for slot = first to last - 1 do
                                            let source_element = numbers.values.(slot) in
                                            let weight =
                                              weights.values.(slot) *. pre_scale *. normalization
                                            in
                                            if weight > !best_weight then begin
                                              best_weight := weight;
                                              best_source := source_element
                                            end;
                                            for job_index = 0 to Array.length numeric - 1 do
                                              let job = numeric.(job_index) in
                                              add_numeric job destination
                                                (source_index job source_element) weight
                                            done
                                          done;
                                          for job_index = 0 to Array.length discrete - 1 do
                                            let job = discrete.(job_index) in
                                            set_discrete job destination
                                              (source_index job !best_source)
                                          done;
                                          let effective_blend = blend *. influence in
                                          for job_index = 0 to Array.length jobs - 1 do
                                            blend_job jobs.(job_index) destination effective_blend
                                          done
                                        end);
                                    interpolate_weighted_groups ?cancel ~grain ?selection ~unmatched
                                      ~blend ~pre_scale ~normalize_weights ~threshold
                                      ~weighted_owner ~numbers ~weights ~topology
                                      ~primitive_of_vertex group_jobs;
                                    install ?cancel ~grain ~groups:(groups_of_jobs group_jobs) jobs
                                      target
                              end))))
        | _ -> Error "Attribute Interpolate: weighted drivers must be int-array and float-array")
