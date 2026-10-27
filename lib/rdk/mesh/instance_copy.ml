open Rays_math

let merge = Mesh_merge.merge

let point_float geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> Ok None
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float values -> Ok (Some values)
       | _ -> Error (Printf.sprintf "Rdk.Instance_copy.copy_to_points: %s must be point float" name))

let point_float3 geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> Ok None
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float3 values -> Ok (Some values)
       | _ -> Error (Printf.sprintf "Rdk.Instance_copy.copy_to_points: %s must be point float3" name))

let point_float4 geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> Ok None
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float4 values -> Ok (Some values)
       | _ -> Error (Printf.sprintf "Rdk.Instance_copy.copy_to_points: %s must be point float4" name))

let point_affine_transform geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> Ok None
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float_array values ->
           let view = Packed.Float_array.Private.view values in
           let rows = Array.length view.offsets - 1 in
           let width = if rows = 0 then 16
             else view.offsets.(1) - view.offsets.(0) in
           if width <> 9 && width <> 16 then Error (Printf.sprintf
             "Rdk.Instance_copy.copy_to_points: %s rows must contain 9 or 16 floats" name)
           else begin
             let valid = ref true in
             for row = 0 to rows - 1 do
               let first = view.offsets.(row) and last = view.offsets.(row + 1) in
               if last - first <> width then valid := false;
               for value = first to last - 1 do
                 if not (Float.is_finite view.values.(value)) then valid := false
               done;
               if width = 16 && (abs_float view.values.(first + 12) > 1e-12
                   || abs_float view.values.(first + 13) > 1e-12
                   || abs_float view.values.(first + 14) > 1e-12
                   || abs_float (view.values.(first + 15) -. 1.) > 1e-12)
               then valid := false
             done;
             if !valid then Ok (Some (view, width))
             else Error (Printf.sprintf
               "Rdk.Instance_copy.copy_to_points: %s must contain finite, fixed-width affine matrices"
               name)
           end
       | _ -> Error (Printf.sprintf
           "Rdk.Instance_copy.copy_to_points: %s must be a point float-array matrix" name))

let checked_product operation left right =
  if left <> 0 && right > max_int / left then
    Error (operation ^ ": output cardinality exceeds OCaml array limits")
  else Ok (left * right)

type copy_target_owner = Copy_target_points | Copy_target_vertices
  | Copy_target_primitives
type copy_target_operation = Copy_target_nothing | Copy_target_copy
  | Copy_target_add | Copy_target_subtract | Copy_target_multiply
type copy_target_attribute_rule = {
  copy_target_pattern : string;
  copy_target_owner : copy_target_owner;
  copy_target_operation : copy_target_operation;
}

let parallel_output_ranges ?cancel ~grain length body =
  if length > 0 then begin
    let chunk_count = 1 + ((length - 1) / grain) in
    Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(chunk_count - 1) (fun chunk ->
      Cancel.check_opt cancel;
      let first = chunk * grain in
      let last = first + min grain (length - first) in
      body ~first ~last)
  end

let copy_target_attribute ?cancel ~grain ~copies ~per_copy rule target_attribute
    geometry =
  try
  let owner = match rule.copy_target_owner with
    | Copy_target_points -> Attribute.Point
    | Copy_target_vertices -> Attribute.Vertex
    | Copy_target_primitives -> Attribute.Primitive in
  let name = Attribute.name target_attribute in
  let output_count = copies * per_copy in
  let existing = Geometry.find_attribute ~owner name geometry in
  let operation = rule.copy_target_operation in
  let[@inline always] float_missing target = match operation with
    | Copy_target_nothing -> assert false
    | Copy_target_subtract -> -.target
    | Copy_target_copy | Copy_target_add | Copy_target_multiply -> target in
  let[@inline always] float_existing source target = match operation with
    | Copy_target_nothing -> assert false
    | Copy_target_copy -> target
    | Copy_target_add -> source +. target
    | Copy_target_subtract -> source -. target
    | Copy_target_multiply -> source *. target in
  let[@inline always] int_missing target = match operation with
    | Copy_target_nothing -> assert false
    | Copy_target_subtract -> -target
    | Copy_target_copy | Copy_target_add | Copy_target_multiply -> target in
  let[@inline always] int_existing source target = match operation with
    | Copy_target_nothing -> assert false
    | Copy_target_copy -> target
    | Copy_target_add -> source + target
    | Copy_target_subtract -> source - target
    | Copy_target_multiply -> source * target in
  let incompatible expected = Error (Printf.sprintf
    "Rdk.Instance_copy.copy_to_points: target attribute %S requires matching %s storage for arithmetic"
    name expected) in
  let fixed_float target destination =
    let output = Array.make output_count 0. in
    (match destination with
    | None -> parallel_output_ranges ?cancel ~grain output_count
        (fun ~first ~last -> for index = first to last - 1 do
          output.(index) <- float_missing target.(index / per_copy) done)
    | Some source -> parallel_output_ranges ?cancel ~grain output_count
        (fun ~first ~last -> for index = first to last - 1 do
          output.(index) <- float_existing source.(index)
              target.(index / per_copy) done));
    Attribute.Float output in
  let fixed_int target destination =
    let output = Array.make output_count 0 in
    (match destination with
    | None -> parallel_output_ranges ?cancel ~grain output_count
        (fun ~first ~last -> for index = first to last - 1 do
          output.(index) <- int_missing target.(index / per_copy) done)
    | Some source -> parallel_output_ranges ?cancel ~grain output_count
        (fun ~first ~last -> for index = first to last - 1 do
          output.(index) <- int_existing source.(index)
              target.(index / per_copy) done));
    Attribute.Int output in
  let destination_storage () = Option.map Attribute.Private.storage existing in
  let storage_result = match Attribute.Private.storage target_attribute with
    | Attribute.Float target ->
        let destination = match operation, destination_storage () with
          | Copy_target_copy, _ | _, None -> Ok None
          | _, Some (Attribute.Float values) -> Ok (Some values)
          | _ -> incompatible "float" in
        Result.map (fixed_float target) destination
    | Attribute.Int target ->
        let destination = match operation, destination_storage () with
          | Copy_target_copy, _ | _, None -> Ok None
          | _, Some (Attribute.Int values) -> Ok (Some values)
          | _ -> incompatible "int" in
        Result.map (fixed_int target) destination
    | Attribute.Float2 target ->
        let target = Packed.Float2.Private.view target in
        let destination = match operation, destination_storage () with
          | Copy_target_copy, _ | _, None -> Ok None
          | _, Some (Attribute.Float2 values) ->
              Ok (Some (Packed.Float2.Private.view values))
          | _ -> incompatible "float2" in
        Result.map (fun (destination : Packed.Float2.Private.view option) ->
          let x = Array.make output_count 0. and y = Array.make output_count 0. in
          parallel_output_ranges ?cancel ~grain output_count (fun ~first ~last ->
            for index = first to last - 1 do
              let target_index = index / per_copy in
              (match destination with
               | None ->
                   x.(index) <- float_missing target.x.(target_index);
                   y.(index) <- float_missing target.y.(target_index)
               | Some values ->
                   x.(index) <- float_existing values.x.(index)
                       target.x.(target_index);
                   y.(index) <- float_existing values.y.(index)
                       target.y.(target_index))
            done);
          Attribute.Float2 (Packed.Float2.of_owned ~x ~y |> Support.get_ok)) destination
    | Attribute.Float3 target ->
        let target = Packed.Float3.Private.view target in
        let destination = match operation, destination_storage () with
          | Copy_target_copy, _ | _, None -> Ok None
          | _, Some (Attribute.Float3 values) ->
              Ok (Some (Packed.Float3.Private.view values))
          | _ -> incompatible "float3" in
        Result.map (fun (destination : Packed.Float3.Private.view option) ->
          let x = Array.make output_count 0. and y = Array.make output_count 0.
          and z = Array.make output_count 0. in
          parallel_output_ranges ?cancel ~grain output_count (fun ~first ~last ->
            for index = first to last - 1 do
              let target_index = index / per_copy in
              (match destination with
               | None ->
                   x.(index) <- float_missing target.x.(target_index);
                   y.(index) <- float_missing target.y.(target_index);
                   z.(index) <- float_missing target.z.(target_index)
               | Some values ->
                   x.(index) <- float_existing values.x.(index)
                       target.x.(target_index);
                   y.(index) <- float_existing values.y.(index)
                       target.y.(target_index);
                   z.(index) <- float_existing values.z.(index)
                       target.z.(target_index))
            done);
          Attribute.Float3 (Packed.Float3.Private.of_owned_exn ~x ~y ~z)) destination
    | Attribute.Float4 target ->
        let target = Packed.Float4.Private.view target in
        let destination = match operation, destination_storage () with
          | Copy_target_copy, _ | _, None -> Ok None
          | _, Some (Attribute.Float4 values) ->
              Ok (Some (Packed.Float4.Private.view values))
          | _ -> incompatible "float4" in
        Result.map (fun (destination : Packed.Float4.Private.view option) ->
          let x = Array.make output_count 0. and y = Array.make output_count 0.
          and z = Array.make output_count 0. and w = Array.make output_count 0. in
          parallel_output_ranges ?cancel ~grain output_count (fun ~first ~last ->
            for index = first to last - 1 do
              let target_index = index / per_copy in
              (match destination with
               | None ->
                   x.(index) <- float_missing target.x.(target_index);
                   y.(index) <- float_missing target.y.(target_index);
                   z.(index) <- float_missing target.z.(target_index);
                   w.(index) <- float_missing target.w.(target_index)
               | Some values ->
                   x.(index) <- float_existing values.x.(index)
                       target.x.(target_index);
                   y.(index) <- float_existing values.y.(index)
                       target.y.(target_index);
                   z.(index) <- float_existing values.z.(index)
                       target.z.(target_index);
                   w.(index) <- float_existing values.w.(index)
                       target.w.(target_index))
            done);
          Attribute.Float4 (Packed.Float4.of_owned ~x ~y ~z ~w |> Support.get_ok)) destination
    | Attribute.Text target ->
        let output = Array.make output_count "" in
        parallel_output_ranges ?cancel ~grain output_count (fun ~first ~last ->
          for index = first to last - 1 do
            output.(index) <- target.(index / per_copy)
          done);
        Ok (Attribute.Text output)
    | Attribute.Int_array target ->
        let target = Packed.Int_array.Private.view target in
        let destination = match operation, destination_storage () with
          | Copy_target_copy, _ | _, None -> Ok None
          | _, Some (Attribute.Int_array values) ->
              Ok (Some (Packed.Int_array.Private.view values))
          | _ -> incompatible "int_array" in
        Result.bind destination
          (fun (destination : Packed.Int_array.Private.view option) ->
          let offsets = Array.make (output_count + 1) 0 in
          for index = 0 to output_count - 1 do
            let target_index = index / per_copy in
            let width = target.offsets.(target_index + 1)
                - target.offsets.(target_index) in
            let width = match destination with
              | None -> width
              | Some values ->
                  let actual = values.offsets.(index + 1) - values.offsets.(index) in
                  if actual <> width then raise (Invalid_argument
                    "copy target integer-array row widths differ");
                  width in
            if offsets.(index) > max_int - width then raise (Invalid_argument
              "copy target integer-array cardinality exceeds limits");
            offsets.(index + 1) <- offsets.(index) + width
          done;
          let values = Array.make offsets.(output_count) 0 in
          parallel_output_ranges ?cancel ~grain output_count (fun ~first ~last ->
            for index = first to last - 1 do
              let target_index = index / per_copy in
              let target_first = target.offsets.(target_index)
              and output_first = offsets.(index) in
              for component = 0 to offsets.(index + 1) - output_first - 1 do
                values.(output_first + component) <- (match destination with
                  | None -> int_missing target.values.(target_first + component)
                  | Some source -> int_existing
                      source.values.(source.offsets.(index) + component)
                      target.values.(target_first + component))
              done
            done);
          Ok (Attribute.Int_array (Packed.Int_array.Private.create_validated_owned
            ~offsets ~values)))
    | Attribute.Float_array target ->
        let target = Packed.Float_array.Private.view target in
        let destination = match operation, destination_storage () with
          | Copy_target_copy, _ | _, None -> Ok None
          | _, Some (Attribute.Float_array values) ->
              Ok (Some (Packed.Float_array.Private.view values))
          | _ -> incompatible "float_array" in
        Result.bind destination
          (fun (destination : Packed.Float_array.Private.view option) ->
          let offsets = Array.make (output_count + 1) 0 in
          for index = 0 to output_count - 1 do
            let target_index = index / per_copy in
            let width = target.offsets.(target_index + 1)
                - target.offsets.(target_index) in
            let width = match destination with
              | None -> width
              | Some values ->
                  let actual = values.offsets.(index + 1) - values.offsets.(index) in
                  if actual <> width then raise (Invalid_argument
                    "copy target float-array row widths differ");
                  width in
            if offsets.(index) > max_int - width then raise (Invalid_argument
              "copy target float-array cardinality exceeds limits");
            offsets.(index + 1) <- offsets.(index) + width
          done;
          let values = Array.make offsets.(output_count) 0. in
          parallel_output_ranges ?cancel ~grain output_count (fun ~first ~last ->
            for index = first to last - 1 do
              let target_index = index / per_copy in
              let target_first = target.offsets.(target_index)
              and output_first = offsets.(index) in
              for component = 0 to offsets.(index + 1) - output_first - 1 do
                values.(output_first + component) <- (match destination with
                  | None -> float_missing target.values.(target_first + component)
                  | Some source -> float_existing
                      source.values.(source.offsets.(index) + component)
                      target.values.(target_first + component))
              done
            done);
          Ok (Attribute.Float_array
            (Packed.Float_array.Private.create_validated_owned ~offsets ~values))) in
  Result.bind storage_result (fun storage ->
    Result.bind (Attribute.create_owned ~name ~owner storage) (fun attribute ->
      Geometry.with_attribute attribute geometry))
  with Invalid_argument message ->
    Error ("Rdk.Instance_copy.copy_to_points: " ^ message)

let compile_copy_target_rules rules =
  let rec compile result = function
    | [] -> Ok (List.rev result)
    | rule :: rest ->
        Result.bind (Attribute_pattern.compile rule.copy_target_pattern)
          (fun pattern -> compile ((rule, pattern) :: result) rest) in
  compile [] rules

let winning_copy_target_rule rules name =
  List.fold_left (fun winner (rule, pattern) ->
    if Attribute_pattern.matches pattern name then Some rule else winner)
    None rules

let copy_target_group ?cancel ~grain ~copies ~per_copy rule target_group
    geometry =
  let owner = match rule.copy_target_owner with
    | Copy_target_points -> Group.Point
    | Copy_target_vertices -> Group.Vertex
    | Copy_target_primitives -> Group.Primitive in
  let name = Group.name target_group in
  let existing = Geometry.find_group ~owner name geometry in
  let length = copies * per_copy in
  let group = Group.init ~grain ~owner ~name length (fun index ->
    if index land 4095 = 0 then Cancel.check_opt cancel;
    let target_member = Group.mem (index / per_copy) target_group in
    match rule.copy_target_operation, existing with
    | Copy_target_nothing, _ -> assert false
    | Copy_target_copy, _ -> target_member
    | (Copy_target_add | Copy_target_multiply), None -> target_member
    | Copy_target_subtract, None -> false
    | Copy_target_add, Some source -> Group.mem index source || target_member
    | Copy_target_multiply, Some source -> Group.mem index source && target_member
    | Copy_target_subtract, Some source -> Group.mem index source && not target_member
  ) in
  Geometry.with_group group geometry

let apply_copy_target_rules ?cancel ~grain ~copies ~source_points
    ~source_vertices ~source_primitives rules targets geometry =
  let per_copy rule = match rule.copy_target_owner with
    | Copy_target_points -> source_points
    | Copy_target_vertices -> source_vertices
    | Copy_target_primitives -> source_primitives in
  let rec apply_attributes geometry = function
    | [] -> Ok geometry
    | attribute :: attributes ->
        let next = match Attribute.owner attribute with
          | Attribute.Point ->
              (match winning_copy_target_rule rules (Attribute.name attribute) with
               | None | Some { copy_target_operation = Copy_target_nothing; _ } ->
                   Ok geometry
               | Some rule -> copy_target_attribute ?cancel ~grain ~copies
                   ~per_copy:(per_copy rule) rule attribute geometry)
          | Attribute.Vertex | Attribute.Primitive | Attribute.Detail -> Ok geometry in
        Result.bind next (fun geometry -> apply_attributes geometry attributes) in
  let rec apply_groups geometry = function
    | [] -> Ok geometry
    | group :: groups ->
        let next = match Group.owner group with
          | Group.Point ->
              (match winning_copy_target_rule rules (Group.name group) with
               | None | Some { copy_target_operation = Copy_target_nothing; _ } ->
                   Ok geometry
               | Some rule -> copy_target_group ?cancel ~grain ~copies
                   ~per_copy:(per_copy rule) rule group geometry)
          | Group.Vertex | Group.Primitive -> Ok geometry in
        Result.bind next (fun geometry -> apply_groups geometry groups) in
  Result.bind (apply_attributes geometry (Geometry.attributes targets))
    (fun geometry -> apply_groups geometry (Geometry.groups targets))

let repeat_array ?cancel ?(grain = 16_384) copies source =
  let length = Array.length source in
  if copies = 0 || length = 0 then [||]
  else begin
    let total = copies * length in
    let output = Array.make total source.(0) in
    parallel_output_ranges ?cancel ~grain total (fun ~first ~last ->
      let output_at = ref first in
      while !output_at < last do
        let source_at = !output_at mod length in
        let count = min (length - source_at) (last - !output_at) in
        Array.blit source source_at output !output_at count;
        output_at := !output_at + count
      done);
    output
  end

let repeat_attribute ?cancel ?(grain = 16_384) copies attribute =
  if Attribute.owner attribute = Attribute.Detail then Ok attribute
  else
    let storage = match Attribute.Private.storage attribute with
      | Attribute.Float values ->
          Attribute.Float (repeat_array ?cancel ~grain copies values)
      | Attribute.Int values ->
          Attribute.Int (repeat_array ?cancel ~grain copies values)
      | Attribute.Text values ->
          Attribute.Text (repeat_array ?cancel ~grain copies values)
      | Attribute.Float2 values ->
          let view = Packed.Float2.Private.view values in
          Attribute.Float2 (Packed.Float2.of_owned
            ~x:(repeat_array ?cancel ~grain copies view.x)
            ~y:(repeat_array ?cancel ~grain copies view.y) |> Support.get_ok)
      | Attribute.Float3 values ->
          let view = Packed.Float3.Private.view values in
          Attribute.Float3 (Packed.Float3.Private.of_owned_exn
            ~x:(repeat_array ?cancel ~grain copies view.x)
            ~y:(repeat_array ?cancel ~grain copies view.y)
            ~z:(repeat_array ?cancel ~grain copies view.z))
      | Attribute.Float4 values ->
          let view = Packed.Float4.Private.view values in
          Attribute.Float4 (Packed.Float4.of_owned
            ~x:(repeat_array ?cancel ~grain copies view.x)
            ~y:(repeat_array ?cancel ~grain copies view.y)
            ~z:(repeat_array ?cancel ~grain copies view.z)
            ~w:(repeat_array ?cancel ~grain copies view.w) |> Support.get_ok)
      | Attribute.Int_array values ->
          let source_count = Packed.Int_array.length values in
          let mapping = Array.init (copies * source_count)
              (fun index -> index mod source_count) in
          Attribute.Int_array (Ragged_ops.remap_int ?cancel ~grain mapping values)
      | Attribute.Float_array values ->
          let source_count = Packed.Float_array.length values in
          let mapping = Array.init (copies * source_count)
              (fun index -> index mod source_count) in
          Attribute.Float_array (Ragged_ops.remap_float ?cancel ~grain mapping values) in
    Attribute.create_owned ~name:(Attribute.name attribute)
      ~owner:(Attribute.owner attribute) storage

let repeat_group ?cancel ?grain copies group =
  let source_count = Group.length group in
  let target = Group.init ?grain ~owner:(Group.owner group)
      ~name:(Group.name group) (copies * source_count)
      (fun index ->
        if index land 4095 = 0 then Cancel.check_opt cancel;
        Group.mem (index mod source_count) group) in
  match Group.Private.order_view group with
  | None -> target
  | Some source_order ->
      let order = Array.make (copies * Array.length source_order) 0
      and output = ref 0 in
      for copy = 0 to copies - 1 do
        if copy land 255 = 0 then Cancel.check_opt cancel;
        Array.iter (fun source_element ->
          order.(!output) <- (copy * source_count) + source_element;
          incr output
        ) source_order
      done;
      Group.Private.with_owned_order order target

let transform_single_instance ?cancel ~grain matrix geometry =
  let source = Packed.Float3.Private.view (Geometry.positions geometry) in
  let count = Array.length source.x in
  let x = Array.make count 0. and y = Array.make count 0.
  and z = Array.make count 0. in
  let (m00,m01,m02,m03), (m10,m11,m12,m13),
      (m20,m21,m22,m23), (m30,m31,m32,m33) = Rays_math.Mat4.to_rows matrix in
  parallel_output_ranges ?cancel ~grain count (fun ~first ~last ->
    for point = first to last - 1 do
      let px = source.x.(point) and py = source.y.(point)
      and pz = source.z.(point) in
      let ox = m00*.px +. m01*.py +. m02*.pz +. m03
      and oy = m10*.px +. m11*.py +. m12*.pz +. m13
      and oz = m20*.px +. m21*.py +. m22*.pz +. m23
      and ow = m30*.px +. m31*.py +. m32*.pz +. m33 in
      if abs_float ow <= 1e-12 then begin
        x.(point) <- ox; y.(point) <- oy; z.(point) <- oz
      end else begin
        x.(point) <- ox /. ow; y.(point) <- oy /. ow;
        z.(point) <- oz /. ow
      end
    done);
  let positions = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
  let output = Geometry.with_positions positions geometry |> Support.get_ok in
  match Rays_math.Mat4.inverse matrix with
  | None ->
      List.fold_left (fun output owner ->
        match Geometry.find_attribute ~owner "N" geometry with
        | Some attribute when Attribute.get (Attribute.normal ~owner) attribute
            <> None -> Geometry.without_attribute ~owner "N" output
        | None | Some _ -> output)
        output [Attribute.Point; Attribute.Vertex]
  | Some inverse ->
      let normal_matrix = Rays_math.Mat4.transpose inverse in
      let (m00,m01,m02,_), (m10,m11,m12,_),
          (m20,m21,m22,_), _ = Rays_math.Mat4.to_rows normal_matrix in
      List.fold_left (fun output owner ->
        match Geometry.find_attribute ~owner "N" geometry with
        | None -> output
        | Some attribute ->
            (match Attribute.get (Attribute.normal ~owner) attribute with
             | None -> output
             | Some source ->
                 let source = Packed.Float3.Private.view source in
                 let count = Array.length source.x in
                 let x = Array.make count 0. and y = Array.make count 0.
                 and z = Array.make count 0. in
                 parallel_output_ranges ?cancel ~grain count
                   (fun ~first ~last ->
                     for index = first to last - 1 do
                       let ox = m00*.source.x.(index) +. m01*.source.y.(index)
                         +. m02*.source.z.(index)
                       and oy = m10*.source.x.(index) +. m11*.source.y.(index)
                         +. m12*.source.z.(index)
                       and oz = m20*.source.x.(index) +. m21*.source.y.(index)
                         +. m22*.source.z.(index) in
                       let length = sqrt (ox*.ox +. oy*.oy +. oz*.oz) in
                       if length > 1e-20 then begin
                         x.(index) <- ox/.length; y.(index) <- oy/.length;
                         z.(index) <- oz/.length
                       end
                     done);
                 let value = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
                 let attribute = Attribute.create_key_owned
                     (Attribute.normal ~owner) value |> Support.get_ok in
                 Geometry.with_attribute attribute output |> Support.get_ok))
        output [Attribute.Point; Attribute.Vertex]

let materialize_instances ?cancel ?(grain = 16_384) ~transforms geometry =
  if grain <= 0 then
    invalid_arg "Rdk.Instance_copy.materialize_instances: grain must be positive";
  Cancel.check_opt cancel;
  let matrices = Array.copy transforms in
  let invalid_transform = ref (-1) in
  Array.iteri (fun instance matrix ->
      for row = 0 to 3 do for column = 0 to 3 do
        if !invalid_transform < 0
            && not (Float.is_finite (Rays_math.Mat4.get matrix ~row ~column)) then
          invalid_transform := instance
      done done) matrices;
  let total = Array.length matrices in
  if !invalid_transform >= 0 then
    Error (Printf.sprintf
      "Rdk.Instance_copy.materialize_instances: transform %d must be finite"
      !invalid_transform)
  else if total = 1 && Rays_math.Mat4.nearly_equal matrices.(0) Rays_math.Mat4.identity ~eps:0. then
    Ok geometry
  else if total = 1 then
    Ok (transform_single_instance ?cancel ~grain matrices.(0) geometry)
  else
    let source_points = Geometry.point_count geometry
    and source_vertices = Geometry.vertex_count geometry
    and source_primitives = Geometry.primitive_count geometry in
    if source_points = 0 && source_vertices = 0 && source_primitives = 0 then
      Ok geometry
    else
    let product label count =
      if count <> 0 && total > Sys.max_array_length / count then
        Error ("Rdk.Instance_copy.materialize_instances: " ^ label
          ^ " output exceeds array limits")
      else Ok (total * count) in
    Result.bind (product "point" source_points) (fun output_points ->
    Result.bind (product "vertex" source_vertices) (fun output_vertices ->
    Result.bind (product "primitive" source_primitives) (fun output_primitives ->
    if output_primitives = Sys.max_array_length then
      Error "Rdk.Instance_copy.materialize_instances: primitive-offset output exceeds array limits"
    else begin
      let source_positions = Packed.Float3.Private.view (Geometry.positions geometry) in
      let x, y, z =
          let rows = Array.map Rays_math.Mat4.to_rows matrices in
          let x = Array.make output_points 0. and y = Array.make output_points 0.
          and z = Array.make output_points 0. in
          parallel_output_ranges ?cancel ~grain output_points
            (fun ~first ~last ->
            let output_at = ref first in
            while !output_at < last do
              let copy = !output_at / source_points in
              let source_at = !output_at - (copy * source_points) in
              let count = min (source_points - source_at) (last - !output_at) in
              let (m00,m01,m02,m03), (m10,m11,m12,m13),
                  (m20,m21,m22,m23), (m30,m31,m32,m33) = rows.(copy) in
              for offset = 0 to count - 1 do
                let point = source_at + offset in
                let px = source_positions.x.(point)
                and py = source_positions.y.(point)
                and pz = source_positions.z.(point) in
                let ox = m00*.px +. m01*.py +. m02*.pz +. m03
                and oy = m10*.px +. m11*.py +. m12*.pz +. m13
                and oz = m20*.px +. m21*.py +. m22*.pz +. m23
                and ow = m30*.px +. m31*.py +. m32*.pz +. m33 in
                let output = !output_at + offset in
                if abs_float ow <= 1e-12 then begin
                  x.(output) <- ox; y.(output) <- oy; z.(output) <- oz
                end else begin
                  x.(output) <- ox /. ow; y.(output) <- oy /. ow;
                  z.(output) <- oz /. ow
                end
              done;
              output_at := !output_at + count
            done);
          x, y, z in
      let source_topology = Topology.Private.view (Geometry.topology geometry) in
      let vertex_points = Array.make output_vertices 0
      and primitive_offsets = Array.make (output_primitives + 1) 0
      and primitive_kinds = Bytes.make output_primitives '\000' in
      parallel_output_ranges ?cancel ~grain output_vertices
        (fun ~first ~last ->
          let output_at = ref first in
          while !output_at < last do
            let copy = !output_at / source_vertices in
            let source_at = !output_at - (copy * source_vertices) in
            let count = min (source_vertices - source_at) (last - !output_at) in
            let point_offset = copy * source_points in
            for offset = 0 to count - 1 do
              vertex_points.(!output_at + offset) <-
                source_topology.vertex_points.(source_at + offset) + point_offset
            done;
            output_at := !output_at + count
          done);
      parallel_output_ranges ?cancel ~grain output_primitives
        (fun ~first ~last ->
          let output_at = ref first in
          while !output_at < last do
            let copy = !output_at / source_primitives in
            let source_at = !output_at - (copy * source_primitives) in
            let count = min (source_primitives - source_at) (last - !output_at) in
            let vertex_offset = copy * source_vertices in
            for offset = 0 to count - 1 do
              primitive_offsets.(!output_at + offset) <-
                source_topology.primitive_offsets.(source_at + offset)
                + vertex_offset
            done;
            Bytes.blit source_topology.primitive_kinds source_at primitive_kinds
              !output_at count;
            output_at := !output_at + count
          done);
      primitive_offsets.(output_primitives) <- output_vertices;
      let topology = Topology.Private.create_validated_owned ~point_count:output_points
          ~vertex_points ~primitive_offsets ~primitive_kinds in
      let typed_normal attribute =
        let owner = Attribute.owner attribute in
        if String.equal (Attribute.name attribute) "N"
            && (owner = Attribute.Point || owner = Attribute.Vertex)
        then Attribute.get (Attribute.normal ~owner) attribute
        else None in
      let has_normals =
        List.exists (fun attribute -> typed_normal attribute <> None)
          (Geometry.attributes geometry) in
      let normal_matrices = if not has_normals then None
        else
          let output = Array.make total Rays_math.Mat4.identity
          and valid = ref true and instance = ref 0 in
          while !valid && !instance < total do
            match Rays_math.Mat4.inverse matrices.(!instance) with
            | None -> valid := false
            | Some inverse ->
                output.(!instance) <- Rays_math.Mat4.transpose inverse;
                incr instance
          done;
          if !valid then Some output else None in
      let transform_normals owner source normal_matrices =
        let source = Packed.Float3.Private.view source in
        let source_count = Array.length source.x in
        let nx = Array.make (total * source_count) 0.
        and ny = Array.make (total * source_count) 0.
        and nz = Array.make (total * source_count) 0. in
        parallel_output_ranges ?cancel ~grain (total * source_count)
          (fun ~first ~last ->
          let output_at = ref first in
          while !output_at < last do
            let copy = !output_at / source_count in
            let source_at = !output_at - (copy * source_count) in
            let count = min (source_count - source_at) (last - !output_at) in
            let (m00,m01,m02,_), (m10,m11,m12,_),
                (m20,m21,m22,_), _ = Rays_math.Mat4.to_rows normal_matrices.(copy) in
            for offset = 0 to count - 1 do
              let index = source_at + offset in
              let ox = m00*.source.x.(index) +. m01*.source.y.(index)
                +. m02*.source.z.(index)
              and oy = m10*.source.x.(index) +. m11*.source.y.(index)
                +. m12*.source.z.(index)
              and oz = m20*.source.x.(index) +. m21*.source.y.(index)
                +. m22*.source.z.(index) in
              let length = sqrt (ox*.ox +. oy*.oy +. oz*.oz) in
              if length > 1e-20 then begin
                let output = !output_at + offset in
                nx.(output) <- ox/.length; ny.(output) <- oy/.length;
                nz.(output) <- oz/.length
              end
            done;
            output_at := !output_at + count
          done);
        let value = Packed.Float3.Private.of_owned_exn ~x:nx ~y:ny ~z:nz in
        Attribute.create_key_owned (Attribute.normal ~owner) value |> Support.get_ok in
      let rec attributes output = function
        | [] -> Ok (List.rev output)
        | attribute :: rest ->
            (match typed_normal attribute, normal_matrices with
             | Some _, None -> attributes output rest
             | Some source, Some matrices ->
                 let attribute = transform_normals (Attribute.owner attribute)
                     source matrices in
                 attributes (attribute :: output) rest
             | None, _ ->
                 Result.bind (repeat_attribute ?cancel ~grain total attribute)
                   (fun attribute -> attributes (attribute :: output) rest)) in
      Result.bind (attributes [] (Geometry.attributes geometry)) (fun attributes ->
        let positions = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
        let edge_groups = match Geometry.edge_groups geometry with
          | [] -> []
          | source_groups ->
              let source_index = Topology_index.create ?cancel
                  (Geometry.topology geometry)
              in
              List.map (fun group -> Edge_group.replicate_exact_copies ?cancel
                ~source_topology:(Geometry.topology geometry) ~source_index
                ~target_topology:topology ~copies:total group |> Support.get_ok)
                source_groups in
        let groups = List.map (fun group -> repeat_group ?cancel ~grain total group)
            (Geometry.groups geometry) in
        Geometry.create ~positions ~topology ~attributes ~groups ~edge_groups ())
    end)))

let duplicate ?cancel ?(grain = 16_384) ?(copies = 1) ?(cumulative = true)
    ?(transform = Rays_math.Mat4.identity) ?primitives ?copy_group_prefix
    ?(preserve_groups = false) geometry =
  if grain <= 0 then invalid_arg "Rdk.Instance_copy.duplicate: grain must be positive";
  let selection_error = match primitives with
    | Some group when Group.owner group <> Group.Primitive ->
        Some "Rdk.Instance_copy.duplicate: source selection must own primitives"
    | Some group when Group.length group <> Geometry.primitive_count geometry ->
        Some "Rdk.Instance_copy.duplicate: source selection length does not match primitive count"
    | None | Some _ -> None in
  match selection_error with
  | Some message -> Error message
  | None when copies < 0 -> Error "Rdk.Instance_copy.duplicate: copy count must be non-negative"
  | None when copies = 0 -> Ok geometry
  | None when copies >= Sys.max_array_length ->
    Error "Rdk.Instance_copy.duplicate: copy count is too large"
  | None -> begin
    let full_matrices () =
      let matrices = Array.make (copies + 1) Rays_math.Mat4.identity in
      for copy = 1 to copies do
        matrices.(copy) <- if cumulative
          then Rays_math.Mat4.mul matrices.(copy - 1) transform else transform
      done;
      matrices in
    match primitives, copy_group_prefix with
    | None, None ->
        materialize_instances ?cancel ~grain ~transforms:(full_matrices ())
          geometry
    | _ ->
    let primitives_per_copy = match primitives with
      | None -> Geometry.primitive_count geometry
      | Some group -> Group.cardinality group in
    let group_preflight = match copy_group_prefix with
      | None -> Ok ()
      | Some prefix ->
          if primitives_per_copy <> 0
              && copies > (Sys.max_array_length
                - Geometry.primitive_count geometry) / primitives_per_copy
          then Error "Rdk.Instance_copy.duplicate: primitive output exceeds array limits"
          else Duplicate.validate_copy_groups ~prefix ~copies
              ~primitive_count:(Geometry.primitive_count geometry
                + (copies * primitives_per_copy)) in
    Result.bind group_preflight (fun () ->
    let output = match primitives with
      | Some primitives ->
          let added = Array.make copies transform in
          if cumulative then
            for copy = 1 to copies - 1 do
              added.(copy) <- Rays_math.Mat4.mul added.(copy - 1) transform
            done;
          Duplicate.selected ?cancel ~grain ~primitives ~transforms:added geometry
      | None ->
          materialize_instances ?cancel ~grain ~transforms:(full_matrices ())
            geometry in
    Result.bind output (fun output -> match copy_group_prefix with
      | None -> Ok output
      | Some prefix ->
          Duplicate.add_copy_groups ?cancel ~grain ~prefix
            ~preserve:preserve_groups ~copies ~primitives_per_copy output)
    )
  end

(* Each target point's copy frame, Houdini's instancing attributes in
   order: scale (pscale, scale), rotation rows (transform, else orient, else
   N/v with up), rot, then translation (P, trans, transform, pivot). [affine]
   records a [transform] attribute, whose normals need the inverse transpose. *)
type frames = {
  sx : float array; sy : float array; sz : float array;
  r00 : float array; r01 : float array; r02 : float array;
  r10 : float array; r11 : float array; r12 : float array;
  r20 : float array; r21 : float array; r22 : float array;
  tx : float array; ty : float array; tz : float array;
  affine : bool;
}

let copy_frames ?cancel ~grain targets =
  let copies = Geometry.point_count targets in
  Result.bind (point_float targets "pscale") (fun pscale ->
  Result.bind (point_float3 targets "scale") (fun scale ->
  Result.bind (point_float4 targets "orient") (fun orient ->
  Result.bind (point_float3 targets "N") (fun target_normal ->
  Result.bind (point_float3 targets "up") (fun target_up ->
  Result.bind (point_float3 targets "v") (fun target_velocity ->
  Result.bind (point_float4 targets "rot") (fun target_rotation ->
  Result.bind (point_float3 targets "trans") (fun target_translation ->
  Result.bind (point_float3 targets "pivot") (fun target_pivot ->
  Result.bind (point_affine_transform targets "transform") (fun target_transform ->
    let target_positions = Packed.Float3.Private.view (Geometry.positions targets) in
    let sx = Array.make copies 1. and sy = Array.make copies 1.
    and sz = Array.make copies 1. in
    if Option.is_none target_transform then begin
      Option.iter (fun values ->
        for index = 0 to copies - 1 do
          sx.(index) <- values.(index); sy.(index) <- values.(index);
          sz.(index) <- values.(index)
        done) pscale;
      Option.iter (fun values ->
        let values = Packed.Float3.Private.view values in
        for index = 0 to copies - 1 do
          sx.(index) <- sx.(index) *. values.x.(index);
          sy.(index) <- sy.(index) *. values.y.(index);
          sz.(index) <- sz.(index) *. values.z.(index)
        done) scale
    end;
    if Array.exists (fun value -> not (Float.is_finite value)) sx
       || Array.exists (fun value -> not (Float.is_finite value)) sy
       || Array.exists (fun value -> not (Float.is_finite value)) sz
    then Error "Rdk.Instance_copy.copy_to_points: target scales must be finite"
    else
      let r00 = Array.make copies 1. and r01 = Array.make copies 0.
      and r02 = Array.make copies 0. and r10 = Array.make copies 0.
      and r11 = Array.make copies 1. and r12 = Array.make copies 0.
      and r20 = Array.make copies 0. and r21 = Array.make copies 0.
      and r22 = Array.make copies 1. in
      let orientation_is_finite = ref true in
      let set_quaternion index x y z w =
        let scale = max (abs_float x) (max (abs_float y)
            (max (abs_float z) (abs_float w))) in
        if not (Float.is_finite scale) then orientation_is_finite := false
        else if scale > 0. then begin
          let x = x /. scale and y = y /. scale and z = z /. scale
          and w = w /. scale in
          let length = sqrt ((x *. x) +. (y *. y) +. (z *. z) +. (w *. w)) in
          let x = x /. length and y = y /. length and z = z /. length
          and w = w /. length in
          let xx = x *. x and yy = y *. y and zz = z *. z
          and xy = x *. y and xz = x *. z and yz = y *. z
          and wx = w *. x and wy = w *. y and wz = w *. z in
          r00.(index) <- 1. -. (2. *. (yy +. zz));
          r01.(index) <- 2. *. (xy -. wz);
          r02.(index) <- 2. *. (xz +. wy);
          r10.(index) <- 2. *. (xy +. wz);
          r11.(index) <- 1. -. (2. *. (xx +. zz));
          r12.(index) <- 2. *. (yz -. wx);
          r20.(index) <- 2. *. (xz -. wy);
          r21.(index) <- 2. *. (yz +. wx);
          r22.(index) <- 1. -. (2. *. (xx +. yy))
        end in
      let target_direction = match target_normal with
        | Some _ -> target_normal
        | None -> target_velocity in
      (match target_transform with
      | Some (transform, width) ->
          if copies > 0 then Parallel.for_ ~chunk_size:grain ~start:0
            ~finish:(copies - 1) (fun index ->
            if index land 4095 = 0 then Cancel.check_opt cancel;
            let first = transform.offsets.(index) in
            r00.(index) <- transform.values.(first);
            r01.(index) <- transform.values.(first + 1);
            r02.(index) <- transform.values.(first + 2);
            let second = first + if width = 9 then 3 else 4 in
            r10.(index) <- transform.values.(second);
            r11.(index) <- transform.values.(second + 1);
            r12.(index) <- transform.values.(second + 2);
            let third = first + if width = 9 then 6 else 8 in
            r20.(index) <- transform.values.(third);
            r21.(index) <- transform.values.(third + 1);
            r22.(index) <- transform.values.(third + 2)
          )
      | None -> match orient with
      | Some values ->
        let values = Packed.Float4.Private.view values in
        for index = 0 to copies - 1 do
          set_quaternion index values.x.(index) values.y.(index)
            values.z.(index) values.w.(index)
        done
      | None -> Option.iter (fun normals ->
          let normals = Packed.Float3.Private.view normals
          and up = Option.map Packed.Float3.Private.view target_up in
          for index = 0 to copies - 1 do
            let nx = normals.x.(index) and ny = normals.y.(index)
            and nz = normals.z.(index) in
            let nscale = max (abs_float nx) (max (abs_float ny) (abs_float nz)) in
            if not (Float.is_finite nscale) then orientation_is_finite := false
            else if nscale > 0. then begin
              let nx = nx /. nscale and ny = ny /. nscale and nz = nz /. nscale in
              let nlength = sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) in
              let zx = nx /. nlength and zy = ny /. nlength
              and zz = nz /. nlength in
              let aligned = match up with
                | None -> false
                | Some up ->
                    let ux = up.x.(index) and uy = up.y.(index)
                    and uz = up.z.(index) in
                    let uscale = max (abs_float ux)
                        (max (abs_float uy) (abs_float uz)) in
                    if not (Float.is_finite uscale) then begin
                      orientation_is_finite := false; true
                    end else if uscale = 0. then false
                    else begin
                      let ux = ux /. uscale and uy = uy /. uscale
                      and uz = uz /. uscale in
                      let xx = (uy *. zz) -. (uz *. zy)
                      and xy = (uz *. zx) -. (ux *. zz)
                      and xz = (ux *. zy) -. (uy *. zx) in
                      let xscale = max (abs_float xx)
                          (max (abs_float xy) (abs_float xz)) in
                      if xscale <= 1e-20 then false
                      else begin
                        let xx = xx /. xscale and xy = xy /. xscale
                        and xz = xz /. xscale in
                        let xlength = sqrt
                            ((xx *. xx) +. (xy *. xy) +. (xz *. xz)) in
                        let xx = xx /. xlength and xy = xy /. xlength
                        and xz = xz /. xlength in
                        let yx = (zy *. xz) -. (zz *. xy)
                        and yy = (zz *. xx) -. (zx *. xz)
                        and yz = (zx *. xy) -. (zy *. xx) in
                        r00.(index) <- xx; r10.(index) <- xy; r20.(index) <- xz;
                        r01.(index) <- yx; r11.(index) <- yy; r21.(index) <- yz;
                        r02.(index) <- zx; r12.(index) <- zy; r22.(index) <- zz;
                        true
                      end
                    end in
              if not aligned then
                if zz <= -1. +. 1e-12 then set_quaternion index 0. 1. 0. 0.
                else set_quaternion index (-.zy) zx 0. (1. +. zz)
            end
          done) target_direction);
      let post_quaternion index x y z w =
        let scale = max (abs_float x) (max (abs_float y)
            (max (abs_float z) (abs_float w))) in
        if not (Float.is_finite scale) then orientation_is_finite := false
        else if scale > 0. then begin
          let x = x /. scale and y = y /. scale and z = z /. scale
          and w = w /. scale in
          let length = sqrt ((x *. x) +. (y *. y) +. (z *. z) +. (w *. w)) in
          let x = x /. length and y = y /. length and z = z /. length
          and w = w /. length in
          let xx = x *. x and yy = y *. y and zz = z *. z
          and xy = x *. y and xz = x *. z and yz = y *. z
          and wx = w *. x and wy = w *. y and wz = w *. z in
          let a00 = 1. -. (2. *. (yy +. zz))
          and a01 = 2. *. (xy -. wz) and a02 = 2. *. (xz +. wy)
          and a10 = 2. *. (xy +. wz) and a11 = 1. -. (2. *. (xx +. zz))
          and a12 = 2. *. (yz -. wx) and a20 = 2. *. (xz -. wy)
          and a21 = 2. *. (yz +. wx) and a22 = 1. -. (2. *. (xx +. yy)) in
          let b00 = r00.(index) and b01 = r01.(index) and b02 = r02.(index)
          and b10 = r10.(index) and b11 = r11.(index) and b12 = r12.(index)
          and b20 = r20.(index) and b21 = r21.(index) and b22 = r22.(index) in
          r00.(index) <- (a00 *. b00) +. (a01 *. b10) +. (a02 *. b20);
          r01.(index) <- (a00 *. b01) +. (a01 *. b11) +. (a02 *. b21);
          r02.(index) <- (a00 *. b02) +. (a01 *. b12) +. (a02 *. b22);
          r10.(index) <- (a10 *. b00) +. (a11 *. b10) +. (a12 *. b20);
          r11.(index) <- (a10 *. b01) +. (a11 *. b11) +. (a12 *. b21);
          r12.(index) <- (a10 *. b02) +. (a11 *. b12) +. (a12 *. b22);
          r20.(index) <- (a20 *. b00) +. (a21 *. b10) +. (a22 *. b20);
          r21.(index) <- (a20 *. b01) +. (a21 *. b11) +. (a22 *. b21);
          r22.(index) <- (a20 *. b02) +. (a21 *. b12) +. (a22 *. b22)
        end in
      if Option.is_none target_transform then
        Option.iter (fun rotations ->
          let rotations = Packed.Float4.Private.view rotations in
          for index = 0 to copies - 1 do
            post_quaternion index rotations.x.(index) rotations.y.(index)
              rotations.z.(index) rotations.w.(index)
          done) target_rotation;
      if not !orientation_is_finite then
        Error "Rdk.Instance_copy.copy_to_points: target orientations must be finite"
      else
      let owns_translation = Option.is_some target_translation
          || Option.is_some target_pivot || Option.is_some target_transform in
      let tx = if owns_translation then Array.copy target_positions.x
        else target_positions.x
      and ty = if owns_translation then Array.copy target_positions.y
        else target_positions.y
      and tz = if owns_translation then Array.copy target_positions.z
        else target_positions.z in
      Option.iter (fun translation ->
        let translation = Packed.Float3.Private.view translation in
        for index = 0 to copies - 1 do
          tx.(index) <- tx.(index) +. translation.x.(index);
          ty.(index) <- ty.(index) +. translation.y.(index);
          tz.(index) <- tz.(index) +. translation.z.(index)
        done) target_translation;
      (match target_transform with
      | Some (transform, 16) ->
          if copies > 0 then Parallel.for_ ~chunk_size:grain ~start:0
            ~finish:(copies - 1) (fun index ->
            if index land 4095 = 0 then Cancel.check_opt cancel;
            let first = transform.offsets.(index) in
            tx.(index) <- tx.(index) +. transform.values.(first + 3);
            ty.(index) <- ty.(index) +. transform.values.(first + 7);
            tz.(index) <- tz.(index) +. transform.values.(first + 11)
          )
      | Some (_, 9) | None -> ()
      | Some _ -> assert false);
      Option.iter (fun pivot ->
        let pivot = Packed.Float3.Private.view pivot in
        if copies > 0 then Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(copies - 1) (fun index ->
          if index land 4095 = 0 then Cancel.check_opt cancel;
          let x = pivot.x.(index) *. sx.(index)
          and y = pivot.y.(index) *. sy.(index)
          and z = pivot.z.(index) *. sz.(index) in
          tx.(index) <- tx.(index) -. ((r00.(index) *. x)
              +. (r01.(index) *. y) +. (r02.(index) *. z));
          ty.(index) <- ty.(index) -. ((r10.(index) *. x)
              +. (r11.(index) *. y) +. (r12.(index) *. z));
          tz.(index) <- tz.(index) -. ((r20.(index) *. x)
              +. (r21.(index) *. y) +. (r22.(index) *. z))
        )) target_pivot;
      if Array.exists (fun value -> not (Float.is_finite value)) tx
          || Array.exists (fun value -> not (Float.is_finite value)) ty
          || Array.exists (fun value -> not (Float.is_finite value)) tz then
        Error "Rdk.Instance_copy.copy_to_points: target translations must be finite"
      else
      Ok { sx; sy; sz; r00; r01; r02; r10; r11; r12; r20; r21; r22; tx; ty; tz;
           affine = Option.is_some target_transform }))))))))))

let copy_to_points_all ?cancel ?(grain = 16_384) ~target_attributes
    ~source ~targets () =
  if grain <= 0 then invalid_arg "Rdk.Instance_copy.copy_to_points: grain must be positive";
  let copies = Geometry.point_count targets and source_points = Geometry.point_count source
  and source_vertices = Geometry.vertex_count source
  and source_primitives = Geometry.primitive_count source in
  Result.bind (checked_product "Rdk.Instance_copy.copy_to_points" copies source_points)
    (fun output_points ->
  Result.bind (checked_product "Rdk.Instance_copy.copy_to_points" copies source_vertices)
    (fun output_vertices ->
  Result.bind (checked_product "Rdk.Instance_copy.copy_to_points" copies source_primitives)
    (fun output_primitives ->
  Result.bind (copy_frames ?cancel ~grain targets) (fun
      { sx; sy; sz; r00; r01; r02; r10; r11; r12; r20; r21; r22; tx; ty; tz; affine } ->
    let source_positions = Packed.Float3.Private.view (Geometry.positions source)
    and source_topology = Topology.Private.view (Geometry.topology source) in
      let px = Array.make output_points 0. and py = Array.make output_points 0.
      and pz = Array.make output_points 0. in
      if copies > 0 then Parallel.for_ ~chunk_size:(max 1 (grain / max 1 source_points))
          ~start:0 ~finish:(copies - 1) (fun copy ->
            let first = copy * source_points in
            for point = 0 to source_points - 1 do
              if point land 4095 = 0 then Cancel.check_opt cancel;
              let x = source_positions.x.(point) *. sx.(copy)
              and y = source_positions.y.(point) *. sy.(copy)
              and z = source_positions.z.(point) *. sz.(copy) in
              let output = first + point in
              px.(output) <- (r00.(copy) *. x) +. (r01.(copy) *. y)
                +. (r02.(copy) *. z) +. tx.(copy);
              py.(output) <- (r10.(copy) *. x) +. (r11.(copy) *. y)
                +. (r12.(copy) *. z) +. ty.(copy);
              pz.(output) <- (r20.(copy) *. x) +. (r21.(copy) *. y)
                +. (r22.(copy) *. z) +. tz.(copy)
            done);
      let matrix_normals_valid = ref true in
      (if affine then begin
          let has_normals = Geometry.find_attribute ~owner:Attribute.Point "N" source
                <> None
              || Geometry.find_attribute ~owner:Attribute.Vertex "N" source <> None in
          if has_normals then begin
            let singular = Bytes.make copies '\000' in
            if copies > 0 then Parallel.for_ ~chunk_size:grain ~start:0
              ~finish:(copies - 1) (fun index ->
              if index land 4095 = 0 then Cancel.check_opt cancel;
              let a = r00.(index) and b = r01.(index) and c = r02.(index)
              and d = r10.(index) and e = r11.(index) and f = r12.(index)
              and g = r20.(index) and h = r21.(index) and i = r22.(index) in
              let matrix_scale = max (abs_float a) (max (abs_float b)
                  (max (abs_float c) (max (abs_float d) (max (abs_float e)
                    (max (abs_float f) (max (abs_float g)
                      (max (abs_float h) (abs_float i)))))))) in
              if matrix_scale = 0. then Bytes.set singular index '\001'
              else begin
                let a = a /. matrix_scale and b = b /. matrix_scale
                and c = c /. matrix_scale and d = d /. matrix_scale
                and e = e /. matrix_scale and f = f /. matrix_scale
                and g = g /. matrix_scale and h = h /. matrix_scale
                and i = i /. matrix_scale in
                let c00 = (e *. i) -. (f *. h)
                and c01 = (f *. g) -. (d *. i)
                and c02 = (d *. h) -. (e *. g)
                and c10 = (c *. h) -. (b *. i)
                and c11 = (a *. i) -. (c *. g)
                and c12 = (b *. g) -. (a *. h)
                and c20 = (b *. f) -. (c *. e)
                and c21 = (c *. d) -. (a *. f)
                and c22 = (a *. e) -. (b *. d) in
                let determinant = (a *. c00) +. (b *. c01) +. (c *. c02) in
                if abs_float determinant <= 1e-20 then
                  Bytes.set singular index '\001'
                else begin
                  let sign = if determinant < 0. then -1. else 1. in
                  r00.(index) <- c00 *. sign; r01.(index) <- c01 *. sign;
                  r02.(index) <- c02 *. sign; r10.(index) <- c10 *. sign;
                  r11.(index) <- c11 *. sign; r12.(index) <- c12 *. sign;
                  r20.(index) <- c20 *. sign; r21.(index) <- c21 *. sign;
                  r22.(index) <- c22 *. sign
                end
              end
            );
            if Bytes.contains singular '\001' then matrix_normals_valid := false
          end end);
      let vertex_points = Array.make output_vertices 0
      and primitive_offsets = Array.make (output_primitives + 1) 0
      and primitive_kinds = Bytes.make output_primitives '\000' in
      for copy = 0 to copies - 1 do
        Cancel.check_opt cancel;
        let point_offset = copy * source_points and vertex_offset = copy * source_vertices
        and primitive_offset = copy * source_primitives in
        for vertex = 0 to source_vertices - 1 do
          vertex_points.(vertex_offset + vertex) <-
            source_topology.vertex_points.(vertex) + point_offset
        done;
        for primitive = 0 to source_primitives - 1 do
          primitive_offsets.(primitive_offset + primitive) <-
            source_topology.primitive_offsets.(primitive) + vertex_offset
        done;
        Bytes.blit source_topology.primitive_kinds 0 primitive_kinds
          primitive_offset source_primitives
      done;
      primitive_offsets.(output_primitives) <- output_vertices;
      let topology = Topology.Private.create_validated_owned
          ~point_count:output_points ~vertex_points ~primitive_offsets
          ~primitive_kinds in
      let rec build_attributes result = function
        | [] -> Ok (List.rev result)
        | attribute :: rest
          when Attribute.name attribute = "N"
            && (Attribute.owner attribute = Attribute.Point
                || Attribute.owner attribute = Attribute.Vertex) ->
            build_attributes result rest
        | attribute :: rest ->
            Result.bind (repeat_attribute ?cancel ~grain copies attribute)
            (fun attribute -> build_attributes (attribute :: result) rest) in
      Result.bind (build_attributes [] (Geometry.attributes source)) (fun attributes ->
        let output_positions = Packed.Float3.Private.of_owned_exn ~x:px ~y:py ~z:pz in
        let edge_groups = match Geometry.edge_groups source with
          | [] -> []
          | source_groups ->
              let source_index = Topology_index.create ?cancel
                  (Geometry.topology source)
              in
              List.map (fun group -> Edge_group.replicate_exact_copies ?cancel
                ~source_topology:(Geometry.topology source) ~source_index
                ~target_topology:topology ~copies group |> Support.get_ok) source_groups in
        let base = Geometry.create ~positions:output_positions ~topology
            ~attributes ~edge_groups () in
        Result.bind base (fun geometry ->
          let geometry = ref geometry in
          List.iter (fun owner ->
            if not !matrix_normals_valid then ()
            else match Geometry.find_attribute ~owner "N" source with
            | None -> ()
            | Some attribute ->
                (match Attribute.get (Attribute.normal ~owner) attribute with
                 | None -> ()
                 | Some values ->
                     let values = Packed.Float3.Private.view values in
                     let source_count = Array.length values.x
                     and count = copies * Array.length values.x in
                     let nx = Array.make count 0. and ny = Array.make count 0.
                     and nz = Array.make count 0. in
                     if copies > 0 then Parallel.for_
                       ~chunk_size:(max 1 (grain / max 1 source_count))
                       ~start:0 ~finish:(copies - 1) (fun copy ->
                         let first = copy * source_count in
                         for index = 0 to source_count - 1 do
                           if index land 4095 = 0 then Cancel.check_opt cancel;
                           let ix = if abs_float sx.(copy) <= 1e-20 then 0.
                             else values.x.(index) /. sx.(copy)
                           and iy = if abs_float sy.(copy) <= 1e-20 then 0.
                             else values.y.(index) /. sy.(copy)
                           and iz = if abs_float sz.(copy) <= 1e-20 then 0.
                             else values.z.(index) /. sz.(copy) in
                           let x = (r00.(copy) *. ix) +. (r01.(copy) *. iy)
                             +. (r02.(copy) *. iz)
                           and y = (r10.(copy) *. ix) +. (r11.(copy) *. iy)
                             +. (r12.(copy) *. iz)
                           and z = (r20.(copy) *. ix) +. (r21.(copy) *. iy)
                             +. (r22.(copy) *. iz) in
                           let length = sqrt ((x*.x) +. (y*.y) +. (z*.z)) in
                           if length > 1e-20 then begin
                             nx.(first + index) <- x /. length;
                             ny.(first + index) <- y /. length;
                             nz.(first + index) <- z /. length
                           end
                         done);
                     let normal = Packed.Float3.Private.of_owned_exn ~x:nx ~y:ny ~z:nz in
                     let attribute = Attribute.create_key_owned
                         (Attribute.normal ~owner) normal |> Support.get_ok in
                     geometry := Geometry.with_attribute attribute !geometry |> Support.get_ok))
            [Attribute.Point; Attribute.Vertex];
          let output_groups = List.map (fun group ->
            repeat_group ?cancel ~grain copies group)
              (Geometry.groups source) in
          let result = List.fold_left (fun result group ->
            Result.bind result (Geometry.with_group group)) (Ok !geometry)
            output_groups in
          Result.bind result (apply_copy_target_rules ?cancel ~grain ~copies
            ~source_points ~source_vertices ~source_primitives target_attributes
            targets)))))))

type copy_piece_values = Copy_piece_int of int array
  | Copy_piece_text of string array

let copy_piece_values attribute = match Attribute.Private.storage attribute with
  | Attribute.Int values -> Ok (Copy_piece_int values)
  | Attribute.Text values -> Ok (Copy_piece_text values)
  | _ -> Error (Printf.sprintf
      "Rdk.Instance_copy.copy_to_points: piece attribute %S must use int or text storage"
      (Attribute.name attribute))

let plan_copy_pieces ?cancel ~name source targets =
  let target_attribute = Geometry.find_attribute ~owner:Attribute.Point name
      targets in
  match target_attribute with
  | None -> Error (Printf.sprintf
      "Rdk.Instance_copy.copy_to_points: target point piece attribute %S does not exist"
      name)
  | Some target_attribute ->
      Result.bind (copy_piece_values target_attribute) (fun target_values ->
      let source_primitive = Geometry.find_attribute
          ~owner:Attribute.Primitive name source
      and source_point = Geometry.find_attribute ~owner:Attribute.Point name source in
      let source_attribute = match source_primitive, source_point with
        | Some attribute, _ -> Some (`Primitive, attribute)
        | None, Some attribute -> Some (`Point, attribute)
        | None, None -> None in
      match source_attribute, target_values with
      | None, Copy_piece_text _ -> Error (Printf.sprintf
          "Rdk.Instance_copy.copy_to_points: text target piece attribute %S requires a matching source point or primitive attribute"
          name)
      | None, Copy_piece_int target_values ->
          let piece_count = Geometry.primitive_count source in
          let primitive_pieces = Array.init piece_count Fun.id in
          let target_pieces = Array.mapi (fun target primitive ->
            if target land 4095 = 0 then Cancel.check_opt cancel;
            if primitive >= 0 && primitive < piece_count then primitive else -1)
              target_values in
          Ok (piece_count, None, primitive_pieces, target_pieces)
      | Some (owner, attribute), _ ->
          Result.bind (copy_piece_values attribute) (fun source_values ->
          let type_matches = match source_values, target_values with
            | Copy_piece_int _, Copy_piece_int _
            | Copy_piece_text _, Copy_piece_text _ -> true
            | _ -> false in
          if not type_matches then Error (Printf.sprintf
            "Rdk.Instance_copy.copy_to_points: source and target piece attribute %S storage must match"
            name)
          else
            let next_piece = ref 0 in
            let point_pieces, primitive_pieces, target_pieces =
              match source_values, target_values with
              | Copy_piece_int source_values, Copy_piece_int target_values ->
                  let pieces = Hashtbl.create (max 16 (Array.length source_values)) in
                  let intern value = match Hashtbl.find_opt pieces value with
                    | Some piece -> piece
                    | None -> let piece = !next_piece in incr next_piece;
                        Hashtbl.add pieces value piece; piece in
                  let source_pieces = Array.map intern source_values in
                  let target_pieces = Array.mapi (fun target value ->
                    if target land 4095 = 0 then Cancel.check_opt cancel;
                    Option.value ~default:(-1) (Hashtbl.find_opt pieces value))
                      target_values in
                  (match owner with
                   | `Primitive -> None, source_pieces, target_pieces
                   | `Point ->
                       let topology = Topology.Private.view
                           (Geometry.topology source) in
                       let primitive_pieces = Array.init
                           (Geometry.primitive_count source) (fun primitive ->
                             let first = topology.primitive_offsets.(primitive)
                             and last = topology.primitive_offsets.(primitive + 1) in
                             let piece = source_pieces.(topology.vertex_points.(first)) in
                             let coherent = ref true in
                             for vertex = first + 1 to last - 1 do
                               if source_pieces.(topology.vertex_points.(vertex)) <> piece
                               then coherent := false
                             done;
                             if !coherent then piece else -1) in
                       Some source_pieces, primitive_pieces, target_pieces)
              | Copy_piece_text source_values, Copy_piece_text target_values ->
                  let pieces = Hashtbl.create (max 16 (Array.length source_values)) in
                  let intern value = match Hashtbl.find_opt pieces value with
                    | Some piece -> piece
                    | None -> let piece = !next_piece in incr next_piece;
                        Hashtbl.add pieces value piece; piece in
                  let source_pieces = Array.map intern source_values in
                  let target_pieces = Array.mapi (fun target value ->
                    if target land 4095 = 0 then Cancel.check_opt cancel;
                    Option.value ~default:(-1) (Hashtbl.find_opt pieces value))
                      target_values in
                  (match owner with
                   | `Primitive -> None, source_pieces, target_pieces
                   | `Point ->
                       let topology = Topology.Private.view
                           (Geometry.topology source) in
                       let primitive_pieces = Array.init
                           (Geometry.primitive_count source) (fun primitive ->
                             let first = topology.primitive_offsets.(primitive)
                             and last = topology.primitive_offsets.(primitive + 1) in
                             let piece = source_pieces.(topology.vertex_points.(first)) in
                             let coherent = ref true in
                             for vertex = first + 1 to last - 1 do
                               if source_pieces.(topology.vertex_points.(vertex)) <> piece
                               then coherent := false
                             done;
                             if !coherent then piece else -1) in
                       Some source_pieces, primitive_pieces, target_pieces)
              | _ -> assert false in
            Ok (!next_piece, point_pieces, primitive_pieces, target_pieces)))

let without_detail_attributes geometry =
  Geometry.attributes geometry |> List.fold_left (fun geometry attribute ->
    if Attribute.owner attribute = Attribute.Detail then
      Geometry.without_attribute ~owner:Attribute.Detail
        (Attribute.name attribute) geometry
    else geometry) geometry

let restore_detail_attributes source geometry =
  Geometry.attributes source |> List.fold_left (fun result attribute ->
    if Attribute.owner attribute <> Attribute.Detail then result
    else Result.bind result (Geometry.with_attribute attribute)) (Ok geometry)

let array_is_identity values =
  let identity = ref true in
  let index = ref 0 in
  while !identity && !index < Array.length values do
    if values.(!index) <> !index then identity := false;
    incr index
  done;
  !identity

let empty_copy_targets ?cancel ~grain targets =
  let empty = Group.init ~owner:Group.Point ~name:"copy_piece_empty"
      (Geometry.point_count targets) (fun _ -> false) in
  Error.unguard (Deletion.delete ?cancel ~grain ~selected:false empty targets)

let checked_piece_cardinality label target_counts source_pieces owner_count =
  let rec loop piece total =
    if piece = Array.length target_counts then Ok ()
    else Result.bind (checked_product "Rdk.Instance_copy.copy_to_points"
        target_counts.(piece) (owner_count source_pieces.(piece))) (fun count ->
      if total > max_int - count then Error (Printf.sprintf
        "Rdk.Instance_copy.copy_to_points: output %s cardinality exceeds OCaml array limits"
        label)
      else loop (piece + 1) (total + count)) in
  loop 0 0

let copy_to_points_pieces ?cancel ~grain ~target_attributes ~piece_attribute
    ~source ~targets () =
  Result.bind (plan_copy_pieces ?cancel ~name:piece_attribute source targets)
    (fun (piece_count, point_pieces, primitive_pieces, target_pieces) ->
  let source_pieces = Deletion.primitive_partitions ?cancel ~grain ~piece_count
      ?point_pieces ~primitive_pieces source
  and no_target_primitives = Array.make (Geometry.primitive_count targets) (-1) in
  let target_pieces_geometry = Deletion.primitive_partitions ?cancel ~grain
      ~piece_count ~point_pieces:target_pieces
      ~primitive_pieces:no_target_primitives targets in
  let target_counts = Array.map Geometry.point_count target_pieces_geometry in
  let active_count = Array.fold_left (fun count targets ->
      if targets = 0 then count else count + 1) 0 target_counts in
  let cardinalities =
    Result.bind (checked_piece_cardinality "point" target_counts source_pieces
      Geometry.point_count) (fun () ->
    Result.bind (checked_piece_cardinality "vertex" target_counts source_pieces
      Geometry.vertex_count) (fun () ->
    checked_piece_cardinality "primitive" target_counts source_pieces
      Geometry.primitive_count)) in
  Result.bind cardinalities (fun () ->
  if active_count = 0 then
    Result.bind (empty_copy_targets ?cancel ~grain targets) (fun targets ->
      copy_to_points_all ?cancel ~grain ~target_attributes ~source ~targets ())
  else begin
    let outputs = Array.make piece_count None in
    let rec cook piece =
      if piece = piece_count then Ok ()
      else if target_counts.(piece) = 0 then cook (piece + 1)
      else Result.bind (copy_to_points_all ?cancel ~grain ~target_attributes
          ~source:source_pieces.(piece) ~targets:target_pieces_geometry.(piece) ())
        (fun output -> outputs.(piece) <- Some output; cook (piece + 1)) in
    Result.bind (cook 0) (fun () ->
    let source_has_normal owner =
      Geometry.find_attribute ~owner "N" source <> None in
    let drop_point_normal = source_has_normal Attribute.Point
      && Array.exists (function None -> false | Some output ->
        Geometry.find_attribute ~owner:Attribute.Point "N" output = None)
        outputs
    and drop_vertex_normal = source_has_normal Attribute.Vertex
      && Array.exists (function None -> false | Some output ->
        Geometry.find_attribute ~owner:Attribute.Vertex "N" output = None)
        outputs in
    let normalized_outputs = Array.map (Option.map (fun output ->
      let output = if drop_point_normal then
          Geometry.without_attribute ~owner:Attribute.Point "N" output
        else output in
      if drop_vertex_normal then
        Geometry.without_attribute ~owner:Attribute.Vertex "N" output
      else output)) outputs in
    let point_bases = Array.make piece_count 0
    and primitive_bases = Array.make piece_count 0
    and point_total = ref 0 and primitive_total = ref 0
    and batches = ref [] in
    Array.iteri (fun piece output -> match output with
      | None -> ()
      | Some output ->
          point_bases.(piece) <- !point_total;
          primitive_bases.(piece) <- !primitive_total;
          point_total := !point_total + Geometry.point_count output;
          primitive_total := !primitive_total + Geometry.primitive_count output;
          batches := without_detail_attributes output :: !batches)
      normalized_outputs;
    Result.bind (merge ?cancel ~grain (List.rev !batches)) (fun merged ->
    Result.bind (restore_detail_attributes source merged) (fun merged ->
    let target_count = Array.length target_pieces in
    let target_ranks = Array.make target_count (-1)
    and piece_next = Array.make piece_count 0
    and point_offsets = Array.make (target_count + 1) 0
    and primitive_offsets = Array.make (target_count + 1) 0 in
    for target = 0 to target_count - 1 do
      let piece = target_pieces.(target) in
      if piece >= 0 then begin
        target_ranks.(target) <- piece_next.(piece);
        piece_next.(piece) <- piece_next.(piece) + 1;
        point_offsets.(target + 1) <- point_offsets.(target)
          + Geometry.point_count source_pieces.(piece);
        primitive_offsets.(target + 1) <- primitive_offsets.(target)
          + Geometry.primitive_count source_pieces.(piece)
      end else begin
        point_offsets.(target + 1) <- point_offsets.(target);
        primitive_offsets.(target + 1) <- primitive_offsets.(target)
      end
    done;
    let point_permutation = Array.make point_offsets.(target_count) 0
    and primitive_permutation = Array.make primitive_offsets.(target_count) 0 in
    if target_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
      ~finish:(target_count - 1) (fun target ->
        if target land 4095 = 0 then Cancel.check_opt cancel;
        let piece = target_pieces.(target) in
        if piece >= 0 then begin
          let rank = target_ranks.(target)
          and points = Geometry.point_count source_pieces.(piece)
          and primitives = Geometry.primitive_count source_pieces.(piece) in
          let source_point = point_bases.(piece) + (rank * points)
          and source_primitive = primitive_bases.(piece) + (rank * primitives) in
          for local = 0 to points - 1 do
            point_permutation.(point_offsets.(target) + local) <-
              source_point + local
          done;
          for local = 0 to primitives - 1 do
            primitive_permutation.(primitive_offsets.(target) + local) <-
              source_primitive + local
          done
        end);
    let merged = if array_is_identity point_permutation then merged
      else Ordering.apply_points ?cancel ~grain point_permutation merged in
    let merged = if array_is_identity primitive_permutation then merged
      else Ordering.apply_primitives ?cancel ~grain primitive_permutation merged in
    Ok merged)))
  end))

let copy_to_points ?cancel ?(grain = 16_384) ?source_primitives ?target_points
    ?piece_attribute ?(target_attributes = []) ~source ~targets () =
  if grain <= 0 then invalid_arg "Rdk.Instance_copy.copy_to_points: grain must be positive";
  Result.bind (compile_copy_target_rules target_attributes) (fun target_attributes ->
  let prepare_source = match source_primitives with
    | None -> Ok source
    | Some group when Group.owner group <> Group.Primitive ->
        Error "Rdk.Instance_copy.copy_to_points: source selection must own primitives"
    | Some group -> Error.unguard (Deletion.delete ?cancel ~grain ~selected:false
        ~compact_points:true group source) in
  Result.bind prepare_source (fun source ->
    let prepare_targets = match target_points with
      | None -> Ok targets
      | Some group when Group.owner group <> Group.Point ->
          Error "Rdk.Instance_copy.copy_to_points: target selection must own points"
      | Some group -> Error.unguard
          (Deletion.delete ?cancel ~grain ~selected:false group targets) in
    Result.bind prepare_targets (fun targets -> match piece_attribute with
      | None -> copy_to_points_all ?cancel ~grain ~target_attributes ~source
          ~targets ()
      | Some name when String.trim name = "" ->
          Error "Rdk.Instance_copy.copy_to_points: piece attribute name must not be empty"
      | Some name -> copy_to_points_pieces ?cancel ~grain ~target_attributes
          ~piece_attribute:name ~source ~targets ())))

let copy_to_points_raw = copy_to_points
let materialize_instances_raw = materialize_instances
let duplicate_raw = duplicate

module Private = struct
  let copy_to_points ?cancel ?grain ~source ~targets () =
    copy_to_points_raw ?cancel ?grain ~source ~targets ()
end

let copy_to_points ?cancel ?grain ?source_primitives ?target_points
    ?piece_attribute ?target_attributes ~source ~targets () =
  let selection_error = match source_primitives, target_points with
    | Some group, _ when Group.owner group <> Group.Primitive ->
        Some "source selection must own primitives"
    | Some group, _ when Group.length group <> Geometry.primitive_count source ->
        Some "source selection length does not match source primitive count"
    | _, Some group when Group.owner group <> Group.Point ->
        Some "target selection must own points"
    | _, Some group when Group.length group <> Geometry.point_count targets ->
        Some "target selection length does not match target point count"
    | _ -> None in
  match selection_error with
  | Some message -> Error
      (Error.of_string ~operation:"copy_to_points" ~code:"invalid_selection" message)
  | None -> Error.guard ~operation:"copy_to_points" ~code:"invalid_attribute"
      (fun () -> copy_to_points_raw ?cancel ?grain ?source_primitives
        ?target_points ?piece_attribute ?target_attributes ~source ~targets ())

(* Each (selected) target's copy transform, from the frames
   [copy_to_points] places copies with: rows R·diag(s) and translation t. *)
let copy_transforms ?cancel ?(grain = 16_384) ?target_points targets =
  Error.guard ~operation:"copy_transforms" ~code:"invalid_attribute" (fun () ->
    Result.bind (copy_frames ?cancel ~grain targets) (fun f ->
      let selected = match target_points with
        | None -> Array.init (Geometry.point_count targets) Fun.id
        | Some group -> Array.of_seq (Seq.filter (fun index -> Group.mem index group)
            (Seq.init (Geometry.point_count targets) Fun.id)) in
      Ok (Array.map (fun i ->
        Rays_math.Mat4.of_rows
          (f.r00.(i) *. f.sx.(i), f.r01.(i) *. f.sy.(i), f.r02.(i) *. f.sz.(i), f.tx.(i))
          (f.r10.(i) *. f.sx.(i), f.r11.(i) *. f.sy.(i), f.r12.(i) *. f.sz.(i), f.ty.(i))
          (f.r20.(i) *. f.sx.(i), f.r21.(i) *. f.sy.(i), f.r22.(i) *. f.sz.(i), f.tz.(i))
          (0., 0., 0., 1.)) selected)))

let materialize_instances ~transforms geometry =
  Error.guard ~operation:"materialize_instances" ~code:"invalid_parameter"
    (fun () -> materialize_instances_raw ~transforms geometry)

let duplicate ?cancel ?grain ?copies ?cumulative ?transform ?primitives
    ?copy_group_prefix ?preserve_groups geometry =
  Error.guard ~operation:"duplicate" ~code:"invalid_parameter"
    (fun () -> duplicate_raw ?cancel ?grain ?copies ?cumulative ?transform
      ?primitives ?copy_group_prefix ?preserve_groups geometry)
