

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
  Support.run_ranges ~grain length (fun ~first ~last -> Cancel.check_opt cancel; body ~first ~last)

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
    let storage = match Attribute.View.storage attribute with
      | Attribute.Float values ->
          Attribute.Float (repeat_array ?cancel ~grain copies values)
      | Attribute.Int values ->
          Attribute.Int (repeat_array ?cancel ~grain copies values)
      | Attribute.Text values ->
          Attribute.Text (repeat_array ?cancel ~grain copies values)
      | Attribute.Float2 values ->
          let view = Packed.Float2.View.view values in
          Attribute.Float2 (Packed.Float2.of_owned
            ~x:(repeat_array ?cancel ~grain copies view.x)
            ~y:(repeat_array ?cancel ~grain copies view.y) |> Support.get_ok)
      | Attribute.Float3 values ->
          let view = Packed.Float3.View.view values in
          Attribute.Float3 (Packed.Float3.View.of_owned_exn
            ~x:(repeat_array ?cancel ~grain copies view.x)
            ~y:(repeat_array ?cancel ~grain copies view.y)
            ~z:(repeat_array ?cancel ~grain copies view.z))
      | Attribute.Float4 values ->
          let view = Packed.Float4.View.view values in
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
  match Group.View.order_view group with
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
      Group.View.with_owned_order order target

let transform_single_instance ?cancel ~grain matrix geometry =
  let source = Packed.Float3.View.view (Geometry.positions geometry) in
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
  let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
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
                 let source = Packed.Float3.View.view source in
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
                 let value = Packed.Float3.View.of_owned_exn ~x ~y ~z in
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
      let source_positions = Packed.Float3.View.view (Geometry.positions geometry) in
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
      let source_topology = Topology.View.view (Geometry.topology geometry) in
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
      let topology = Topology.View.create_validated_owned ~point_count:output_points
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
        let source = Packed.Float3.View.view source in
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
        let value = Packed.Float3.View.of_owned_exn ~x:nx ~y:ny ~z:nz in
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
        let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
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

(* Each target point's copy frame, Houdini's instancing attributes in
   order: scale (pscale, scale), rotation rows (transform, else orient, else
   N/v with up), rot, then translation (P, trans, transform, pivot). [affine]
   records a [transform] attribute, whose normals need the inverse transpose. *)

let materialize_instances_raw = materialize_instances

(* Each (selected) target's copy transform, from the frames
   [copy_to_points] places copies with: rows R·diag(s) and translation t. *)

let materialize_instances ~transforms geometry =
  Error.guard ~operation:"materialize_instances" ~code:"invalid_parameter"
    (fun () -> materialize_instances_raw ~transforms geometry)
