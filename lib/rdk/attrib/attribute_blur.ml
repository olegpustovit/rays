
type mode = Laplacian of float | Custom_steps of { odd : float; even : float }

type blur_target =
  | Blur_position of int
  | Blur_float of Attribute.t * int
  | Blur_float2 of Attribute.t * int
  | Blur_float3 of Attribute.t * int
  | Blur_float4 of Attribute.t * int

let run ?cancel ?(grain = 16_384) ?selection ?(iterations = 1)
    ?(mode = Laplacian 0.5) ~pattern geometry =
  Error.guard ~cancelled:"attribute blur was cancelled" ~invalid_argument:"invalid_parameter" ~operation:"attribute_blur"
    ~code:"invalid_blur" @@ fun () ->
  if grain <= 0 then invalid_arg
      "Rdk.Attribute_ops.blur_points: grain must be positive";
  if iterations < 0 then Error
      "Rdk.Attribute_ops.blur_points: iterations must be non-negative"
  else
    let steps_valid = match mode with
      | Laplacian step -> Float.is_finite step
      | Custom_steps { odd; even } ->
          Float.is_finite odd && Float.is_finite even in
    if not steps_valid then Error
        "Rdk.Attribute_ops.blur_points: step sizes must be finite"
    else
      let point_count = Geometry.point_count geometry in
      let selection_result = match selection with
        | Some group when Group.owner group <> Group.Point
            || Group.length group <> point_count -> Error
              "Rdk.Attribute_ops.blur_points: selection must be a matching point group"
        | None | Some _ -> Ok () in
      Result.bind selection_result (fun () ->
      if String.trim pattern = "" || iterations = 0
          || (match selection with Some group -> Group.cardinality group = 0
               | None -> point_count = 0)
      then Ok geometry
      else Result.bind (Attribute_pattern.compile pattern) (fun compiled ->
      let targets = ref [] and planes = ref [] and plane_count = ref 0 in
      let add_target make arrays =
        let first = !plane_count in
        plane_count := first + List.length arrays;
        List.iter (fun values -> planes := values :: !planes) arrays;
        targets := make first :: !targets in
      let positions = Packed.Float3.View.view (Geometry.positions geometry) in
      if Attribute_pattern.matches compiled "P" then
        add_target (fun first -> Blur_position first)
          [positions.x; positions.y; positions.z];
      List.iter (fun attribute ->
        if Attribute.owner attribute = Attribute.Point
            && Attribute_pattern.matches compiled (Attribute.name attribute) then
          match Attribute.View.storage attribute with
          | Attribute.Float values ->
              add_target (fun first -> Blur_float (attribute, first)) [values]
          | Attribute.Float2 values ->
              let values = Packed.Float2.View.view values in
              add_target (fun first -> Blur_float2 (attribute, first))
                [values.x; values.y]
          | Attribute.Float3 values ->
              let values = Packed.Float3.View.view values in
              add_target (fun first -> Blur_float3 (attribute, first))
                [values.x; values.y; values.z]
          | Attribute.Float4 values ->
              let values = Packed.Float4.View.view values in
              add_target (fun first -> Blur_float4 (attribute, first))
                [values.x; values.y; values.z; values.w]
          | Attribute.Int _ | Attribute.Text _ | Attribute.Int_array _
          | Attribute.Float_array _ -> ())
        (Geometry.attributes geometry);
      let targets = Array.of_list (List.rev !targets)
      and original = Array.of_list (List.rev !planes) in
      if Array.length targets = 0 then Ok geometry
      else begin
        let failure = ref None in
        Array.iteri (fun plane values ->
          let point = ref 0 in
          while !point < point_count && !failure = None do
            if !point land 16_383 = 0 then Cancel.check_opt cancel;
            if not (Float.is_finite values.(!point)) then
              failure := Some (Printf.sprintf
                  "Rdk.Attribute_ops.blur_points: plane %d contains a non-finite value at point %d"
                  plane !point);
            incr point
          done) original;
        Result.bind (match !failure with None -> Ok () | Some message -> Error message)
          (fun () ->
        let topology_index = Topology_index.create ?cancel (Geometry.topology geometry)
        and current = ref (Array.map Array.copy original)
        and next = ref (Array.map Array.copy original) in
        let topology = Topology_index.View.view topology_index in
        for iteration = 0 to iterations - 1 do
          Cancel.check_opt cancel;
          let step = match mode with
            | Laplacian step -> step
            | Custom_steps { odd; even } ->
                if iteration land 1 = 0 then odd else even in
          let range_count = (point_count + grain - 1) / grain in
          Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(range_count - 1)
            (fun range ->
              let first_point = range * grain
              and last_point = min point_count ((range + 1) * grain) in
              let sum = ref 0. and total = ref 0. in
              for point = first_point to last_point - 1 do
                if point land 4095 = 0 then Cancel.check_opt cancel;
                let update = (match selection with
                      | None -> true | Some group -> Group.mem point group)
                    in
                for plane = 0 to Array.length !current - 1 do
                  let source = (!current).(plane)
                  and destination = (!next).(plane) in
                  if not update then destination.(point) <- source.(point)
                  else begin
                  let first = topology.point_edge_offsets.(point)
                  and last = topology.point_edge_offsets.(point + 1) in
                  sum := 0.; total := 0.;
                  for local = first to last - 1 do
                    let edge = topology.point_edges.(local) in
                    let a = topology.edge_a.(edge) and b = topology.edge_b.(edge) in
                    let neighbor = if a = point then b else a in
                    if neighbor <> point then begin
                      sum := !sum +. source.(neighbor);
                      total := !total +. 1.
                    end
                  done;
                  if !total = 0. then destination.(point) <- source.(point)
                  else
                    let average = !sum /. !total in
                    destination.(point) <- source.(point)
                      +. (step *. (average -. source.(point)))
                  end
                done
              done);
          let swap = !current in current := !next; next := swap
        done;
        let output = !current in
        let invalid = ref None in
        Array.iteri (fun plane values ->
          let point = ref 0 in
          while !point < point_count && !invalid = None do
            if not (Float.is_finite values.(!point)) then
              invalid := Some (plane, !point);
            incr point
          done) output;
        match !invalid with
        | Some (plane, point) -> Error (Printf.sprintf
            "Rdk.Attribute_ops.blur_points: output plane %d is non-finite at point %d"
            plane point)
        | None ->
            let positions_changed = ref false in
            Array.iter (function
              | Blur_position first ->
                  let point = ref 0 in
                  while !point < point_count && not !positions_changed do
                    if output.(first).(!point) <> positions.x.(!point)
                        || output.(first + 1).(!point) <> positions.y.(!point)
                        || output.(first + 2).(!point) <> positions.z.(!point)
                    then positions_changed := true;
                    incr point
                  done
              | Blur_float _ | Blur_float2 _ | Blur_float3 _ | Blur_float4 _ -> ())
              targets;
            let result = if !positions_changed then
                Geometry.with_positions (Packed.Float3.View.of_owned_exn
                  ~x:output.(0) ~y:output.(1) ~z:output.(2)) geometry
              else Ok geometry in
            Result.bind result (fun geometry ->
              let geometry = if !positions_changed then geometry
                  |> Geometry.without_attribute ~owner:Attribute.Point "N"
                  |> Geometry.without_attribute ~owner:Attribute.Vertex "N"
                else geometry in
              let rec install geometry target =
                if target = Array.length targets then Ok geometry
                else match targets.(target) with
                  | Blur_position _ -> install geometry (target + 1)
                  | Blur_float (source, first) ->
                      Result.bind (Attribute.create_owned ~name:(Attribute.name source)
                          ~owner:Attribute.Point (Attribute.Float output.(first)))
                        (fun attribute -> Result.bind
                          (Geometry.with_attribute attribute geometry)
                          (fun geometry -> install geometry (target + 1)))
                  | Blur_float2 (source, first) ->
                      Result.bind (Packed.Float2.of_owned ~x:output.(first)
                          ~y:output.(first + 1)) (fun values ->
                      Result.bind (Attribute.create_owned ~name:(Attribute.name source)
                          ~owner:Attribute.Point (Attribute.Float2 values))
                        (fun attribute -> Result.bind
                          (Geometry.with_attribute attribute geometry)
                          (fun geometry -> install geometry (target + 1))))
                  | Blur_float3 (source, first) ->
                      let values = Packed.Float3.View.of_owned_exn
                          ~x:output.(first) ~y:output.(first + 1)
                          ~z:output.(first + 2) in
                      Result.bind (Attribute.create_owned ~name:(Attribute.name source)
                          ~owner:Attribute.Point (Attribute.Float3 values))
                        (fun attribute -> Result.bind
                          (Geometry.with_attribute attribute geometry)
                          (fun geometry -> install geometry (target + 1)))
                  | Blur_float4 (source, first) ->
                      Result.bind (Packed.Float4.of_owned ~x:output.(first)
                          ~y:output.(first + 1) ~z:output.(first + 2)
                          ~w:output.(first + 3)) (fun values ->
                      Result.bind (Attribute.create_owned ~name:(Attribute.name source)
                          ~owner:Attribute.Point (Attribute.Float4 values))
                        (fun attribute -> Result.bind
                          (Geometry.with_attribute attribute geometry)
                          (fun geometry -> install geometry (target + 1)))) in
              install geometry 0)
        )
      end))
