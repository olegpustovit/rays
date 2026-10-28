include Subdivide_refine
include Subdivide_stitch

let extract_primitive_part ?cancel ?grain ~selected primitives geometry =
  let geometry = Error.get (Deletion.delete ?cancel ?grain ~selected:(not selected)
      ~compact_points:true primitives geometry) in
  if selected then split_disconnected_point_fans ?cancel ?grain geometry
  else geometry

let free_point_part ?cancel ?grain geometry =
  let topology = Topology.View.view (Geometry.topology geometry) in
  let used = Bytes.make topology.point_count '\000' in
  Array.iteri (fun vertex point ->
    Cancel.poll cancel vertex 16_383;
    Bytes.set used point '\001') topology.vertex_points;
  let free_count = ref 0 in
  for point = 0 to topology.point_count - 1 do
    if Bytes.get used point = '\000' then incr free_count
  done;
  if !free_count = 0 then None
  else begin
    let referenced = Group.init ?grain ~owner:Group.Point
        ~name:"__rdk_subdivide_referenced_points" topology.point_count
        (fun point -> Bytes.get used point <> '\000') in
    Some (Error.get (Deletion.delete ?cancel ?grain ~selected:true referenced geometry))
  end

let iterate ?cancel ?grain ?edge_ancestry_attribute ?initial_edge_crease_override
    ?(generate_resulting_creases = true) ?resulting_crease_group_name
    ?(remove_holes = true)
    ?(boundary_interpolation = Subdivide_boundary_edge_only)
    ?(face_varying_interpolation = Subdivide_fvar_all)
    ?(triangle_subdivision = Subdivide_triangles_catmull_clark)
    ?(creasing_method = Subdivide_creasing_uniform)
    scheme iterations geometry =
  let rec loop remaining edge_crease_override current =
    Cancel.check_opt cancel;
    if remaining = 0 then current
    else
      let final = remaining = 1 in
      let next = once ?cancel ?grain ?edge_ancestry_attribute
          ?edge_crease_override
          ~generate_resulting_creases:(not final || generate_resulting_creases)
          ?resulting_crease_group_name:(if final
            then resulting_crease_group_name else None)
          ~remove_holes:(final && remove_holes)
          ~boundary_interpolation
          ~face_varying_interpolation
          ~triangle_subdivision
          ~creasing_method
          scheme current in
      loop (remaining - 1) None next in
  loop iterations initial_edge_crease_override geometry

let with_empty_edge_group ?cancel name geometry =
  let topology = Geometry.topology geometry in
  let index = Topology_index.create ?cancel topology in
  let group = Edge_group.init ~topology ~index ~name (fun _ -> false) in
  Geometry.with_edge_group group geometry |> get_ok

let without_resulting_creases geometry =
  geometry
  |> Geometry.without_attribute ~owner:Attribute.Vertex "creaseweight"
  |> Geometry.without_attribute ~owner:Attribute.Primitive "creaseweight"
  |> Geometry.without_attribute ~owner:Attribute.Point "cornerweight"

let automatic_boundary_holes ?cancel ?grain geometry =
  let topology_value = Geometry.topology geometry in
  let topology = Topology.View.view topology_value in
  let index_value = Topology_index.create ?cancel topology_value in
  let index = Topology_index.View.view index_value in
  let point_byte_count = (topology.point_count lsr 3)
      + if topology.point_count land 7 = 0 then 0 else 1 in
  let boundary_points = Bytes.make point_byte_count '\000' in
  run ?grain ?cancel point_byte_count (fun byte ->
    let base = byte lsl 3 and value = ref 0 in
    for bit = 0 to min 7 (topology.point_count - base - 1) do
      let point = base + bit in
      let at = ref index.point_edge_offsets.(point)
      and last = index.point_edge_offsets.(point + 1)
      and boundary = ref false in
      while !at < last && not !boundary do
        let edge = index.point_edges.(!at) in
        if index.edge_offsets.(edge + 1) - index.edge_offsets.(edge) = 1 then
          boundary := true;
        incr at
      done;
      if !boundary then value := !value lor (1 lsl bit)
    done;
    Bytes.set boundary_points byte (Char.chr !value));
  let primitive_count = Bytes.length topology.primitive_kinds in
  let byte_count = (primitive_count lsr 3)
      + if primitive_count land 7 = 0 then 0 else 1 in
  let hole_bits = Bytes.make byte_count '\000' in
  run ?grain ?cancel byte_count (fun byte ->
    let base = byte lsl 3 and value = ref 0 in
    for bit = 0 to min 7 (primitive_count - base - 1) do
      let primitive = base + bit in
      let first = topology.primitive_offsets.(primitive)
      and last = topology.primitive_offsets.(primitive + 1) in
      let vertex = ref first and boundary = ref false in
      while !vertex < last && not !boundary do
        Cancel.poll cancel !vertex 4095;
        let point = topology.vertex_points.(!vertex) in
        if Char.code (Bytes.get boundary_points (point lsr 3))
            land (1 lsl (point land 7)) <> 0 then boundary := true;
        incr vertex
      done;
      if !boundary then value := !value lor (1 lsl bit)
    done;
    Bytes.set hole_bits byte (Char.chr !value));
  let holes = Group.View.of_owned_bits ~owner:Group.Primitive
      ~name:"subdivision_hole" ~length:primitive_count hole_bits in
  if Group.cardinality holes = 0 then None else Some holes

type detail_overrides = {
  osd_scheme : Attribute.t option;
  osd_vtxboundaryinterpolation : Attribute.t option;
  osd_fvarlinearinterpolation : Attribute.t option;
  osd_creasingmethod : Attribute.t option;
  osd_trianglesubdiv : Attribute.t option;
}

let detail_overrides geometry =
  let osd_scheme = ref None
  and osd_vtxboundaryinterpolation = ref None
  and osd_fvarlinearinterpolation = ref None
  and osd_creasingmethod = ref None
  and osd_trianglesubdiv = ref None in
  Array.iter (fun attribute ->
    if Attribute.owner attribute = Attribute.Detail then
      match Attribute.name attribute with
      | "osd_scheme" -> osd_scheme := Some attribute
      | "osd_vtxboundaryinterpolation" ->
          osd_vtxboundaryinterpolation := Some attribute
      | "osd_fvarlinearinterpolation" ->
          osd_fvarlinearinterpolation := Some attribute
      | "osd_creasingmethod" -> osd_creasingmethod := Some attribute
      | "osd_trianglesubdiv" -> osd_trianglesubdiv := Some attribute
      | _ -> ()) (Geometry.View.attributes geometry);
  { osd_scheme = !osd_scheme;
    osd_vtxboundaryinterpolation = !osd_vtxboundaryinterpolation;
    osd_fvarlinearinterpolation = !osd_fvarlinearinterpolation;
    osd_creasingmethod = !osd_creasingmethod;
    osd_trianglesubdiv = !osd_trianglesubdiv }

let detail_int name attribute =
  match Attribute.View.storage attribute with
  | Attribute.Int [|value|] -> value
  | Attribute.Int _ ->
      fail (Printf.sprintf "detail attribute %s must contain exactly one value" name)
  | _ -> fail (Printf.sprintf
      "detail attribute %s must use integer storage, not %s"
      name (Attribute.kind_name attribute))

let resolve_scheme default = function
  | None -> default
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int [|0|] -> Catmull_clark
       | Attribute.Int [|1|] -> Loop
       | Attribute.Int [|2|] -> Bilinear
       | Attribute.Int [|value|] -> fail (Printf.sprintf
           "detail attribute osd_scheme value %d is outside [0, 2]" value)
       | Attribute.Int _ | Attribute.Text _ when Attribute.length attribute <> 1 ->
           fail "detail attribute osd_scheme must contain exactly one value"
       | Attribute.Text [|"catmull-clark"|] -> Catmull_clark
       | Attribute.Text [|"loop"|] -> Loop
       | Attribute.Text [|"bilinear"|] -> Bilinear
       | Attribute.Text [|value|] -> fail (Printf.sprintf
           "detail attribute osd_scheme has unsupported value %S; expected \"catmull-clark\", \"loop\", or \"bilinear\""
           value)
       | _ -> fail (Printf.sprintf
           "detail attribute osd_scheme must use integer or text storage, not %s"
           (Attribute.kind_name attribute)))

let resolve_boundary_interpolation default = function
  | None -> default
  | Some attribute ->
      match detail_int "osd_vtxboundaryinterpolation" attribute with
      | 0 -> Subdivide_boundary_none
      | 1 -> Subdivide_boundary_edge_only
      | 2 -> Subdivide_boundary_edge_and_corner
      | value -> fail (Printf.sprintf
          "detail attribute osd_vtxboundaryinterpolation value %d is outside [0, 2]"
          value)

let resolve_face_varying_interpolation default = function
  | None -> default
  | Some attribute ->
      match detail_int "osd_fvarlinearinterpolation" attribute with
      | 0 -> Subdivide_fvar_none
      | 1 -> Subdivide_fvar_corners_only
      | 2 -> Subdivide_fvar_corners_plus1
      | 3 -> Subdivide_fvar_corners_plus2
      | 4 -> Subdivide_fvar_boundaries
      | 5 -> Subdivide_fvar_all
      | value -> fail (Printf.sprintf
          "detail attribute osd_fvarlinearinterpolation value %d is outside [0, 5]"
          value)

let resolve_creasing_method default = function
  | None -> default
  | Some attribute ->
      match detail_int "osd_creasingmethod" attribute with
      | 0 -> Subdivide_creasing_uniform
      | 1 -> Subdivide_creasing_chaikin
      | value -> fail (Printf.sprintf
          "detail attribute osd_creasingmethod value %d is outside [0, 1]" value)

let resolve_triangle_subdivision default = function
  | None -> default
  | Some attribute ->
      match detail_int "osd_trianglesubdiv" attribute with
      | 0 -> Subdivide_triangles_catmull_clark
      | 1 -> Subdivide_triangles_smooth
      | value -> fail (Printf.sprintf
          "detail attribute osd_trianglesubdiv value %d is outside [0, 1]" value)

let subdivide_polygons ?cancel ?grain ?(scheme = Catmull_clark) ?(iterations = 1)
    ?primitives ?(cracks = Subdivide_do_not_close)
    ?(consistent_topology = false) ?creases ?crease_primitives ?crease_weight
    ?(generate_resulting_creases = true) ?resulting_crease_group
    ?hole_primitives ?(remove_holes = true)
    ?(boundary_interpolation = Subdivide_boundary_edge_only)
    ?(face_varying_interpolation = Subdivide_fvar_all)
    ?(triangle_subdivision = Subdivide_triangles_catmull_clark)
    ?(creasing_method = Subdivide_creasing_uniform) geometry =
  if iterations < 0 then invalid_arg "Rdk_mesh.Subdivide.subdivide: iterations must be non-negative";
  (match cracks with
   | (Subdivide_pull_divide_edges bias | Subdivide_pull_triangulate bias)
       when not (Float.is_finite bias) || bias < 0. || bias > 1. ->
       invalid_arg "Rdk_mesh.Subdivide.subdivide: Pull Divide bias must be finite and in [0, 1]"
   | _ -> ());
  try
    Cancel.check_opt cancel;
    let overrides = detail_overrides geometry in
    let scheme = resolve_scheme scheme overrides.osd_scheme
    and boundary_interpolation = resolve_boundary_interpolation
        boundary_interpolation overrides.osd_vtxboundaryinterpolation
    and face_varying_interpolation = resolve_face_varying_interpolation
        face_varying_interpolation overrides.osd_fvarlinearinterpolation
    and triangle_subdivision = resolve_triangle_subdivision
        triangle_subdivision overrides.osd_trianglesubdiv
    and creasing_method = resolve_creasing_method
        creasing_method overrides.osd_creasingmethod in
    (match creases, crease_primitives with
     | None, Some _ -> fail "crease primitive selection requires a crease input"
     | _ -> ());
    Option.iter (fun value ->
      if not (Float.is_finite value) || value < 0. then
        fail "crease override must be finite and non-negative") crease_weight;
    Option.iter (fun name ->
      if String.trim name = "" then fail "resulting crease group name must not be empty";
      if not generate_resulting_creases then
        fail "resulting crease group requires generated resulting creases")
      resulting_crease_group;
    let prepared = match creases with
      | None -> geometry
      | Some creases -> apply_crease_input ?cancel ?grain ?selection:crease_primitives
          ?override:crease_weight geometry creases in
    let initial_edge_crease_override = match creases with
      | None -> crease_weight | Some _ -> None in
    let prepared = match hole_primitives with
      | None -> prepared
      | Some holes ->
          validate_hole_group holes prepared;
          Geometry.with_group (Group.with_name "subdivision_hole" holes) prepared
          |> get_ok in
    let prepared = match boundary_interpolation, scheme with
      | Subdivide_boundary_none, (Catmull_clark | Loop) ->
          (match automatic_boundary_holes ?cancel ?grain prepared with
           | None -> prepared
           | Some boundary_holes ->
               let holes = match Geometry.find_group ~owner:Group.Primitive
                   "subdivision_hole" prepared with
                 | None -> boundary_holes
                 | Some existing -> Group.union existing boundary_holes |> get_ok in
               Geometry.with_group holes prepared |> get_ok)
      | (Subdivide_boundary_edge_only | Subdivide_boundary_edge_and_corner), _
      | Subdivide_boundary_none, Bilinear -> prepared in
    let prepared = match resulting_crease_group with
      | Some name when iterations > 0 -> with_empty_edge_group ?cancel name prepared
      | None | Some _ -> prepared in
    let primitives = match primitives,
        (if remove_holes then Geometry.find_group ~owner:Group.Primitive
          "subdivision_hole" prepared else None) with
      | Some selection, Some holes -> Some (Group.union selection holes |> get_ok)
      | Some selection, None -> Some selection
      | None, _ -> None in
    match primitives with
    | None ->
        if iterations = 0 then Ok geometry
        else Ok (iterate ?cancel ?grain ?initial_edge_crease_override
          ~generate_resulting_creases
          ?resulting_crease_group_name:resulting_crease_group
          ~remove_holes
          ~boundary_interpolation
          ~face_varying_interpolation
          ~triangle_subdivision
          ~creasing_method
          scheme iterations prepared)
    | Some selection ->
        validate_primitive_selection selection prepared;
        if iterations = 0 || Group.cardinality selection = 0 then Ok geometry
        else if Group.cardinality selection = Group.length selection then
          Ok (iterate ?cancel ?grain ?initial_edge_crease_override
            ~generate_resulting_creases
            ?resulting_crease_group_name:resulting_crease_group
            ~remove_holes
            ~boundary_interpolation
            ~face_varying_interpolation
            ~triangle_subdivision
            ~creasing_method
            scheme iterations prepared)
        else begin
          let selected_source, pull_plan = match cracks with
            | Subdivide_do_not_close -> prepared, None
            | Subdivide_pull_no_edge_division
            | Subdivide_pull_divide_edges _
            | Subdivide_pull_triangulate _
            | Subdivide_stitch_no_edge_division
            | Subdivide_stitch_divide_edges
            | Subdivide_stitch_triangulate ->
                annotate_pull_interfaces ?cancel selection prepared in
          let selected = extract_primitive_part ?cancel ?grain ~selected:true
                  selection selected_source in
          let unselected = match cracks with
            | Subdivide_pull_divide_edges _ | Subdivide_pull_triangulate _
            | Subdivide_stitch_divide_edges | Subdivide_stitch_triangulate -> None
            | Subdivide_do_not_close | Subdivide_pull_no_edge_division
            | Subdivide_stitch_no_edge_division ->
                Some (extract_primitive_part ?cancel ?grain ~selected:false
                  selection prepared) in
          let refined_with_ancestry = iterate ?cancel ?grain
              ?initial_edge_crease_override
              ?edge_ancestry_attribute:(Option.map
                (fun plan -> plan.ancestry_attribute) pull_plan)
              ~generate_resulting_creases
              ?resulting_crease_group_name:resulting_crease_group
              ~remove_holes
              ~boundary_interpolation
              ~face_varying_interpolation
              ~triangle_subdivision
              ~creasing_method
              scheme iterations selected in
          let chains = match cracks, pull_plan with
            | (Subdivide_pull_divide_edges _
              | Subdivide_pull_triangulate _
              | Subdivide_stitch_divide_edges
              | Subdivide_stitch_triangulate), Some plan ->
                Some (interface_chains ?cancel plan refined_with_ancestry)
            | _ -> None in
          let divided = match pull_plan, chains with
            | Some plan, Some chains ->
                let divided = divide_unselected_edges ?cancel ?grain plan chains
                    prepared in
                Some (match cracks with
                  | Subdivide_pull_triangulate bias ->
                      triangulate_divided_boundary ?cancel ?grain ~pull_bias:bias
                        ~consistent_topology plan chains divided
                        ~refined:refined_with_ancestry
                  | Subdivide_stitch_triangulate ->
                      triangulate_divided_boundary ?cancel ?grain
                        ~consistent_topology plan chains divided
                        ~refined:refined_with_ancestry
                  | _ -> divided)
            | _ -> None in
          let refined = match cracks, pull_plan with
            | Subdivide_pull_no_edge_division, Some plan ->
                pull_boundary_no_edge_division ?cancel ?grain plan
                  refined_with_ancestry
            | Subdivide_stitch_no_edge_division, Some plan ->
                Geometry.without_attribute ~owner:Attribute.Vertex
                  plan.ancestry_attribute refined_with_ancestry
            | (Subdivide_pull_divide_edges _ | Subdivide_pull_triangulate _
              | Subdivide_stitch_divide_edges | Subdivide_stitch_triangulate),
                Some plan ->
                Geometry.without_attribute ~owner:Attribute.Vertex
                  plan.ancestry_attribute refined_with_ancestry
            | Subdivide_do_not_close, Some plan ->
                Geometry.without_attribute ~owner:Attribute.Vertex
                  plan.ancestry_attribute refined_with_ancestry
            | (Subdivide_do_not_close | Subdivide_pull_no_edge_division
              | Subdivide_pull_divide_edges _
              | Subdivide_pull_triangulate _
              | Subdivide_stitch_no_edge_division
              | Subdivide_stitch_divide_edges
              | Subdivide_stitch_triangulate), None -> refined_with_ancestry in
          let free = free_point_part ?cancel ?grain prepared in
          let free_points = match free with None -> 0
            | Some geometry -> Geometry.point_count geometry in
          let free_vertices = match free with None -> 0
            | Some geometry -> Geometry.vertex_count geometry in
          let coarse = match divided with
            | Some divided -> divided.geometry
            | None -> Option.get unselected in
          let parts = match free with
            | None -> [coarse; refined]
            | Some free -> [coarse; free; refined] in
          let append = match cracks, pull_plan, chains, divided with
            | Subdivide_stitch_no_edge_division, Some plan, _, _ ->
                Some (stitch_boundary_no_edge_division ?cancel plan
                  ~unselected:(Option.get unselected) ~free_points
                  ~refined:refined_with_ancestry)
            | (Subdivide_stitch_divide_edges | Subdivide_stitch_triangulate),
                Some plan, Some chains,
                Some divided ->
                Some (stitch_boundary_divide_edges ?cancel ~consistent_topology
                  plan chains divided ~free_points ~free_vertices refined)
            | (Subdivide_do_not_close | Subdivide_pull_no_edge_division
              | Subdivide_pull_divide_edges _
              | Subdivide_pull_triangulate _
              | Subdivide_stitch_no_edge_division
              | Subdivide_stitch_divide_edges
              | Subdivide_stitch_triangulate), _, _, _ -> None in
          let total_points = checked_total "combined point count"
              (List.map Geometry.point_count parts) in
          let weld = match cracks, chains, divided with
            | (Subdivide_pull_divide_edges bias
              | Subdivide_pull_triangulate bias), Some chains, Some divided ->
                Some (make_boundary_weld ?cancel ~bias ~only_equal:false
                  chains divided ~free_points ~total_points refined)
            | (Subdivide_stitch_divide_edges | Subdivide_stitch_triangulate),
                Some chains, Some divided ->
                if consistent_topology then None
                else Some (make_boundary_weld ?cancel ~bias:0. ~only_equal:true
                  chains divided ~free_points ~total_points refined)
            | _ -> None in
          let output = combine_parts ?cancel ?grain ?append ?weld
              ~source:prepared parts in
          Ok (if generate_resulting_creases then output
            else without_resulting_creases output)
        end
  with
  | Error.Fail message -> Error message

let primitive_kind_counts geometry =
  let topology = Topology.View.view (Geometry.topology geometry) in
  let polygons = ref 0 and curves = ref 0 in
  Bytes.iter (fun kind ->
    if kind = '\000' then incr polygons else incr curves) topology.primitive_kinds;
  !polygons, !curves

let internal_attribute_name geometry base =
  let rec choose suffix =
    let name = if suffix = 0 then base else base ^ "_" ^ string_of_int suffix in
    if Geometry.find_attribute ~owner:Attribute.Primitive name geometry = None
    then name else choose (suffix + 1) in
  choose 0

let primitive_kind_group ?grain ~polygon geometry =
  let topology = Topology.View.view (Geometry.topology geometry) in
  Group.init ?grain ~owner:Group.Primitive
    ~name:"__rdk_subdivide_primitive_kind"
    (Bytes.length topology.primitive_kinds) (fun primitive ->
      (Bytes.get topology.primitive_kinds primitive = '\000') = polygon)

let extract_kind ?cancel ?grain ~polygon geometry =
  let selection = primitive_kind_group ?grain ~polygon geometry in
  Error.get (Deletion.delete ?cancel ?grain ~selected:false ~compact_points:true
    selection geometry)

let selection_for_kind ?grain ~polygon selection geometry =
  match selection with
  | None -> None
  | Some selection ->
      let topology = Topology.View.view (Geometry.topology geometry) in
      let count = ref 0 in
      Bytes.iter (fun kind ->
        if (kind = '\000') = polygon then incr count) topology.primitive_kinds;
      let source_of_output = Array.make !count 0 and output = ref 0 in
      for primitive = 0 to Bytes.length topology.primitive_kinds - 1 do
        if (Bytes.get topology.primitive_kinds primitive = '\000') = polygon then begin
          source_of_output.(!output) <- primitive;
          incr output
        end
      done;
      Some (Group.init ?grain ~owner:Group.Primitive
        ~name:"__rdk_subdivide_kind_selection" !count (fun output ->
          Group.mem source_of_output.(output) selection))

let refine_curves ?cancel ?grain ?selection ~independent scheme iterations geometry =
  let refine geometry =
    let scheme = match scheme with
      | Catmull_clark -> Curve_subdivide.Catmull_clark
      | Bilinear -> Curve_subdivide.Bilinear
      | Loop -> fail "Loop subdivision requires triangle polygons and cannot refine polygon curves" in
    Curve_subdivide.iterate ?cancel ?grain ~independent scheme iterations geometry in
  match selection with
  | None -> refine geometry
  | Some selection ->
      validate_primitive_selection selection geometry;
      let selected = Group.cardinality selection in
      if selected = 0 then geometry
      else if selected = Group.length selection then refine geometry
      else begin
        let refined_source = Error.get (Deletion.delete ?cancel ?grain ~selected:false
            ~compact_points:true selection geometry) in
        let retained = Error.get (Deletion.delete ?cancel ?grain ~selected:true
            ~compact_points:true selection geometry) in
        combine_parts ?cancel ?grain ~source:geometry [retained; refine refined_source]
      end

type params = {
  cancel : Rdk_core.Cancel.t option;
  grain : int option;
  scheme : scheme;
  iterations : int;
  primitives : Rdk_core.Group.t option;
  cracks : crack_policy;
  consistent_topology : bool;
  creases : Rdk_core.Geometry.t option;
  crease_primitives : Rdk_core.Group.t option;
  crease_weight : float option;
  generate_resulting_creases : bool;
  resulting_crease_group : String.t option;
  hole_primitives : Rdk_core.Group.t option;
  remove_holes : bool;
  boundary_interpolation : boundary_interpolation;
  face_varying_interpolation : face_varying_interpolation;
  triangle_policy : triangle_policy;
  creasing_method : creasing_method;
  treat_curves_as_independent : bool;
  recompute_point_normals : bool;
}

let subdivide ({ cancel; grain; scheme; iterations; primitives; cracks; consistent_topology; creases;
    crease_primitives; crease_weight; generate_resulting_creases; resulting_crease_group;
    hole_primitives; remove_holes; boundary_interpolation; face_varying_interpolation;
    triangle_policy = triangle_subdivision; creasing_method; treat_curves_as_independent;
    recompute_point_normals } : params) geometry =
  Error.guard ~operation:"subdivide" ~code:"invalid_topology" @@ fun () ->
  let had_point_normals =
    Geometry.find_attribute ~owner:Attribute.Point "N" geometry <> None in
  let polygon_count, curve_count = primitive_kind_counts geometry in
  let result = if curve_count = 0 then
    subdivide_polygons ?cancel ?grain ~scheme ~iterations ?primitives ~cracks
      ~consistent_topology ?creases ?crease_primitives ?crease_weight
      ~generate_resulting_creases ?resulting_crease_group ?hole_primitives
      ~remove_holes ~boundary_interpolation ~face_varying_interpolation
      ~triangle_subdivision ~creasing_method geometry
  else
    try
      if iterations < 0 then
        invalid_arg "Rdk_mesh.Subdivide.subdivide: iterations must be non-negative";
      (match cracks with
       | (Subdivide_pull_divide_edges bias | Subdivide_pull_triangulate bias)
           when not (Float.is_finite bias) || bias < 0. || bias > 1. ->
           invalid_arg
             "Rdk_mesh.Subdivide.subdivide: Pull Divide bias must be finite and in [0, 1]"
       | _ -> ());
      Cancel.check_opt cancel;
      Option.iter (fun selection -> validate_primitive_selection selection geometry)
        primitives;
      (match creases, crease_primitives with
       | None, Some _ -> fail "crease primitive selection requires a crease input"
       | _ -> ());
      Option.iter (fun value ->
        if not (Float.is_finite value) || value < 0. then
          fail "crease override must be finite and non-negative") crease_weight;
      Option.iter (fun name ->
        if String.trim name = "" then
          fail "resulting crease group name must not be empty";
        if not generate_resulting_creases then
          fail "resulting crease group requires generated resulting creases")
        resulting_crease_group;
      let overrides = detail_overrides geometry in
      let scheme = resolve_scheme scheme overrides.osd_scheme
      and boundary_interpolation = resolve_boundary_interpolation
          boundary_interpolation overrides.osd_vtxboundaryinterpolation
      and face_varying_interpolation = resolve_face_varying_interpolation
          face_varying_interpolation overrides.osd_fvarlinearinterpolation
      and triangle_subdivision = resolve_triangle_subdivision
          triangle_subdivision overrides.osd_trianglesubdiv
      and creasing_method = resolve_creasing_method
          creasing_method overrides.osd_creasingmethod in
      let selected_curve_count = match primitives with
        | None -> curve_count
        | Some selection ->
            let topology = Topology.View.view (Geometry.topology geometry) in
            let count = ref 0 in
            for primitive = 0 to Bytes.length topology.primitive_kinds - 1 do
              if Bytes.get topology.primitive_kinds primitive <> '\000'
                 && Group.mem primitive selection then incr count
            done;
            !count in
      if scheme = Loop && selected_curve_count > 0 && iterations > 0 then
        fail "Loop subdivision requires triangle polygons and cannot refine polygon curves";
      if iterations = 0
         || (match primitives with Some selection -> Group.cardinality selection = 0
             | None -> false) then Ok geometry
      else begin
        let prepared = match creases with
          | None -> geometry
          | Some creases -> apply_crease_input ?cancel ?grain
              ?selection:crease_primitives ?override:crease_weight geometry creases in
        let prepared = match hole_primitives with
          | None -> prepared
          | Some holes ->
              validate_hole_group holes prepared;
              Geometry.with_group (Group.with_name "subdivision_hole" holes)
                prepared |> get_ok in
        let prepared = match resulting_crease_group with
          | Some name -> with_empty_edge_group ?cancel name prepared
          | None -> prepared in
        let all_curves_selected = polygon_count = 0
            && match primitives with
              | None -> true
              | Some selection -> Group.cardinality selection = Group.length selection in
        if all_curves_selected then begin
          let refine geometry = refine_curves ?cancel ?grain
              ~independent:treat_curves_as_independent scheme iterations geometry in
          let output = if not treat_curves_as_independent then refine prepared
            else match free_point_part ?cancel ?grain prepared with
              | None -> refine prepared
              | Some free ->
                  let curves = extract_kind ?cancel ?grain ~polygon:false prepared in
                  combine_parts ?cancel ?grain ~source:prepared [refine curves; free] in
          Ok (if generate_resulting_creases then output
            else without_resulting_creases output)
        end else begin
        let source_order_name = internal_attribute_name prepared
            "__rdk_subdivide_source_primitive" in
        let source_order = Attribute.create_owned ~name:source_order_name
            ~owner:Attribute.Primitive
            (Attribute.Int (Array.init (Geometry.primitive_count prepared) Fun.id))
            |> get_ok in
        let prepared = Geometry.with_attribute source_order prepared |> get_ok in
        let parts = ref [] in
        if polygon_count > 0 then begin
          let surface = extract_kind ?cancel ?grain ~polygon:true prepared in
          let surface_selection = selection_for_kind ?grain ~polygon:true
              primitives prepared in
          let surface_crease_weight = match creases with
            | None -> crease_weight | Some _ -> None in
          let refined = subdivide_polygons ?cancel ?grain ~scheme ~iterations
              ?primitives:surface_selection ~cracks ~consistent_topology
              ?crease_weight:surface_crease_weight ~generate_resulting_creases
              ?resulting_crease_group ~remove_holes ~boundary_interpolation
              ~face_varying_interpolation ~triangle_subdivision ~creasing_method
              surface |> get_ok in
          parts := refined :: !parts
        end;
        let curves = extract_kind ?cancel ?grain ~polygon:false prepared in
        let curve_selection = selection_for_kind ?grain ~polygon:false
            primitives prepared in
        let curves = refine_curves ?cancel ?grain ?selection:curve_selection
            ~independent:treat_curves_as_independent scheme iterations curves in
        parts := curves :: !parts;
        (match free_point_part ?cancel ?grain prepared with
         | None -> () | Some free -> parts := free :: !parts);
        let combined = combine_parts ?cancel ?grain ~source:prepared
            (List.rev !parts) in
        let ordered = Error.get (Ordering.sort_primitives ?cancel ?grain
            ~name:source_order_name combined) in
        let output = Geometry.without_attribute ~owner:Attribute.Primitive
            source_order_name ordered in
        Ok (if generate_resulting_creases then output
          else without_resulting_creases output)
        end
      end
    with
    | Error.Fail message -> Error message
    | Invalid_argument message -> Error message in
  if not recompute_point_normals || not had_point_normals then result
  else Result.bind result (fun output ->
    Deform.normals ?cancel ~grain:(Option.value ~default:16_384 grain) output)
