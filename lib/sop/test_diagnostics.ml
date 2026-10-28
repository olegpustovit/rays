(* SOP-level diagnostics and Lisp-visible wiring, one table of Lisp texts.

   The per-node operations are tested in lib/rdk/test_<name>.ml. What only the
   SOP layer (lib/sop/*.ml) decides is checked here: the diagnostic
   code of a missing group or attribute, and that the Lisp keywords reach the
   operation (cardinality of the cooked geometry). Every row is cooked at 1
   and 4 domains and the two results must be equal. *)

open Sop
open Rdk_test_support

type expect =
  | Counts of int * int * int (* points, vertices, primitives *)
  | Code of string (* the cook fails with this diagnostic code *)
  | Rejected (* the Lisp text does not lower to a node *)

let grid = "(sop/grid :columns 4 :rows 4 :connectivity \"Quads\")"
let tri_grid = "(sop/grid :columns 4 :rows 4 :connectivity \"Triangles\")"
let box = "(sop/box)"
let curve = "(sop/curve (list [0.0 0.0 0.0] [1.0 0.0 0.0] [0.0 1.0 0.0]) :closed true)"

let sphere =
  "(sop/uv_sphere :connectivity \"Alternating triangles\" :segments 48 :rings 24 :base_radius 2)"

let rows =
  [
    (* missing groups, raised by the SOP node wrapper *)
    ( "attribute_fade group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/attribute_fade :group \"missing\"))" grid );
    ( "attribute_fade start reference",
      Code "invalid_attribute_fade",
      Printf.sprintf
        "(sop/attribute_fade :start_attribute \"start\" %s (sop/line :kind \"Points\" :points 1))"
        grid );
    ( "attribute_mirror mapping group",
      Code "missing_group",
      Printf.sprintf
        "(-> %s (sop/attribute_mirror :method_ \"Mapping attribute\" :mapping_attribute \"map\" \
         :mapping_destination_group \"missing\"))"
        grid );
    ( "blast_by_attribute group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/blast_by_attribute :group \"missing\"))" grid );
    ( "blast_by_attribute attribute",
      Code "invalid_blast",
      Printf.sprintf "(-> %s (sop/blast_by_attribute :attribute \"missing\" :threshold 0.))" grid );
    ( "boolean_detect source group",
      Code "missing_group",
      Printf.sprintf
        "(sop/boolean_detect :source_group \"missing\" :self_intersecting_group \"\" %s %s)" box box
    );
    ( "circle_from_edges group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/circle_from_edges :group \"missing\"))" curve );
    ( "crease group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/crease :group \"missing\"))" grid );
    ( "dissolve edge group",
      Code "missing_edge_group",
      Printf.sprintf "(-> %s (sop/dissolve :group \"missing\"))" grid );
    ( "distance_along_geometry start",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/distance_along_geometry :start_group \"missing\"))" grid );
    ( "distance_from_geometry reference",
      Code "missing_group",
      Printf.sprintf "(sop/distance_from_geometry :reference_group \"missing\" %s %s)" grid box );
    ( "distance_from_target group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/distance_from_target :affected_group \"missing\"))" grid );
    ( "duplicate group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/duplicate :group \"missing\"))" grid );
    ( "ends group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/ends :group \"missing\"))" curve );
    ( "extract_point_from_curve group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/extract_point_from_curve :group \"absent\"))" curve );
    ( "graph_color group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/graph_color :group_owner \"Point\" :group \"missing\"))" grid );
    ( "intersection_analysis source group",
      Code "missing_group",
      Printf.sprintf "(sop/intersection_analysis :source_group \"missing\" %s %s)" box box );
    ( "point_generate group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/point_generate :group \"missing\"))" grid );
    ( "point_replicate group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/point_replicate :group \"missing\"))" grid );
    ( "point_split group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/point_split :group_owner \"Vertex\" :group \"missing\"))" grid );
    ( "point_split attribute",
      Code "invalid_geometry",
      Printf.sprintf "(-> %s (sop/point_split :attributes \"missing\"))" grid );
    ( "poly_bevel edge group",
      Code "missing_edge_group",
      Printf.sprintf "(-> %s (sop/poly_bevel :group \"missing\" :distance 0.1))" grid );
    ( "poly_bridge source edge group",
      Code "missing_edge_group",
      Printf.sprintf
        "(-> %s (sop/poly_bridge :source_group \"missing\" :destination_group \"destination\"))"
        grid );
    ( "poly_cut primitive group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/poly_cut :group \"missing\"))" grid );
    ( "poly_cut edge cut group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/poly_cut :element \"Edges\" :cut_group \"missing\"))" grid );
    ( "poly_loft group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/poly_loft :group \"missing\"))" curve );
    ( "poly_reduce hard edge group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/poly_reduce :hard_edge_group \"missing\"))" tri_grid );
    ( "polywire group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/polywire :group \"absent\" :radius 0.1))" curve );
    ( "polywire segment seam attribute",
      Code "invalid_geometry",
      Printf.sprintf "(-> %s (sop/polywire :segment_seam_attribute \"edge_seam\" :radius 0.1))"
        curve );
    ( "remesh hard edge group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/remesh :target_length 0.2 :hard_edge_group \"missing\"))" tri_grid
    );
    ( "rewire_vertices selection",
      Code "missing_group",
      Printf.sprintf
        "(-> %s (sop/rewire_vertices :selection_owner \"Point\" :selection \"missing\" :owner \
         \"Point\" :delete_target_attribute false))"
        grid );
    ( "rewire_vertices target attribute",
      Code "invalid_rewire_vertices",
      Printf.sprintf
        "(-> %s (sop/rewire_vertices :owner \"Point\" :target_attribute \"missing\" \
         :delete_target_attribute false))"
        grid );
    ( "separate_pieces attribute",
      Code "invalid_separate_pieces",
      Printf.sprintf "(-> %s (sop/separate_pieces :piece_attribute \"missing\"))" grid );
    ( "skin group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/skin :group \"missing\"))" curve );
    ( "soft_transform group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/soft_transform :group \"missing\"))" grid );
    ( "sort index attribute",
      Code "invalid_sort",
      Printf.sprintf "(-> %s (sop/sort :key \"Index attribute\" :attribute \"missing\"))" grid );
    ( "transform group",
      Code "missing_group",
      Printf.sprintf
        "(-> %s (sop/transform :translate [1 0 0] :group_owner \"Point\" :group \"missing\"))" grid
    );
    ( "triangulate_2d point group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/triangulate_2d :point_group \"missing\"))" grid );
    (* attribute_laplacian and measure_curvature have no rdk twin *)
    ( "attribute_laplacian group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/attribute_laplacian :output \"\" :point_group \"missing\"))"
        sphere );
    ( "attribute_laplacian curve",
      Code "invalid_laplacian",
      Printf.sprintf "(-> %s (sop/attribute_laplacian :output \"\"))" curve );
    ( "measure_curvature group",
      Code "missing_group",
      Printf.sprintf "(-> %s (sop/measure_curvature :point_group \"missing\"))" sphere );
    ( "measure_curvature curve",
      Code "invalid_curvature",
      Printf.sprintf "(-> %s (sop/measure_curvature))" curve );
    ( "attribute_laplacian blank point group",
      Counts (1106, 6624, 2208),
      Printf.sprintf "(-> %s (sop/attribute_laplacian :output \"\" :point_group \" \"))" sphere );
    ( "attribute_laplacian blank source",
      Code "invalid_laplacian",
      Printf.sprintf "(-> %s (sop/attribute_laplacian :output \"\" :source \" \"))" sphere );
    ( "attribute_laplacian P as output",
      Rejected,
      Printf.sprintf "(-> %s (sop/attribute_laplacian :source \"P\" :output \"P\"))" sphere );
    ( "measure_curvature blank point group",
      Counts (1106, 6624, 2208),
      Printf.sprintf "(-> %s (sop/measure_curvature :point_group \" \"))" sphere );
    ( "measure_curvature nan smoothing",
      Counts (1106, 6624, 2208),
      Printf.sprintf "(-> %s (sop/measure_curvature :smoothing_strength (/ 0.0 0.0)))" sphere );
    ( "measure_curvature duplicate outputs",
      Rejected,
      Printf.sprintf
        "(-> %s (sop/measure_curvature :mean \"mean\" :gaussian \"mean\" :minimum \"minimum\" \
         :maximum \"maximum\" :curvedness \"curvedness\" :shape_index \"shape\"))"
        sphere );
    ( "circle_from_edges blank group",
      Counts (3, 3, 1),
      Printf.sprintf "(-> %s (sop/circle_from_edges :group \" \"))" curve );
    ( "circle_from_edges zero radius",
      Rejected,
      Printf.sprintf "(-> %s (sop/circle_from_edges :use_radius true :radius 0.))" curve );
    ( "circle_from_edges blank output group",
      Counts (3, 3, 1),
      Printf.sprintf "(-> %s (sop/circle_from_edges :output_group \" \"))" curve );
    ( "crease negative weight",
      Rejected,
      Printf.sprintf "(-> %s (sop/crease :operation \"Set\" :weight -1.))" grid );
    ( "graph_color P output",
      Rejected,
      Printf.sprintf "(-> %s (sop/graph_color :color_attribute \"P\"))" grid );
    ( "graph_color blank group",
      Counts (25, 64, 16),
      Printf.sprintf "(-> %s (sop/graph_color :group_owner \"Point\" :group \" \"))" grid );
    ( "graph_color worksets",
      Rejected,
      Printf.sprintf
        "(-> %s (sop/graph_color :output_worksets true :workset_begin_attribute \"begin\" \
         :workset_length_attribute \"length\"))"
        grid );
    ( "poly_cut crossing on P",
      Rejected,
      Printf.sprintf "(-> %s (sop/poly_cut :detection \"Attribute crossing\" :attribute \"P\"))"
        grid );
    ( "convex_hull P ancestry",
      Rejected,
      Printf.sprintf "(-> %s (sop/convex_hull :hull_group \"\" :source_point_attribute \"P\"))" grid
    );
    ( "extract_centroid P piece",
      Rejected,
      "(-> (sop/line :kind \"Points\" :points 3) (sop/extract_centroid :run_over \"Point pieces\" \
       :piece_attribute \"P\"))" );
    ( "boolean shatter same groups",
      Rejected,
      Printf.sprintf
        "(sop/boolean :operation \"Shatter\" :left_piece_group \"same\" :right_piece_group \
         \"same\" %s %s)"
        box box );
    ( "attribute_mirror empty mapping",
      Rejected,
      Printf.sprintf
        "(-> %s (sop/attribute_mirror :method_ \"Mapping attribute\" :mapping_attribute \"\" \
         :mapping_destination_group \"destination\"))"
        grid );
    ( "attribute_fade negative duration",
      Rejected,
      Printf.sprintf "(-> %s (sop/attribute_fade :fade_in -1.))" grid );
    ( "boolean_detect no outputs",
      Rejected,
      Printf.sprintf
        "(sop/boolean_detect :intersecting_group \"\" :self_intersecting_group \"\" %s %s)" box box
    );
    ( "boolean_detect same attributes",
      Rejected,
      Printf.sprintf
        "(sop/boolean_detect :intersections_attribute \"same\" :count_attribute \"same\" \
         :self_intersecting_group \"\" %s %s)"
        box box );
    ( "boolean_detect colliding group names",
      Rejected,
      Printf.sprintf "(sop/boolean_detect :self_intersecting_group \"boolean_intersections\" %s %s)"
        box box );
    ( "intersection_analysis same attributes",
      Rejected,
      Printf.sprintf
        "(sop/intersection_analysis :input_attribute \"same\" :primitive_attribute \"same\" %s)" box
    );
    ( "triangulate_2d blank group",
      Counts (25, 96, 32),
      Printf.sprintf "(-> %s (sop/triangulate_2d :point_group \" \"))" grid );
    (* Lisp keyword wiring, cardinality of the cooked geometry *)
    ("dissolve", Counts (25, 64, 16), Printf.sprintf "(-> %s (sop/dissolve))" grid);
    ( "transform keeps counts",
      Counts (25, 64, 16),
      Printf.sprintf "(-> %s (sop/transform :translate [1 0 0]))" grid );
    ( "attribute_laplacian",
      Counts (1106, 6624, 2208),
      Printf.sprintf
        "(-> %s (sop/group_bounds :name \"upper\" :center [0 1 0] :size [8 2 8]) \
         (sop/attribute_laplacian :point_group \"upper\" :weighting \"Positive cotangent\" \
         :normalize false :output \"delta_p\"))"
        sphere );
    ( "measure_curvature",
      Counts (1106, 6624, 2208),
      Printf.sprintf
        "(-> %s (sop/measure_curvature :boundary \"One-sided\" :smoothing_iterations 2 \
         :smoothing_strength 0.25 :gaussian \"gaussian\" :minimum \"minimum\" :maximum \"maximum\" \
         :curvedness \"curvedness\" :shape_index \"shape\"))"
        sphere );
    ( "sort",
      Counts (60001, 0, 0),
      "(-> (sop/line :kind \"Points\" :points 60001) (sop/enumerate :name \"id\") (sop/sort :key \
       \"Random\" :seed 73421) (sop/sort :key \"X\" :output_indices \"rank\"))" );
  ]

let context domains = Context.create ~domains ~grain:257 () |> get

let cook node domains =
  let session = Session.create ~max_entries:8 ~max_payload_bytes:160_000_000 |> get in
  Fun.protect
    ~finally:(fun () -> Session.close session)
    (fun () ->
      match Session.cook session ~context:(context domains) node with
      | Ok output -> Ok (Result.get_ok (Payload.geometry output.Session.payload))
      | Error error -> Error error.Diagnostic.code)

let check_row (name, expect, text) =
  match (Lisp_sop.node_result text, expect) with
  | Error _, Rejected -> None
  | Error message, _ -> Some (name ^ ": does not lower: " ^ message)
  | Ok _, Rejected -> Some (name ^ ": lowered but must be rejected")
  | Ok node, Code code -> (
      match (cook node 1, cook node 4) with
      | Error one, Error four when one = code && four = code -> None
      | Error one, Error four ->
          Some (Printf.sprintf "%s: codes %s / %s, expected %s" name one four code)
      | _ -> Some (name ^ ": cooked, expected " ^ code))
  | Ok node, Counts (points, vertices, primitives) -> (
      match (cook node 1, cook node 4) with
      | Ok one, Ok four ->
          let counts = Rdk.Geometry.(point_count one, vertex_count one, primitive_count one) in
          if not (equal_geometry one four) then Some (name ^ ": one and four domains differ")
          else if counts <> (points, vertices, primitives) then
            let p, v, q = counts in
            Some
              (Printf.sprintf "%s: counts (%d, %d, %d), expected (%d, %d, %d)" name p v q points
                 vertices primitives)
          else None
      | Error code, _ | _, Error code -> Some (name ^ ": failed with " ^ code))

(* The cache identity of a Lisp node (these two have no rdk twin). *)

(* The immutable cache and the shared source geometry (no rdk twin). *)
let check_cache_and_sharing () =
  let source =
    Rdk_test_support.Uv_sphere.run
      ~connectivity:Rdk_test_support.Uv_sphere.Sphere_alternating_triangles ~segments:48 ~rings:24
      ~radius:2. ()
    |> Result.get_ok
  in
  let with_ = [ ("src", Lisp_sop.snapshot source) ] in
  List.iter
    (fun (name, text) ->
      let node = Lisp_sop.node ~with_ text in
      let session = Session.create ~max_entries:4 ~max_payload_bytes:64_000_000 |> get in
      let cook domains =
        match Session.cook session ~context:(context domains) node with
        | Ok output -> Result.get_ok (Payload.geometry output.Session.payload)
        | Error error -> fail (Diagnostic.error_to_string error)
      in
      let first = cook 1 and before = Session.stats session in
      let repeated = cook 4 and after = Session.stats session in
      Session.close session;
      check (first == repeated && after.hits > before.hits) (name ^ " missed the immutable cache");
      check
        (Rdk.Geometry.topology first == Rdk.Geometry.topology source
        && Rdk.Geometry.positions first == Rdk.Geometry.positions source)
        (name ^ " did not share the source geometry"))
    [
      ("attribute_laplacian", "(-> (sop/ext_src) (sop/attribute_laplacian :output \"delta_p\"))");
      ("measure_curvature", "(-> (sop/ext_src) (sop/measure_curvature))");
    ]

(* The context facts a node declares decide its cache invalidation. *)
let check_dependencies () =
  let facts text = Context.Dependencies.to_list (Node.dependencies (Lisp_sop.node text)) in
  let attribute_fade =
    Node.dependencies (Lisp_sop.node (Printf.sprintf "(-> %s (sop/attribute_fade))" grid))
  in
  check
    (Context.Dependencies.mem Context.Dependencies.Frame attribute_fade
    && (not (Context.Dependencies.mem Context.Dependencies.Time attribute_fade))
    && not (Context.Dependencies.mem Context.Dependencies.Seed attribute_fade))
    "Attribute Fade context dependency declaration is not frame-only";
  check
    (facts (Printf.sprintf "(-> %s (sop/extract_point_from_curve))" curve) = [])
    "static Extract Point from Curve depends on context facts";
  check
    (Context.Dependencies.mem Context.Dependencies.Time
       (Node.dependencies
          (Lisp_sop.node
             (Printf.sprintf "(-> %s (sop/extract_point_from_curve :cut \"Current time\"))" curve))))
    "current-time Extract Point from Curve lacks the time dependency";
  check
    (facts (Printf.sprintf "(-> %s (sop/poly_cut))" grid) = [])
    "PolyCut depends on context facts";
  check
    (facts (Printf.sprintf "(-> %s (sop/separate_pieces))" grid) = [])
    "Separate Pieces depends on context facts"

let run () =
  check_dependencies ();
  check_cache_and_sharing ();
  match List.filter_map check_row rows with
  | [] -> print_endline "sop diagnostics tests passed"
  | failures -> fail (String.concat "\n" failures)
