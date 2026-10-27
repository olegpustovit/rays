open Rays
open Rdk
open Rdk_test_support

let get_ok = function Ok value -> value | Error _ -> fail "unexpected error"

let patterns () =
  let pattern source = Attribute_pattern.compile source |> get_ok in
  let reference_glob glob name =
    let glob_length = String.length glob and name_length = String.length name in
    let matches = Array.make_matrix (glob_length + 1) (name_length + 1) false in
    matches.(glob_length).(name_length) <- true;
    for glob_index = glob_length - 1 downto 0 do
      for name_index = name_length downto 0 do
        matches.(glob_index).(name_index) <- match glob.[glob_index] with
          | '*' -> matches.(glob_index + 1).(name_index)
              || (name_index < name_length
                  && matches.(glob_index).(name_index + 1))
          | '?' -> name_index < name_length
              && matches.(glob_index + 1).(name_index + 1)
          | literal -> name_index < name_length && literal = name.[name_index]
              && matches.(glob_index + 1).(name_index + 1)
      done
    done;
    matches.(0).(0) in
  let words alphabet max_length =
    let values = ref [""] and frontier = ref [""] in
    for _ = 1 to max_length do
      frontier := List.concat_map (fun prefix ->
        List.map (fun character -> prefix ^ String.make 1 character) alphabet)
        !frontier;
      values := List.rev_append !frontier !values
    done;
    !values in
  let expect_pattern source included excluded =
    let pattern = pattern source in
    List.iter (fun name -> if not (Attribute_pattern.matches pattern name) then
      fail (Printf.sprintf "attribute pattern %S did not include %S" source name))
      included;
    List.iter (fun name -> if Attribute_pattern.matches pattern name then
      fail (Printf.sprintf "attribute pattern %S did not exclude %S" source name))
      excluded in
  expect_pattern "" ["Cd"; "N"; "anything"] [];
  expect_pattern "Cd pscale" ["Cd"; "pscale"] ["N"; "Cdx"];
  expect_pattern "* ^tmp* ^N Cd" ["Cd"; "width"] ["N"; "tmp1"];
  expect_pattern "^tmp* ^N N" ["Cd"; "N"] ["tmp_cache"];
  expect_pattern "foo? value[0-2] kind[!0-9]"
    ["foo1"; "value0"; "value2"; "kindx"]
    ["foo"; "foooo"; "value3"; "kind7"];
  expect_pattern "literal\\* has\\ space \\^caret"
    ["literal*"; "has space"; "^caret"] ["literalX"; "has"];
  List.iter (fun invalid -> match Attribute_pattern.compile invalid with
    | Error _ -> ()
    | Ok _ -> fail (Printf.sprintf "accepted malformed attribute pattern %S" invalid))
    ["^"; "bad\\"; "[abc"; "[]"; "[z-a]"];
  let exhaustive_names = words ['a'; 'b'] 5
  and exhaustive_globs = List.filter (fun value -> value <> "")
      (words ['a'; 'b'; '*'; '?'] 5) in
  List.iter (fun glob ->
    let compiled = pattern glob in
    let rewrite = Attribute_pattern.compile_rewrite ~pattern:glob
        ~replacement:glob |> get_ok in
    List.iter (fun name ->
      let expected = reference_glob glob name
      and actual = Attribute_pattern.matches compiled name in
      if expected <> actual then fail (Printf.sprintf
        "attribute pattern/reference disagreement for %S and %S" glob name);
      let rewritten = Attribute_pattern.rewrite rewrite name in
      if rewritten <> (if expected then Some name else None) then
        fail (Printf.sprintf
          "attribute rewrite/reference disagreement for %S and %S" glob name))
      exhaustive_names)
    exhaustive_globs;
  let expect_rewrite source replacement name expected =
    let rewrite = Attribute_pattern.compile_rewrite ~pattern:source ~replacement
        |> get_ok in
    if Attribute_pattern.rewrite rewrite name <> expected then fail
        (Printf.sprintf "attribute rewrite %S -> %S failed for %S"
          source replacement name) in
  expect_rewrite "instance*" "my*instance" "instancepoint"
    (Some "mypointinstance");
  expect_rewrite "*_alpha_*" "*_beta_*" "lamp_alpha_matt"
    (Some "lamp_beta_matt");
  expect_rewrite "*_???" "old_???_*" "lamp_esr"
    (Some "old_esr_lamp");
  expect_rewrite "kind[ab]" "class?" "kinda" (Some "classa");
  expect_rewrite "literal\\**" "escaped_*" "literal*tail"
    (Some "escaped_tail");
  expect_rewrite "instance*" "my*instance" "other" None;
  List.iter (fun (source, replacement) ->
    match Attribute_pattern.compile_rewrite ~pattern:source ~replacement with
    | Error _ -> ()
    | Ok _ -> fail (Printf.sprintf "accepted invalid rewrite %S -> %S"
        source replacement))
    [("a*", "plain"); ("a b", "c d"); ("^a*", "b*"); ("a*", "")];
  let rewrite_set source replacement name expected =
    let rules = Attribute_pattern.compile_rewrite_set ~pattern:source
        ~replacement |> get_ok in
    let actual = Attribute_pattern.rewrite_set rules name in
    if actual <> expected then
      fail (Printf.sprintf
        "attribute rewrite set %S -> %S failed for %S: expected %s, got %s"
        source replacement name
        (Option.value ~default:"none" expected)
        (Option.value ~default:"none" actual)) in
  rewrite_set "foo* bar? ^bar_tmp" "out* short_?" "foo_tail"
    (Some "out_tail");
  rewrite_set "foo* bar? ^bar_tmp" "out* short_?" "bar7"
    (Some "short_7");
  rewrite_set "* foo*" "all_* exact*" "foo_tail" (Some "exact_tail");
  List.iter (fun (source, replacement) ->
    match Attribute_pattern.compile_rewrite_set ~pattern:source ~replacement with
    | Error _ -> ()
    | Ok _ -> fail (Printf.sprintf "accepted invalid rewrite set %S -> %S"
        source replacement))
    [("foo* bar*", "only_*"); ("^tmp*", "renamed_*");
     ("foo* ^tmp*", "one_* two_*")];
  (* A class member must land on its own bit on every compile: with a tiny minor heap, ocamlopt 5.3.0 (arm64)
     once stored a clobbered character from [escaped_character], so "[fa]*" intermittently missed "f". *)
  let heap = (Gc.get ()).minor_heap_size in
  Gc.set { (Gc.get ()) with minor_heap_size = 4096 };
  Fun.protect ~finally:(fun () -> Gc.set { (Gc.get ()) with minor_heap_size = heap }) (fun () ->
    for i = 1 to 200_000 do
      let compiled = pattern "[fa]* ^[a-b]x \\[z]" in
      ignore (Sys.opaque_identity (Array.make (i land 63) i));
      if not (Attribute_pattern.matches compiled "f" && Attribute_pattern.matches compiled "alpha"
              && Attribute_pattern.matches compiled "[z]")
         || Attribute_pattern.matches compiled "g" then
        fail (Printf.sprintf "class pattern compile %d placed a member on the wrong bit" i)
    done);
  ()

let lines () =
  let generated_line domains = Parallel.run ~domains (fun () ->
    Line_geometry.line ~grain:257 ~points:100_001
      ~origin:(Rays_math.Vec3.create 1. 2. 3.)
      ~direction:(Rays_math.Vec3.create max_float max_float 0.) ~length:(sqrt 2.) ()
    |> get_ok) in
  let line_one = generated_line 1 and line_many = generated_line 4 in
  let line_positions = Packed.Float3.Private.view (Geometry.positions line_one)
  and line_many_positions = Packed.Float3.Private.view
      (Geometry.positions line_many) in
  if Geometry.point_count line_one <> 100_001
      || Geometry.vertex_count line_one <> 100_001
      || Geometry.primitive_count line_one <> 1
      || Topology.primitive_kind (Geometry.topology line_one) 0
           <> Topology.Open_polyline
      || line_positions.x.(0) <> 1. || line_positions.y.(0) <> 2.
      || abs_float (line_positions.x.(100_000) -. 2.) > 1e-12
      || abs_float (line_positions.y.(100_000) -. 3.) > 1e-12
      || line_positions.x <> line_many_positions.x
      || line_positions.y <> line_many_positions.y
      || line_positions.z <> line_many_positions.z then
    fail "Line packed generation/domain exactness";
  let free_line = Line_geometry.line ~kind:Line_geometry.Line_points ~points:3
      ~origin:(Rays_math.Vec3.create (-1.) 0. 0.) ~direction:Rays_math.Vec3.unit_x ~length:2. ()
      |> get_ok in
  let free_positions = Packed.Float3.Private.view
      (Geometry.positions free_line) in
  if Geometry.primitive_count free_line <> 0
      || free_positions.x <> [|-1.; 0.; 1.|] then
    fail "Line free-point mode";
  (match Line_geometry.line ~origin:Rays_math.Vec3.zero ~direction:Rays_math.Vec3.zero ~length:1. () with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | _ -> fail "Line accepted a zero direction");
  (match Line_geometry.line ~points:1 ~origin:Rays_math.Vec3.zero ~direction:Rays_math.Vec3.unit_x
      ~length:1. () with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | _ -> fail "Line accepted one point in curve mode");
  (match Line_geometry.line ~origin:(Rays_math.Vec3.create max_float 0. 0.)
      ~direction:Rays_math.Vec3.unit_x ~length:max_float () with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | _ -> fail "Line accepted a non-finite endpoint");
  let cancelled_line = Cancel.create () in
  Cancel.cancel cancelled_line;
  (match Line_geometry.line ~cancel:cancelled_line ~points:10_000 ~origin:Rays_math.Vec3.zero
      ~direction:Rays_math.Vec3.unit_x ~length:1. () with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "Line ignored cancellation");
  (match Line_geometry.polyline [|(0., 0., 0.)|] with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | _ -> fail "Polyline accepted one point");
  ()

let kernel () =
  let count = 1_000_000 in
  let generated_ranges domains = Parallel.run ~domains (fun () ->
    Kernel.generate_point_ranges count
      (fun ~first ~last ~x ~y ~z ->
        for index = first to last - 1 do
          let value = float_of_int index in
          x.(index) <- value; y.(index) <- value *. 0.5; z.(index) <- -.value
        done)) in
  if not (equal_positions (generated_ranges 1) (generated_ranges 4)) then
    fail "million-point deterministic range kernel";
  ()

let run () = patterns (); lines (); kernel ()
