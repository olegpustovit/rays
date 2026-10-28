let operation = "boolean_solid"

type treatment = Solid | Surface
type operation = Union | Intersection | Difference | Reverse_difference | Xor | Shatter

type t = {
  complex : Boolean_complex.t;
  weiler : Boolean_weiler.t;
  cells : Boolean_cells.t;
  left_treatment : treatment;
  right_treatment : treatment;
}

module View = struct
  let weiler value = value.weiler
end

let bind result next = match result with Ok value -> next value | Error _ as error -> error

let prepare ?cancel ?resolve_left_self_intersections
    ?resolve_right_self_intersections ?(left_treatment = Solid)
    ?(right_treatment = Solid) ~grain ~left ~right () =
  bind (Boolean_constraints.build ?cancel ?resolve_left_self_intersections
      ?resolve_right_self_intersections ~grain ~left ~right ())
    (fun constraints ->
      let degenerate_pairs = Boolean_constraints.degenerate_pair_count constraints in
      if degenerate_pairs <> 0 then
        Error (Error.make ~operation ~code:"degenerate_triangle"
          ~hints:["remove or repair zero-area source triangles before solid Boolean"]
          (Printf.sprintf
             "%d candidate triangle pair(s) contain a degenerate source triangle"
             degenerate_pairs))
      else
        bind (Boolean_coplanar.build ?cancel ~grain constraints)
          (fun coplanar ->
            bind (Boolean_refinement.build ?cancel ~coplanar ~grain constraints)
              (fun refinement ->
                bind (Boolean_complex.build ?cancel constraints refinement)
                  (fun complex ->
                    bind (Boolean_radial.build ?cancel complex)
                      (fun radial ->
                        bind (Boolean_weiler.build ?cancel complex radial)
                          (fun weiler ->
                            bind (Boolean_cells.build ?cancel
                              ~track_left:(left_treatment = Solid)
                              ~track_right:(right_treatment = Solid)
                              complex weiler)
                              (fun cells ->
                                Ok { complex; weiler; cells;
                                  left_treatment; right_treatment })))))))

let extract_with_ancestry ?cancel ?require_closed ?defer_rounded_slivers
    ?corner_payload
    ~expression value =
  Boolean_extract.build_with_ancestry ?cancel ?require_closed
    ?defer_rounded_slivers ?corner_payload ~expression
    value.complex value.weiler value.cells

let operation_expression = function
  | Union -> Boolean_extract.union
  | Intersection -> Boolean_extract.intersection
  | Difference -> Boolean_extract.difference
  | Reverse_difference -> Boolean_extract.reverse_difference
  | Xor -> Boolean_extract.xor
  | Shatter -> invalid_arg "Shatter has no single product expression"

let first_member_on_side complex facet side =
  let first, last = Boolean_complex.facet_member_range complex facet in
  let found = ref (-1) and member = ref first in
  while !found < 0 && !member < last do
    if Boolean_complex.member_side complex !member = side then found := !member;
    incr member
  done;
  !found

let source_oriented_selection complex side keep =
  let facets = Boolean_complex.facet_count complex
  and selection = Bytes.make (Boolean_complex.facet_count complex) '\000' in
  for facet = 0 to facets - 1 do
    let member = first_member_on_side complex facet side in
    if member >= 0 && keep facet then
      Bytes.unsafe_set selection facet
        (if Boolean_complex.member_winding complex member > 0 then '\001' else '\002')
  done;
  selection

type location = Outside | Inside | Boundary

let relative_location cells weiler facet side =
  let negative = Boolean_weiler.half_facet_shell weiler
      (Boolean_weiler.half_facet facet Boolean_weiler.Negative)
  and positive = Boolean_weiler.half_facet_shell weiler
      (Boolean_weiler.half_facet facet Boolean_weiler.Positive) in
  let negative_winding, positive_winding = match side with
    | Boolean_complex.Left ->
        Boolean_cells.left_winding cells negative,
        Boolean_cells.left_winding cells positive
    | Boolean_complex.Right ->
        Boolean_cells.right_winding cells negative,
        Boolean_cells.right_winding cells positive in
  if negative_winding = 0 && positive_winding = 0 then Outside
  else if negative_winding <> 0 && positive_winding <> 0 then Inside
  else Boundary

let extract_selection ?cancel ?defer_rounded_slivers ?barycentric_cache
    ?corner_payload
    value side selection =
  Boolean_extract.View.build_selected_with_ancestry ?cancel
    ?defer_rounded_slivers ?barycentric_cache ?corner_payload ~selection ~side
    value.complex value.weiler value.cells

let selected_count selection =
  let count = ref 0 in
  for facet = 0 to Bytes.length selection - 1 do
    if Bytes.unsafe_get selection facet <> '\000' then incr count
  done;
  !count

let extract_mixed_with_ancestry ?cancel ?defer_rounded_slivers
    ?(corner_payload = true) ~operation
    ~solid_side ~surface_side value =
  let surface_location facet =
    relative_location value.cells value.weiler facet solid_side in
  let surface_is_left = surface_side = Boolean_complex.Left in
  let keep_surface facet = match operation, surface_location facet with
    | Union, Outside -> true
    | Intersection, (Inside | Boundary) -> true
    | Difference, Outside when surface_is_left -> true
    | Reverse_difference, Outside when not surface_is_left -> true
    | Xor, Outside -> true
    | _ -> false in
  let double_surface facet = match operation, surface_location facet with
    | Difference, Inside when not surface_is_left -> true
    | Reverse_difference, Inside when surface_is_left -> true
    | Xor, Inside -> true
    | _ -> false in
  let volume_present = match operation, solid_side with
    | (Union | Xor), _ -> true
    | Difference, Boolean_complex.Left -> true
    | Reverse_difference, Boolean_complex.Right -> true
    | _ -> false in
  let volume_selection = Bytes.make (Boolean_complex.facet_count value.complex)
      '\000' in
  if volume_present then
    for facet = 0 to Boolean_complex.facet_count value.complex - 1 do
      if first_member_on_side value.complex facet solid_side >= 0 then begin
        let negative = Boolean_weiler.half_facet_shell value.weiler
            (Boolean_weiler.half_facet facet Boolean_weiler.Negative)
        and positive = Boolean_weiler.half_facet_shell value.weiler
            (Boolean_weiler.half_facet facet Boolean_weiler.Positive) in
        let winding shell = match solid_side with
          | Boolean_complex.Left -> Boolean_cells.left_winding value.cells shell
          | Boolean_complex.Right -> Boolean_cells.right_winding value.cells shell in
        let negative_inside = winding negative <> 0
        and positive_inside = winding positive <> 0 in
        if negative_inside <> positive_inside then
          Bytes.unsafe_set volume_selection facet
            (if positive_inside then '\002' else '\001')
      end
    done;
  let surface_selection = source_oriented_selection value.complex surface_side
      keep_surface
  and double_selection = source_oriented_selection value.complex surface_side
      double_surface in
  let barycentric_cache = Boolean_extract.View.barycentric_cache
      ~capacity:(if corner_payload then 3 * (selected_count volume_selection
        + selected_count surface_selection + selected_count double_selection) else 0) in
  bind (extract_selection ?cancel ?defer_rounded_slivers ~barycentric_cache
      ~corner_payload value solid_side
      volume_selection) (fun volume ->
    bind (extract_selection ?cancel ?defer_rounded_slivers ~barycentric_cache
        ~corner_payload value surface_side
        surface_selection)
      (fun surface ->
        bind (extract_selection ?cancel ?defer_rounded_slivers ~barycentric_cache
            ~corner_payload value surface_side
            double_selection)
          (fun wall ->
            bind (Boolean_extract.View.reverse_ancestry ?cancel wall)
              (fun reverse_wall ->
                Boolean_extract.View.concatenate_ancestries ?cancel
                  [|volume; surface; wall; reverse_wall|]))))

let extract_surface_pair_with_ancestry ?cancel ?defer_rounded_slivers
    ?(corner_payload = true) ~operation value =
  let has facet side = first_member_on_side value.complex facet side >= 0 in
  let choose_left facet = match operation with
    | Union -> has facet Boolean_complex.Left
    | Intersection -> has facet Boolean_complex.Left && has facet Boolean_complex.Right
    | Difference -> has facet Boolean_complex.Left && not (has facet Boolean_complex.Right)
    | Reverse_difference -> false
    | Xor ->
        has facet Boolean_complex.Left <> has facet Boolean_complex.Right
    | Shatter -> invalid_arg "Shatter has no single product expression" in
  let choose_right facet = match operation with
    | Union -> not (has facet Boolean_complex.Left) && has facet Boolean_complex.Right
    | Intersection | Difference -> false
    | Reverse_difference ->
        has facet Boolean_complex.Right && not (has facet Boolean_complex.Left)
    | Xor -> not (has facet Boolean_complex.Left) && has facet Boolean_complex.Right
    | Shatter -> invalid_arg "Shatter has no single product expression" in
  let left = source_oriented_selection value.complex Boolean_complex.Left choose_left
  and right = source_oriented_selection value.complex Boolean_complex.Right choose_right in
  let barycentric_cache = Boolean_extract.View.barycentric_cache
      ~capacity:(if corner_payload then
        3 * (selected_count left + selected_count right) else 0) in
  bind (extract_selection ?cancel ?defer_rounded_slivers ~barycentric_cache
      ~corner_payload value
      Boolean_complex.Left left) (fun left ->
    bind (extract_selection ?cancel ?defer_rounded_slivers ~barycentric_cache
        ~corner_payload value Boolean_complex.Right right) (fun right ->
      Boolean_extract.View.concatenate_ancestries ?cancel [|left; right|]))

let extract_product_with_ancestry ?cancel ?require_closed ?defer_rounded_slivers
    ?(corner_payload = true) ~operation value =
  if operation = Shatter then invalid_arg "Shatter has no single product expression";
  let result = match value.left_treatment, value.right_treatment with
    | Solid, Solid -> extract_with_ancestry ?cancel ?require_closed
        ?defer_rounded_slivers
        ~corner_payload
        ~expression:(operation_expression operation) value
    | Solid, Surface ->
        extract_mixed_with_ancestry ?cancel ?defer_rounded_slivers
          ~corner_payload ~operation
          ~solid_side:Boolean_complex.Left ~surface_side:Boolean_complex.Right value
    | Surface, Solid ->
        extract_mixed_with_ancestry ?cancel ?defer_rounded_slivers
          ~corner_payload ~operation
          ~solid_side:Boolean_complex.Right ~surface_side:Boolean_complex.Left value
    | Surface, Surface -> extract_surface_pair_with_ancestry ?cancel
        ?defer_rounded_slivers ~corner_payload ~operation value in
  match result, require_closed, value.left_treatment, value.right_treatment with
  | (Ok ancestry, Some true, Surface, _
    | Ok ancestry, Some true, _, Surface) ->
      (match Boolean_extract.View.validate_closed_topology
          (Geometry.topology (Boolean_extract.geometry ancestry)) with
       | Ok () -> Ok ancestry
       | Error _ as failure -> failure)
  | result, _, _, _ -> result

let seams ?cancel ?grain ?materialize value =
  let parallel_cutoff = None in Boolean_seam.build ?cancel ?grain ?parallel_cutoff ?materialize value.complex

let shatter_with_ancestry ?cancel ?require_closed ?defer_rounded_slivers
    ?corner_payload value =
  let extract expression = extract_with_ancestry ?cancel ?require_closed
      ?defer_rounded_slivers ?corner_payload
      ~expression value in
  match extract Boolean_extract.difference with
  | Error _ as failure -> failure
  | Ok left_only ->
      (match extract Boolean_extract.intersection with
       | Error _ as failure -> failure
       | Ok overlap ->
           match extract Boolean_extract.reverse_difference with
           | Error _ as failure -> failure
           | Ok right_only -> Ok [|left_only; overlap; right_only|])
