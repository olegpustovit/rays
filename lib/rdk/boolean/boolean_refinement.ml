open Rays_math

type t = {
  constraints : Boolean_constraints.t;
  left : Boolean_face_cdt.t option array;
  right : Boolean_face_cdt.t option array;

}

let operation = "boolean_refinement"
let error code message = Error (Error.make ~operation ~code message)

let left_face value face = value.left.(face)
let right_face value face = value.right.(face)

module View = struct
  let constraints value = value.constraints
end

let build_side ?cancel ?coplanar ~grain constraints side face_count range
    coplanar_range =
  let output = Array.make face_count None and errors = Array.make face_count None in
  (* Keep enough sequential faces per exclusively owned CDT workspace to
     amortize its edge/topology planes while retaining many coarse tasks on
     the small, high-intersection fracture inputs that use grain one or two. *)
  let range_size = max 8 grain in
  let range_count = (face_count + range_size - 1) / range_size in
  if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
      ~finish:(range_count - 1) (fun work_range ->
    let workspace = Boolean_face_cdt.create_workspace () in
    let first_face = work_range * range_size
    and last_face = min face_count ((work_range + 1) * range_size) in
    for face = first_face to last_face - 1 do
      Cancel.poll cancel face 255;
      let first, last = range constraints face in
      let coplanar_first, coplanar_last = match coplanar with
        | None -> 0, 0
        | Some value -> coplanar_range value face in
      if first < last || coplanar_first < coplanar_last then
        match Boolean_face_arrangement.build ?cancel ?coplanar constraints
            ~side ~triangle:face with
        | Error failure -> errors.(face) <- Some failure
        | Ok arrangement ->
            (match Boolean_face_cdt.build ?cancel ~workspace constraints arrangement
                ~side ~triangle:face with
             | Ok triangulation -> output.(face) <- Some triangulation
             | Error failure -> errors.(face) <- Some failure)
    done);
  let first_error = ref None and face = ref 0 and refined = ref 0 in
  while Option.is_none !first_error && !face < face_count do
    (match errors.(!face) with
     | Some failure -> first_error := Some failure
     | None -> if Option.is_some output.(!face) then incr refined);
    incr face
  done;
  match !first_error with
  | Some failure -> Error failure
  | None -> Ok (output, !refined)

let build ?cancel ?coplanar ~grain constraints =
  try
    if grain <= 0 then error "invalid_parameter" "grain must be positive"
    else
      let left_count = Boolean_constraints.left_triangle_count constraints
      and right_count = Boolean_constraints.right_triangle_count constraints in
      match build_side ?cancel ~grain constraints Boolean_constraints.Left
          ?coplanar left_count Boolean_constraints.left_constraint_range
          Boolean_coplanar.View.left_pair_range with
      | Error failure -> Error failure
      | Ok (left, _refined_left) ->
          match build_side ?cancel ~grain constraints Boolean_constraints.Right
              ?coplanar right_count Boolean_constraints.right_constraint_range
              Boolean_coplanar.View.right_pair_range with
          | Error failure -> Error failure
          | Ok (right, _refined_right) ->
              Ok { constraints; left; right;  }
  with Cancel.Cancelled -> error "cancelled" "Boolean refinement was cancelled"
