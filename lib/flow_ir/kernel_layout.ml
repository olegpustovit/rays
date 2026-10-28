type t = {
  view : Packed.Private.view;
  used : bool array;
  finite_checks : bool array;
  output_checks : bool;
  uniforms : int array;
  frames : int array;
  image_width : int option;
  image_height : int option;
  table_seeds : int array;
  gathers : (int * (int * int)) list;
  gather_lengths : int array;
  input_used : bool array;
  input_count : int;
  output_count : int;
  uniform_layout : (string * int * int) list;
  uniform_bytes : int;
  uniform_binding : int;
  status_binding : int;
  table_binding : int option;
  gather_base : int;
  count_base : int;
  counts : int option;
  scene_binding : int option;
  scene_spheres : int option;
  scene_instances : int option;
}

let rec guarded = function
  | Packed.Guarded (_, instruction) -> guarded instruction
  | instruction -> instruction

let plan ?(fast = false) ?(image = false) ?(instanced = false) program =
  let view = Packed.Private.view program in
  let offset = ref 4 and uniform_layout = ref [] in
  let uniform name width =
    let first = !offset in
    offset := first + width * 4;
    uniform_layout := (name, first, width) :: !uniform_layout;
    first / 4
  in
  let tabled =
    if fast then
      Array.to_list view.code
      |> List.filter_map (fun instruction ->
           match guarded instruction with
           | Packed.Array_prepare (u, _) -> Some u
           | _ -> None)
    else []
  in
  let uniforms =
    Array.mapi
      (fun index width ->
        if List.mem index tabled then 0
        else uniform ("uniform:" ^ string_of_int index) width)
      view.uniform_widths
  in
  let frames =
    Array.mapi
      (fun index instruction ->
        match guarded instruction with
        | Packed.Frame name | Packed.Int_frame name -> uniform (Printf.sprintf "frame:%d:%s" index name) 1
        | _ -> 0)
      view.code
  in
  let image_width = if image then Some (uniform "image:width" 1) else None in
  let image_height = if image then Some (uniform "image:height" 1) else None in
  (* fast kernels check only their outputs (specification/gpu-codegen.md): dead loop code is not kept *)
  let used = Packed.Private.output_reachable ~whole_loops:(not fast) program in
  (* fast checks its inputs (a resident producer's buffer is not scanned on the host) and outputs *)
  let finite_checks =
    if fast then Array.map (fun i -> match guarded i with Packed.Input _ -> true | _ -> false) view.code
    else Packed.Private.finite_checks program in
  let table_seeds =
    Array.to_list (Array.mapi (fun index instruction -> index, guarded instruction) view.code)
    |> List.filter_map (function
         | index, Packed.Noise3 (_, _, _, seed, _, _, _) when used.(index) -> Some seed
         | _ -> None)
    |> List.sort_uniq Int.compare |> Array.of_list
  in
  let gathers =
    if not fast then []
    else
      Array.to_list (Array.mapi (fun slot instruction -> slot, guarded instruction) view.code)
      |> List.filter_map (function
           | slot, Packed.Array_prepare (u, ty) when used.(slot) ->
               Some (slot, (u, match ty with
                 | Flow.Ty.Array Flow.Ty.Vec2 -> 2
                 | Flow.Ty.Array Flow.Ty.Vec3 -> 3
                 | Flow.Ty.Array Flow.Ty.Vec4 -> 4
                 | _ -> 1))
           | _ -> None)
  in
  let gather_lengths =
    List.mapi (fun k (_, (u, _)) -> uniform (Printf.sprintf "gather:%d:%d" k u) 1) gathers
    |> Array.of_list
  in
  let traced = Array.exists (fun i -> match guarded i with Packed.Trace _ -> true | _ -> false) view.code in
  let scene_spheres = if traced then Some (uniform "scene:spheres" 1) else None in
  let counts = if fast then (let first = !offset in offset := first + 12; Some first) else None in
  let count = if image then 0 else Array.length view.widths in
  let outputs = Array.length view.outputs in
  let uniform_binding = count + outputs in
  let status_binding = uniform_binding + 1 in
  let table_binding = if table_seeds = [||] then None else Some (status_binding + 1) in
  let gather_base = count + outputs + 2 + (if table_seeds = [||] then 0 else 1) in
  let count_base = gather_base + List.length gathers in
  let input_used = Array.make count false in
  Array.iteri
    (fun slot instruction ->
      match guarded instruction with
      | Packed.Input (input, _, _) | Packed.Int_input input when used.(slot) && not image -> input_used.(input) <- true
      | _ -> ())
    view.code;
  { view; used; finite_checks; output_checks = fast; uniforms; frames; image_width; image_height; table_seeds; gathers; gather_lengths;
    input_count = count; output_count = outputs;
    input_used; uniform_layout = List.rev !uniform_layout; uniform_bytes = !offset;
    uniform_binding; status_binding; table_binding;
    gather_base; count_base; counts;
    scene_spheres; scene_instances = (if traced && instanced then Some (count_base + (if counts = None then 0 else 2) + 3) else None);
    scene_binding = (if traced then Some (count_base + (if counts = None then 0 else 2)) else None) }

let uniform_words plan = (plan.uniform_bytes + 15) / 16

let iter plan ~instruction ~loop =
  let code=plan.view.code in
  let rec visit first last =
    let slot=ref first in
    while !slot<last do
      match guarded code.(!slot)with
      | Packed.Loop_begin control ->
          let header= !slot in
          let next,stop=match guarded code.(control.finish)with Packed.Loop_end(_,values,stop)->values,stop|_->assert false in
          loop header control ~next ~stop ~body:(fun()->visit (header+1+Array.length control.initial)control.finish);
          slot:=control.finish+1
      | _ ->instruction !slot code.(!slot);incr slot
    done in
  visit 0 (Array.length code)

module Scope = struct
  type t = { id : int; decls : Buffer.t; code : Buffer.t; mutable opened : (int * bool) list }

  let create id = { id; decls = Buffer.create 1024; code = Buffer.create 4096; opened = [] }
  let indent scope = String.make (2 * (1 + List.length scope.opened)) ' '

  let enter scope ~opening guards =
    let rec common = function x :: a, y :: b when x = y -> 1 + common (a, b) | _ -> 0 in
    let kept = common (scope.opened, guards) in
    for depth = List.length scope.opened downto kept + 1 do
      Buffer.add_string scope.code (String.make (2 * depth) ' ' ^ "}\n")
    done;
    List.iteri
      (fun depth (condition, selected) ->
        if depth >= kept then
          Printf.bprintf scope.code "%s%s\n" (String.make (2 * (depth + 1)) ' ') (opening condition selected))
      guards;
    scope.opened <- guards
end
