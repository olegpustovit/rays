type t = {
  view : Packed.Private.view;
  used : bool array;
  finite_checks : bool array;
  output_checks : bool;
  narrow : bool array;
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

(* Fast kernels print an Int register as a 32-bit unsigned word when that keeps its observable
   value: it always lies in [0, 2^32) ([fits]), or every use reads only its low 32 bits (wrapping
   +, -, *, and, or, xor, the shifted operand of a left shift, a select, a carried next value).
   The low 32 bits of signed-63 wrapping arithmetic are the uint32 results, so CPU bits are kept.
   Both are greatest fixpoints over the loop-carried registers. *)
let narrow_registers (view : Packed.Private.view) used =
  let code = Array.map guarded view.code in
  let size = Array.length code in
  let integer slot = Core.Region.integer_register view.code.(slot) in
  let carried = Array.make size None and results = Array.make size [] in
  Array.iteri (fun slot -> function
    | Packed.Loop_begin loop ->
        let next = match code.(loop.finish) with Packed.Loop_end (_, next, _) -> next | _ -> assert false in
        Array.iteri (fun c initial -> carried.(slot + 1 + c) <- Some (initial, next.(c))) loop.initial
    | Packed.Loop_result (loop, c, _) -> results.(loop + 1 + c) <- slot :: results.(loop + 1 + c)
    | _ -> ()) code;
  let fixpoint flags rule =
    let changed = ref true in
    while !changed do
      changed := false;
      Array.iteri (fun slot flag -> if flag && not (rule slot) then (flags.(slot) <- false; changed := true)) flags
    done in
  (* upper bounds of nonnegative Int registers: [`Bot] not yet reached, [`Max m] in [0, m] *)
  let limit = 0xffff_ffff in
  let bound = Array.make size `Bot in
  let at_most m = if 0 <= m && m <= limit then `Max m else `Top in
  let join a b = match a, b with `Bot, x | x, `Bot -> x | `Max a, `Max b -> `Max (max a b) | _ -> `Top in
  let both f a b = match bound.(a), bound.(b) with
    | `Bot, _ | _, `Bot -> `Bot | `Max a, `Max b -> f a b | _ -> `Top in
  let header = Array.make size (-1) in
  Array.iteri (fun slot -> function
    | Packed.Loop_begin loop -> Array.iteri (fun c _ -> header.(slot + 1 + c) <- slot) loop.initial
    | _ -> ()) code;
  (* a carried register's largest step per pass: the carry plus nonnegative bounded terms, or a select of such *)
  let rec step carry r = if r = carry then Some 0 else match code.(r) with
    | Packed.Int_binary (Iadd, a, b) ->
        (match step carry a, bound.(b), step carry b, bound.(a) with
         | Some d, `Max k, _, _ | _, _, Some d, `Max k -> Some (d + k) | _ -> None)
    | Int_select (_, yes, no) -> (match step carry yes, step carry no with Some a, Some b -> Some (max a b) | _ -> None)
    | _ -> None in
  let rule slot = match code.(slot) with
    | Packed.Int_const n -> at_most n
    | Int_binary (Iand, a, b) -> (match bound.(a), bound.(b) with
        | `Bot, _ | _, `Bot -> `Bot | `Max a, `Max b -> `Max (min a b) | `Max m, _ | _, `Max m -> `Max m | _ -> `Top)
    | Int_binary ((Ior | Ixor), a, b) ->
        both (fun a b -> let rec ones m = if m >= max a b then m else ones (2 * m + 1) in at_most (ones 0)) a b
    | Int_binary (Ishr, a, b) -> (match bound.(a), code.(b) with
        | `Max m, Packed.Int_const k when 0 <= k && k <= 62 -> `Max (m lsr k)
        | (`Max _ | `Bot) as m, _ -> m | `Top, _ -> `Top)
    | Int_binary (Iadd, a, b) -> both (fun a b -> at_most (a + b)) a b
    | Int_binary (Imul, a, b) -> both (fun a b -> if b = 0 || a <= limit / b then at_most (a * b) else `Top) a b
    | Int_binary ((Idiv | Imod), a, b) -> both (fun a _ -> `Max a) a b
    | Int_select (_, yes, no) -> join bound.(yes) bound.(no)
    | Loop_carry _ ->
        let initial, next = Option.get carried.(slot) in
        let loop = match code.(header.(slot)) with Packed.Loop_begin loop -> loop | _ -> assert false in
        (match step slot next, bound.(initial), bound.(loop.last) with
         | Some d, `Max i, `Max trips when d = 0 || trips <= limit / d -> at_most (i + d * trips)
         | _ -> join bound.(initial) bound.(next))
    | Loop_begin {first; last; _} -> both (fun _ last -> `Max last) first last
    | Loop_result (loop, c, _) -> bound.(loop + 1 + c)
    | _ -> `Top in
  let changed = ref true and passes = ref 0 in
  while !changed do
    changed := false; incr passes;
    Array.iteri (fun slot _ -> if integer slot then begin
      let next = rule slot in
      (* widen a carried bound that keeps growing *)
      let next = if !passes > 16 && next <> bound.(slot) && header.(slot) >= 0 then `Top else next in
      let joined = join bound.(slot) next in
      if joined <> bound.(slot) then (bound.(slot) <- joined; changed := true) end) code
  done;
  let fits = Array.map (function `Top -> false | `Bot | `Max _ -> true) bound in
  let uses = Array.make size [] in
  let add user operand = uses.(operand) <- user :: uses.(operand) in
  Array.iteri (fun slot instruction -> if used.(slot) then match instruction with
    | Packed.Loop_carry _ -> let initial, _ = Option.get carried.(slot) in add (`Carry slot) initial
    | Loop_begin {first; last; _} -> add `Exact first; add `Exact last
    | Loop_end (header, next, stop) ->
        Array.iteri (fun c value -> add (`Carry (header + 1 + c)) value) next;
        Option.iter (add `Exact) stop
    | Loop_result _ -> ()
    | Int_binary ((Iadd | Isub | Imul | Iand | Ior | Ixor), a, b) -> add (`Low slot) a; add (`Low slot) b
    | Int_binary (Ishl, a, b) -> add (`Low slot) a; add `Exact b
    | Int_select (condition, yes, no) -> add `Exact condition; add (`Low slot) yes; add (`Low slot) no
    (* a device gather clamps int(index), the low 32 bits *)
    | Gather (table, index, _, _) | Int_gather (table, index) -> add `Exact table; add `Bits index
    | instruction -> List.iter (add `Exact) (Core.Region.operands instruction)) code;
  Array.iter (Array.iter (add `Exact)) view.outputs;
  Array.iteri (fun slot instruction -> match instruction with
    | Packed.Guarded _ when used.(slot) ->
        let rec conditions = function Packed.Guarded (gs, body) -> List.map fst gs @ conditions body | _ -> [] in
        List.iter (add `Exact) (conditions instruction)
    | _ -> ()) view.code;
  let narrow = Array.init size (fun slot -> integer slot && match code.(slot) with
    | Packed.Int_const _ | Int_binary ((Iadd | Isub | Imul | Iand | Ior | Ixor | Ishl), _, _)
    | Int_select _ | Loop_carry _ -> true
    | Int_binary (Ishr, a, _) -> fits.(a)
    | Int_binary ((Idiv | Imod), a, b) -> fits.(a) && fits.(b)
    | Loop_begin _ -> fits.(slot)
    | _ -> false) in
  let low use = match use with `Low user | `Carry user -> narrow.(user) | `Bits -> true | `Exact -> false in
  fixpoint narrow (fun slot -> fits.(slot)
    || List.for_all low uses.(slot) && List.for_all (fun result -> List.for_all low uses.(result)) results.(slot));
  Array.mapi (fun slot instruction -> match instruction with
    | Packed.Loop_result (loop, c, _) -> narrow.(loop + 1 + c)
    | _ -> narrow.(slot)) code

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
  { view; used; finite_checks; output_checks = fast;
    narrow = (if fast then narrow_registers view used else Array.make (Array.length view.code) false); uniforms; frames; image_width; image_height; table_seeds; gathers; gather_lengths;
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
