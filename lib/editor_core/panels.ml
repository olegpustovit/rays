type panel =
  | View of string
  | Canvas of string
  | Graph
  | List
  | Lisp
  | Inspector
  | Spreadsheet
  | Outline
  | Timeline

type axis = [ `H | `V ]
type size = [ `Ratio of float | `First of int | `Second of int ]

type t =
  | Leaf of panel
  | Split of { axis : axis; size : size; a : t; b : t }
  | Tile of t list
  | Float of t

type path = int list
type state = { collapsed : bool; window : (int * int * int * int) option }

let default_state = { collapsed = false; window = None }
let main = View "main"

(* graph and inspector widths of the default 45/35/20 columns *)
let default =
  Split
    {
      axis = `H;
      size = `Ratio 0.45;
      a = Leaf main;
      b = Split { axis = `H; size = `Ratio (0.35 /. 0.55); a = Leaf Graph; b = Leaf Inspector };
    }

let name = function
  | Canvas _ -> "CANVAS"
  | View _ -> "VIEW"
  | Graph -> "GRAPH"
  | List -> "LIST"
  | Lisp -> "LISP"
  | Inspector -> "INSPECTOR"
  | Spreadsheet -> "SPREADSHEET"
  | Outline -> "OUTLINE"
  | Timeline -> "TIMELINE"

let leaves tree =
  (* ponytail: a plain recursive walk; floats are collected after the rest *)
  let rec walk path = function
    | Leaf p -> ([ (List.rev path, p) ], [])
    | Split { a; b; _ } -> both path [ a; b ]
    | Tile cells -> both path cells
    | Float t ->
        let r, f = walk (0 :: path) t in
        ([], r @ f)
  and both path children =
    let parts = List.mapi (fun i c -> walk (i :: path) c) children in
    (List.concat_map fst parts, List.concat_map snd parts)
  in
  let r, f = walk [] tree in
  r @ f

(* The subtree at a path: a split's sides are 0 and 1, a tile's cells their index, a float's
   panel 0. *)
let rec at path tree =
  match (path, tree) with
  | [], tree -> Some tree
  | 0 :: rest, Split s -> at rest s.a
  | 1 :: rest, Split s -> at rest s.b
  | i :: rest, Tile cells -> Option.bind (List.nth_opt cells i) (at rest)
  | 0 :: rest, Float t -> at rest t
  | _ -> None

let clamp_size : size -> size = function
  | `Ratio r -> `Ratio (Float.max 0.1 (Float.min 0.9 r))
  | `First n -> `First (max 1 n)
  | `Second n -> `Second (max 1 n)

let rec set_size path size tree =
  match (path, tree) with
  | [], Split s -> Split { s with size = clamp_size size }
  | 0 :: rest, Split s -> Split { s with a = set_size rest size s.a }
  | 1 :: rest, Split s -> Split { s with b = set_size rest size s.b }
  | i :: rest, Tile cells ->
      Tile (List.mapi (fun j c -> if i = j then set_size rest size c else c) cells)
  | 0 :: rest, Float t -> Float (set_size rest size t)
  | _ -> tree

let rec valid = function
  | Leaf _ -> Ok ()
  | Split { size; a; b; _ } -> (
      match size with
      | `Ratio r when r < 0.1 || r > 0.9 -> Error "Split ratio is 0.1-0.9."
      | (`First n | `Second n) when n < 1 -> Error "A fixed split size is 1 point or more."
      | _ -> Result.bind (valid a) (fun () -> valid b))
  | Tile cells ->
      let n = List.length cells in
      if n < 1 || n > 16 then Error "A tile holds 1-16 panels."
      else List.fold_left (fun r c -> Result.bind r (fun () -> valid c)) (Ok ()) cells
  | Float t -> valid t

let rec to_string = function
  | Leaf p -> String.lowercase_ascii (name p)
  | Split { axis; a; b; _ } ->
      Printf.sprintf "(%s %s %s)" (if axis = `H then "h" else "v") (to_string a) (to_string b)
  | Tile cells -> "(tile " ^ String.concat " " (List.map to_string cells) ^ ")"
  | Float t -> "(float " ^ to_string t ^ ")"

(* ---- layout names: a pure reading of the tree ---- *)

let word = function
  | Canvas _ -> "Canvas"
  | View _ -> "View"
  | Graph -> "Graph"
  | List -> "List"
  | Lisp -> "Lisp"
  | Inspector -> "Inspector"
  | Spreadsheet -> "Spreadsheet"
  | Outline -> "Outline"
  | Timeline -> "Timeline"

(* the tree without its floats; [None] when nothing is docked *)
let rec docked = function
  | Leaf _ as t -> Some t
  | Float _ -> None
  | Split s -> (
      match (docked s.a, docked s.b) with
      | Some a, Some b -> Some (Split { s with a; b })
      | Some x, None | None, Some x -> Some x
      | None, None -> None)
  | Tile cells -> ( match List.filter_map docked cells with [] -> None | cs -> Some (Tile cs))

let rec floats = function
  | Leaf _ -> []
  | Float t -> [ t ]
  | Split s -> floats s.a @ floats s.b
  | Tile cells -> List.concat_map floats cells

(* a tile reads as a row; neighbours of one name collapse to [name ×n]; a row inside a stack is bracketed *)
let rec shape parent = function
  | Leaf p -> word p
  | Float t -> shape parent t
  | t ->
      let axis = match t with Split s -> s.axis | _ -> `H in
      let rec parts = function
        | Split s when s.axis = axis -> parts s.a @ parts s.b
        | Tile cells when axis = `H -> List.concat_map parts cells
        | t -> [ t ]
      in
      let rec runs = function
        | [] -> []
        | n :: rest -> (
            match runs rest with (m, c) :: tl when m = n -> (m, c + 1) :: tl | tl -> (n, 1) :: tl)
      in
      let runs = runs (List.map (shape (Some axis)) (parts t)) in
      let text =
        String.concat
          (if axis = `H then " | " else " / ")
          (List.map (fun (n, c) -> if c > 1 then Printf.sprintf "%s \xc3\x97%d" n c else n) runs)
      in
      if axis = `H && parent = Some `V && List.length runs > 1 then "(" ^ text ^ ")" else text

let label t =
  let main = Option.fold ~none:[] ~some:(fun d -> [ shape None d ]) (docked t) in
  String.concat " + " (main @ List.map (fun f -> shape None f) (floats t))

(* the short reading, one name per column group: the docked panels side by side, each stack or tile
   as its first panel, neighbours of one name collapsed; floating windows count apart *)
let summary t =
  let rec first = function
    | Leaf p -> Some p
    | Float _ -> None
    | Split s -> ( match first s.a with Some _ as p -> p | None -> first s.b)
    | Tile cells -> List.find_map first cells
  in
  let rec columns = function
    | Split { axis = `H; a; b; _ } -> columns a @ columns b
    | Tile cells -> List.concat_map columns cells
    | Float _ -> []
    | t -> Option.to_list (Option.map word (first t))
  in
  let rec runs = function
    | [] -> []
    | n :: rest -> (
        match runs rest with (m, c) :: tl when m = n -> (m, c + 1) :: tl | tl -> (n, 1) :: tl)
  in
  String.concat " | "
    (List.map
       (fun (n, c) -> if c > 1 then Printf.sprintf "%s \xc3\x97%d" n c else n)
       (runs (columns t)))

(* the largest docked panel and its share, in percent *)
let largest t =
  let rec areas share = function
    | Leaf p -> [ (p, share) ]
    | Split s ->
        (* ponytail: a fixed side has no share without a window size; it reads as half *)
        let r = match s.size with `Ratio r -> r | `First _ | `Second _ -> 0.5 in
        areas (share *. r) s.a @ areas (share *. (1. -. r)) s.b
    | Tile cells -> List.concat_map (areas (share /. float (List.length cells))) cells
    | Float _ -> []
  in
  match Option.map (areas 1.) (docked t) with
  | Some (first :: rest) ->
      let p, a =
        List.fold_left (fun (p, a) (q, b) -> if b > a then (q, b) else (p, a)) first rest
      in
      Some (p, int_of_float (Float.round (a *. 100.)))
  | _ -> None

let labels trees =
  let base = List.map label trees in
  let step =
    List.map2
      (fun t n ->
        if List.length (List.filter (( = ) n) base) < 2 then n
        else
          match largest t with
          | Some (p, share) -> Printf.sprintf "%s \xc2\xb7 %s %d%%" n (word p) share
          | None -> n)
      trees base
  in
  let seen = Hashtbl.create 8 in
  List.map
    (fun n ->
      let k = 1 + Option.value ~default:0 (Hashtbl.find_opt seen n) in
      Hashtbl.replace seen n k;
      if k > 1 then Printf.sprintf "%s (%d)" n k else n)
    step
