module E = Flow.Eval
module P = Projection

type path = Flow.Workspace.path

type geometry = {
  points : int;
  prims : int;
  groups : string list;
  extent : (float * float * float) option;
  seconds : float option;
  attributes : (string * string * string * int) list;
}

type image = { node : int; width : int; height : int }
type summary = Value of E.value | Geometry of geometry | Image of image
type execution = { tier : string; group : path; seconds : float option }

type force =
  ?state:E.state ->
  ?elems:(string * E.value) list ->
  ?resolve:(E.value -> (E.value, Diagnostic.t) result) ->
  E.value ->
  live:Input.t ->
  (E.value, Diagnostic.t) result

type footer = {
  value : string;
  spark : (float array * int) option;
  branch : string option;
  kept : string option;
  runs : int option;
  invariant : bool;
  live : bool;
  execution : execution option;
}

type t = {
  force : force;
  guard : 'a. fallback:'a -> (unit -> 'a) -> 'a;
      (* every evaluation a probe starts, stopped past its budget (see [make]) *)
  execution : path -> probes:int list -> execution option;
  live_frame : Input.t option;
  state : E.state option;
  resolve : (E.value -> (E.value, Diagnostic.t) result) option;
  maps :
    ( path * int list,
      (int * int * (int -> ((path * (int list * E.value) list) list, Diagnostic.t) result)) list )
    Hashtbl.t;
  bulk_calls :
    path ->
    int list ->
    (int * int * (int -> ((path * (int list * E.value) list) list, Diagnostic.t) result)) list;
  calls : (path * int list, (path * (int list * E.value) list) list) Hashtbl.t;
  dynamic : path -> int option;
  templates : (path, unit) Hashtbl.t;
  element : path -> int -> (string * E.value) list option;
      (* the element [k] of the loop over geometry at this path, by its name *)
  geometry : int -> geometry option;
  image : int -> image option;
  raw : (path, (int list * E.value) list) Hashtbl.t;
  forced : (path, (int list * summary) array) Hashtbl.t;
      (* memo: forcing is per lookup, not per frame *)
  spots : (path * int list, summary option) Hashtbl.t;
  across : (path * int list, summary array) Hashtbl.t; (* memo of [series] *)
  feet : (path * int list, footer) Hashtbl.t; (* memo of [footer]: the pane asks every frame *)
  arms : (path * int list, int option) Hashtbl.t;
}

(* What a probe reads once one ran over: the frame it ran on is the UI's. *)
let stopped = Error (Diagnostic.error ~code:"cancelled" "Probe over its time budget.")
let budget = 0.008

let make ?clock ?state ?live ?time ?resolve ?force ?(bulk_calls = fun _ _ -> [])
    ?(execution = fun _ ~probes:_ -> None) ?(geometry = fun _ -> None) ?(image = fun _ -> None)
    ?(dynamic = fun _ -> None) ?(element = fun _ _ -> None) (eval : E.t) =
  let raw = Hashtbl.create 64 in
  List.iter (fun (p, l) -> Hashtbl.replace raw p l) eval.records;
  let templates = Hashtbl.create 8 in
  Array.iter
    (fun (node : E.node) ->
      if node.kind = "zone/points" || node.kind = "zone/pieces" then
        Hashtbl.replace templates node.site ())
    eval.plan.nodes;
  let live_frame = match live with Some _ -> live | None -> Option.map Input.at_time time in
  let state = Option.map E.fork_state state in
  let force : force option = force in
  (* With a [clock], one evaluation stops after [budget] seconds and every later one on these
     records is skipped: a probe never holds a frame longer, whatever the graph (the next cook's
     records try again). Without one (tests, exports) probes run to the end. *)
  let over = ref false in
  let guard ~fallback f =
    match clock with
    | None -> f ()
    | Some _ when !over -> fallback
    | Some clock -> (
        let start = clock () in
        match E.with_interrupt (fun () -> clock () -. start > budget) f with
        | Some v -> v
        | None ->
            over := true;
            fallback)
  in
  let force = Option.value ~default:E.Host.force_reference force in
  let force ?state ?elems ?resolve v ~live =
    guard ~fallback:stopped (fun () -> force ?state ?elems ?resolve v ~live)
  in
  let image id =
    let dimensions =
      match image id with
      | Some _ as dimensions -> dimensions
      | None ->
          if id < 0 || id >= Array.length eval.plan.nodes then None
          else
            let node = eval.plan.nodes.(id) in
            if node.kind <> "image/noise" && node.kind <> "image/render" then None
            else
              let live = Option.value ~default:(Input.at_time 0.) live_frame in
              let dimension name default =
                match List.assoc_opt name node.args with
                | None -> Some default
                | Some value -> (
                    match force ?state ?resolve value ~live with
                    | Ok (E.Int n) when n > 0 -> Some n
                    | _ -> None)
              in
              let w, h = if node.kind = "image/noise" then (256, 256) else live.size in
              Option.bind (dimension "width" w) (fun width ->
                  Option.map (fun height -> (width, height)) (dimension "height" h))
    in
    Option.map (fun (width, height) -> { node = id; width; height }) dimensions
  in
  {
    force;
    guard = (fun ~fallback f -> guard ~fallback f);
    live_frame;
    state;
    resolve;
    execution;
    bulk_calls;
    maps = Hashtbl.create 16;
    calls = Hashtbl.create 16;
    dynamic;
    templates;
    element;
    geometry;
    image;
    raw;
    forced = Hashtbl.create 64;
    spots = Hashtbl.create 64;
    across = Hashtbl.create 64;
    feet = Hashtbl.create 64;
    arms = Hashtbl.create 8;
  }

let same_eval a b = a.raw == b.raw
let execution t path ~probes = t.execution path ~probes

(* the memos have a capacity: [forced] holds one entry per recorded path, [spots], [across], [feet] and [arms] one per
   path and probe tuple asked, so a full one starts over (its entries are computed again on demand) *)
let memo_capacity = 4096

let remember tbl key v =
  if Hashtbl.length tbl >= memo_capacity then Hashtbl.reset tbl;
  Hashtbl.replace tbl key v

let summarize t v =
  let v =
    match (t.live_frame, t.resolve) with
    | (Some _ | None), _ when E.is_live v && (t.live_frame <> None || Option.is_some t.resolve) -> (
        match
          t.force ?state:t.state ?resolve:t.resolve v
            ~live:(Option.value ~default:(Input.at_time 0.) t.live_frame)
        with
        | Ok v -> v
        | Error _ -> v)
    | _ -> v
  in
  match v with
  | E.Deferred (Flow.Ty.Named "geometry", id) -> (
      match t.geometry id with Some g -> Geometry g | None -> Value v)
  | E.Deferred (Flow.Ty.Named "image", id) -> (
      match t.image id with Some image -> Image image | None -> Value v)
  | v -> Value v

let plan_node ?(ty = Flow.Ty.geometry) t path ~probes =
  let find probes =
    Option.bind (Hashtbl.find_opt t.raw path)
      (List.find_map (fun (it, v) ->
           match v with
           | E.Deferred (deferred, id) when deferred = ty && it = probes -> Some id
           | _ -> None))
  in
  match find probes with
  | Some _ as found -> found
  | None ->
      let template = ref probes in
      for length = 1 to List.length path - 1 do
        let zone = List.take length path in
        if Hashtbl.mem t.templates zone then
          List.iter
            (fun (it, _) ->
              if it = List.take (List.length it) !template then
                template :=
                  List.mapi (fun i value -> if i = List.length it then 0 else value) !template)
            (Option.value ~default:[] (Hashtbl.find_opt t.raw zone))
      done;
      find !template

let records t path =
  match Hashtbl.find_opt t.forced path with
  | Some a -> a
  | None ->
      let a =
        match Hashtbl.find_opt t.raw path with
        | None -> [||]
        | Some l -> Array.of_list (List.map (fun (it, v) -> (it, summarize t v)) l)
      in
      remember t.forced path a;
      a

let record_count t path = Option.fold ~none:0 ~some:List.length (Hashtbl.find_opt t.raw path)

(* ---- describe ---- *)

let num x =
  let s = Printf.sprintf "%.3f" x in
  let s =
    if String.contains s '.' then begin
      let n = ref (String.length s) in
      while s.[!n - 1] = '0' do
        decr n
      done;
      if s.[!n - 1] = '.' then decr n;
      String.sub s 0 !n
    end
    else s
  in
  if s = "-0" then "0" else s

let rec describe_value = function
  | E.Int i -> string_of_int i
  | Float f -> num f
  | Bool b -> string_of_bool b
  | Text s -> s
  | Vec2 (x, y) -> Printf.sprintf "[%s %s]" (num x) (num y)
  | Vec3 (x, y, z) -> Printf.sprintf "[%s %s %s]" (num x) (num y) (num z)
  | Vec4 (x, y, z, w) -> Printf.sprintf "[%s %s %s %s]" (num x) (num y) (num z) (num w)
  | List xs ->
      let n = Array.length xs in
      let numeric = function E.Int _ | Float _ -> true | _ -> false in
      let what =
        if n = 0 then "items"
        else if Array.for_all numeric xs then
          "["
          ^ String.concat " " (List.init (min 4 n) (fun i -> describe_value xs.(i)))
          ^ if n > 4 then " …]" else "]"
        else
          match xs.(0) with
          | Deferred (Flow.Ty.Named "geometry", _) | No_geo -> "geometry"
          | Text _ -> "text"
          | Vec2 _ -> "vec2"
          | Vec3 _ -> "vec3"
          | Vec4 _ -> "vec4"
          | Bool _ -> "bool"
          | List _ -> "list"
          | Record _ -> "record"
          | Fn _ -> "function"
          | _ -> "value"
      in
      if n = 0 then "0 items" else Printf.sprintf "%d × %s" n what
  | Int_array xs -> Printf.sprintf "%d × int" (Array.length xs)
  | Text_array xs -> Printf.sprintf "%d × text" (Array.length xs)
  | Float_array xs -> Printf.sprintf "%d × float" (Array.length xs)
  | Vec2_array xs -> Printf.sprintf "%d × vec2" (Array.length xs / 2)
  | Vec3_array xs -> Printf.sprintf "%d × vec3" (Array.length xs / 3)
  | Vec4_array xs -> Printf.sprintf "%d × vec4" (Array.length xs / 4)
  | Record fs ->
      "{" ^ String.concat " · " (List.map (fun (f, v) -> f ^ " " ^ describe_value v) fs) ^ "}"
  | Deferred (ty, _) -> Flow.Ty.to_string ty
  | No_geo -> "nil"
  | Struct (op, _, _) | Source (op, _, _) -> op
  | Fn _ -> "function"
  | Residual _ -> "?"

let describe = function
  | Value v -> describe_value v
  | Image image -> Printf.sprintf "%d × %d image" image.width image.height
  | Geometry g -> (
      (* point-only geometry (a scatter) reads in points, thousands apart as the sheet's [1 204 pts] *)
      let spaced n =
        let s = string_of_int n in
        String.concat ""
          (List.mapi
             (fun i c ->
               (if i > 0 && (String.length s - i) mod 3 = 0 then " " else "") ^ String.make 1 c)
             (List.of_seq (String.to_seq s)))
      in
      (if g.prims = 0 && g.points > 0 then
         Printf.sprintf "%s pt%s" (spaced g.points) (if g.points = 1 then "" else "s")
       else Printf.sprintf "%d prim%s" g.prims (if g.prims = 1 then "" else "s"))
      ^ (if g.groups = [] then "" else " · groups " ^ String.concat ", " g.groups)
      ^
      match g.seconds with
      | Some s ->
          (if g.prims > 0 then Printf.sprintf " · %s pts" (spaced g.points) else "")
          ^ Printf.sprintf " · %.3f s" s
      | None -> "")

(* ---- chains, series ---- *)

let chains (s : P.scope) =
  let tbl = Hashtbl.create 64 in
  let rec go chain (s : P.scope) =
    List.iter
      (fun (n : P.node) ->
        Hashtbl.replace tbl n.path chain;
        match n.zone with
        | Some z ->
            go
              (if z.kind = P.Let || z.kind = P.State || z.kind = P.Branch then chain
               else chain @ [ n.path ])
              z.scope
        | None -> ())
      s.nodes
  in
  go [] s;
  tbl

let rec drop_last = function [] | [ _ ] -> [] | x :: r -> x :: drop_last r

let map_calls t zone outer =
  match Hashtbl.find_opt t.maps (zone, outer) with
  | Some calls -> calls
  | None ->
      let calls =
        List.filter_map
          (fun (it, value) ->
            if it <> outer then None
            else
              match value with
              | E.Record [ ("offset", E.Int offset); ("call", E.Residual call) ] -> (
                  match
                    t.guard ~fallback:stopped (fun () ->
                        E.Host.map_probe ?state:t.state ?resolve:t.resolve ~offset call
                          ~live:(Option.value ~default:(Input.at_time 0.) t.live_frame))
                  with
                  | Ok (count, at) ->
                      Some (offset, count, fun k -> t.guard ~fallback:stopped (fun () -> at k))
                  | Error _ -> None)
              | _ -> None)
          (Option.value ~default:[] (Hashtbl.find_opt t.raw (zone @ [ "~calls" ])))
      in
      let base = List.fold_left (fun n (offset, count, _) -> max n (offset + count)) 0 calls in
      let bulk =
        t.guard ~fallback:[] (fun () -> t.bulk_calls zone outer)
        |> List.map (fun (offset, count, at) ->
            let at k = t.guard ~fallback:stopped (fun () -> at k) in
            let at k =
              Result.map
                (List.map (fun (path, values) ->
                     ( path,
                       List.map
                         (fun (iter, value) ->
                           ( List.mapi
                               (fun i n -> if i = List.length outer then n + base else n)
                               iter,
                             value ))
                         values )))
                (at k)
            in
            (base + offset, count, at))
      in
      let calls = calls @ bulk in
      remember t.maps (zone, outer) calls;
      calls

let map_scope t path probes =
  if probes = [] then None
  else
    let rec find zone =
      match zone with
      | [] -> None
      | _ -> (
          match map_calls t zone (drop_last probes) with
          | [] -> find (drop_last zone)
          | calls -> Some (zone, calls))
    in
    find (drop_last path)

let map_at t path probes =
  Option.bind (map_scope t path probes) (fun (zone, calls) ->
      let k = List.hd (List.rev probes) in
      let records =
        match Hashtbl.find_opt t.calls (zone, probes) with
        | Some records -> records
        | None ->
            let records =
              List.find_map
                (fun (offset, count, at) ->
                  if k < offset || k - offset >= count then None
                  else Result.to_option (at (k - offset)))
                calls
              |> Option.value ~default:[]
            in
            remember t.calls (zone, probes) records;
            records
      in
      Option.bind (List.assoc_opt path records)
        (List.find_map (fun (it, value) -> if it = probes then Some (summarize t value) else None)))

(* the template record of [path] forced for element [k] of the loop over geometry at [zone] *)
let at_element t path zone k =
  let raw =
    List.find_map
      (fun (it, v) -> if List.for_all (( = ) 0) it then Some v else None)
      (Option.value ~default:[] (Hashtbl.find_opt t.raw path))
  in
  match (raw, t.element zone k) with
  | Some raw, Some elems when E.is_live raw -> (
      match
        t.force ?state:t.state ?resolve:t.resolve ~elems raw
          ~live:(Option.value ~default:(Input.at_time 0.) t.live_frame)
      with
      | Ok v -> Some (summarize t v)
      | Error _ -> None)
  | _ -> None

(* the record with exactly this tuple; inside a loop over geometry the body is one template
   record (its iteration is 0), which reads the element: with the element's position known (the
   zone cooked) the value is forced for it, else it reads [?] *)
let compute_at t path ~probes =
  match map_at t path probes with
  | Some _ as found -> found
  | None -> (
      match
        Option.bind (Hashtbl.find_opt t.raw path)
          (List.find_map (fun (it, v) -> if it = probes then Some (summarize t v) else None))
      with
      | Some _ as found -> found
      | None -> (
          let zones =
            List.filter
              (fun i -> t.dynamic (List.take i path) <> None)
              (List.init (max 0 (List.length path - 1)) succ)
          in
          if zones = [] then None
          else
            let template it =
              List.compare_lengths it probes = 0
              && List.for_all2 (fun r p -> r = p || r = 0) it probes
            in
            let zone = List.take (List.fold_left max 0 zones) path in
            let raw =
              List.find_map
                (fun (it, v) -> if template it then Some v else None)
                (Option.value ~default:[] (Hashtbl.find_opt t.raw path))
            in
            let elems =
              if probes = [] then None
              else t.element zone (List.nth probes (List.length probes - 1))
            in
            match (raw, elems) with
            | Some raw, Some elems when E.is_live raw -> (
                match
                  t.force ?state:t.state ?resolve:t.resolve ~elems raw
                    ~live:(Option.value ~default:(Input.at_time 0.) t.live_frame)
                with
                | Ok v -> Some (summarize t v)
                | Error _ ->
                    List.find_map
                      (fun (it, v) -> if template it then Some (summarize t v) else None)
                      (Option.value ~default:[] (Hashtbl.find_opt t.raw path)))
            | _ ->
                List.find_map
                  (fun (it, v) -> if template it then Some (summarize t v) else None)
                  (Option.value ~default:[] (Hashtbl.find_opt t.raw path))))

let at t path ~probes =
  let key = (path, probes) in
  match Hashtbl.find_opt t.spots key with
  | Some (Some (Value (E.Deferred (Flow.Ty.Named "image", _) as value))) -> Some (summarize t value)
  | Some (Some (Image image)) -> Option.map (fun image -> Image image) (t.image image.node)
  | Some value -> value
  | None ->
      let value = compute_at t path ~probes in
      remember t.spots key value;
      value

let compute_taken_arm t (node : P.node) ~probes =
  let missing key =
    Option.bind t.live_frame (fun live ->
        Option.bind (Hashtbl.find_opt t.raw node.path) (fun records ->
            List.find_map
              (fun (tuple, value) ->
                match value with
                | E.Residual residual when tuple = probes ->
                    let view = E.Residual.residual_view residual in
                    let term =
                      match (view.term.node, key) with
                      | Flow.Workspace.If (test, _, _), "test" | Case (test, _, _), "of" ->
                          Some test
                      | Cond (arms, _), key when String.starts_with ~prefix:"test" key ->
                          Option.bind
                            (int_of_string_opt (String.sub key 4 (String.length key - 4)))
                            (fun i -> Option.map fst (List.nth_opt arms i))
                      | _ -> None
                    in
                    Option.bind term (fun term ->
                        Result.to_option
                          (E.Residual.eval_term ?state:t.state ?resolve:t.resolve residual term
                             ~live)
                        |> Option.map (fun value -> Value value))
                | _ -> None)
              records))
  in
  let at_key key =
    match List.rev node.path with
    | leaf :: outer -> (
        match at t (List.rev outer @ [ leaf ^ "#" ^ key ]) ~probes with
        | None -> missing key
        | some -> some)
    | [] -> None
  in
  let tests =
    List.filter_map
      (fun (row : P.row) -> match row.key with Flow_edit.Arm i when i >= 0 -> Some i | _ -> None)
      node.rows
  in
  let count = List.length tests in
  match node.head with
  | "if" -> (
      match at_key "test" with
      | Some (Value ((E.Bool _ | Int _ | Float _) as value)) ->
          Some (if Flow.Value.truthy value then 0 else 1)
      | _ -> None)
  | "cond" ->
      let rec find = function
        | [] -> Some count
        | i :: rest -> (
            match at_key ("test" ^ string_of_int i) with
            | Some (Value ((E.Bool _ | Int _ | Float _) as value)) ->
                if Flow.Value.truthy value then Some i else find rest
            | _ -> None)
      in
      find tests
  | "case" -> (
      let equal value (literal : Flow.Syntax.t) =
        match (value, literal.node) with
        | E.Text a, Str b -> a = b
        | (Bool _ | Int _ | Float _), Sym "true" -> Flow.Value.truthy value
        | (Bool _ | Int _ | Float _), Sym "false" -> not (Flow.Value.truthy value)
        | Bool a, Num b -> a = (float_of_string b <> 0.)
        | Int a, Num b -> float a = float_of_string b
        | Float a, Num b -> a = float_of_string b
        | _ -> false
      in
      match at_key "of" with
      | Some (Value value) ->
          Some
            (Option.value ~default:count
               (List.find_opt
                  (fun i ->
                    let key = Flow_edit.Pos ((2 * i) + 1) in
                    List.exists
                      (fun (row : P.row) ->
                        row.key = key && Option.fold ~none:false ~some:(equal value) row.expr)
                      node.rows)
                  tests))
      | _ -> None)
  | _ -> None

let taken_arm t (node : P.node) ~probes =
  let key = (node.path, probes) in
  match Hashtbl.find_opt t.arms key with
  | Some arm -> arm
  | None ->
      let arm = compute_taken_arm t node ~probes in
      remember t.arms key arm;
      arm

(* the records whose tuple is [outer] followed by one more index, in order; in a loop over
   geometry the body has one template record, which is forced for each element once the zone
   cooked *)
let across t path ~outer =
  match Hashtbl.find_opt t.across (path, outer) with
  | Some a -> a
  | None ->
      let n = List.length outer in
      let matches it =
        List.compare_length_with it (n + 1) = 0 && List.for_all2 ( = ) outer (List.take n it)
      in
      let a =
        Array.of_seq
          (Seq.filter_map
             (fun (it, s) -> if matches it then Some s else None)
             (Array.to_seq (records t path)))
      in
      let a =
        if Array.length a > 1 || outer <> [] then a
        else
          let zones =
            List.filter
              (fun i -> t.dynamic (List.take i path) <> None)
              (List.init (max 0 (List.length path - 1)) succ)
          in
          match zones with
          | [] -> a
          | _ -> (
              let zone = List.take (List.fold_left max 0 zones) path in
              match t.dynamic zone with
              | Some count when count > 1 ->
                  let forced = Array.init count (fun k -> at_element t path zone k) in
                  if Array.for_all Option.is_some forced then Array.map Option.get forced else a
              | _ -> a)
      in
      let a =
        match map_scope t path (outer @ [ 0 ]) with
        | Some (_, calls) when calls <> [] ->
            let count =
              List.fold_left (fun n (offset, count, _) -> max n (offset + count)) 0 calls
            in
            (* ponytail: at most 64 reference samples per packed function, shared
               across its body cards; an explicit full-array inspector can page later. *)
            let samples = min 64 count in
            let sampled =
              Array.init samples (fun i ->
                  let k =
                    if samples < 2 then 0
                    else
                      int_of_float
                        (Float.round (float i *. float (count - 1) /. float (samples - 1)))
                  in
                  map_at t path (outer @ [ k ]))
            in
            if Array.for_all Option.is_some sampled then Array.map Option.get sampled else a
        | _ -> a
      in
      remember t.across (path, outer) a;
      a

let series t path ~probes = if probes = [] then [||] else across t path ~outer:(drop_last probes)

(* Where the probed iteration sits in [series]: the iterations a [:skip] left out have no record,
   so it is the number of recorded ones before it (the probe itself when none is missing). *)
let position t path ~probes ~len =
  let n = List.length probes - 1 in
  let outer = drop_last probes and p = List.nth probes n in
  match map_scope t path probes with
  | Some (_, calls) when calls <> [] ->
      let count = List.fold_left (fun n (offset, count, _) -> max n (offset + count)) 0 calls in
      if count < 2 then 0
      else
        min (len - 1) (int_of_float (Float.round (float p *. float (len - 1) /. float (count - 1))))
  | _ ->
      let mine =
        List.filter
          (fun (it, _) -> List.compare_length_with it (n + 1) = 0 && List.take n it = outer)
          (Array.to_list (records t path))
      in
      (* a loop over geometry has one template record, forced for each element: no iteration is missing *)
      if List.length mine <> len then min p (len - 1)
      else min (List.length (List.filter (fun (it, _) -> List.nth it n < p) mine)) (len - 1)

let counts t (s : P.scope) ~probe =
  let chain = chains s in
  List.filter_map
    (fun (n : P.node) ->
      let z = Option.get n.zone in
      let outer = List.map probe (Option.value ~default:[] (Hashtbl.find_opt chain n.path)) in
      let calls = map_calls t n.path outer in
      if calls <> [] then
        Some (n.path, List.fold_left (fun n (offset, count, _) -> max n (offset + count)) 0 calls)
      else
        match t.dynamic n.path with
        | Some count -> Some (n.path, count)
        | None ->
            List.find_map
              (fun (r : P.rail_row) ->
                if r.role = P.Capture then None
                else
                  match across t (n.path @ [ ":" ^ List.hd r.names ]) ~outer with
                  | a when Array.length a = 0 -> None
                  | a -> Some (n.path, Array.length a))
              z.rail)
    (P.zones s)

(* ---- footers ---- *)

let number = function
  | Value (E.Int i) -> Some (float i)
  | Value (Float f) -> Some f
  | Value (Bool b) -> Some (if b then 1. else 0.)
  | _ -> None

let arg (n : P.node) i =
  Option.bind
    (List.find_opt (fun (r : P.row) -> r.key = Flow_edit.Pos i) n.rows)
    (fun (r : P.row) ->
      match r.expr with Some { Flow.Syntax.node = Sym s; _ } -> Some s | _ -> None)

let compute_footer t (n : P.node) ~probes =
  let parent = drop_last n.path in
  let here = at t n.path ~probes in
  let value =
    match here with
    | Some s -> describe s
    | None ->
        if probes <> [] && record_count t n.path > 0 then "not run here" else Flow.Ty.to_string n.ty
  in
  let across_probes = series t in
  let spark =
    if
      probes = [] || n.invariant
      || match n.ty with Flow.Ty.Float | Int | Bool | Any -> false | _ -> true
    then None
    else
      let a = Array.map number (across_probes n.path ~probes) in
      if Array.length a < 2 || Array.exists Option.is_none a then None
      else Some (Array.map Option.get a, position t n.path ~probes ~len:(Array.length a))
  in
  let branch =
    if List.mem n.head [ "cond"; "case" ] then
      Option.map (fun i -> Printf.sprintf "arm %d" (i + 1)) (taken_arm t n ~probes)
    else if probes = [] || n.head <> "if" then None
    else
      Option.bind (arg n 0) (fun c ->
          let cs = across_probes (parent @ [ c ]) ~probes in
          if Array.length cs = 0 then None
          else
            let yes =
              Array.fold_left (fun a s -> if s = Value (E.Bool true) then a + 1 else a) 0 cs
            in
            Some (Printf.sprintf "then %d · else %d" yes (Array.length cs - yes)))
  in
  let kept =
    if n.head <> "filter" then None
    else
      match (here, Option.bind (arg n 1) (fun s -> at t (parent @ [ s ]) ~probes)) with
      | Some (Value (E.List k)), Some (Value (E.List all)) ->
          Some (Printf.sprintf "kept %d of %d" (Array.length k) (Array.length all))
      | _ -> None
  in
  let invariant = probes <> [] && n.invariant in
  {
    value;
    spark;
    branch;
    kept;
    invariant;
    live = n.live;
    execution =
      (match (t.execution n.path ~probes, here) with
      | None, Some (Geometry _) -> Some { tier = "Cooked"; group = n.path; seconds = None }
      | execution, _ -> execution);
    runs =
      (if probes <> [] && not n.invariant then
         Some
           (match map_scope t n.path probes with
           | Some (_, calls) when calls <> [] ->
               List.fold_left (fun n (offset, count, _) -> max n (offset + count)) 0 calls
           | _ -> record_count t n.path)
       else None);
  }

let footer t (n : P.node) ~probes =
  if n.ty = Flow.Ty.image then compute_footer t n ~probes
  else
    match Hashtbl.find_opt t.feet (n.path, probes) with
    | Some f -> f
    | None ->
        let f = compute_footer t n ~probes in
        remember t.feet (n.path, probes) f;
        f

let text f =
  String.concat " · "
    (List.filter_map Fun.id
       [
         Some f.value;
         f.branch;
         f.kept;
         Option.map (Printf.sprintf "×%d") f.runs;
         (if f.invariant then Some "↑ same each time" else None);
         (if f.live then Some "t" else None);
         Option.map
           (fun e ->
             e.tier
             ^ Option.fold ~none:""
                 ~some:(fun seconds -> Printf.sprintf " · %.3f ms" (seconds *. 1000.))
                 e.seconds)
           f.execution;
       ])

let iterations t (n : P.node) ~probes = Array.map describe (series t n.path ~probes)

let geometry_targets t (s : P.scope) ~probe =
  let chain = chains s in
  let rec go (s : P.scope) =
    List.concat_map
      (fun (n : P.node) ->
        match n.zone with
        | Some z -> go z.scope
        | None when n.ty = Flow.Ty.geometry ->
            List.map
              (fun id -> (n.path, id))
              (Option.to_list
                 (plan_node t n.path ~probes:(List.map probe (Hashtbl.find chain n.path))))
        | None -> [])
      s.nodes
  in
  go s

let readouts t (n : P.node) ~probes =
  let footer = footer t n ~probes in
  [
    ((if probes = [] then "value" else "value at probe"), footer.value);
    ("cook", if n.live then "live, recooks every frame" else "cached");
  ]
  @ Option.fold ~none:[]
      ~some:(fun e ->
        [ ("tier", e.tier); ("execution group", String.concat "/" e.group) ]
        @ Option.fold ~none:[]
            ~some:(fun seconds -> [ ("group time", Printf.sprintf "%.3f ms" (seconds *. 1000.)) ])
            e.seconds)
      footer.execution
