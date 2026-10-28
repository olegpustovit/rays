(* An authored-form census, not a typechecker or an exporter. *)
module S = Flow.Syntax
module Names = Set.Make (String)
module Counts = Map.Make (String)

type declaration = {
  name : string;
  context : string option; (* a graph's; None for a defn or a macro, which are called *)
  written : string option; (* the context a graph or defn declares *)
  expressions : S.t list;
}

let rec files path =
  if Sys.is_directory path then
    Sys.readdir path |> Array.to_list |> List.sort String.compare
    |> List.concat_map (fun name -> files (Filename.concat path name))
  else if Filename.check_suffix path ".rays" then [ path ]
  else []

let declarations forms =
  let children =
    List.concat_map
      (fun (form : S.t) ->
        match form.node with
        | S.List ({ node = S.Sym "workspace"; _ } :: _ :: children) -> children
        | _ -> [])
      forms
  in
  List.filter_map
    (fun (form : S.t) ->
      match form.node with
      | S.List
          ({ node = S.Sym (("graph" | "defn") as kind); _ }
          :: { node = S.Sym name; _ }
          :: { node = S.Kw "context"; _ }
          :: { node = S.Sym context; _ }
          :: rest) ->
          let expressions =
            match rest with
            | [ body ] -> [ body ]
            | [ { node = S.Vec params; _ }; body ] ->
                let defaults =
                  List.filter_map
                    (fun (param : S.t) ->
                      match param.node with
                      | S.List [ _; { node = S.Sym ":"; _ }; _; value ] -> Some value
                      | _ -> None)
                    params
                in
                defaults @ [ body ]
            | _ -> []
          in
          Some
            {
              name;
              context = (if kind = "graph" then Some context else None);
              written = Some context;
              expressions;
            }
      | S.List ({ node = S.Sym "defmacro"; _ } :: { node = S.Sym name; _ } :: rest) ->
          Some
            {
              name;
              context = None;
              written = None;
              expressions = (match List.rev rest with body :: _ -> [ body ] | [] -> []);
            }
      | _ -> None)
    children

let rec heads counts (form : S.t) =
  let counts =
    match S.head form with
    | None -> counts
    | Some head -> Counts.update head (fun count -> Some (1 + Option.value count ~default:0)) counts
  in
  List.fold_left heads counts (S.children form)

let blockers declarations root =
  let rec visit seen blocked declaration =
    if Names.mem declaration.name seen then (seen, blocked)
    else List.fold_left walk (Names.add declaration.name seen, blocked) declaration.expressions
  and follow (seen, blocked) name =
    match List.find_opt (fun declaration -> declaration.name = name) declarations with
    | None -> (seen, Names.add ("missing:" ^ name) blocked)
    | Some declaration ->
        let blocked =
          match declaration.context with
          | Some (("sop" | "scene") as context) -> Names.add (name ^ ":" ^ context) blocked
          | _ -> blocked
        in
        visit seen blocked declaration
  and walk state (form : S.t) =
    let state =
      match form.node with
      | S.List ({ node = S.Sym "ref"; _ } :: { node = S.Sym name; _ } :: _) -> follow state name
      | _ -> (
          match S.head form with
          | Some name when List.exists (fun d -> d.name = name && d.context = None) declarations ->
              follow state name
          | _ -> state)
    in
    List.fold_left walk state (S.children form)
  in
  snd (visit Names.empty Names.empty root) |> Names.elements

(* what the core counts: a draw, host, image or value graph or defn, and macros; never sop or scene code *)
let core d =
  Option.fold ~none:true ~some:(fun c -> List.mem c [ "draw"; "host"; "image"; "value" ]) d.written

let census path forms counts =
  let declarations = declarations forms in
  let graphs =
    List.filter (fun d -> d.context <> None) declarations
    |> List.sort (fun a b -> String.compare a.name b.name)
  in
  Printf.printf "file %s contexts=%s\n" path
    (String.concat "," (List.map (fun d -> d.name ^ ":" ^ Option.get d.context) graphs));
  let draws = List.filter (fun d -> d.context = Some "draw") graphs in
  let eligible =
    List.fold_left
      (fun eligible graph ->
        let blocked = blockers declarations graph in
        Printf.printf "draw %s %s %s reaches=%s\n" path graph.name
          (if blocked = [] then "eligible" else "refused")
          (if blocked = [] then "-" else String.concat "," blocked);
        eligible && blocked = [])
      true draws
  in
  if draws <> [] && eligible then Printf.printf "eligible %s\n" path;
  List.fold_left
    (fun counts d -> if core d then List.fold_left heads counts d.expressions else counts)
    counts declarations

let parse path source =
  match S.parse source with
  | Ok forms -> forms
  | Error diagnostic ->
      prerr_endline (Diagnostic.report ~file:path ~source diagnostic);
      exit 1

let self_check () =
  let declarations =
    declarations
      (parse "self-check"
         {|
    (workspace check
      (graph picture :context draw (ref data))
      (graph data :context value (ref geometry))
      (graph geometry :context sop (ref data))
      (graph clean :context draw (draw/circle [0 0 0] 1))
      (graph unknown :context draw (ref absent))
      (graph macro :context draw (view))
      (defmacro view [] `(ref geometry))
      (defn twice :context value [(x : float)] (* x 2))
      (defn lift :context sop [(g : geometry)] (sop/with_attr g "y" (sop/attr g :P)))
      (graph default :context value [(n : int (+ 1 2))] n))
  |})
  in
  let graph name = List.find (fun d -> d.name = name) declarations in
  assert (blockers declarations (graph "picture") = [ "geometry:sop" ]);
  assert (blockers declarations (graph "clean") = []);
  assert (blockers declarations (graph "unknown") = [ "missing:absent" ]);
  assert (blockers declarations (graph "macro") = [ "geometry:sop" ]);
  let counts =
    List.fold_left
      (fun counts d -> List.fold_left heads counts d.expressions)
      Counts.empty declarations
  in
  assert (Counts.find "ref" counts = 5);
  assert (Counts.find "*" counts = 1);
  assert (Counts.find "+" counts = 1);
  assert (not (Counts.mem "x" counts));
  assert (core (graph "twice") && (not (core (graph "lift"))) && not (core (graph "geometry")));
  print_endline "core census self-check passed"

let () =
  match Array.to_list Sys.argv with
  | [ _; "--self-check" ] -> self_check ()
  | _ :: paths ->
      let paths = if paths = [] then [ "sketches"; "specification" ] else paths in
      let paths = List.concat_map files paths |> List.sort_uniq String.compare in
      let counts =
        List.fold_left
          (fun counts path ->
            let source = In_channel.with_open_bin path In_channel.input_all in
            census path (parse path source) counts)
          Counts.empty paths
      in
      Counts.iter (fun head count -> Printf.printf "head %s %d\n" head count) counts
  | [] -> assert false
