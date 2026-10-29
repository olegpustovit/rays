(* Gesture echo: what an edit wrote, in the words of the text (the status strip prints it, as the
   carry's preview line does for a put).  An op with no short words has none, and the history
   label stands. *)
module E = Flow_graph.Flow_edit

let path node = String.concat "/" node
let shorten text = if String.length text <= 40 then text else String.sub text 0 37 ^ "..."

let key = function
  | E.Kw k | E.Field k -> Some (":" ^ k)
  | Pos i -> Some (Printf.sprintf "input %d" i)
  | Bv _ | Whole | Arm _ | Cell _ -> None

let words (op : E.op) =
  match op with
  | Set_arg { node; key = k; sub = []; value } ->
      Option.map
        (fun k -> Printf.sprintf "%s %s on %s" k (shorten (Flow.Lisp.flat value)) (path node))
        (key k)
  | Set_arg { node; key = k; sub; value } ->
      Option.map
        (fun k ->
          Printf.sprintf "%s part %s %s on %s" k
            (String.concat "." (List.map string_of_int sub))
            (shorten (Flow.Lisp.flat value))
            (path node))
        (key k)
  | Connect { node; key = Whole; src; _ } ->
      Some (Printf.sprintf "%s as the result of %s" src (path node))
  | Connect { node; key = k; src; _ } ->
      Option.map (fun k -> Printf.sprintf "%s %s on %s" k src (path node)) (key k)
  | Disconnect { node; key = k; _ } ->
      Some
        (Printf.sprintf "%s unwired on %s" (Option.value ~default:"an input" (key k)) (path node))
  | Add_node { scope; name; expr } ->
      Some (Printf.sprintf "%s %s in %s" name (shorten (Flow.Lisp.flat expr)) (path scope))
  | Delete_nodes { nodes } -> Some ("removed " ^ shorten (String.concat ", " (List.map path nodes)))
  | Rename { node; to_ } -> Some (Printf.sprintf "%s renamed %s" (path node) to_)
  | Toggle_bypass { node } -> Some (path node ^ " bypass toggled")
  | _ -> None

let batch ops =
  match List.filter_map words ops with
  | [] -> None
  | [ one ] -> Some one
  | first :: rest -> Some (Printf.sprintf "%s and %d more" first (List.length rest))
