type span = { start : int; finish : int }
type severity = Error_severity | Warning
type position = { line : int; col : int }
type trace = { node_id : int; label : string; operation : string }

type t = {
  code : string;
  severity : severity;
  position : position option;
  message : string;
  span : span option;
  cause : string option;
  hints : string list;
  trace : trace list;
}

exception Error of t

let error ?span ?position ?cause ?(hints = []) ?(trace = []) ~code message =
  { code; severity = Error_severity; position; message; span; cause; hints; trace }

let warning ?span ~code message =
  { code; severity = Warning; position = None; message; span; cause = None; hints = []; trace = [] }

let prepend_trace frame value = { value with trace = frame :: value.trace }

let to_string value =
  match value.position with
  | None -> value.code ^ ": " ^ value.message
  | Some position ->
      Printf.sprintf "%s at %d:%d: %s" value.code position.line position.col value.message

let error_to_string value =
  let path =
    match value.trace with
    | [] -> ""
    | trace -> String.concat "/" (List.map (fun frame -> frame.label) trace) ^ ": "
  in
  let cause = Option.fold ~none:"" ~some:(fun cause -> "\n  cause: " ^ cause) value.cause in
  let hints =
    match value.hints with [] -> "" | hints -> "\n  hint: " ^ String.concat "\n  hint: " hints
  in
  Printf.sprintf "%s[%s] %s%s%s" path value.code value.message cause hints

let report ~file ~source d =
  let line_of offset =
    let offset = min (max offset 0) (String.length source) in
    let rec go i line start =
      if i >= offset then (line, start)
      else if source.[i] = '\n' then go (i + 1) (line + 1) (i + 1)
      else go (i + 1) line start
    in
    go 0 1 0
  in
  let line, a, b =
    match (d.span, d.position) with
    | Some { start; finish }, _ ->
        let line, first = line_of start in
        let eol =
          try String.index_from source first '\n'
          with Not_found | Invalid_argument _ -> String.length source
        in
        (line, start - first, min (max finish (start + 1)) eol - first)
    | None, Some { line; col } -> (line, max 0 (col - 1), max 0 (col - 1))
    | None, None -> (1, 0, 0)
  in
  Printf.sprintf "File \"%s\", line %d, characters %d-%d:\n%s [%s]: %s" file line a b
    (match d.severity with Error_severity -> "Error" | Warning -> "Warning")
    d.code d.message

let position_of_offset source offset =
  let line = ref 1 and col = ref 1 in
  for i = 0 to min (String.length source) offset - 1 do
    if source.[i] = '\n' then (
      incr line;
      col := 1)
    else incr col
  done;
  { line = !line; col = !col }
