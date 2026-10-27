module E = Flow.Eval
let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let checked body =
  let forms = ok (Flow.Syntax.parse ("(workspace w (graph g :context draw " ^ body ^ "))")) in
  match Flow.Workspace.check {Flow.Check.version=1;kinds=[]} forms with
  | Some ws,[] -> ws | _,ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
let pictures = Core_fixtures.pictures
let dump () =
  let buffer = Buffer.create 4096 in
  let fmt = Format.formatter_of_buffer buffer in
  List.iteri (fun i body ->
    Format.fprintf fmt "picture %d: %s\n" i body;
    let evaluation = ok (E.static (checked body)) in
    (match Flow_ir.Core_draw.lower evaluation.plan (List.assoc "g" evaluation.results) with
     | Ok (program, root) ->
         Format.fprintf fmt "%a" Flow_ir.Core.pp program;
         Format.fprintf fmt "root %b\n" (Option.is_some root)
     | Error name -> Format.fprintf fmt "refused %s\n" name)) pictures;
  Format.pp_print_flush fmt ();
  Buffer.contents buffer
let () =
  let actual = dump () in
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--print" then print_string actual
  else if actual <> In_channel.with_open_bin "core_draw.expected" In_channel.input_all
  then failwith ("Core draw lowering changed:\n" ^ actual)
