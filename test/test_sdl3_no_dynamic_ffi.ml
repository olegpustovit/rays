let fail format = Printf.ksprintf failwith format

let roots =
  [ "lib/sdl3"; "lib/sdl3_image"; "lib/sdl3_ttf"; "lib/sdl3_mixer" ]

let suffixes = [ ".ml"; ".mli" ]
let forbidden = [ "Ctypes"; "Foreign.foreign"; "foreign_value" ]

let has_suffix path = List.exists (Filename.check_suffix path) suffixes

let rec files path =
  if Sys.is_directory path then
    Sys.readdir path |> Array.to_list
    |> List.concat_map (fun name -> files (Filename.concat path name))
  else if has_suffix path then [ path ] else []

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let contains text needle =
  let text_length = String.length text and needle_length = String.length needle in
  let rec loop offset =
    offset + needle_length <= text_length
    && (String.sub text offset needle_length = needle || loop (offset + 1))
  in
  loop 0

let () =
  let root = Sys.getcwd () in
  let violations =
    roots
    |> List.concat_map (fun relative -> files (Filename.concat root relative))
    |> List.concat_map (fun path ->
         let source = read path in
         forbidden
         |> List.filter_map (fun token ->
              if contains source token then Some (path, token) else None))
  in
  match violations with
  | [] -> print_endline "SDL3 production bindings use no Ctypes Foreign calls"
  | values ->
      values
      |> List.map (fun (path, token) -> Printf.sprintf "%s contains %s" path token)
      |> String.concat "\n" |> fail "%s"
