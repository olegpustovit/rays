open Sdl3_ttf

let fail message = failwith ("SDL3_ttf discovery-failure test: " ^ message)

let run () =
  match Font.system_path ?font:(Sys.getenv_opt "RAYS_UI_FONT") () with
  | Error { kind = Font_not_found; message; _ } when message <> "" ->
      print_endline "SDL3_ttf typed system-font discovery failure passed"
  | Ok path -> fail ("invalid RAYS_UI_FONT resolved to " ^ path)
  | Error _ -> fail "invalid RAYS_UI_FONT returned the wrong error"
