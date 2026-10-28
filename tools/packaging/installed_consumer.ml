(* The program an installed Rays prefix must build and run: it is the
   source check_installed_consumer.exe copies into a project outside the
   checkout. It is also built in the tree, so the compiler and the dead-code
   tools see the version facts it reads. *)

let fail label printer error = failwith (Format.asprintf "%s: %a" label printer error)

let at_least ~label ~(compiled : Sdl3.version) ~(linked : Sdl3.version) =
  if
    compare
      (linked.major, linked.minor, linked.patch)
      (compiled.major, compiled.minor, compiled.patch)
    < 0
  then failwith (label ^ ": the linked library is older than its headers")

let () =
  (match Sdl3.check_version ~release:true () with
  | Ok () -> ()
  | Error error -> fail "SDL3" Sdl3.pp_error error);
  at_least ~label:"SDL3" ~compiled:Sdl3.compiled_version ~linked:(Sdl3.linked_version ());
  (match Sdl3_image.check_version ~release:true () with
  | Ok () -> ()
  | Error error -> fail "SDL3_image" Sdl3_image.pp_error error);
  at_least ~label:"SDL3_image" ~compiled:Sdl3_image.compiled_version
    ~linked:(Sdl3_image.linked_version ());
  (match Sdl3_ttf.check_version ~release:true () with
  | Ok () -> ()
  | Error error -> fail "SDL3_ttf" Sdl3_ttf.pp_error error);
  at_least ~label:"SDL3_ttf" ~compiled:Sdl3_ttf.compiled_version
    ~linked:(Sdl3_ttf.linked_version ());
  (match Sdl3_mixer.check_version ~release:true () with
  | Ok () -> ()
  | Error error -> fail "SDL3_mixer" Sdl3_mixer.pp_error error);
  at_least ~label:"SDL3_mixer" ~compiled:Sdl3_mixer.compiled_version
    ~linked:(Sdl3_mixer.linked_version ());
  print_endline "installed SDL3 consumer passed"
