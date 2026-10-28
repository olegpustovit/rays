let fail format = Printf.ksprintf failwith format

let run tool arguments =
  let program = "/usr/bin/xcrun" in
  let arguments = tool :: arguments in
  let pid =
    Unix.create_process program
      (Array.of_list (program :: arguments))
      Unix.stdin Unix.stdout Unix.stderr
  in
  let _, status = Unix.waitpid [] pid in
  match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED code -> fail "%s exited with status %d" tool code
  | Unix.WSIGNALED signal -> fail "%s was killed by signal %d" tool signal
  | Unix.WSTOPPED signal -> fail "%s was stopped by signal %d" tool signal

type arguments = {
  sanitizers : string;
  profile : string;
  ocaml_include : string;
  source : string;
  object_file : string;
  archive : string;
  shared : string;
}

let parse () =
  let sanitizers = ref None in
  let profile = ref None in
  let ocaml_include = ref None in
  let source = ref None in
  let object_file = ref None in
  let archive = ref None in
  let shared = ref None in
  let set target value = target := Some value in
  let options =
    [
      ("--sanitizers", Arg.String (set sanitizers), "LIST sanitizer list");
      ("--profile", Arg.String (set profile), "NAME Dune build profile");
      ("--ocaml-include", Arg.String (set ocaml_include), "DIR OCaml include");
      ("--source", Arg.String (set source), "FILE Objective-C++ source");
      ("--object", Arg.String (set object_file), "FILE object output");
      ("--archive", Arg.String (set archive), "FILE archive output");
      ("--shared", Arg.String (set shared), "FILE shared-library output");
    ]
  in
  Arg.parse options (fun value -> fail "unexpected argument %S" value) "build_bridge [OPTIONS]";
  let required name = function Some value -> value | None -> fail "%s is required" name in
  {
    sanitizers = required "--sanitizers" !sanitizers;
    profile = required "--profile" !profile;
    ocaml_include = required "--ocaml-include" !ocaml_include;
    source = required "--source" !source;
    object_file = required "--object" !object_file;
    archive = required "--archive" !archive;
    shared = required "--shared" !shared;
  }

let main () =
  let arguments = parse () in
  let sanitizers =
    match Metal_build_config.parse_sanitizers arguments.sanitizers with
    | Ok value -> value
    | Error message -> fail "%s" message
  in
  let compile_flags = Metal_build_config.compile_flags sanitizers in
  let profile_flags = Metal_build_config.profile_compile_flags arguments.profile in
  let link_flags = Metal_build_config.link_flags sanitizers in
  run "clang++"
    ([
       "-x";
       "objective-c++";
       "-std=c++17";
       "-fobjc-arc";
       "-fblocks";
       "-mmacosx-version-min=14.0";
       "-Wall";
       "-Wextra";
       "-Werror";
       "-I";
       arguments.ocaml_include;
     ]
    @ profile_flags @ compile_flags
    @ [ "-c"; arguments.source; "-o"; arguments.object_file ]);
  run "ar" [ "rcs"; arguments.archive; arguments.object_file ];
  run "clang++"
    ([
       "-dynamiclib";
       "-undefined";
       "dynamic_lookup";
       "-mmacosx-version-min=14.0";
       arguments.object_file;
       "-framework";
       "Foundation";
       "-framework";
       "Metal";
       "-framework";
       "QuartzCore";
       "-framework";
       "CoreGraphics";
       "-framework";
       "IOSurface";
     ]
    @ link_flags @ [ "-o"; arguments.shared ])

let () =
  try main ()
  with Failure message ->
    prerr_endline message;
    exit 1
