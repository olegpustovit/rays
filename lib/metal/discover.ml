let fail format = Printf.ksprintf failwith format

let check_sdk () =
  let input =
    Unix.open_process_args_in "/usr/bin/xcrun"
      [| "/usr/bin/xcrun"; "--sdk"; "macosx"; "--show-sdk-version" |]
  in
  let version = try input_line input with End_of_file -> "" in
  (match Unix.close_process_in input with
  | Unix.WEXITED 0 -> ()
  | _ -> fail "xcrun could not report the macOS SDK version");
  match Metal_build_config.check_sdk_version version with
  | Ok () -> ()
  | Error message -> fail "%s" message

let () =
  try
    match Array.to_list Sys.argv with
    | [ _; "--sanitizers"; sanitizers; "--output"; output ] ->
        check_sdk ();
        let sanitizers =
          match Metal_build_config.parse_sanitizers sanitizers with
          | Ok value -> value
          | Error message -> fail "%s" message
        in
        Metal_build_config.write_sexp output
          (Metal_build_config.framework_link_flags @ Metal_build_config.link_flags sanitizers)
    | _ -> fail "usage: discover --sanitizers LIST --output FILE"
  with Failure message ->
    prerr_endline message;
    exit 1
