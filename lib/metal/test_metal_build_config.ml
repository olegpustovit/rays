open Metal_build_config

let fail format = Printf.ksprintf failwith format
let get = function Ok value -> value | Error message -> fail "%s" message

let () =
  if get (parse_sanitizers "") <> [] then fail "empty sanitizer list changed";
  if get (parse_sanitizers "none") <> [] then fail "none sanitizer list changed";
  let combined = get (parse_sanitizers "undefined,address,address") in
  if combined <> [ Address; Undefined ] then fail "sanitizers were not normalized";
  let flags = compile_flags combined in
  if
    not
      (List.mem "-fsanitize=address,undefined" flags
      && List.mem "-fno-sanitize-recover=undefined" flags)
  then fail "combined sanitizer flags are incomplete";
  if profile_compile_flags "release" <> [ "-O3"; "-DNDEBUG" ] then
    fail "release bridge optimization flags changed";
  List.iter
    (fun version ->
      match check_sdk_version version with
      | Ok () -> ()
      | Error message -> fail "supported SDK %s was rejected: %s" version message)
    [ "26.0"; "26.0.1"; "26.5"; "27.0" ];
  List.iter
    (fun version ->
      match check_sdk_version version with
      | Error _ -> ()
      | Ok () -> fail "unsupported SDK %S was accepted" version)
    [ ""; "25.9"; "26"; "26.beta"; "26.0.bad"; "27.0junk" ];
  if not (List.exists (fun framework -> framework = "IOSurface") framework_link_flags) then
    fail "IOSurface framework link flag is missing";
  (match parse_sanitizers "thread,address" with
  | Error _ -> ()
  | Ok _ -> fail "incompatible sanitizer combination was accepted");
  (match parse_sanitizers "python" with
  | Error _ -> ()
  | Ok _ -> fail "unknown sanitizer was accepted");
  Printf.printf "Metal OCaml build configuration tests passed\n%!"
