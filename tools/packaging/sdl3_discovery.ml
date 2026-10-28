module C = Configurator.V1

type component = Core | Image | Ttf | Mixer
type link_mode = Dynamic | Static
type sanitizer = Address | Undefined

type details = {
  configurator_name : string;
  package : string;
  library : string;
  environment_prefix : string;
  inherits_core : bool;
}

let details = function
  | Core ->
      {
        configurator_name = "sdl3";
        package = "sdl3";
        library = "SDL3";
        environment_prefix = "RAYS_SDL3";
        inherits_core = false;
      }
  | Image ->
      {
        configurator_name = "sdl3_image";
        package = "sdl3-image";
        library = "SDL3_image";
        environment_prefix = "RAYS_SDL3_IMAGE";
        inherits_core = true;
      }
  | Ttf ->
      {
        configurator_name = "sdl3_ttf";
        package = "sdl3-ttf";
        library = "SDL3_ttf";
        environment_prefix = "RAYS_SDL3_TTF";
        inherits_core = true;
      }
  | Mixer ->
      {
        configurator_name = "sdl3_mixer";
        package = "sdl3-mixer";
        library = "SDL3_mixer";
        environment_prefix = "RAYS_SDL3_MIXER";
        inherits_core = true;
      }

let environment name =
  match Sys.getenv_opt name with
  | Some value when String.trim value <> "" -> Some (String.trim value)
  | Some _ | None -> None

let link_mode () =
  match environment "RAYS_SDL3_LINK_MODE" with
  | None | Some "dynamic" -> Dynamic
  | Some "static" -> Static
  | Some value -> C.die "RAYS_SDL3_LINK_MODE must be dynamic or static, not %S" value

let sanitizer_configuration () =
  let sanitizers =
    match environment "RAYS_SDL3_SANITIZERS" with
    | None | Some "none" -> []
    | Some value ->
        String.split_on_char ',' value |> List.map String.trim
        |> List.map (function
          | "address" -> Address
          | "undefined" -> Undefined
          | sanitizer ->
              C.die "RAYS_SDL3_SANITIZERS accepts address and undefined, not %S" sanitizer)
        |> List.sort_uniq compare
  in
  let names = List.map (function Address -> "address" | Undefined -> "undefined") sanitizers in
  match names with
  | [] -> ([], [])
  | _ ->
      let sanitizer_flag = "-fsanitize=" ^ String.concat "," names in
      let undefined_flags =
        if List.mem Undefined sanitizers then [ "-fno-sanitize-recover=undefined" ] else []
      in
      let compile_flags = sanitizer_flag :: "-fno-omit-frame-pointer" :: undefined_flags in
      let link_flags = sanitizer_flag :: undefined_flags in
      (compile_flags, link_flags)

let starts_with ~prefix value =
  let prefix_length = String.length prefix in
  String.length value >= prefix_length && String.sub value 0 prefix_length = prefix

let words value = C.Flags.extract_blank_separated_words value

let prepare_static_pkg_config config =
  let arguments =
    match Sys.getenv_opt "PKG_CONFIG_ARGN" with
    | Some value -> words value
    | None -> (
        if Option.is_some (Sys.getenv_opt "PKG_CONFIG") || Option.is_none (C.which config "pkgconf")
        then []
        else
          match C.ocaml_config_var config "target" with
          | Some target -> [ "--personality"; target ]
          | None -> [])
  in
  let arguments = if List.mem "--static" arguments then arguments else arguments @ [ "--static" ] in
  Unix.putenv "PKG_CONFIG_ARGN" (String.concat " " arguments)

let package_queries config mode details =
  (match mode with Dynamic -> () | Static -> prepare_static_pkg_config config);
  match C.Pkg_config.get config with
  | None -> (None, None)
  | Some pkg_config ->
      let package = C.Pkg_config.query pkg_config ~package:details.package in
      let core =
        if details.inherits_core then C.Pkg_config.query pkg_config ~package:"sdl3" else None
      in
      (package, core)

let existing_directory variable =
  match environment variable with
  | None -> None
  | Some path ->
      if not (Sys.file_exists path && Sys.is_directory path) then
        C.die "%s does not name a directory: %s" variable path;
      Some (Unix.realpath path)

let remove_inherited ~inherited flags =
  List.filter (fun flag -> not (List.mem flag inherited)) flags

let library_directories flags =
  List.filter_map
    (fun flag ->
      if starts_with ~prefix:"-L" flag && String.length flag > 2 then
        Some (String.sub flag 2 (String.length flag - 2))
      else None)
    flags

let archive_name library = "lib" ^ library ^ ".a"

let find_archive details ?override_directory flags =
  let directories =
    match override_directory with
    | Some directory -> [ directory ]
    | None -> library_directories flags
  in
  let candidates =
    List.map (fun directory -> Filename.concat directory (archive_name details.library)) directories
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> Unix.realpath path
  | None ->
      let searched =
        match candidates with
        | [] -> "no pkg-config -L directories"
        | _ -> String.concat ", " candidates
      in
      C.die "static %s discovery requires %s (%s)" details.package (archive_name details.library)
        searched

let replace_library_flag details archive flags =
  let target = "-l" ^ details.library in
  let replaced = ref false in
  let flags =
    List.map
      (fun flag ->
        if flag = target then (
          replaced := true;
          archive)
        else flag)
      flags
  in
  if !replaced then flags else archive :: flags

let component_cflags details package =
  let override = existing_directory (details.environment_prefix ^ "_INCLUDE_DIR") in
  match (override, package) with
  | None, Some package -> package.C.Pkg_config.cflags
  | None, None -> []
  | Some directory, None -> [ "-I" ^ directory ]
  | Some directory, Some package -> ("-I" ^ directory) :: package.C.Pkg_config.cflags

let dynamic_libraries details package core override_directory =
  match (override_directory, package) with
  | Some directory, _ -> [ "-L" ^ directory; "-l" ^ details.library ]
  | None, Some package ->
      let inherited = match core with Some package -> package.C.Pkg_config.libs | None -> [] in
      remove_inherited ~inherited package.C.Pkg_config.libs
  | None, None -> [ "-l" ^ details.library ]

let static_libraries details package core override_directory =
  let package =
    match package with
    | Some package -> package
    | None ->
        C.die "static %s discovery requires pkg-config metadata for transitive link flags"
          details.package
  in
  let archive = find_archive details ?override_directory package.C.Pkg_config.libs in
  let libraries = replace_library_flag details archive package.C.Pkg_config.libs in
  let inherited = match core with Some package -> package.C.Pkg_config.libs | None -> [] in
  remove_inherited ~inherited libraries

let platform_libraries config libraries =
  match C.ocaml_config_var config "system" with
  | Some "macosx" -> "-Wl,-no_warn_duplicate_libraries" :: libraries
  | _ -> libraries

(* The version pkg-config reports for the installed package, as the number
   [SDL_VERSIONNUM] produces, or 0 when it cannot say: explicit header
   directories may describe a different install than pkg-config does. The
   stubs include it, so a changed SDL rebuilds them, and compare it with the
   header they compile against. *)
let probed_version config details =
  match environment (details.environment_prefix ^ "_INCLUDE_DIR") with
  | Some _ -> 0
  | None -> (
      let program =
        match environment "PKG_CONFIG" with Some value -> value | None -> "pkg-config"
      in
      let result = C.Process.run config program [ "--modversion"; details.package ] in
      if result.C.Process.exit_code <> 0 then 0
      else
        match Sdl3_lock.parse_version result.C.Process.stdout with
        | version -> Sdl3_lock.version_number version
        | exception Failure _ -> 0)

let configure config component =
  let details = details component in
  let mode = link_mode () in
  let sanitizer_cflags, sanitizer_libraries = sanitizer_configuration () in
  let package, core = package_queries config mode details in
  let cflags = component_cflags details package @ sanitizer_cflags in
  let override_directory = existing_directory (details.environment_prefix ^ "_LIB_DIR") in
  let libraries =
    match mode with
    | Dynamic -> dynamic_libraries details package core override_directory
    | Static -> static_libraries details package core override_directory
  in
  let libraries = libraries @ sanitizer_libraries in
  C.Flags.write_sexp "c_flags.sexp" cflags;
  C.Flags.write_sexp "c_library_flags.sexp" (platform_libraries config libraries);
  C.Flags.write_lines "sdl3_probed.h"
    [ "#define RAYS_SDL3_PROBED_VERSION " ^ string_of_int (probed_version config details) ]

let main component =
  let details = details component in
  C.main ~name:details.configurator_name (fun config -> configure config component)
