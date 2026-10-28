open Metal

let get = function
  | Ok value -> value
  | Error error -> failwith (Format.asprintf "%a" pp_error error)

let reject kind = function
  | Error error when error.kind = kind -> ()
  | Error error -> failwith (Format.asprintf "%a" pp_error error)
  | Ok library ->
      ignore (Library.destroy library);
      failwith "expected Device library-constructor rejection"

let run () =
  match Device.system_default () with
  | Error _ -> print_endline "Device library: skipped (no Metal device)"
  | Ok device ->
      (* Source is the one library constructor left: empty or NUL-carrying
         text is refused before the compiler, text that does not compile after. *)
      reject Invalid_argument (Library.compile_source ~device "");
      reject Invalid_argument (Library.compile_source ~device "kernel void k() {}\000");
      reject Invalid_argument (Library.compile_source ~label:"a\000b" ~device "kernel void k() {}");
      reject Native_error (Library.compile_source ~device "this is not Metal");
      let library =
        get
          (Library.compile_source ~label:"source_library" ~device
             "#include <metal_stdlib>\n\
              using namespace metal; kernel void k(uint i [[thread_position_in_grid]]) {}")
      in
      (match Function.find ~library "missing" with
      | Error _ -> ()
      | Ok _ -> failwith "a function the library does not define was found");
      let function_ = get (Function.find ~library "k") in
      (match Library.destroy library with
      | Error { kind = Parent_has_dependents; _ } -> ()
      | _ -> failwith "a library with a live function was destroyed");
      get (Function.destroy function_);
      get (Library.destroy library);
      get (Device.destroy device);
      print_endline "Device library: source compile, rejection and ownership passed"
