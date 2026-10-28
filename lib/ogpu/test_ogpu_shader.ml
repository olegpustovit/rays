let fail message = raise (Failure message)
let ok = function Ok value -> value | Error error -> fail (Ogpu.Error.to_string error)

let rejected = function
  | Error (error : Ogpu.Error.t) when error.kind = Invalid_argument -> ()
  | _ -> fail "expected rejection"

let entry name stage : Ogpu.Shader.entry_point = { name; stage }

let binding group binding kind visibility : Ogpu.Shader.binding =
  { group; binding; kind; visibility }

let descriptor bytes : Ogpu.Shader.descriptor =
  {
    backend = "mock";
    label = Some "deterministic";
    bytes;
    entry_points = [ entry "main_vertex" Vertex; entry "main_fragment" Fragment ];
    bindings =
      [ binding 0 0 Uniform_buffer [ Vertex; Fragment ]; binding 0 1 Sampled_texture [ Fragment ] ];
  }

let run () =
  let source = Bytes.of_string "stable shader bytes" in
  let first = ok (Ogpu.Shader.create (descriptor source)) in
  Bytes.fill source 0 (Bytes.length source) 'x';
  if Ogpu.Shader.bytes first <> Bytes.of_string "stable shader bytes" then
    fail "artifact aliases input bytes";
  let copy = Ogpu.Shader.bytes first in
  Bytes.set copy 0 'x';
  if Bytes.get (Ogpu.Shader.bytes first) 0 = 'x' then fail "artifact snapshot is mutable";
  rejected (Ogpu.Shader.create { (descriptor (Bytes.of_string "x")) with backend = "future" });
  rejected (Ogpu.Shader.create { (descriptor Bytes.empty) with label = None });
  rejected (Ogpu.Shader.create { (descriptor (Bytes.of_string "x")) with label = Some "" });
  rejected
    (Ogpu.Shader.create
       {
         (descriptor (Bytes.of_string "x")) with
         entry_points = [ entry "same" Vertex; entry "same" Fragment ];
       });
  rejected
    (Ogpu.Shader.create
       {
         (descriptor (Bytes.of_string "x")) with
         bindings =
           [
             binding 1 2 Storage_buffer [ Compute ]; binding 1 2 Acceleration_structure [ Compute ];
           ];
       });
  (* Buffers, textures, and samplers are separate index spaces. *)
  ignore
    (ok
       (Ogpu.Shader.create
          {
            (descriptor (Bytes.of_string "x")) with
            bindings =
              [
                binding 1 2 Storage_buffer [ Compute ];
                binding 1 2 Sampler [ Compute ];
                binding 1 2 Storage_texture [ Compute ];
              ];
          }));
  rejected
    (Ogpu.Shader.create
       {
         (descriptor (Bytes.of_string "x")) with
         bindings = [ binding 0 0 Uniform_buffer [ Fragment; Vertex ] ];
       });
  print_endline "OGPU immutable deterministic shader artifact passed"
