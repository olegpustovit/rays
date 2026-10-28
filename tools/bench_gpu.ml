(* P5 fixed costs. Run alone after building: tools/bench_gpu.exe.
   This is a native compute measurement, never a substitute mock timing. *)
module B = Ogpu.Backend
module S = Ogpu.Shader

let get = function Ok value -> value | Error error -> failwith (Ogpu.Error.to_string error)
let now = Unix.gettimeofday

let median values =
  Array.sort Float.compare values;
  values.(Array.length values / 2)

let timed f =
  let start = now () in
  let value = f () in
  (value, now () -. start)

let source =
  {|#include <metal_stdlib>
using namespace metal;
float fade(float t) { return t*t*t*(t*(t*6.0f-15.0f)+10.0f); }
float blend(float a, float b, float t) { return a+t*(b-a); }
float gradient(int h, float x, float y, float z) {
  h &= 15;
  float u=h<8?x:y, v=h<4?y:((h==12||h==14)?x:z);
  return ((h&1)==0?u:-u)+((h&2)==0?v:-v);
}
float noise(float3 p, device const int* table) {
  float3 base=floor(p), q=p-base, f=float3(fade(q.x),fade(q.y),fade(q.z));
  int x=int(base.x)&255, y=int(base.y)&255, z=int(base.z)&255;
  int a=table[x]+y, b=table[x+1]+y;
  int aa=table[a]+z, ab=table[a+1]+z, ba=table[b]+z, bb=table[b+1]+z;
  float x00=blend(gradient(table[aa],q.x,q.y,q.z),gradient(table[ba],q.x-1,q.y,q.z),f.x);
  float x10=blend(gradient(table[ab],q.x,q.y-1,q.z),gradient(table[bb],q.x-1,q.y-1,q.z),f.x);
  float x01=blend(gradient(table[aa+1],q.x,q.y,q.z-1),gradient(table[ba+1],q.x-1,q.y,q.z-1),f.x);
  float x11=blend(gradient(table[ab+1],q.x,q.y-1,q.z-1),gradient(table[bb+1],q.x-1,q.y-1,q.z-1),f.x);
  return clamp((blend(blend(x00,x10,f.y),blend(x01,x11,f.y),f.z)+1.0f)*0.5f,0.0f,1.0f);
}
kernel void noise_map(device const float* positions [[buffer(0)]],
                      device const float* normals [[buffer(1)]],
                      device float* output [[buffer(2)]],
                      constant uint& count [[buffer(3)]],
                      device const int* table [[buffer(4)]],
                      uint i [[thread_position_in_grid]]) {
  if (i>=count) return;
  float3 p=float3(positions[i*3],positions[i*3+1],positions[i*3+2]);
  float3 n=float3(normals[i*3],normals[i*3+1],normals[i*3+2]);
  float3 r=p+n*(0.8f*noise(p*0.16f,table));
  output[i*3]=r.x; output[i*3+1]=r.y; output[i*3+2]=r.z;
}
|}

let interface =
  List.init 5 (fun binding ->
      S.
        {
          group = 0;
          binding;
          kind = (if binding = 3 then Uniform_buffer else Storage_buffer);
          visibility = [ Compute ];
        })

(* A distinct trailing comment per compile defeats the system shader cache, so
   the median is a cold source compile rather than a cache lookup. *)
let compile ?(salt = 0) device =
  let source = if salt = 0 then source else Printf.sprintf "%s// %d\n" source salt in
  let shader =
    get
      (S.of_source
         {
           backend = "metal";
           label = Some "P5-baseline";
           bytes = Bytes.of_string source;
           entry_points = [ { name = "noise_map"; stage = Compute } ];
           bindings = interface;
         })
  in
  let library = get (B.create_library device shader) in
  match B.create_compute_pipeline_from library ~entry:"noise_map" ~interface () with
  | Ok pipeline -> (library, pipeline)
  | Error error ->
      get (B.destroy_library library);
      get (Error error)

let destroy (library, pipeline) =
  get (B.destroy_pipeline pipeline);
  get (B.destroy_library library)

let with_buffer device length f =
  let buffer =
    get
      (B.create_buffer device
         {
           label = Some "P5-baseline";
           size = Int64.of_int length;
           usage = [ Storage; Copy_src; Copy_dst ];
         })
  in
  Fun.protect ~finally:(fun () -> get (B.destroy_buffer buffer)) (fun () -> f buffer)

(* Loops rather than closures: a per-element closure call boxes every float. *)
let pack into (values : float array) =
  for index = 0 to Array.length values - 1 do
    Bytes.set_int32_le into (index * 4) (Int32.bits_of_float (Array.unsafe_get values index))
  done

let unpack bytes width =
  let values = Array.make width 0. in
  for index = 0 to width - 1 do
    Array.unsafe_set values index (Int32.float_of_bits (Bytes.get_int32_le bytes (index * 4)))
  done;
  values

let permutation () =
  let shuffled, _ = Rays_math.Rand.shuffle (List.init 256 Fun.id) (Rays_math.Rand.seed 0) in
  let table = Array.of_list shuffled and bytes = Bytes.create (512 * 4) in
  for index = 0 to 511 do
    Bytes.set_int32_le bytes (index * 4) (Int32.of_int table.(index land 255))
  done;
  bytes

let measure gpu compile_s (_, pipeline) count =
  let device = Renderer.gpu_device gpu and queue = Renderer.gpu_queue gpu in
  let width = count * 3 and length = count * 12 in
  let positions =
    Array.init width (fun index ->
        if index mod 3 = 0 then float (index / 3 mod 1000) *. 0.1
        else if index mod 3 = 2 then float (index / 3000) *. 0.1
        else 0.)
  in
  let normals = Array.init width (fun index -> if index mod 3 = 1 then 1. else 0.) in
  let packed_positions = Bytes.create length and packed_normals = Bytes.create length in
  let uniforms = Bytes.create 4 in
  Bytes.set_int32_le uniforms 0 (Int32.of_int count);
  let table = permutation () in
  with_buffer device length (fun input_a ->
      with_buffer device length (fun input_b ->
          with_buffer device length (fun output ->
              with_buffer device (Bytes.length table) (fun noise ->
                  get (B.write_buffer noise ~offset:0L table);
                  List.iter
                    (fun readback ->
                      let packs = Array.make 7 0.
                      and writes = Array.make 7 0.
                      and dispatches = Array.make 7 0.
                      and gpu_seconds = Array.make 7 0.
                      and reads = Array.make 7 0.
                      and totals = Array.make 7 0. in
                      for repeat = 0 to 6 do
                        let started = now () in
                        let (), packing =
                          timed (fun () ->
                              pack packed_positions positions;
                              pack packed_normals normals)
                        in
                        packs.(repeat) <- packing;
                        let (), write =
                          timed (fun () ->
                              get (B.write_buffer input_a ~offset:0L packed_positions);
                              get (B.write_buffer input_b ~offset:0L packed_normals))
                        in
                        writes.(repeat) <- write;
                        let receipt, dispatch =
                          timed (fun () ->
                              let commands = get (B.begin_commands queue) in
                              let encoder = get (B.compute_encoder commands) in
                              get (B.set_pipeline encoder pipeline);
                              get (B.set_buffer encoder ~index:0 input_a);
                              get (B.set_buffer encoder ~index:1 input_b);
                              get (B.set_buffer encoder ~index:2 output);
                              get (B.set_bytes encoder ~index:3 uniforms);
                              get (B.set_buffer encoder ~index:4 noise);
                              get
                                (B.dispatch_threads encoder ~threads:(count, 1, 1)
                                   ~threadgroup:(256, 1, 1));
                              get (B.end_compute encoder);
                              let receipt = get (B.commit commands) in
                              get (B.complete_through queue receipt.epoch);
                              receipt)
                        in
                        dispatches.(repeat) <- dispatch;
                        gpu_seconds.(repeat) <-
                          Option.value (B.gpu_duration queue receipt) ~default:nan;
                        if readback then begin
                          let (), read =
                            timed (fun () ->
                                let values =
                                  unpack (get (B.read_buffer output ~offset:0L ~length)) width
                                in
                                assert (Array.for_all Float.is_finite values))
                          in
                          reads.(repeat) <- read
                        end;
                        totals.(repeat) <- now () -. started
                      done;
                      Printf.printf "%s,%d,%.9f,%.9f,%.9f,%.9f,%.9f,%.9f,%.9f\n%!"
                        (if readback then "noise_readback" else "noise_display")
                        count compile_s (median packs) (median writes) (median dispatches)
                        (median gpu_seconds) (median reads) (median totals))
                    [ true; false ]))))

let () =
  if Array.to_list Sys.argv = [ Sys.argv.(0); "--msl" ] then begin
    print_string source;
    exit 0
  end;
  match Renderer.acquire_gpu () with
  | Error error ->
      Format.eprintf "%a@." Renderer.pp_error error;
      exit 2
  | Ok gpu ->
      Fun.protect
        ~finally:(fun () -> Renderer.release_gpu gpu)
        (fun () ->
          if not (Ogpu.Caps.has (B.capabilities (Renderer.gpu_device gpu)) Compute_pipeline) then
            print_endline "SKIP: backend lacks Compute_pipeline"
          else begin
            let device = Renderer.gpu_device gpu in
            let compiles =
              Array.init 10 (fun salt ->
                  let pipeline, seconds = timed (fun () -> compile ~salt:(salt + 1) device) in
                  destroy pipeline;
                  seconds)
            in
            let compile_s = median compiles in
            let pipeline = compile device in
            Fun.protect
              ~finally:(fun () -> destroy pipeline)
              (fun () ->
                print_endline
                  "name,count,compile_s,pack_s,write_s,dispatch_s,gpu_s,readback_s,total_s";
                List.iter (measure gpu compile_s pipeline) [ 1024; 65536; 1_000_000 ])
          end)
