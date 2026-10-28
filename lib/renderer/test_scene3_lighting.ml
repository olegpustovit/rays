let get = function
  | Ok value -> value
  | Error error -> failwith (Format.asprintf "%a" Renderer.pp_error error)

let create_window width height =
  Renderer.create
    {
      Renderer.default_configuration with
      logical_width = width;
      logical_height = height;
      drawable_width = width;
      drawable_height = height;
    }

let frames = [ 1; 2; 60; 600 ]

let uniform ~ambient ~diffuse ~global ~light =
  let values = Array.make 1364 0. in
  for matrix = 0 to 2 do
    for diagonal = 0 to 3 do
      values.((matrix * 16) + (diagonal * 5)) <- 1.
    done
  done;
  values.(50) <- 1.;
  let color offset (r, g, b, a) =
    values.(offset) <- r;
    values.(offset + 1) <- g;
    values.(offset + 2) <- b;
    values.(offset + 3) <- a
  in
  color 52 ambient;
  color 56 diffuse;
  color 60 (0., 0., 0., 1.);
  color 64 (0., 0., 0., 1.);
  values.(68) <- 16.;
  color 69 global;
  (match light with
  | None -> ()
  | Some (direction, color_value, intensity) ->
      values.(73) <- 1.;
      values.(85) <- direction.(0);
      values.(86) <- direction.(1);
      values.(87) <- direction.(2);
      color 88 color_value;
      values.(92) <- intensity);
  let bytes = Bytes.create 5456 in
  Array.iteri
    (fun index value -> Bytes.set_int32_le bytes (index * 4) (Int32.bits_of_float value))
    values;
  bytes

let run () =
  match create_window 4 4 with
  | Error _ -> print_endline "renderer Scene3 lighting: skipped"
  | Ok runtime ->
      let indices = Bytes.make 12 '\000' and vertices = Bytes.make (24 * 3) '\000' and attributes = Bytes.make (12 * 3) '\000' in
      Bytes.set_int32_le indices 4 1l;
      Bytes.set_int32_le indices 8 2l;
      List.iteri
        (fun index (x, y) ->
          let offset = index * 24 in
          Bytes.set_int32_le vertices offset (Int32.bits_of_float x);
          Bytes.set_int32_le vertices (offset + 4) (Int32.bits_of_float y);
          Bytes.set_int32_le vertices (offset + 20) (Int32.bits_of_float 1.);
          Bytes.set_int32_le attributes (index * 12) 0xffffffffl)
        [ (-1., -1.); (3., -1.); (-1., 3.) ];
      let mesh : Renderer.Scene_execution.mesh =
        {
          key = Host "runtime-scene3-light";
          vertices;
          vertex_count = 3;
          indices;
          index_count = 3;
          primitive = Ogpu.Render_pass.Triangle_list;
        }
      in
      let base : Renderer.Scene_execution.state =
        {
          viewport = (0, 0, 4, 4);
          scissor = (0, 0, 4, 4);
          cull = Ogpu.Render_pass.Cull_none;
          depth_compare = Ogpu.Render_pass.Always;
          depth_write = false;
          depth_load = Ogpu.Render_pass.Clear;
          depth_clear = 1.;
          transform_uniforms = None;
        }
      in
      let entry family state : Renderer.Scene_execution.sampled_draw =
        {
          family;
          blend = Ogpu.Pipeline.Replace;
          texture = None;
          auxiliary = None;
          vertex_attributes = Some ("test-attributes", attributes);
          samples = 1;
          draw = { mesh; state };
        }
      in
      let check name uniforms expected =
        let stable = ref None in
        List.iter
          (fun _frame ->
            let state = { base with transform_uniforms = Some uniforms } in
            ignore (get (Renderer.draw runtime [ entry Scene3 state ]));
            let bytes =
              get
                (Renderer.read_pixels runtime
                   ~bytes_per_row:((get (Runtime.presentation_facts (Renderer.Native.runtime runtime))).drawable_width * 4))
            in
            let actual =
              ( Char.code (Bytes.get bytes 0),
                Char.code (Bytes.get bytes 1),
                Char.code (Bytes.get bytes 2) )
            in
            let near (a, b, c) (x, y, z) =
              abs (a - x) <= 1 && abs (b - y) <= 1 && abs (c - z) <= 1
            in
            (if not (near actual expected) then
               let r, g, b = actual in
               failwith (Printf.sprintf "%s pixel %d,%d,%d" name r g b));
            let uploaded = (get (Renderer.stats runtime)).uploaded_bytes in
            match !stable with
            | None -> stable := Some uploaded
            | Some previous when previous = uploaded -> ()
            | Some _ -> failwith (Printf.sprintf "%s reuploaded" name))
          frames
      in
      check "ambient"
        (uniform ~ambient:(1., 0., 0., 1.) ~diffuse:(0., 0., 0., 1.) ~global:(1., 1., 1., 1.)
           ~light:None)
        (255, 0, 0);
      check "directional"
        (uniform ~ambient:(0., 0., 0., 1.) ~diffuse:(1., 1., 1., 1.) ~global:(0., 0., 0., 1.)
           ~light:(Some ([| 0.; 0.; -1. |], (0., 1., 0., 1.), 1.)))
        (0, 255, 0);
      let set bytes index value =
        Bytes.set_int32_le bytes (index * 4) (Int32.bits_of_float value)
      in
      let positional kind color =
        let bytes =
          uniform ~ambient:(0., 0., 0., 1.) ~diffuse:(1., 1., 1., 1.) ~global:(0., 0., 0., 1.)
            ~light:(Some ([| 0.; 0.; -1. |], color, 1.))
        in
        set bytes 84 kind;
        set bytes 85 0.;
        set bytes 86 0.;
        set bytes 87 1.;
        bytes
      in
      (* Positional lights depend on where pixel (0,0) lands: its centre is at
     world (x,y,0) with x=-1+1/width, which differs between 1x and Retina
     backing. Expected channels follow the shader for that centre with the
     light at (0,0,1), unit attenuation and normal +z (specular is black). *)
      let facts = get (Runtime.presentation_facts (Renderer.Native.runtime runtime)) in
      let x = -1. +. (1. /. float facts.drawable_width)
      and y = 1. -. (1. /. float facts.drawable_height) in
      let byte value = int_of_float (Float.round (255. *. Float.min 1. (Float.max 0. value))) in
      let inverse_length = 1. /. sqrt ((x *. x) +. (y *. y) +. 1.) in
      let area_diffuse =
        (* 2x2 samples over a unit square facing -z: u=(-1,0,0), w=(0,1,0). *)
        let wx = ref 0. and wy = ref 0. and wz = ref 0. and total = ref 0. in
        List.iter
          (fun (a, b) ->
            let dx = -.a -. x and dy = b -. y and dz = 1. in
            let distance = sqrt ((dx *. dx) +. (dy *. dy) +. (dz *. dz)) in
            let weight = dz /. distance /. 4. in
            wx := !wx +. (dx /. distance *. weight);
            wy := !wy +. (dy /. distance *. weight);
            wz := !wz +. (dz /. distance *. weight);
            total := !total +. weight)
          [ (-0.25, -0.25); (0.25, -0.25); (-0.25, 0.25); (0.25, 0.25) ];
        !wz /. sqrt ((!wx *. !wx) +. (!wy *. !wy) +. (!wz *. !wz)) *. !total
      in
      let point = positional 1. (1., 0., 0., 1.) in
      set point 93 1.;
      check "point" point (byte inverse_length, 0, 0);
      let spot = positional 2. (0., 0., 1., 1.) in
      set spot 93 0.;
      set spot 94 0.;
      set spot 95 (-1.);
      set spot 96 1.;
      set spot 97 0.5;
      set spot 98 1.;
      set spot 99 1.;
      check "spot" spot (0, 0, byte (inverse_length *. inverse_length));
      let area = positional 3. (1., 0., 0., 1.) in
      set area 93 0.;
      set area 94 0.;
      set area 95 (-1.);
      set area 96 1.;
      set area 97 1.;
      set area 98 4.;
      set area 99 1.;
      check "area" area (byte area_diffuse, 0, 0);
      let too_many =
        uniform ~ambient:(1., 1., 1., 1.) ~diffuse:(0., 0., 0., 1.) ~global:(1., 1., 1., 1.)
          ~light:None
      in
      set too_many 73 65.;
      let before_reject = (get (Renderer.stats runtime)).uploaded_bytes in
      (match
         Renderer.draw runtime [ entry Scene3 { base with transform_uniforms = Some too_many } ]
       with
      | Error { Ogpu.Error.kind = Ogpu.Error.Invalid_argument; _ }
        when (get (Renderer.stats runtime)).uploaded_bytes = before_reject ->
          ()
      | _ -> failwith "65-light uniforms were not rejected before upload");
      get (Renderer.destroy runtime);
      print_endline "runtime Scene3 lighting: ambient/directional frames1/2/60/600"
