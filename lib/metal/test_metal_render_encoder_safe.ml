open Metal

let fail format = Printf.ksprintf failwith format

let get = function
  | Ok value -> value
  | Error error -> fail "%s" (Format.asprintf "%a" pp_error error)

let expect kind = function
  | Error error when error.kind = kind -> ()
  | Error error -> fail "expected another error kind: %s" (Format.asprintf "%a" pp_error error)
  | Ok _ -> fail "expected Metal operation to fail"

let depth_actions device =
  let pass = get (Render_pass_descriptor.create ~width:4 ~height:4 ()) in
  let color =
    get
      (Texture.create ~device
         (Texture.descriptor_2d ~storage:Buffer.Private ~usage:[ Texture.Render_target ]
            ~format:Texture.Bgra8_unorm ~width:4 ~height:4 ()))
  in
  let depth =
    get
      (Texture.create ~device
         (Texture.descriptor_2d ~storage:Buffer.Private ~usage:[ Texture.Render_target ]
            ~format:Texture.Depth32_float ~width:4 ~height:4 ()))
  in
  get (Render_pass_descriptor.set_attachments pass ~color ~depth ());
  get
    (Render_pass_descriptor.set_depth_stencil_actions pass
       ~depth:(Render_pass_descriptor.Load, Render_pass_descriptor.Store, 0.5)
       ~stencil:(Render_pass_descriptor.Clear, Render_pass_descriptor.Store_dont_care, 0));
  expect Invalid_argument
    (Render_pass_descriptor.set_depth_stencil_actions pass
       ~depth:(Render_pass_descriptor.Load, Render_pass_descriptor.Store, 1.5)
       ~stencil:(Render_pass_descriptor.Clear, Render_pass_descriptor.Store, 0));
  expect Invalid_argument
    (Render_pass_descriptor.set_depth_stencil_actions pass
       ~depth:(Render_pass_descriptor.Load, Render_pass_descriptor.Store, 0.)
       ~stencil:(Render_pass_descriptor.Clear, Render_pass_descriptor.Store, 256));
  get (Render_pass_descriptor.destroy pass);
  get (Texture.destroy depth);
  get (Texture.destroy color)

let run () =
  let device = get (Device.system_default ()) in
  depth_actions device;
  let queue = get (Command_queue.create device) in
  let target =
    get
      (Texture.create ~device
         (Texture.descriptor_2d ~storage:Buffer.Shared ~usage:[ Texture.Render_target ]
            ~format:Texture.Bgra8_unorm ~width:8 ~height:8 ()))
  in
  let commands = get (Command_buffer.create queue ()) in
  let encoder = get (Render_encoder.create commands ~target ()) in
  let sampled =
    get
      (Texture.create ~device
         (Texture.descriptor_2d ~storage:Buffer.Shared ~usage:[ Texture.Shader_read ]
            ~format:Texture.Rgba8_unorm ~width:1 ~height:1 ()))
  in
  (* A viewport maps clip space, so it may hang over the 8x8 target; an empty
     one, a non-finite one and a depth range outside 0..1 or reversed are refused. *)
  get
    (Render_encoder.set_viewport encoder
       { x = -2.; y = 0.; width = 11.; height = 8.; znear = 0.; zfar = 1. });
  List.iter
    (fun viewport -> expect Invalid_argument (Render_encoder.set_viewport encoder viewport))
    [
      { Render_encoder.x = 0.; y = 0.; width = 0.; height = 8.; znear = 0.; zfar = 1. };
      { x = 0.; y = 0.; width = 8.; height = -1.; znear = 0.; zfar = 1. };
      { x = nan; y = 0.; width = 8.; height = 8.; znear = 0.; zfar = 1. };
      { x = 0.; y = 0.; width = 8.; height = 8.; znear = 0.; zfar = 1.5 };
      { x = 0.; y = 0.; width = 8.; height = 8.; znear = 0.75; zfar = 0.25 };
    ];
  expect Invalid_argument
    (Render_encoder.set_scissor encoder { x = 7; y = 0; width = 2; height = 8 });
  expect Invalid_argument (Render_encoder.set_vertex_texture encoder ~index:31 sampled);
  expect Invalid_state
    (Render_encoder.draw_primitives encoder ~primitive:Render_encoder.Point ~first:0 ~count:1 ())
