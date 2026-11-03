let () =
  let driver = Ogpu.Impl.create_driver () in
  let live_handles = Ogpu.Impl.live_handles in
  Ogpu_conformance.Conformance.run driver;
  (* the mock models a discrete device's release: a view kept past it reads NaN, as does the view
     made next until an invalidate fills it *)
  (let open Ogpu in
   let get = function Ok v -> v | Error e -> failwith (Error.to_string e) in
   let device = get (Backend.create_device driver) in
   let buffer =
     get (Backend.create_mapped_buffer device { label = None; size = 8L; usage = [ Storage ] })
   in
   let kept = Option.get (Backend.mapped_view buffer) in
   get (Backend.release_mapped_view buffer);
   if not (Float.is_nan kept.{0} && Float.is_nan (Option.get (Backend.mapped_view buffer)).{1})
   then failwith "a released mock view is still readable";
   get (Backend.destroy_buffer buffer);
   get (Backend.destroy_device device));
  if live_handles () <> 0 then failwith "mock backend leaked handles";
  print_endline
    "OGPU conformance (mock): capabilities, buffer round trip, texture upload and host read \
     (padded rows, inset origin), Rgba8/Rgba16_float/Rgba32_float level-0 round trips, typed \
     compute/render/ray-tracing rejection, instance record packing, abandoned commands, lifetime, \
     released mapped views, zero handles"
