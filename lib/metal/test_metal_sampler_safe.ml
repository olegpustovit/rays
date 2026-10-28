open Metal
open Test_metal_support

let run () =
  match Device.system_default () with
  | Error _ -> print_endline "Metal sampler safe: skipped (no device)"
  | Ok device ->
      let sampler_descriptor =
        {
          (Sampler.default ~label:"Metal linear sampler" ()) with
          min_filter = Sampler.Linear;
          mag_filter = Sampler.Linear;
          mip_filter = Sampler.Mip_linear;
          max_anisotropy = 4;
          s_address = Sampler.Repeat;
          t_address = Sampler.Repeat;
          r_address = Sampler.Repeat;
          lod_max_clamp = 3.;
          support_argument_buffers = true;
        }
      in
      finalizer_releases "sampler" ~count:1 (fun () ->
          ignore (get (Sampler.create ~device sampler_descriptor)));
      let sampler = get (Sampler.create ~device sampler_descriptor) in
      reject Parent_has_dependents (Device.destroy device);
      let plain = get (Sampler.create ~device (Sampler.default ())) in
      get (Sampler.destroy plain);
      (* Reduction modes and LOD bias are an Apple10 feature: made there and
         refused, before any allocation, everywhere else. *)
      List.iter
        (fun (reduction_mode, lod_bias) ->
          let advanced_descriptor =
            {
              sampler_descriptor with
              reduction_mode;
              lod_bias;
              label = Some "Metal reduction sampler";
            }
          in
          match Sampler.create ~device advanced_descriptor with
          | Ok advanced ->
              if not (get (Device.supports_family device Device.Apple10)) then
                fail "sampler reduction support escaped its Apple10 hardware gate";
              get (Sampler.destroy advanced)
          | Error { kind = Unsupported; _ } ->
              without_handles "unsupported sampler extensions" (fun () ->
                  reject Unsupported (Sampler.create ~device advanced_descriptor))
          | Error error -> fail "%s" (Format.asprintf "%a" pp_error error))
        [
          (Sampler.Minimum, -16.);
          (Sampler.Maximum, 15.999);
          (Sampler.Minimum, 0.5);
          (Sampler.Weighted_average, 0.5);
        ];
      without_handles "invalid sampler inputs" (fun () ->
          [
            { sampler_descriptor with max_anisotropy = 0 };
            { sampler_descriptor with max_anisotropy = 17 };
            { sampler_descriptor with lod_min_clamp = nan };
            { sampler_descriptor with lod_min_clamp = -1. };
            { sampler_descriptor with lod_bias = nan };
            { sampler_descriptor with lod_bias = -16.001 };
            { sampler_descriptor with lod_bias = 16. };
            { sampler_descriptor with normalized_coordinates = false; s_address = Sampler.Repeat };
            { sampler_descriptor with lod_min_clamp = 2.; lod_max_clamp = 1. };
            { sampler_descriptor with label = Some "invalid\000label" };
          ]
          |> List.iter (fun invalid -> reject Invalid_argument (Sampler.create ~device invalid)));
      without_handles "sampler label Metal refuses" (fun () ->
          reject Native_error
            (Sampler.create ~device { sampler_descriptor with label = Some "\255" }));
      (* Pixel coordinates are accepted with the one addressing they allow. *)
      let unnormalized =
        get (Sampler.create ~device { (Sampler.default ()) with normalized_coordinates = false })
      in
      get (Sampler.destroy unnormalized);
      get (Sampler.destroy sampler);
      get (Sampler.destroy sampler);
      get (Device.destroy device);
      reject Destroyed (Sampler.create ~device sampler_descriptor);
      ignore (settle_finalizers ~expected_live:0);
      print_endline "Metal sampler safe: descriptor validation, reduction gate and ownership passed"
