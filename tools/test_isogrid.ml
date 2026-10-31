(* The sketch owns the algorithm. Exercise its actual definitions, without cooking the viewport.
   --gpu also qualifies the same maps on native Metal and reports dispatch/readback counts. *)
module S = Flow.Syntax
module E = Flow.Eval

let get = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let read path = In_channel.with_open_bin path In_channel.input_all

let checks =
  {rays|
(graph checks :context host
  (let* [side 3 full (array/int 27 1)
         hollow (edit-volume full side 17 "hollow")
         single (map (fn [(i : int)] (if (= i 0) 1 0)) (array/range 27 :type int))
         inverted (edit-volume single side 17 "invert")
         flipped (edit-volume single side 17 "flip")
         mirrored (edit-volume single side 17 "mirror-x")
         rotated (fold [v single] [i (range 4)] (edit-volume v side 17 "rotate-x"))
         carve-all (carve full side 17 4 1.0 0.0 0.0)
         carve-none (carve full side 17 4 1.0 1.0 0.0)
         maze (maze-links 2 17)
         center (map (fn [(i : int)] (if (= i 13) 1 0)) (array/range 27 :type int))
         plane (map (fn [(i : int)] (if (= (mod (int/div i 3) 3) 1) 1 0)) (array/range 27 :type int))]
    {:menger-count (= (array/sum (structure 9 17 0)) 400)
     :edge-solid (= (array/sum (edit-volume full 3 17 "edge")) 0)
     :edge-octant (= (array/sum (edit-volume center 3 17 "edge")) 7)
     :wire-plane (= (array/sum (edit-volume plane 3 17 "wire")) 0)
     :wire-isolated (= (array/sum (edit-volume center 3 17 "wire")) 1)
     :slice-y (= (array/sum (edit-volume (array/int 512 1) 8 17 "slice-y")) 384)
     :random-none (equal? (selected-edit full 3 17 1 0.0 32) full)
     :random-all (= (array/sum (selected-edit full 3 17 1 1.0 32)) 0)
     :reset (= (array/sum (command single 3 17 "reset")) 27)
     :maze-deterministic (equal? maze (maze-links 2 17))
     :maze-edges (and (>= (array/sum maze) 12) (<= (array/sum maze) 14))
     :outside (= (occupied full side [-1 0 0]) 0)
     :row-boundary (= (occupied full side [0 3 0]) 0)
     :hollow (= (array/sum hollow) 26)
     :hollow-center (= (array/nth hollow 13) 0)
     :invert (= (array/sum inverted) 26)
     :flip (= (array/nth flipped 26) 1)
     :mirror (= (array/sum mirrored) 2)
     :rotate (equal? rotated single)
     :carve-all (= (array/sum carve-all) 0)
     :carve-none (= (array/sum carve-none) 27)}))
(graph presets :context host
  (for [preset (recipe-book)]
    (let* [voxels (build-volume 12 17 preset.steps)]
      {:name preset.name :filled (array/sum voxels)
       :binary (= (array/sum (map (fn [(bit : int)] (if (or (= bit 0) (= bit 1)) 0 1)) voxels)) 0)})))
(graph bulk-carved :context host
  (array/sum (carve (array/int 8000 1) 20 17 8 0.6 0.4 0.0)))
(graph bulk :context host
  (let* [full (array/int 8000 1)
         hollow (edit-volume full 20 17 "hollow")]
    (array/sum hollow)))
|rays}

let () =
  let forms = get (S.parse (read Sys.argv.(1))) in
  let definitions =
    match forms with
    | [ { S.node = List (_ :: _ :: forms); _ } ] ->
        List.filter (fun form -> S.head form = Some "defn") forms
    | _ -> failwith "expected one workspace"
  in
  let source =
    S.make
      (S.List
         ((S.make (S.Sym "workspace") :: S.make (S.Sym "test_isogrid") :: definitions)
         @ get (S.parse checks)))
    |> fun form -> [ fst (S.renumber 0 form) ]
  in
  let workspace =
    match Flow.Workspace.check Flow.Catalog.{ version = 1; kinds = [] } source with
    | Some ws, ds when not (List.exists (fun (d : Diagnostic.t) -> d.severity = Error_severity) ds)
      ->
        ws
    | _, ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))
  in
  let verify evaluated =
    (match List.assoc "checks" evaluated.E.results with
    | E.Record fields ->
        List.iter
          (fun (name, value) -> if value <> E.Bool true then failwith ("isogrid: " ^ name))
          fields
    | _ -> assert false);
    (match List.assoc "presets" evaluated.results with
    | E.List presets ->
        assert (Array.length presets = 8);
        Array.iter
          (function
            | E.Record fields -> (
                assert (List.assoc "binary" fields = E.Bool true);
                match List.assoc "filled" fields with
                | E.Int count -> assert (count > 0 && count < 1728)
                | _ -> assert false)
            | _ -> assert false)
          presets
    | _ -> assert false);
    assert (List.assoc "bulk" evaluated.results = E.Int (8000 - (18 * 18 * 18)))
  in
  let cpu = get (Flow_ir.static workspace) in
  verify cpu;
  if Array.length Sys.argv > 2 && Sys.argv.(2) = "--gpu" then begin
    let gpu =
      match Renderer.acquire_gpu () with
      | Ok gpu -> gpu
      | Error e -> failwith (Format.asprintf "%a" Renderer.pp_error e)
    in
    Fun.protect
      ~finally:(fun () -> Renderer.release_gpu gpu)
      (fun () ->
        let host = Flow_gpu.Host.create ~clock:Unix.gettimeofday gpu in
        Fun.protect
          ~finally:(fun () -> Flow_gpu.Host.close host)
          (fun () ->
            let config =
              Flow_ir.Gpu.
                { backend = Some (Flow_gpu.Host.backend host); fast = true; policy = Qualification }
            in
            let dispatched = ref [] in
            let observe path residual =
              if
                List.mem path
                  [ Some [ "def:edit-cells"; "@result" ]; Some [ "def:carve"; "@result" ] ]
                && not (List.mem path !dispatched)
              then begin
                let term = (E.Residual.residual_view residual).term in
                match Flow_ir.Packed.compile residual term with
                | None -> failwith "bulk hollow did not compile"
                | Some packed ->
                    let live = Input.at_time 0. in
                    let inputs = get (Flow_ir.Packed.Host.prepare ~fast:true packed ~live) in
                    if inputs.count = 8000 then begin
                      let refusals = Flow_ir.Packed.gpu_refusals ~fused:true ~fast:true packed in
                      if refusals <> [] then
                        failwith (String.concat "\n" (List.map Diagnostic.to_string refusals));
                      let backend = Flow_gpu.Host.backend host in
                      let kernel = get (backend.prepare ~fast:true packed) in
                      let actual = get (kernel.readback (get (kernel.run inputs))) in
                      let expected = get (Flow_ir.Packed.force packed ~live) in
                      (match (actual, expected) with
                      | E.Float_array actual, E.Int_array expected ->
                          assert (actual = Array.map float_of_int expected)
                      | _ -> assert (actual = expected));
                      dispatched := path :: !dispatched
                    end
              end
            in
            let _, result = get (Flow_ir.qualify_workspace ~observe ~gpu:config workspace) in
            verify result;
            assert (List.length !dispatched = 2);
            let stats = Flow_gpu.Host.stats host in
            Printf.printf "GPU: %d pipelines, %d status reads, %d readback bytes\n"
              stats.pipeline_compilations stats.status_reads stats.readback_bytes))
  end;
  print_endline "isogrid checks passed"
