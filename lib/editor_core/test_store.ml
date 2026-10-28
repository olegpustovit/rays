let get = function Ok value -> value | Error message -> failwith message

(* a symlinked file stays a link after a save, and the file it points at gets the text *)
let () =
  let root = Filename.temp_dir "rays-store-link" "" in
  let target = Filename.concat root "real.rays" and link = Filename.concat root "link.rays" in
  Fun.protect
    ~finally:(fun () ->
      Sys.remove link;
      Sys.remove target;
      Unix.rmdir root)
    (fun () ->
      get (Editor_core.Store.write_text ~filename:target "old");
      Unix.symlink target link;
      get (Editor_core.Store.write_text ~filename:link "new");
      assert ((Unix.lstat link).st_kind = Unix.S_LNK);
      assert (get (Editor_core.Store.read_text ~filename:target) = "new"))

let () =
  let root = Filename.temp_dir "rays-store" "" in
  let directory = Filename.concat root "nested" in
  let path = Filename.concat directory "settings.rays" in
  Fun.protect
    ~finally:(fun () ->
      Sys.remove path;
      Unix.rmdir directory;
      Unix.rmdir root)
    (fun () ->
      let values =
        Editor_core.Store.Settings.
          [
            ("animate", Bool true);
            ("density", Float 0.625);
            ("label", Text "hello\nworld");
            ("range", Pair (0.2, 0.8));
          ]
      in
      get (Editor_core.Store.Settings.save ~sketch:"test" path values);
      assert (get (Editor_core.Store.Settings.load ~sketch:"test" path) = values);
      assert (Result.is_error (Editor_core.Store.Settings.load ~sketch:"other" path));
      let text = get (Editor_core.Store.read_text ~filename:path) in
      assert (String.starts_with ~prefix:"(settings :sketch \"test\"" text);
      assert (
        Result.is_error
          (Editor_core.Store.Settings.save ~sketch:"test" path [ ("bad", Float infinity) ]));
      assert (get (Editor_core.Store.Settings.load ~sketch:"test" path) = values));
  let open Rays in
  let view3 = Easy_camera.create ~distance:9. ~fov_y:0.8 () in
  let loaded3, look =
    Editor_core.Store.Viewport.decode (Easy_camera.create ())
      (Editor_core.Store.Viewport.encode view3 ~look_through:true)
  in
  assert (look && abs_float (Easy_camera.distance loaded3 -. 9.) < 0.000001);
  assert (Easy_camera.fov_y loaded3 = Easy_camera.fov_y view3);
  print_endline "editor store: atomic s-expression round-trip"
