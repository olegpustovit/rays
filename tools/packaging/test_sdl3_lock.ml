(* packaging/sdl3.lock is the only place an SDL version is written. The opam
   probes cannot read it, so this test makes them agree with it, and keeps a
   literal patch release out of the files that used to carry one. *)

let fail format = Printf.ksprintf failwith format

let find_from text needle start =
  let n = String.length needle and length = String.length text in
  let rec go i =
    if i + n > length then None else if String.sub text i n = needle then Some i else go (i + 1)
  in
  go start

let contains text needle = find_from text needle 0 <> None

(* The value after [--atleast-version=] in an opam [build:] line. *)
let atleast_version path text =
  let marker = "--atleast-version=" in
  match find_from text marker 0 with
  | None -> fail "%s has no pkg-config --atleast-version probe" path
  | Some at ->
      let start = at + String.length marker in
      let stop =
        let rec go i = if i < String.length text && text.[i] <> '"' then go (i + 1) else i in
        go start
      in
      String.sub text start (stop - start)

(* Every [3.x.y] in [text]: a version written out. *)
let version_literals text =
  let length = String.length text in
  let digit i = i < length && text.[i] >= '0' && text.[i] <= '9' in
  let rec number i = if digit i then number (i + 1) else i in
  let rec scan i acc =
    if i >= length then List.rev acc
    else if text.[i] = '3' && (i = 0 || not (digit (i - 1) || text.[i - 1] = '.')) then begin
      let after_major = i + 1 in
      if after_major < length && text.[after_major] = '.' && digit (after_major + 1) then begin
        let minor_end = number (after_major + 1) in
        if minor_end < length && text.[minor_end] = '.' && digit (minor_end + 1) then
          let patch_end = number (minor_end + 1) in
          scan patch_end (String.sub text i (patch_end - i) :: acc)
        else scan (i + 1) acc
      end
      else scan (i + 1) acc
    end
    else scan (i + 1) acc
  in
  scan 0 []

let () =
  let root = ref "." in
  Arg.parse
    [ ("--root", Arg.Set_string root, "repository root") ]
    (fun value -> raise (Arg.Bad value))
    "test_sdl3_lock";
  let at path = Filename.concat !root path in
  let lock = Sdl3_lock.read (at "packaging/sdl3.lock") in
  let components =
    [
      ("sdl3", "packaging/conf-sdl3/conf-sdl3.opam");
      ("sdl3_image", "packaging/conf-sdl3-image/conf-sdl3-image.opam");
      ("sdl3_ttf", "packaging/conf-sdl3-ttf/conf-sdl3-ttf.opam");
      ("sdl3_mixer", "packaging/conf-sdl3-mixer/conf-sdl3-mixer.opam");
    ]
  in
  if List.length lock <> List.length components then
    fail "sdl3.lock lists %d components, expected %d" (List.length lock) (List.length components);
  List.iter
    (fun (key, opam) ->
      let { Sdl3_lock.floor; tested } = Sdl3_lock.find lock key in
      if compare tested floor < 0 then
        fail "%s: tested %s is older than the floor %s" key (Sdl3_lock.version_string tested)
          (Sdl3_lock.version_string floor);
      if not (Sdl3_lock.stable floor && Sdl3_lock.stable tested) then
        fail "%s: the lock names a development release" key;
      let text = Sdl3_lock.read_file (at opam) in
      let probe = atleast_version opam text in
      if probe <> Sdl3_lock.version_string floor then
        fail "%s probes --atleast-version=%s but the lock's floor is %s" opam probe
          (Sdl3_lock.version_string floor);
      if contains text "exact" then fail "%s calls the probe exact; the lock names a floor" opam;
      if version_literals text <> [ probe ] then
        fail "%s writes a version other than its --atleast-version probe" opam)
    components;
  (* bump.exe rewrites one line and nothing else *)
  let text = Sdl3_lock.read_file (at "packaging/sdl3.lock") in
  let bumped = Sdl3_lock.set_tested text ~key:"sdl3_ttf" (9, 8, 6) in
  let changed =
    List.filter
      (fun (a, b) -> a <> b)
      (List.combine (String.split_on_char '\n' text) (String.split_on_char '\n' bumped))
  in
  (match changed with
  | [ (_, line) ] when contains line "(sdl3_ttf" && contains line "(tested 9.8.6)" -> ()
  | _ -> fail "set_tested did not change exactly the one tested field");
  let reread = Sdl3_lock.read_file (at "packaging/sdl3.lock") in
  if reread <> text then fail "lock changed while testing";
  print_endline "sdl3.lock agrees with the opam probes"
