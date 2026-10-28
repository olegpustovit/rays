open Runtime_resources
open Ogpu.Error

let get = function Ok x -> x | Error e -> failwith (Format.asprintf "%a" pp_error e)

let u16 b o v =
  Bytes.set_uint8 b o (v land 255);
  Bytes.set_uint8 b (o + 1) ((v lsr 8) land 255)

let u32 b o v =
  u16 b o (v land 65535);
  u16 b (o + 2) ((v lsr 16) land 65535)

let wav () =
  let n = 800 in
  let b = Bytes.make (44 + (n * 2)) '\000' in
  Bytes.blit_string "RIFF" 0 b 0 4;
  u32 b 4 (36 + (n * 2));
  Bytes.blit_string "WAVEfmt " 0 b 8 8;
  u32 b 16 16;
  u16 b 20 1;
  u16 b 22 1;
  u32 b 24 8000;
  u32 b 28 16000;
  u16 b 32 2;
  u16 b 34 16;
  Bytes.blit_string "data" 0 b 36 4;
  u32 b 40 (n * 2);
  for i = 0 to n - 1 do
    u16 b (44 + (i * 2)) ((if i land 8 = 0 then 8000 else -8000) land 65535)
  done;
  b

let run () =
  let owner = get (Audio.create_device ~max_channels:16) in
  let encoded = wav () in
  let sample = get (Audio.load_sample_bytes owner encoded) in
  Bytes.fill encoded 0 (Bytes.length encoded) '\000';
  (match Audio.load_sample_bytes owner (Bytes.of_string "bad") with
  | Error { kind = Decode; _ } -> ()
  | _ -> failwith "undecodable sample accepted");
  (* The sample owns its copy of the bytes: it still plays after the caller zeroed them. *)
  let channel = get (Audio.play_sample owner ~loops:1 ~volume:0.5 sample) in
  if channel < 0 || channel >= 16 then failwith "sample played outside the channel bank";
  List.iter
    (fun volume ->
      match Audio.play_sample owner ~volume sample with
      | Error { kind = Invalid_argument; _ } -> ()
      | _ -> failwith "volume outside 0..1 accepted")
    [ 1.5; -0.1; nan ];
  let second = get (Audio.play_sample owner sample) in
  if second = channel then failwith "a second voice took the playing channel";
  get (Audio.destroy_sample sample);
  get (Audio.destroy_sample sample);
  (match Audio.play_sample owner sample with
  | Error { kind = Destroyed; _ } -> ()
  | _ -> failwith "destroyed sample played");
  get (Audio.destroy owner);
  get (Audio.destroy owner);
  (match Audio.load_sample_bytes owner (wav ()) with
  | Error { kind = Destroyed; _ } -> ()
  | _ -> failwith "stale audio");
  print_endline "runtime_resources Audio: owned sample bytes, voices, volume range, teardown passed"
