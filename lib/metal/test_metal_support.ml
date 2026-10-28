(* Shared by the safe suites: typed expectations, native-handle accounting and
   the one way left to put bytes in a texture (a blit from a buffer). *)
open Metal

let fail format = Printf.ksprintf failwith format

let get = function
  | Ok value -> value
  | Error error -> fail "%s" (Format.asprintf "%a" pp_error error)

let expect kind = function
  | Error error when error.kind = kind -> error
  | Error error -> fail "expected a different error kind: %s" (Format.asprintf "%a" pp_error error)
  | Ok _ -> fail "operation unexpectedly succeeded"

let reject kind result = ignore (expect kind result)

let contains text pattern =
  let text_length = String.length text and pattern_length = String.length pattern in
  let rec search offset =
    offset <= text_length - pattern_length
    && (String.sub text offset pattern_length = pattern || search (offset + 1))
  in
  pattern_length = 0 || search 0

let stats () = get (Release_queue.stats ())

(* Every rejection in [operation] is decided before Metal is asked for an
   object: no handle is created and none stays live. *)
let without_handles name operation =
  ignore (get (Release_queue.drain ()));
  ignore (get (Release_queue.drain ()));
  let before = stats () in
  let result = operation () in
  let after = stats () in
  if after.total_created <> before.total_created || after.live_handles <> before.live_handles then
    fail "%s allocated a native handle (created %Ld -> %Ld, live %d -> %d)" name
      before.total_created after.total_created before.live_handles after.live_handles;
  result

let settle_finalizers ~expected_live =
  let rec loop remaining =
    Gc.full_major ();
    ignore (get (Release_queue.drain ()));
    let stats = stats () in
    if stats.pending = 0 && stats.live_handles = expected_live then stats
    else if remaining = 0 then
      fail "Metal finalizers did not settle (%d pending, %d live; expected %d)" stats.pending
        stats.live_handles expected_live
    else loop (remaining - 1)
  in
  loop 8

(* [allocate] makes [count] handles and drops them: collection releases
   exactly those. *)
let finalizer_releases name ~count allocate =
  let before = stats () in
  allocate ();
  let after = settle_finalizers ~expected_live:before.live_handles in
  if
    Int64.sub after.total_created before.total_created <> Int64.of_int count
    || Int64.sub after.total_released before.total_released <> Int64.of_int count
  then
    fail "%s finalization created %Ld and released %Ld handle(s), expected %d" name
      (Int64.sub after.total_created before.total_created)
      (Int64.sub after.total_released before.total_released)
      count

let complete commands =
  get (Command_buffer.commit commands);
  get (Command_buffer.wait_until_completed commands);
  (match get (Command_buffer.status commands) with
  | Command_buffer.Completed -> ()
  | _ -> fail "command buffer did not complete");
  get (Command_buffer.destroy commands)

let full_region ?(depth = 1) width height : Texture.region =
  { x = 0; y = 0; z = 0; width; height; depth }

(* Texture contents arrive through a staging buffer and a blit. *)
let upload ~device ~queue ?(level = 0) ?(slice = 0) ?bytes_per_image texture
    ~(region : Texture.region) ~bytes_per_row bytes =
  let staging =
    get
      (Buffer.create ~device ~length:(Int64.of_int (Bytes.length bytes)) ~storage:Buffer.Shared ())
  in
  get (Buffer.write_bytes staging ~dst_offset:0L bytes);
  let commands = get (Command_buffer.create queue ()) in
  let blit = get (Blit_encoder.create commands) in
  get
    (Blit_encoder.copy_buffer_to_texture blit ~source:staging ~source_offset:0L
       ~source_bytes_per_row:bytes_per_row
       ~source_bytes_per_image:(Option.value bytes_per_image ~default:(Bytes.length bytes))
       ~destination:texture ~destination_slice:slice ~destination_level:level
       ~destination_region:region);
  get (Blit_encoder.end_encoding blit);
  complete commands;
  get (Buffer.destroy staging)

let int32_bytes values =
  let bytes = Bytes.create (4 * Array.length values) in
  Array.iteri (fun index value -> Bytes.set_int32_le bytes (index * 4) value) values;
  bytes

let float_bytes values = int32_bytes (Array.map Int32.bits_of_float values)
