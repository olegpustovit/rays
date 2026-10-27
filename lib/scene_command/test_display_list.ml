open Scene_command

let require condition message = if not condition then failwith message

let () =
  let encoded = Render_ir.create [|
      Clear 0x89abcdefl;
      Geometry { vertices = [|-0.; 1.|]; indices = [||]; color = 0x10203040l }
    |] |> Result.get_ok |> Render_ir.serialize in
  require (Bytes.to_string encoded =
    "\x31\x49\x32\x52\x02\x00\x00\x00\x00\x00\x00\x00\
     \x00\xef\xcd\xab\x89\
     \x05\x40\x30\x20\x10\x02\x00\x00\x00\x00\x00\x00\x00\
     \x00\x00\x00\x00\x00\x00\x00\x80\
     \x00\x00\x00\x00\x00\x00\xf0\x3f\
     \x00\x00\x00\x00\x00\x00\x00\x00")
    "render IR encoding must preserve the version, little-endian words and signed zero";
  let workers = Array.init 4 (fun _ ->
      Domain.spawn (fun () -> Array.init 4096 (fun _ -> Display_list.fresh_id ()))) in
  let ids = Array.concat (Array.to_list (Array.map Domain.join workers)) in
  Array.sort Int64.compare ids;
  Array.iteri (fun index id ->
      require (id = Int64.add ids.(0) (Int64.of_int index))
        "display-list identities must be unique and consecutive across domains") ids;
  (* Sixteen rectangles inside one clip are 18 commands: more than the
     builder's first 16 slots, so its storage doubles once to 32. *)
  let rectangle index = { Render_ir.vertices = [|float index; 2.; float index +. 3.; 2.;
      float index +. 3.; 6.; float index; 6.|];
    indices = [|0; 1; 2; 0; 2; 3|]; color = Int32.of_int index } in
  (* What a builder of [capacity] slots holding these geometries reports:
     an opcode byte, eight values, a colour and an integer a slot, and a word
     for each vertex coordinate and index. *)
  let word = Sys.word_size / 8 in
  let source_bytes ~capacity geometries =
    capacity + (capacity * 8 * word) + (capacity * 4) + (capacity * word)
    + List.fold_left (fun total (geometry : Render_ir.geometry) ->
        total + ((Array.length geometry.vertices + Array.length geometry.indices) * word))
      0 geometries in
  let builder = Display_list.Builder.create () in
  Display_list.Builder.push_clip builder ~x:0. ~y:0. ~width:64. ~height:64.;
  for index = 0 to 15 do Display_list.Builder.geometry builder (rectangle index) done;
  Display_list.Builder.pop_clip builder;
  let first = Result.get_ok (Display_list.Builder.publish builder ~id:7L ~version:1L)
  and same = Result.get_ok (Display_list.Builder.publish builder ~id:7L ~version:1L) in
  require (first == same && Display_list.id first = 7L
    && Display_list.version first = 1L)
    "display-list stable publication identity drift";
  require (Render_ir.Private.identity (Display_list.render_ir first) =
    Render_ir.Private.identity (Display_list.render_ir same))
    "unchanged display-list IR lost its identity";
  let commands = Render_ir.Private.commands_readonly (Display_list.render_ir first) in
  require (Array.length commands = 18)
    "display-list command cardinality drift";
  (match commands.(0), commands.(17) with
   | Render_ir.Push_clip { x = 0.; y = 0.; width = 64.; height = 64. }, Render_ir.Pop_clip -> ()
   | _ -> failwith "display-list command order drift");
  for index = 0 to 15 do
    match commands.(index + 1) with
    | Render_ir.Geometry geometry when geometry = rectangle index -> ()
    | _ -> failwith "display-list growth lost or reordered a command"
  done;
  require (Display_list.source_bytes first
           = source_bytes ~capacity:32 (List.init 16 rectangle))
    "display-list geometric growth drift";
  Display_list.Builder.reset builder;
  let reset = Result.get_ok
      (Display_list.Builder.publish builder ~id:7L ~version:1L) in
  require (reset != first
    && Array.length
         (Render_ir.Private.commands_readonly (Display_list.render_ir reset)) = 0)
    "display-list reset reused a stale publication";
  require (Render_ir.Private.identity (Display_list.render_ir reset) <>
    Render_ir.Private.identity (Display_list.render_ir first))
    "changed display-list IR reused its old identity";
  require (Display_list.source_bytes reset = source_bytes ~capacity:32 [])
    "display-list reset released capacity";
  Display_list.Builder.geometry builder (rectangle 3);
  let second = Result.get_ok
      (Display_list.Builder.publish builder ~id:7L ~version:2L) in
  require (second != first && second != reset && Display_list.version second = 2L
    && Render_ir.Private.commands_readonly (Display_list.render_ir second)
       = [|Render_ir.Geometry (rectangle 3)|]
    && Display_list.source_bytes second = source_bytes ~capacity:32 [rectangle 3])
    "display-list reset released capacity or failed to republish";
  let invalid = Display_list.Builder.create () in
  Display_list.Builder.pop_clip invalid;
  (match Display_list.Builder.publish invalid ~id:8L ~version:1L with
   | Error Render_ir.Unbalanced_clip -> ()
   | _ -> failwith "display-list accepted an unbalanced clip");

  let complete = Display_list.Builder.create () in
  let geometry = { Render_ir.vertices = [|0.; 0.; 2.; 0.; 0.; 2.|];
    indices = [|0; 1; 2|]; color = 0x01020304l } in
  Display_list.Builder.geometry complete geometry;
  Display_list.Builder.image complete ~resource_id:17
    ~source:{ x = 1.; y = 2.; width = 3.; height = 4. }
    ~destination:{ x = 5.; y = 6.; width = 7.; height = 8. };
  geometry.vertices.(0) <- 99.;
  let complete = Result.get_ok
      (Display_list.Builder.publish complete ~id:9L ~version:4L) in
  (match Array.to_list
      (Render_ir.Private.commands_readonly (Display_list.render_ir complete)) with
   | [ Geometry owned;
       Image { resource_id = 17;
         source = { x = 1.; y = 2.; width = 3.; height = 4. };
         destination = { x = 5.; y = 6.; width = 7.; height = 8. } } ]
       when owned.vertices.(0) = 0. -> ()
   | _ -> failwith "display-list complete command or ownership parity drift");

  print_endline "display list: packed growth, exact validation, stable publication"
