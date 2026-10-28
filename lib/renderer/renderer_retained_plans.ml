open Scene_command.Render_ir

type resource =
  | Image of Runtime_resources.Image.t
  | Text of Runtime_resources.Text.t
  | Canvas of Runtime_resources.Canvas.t

type resource_stamp =
  | Image_stamp of int * Runtime_resources.Image.t * int
  | Text_stamp of int * Runtime_resources.Text.t * int
  | Canvas_stamp of int * Runtime_resources.Canvas.t * int

type plan = {
  mutable plan_ir_id : int;
  plan_command_count : int;
  plan_source_bytes : int;
  plan_density : int;
  plan_extent : int * int * int * int;
  plan_resources : resource_stamp list;
  mutable plan_ir : Scene_command.Render_ir.t;
  plan_draws : Scene_execution.sampled_draw list;
}

type segment = {
  segment_version : int64;
  segment_density : int;
  segment_width : int;
  segment_height : int;
  segment_draws : Scene_execution.sampled_draw list;
}

module Int_table = Lru.Make (Int)
module Segment_table = Lru.Make (struct
  type t = int64
  let equal = Int64.equal
  let hash = Hashtbl.hash
end)

type t = {
  by_fingerprint : plan Int_table.t;
  by_ir : (int, plan) Hashtbl.t;
  segments : segment Segment_table.t;
}

let create () =
  let by_ir = Hashtbl.create 16 in
  let by_fingerprint =
    Int_table.create 16 ~byte_capacity:(64 * 1024 * 1024)
      ~release:(fun _ plan ->
        match Hashtbl.find_opt by_ir plan.plan_ir_id with
        | Some current when current == plan -> Hashtbl.remove by_ir plan.plan_ir_id
        | _ -> ())
  in
  { by_fingerprint; by_ir;
    segments = Segment_table.create 256 ~byte_capacity:(64 * 1024 * 1024) }

let find t fingerprint = Int_table.find t.by_fingerprint fingerprint
let find_ir t ir_id = Hashtbl.find_opt t.by_ir ir_id
let add t fingerprint plan =
  Int_table.add t.by_fingerprint ~bytes:plan.plan_source_bytes fingerprint plan;
  Hashtbl.replace t.by_ir plan.plan_ir_id plan

let rebind t plan ~ir_id ~ir =
  if plan.plan_ir_id <> ir_id then begin
    (match Hashtbl.find_opt t.by_ir plan.plan_ir_id with
     | Some current when current == plan -> Hashtbl.remove t.by_ir plan.plan_ir_id
     | _ -> ());
    plan.plan_ir_id <- ir_id;
    plan.plan_ir <- ir;
    Hashtbl.replace t.by_ir ir_id plan
  end

let find_segment t identity = Segment_table.find t.segments identity
let add_segment t identity ~bytes segment =
  Segment_table.add t.segments ~bytes identity segment
let segment_count t = Segment_table.length t.segments
let clear t =
  Int_table.clear t.by_fingerprint;
  Hashtbl.reset t.by_ir;
  Segment_table.clear t.segments

let plan_source_bytes commands =
  Array.fold_left (fun total -> function
    | Scene_command.Render_ir.Geometry geometry ->
        total + Array.length geometry.vertices * (Sys.word_size / 8)
        + Array.length geometry.indices * (Sys.word_size / 8)
    | Glyphs glyphs -> total + Array.length glyphs.glyphs * 24
    | Shapes shapes -> total + Bytes.length (Scene_command.Ui_batch.instances shapes)
    | _ -> total + 32) 0 commands

let same_resources left right =
  let rec loop left right = match left, right with
    | [], [] -> true
    | Text_stamp (id, a, generation) :: xs,
      Text_stamp (id', b, generation') :: ys ->
        id = id' && a == b && generation = generation' && loop xs ys
    | Image_stamp (id, a, generation) :: xs,
      Image_stamp (id', b, generation') :: ys ->
        id = id' && a == b && generation = generation' && loop xs ys
    | Canvas_stamp (id, a, generation) :: xs,
      Canvas_stamp (id', b, generation') :: ys ->
        id = id' && a == b && generation = generation' && loop xs ys
    | _ -> false
  in
  loop left right

let resource_stamps ~resolve commands =
  let ids = ref [] and missing = ref false in
  Array.iter (function
    | Scene_command.Render_ir.Image image -> ids := image.resource_id :: !ids
    | Glyphs glyphs -> ids := glyphs.resource_id :: !ids
    | _ -> ()) commands;
  let stamps =
    List.filter_map (fun id ->
      match resolve id with
      | None -> missing := true; None
      | Some (Image image) ->
          if Result.is_error (Runtime_resources.Image.gpu_snapshot image) then (
            missing := true; None)
          else Some (Image_stamp (id, image, Runtime_resources.Image.generation image))
      | Some (Text text) ->
          if Runtime_resources.Text.destroyed text then (missing := true; None)
          else Some (Text_stamp (id, text, Runtime_resources.Text.generation text))
      | Some (Canvas canvas) ->
          if Runtime_resources.Canvas.destroyed canvas then (missing := true; None)
          else Some (Canvas_stamp (id, canvas, Runtime_resources.Canvas.generation canvas)))
      (List.sort_uniq Int.compare !ids)
  in
  not !missing, stamps
