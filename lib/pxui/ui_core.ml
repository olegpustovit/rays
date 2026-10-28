open Rays

module Int_table = Hashtbl.Make (Int)
module Batch = Scene.Layer.Ui_batch

(* ---------------------------------------------------------------- keys *)

(* 63-bit FNV-1a; keys 0 and 1 are the open-addressing empty/tombstone marks. *)
let hash_range seed text first last =
  let hash = Rays_math.Hash.fnv_int_range ~seed:(seed lxor 0x2bf29ce484222325)
      text first last in
  let hash = hash lxor (hash lsr 29) in
  if hash = 0 || hash = 1 then hash + 2 else hash

(* Index of [marker] in [text], or [-1]; no closure or substring per call. *)
let rec marker_at text marker at offset =
  offset = String.length marker
  || (String.unsafe_get text (at + offset) = String.unsafe_get marker offset
      && marker_at text marker at (offset + 1))
let rec marker_from text marker index =
  if index + String.length marker > String.length text then -1
  else if marker_at text marker index 0 then index
  else marker_from text marker (index + 1)
let find_marker text marker =
  if String.contains text '#' then marker_from text marker 0 else -1

let key_of seed text =
  let index = find_marker text "###" in
  (* the id after ### replaces the label's hash, under the same parent: two panels that build
     the same widget keep separate state *)
  if index >= 0 then hash_range seed text (index + 3) (String.length text)
  else hash_range seed text 0 (String.length text)

let display text =
  let index = find_marker text "##" in
  if index >= 0 then String.sub text 0 index else text

(* ------------------------------------------------------ retained cache *)

module Table = struct
  (* [filled] counts the cells a probe stops or steps over (keys and tombstones), [live] the keys:
     a full table grows only when its keys need the room, else it is rebuilt without its tombstones *)
  type t = { mutable keys : int array; mutable slots : int array;
    mutable filled : int; mutable live : int }

  let create () = { keys = Array.make 256 0; slots = Array.make 256 0; filled = 0; live = 0 }

  let index table key =
    let mask = Array.length table.keys - 1 in
    let rec probe position =
      let found = Array.unsafe_get table.keys position in
      if found = key || found = 0 then position
      else probe ((position + 1) land mask) in
    probe (key land mask)

  let find table key =
    let position = index table key in
    if Array.unsafe_get table.keys position = key
    then Array.unsafe_get table.slots position else -1

  let rec add table key slot =
    if 10 * (table.filled + 1) > 7 * Array.length table.keys then begin
      let keys = table.keys and slots = table.slots in
      let capacity = if 10 * table.live > 3 * Array.length keys
        then 2 * Array.length keys else Array.length keys in
      table.keys <- Array.make capacity 0;
      table.slots <- Array.make capacity 0;
      table.filled <- 0; table.live <- 0;
      Array.iteri (fun position key ->
        if key <> 0 && key <> 1 then add table key slots.(position)) keys
    end;
    let position = index table key in
    if Array.unsafe_get table.keys position <> key then begin
      table.filled <- table.filled + 1; table.live <- table.live + 1
    end;
    table.keys.(position) <- key;
    table.slots.(position) <- slot

  let remove table key =
    let position = index table key in
    if table.keys.(position) = key then begin
      table.keys.(position) <- 1; table.live <- table.live - 1
    end
end

let grow values size default =
  if Array.length values >= size then values
  else begin
    let grown = Array.make (max size (2 * Array.length values)) default in
    Array.blit values 0 grown 0 (Array.length values); grown
  end

(* ------------------------------------------------------------- boxes *)

type size =
  | Px of float
  | Pct of float
  | Rel of (float -> float)
  | Grow
  | Fit

type axis = Row | Column
type flags = int

let none = 0
let clickable = 1
let focusable = 2
let scroll = 4
let clip = 8
let blocking = 16
(* Kit controls enter keyboard mode through Tab; pointer editing retains the
   existing host shortcut behavior. Text-entry rows also focus on a press. *)
let tab_only = 32
let tab_stop = focusable lor tab_only
(* a multiline editor keeps Tab for itself (two spaces) instead of moving the focus *)
let keep_tab = 64
(* a press on such a box leaves the keyboard focus where it is (a completion row under an editor) *)
let keep_focus = 128
(* a scrolling box whose thumb is drawn over its content (an inspector body): no gutter *)
let over_thumb = 4096
(* an ancestor captures a press that no box under it took (the topmost hit box is not clickable) *)
let capture_unclaimed = 8192
(* the control marks its own keyboard focus (a button's accent line, a field's accent underline):
   the engine strokes no ring round it *)
let focus_mark = 256
let tab_stop_marked = tab_stop lor focus_mark
let add = Stdlib.( + )
let hit_flags = clickable lor focusable lor scroll lor blocking lor capture_unclaimed

type rect = float * float * float * float

(* What a carry holds, and how it was picked up: by pressing a box and moving past the dead
   zone (the box's key), or by a key (no pointer capture). *)
type payload = { kind : string; value : string }
type origin = From_pointer of int | From_key
type drop = Hover of payload | Dropped of payload

(* Points a pressed pointer travels before a press is a drag: a carry starts, a click ends. *)
let drag_threshold = 4.

type paint = {
  owner : ui;
  builder : Batch.Builder.t;
  mutable clip_rect : rect;
}

and painter = paint -> rect -> unit

and ui = {
  mutable theme : Theme.t;
  mutable font_size : int;
  (* owned kit faces by size; [None] caches a failed load *)
  faces : Font.t option Int_table.t;
  kit_font : string option Lazy.t;  (* the kit face's file, found on first use *)
  ascent_ratio : float Lazy.t;
  (* retained, by slot *)
  table : Table.t;
  mutable slot_key : int array;
  mutable free : int list;
  mutable next_slot : int;
  mutable touched : int array;
  mutable rx : float array; mutable ry : float array;
  mutable rw : float array; mutable rh : float array;
  mutable scroll_y : float array;
  mutable scroll_raw : float array;
  mutable scroll_visual : float array;
  mutable scroll_event_time : float array;
  mutable scroll_frame_time : float array;
  (* a trackpad's coast or its bounce off an edge: 0 neither, 1 coast, 2 bounce; the release
     velocity (the bounce's amplitude), when it began and the offset it began at *)
  mutable scroll_mode : int array;
  mutable scroll_velocity : float array;
  mutable scroll_start : float array;
  mutable scroll_from : float array;
  mutable state_values : int array;
  mutable text_values : string option array;
  mutable press_time : float array;
  mutable press_x : float array; mutable press_y : float array;
  mutable press_count : int array;  (* consecutive left presses within 0.35 s and 5 points *)
  (* per-frame boxes *)
  mutable count : int;
  mutable b_key : int array; mutable b_slot : int array;
  mutable b_parent : int array; mutable b_first : int array;
  mutable b_last : int array; mutable b_next : int array;
  mutable b_flags : int array;
  mutable b_w : size array; mutable b_h : size array;
  mutable b_max_h : float array;
  mutable b_row : bool array;
  mutable b_padding : float array;
  mutable b_at_x : float array; mutable b_at_y : float array;
  mutable b_scroll_step : float array;
  mutable b_hit : (rect -> rect) option array;
  mutable b_painters : painter list array;
  mutable b_overlays : painter list array;
  (* layout results, in logical screen points *)
  mutable l_x : float array; mutable l_y : float array;
  mutable l_w : float array; mutable l_h : float array;
  mutable l_content : float array; mutable l_gutter : float array;
  (* build state *)
  mutable parents : int list;
  mutable overlays : int list;  (* popup boxes this frame, newest first *)
  mutable foreground : (int * int) list;  (* floating panels, below modal popups *)
  mutable seeds : int list;
  mutable building : bool;
  mutable frame_number : int;
  mutable density : int;
  mutable kit_row_height : int;
  mutable closed_section : int;  (* the last closed section header: the next one follows it closely *)
  mutable kit_window : bool;  (* inside a floating window's inspector: its row grid and head *)
  (* input *)
  mutable pointer : float * float;
  mutable hot : int;
  mutable hover_rest : (string * (float * float) * float * int) option;
  mutable active : int option;  (* Some 0: unconsumed background gesture *)
  mutable active_button : Input.mouse_button;
  mutable active_press : float * float;
  mutable active_keys : Input.key list;
  mutable focus : int;
  mutable keyboard_focus : bool;
  mutable composition : string;
  mutable previous_keys : Input.key list;
  mutable edit_focus : int;
  mutable edit_value : string;
  mutable edit_caret : int;
  mutable edit_anchor : int;
  mutable edit_scroll_x : float;
  (* the trackpad gesture in progress: the scroll box it began over (0: none), its last motion's
     own clock and the velocity of that motion in points per second *)
  mutable window_ground : Rays_math.Color.t option;  (* the title bar colour last given to the window *)
  mutable gesture_key : int;
  mutable gesture_time : float;
  mutable gesture_velocity : float;
  (* the focused text's undo and redo stacks (text, caret, anchor), and the caret after the
     last typed character so consecutive typing undoes as one step *)
  mutable edit_undo : (string * int * int) list;
  mutable edit_redo : (string * int * int) list;
  mutable edit_group : int;
  (* the undo and redo stacks of texts left behind (key, value, undo, redo), newest first: a text
     that comes back unchanged takes its own back *)
  mutable edit_shelf : (int * string * (string * int * int) list * (string * int * int) list) list;
  (* a double or triple click's selection unit, extended by the drag that follows it *)
  mutable edit_unit : (int * int * int) option;
  (* the x an Up or Down keeps across short lines (a point offset in the row; negative: none) *)
  mutable edit_column : float;
  mutable scrub_origin : (int * string) option;
  (* the carry: the payload in flight, and the one released this frame with the box that was
     hot at the release *)
  mutable payload : (payload * origin) option;
  mutable dropped : (payload * int) option;
  (* the box whose press was carried until the host cancelled: it does not carry again before
     the next press *)
  mutable carry_latch : int option;
  (* the undo or redo key the focused text declined this frame because its own stack was empty *)
  mutable passed_undo : [`Undo | `Redo] option;
  mutable requested_cursor : [`Horizontal_resize|`Vertical_resize|`Text] option;
  (* this frame's raw events and logical size, for modal dismissal *)
  mutable frame_events : Input.event list;
  mutable input_frame : Frame.t option;
  mutable routed_events : (int * Input.event * (float * float)) list;
  mutable cancelled : Input.mouse_button list;
  mutable modal_key : int option;
  (* popups attached to the modal one (a submenu): its events are theirs too *)
  mutable modal_extra : int list;
  mutable modal_in_frame : bool;
  mutable view_w : float;
  mutable view_h : float;
  (* last laid-out height per modal key, kept while the modal is closed *)
  modal_heights : (int, float * int) Hashtbl.t;
  signals : accumulator Int_table.t;
  (* previous frame's hit list, in paint order *)
  mutable hit_count : int;
  (* the hit entry [chain_key] and its ancestors, kept between questions about one key (the
     hovered box is asked about by every card of a canvas); [0]: not known *)
  mutable chain_key : int; mutable chain : int list;
  mutable hit_keys : int array; mutable hit_parent : int array;
  mutable hit_flags_of : int array;
  mutable hit_x : float array; mutable hit_y : float array;
  mutable hit_w : float array; mutable hit_h : float array;
  (* output *)
  batch_builder : Batch.Builder.t;
  mutable regions : Scene.t;
  mutable scene_layers : (int * Scene.t) list;
  mutable borrowed_images : (int*Image.t)list;
  mutable scene : Scene.t;
  atlas : atlas;
  mutable destroyed : bool;
}

and accumulator = {
  mutable pressed : bool;
  mutable subtree_press : int option;
  mutable released : bool;
  mutable clicked : bool;
  mutable double_clicked : bool;
  mutable clicks : int;
  mutable moved : bool;
  mutable drag_x : float;
  mutable drag_y : float;
  mutable press_point : float * float;
  mutable release_point : float * float;
  mutable button : Input.mouse_button option;
  mutable scroll_x : float;
  mutable scroll_y_steps : float;
  (* what moves a scroll box: an unphased wheel's steps, the fingers' points on a trackpad, and
     whether they touched or lifted this frame *)
  mutable scroll_wheel : float;
  mutable scroll_drag : float;
  mutable scroll_touch : bool;
  mutable scroll_lift : bool;
  mutable pinch : float;  (* product of this frame's pinch factors *)
  mutable keys : (Input.event * Input.key list) list;
  mutable press_keys : Input.key list;
}

(* ------------------------------------------------------ glyph atlas *)

and atlas = {
  width : int;
  mutable height : int;
  mutable pixels : bytes;
  glyphs : glyph Int_table.t;
  mutable shelf_x : int;
  mutable shelf_y : int;
  mutable shelf_h : int;
  mutable dirty : bool;
  mutable image : Image.t option;
  mutable fonts : (Font.t * int * int) list;
  mutable next_font : int;
}

and glyph = { gx : int; gy : int; gw : int; gh : int; advance : int }

type t = ui

let atlas_width = 1024
let atlas_max_height = 4096

let create_atlas () = {
  width = atlas_width; height = 256;
  pixels = Bytes.make (atlas_width * 256 * 4) '\000';
  glyphs = Int_table.create 512; shelf_x = 0; shelf_y = 0; shelf_h = 0;
  dirty = false; image = None; fonts = []; next_font = 1 }

let font_id atlas font =
  let generation = Font.Raster.generation font in
  match List.find_opt (fun (candidate, _, _) -> candidate == font) atlas.fonts with
  | Some (_, id, known) when known = generation -> id
  | Some (_, id, _) ->
      (* A mutated font (style, hinting) rasterizes differently: new glyph ids. *)
      Int_table.filter_map_inplace (fun key glyph ->
        if key lsr 26 = id then None else Some glyph) atlas.glyphs;
      atlas.fonts <- (font, id, generation)
        :: List.filter (fun (candidate, _, _) -> candidate != font) atlas.fonts;
      id
  | None ->
      let id = atlas.next_font in
      atlas.next_font <- add id 1;
      atlas.fonts <- (font, id, generation) :: atlas.fonts;
      id

let reset_atlas atlas =
  Int_table.reset atlas.glyphs;
  Bytes.fill atlas.pixels 0 (Bytes.length atlas.pixels) '\000';
  atlas.shelf_x <- 0; atlas.shelf_y <- 0; atlas.shelf_h <- 0;
  atlas.dirty <- true

(* ponytail: a single shelf-packed page that doubles in height up to 4096
   rows; when that fills, the atlas is cleared and glyphs re-rasterize. *)
let reserve atlas width height =
  let padded_w = add width 1 and padded_h = add height 1 in
  if padded_w > atlas.width then None else begin
    if atlas.shelf_x + padded_w > atlas.width then begin
      atlas.shelf_y <- add atlas.shelf_y atlas.shelf_h;
      atlas.shelf_x <- 0; atlas.shelf_h <- 0
    end;
    if atlas.shelf_y + padded_h > atlas.height then begin
      let height = ref atlas.height in
      while atlas.shelf_y + padded_h > !height && !height < atlas_max_height do
        height := 2 * !height done;
      if atlas.shelf_y + padded_h > !height then reset_atlas atlas
      else begin
        let pixels = Bytes.make (atlas.width * !height * 4) '\000' in
        Bytes.blit atlas.pixels 0 pixels 0 (Bytes.length atlas.pixels);
        atlas.pixels <- pixels; atlas.height <- !height
      end
    end;
    let x = atlas.shelf_x and y = atlas.shelf_y in
    atlas.shelf_x <- add x padded_w;
    atlas.shelf_h <- max atlas.shelf_h padded_h;
    Some (x, y)
  end

let glyph atlas font ~id ~density code =
  let key = ((id * 32 + density) lsl 21) lor code in
  match Int_table.find_opt atlas.glyphs key with
  | Some _ as found -> found
  | None ->
      match Font.Raster.glyph ~density font code with
      | Error _ -> None
      | Ok raster ->
          let glyph = if raster.glyph_width = 0 || raster.glyph_height = 0 then
              Some { gx = 0; gy = 0; gw = 0; gh = 0;
                advance = raster.glyph_advance }
            else match reserve atlas raster.glyph_width raster.glyph_height with
              | None -> None
              | Some (gx, gy) ->
                  for row = 0 to raster.glyph_height - 1 do
                    for column = 0 to raster.glyph_width - 1 do
                      let offset = (((add gy row) * atlas.width) + add gx column) * 4 in
                      Bytes.set_int32_le atlas.pixels offset 0x00ffffffl;
                      Bytes.set atlas.pixels (add offset 3)
                        (Bytes.get raster.glyph_alpha
                           ((row * raster.glyph_width) + column))
                    done
                  done;
                  atlas.dirty <- true;
                  Some { gx; gy; gw = raster.glyph_width; gh = raster.glyph_height;
                    advance = raster.glyph_advance } in
          Option.iter (Int_table.replace atlas.glyphs key) glyph;
          glyph

let publish_atlas atlas =
  if atlas.dirty then begin
    match Image.upload_rgba ?into:atlas.image ~width:atlas.width
        ~height:atlas.height ~rgba:atlas.pixels () with
    | Ok image -> atlas.image <- Some image; atlas.dirty <- false
    | Error _ -> ()
  end

(* The kit face: the caller's [font_path], else Pragmasevka found from the working
   directory or the executable upward. *)
let kit_font_path font_path = lazy (
  match font_path with
  | Some path -> Some path
  | None ->
      let relative = Filename.concat "assets"
          (Filename.concat "fonts" "Pragmasevka-Regular.ttf") in
      let rec upward directory =
        let candidate = Filename.concat directory relative in
        if Sys.file_exists candidate then Some candidate
        else let parent = Filename.dirname directory in
          if parent = directory then None else upward parent in
      match upward (Sys.getcwd ()) with
      | Some _ as found -> found
      | None -> upward (Filename.dirname (Sys.executable_name)))

let face ui size =
  let size = Option.value size ~default:ui.font_size in
  match Int_table.find_opt ui.faces size with
  | Some face -> face
  | None ->
      let loaded = match Lazy.force ui.kit_font with
        | Some path -> Font.load path size
          (* the sheets' renderer does not hint: unhinted glyphs have its weight and x-height *)
          |> Result.map (fun font -> ignore (Font.set_hinting font Font.None_hinting); font)
        | None -> Error (`Msg "no kit font") in
      let face = match loaded with
        | Ok font -> Some font
        | Error _ -> Result.to_option (Font.system ~size ()) in
      Int_table.replace ui.faces size face;
      face

(* The kit face's ascent as a fraction of its size, measured once on a large face.  SDL_ttf rounds
   a font's ascent up to a whole pixel; the sheets' renderer rounds it to the nearest.  Where the two
   differ (13 points at two pixels a point: 23.4 pixels, so 24 against 23) every glyph of the string
   sits a device pixel lower than the reference, see [ascent_shift]. *)
let ascent_ratio_of kit_font = lazy (match Lazy.force kit_font with
  | Some path ->
      (match Font.load path 4096 with
       | Ok font -> let ratio = float (Font.get_ascent font) /. 4096. in Font.destroy font; ratio
       | Error _ -> 0.)
  | None -> 0.)

(* the move, in points, that puts a glyph raster's baseline on the nearest pixel row of its
   ascent instead of the one above: 0 or one device pixel up *)
let ascent_shift ui ~size ~density =
  let ratio = Lazy.force ui.ascent_ratio in
  if ratio = 0. then 0. else
  let exact = ratio *. float size *. float density in
  (Float.round exact -. Float.ceil (exact -. 0.02)) /. float density

let iter_code_points text visit =
  let length = String.length text in
  let rec loop index =
    if index < length then begin
      let decoded = String.get_utf_8_uchar text index in
      visit (Uchar.to_int (Uchar.utf_decode_uchar decoded));
      loop (add index (Uchar.utf_decode_length decoded))
    end in
  loop 0

let text_width_px ui ?size text =
  match face ui size with
  | None -> 0
  | Some font ->
      let id = if text = "" then 0 else font_id ui.atlas font in
      let total = ref 0 in
      iter_code_points text (fun code ->
        match glyph ui.atlas font ~id ~density:ui.density code with
        | Some glyph -> total := add !total glyph.advance
        | None -> ());
      !total

(* the width of kit text in points: the one measure of the build and of painting *)
let text_width ui ?size text = float (text_width_px ui ?size text) /. float ui.density

(* ----------------------------------------------------------- create *)

let create ?(font_size = Theme.font_size) ?font_path () =
  if font_size <= 0 then invalid_arg "Ui.create: font_size must be positive";
  let capacity = 64 in
  let kit_font = kit_font_path font_path in
  { theme = Theme.default; font_size; faces = Int_table.create 4;
    kit_font; ascent_ratio = ascent_ratio_of kit_font; table = Table.create ();
    slot_key = Array.make capacity 0; free = []; next_slot = 0;
    touched = Array.make capacity (-1);
    rx = Array.make capacity 0.; ry = Array.make capacity 0.;
    rw = Array.make capacity 0.; rh = Array.make capacity 0.;
    scroll_y = Array.make capacity 0.;
    scroll_raw = Array.make capacity 0.; scroll_visual = Array.make capacity 0.;
    scroll_event_time = Array.make capacity Float.neg_infinity;
    scroll_frame_time = Array.make capacity 0.;
    scroll_mode = Array.make capacity 0; scroll_velocity = Array.make capacity 0.;
    scroll_start = Array.make capacity 0.; scroll_from = Array.make capacity 0.;
    state_values = Array.make capacity min_int;
    text_values = Array.make capacity None;
    press_time = Array.make capacity Float.neg_infinity;
    press_x = Array.make capacity 0.; press_y = Array.make capacity 0.;
    press_count = Array.make capacity 0;
    count = 0;
    b_key = Array.make capacity 0; b_slot = Array.make capacity 0;
    b_parent = Array.make capacity (-1); b_first = Array.make capacity (-1);
    b_last = Array.make capacity (-1); b_next = Array.make capacity (-1);
    b_flags = Array.make capacity 0;
    b_w = Array.make capacity Grow; b_h = Array.make capacity Grow;
    b_max_h = Array.make capacity Float.infinity;
    b_row = Array.make capacity false;
    b_padding = Array.make capacity 0.;
    b_at_x = Array.make capacity Float.nan; b_at_y = Array.make capacity Float.nan;
    b_scroll_step = Array.make capacity 0.;
    b_hit = Array.make capacity None;
    b_painters = Array.make capacity []; b_overlays = Array.make capacity [];
    l_x = Array.make capacity 0.; l_y = Array.make capacity 0.;
    l_w = Array.make capacity 0.; l_h = Array.make capacity 0.;
    l_content = Array.make capacity 0.; l_gutter = Array.make capacity 0.;
    parents = []; overlays = []; foreground = []; seeds = []; building = false; frame_number = 0; density = 1;
    kit_row_height = 24; closed_section = -1; kit_window = false;
    pointer = (Float.nan, Float.nan); hot = 0; hover_rest = None; active = None;
    active_button = Input.LeftButton; active_press = (0., 0.); focus = 0;
    keyboard_focus = false; composition = "";
    previous_keys = []; active_keys = [];
    edit_focus = 0; edit_value = ""; edit_caret = 0; edit_anchor = 0;
    edit_scroll_x = 0.;
    window_ground = None; gesture_key = 0; gesture_time = Float.nan; gesture_velocity = 0.;
    edit_undo = []; edit_redo = []; edit_group = -1; edit_shelf = []; edit_unit = None; edit_column = -1.;
    scrub_origin = None; payload = None; dropped = None; carry_latch = None;
    passed_undo = None;
    requested_cursor = None;
    frame_events = []; input_frame = None; routed_events = []; cancelled = [];
    modal_key = None; modal_extra = []; modal_in_frame = false;
    view_w = 0.; view_h = 0.; modal_heights = Hashtbl.create 4;
    signals = Int_table.create 16;
    hit_count = 0; chain_key = 0; chain = []; hit_keys = [||]; hit_parent = [||]; hit_flags_of = [||];
    hit_x = [||]; hit_y = [||]; hit_w = [||]; hit_h = [||];
    batch_builder = Batch.Builder.create ~capacity:1024 ();
    regions = []; scene_layers = []; borrowed_images=[]; scene = []; atlas = create_atlas (); destroyed = false }

let destroy ui =
  if not ui.destroyed then begin
    ui.destroyed <- true;
    Option.iter(fun image->ignore(Image.destroy image))ui.atlas.image;
    ui.atlas.image <- None;
    Int_table.iter (fun _ face -> Option.iter Font.destroy face) ui.faces;
    Int_table.reset ui.faces;
    ui.atlas.fonts <- [];
    ui.borrowed_images <- [];
    ui.scene <- []
  end

let theme ui = ui.theme
let set_theme ui theme = ui.theme <- theme
let font_size ui = ui.font_size
(* the kit text size and, with it, the row height (24 at 11 points); set between frames *)
let set_font_size ui size =
  if size <= 0 then invalid_arg "Ui.set_font_size: size must be positive";
  ui.font_size <- size; ui.kit_row_height <- max 24 (size + 11)
let scene ?under ui = match under with
  | None -> ui.scene
  | Some under -> List.concat_map (fun (key, layer) -> under key @ layer) ui.scene_layers
      @ List.rev ui.regions
let row_height ui = ui.kit_row_height
let view_size ui = ui.view_w, ui.view_h
let text_line_height ui = ui.font_size + (ui.font_size + 1) / 2

(* ----------------------------------------------------- retained slots *)

let ensure_slot_capacity ui size =
  if size > Array.length ui.slot_key then begin
    ui.slot_key <- grow ui.slot_key size 0;
    ui.touched <- grow ui.touched size (-1);
    ui.rx <- grow ui.rx size 0.; ui.ry <- grow ui.ry size 0.;
    ui.rw <- grow ui.rw size 0.; ui.rh <- grow ui.rh size 0.;
    ui.scroll_y <- grow ui.scroll_y size 0.;
    ui.scroll_raw <- grow ui.scroll_raw size 0.;
    ui.scroll_visual <- grow ui.scroll_visual size 0.;
    ui.scroll_event_time <- grow ui.scroll_event_time size Float.neg_infinity;
    ui.scroll_frame_time <- grow ui.scroll_frame_time size 0.;
    ui.scroll_mode <- grow ui.scroll_mode size 0;
    ui.scroll_velocity <- grow ui.scroll_velocity size 0.;
    ui.scroll_start <- grow ui.scroll_start size 0.;
    ui.scroll_from <- grow ui.scroll_from size 0.;
    ui.state_values <- grow ui.state_values size min_int;
    ui.text_values <- grow ui.text_values size None;
    ui.press_time <- grow ui.press_time size Float.neg_infinity;
    ui.press_x <- grow ui.press_x size 0.;
    ui.press_y <- grow ui.press_y size 0.;
    ui.press_count <- grow ui.press_count size 0
  end

let slot_of ui key =
  let slot = Table.find ui.table key in
  if slot >= 0 then slot
  else begin
    let slot = match ui.free with
      | slot :: rest -> ui.free <- rest; slot
      | [] -> let slot = ui.next_slot in ui.next_slot <- add slot 1; slot in
    ensure_slot_capacity ui (add slot 1);
    ui.slot_key.(slot) <- key;
    ui.touched.(slot) <- ui.frame_number;
    ui.rx.(slot) <- 0.; ui.ry.(slot) <- 0.; ui.rw.(slot) <- 0.; ui.rh.(slot) <- 0.;
    ui.scroll_y.(slot) <- 0.; ui.state_values.(slot) <- min_int;
    ui.scroll_raw.(slot) <- 0.; ui.scroll_visual.(slot) <- 0.;
    ui.scroll_event_time.(slot) <- Float.neg_infinity;
    ui.scroll_frame_time.(slot) <- 0.; ui.scroll_mode.(slot) <- 0;
    ui.text_values.(slot) <- None; ui.press_time.(slot) <- Float.neg_infinity;
    ui.press_count.(slot) <- 0;
    Table.add ui.table key slot;
    slot
  end

let prune ui =
  for slot = 0 to ui.next_slot - 1 do
    let key = ui.slot_key.(slot) in
    if key <> 0 && ui.touched.(slot) < ui.frame_number then begin
      Table.remove ui.table key;
      ui.slot_key.(slot) <- 0;
      ui.text_values.(slot) <- None;
      ui.free <- slot :: ui.free;
      if ui.focus = key then begin
        ui.focus <- 0; ui.composition <- ""; ui.edit_focus <- 0
      end;
      if ui.active = Some key then begin
        ui.cancelled <- ui.active_button :: ui.cancelled; ui.active <- None
      end;
      (* the box that holds a carry's pointer is gone: the carry goes with it *)
      (match ui.payload with
       | Some (_, From_pointer owner) when owner = key -> ui.payload <- None
       | _ -> ());
      if ui.hot = key then ui.hot <- 0
    end
  done

(* -------------------------------------------------------------- input *)

let contains (x, y, w, h) (px, py) = px >= x && py >= y && px < x +. w && py < y +. h
let command_modifiers keys = List.mem Input.Meta keys || List.mem Input.Ctrl keys

let accumulator ui key =
  match Int_table.find_opt ui.signals key with
  | Some value -> value
  | None ->
      let value = { pressed = false; subtree_press = None;
        released = false; clicked = false;
        double_clicked = false; clicks = 0; moved = false; drag_x = 0.; drag_y = 0.;
        press_point = (0., 0.); release_point = (0., 0.); button = None;
        scroll_x = 0.; scroll_y_steps = 0.; pinch = 1.; keys = [];
        scroll_wheel = 0.; scroll_drag = 0.; scroll_touch = false; scroll_lift = false;
        press_keys = if ui.active = Some key then ui.active_keys else [] } in
      Int_table.replace ui.signals key value;
      value

let hit_index ui key =
  let rec search index =
    if index < 0 then -1
    else if ui.hit_keys.(index) = key then index
    else search (index - 1) in
  search (ui.hit_count - 1)

let rec hit_ancestors ui key f =
  if key <> 0 then begin
    f key;
    let index = hit_index ui key in
    if index >= 0 then hit_ancestors ui ui.hit_parent.(index) f
  end

(* [key] is [ancestor] or under it in the hit list.  The chain of a key is walked once (each step
   scans the list) and kept until the list is rebuilt. *)
let hit_within ui key ancestor =
  key = ancestor || (key <> 0 && begin
    if ui.chain_key <> key then begin
      let chain = ref [] in
      hit_ancestors ui key (fun k -> chain := k :: !chain);
      ui.chain_key <- key; ui.chain <- !chain
    end;
    List.mem ancestor ui.chain end)

let hit_contains ui index point =
  contains (ui.hit_x.(index), ui.hit_y.(index), ui.hit_w.(index), ui.hit_h.(index))
    point

(* Topmost hit entry under [point]; [0] when none. *)
let topmost ui point =
  let rec search index =
    if index < 0 then 0
    else if hit_contains ui index point then ui.hit_keys.(index)
    else search (index - 1) in
  search (ui.hit_count - 1)

let root_at ui point roots =
  let found = ref None in
  hit_ancestors ui (topmost ui point) (fun key ->
    if !found = None && List.mem key roots then found := Some key);
  !found

let last_press_within ui (frame : Frame.t) roots =
  List.fold_left (fun last -> function
    | Input.Pointer_pressed (_, point) ->
        (match root_at ui point roots with Some _ as root -> root | None -> last)
    | _ -> last) None frame.input.events

let flags_of_key ui key =
  let index = hit_index ui key in
  if index < 0 then 0 else ui.hit_flags_of.(index)

(* The scroll box the wheel goes to at [point]: the topmost one, unless a blocking box lies over it
   that is not its own content (a field inside a scrolling panel lets the panel scroll; a menu over
   it does not). *)
let scroll_target ui point =
  let rec search blocker index =
    if index < 0 then 0
    else if not (hit_contains ui index point) then search blocker (index - 1)
    else if ui.hit_flags_of.(index) land scroll <> 0 then
      (if blocker = 0 || hit_within ui blocker ui.hit_keys.(index) then ui.hit_keys.(index) else 0)
    else if blocker = 0 && ui.hit_flags_of.(index) land blocking <> 0 then search ui.hit_keys.(index) (index - 1)
    else search blocker (index - 1) in
  search 0 (ui.hit_count - 1)

let traverse_focus ui ~shift =
  let eligible index = ui.hit_flags_of.(index) land focusable <> 0
    && Option.fold ~none:true ~some:(fun modal ->
      hit_within ui ui.hit_keys.(index) modal
      || List.exists (hit_within ui ui.hit_keys.(index)) ui.modal_extra) ui.modal_key in
  let current = hit_index ui ui.focus in
  let direction = if shift then -1 else 1 in
  let start = if current >= 0 then current
    else if direction < 0 then 0 else ui.hit_count - 1 in
  let rec seek distance =
    if distance > ui.hit_count then () else
      let index = (start + (direction * distance) + ui.hit_count) mod ui.hit_count in
      if eligible index then begin
        ui.focus <- ui.hit_keys.(index); ui.keyboard_focus <- true;
        ui.composition <- ""; ui.edit_focus <- 0
      end else seek (distance + 1) in
  seek 1

let route ui (frame : Frame.t) =
  Int_table.reset ui.signals;
  ui.requested_cursor <- None;
  ui.frame_events <- frame.input.events;
  ui.input_frame <- Some frame;
  ui.routed_events <- []; ui.cancelled <- [];
  ui.dropped <- None;
  (* a carry lives with the press that holds it: capture taken away since (a popup, a host
     dismissal) takes the payload too, so no later release puts it *)
  (match ui.payload with
   | Some (_, From_pointer owner) when ui.active <> Some owner -> ui.payload <- None
   | _ -> ());
  let released_carry = ref false in
  ui.passed_undo <- None;
  ui.modal_in_frame <- ui.modal_key <> None;
  let modifiers = ref (Input.keys_before ~previous:ui.previous_keys
    ~held:frame.input.keys frame.input.events) in
  ui.previous_keys <- frame.input.keys;
  ui.view_w <- float (fst frame.input.size); ui.view_h <- float (snd frame.input.size);
  let set_pointer point = ui.pointer <- point in
  (* a trackpad's motion also arrives as wheel steps: in such a frame those only feed
     [signal.scroll], and the gesture moves the scroll box *)
  let phased = List.exists (function
    | Input.Trackpad_scrolled { delta = (dx, dy); _ } -> dx <> 0. || dy <> 0.
    | _ -> false) frame.input.events in
  let lift () =
    if ui.gesture_key <> 0 then (accumulator ui ui.gesture_key).scroll_lift <- true;
    ui.gesture_key <- 0 in
  let modal_target target = match ui.modal_key with
    | Some modal when not (hit_within ui target modal
        || List.exists (hit_within ui target) ui.modal_extra) -> modal
    | _ -> target in
  let pointer_target point =
    match ui.active with Some target -> target | None -> modal_target (topmost ui point) in
  List.iteri (fun event_index (event : Input.event) ->
    modifiers := Input.keys_after !modifiers event;
    let command = command_modifiers !modifiers in
    (match event with
     | Input.Key_pressed Input.Tab when not command ->
         if flags_of_key ui ui.focus land keep_tab = 0 then
           traverse_focus ui ~shift:(List.mem Input.Shift !modifiers)
     | _ -> ());
    let owner, delta = match event with
      | Input.Pointer_moved ((x, y) as point) ->
          pointer_target point,
          (if Float.is_finite (fst ui.pointer) then x -. fst ui.pointer, y -. snd ui.pointer
           else 0., 0.)
      | Pointer_pressed (_, point) | Pointer_released (_, point) -> pointer_target point, (0., 0.)
      | Scrolled _ | Pinched _ ->
          let target = match ui.active with Some target -> target
            | None -> let scroll = scroll_target ui ui.pointer in
                modal_target (if scroll <> 0 then scroll else topmost ui ui.pointer) in
          target, (0., 0.)
      | Key_pressed Input.Tab when not command ->
          (if ui.focus = 0 then 0x2c1b3c6d else ui.focus), (0., 0.)
      | Key_pressed _ | Key_released _ | Text_input _ | Text_editing _ ->
          ui.focus, (0., 0.)
      | _ -> 0, (0., 0.) in
    ui.routed_events <- (owner, event, delta) :: ui.routed_events;
    match event with
    | Input.Pointer_moved point ->
        let px, py = ui.pointer in
        set_pointer point;
        Option.iter (fun active ->
          let value = accumulator ui active in
          let x, y = ui.pointer in
          if Float.is_finite px then begin
            value.drag_x <- value.drag_x +. (x -. px);
            value.drag_y <- value.drag_y +. (y -. py)
          end;
          value.moved <- true
        ) ui.active
    (* a carry started from a key: a left press is the put, on what is under it, and nothing
       else sees the press *)
    | Input.Pointer_pressed (Input.LeftButton, point)
      when (match ui.payload with Some (_, From_key) -> true | _ -> false) ->
        set_pointer point;
        (match ui.payload with
         | Some (payload, _) -> ui.dropped <- Some (payload, topmost ui point)
         | None -> ());
        ui.payload <- None;
        ui.routed_events <- (match ui.routed_events with
          | (_, event, delta) :: rest -> (-1, event, delta) :: rest | [] -> [])
    | Input.Pointer_pressed (button, point) ->
        set_pointer point;
        let target =
          if owner = 0 || flags_of_key ui owner land clickable <> 0 then owner
          else begin
            let found = ref None in
            hit_ancestors ui owner (fun key ->
              if !found = None && flags_of_key ui key land capture_unclaimed <> 0 then found := Some key);
            Option.value !found ~default:owner
          end in
        hit_ancestors ui target (fun key ->
          (accumulator ui key).subtree_press <- Some event_index);
        let flags = flags_of_key ui target in
        if button = Input.LeftButton then begin
          let focus = if flags land keep_focus <> 0 then ui.focus
            else if flags land focusable <> 0 && flags land tab_only = 0 then target else 0 in
          if focus <> ui.focus then begin
            ui.composition <- ""; ui.edit_focus <- 0
          end;
          ui.focus <- focus;
          ui.keyboard_focus <- false
        end;
        if (target = 0 || flags land (clickable lor capture_unclaimed) <> 0) && ui.active = None then begin
          ui.active <- Some target; ui.active_button <- button;
          ui.active_press <- ui.pointer;
          ui.active_keys <- !modifiers;
          let value = accumulator ui target in
          value.pressed <- true;
          value.press_point <- ui.pointer;
          value.press_keys <- !modifiers;
          value.button <- Some button;
          let slot = Table.find ui.table target in
          if slot >= 0 && button = Input.LeftButton then begin
            let x, y = ui.pointer in
            let dx = x -. ui.press_x.(slot) and dy = y -. ui.press_y.(slot) in
            (* the second press within 0.35 s and 5 points is a double click, the third a
               triple; the fourth starts over *)
            let count = if frame.input.t >= ui.press_time.(slot)
                && frame.input.t -. ui.press_time.(slot) <= 0.35
                && (dx *. dx) +. (dy *. dy) <= 25. && ui.press_count.(slot) < 3
              then ui.press_count.(slot) + 1 else 1 in
            ui.press_count.(slot) <- count;
            value.clicks <- count;
            value.double_clicked <- count = 2;
            ui.press_time.(slot) <- frame.input.t;
            ui.press_x.(slot) <- x; ui.press_y.(slot) <- y
          end
        end
    | Input.Pointer_released (button, point) ->
        set_pointer point;
        (match ui.active with
        | Some active when button = ui.active_button ->
          (match ui.payload with
           | Some (_, From_pointer owner) when owner = active && button = Input.LeftButton ->
               released_carry := true
           | _ -> ());
          let value = accumulator ui active in
          value.released <- true;
          value.release_point <- ui.pointer;
          value.press_point <- ui.active_press;
          value.press_keys <- ui.active_keys;
          value.button <- Some button;
          let index = hit_index ui active in
          if button = Input.LeftButton && index >= 0
             && hit_contains ui index ui.pointer
             && hit_contains ui index ui.active_press
          then value.clicked <- true;
          ui.active <- None
        | _ -> ())
    | Input.Scrolled (horizontal, vertical) ->
        let target = modal_target (scroll_target ui ui.pointer) in
        if target <> 0 then begin
          let value = accumulator ui target in
          value.scroll_x <- value.scroll_x +. horizontal;
          value.scroll_y_steps <- value.scroll_y_steps +. vertical;
          if not phased then value.scroll_wheel <- value.scroll_wheel +. vertical
        end
    (* The fingers on a trackpad: the gesture stays with the box it began over, and the velocity
       it is released with is its last motion over the time since the one before.  The system's
       own momentum is not used: the box coasts by itself ([apply_scroll]). *)
    | Input.Trackpad_scrolled { phase = Momentum; _ } -> ()
    | Input.Trackpad_scrolled { phase = Lifted; time; _ } ->
        (* fingers that rested before they lifted leave the content where it is *)
        if time -. ui.gesture_time > 0.1 then ui.gesture_velocity <- 0.;
        lift ()
    | Input.Trackpad_scrolled { delta = (_, vertical); phase; time } ->
        if phase = Touched || ui.gesture_key = 0 then begin
          ui.gesture_key <- modal_target (scroll_target ui ui.pointer);
          ui.gesture_time <- Float.nan; ui.gesture_velocity <- 0.
        end;
        ui.gesture_velocity <-
          if time > ui.gesture_time then -. vertical /. (time -. ui.gesture_time) else 0.;
        ui.gesture_time <- time;
        if ui.gesture_key <> 0 then begin
          let value = accumulator ui ui.gesture_key in
          if phase = Touched then begin
            value.scroll_touch <- true; value.scroll_lift <- false
          end;
          value.scroll_drag <- value.scroll_drag +. vertical
        end
    (* a trackpad pinch goes where the wheel goes *)
    | Input.Pinched scale ->
        let target = modal_target (scroll_target ui ui.pointer) in
        if target <> 0 then begin
          let value = accumulator ui target in
          value.pinch <- value.pinch *. scale
        end
    (* Cancellation ends capture without a release: no click, no commit. *)
    | Input.Pointer_cancelled button ->
        if button = ui.active_button then ui.active <- None;
        (match ui.payload with
         | Some (_, From_pointer _) when button = Input.LeftButton ->
             ui.payload <- None; released_carry := false
         | _ -> ())
    | Input.Focus_lost ->
        ui.gesture_velocity <- 0.; lift ();
        ui.payload <- None; released_carry := false;
        ui.active <- None; ui.focus <- 0; ui.hot <- 0; ui.composition <- "";
        ui.edit_focus <- 0; ui.keyboard_focus <- false
    | Input.Key_pressed _ | Input.Key_released _ | Input.Text_input _
    | Input.Text_editing _ ->
        if ui.focus <> 0 then begin
          let value = accumulator ui ui.focus in
          value.keys <- (event, !modifiers) :: value.keys;
          (match event with
           | Input.Key_pressed (Input.Enter | Space | KeyChar ' ')
               when not command -> value.clicked <- true
           | _ -> ());
          (match event with
           | Input.Text_editing { text; _ } -> ui.composition <- text
           | Input.Text_input _ -> ui.composition <- ""
           | Input.Key_pressed Input.Escape when flags_of_key ui ui.focus land tab_only <> 0 ->
               ui.focus <- 0; ui.keyboard_focus <- false; ui.edit_focus <- 0
           | _ -> ())
        end
    | _ -> ()) frame.input.events;
  ui.routed_events <- List.rev ui.routed_events;
  Int_table.iter (fun _ value -> value.keys <- List.rev value.keys) ui.signals;
  let mouse_x, mouse_y = frame.input.pointer in
  if not (Float.is_finite (fst ui.pointer)) then
    ui.pointer <- (mouse_x, mouse_y);
  ui.hot <- (if List.exists (function Input.Focus_lost -> true | _ -> false)
      frame.input.events then 0 else topmost ui ui.pointer);
  if ui.active = None then ui.carry_latch <- None;
  (* released: what the carry was put on is what was hot at the release *)
  if !released_carry then begin
    (match ui.payload with
     | Some (payload, _) -> ui.dropped <- Some (payload, ui.hot)
     | None -> ());
    ui.payload <- None
  end

let wants_pointer ui = ui.hot <> 0 || Option.fold ~none:false ~some:(( <> ) 0) ui.active
let cursor ui = ui.requested_cursor
let request_cursor ui shape = ui.requested_cursor <- Some shape
let text_input_focused ui = ui.focus <> 0
let passed_undo ui = ui.passed_undo

(* [label] cut to [limit] points wide with an ellipsis, on a character boundary.  [width] measures
   text as the sum of its characters, so one pass from the start measures each character once and
   stops at the first that does not fit: the cost follows what is shown, not the label's length. *)
let ellipsis ~width ~limit label =
  let length = String.length label and dots = width "…" in
  (* [cut]: the longest prefix that fits with the ellipsis after it; -1 when the ellipsis alone does not *)
  let rec fit index used cut =
    if index >= length then label else
    let next = index + Uchar.utf_decode_length (String.get_utf_8_uchar label index) in
    let used = used +. width (String.sub label index (next - index)) in
    if used > limit then (if cut < 0 then "" else String.sub label 0 cut ^ "…")
    else fit next used (if used +. dots <= limit then next else cut) in
  fit 0 0. (if dots <= limit then 0 else -1)
let key_pressed ui key =
  List.exists (function Input.Key_pressed k -> k = key | _ -> false) ui.frame_events
let unfocus ui = ui.focus <- 0; ui.composition <- ""; ui.edit_focus <- 0;
  ui.keyboard_focus <- false

let dismiss_popup ui =
  ui.modal_key <- None; ui.modal_extra <- [];
  unfocus ui;
  if ui.active <> None then begin
    ui.cancelled <- ui.active_button :: ui.cancelled; ui.active <- None
  end

(* Event ownership is recorded before release/cancellation clears capture.
   A viewport passes its exact root key: child widgets retain their events. *)
let input ?(owner = 0) ui =
  let frame = match ui.input_frame with Some frame -> frame
    | None -> invalid_arg "Ui.input: no frame has completed" in
  let accepts target = not ui.modal_in_frame && (target = 0 || target = owner) in
  let accepts_pointer target = accepts target && (owner = 0 || target = owner) in
  let events, delta, complete = List.fold_left (fun (events, (dx, dy), complete)
      (target, event, (mx, my)) -> match event with
    | Input.Focus_lost | Pointer_cancelled _ -> event :: events, (dx, dy), complete
    | _ when (match event with
        | Pointer_pressed _ | Pointer_released _ | Pointer_moved _ | Scrolled _ -> accepts_pointer target
        | _ -> accepts target) -> event :: events, (dx +. mx, dy +. my), complete
    | Pointer_released (button, _) ->
        Input.Pointer_cancelled button :: events, (dx, dy), false
    | _ -> events, (dx, dy), false) ([], (0., 0.), true) ui.routed_events in
  let held = Option.fold ~none:false ~some:accepts_pointer ui.active in
  let pointer = held || (ui.active = None && frame.input.buttons = [] && accepts_pointer ui.hot) in
  let cancelled = ui.cancelled @ (if ui.modal_in_frame then frame.input.buttons else []) in
  { frame with input = { frame.input with
      events = List.rev events @ List.map (fun button -> Input.Pointer_cancelled button) cancelled;
      keys = if ui.modal_in_frame || ui.focus <> 0 then [] else frame.input.keys;
      buttons = if held then frame.input.buttons else [] };
    mouse_delta = if not pointer then 0., 0.
      else if complete then frame.mouse_delta else delta }

(* -------------------------------------------------------------- boxes *)

type box = { index : int; box_key : int; box_slot : int }

let key box = box.box_key

let box_of_key ui key =
  let slot = Table.find ui.table key in
  if slot < 0 then None else Some { index = -1; box_key = key; box_slot = slot }

let ensure_box_capacity ui size =
  if size > Array.length ui.b_key then begin
    ui.b_key <- grow ui.b_key size 0; ui.b_slot <- grow ui.b_slot size 0;
    ui.b_parent <- grow ui.b_parent size (-1);
    ui.b_first <- grow ui.b_first size (-1);
    ui.b_last <- grow ui.b_last size (-1);
    ui.b_next <- grow ui.b_next size (-1);
    ui.b_flags <- grow ui.b_flags size 0;
    ui.b_w <- grow ui.b_w size Grow; ui.b_h <- grow ui.b_h size Grow;
    ui.b_max_h <- grow ui.b_max_h size Float.infinity;
    ui.b_row <- grow ui.b_row size false;
    ui.b_padding <- grow ui.b_padding size 0.;
    ui.b_at_x <- grow ui.b_at_x size Float.nan;
    ui.b_at_y <- grow ui.b_at_y size Float.nan;
    ui.b_scroll_step <- grow ui.b_scroll_step size 0.;
    ui.b_hit <- grow ui.b_hit size None;
    ui.b_painters <- grow ui.b_painters size [];
    ui.b_overlays <- grow ui.b_overlays size [];
    ui.l_x <- grow ui.l_x size 0.; ui.l_y <- grow ui.l_y size 0.;
    ui.l_w <- grow ui.l_w size 0.; ui.l_h <- grow ui.l_h size 0.;
    ui.l_content <- grow ui.l_content size 0.;
    ui.l_gutter <- grow ui.l_gutter size 0.
  end

let require_building ui =
  if not ui.building then invalid_arg "Ui: boxes must be built inside Ui.frame"

let current_parent ui = match ui.parents with parent :: _ -> parent | [] -> 0
let current_seed ui = match ui.seeds with seed :: _ -> seed | [] -> 0

(* Two boxes with one key in a frame get distinct, order-stable keys. *)
let unique_key ui key =
  let rec loop key attempt =
    let slot = Table.find ui.table key in
    if slot >= 0 && ui.touched.(slot) = ui.frame_number
    then loop (hash_range key "#dup" 0 4) (add attempt 1)
    else key in
  loop key 0

let append_box ui ~key ~parent =
  let index = ui.count in
  ensure_box_capacity ui (add index 1);
  ui.count <- add index 1;
  let slot = slot_of ui key in
  ui.touched.(slot) <- ui.frame_number;
  ui.b_key.(index) <- key; ui.b_slot.(index) <- slot;
  ui.b_parent.(index) <- parent; ui.b_first.(index) <- -1;
  ui.b_last.(index) <- -1; ui.b_next.(index) <- -1;
  if parent >= 0 then begin
    if ui.b_last.(parent) < 0 then ui.b_first.(parent) <- index
    else ui.b_next.(ui.b_last.(parent)) <- index;
    ui.b_last.(parent) <- index
  end;
  index

let set_box ui index ~flags ~w ~h ~max_h ~row ~padding ~at_x ~at_y
    ~scroll_step ~hit =
  ui.b_flags.(index) <- flags; ui.b_w.(index) <- w; ui.b_h.(index) <- h;
  ui.b_max_h.(index) <- max_h; ui.b_row.(index) <- row;
  ui.b_padding.(index) <- padding;
  ui.b_at_x.(index) <- at_x; ui.b_at_y.(index) <- at_y;
  ui.b_scroll_step.(index) <- scroll_step;
  ui.b_hit.(index) <- hit; ui.b_painters.(index) <- [];
  ui.b_overlays.(index) <- []

(* Integer-derived child keys: no label string per frame. *)
let int_key seed n =
  let hash = Rays_math.Hash.fnv_int_mix seed (n * 0x1e3779b97f4a7c15) in
  let hash = hash lxor (hash lsr 29) in
  if hash = 0 || hash = 1 then hash + 2 else hash

let box_keyed ui ?(flags = none) ?(w = Grow) ?(h = Fit) ?(max_h = Float.infinity)
    ?(axis = Column) ?(padding = 0.) ?at
    ?(scroll_step = 24.) ?hit key =
  require_building ui;
  let key = unique_key ui key in
  let index = append_box ui ~key ~parent:(current_parent ui) in
  let at_x, at_y = match at with Some (x, y) -> x, y | None -> Float.nan, Float.nan in
  set_box ui index ~flags ~w ~h ~max_h ~row:(axis = Row) ~padding ~at_x ~at_y
    ~scroll_step ~hit;
  { index; box_key = key; box_slot = ui.b_slot.(index) }

let box ui ?flags ?w ?h ?max_h ?axis ?padding ?at
    ?scroll_step ?hit label =
  require_building ui;
  box_keyed ui ?flags ?w ?h ?max_h ?axis ?padding ?at ?scroll_step ?hit (key_of (current_seed ui) label)

let set_at ui box ~at:(x, y) =
  require_building ui;
  ui.b_at_x.(box.index) <- x;
  ui.b_at_y.(box.index) <- y

let within ui box f =
  require_building ui;
  let parents = ui.parents and seeds = ui.seeds in
  ui.parents <- box.index :: parents;
  ui.seeds <- box.box_key :: seeds;
  Fun.protect ~finally:(fun () -> ui.parents <- parents; ui.seeds <- seeds) f

let scope ui label f =
  require_building ui;
  let seeds = ui.seeds in
  ui.seeds <- key_of (current_seed ui) label :: seeds;
  Fun.protect ~finally:(fun () -> ui.seeds <- seeds) f

let rect ui box = ui.rx.(box.box_slot), ui.ry.(box.box_slot),
  ui.rw.(box.box_slot), ui.rh.(box.box_slot)

type signal = {
  hovered : bool;
  pressed : bool;
  subtree_press : int option;
  held : bool;
  released : bool;
  clicked : bool;
  double_clicked : bool;
  clicks : int;
  dragging : bool;
  drag : float * float;
  pointer : float * float;
  press_point : float * float;
  release_point : float * float;
  button : Input.mouse_button option;
  scroll : float * float;
  pinch : float;
  keys : Input.event list;
}

let signal ui box =
  let key = box.box_key in
  let held = ui.active = Some key in
  match Int_table.find_opt ui.signals key with
  | None ->
      { hovered = ui.hot = key; pressed = false; subtree_press = None;
        held; released = false;
        clicked = false; double_clicked = false; clicks = 0; dragging = false;
        drag = (0., 0.); pointer = ui.pointer;
        press_point = (if held then ui.active_press
          else (ui.press_x.(box.box_slot), ui.press_y.(box.box_slot)));
        release_point = ui.pointer;
        button = (if held then Some ui.active_button else None);
        scroll = (0., 0.); pinch = 1.; keys = [] }
  | Some value ->
      { hovered = ui.hot = key; pressed = value.pressed;
        subtree_press = value.subtree_press; held;
        released = value.released; clicked = value.clicked;
        double_clicked = value.double_clicked; clicks = value.clicks;
        dragging = value.moved && (held || value.released);
        drag = (value.drag_x, value.drag_y); pointer = ui.pointer;
        press_point = (if value.pressed || value.released then value.press_point
          else if held then ui.active_press
          else (ui.press_x.(box.box_slot), ui.press_y.(box.box_slot)));
        release_point = value.release_point;
        button = (match value.button with
          | Some _ as button -> button
          | None -> if held then Some ui.active_button else None);
        scroll = (value.scroll_x, value.scroll_y_steps);
        pinch = value.pinch;
        keys = List.map fst value.keys }

let key_events ui box = match Int_table.find_opt ui.signals box.box_key with
  | None -> [] | Some value -> value.keys

let press_keys ui box = match Int_table.find_opt ui.signals box.box_key with
  | None -> if ui.active = Some box.box_key then ui.active_keys else []
  | Some value -> value.press_keys

let press_shift ui box = List.mem Input.Shift (press_keys ui box)

let focused ui box = ui.focus = box.box_key
let focus ui box =
  if ui.focus <> box.box_key then begin
    ui.composition <- ""; ui.edit_focus <- 0
  end;
  ui.focus <- box.box_key
let hovered_within ui box = hit_within ui ui.hot box.box_key

(* ------------------------------------------------------------- carry *)

let carrying ui = Option.map fst ui.payload

let carry ui ?from ~kind ~value () =
  match from with
  | None ->
      (match ui.payload with
       | Some (held, From_key) when held.kind = kind && held.value = value -> ()
       | _ -> ui.payload <- Some ({ kind; value }, From_key))
  | Some box ->
      if ui.payload = None && ui.active = Some box.box_key && ui.carry_latch <> ui.active
         && ui.active_button = Input.LeftButton then begin
        let px, py = ui.active_press and x, y = ui.pointer in
        if Float.hypot (x -. px) (y -. py) >= drag_threshold then
          ui.payload <- Some ({ kind; value }, From_pointer box.box_key)
      end

let cancel_carry ui =
  if ui.payload <> None then ui.carry_latch <- ui.active;
  ui.payload <- None

let drop_target ui box =
  match ui.dropped, ui.payload with
  | Some (payload, hot), _ ->
      if hot <> 0 && hit_within ui hot box.box_key then Some (Dropped payload) else None
  | None, Some (payload, _) ->
      if ui.hot <> 0 && hit_within ui ui.hot box.box_key then Some (Hover payload) else None
  | None, None -> None

let scroll_offset ui box = ui.scroll_y.(box.box_slot)
let scroll_position ui box =
  ui.scroll_y.(box.box_slot) +. ui.scroll_visual.(box.box_slot)
let set_scroll_offset ui box value =
  ui.scroll_y.(box.box_slot) <- value;
  ui.scroll_raw.(box.box_slot) <- 0.;
  ui.scroll_visual.(box.box_slot) <- 0.;
  ui.scroll_mode.(box.box_slot) <- 0

let state ui box ~default =
  let value = ui.state_values.(box.box_slot) in
  if value = min_int then default else value
let set_state ui box value = ui.state_values.(box.box_slot) <- value
let text_state ui box = ui.text_values.(box.box_slot)
let set_text_state ui box value = ui.text_values.(box.box_slot) <- value

let draw ui box painter =
  ui.b_painters.(box.index) <- painter :: ui.b_painters.(box.index)
let draw_over ui box painter =
  ui.b_overlays.(box.index) <- painter :: ui.b_overlays.(box.index)

let to_front ui ?(order = 0) box =
  require_building ui;
  if ui.b_parent.(box.index) <> 0 then invalid_arg "Ui.to_front: expected a root box";
  ui.foreground <- (order, box.index) :: ui.foreground

(* ------------------------------------------------------------- layout *)

let children ui index visit =
  let child = ref ui.b_first.(index) in
  while !child >= 0 do visit !child; child := ui.b_next.(!child) done

let flow ui index = Float.is_nan ui.b_at_x.(index)

(* Bottom-up: fixed and fit sizes. Children follow their parent in
   build order, so a reverse sweep sees every child first. *)
let intrinsic ui =
  for index = ui.count - 1 downto 0 do
    let padding = ui.b_padding.(index) and row = ui.b_row.(index) in
    let along = ref 0. and across = ref 0. in
    let next = ref ui.b_first.(index) in
    while !next >= 0 do
      let child = !next in
      if flow ui child then begin
        let cw = ui.l_w.(child) and ch = ui.l_h.(child) in
        if row then (along := !along +. cw; across := Float.max !across ch)
        else (along := !along +. ch; across := Float.max !across cw)
      end;
      next := ui.b_next.(child)
    done;
    let content_w = (if row then !along else !across) +. (2. *. padding)
    and content_h = (if row then !across else !along) +. (2. *. padding) in
    ui.l_content.(index) <- content_h;
    let resolve kind content = match kind with
      | Px value -> value
      | Fit -> content
      | Pct _ | Rel _ | Grow -> 0. in
    ui.l_w.(index) <- resolve ui.b_w.(index) content_w;
    ui.l_h.(index) <- Float.min ui.b_max_h.(index)
        (resolve ui.b_h.(index) content_h)
  done

(* Scrolling as NSScrollView does it, the constants fitted to a recording of one (scriptc-ui
   packages/appkit/src/scrolling-behavior.ts).  Past an edge [travel] points of input show as
   [stretch travel height]; left alone the overscroll decays with tau 0.084 s.  A trackpad's fingers
   move the content point for point and hold a stretch until they lift; lifted in motion, the box
   coasts on the release velocity decaying with tau 0.26 s, and reaching an edge it overshoots as
   the overdamped spring A(e^(-9.25t) - e^(-19t)) launched at 0.39 of the arrival velocity.  Every
   motion is a closed form of the time since it began, so the frame rate does not change it. *)
(* the fingers past an edge: the content follows at about half their travel at first (AppKit's
   rubber band), ever less as it nears the clip's height.  A wheel notch is not a pull: its
   travel past the edge counts for a tenth. *)
let scroll_stiffness = 0.55
let scroll_wheel_past = 0.05 /. 0.55
let scroll_spring_tau = 0.084
let scroll_coast_tau = 0.26
let scroll_bounce_a = 9.25
let scroll_bounce_b = 19.
let scroll_bounce_launch = 0.39
let scroll_min_velocity = 20.

let scroll_stretch travel height =
  if height <= 0. then 0.
  else scroll_stiffness *. travel /. (1. +. scroll_stiffness *. Float.abs travel /. height)

(* the travel that shows [overscroll]: the inverse of [scroll_stretch] *)
let scroll_travel overscroll height =
  if height <= 0. || overscroll = 0. then 0.
  else
    let shown = Float.min (Float.abs overscroll) (height *. 0.999) in
    Float.copy_sign (shown /. (scroll_stiffness *. (1. -. shown /. height))) overscroll

(* One scroll box, once its height is laid out ([arrange]): a height that comes from the parent
   (Grow, Pct, Rel) stretches and bounces as a fixed one does. *)
let apply_scroll ?(consume = false) ui time index =
  let slot = ui.b_slot.(index) in
  let height = ui.l_h.(index) in
  let max_scroll = Float.max 0. (ui.l_content.(index) -. height) in
  let wheel, drag, touch, lift = match Int_table.find_opt ui.signals ui.b_key.(index) with
    | Some value -> value.scroll_wheel, value.scroll_drag, value.scroll_touch, value.scroll_lift
    | None -> 0., 0., false, false in
  (* input moves the content by [delta] points; content that fits does not stretch *)
  let pull ?(past = 1.) delta =
    let old = ui.scroll_raw.(slot) in
    let next = ui.scroll_y.(slot) +. old -. delta in
    let bounded = Float.max 0. (Float.min max_scroll next) in
    let over = next -. bounded in
    let raw = if max_scroll <= 0. || over = 0. then 0. else old +. (over -. old) *. past in
    ui.scroll_y.(slot) <- bounded;
    ui.scroll_raw.(slot) <- raw;
    ui.scroll_visual.(slot) <- scroll_stretch raw height in
  (* any input catches a coast or a bounce where it is *)
  if touch || wheel <> 0. || drag <> 0. then ui.scroll_mode.(slot) <- 0;
  if touch then ui.scroll_event_time.(slot) <- Float.infinity;
  if drag <> 0. then begin
    pull drag;
    (* held by the fingers: it springs back when they lift *)
    ui.scroll_event_time.(slot) <- Float.infinity
  end else if wheel <> 0. then begin
    pull ~past:scroll_wheel_past (wheel *. ui.b_scroll_step.(index));
    ui.scroll_event_time.(slot) <- time
  end;
  if lift then begin
    if ui.scroll_visual.(slot) <> 0. then ui.scroll_event_time.(slot) <- Float.neg_infinity
    else if max_scroll > 0. && Float.abs ui.gesture_velocity >= scroll_min_velocity then begin
      ui.scroll_mode.(slot) <- 1;
      ui.scroll_velocity.(slot) <- ui.gesture_velocity;
      ui.scroll_start.(slot) <- time;
      ui.scroll_from.(slot) <- ui.scroll_y.(slot)
    end
  end;
  if wheel = 0. && drag = 0. then begin
    if ui.scroll_mode.(slot) = 1 then begin
      (* v(t) = v0 e^(-t/tau), so y(t) = y0 + v0 tau (1 - e^(-t/tau)) *)
      let velocity = ui.scroll_velocity.(slot) and from = ui.scroll_from.(slot) in
      let at t = from +. velocity *. scroll_coast_tau
        *. (1. -. Float.exp (-. t /. scroll_coast_tau)) in
      let t = Float.max 0. (time -. ui.scroll_start.(slot)) in
      let y = at t in
      if y < 0. || y > max_scroll then begin
        (* the coast is monotonic: bisect for when it met the edge and bounce from then *)
        let edge = if y < 0. then 0. else max_scroll in
        let early = ref 0. and late = ref t in
        for _ = 1 to 24 do
          let mid = (!early +. !late) /. 2. in
          if (at mid -. edge) *. (y -. edge) > 0. then late := mid else early := mid
        done;
        let arrival = velocity *. Float.exp (-. !late /. scroll_coast_tau) in
        ui.scroll_y.(slot) <- edge;
        ui.scroll_mode.(slot) <- 2;
        ui.scroll_start.(slot) <- ui.scroll_start.(slot) +. !late;
        ui.scroll_velocity.(slot) <-
          scroll_bounce_launch *. arrival /. (scroll_bounce_b -. scroll_bounce_a)
      end else begin
        (* on whole device pixels, so text is sharp where the coast ends *)
        let density = float ui.density in
        ui.scroll_y.(slot) <- Float.round (y *. density) /. density;
        if Float.abs (velocity *. Float.exp (-. t /. scroll_coast_tau)) < scroll_min_velocity
        then ui.scroll_mode.(slot) <- 0
      end
    end;
    if ui.scroll_mode.(slot) = 2 then begin
      let t = Float.max 0. (time -. ui.scroll_start.(slot)) in
      let visual = ui.scroll_velocity.(slot)
        *. (Float.exp (-. scroll_bounce_a *. t) -. Float.exp (-. scroll_bounce_b *. t)) in
      let settled = t > 1. /. scroll_bounce_a && Float.abs visual < 0.25 in
      if settled then ui.scroll_mode.(slot) <- 0;
      ui.scroll_visual.(slot) <- if settled then 0. else visual;
      ui.scroll_raw.(slot) <- scroll_travel ui.scroll_visual.(slot) height
    end else if ui.scroll_visual.(slot) <> 0.
        && time -. ui.scroll_event_time.(slot) > 0.08 then begin
      let dt = Float.max 0. (time -. ui.scroll_frame_time.(slot)) in
      let visual = ui.scroll_visual.(slot) *. Float.exp (-. dt /. scroll_spring_tau) in
      ui.scroll_visual.(slot) <- if Float.abs visual < 0.25 then 0. else visual;
      ui.scroll_raw.(slot) <- scroll_travel ui.scroll_visual.(slot) height
    end
  end;
  ui.scroll_frame_time.(slot) <- time;
  if consume then Option.iter (fun value ->
    value.scroll_wheel <- 0.; value.scroll_drag <- 0.;
    value.scroll_touch <- false; value.scroll_lift <- false)
    (Int_table.find_opt ui.signals ui.b_key.(index))

(* Top-down: relative sizes, grow shares, scrolling, scroll gutters, and positions. *)
let arrange ui time =
  ui.l_x.(0) <- 0.; ui.l_y.(0) <- 0.;
  for index = 0 to ui.count - 1 do
    let padding = ui.b_padding.(index) and row = ui.b_row.(index) in
    let scrolls = ui.b_flags.(index) land scroll <> 0 in
    let max_scroll = Float.max 0. (ui.l_content.(index) -. ui.l_h.(index)) in
    let gutter = if scrolls && max_scroll > 0. && ui.b_flags.(index) land over_thumb = 0 then 10. else 0. in
    ui.l_gutter.(index) <- gutter;
    let slot = ui.b_slot.(index) in
    if scrolls then begin
      apply_scroll ui time index;
      ui.scroll_y.(slot) <- Float.max 0. (Float.min max_scroll ui.scroll_y.(slot))
    end;
    let inner_w = ui.l_w.(index) -. (2. *. padding) -. gutter
    and inner_h = ui.l_h.(index) -. (2. *. padding) in
    let relative kind inner current = match kind with
      | Pct fraction -> inner *. fraction
      | Rel f -> f inner
      | Px _ | Fit | Grow -> current in
    let fixed = ref 0. and grow_count = ref 0 in
    let next = ref ui.b_first.(index) in
    while !next >= 0 do
      let child = !next in
      ui.l_w.(child) <- relative ui.b_w.(child) inner_w ui.l_w.(child);
      ui.l_h.(child) <- Float.min ui.b_max_h.(child)
        (relative ui.b_h.(child) inner_h ui.l_h.(child));
      if flow ui child then begin
        let kind = if row then ui.b_w.(child) else ui.b_h.(child) in
        if kind = Grow then incr grow_count
        else fixed := !fixed +. (if row then ui.l_w.(child) else ui.l_h.(child))
      end;
      next := ui.b_next.(child)
    done;
    let share = if !grow_count = 0 then 0.
      else Float.max 0. (((if row then inner_w else inner_h) -. !fixed)
        /. float !grow_count) in
    let cursor = ref padding in
    let scroll_offset = if scrolls then ui.scroll_y.(slot) +. ui.scroll_visual.(slot) else 0. in
    next := ui.b_first.(index);
    while !next >= 0 do
      let child = !next in
      if row then begin
        if ui.b_w.(child) = Grow && flow ui child then ui.l_w.(child) <- share;
        if ui.b_h.(child) = Grow then
          ui.l_h.(child) <- Float.min ui.b_max_h.(child) inner_h
      end else begin
        if ui.b_h.(child) = Grow && flow ui child then
          ui.l_h.(child) <- Float.min ui.b_max_h.(child) share;
        if ui.b_w.(child) = Grow then ui.l_w.(child) <- inner_w
      end;
      if flow ui child then begin
        if row then begin
          ui.l_x.(child) <- ui.l_x.(index) +. !cursor;
          ui.l_y.(child) <- ui.l_y.(index) +. padding -. scroll_offset;
          cursor := !cursor +. ui.l_w.(child)
        end else begin
          ui.l_x.(child) <- ui.l_x.(index) +. padding;
          ui.l_y.(child) <- ui.l_y.(index) +. !cursor -. scroll_offset;
          cursor := !cursor +. ui.l_h.(child)
        end
      end else begin
        ui.l_x.(child) <- ui.l_x.(index) +. ui.b_at_x.(child);
        ui.l_y.(child) <- ui.l_y.(index) +. ui.b_at_y.(child)
      end;
      next := ui.b_next.(child)
    done
  done

let screen ui index =
  ui.l_x.(index), ui.l_y.(index), ui.l_w.(index), ui.l_h.(index)

let intersect (ax, ay, aw, ah) (bx, by, bw, bh) =
  let x = Float.max ax bx and y = Float.max ay by in
  let right = Float.min (ax +. aw) (bx +. bw)
  and bottom = Float.min (ay +. ah) (by +. bh) in
  x, y, Float.max 0. (right -. x), Float.max 0. (bottom -. y)

(* ----------------------------------------------------------- painting *)

let packed (color : Rays_math.Color.t) =
  Int32.logor (Int32.shift_left (Int32.of_int color.r) 24)
    (Int32.logor (Int32.shift_left (Int32.of_int color.g) 16)
       (Int32.logor (Int32.shift_left (Int32.of_int color.b) 8)
          (Int32.of_int color.a)))

module Paint = struct
  type t = paint

  let prepare paint =
    let x, y, w, h = paint.clip_rect in
    Batch.Builder.set_clip paint.builder
      (Some { Batch.x; y; width = w; height = h })

  let image paint ~x ~y ~w ~h image =
    let ui=paint.owner in
    (* ponytail: at most 64 borrowed images per frame, linear identity lookup;
       use the existing Int_table if a measured multi-image pane needs more. *)
    let id=match List.find_opt(fun(_,other)->other==image)ui.borrowed_images with
      |Some(id,_)->id
      |None->if List.length ui.borrowed_images>=64 then invalid_arg "Ui.Paint.image: 64 images per frame";
          let id=List.length ui.borrowed_images+2 in ui.borrowed_images<-(id,image)::ui.borrowed_images;id in
    prepare paint;
    Batch.Builder.textured paint.builder ~texture:id ~x ~y ~width:w ~height:h
      ~u0:0. ~v0:0. ~u1:(float(fst(Image.dimensions image)))
      ~v1:(float(snd(Image.dimensions image))) ~color:0xffffffffl

  let fill paint ~x ~y ~w ~h color =
    prepare paint;
    Batch.Builder.rect paint.builder ~x ~y ~width:w ~height:h
      ~color:(packed color) ~radius:0. ()

  let stroke paint ~x ~y ~w ~h ?(width = 1.) color =
    prepare paint;
    Batch.Builder.rect paint.builder ~x ~y ~width:w ~height:h
      ~border_color:(packed color) ~border:width ~radius:0. ()

  (* A border drawn inside its box, as CSS draws one: [stroke] centres its line on the rectangle's
     edge, so a box of whole points gets the line half a width in; the border then covers whole
     points (a 1-point border on a 2x display is two pixels, on the box's own pixels). *)
  let frame paint ~x ~y ~w ~h ?(width = 1.) color =
    let half = width /. 2. in
    stroke paint ~x:(x +. half) ~y:(y +. half) ~w:(w -. width) ~h:(h -. width) ~width color

  let rect paint ~x ~y ~w ~h ?fill:fill_color ?stroke:stroke_color () =
    let fill_color = match fill_color, stroke_color with
      | None, None -> Some Rays_math.Color.white | _ -> fill_color in
    Option.iter (fill paint ~x ~y ~w ~h) fill_color;
    Option.iter (stroke paint ~x ~y ~w ~h) stroke_color

  let wire paint p0 p1 p2 p3 ?(width = 1.) color =
    prepare paint;
    Batch.Builder.wire paint.builder p0 p1 p2 p3 ~width ~color:(packed color)

  let line paint ~from_:(x0, y0) ~to_:(x1, y1) ?(width = 1.) color =
    let half = width /. 2. in
    if x0 = x1 then
      fill paint ~x:(x0 -. half) ~y:(Float.min y0 y1) ~w:width
        ~h:(Float.abs (y1 -. y0)) color
    else if y0 = y1 then
      fill paint ~x:(Float.min x0 x1) ~y:(y0 -. half) ~w:(Float.abs (x1 -. x0))
        ~h:width color
    else
      wire paint (x0, y0)
        (x0 +. ((x1 -. x0) /. 3.), y0 +. ((y1 -. y0) /. 3.))
        (x0 +. (2. *. (x1 -. x0) /. 3.), y0 +. (2. *. (y1 -. y0) /. 3.))
        (x1, y1) ~width color

  let circle paint ~at:(cx, cy) ~radius ?fill:fill_color ?stroke:stroke_color () =
    prepare paint;
    let fill_color = match fill_color, stroke_color with
      | None, None -> Some Rays_math.Color.white | _ -> fill_color in
    Batch.Builder.rect paint.builder ~x:(cx -. radius) ~y:(cy -. radius)
      ~width:(2. *. radius) ~height:(2. *. radius)
      ~color:(Option.fold ~none:0l ~some:packed fill_color)
      ~border_color:(Option.fold ~none:0l ~some:packed stroke_color)
      ~border:(if Option.is_some stroke_color then 1. else 0.)
      ~radius ~anti_alias:true ()

  let grid paint ~x ~y ~w ~h ~origin:(ox, oy) ~spacing ?(dot = 1.) color =
    prepare paint;
    Batch.Builder.grid paint.builder ~x ~y ~width:w ~height:h ~origin_x:ox
      ~origin_y:oy ~spacing ~dot ~color:(packed color)

  let text paint ~at:(x, y) ?size ?(tracking = 0.) ?color text =
    let ui = paint.owner in
    if text <> "" then match face ui size with
      | None -> ()
      | Some font ->
          prepare paint;
          let id = font_id ui.atlas font in
          let color = packed (Option.value color ~default:ui.theme.foreground) in
          let density = float ui.density in
          (* Snap the run's origin to a physical pixel so each glyph texel
             lands on exactly one backing pixel, as whole-string text does. *)
          let snap value = Float.round (value *. density) /. density in
          let screen_x = snap x
          and screen_y = snap (y
                               +. ascent_shift ui ~size:(Option.value size ~default:ui.font_size) ~density:ui.density) in
          (* the pen runs in fractional physical pixels (0.88 points of tracking is 1.76 at 2x);
             each glyph is placed on a whole pixel *)
          let pen = ref 0. and tracking = tracking *. density in
          iter_code_points text (fun code ->
            match glyph ui.atlas font ~id ~density:ui.density code with
            | None -> ()
            | Some glyph ->
                if glyph.gw > 0 then begin
                  let gx = screen_x +. (Float.round !pen /. density)
                  and gw = float glyph.gw /. density
                  and gh = float glyph.gh /. density in
                  Batch.Builder.textured paint.builder ~texture:1
                    ~x:gx ~y:screen_y ~width:gw ~height:gh
                    ~u0:(float glyph.gx) ~v0:(float glyph.gy)
                    ~u1:(float (add glyph.gx glyph.gw))
                    ~v1:(float (add glyph.gy glyph.gh)) ~color
                end;
                pen := !pen +. float glyph.advance +. tracking)

  let text_width paint ?size text = text_width paint.owner ?size text

  (* The label style: upper case, two points under the body, 0.08 em of tracking. *)
  let label_size paint = max 8 (paint.owner.font_size - 2)
  let cap_tracking paint = 0.08 *. float (label_size paint)
  let cap paint ~at ?color label =
    text paint ~at ~size:(label_size paint) ~tracking:(cap_tracking paint)
      ~color:(Option.value color ~default:(Theme.ink_2 paint.owner.theme)) (String.uppercase_ascii label)
  (* the tracking follows every letter, the last too (CSS letter-spacing): text after a label
     starts one step later, and a right-aligned label ends one step early *)
  let cap_width paint label =
    let label = String.uppercase_ascii label in
    let count = ref 0 in
    iter_code_points label (fun _ -> incr count);
    text_width paint ~size:(label_size paint) label +. (cap_tracking paint *. float !count)

  (* A chevron centred on [at]: its strokes span 7 x 3.5, which with the 1-point line is the
     sheet's 9 x 5.5 of ink. *)
  let chevron paint ~at:(cx, cy) direction color =
    let a, b = match direction with
      | `Down -> (-3.5, -1.75), (3.5, -1.75) | `Up -> (-3.5, 1.75), (3.5, 1.75)
      | `Right -> (-1.75, -3.5), (-1.75, 3.5) | `Left -> (1.75, -3.5), (1.75, 3.5) in
    let tip = match direction with
      | `Down -> 0., 1.75 | `Up -> 0., -1.75 | `Right -> 1.75, 0. | `Left -> -1.75, 0. in
    let p (dx, dy) = cx +. dx, cy +. dy in
    line paint ~from_:(p a) ~to_:(p tip) color; line paint ~from_:(p tip) ~to_:(p b) color

  (* Selection: four corner brackets [offset] outside the rectangle. *)
  let brackets paint ~x ~y ~w ~h ?(offset = 4.) ?(length = 8.) ?(width = 2.) color =
    let x = x -. offset and y = y -. offset and w = w +. (2. *. offset) and h = h +. (2. *. offset) in
    List.iter (fun (cx, cy, sx, sy) ->
      fill paint ~x:(if sx > 0. then cx else cx -. length) ~y:(if sy > 0. then cy else cy -. width)
        ~w:length ~h:width color;
      fill paint ~x:(if sx > 0. then cx else cx -. width) ~y:(if sy > 0. then cy else cy -. length)
        ~w:width ~h:length color)
      [ x, y, 1., 1.; x +. w, y, -1., 1.; x, y +. h, 1., -1.; x +. w, y +. h, -1., -1. ]

  let dashed paint ~from_:(x0, y0) ~to_:(x1, y1) ?(width = 1.) color =
    let length = Float.hypot (x1 -. x0) (y1 -. y0) in
    let steps = int_of_float (Float.ceil (length /. 7.)) in
    for step = 0 to steps - 1 do
      let a = float step *. 7. /. length and b = Float.min 1. ((float step *. 7. +. 4.) /. length) in
      line paint ~from_:(x0 +. ((x1 -. x0) *. a), y0 +. ((y1 -. y0) *. a))
        ~to_:(x0 +. ((x1 -. x0) *. b), y0 +. ((y1 -. y0) *. b)) ~width color
    done

  let dashed_rect paint ~x ~y ~w ~h color =
    dashed paint ~from_:(x, y) ~to_:(x +. w, y) color; dashed paint ~from_:(x, y +. h) ~to_:(x +. w, y +. h) color;
    dashed paint ~from_:(x, y) ~to_:(x, y +. h) color; dashed paint ~from_:(x +. w, y) ~to_:(x +. w, y +. h) color

  (* Empty: a hairline box crossed corner to corner. *)

  (* Bypassed, stale, cooking: diagonal hairlines every 5 points, clipped to the rectangle. *)

  (* A flag of the outline: 12 points, the input fill, a line-3 edge inside the box and, when on, a
     6-point mark in ink-2; square, or round for the second column onwards. *)
  let flag paint ~at:(cx, cy) ?(round = false) on =
    let theme = paint.owner.theme in
    if round then begin
      circle paint ~at:(cx, cy) ~radius:5.5 ~fill:theme.input ~stroke:(Theme.border theme) ();
      if on then circle paint ~at:(cx, cy) ~radius:3. ~fill:(Theme.ink_2 theme) ()
    end else begin
      fill paint ~x:(cx -. 6.) ~y:(cy -. 6.) ~w:12. ~h:12. theme.input;
      frame paint ~x:(cx -. 6.) ~y:(cy -. 6.) ~w:12. ~h:12. (Theme.border theme);
      if on then fill paint ~x:(cx -. 3.) ~y:(cy -. 3.) ~w:6. ~h:6. (Theme.ink_2 theme)
    end

  (* Progress of work with no known end shape: a 96 by 8 box with a line-3 edge, hatched, and the
     done part a 6-point bar in ink inside the edge. *)

  (* The ratio of a splitter in the drag: an accent label ("58 / 42"), no box. *)
  let ratio paint ~at first second =
    cap paint ~at ~color:paint.owner.theme.accent (Printf.sprintf "%d / %d" first second)

  let input_region paint ?(cursor=0.) ~x ~y ~w ~h ~focused () =
    paint.owner.regions <- Scene.text_input_region
        ~at:(int_of_float x, int_of_float y)
        ~w:(int_of_float w) ~h:(int_of_float h)
        ~focused ~cursor:(max 0 (int_of_float (Float.round cursor))) ()
        :: paint.owner.regions
end

let record_hit ui index parent (x, y, w, h) =
  let position = ui.hit_count in
  if position >= Array.length ui.hit_keys then begin
    let size = max 64 (2 * Array.length ui.hit_keys) in
    ui.hit_keys <- grow ui.hit_keys size 0;
    ui.hit_parent <- grow ui.hit_parent size 0;
    ui.hit_flags_of <- grow ui.hit_flags_of size 0;
    ui.hit_x <- grow ui.hit_x size 0.; ui.hit_y <- grow ui.hit_y size 0.;
    ui.hit_w <- grow ui.hit_w size 0.; ui.hit_h <- grow ui.hit_h size 0.
  end;
  ui.hit_keys.(position) <- ui.b_key.(index);
  ui.hit_parent.(position) <- parent;
  ui.hit_flags_of.(position) <- ui.b_flags.(index);
  ui.hit_x.(position) <- x; ui.hit_y.(position) <- y;
  ui.hit_w.(position) <- w; ui.hit_h.(position) <- h;
  ui.chain_key <- 0;
  ui.hit_count <- add position 1

let paint_all ui (frame : Frame.t) =
  let builder = ui.batch_builder in
  Batch.Builder.reset builder;
  ui.regions <- [];
  ui.hit_count <- 0; ui.chain_key <- 0;
  let paint = { owner = ui; builder;
    clip_rect = (0., 0., float (fst frame.input.size), float (snd frame.input.size)) } in
  let layers = ref [] and key = ref 0 in
  let flush () = layers := (!key, Batch.Builder.publish builder) :: !layers;
    Batch.Builder.reset builder in
  let run painters index clip_rect =
    if painters <> [] then begin
      paint.clip_rect <- clip_rect;
      let local = ui.l_x.(index), ui.l_y.(index), ui.l_w.(index), ui.l_h.(index) in
      (* [draw] prepends; paint oldest first without reversing a copy. *)
      let rec in_order = function
        | [] -> ()
        | painter :: earlier -> in_order earlier; painter paint local in
      in_order painters
    end in
  let rec visit index clip_rect parent_hit =
    if ui.b_parent.(index) = 0 && List.exists (fun (_, root) -> root = index) ui.foreground then begin
      flush (); key := ui.b_key.(index)
    end;
    let screen_rect = screen ui index in
    let slot = ui.b_slot.(index) in
    let x, y, w, h = screen_rect in
    ui.rx.(slot) <- x; ui.ry.(slot) <- y; ui.rw.(slot) <- w; ui.rh.(slot) <- h;
    let hit = match ui.b_hit.(index) with
      | None -> screen_rect
      | Some f -> f screen_rect in
    let clipped_hit = intersect hit clip_rect in
    let _, _, hw, hh = clipped_hit in
    let visible = match ui.b_hit.(index) with
      | None -> hw > 0. && hh > 0.
      | Some _ -> let _, _, vw, vh = intersect screen_rect clip_rect in
          vw > 0. && vh > 0. in
    let has_hit = ui.b_flags.(index) land hit_flags <> 0 && hw > 0. && hh > 0. in
    if has_hit then record_hit ui index parent_hit clipped_hit;
    (* a box with no extent of its own (a [Fit] box whose children are all placed with [~at]) is
       not out of view: its children show unless it clips them *)
    let shown = visible in
    if shown || ((w <= 0. || h <= 0.) && ui.b_flags.(index) land clip = 0) then begin
      if shown then run ui.b_painters.(index) index clip_rect;
      let child_clip = if ui.b_flags.(index) land clip <> 0 then
          let padding = ui.b_padding.(index) in
          intersect clip_rect (x +. padding, y +. padding,
            Float.max 0. (w -. (2. *. padding)), Float.max 0. (h -. (2. *. padding)))
        else clip_rect in
      let parent_hit = if has_hit then ui.b_key.(index) else parent_hit in
      children ui index (fun child -> visit child child_clip parent_hit);
      if shown then run ui.b_overlays.(index) index clip_rect;
      if shown && ui.keyboard_focus && ui.focus = ui.b_key.(index)
         && ui.b_flags.(index) land focus_mark = 0 then begin
        paint.clip_rect <- clip_rect;
        Paint.frame paint ~x:ui.l_x.(index) ~y:ui.l_y.(index)
          ~w:(Float.max 0. ui.l_w.(index)) ~h:(Float.max 0. ui.l_h.(index)) ui.theme.accent
      end
    end else
      (* Culled: keep retained rectangles current for [rect] queries. *)
      let rec retain index = children ui index (fun child ->
        let x, y, w, h = screen ui child in
        let slot = ui.b_slot.(child) in
        ui.rx.(slot) <- x; ui.ry.(slot) <- y; ui.rw.(slot) <- w; ui.rh.(slot) <- h;
        retain child) in
      retain index in
  visit 0 paint.clip_rect 0;
  flush (); List.rev !layers

(* -------------------------------------------------------------- frame *)

(* Build at the root under the root's own seed, wherever the caller is in the tree: what is built
   is laid out in the window, clipped by no pane. *)
let at_root ui f =
  let parents = ui.parents and seeds = ui.seeds in
  ui.parents <- [0]; ui.seeds <- [0x2c1b3c6d];
  Fun.protect ~finally:(fun () -> ui.parents <- parents; ui.seeds <- seeds) f

(* The payload in flight follows the pointer as a small label above everything. Idle frames
   (nothing carried) build and paint nothing. *)
let carry_ghost ui =
  match ui.payload with
  | Some (payload, _) when Float.is_finite (fst ui.pointer) ->
      let px, py = ui.pointer in
      let width = text_width ui payload.value +. 12. in
      at_root ui (fun () ->
        (* kit overlays [07]: a white 20-point chip with the payload in body-size ink, and under
           it the sheet's tip with the key that drops it *)
        let ghost = box ui ~w:(Px width) ~h:(Px 20.) ~at:(px +. 14., py +. 14.) "ui-carry-ghost" in
        let theme = ui.theme in
        let size = ui.font_size in
        let small = max 8 (size - 2) in
        let tip_text = "drop to put" and tip_key = "esc" in
        let tw = text_width ui ~size:small in
        let tip_w = 2. +. 12. +. tw tip_text +. 6. +. tw tip_key in
        let tip = box ui ~w:(Px tip_w) ~h:(Px 20.) ~at:(px +. 14., py +. 14. +. 20. +. 8.) "ui-carry-tip" in
        draw_over ui ghost (fun paint (x, y, w, h) ->
          Paint.fill paint ~x ~y ~w ~h theme.input;
          Paint.text paint ~at:(x +. 6., y +. Float.floor (float (20 - size) /. 2.)) ~size ~color:theme.foreground payload.value);
        draw_over ui tip (fun paint (x, y, w, h) ->
          Paint.fill paint ~x ~y ~w ~h theme.input;
          Paint.stroke paint ~x:(x +. 0.5) ~y:(y +. 0.5) ~w:(w -. 1.) ~h:(h -. 1.) (Theme.edge theme);
          let ty = y +. Float.floor (float (20 - small) /. 2.) in
          Paint.text paint ~at:(x +. 7., ty) ~size:small ~color:theme.foreground tip_text;
          Paint.text paint ~at:(x +. 7. +. tw tip_text +. 6., ty) ~size:small ~color:(Theme.ink_3 theme) tip_key);
        ui.foreground <- (max_int, ghost.index) :: (max_int, tip.index) :: ui.foreground)
  | _ -> ()

let frame ui (frame : Frame.t) f =
  if ui.destroyed then invalid_arg "Ui.frame: the UI was destroyed";
  if ui.building then invalid_arg "Ui.frame: frames cannot nest";
  if not (Float.is_finite frame.input.t) then invalid_arg "Ui.frame: time must be finite";
  let scale_x, scale_y = frame.pixel_scale in
  ui.density <- max 1 (int_of_float (Float.round (Float.max scale_x scale_y)));
  ui.frame_number <- add ui.frame_number 1;
  (* the window's title bar shows the ground the UI is painted on; no window, no effect *)
  if ui.window_ground <> Some ui.theme.panel then begin
    ui.window_ground <- Some ui.theme.panel;
    ignore (Sketch.set_window_background ui.theme.panel)
  end;
  route ui frame;
  ui.modal_key <- None; ui.modal_extra <- [];
  ui.count <- 0; ui.closed_section <- -1;
  ui.building <- true;
  ui.parents <- []; ui.seeds <- [];
  let root = append_box ui ~key:0x2c1b3c6d ~parent:(-1) in
  set_box ui root ~flags:none ~w:(Px (float (fst frame.input.size))) ~h:(Px (float (snd frame.input.size)))
    ~max_h:Float.infinity ~row:false ~padding:0. ~at_x:0. ~at_y:0.
    ~scroll_step:0. ~hit:None;
  ui.parents <- [root]; ui.seeds <- [0x2c1b3c6d];
  let result = Fun.protect ~finally:(fun () -> ui.building <- false)
      (fun () -> let result = f ui in carry_ghost ui; result) in
  ui.parents <- []; ui.seeds <- [];
  (* Move popups to the end of the root's children, oldest first. *)
  List.iter (fun index ->
    if ui.b_parent.(index) = 0 && ui.b_last.(0) <> index then begin
      let rec unlink previous child =
        if child >= 0 then
          if child = index then begin
            (if previous < 0 then ui.b_first.(0) <- ui.b_next.(index)
             else ui.b_next.(previous) <- ui.b_next.(index))
          end else unlink child ui.b_next.(child) in
      unlink (-1) ui.b_first.(0);
      ui.b_next.(ui.b_last.(0)) <- index; ui.b_next.(index) <- -1;
      ui.b_last.(0) <- index
    end) (List.stable_sort (fun (a, _) (b, _) -> Int.compare a b) (List.rev ui.foreground)
      |> List.map snd |> fun foreground -> foreground @ List.rev ui.overlays);
  ui.overlays <- [];
  intrinsic ui;
  arrange ui frame.input.t;
  ui.borrowed_images<-[];
  let layers = paint_all ui frame in
  ui.foreground <- [];
  prune ui;
  if ui.focus <> 0 && Table.find ui.table ui.focus < 0 then begin
    ui.focus <- 0; ui.edit_focus <- 0
  end;
  publish_atlas ui.atlas;
  ui.scene_layers <- List.map (fun (key, batch) ->
    let images = match ui.atlas.image with
      | Some image when Batch.textures batch <> [] -> [1, image]
      | Some _ | None -> [] in
    let images=images@List.filter(fun(id,_)->List.mem id(Batch.textures batch))ui.borrowed_images in
    let batch = if images = [] && Batch.textures batch <> [] then Batch.empty else batch in
    key, (if Batch.count batch = 0 then [] else [Scene.Layer.ui ~images batch])) layers;
  ui.scene <- List.concat_map snd ui.scene_layers @ List.rev ui.regions;
  result

(* ------------------------------------------------------ layout helpers *)

let hover_delay ui ~key =
  let time = match ui.input_frame with Some frame -> frame.input.t | None -> 0. in
  if ui.active <> None || ui.modal_in_frame
      || List.exists (function Input.Focus_lost -> true | _ -> false)
      ui.frame_events then (ui.hover_rest <- None; false)
  else match ui.hover_rest with
    | Some (previous, pointer, start, seen) when previous = key && pointer = ui.pointer
        && (seen = ui.frame_number || seen = ui.frame_number - 1) && time >= start ->
        ui.hover_rest <- Some (key, pointer, start, ui.frame_number);
        time -. start >= 0.380
    | _ -> ui.hover_rest <- Some (key, ui.pointer, time, ui.frame_number); false

(* A tooltip: a 20-point sheet with a line-2 edge and 6 points of padding, text at the label size and
   long text wraps to further 14-point lines. *)
let tooltip ui ~key ~text =
  if hover_delay ui ~key then begin
    let size = max 8 (ui.font_size - 2) in
    let char_width = Float.max 1. (text_width ui ~size "0") in
    let width = Float.min 400. (Float.max 40. (ui.view_w -. 16.)) in
    let columns = max 1 (int_of_float ((width -. 14.) /. char_width)) in
    let lines, last = List.fold_left (fun (lines, line) word ->
      if line = "" then lines, word
      else if String.length line + String.length word + 1 <= columns then
        lines, line ^ " " ^ word
      else line :: lines, word) ([], "") (String.split_on_char ' ' text) in
    let lines = List.rev (last :: lines) in
    let widest = List.fold_left (fun most line -> Float.max most (text_width ui ~size line)) 0. lines in
    let width = Float.min width (14. +. widest) in
    let height = 20. +. (14. *. float (List.length lines - 1)) in
    let px, py = ui.pointer in
    let x = Float.max 8. (Float.min (px +. 12.) (ui.view_w -. width -. 8.)) in
    let y = if py +. height +. 24. <= ui.view_h then py +. 20.
      else Float.max 8. (py -. height -. 8.) in
    at_root ui (fun () ->
      let tip = box ui ~w:(Px width) ~h:(Px height) ~at:(x, y) "ui-tooltip" in
      ui.overlays <- tip.index :: ui.overlays;
      draw ui tip (fun paint (x, y, w, h) ->
        Paint.fill paint ~x ~y ~w ~h ui.theme.input;
        Paint.frame paint ~x ~y ~w ~h (Theme.edge ui.theme);
        List.iteri (fun i line ->
          Paint.text paint ~size ~color:ui.theme.foreground
            ~at:(x +. 7., y +. (14. *. float i) +. Float.floor (float (20 - size) /. 2.)) line) lines))
  end

let col ui ?(padding = 0.) label f =
  within ui (box ui ~axis:Column ~padding label) f

