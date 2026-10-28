open Rays

module Int_table = Hashtbl.Make (Int)
module Batch = Scene.Private.Ui_batch

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
(* the control marks its own keyboard focus (a button's accent line, a field's accent underline):
   the engine strokes no ring round it *)
let focus_mark = 256
let tab_stop_marked = tab_stop lor focus_mark
let add = Stdlib.( + )
let hit_flags = clickable lor focusable lor scroll lor blocking

type rect = float * float * float * float

(* What a carry holds, and how it was picked up: by pressing a box and moving past the dead
   zone (the box's key), or by a key (no pointer capture). *)
type payload = { kind : string; value : string }
type origin = From_pointer of int | From_key
type drop = Hover of payload | Dropped of payload

(* The dead zone of a carry started by the pointer, in points. *)
let carry_dead_zone = 4.

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
  let generation = Font.Private.generation font in
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
      match Font.Private.glyph ~density font code with
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
        let target = owner in
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
        if (target = 0 || flags land clickable <> 0) && ui.active = None then begin
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
        if Float.hypot (x -. px) (y -. py) >= carry_dead_zone then
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
    key, (if Batch.count batch = 0 then [] else [Scene.Private.ui ~images batch])) layers;
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

(* --------------------------------------------------------- kit widgets *)

let kit_row ui ?(flags = clickable lor focusable lor blocking lor tab_only) ?hit label =
  box ui ~flags ~w:Grow ~h:(Px (float ui.kit_row_height)) ?hit label

let ints (x, y, w, h) = int_of_float x, int_of_float y, int_of_float w, int_of_float h
let floats (x, y, w, h) = float x, float y, float w, float h

(* Kit rev 3 geometry from a row rectangle: 12 points at each side, a label column of 112
   (half of a narrow row), then the 20-point control. *)
let side = 12
(* Text of [size] points centred in a row: its top is half of what the row has over the text
   size.  Measured against the kit's reference renders (specification/pxui-kit): 13-point text
   in a 24-point row has its ascenders at 7 and its descenders at 18. *)
let text_top ui ?size y h =
  y +. Float.floor ((h -. float (Option.value size ~default:ui.font_size)) /. 2.)

let table ui ~at ~w ~h ~headers ~rows ~cell label =
  if rows < 0 then invalid_arg "Ui.table: negative row count";
  let row_h = 24. in
  let widths = Array.map (fun header -> Float.max 72. (text_width ui header +. 24.)) headers in
  let stops = Array.make (Array.length headers + 1) 0. in
  Array.iteri (fun i width -> stops.(i + 1) <- stops.(i) +. width) widths;
  let root = box ui ~flags:clip ~at ~w:(Px w) ~h:(Px h) label in
  within ui root (fun () ->
    let header = box ui ~w:(Px w) ~h:(Px row_h) (label ^ "-header") in
    draw ui header (fun paint (x, y, _, _) ->
      Array.iteri (fun i title ->
        Paint.text paint ~color:(Theme.ink_2 ui.theme) ~at:(x +. stops.(i) +. 8., text_top ui y row_h) title) headers;
      Paint.line paint ~from_:(x, y +. row_h -. 0.5) ~to_:(x +. w, y +. row_h -. 0.5) (Theme.border ui.theme));
    let body_h = Float.max 0. (h -. row_h) in
    let body = box ui ~flags:(scroll + clip) ~w:(Px w) ~h:(Px body_h)
        ~scroll_step:row_h (label ^ "-body") in
    (* Both sizes are explicit: use the shared wheel/trackpad/coast state before
       choosing rows. Arrange then sees consumed input and a zero time delta. *)
    ui.l_h.(body.index) <- body_h;
    ui.l_content.(body.index) <- float rows *. row_h;
    Option.iter (fun (frame : Frame.t) -> apply_scroll ~consume:true ui frame.input.t body.index) ui.input_frame;
    let first = min rows (max 0 (int_of_float (Float.floor (scroll_offset ui body /. row_h)))) in
    let last = min rows (first + int_of_float (Float.ceil (body_h /. row_h)) + 2) in
    let clicked = ref None in
    within ui body (fun () ->
      let extent = box ui ~w:(Px w) ~h:(Px (float rows *. row_h)) (label ^ "-extent") in
      within ui extent (fun () ->
      for index = first to last - 1 do
        let row = box ui ~flags:clickable ~at:(0., float index *. row_h)
            ~w:(Px w) ~h:(Px row_h) (label ^ "-row-" ^ string_of_int index) in
        if (signal ui row).clicked then clicked := Some index;
        draw ui row (fun paint (x, y, _, _) ->
          Array.iteri (fun column _ ->
            let value = cell index column in
            let right = x +. stops.(column + 1) -. 8. in
            Paint.text paint ~color:ui.theme.foreground
              ~at:(right -. Paint.text_width paint value, text_top ui y row_h) value) headers;
          Paint.line paint ~from_:(x, y +. row_h -. 0.5) ~to_:(x +. w, y +. row_h -. 0.5) (Theme.faint_border ui.theme))
      done));
    !clicked, signal ui body)

let label_y ui y h = y + ((h - ui.font_size) / 2)
let value_column w = min 112 (max 0 ((w - (2 * side) - 8) / 2))
let value_control (x, y, w, h) =
  let cx = x + side + value_column w + 8 in
  cx, y + 2, max 1 (x + w - side - cx), max 1 (h - 4)
let toggle_control (x, y, w, h) = x + w - side - 28, y + ((h - 14) / 2), 28, 14
let control_hit shape rect = floats (shape (ints rect))

(* A value shown to three significant digits of its range (48.0 in 0..120, 0.20 in 0..1). *)

let fraction_at (x, _, w, _) pointer_x =
  Float.max 0. (Float.min 1. ((pointer_x -. float x) /. float (max 1 (w - 1))))

let fill paint (x, y, w, h) color =
  Paint.fill paint ~x:(float x) ~y:(float y) ~w:(float w) ~h:(float h) color
let kit_text paint ?color x y text =
  Paint.text paint ~at:(float x, float y) ?color text

let hover_row paint ui bounds = fill paint bounds (Theme.faint_border ui.theme)
(* a field is a value on a hairline: accent while it holds the keyboard *)
let underline paint (cx, cy, cw, ch) color = fill paint (cx, cy + ch - 1, cw, 1) color
let paint_switch paint (theme : Theme.t) ~x ~y value =
  Paint.fill paint ~x ~y ~w:28. ~h:14. (if value then theme.input else theme.track);
  Paint.stroke paint ~x:(x +. 0.5) ~y:(y +. 0.5) ~w:27. ~h:13. (Theme.border theme);
  Paint.fill paint ~x:(x +. (if value then 17. else 3.)) ~y:(y +. 3.) ~w:8. ~h:8.
    (if value then theme.accent else Theme.ink_3 theme)
let cap_y ui y h = text_top ui ~size:(max 8 (ui.font_size - 2)) y h
(* a section header: a label in ink-3 and a chevron, under 16 points of space *)
let paint_section paint ui (x, y, w, h) label ~open_ ~hovered =
  ignore hovered;  (* the sheet has no hover state on a section header *)
  Paint.cap paint ~at:(x +. float side, cap_y ui y h) ~color:(Theme.ink_3 ui.theme) label;
  Paint.chevron paint ~at:(x +. w -. float side -. 3., y +. (h /. 2.))
    (if open_ then `Down else `Right) ui.theme.foreground
(* the space above a section header: 4 at the top of its parent (16 under a window's head), 16
   after anything else, none when this closed section directly follows a closed one *)
let section_gap ?(next_closed = true) ?last ui =
  let last = match last with Some last -> last | None -> ui.b_last.(current_parent ui) in
  if last < 0 then (if ui.kit_window then 16 else 4)
  else if last = ui.closed_section && next_closed then 0 else 16

(* The ground of a button, the one painter of [button], of the inspector's and of the shell's positioned buttons: a fill
   on press and hover, the control fill when [on], a line-3 edge for the [primary]. *)
let paint_button_ground paint theme ~held ~hovered ~on ~primary (cx, cy, cw, ch) =
  if held then Paint.fill paint ~x:cx ~y:cy ~w:cw ~h:ch (Theme.pressed_fill theme)
  else if hovered then Paint.fill paint ~x:cx ~y:cy ~w:cw ~h:ch (Theme.hover_fill theme)
  else if on then Paint.fill paint ~x:cx ~y:cy ~w:cw ~h:ch theme.control;
  if primary then
    Paint.stroke paint ~x:(cx +. 0.5) ~y:(cy +. 0.5) ~w:(cw -. 1.) ~h:(ch -. 1.) (Theme.border theme)

let panel_with ?stroke ?(window = false) ui ?(x = 12.) ?(y = 12.) ?(width = 280.) ?height ?max_height
    ?(padding = 0) label f =
  let row_height = 24 in
  if padding < 0 then invalid_arg "Ui.panel: padding must be non-negative";
  let width = Float.max 180. width in
  let max_h = match max_height with
    | Some height -> Float.max height (float (add row_height (2 * padding)))
    | None -> Float.infinity in
  let panel = box ui ~flags:(scroll lor clip lor blocking) ~w:(Px width)
      ~h:(Option.fold ~none:Fit ~some:(fun height -> Px height) height) ~max_h
      ~axis:Column ~padding:(float padding) ~at:(x, y)
      ~scroll_step:(float row_height) label in
  let theme = ui.theme and index = panel.index in
  draw ui panel (fun paint (x, y, w, h) ->
    (* a floating sheet (a menu, a window) is the input white; a docked panel is the ground *)
    Paint.fill paint ~x ~y ~w ~h (if Option.is_some stroke then theme.input else theme.panel));
  draw_over ui panel (fun paint rect ->
    Option.iter (fun color -> let x, y, w, h = rect in
      Paint.frame paint ~x ~y ~w ~h color) stroke;
    let x, y, width, height = ints rect in
    let content = int_of_float ui.l_content.(index) in
    let maximum = max 0 (content - height) in
    if maximum > 0 then begin
      let track_x = x + width - 6 and track_y = y + padding
      and track_height = max 1 (height - (2 * padding)) in
      let thumb_height = min track_height
          (max 20 (track_height * height / max 1 content)) in
      let travel = track_height - thumb_height in
      let scroll_y = int_of_float ui.scroll_y.(panel.box_slot) in
      let thumb_y = track_y + (scroll_y * travel / maximum) in
      (* no track, no arrows: a 4-point thumb *)
      Paint.fill paint ~x:(float track_x) ~y:(float thumb_y) ~w:4.
        ~h:(float thumb_height) (Theme.border theme)
    end);
  let previous_row = ui.kit_row_height
  and previous_window = ui.kit_window in
  ui.kit_row_height <- row_height; ui.kit_window <- window;
  Fun.protect ~finally:(fun () ->
    ui.kit_row_height <- previous_row;
    ui.kit_window <- previous_window)
    (fun () -> within ui panel f)

let panel ?window ui = panel_with ?window ui

let inspector_width ui =
  let rec find index =
    if index <= 0 then 280. else
    match ui.b_w.(index) with
    | Px width -> width
    | _ -> find ui.b_parent.(index) in
  find (current_parent ui)

(* [ellipsis] at a text size; a text with no room at all is the ellipsis alone *)
let inspector_fit paint ~size ~width text =
  let shown = ellipsis ~width:(Paint.text_width paint ~size) ~limit:width text in
  if shown = "" && not (text = "" && width >= 0.) then "…" else shown

(* The kit's inspector row, from its CSS: 12 side padding, a 6-point slot for the pin dot, 8, the
   label column, 8, the control to 12 from the right edge.  The label column is 0.3 of the panel
   less 16 (98 at 380 wide, 80 at 320); a longer label ellipsizes in it, never stacks. *)
let inspector_label_width width = Float.max 56. (Float.floor (width *. 0.3) -. 16.)
let inspector_control_x width = 12. +. 6. +. 8. +. inspector_label_width width +. 8.

(* A window's rows (windows.html): the label at 12, a 88-point column and no pin slot. *)
let inspector_label_x ui = if ui.kit_window then 12. else 26.

(* The 4 points between a head's rule and the first element of the body, when it is not a section. *)
let lead_gap ui =
  if ui.b_last.(current_parent ui) < 0 then ignore (box ui ~w:Grow ~h:(Px 4.) "lead-gap")

let inspector_row ui ?width ?pin ~key ~label () =
  let width = Option.value width ~default:(inspector_width ui) in
  let label_x = inspector_label_x ui in
  let label_w = if ui.kit_window then 88. else inspector_label_width width in
  let control_x = if ui.kit_window then 12. +. 88. +. 8. else inspector_control_x width in
  let pin = if ui.kit_window then None else pin in
  lead_gap ui;
  let row = box ui ~flags:clickable ~w:Grow ~h:(Px (float ui.kit_row_height)) key in
  let theme = ui.theme and hovered = hovered_within ui row in
  draw ui row (fun paint (x, y, w, h) ->
    if hovered then Paint.fill paint ~x ~y ~w ~h (Theme.faint_border theme);
    (* the pin dot: filled ink when the row is on its card, an ink-3 ring when it is not *)
    Option.iter (fun on ->
      let at = x +. 15., y +. (h /. 2.) in
      if on then Paint.circle paint ~at ~radius:3. ~fill:theme.foreground ()
      else Paint.circle paint ~at ~radius:2.5 ~stroke:(Theme.ink_3 theme) ()) pin;
    Paint.text paint ~at:(x +. label_x, text_top ui y h) ~color:(Theme.ink_2 theme)
      (inspector_fit paint ~size:ui.font_size ~width:label_w label));
  row, control_x, 2., Float.max 40. (width -. control_x -. 12.)

let inspector_section ui ~key ?(expanded = false) ?set_expanded label f =
  let last = ui.b_last.(current_parent ui) in
  let title = box ui ~flags:(clickable lor tab_stop) ~w:Grow
      ~h:(Px (float ui.kit_row_height)) key in
  let open_ = state ui title ~default:(if expanded then 1 else 0) <> 0 in
  let open_ = Option.value set_expanded ~default:open_ in
  let signal = signal ui title in
  let open_ = if signal.clicked then not open_ else open_ in
  (* the gap above depends on this section's own state, known only now *)
  let gap = float (section_gap ~next_closed:(not open_) ~last ui) in
  ui.b_h.(title.index) <- Px (gap +. float ui.kit_row_height);
  set_state ui title (if open_ then 1 else 0);
  if not open_ then ui.closed_section <- title.index;
  draw ui title (fun paint (x, y, w, h) ->
    paint_section paint ui (x, y +. gap, w, h -. gap) label ~open_ ~hovered:signal.hovered);
  if open_ then Some (f ()) else None

let inspector_toggle_value ui ~key ~at:(x, y) value =
  let control = box ui ~flags:(clickable lor tab_stop) ~at:(x, y)
      ~w:(Px 28.) ~h:(Px 20.) key in
  let value = if (signal ui control).clicked then not value else value in
  draw ui control (fun paint (x, y, _, _) -> paint_switch paint ui.theme ~x ~y:(y +. 3.) value);
  value

(* A switch row: the switch sits at the start of the control column, not at the row's end. *)
let inspector_toggle ui ~key ~label value =
  let row, cx, cy, _ = inspector_row ui ~key ~label () in
  within ui row (fun () -> inspector_toggle_value ui ~key:(key ^ "-value") ~at:(cx, cy) value)

(* A text button on a row of its own. *)
let inspector_button ui ~key label =
  lead_gap ui;
  let row = box ui ~flags:(clickable lor tab_stop) ~w:Grow ~h:(Px (float ui.kit_row_height)) key in
  let signal = signal ui row in
  draw ui row (fun paint (x, y, w, h) ->
    let shown = inspector_fit paint ~size:ui.font_size ~width:(w -. 24.) label in
    paint_button_ground paint ui.theme ~held:signal.held ~hovered:signal.hovered ~on:false ~primary:false
      (x +. 6., y +. 2., Paint.text_width paint shown +. 12., h -. 4.);
    Paint.text paint ~at:(x +. float side, text_top ui y h) ~color:ui.theme.foreground shown);
  signal.clicked

(* A read-out: the label, then the value right-aligned in the control column; no pin dot. *)
let inspector_readout ui ?width ~key ~label value =
  let row, control_x, _, control_width = inspector_row ui ?width ~key ~label () in
  draw ui row (fun paint (x, y, _, h) ->
    let shown = inspector_fit paint ~size:ui.font_size ~width:control_width value in
    Paint.text paint
      ~at:(x +. control_x +. control_width -. Paint.text_width paint shown, text_top ui y h)
      ~color:ui.theme.foreground shown)

(* The scrolling part of an inspector under its head (the head stays): a column that takes the
   rest of the panel and scrolls, its 4-point line-3 thumb 2 from the right edge. *)
let inspector_body ui f =
  let body = box ui ~flags:(scroll lor clip lor over_thumb) ~w:Grow ~h:Grow ~axis:Column
      ~scroll_step:(float ui.kit_row_height) "inspector-body" in
  draw_over ui body (fun paint (x, y, w, h) ->
    let content = ui.l_content.(body.index) in
    if content > h then begin
      let track_y = y +. 6. and track_h = Float.max 1. (h -. 12.) in
      let thumb_h = Float.min track_h (Float.max 20. (track_h *. h /. content)) in
      let travel = track_h -. thumb_h and maximum = content -. h in
      let thumb_y = track_y +. (ui.scroll_y.(body.box_slot) *. travel /. maximum) in
      Paint.fill paint ~x:(x +. w -. 6.) ~y:thumb_y ~w:4. ~h:thumb_h (Theme.border ui.theme)
    end);
  within ui body f

(* The width the kit's CSS gives [text] of [size] points: the face is monospaced at half an em
   (plus [tracking] a character).  A head lays its buttons and chips out with it, as the sheet
   does, whatever advance the rasterised glyphs round to. *)
let mono_width ?(tracking = 0.) size text =
  let count = ref 0 in
  String.iter (fun c -> if Char.code c land 0xc0 <> 0x80 then incr count) text;
  float !count *. ((float size /. 2.) +. tracking)

(* The bar under an inspector (inspector.html): a line-2 hairline and a 24-point bar of hints, each a
   key in ink-3 at the label size and what it does in ink-2, and at the right a 6-point dot and a
   count as a label.  It sits below the body, which scrolls above it. *)
let inspector_bar ui ~hints ~count =
  let small = max 8 (ui.font_size - 2) in
  let line = float ui.kit_row_height in
  let bar = box ui ~w:Grow ~h:(Px (line +. 1.)) "inspector-bar" in
  draw ui bar (fun paint (x, y, w, _) ->
    let theme = ui.theme in
    Paint.fill paint ~x ~y ~w ~h:1. (Theme.edge theme);
    let y = y +. 1. in
    let at = ref (x +. 12.) in
    List.iter (fun (key, what) ->
      Paint.text paint ~size:small ~color:(Theme.ink_3 theme) ~at:(!at, text_top ui ~size:small y line) key;
      at := !at +. mono_width small key +. 8.;
      Paint.text paint ~color:(Theme.ink_2 theme) ~at:(!at, text_top ui y line) what;
      at := !at +. mono_width ui.font_size what +. 8.) hints;
    let label = Printf.sprintf "%d on card" count in
    (* the sheet's tracking follows the last letter too: the ink ends that much before the 12 *)
    let label_w = Paint.cap_width paint label +. Paint.cap_tracking paint in
    let left = x +. w -. 12. -. label_w in
    Paint.cap paint ~at:(left, cap_y ui y line) ~color:(Theme.ink_2 theme) label;
    Paint.circle paint ~at:(left -. 11., y +. (line /. 2.)) ~radius:3. ~fill:theme.foreground ())

let inspector_message ui ~key message =
  lead_gap ui;
  let row = box ui ~w:Grow ~h:(Px (float ui.kit_row_height)) key in
  draw ui row (fun paint (x, y, w, h) ->
    Paint.circle paint ~at:(x +. 15., y +. (h /. 2.)) ~radius:3. ~fill:ui.theme.accent ();
    Paint.text paint ~at:(x +. 26., y +. float (label_y ui 0 ui.kit_row_height))
      ~color:(Theme.ink_2 ui.theme)
      (inspector_fit paint ~size:ui.font_size ~width:(w -. 38.) message))

let popup_with_related ui ?stroke ?max_height ?(dismiss_initial = true) ?(attached = false) ?(keep = []) ?(related = [])
    ~at:(x, y) ~width ~height label f =
  let key = key_of (current_seed ui) label in
  (* Build a popup before the body it shields. Previous popups arbitrate in
     [route]; this also shields a newly opened/dismissed popup's frame.  An [attached] popup (a
     submenu, built after the modal one) shields nothing of its own: it shares the modal's
     events, and only the modal one is dismissed, by a press outside it and every [keep] rectangle. *)
  ui.modal_in_frame <- true;
  if not attached then begin
    let belongs target = hit_within ui target key
      || List.exists (hit_within ui target) related in
    Int_table.filter_map_inplace (fun target signal ->
      if belongs target then Some signal else None) ui.signals;
    if Option.fold ~none:false ~some:(fun active -> not (belongs active)) ui.active then begin
      ui.cancelled <- ui.active_button :: ui.cancelled; ui.active <- None
    end;
    if ui.focus <> 0 && not (belongs ui.focus) then unfocus ui
  end;
  let slot = Table.find ui.table key in
  let rect = if slot >= 0 then ui.rx.(slot), ui.ry.(slot), ui.rw.(slot), ui.rh.(slot)
    else x, y, width, height in
  let dismissed = List.exists (function
    | Input.Key_pressed Input.Escape | Input.Focus_lost -> true
    | Input.Pointer_pressed (_, point) ->
        (slot >= 0 || dismiss_initial) && not (contains rect point)
        && not (List.exists (fun r -> contains r point) keep)
    | _ -> false) ui.frame_events in
  if dismissed && not attached then None else begin
    (* Popups live at the root wherever they are built, so a pane's hit
       area never clips them, and are moved last so they paint on top. *)
    let parents = ui.parents in
    ui.parents <- [0];
    let index = ui.count in
    Fun.protect ~finally:(fun () -> ui.parents <- parents) (fun () ->
      (* a window's 1-point edge is part of its box: its content starts inside it *)
      let result = panel_with ?stroke ?max_height ~padding:(if stroke = None then 0 else 1)
          ui ~x ~y ~width label f in
      ui.overlays <- index :: ui.overlays;
      if attached then ui.modal_extra <- key :: ui.modal_extra else ui.modal_key <- Some key;
      Some result)
  end

let popup ui ?stroke ?max_height ?dismiss_initial ?attached ~at ~width ~height label f =
  popup_with_related ui ?stroke ?max_height ?dismiss_initial ?attached ~at ~width ~height label f

(* A centered panel from last frame's height. *)
let modal ui ?(width = 320.) label f =
  let key = key_of (current_seed ui) label in
  let slot = Table.find ui.table key in
  if slot >= 0 then begin
    if not (Hashtbl.mem ui.modal_heights key) && Hashtbl.length ui.modal_heights >= 32 then begin
      (* ponytail: scan at most 32 heights on insertion; use LRU if this
         small working set ever needs a larger capacity. *)
      let oldest = Hashtbl.fold (fun key (_, seen) oldest -> match oldest with
        | Some (_, previous) when previous <= seen -> oldest
        | _ -> Some (key, seen)) ui.modal_heights None in
      Option.iter (fun (key, _) -> Hashtbl.remove ui.modal_heights key) oldest
    end;
    Hashtbl.replace ui.modal_heights key (ui.rh.(slot), ui.frame_number)
  end;
  let height = Option.fold ~none:0. ~some:fst (Hashtbl.find_opt ui.modal_heights key) in
  let x = Float.round (Float.max 0. ((ui.view_w -. width) /. 2.))
  and y = Float.round (Float.max 0. ((ui.view_h -. height) /. 2.)) in
  popup ui ~stroke:(Theme.border ui.theme) ~dismiss_initial:false ~at:(x, y)
    ~width ~height ~max_height:(Float.max 48. (ui.view_h -. 32.)) label f

(* A label: ink-2 capitals 12 from the left (a window's title), 4 points under the top of its parent. *)
let label ui text =
  let gap = if ui.b_last.(current_parent ui) < 0 then 4. else 0. in
  let row = box ui ~flags:none ~w:Grow ~h:(Px (gap +. float ui.kit_row_height)) text in
  let theme = ui.theme and shown = display text in
  draw ui row (fun paint (x, y, _, h) ->
    Paint.cap paint ~at:(x +. float side, cap_y ui (y +. gap) (h -. gap)) ~color:(Theme.ink_2 theme) shown)

(* A window's foot: a line-2 hairline under 4 points of space, then the keys in ink-3 each followed by
   what it does in ink-2, 8 points apart, and [right] (a key or a count) at the end. *)
let footer ui ?right hints =
  let row = box ui ~flags:none ~w:Grow ~h:(Px (5. +. float ui.kit_row_height)) "footer" in
  let theme = ui.theme and size = max 8 (ui.font_size - 2) in
  draw ui row (fun paint (x, y, w, h) ->
    Paint.fill paint ~x ~y:(y +. 4.) ~w ~h:1. (Theme.edge theme);
    let key_y = text_top ui ~size (y +. 5.) (h -. 5.) and hint_y = text_top ui (y +. 5.) (h -. 5.) in
    let pen = List.fold_left (fun pen (key, what) ->
      Paint.text paint ~size ~color:(Theme.ink_3 theme) ~at:(pen, key_y) key;
      let pen = pen +. Paint.text_width paint ~size key +. 8. in
      Paint.text paint ~color:(Theme.ink_2 theme) ~at:(pen, hint_y) what;
      pen +. Paint.text_width paint what +. 8.) (x +. float side) hints in
    ignore pen;
    Option.iter (fun text ->
      Paint.cap paint ~at:(x +. w -. float side -. Paint.cap_width paint text, key_y) text) right)

(* A message under a section: a 6-point dot (the accent for information, the error ink for [error]) and
   the text in ink-2 (or the error ink), wrapped to the width of the parent as it was laid out last
   frame, on lines of 20 points with 4 above and below: a message of one line is 28 high, of two 48. *)

(* A button is its text on a 20-point box (6 points of padding in a transparent 1-point edge):
   a fill on hover and press, an accent line inside the bottom edge with the keyboard.  The box
   starts 5 points in so that its text lines up with the labels at 12. *)
let button ui ?(bare = false)
    ?(icon = false) ?(at_end = false) ?ink text =
  let shown = display text in
  let measure = text_width ui in
  (* the box: a 1-point edge and 6 points of padding (4 when [bare]) round the text; an [icon] is the
     20-point control square with its glyph centred *)
  let width = if icon then 20. else measure shown +. (if bare then 10. else 14.) in
  let extent (x, y, w, h) =
    let bx = if at_end then x +. w -. 8. -. width else x +. 5. in
    bx, y +. 2., Float.max 1. (Float.min width (w -. 10.)), h -. 4. in
  let row = kit_row ui ~flags:(clickable lor focusable lor blocking lor tab_only lor focus_mark)
      ~hit:extent text in
  let signal = signal ui row in
  let theme = ui.theme and focus = focused ui row in
  draw ui row (fun paint rect ->
    let x, y, w, h = rect in
    let cx, cy, cw, ch = extent (x, y, w, h) in
    let ink = Option.value ink ~default:(if bare then Theme.ink_2 theme else theme.foreground) in
    paint_button_ground paint theme ~held:signal.held ~hovered:signal.hovered
        ~on:false ~primary:false (cx, cy, cw, ch);
    (* the keyboard's mark: the accent over the last row of the box, its whole width *)
    if focus then Paint.fill paint ~x:cx ~y:(cy +. ch -. 1.) ~w:cw ~h:1. theme.accent;
    let pad = if icon then Float.floor ((cw -. measure shown) /. 2.) else if bare then 5. else 7. in
    match icon, shown with
    | true, ("<" | ">" | "v" | "^") ->
        let direction = match shown with
          | "<" -> `Left | ">" -> `Right | "v" -> `Down | _ -> `Up in
        Paint.chevron paint ~at:(cx +. (cw /. 2.), cy +. (ch /. 2.)) direction ink
    | _ -> Paint.text paint ~at:(cx +. pad, text_top ui y h) ~color:ink shown);
  signal.clicked

(* A switch on its row: the label and the 28 x 14 switch at the end. *)
let toggle ui text value =
  let row = kit_row ui ~flags:(clickable lor focusable lor blocking lor tab_only)
      ~hit:(control_hit toggle_control) text in
  let signal = signal ui row in
  let value = if signal.clicked then not value else value in
  let theme = ui.theme and shown = display text in
  let hovered = signal.hovered in
  draw ui row (fun paint rect ->
    let (x, y, _, h) as bounds = ints rect in
    let cx, cy, _, _ = toggle_control bounds in
    if hovered then hover_row paint ui bounds;
    kit_text paint ~color:theme.foreground (x + side) (label_y ui y h) shown;
    paint_switch paint theme ~x:(float cx) ~y:(float cy) value);
  value

let previous_utf8 text index =
  let rec seek index =
    if index <= 0 then 0
    else if Char.code text.[index] land 0xc0 <> 0x80 then index
    else seek (index - 1) in
  if index <= 0 then 0 else seek (index - 1)

let next_utf8 text index =
  let length = String.length text in
  let rec seek index =
    if index >= length then length
    else if Char.code text.[index] land 0xc0 <> 0x80 then index
    else seek (index + 1) in
  if index >= length then length else seek (index + 1)

let text_caret_at ui ?size text x =
  match face ui size with
  | None -> String.length text
  | Some font ->
      let length = String.length text and density = float ui.density in
      let id = if length = 0 then 0 else font_id ui.atlas font in
      let rec seek index width =
        if index >= length then length else
          let decoded = String.get_utf_8_uchar text index in
          let code = Uchar.to_int (Uchar.utf_decode_uchar decoded) in
          let advance = match glyph ui.atlas font ~id ~density:ui.density code with
            | Some glyph -> float glyph.advance /. density
            | None -> 0. in
          if x < width +. (advance /. 2.) then index
          else seek (index + Uchar.utf_decode_length decoded) (width +. advance) in
      seek 0 0.

let numeric_character = function
  | '0' .. '9' | '+' | '-' | '.' | 'e' | 'E' -> true
  | _ -> false

let clipboard_command ~command = function
  | Input.Key_pressed (Input.KeyChar key) when command ->
      Some (Char.lowercase_ascii key)
  | _ -> None

type text_edit = { mutable text : string; mutable caret : int; mutable anchor : int }

(* A text starts being edited: the caret at its end, the selection from [anchor], and nothing of
   the text edited before it (scroll, undo stacks, a double click's selection unit). *)
let reset_edit ui key text ~anchor =
  ui.edit_focus <- key; ui.edit_value <- text;
  ui.edit_caret <- String.length text; ui.edit_anchor <- anchor;
  ui.edit_scroll_x <- 0.;
  ui.edit_undo <- []; ui.edit_redo <- []; ui.edit_group <- -1; ui.edit_unit <- None;
  ui.edit_column <- -1.

let load_text_edit ui key text =
  if ui.edit_focus <> key || ui.edit_value <> text then begin
    let left = ui.edit_focus, ui.edit_value, ui.edit_undo, ui.edit_redo in
    let back = List.find_opt (fun (k, v, _, _) -> k = key && v = text) ui.edit_shelf in
    let shelf = List.filter (fun entry -> Some entry <> back) ui.edit_shelf in
    ui.edit_shelf <- List.take 4
      (if ui.edit_undo = [] && ui.edit_redo = [] then shelf else left :: shelf);
    reset_edit ui key text ~anchor:(String.length text);
    Option.iter (fun (_, _, undo, redo) -> ui.edit_undo <- undo; ui.edit_redo <- redo) back
  end;
  { text; caret = ui.edit_caret; anchor = ui.edit_anchor }

let save_text_edit ui edit =
  ui.edit_value <- edit.text;
  ui.edit_caret <- edit.caret;
  ui.edit_anchor <- edit.anchor

let text_selection edit =
  min edit.caret edit.anchor, max edit.caret edit.anchor

let replace_text edit inserted =
  let start, stop = text_selection edit in
  edit.text <- String.sub edit.text 0 start ^ inserted ^
    String.sub edit.text stop (String.length edit.text - stop);
  edit.caret <- start + String.length inserted;
  edit.anchor <- edit.caret

(* Words as macOS counts them: letters, digits, underscores and every non-ASCII byte. *)
let word_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | c -> Char.code c >= 128

(* Option-Left: back over the separators, then over the word *)
let word_left text i =
  let i = ref i in
  while !i > 0 && not (word_char text.[!i - 1]) do decr i done;
  while !i > 0 && word_char text.[!i - 1] do decr i done;
  !i

(* Option-Right: over the separators, then to the end of the word *)
let word_right text i =
  let n = String.length text in
  let i = ref i in
  while !i < n && not (word_char text.[!i]) do incr i done;
  while !i < n && word_char text.[!i] do incr i done;
  !i

let line_start text i =
  let i = ref i in
  while !i > 0 && text.[!i - 1] <> '\n' do decr i done;
  !i

let line_end text i =
  let n = String.length text in
  let i = ref i in
  while !i < n && text.[!i] <> '\n' do incr i done;
  !i

(* ---- line and form edits of the text area ---- *)

(* the lines a selection touches, as the byte range [first, stop) without the last break *)
let line_block text a b =
  line_start text a, line_end text (if b > a && text.[b - 1] = '\n' then b - 1 else b)

(* the bytes [first, stop) of the edit's text replaced by [replacement]; a selection over the
   replacement afterwards keeps its direction *)
let replace_block edit first stop replacement =
  edit.text <- String.sub edit.text 0 first ^ replacement ^ String.sub edit.text stop (String.length edit.text - stop)
let keep_selection edit first last =
  if edit.caret >= edit.anchor then (edit.anchor <- first; edit.caret <- last)
  else (edit.anchor <- last; edit.caret <- first)

(* [block] with every line commented by [prefix], or with it taken off when each non-blank line
   has it (and the space after it); a comment goes in at the block's smallest indent *)
let toggle_comment prefix block =
  let lines = String.split_on_char '\n' block in
  let blank l = String.trim l = "" in
  let indent l = let i = ref 0 in while l.[!i] = ' ' || l.[!i] = '\t' do incr i done; !i in
  let body = List.filter (fun l -> not (blank l)) lines in
  let n = String.length prefix in
  let commented l = String.starts_with ~prefix (String.sub l (indent l) (String.length l - indent l)) in
  String.concat "\n" (List.map (fun l ->
    if blank l then l else
    let i = indent l in
    if body <> [] && List.for_all commented body then
      let n = if i + n < String.length l && l.[i + n] = ' ' then n + 1 else n in
      String.sub l 0 i ^ String.sub l (i + n) (String.length l - i - n)
    else
      let col = List.fold_left (fun m l -> min m (indent l)) max_int body in
      String.sub l 0 col ^ prefix ^ " " ^ String.sub l col (String.length l - col)) lines)

(* the innermost bracket pair that holds [a, b) and is larger than it, as (open, past the close) *)
let enclosing_pair pairs a b =
  List.fold_left (fun best (o, c) ->
    if o <= a && b <= c + 1 && (o, c + 1) <> (a, b) then
      match best with Some (bo, bc) when bc - bo <= c + 1 - o -> best | _ -> Some (o, c + 1)
    else best) None pairs

(* the caret across the bracket beside it, else out to the opener of the pair around it *)
let bracket_jump pairs caret =
  match List.find_map (fun (o, c) ->
      if o = caret then Some (c + 1) else if c + 1 = caret then Some o else None) pairs with
  | Some _ as jump -> jump
  | None -> Option.map fst (enclosing_pair pairs caret caret)

(* what a double click selects at [i]: the word there, else the run of blanks, else the one
   punctuation character; nothing on a line break *)
let word_at text i =
  let n = String.length text in
  let class_of c = if word_char c then 1 else if c = ' ' || c = '\t' then 2
    else if c = '\n' || c = '\r' then 0 else 3 in
  let pick = if i < n && class_of text.[i] <> 0 then Some i
    else if i > 0 && class_of text.[i - 1] <> 0 then Some (i - 1) else None in
  match pick with
  | None -> i, i
  | Some p when class_of text.[p] = 3 -> p, p + 1
  | Some p ->
      let k = class_of text.[p] in
      let a = ref p and b = ref (p + 1) in
      while !a > 0 && class_of text.[!a - 1] = k do decr a done;
      while !b < n && class_of text.[!b] = k do incr b done;
      !a, !b

(* ---- undo inside the focused text: snapshots before each change, consecutive typing as one *)

let rec bounded n = function x :: rest when n > 0 -> x :: bounded (n - 1) rest | _ -> []

let remember ui edit ~typing =
  if not (typing && edit.caret = edit.anchor && ui.edit_group = edit.caret) then begin
    ui.edit_undo <- bounded 200 ((edit.text, edit.caret, edit.anchor) :: ui.edit_undo);
    ui.edit_redo <- []
  end;
  ui.edit_group <- -1

let undo_text ui edit ~redo =
  match if redo then ui.edit_redo else ui.edit_undo with
  | [] -> false
  | (text, caret, anchor) :: rest ->
      let now = edit.text, edit.caret, edit.anchor in
      if redo then (ui.edit_redo <- rest; ui.edit_undo <- now :: ui.edit_undo)
      else (ui.edit_undo <- rest; ui.edit_redo <- now :: ui.edit_redo);
      edit.text <- text; edit.caret <- caret; edit.anchor <- anchor;
      ui.edit_group <- -1;
      true

(* an exhausted stack hands the key to the host's own undo ([passed_undo]) *)
let pass_undo ui edit ~redo =
  undo_text ui edit ~redo || (ui.passed_undo <- Some (if redo then `Redo else `Undo); false)

(* A press places the caret (Shift extends the selection), a double click selects the word
   there, a triple click the [line_of] range; the drag that follows extends by the same unit. *)
let press_select ui edit ~shift ~clicks ~line_of at =
  if clicks < 2 then begin
    edit.caret <- at;
    if not shift then edit.anchor <- at;
    ui.edit_unit <- None
  end else begin
    let a, b = if clicks = 2 then word_at edit.text at else line_of at in
    edit.anchor <- a; edit.caret <- b;
    ui.edit_unit <- Some (a, b, clicks)
  end

let drag_select ui edit ~line_of at =
  match ui.edit_unit with
  | None -> edit.caret <- at
  | Some (a, b, clicks) ->
      let c, d = if clicks = 2 then word_at edit.text at else line_of at in
      if c < a then (edit.anchor <- b; edit.caret <- c)
      else (edit.anchor <- a; edit.caret <- max b d)

let point_text_caret ui ?size edit signal ~shift ~x ~right =
  let at (px, _) = text_caret_at ui ?size edit.text
    (Float.max 0. (Float.min right px -. x +. ui.edit_scroll_x)) in
  let line_of _ = 0, String.length edit.text in
  if signal.pressed then
    press_select ui edit ~shift ~clicks:signal.clicks ~line_of (at signal.press_point);
  if signal.dragging || (signal.held && signal.pointer <> signal.press_point) then begin
    let px = fst signal.pointer in
    if px < x then ui.edit_scroll_x <- Float.max 0.
      (ui.edit_scroll_x -. Float.max 1. (Float.min 24. ((x -. px) *. 0.25)))
    else if px > right then ui.edit_scroll_x <- ui.edit_scroll_x +.
      Float.max 1. (Float.min 24. ((px -. right) *. 0.25));
    drag_select ui edit ~line_of (at signal.pointer)
  end

(* The keys every text widget shares, as macOS binds them: Command-A/C/X/V, Command-Z and
   Shift-Command-Z (or Command-Y) undo and redo, Option-arrows move by words, Command-arrows
   and Home/End by lines, Option-Backspace/Delete take a word, Command-Backspace/Delete the
   line to the caret.  Returns whether the text changed. *)
let edit_text_event ?(multiline = false) ui edit ~accept ~modifiers event =
  let command = command_modifiers modifiers
  and shift = List.mem Input.Shift modifiers
  and alt = List.mem Input.Alt modifiers in
  let selected () = edit.caret <> edit.anchor in
  let move target =
    edit.caret <- target;
    if not shift then edit.anchor <- target in
  (* the selection, else from the caret to [target] *)
  let delete_to target =
    if (not (selected ())) && target = edit.caret then false else begin
      remember ui edit ~typing:false;
      if not (selected ()) then edit.anchor <- target;
      replace_text edit ""; true
    end in
  match event with
  | event when clipboard_command ~command event = Some 'a' ->
      edit.anchor <- 0; edit.caret <- String.length edit.text; false
  | event when clipboard_command ~command event = Some 'c' ->
      let start, stop = text_selection edit in
      ignore (Clipboard.set_text (if selected () then
        String.sub edit.text start (stop - start) else edit.text)); false
  | event when clipboard_command ~command event = Some 'x' ->
      let start, stop = text_selection edit in
      let copied = if selected () then
        String.sub edit.text start (stop - start) else edit.text in
      if Clipboard.set_text copied = Ok () then begin
        remember ui edit ~typing:false;
        if selected () then replace_text edit "" else begin
          edit.text <- ""; edit.caret <- 0; edit.anchor <- 0
        end;
        true
      end else false
  | event when clipboard_command ~command event = Some 'v' ->
      (* a field of one line takes a copied line without its break *)
      let one_line text = if multiline then text
        else String.of_seq (Seq.filter (fun c -> c <> '\n' && c <> '\r') (String.to_seq text)) in
      (match Result.map one_line (Clipboard.get_text ()) with
       | Ok text when accept text -> remember ui edit ~typing:false; replace_text edit text; true
       | Ok _ | Error _ -> false)
  | event when clipboard_command ~command event = Some 'z' -> pass_undo ui edit ~redo:shift
  | event when clipboard_command ~command event = Some 'y' -> pass_undo ui edit ~redo:true
  | Input.Text_input text when accept text ->
      let typing = not (String.exists (function ' ' | '\t' | '\n' -> true | _ -> false) text) in
      remember ui edit ~typing;
      replace_text edit text;
      if typing then ui.edit_group <- edit.caret;
      true
  | Input.Key_pressed Input.Backspace ->
      delete_to (if command then line_start edit.text edit.caret
        else if alt then word_left edit.text edit.caret
        else previous_utf8 edit.text edit.caret)
  | Input.Key_pressed Input.Delete ->
      delete_to (if command then line_end edit.text edit.caret
        else if alt then word_right edit.text edit.caret
        else next_utf8 edit.text edit.caret)
  | Input.Key_pressed Input.ArrowLeft ->
      move (if command then line_start edit.text edit.caret
        else if alt then word_left edit.text edit.caret
        else if selected () && not shift then fst (text_selection edit)
        else previous_utf8 edit.text edit.caret);
      false
  | Input.Key_pressed Input.ArrowRight ->
      move (if command then line_end edit.text edit.caret
        else if alt then word_right edit.text edit.caret
        else if selected () && not shift then snd (text_selection edit)
        else next_utf8 edit.text edit.caret);
      false
  | Input.Key_pressed Input.Home -> move (line_start edit.text edit.caret); false
  | Input.Key_pressed Input.End -> move (line_end edit.text edit.caret); false
  | _ -> false

let paint_text_edit paint ?size ?(right = false) ?(inset = 2.) ?(caret = true) ~control:(cx, cy, cw, ch) ~y ~composition edit =
  let ui = paint.owner in
  let theme = ui.theme in
  let width text = Paint.text_width paint ?size text in
  let before = String.sub edit.text 0 edit.caret in
  let visible = float (max 1 (cw - 4)) in
  let scroll = Float.max 0. (Float.min ui.edit_scroll_x
    (Float.max 0. (width edit.text -. visible))) in
  let caret_width = width before in
  let scroll = if caret_width < scroll then caret_width
    else if caret_width -. scroll > visible then caret_width -. visible
    else scroll in
  ui.edit_scroll_x <- scroll;
  (* a number keeps its right edge while it is edited, until it outgrows the field *)
  let origin = if right && width edit.text <= visible
    then float (cx + cw - 2) -. width edit.text else float cx +. inset in
  let text_x = origin -. scroll in
  let caret_x = text_x +. caret_width in
  Paint.input_region paint ~x:(float cx) ~y:(float cy) ~w:(float cw)
    ~h:(float ch) ~focused:true ~cursor:(caret_x -. float cx) ();
  let previous_clip = paint.clip_rect in
  paint.clip_rect <- intersect previous_clip
    (float cx, float cy, float cw, float ch);
  if edit.caret <> edit.anchor then begin
    let start, stop = text_selection edit in
    Paint.fill paint
      ~x:(text_x +. width (String.sub edit.text 0 start))
      ~y:(float cy +. 0.5)
      ~w:(width (String.sub edit.text start (stop - start)))
      ~h:(float (max 1 (ch - 2))) (Theme.tint theme)
  end;
  if composition = "" then Paint.text paint ?size ~at:(text_x, float y) edit.text
  else begin
    Paint.text paint ?size ~at:(text_x, float y) before;
    Paint.text paint ?size ~at:(caret_x, float y) composition;
    Paint.text paint ?size ~at:(caret_x +. width composition, float y)
      (String.sub edit.text edit.caret
        (String.length edit.text - edit.caret))
  end;
  (* the caret is the line of the text it sits in: 14 of a 20-point field at body size *)
  let caret_y, caret_h = match size with
    | None -> float cy +. 2.5, 14.
    | Some size -> let h = float size *. 1.1 in float cy +. ((float ch -. h) /. 2.), h in
  if caret then Paint.fill paint ~x:caret_x ~y:caret_y ~w:1. ~h:caret_h theme.accent;
  paint.clip_rect <- previous_clip

(* Numeric label editing shared by float and integer sliders. The retained
   state records validity; [parse] validates the edit buffer. *)
let rec numeric_editor ?size ?control ?(click_to_edit = false)
    ?(edit_request = false) ?(alt_to_edit = false) ?(align_right = false)
    ?(accept = String.for_all numeric_character) ui row signal ~keys ~current ~parse =
  let enter = function
    | Input.Key_pressed Input.Enter, modifiers -> not (command_modifiers modifiers)
    | _ -> false in
  let bounds = ints (rect ui row) in
  let control_bounds = Option.value ~default:(value_control bounds) control in
  let control = control_bounds in
  let inside_control point = contains (floats control) point in
  let editing = text_state ui row in
  let state = state ui row ~default:1 in
  let set text ~valid =
    set_text_state ui row (Some text);
    set_state ui row (if valid then 1 else 0) in
  let finish () = set_text_state ui row None; set_state ui row 1 in
  match editing with
  | Some text when not (focused ui row) || (signal.pressed
      && not (inside_control signal.press_point)) ->
      (* Focus moved away: commit a valid value, drop an invalid one. *)
      finish ();
      if focused ui row then unfocus ui;
      (match parse text with Some value -> Some value | None -> None), false
  | Some text ->
      let edit = load_text_edit ui row.box_key text in
      let committed = ref None and state = ref state
      and cancelled = ref false in
      let (cx, _, cw, _) = control in
      let left = if align_right then
          Float.max (float (cx + 2)) (float (cx + cw - 2) -. text_width ui ?size edit.text)
        else float (cx + 2) in
      point_text_caret ui ?size edit signal ~shift:(press_shift ui row)
        ~x:left ~right:(float (cx + cw - 2));
      List.iter (fun ((event : Input.event), modifiers) -> match event with
        | Input.Key_pressed Input.Enter when not (command_modifiers modifiers) ->
            (match parse edit.text with
             | Some value -> committed := Some value
             | None -> state := 0)
        | Input.Key_pressed Input.Escape -> cancelled := true
        | event ->
            if edit_text_event ui edit ~modifiers
                ~accept event then
              state := if parse edit.text <> None then 1 else 0) keys;
      if !cancelled then (finish (); unfocus ui; None, false)
      else (match !committed with
        | Some value -> finish (); unfocus ui; Some value, false
        | None ->
            save_text_edit ui edit;
            set edit.text ~valid:(!state land 1 <> 0);
            None, true)
  | None ->
      let click = click_to_edit && signal.clicked
        && abs_float (fst signal.release_point -. fst signal.press_point) < 4.
        && abs_float (snd signal.release_point -. snd signal.press_point) < 4. in
      if edit_request || (alt_to_edit && signal.clicked
          && List.mem Input.Alt (press_keys ui row)) || click
          || (signal.double_clicked && not (inside_control signal.press_point))
          || (focused ui row && List.exists enter keys) then begin
        let text = current () in
        set text ~valid:true;
        focus ui row;
        reset_edit ui row.box_key text ~anchor:0;
        let rec after_enter = function
          | event :: rest when enter event -> rest
          | _ :: rest -> after_enter rest | [] -> [] in
        let keys = if signal.double_clicked || signal.clicked then keys else after_enter keys in
        numeric_editor ?size ~control:control_bounds ~click_to_edit ~align_right ~accept ui row
          { signal with keys = List.map fst keys; pressed = false;
            clicked = false; double_clicked = false } ~keys ~current ~parse
      end else None, false

let value_field ui ~at ~w ~h ?size ?display ?fraction ?slide ?scrub ?(left = false)
    ?(edit = false) ?lead ?trail ?line ?(bare = false) ?placeholder ?tracking ~valid label value =
  let box = box ui ~flags:(clickable lor tab_stop lor blocking lor clip)
    ~at ~w:(Px w) ~h:(Px h) label in
  let signal = signal ui box in
  let bounds = ints (rect ui box) in
  let slider = Option.is_some slide || Option.is_some scrub in
  let typed, editing = numeric_editor ?size ~control:bounds
    ~click_to_edit:(not slider) ~edit_request:(edit || (slider && signal.double_clicked)) ~alt_to_edit:slider
    ~accept:(fun _ -> true) ui box signal ~keys:(key_events ui box)
    ~current:(fun () -> value) ~parse:(fun text -> if valid text then Some text else None) in
  if editing then ui.b_flags.(box.index) <- clickable lor focusable lor blocking lor clip;
  let origin = if signal.pressed then begin
      ui.scrub_origin <- Some (box.box_key, value); value
    end else match ui.scrub_origin with
      | Some (key, origin) when key = box.box_key -> origin | _ -> value in
  let value = match typed with
    | Some text -> text
    | None when not editing && (signal.pressed || signal.held || signal.released)
        && signal.button = Some Input.LeftButton
        && not (List.mem Input.Alt (press_keys ui box)) ->
        let pointer = if signal.released then signal.release_point else signal.pointer in
        (match slide, scrub with
         | Some slide, _ -> slide (fraction_at bounds (fst pointer))
         | None, Some scrub -> scrub origin (fst pointer -. fst signal.press_point)
             (List.mem Input.Shift (press_keys ui box))
         | None, None -> value)
    | _ -> value in
  if not signal.held && Option.fold ~none:false
      ~some:(fun (key, _) -> key = box.box_key) ui.scrub_origin then ui.scrub_origin <- None;
  let buffer = text_state ui box in
  let edit = if editing && focused ui box then
      load_text_edit ui box.box_key (Option.value ~default:value buffer)
    else { text = value; caret = 0; anchor = 0 } in
  let composition = ui.composition and focus = focused ui box in
  let display = if typed <> None || signal.held || signal.released then value
    else Option.value ~default:value display in
  let invalid = state ui box ~default:1 land 1 = 0 in
  draw ui box (fun paint rect ->
    let (x, y, w, h) as bounds = ints rect in
    let previous_clip = paint.clip_rect in
    paint.clip_rect <- intersect previous_clip
      (float x, float y, float w, float h);
    if invalid || editing || not bare then
      underline paint bounds (if invalid then Theme.invalid
        else if editing then ui.theme.accent
        else Option.value line ~default:(Theme.edge ui.theme));
    (* the content is centred in the 19 points above the hairline: a body line starts half a point up *)
    let text_y = match size with
      | Some size -> float (y + max 0 ((h - size) / 2))
      | None -> float y +. (float (h - 1 - ui.font_size) /. 2.) -. 0.5 in
    if editing && focus then paint_text_edit paint ?size ~control:bounds ~y:(y + max 0 ((h - Option.value ~default:ui.font_size size) / 2)) ~composition edit
    else begin
      (* the 2-point line under the value is its position in the soft range *)
      Option.iter (fun f -> Paint.fill paint ~x:(float x) ~y:(float (y + h - 3))
        ~w:(float w *. Float.max 0. (Float.min 1. f)) ~h:2. (Theme.ink_2 ui.theme)) fraction;
      match lead, trail with
      | None, None when display = "" && placeholder <> None ->
          Paint.text paint ?size ~color:(Theme.ink_3 ui.theme)
            ~at:(float (x + 2), text_y) (Option.get placeholder)
      | None, None ->
          let width = Paint.text_width paint ?size display in
          Paint.text paint ?size ?tracking ~at:((if left then float (x + 2)
            else float (x + w - 2) -. width), text_y) display
      | _ ->
          (* a lead (the expression's ƒ) and a trail (its live value) frame the value: 6 between *)
          let draw_part ?(right = false) (text, color) =
            let width = Paint.text_width paint ?size text in
            Paint.text paint ?size ~color
              ~at:((if right then float (x + w - 2) -. width else float (x + 2)), text_y) text;
            width in
          let lead_w = Option.fold ~none:0. ~some:(fun l -> draw_part l +. 6.) lead in
          let trail_w = Option.fold ~none:0. ~some:(fun t -> draw_part ~right:true t +. 6.) trail in
          (* with no lead the value sits against its trail (a unit: [35 mm]) *)
          let shown = inspector_fit paint ~size:(Option.value ~default:ui.font_size size)
            ~width:(float w -. 4. -. lead_w -. trail_w) display in
          let value_x = if lead = None && not left
            then float (x + w - 2) -. trail_w -. Paint.text_width paint ?size shown
            else float (x + 2) +. lead_w in
          Paint.text paint ?size ~color:ui.theme.foreground ~at:(value_x, text_y) shown
    end;
    paint.clip_rect <- previous_clip);
  value, editing

type head_action = { caption : string; keycap : string; active : bool; usable : bool }
type head = { renamed : string; chosen : int option; reset_pressed : bool }

(* The head block of a panel, from the kit's inspector sheet: 12 above, a chips row (the kind in
   ink-2, a badge in the accent, an index at the right in ink-3), the name at the display size
   (edited in place when [rename] says which names are valid), one detail line in ink-2, a row of
   text buttons with their keys and a trailing [reset] button, 8 below and a line-2 hairline. *)
let inspector_header ui ~key ?kind ?badge ?index ?rename ?(actions = []) ?reset ?(reset_enabled = true) ~title ~detail () =
  let win = ui.kit_window in
  (* a window's head (windows.html): the 20-point title and one detail line, 8 above, no chips,
     buttons or hairline *)
  let display = ui.font_size * (if win then Theme.title_size else Theme.display_size) / Theme.font_size in
  let small = max 8 (ui.font_size - 2) in
  let line = float ui.kit_row_height and title_h = if win then float ui.kit_row_height else float (display + 8) in
  let width = inspector_width ui in
  let chips = not win && (kind <> None || badge <> None || index <> None) in
  let actions = if win then [] else actions and reset = if win then None else reset in
  let buttons = actions <> [] || reset <> None in
  let text_x = if win then 12. else 8. in
  let y_title = if win then 8. else if chips then 12. +. line +. 4. else 12. in
  let y_detail = y_title +. title_h +. (if win then 0. else 4.) in
  let y_buttons = y_detail +. line +. 8. in
  let height = if win then y_detail +. line
    else (if buttons then y_buttons +. 20. else y_detail +. line) +. 8. +. 1. in
  let header = box ui ~flags:clip ~w:Grow ~h:(Px height) key in
  let theme = ui.theme in
  let measure ?(size = ui.font_size) text = mono_width size text in
  let title', editing = match rename with
    | Some valid ->
        within ui header (fun () ->
          value_field ui ~at:(text_x -. 2., if win then y_title -. 1. else y_title -. 2.)
            ~w:(width -. (2. *. (text_x -. 2.))) ~h:title_h ~size:display
            ~left:true ~bare:true ~tracking:(-0.01 *. float display) ~valid (key ^ "-name") title)
    | None -> title, false in
  (* the buttons: a box each, in a row from 8 with 4 between; [reset] is bare at the right *)
  let pressed = ref None in
  let button index ~label ~hint ~on ~dim ~enabled ~x ~w ~pad =
    let b = within ui header (fun () ->
      box ui ~flags:(if enabled then clickable lor tab_stop else none) ~at:(x, y_buttons)
        ~w:(Px w) ~h:(Px 20.) (Printf.sprintf "%s-button-%d" key index)) in
    let signal = signal ui b in
    if enabled && signal.clicked then pressed := Some index;
    draw ui b (fun paint ((x, y, _, h) as rect) ->
      paint_button_ground paint theme ~held:(enabled && signal.held) ~hovered:(enabled && signal.hovered)
        ~on ~primary:false rect;
      Paint.text paint ~at:(x +. 1. +. pad, text_top ui y h)
        ~color:(if not enabled then Theme.ink_3 theme
                else if dim then Theme.ink_2 theme else theme.foreground) label;
      if hint <> "" then
        Paint.text paint ~size:small
          ~at:(x +. 1. +. pad +. mono_width ui.font_size label +. 6., text_top ui ~size:small y h)
          ~color:(Theme.ink_3 theme) hint) in
  let place = ref 8. in
  List.iteri (fun index (a : head_action) ->
    let w = 14. +. measure a.caption +. (if a.keycap = "" then 0. else 6. +. measure ~size:small a.keycap) in
    button index ~label:a.caption ~hint:a.keycap ~on:a.active ~dim:false ~enabled:a.usable ~x:!place ~w ~pad:6.;
    place := !place +. w +. 4.) actions;
  let reset_pressed = ref false in
  Option.iter (fun label ->
    let w = 10. +. measure label in
    let before = !pressed in
    button (-1) ~label ~hint:"" ~on:false ~dim:true ~enabled:reset_enabled ~x:(width -. 8. -. w) ~w ~pad:4.;
    if !pressed <> before then (pressed := before; reset_pressed := true)) reset;
  draw ui header (fun paint (x, y, w, h) ->
    if not win then Paint.fill paint ~x ~y:(y +. h -. 1.) ~w ~h:1. (Theme.edge theme);
    if chips then begin
      let at = y +. 12. +. text_top ui ~size:small 0. line in
      let cx = ref (x +. 8.) in
      (* the kind gives way when the index and badge need the room *)
      let per = (float small /. 2.) +. (0.08 *. float small) in
      let taken = Option.fold ~none:0. ~some:(fun t -> mono_width small t +. 6.) index
        +. Option.fold ~none:0. ~some:(fun t -> mono_width ~tracking:(0.08 *. float small) small t +. 6.) badge in
      Option.iter (fun text ->
        let room = int_of_float ((w -. 16. -. taken) /. per) in
        let text = if String.length text <= room then text
          else if room <= 1 then "" else String.sub text 0 (room - 1) ^ "." in
        Paint.cap paint ~at:(!cx, at) text;
        (* the glyphs run a little wider than the design's metrics: never closer than 6 *)
        cx := !cx +. Float.max (mono_width ~tracking:(0.08 *. float small) small text)
                (Paint.cap_width paint text) +. 6.) kind;
      Option.iter (fun text -> Paint.cap paint ~at:(!cx, at) ~color:theme.accent text) badge;
      (* the sheet's 0.04em tracking follows every letter, the last too, so it ends 8 from the edge *)
      Option.iter (fun text ->
        let tracking = 0.04 *. float small in
        let count = ref 0 in
        String.iter (fun c -> if Char.code c land 0xc0 <> 0x80 then incr count) text;
        Paint.text paint ~size:small ~tracking ~color:(Theme.ink_3 theme)
          ~at:(x +. w -. 8. -. Paint.text_width paint ~size:small text -. (tracking *. float !count), at) text) index
    end;
    if rename = None then
      Paint.text paint ~at:(x +. text_x,
          y +. y_title +. (if win then text_top ui ~size:display 0. title_h else 2.))
        ~size:display ~tracking:(-0.01 *. float display) ~color:theme.foreground
        (inspector_fit paint ~size:display ~width:(w -. (2. *. text_x)) title);
    Paint.text paint ~at:(x +. text_x, y +. y_detail +. text_top ui 0. line) ~color:(Theme.ink_2 theme)
      (inspector_fit paint ~size:ui.font_size ~width:(w -. (2. *. text_x)) detail));
  ignore editing;
  { renamed = title'; chosen = !pressed; reset_pressed = !reset_pressed }

let text_field ui text value =
  let row = kit_row ui ~flags:(clickable lor focusable lor blocking lor focus_mark)
      ~hit:(control_hit value_control) text in
  let signal = signal ui row in
  let focused = focused ui row in
  let edit = if focused then load_text_edit ui row.box_key value else
    let end_ = String.length value in
    { text = value; caret = end_; anchor = end_ } in
  if focused then begin
    let (cx, _, cw, _) = value_control (ints (rect ui row)) in
    point_text_caret ui edit signal ~shift:(press_shift ui row)
      ~x:(float (cx + 2)) ~right:(float (cx + cw - 2));
    List.iter (fun (event, modifiers) -> ignore (edit_text_event ui edit ~modifiers
      ~accept:(fun _ -> true) event)) (key_events ui row);
    save_text_edit ui edit
  end;
  let value = edit.text in
  let theme = ui.theme and shown = display text and composition = ui.composition in
  let hovered = signal.hovered in
  (* the I-beam over the value, as in any text field *)
  (let (px, py) = ui.pointer in
   let (cx, cy, cw, ch) = value_control (ints (rect ui row)) in
   if hovered && px >= float cx && px < float (cx + cw)
      && py >= float cy && py < float (cy + ch) then request_cursor ui `Text);
  draw ui row (fun paint rect ->
    let (x, y, _, h) as bounds = ints rect in
    let (cx, cy, cw, ch) as control = value_control bounds in
    if hovered then hover_row paint ui bounds;
    kit_text paint ~color:(Theme.ink_2 theme) (x + side) (label_y ui y h) shown;
    underline paint control (if focused then theme.accent else Theme.edge theme);
    if focused then paint_text_edit paint ~control ~y:(label_y ui y h)
      ~composition edit
    else begin
      Paint.input_region paint ~x:(float cx) ~y:(float cy) ~w:(float cw)
        ~h:(float ch) ~focused:false ();
      kit_text paint ~color:theme.foreground (cx + 2) (label_y ui y h) value
    end);
  value

(* A search field at a place: [text_field]'s editing, a hairline for the control and its
   [placeholder] while empty; Enter, Shift-Enter, Down and Up answer a step, Escape a close. *)
let number_field ui ~at ~w ~h ?size ?display ?edit ?left ?trail ~kind ?range ?valid key text =
  let module N = Param.Number in
  let valid = Option.value valid ~default:(N.valid kind) in
  fst (value_field ui ~at ~w ~h ?size ?display ?fraction:(Option.bind range (fun r -> N.fraction r text))
    ~scrub:(N.scrub kind ?range) ?edit ?left ?trail ~valid key text)

let line_field ui ~at ~w ~h ?(grab = false) ?(placeholder = "") label value =
  let field = box ui ~flags:(clickable lor focusable lor blocking lor focus_mark) ~at ~w:(Px w) ~h:(Px h) label in
  if grab then focus ui field;
  let signal = signal ui field in
  let focused = focused ui field in
  let edit = if focused then load_text_edit ui field.box_key value else
    { text = value; caret = String.length value; anchor = String.length value } in
  let step = ref `None in
  if focused then begin
    let (fx, _, fw, _) = ints (rect ui field) in
    point_text_caret ui edit signal ~shift:(press_shift ui field) ~x:(float (fx + 2)) ~right:(float (fx + fw - 2));
    List.iter (fun ((event : Input.event), modifiers) -> match event with
      | Input.Key_pressed Input.Enter when not (command_modifiers modifiers) ->
          step := if List.mem Input.Shift modifiers then `Previous else `Next
      | Input.Key_pressed Input.ArrowDown -> step := `Next
      | Input.Key_pressed Input.ArrowUp -> step := `Previous
      | Input.Key_pressed Input.Escape -> step := `Close
      | event -> ignore (edit_text_event ui edit ~modifiers ~accept:(fun _ -> true) event)) (key_events ui field);
    save_text_edit ui edit
  end;
  if signal.hovered then request_cursor ui `Text;
  let theme = ui.theme and composition = ui.composition in
  draw ui field (fun paint rect ->
    let (x, y, w, h) = ints rect in
    let control = x, y, w, h in
    underline paint control (if focused then theme.accent else Theme.edge theme);
    if focused then paint_text_edit paint ~control ~y:(label_y ui y h) ~composition edit
    else kit_text paint ~color:(if edit.text = "" then Theme.ink_3 theme else theme.foreground)
      (x + 2) (label_y ui y h) (if edit.text = "" then placeholder else edit.text));
  edit.text, !step

(* Multiline text: the same focus, IME composition, clipboard and edit
   events as [text_field] ([edit_text_event], the [ui.edit_*] retained
   state); added are Enter, Tab (two spaces), the vertical keys and row-scoped Home/End,
   and the pointer maps to (row, column).  With [wrap] a long line continues on the next row
   after a fixed number of Unicode code points.  ponytail: rows are
   found per frame (O(text)), only visible rows are drawn. *)

(* The rows of a text: (start, stop, logical line) with [stop] exclusive and before the newline.
   Without [cols] a row is a line; with it a line continues every [cols] characters. *)
let text_rows ?cols text =
  let n = String.length text in
  let rows = ref [] in
  let rec line start logical =
    let stop = match String.index_from_opt text start '\n' with Some i -> i | None -> n in
    let rec chunk from = match cols with
      | None -> rows := (from, stop, logical) :: !rows
      | Some cols ->
          let rec advance i k =
            if k = 0 || i >= stop then i
            else advance (i + Uchar.utf_decode_length (String.get_utf_8_uchar text i)) (k - 1) in
          let upto = advance from (max 1 cols) in
          if upto >= stop then rows := (from, stop, logical) :: !rows
          else (rows := (from, upto, logical) :: !rows; chunk upto) in
    chunk start;
    if stop < n then line (stop + 1) (logical + 1) in
  line 0 0;
  Array.of_list (List.rev !rows)

(* the row holding byte [index] *)
let row_at rows index =
  let start i = let s, _, _ = rows.(i) in s in
  let rec seek low high =
    if low >= high then low else
      let middle = (low + high + 1) / 2 in
      if start middle <= index then seek middle high else seek low (middle - 1) in
  seek 0 (Array.length rows - 1)

let context_clicked (signal : signal) =
  let (px, py), (rx, ry) = signal.press_point, signal.release_point in
  signal.released && signal.button = Some Input.RightButton
  && ((px -. rx) *. (px -. rx)) +. ((py -. ry) *. (py -. ry)) < 16.

let context_at ui box =
  Int_table.fold (fun target (value : accumulator) found ->
    if found = None && hit_within ui target (key box) && value.released && value.button = Some Input.RightButton
       && Float.hypot (fst value.press_point -. fst value.release_point)
            (snd value.press_point -. snd value.release_point) < 4.
    then Some value.release_point else found) ui.signals None

(* What a code editor knows about its text: colours by byte span, matching bracket pairs, the
   indentation of a new line, the brackets typed in pairs, ranked completions for the token at
   the caret, a description of the token under the pointer, and the numeric literals a drag
   changes.  A host supplies it; the widget stays language-free. *)
type completion = {
  replace : int * int;  (* the byte span [insert] replaces *)
  insert : string;
  label : string;  (* the row *)
  detail : string;  (* the row's right column: a type, a category *)
  doc : string;  (* one line under the rows while the row is selected *)
}

type language = {
  colorize : string -> (int * int * Rays_math.Color.t) list;  (* sorted, non-overlapping byte spans *)
  brackets : string -> (int * int) list;  (* (open, close) byte positions of matched pairs *)
  indent : string -> int -> string;  (* the indentation of a line broken at this byte *)
  pairs : (char * char) list;  (* typing the first inserts both; the second skips over itself *)
  complete : string -> int -> completion list;  (* ranked, for the token ending at the caret *)
  describe : string -> int -> (int * int * string) option;  (* the token at a byte and its doc *)
  number_at : string -> int -> (int * int) option;  (* the numeric literal at a byte *)
  context_at : string -> int -> [ `Code | `String | `Comment ];  (* what a caret at a byte is in *)
  comment : string;  (* what starts a line comment *)
  rewrite : (string -> int -> string * int) option;
  (* [rewrite text caret] after an edit: the text as the language keeps it (parinfer), and
     where [caret] lands in it *)
}

(* A box at the root, laid out and painted after every pane (like a tooltip), so a popup under
   a caret is never clipped by its pane. *)
let overlay_box ui ?flags ~at ~w ~h label =
  at_root ui (fun () ->
    let overlay = box ui ?flags ~w:(Px w) ~h:(Px h) ~at label in
    ui.overlays <- overlay.index :: ui.overlays;
    overlay)

(* A number literal dragged [dx] points: a float moves a tenth of its last decimal place per
   point (Shift: ten times that) and keeps one more decimal; an integer moves one per five
   points (Shift: one per point). *)
let scrubbed literal dx ~coarse =
  match String.index_opt literal '.', int_of_string_opt literal with
  | None, Some v -> string_of_int (v + int_of_float (Float.round (dx /. (if coarse then 1. else 5.))))
  | Some dot, _ ->
      let decimals = String.length literal - dot - 1 in
      let step = (10. ** float (- (decimals + 1))) *. (if coarse then 10. else 1.) in
      let v = (match float_of_string_opt literal with Some v -> v | None -> 0.) +. (dx *. step) in
      let s = Printf.sprintf "%.*f" (decimals + 1) v in
      let n = ref (String.length s) in
      while !n > 0 && s.[!n - 1] = '0' do decr n done;
      if !n > 0 && s.[!n - 1] = '.' then String.sub s 0 (!n + 1) else String.sub s 0 !n
  | None, None -> literal

(* the text area's painting: gutter, selection, runs, diagnostics, caret *)
let paint_text_area ~colors ~composition ~messages ~chips ~hovered_number ~scrubbing ~matched ~spans ~errors ~slot_of ~notes ~char_w ~final ~stop_of ~start_of ~focused ~edit ~rows ~count ~notes ~row ~pad ~offset ~horizontal ~gutter ~(theme : Theme.t) ~body ui =
  draw ui body (fun paint (bx, by, bw, bh) ->
    let width text = Paint.text_width paint text in
    Paint.fill paint ~x:bx ~y:by ~w:bw ~h:bh theme.input;
    let previous = paint.clip_rect in
    let clip x w = paint.clip_rect <- intersect previous
      (x, by, w, bh) in
    let text_x = bx +. gutter -. horizontal in
    Paint.fill paint ~x:(bx +. gutter) ~y:by ~w:1. ~h:bh (Theme.faint_border theme);
    let small = Paint.label_size paint in
    let first = max 0 (int_of_float (Float.floor ((offset -. pad) /. row)) - Array.length notes)
    and last = min (count - 1) (int_of_float (Float.floor ((offset +. bh) /. row))) in
    let caret_line = row_at rows edit.caret in
    let selected = focused && edit.caret <> edit.anchor in
    let s0, s1 = text_selection edit in
    let band line (start, stop) color y =
      let ls = start_of rows line and le = stop_of rows line in
      (* a selection runs on past the end of a row that ends a line *)
      let newline = le < String.length final && final.[le] = '\n' in
      let start = max start ls and stop = min stop (if newline then le + 1 else le) in
      if start < stop || (start <= le && stop > le) then
        let x0 = width (String.sub final ls (min start le - ls)) in
        let x1 = if stop > le then width (String.sub final ls (le - ls)) +. char_w
          else width (String.sub final ls (stop - ls)) in
        Paint.fill paint ~x:(text_x +. x0) ~y ~w:(Float.max 1. (x1 -. x0)) ~h:row color in
    for line = first to last do
      let y = by -. offset +. pad +. (float (slot_of notes line) *. row) in
      let text_y = y +. float (max 0 ((text_line_height ui - ui.font_size) / 2)) in
      let _, _, logical = rows.(line) in
      let starts_line = line = 0 || (let _, _, before = rows.(line - 1) in before <> logical) in
      clip bx bw;
      let wrong = List.mem (logical + 1) errors and current = focused && line = caret_line in
      if wrong then Paint.fill paint ~x:bx ~y ~w:bw ~h:row (Rays_math.Color.with_alpha Theme.invalid 18)
      else if current then Paint.fill paint ~x:bx ~y ~w:bw ~h:row (Theme.faint_border theme);
      if starts_line then begin
        let number = string_of_int (logical + 1) in
        let number_x = bx +. gutter -. 10. -. Paint.text_width paint ~size:small number in
        if wrong then Paint.fill paint ~x:(number_x -. 10.) ~y:(y +. (row /. 2.) -. 3.) ~w:6. ~h:6. Theme.invalid;
        Paint.text paint ~size:small ~at:(number_x, text_y +. 1.)
          ~color:(if wrong then Theme.invalid else if current then theme.foreground else Theme.ink_3 theme) number
      end;
      clip (bx +. gutter) (bw -. gutter);
      List.iter (fun span -> band line span (Theme.tint theme) y) spans;
      if selected then band line (s0, s1) (Theme.tint theme) y;
      (* the bracket pair at the caret: an accent outline *)
      Option.iter (fun (o, k) -> List.iter (fun b ->
        let ls = start_of rows line and le = stop_of rows line in
        if b >= ls && b < le then
          (* the glyph's box (6.5 x 14) with a 1-point outline outside it: 8.5 x 16 *)
          Paint.frame paint ~x:(text_x +. width (String.sub final ls (b - ls)) -. 1.) ~y:(y +. 2.)
            ~w:(char_w +. 2.) ~h:16. theme.accent) [ o; k ]) matched;
      (* a number under the pointer (or being dragged) wears an accent underline *)
      Option.iter (fun (a, b) ->
        let ls = start_of rows line and le = stop_of rows line in
        if a >= ls && a < le then begin
          let x0 = width (String.sub final ls (a - ls)) and x1 = width (String.sub final ls (min b le - ls)) in
          Paint.fill paint ~x:(text_x +. x0) ~y:(y +. row -. 4.) ~w:(Float.max 1. (x1 -. x0)) ~h:2.
            (if !scrubbing <> None then theme.accent else Rays_math.Color.with_alpha theme.accent 160)
        end) hovered_number;
      let ls = start_of rows line and le = stop_of rows line in
      (* a colour literal wears its colour as a bar under it *)
      List.iter (fun (a, b, color) ->
        if a >= ls && a < le && b <= String.length final then
          Paint.fill paint ~x:(text_x +. width (String.sub final ls (a - ls))) ~y:(y +. row -. 5.)
            ~w:(width (String.sub final a (min b le - a))) ~h:4. color) chips;
      (* the wrong span of a diagnostic is underlined in the error colour *)
      List.iter (fun (_, span, _) -> match span with
        | Some (a, b) when a < le && b > ls && b > a && b <= String.length final ->
            let a = max a ls and b = min b le in
            Paint.fill paint ~x:(text_x +. width (String.sub final ls (a - ls))) ~y:(y +. row -. 3.)
              ~w:(Float.max 1. (width (String.sub final a (b - a)))) ~h:1. Theme.invalid
        | _ -> ()) messages;
      let line_str = String.sub final ls (le - ls) in
      if focused && line = caret_line && composition <> "" then begin
        let before = String.sub final ls (edit.caret - ls) in
        let caret_x = text_x +. width before in
        Paint.text paint ~at:(text_x, text_y) ~color:theme.foreground before;
        Paint.text paint ~at:(caret_x, text_y) ~color:theme.foreground composition;
        Paint.text paint ~at:(caret_x +. width composition, text_y) ~color:theme.foreground
          (String.sub line_str (String.length before) (String.length line_str - String.length before))
      end else if colors = [] then Paint.text paint ~at:(text_x, text_y) ~color:theme.foreground line_str
      else begin
        (* the row in coloured runs, the gaps in the foreground colour; ponytail: every row scans
           the whole colour list (visible rows x tokens), fine for workspace-sized text *)
        let at a = text_x +. width (String.sub final ls (a - ls)) in
        let run a b color = if a < b then Paint.text paint ~at:(at a, text_y) ~color (String.sub final a (b - a)) in
        let pos = List.fold_left (fun pos (a, b, color) ->
          let a = max a ls and b = min b le in
          if a >= b || a < pos then pos else (run pos a theme.foreground; run a b color; b)) ls colors in
        run pos le theme.foreground
      end
    done;
    (* the message rows: the diagnostic at the label size, under the indent of its line *)
    clip (bx +. gutter) (bw -. gutter);
    Array.iteri (fun k (after, _, message) ->
      let y = by -. offset +. pad +. (float (after + 1 + k) *. row) in
      if y +. row > by && y < by +. bh then begin
        let ls = start_of rows after in
        let indent = ref 0 in
        while ls + !indent < String.length final && final.[ls + !indent] = ' ' do incr indent done;
        Paint.text paint ~size:small ~at:(text_x +. width (String.make !indent ' '), text_top ui ~size:small y row)
          ~color:Theme.invalid message
      end) notes;
    clip (bx +. gutter) (bw -. gutter);
    if focused then begin
      let y = by -. offset +. pad +. (float (slot_of notes caret_line) *. row) in
      let caret_x = text_x +. width (String.sub final (start_of rows caret_line)
        (edit.caret - start_of rows caret_line)) in
      Paint.input_region paint ~x:bx ~y ~w:bw ~h:row ~focused:true ~cursor:(caret_x -. bx) ();
      Paint.fill paint ~x:(Float.round caret_x) ~y:(y +. 2.) ~w:1. ~h:(row -. 4.) theme.accent
    end else Paint.input_region paint ~x:bx ~y:by ~w:bw ~h:bh ~focused:false ();
    paint.clip_rect <- previous);
  ()

(* the completion popup, under the caret's row (above it near the bottom of the view) *)
let paint_completions ~slot_of ~popup ~signal_of ~first ~row ~notes ~pad ~offset ~by ~selected ~items ~x_at ~edit ~rows ~(theme : Theme.t) ui =
  Option.iter (fun (container, rows_boxes, width, height) ->
    let caret_line = row_at rows edit.caret in
    let anchor = x_at (fst items.(!selected).replace) in
    let px = Float.max 8. (Float.min (anchor -. 8.) (ui.view_w -. width -. 8.)) in
    let below = by -. offset +. pad +. (float (slot_of notes caret_line + 1) *. row) in
    let py = if below +. height <= ui.view_h -. 8. then below
      else Float.max 8. (by -. offset +. pad +. (float (slot_of notes caret_line) *. row) -. height) in
    set_at ui container ~at:(px, py);
    let selected = !selected in
    draw ui container (fun paint (x, y, w, h) ->
      Paint.fill paint ~x ~y ~w ~h theme.input;
      Paint.frame paint ~x ~y ~w ~h (Theme.edge theme);
      let doc = items.(selected).doc in
      if doc <> "" then
        Paint.text paint ~color:(Theme.muted theme)
          ~at:(x +. 8., y +. h -. 18.) (display doc));
    List.iteri (fun i r ->
      let c = items.(first + i) and hovered = (signal_of ui r).hovered in
      draw ui r (fun paint rect ->
        let (rx, ry, rw, rh) as bounds = ints rect in
        if first + i = selected then fill paint bounds theme.control
        else if hovered then hover_row paint ui bounds;
        kit_text paint ~color:theme.foreground (rx + side) (label_y ui ry rh) c.label;
        let dw = int_of_float (text_width ui c.detail) in
        kit_text paint ~color:(Theme.muted theme) (rx + rw - dw - side) (label_y ui ry rh) c.detail)) rows_boxes)
    popup;
  ()

(* a dragged number: the literal at the press follows the pointer *)
let apply_scrub ~scrubbing ~on_scrub ~suggest ~on_scrub_edit ~edit ui =
  Option.iter (fun (start, literal, dx, first) ->
    if start <= String.length edit.text && literal <> "" then begin
      (* the whole drag is one step of the editor's undo *)
      if first then remember ui edit ~typing:false;
      let stop = ref start in
      while !stop < String.length edit.text && numeric_character edit.text.[!stop] do incr stop done;
      let coarse = match ui.input_frame with
        | Some (frame : Frame.t) -> List.mem Input.Shift frame.input.keys | None -> false in
      let next = scrubbed literal dx ~coarse in
      Option.iter (fun f -> f (start, !stop) next) on_scrub_edit;
      edit.text <- String.sub edit.text 0 start ^ next
        ^ String.sub edit.text !stop (String.length edit.text - !stop);
      edit.caret <- start + String.length next; edit.anchor <- edit.caret;
      set_state ui suggest 0;
      Option.iter (fun f -> f `Live) on_scrub
    end) !scrubbing;
  ()

(* the keys of a focused text area: completion, pairs, indent, line edits, movement, typing *)
let handle_keys ~body ~stop_of ~page ~typed ~language ~suggest ~open_ ~accept ~selected ~chosen ~items ~submitted ~changed ~closed ~shown ~row_text ~start_of ~edit ~rows_of ui =
  List.iter (fun ((event : Input.event), modifiers) ->
    let command = command_modifiers modifiers and shift = List.mem Input.Shift modifiers in
    let rows = rows_of edit.text in
    let count = Array.length rows in
    let x_of line index = text_width ui
      (String.sub edit.text (start_of rows line) (index - start_of rows line)) in
    let line = row_at rows edit.caret in
    let move target = edit.caret <- target; if not shift then edit.anchor <- target in
    (* Up and Down keep the column they started from across shorter rows *)
    let vertical step =
      let target = line + step in
      let x = if ui.edit_column >= 0. then ui.edit_column else x_of line edit.caret in
      ui.edit_column <- x;
      if target < 0 then move 0
      else if target >= count then move (String.length edit.text)
      else move (start_of rows target + text_caret_at ui (row_text edit.text rows target) x) in
    (match event with
     | Input.Key_pressed (Input.ArrowUp | ArrowDown) when not command -> ()
     | Input.Key_pressed (Input.PageUp | PageDown | Shift | Ctrl | Alt | Meta) -> ()
     | _ -> ui.edit_column <- -1.);
    let listing = shown > 0 && not !closed in
    let change () = changed := true; remember ui edit ~typing:false in
    (match event with
    | Input.Key_pressed Input.Enter when command -> submitted := true
    (* the completion popup takes Up, Down, Tab, Enter and Escape while it is open *)
    | Input.Key_pressed ((Input.ArrowUp | Input.ArrowDown) as key) when listing ->
        let n = Array.length items in
        chosen := true;
        selected := (!selected + (if key = Input.ArrowUp then n - 1 else 1)) mod n
    | Input.Key_pressed Input.Tab when listing && not command -> accept items.(!selected)
    | Input.Key_pressed Input.Enter when listing && !chosen -> accept items.(!selected)
    | Input.Key_pressed Input.Escape when open_ <> 0 && not !closed -> set_state ui suggest 0; closed := true
    | Input.Key_pressed Input.Escape -> unfocus ui
    | Input.Key_pressed Input.Enter ->
        begin
          let indent = match language with
            | Some l -> l.indent edit.text (fst (text_selection edit)) | None -> "" in
          change (); replace_text edit ("\n" ^ indent);
          if open_ <> 0 then (set_state ui suggest 0; closed := true)
        end
    (* brackets come in pairs: an opener wraps the selection or inserts both, a closer typed
       before itself steps over it, Backspace between an empty pair takes both *)
    | Input.Text_input s when language <> None && String.length s = 1 && not command ->
        let c = s.[0] and pairs = (Option.get language).pairs in
        let start, stop = text_selection edit in
        let next = if edit.caret < String.length edit.text then Some edit.text.[edit.caret] else None in
        (* a bracket is a bracket only in code: in a string only a quote steps over its closing
           quote, and a comment takes every character as typed; in code a quote before a quote
           is the next string's opening one *)
        let context = (Option.get language).context_at edit.text edit.caret in
        let quote = List.mem (c, c) pairs in
        let step_over = next = Some c && (match context with
          | `Code -> List.exists (fun (_, close) -> close = c) pairs && not quote
          | `String -> quote
          | `Comment -> false) in
        (match context, List.assoc_opt c pairs with
         | `Code, Some close when start <> stop ->
             change ();
             replace_text edit (String.make 1 c ^ String.sub edit.text start (stop - start) ^ String.make 1 close);
             edit.anchor <- start + 1; edit.caret <- stop + 1
         | _ when step_over ->
             edit.caret <- edit.caret + 1; edit.anchor <- edit.caret
         | `Code, Some close ->
             change ();
             replace_text edit (String.make 1 c ^ String.make 1 close);
             edit.caret <- edit.caret - 1; edit.anchor <- edit.caret
         | _ -> changed := edit_text_event ~multiline:true ui edit ~accept:(fun _ -> true) ~modifiers event || !changed);
        typed := true
    | Input.Key_pressed Input.Backspace when language <> None && not command
        && edit.caret = edit.anchor && edit.caret > 0 && edit.caret < String.length edit.text
        && List.mem (edit.text.[edit.caret - 1], edit.text.[edit.caret]) (Option.get language).pairs ->
        change (); edit.anchor <- edit.caret - 1; edit.caret <- edit.caret + 1; replace_text edit ""; typed := true
    (* Command-X / C with no selection take the caret's whole logical line, newline included *)
    | Input.Key_pressed (Input.KeyChar ('x' | 'X' | 'c' | 'C' as key)) when command && edit.caret = edit.anchor ->
        let s = line_start edit.text edit.caret in
        let e = min (String.length edit.text) (line_end edit.text edit.caret + 1) in
        if Clipboard.set_text (String.sub edit.text s (e - s)) = Ok ()
           && (key = 'x' || key = 'X') then begin
          change (); edit.anchor <- s; edit.caret <- e; replace_text edit ""; typed := true
        end
    | Input.Key_pressed Input.Tab when not command && edit.caret <> edit.anchor ->
        (* Tab and Shift-Tab with a selection indent and dedent every line it touches *)
        let a, b = text_selection edit in
        let first, stop = line_block edit.text a b in
        let block = String.sub edit.text first (stop - first) in
        let shifted = String.concat "\n" (List.map (fun l ->
          if not shift then (if l = "" then l else "  " ^ l)
          else if String.starts_with ~prefix:"  " l then String.sub l 2 (String.length l - 2)
          else if String.starts_with ~prefix:" " l then String.sub l 1 (String.length l - 1) else l)
          (String.split_on_char '\n' block)) in
        if shifted <> block then begin
          change (); replace_block edit first stop shifted;
          keep_selection edit first (first + String.length shifted)
        end
    | Input.Key_pressed Input.Tab when not command ->
        if not shift then (change (); replace_text edit "  ")
        else begin
          (* Shift-Tab: up to two spaces leave the start of the line *)
          let line_start = line_start edit.text edit.caret in
          let spaces = if line_start < String.length edit.text && edit.text.[line_start] = ' '
            then (if line_start + 1 < String.length edit.text && edit.text.[line_start + 1] = ' ' then 2 else 1)
            else 0 in
          if spaces > 0 then begin
            change ();
            edit.text <- String.sub edit.text 0 line_start
              ^ String.sub edit.text (line_start + spaces) (String.length edit.text - line_start - spaces);
            edit.caret <- max line_start (edit.caret - spaces); edit.anchor <- edit.caret
          end
        end
    (* line and form edits: Command-/ toggles comments, Option-Up and Down move the lines, with
       Shift duplicate them, Command-Option-Up selects the enclosing form, Command-Shift-\\ jumps
       to the matching bracket *)
    | Input.Key_pressed (Input.KeyChar '/') when command && language <> None ->
        let a, b = text_selection edit in
        let first, stop = line_block edit.text a b in
        let block = String.sub edit.text first (stop - first) in
        let toggled = toggle_comment (Option.get language).comment block in
        if toggled <> block then begin
          change (); replace_block edit first stop toggled;
          let last = first + String.length toggled in
          if a = b then (edit.caret <- max first (a + last - stop); edit.anchor <- edit.caret)
          else keep_selection edit first last
        end
    | Input.Key_pressed (Input.ArrowUp | Input.ArrowDown as key) when List.mem Input.Alt modifiers && not command ->
        let a, b = text_selection edit in
        let first, stop = line_block edit.text a b in
        let n = String.length edit.text in
        let block = String.sub edit.text first (stop - first) in
        let up = key = Input.ArrowUp in
        let shift_by d = edit.anchor <- edit.anchor + d; edit.caret <- edit.caret + d in
        if shift then begin
          (* a copy below keeps the selection on the copy, a copy above on the original *)
          change ();
          replace_block edit stop stop ("\n" ^ block);
          if not up then shift_by (String.length block + 1)
        end else if up && first > 0 then begin
          let before = line_start edit.text (first - 1) in
          change ();
          replace_block edit before stop (block ^ "\n" ^ String.sub edit.text before (first - 1 - before));
          shift_by (before - first)
        end else if not up && stop < n then begin
          let after = line_end edit.text (stop + 1) in
          change ();
          replace_block edit first after (String.sub edit.text (stop + 1) (after - stop - 1) ^ "\n" ^ block);
          shift_by (after - stop)
        end
    | Input.Key_pressed Input.ArrowUp when command && List.mem Input.Alt modifiers ->
        let a, b = text_selection edit in
        (match language with
         | Some l ->
             Option.iter (fun (o, c) -> edit.anchor <- o; edit.caret <- c) (enclosing_pair (l.brackets edit.text) a b)
         | None -> ())
    | Input.Key_pressed (Input.KeyChar '\\') when command && shift && language <> None ->
        Option.iter (fun at -> edit.caret <- at; edit.anchor <- at)
          (bracket_jump ((Option.get language).brackets edit.text) edit.caret)
    | Input.Key_pressed Input.ArrowUp when command -> move 0
    | Input.Key_pressed Input.ArrowDown when command -> move (String.length edit.text)
    | Input.Key_pressed Input.ArrowUp -> vertical (-1)
    | Input.Key_pressed Input.ArrowDown -> vertical 1
    | Input.Key_pressed Input.PageUp -> vertical (-page)
    | Input.Key_pressed Input.PageDown -> vertical page
    | Input.Key_pressed (Input.Home | Input.ArrowLeft) when command || event = Input.Key_pressed Input.Home ->
        move (start_of rows line)
    | Input.Key_pressed (Input.End | Input.ArrowRight) when command || event = Input.Key_pressed Input.End ->
        move (stop_of rows line)
    | event ->
        if edit_text_event ~multiline:true ui edit ~accept:(fun _ -> true) ~modifiers event then begin
          changed := true;
          (match event with
           | Input.Text_input _ | Input.Key_pressed (Input.Backspace | Input.Delete) -> typed := true
           | _ -> ())
        end))
    (key_events ui body);
  ()

(* a press on a number arms a scrub; three points sideways make it one *)
let arm_scrub ~scrubbing ~scrub_state ~on_scrub ~point_at ~text ~on_handle ~signal ~language ~scrub ui =
  (* a press on a number arms a scrub (its state is the start + 1, negated once dragging); three
     points sideways make it one, three points down a selection as before *)
  (match language with
   | Some l when signal.pressed && signal.button = Some Input.LeftButton ->
       (match (if on_handle signal.press_point then l.number_at text (point_at signal.press_point) else None) with
        | Some (a, b) -> set_state ui scrub (a + 1); set_text_state ui scrub (Some (String.sub text a (b - a)))
        | None -> set_state ui scrub 0)
   | Some _ when scrub_state <> 0 && signal.held ->
       let dx = fst signal.pointer -. fst signal.press_point
       and dy = snd signal.pointer -. snd signal.press_point in
       if scrub_state < 0 || (Float.abs dx >= 3. && Float.abs dx >= Float.abs dy) then begin
         let start = abs scrub_state - 1 in
         set_state ui scrub (- (start + 1));
         scrubbing := Some (start, Option.value ~default:"" (text_state ui scrub), dx, scrub_state > 0)
       end else if Float.abs dy >= 3. then set_state ui scrub 0
   | _ when scrub_state <> 0 && not signal.held ->
       set_state ui scrub 0;
       if scrub_state < 0 then Option.iter (fun f -> f `Done) on_scrub
   | _ -> ());
  ()

let text_area_submit ui ~at ~w ~h ?(wrap = false) ?(errors = []) ?(messages = []) ?(spans = [])
    ?reveal ?(reveal_nonce = 0) ?select ?(splice = []) ?language ?on_context ?on_scrub ?on_scrub_edit ?on_click ?on_caret
    ?on_caret_move ?on_select ?on_drop ?(chips = []) label text =
  (* a line of code is one and a half times its text, as code editors set it (17 points at 11,
     20 at 13): the 24-point row is a control's, twice the text *)
  let row = float (text_line_height ui) in
  let body = box ui
      ~flags:(clickable lor focusable lor blocking lor scroll lor clip
              lor keep_tab)
      ~at ~w:(Px w) ~h:(Px h) ~scroll_step:row label in
  (* a [select] request takes the keyboard for the area *)
  if select <> None then focus ui body;
  let char_w = text_width ui "0" in
  (* the sheet's code area: a 36-point gutter (wider only past 999 lines), the text at its edge,
     6 points of padding above the first row *)
  let gutter_of lines = 36. +. char_w *. float (max 0 (String.length (string_of_int lines) - 3)) in
  let pad = 6. in
  let columns_of count =
    if not wrap then None
    else
      let gutter = gutter_of count in
      Some (int_of_float (Float.floor (Float.max 1. (w -. gutter -. 16.) /. Float.max 1. char_w))) in
  (* the gutter width follows the number of logical lines, the wrap width follows the gutter *)
  let rows_of text =
    let logical = 1 + String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 text in
    text_rows ?cols:(columns_of logical) text in
  let rows = rows_of text in
  let count = Array.length rows in
  (* a diagnostic's message is a row of its own under the last row of its line: [note_rows] are
     (the display row it follows, the wrong span, the message), in order; a row's slot counts
     the message rows above it *)
  let note_rows rows = if messages = [] then [||] else begin
    let found = List.filter_map (fun (line, span, message) ->
      let last = ref (-1) in
      Array.iteri (fun i (_, _, logical) -> if logical = line - 1 then last := i) rows;
      if !last >= 0 then Some (!last, span, message) else None) messages in
    Array.of_list (List.stable_sort (fun (a, _, _) (b, _, _) -> Int.compare a b) found)
  end in
  let slot_of notes i = Array.fold_left (fun n (after, _, _) -> if after < i then n + 1 else n) i notes in
  let row_at_slot notes count k =
    if notes = [||] then max 0 (min (count - 1) k) else begin
      let i = ref 0 in
      while !i + 1 < count && slot_of notes (!i + 1) <= k do incr i done; !i
    end in
  let logical_count rows = let _, _, l = rows.(Array.length rows - 1) in l + 1 in
  let start_of rows i = let s, _, _ = rows.(i) in s in
  let stop_of rows i = let _, e, _ = rows.(i) in e in
  let row_text text rows i = String.sub text (start_of rows i) (stop_of rows i - start_of rows i) in
  (* the content box scrolls; two empty boxes keep the completion popup's and the number
     scrub's state between frames *)
  let content, suggest, scrub = within ui body (fun () ->
    let content = box ui ~w:(Px w)
        ~h:(Px ((float (count + Array.length (note_rows rows)) *. row) +. pad)) (label ^ "-content") in
    let suggest = box ui ~w:(Px 0.) ~h:(Px 0.) ~at:(0., 0.) (label ^ "-suggest") in
    let scrub = box ui ~w:(Px 0.) ~h:(Px 0.) ~at:(0., 0.) (label ^ "-scrub") in
    content, suggest, scrub) in
  let signal_of = signal in
  let signal = signal ui body in
  (* the I-beam over text; a number under the pointer asks for the scrub
     cursor further down, and the last request wins *)
  if signal.hovered then request_cursor ui `Text;
  let focused = focused ui body in
  let gutter = gutter_of (logical_count rows) in
  let bx, by, bw, bh = rect ui body in
  let horizontal = ref (float (state ui body ~default:0)) in
  (* the byte at a point; [strict] answers only over the row's glyphs *)
  let point_in ?(strict = false) text rows (px, py) =
    let count = Array.length rows in
    let line = row_at_slot (note_rows rows) count
      (int_of_float (Float.floor ((py -. by +. scroll_position ui body -. pad) /. row))) in
    let x = px -. bx -. gutter +. !horizontal in
    let line_text = row_text text rows line in
    if strict && (x < 0. || x > text_width ui line_text || py < by || py > by +. bh) then None
    else Some (start_of rows line + text_caret_at ui line_text (Float.max 0. x)) in
  let point_at point = Option.get (point_in text rows point) in
  (* a number is dragged by its underline: the lowest 9 points of its row, so a drag that selects
     text from a number does not rewrite it *)
  let on_handle (_, py) =
    let y = py -. by +. scroll_position ui body -. pad in
    y -. (row *. Float.floor (y /. row)) >= row -. 9. in
  let edit = if focused then load_text_edit ui body.box_key text
    else { text; caret = String.length text; anchor = String.length text } in
  let moved = ref false and submitted = ref false in
  (match on_context with
   | Some f when context_clicked signal -> f signal.release_point
   | _ -> ());
  let scrub_state = state ui scrub ~default:0 in
  let scrubbing = ref None in
  arm_scrub ~scrub ~language ~signal ~on_handle ~text ~point_at ~on_scrub ~scrub_state ~scrubbing ui;
  (* the completion popup: its state is (token start + 1) * 512 + 256 once an arrow has moved
     the selection + the selected row; the rows are built before the keys so a click on one is
     seen this frame *)
  let open_ = if focused then state ui suggest ~default:0 else (set_state ui suggest 0; 0) in
  let items = match language with
    | Some l when open_ <> 0 -> Array.of_list (l.complete edit.text edit.caret)
    | _ -> [||] in
  let shown = min 8 (Array.length items) in
  let selected = ref (min (open_ land 255) (max 0 (Array.length items - 1))) in
  (* Enter takes the row only after Up or Down chose it; Tab takes it always *)
  let chosen = ref (open_ land 256 <> 0) in
  let first = max 0 (!selected - shown + 1) in
  let closed = ref false in
  let accept (c : completion) =
    remember ui edit ~typing:false;
    let a, b = c.replace in
    let a = max 0 (min a (String.length edit.text)) and b = max 0 (min b (String.length edit.text)) in
    edit.text <- String.sub edit.text 0 a ^ c.insert ^ String.sub edit.text b (String.length edit.text - b);
    edit.caret <- a + String.length c.insert; edit.anchor <- edit.caret;
    set_state ui suggest 0; closed := true in
  let popup = if shown = 0 then None else begin
    let doc = items.(!selected).doc in
    let width = Array.fold_left (fun w (c : completion) ->
      Float.max w (text_width ui c.label +. text_width ui c.detail +. 44.)) 220. items in
    let width = Float.min width (Float.max 120. (ui.view_w -. 16.)) in
    let height = (float (shown * ui.kit_row_height)) +. (if doc = "" then 4. else 22.) in
    let container = overlay_box ui ~flags:blocking ~at:(0., 0.) ~w:width ~h:height (label ^ "-completions") in
    let boxes = within ui container (fun () ->
      List.init shown (fun i ->
        kit_row ui ~flags:(clickable lor blocking lor keep_focus) (Printf.sprintf "row-%d" i))) in
    List.iteri (fun i r -> if (signal_of ui r).clicked then accept items.(first + i)) boxes;
    Some (container, boxes, width, height)
  end in
  if focused then begin
    let caret0 = edit.caret in
    (* a triple click takes the logical line with its break *)
    let line_of at =
      let s = line_start edit.text at and e = line_end edit.text at in
      s, (if e < String.length edit.text then e + 1 else e) in
    if signal.pressed && signal.button = Some Input.LeftButton then
      begin
        ui.edit_column <- -1.;
        press_select ui edit ~shift:(press_shift ui body) ~clicks:signal.clicks ~line_of
          (point_at signal.press_point);
        (* a double click on true / false flips it, as dragging flips a number *)
        if signal.clicks = 2 && language <> None then begin
          let a, b = text_selection edit in
          let flipped = match String.sub edit.text a (b - a) with
            | "true" -> Some "false" | "false" -> Some "true" | _ -> None in
          Option.iter (fun word ->
            remember ui edit ~typing:false;
            replace_text edit word; edit.anchor <- a; edit.caret <- a + String.length word) flipped
        end
      end
    else if !scrubbing = None && scrub_state = 0
        && (signal.dragging || (signal.held && signal.pointer <> signal.press_point)) then
      drag_select ui edit ~line_of (point_at signal.pointer);
    let typed = ref false and changed = ref false in
    (* the host's replacements (one undo step, last first so the offsets hold), then its selection *)
    if splice <> [] then begin
      remember ui edit ~typing:false;
      List.iter (fun (a, b, by) ->
        if 0 <= a && a <= b && b <= String.length edit.text then
          edit.text <- String.sub edit.text 0 a ^ by ^ String.sub edit.text b (String.length edit.text - b))
        (List.sort (fun (a, _, _) (b, _, _) -> Int.compare b a) splice);
      changed := true
    end;
    Option.iter (fun (a, b) ->
      let n = String.length edit.text in
      edit.anchor <- max 0 (min n a); edit.caret <- max 0 (min n b)) select;
    let page = max 1 (int_of_float (bh /. row) - 1) in
    handle_keys ~rows_of ~edit ~start_of ~row_text ~shown ~closed ~changed ~submitted ~items ~chosen ~selected ~accept ~open_ ~suggest ~language ~typed ~page ~stop_of ~body ui;
    (* a dragged number: the literal at the press follows the pointer (the caret sits after it) *)
    apply_scrub ~edit ~on_scrub_edit ~suggest ~on_scrub ~scrubbing ui;
    (* typing opens the popup on the token at the caret (the selection starts over); a popup
       left open follows the caret and closes when the caret leaves its token *)
    (match language with
     | Some l when (!typed || (open_ <> 0 && not !closed)) ->
         (match l.complete edit.text edit.caret with
          | c :: _ when !typed -> set_state ui suggest ((fst c.replace + 1) * 512)
          | c :: _ when fst c.replace = (open_ lsr 9) - 1 ->
              set_state ui suggest ((open_ lsr 9) * 512 + (if !chosen then 256 else 0) + !selected)
          | _ -> set_state ui suggest 0)
     | _ -> ());
    (* the language's rewrite (parinfer) after this frame's edits; the anchor maps like the caret *)
    (match language with
     | Some { rewrite = Some rewrite; _ } when !changed ->
         let text', caret' = rewrite edit.text edit.caret in
         let anchor' = if edit.anchor = edit.caret then caret' else snd (rewrite edit.text edit.anchor) in
         edit.text <- text'; edit.caret <- caret'; edit.anchor <- anchor'
     | _ -> ());
    save_text_edit ui edit;
    moved := edit.caret <> caret0 || edit.text != text || signal.pressed
  end;
  (* scrolling: the wheel, then whatever keeps the caret (or [reveal]) in view *)
  let final = edit.text in
  if focused then Option.iter (fun f -> f edit.caret) on_caret;
  if focused then Option.iter (fun f -> f (text_selection edit)) on_select;
  if focused && !moved && (signal.pressed || signal.dragging || key_events ui body <> []) then
    Option.iter (fun f -> f edit.caret) on_caret_move;
  (match on_click with
   | Some f when signal.clicked && signal.button = Some Input.LeftButton && scrub_state >= 0 ->
       Option.iter (fun byte -> f byte (command_modifiers (press_keys ui body)))
         (point_in ~strict:true text rows signal.release_point)
   | _ -> ());
  (* a payload in flight over the area: the byte under the pointer, hovering or released *)
  (match on_drop with
   | Some f ->
       Option.iter (fun drop -> Option.iter (fun byte -> f byte drop) (point_in text rows signal.pointer))
         (drop_target ui body)
   | None -> ());
  let rows = if final == text then rows else rows_of final in
  let count = Array.length rows in
  let notes = note_rows rows in
  (* [reveal] scrolls once per (offset, length, nonce): the content box remembers it, and forgets
     it while there is nothing to reveal *)
  let revealed = match reveal with
    | Some index when bw > 0.
        && state ui content ~default:0 <> (index * 1_000_003) + String.length final + 1 + (reveal_nonce * 1_000_000_007) ->
        set_state ui content ((index * 1_000_003) + String.length final + 1 + (reveal_nonce * 1_000_000_007)); Some index
    | None -> set_state ui content 0; None
    | _ -> None in
  let target = if !moved then Some edit.caret
    else Option.map (fun index -> min (String.length final) index) revealed in
  let longest = ref 0 in
  Array.iteri (fun i _ -> longest := max !longest (stop_of rows i - start_of rows i)) rows;
  let visible = Float.max 1. (bw -. gutter -. 16.) in
  let max_x = if wrap then 0. else Float.max 0. (float !longest *. char_w -. visible) in
  horizontal := Float.max 0. (Float.min max_x (!horizontal +. fst signal.scroll *. row));
  let vertical = ref (Float.max 0. (Float.min (Float.max 0. ((float (count + Array.length notes) *. row) +. pad -. bh))
    (scroll_offset ui body))) in
  Option.iter (fun index ->
    let line = row_at rows index in
    let top = pad +. (float (slot_of notes line) *. row) in
    if top < !vertical then vertical := top
    else if top +. row > !vertical +. bh then vertical := top +. row -. bh;
    let x = text_width ui (String.sub final (start_of rows line) (min index (stop_of rows line) - start_of rows line)) in
    if x < !horizontal then horizontal := x
    else if x -. !horizontal > visible then horizontal := x -. visible) target;
  set_state ui body (int_of_float !horizontal);
  if !vertical <> scroll_offset ui body then set_scroll_offset ui body !vertical;
  (* painted where the shared elastic scroll puts the content, overshoot included *)
  let offset = scroll_position ui body and horizontal = !horizontal in
  let theme = ui.theme and composition = ui.composition in
  (* the language's colours and the bracket pair at the caret, once per frame *)
  let colors = match language with Some l -> l.colorize final | None -> [] in
  let matched = match language with
    | Some l when focused ->
        let c = edit.caret in
        List.find_opt (fun (o, k) -> o = c - 1 || k = c - 1 || o = c || k = c) (l.brackets final)
    | _ -> None in
  (* the pointer over a number invites a drag; over any other token it describes it after a rest *)
  let under_pointer = match language with
    | Some _ when signal.hovered && ui.active = None ->
        point_in ~strict:true final rows signal.pointer
    | _ -> None in
  let hovered_number = match language, under_pointer, !scrubbing with
    | _, _, Some (start, _, _, _) ->
        let stop = ref start in
        while !stop < String.length final && numeric_character final.[!stop] do incr stop done;
        Some (start, !stop)
    | Some l, Some byte, None when on_handle signal.pointer -> l.number_at final byte
    | _ -> None in
  if hovered_number <> None then request_cursor ui `Horizontal_resize;
  (match language, under_pointer with
   | Some l, Some byte when hovered_number = None && open_ = 0 ->
       Option.iter (fun (start, _, doc) ->
         if doc <> "" then tooltip ui ~key:(Printf.sprintf "%s#%d" label start) ~text:doc)
         (l.describe final byte)
   | _ -> ());
  (* the completion popup sits under the caret's row (above it near the bottom of the view) *)
  let text_x0 = bx +. gutter -. horizontal in
  let x_at index =
    let line = row_at rows index in
    text_x0 +. text_width ui (String.sub final (start_of rows line) (index - start_of rows line)) in
  paint_completions ~theme ~rows ~edit ~x_at ~items ~selected ~by ~offset ~pad ~notes ~row ~first ~signal_of ~popup ~slot_of ui;
  paint_text_area ~body ~theme ~gutter ~horizontal ~offset ~pad ~row ~notes ~count ~rows ~edit ~focused ~start_of ~stop_of ~final ~char_w ~notes ~slot_of ~errors ~spans ~matched ~scrubbing ~hovered_number ~chips ~messages ~composition ~colors ui;
  final, !submitted

type pick = [ `None | `Pick of int | `Delete of int | `Submit | `Back | `Cancel ]

(* ponytail: case-insensitive subsequence match, no scoring; swap for
   fzf-style ranking if lists grow long. *)
let fuzzy_match ~query text =
  let query = String.lowercase_ascii query and text = String.lowercase_ascii text in
  let length = String.length text in
  let rec walk at index =
    index = String.length query
    || (at < length && walk (at + 1) (if text.[at] = query.[index] then index + 1 else index)) in
  walk 0 0

(* A focused search row over a windowed list. The cursor (search box state)
   and an armed delete row (list box state) are retained; the host keeps the
   query and applies the result. *)
let picker ui ?(limit = 10) ?mark ?(off = fun _ -> false) ?(slash = true) ?(at_rest = false) label ~query rows_of =
  let rows = ref (rows_of query) in
  let count () = Array.length !rows in
  (* the search row: a 20-point field in 4 points of padding, a [/] before the query *)
  let search = box ui ~flags:(clickable lor focusable lor blocking lor focus_mark) ~w:Grow ~h:(Px 28.) label in
  if ui.focus = 0 || ui.rw.(search.box_slot) = 0. then focus ui search;
  let list = box_keyed ui ~w:Grow ~h:Fit ~axis:Column (int_key search.box_key 0) in
  let search_signal = signal ui search in
  if search_signal.hovered then request_cursor ui `Text;
  let keys = key_events ui search in
  let clamp cursor = if count () = 0 then 0 else max 0 (min (count () - 1) cursor) in
  let cursor = ref (clamp (state ui search ~default:0))
  and armed = ref (state ui list ~default:(-1)) and query = ref query
  and result = ref `None in
  let edit = load_text_edit ui search.box_key !query in
  let (sx, _, sw, _) = ints (rect ui search) in
  let prefix = if slash then 13 else 0 in
  (* the slash is 6.5 wide and 6 of gap: the query starts 14.5 in from the field's edge *)
  let inset = if slash then 14.5 -. float prefix else 2. in
  point_text_caret ui edit search_signal ~shift:(press_shift ui search)
    ~x:(float (sx + 4 + prefix) +. inset) ~right:(float (sx + sw - 4 - 2));
  let set_query text = query := text; rows := rows_of text; cursor := 0; armed := -1 in
  List.iter (fun ((event : Input.event), modifiers) -> let count = count () in
    if !result = `None then match event with
    | Input.Key_pressed Input.Backspace when edit.text = "" -> result := `Back
    | Input.Key_pressed Input.ArrowLeft when edit.text = "" -> result := `Back
    | Input.Key_pressed Input.ArrowDown when count > 0 ->
        cursor := (!cursor + 1) mod count; armed := -1
    | Input.Key_pressed Input.ArrowUp when count > 0 ->
        cursor := (!cursor + count - 1) mod count; armed := -1
    | Input.Key_pressed Input.Enter ->
        result := if count > 0 then (if off !cursor then `None else `Pick !cursor) else `Submit
    | Input.Key_pressed Input.Delete when count > 0 &&
        edit.caret = edit.anchor ->
        if !armed = !cursor then (result := `Delete !cursor; armed := -1)
        else armed := !cursor
    | Input.Key_pressed Input.Escape -> result := `Cancel
    | event ->
        ignore (edit_text_event ui edit ~modifiers ~accept:(fun _ -> true) event);
        if edit.text <> !query then set_query edit.text) keys;
  save_text_edit ui edit;
  let count = count () and rows = !rows in
  let length = min limit count in
  let start = if length = count then 0
    else max 0 (min (count - length) (!cursor - (length / 2))) in
  let theme = ui.theme and composition = ui.composition in
  let placeholder = display label in
  draw ui search (fun paint rect ->
    let (x, y, w, h) = ints rect in
    let (cx, cy, cw, ch) as field = x + 4, y + 4, max 1 (w - 8), max 1 (h - 8) in
    (* a field that has not been typed in yet can sit at rest: the hairline and no caret *)
    let resting = at_rest && edit.text = "" in
    underline paint field (if resting then Theme.edge theme else theme.accent);
    if slash then kit_text paint ~color:(Theme.ink_2 theme) (cx + 2) (label_y ui cy ch) "/";
    let control = cx + prefix, cy, max 1 (cw - prefix), ch in
    if edit.text = "" then
      Paint.text paint ~color:(Theme.ink_3 theme) ~at:(float (cx + prefix) +. inset, float (label_y ui cy ch))
        placeholder;
    paint_text_edit paint ~inset ~caret:(edit.text <> "") ~control ~y:(label_y ui cy ch) ~composition edit);
  within ui list (fun () ->
    for visible = 0 to length - 1 do
      let index = start + visible in
      (* a row that cannot be picked leaves the keyboard with the search field *)
      let row = box_keyed ui
          ~flags:(clickable lor focusable lor blocking lor (if off index then keep_focus else 0))
          ~w:Grow ~h:(Px (float ui.kit_row_height)) (int_key list.box_key visible) in
      let row_signal = signal ui row in
      if !result = `None && row_signal.clicked && not (off index) then result := `Pick index;
      let current = index = !cursor and hovered = row_signal.hovered in
      let doomed = index = !armed in
      let text, detail = rows.(index) in
      let swatch = Option.bind mark (fun mark -> mark index) in
      draw ui row (fun paint rect ->
        let (x, y, w, h) as bounds = ints rect in
        (* the keyboard cursor is the control fill; a row armed for deletion reads in the error colour *)
        if doomed then fill paint bounds (Theme.pressed_fill theme)
        else if current then fill paint bounds theme.control
        else if hovered then hover_row paint ui bounds;
        let text_x = if mark = None then x + side else x + side + 16 in
        let unavailable = off index in
        (* a label that would run into the detail is cut with an ellipsis *)
        let key_size = max 8 (ui.font_size - 2) in
        let key_text = "\xe2\x86\xb5" in
        let right = if current then
            float (x + w - side) -. Paint.text_width paint ~size:key_size key_text -. 8.
          else float (x + w - side) in
        let text = ellipsis ~width:(Paint.text_width paint)
            ~limit:(right -. Paint.text_width paint detail -. 12. -. float text_x) text in
        Option.iter (fun color -> fill paint (x + side, y + ((h - 8) / 2), 8, 8)
          (if unavailable then Theme.ink_3 theme else color)) swatch;
        (* the query's letters, matched in order, in the accent *)
        let query = String.lowercase_ascii !query and lower = String.lowercase_ascii text in
        let matched = Array.make (String.length text) false in
        let next = ref 0 and at = ref 0 in
        while !at < String.length text do
          let decoded = String.get_utf_8_uchar text !at in
          let length = Uchar.utf_decode_length decoded in
          if !next < String.length query && String.length lower > !at && lower.[!at] = query.[!next]
          then (matched.(!at) <- true; incr next);
          at := !at + length
        done;
        let pen = ref (float text_x) and start = ref 0 in
        while !start < String.length text do
          let flag = matched.(!start) in
          let stop = ref !start in
          while !stop < String.length text
                && (!stop = !start || matched.(!stop) = flag || Char.code text.[!stop] land 0xc0 = 0x80) do
            incr stop
          done;
          let run = String.sub text !start (!stop - !start) in
          Paint.text paint ~at:(!pen, float (label_y ui y h))
            ~color:(if doomed then Theme.invalid else if unavailable then Theme.ink_3 theme
              else if flag then theme.accent else theme.foreground) run;
          pen := !pen +. Paint.text_width paint run;
          start := !stop
        done;
        let right = if current then
            (let width = Paint.text_width paint ~size:key_size key_text in
             Paint.text paint ~size:key_size ~color:(Theme.ink_3 theme)
               ~at:(float (x + w - side) -. width, text_top ui ~size:key_size (float y) (float h)) key_text;
             float (x + w - side) -. width -. 8.)
          else float (x + w - side) in
        Paint.text paint ~color:(if unavailable then Theme.ink_3 theme else Theme.muted theme)
          ~at:(right -. Paint.text_width paint detail, float (label_y ui y h)) detail)
    done);
  set_state ui search !cursor; set_state ui list !armed;
  !query, !result

(* A floating kit menu at [at], kept inside the frame. The host holds whether
   it is open; rows commit on press and release inside, disabled rows are
   inert, and Escape, focus loss, or a press outside dismiss it. *)
type submenu = { row : int; rows : (string * bool) list; keys : string list; current : int option }

let context_menu ui ~at:(x, y) ?width ?selected ?(swatches = []) ?(keys = []) ?(danger = [])
    ?(submenus = []) ?(lead_from = 0) label items =
  let submenu_key = key_of (current_seed ui) (label ^ "-sub") in
  (* the width follows the longest row; an empty label is a separator line; [keys] are the
     shortcuts at the right of the rows, in ink-2 at the label size *)
  let row_height = float ui.kit_row_height and gap = 9. in
  let key_size = max 8 (ui.font_size - 2) in
  let key_of keys index = match List.nth_opt keys index with Some key when key <> "" -> Some key | _ -> None in
  let swatch_pad = if List.exists Option.is_some swatches then 14. else 0. in
  let measure ~lead ~keys ~chevrons base items =
    (* the 1-point edge, 12 points of padding, then the label, 24 points and the key or chevron *)
    fst (List.fold_left (fun (w, index) (text, _) ->
      Float.max w (text_width ui text +. 26. +. lead +. swatch_pad
        +. (match key_of keys index with
            | Some key -> 24. +. text_width ui ~size:key_size key
            | None -> if List.mem index chevrons then 32.5 else 0.)),
      index + 1) (base, 0) items) in
  let height_of items = List.fold_left (fun h (text, _) -> h +. (if text = "" then gap else row_height)) 14. items in
  let lead = if selected = None then 0. else 14. in
  let width = Float.min ui.view_w (measure ~lead ~keys ~chevrons:(List.map (fun sub -> sub.row) submenus)
    (Option.value ~default:150. width) items) in
  let height = height_of items in
  let x = Float.max 0. (Float.min x (ui.view_w -. width))
  and y = Float.max 0. (Float.min y (ui.view_h -. height)) in
  (* the submenu of a row: a second menu overlapping this one by a point, level with its row *)
  let row_top index = y +. 7. +. List.fold_left (fun top (text, _) -> top +. (if text = "" then gap else row_height))
      0. (List.take (max 0 index) items) in
  let sub_geometry sub =
    let sub_width = Float.min ui.view_w (measure ~lead:(if sub.current = None then 0. else 14.) ~keys:sub.keys
      ~chevrons:[] 0. sub.rows) in
    let sub_height = height_of sub.rows in
    let sx = if x +. width -. 1. +. sub_width <= ui.view_w then x +. width -. 1. else x -. sub_width +. 1. in
    let sy = Float.max 0. (Float.min (row_top sub.row -. 7.) (ui.view_h -. sub_height)) in
    sx, sy, sub_width, sub_height in
  (* the rows of the submenus are numbered after those of this menu, in order *)
  let bases = fst (List.fold_left (fun (acc, n) sub -> (sub, n) :: acc, n + List.length sub.rows)
    ([], List.length items) submenus) in
  let base_of sub = List.assq sub bases in
  let pad name = box ui ~flags:blocking ~w:Grow ~h:(Px 6.) name in
  (* the rows of one menu: the row picked, and the row the pointer is on *)
  let rows_of ~lead_from ~items ~keys ~selected ~swatches ~danger ~opened ~chevrons =
    let hovered = ref None in
    let lead_of index = if selected = None || index < lead_from then 0 else 14 in
    let picked = List.mapi (fun index (text, enabled) ->
      if text = "" then begin
        let line = box ui ~flags:blocking ~w:Grow ~h:(Px gap) (Printf.sprintf "separator-%d" index) in
        draw ui line (fun paint (x, y, w, _) ->
          Paint.fill paint ~x ~y:(y +. 4.) ~w ~h:1. (Theme.edge ui.theme));
        None
      end else begin
        let row = kit_row ui ~flags:(if enabled then clickable lor focusable lor blocking lor tab_only else blocking)
            text in
        let signal = signal ui row in
        if enabled && signal.hovered then hovered := Some index;
        let theme = ui.theme and shown = display text in
        let chevron = List.mem index chevrons in
        draw ui row (fun paint rect ->
          let (_, y, _, h) as bounds = ints rect in
          let rx, _, _, _ = bounds in
          if enabled && (signal.hovered || opened = Some index) then fill paint bounds theme.control;
          (* the current choice: a 6-point accent square before the label *)
          if selected = Some index then
            Paint.fill paint ~x:(float rx +. float side) ~y:(float y +. (float h /. 2.) -. 3.) ~w:6. ~h:6. theme.accent;
          Option.iter (fun color ->
            let sx = float (rx + side + lead_of index) and sy = float y +. float h /. 2. -. 4. in
            Paint.fill paint ~x:sx ~y:sy ~w:8. ~h:8. color;
            Paint.frame paint ~x:sx ~y:sy ~w:8. ~h:8. (Theme.edge theme))
            (Option.join (List.nth_opt swatches index));
          (* the destructive row reads in the error ink *)
          kit_text paint ~color:(if not enabled then Theme.ink_3 theme
              else if List.mem index danger then Theme.invalid else theme.foreground)
            (rx + side + lead_of index + int_of_float swatch_pad) (label_y ui y h) shown;
          let _, _, w, _ = bounds in
          if chevron then Paint.chevron paint ~at:(float (rx + w) -. 15.25, float y +. (float h /. 2.))
            `Right (Theme.ink_2 theme);
          Option.iter (fun key ->
            Paint.text paint ~size:key_size ~color:(Theme.ink_2 theme)
              ~at:(float (rx + w - side) -. Paint.text_width paint ~size:key_size key,
                   text_top ui ~size:key_size (float y) (float h)) key) (key_of keys index));
        if enabled && signal.clicked && not chevron then Some index else None
      end) items in
    List.find_map Fun.id picked, !hovered in
  let keep = List.map sub_geometry submenus in
  let stroke = Theme.edge ui.theme in
  let opened = ref None in
  let related = if submenus = [] then [] else [submenu_key] in
  match popup_with_related ui ~stroke ~keep ~related ~at:(x, y) ~width ~height label (fun () ->
      let top = pad "menu-top" in
      let open_row = state ui top ~default:(-1) in
      opened := if open_row >= 0 then Some open_row else None;
      let picked, hovered = rows_of ~lead_from ~items ~keys ~selected ~swatches ~danger ~opened:!opened
          ~chevrons:(List.map (fun sub -> sub.row) submenus) in
      (* the pointer opens the submenu of a row, or closes it on any other *)
      Option.iter (fun index ->
        let now = if List.exists (fun sub -> sub.row = index) submenus then index else -1 in
        if now <> open_row then set_state ui top now;
        opened := if now >= 0 then Some now else None) hovered;
      ignore (pad "menu-bottom");
      picked) with
  | None -> dismiss_popup ui; `Dismiss
  | Some (Some index) -> dismiss_popup ui; `Pick index
  | Some None ->
      (match Option.bind !opened (fun row -> List.find_opt (fun sub -> sub.row = row) submenus) with
       | None -> `Open
       | Some sub ->
           let sx, sy, sw, sh = sub_geometry sub in
           let result = popup ui ~stroke ~attached:true ~at:(sx, sy) ~width:sw ~height:sh
               (label ^ "-sub") (fun () ->
             ignore (pad "menu-top");
             let picked, _ = rows_of ~lead_from:0 ~items:sub.rows ~keys:sub.keys ~selected:sub.current
                 ~swatches:[] ~danger:[] ~opened:None ~chevrons:[] in
             ignore (pad "menu-bottom");
             picked) in
           (match result with
            | Some (Some index) -> dismiss_popup ui; `Pick (base_of sub + index)
            | _ -> `Open))

let expanded ui text =
  let key = key_of (current_seed ui) text in
  let slot = Table.find ui.table key in
  if slot < 0 || ui.state_values.(slot) = min_int then None
  else Some (ui.state_values.(slot) = 1)

let ( + ) left right = left lor right
