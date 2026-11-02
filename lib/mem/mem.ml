type kind = Cache | Plan | Document
type pool = Host | Device

type account = {
  name : string;
  kind : kind;
  pool : pool;
  bytes : int Atomic.t;
  peak : int Atomic.t;
}

type owner = account

type evictor = {
  owner : owner;
  rebuild : float;
  held : unit -> int;
  last_use : unit -> int;
  drop : unit -> bool;
  live : unit -> bool;
  forget : unit -> unit;
}

(* The tables below change when an owner or a table is made, never per charge: a charge touches only
   its account's atomics. *)
let lock = Mutex.create ()
let accounts : (string * kind * pool, account) Hashtbl.t = Hashtbl.create 32
let evictors : evictor list ref = ref []
let evictor_mark = ref 16
let locked f = Mutex.protect lock f

let get ?(pool = Host) kind name =
  let key = (name, kind, pool) in
  locked (fun () ->
      match Hashtbl.find_opt accounts key with
      | Some account -> account
      | None ->
          let account =
            { name; kind; pool; bytes = Atomic.make 0; peak = Atomic.make 0 }
          in
          Hashtbl.add accounts key account;
          account)

let cache ?pool name = get ?pool Cache name
let plan ?pool name = get ?pool Plan name
let document ?pool name = get ?pool Document name
let name (account : owner) = account.name
let kind (account : owner) = account.kind
let pool (account : owner) = account.pool
let bytes (account : owner) = Atomic.get account.bytes
let peak (account : owner) = Atomic.get account.peak

let rec raise_peak (account : owner) value =
  let seen = Atomic.get account.peak in
  if value > seen && not (Atomic.compare_and_set account.peak seen value) then
    raise_peak account value

let charge (account : owner) n =
  if n > 0 then raise_peak account (Atomic.fetch_and_add account.bytes n + n)

let rec release (account : owner) n =
  if n > 0 then begin
    let seen = Atomic.get account.bytes in
    if not (Atomic.compare_and_set account.bytes seen (max 0 (seen - n))) then release account n
  end

let reachable_bytes value = Obj.reachable_words (Obj.repr value) * (Sys.word_size / 8)

(* Owners sized by a walk: what compiled programs and lowered records hold has no size to charge as
   it is built. Named roots first, then every table holding nothing on the ledger, oldest first. *)
let rooted : (owner * (unit -> Obj.t list)) list ref = ref []
let tables : (owner * Obj.t Weak.t * (unit -> bool)) list ref = ref []
let tables_added = ref 0
let walked : (owner * int) list ref = ref []

let root (account : owner) roots =
  locked (fun () ->
      rooted :=
        if List.mem_assq account !rooted then
          List.map (fun ((a, _) as e) -> if a == account then (a, roots) else e) !rooted
        else !rooted @ [ (account, roots) ])

let walk_table (account : owner) table ~uncharged =
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some (Obj.repr table));
  locked (fun () ->
      incr tables_added;
      if !tables_added land 255 = 0 then
        tables := List.filter (fun (_, w, _) -> Weak.check w 0) !tables;
      tables := (account, weak, uncharged) :: !tables)

(* On a report, after its collection: each owner is what its roots reach beyond those walked before
   it, so a plane or a catalog two owners reach is charged once. O(owners * reach): never per frame. *)
let walk_roots ~live:heap_live =
  let named, counted = locked (fun () -> (!rooted, List.rev !tables)) in
  (* tables by owner: the walked ones as owners here, the ones charging their own bytes as ledger *)
  let group add (account, table) grouped =
    if add then
      (account, table :: Option.value ~default:[] (List.assq_opt account grouped))
      :: List.remove_assq account grouped
    else grouped
  in
  let live = List.filter_map (fun (a, w, u) -> Option.map (fun t -> (a, t, u ())) (Weak.get w 0)) counted in
  let walked_tables, ledger =
    List.fold_left
      (fun (w, l) (account, table, uncharged) ->
        (group uncharged (account, table) w, group (not uncharged) (account, table) l))
      ([], []) live
  in
  let words o = Obj.reachable_words (Obj.repr o) * (Sys.word_size / 8) in
  (* A table charges what it holds, so a root reaching it (a lowered document's closures reach the
     cook's caches) must not claim those bytes again: the overlap, at most what it charges, once. *)
  let ledger =
    List.filter_map
      (fun ((account : owner), held) ->
        if account.pool <> Host then None
        else
          let walked_before = Option.value ~default:0 (List.assq_opt account !walked) in
          Some (account, ref (max 0 (Atomic.get account.bytes - walked_before)), held))
      ledger
  in
  let already_charged before after extra =
    if extra < 65_536 then 0
    else
      List.fold_left
        (fun sum (_, left, held) ->
          (* of what this walk adds, what lies in the table *)
          if !left < 65_536 then sum
          else
            let inside = extra - (words (after, held) - words (before, held)) in
            let credit = max 0 (min (extra - sum) (min !left inside)) in
            left := !left - credit;
            sum + credit)
        0 ledger
  in
  let walks = named @ List.map (fun (a, held) -> (a, fun () -> List.map Obj.repr held)) walked_tables in
  let seen = ref [] and total = ref 0 and now = ref [] in
  List.iter
    (fun ((account : owner), roots) ->
      let before = !seen in
      seen := roots () @ before;
      let reach = words !seen in
      let extra = max 0 (reach - !total) in
      total := reach;
      let extra = max 0 (extra - already_charged before !seen extra) in
      now :=
        (account, extra + Option.value ~default:0 (List.assq_opt account !now))
        :: List.remove_assq account !now)
    walks;
  (* last, what a table holds beyond its charge and every walk: boxed texels it counted as bytes, the
     keys and closures of its entries, which no other owner would ever claim. Up to what the heap has
     left: a table can also reach what an owner charges with no table (a canvas's bytes). *)
  let host_walked assoc =
    List.fold_left (fun sum ((a : owner), n) -> if a.pool = Host then sum + n else sum) 0 assoc
  in
  let unclaimed () =
    heap_live - host_walked !now + host_walked !walked
    - locked (fun () ->
          Hashtbl.fold (fun _ (a : account) sum -> if a.pool = Host then sum + bytes a else sum) accounts 0)
  in
  List.iter
    (fun ((account : owner), left, held) ->
      if words held > !left + 65_536 then begin
        seen := List.map Obj.repr held @ !seen;
        let reach = words !seen in
        let beyond = min (max 0 (reach - !total - !left)) (unclaimed ()) in
        total := reach;
        if beyond >= 65_536 then now := (account, beyond) :: List.remove_assq account !now
      end)
    ledger;
  List.iter (fun (account, bytes) -> if not (List.mem_assq account !now) then release account bytes) !walked;
  List.iter
    (fun (account, bytes) ->
      let before = Option.value ~default:0 (List.assq_opt account !walked) in
      if bytes > before then charge account (bytes - before) else release account (before - bytes))
    !now;
  walked := !now

let accounts_sum keep =
  locked (fun () ->
      Hashtbl.fold (fun _ (a : account) sum -> if keep a then sum + bytes a else sum) accounts 0)

let attributed () = accounts_sum (fun _ -> true)
let attributed_device () = accounts_sum (fun a -> a.pool = Device)

(* ---- process facts, one small function per OS each in the stubs *)

external phys_footprint : unit -> int option = "caml_rays_mem_phys_footprint"
external peak_footprint : unit -> int option = "caml_rays_mem_peak_footprint"
external instructions : unit -> int64 option = "caml_rays_mem_instructions"
external physical_ram : unit -> int = "caml_rays_mem_physical_ram"
external pressure_start : unit -> bool = "caml_rays_mem_pressure_start"
external pressure_state : unit -> int = "caml_rays_mem_pressure_state" [@@noalloc]
external pressure_inject : int -> unit = "caml_rays_mem_pressure_inject" [@@noalloc]
external pressure_relief : unit -> unit = "caml_rays_mem_pressure_relief"

(* ---- the budget: who set it wins by rank *)

type source = Machine | Preference | Override

let rank = function Machine -> 1 | Preference -> 2 | Override -> 3
let current_budget = Atomic.make 0
let budget_rank = Atomic.make 0

let set_budget ?(source = Override) n =
  if rank source >= Atomic.get budget_rank then begin
    Atomic.set budget_rank (rank source);
    Atomic.set current_budget (max 0 n)
  end

let budget () = Atomic.get current_budget

let machine_budget ~working_set =
  let ram = physical_ram () * 6 / 10 in
  match (working_set > 0, ram > 0) with
  | true, true -> min working_set ram
  | true, false -> working_set
  | false, _ -> ram

let parse_budget s =
  let len = String.length s in
  if len = 0 then None
  else
    let scale, digits =
      match Char.uppercase_ascii s.[len - 1] with
      | 'G' -> (1_000_000_000, String.sub s 0 (len - 1))
      | 'M' -> (1_000_000, String.sub s 0 (len - 1))
      | 'K' -> (1_000, String.sub s 0 (len - 1))
      | _ -> (1, s)
    in
    match int_of_string_opt digits with Some n when n >= 0 -> Some (n * scale) | _ -> None

let clobbering = Atomic.make false
let clobber () = Atomic.get clobbering

let configure read =
  Atomic.set clobbering (read "RAYS_MEM_CLOBBER" = Some "1");
  match read "RAYS_MEM_BUDGET" with
  | None | Some "" -> Ok ()
  | Some text -> (
      match parse_budget text with
      | Some n ->
          set_budget n;
          Ok ()
      | None -> Error (Printf.sprintf "RAYS_MEM_BUDGET: %S is not a size like 4G, 200M or 1000" text))

(* ---- evictors *)

let prune () =
  let dead =
    locked (fun () ->
        let live, dead = List.partition (fun e -> e.live ()) !evictors in
        evictors := live;
        evictor_mark := max 16 (2 * List.length live);
        dead)
  in
  List.iter (fun e -> e.forget ()) dead

let epoch_counter = Atomic.make 0
let epoch () = Atomic.get epoch_counter

let register_evictor owner ?(rebuild = 1.) ?held ?(last_use = fun () -> 0) ?(live = fun () -> true)
    ?(forget = ignore) drop =
  if kind owner = Cache then begin
    let held = match held with Some held -> held | None -> fun () -> bytes owner in
    let full =
      locked (fun () ->
          evictors := { owner; rebuild; held; last_use; drop; live; forget } :: !evictors;
          List.compare_length_with !evictors !evictor_mark > 0)
    in
    (* amortised: a table made per call (a host session) does not pay a walk each *)
    if full then prune ()
  end

let evictor_count () = locked (fun () -> List.length !evictors)

(* Coldest and largest per rebuild cost: frames since last use weigh the bytes a drop would free. *)
let score now e = float (1 + max 0 (now - e.last_use ())) *. float (e.held ()) /. e.rebuild

(* One entry of the best-scoring cache that can drop one right now. A cache that cannot (busy, on
   another domain, all pinned) stays out for the rest of this retreat. *)
let drop_coldest ~refused =
  let now = epoch () in
  let ranked =
    locked (fun () -> !evictors)
    |> List.filter_map (fun e ->
        if List.memq e !refused || not (e.live ()) then None
        else
          let s = score now e in
          if s > 0. then Some (s, e) else None)
    |> List.sort (fun (a, _) (b, _) -> Float.compare b a)
  in
  let rec go = function
    | [] -> false
    | (_, e) :: rest ->
        if e.drop () then true
        else begin
          refused := e :: !refused;
          go rest
        end
  in
  go ranked

(* ---- the retreat *)

type overdraft = { used : int; limit : int; top : (string * int) list }
type verdict = Within | Over of overdraft

let top_owners n =
  locked (fun () -> Hashtbl.fold (fun _ (a : account) acc -> (a.name, bytes a) :: acc) accounts [])
  |> List.filter (fun (_, b) -> b > 0)
  |> List.sort (fun (a, x) (b, y) ->
      let c = compare y x in
      if c <> 0 then c else String.compare a b)
  |> List.filteri (fun i _ -> i < n)

let format_bytes n =
  if n >= 1_000_000_000 then Printf.sprintf "%.2f GB" (float n /. 1e9)
  else if n >= 1_000_000 then Printf.sprintf "%.1f MB" (float n /. 1e6)
  else if n >= 1_000 then Printf.sprintf "%.1f KB" (float n /. 1e3)
  else Printf.sprintf "%d B" n

let describe { used; limit; top } =
  Printf.sprintf "%s in use over the %s budget; largest owners: %s" (format_bytes used)
    (format_bytes limit)
    (match top with
    | [] -> "none attributed"
    | top ->
        String.concat ", "
          (List.map (fun (name, bytes) -> name ^ " " ^ format_bytes bytes) top))

let total_owner () = match top_owners 1 with [ top ] -> Some top | _ -> None

(* What the footprint still shows of bytes already dropped: garbage until a major cycle passes, so
   the next boundary and a cook do not count it again. *)
let shed_bytes = Atomic.make 0
let shed_cycle = Atomic.make 0
let major_cycles () = (Gc.quick_stat ()).major_collections

let credit () =
  let pending = Atomic.get shed_bytes in
  if pending = 0 then 0
  else if major_cycles () >= Atomic.get shed_cycle + 2 then begin
    Atomic.set shed_bytes 0;
    0
  end
  else pending

let note_shed n =
  if n > 0 then begin
    if Atomic.get shed_bytes = 0 then Atomic.set shed_cycle (major_cycles ());
    ignore (Atomic.fetch_and_add shed_bytes n)
  end

(* The footprint is what the OS charges, the device on one memory pool included. Where it cannot be
   read: what the host owners claim, plus what the backend measures. *)
let measured ?(device = fun () -> 0) () =
  match phys_footprint () with
  | Some n -> n
  | None -> accounts_sum (fun a -> a.pool = Host) + max 0 (device ())

(* A pressure event's lowered limit, until the system says normal again. *)
let lowered = Atomic.make 0
let seen_pressure = Atomic.make 0

let limit () =
  let base = budget () and low = Atomic.get lowered in
  if low > 0 && (base = 0 || low < base) then low else base

(* Below this many frames since the last one, a crossing is not worth a stop-the-world: a process
   hovering at its limit must not stall on each crossing. *)
let collect_gap = 120
let last_collect = Atomic.make min_int

let collected_now () =
  let now = epoch () in
  if now - Atomic.get last_collect < collect_gap && Atomic.get last_collect <> min_int then false
  else begin
    Atomic.set last_collect now;
    Gc.full_major ();
    Atomic.set shed_bytes 0;
    true
  end

let over_latch : overdraft option Atomic.t = Atomic.make None
let overdraft () = Atomic.get over_latch

let apply_pressure () =
  let state = pressure_state () in
  let critical = ref false in
  if state <> Atomic.exchange seen_pressure state then begin
    match state land 3 with
    | 0 -> Atomic.set lowered 0
    | level ->
        (* below the footprint, so the same steps run *)
        Atomic.set lowered (max 1 (measured ?device:None () * 9 / 10));
        critical := level = 2
  end;
  !critical

(* what the status shows, from a quarter below the limit up: a frame pays for nothing else *)
let near_line : string option Atomic.t = Atomic.make None
let status_line () = Atomic.get near_line

let note_near used bound =
  Atomic.set near_line
    (if bound > 0 && used / 3 >= bound / 4 then
       Some
         (Printf.sprintf "mem %s of %s%s" (format_bytes used) (format_bytes bound)
            (match total_owner () with
            | Some (name, bytes) -> Printf.sprintf " \xc2\xb7 top %s %s" name (format_bytes bytes)
            | None -> ""))
     else None)

let retreat ?device ?usage ?(later = []) () =
  Atomic.incr epoch_counter;
  let critical = apply_pressure () in
  let bound = limit () in
  let read = match usage with Some f -> f | None -> measured ?device in
  let finish verdict =
    if critical then begin
      Gc.full_major ();
      pressure_relief ()
    end;
    (match verdict with
    | Within -> Atomic.set over_latch None
    | Over overdraft -> Atomic.set over_latch (Some overdraft));
    verdict
  in
  if bound <= 0 then begin
    Atomic.set near_line None;
    finish Within
  end
  else
    let used = read () - if usage = None then credit () else 0 in
    note_near used bound;
    if used <= bound then finish Within
    else begin
      let before = attributed () in
      let excess = used - bound in
      let shed () = before - attributed () in
      let refused = ref [] in
      while shed () < excess && drop_coldest ~refused do
        ()
      done;
      List.iter
        (fun step ->
          while shed () < excess && step () do
            ()
          done)
        later;
      let dropped = max 0 (shed ()) in
      note_shed dropped;
      if dropped >= excess then finish Within
      else
        (* Everything rebuildable went. Dead blocks may still be most of the footprint, so a verdict
           comes from a full collection, once per crossing: never per frame. *)
        let latched = Atomic.get over_latch <> None in
        let used =
          if (not latched) && usage = None && collected_now () then read () else used - dropped
        in
        if used <= bound then finish Within
        else finish (Over { used; limit = bound; top = top_owners 4 })
    end

(* The limit a cook was last refused under: the same cook under the same limit would only reach past
   it again, with the caches it dropped to make room to rebuild. *)
let refused_under = Atomic.make 0
let retry_worthwhile () = let bound = limit () in bound <= 0 || bound > Atomic.get refused_under


let admit ?(shed = fun () -> false) () =
  let bound = limit () in
  if bound <= 0 then Ok ()
  else
    let used () = measured ?device:None () - credit () in
    if used () <= bound then Ok ()
    else begin
      let before = attributed () in
      while before - attributed () < used () - bound && shed () do
        ()
      done;
      note_shed (max 0 (before - attributed ()));
      if used () <= bound then Ok ()
      else begin
        ignore (collected_now ());
        let used = used () in
        if used <= bound then Ok ()
        else begin
          Atomic.set refused_under bound;
          Error { used; limit = bound; top = top_owners 4 }
        end
      end
    end

exception Refused of overdraft

let admit_bytes n =
  let bound = limit () in
  if bound > 0 then begin
    let used () = measured ?device:None () - credit () in
    if used () + n > bound then begin
      ignore (collected_now ());
      let used = used () in
      if used + n > bound then begin
        Atomic.set refused_under bound;
        raise (Refused { used = used + n; limit = bound; top = top_owners 4 })
      end
    end
  end

let reset () =
  locked (fun () ->
      Hashtbl.iter
        (fun _ (a : account) ->
          Atomic.set a.bytes 0;
          Atomic.set a.peak 0)
        accounts;
      evictors := [];
      rooted := [];
      tables := [];
      walked := []);
  Atomic.set current_budget 0;
  Atomic.set budget_rank 0;
  Atomic.set lowered 0;
  Atomic.set over_latch None;
  Atomic.set refused_under 0;
  Atomic.set shed_bytes 0;
  Atomic.set last_collect min_int

module Report = struct
  type entry = { name : string; kind : kind; pool : pool; bytes : int; peak : int }

  type t = {
    owners : entry list;
    attributed : int;
    attributed_device : int;
    ocaml_live : int;
    device : int;
    footprint : int;
    unattributed : int;
    overclaimed : int;
    budget : int;
  }

  let kind_label = function Cache -> "cache" | Plan -> "plan" | Document -> "document"
  let pool_label = function Host -> "host" | Device -> "device"
  let format_bytes = format_bytes

  let pp fmt t =
    List.iter
      (fun (e : entry) ->
        Format.fprintf fmt "%-8s  %-6s  %-30s  %10s  %10s@." (kind_label e.kind)
          (pool_label e.pool) e.name (format_bytes e.bytes) (format_bytes e.peak))
      t.owners;
    let explicit = t.ocaml_live + t.device in
    let pct =
      if explicit <= 0 then 0 else int_of_float (100. *. float t.unattributed /. float explicit)
    in
    Format.fprintf fmt
      "ocaml live %s · device %s · footprint %s · unattributed %s (%d%% of explicit)%s · budget %s@."
      (format_bytes t.ocaml_live) (format_bytes t.device) (format_bytes t.footprint)
      (format_bytes t.unattributed) pct
      (if t.overclaimed = 0 then "" else " · overclaimed " ^ format_bytes t.overclaimed)
      (format_bytes t.budget)

  let to_string t = Format.asprintf "%a" pp t
end

let report ?(collect = true) ?(walk = collect) ?(device = 0) ?budget:shown () =
  if collect then Gc.full_major ();
  (* before the walk, whose hash set is the allocator's afterwards *)
  let footprint = phys_footprint () in
  let stat = if collect then Gc.stat () else Gc.quick_stat () in
  let ocaml_live =
    (if collect then stat.live_words else stat.heap_words) * (Sys.word_size / 8)
  in
  if walk then walk_roots ~live:ocaml_live;
  prune ();
  let owners =
    locked (fun () ->
        Hashtbl.fold
          (fun _ (a : account) acc ->
            if peak a = 0 && bytes a = 0 then acc
            else
              { Report.name = a.name; kind = a.kind; pool = a.pool; bytes = bytes a; peak = peak a }
              :: acc)
          accounts [])
    |> List.sort (fun (a : Report.entry) b ->
        let c = compare b.bytes a.bytes in
        if c <> 0 then c else String.compare a.name b.name)
  in
  let sum pool =
    List.fold_left
      (fun sum (e : Report.entry) -> if e.pool = pool then sum + e.bytes else sum)
      0 owners
  in
  let host = sum Host and attributed_device = sum Device in
  let device = max 0 device in
  let footprint = match footprint with Some n -> n | None -> ocaml_live + device in
  {
    Report.owners;
    attributed = host + attributed_device;
    attributed_device;
    ocaml_live;
    device;
    footprint;
    unattributed = max 0 (ocaml_live - host) + max 0 (device - attributed_device);
    overclaimed = max 0 (host - ocaml_live) + max 0 (attributed_device - device);
    budget = (match shown with Some n -> max 0 n | None -> budget ());
  }

module Private = struct
  let pressure_inject = pressure_inject
  let major_cycles = major_cycles
end
