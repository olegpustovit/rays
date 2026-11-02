type kind = Cache | Plan | Document

type account = {
  name : string;
  kind : kind;
  mutable bytes : int;
  mutable peak : int;
}

type owner = account

let accounts : (string * kind, account) Hashtbl.t = Hashtbl.create 32

let get kind name =
  let key = (name, kind) in
  match Hashtbl.find_opt accounts key with
  | Some account -> account
  | None ->
      let account = { name; kind; bytes = 0; peak = 0 } in
      Hashtbl.add accounts key account;
      account

let cache name = get Cache name
let plan name = get Plan name
let document name = get Document name
let name (account : owner) = account.name
let kind (account : owner) = account.kind
let bytes (account : owner) = account.bytes
let peak (account : owner) = account.peak

let charge (account : owner) n =
  if n > 0 then begin
    account.bytes <- account.bytes + n;
    if account.bytes > account.peak then account.peak <- account.bytes
  end

let release (account : owner) n =
  if n > 0 then account.bytes <- max 0 (account.bytes - n)

let reset () = Hashtbl.clear accounts

external phys_footprint_raw : unit -> int option = "caml_rays_mem_phys_footprint"
external instructions_raw : unit -> int64 option = "caml_rays_mem_instructions"

let phys_footprint = phys_footprint_raw
let instructions = instructions_raw

module Report = struct
  type entry = { name : string; kind : kind; bytes : int; peak : int }

  type t = {
    owners : entry list;
    attributed : int;
    ocaml_live : int;
    device : int;
    footprint : int;
    unattributed : int;
    budget : int;
  }

  let kind_label = function Cache -> "cache" | Plan -> "plan" | Document -> "document"

  let format_bytes n =
    if n >= 1_000_000_000 then Printf.sprintf "%.2f GB" (float n /. 1e9)
    else if n >= 1_000_000 then Printf.sprintf "%.1f MB" (float n /. 1e6)
    else if n >= 1_000 then Printf.sprintf "%.1f KB" (float n /. 1e3)
    else Printf.sprintf "%d B" n

  let pp fmt t =
    List.iter
      (fun (e : entry) ->
        Format.fprintf fmt "%-8s  %-28s  %10s  %10s@." (kind_label e.kind) e.name
          (format_bytes e.bytes) (format_bytes e.peak))
      t.owners;
    let explicit = t.ocaml_live + t.device in
    let pct =
      if explicit <= 0 then 0 else int_of_float (100. *. float t.unattributed /. float explicit)
    in
    Format.fprintf fmt
      "ocaml live %s · device %s · footprint %s · unattributed %s (%d%% of explicit) · budget %s@."
      (format_bytes t.ocaml_live) (format_bytes t.device) (format_bytes t.footprint)
      (format_bytes t.unattributed) pct (format_bytes t.budget)

  let to_string t = Format.asprintf "%a" pp t
end

let report ?(device = 0) ?(budget = 0) () =
  Gc.full_major ();
  let stat = Gc.stat () in
  let ocaml_live = stat.live_words * (Sys.word_size / 8) in
  let owners =
    Hashtbl.fold
      (fun _ (a : account) acc ->
        if a.peak = 0 && a.bytes = 0 then acc
        else { Report.name = a.name; kind = a.kind; bytes = a.bytes; peak = a.peak } :: acc)
      accounts []
    |> List.sort (fun (a : Report.entry) b ->
           let c = compare b.bytes a.bytes in
           if c <> 0 then c else String.compare a.name b.name)
  in
  let attributed = List.fold_left (fun sum (e : Report.entry) -> sum + e.bytes) 0 owners in
  let device = max 0 device in
  let footprint =
    match phys_footprint () with Some n -> n | None -> ocaml_live + device
  in
  (* Compare owners to explicit Rays memory (OCaml live + device). Footprint still includes
     OS/Metal/shared mappings that no owner can claim; gating on it hides real gaps. *)
  let explicit = ocaml_live + device in
  let unattributed = max 0 (explicit - attributed) in
  {
    Report.owners;
    attributed;
    ocaml_live;
    device;
    footprint;
    unattributed;
    budget = max 0 budget;
  }
