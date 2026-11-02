let capacity = 64
let lock = Mutex.create ()
let entries : (string * string, int ref) Hashtbl.t = Hashtbl.create 16

let note ~site reason =
  Mutex.protect lock (fun () ->
      match Hashtbl.find_opt entries (site, reason) with
      | Some count -> incr count
      | None -> if Hashtbl.length entries < capacity then Hashtbl.add entries (site, reason) (ref 1))

let seen () =
  Mutex.protect lock (fun () ->
      Hashtbl.fold (fun (site, reason) count all -> (site, reason, !count) :: all) entries [])
  |> List.sort (fun (a, b, _) (c, d, _) -> compare (a, b) (c, d))

let reset () = Mutex.protect lock (fun () -> Hashtbl.reset entries)

let summary () =
  match seen () with
  | [] -> None
  | (site, reason, _) :: rest ->
      Some
        (Printf.sprintf "off GPU: %s: %s%s" site reason
           (if rest = [] then "" else Printf.sprintf " (+%d)" (List.length rest)))
