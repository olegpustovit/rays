module Cancel = Rdk.Cancel

module Dependencies = struct
  type fact = Frame | Time | Seed | Domains | Grain | Input | Pointer | Keys | Size | Step
  type t = int

  let bit = function
    | Frame -> 1
    | Time -> 2
    | Seed -> 4
    | Domains -> 8
    | Grain -> 16
    | Input -> 32
    | Pointer -> 64
    | Keys -> 128
    | Size -> 256
    | Step -> 512

  let static = 0
  let one fact = bit fact
  let union = ( lor )
  let mem fact value = value land bit fact <> 0
  let all = [ Frame; Time; Seed; Domains; Grain; Input; Pointer; Keys; Size; Step ]
  let to_list value = List.filter (fun fact -> mem fact value) all

  let fact_name = function
    | Frame -> "frame"
    | Time -> "time"
    | Seed -> "seed"
    | Domains -> "domains"
    | Grain -> "grain"
    | Input -> "input"
    | Pointer -> "pointer"
    | Keys -> "keys"
    | Size -> "size"
    | Step -> "step"

  let to_string value =
    match to_list value with
    | [] -> "static"
    | facts -> String.concat "," (List.map fact_name facts)
end

type t = {
  frame : int64;
  time : float;
  input : Input.t;
  seed : int64;
  domains : int;
  grain : int;
  cancel : Cancel.t;
  deferred : (Domain.id * int * (unit -> (unit, Diagnostic.t) result)) list Atomic.t;
  refused : int list Atomic.t;
}

let create ?(frame = 0L) ?(time = 0.) ?input ?(seed = 0L) ?domains ?(grain = 16_384) ?cancel () =
  let domains = Option.value ~default:(Parallel.recommended_domains ()) domains in
  if not (Float.is_finite time) then Error "Context.create: time must be finite"
  else if frame < 0L then Error "Context.create: frame must be non-negative"
  else if domains <= 0 then Error "Context.create: domains must be positive"
  else if grain <= 0 then Error "Context.create: grain must be positive"
  else if frame > Int64.of_int max_int then
    Error "Context.create: frame exceeds the logical frame index range"
  else
    let input =
      {
        (Option.value ~default:(Input.at_time time) input) with
        Input.t = time;
        frame = Int64.to_int frame;
      }
    in
    Result.map
      (fun () ->
        {
          frame;
          time;
          input;
          seed;
          domains;
          grain;
          cancel = Option.value ~default:(Cancel.create ()) cancel;
          deferred = Atomic.make [];
          refused = Atomic.make [];
        })
      (Input.validate input)

let frame value = value.frame
let time value = value.time
let input value = value.input
let seed value = value.seed
let domains value = value.domains
let grain value = value.grain
let cancel_token value = value.cancel
let cancelled value = Cancel.is_cancelled value.cancel

let rec push cell value =
  let old = Atomic.get cell in
  if not (Atomic.compare_and_set cell old (value :: old)) then push cell value

let defer value ~node settle = push value.deferred (Domain.self (), node, settle)
let device_refused value node = List.mem node (Atomic.get value.refused)
let refusals value = List.length (Atomic.get value.refused)

let rec take_own cell =
  let old = Atomic.get cell in
  let self = Domain.self () in
  let own, others = List.partition (fun (d, _, _) -> d = self) old in
  if own = [] then [] else if Atomic.compare_and_set cell old others then own else take_own cell

let settle ?(all = false) value =
  match if all then Atomic.exchange value.deferred [] else take_own value.deferred with
  | [] -> []
  | pending ->
      (* oldest first; every pending check runs, so each failed node is known *)
      List.filter_map
        (fun (_, node, settle) ->
          match settle () with
          | Ok () -> None
          | Error _ ->
              push value.refused node;
              Some node)
        (List.rev pending)

let input_part (fact : Dependencies.fact) (i : Input.t) =
  let bits = Marshal.to_string in
  match fact with
  | Time -> bits i.t []
  | Pointer -> bits (i.pointer, i.buttons) []
  | Keys -> bits i.keys []
  | Size -> bits i.size []
  | Step -> bits (i.dt, i.tick, i.frame) []
  | Input -> Input.identity i
  | Frame | Seed | Domains | Grain -> ""

let input_changed dependencies a b =
  List.exists
    (fun (fact : Dependencies.fact) -> input_part fact a <> input_part fact b)
    (List.filter
       (fun f -> Dependencies.mem f dependencies)
       Dependencies.[ Time; Pointer; Keys; Size; Step; Input ])

let cache_projection dependencies value =
  if dependencies = Dependencies.static then ""
  else
    let buffer = Buffer.create 80 in
    let add tag data =
      Buffer.add_char buffer tag;
      Buffer.add_string buffer data;
      Buffer.add_char buffer ';'
    in
    if Dependencies.mem Frame dependencies then add 'f' (Int64.to_string value.frame);
    if Dependencies.mem Time dependencies then
      add 't' (Int64.to_string (Int64.bits_of_float value.time));
    if Dependencies.mem Seed dependencies then add 's' (Int64.to_string value.seed);
    if Dependencies.mem Domains dependencies then add 'd' (string_of_int value.domains);
    if Dependencies.mem Grain dependencies then add 'g' (string_of_int value.grain);
    if Dependencies.mem Input dependencies then add 'i' (Input.identity value.input);
    List.iter
      (fun (tag, fact) ->
        if Dependencies.mem fact dependencies then add tag (input_part fact value.input))
      Dependencies.[ ('p', Pointer); ('k', Keys); ('z', Size); ('e', Step) ];
    Buffer.contents buffer
