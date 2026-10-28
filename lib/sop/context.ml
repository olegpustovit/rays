module Cancel = Rdk.Cancel

module Dependencies = struct
  type fact = Frame | Time | Seed | Domains | Grain | Input
  type t = int

  let bit = function Frame -> 1 | Time -> 2 | Seed -> 4 | Domains -> 8 | Grain -> 16 | Input -> 32
  let static = 0
  let one fact = bit fact
  let union = ( lor )
  let mem fact value = value land bit fact <> 0
  let all = [ Frame; Time; Seed; Domains; Grain; Input ]
  let to_list value = List.filter (fun fact -> mem fact value) all

  let fact_name = function
    | Frame -> "frame"
    | Time -> "time"
    | Seed -> "seed"
    | Domains -> "domains"
    | Grain -> "grain"
    | Input -> "input"

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
    Buffer.contents buffer
