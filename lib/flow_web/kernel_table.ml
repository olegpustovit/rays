let json_string = Text_literal.quote
let json_list f items = "[" ^ String.concat "," (List.map f items) ^ "]"
let json_ints a = json_list string_of_int (Array.to_list a)
let kind_name = function
  | Wgsl.Uniform_buffer -> "uniform" | Storage_read -> "read" | Storage_read_write -> "read_write"

let entry ?cpu ~id (w : Wgsl.wgsl) =
  let tables = json_list (fun seed -> json_ints (Rays_math.Noise.permutation (Rays_math.Noise.create seed)))
    (Array.to_list w.table_seeds) in
  Printf.sprintf
    "{\"id\":%s,%s\"entry\":%s,\"source\":%s,\"interface\":%s,\"uniform_layout\":%s,\"uniform_bytes\":%d,\"table_seeds\":%s,\"tables\":%s,\"input_widths\":%s,\"output_widths\":%s,\"gathers\":%s,\"counts\":%s}"
    (json_string id) (match cpu with Some c -> "\"cpu\":" ^ json_string c ^ "," | None -> "")
    (json_string w.entry) (json_string w.source)
    (json_list (fun (b : Wgsl.binding) -> Printf.sprintf "{\"group\":%d,\"binding\":%d,\"kind\":%s}" b.group b.binding (json_string (kind_name b.kind))) w.interface)
    (json_list (fun (n, o, width) -> Printf.sprintf "[%s,%d,%d]" (json_string n) o width) w.uniform_layout)
    w.uniform_bytes (json_ints w.table_seeds) tables (json_ints w.input_widths) (json_ints w.output_widths)
    (json_list (fun (u, width) -> Printf.sprintf "[%d,%d]" u width) (Array.to_list w.gathers))
    (match w.counts with Some o -> string_of_int o | None -> "null")

let table entries = "{\"kernels\":[" ^ String.concat ",\n" entries ^ "],\n\"compaction\":" ^ json_string Wgsl.compaction ^ "}\n"
