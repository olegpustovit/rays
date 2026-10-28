(* Instruction-level snapshots of every packed program a test compiles. *)
module P = Flow_ir.Packed
let buffer = Buffer.create 4096

let rec instruction : P.instruction -> string = function
  | Const x -> Printf.sprintf "Const %h" x
  | Input (i, w, c) -> Printf.sprintf "Input %d %d %d" i w c
  | Uniform (i, c) -> Printf.sprintf "Uniform %d %d" i c
  | Frame name -> Printf.sprintf "Frame %S" name
  | Accumulator c -> Printf.sprintf "Accumulator %d" c
  | Binary (op, a, b) -> Printf.sprintf "Binary %s %d %d" (Flow.Packed_ops.(match op with
      | Add -> "Add" | Sub -> "Sub" | Mul -> "Mul" | Div -> "Div" | Mod -> "Mod" | Pow -> "Pow"
      | Min -> "Min" | Max -> "Max" | Lt -> "Lt" | Le -> "Le" | Gt -> "Gt" | Ge -> "Ge"
      | Eq -> "Eq" | And -> "And" | Or -> "Or" | Atan2->"Atan2" | Hypot->"Hypot")) a b
  | Unary (op, a) -> Printf.sprintf "Unary %s %d" (Flow.Packed_ops.(match op with
      | Sin -> "Sin" | Cos -> "Cos" | Sqrt -> "Sqrt" | Abs -> "Abs" | Not -> "Not"
      |Tan->"Tan"|Asin->"Asin"|Acos->"Acos"|Exp->"Exp"|Log->"Log"|Sign->"Sign")) a
  | Noise3 (x, y, z, seed, octaves, lacunarity, gain) -> Printf.sprintf "Noise3 %d %d %d seed=%d octaves=%d%s" x y z seed octaves (if lacunarity = 2. && gain = 0.5 then "" else Printf.sprintf " lacunarity=%h gain=%h" lacunarity gain)
  | Stage (_, r) -> "Stage" ^ String.concat "" (List.map (Printf.sprintf " %d") (Array.to_list r))
  | Trace (field, r) -> Printf.sprintf "Trace %d%s" field (String.concat "" (List.map (Printf.sprintf " %d") (Array.to_list r)))
  | Fma (a,b,c) -> Printf.sprintf "Fma %d %d %d" a b c
  | Checked_mul(p,a,b) -> Printf.sprintf "Checked_mul %s %d %d" (match p with Value->"value"|Valid->"valid") a b
  | Array_prepare(i,ty) -> Printf.sprintf "Array_prepare %d %s" i (Flow.Ty.to_string ty)
  | Gather(a,i,w,c) -> Printf.sprintf "Gather %d %d %d %d" a i w c
  | Int_gather(a,i) -> Printf.sprintf "Int_gather %d %d" a i
  | Text_const s -> Printf.sprintf "Text_const %S" s
  | Text_input i -> Printf.sprintf "Text_input %d" i
  | Text_uniform i -> Printf.sprintf "Text_uniform %d" i
  | Text_gather(a,i) -> Printf.sprintf "Text_gather %d %d" a i
  | Text_select(c,a,b) -> Printf.sprintf "Text_select %d %d %d" c a b
  | Checked_fma(p,a,b,c) -> Printf.sprintf "Checked_fma %s %d %d %d" (match p with Value->"value"|Valid->"valid") a b c
  | Rand_at (seed, index) -> Printf.sprintf "Rand_at %d %d" seed index
  | Select (c, a, b) -> Printf.sprintf "Select %d %d %d" c a b
  | Int_const n->Printf.sprintf "Int_const %d" n
  | Int_input n->Printf.sprintf "Int_input %d" n | Int_uniform n->Printf.sprintf "Int_uniform %d" n
  | Int_accumulator->"Int_accumulator"
  | Int_frame name->Printf.sprintf "Int_frame %S" name
  | Int_binary(op,a,b)->Printf.sprintf "Int_binary %s %d %d" (Flow.Packed_ops.integer_name op) a b
  | Int_compare(op,a,b)->Printf.sprintf "Int_compare %s %d %d" (Flow.Packed_ops.comparison_name op) a b
  | Int_abs a->Printf.sprintf "Int_abs %d" a
  | Int_select(c,a,b)->Printf.sprintf "Int_select %d %d %d" c a b
  | Convert(op,a)->Printf.sprintf "Convert %s %d" (match op with To_float->"float"|Truncate->"int"|Floor->"floor"|Ceil->"ceil"|Round->"round"|Coerce_int->"coerce:int"|Integer_result n->"int-result:"^n) a
  | Guarded(guards,body)->Printf.sprintf "Guarded %s (%s)" (String.concat ";" (List.map(fun(r,b)->Printf.sprintf "%d:%b" r b)guards))(instruction body)
  | Raise(ty,code,message,cause)->Printf.sprintf "Raise %s %S %S%s"(Flow.Ty.to_string ty)code message
      (Option.fold ~none:"" ~some:(Printf.sprintf " cause=%S")cause)
  | Loop_begin loop -> Printf.sprintf "Loop_begin %d %d end=%d" loop.first loop.last loop.finish
  | Loop_carry(loop,component,ty) -> Printf.sprintf "Loop_carry %d %d %s" loop component (Flow.Ty.to_string ty)
  | Loop_end(loop,_,stop) -> Printf.sprintf "Loop_end %d%s" loop
      (Option.fold ~none:"" ~some:(Printf.sprintf " until=%d") stop)
  | Loop_result(loop,component,ty) -> Printf.sprintf "Loop_result %d %d %s" loop component (Flow.Ty.to_string ty)

let ints a = "[" ^ String.concat ";" (Array.to_list (Array.map string_of_int a)) ^ "]"

let seen = Hashtbl.create 64

let record result =
  let entry = Buffer.create 256 in
  let add format = Printf.bprintf entry format in
  (match result with
  | Error (ds : Diagnostic.t list) ->
      List.iter (fun (d : Diagnostic.t) ->
        add "refused %s%s %S\n" d.code
          (match d.span with Some s -> Printf.sprintf " %d..%d" s.start s.finish | None -> "") d.message) ds;
      add "\n"
  | Ok packed ->
      let view = P.Host.view packed in
      add "program\n";
      Array.iteri (fun i x -> add "  r%d = %s\n" i (instruction x)) view.code;
      add "  widths %s\n%s  uniform_widths %s\n  collecting %b\n"
        (ints view.widths)
        (String.concat "" (Array.to_list (Array.map (fun o -> "  output " ^ ints o ^ "\n") view.outputs)))
        (ints view.uniform_widths) view.collecting;
      List.iter (fun (d : Diagnostic.t) -> add "  gpu_refusal %s %S\n" d.code d.message)
        (P.gpu_refusals packed);
      add "\n");
  let text = Buffer.contents entry in
  if not (Hashtbl.mem seen text) then (Hashtbl.add seen text (); Buffer.add_string buffer text)

let start () =
  Buffer.clear buffer; Hashtbl.reset seen

let finish name =
  let text = Buffer.contents buffer in
  match Sys.getenv_opt "REGION_SNAPSHOT_DIR" with
  | Some dir -> Out_channel.with_open_text (Filename.concat dir (name ^ ".txt")) (fun c -> output_string c text)
  | None ->
      let expected = In_channel.with_open_text (Filename.concat "snapshots" (name ^ ".txt")) In_channel.input_all in
      if expected <> text then begin
        Out_channel.with_open_text (name ^ ".actual") (fun c -> output_string c text);
        let a = String.split_on_char '\n' expected and b = String.split_on_char '\n' text in
        let rec first n = function
          | x :: xs, y :: ys when x = y -> first (n + 1) (xs, ys)
          | x :: _, y :: _ -> Printf.sprintf "line %d: expected %S, got %S" n x y
          | _ -> Printf.sprintf "length differs at line %d" n in
        failwith ("region snapshot " ^ name ^ " differs: " ^ first 1 (a, b))
      end
