(* Invalid native UTF-8 bytes use isolated low surrogates. Valid Unicode stays
   Unicode; runtime textBytes reverses the exceptional byte representation. *)
let quote s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  let i = ref 0 in
  while !i < String.length s do
    let c = s.[!i] in
    if Char.code c >= 128 then begin
      let decoded = String.get_utf_8_uchar s !i in
      if Uchar.utf_decode_is_valid decoded then begin
        let n = Uchar.utf_decode_length decoded in
        Buffer.add_substring b s !i n;
        i := !i + n
      end
      else begin
        Printf.bprintf b "\\udc%02x" (Char.code c);
        incr i
      end
    end
    else begin
      (match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 32 || Char.code c = 127 -> Printf.bprintf b "\\u%04x" (Char.code c)
      | c -> Buffer.add_char b c);
      incr i
    end
  done;
  Buffer.add_char b '"';
  Buffer.contents b
