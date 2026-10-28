(* Text helpers the test_main modules share: substring search and fixture files. Kept
   apart from Test_support so a test with its own [fail] can open this one. *)

let has text sub =
  let n = String.length sub in
  let rec at i = i + n <= String.length text && (String.sub text i n = sub || at (i + 1)) in
  at 0

let read path = In_channel.with_open_bin path In_channel.input_all

let case name = In_channel.with_open_bin
  (Filename.concat "../../../specification/workspace/cases" (name ^ ".lisp")) In_channel.input_all
