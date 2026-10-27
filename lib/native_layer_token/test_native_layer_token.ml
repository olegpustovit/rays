external create : int64 -> int64 -> Native_layer_token.t = "caml_token_test_create"
external invalidate : Native_layer_token.t -> unit = "caml_token_test_invalidate"
external borrow : Native_layer_token.t -> int64 -> int64 -> bool = "caml_token_test_borrow"
external counts : unit -> int * int = "caml_token_test_counts"
external unpaired : unit -> Native_layer_token.t = "caml_token_test_unpaired"

let () =
  (match unpaired () with
   | exception Invalid_argument _ -> ()
   | _ -> assert false);
  let token = create 17L 23L in
  assert (counts () = (1, 0));
  assert (Native_layer_token.alive token);
  assert (Native_layer_token.owner_id token = 17L);
  assert (Native_layer_token.generation token = 23L);
  assert (borrow token 17L 23L);
  assert (not (borrow token 18L 23L));
  assert (not (borrow token 17L 24L));
  invalidate token;
  invalidate token;
  assert (not (Native_layer_token.alive token));
  assert (not (borrow token 17L 23L));
  assert (counts () = (1, 1));
  print_endline "native layer token: ownership, identity, invalidation passed"
