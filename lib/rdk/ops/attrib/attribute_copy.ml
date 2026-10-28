

type rule = {
  copy_owner : Attribute.owner;
  copy_pattern : string;
  copy_into : string option;
}

module Int_table = Hashtbl.Make (struct
  type t = int
  let equal = Int.equal
  let hash value = value land max_int
end)

module String_table = Hashtbl.Make (struct
  type t = string
  let equal = String.equal
  let hash = Hashtbl.hash
end)

(* Match-plan construction is kept separate from topology projection so every
   copied attribute owner can reuse the same base correspondence. *)
