
type match_mode =
  | Cyclic
  | By_values of { source_attribute : string; target_attribute : string }
  | To_element of { target_attribute : string }

type rule = {
  copy_owner : Attribute.owner;
  copy_pattern : string;
  copy_into : string option;
}

type payload = Position | Ordinary of Attribute.t
type selected = {
  source_owner : Attribute.owner;

  target_name : string;
  payload : payload;
}
type mapping_kind = No_match | Identity | General

type selector = Select of Attribute_pattern.t | Rewrite of Attribute_pattern.rewrite

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

