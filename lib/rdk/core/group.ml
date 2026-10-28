
type owner = Point | Vertex | Primitive
type t = {
  owner : owner;
  name : string;
  length : int;
  bits : bytes;
  order : int array option;
  data_id : int;
}
type group = t

let byte_count length = (length + 7) / 8
let require_name name =
  if String.trim name = "" then invalid_arg "Group: empty name"
let name value = value.name
let owner value = value.owner
let length value = value.length
let data_id value = value.data_id
let with_name name value =
  require_name name;
  if String.equal name value.name then value
  else { value with name; data_id = Data_id.fresh () }
let payload_bytes value =
  Bytes.length value.bits
  + match value.order with
    | None -> 0
    | Some order -> Array.length order * (Sys.word_size / 8)
let mem index value =
  if index < 0 || index >= value.length then false
  else Char.code (Bytes.get value.bits (index lsr 3)) land (1 lsl (index land 7)) <> 0

let init ?(grain = 4096) ~owner ~name length predicate =
  require_name name;
  if length < 0 then invalid_arg "Group.init: negative length";
  if grain <= 0 then invalid_arg "Group.init: grain must be positive";
  let bytes_count = byte_count length and bits = Bytes.make (byte_count length) '\000' in
  if bytes_count > 0 then
    Parallel.for_ ~chunk_size:(max 1 (grain / 8)) ~start:0
      ~finish:(bytes_count - 1) (fun byte ->
        let base = byte * 8 and value = ref 0 in
        for bit = 0 to min 7 (length - base - 1) do
          if predicate (base + bit) then value := !value lor (1 lsl bit)
        done;
        Bytes.set bits byte (Char.chr !value));
  { owner; name; length; bits; order = None; data_id = Data_id.fresh () }

let ordered ~owner ~name ~length elements =
  if String.trim name = "" then Error "Group: empty name"
  else if length < 0 then Error "Group.ordered: negative length"
  else begin
    let bits = Bytes.make (byte_count length) '\000' in
    let rec fill index =
      if index = Array.length elements then
        Ok {
          owner;
          name;
          length;
          bits;
          order = Some (Array.copy elements);
          data_id = Data_id.fresh ();
        }
      else
        let element = Array.unsafe_get elements index in
        if element < 0 || element >= length then
          Error "Group.ordered: element index out of bounds"
        else
          let byte = element lsr 3 and mask = 1 lsl (element land 7) in
          let current = Char.code (Bytes.unsafe_get bits byte) in
          if current land mask <> 0 then
            Error "Group.ordered: duplicate element index"
          else begin
            Bytes.unsafe_set bits byte (Char.chr (current lor mask));
            fill (index + 1)
          end
    in
    fill 0
  end

let is_ordered value = Option.is_some value.order
let ordered_elements value = Option.map Array.copy value.order

let cardinality value =
  let popcount byte =
    let value = ref byte and count = ref 0 in
    while !value <> 0 do
      value := !value land (!value - 1);
      incr count
    done;
    !count
  in
  let count = ref 0 in
  Bytes.iter (fun byte -> count := !count + popcount (Char.code byte)) value.bits;
  !count

type combine_order = Union_order | Intersection_order | Difference_order | Xor_order

let iter_membership operation value =
  for index = 0 to value.length - 1 do
    if mem index value then operation index
  done

let iter_preferred operation value =
  match value.order with
  | Some order -> Array.iter operation order
  | None -> iter_membership operation value

let combine order_kind operation left right =
  if left.owner <> right.owner || left.length <> right.length then
    Error "Group: owners and lengths must match"
  else
    let count = Bytes.length left.bits in
    let bits = Bytes.make count '\000' in
    if count > 0 then
      Parallel.for_ ~chunk_size:4096 ~start:0 ~finish:(count - 1) (fun index ->
        Bytes.unsafe_set bits index
          (Char.chr (operation
            (Char.code (Bytes.unsafe_get left.bits index))
            (Char.code (Bytes.unsafe_get right.bits index)))));
    let base = {
      owner = left.owner;
      name = left.name;
      length = left.length;
      bits;
      order = None;
      data_id = Data_id.fresh ();
    } in
    let order =
      match order_kind with
      | Difference_order ->
          Option.map (fun left_order ->
            Array.of_list
              (Array.fold_right
                (fun element result ->
                  if mem element base then element :: result else result)
                left_order []))
            left.order
      | Intersection_order ->
          begin match left.order, right.order with
          | None, None -> None
          | preferred, _ ->
              let source = match preferred with Some _ -> left | None -> right in
              let result = Array.make (cardinality base) 0 and next = ref 0 in
              iter_preferred (fun element ->
                if mem element base then begin
                  Array.unsafe_set result !next element;
                  incr next
                end) source;
              Some result
          end
      | Union_order | Xor_order ->
          if Option.is_none left.order && Option.is_none right.order then None
          else
            let result = Array.make (cardinality base) 0 in
            let emitted = Bytes.make (byte_count base.length) '\000' and next = ref 0 in
            let append element =
              if mem element base then begin
                let byte = element lsr 3 and mask = 1 lsl (element land 7) in
                let current = Char.code (Bytes.unsafe_get emitted byte) in
                if current land mask = 0 then begin
                  Bytes.unsafe_set emitted byte (Char.chr (current lor mask));
                  Array.unsafe_set result !next element;
                  incr next
                end
              end
            in
            iter_preferred append left;
            iter_preferred append right;
            Some result
    in
    Ok { base with order }
let union = combine Union_order (lor)

let union_many ?cancel ?(grain = 32_768) ~name groups =
  require_name name;
  if grain <= 0 then invalid_arg "Group.union_many: grain must be positive";
  match groups with
  | [] -> Error "Group.union_many: at least one group is required"
  | first :: rest ->
      if List.exists (fun group ->
          group.owner <> first.owner || group.length <> first.length) rest then
        Error "Group.union_many: owners and lengths must match"
      else if rest = [] then Ok (with_name name first)
      else begin
        let inputs = Array.of_list groups |> Array.map (fun group -> group.bits) in
        let input_count = Array.length inputs in
        let count = Bytes.length first.bits in
        let bits = Bytes.make count '\000' in
        if count > 0 then
          Parallel.for_ ~chunk_size:(max 1 (grain / 8)) ~start:0
            ~finish:(count - 1) (fun byte ->
              if byte land 4095 = 0 then Cancel.check_opt cancel;
              let value = ref 0 in
              for group = 0 to input_count - 1 do
                value := !value lor
                  Char.code (Bytes.unsafe_get inputs.(group) byte)
              done;
              Bytes.unsafe_set bits byte (Char.chr !value));
        Ok {
          owner = first.owner;
          name;
          length = first.length;
          bits;
          order = None;
          data_id = Data_id.fresh ();
        }
      end

let intersection = combine Intersection_order (land)
let difference = combine Difference_order (fun left right -> left land lnot right)
let symmetric_difference = combine Xor_order (lxor)

let iter = iter_membership
let iter_ordered operation value =
  match value.order with
  | Some order -> Array.iter operation order
  | None -> iter_membership operation value

module Builder = struct
  type nonrec t = {
    owner : owner; name : string; length : int; bits : bytes;
    mutable frozen : bool;
  }
  let create ~owner ~name length =
    require_name name;
    if length < 0 then invalid_arg "Group.Builder.create: negative length";
    { owner; name; length; bits = Bytes.make (byte_count length) '\000';
      frozen = false }
  let set value index member =
    if value.frozen then invalid_arg "Group.Builder.set: builder is frozen";
    if index < 0 || index >= value.length then invalid_arg "Group.Builder.set: invalid index";
    let byte = index lsr 3 and mask = 1 lsl (index land 7) in
    let current = Char.code (Bytes.get value.bits byte) in
    Bytes.set value.bits byte
      (Char.chr (if member then current lor mask else current land lnot mask))
  let freeze value =
    if value.frozen then invalid_arg "Group.Builder.freeze: already frozen";
    value.frozen <- true;
    { owner = value.owner; name = value.name; length = value.length;
      bits = value.bits; order = None; data_id = Data_id.fresh () }
end

module View = struct
  let bits_view value = value.bits

  let with_owner owner value =
    if owner = value.owner then value else { value with owner }

  let of_owned_bits ~owner ~name ~length bits =
    require_name name;
    if length < 0 || Bytes.length bits <> byte_count length then
      invalid_arg "Group.View.of_owned_bits: invalid packed length";
    if length land 7 <> 0 && Bytes.length bits > 0 then begin
      let valid_mask = (1 lsl (length land 7)) - 1 in
      let last = Bytes.length bits - 1 in
      if Char.code (Bytes.get bits last) land lnot valid_mask <> 0 then
        invalid_arg "Group.View.of_owned_bits: set padding bits"
    end;
    { owner; name; length; bits; order = None; data_id = Data_id.fresh () }

  let order_view value = value.order

  let with_owned_order order value =
    let count = Array.length order in
    if count <> cardinality value then
      invalid_arg "Group.View.with_owned_order: order does not match membership";
    let seen = Bytes.make (byte_count value.length) '\000' in
    Array.iter (fun element ->
      if element < 0 || element >= value.length || not (mem element value) then
        invalid_arg "Group.View.with_owned_order: invalid element";
      let byte = element lsr 3 and mask = 1 lsl (element land 7) in
      let current = Char.code (Bytes.unsafe_get seen byte) in
      if current land mask <> 0 then
        invalid_arg "Group.View.with_owned_order: duplicate element";
      Bytes.unsafe_set seen byte (Char.chr (current lor mask))) order;
    { value with order = Some order; data_id = Data_id.fresh () }

  let remap_order ~source ~source_of_target target =
    match source.order with
    | None -> target
    | Some source_order ->
        if source.owner <> target.owner then
          invalid_arg "Group.View.remap_order: owners do not match";
        if Array.length source_of_target <> target.length then
          invalid_arg "Group.View.remap_order: mapping length does not match target";
        let rank = Array.make source.length (-1) in
        Array.iteri (fun index element -> rank.(element) <- index) source_order;
        let counts = Array.make (Array.length source_order) 0 in
        for target_element = 0 to target.length - 1 do
          let source_element = source_of_target.(target_element) in
          if source_element < -1 || source_element >= source.length then
            invalid_arg "Group.View.remap_order: source index out of bounds";
          if mem target_element target && source_element >= 0 then begin
            let source_rank = rank.(source_element) in
            if source_rank >= 0 then counts.(source_rank) <- counts.(source_rank) + 1
          end
        done;
        let offsets = Array.make (Array.length counts + 1) 0 in
        for index = 0 to Array.length counts - 1 do
          offsets.(index + 1) <- offsets.(index) + counts.(index)
        done;
        let mapped_count = offsets.(Array.length counts) in
        let order = Array.make (cardinality target) 0
        and next = Array.copy offsets
        and emitted = Bytes.make (byte_count target.length) '\000' in
        for target_element = 0 to target.length - 1 do
          let source_element = source_of_target.(target_element) in
          if mem target_element target && source_element >= 0 then begin
            let source_rank = rank.(source_element) in
            if source_rank >= 0 then begin
              let output = next.(source_rank) in
              order.(output) <- target_element;
              next.(source_rank) <- output + 1;
              let byte = target_element lsr 3
              and mask = 1 lsl (target_element land 7) in
              Bytes.unsafe_set emitted byte
                (Char.chr (Char.code (Bytes.unsafe_get emitted byte) lor mask))
            end
          end
        done;
        let output = ref mapped_count in
        for target_element = 0 to target.length - 1 do
          let byte = target_element lsr 3 and mask = 1 lsl (target_element land 7) in
          if mem target_element target
              && Char.code (Bytes.unsafe_get emitted byte) land mask = 0 then begin
            order.(!output) <- target_element;
            incr output
          end
        done;
        with_owned_order order target
end
