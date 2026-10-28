module Mock = struct
  type node =
    | Scalar
    | Texture_reference
    | Array of node
    | Struct of (string * node) list
    | Member of string * node

  type handle = { node : node; owned : bool; mutable destroyed : bool }

  type kind =
    | Scalar_kind
    | Array_kind
    | Pointer_kind
    | Struct_kind
    | Texture_reference_kind
    | Tensor_reference_kind

  let live = ref 0
  let fail_member_name = ref false
  let root node = { node; owned = false; destroyed = false }

  let own node =
    incr live;
    { node; owned = true; destroyed = false }

  let destroy handle =
    if handle.owned && not handle.destroyed then begin
      handle.destroyed <- true;
      decr live
    end

  let ok (value : 'a) (_ : handle) : ('a, string) result = Ok value

  let kind handle =
    Ok
      (match handle.node with
      | Scalar | Member _ -> Scalar_kind
      | Texture_reference -> Texture_reference_kind
      | Array _ -> Array_kind
      | Struct _ -> Struct_kind)

  let data_type = ok 1L
  let array_length = ok 1L
  let array_stride = ok 4L
  let argument_index_stride = ok 1L
  let element handle = match handle.node with Array child -> Ok (Some (own child)) | _ -> Ok None
  let pointer_access = ok 0L
  let alignment = ok 4L
  let data_size = ok 4L
  let element_is_argument_buffer = ok false

  let members handle =
    match handle.node with
    | Struct members -> Ok (List.map (fun (name, node) -> own (Member (name, node))) members)
    | _ -> Ok []

  let member_name handle =
    if !fail_member_name then Error "injected member-name failure"
    else match handle.node with Member (name, _) -> Ok name | _ -> Error "not member"

  let member_offset = ok 0L
  let member_argument_index = ok 0L

  let member_type handle =
    match handle.node with Member (_, child) -> Ok (Some (own child)) | _ -> Ok None

  let texture_access = ok 0L
  let texture_type = ok 2L
  let is_depth_texture = ok false
  let tensor_access = ok 0L
  let tensor_data_type = ok 3L
  let tensor_index_type = ok 4L
  let tensor_dimensions = ok (Some [ 2L; 3L ])
  let argument_name = ok "items"
  let argument_index = ok 0L
  let argument_active = ok true
  let argument_access = ok 0L
  let argument_type = ok 0L
  let argument_array_length = ok 1L
  let argument_buffer_alignment = ok 16L
  let argument_buffer_data_size = ok 16L
  let argument_buffer_data_type = ok 1L
  let argument_texture_data_type = ok 1L
  let argument_texture_type = ok 2L
  let argument_is_depth_texture = ok false
  let argument_threadgroup_memory_alignment = ok 16L
  let argument_threadgroup_memory_data_size = ok 0L

  let argument_buffer_pointer_type (_ : handle) : (handle option, string) result =
    Ok (Some (own (Array Scalar)))

  let argument_buffer_struct_type (_ : handle) : (handle option, string) result =
    Ok (Some (own (Struct [ ("x", Scalar) ])))
end

module Snapshot = Metal.Reflection.Make (Mock)

let check condition message = if not condition then failwith message
let no_delta before label = check (!Mock.live = before) (label ^ ": native handle delta")

let run () =
  let root = Mock.root (Struct [ ("position", Array Scalar); ("normal", Scalar) ]) in
  let before = !Mock.live in
  (match Snapshot.reflected_type root with
  | Ok (Struct members) -> check (List.length members = 2) "member snapshot"
  | Ok _ -> failwith "unexpected reflection kind"
  | Error message -> failwith message);
  no_delta before "success";
  (match Snapshot.reflected_type ~limits:{ max_depth = 0; max_members = 8 } root with
  | Error _ -> ()
  | Ok _ -> failwith "depth limit accepted");
  no_delta before "depth rejection";
  (match Snapshot.reflected_type ~limits:{ max_depth = 8; max_members = 1 } root with
  | Error _ -> ()
  | Ok _ -> failwith "member limit accepted");
  no_delta before "member rejection";
  Mock.fail_member_name := true;
  (match Snapshot.reflected_type root with
  | Error _ -> ()
  | Ok _ -> failwith "injected failure accepted");
  Mock.fail_member_name := false;
  no_delta before "failure unwind";
  (match Snapshot.argument (Mock.root Scalar) with
  | Ok argument -> check (argument.name = "items") "argument snapshot"
  | Error message -> failwith message);
  no_delta before "argument success";
  (match Snapshot.reflected_type (Mock.root Texture_reference) with
  | Ok (Texture_reference { data_type = 1L; access = 0L; texture_type = 2L; depth = false }) -> ()
  | Ok _ -> failwith "texture-reference snapshot mismatch"
  | Error message -> failwith message);
  no_delta before "texture-reference success";
  print_endline "Metal argument reflection snapshot: bounded conversion and zero handle delta"
