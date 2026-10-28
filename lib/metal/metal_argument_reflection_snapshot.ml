type limits = { max_depth : int; max_members : int }

let default_limits = { max_depth = 32; max_members = 65_536 }

type reflected_type =
  | Scalar of int64
  | Array of {
      data_type : int64;
      length : int64;
      stride : int64;
      argument_index_stride : int64;
      element : reflected_type option;
    }
  | Pointer of {
      data_type : int64;
      access : int64;
      alignment : int64;
      data_size : int64;
      element_is_argument_buffer : bool;
      element : reflected_type option;
    }
  | Struct of member list
  | Texture_reference of { data_type : int64; access : int64; texture_type : int64; depth : bool }
  | Tensor_reference of {
      data_type : int64;
      access : int64;
      tensor_data_type : int64;
      index_type : int64;
      dimensions : int64 list option;
    }

and member = {
  name : string;
  offset : int64;
  argument_index : int64;
  data_type : int64;
  reflected_type : reflected_type option;
}

type argument = {
  name : string;
  index : int64;
  active : bool;
  access : int64;
  argument_type : int64;
  array_length : int64;
  buffer_alignment : int64;
  buffer_data_size : int64;
  buffer_data_type : int64;
  texture_data_type : int64;
  texture_type : int64;
  depth_texture : bool;
  threadgroup_memory_alignment : int64;
  threadgroup_memory_data_size : int64;
  buffer_pointer_type : reflected_type option;
  buffer_struct_type : reflected_type option;
}

module type Raw = sig
  type handle

  type kind =
    | Scalar_kind
    | Array_kind
    | Pointer_kind
    | Struct_kind
    | Texture_reference_kind
    | Tensor_reference_kind

  val destroy : handle -> unit
  val kind : handle -> (kind, string) result
  val data_type : handle -> (int64, string) result
  val array_length : handle -> (int64, string) result
  val array_stride : handle -> (int64, string) result
  val argument_index_stride : handle -> (int64, string) result
  val element : handle -> (handle option, string) result
  val pointer_access : handle -> (int64, string) result
  val alignment : handle -> (int64, string) result
  val data_size : handle -> (int64, string) result
  val element_is_argument_buffer : handle -> (bool, string) result
  val members : handle -> (handle list, string) result
  val member_name : handle -> (string, string) result
  val member_offset : handle -> (int64, string) result
  val member_argument_index : handle -> (int64, string) result
  val member_type : handle -> (handle option, string) result
  val texture_access : handle -> (int64, string) result
  val texture_type : handle -> (int64, string) result
  val is_depth_texture : handle -> (bool, string) result
  val tensor_access : handle -> (int64, string) result
  val tensor_data_type : handle -> (int64, string) result
  val tensor_index_type : handle -> (int64, string) result
  val tensor_dimensions : handle -> (int64 list option, string) result
  val argument_name : handle -> (string, string) result
  val argument_index : handle -> (int64, string) result
  val argument_active : handle -> (bool, string) result
  val argument_access : handle -> (int64, string) result
  val argument_type : handle -> (int64, string) result
  val argument_array_length : handle -> (int64, string) result
  val argument_buffer_alignment : handle -> (int64, string) result
  val argument_buffer_data_size : handle -> (int64, string) result
  val argument_buffer_data_type : handle -> (int64, string) result
  val argument_texture_data_type : handle -> (int64, string) result
  val argument_texture_type : handle -> (int64, string) result
  val argument_is_depth_texture : handle -> (bool, string) result
  val argument_threadgroup_memory_alignment : handle -> (int64, string) result
  val argument_threadgroup_memory_data_size : handle -> (int64, string) result
  val argument_buffer_pointer_type : handle -> (handle option, string) result
  val argument_buffer_struct_type : handle -> (handle option, string) result
end

module Make (Raw : Raw) = struct
  let ( let* ) value continuation =
    match value with Ok value -> continuation value | Error _ as error -> error

  let validate_limits limits =
    if limits.max_depth < 0 then Error "Metal reflection max_depth must be nonnegative"
    else if limits.max_members < 0 then Error "Metal reflection max_members must be nonnegative"
    else Ok ()

  let with_owned option convert =
    match option with
    | None -> Ok None
    | Some handle ->
        Fun.protect
          ~finally:(fun () -> Raw.destroy handle)
          (fun () -> Result.map Option.some (convert handle))

  let destroy_all handles = List.iter Raw.destroy handles

  let reflected_type ?(limits = default_limits) root =
    let* () = validate_limits limits in
    let remaining = ref limits.max_members in
    let rec type_ depth handle =
      if depth > limits.max_depth then Error "Metal reflection exceeded max_depth"
      else
        let* data_type = Raw.data_type handle in
        let* kind = Raw.kind handle in
        match kind with
        | Raw.Scalar_kind -> Ok (Scalar data_type)
        | Raw.Array_kind ->
            let* length = Raw.array_length handle in
            let* stride = Raw.array_stride handle in
            let* argument_index_stride = Raw.argument_index_stride handle in
            let* child = Raw.element handle in
            let* element = with_owned child (type_ (depth + 1)) in
            Ok (Array { data_type; length; stride; argument_index_stride; element })
        | Raw.Pointer_kind ->
            let* access = Raw.pointer_access handle in
            let* alignment = Raw.alignment handle in
            let* data_size = Raw.data_size handle in
            let* element_is_argument_buffer = Raw.element_is_argument_buffer handle in
            let* child = Raw.element handle in
            let* element = with_owned child (type_ (depth + 1)) in
            Ok
              (Pointer
                 { data_type; access; alignment; data_size; element_is_argument_buffer; element })
        | Raw.Struct_kind ->
            let* handles = Raw.members handle in
            if List.length handles > !remaining then begin
              destroy_all handles;
              Error "Metal reflection exceeded max_members"
            end
            else begin
              remaining := !remaining - List.length handles;
              let rec collect output = function
                | [] -> Ok (Struct (List.rev output))
                | member_handle :: rest -> (
                    let converted =
                      Fun.protect
                        ~finally:(fun () -> Raw.destroy member_handle)
                        (fun () -> member_ (depth + 1) member_handle)
                    in
                    match converted with
                    | Ok value -> collect (value :: output) rest
                    | Error _ as error ->
                        destroy_all rest;
                        error)
              and member_ depth member_handle =
                let* name = Raw.member_name member_handle in
                let* offset = Raw.member_offset member_handle in
                let* argument_index = Raw.member_argument_index member_handle in
                let* data_type = Raw.data_type member_handle in
                let* child = Raw.member_type member_handle in
                let* reflected_type = with_owned child (type_ depth) in
                Ok { name; offset; argument_index; data_type; reflected_type }
              in
              collect [] handles
            end
        | Raw.Texture_reference_kind ->
            let* access = Raw.texture_access handle in
            let* texture_type = Raw.texture_type handle in
            let* depth = Raw.is_depth_texture handle in
            Ok (Texture_reference { data_type; access; texture_type; depth })
        | Raw.Tensor_reference_kind ->
            let* access = Raw.tensor_access handle in
            let* tensor_data_type = Raw.tensor_data_type handle in
            let* index_type = Raw.tensor_index_type handle in
            let* dimensions = Raw.tensor_dimensions handle in
            Ok (Tensor_reference { data_type; access; tensor_data_type; index_type; dimensions })
    in
    type_ 0 root

  let argument ?(limits = default_limits) handle =
    let snapshot_owned value = with_owned value (reflected_type ~limits) in
    let* () = validate_limits limits in
    let* name = Raw.argument_name handle in
    let* index = Raw.argument_index handle in
    let* active = Raw.argument_active handle in
    let* access = Raw.argument_access handle in
    let* argument_type = Raw.argument_type handle in
    let* array_length = Raw.argument_array_length handle in
    let* buffer_alignment = Raw.argument_buffer_alignment handle in
    let* buffer_data_size = Raw.argument_buffer_data_size handle in
    let* buffer_data_type = Raw.argument_buffer_data_type handle in
    let* texture_data_type = Raw.argument_texture_data_type handle in
    let* texture_type = Raw.argument_texture_type handle in
    let* depth_texture = Raw.argument_is_depth_texture handle in
    let* threadgroup_memory_alignment = Raw.argument_threadgroup_memory_alignment handle in
    let* threadgroup_memory_data_size = Raw.argument_threadgroup_memory_data_size handle in
    let* pointer = Raw.argument_buffer_pointer_type handle in
    let* buffer_pointer_type = snapshot_owned pointer in
    let* struct_ = Raw.argument_buffer_struct_type handle in
    let* buffer_struct_type = snapshot_owned struct_ in
    Ok
      {
        name;
        index;
        active;
        access;
        argument_type;
        array_length;
        buffer_alignment;
        buffer_data_size;
        buffer_data_type;
        texture_data_type;
        texture_type;
        depth_texture;
        threadgroup_memory_alignment;
        threadgroup_memory_data_size;
        buffer_pointer_type;
        buffer_struct_type;
      }
end
