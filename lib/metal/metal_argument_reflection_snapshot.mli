(** Immutable, ownership-free snapshots of Metal shader argument reflection. *)

type limits = { max_depth : int; max_members : int }

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

module Make (Raw : Raw) : sig
  val reflected_type : ?limits:limits -> Raw.handle -> (reflected_type, string) result
  val argument : ?limits:limits -> Raw.handle -> (argument, string) result
end
