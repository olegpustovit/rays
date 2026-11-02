(* The compute stages Run and Host dispatch, printed at build time from shaders/NAME.rays (dune). *)
module S = Ogpu.Shader

let spirv bytes interface =
  lazy
    (Ok
       ( Bytes.of_string bytes,
         "main",
         List.map (fun binding -> S.{ group = 0; binding; kind = Storage_buffer; visibility = [ Compute ] }) interface ))

(* Stream compaction for a resident fold: the indices of a mask's nonzero elements, in order, as
   floats, and their count; three dispatches over blocks of 1,024 (count, scan the counts, scatter).
   The kernel is shaders/compact.rays, a compute stage printed by Raster.compute_msl. *)
let compaction =
  {
    Emit.source = Compact_source.source;
    spirv = spirv Spirv_kernels.compact_comp (List.init 6 Fun.id);
    entry = "compact_mask";
    uniform_layout = [];
    uniform_bytes = 12;
    uint32_uniforms = [];
    table_seeds = [||];
    output_widths = [| 1 |];
    output_uint32 = [| false |];
    input_widths = [| 1 |];
    input_uint32 = [| false |];
    gathers = [||];
    gather_uint32 = [||];
    counts = None;
    scene = None;
    interface =
      List.map
        (fun binding ->
          S.
            {
              group = 0;
              binding;
              kind = (if binding = 3 then Uniform_buffer else Storage_buffer);
              visibility = [ Compute ];
            })
        [ 0; 1; 2; 3; 4; 5 ];
  }

let compaction_uint32 =
  {
    compaction with
    source = Compact_uint32_source.source;
    spirv = spirv Spirv_kernels.compact_uint32_comp (List.init 6 Fun.id);
    entry = "compact_mask_uint32";
    output_uint32 = [| true |];
  }

let indices =
  {
    compaction_uint32 with
    source = Indices_source.source;
    spirv = spirv Spirv_kernels.indices_comp (List.init 2 Fun.id);
    entry = "write_indices";
    uniform_bytes = 12;
    interface =
      List.map
        (fun binding ->
          S.
            {
              group = 0;
              binding;
              kind = (if binding = 1 then Uniform_buffer else Storage_buffer);
              visibility = [ Compute ];
            })
        [ 0; 1 ];
  }

let triangles =
  {
    compaction_uint32 with
    source = Triangles_source.source;
    spirv = spirv Spirv_kernels.triangles_comp (List.init 8 Fun.id);
    entry = "triangle_indices";
    uniform_bytes = 16;
    interface = List.init 8 (fun binding ->
      S.{ group = 0; binding; kind = (if binding = 7 then Uniform_buffer else Storage_buffer);
          visibility = [ Compute ] });
  }

let topology =
  {
    compaction_uint32 with
    source = Topology_source.source;
    spirv = spirv Spirv_kernels.topology_comp (List.init 5 Fun.id);
    entry = "topology_check";
    uniform_bytes = 20;
    interface =
      List.init 5 (fun binding ->
          S.{ group = 0; binding;
              kind = (if binding = 4 then Uniform_buffer else Storage_buffer);
              visibility = [ Compute ] });
  }

(* A resident dispatch's threadgroup counts from a count cell, one thread (shaders/dispatch.rays,
   printed as [compaction] is). Buffers: the cell, {capacity, mul, div, group}, the three counts. *)
let dispatch_arguments =
  {
    compaction with
    source = Dispatch_source.source;
    spirv = spirv Spirv_kernels.dispatch_comp (List.init 3 Fun.id);
    entry = "dispatch_arguments";
    uniform_bytes = 16;
    interface =
      List.map
        (fun binding ->
          S.
            {
              group = 0;
              binding;
              kind = (if binding = 1 then Uniform_buffer else Storage_buffer);
              visibility = [ Compute ];
            })
        [ 0; 1; 2 ];
  }

(* A resident packed float3 array's bounds: each lane of one threadgroup of 1,024 the min and max of
   every 1,024th element from it, at lane x 6, finished on the host (shaders/bounds.rays, printed as [compaction] is).
   Buffers: the array, the lanes' floats, the count. *)
let bounds =
  {
    compaction with
    source = Bounds_source.source;
    spirv = spirv Spirv_kernels.bounds_comp (List.init 3 Fun.id);
    entry = "chunk_bounds";
    uniform_bytes = 4;
    output_widths = [| 6 |];
    input_widths = [| 3 |];
    interface =
      List.map
        (fun binding ->
          S.
            {
              group = 0;
              binding;
              kind = (if binding = 2 then Uniform_buffer else Storage_buffer);
              visibility = [ Compute ];
            })
        [ 0; 1; 2 ];
  }

(* A triangle mesh's display normals from its resident P (shaders/normals.rays, one thread a point
   over groups of 256). Buffers: P, the corners, the CSR offsets and corners of each point, the
   normals, the count. *)
let normals =
  {
    compaction with
    source = Normals_source.source;
    spirv = spirv Spirv_kernels.normals_comp (List.init 6 Fun.id);
    entry = "point_normal";
    uniform_bytes = 4;
    output_widths = [| 3 |];
    input_widths = [| 3 |];
    interface =
      List.map
        (fun binding ->
          S.
            {
              group = 0;
              binding;
              kind = (if binding = 5 then Uniform_buffer else Storage_buffer);
              visibility = [ Compute ];
            })
        [ 0; 1; 2; 3; 4; 5 ];
  }
