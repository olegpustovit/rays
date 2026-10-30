(* The compute stages Run and Host dispatch, printed at build time from shaders/NAME.rays (dune). *)
module S = Ogpu.Shader

(* Stream compaction for a resident fold: the indices of a mask's nonzero elements, in order, as
   floats, and their count; three dispatches over blocks of 1,024 (count, scan the counts, scatter).
   The kernel is shaders/compact.rays, a compute stage printed by Raster.compute_msl. *)
let compaction =
  {
    Emit.source = Compact_source.source;
    entry = "compact_mask";
    uniform_layout = [];
    uniform_bytes = 12;
    table_seeds = [||];
    output_widths = [| 1 |];
    input_widths = [| 1 |];
    gathers = [||];
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

(* A resident dispatch's threadgroup counts from a count cell, one thread (shaders/dispatch.rays,
   printed as [compaction] is). Buffers: the cell, {capacity, mul, div, group}, the three counts. *)
let dispatch_arguments =
  {
    compaction with
    source = Dispatch_source.source;
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
