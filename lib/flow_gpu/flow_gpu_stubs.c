/* A block that tells the GC how much device memory its holder pins, as a Bigarray does for its
   data: a dead holder is then collected as promptly as a dead array of that size would be, and its
   buffer returns to the pool instead of being read back. No finaliser: the holder's own returns
   the buffer. */
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/mlvalues.h>

static struct custom_operations pressure_ops = {
    "rays.flow_gpu.device_pressure", custom_finalize_default,  custom_compare_default,
    custom_hash_default,             custom_serialize_default, custom_deserialize_default,
    custom_compare_ext_default,      custom_fixed_length_default};

value rays_flow_gpu_device_pressure(value bytes) {
  return caml_alloc_custom(&pressure_ops, sizeof(value), (mlsize_t)Long_val(bytes), 64 << 20);
}
