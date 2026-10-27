#include "native_layer_token.h"
#include <caml/alloc.h>
#include <caml/memory.h>

static int handle, retained, released;
static void retain_handle(void *pointer) { if (pointer == &handle) retained++; }
static void release_handle(void *pointer) { if (pointer == &handle) released++; }

CAMLprim value caml_token_test_create(value owner, value generation)
{
  return rays_native_layer_token_create(&handle, Int64_val(owner),
      Int64_val(generation), retain_handle, release_handle);
}

CAMLprim value caml_token_test_invalidate(value token)
{
  rays_native_layer_token_invalidate(token);
  return Val_unit;
}

CAMLprim value caml_token_test_unpaired(value unit)
{
  (void)unit;
  return rays_native_layer_token_create(&handle, 1, 1, retain_handle, NULL);
}

CAMLprim value caml_token_test_borrow(value token, value owner, value generation)
{
  return Val_bool(rays_native_layer_token_borrow(token, Int64_val(owner),
      Int64_val(generation)) == &handle);
}

CAMLprim value caml_token_test_counts(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(result);
  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(retained));
  Store_field(result, 1, Val_int(released));
  CAMLreturn(result);
}
