#include "native_layer_token.h"
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>

typedef struct { void *layer; uint64_t owner; uint64_t generation; int alive; void (*release)(void *); } token;
static token *get(value v){return(token*)Data_custom_val(v);}
void rays_native_layer_token_invalidate(value v){token*t=get(v);if(t->layer!=NULL&&t->release!=NULL)t->release(t->layer);t->layer=NULL;t->alive=0;}
static void finalize(value v){rays_native_layer_token_invalidate(v);}
static struct custom_operations ops={"rays.native-layer-token",finalize,custom_compare_default,custom_hash_default,custom_serialize_default,custom_deserialize_default,custom_compare_ext_default,custom_fixed_length_default};
value rays_native_layer_token_create(void *layer, uint64_t owner, uint64_t generation,
    void (*retain)(void *), void (*release)(void *))
{
  CAMLparam0();
  CAMLlocal1(v);
  if ((retain == NULL) != (release == NULL))
    caml_invalid_argument("native_layer_token: ownership operations must be paired");
  v = caml_alloc_custom(&ops, sizeof(token), 0, 1);
  token *t = get(v);
  t->layer = layer;
  t->owner = owner;
  t->generation = generation;
  t->alive = layer != NULL;
  t->release = release;
  if (layer != NULL && retain != NULL) retain(layer);
  CAMLreturn(v);
}
void *rays_native_layer_token_borrow(value v,uint64_t owner,uint64_t generation){token*t=get(v);return t->alive&&t->owner==owner&&t->generation==generation?t->layer:NULL;}
CAMLprim value caml_rays_native_layer_token_owner(value v){return caml_copy_int64(get(v)->owner);}
CAMLprim value caml_rays_native_layer_token_generation(value v){return caml_copy_int64(get(v)->generation);}
CAMLprim value caml_rays_native_layer_token_alive(value v){return Val_bool(get(v)->alive);}
