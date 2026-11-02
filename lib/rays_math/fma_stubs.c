/* OCaml's own fma is, on Windows, a software emulation (no working fma at configure time) that
   rounds subnormal results wrongly; the C library's (ucrtbase on Windows, hardware elsewhere) is exact. */
#include <math.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

double rays_fma(double a, double b, double c) { return fma(a, b, c); }

value rays_fma_byte(value a, value b, value c)
{
  return caml_copy_double(fma(Double_val(a), Double_val(b), Double_val(c)));
}
