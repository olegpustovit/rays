/* Process physical footprint and retired instructions via proc_pid_rusage (macOS). */
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#if defined(__APPLE__)
#include <libproc.h>
#include <sys/resource.h>
#include <unistd.h>

static int read_rusage(struct rusage_info_v4 *info) {
  return proc_pid_rusage(getpid(), RUSAGE_INFO_V4, (rusage_info_t *)info) == 0;
}
#endif

CAMLprim value caml_rays_mem_phys_footprint(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
#if defined(__APPLE__)
  struct rusage_info_v4 info;
  if (read_rusage(&info) && info.ri_phys_footprint <= (uint64_t)Max_long) {
    result = caml_alloc_some(Val_long((intnat)info.ri_phys_footprint));
  } else {
    result = Val_none;
  }
#else
  result = Val_none;
#endif
  CAMLreturn(result);
}

CAMLprim value caml_rays_mem_instructions(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
#if defined(__APPLE__)
  struct rusage_info_v4 info;
  if (read_rusage(&info)) {
    result = caml_alloc_some(caml_copy_int64((int64_t)info.ri_instructions));
  } else {
    result = Val_none;
  }
#else
  result = Val_none;
#endif
  CAMLreturn(result);
}
