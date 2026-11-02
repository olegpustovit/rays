/* What a process asks its OS about memory. One small function each per OS: footprint, instruction
   count, installed RAM, the pressure source and its relief. A new port fills these in; nothing above
   this file names an OS. */
#include <stdatomic.h>
#include <stdint.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

/* The level the pressure callback stores and the next frame boundary reads, with a count so a second
   event at the same level is seen: bits 0-1 level (0 normal, 1 warning, 2 critical), the rest count.
   No OCaml ever runs on the OS's thread. */
static _Atomic intnat pressure = 0;

static void pressure_store(int level) {
  intnat seen = atomic_load(&pressure);
  while (!atomic_compare_exchange_weak(&pressure, &seen, (((seen >> 2) + 1) << 2) | level)) {
  }
}

CAMLprim value caml_rays_mem_pressure_state(value unit) {
  (void)unit;
  return Val_long(atomic_load(&pressure));
}

CAMLprim value caml_rays_mem_pressure_inject(value level) {
  pressure_store((int)Long_val(level));
  return Val_unit;
}

#if defined(__APPLE__)
#include <dispatch/dispatch.h>
#include <libproc.h>
#include <malloc/malloc.h>
#include <sys/resource.h>
#include <sys/sysctl.h>
#include <unistd.h>

static int read_rusage(struct rusage_info_v4 *info) {
  return proc_pid_rusage(getpid(), RUSAGE_INFO_V4, (rusage_info_t *)info) == 0;
}

static dispatch_source_t pressure_source = NULL;

static void on_pressure(void *context) {
  (void)context;
  unsigned long flags = dispatch_source_get_data(pressure_source);
  pressure_store((flags & DISPATCH_MEMORYPRESSURE_CRITICAL)
                     ? 2
                     : ((flags & DISPATCH_MEMORYPRESSURE_WARN) ? 1 : 0));
}

CAMLprim value caml_rays_mem_pressure_start(value unit) {
  (void)unit;
  if (pressure_source == NULL) {
    pressure_source = dispatch_source_create(
        DISPATCH_SOURCE_TYPE_MEMORYPRESSURE, 0,
        DISPATCH_MEMORYPRESSURE_NORMAL | DISPATCH_MEMORYPRESSURE_WARN |
            DISPATCH_MEMORYPRESSURE_CRITICAL,
        dispatch_get_global_queue(QOS_CLASS_UTILITY, 0));
    if (pressure_source == NULL) return Val_false;
    dispatch_source_set_event_handler_f(pressure_source, on_pressure);
    dispatch_activate(pressure_source);
  }
  return Val_true;
}

CAMLprim value caml_rays_mem_pressure_relief(value unit) {
  (void)unit;
  malloc_zone_pressure_relief(NULL, 0);
  return Val_unit;
}

CAMLprim value caml_rays_mem_physical_ram(value unit) {
  (void)unit;
  uint64_t bytes = 0;
  size_t size = sizeof bytes;
  if (sysctlbyname("hw.memsize", &bytes, &size, NULL, 0) != 0 || bytes > (uint64_t)Max_long)
    return Val_long(0);
  return Val_long((intnat)bytes);
}
#else
CAMLprim value caml_rays_mem_pressure_start(value unit) {
  (void)unit;
  return Val_false;
}

CAMLprim value caml_rays_mem_pressure_relief(value unit) {
  (void)unit;
  return Val_unit;
}

CAMLprim value caml_rays_mem_physical_ram(value unit) {
  (void)unit;
  return Val_long(0);
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

CAMLprim value caml_rays_mem_peak_footprint(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
#if defined(__APPLE__)
  struct rusage_info_v4 info;
  if (read_rusage(&info) && info.ri_lifetime_max_phys_footprint <= (uint64_t)Max_long) {
    result = caml_alloc_some(Val_long((intnat)info.ri_lifetime_max_phys_footprint));
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
