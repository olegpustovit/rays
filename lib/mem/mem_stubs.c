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

#if defined(_WIN32)
static void poll_low_memory(void);
#endif

CAMLprim value caml_rays_mem_pressure_state(value unit) {
  (void)unit;
#if defined(_WIN32)
  poll_low_memory();
#endif
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
#elif defined(_WIN32)
#include <malloc.h>
#include <windows.h>
#include <psapi.h>

/* Windows has one low-memory signal: level 1 (warning) while the system reports low memory, 0 once it
   stops. Critical (2) is never stored. The handle is queried by the state read, which the frame
   boundary makes, so no OS thread runs OCaml and a change is stored once. */
static HANDLE low_memory = NULL;
static int low_memory_seen = 0;

static void poll_low_memory(void) {
  BOOL low = FALSE;
  if (low_memory == NULL || !QueryMemoryResourceNotification(low_memory, &low)) return;
  int level = low ? 1 : 0;
  if (level != low_memory_seen) {
    low_memory_seen = level;
    pressure_store(level);
  }
}

static int read_counters(PROCESS_MEMORY_COUNTERS_EX *counters) {
  counters->cb = (DWORD)sizeof *counters;
  return GetProcessMemoryInfo(GetCurrentProcess(), (PROCESS_MEMORY_COUNTERS *)counters,
                              (DWORD)sizeof *counters);
}

CAMLprim value caml_rays_mem_pressure_start(value unit) {
  (void)unit;
  if (low_memory == NULL) low_memory = CreateMemoryResourceNotification(LowMemoryResourceNotification);
  return low_memory != NULL ? Val_true : Val_false;
}

/* Critical only, as on macOS: the CRT's heap minimum. OCaml's own heap is not the CRT's, so this
   mostly returns nothing. */
CAMLprim value caml_rays_mem_pressure_relief(value unit) {
  (void)unit;
  _heapmin();
  return Val_unit;
}

/* Not ported: 0 is unknown, as the stub answered before. */
CAMLprim value caml_rays_mem_physical_ram(value unit) {
  (void)unit;
  return Val_long(0);
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

/* macOS's footprint is the dirty and compressed memory the process owns. Windows has no such number
   here: PrivateUsage is committed private memory (resident or paged out, shared file mappings
   excluded), so memory reserved and committed but never touched counts on Windows and not on macOS. */
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
#elif defined(_WIN32)
  PROCESS_MEMORY_COUNTERS_EX counters;
  if (read_counters(&counters) && counters.PrivateUsage <= (SIZE_T)Max_long) {
    result = caml_alloc_some(Val_long((intnat)counters.PrivateUsage));
  } else {
    result = Val_none;
  }
#else
  result = Val_none;
#endif
  CAMLreturn(result);
}

/* Windows: PeakPagefileUsage, the peak committed private memory (the peak of what PrivateUsage reads). */
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
#elif defined(_WIN32)
  PROCESS_MEMORY_COUNTERS_EX counters;
  if (read_counters(&counters) && counters.PeakPagefileUsage <= (SIZE_T)Max_long) {
    result = caml_alloc_some(Val_long((intnat)counters.PeakPagefileUsage));
  } else {
    result = Val_none;
  }
#else
  result = Val_none;
#endif
  CAMLreturn(result);
}

/* Windows and other ports: None. Windows has no per-process retired-instruction count (its cycle
   counter is not instructions), so none is approximated. */
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
