#include <stdint.h>
#include <stddef.h>

typedef void (*pressure_callback)(int);
static pressure_callback store_pressure;

#if defined(__APPLE__)
#include <dispatch/dispatch.h>
#include <libproc.h>
#include <malloc/malloc.h>
#include <sys/resource.h>
#include <sys/sysctl.h>
#include <unistd.h>

static dispatch_source_t pressure_source;

static int read_rusage(struct rusage_info_v4 *info) {
  return proc_pid_rusage(getpid(), RUSAGE_INFO_V4, (rusage_info_t *)info) == 0;
}

static void on_pressure(void *context) {
  (void)context;
  unsigned long flags = dispatch_source_get_data(pressure_source);
  if (store_pressure != NULL)
    store_pressure((flags & DISPATCH_MEMORYPRESSURE_CRITICAL)
                       ? 2
                       : ((flags & DISPATCH_MEMORYPRESSURE_WARN) ? 1 : 0));
}

int mem_sys_pressure_start(pressure_callback callback) {
  store_pressure = callback;
  if (pressure_source == NULL) {
    pressure_source = dispatch_source_create(
        DISPATCH_SOURCE_TYPE_MEMORYPRESSURE, 0,
        DISPATCH_MEMORYPRESSURE_NORMAL | DISPATCH_MEMORYPRESSURE_WARN |
            DISPATCH_MEMORYPRESSURE_CRITICAL,
        dispatch_get_global_queue(QOS_CLASS_UTILITY, 0));
    if (pressure_source == NULL) return 0;
    dispatch_source_set_event_handler_f(pressure_source, on_pressure);
    dispatch_activate(pressure_source);
  }
  return 1;
}

int mem_sys_phys_footprint(int64_t *out) {
  struct rusage_info_v4 info;
  if (!read_rusage(&info) || info.ri_phys_footprint > INT64_MAX) return 0;
  *out = (int64_t)info.ri_phys_footprint;
  return 1;
}

int mem_sys_peak_footprint(int64_t *out) {
  struct rusage_info_v4 info;
  if (!read_rusage(&info) || info.ri_lifetime_max_phys_footprint > INT64_MAX) return 0;
  *out = (int64_t)info.ri_lifetime_max_phys_footprint;
  return 1;
}

int mem_sys_instructions(int64_t *out) {
  struct rusage_info_v4 info;
  if (!read_rusage(&info)) return 0;
  *out = (int64_t)info.ri_instructions;
  return 1;
}

int64_t mem_sys_physical_ram(void) {
  uint64_t bytes = 0;
  size_t size = sizeof bytes;
  if (sysctlbyname("hw.memsize", &bytes, &size, NULL, 0) != 0 || bytes > INT64_MAX) return 0;
  return (int64_t)bytes;
}

int mem_sys_pressure_poll(void) { return -1; }
void mem_sys_pressure_relief(void) { malloc_zone_pressure_relief(NULL, 0); }

#elif defined(_WIN32)
#include <malloc.h>
#include <windows.h>
#include <psapi.h>

static HANDLE low_memory;
static int low_memory_seen;

static int read_counters(PROCESS_MEMORY_COUNTERS_EX *counters) {
  counters->cb = (DWORD)sizeof *counters;
  return GetProcessMemoryInfo(GetCurrentProcess(), (PROCESS_MEMORY_COUNTERS *)counters,
                              (DWORD)sizeof *counters);
}

int mem_sys_pressure_start(pressure_callback callback) {
  store_pressure = callback;
  if (low_memory == NULL)
    low_memory = CreateMemoryResourceNotification(LowMemoryResourceNotification);
  return low_memory != NULL;
}

int mem_sys_pressure_poll(void) {
  BOOL low = FALSE;
  if (low_memory == NULL || !QueryMemoryResourceNotification(low_memory, &low)) return -1;
  int level = low ? 1 : 0;
  if (level != low_memory_seen) {
    low_memory_seen = level;
    if (store_pressure != NULL) store_pressure(level);
  }
  return -1;
}

int mem_sys_phys_footprint(int64_t *out) {
  PROCESS_MEMORY_COUNTERS_EX counters;
  if (!read_counters(&counters) || counters.PrivateUsage > INT64_MAX) return 0;
  *out = (int64_t)counters.PrivateUsage;
  return 1;
}

int mem_sys_peak_footprint(int64_t *out) {
  PROCESS_MEMORY_COUNTERS_EX counters;
  if (!read_counters(&counters) || counters.PeakPagefileUsage > INT64_MAX) return 0;
  *out = (int64_t)counters.PeakPagefileUsage;
  return 1;
}

int mem_sys_instructions(int64_t *out) { (void)out; return 0; }

int64_t mem_sys_physical_ram(void) {
  MEMORYSTATUSEX status;
  status.dwLength = (DWORD)sizeof status;
  if (!GlobalMemoryStatusEx(&status) || status.ullTotalPhys > INT64_MAX) return 0;
  return (int64_t)status.ullTotalPhys;
}

void mem_sys_pressure_relief(void) { _heapmin(); }

#else
int mem_sys_pressure_start(pressure_callback callback) { (void)callback; return 0; }
int mem_sys_pressure_poll(void) { return -1; }
int mem_sys_phys_footprint(int64_t *out) { (void)out; return 0; }
int mem_sys_peak_footprint(int64_t *out) { (void)out; return 0; }
int mem_sys_instructions(int64_t *out) { (void)out; return 0; }
int64_t mem_sys_physical_ram(void) { return 0; }
void mem_sys_pressure_relief(void) {}
#endif
