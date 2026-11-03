/* A monotonic clock at the hardware's resolution. Unix.gettimeofday steps about 0.9 ms on Windows,
   which reads a 100 microsecond kernel as 0 or a whole step. */
#ifdef _WIN32
#include <windows.h>
#else
#include <time.h>
#endif
#include <caml/mlvalues.h>
#include <caml/alloc.h>

static
double rays_clock_now(void)
{
#ifdef _WIN32
  static LARGE_INTEGER frequency;
  LARGE_INTEGER count;
  if (frequency.QuadPart == 0) QueryPerformanceFrequency(&frequency);
  QueryPerformanceCounter(&count);
  return (double)count.QuadPart / (double)frequency.QuadPart;
#else
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  return (double)now.tv_sec + (double)now.tv_nsec * 1e-9;
#endif
}

value rays_clock_now_byte(value unit)
{
  (void)unit;
  return caml_copy_double(rays_clock_now());
}
