#define _POSIX_C_SOURCE 200809L

#include <locale.h>
#include <stdlib.h>

#if defined(__APPLE__)
/* macOS/BSD declare strtod_l and newlocale users in <xlocale.h>. */
#include <xlocale.h>
#endif

#ifdef _WIN32
static _locale_t cfmt_c_locale;

static void cfmt_free_locale(void) {
  if (cfmt_c_locale == NULL) return;
  _free_locale(cfmt_c_locale);
  cfmt_c_locale = NULL;
}

void *cfmt_locale_create(void) {
  cfmt_c_locale = _create_locale(LC_ALL, "C");
  if (cfmt_c_locale == NULL) return NULL;
  if (atexit(cfmt_free_locale) != 0) {
    cfmt_free_locale();
    return NULL;
  }
  return (void *)cfmt_c_locale;
}

int cfmt_parse(void *locale, const char *text, double *value) {
  if (locale == NULL) return 0;
  char *end;
  const double parsed = _strtod_l(text, &end, (_locale_t)locale);
  if (end == text || *end != '\0') return 0;
  *value = parsed;
  return 1;
}
#else
static locale_t cfmt_c_locale;

static void cfmt_free_locale(void) {
  if (cfmt_c_locale == (locale_t)0) return;
  freelocale(cfmt_c_locale);
  cfmt_c_locale = (locale_t)0;
}

void *cfmt_locale_create(void) {
  cfmt_c_locale = newlocale(LC_ALL_MASK, "C", (locale_t)0);
  if (cfmt_c_locale == (locale_t)0) return NULL;
  if (atexit(cfmt_free_locale) != 0) {
    cfmt_free_locale();
    return NULL;
  }
  return (void *)cfmt_c_locale;
}

int cfmt_parse(void *locale, const char *text, double *value) {
  if (locale == NULL) return 0;
  char *end;
  const double parsed = strtod_l(text, &end, (locale_t)locale);
  if (end == text || *end != '\0') return 0;
  *value = parsed;
  return 1;
}
#endif
