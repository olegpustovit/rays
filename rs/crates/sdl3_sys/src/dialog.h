#ifndef RAYS_SDL3_SYS_DIALOG_H
#define RAYS_SDL3_SYS_DIALOG_H

#include <SDL3/SDL.h>
#include <stdbool.h>
#include <stddef.h>

enum { RAYS_DIALOG_SLOTS = 8 };

typedef enum {
  RAYS_DIALOG_OPEN_FILE,
  RAYS_DIALOG_OPEN_FILES,
  RAYS_DIALOG_SAVE_FILE,
  RAYS_DIALOG_OPEN_FOLDER
} rays_dialog_kind;

typedef enum {
  RAYS_OUTCOME_CHOSEN,
  RAYS_OUTCOME_CANCELLED,
  RAYS_OUTCOME_FAILED
} rays_dialog_outcome_kind;

typedef struct {
  int id;
  rays_dialog_outcome_kind outcome;
  char *payload;
  size_t payload_length;
} rays_dialog_result;

int rays_dialog_show(rays_dialog_kind kind, SDL_Window *window,
    const char *const *names, const char *const *patterns, int count,
    const char *default_location);
bool rays_dialog_take(rays_dialog_result *result);

void *rays_dialog_reserve(const char *const *names, const char *const *patterns,
    int count, const char *default_location, int *id);
void rays_dialog_abandon(void *slot);
void SDLCALL rays_dialog_callback(void *slot, const char *const *filelist, int filter);
#endif
