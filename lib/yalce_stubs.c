#include "yalce/src/midi.h"
#include "yalce/src/start_audio.h"
#include <caml/mlvalues.h>
#include <stdlib.h>

CAMLprim value caml_start_audio() {
  setup_audio();
  return Val_unit;
}

CAMLprim value caml_midi_setup() {
  midi_setup();
  return Val_unit;
}

// void qsort(void *base, size_t nmemb, size_t size,
//            int(*compar)(const void *, const void *));
//
// e
typedef int(compare_t)(const void *, const void *);

// void qsort(void *base, size_t nmemb, size_t size, compare_t *);
CAMLprim value caml_qsort() {}
