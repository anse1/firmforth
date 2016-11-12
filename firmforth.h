#ifndef FIRMFORTH_H
#define FIRMFORTH_H

#include <stdint.h>
#include <stdbool.h>
#include <libfirm/firm.h>

/* Type of elements on the forth data stack */
union cell {
  intptr_t i;
  uintptr_t u;
  char *s;
  const char *cs;
  void *a;
  void **aa;
};

typedef union cell cell;

/* Type of functions implementing forth words */
typedef cell* (*word)(cell *sp);
#define WORD(name) cell* (name)(cell *sp)

struct dict {
  const char *name; /* Forth word */
  int immediate : 1; /* execute even when compiling */
  word code; /* pointer to function implementing the word */
  ir_entity *entity; /* Firm entity for function */
  const char *ldname; /* Optionally override .name as linker symbol */
  struct dict *next;
};

extern struct dict *dictionary;

#endif
