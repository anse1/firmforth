#ifndef MANGLE_H
#define MANGLE_H

/* Mangle WORD into a malloc'd string that is a valid linker name.
   The mangling is incomplete and improved on demand. */
char* mangle(const char *prefix, const char *word, const char *suffix);

#endif
