/* Human readable mangling of forth words into linker symbols. */

#include <stdlib.h>
#include <string.h>

const char* mangle_tab[256] = {
  ['.'] = "dot",
  ['>'] = "greater",
  ['<'] = "less",
  ['='] = "equal",
  ['?'] = "question",
  ['\''] = "tick",
  ['/'] = "slash",
  ['@'] = "load",
  ['!'] = "store",
  [','] = "comma",
  ['-'] = "minus",
  ['+'] = "plus",
  ['*'] = "star",
  ['['] = "brkleft",
  [']'] = "brkright",
  ['('] = "parenleft",
  [')'] = "parenright",
};

char* mangle(const char *prefix, const char *word, const char *suffix)
{
  size_t len = strlen(word);

  /* worst case: longest possible translation for all characters */
  size_t maxlen = len * 10 + 1 + strlen(suffix) + strlen(prefix);
  char *result = malloc(maxlen);

  char chrbuf[2] = {'\0','\0'};

  *result = '\0';

  if (prefix)
    strcat(result, prefix);

  while (*word) {
    const char *translated = mangle_tab[(unsigned char)*word];
    if (!translated) {
      chrbuf[0] = *word;
      translated = chrbuf;
    }
    strcat(result, translated);
    word++;
  }

  if (suffix)
    strcat(result, suffix);

  return result;
}

