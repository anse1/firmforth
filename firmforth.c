#include <stdio.h>
#include <string.h>

#include "firmforth.h"
#include "gitrev.h"

union cell parameter_stack[1<<20];

union cell **sp = parameter_stack;

void hello(union cell *param_stack[])
{
  puts("firmforth " GITREV);
}

struct dict dict = {
   .name = "hi",
   .code = hello,
   .next = 0
};

void interpret(union cell *param_stack[])
{
  char *line = 0;
  size_t n = 0;
  struct dict *entry = &dict;
  getline(&line, &n, stdin);
  for (char *pos = line; *pos; pos++)
    if (*pos == '\n')
      *pos = '\0';
  while (entry && strcmp(entry->name, line))
    entry = entry->next;
  if (entry) {
    entry->code(param_stack);
  } else {
    fprintf(stderr, "unknown word\n");
  }
}

int main(int argc, char *argv[])
{
  while(1)
    interpret(sp);
}
