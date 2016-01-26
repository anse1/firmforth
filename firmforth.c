#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "firmforth.h"
#include "gitrev.h"

union cell parameter_stack[1<<20];

union cell *sp = parameter_stack;

void hello(union cell *sp[])
{
  puts("firmforth " GITREV);
}
struct dict hello_entry = {
   .name = "hi",
   .code = hello,
};

void dot(union cell *sp[])
{
  (*sp)--;
  printf("%ld\n", (**sp).i);
}
struct dict dot_entry = {
   .name = ".",
   .code = dot,
   .next = &hello_entry
};

void add(union cell *sp[])
{
  (*sp) -= 2;
  (*sp)[-1].i = (*sp)[0].i + (*sp)[1].i;
}
struct dict add_entry = {
   .name = "+",
   .code = add,
   .next = &dot_entry
};

void greater_than(union cell *sp[])
{
  (*sp) -= 2;
  (*sp)[-1].i = (*sp)[0].i > (*sp)[1].i ? 1 : 0;
}
struct dict greater_than_entry = {
   .name = ">",
   .code = greater_than,
   .next = &add_entry
};

void interpret(union cell *sp[])
{
  char *line = 0;
  size_t n = 0;
  struct dict *entry = &greater_than_entry;
  if (0 > getline(&line, &n, stdin))
    exit(0);
  for (char *pos = line; *pos; pos++)
    if (*pos == '\n')
      *pos = '\0';
  while (entry && strcmp(entry->name, line))
    entry = entry->next;
  if (entry) {
    entry->code(sp);
  } else if ((line[0] >= '0' && line[0] <= '9') || line[0] == '-') {
    (**sp).i = atoll(line);
    (*sp)++;
  } else {
    fprintf(stderr, "unknown word\n");
  }
}

int main(int argc, char *argv[])
{
  while(1)
    interpret(&sp);
}
