#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include <libfirm/firm.h>

#include "firmforth.h"
#include "gitrev.h"

union cell parameter_stack[1<<20];

union cell *sp = parameter_stack;

static void initialize_firm(void)
{
	ir_init();
	int res = be_parse_arg("isa=amd64");
	assert(res != 0);
}

void hello(union cell *sp[])
{
  (void) sp;
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

const char *next() {
  static char *line;
  static size_t n;
  const char *token = 0;
  do {
    if (!line)
      if (0 > getline(&line, &n, stdin))
	exit(0);

    if (!(token = strtok(line, "\n\t\v "))) {
      free(line);
      line = 0;
    }
  } while (!token);
  return token;
}

void interpret(union cell *sp[])
{
  struct dict *entry = &greater_than_entry;
  const char *token;
  while ((token = next())) {
    while (entry && strcmp(entry->name, token))
      entry = entry->next;
    if (entry) {
      entry->code(sp);
    } else if ((token[0] >= '0' && token[0] <= '9') || token[0] == '-') {
      (**sp).i = atoll(token);
      (*sp)++;
    } else {
      fprintf(stderr, "unknown word\n");
    }
  }
}

int main()
{
  initialize_firm();
  while(1)
    interpret(&sp);
}
