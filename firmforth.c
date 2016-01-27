#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <dlfcn.h>

#include <libfirm/firm.h>

#include "firmforth.h"
#include "gitrev.h"

#define LINK_COMMAND "gcc -shared a.s"

union cell parameter_stack[1<<20];

union cell *sp = parameter_stack;

int compiling = 0;
ir_type *word_method_type = 0;

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

struct dict dot_entry = { .name = ".", .code = dot,  .next = &hello_entry };

void add(union cell *sp[])
{
  (*sp) -= 2;
  (*sp)[-1].i = (*sp)[0].i + (*sp)[1].i;
}

struct dict add_entry = {.name = "+", .code = add, .next = &dot_entry };

void greater_than(union cell *sp[])
{
  (*sp) -= 2;
  (*sp)[-1].i = (*sp)[0].i > (*sp)[1].i ? 1 : 0;
}

struct dict greater_than_entry =
  {.name = ">", .code = greater_than, .next = &add_entry };

static const char *next() {
  static char *line;
  static size_t n;
  const char *token = 0;
  do {
    if (!line) {
      if (0 > getline(&line, &n, stdin))
	exit(0);
      token = strtok(line, "\n\t\v ");
    } else {
      token = strtok(0, "\n\t\v ");
    }
    if (!token) {
      free(line);
      line = 0;
    }
  } while (!token);
  return token;
}

static ir_graph *create_graph(struct dict *entry)
{
  ident *id = new_id_from_str(entry->name);
  ir_type   *global_type = get_glob_type();
  ir_entity *entity      = new_entity(global_type, id, word_method_type);
  ir_graph *irg = new_ir_graph(entity, 0);

  set_entity_ld_ident(entity, id);
  return irg;
}

void colon(union cell *sp[])
{
  compiling = 1;
  struct dict *entry = malloc(sizeof(struct dict));
  entry->name = strdup(next());
  entry->smudge = 1;
  entry->code = 0;
  entry->immediate = 0;
  entry->next = dictionary;

  dictionary = entry;

  ir_graph *irg = create_graph(entry);
  set_current_ir_graph(irg);
  ir_node *args = new_Proj(get_irg_start(irg), mode_T, pn_Start_T_args);
  ir_node *stacknode = new_Proj(args, mode_P, 0);
  (**sp).a = stacknode;
  (*sp)++;
}

struct dict colon_entry =
  {.name = ":", .code = colon, .next = &greater_than_entry };

static void create_return(void)
{
	ir_node   *mem         = get_store();
	ir_node   *return_node = new_Return(mem, 0, 0);

	ir_node *end_block   = get_irg_end_block(current_ir_graph);
	add_immBlock_pred(end_block, return_node);

	mature_immBlock(get_cur_block());
	set_cur_block(NULL);
}


void semicolon(union cell *sp[])
{
  ir_node *stacknode = (ir_node *)(*sp)[-1].aa;
  (void) stacknode;
  (*sp)--;
  compiling = 0;

  ir_graph *irg = get_current_ir_graph();

  create_return();
  irg_finalize_cons(irg);
  irg_assert_verify(irg);


  /* perform a bunch of optimisations */
  do_loop_inversion(irg);
  optimize_reassociation(irg);
  optimize_load_store(irg);
  optimize_graph_df(irg);
  combo(irg);
  scalar_replacement_opt(irg);
  place_code(irg);
  optimize_reassociation(irg);
  optimize_graph_df(irg);
  opt_jumpthreading(irg);
  optimize_graph_df(irg);
  construct_confirms(irg);
  optimize_graph_df(irg);
  remove_confirms(irg);
  optimize_cf(irg);
  optimize_load_store(irg);
  optimize_graph_df(irg);
  combo(irg);
  place_code(irg);
  optimize_cf(irg);

  FILE *out = fopen("a.s", "w");
  if(out == NULL) {
    perror("couldn't open a.s for writing");
    exit(-1);
  }

  be_main(out, "isthistheprogramname");
  fclose(out);

  system(LINK_COMMAND);

  void *dlhandle = dlopen("./a.out", RTLD_NOW);
  if(dlerror())
    puts(dlerror());
  dictionary->code = dlsym(dlhandle, dictionary->name);
  if(dlerror())
    puts(dlerror());
  dictionary->smudge = 0;
}

struct dict semicolon_entry =
  {.name = ";", .code = semicolon, .next = &colon_entry, .immediate=1 };

struct dict *dictionary = &semicolon_entry;

static void initialize_firm(void)
{
  ir_init();
  int res = be_parse_arg("isa=amd64");
  res |= be_parse_arg("pic=elf");
  assert(res != 0);
  word_method_type = new_type_method(1, 0);
/*   ir_type *type_int = new_type_primitive(mode_Ls); */
/*   ir_type *type_int_p = find_pointer_type_to_type(type_int); */
/*   ir_type *type_int_p_p = find_pointer_type_to_type(type_int_p); */

  ir_type *type_P = new_type_primitive(mode_P);

  set_method_param_type(word_method_type, 0, type_P);
}

static void compile(union cell *sp[], struct dict *entry) {
  ir_node *stacknode = (*sp)[-1].a;
  ir_node *mem = get_store();
  ir_node *ptr = new_Const_long(mode_P, (long)(entry->code));
  ir_node *call = new_Call(mem, ptr, 1, &stacknode, word_method_type);
  ir_node *store_mem = new_Proj(call, mode_M, pn_Call_M);
  set_store(store_mem);
  (void) call;
}

void interpret(union cell *sp[])
{
  struct dict *entry = dictionary;
  const char *token = next();
  while (entry && strcmp(entry->name, token)) {
    entry = entry->next;
  }
  if (entry) {
    if (compiling && !entry->immediate)
      compile(sp, entry);
    else
      entry->code(sp);
  } else if ((token[0] >= '0' && token[0] <= '9') || token[0] == '-') {
    (**sp).i = atoll(token);
    (*sp)++;
  } else {
    fprintf(stderr, "unknown word\n");
  }
}

int main()
{
  initialize_firm();
  while(1)
    interpret(&sp);
}
