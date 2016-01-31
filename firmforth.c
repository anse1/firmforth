#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <dlfcn.h>

#include <libfirm/firm.h>
#include <libfirm/ident.h>

#include "firmforth.h"
#include "gitrev.h"

/* Format string to generate a shared object file (first parameter)
   for dlopen() from assembly file (second parameter). */
#define LINK_COMMAND "gcc -shared -o %s %s"

/* Forth data stack */
union cell data_stack[1<<10];

/* Firm type for elements of the data stack */
ir_type *type_cell;

/* Forth stack pointer */
union cell *sp = data_stack;

/* Firm entity and type for stack pointer */
ir_entity *sp_entity;
ir_type *type_cell_ptr;

/* The interpreter splits the input into tokens and searches the
   dictionary for a forth word of that name.  Depending on the
   following mode variable, it either executes the word right-away or
   adds the execution of the word to the intermediate language of a
   new word being defined. */
int compiling = 0;

#define ASSERT_STACK() \
  do { assert(sp >= data_stack); \
    assert(sp < (data_stack + sizeof(data_stack)));	\
  } while (0)

#define CROAK_UNLESS_COMPILING() \
  do {if (!compiling) {fprintf(stderr, "ERROR: not in compilation mode\n");return;}} while (0)

/* Firm type of methods implementing forth words */
ir_type *word_method_type = 0;

/* not in public Firm API */
void remove_irp_irg(ir_graph *irg);
void add_irp_irg(ir_graph *irg);
ir_graph *create_irg_copy(ir_graph *irg);
void set_entity_irg(ir_entity *ent, ir_graph *irg);

/* The first forth word method */
void hi()
{
  puts("firmforth " GITREV);
}

/* Forth dictionary entry for "hi" */
struct dict hi_entry = {
   .name = "hi",
   .code = hi,
};

/* Definitions of standard forth words have no comments on them.  See
   ANSI X3.215-1994 for their definition. */
void dot()
{
  sp--;
  printf("%ld\n", sp->i);
}

struct dict dot_entry = {
  .name = ".",
  .code = dot,
  .next = &hi_entry,
  .ldname = "dot"
};

void add()
{
  sp--;
  sp[-1].i = sp[-1].i + sp[0].i;
}

struct dict add_entry = {
  .name = "+",
  .code = add,
  .next = &dot_entry,
  .ldname = "add"
};

void greater_than()
{
  sp -= 1;
  sp[-1].i = sp[-1].i > sp[0].i ? 1 : 0;
}

struct dict greater_than_entry =
{
  .name = ">",
  .code = greater_than,
  .next = &add_entry,
  .ldname = "greater_than"
};

/* Fetch a whitespace-delimited string from stdin, skipping # and
   \-comments */
static const char *next() {
  static char *line;
  static size_t n;
  const char *token = 0;
  do {
    if (!line) {
    retry:
      if (0 > getline(&line, &n, stdin))
	exit(0);
      if (*line == '#') {
	free(line);
	goto retry;
      }
      token = strtok(line, "\n\t\v ");
    } else {
      token = strtok(0, "\n\t\v ");
    }
    if (!token || *token == '\\') {
      free(line);
      token = line = 0;
    }
  } while (!token);
  return token;
}

/* Start compilation of a new word */
void colon()
{
  compiling = 1; /* switch interpreter mode */

  /* create dictionary entry */
  struct dict *entry = malloc(sizeof(struct dict));
  entry->name = strdup(next());
  entry->smudge = 1;
  entry->code = 0;
  entry->immediate = 0;
  entry->next = dictionary;
  dictionary = entry;

  /* Create Firm entity for word */
  ident *id = id_unique("word_%03x");
  ir_type   *global_type = get_glob_type();
  entry->entity = new_entity(global_type, id, word_method_type);
  set_entity_ld_ident(entry->entity, id);

  /* Create IR graph */
  ir_graph *irg = new_ir_graph(entry->entity, 0);
  set_current_ir_graph(irg);

  entry->ldname = get_id_str(id);
}

struct dict colon_entry =
{
  .name = ":",
  .code = colon,
  .next = &greater_than_entry,
  .ldname = "colon"
};

static void create_return(void)
{
  ir_node   *mem         = get_store();
  ir_node   *return_node = new_Return(mem, 0, 0);

  ir_node *end_block   = get_irg_end_block(current_ir_graph);
  add_immBlock_pred(end_block, return_node);

  mature_immBlock(get_cur_block());
  set_cur_block(NULL);
}

static void after_inline_opt(ir_graph *irg)
{
  scalar_replacement_opt(irg);
  optimize_graph_df(irg);
  optimize_cf(irg);
  combo(irg);
}

/* End compilation of a word */
void semicolon(void)
{
  CROAK_UNLESS_COMPILING();
  ASSERT_STACK();

  compiling = 0;

  ir_graph *irg = get_current_ir_graph();

  create_return();
  irg_finalize_cons(irg);
  irg_assert_verify(irg);

  dump_ir_graph(irg, "constructed");

  /* perform a bunch of optimisations */
  do_loop_inversion(irg);
  optimize_reassociation(irg);
  optimize_load_store(irg);

  optimize_cf(irg);
  dump_ir_graph(irg, "pre-inline");

  inline_functions(750 /* maxsize */,
		   0 /* threshold */,
		   after_inline_opt);

  dump_ir_graph(irg, "inline");

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

  dump_ir_graph(irg, "optimized");

  lower_highlevel_graph(irg);

  /* Record the pristine IRG for future inlining. */
  ir_graph *pristine = create_irg_copy(irg);

  /* Generate assembly */

  char filename_s[64];
  snprintf(filename_s, sizeof(filename_s), "jit-%s.s", dictionary->ldname);
  FILE *out = fopen(filename_s, "w");
  if(out == NULL) {
    perror("couldn't open assembly file for writing");
    exit(-1);
  }
  be_main(out, "cup");
  fclose(out);

  /* Assemble shared object */
  char filename_so[64];
  snprintf(filename_so, sizeof(filename_so), "./jit-%s.so", dictionary->ldname);
  char command[128];
  snprintf(command, sizeof(command), LINK_COMMAND,
	   filename_so, filename_s);
  system(command);

  /* dlopen() the shared object */
  void *dlhandle = dlopen(filename_so, RTLD_NOW|RTLD_GLOBAL);
  const char *err = dlerror();
  if(err)
    puts(err), exit(-1);
  dictionary->code = dlsym(dlhandle, dictionary->ldname);
  err = dlerror();
  if(err)
    puts(err), exit(-1);

  /* restore pristine IRG for future inlining */
  remove_irp_irg(irg);
  set_irg_entity(pristine, dictionary->entity);
  set_entity_irg(dictionary->entity, pristine);
  add_irp_irg(pristine);

  /* Avoid repeated code generation for the entity */
  set_entity_linkage(dictionary->entity, IR_LINKAGE_NO_CODEGEN);

  dictionary->smudge = 0;
}

struct dict semicolon_entry =
{
  .name = ";",
  .code = semicolon,
  .next = &colon_entry,
  .immediate = 1,
  .ldname = "semicolon"
};

void key()
{
  sp->i = getchar();
  sp++;
}

struct dict key_entry =
{
  .name = "key",
  .code = key,
  .next = &semicolon_entry,
};

void emit()
{
  sp--;
  putchar(sp->i);
}

struct dict emit_entry =
{
  .name = "emit",
  .code = emit,
  .next = &key_entry,
};

void sp_load()
{
  sp->a = sp;
  sp++;
}

struct dict sp_load_entry =
{
  .name = "sp@",
  .code = sp_load,
  .next = &emit_entry,
  .ldname = "sp_load"
};

void sp_store()
{
  sp = sp[-1].a;
}

struct dict sp_store_entry =
{
  .name = "sp!",
  .code = sp_store,
  .next = &sp_load_entry,
  .ldname = "sp_store"
};

void cells()
{
  sp[-1].i = sizeof(union cell) * sp[-1].i;
}

struct dict cells_entry =
{
  .name = "cells",
  .code = cells,
  .next = &sp_store_entry,
};

void zero()
{
  sp->i = 0;
  sp++;
}

struct dict zero_entry =
{
  .name = "0",
  .code = zero,
  .next = &cells_entry,
  .ldname = "zero"
};

void one()
{
  sp->i = 1;
  sp++;
}

struct dict one_entry =
{
  .name = "1",
  .code = one,
  .next = &zero_entry,
  .ldname = "one"
};

void negate()
{
  sp[-1].i = -sp[-1].i;
}

struct dict negate_entry =
{
  .name = "negate",
  .code = negate,
  .next = &one_entry,
};

void load()
{
  sp[-1].a = *(sp[-1].aa);
}

struct dict load_entry =
{
  .name = "@",
  .code = load,
  .next = &negate_entry,
  .ldname = "load"
};

/* ( x a-addr -- ) */
void store()
{
  *(sp[-1].aa) = sp[-2].a;
  sp -= 2;
}

struct dict store_entry =
{
  .name = "!",
  .code = store,
  .next = &load_entry,
  .ldname = "store"
};

/* Construct IR to fetch a value from the stack and fork control flow.
   Two new basic blocks are created.  The current basic block is
   finalized.  The basic block projected from the true condition is
   made the current one.  The basic block for the false condition is
   pushed on the forth stack. */
void w_if() /* -- bb_false */
{
  CROAK_UNLESS_COMPILING();

  /* IR to load current forth data stack pointer */
  ir_node *ir_sp = new_Address(sp_entity);
  ir_node *load_ptr = new_Load(get_store(), ir_sp, mode_P, type_cell_ptr, 0);
  ir_node *load_ptr_res = new_Proj(load_ptr, mode_P, pn_Load_res);
  ir_node *load_ptr_mem = new_Proj(load_ptr, mode_M, pn_Load_M);
  set_store(load_ptr_mem);

  /* IR to load value at top of stack sp[-1] */
  ir_node *offset = new_Const_long(mode_Ls, -sizeof(union cell));
  ir_node *add = new_Add(load_ptr_res, offset, mode_P);
  ir_node *load_data = new_Load(get_store(), add, mode_Lu, type_cell, 0);
  ir_node *load_data_res = new_Proj(load_data, mode_Lu, pn_Load_res);
  ir_node *load_data_mem = new_Proj(load_data, mode_M, pn_Load_M);
  set_store(load_data_mem);

  /* IR to decrement stack pointer */
  ir_node *store_ptr = new_Store(get_store(), ir_sp, add, type_cell_ptr, 0);
  ir_node *store_ptr_mem = new_Proj(store_ptr, mode_M, pn_Store_M);
  set_store(store_ptr_mem);

  /* IR to compare with zero */
  ir_node *cmp = new_Cmp(load_data_res,
			 new_Const_long(mode_Lu, 0),
			 ir_relation_less_greater);
  ir_node *cond = new_Cond(cmp);

  /* Create block for condition true */
  ir_node *proj_true  = new_Proj(cond, mode_X, pn_Cond_true);
  ir_node *block_true = new_immBlock();
  add_immBlock_pred(block_true, proj_true);

  /* Same for false */
  ir_node *proj_false = new_Proj(cond, mode_X, pn_Cond_false);
  ir_node *block_false = new_immBlock();
  add_immBlock_pred(block_false, proj_false);

  /* Make true block the current one */
  mature_immBlock(get_cur_block());
  set_cur_block(block_true);

  /* Push false block on stack */
  sp->a = block_false;
  sp++;
}

struct dict if_entry =
{
  .name = "if",
  .immediate = 1,
  .code = w_if,
  .next = &store_entry,
};

/* Finalize basic block under construction and switch to the basic
   block on the stack.  Push the basic block following the true/false
   blocks onto the stack. */
void w_else() /* bb_false -- bb_then */
{
  CROAK_UNLESS_COMPILING();

  ir_node *bb_false = sp[-1].a;

  ir_node *jump = new_Jmp();
  ir_node *bb_then = new_immBlock();
  add_immBlock_pred(bb_then, jump);
  mature_immBlock(get_cur_block());
  set_cur_block(bb_false);

  sp[-1].a = bb_then;
}

struct dict else_entry =
{
  .name = "else",
  .immediate = 1,
  .code = w_else,
  .next = &if_entry,
};

/* Finalize current basic block and make the one on the stack the
   current one. */
/* ( bb_then -- ) */
void w_then()
{
  CROAK_UNLESS_COMPILING();
  sp--;
  ir_node *bb_then = sp->a;
  ir_node *jump = new_Jmp();
  add_immBlock_pred(bb_then, jump);
  mature_immBlock(get_cur_block());
  set_cur_block(bb_then);
}

struct dict then_entry =
{
  .name = "then",
  .immediate = 1,
  .code = w_then,
  .next = &else_entry,
};

struct dict *dictionary = &then_entry;

static ir_entity *find_global_entity(const char *name)
{
  size_t n_members = get_compound_n_members(get_glob_type());
  for (size_t i = 0; i < n_members; ++i) {
    ir_entity *member = get_compound_member(get_glob_type(), i);

    if (!strcmp(get_entity_ld_name(member), name))
      return member;
  }
  return NULL;
}

/* initialize libfirm and set globally visible entities/types */
static void initialize_firm(void)
{
  ir_init();

  /* initialize backend early */
  int res = be_parse_arg("isa=amd64");
  res |= be_parse_arg("pic=elf");
  be_get_backend_param();
  assert(res != 0);

  /* create types */
  word_method_type = new_type_method(0, 0);
  type_cell = new_type_primitive(mode_Ls);
  type_cell_ptr = find_pointer_type_to_type(type_cell);

  /* If we do have IR for ourselves, load it so we can inline
     primitives later instead of calling them. */
  if (ir_import("firmforth.ir")) {
    fprintf(stderr, "Cannot load intermediate representation for forth primitives.\n"
	    "You can generate it using \"cparser --export-ir firmforth.c\"\n"
	    "Continuing anyway but your programs will be slower.\n");
  }

  sp_entity = find_global_entity("sp");
  if (!sp_entity)
    sp_entity = new_entity(get_glob_type(), new_id_from_str("sp"), type_cell_ptr);

  /* Add entities for functions.  This is needed as long as the code
     generator can't call a function at an absolute address */
  for (struct dict *entry = dictionary; entry; entry = entry->next) {
    const char* ld_name = entry->ldname ? entry->ldname : entry->name;
    ir_entity *entity = find_global_entity(ld_name);
    if (!entity) {
      ident *id = new_id_from_str(ld_name);
      entity = new_entity(get_glob_type(), id, word_method_type);
    }
    set_entity_linkage(entity, IR_LINKAGE_NO_CODEGEN);
    entry->entity = entity;
  }

#define foreach_irp_irg(idx, irg) \
	for (bool irg##__b = true; irg##__b; irg##__b = false) \
		for (size_t idx = 0, irg##__n = get_irp_n_irgs(); irg##__b && idx != irg##__n; ++idx) \
			for (ir_graph *const irg = (irg##__b = false, get_irp_irg(idx)); !irg##__b; irg##__b = true)

  /* Do not generate Code for the imported IR */
  foreach_irp_irg(i, old_irg) {
    set_entity_linkage(get_irg_entity(old_irg), IR_LINKAGE_NO_CODEGEN);
  }
}

/* Add IR to the program to execute the forth word described by
   ENTRY. */
static void compile(struct dict *entry) {
  ir_node *mem = get_store();
/*   ir_node *ptr = new_Const_long(mode_P, (long)(entry->code)); */
  ir_node *ptr = new_Address(entry->entity);
  ir_node *call = new_Call(mem, ptr, 0, 0, word_method_type);
  ir_node *store_mem = new_Proj(call, mode_M, pn_Call_M);
  set_store(store_mem);
}

/* Read tokens and look them up in the dictionary.  When not
   compiling, execute the words, Otherwise, add IR for their execution
   to the word under construction. */
void interpret()
{
  struct dict *entry = dictionary;
  const char *token;
 retry:
  token = next();

  if (!strcmp(token, "(")) {
    do {
      token = next();
    } while (strcmp(token, ")"));
    goto retry;
  }
  while (entry && strcmp(entry->name, token)) {
    entry = entry->next;
  }
  if (entry) {
    ASSERT_STACK();
    if (compiling && !entry->immediate)
      compile(entry);
    else
      entry->code();
    ASSERT_STACK();
  } else if ((token[0] >= '0' && token[0] <= '9') || token[0] == '-') {
    if (compiling) {
      fprintf(stderr, "ERROR: NIY\n");
    } else {
      sp->i = atoll(token);
      sp++;
    }
  } else {
    fprintf(stderr, "ERROR: unknown word\n");
  }
}

int main()
{
  initialize_firm();
  while(1)
    interpret();
}
