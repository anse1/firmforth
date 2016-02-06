#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <dlfcn.h>

#include <libfirm/firm.h>
#include <libfirm/ident.h>

#include "firmforth.h"
#include "gitrev.h"
#include "mangle.h"

/* Format string to generate a shared object file (first parameter)
   for dlopen() from assembly file (second parameter). */
#define LINK_COMMAND "gcc -shared -o %s %s"

/* Forth data stack */
union cell data_stack[1<<20];

/* Firm type for elements of the data stack */
ir_type *type_cell;

/* Firm type for stack pointer */
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
  do {if (!compiling) {fprintf(stderr, "ERROR: not in compilation mode\n");return sp;}} while (0)

/* Firm type of methods implementing forth words */
ir_type *word_method_type = 0;

/* not in public Firm API */
void remove_irp_irg(ir_graph *irg);
void add_irp_irg(ir_graph *irg);
ir_graph *create_irg_copy(ir_graph *irg);
void set_entity_irg(ir_entity *ent, ir_graph *irg);

/* The first forth word method */
cell* hi(cell *sp)
{
  puts("firmforth " GITREV);
  return sp;
}

/* Forth dictionary entry for "hi" */
struct dict hi_entry = {
   .name = "hi",
   .code = hi,
};

/* Definitions of standard forth words have no comments on them.  See
   ANSI X3.215-1994 for their definition. */
cell* dot(cell *sp)
{
  sp--;
  printf("%ld\n", sp->i);
  return sp;
}

struct dict dot_entry = {
  .name = ".",
  .code = dot,
  .next = &hi_entry,
  .ldname = "dot"
};

cell* add(cell *sp)
{
  sp--;
  sp[-1].i = sp[-1].i + sp[0].i;
  return sp;
}

struct dict add_entry = {
  .name = "+",
  .code = add,
  .next = &dot_entry,
  .ldname = "add"
};

cell* greater_than(cell *sp)
{
  sp -= 1;
  sp[-1].i = sp[-1].i > sp[0].i ? 1 : 0;
  return sp;
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
cell* colon(cell *sp)
{
  compiling = 1; /* switch interpreter mode */

  /* create dictionary entry */
  struct dict *entry = malloc(sizeof(struct dict));
  entry->name = strdup(next());
  entry->code = 0;
  entry->immediate = 0;
  entry->next = dictionary;
  dictionary = entry;

  /* Create valid linker symbol */
  ident *id;
  {
    char *mangled = mangle("word_%03d_", entry->name, "");
    id = id_unique(mangled);
    free(mangled);
  }

  /* Create Firm entity for word */
  ir_type   *global_type = get_glob_type();
  entry->entity = new_entity(global_type, id, word_method_type);
  set_entity_ld_ident(entry->entity, id);

  /* Create IR graph */
  ir_graph *irg = new_ir_graph(entry->entity, 1);
  set_current_ir_graph(irg);

  ir_node *proj_arg_t = new_Proj(get_irg_start(irg), mode_T, pn_Start_T_args);
  /* IR to load current forth data stack pointer */
  set_value(0, new_Proj(proj_arg_t, mode_P, 0));

  entry->ldname = get_id_str(id);
  return sp;
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
  ir_node *ir_sp = get_value(0, mode_P);
  ir_node   *return_node = new_Return(mem, 1, &ir_sp);

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
cell* semicolon(cell *sp)
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
  lower_highlevel_graph(irg);

  dump_ir_graph(irg, "optimized");

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
  return sp;
}

struct dict semicolon_entry =
{
  .name = ";",
  .code = semicolon,
  .next = &colon_entry,
  .immediate = 1,
  .ldname = "semicolon"
};

cell* key(cell *sp)
{
  sp->i = getchar();
  sp++;
  return sp;
}

struct dict key_entry =
{
  .name = "key",
  .code = key,
  .next = &semicolon_entry,
};

cell* emit(cell *sp)
{
  sp--;
  putchar(sp->i);
  return sp;
}

struct dict emit_entry =
{
  .name = "emit",
  .code = emit,
  .next = &key_entry,
};

cell* sp_load(cell *sp)
{
  sp->a = sp;
  sp++;
  return sp;
}

struct dict sp_load_entry =
{
  .name = "sp@",
  .code = sp_load,
  .next = &emit_entry,
  .ldname = "sp_load"
};

cell* sp_store(cell *sp)
{
  sp = sp[-1].a;
  return sp;
}

struct dict sp_store_entry =
{
  .name = "sp!",
  .code = sp_store,
  .next = &sp_load_entry,
  .ldname = "sp_store"
};

cell* cells(cell *sp)
{
  sp[-1].i = sizeof(union cell) * sp[-1].i;
  return sp;
}

struct dict cells_entry =
{
  .name = "cells",
  .code = cells,
  .next = &sp_store_entry,
};

cell *swap(cell *sp)
{
  cell tmp = sp[-1];
  sp[-1] = sp[-2];
  sp[-2] = tmp;
  return sp;
}

struct dict swap_entry =
{
  .name = "swap",
  .code = swap,
  .next = &cells_entry,
};

cell *drop(cell *sp)
{
  return --sp;
}

struct dict drop_entry =
{
  .name = "drop",
  .code = drop,
  .next = &swap_entry,
};

cell *equal(cell *sp)
{
  sp[-2].i = sp[-2].i == sp[-1].i;
  return --sp;
}

struct dict equal_entry =
{
  .name = "=",
  .code = equal,
  .next = &drop_entry,
  .ldname = "equal",
};

cell* zero(cell *sp)
{
  sp->i = 0;
  sp++;
  return sp;
}

struct dict zero_entry =
{
  .name = "0",
  .code = zero,
  .next = &equal_entry,
  .ldname = "zero"
};

cell* one(cell *sp)
{
  sp->i = 1;
  sp++;
  return sp;
}

struct dict one_entry =
{
  .name = "1",
  .code = one,
  .next = &zero_entry,
  .ldname = "one"
};

cell* negate(cell *sp)
{
  sp[-1].i = -sp[-1].i;
  return sp;
}

struct dict negate_entry =
{
  .name = "negate",
  .code = negate,
  .next = &one_entry,
};

cell* load(cell *sp)
{
  sp[-1].a = *(sp[-1].aa);
  return sp;
}

struct dict load_entry =
{
  .name = "@",
  .code = load,
  .next = &negate_entry,
  .ldname = "load"
};

/* ( x a-addr -- ) */
cell* store(cell *sp)
{
  *(sp[-1].aa) = sp[-2].a;
  sp -= 2;
  return sp;
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
   pushed on the stack for future maturing.  The basic block
   projected from the true condition is made the current one.  The
   basic block for the false condition is pushed on the stack as
   well. */
cell* w_if(cell *sp) /* -- bb_cond bb_false */
{
  CROAK_UNLESS_COMPILING();

  /* IR to load value at top of stack sp[-1] */
  ir_node *offset = new_Const_long(mode_Ls, -sizeof(union cell));
  ir_node *ir_sp = get_value(0, mode_P);
  ir_sp = new_Add(ir_sp, offset, mode_P);
  set_value(0, ir_sp);
  ir_node *load_data = new_Load(get_store(), ir_sp, mode_Lu, type_cell, 0);
  ir_node *load_data_res = new_Proj(load_data, mode_Lu, pn_Load_res);
  ir_node *load_data_mem = new_Proj(load_data, mode_M, pn_Load_M);
  set_store(load_data_mem);

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

  /* May not mature the current block yet because repeat might add a
     backedge to it if we are in a "loop header" */
  sp->a = get_cur_block();
  sp++;

  set_cur_block(block_true);

  /* Push false block on stack */
  sp->a = block_false;
  sp++;
  return sp;
}

struct dict if_entry =
{
  .name = "if",
  .immediate = 1,
  .code = w_if,
  .next = &store_entry,
};

/* also implements while word */
struct dict while_entry =
{
  .name = "while",
  .immediate = 1,
  .code = w_if,
  .next = &if_entry,
};

/* Mature the basic block under construction and switch construction
   to the basic block on the stack.  Push the basic block following
   the true/false blocks onto the stack. */
cell* w_else(cell *sp) /* bb_false -- bb_then */
{
  CROAK_UNLESS_COMPILING();

  ir_node *bb_false = sp[-1].a;

  ir_node *jump = new_Jmp();
  ir_node *bb_then = new_immBlock();
  add_immBlock_pred(bb_then, jump);
  mature_immBlock(get_cur_block());
  set_cur_block(bb_false);

  sp[-1].a = bb_then;
  return sp;
}

struct dict else_entry =
{
  .name = "else",
  .immediate = 1,
  .code = w_else,
  .next = &while_entry,
};

/* Finalize current basic block as well as the condition block and
   make the one on the stack the current one. */
/* ( bb_cond bb_then -- ) */
cell* w_then(cell *sp)
{
  CROAK_UNLESS_COMPILING();
  sp--;
  ir_node *bb_then = sp->a;
  ir_node *jump = new_Jmp();
  mature_immBlock(get_cur_block());
  add_immBlock_pred(bb_then, jump);

  sp--;
  ir_node *bb_cond = sp->a;
  mature_immBlock(bb_cond);

  set_cur_block(bb_then);
  return sp;
}

struct dict then_entry =
{
  .name = "then",
  .immediate = 1,
  .code = w_then,
  .next = &else_entry,
};

/* Begin loop construction */
/* -- bb_head */
cell *begin(cell *sp)
{
  ir_node *jump = new_Jmp();
  mature_immBlock(get_cur_block());

  ir_node *bb_head = new_immBlock();
  sp->a = bb_head;
  sp++;
  set_cur_block(bb_head);
  add_immBlock_pred(bb_head, jump);
  return sp;
}

struct dict begin_entry =
{
  .name = "begin",
  .immediate = 1,
  .code = begin,
  .next = &then_entry
};

/* Finalize loop construction */
/* ( bb_head bb_then -- ) */
cell *repeat(cell *sp)
{
  CROAK_UNLESS_COMPILING();
  sp--;
  ir_node *bb_then = sp->a;
  ir_node *jump = new_Jmp();
  mature_immBlock(get_cur_block());

  sp--;
  ir_node *bb_head = sp->a;
  add_immBlock_pred(bb_head, jump);
  mature_immBlock(bb_head);

  set_cur_block(bb_then);
  return sp;
}

struct dict repeat_entry =
{
  .name = "repeat",
  .immediate = 1,
  .code = repeat,
  .next = &begin_entry
};

static void compile(struct dict *entry)
{
  ir_node *mem = get_store();
/*   ir_node *ptr = new_Const_long(mode_P, (long)(entry->code)); */
  ir_node *ptr = new_Address(entry->entity);
  ir_node *ir_sp = get_value(0, mode_P);
  ir_node *call = new_Call(mem, ptr, 1, &ir_sp, word_method_type);
  mem = new_Proj(call, mode_M, pn_Call_M);
  set_store(mem);
  ir_sp = new_Proj(call, mode_T, pn_Call_T_result);
  ir_sp = new_Proj(ir_sp, mode_P, 0);
  set_value(0, ir_sp);
}

struct dict *dictionary = &repeat_entry;

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
static void initialize_firm()
{
  ir_init();

  /* initialize backend early */
  int res = be_parse_arg("isa=amd64");
  res |= be_parse_arg("pic=elf");
  be_get_backend_param();
  assert(res != 0);

  /* create types */
  word_method_type = new_type_method(1, 1);
  type_cell = new_type_primitive(mode_Ls);
  type_cell_ptr = new_type_pointer(type_cell);
  set_method_res_type(word_method_type, 0, type_cell_ptr);
  set_method_param_type(word_method_type, 0, type_cell_ptr);

  /* If we do have IR for ourselves, load it so we can inline
     primitives later instead of calling them. */
  if (ir_import("firmforth.ir")) {
    fprintf(stderr, "Cannot load intermediate representation for forth primitives.\n"
	    "You can generate it using \"cparser --export-ir firmforth.c\"\n"
	    "Continuing anyway but your programs will be slower.\n");
  }

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
/* Read tokens and look them up in the dictionary.  When not
   compiling, execute the words, Otherwise, add IR for their execution
   to the word under construction. */
cell* interpret(cell *sp)
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
      sp = entry->code(sp);
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
  return sp;
}

int main()
{
  initialize_firm();
  cell *sp = data_stack;
  while(1)
    sp = interpret(sp);
}
