#include <stdio.h>
#include <string.h>
#include <strings.h>
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

/* Forth (pseudo) return stack */
union cell return_stack[1<<20];
union cell *rp = return_stack;

ir_node *control_stack[1<<10];
ir_node **cs = control_stack;

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

/* Check stack for over/underflow */
#define ASSERT_STACK() \
  do { \
    assert(sp >= data_stack);				\
    assert(sp < (data_stack + sizeof(data_stack)));	\
    assert(cs >= control_stack);				\
    assert(cs < (control_stack + sizeof(control_stack)));	\
  } while (0)

#define foreach_irp_irg(idx, irg) \
	for (bool irg##__b = true; irg##__b; irg##__b = false) \
		for (size_t idx = 0, irg##__n = get_irp_n_irgs(); irg##__b && idx != irg##__n; ++idx) \
			for (ir_graph *const irg = (irg##__b = false, get_irp_irg(idx)); !irg##__b; irg##__b = true)

#define foreach_compound_member(idx, mem, type)		       \
	for (bool mem##__b = true; mem##__b; mem##__b = false) \
		for (size_t idx = 0, mem##__n = get_compound_n_members(type); mem##__b && idx != mem##__n; ++idx) \
			for (ir_entity *const mem = (mem##__b = false, get_compound_member(type, idx)); !mem##__b; mem##__b = true)

/* Firm type of methods implementing forth words */
ir_type *word_method_type = 0;

/* not in public Firm API */
void remove_irp_irg(ir_graph *irg);
void add_irp_irg(ir_graph *irg);
ir_graph *create_irg_copy(ir_graph *irg);
void set_entity_irg(ir_entity *ent, ir_graph *irg);

/* The first forth word method */
WORD(hi)
{
  puts("firmforth " GITREV);
  return sp;
}

/* Forth dictionary entry for "hi" */
struct dict hi_entry = {
   .name = "hi",
   .code = hi,
};

WORD(bye)
{
  (void) sp;
  exit(0);
}

struct dict bye_entry = {
  .name = "bye",
  .code = bye,
  .next = &hi_entry
};

/* Definitions of standard forth words have no comments on them.  See
   ANSI X3.215-1994 for their definition. */
WORD(dot)
{
  sp--;
  printf("%ld ", sp->i);
  return sp;
}

struct dict dot_entry = {
  .name = ".",
  .code = dot,
  .next = &bye_entry,
  .ldname = "dot"
};

WORD(add)
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

/* Floored division */
WORD(divide)
{
  sp--;
  sp[-1].i = (((sp[-1].i < 0) != (sp[0].i < 0))?
             (((sp[-1].i + 1) / sp[0].i) - 1):
             ((sp[-1].i) / (sp[0].i)));
  return sp;
}

struct dict divide_entry = {
  .name = "/",
  .code = divide,
  .next = &add_entry,
  .ldname = "divide"
};

WORD(greater_than)
{
  sp -= 1;
  sp[-1].i = sp[-1].i > sp[0].i ? 1 : 0;
  return sp;
}

struct dict greater_than_entry =
{
  .name = ">",
  .code = greater_than,
  .next = &divide_entry,
  .ldname = "greater_than"
};

/* Fetch a whitespace-delimited string from stdin, skipping # and
   \-comments */
__attribute__((noinline))
const char *next() {
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
WORD(colon)
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
    char *mangled = mangle("word_", entry->name, "");
    id = id_unique(mangled);
    free(mangled);
  }

  /* Create Firm entity for word */
  ir_type   *global_type = get_glob_type();
  entry->entity = new_entity(global_type, id, word_method_type);
  set_entity_ld_ident(entry->entity, id);

  /* Create IR graph */
  /* We use one local variable for the stack pointer */
  ir_graph *irg = new_ir_graph(entry->entity, 1);
  set_current_ir_graph(irg);

  /* Put stack pointer argument into local variable 0 */
  ir_node *proj_arg_t = new_Proj(get_irg_start(irg), mode_T, pn_Start_T_args);
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

/* Create Return node.  Uses local variable 0 and get_store() to
   retrieve the sp/memory to be returned */
void create_return(void)
{
  ir_node   *mem         = get_store();
  ir_node *ir_sp = get_value(0, mode_P);
  ir_node   *return_node = new_Return(mem, 1, &ir_sp);

  ir_node *end_block   = get_irg_end_block(current_ir_graph);
  add_immBlock_pred(end_block, return_node);

  mature_immBlock(get_cur_block());
  set_cur_block(NULL);
}

void after_inline_opt(ir_graph *irg)
{
  scalar_replacement_opt(irg);
  optimize_graph_df(irg);
  optimize_cf(irg);
  optimize_load_store(irg);
  combo(irg);
}

void codegen(struct dict *entry) {
  ir_entity *method = entry->entity;
  ir_graph *irg = get_entity_irg(method);
  /* Record the pristine IRG for future inlining. */
  ir_graph *pristine = create_irg_copy(irg);

  /* Generate assembly */

  char filename_s[64];
  snprintf(filename_s, sizeof(filename_s), "jit-%s.s", entry->ldname);
  FILE *out = fopen(filename_s, "w");
  if(out == NULL) {
    perror("couldn't open assembly file for writing");
    exit(-1);
  }
  be_main(out, "cup");
  fclose(out);

  /* Assemble shared object */
  char filename_so[64];
  snprintf(filename_so, sizeof(filename_so), "./jit-%s.so", entry->ldname);

  {
    char command[128];
    snprintf(command, sizeof(command), LINK_COMMAND,
	     filename_so, filename_s);
    int rc = system(command);
    if (!WIFEXITED(rc) || WEXITSTATUS(rc)) {
      fprintf(stderr, "assembler/linker command failed: %s\n", command);
      exit(-1);
    }
  }

  /* dlopen() the shared object */
  void *dlhandle = dlopen(filename_so, RTLD_NOW|RTLD_GLOBAL);
  const char *err = dlerror();
  if(err)
    puts(err), exit(-1);
  entry->code = dlsym(dlhandle, entry->ldname);
  err = dlerror();
  if(err)
    puts(err), exit(-1);

  /* restore pristine IRG for future inlining */
  remove_irp_irg(irg);
  set_irg_entity(pristine, entry->entity);
  set_entity_irg(entry->entity, pristine);
  add_irp_irg(pristine);

  /* Avoid repeated code generation for the entity */
  foreach_irp_irg(i, old_irg) {
    ir_entity *ent = get_irg_entity(old_irg);
    set_entity_linkage(ent, IR_LINKAGE_NO_CODEGEN);
  }

  /* TODO: This includes static data, need to find a better way.
     Repeatedly emit global entites for every word for now.  */
/*   foreach_compound_member(i, mem, get_glob_type()) { */
/*     set_entity_linkage(mem, IR_LINKAGE_NO_CODEGEN); */
/*     set_entity_visibility(mem, ir_visibility_external); */
/*   } */

}

/* End compilation of a word */
WORD(semicolon)
{
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

  inline_functions(250 /* maxsize */,
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

  codegen(dictionary);

  /* Make sure there's no control flow still unwired. */
  assert (cs == control_stack);

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

WORD(key)
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

WORD(emit)
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

WORD(sp_load)
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

WORD(sp_store)
{
  sp = sp[-1].a;
  return --sp;
}

struct dict sp_store_entry =
{
  .name = "sp!",
  .code = sp_store,
  .next = &sp_load_entry,
  .ldname = "sp_store"
};

WORD(pick)
{
  sp[-1] = sp[-sp[-1].i - 2];
  return sp;
}

struct dict sp_pick_entry =
{
  .name = "pick",
  .code = pick,
  .next = &sp_store_entry,
};

WORD(cells)
{
  sp[-1].i = sizeof(union cell) * sp[-1].i;
  return sp;
}

struct dict cells_entry =
{
  .name = "cells",
  .code = cells,
  .next = &sp_pick_entry,
};

WORD(swap)
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

WORD(drop)
{
  return --sp;
}

struct dict drop_entry =
{
  .name = "drop",
  .code = drop,
  .next = &swap_entry,
};

WORD(equal)
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

WORD(negate)
{
  sp[-1].i = -sp[-1].i;
  return sp;
}

struct dict negate_entry =
{
  .name = "negate",
  .code = negate,
  .next = &equal_entry,
};

WORD(load)
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
WORD(store)
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
   matured.  The basic block projected from the true condition is made
   the current one.  The basic block for the false condition is pushed
   on the stack. */
WORD(w_if) /* C: -- bb_false */
{
  /* IR to load value at top of stack sp[-1] */
  ir_node *offset = new_Const_long(mode_Ls, -sizeof(union cell));
  ir_node *ir_sp = get_value(0, mode_P);
  ir_sp = new_Add(ir_sp, offset);
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

  mature_immBlock(get_cur_block());

  set_cur_block(block_true);

  /* Push false block on stack */
  *cs++ = block_false;
  return sp;
}

struct dict if_entry =
{
  .name = "if",
  .immediate = 1,
  .code = w_if,
  .ldname = "w_if",
  .next = &store_entry,
};

/* Mature the basic block under construction and switch construction
   to the basic block on the stack.  Push the basic block following
   the true/false blocks onto the stack. */
WORD(w_else) /* bb_false -- bb_then */
{
  ir_node *bb_false = *--cs;

  ir_node *jump = new_Jmp();
  ir_node *bb_then = new_immBlock();
  add_immBlock_pred(bb_then, jump);
  mature_immBlock(get_cur_block());
  set_cur_block(bb_false);

  *cs++ = bb_then;
  return sp;
}

struct dict else_entry =
{
  .name = "else",
  .immediate = 1,
  .code = w_else,
  .ldname = "w_else",
  .next = &if_entry,
};

/* Finalize current basic block as well as the condition block and
   make the one on the stack the current one. */
/* ( bb_then -- ) */
WORD(w_then)
{
  ir_node *bb_then = *--cs;
  ir_node *jump = new_Jmp();
  mature_immBlock(get_cur_block());
  add_immBlock_pred(bb_then, jump);

  set_cur_block(bb_then);
  return sp;
}

struct dict then_entry =
{
  .name = "then",
  .immediate = 1,
  .code = w_then,
  .ldname = "w_then",
  .next = &else_entry,
};

/* BEGIN loop construction */
/* -- bb_head */
WORD(begin)
{
  ir_node *jump = new_Jmp();
  mature_immBlock(get_cur_block());

  ir_node *bb_head = new_immBlock();
  *cs++ = bb_head;
  set_cur_block(bb_head);
  add_immBlock_pred(bb_head, jump);

  ir_node *body = new_immBlock();
  add_immBlock_pred(body, new_Jmp());
  set_cur_block(body);

  return sp;
}

struct dict begin_entry =
{
  .name = "begin",
  .immediate = 1,
  .code = begin,
  .next = &then_entry
};

/* Finalize unconditional loop construction */
/* ( bb_head -- ) */
WORD(again)
{
  ir_node *bb_head = *--cs;
  ir_node *jump = new_Jmp();
  add_immBlock_pred(bb_head, jump);

  /* force a PhiM to be created */
  get_store();
  /* connect loop with End node */
  keep_alive(bb_head);

  mature_immBlock(bb_head);

  set_cur_block(new_immBlock());
  return sp;
}

struct dict again_entry =
{
  .name = "again",
  .immediate = 1,
  .code = again,
  .next = &begin_entry
};

/* Finalize conditional loop construction */
/* ( bb_head bb_then -- ) */
WORD(repeat)
{
  ir_node *bb_then = *--cs;
  ir_node *jump = new_Jmp();
  mature_immBlock(get_cur_block());

  ir_node *bb_head = *--cs;
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
  .next = &again_entry
};

/* Finalize conditional loop construction */
/* ( bb_head -- ) */
WORD(until)
{
  sp = w_if(sp);
  /* (bb_head bb_false) */
  ir_node *bb_true = get_cur_block();

  ir_node *bb_false = *--cs;
  set_cur_block(bb_false);

  *cs++ = bb_true;
  /* (bb_head bb_true) */

  return sp = repeat(sp);
}

struct dict until_entry =
{
  .name = "until",
  .immediate = 1,
  .code = until,
  .next = &repeat_entry
};

WORD(compile_comma)
{
  struct dict *entry = sp[-1].a;
  assert(entry);
  ir_node *mem = get_store();

  ir_node *ptr;
  if (entry->entity) {
    ptr = new_Address(entry->entity);
  } else {
    assert(entry->code);
    ptr = new_Const_long(mode_P, (long)(entry->code));
  }

  ir_node *ir_sp = get_value(0, mode_P);
  ir_node *call = new_Call(mem, ptr, 1, &ir_sp, word_method_type);
  mem = new_Proj(call, mode_M, pn_Call_M);
  set_store(mem);
  ir_sp = new_Proj(call, mode_T, pn_Call_T_result);
  ir_sp = new_Proj(ir_sp, mode_P, 0);
  set_value(0, ir_sp);
  return --sp;
}

struct dict compile_comma_entry = {
     .name = "compile,",
     .code = compile_comma,
     .immediate = 1,
     .next = &until_entry,
     .ldname = "compile_comma"
};

WORD(w_word)
{
     sp->a = strdup(next()); /* TODO: fix memory leak */
     return ++sp;
}

struct dict word_entry = {
     .name = "word",
     .ldname = "w_word",
     .code = w_word,
     .next = &compile_comma_entry
};

WORD(find)
{
     struct dict *entry = dictionary;
     const char *token = sp[-1].a;
     while (entry && strcasecmp(entry->name, token)) {
	  entry = entry->next;
     }
     if (!entry) {
	  sp->i = 0;
     } else {
	  sp[-1].a = entry; /* TODO: fix memory leak */
	  if (entry->immediate)
	       sp->i = 1;
	  else
	       sp->i = -1;
     }
     return ++sp;
}

struct dict find_entry =
{
     .name = "find",
     .code = find,
     .next = &word_entry
};

WORD(immediate)
{
     dictionary->immediate = 1;
     return sp;
}

struct dict immediate_entry =
{
     .name = "immediate",
     .code = immediate,
     .next = &find_entry
};

WORD(tor)
{
     *rp++ = *--sp;
     return sp;
}

struct dict tor_entry =
{
     .name = ">r",
     .ldname = "tor",
     .code = tor,
     .next = &immediate_entry
};

WORD(rfrom)
{
     *sp++ = *--rp;
     return sp;
}

struct dict rfrom_entry =
{
     .name = "r>",
     .ldname = "rfrom",
     .code = rfrom,
     .next = &tor_entry
};

WORD(rload)
{
     *sp++ = rp[-1];
     return sp;
}

struct dict rload_entry =
{
     .name = "r@",
     .ldname = "rload",
     .code = rload,
     .next = &rfrom_entry
};

/* Compile code to put a Const on the stack */
WORD(literal)
{
     ir_node *ir_sp = get_value(0, mode_P);
     ir_node *constnode = new_Const_long(mode_Lu, sp[-1].u);
     sp--;
     ir_node *store = new_Store(get_store(), ir_sp, constnode, type_cell, 0);
     ir_node *mem = new_Proj(store, mode_M, pn_Store_M);
     set_store(mem);
     ir_node *add = new_Add(ir_sp, new_Const_long(mode_Ls, sizeof(union cell)));
     set_value(0, add);
     return sp;
}

struct dict literal_entry = {
     .name = "literal",
     .code = literal,
     .immediate = 1,
     .next = &rload_entry
};

WORD(w_brkleft)
{
     compiling = 0;
     return sp;
}

struct dict w_brkleft_entry = {
     .name = "[",
     .ldname = "w_brkleft",
     .code = w_brkleft,
     .immediate = 1,
     .next = &literal_entry,
};

WORD(w_brkright)
{
     compiling = 1;
     return sp;
}

struct dict w_brkright_entry = {
     .name = "]",
     .ldname = "w_brkright",
     .code = w_brkright,
     .next = &w_brkleft_entry,
};

/* ( u -- a-addr ior ) */
WORD(allocate)
{
     size_t sz = sp[-1].u;
     sp[-1].a = malloc(sz);
     assert(sp[-1].a);
     sp[0].a = 0;
     return ++sp;
};

struct dict allocate_entry = {
     .name = "allocate",
     .code = allocate,
     .next = &w_brkright_entry
};

WORD(rot)
{
     cell tmp = sp[-1];
     sp[-1] = sp[-3];
     sp[-3] = sp[-2];
     sp[-2] = tmp;
     return sp;
}

struct dict rot_entry = {
     .name = "rot",
     .code = rot,
     .next = &allocate_entry
};

WORD(mod)
{
     sp[-2].i = sp[-2].i % sp[-1].i;
     return --sp;
};

struct dict mod_entry = {
     .name = "mod",
     .code = rot,
     .next = &rot_entry
};

WORD(depth)
{
     sp->u = (sp - data_stack);
     return ++sp;
};

struct dict depth_entry = {
     .name = "depth",
     .code = depth,
     .next = &mod_entry
};

struct dict *dictionary = &depth_entry;

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
  ir_target_set("amd64");
  assert(1 == ir_target_option("pic=1"));
  ir_target_init();

  /* create types */
  word_method_type = new_type_method(1, 1, false, 0, 0);
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
WORD(interpret)
{
  struct dict *entry = dictionary;
  const char *token;
 retry:
  fflush(stdout);
  token = next();

  /* Skip comments */
  if (!strcmp(token, "(")) {
    do {
      token = next();
    } while (strcmp(token, ")"));
    goto retry;
  }

  /* Search dictionary */
  while (entry && strcasecmp(entry->name, token)) {
    entry = entry->next;
  }

  if (entry) {
    /* Word found in dictionary, execute or compile a call to it */
    ASSERT_STACK();
    if (compiling && !entry->immediate) {
	 sp->a = entry;
	 ++sp;
         sp = compile_comma(sp);
    } else {
      if (!compiling && entry->immediate) {
	fprintf(stderr, "ERROR: immediate words are only allowed in compilation mode\n");
	return sp;
      }
      if (!entry->code)
	codegen(entry);
      sp = entry->code(sp);
    }
    ASSERT_STACK();
  } else if ((token[0] >= '0' && token[0] <= '9') || token[0] == '-') {
    /* Integer found, put it on the stack */
     sp->i = atoll(token);
     sp++;
     if (compiling)
       sp = literal(sp);
  } else {
     fprintf(stderr, "ERROR: unknown word: %s\n", token);
  }
  return sp;
}

/* Define a word for a Firm binary operator */
cell* init_binop(cell *sp, const char *name, ir_node *(*constructor)(ir_node *, ir_node *) )
{
  /* create dictionary entry */
  struct dict *entry = malloc(sizeof(struct dict));
  entry->name = name;
  entry->code = 0;
  entry->immediate = 0;
  entry->next = dictionary;
  dictionary = entry;

  ident *id;
  {
    char *mangled = mangle("word_", name, "");
    id = id_unique(mangled);
    free(mangled);
  }

  /* Create Firm entity for word */
  ir_type *global_type = get_glob_type();
  entry->entity = new_entity(global_type, id, word_method_type);
  set_entity_ld_ident(entry->entity, id);

  entry->ldname = get_id_str(id);

  /* Create IR graph */
  ir_graph *irg = new_ir_graph(entry->entity, 1);
  /* Sets graph which is currently constructed. */
  set_current_ir_graph(irg);

  ir_node *proj_arg_t = new_Proj(get_irg_start(irg), mode_T, pn_Start_T_args);

  /* IR to load value at top-of-stack (TOS) sp[-1] */
  ir_node *cell_size = new_Const_long(mode_Ls, sizeof(union cell));
  ir_node *ir_sp = new_Proj(proj_arg_t, mode_P, 0);
  ir_node *ir_sp_TOS = new_Sub(ir_sp, cell_size);
  ir_node *load_data = new_Load(get_store(), ir_sp_TOS, mode_Lu, type_cell, 0);
  ir_node *load_data_res_TOS = new_Proj(load_data, mode_Lu, pn_Load_res);
  ir_node *load_data_mem_TOS = new_Proj(load_data, mode_M, pn_Load_M);

  /* IR to load value at next-on-stack (NOS) sp[-2] */
  ir_node *ir_sp_NOS = new_Sub(ir_sp_TOS, cell_size);
  load_data = new_Load(load_data_mem_TOS, ir_sp_NOS, mode_Lu, type_cell, 0);
  ir_node *load_data_res_NOS = new_Proj(load_data, mode_Lu, pn_Load_res);
  ir_node *load_data_mem_NOS = new_Proj(load_data, mode_M, pn_Load_M);
  set_store(load_data_mem_NOS);

  ir_node *binop = constructor(load_data_res_NOS, load_data_res_TOS);

  ir_node *store = new_Store(get_store(), ir_sp_NOS, binop, type_cell, 0);
  ir_node* store_m = new_Proj(store, mode_M, pn_Store_M);
  set_store(store_m);
  set_value(0, ir_sp_TOS);

  return semicolon(sp);
}

int main()
{
  initialize_firm();
  cell *sp = data_stack;
  sp = init_binop(sp, "lshift", new_Shl);
  sp = init_binop(sp, "rshift", new_Shr);
  sp = init_binop(sp, "or", new_Or);
  sp = init_binop(sp, "and", new_And);
  sp = init_binop(sp, "xor", new_Eor);
  sp = init_binop(sp, "*", new_Mul);
  assert(sp == data_stack);
  while(1)
    sp = interpret(sp);
}
