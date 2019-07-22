/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#if DBG>1
#include "dvars.h"              /* dvar_validate */
#endif

TAGGED fd_attribute[] = {
/*  0 */  functor_v4,
	   TaggedOne,
           TaggedZero,
	   TagOffset(STRUCT_TAG,5),
	   TagOffset(STRUCT_TAG,8),
/*  5 */  functor_Dmutable,
	   TagOffset(STRUCT_TAG,11),
	   TaggedZero,
/*  8 */  functor_Dmutable,
	   TagOffset(STRUCT_TAG,16),
	   TaggedZero,
/* 11 */  0, /*functor_dom4,*/
	   TagOffset(LIST_TAG,39),
	   0,			/* min = inf or 0 */
	   0,			/* max = sup or 1 */
	   0,			/* size = sup or 2 */
/* 16 */  0, /*functor_fdlists22,*/
	   TaggedZero,
	   TaggedZero,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
/* 39 */   TagOffset(LIST_TAG,13),
	   atom_nil
};

/* Initialization etc.  Must be last.  */

static void fd_define_predicates(Wam wam, int install)
{
  /* Hooks for abolish, gc, save & restore */
  {
    int res;
    
    res  = SP_install_fd_hooks(wam,
			       fd_manager_hook,
			       fd_destructor_hook, 
			       install);
#if DBG
    fd.generation = res;
#if DBG>1
    fprintf(stderr, "%s: generation==%d (%s)\n", __FILE__, (int)fd.generation, ( install ? "install" : "uninstall"));fflush(stderr);
#endif
#else
    (void)res;
#endif
  }

}



struct pred_spec {
  char *name;
  int arity;
  void (SPCDECL *function)(Wam wam, SP_term_ref State, SP_term_ref NewState, SP_term_ref Actions);
};

static struct pred_spec pred_table[] = {
  {"ax=y",3,SP_MANGLE(prolog_fd_int_times)},
  {"x*x=y",2,SP_MANGLE(prolog_fd_square)},
  {"x*y=z",3,SP_MANGLE(prolog_fd_product)},
  {"x/y=z",3,SP_MANGLE(prolog_fd_quotient)},
  {"x div y=z",3,SP_MANGLE(prolog_fd_divide)},
  {"x mod y=z",3,SP_MANGLE(prolog_fd_modulo)},
  {"x rem y=z",3,SP_MANGLE(prolog_fd_remainder)},
  {"scalar_product",4,SP_MANGLE(prolog_fd_linear)},
  {"gcd_aux",3,SP_MANGLE(prolog_fd_gcd_aux)},
  {"all_different",1,SP_MANGLE(prolog_fd_all_different)},
  {"all_distinct",1,SP_MANGLE(prolog_fd_all_distinct)},
  {"bc_alldiff",1,SP_MANGLE(prolog_fd_bc_alldiff)},
  {"bc_alldiff_lia",1,SP_MANGLE(prolog_fd_bc_alldiff_lia)},
  {"atleast_le",5,SP_MANGLE(prolog_fd_atleast_le)},
  {"pairing",2,SP_MANGLE(prolog_fd_pairing)},
  {"sorting",3,SP_MANGLE(prolog_fd_sorting)},
  {"keysorting",3,SP_MANGLE(prolog_fd_keysorting)},
  {"assignment",3,SP_MANGLE(prolog_fd_assignment)},
  {"assignment_helper",2,SP_MANGLE(prolog_fd_assignment_helper)},
  {"circuit",1,SP_MANGLE(prolog_fd_circuit)},
  {"element",3,SP_MANGLE(prolog_fd_element)},
  {"cumulative",2,SP_MANGLE(prolog_fd_cumulative)},
  {"ttef_cumulative",2,SP_MANGLE(prolog_fd_ttef_cumulative)},
  {"multi_cumulative",3,SP_MANGLE(prolog_fd_multi_cumulative)},
  {"bin_packing",2,SP_MANGLE(prolog_fd_bin_packing)},
  {"compact_table",2,SP_MANGLE(prolog_fd_compact_table)},
  {"disjoint1",2,SP_MANGLE(prolog_fd_disjoint1)},
  {"disjoint2",2,SP_MANGLE(prolog_fd_disjoint2)},
  {"cumulatives",3,SP_MANGLE(prolog_fd_cumulatives)},
  {"global_cardinality",3,SP_MANGLE(prolog_fd_gcc)},
  {"global_cardinality_helper",2,SP_MANGLE(prolog_fd_gcc_helper)},
  {"local_cardinality",0,SP_MANGLE(prolog_fd_lcc)},
  {"lex_chain",2,SP_MANGLE(prolog_fd_lex_chain)},
  {"minimum",2,SP_MANGLE(prolog_fd_minmax)},
  {"maximum",2,SP_MANGLE(prolog_fd_minmax)},
  {"nvalue",2,SP_MANGLE(prolog_fd_nvalue)},
  {"geost",4,SP_MANGLE(prolog_fd_geost)},
  {"mddi",4,SP_MANGLE(prolog_fd_mddi)},
  {"bool_and",2,SP_MANGLE(prolog_fd_bool_or)},
  {"bool_or",2,SP_MANGLE(prolog_fd_bool_or)},
  {"bool_xor",2,SP_MANGLE(prolog_fd_bool_xor)},
  {"bool_channel",4,SP_MANGLE(prolog_fd_bool_channel)},
  {"ac3element",2,SP_MANGLE(prolog_fd_ac3element)},
  {"ac3intervals",2,SP_MANGLE(prolog_fd_ac3intervals)},
  {NULL,0,NULL}
};

/* Initialization upon load_foreign_resource, save, or restore. */
void SPCDECL fd_initialize(Wam wam, int when)
{
  TAGGED *table;
  struct pred_spec *pt;

  (void)when;
  (*SP_foreign_stash()) = fd_malloc(wam, sizeof(struct fd_state));

  fd.batching = 0;
  fd.hiding = 0;
  fd.debugging = 0;
  fd.overflowing = 1;
  fd.resumptions = 0;
  fd.entailments = 0;
  fd.prunings = 0;
  fd.failures = 0;
  fd.constraints = 0;
  fd.current_propagator = NULL;
  fd.free_propagators = NULL;
  fd_attribute[FD_ATTR_MIN_OFFSET-2] = functor_dom4;
  fd.functor_in_set2 = SetArity(SP_atom_from_string("in_set"),2);
  (void)SP_register_atom(fd.functor_in_set2);
  fd.functor_min     = SetArity(SP_atom_from_string("min"),1);
  (void)SP_register_atom(fd.functor_min);
  fd.functor_max     = SetArity(SP_atom_from_string("max"),1);
  (void)SP_register_atom(fd.functor_max);
  fd.functor_minmax  = SetArity(SP_atom_from_string("minmax"),1);
  (void)SP_register_atom(fd.functor_minmax);
  fd.functor_val     = SetArity(SP_atom_from_string("val"),1);
  (void)SP_register_atom(fd.functor_val);
  fd.functor_none     = SetArity(SP_atom_from_string("none"),1);
  (void)SP_register_atom(fd.functor_none);
  fd.functor_call    = SetArity(SP_atom_from_string("call"),1);
  (void)SP_register_atom(fd.functor_call);
  fd.functor_eq      = SetArity(SP_atom_from_string("="),2);
  (void)SP_register_atom(fd.functor_eq);
  fd.functor_t_eq_u      = SetArity(SP_atom_from_string("t=u IND"),2);
  (void)SP_register_atom(fd.functor_t_eq_u);
  fd.functor_fdlists22  = fd_attribute[FD_ATTR_MIN_OFFSET+3]
    = SetArity(SP_atom_from_string("$fdlists"),22);
  (void)SP_register_atom(fd.functor_fdlists22);
  fd.functor_leqc  = SetArity(SP_atom_from_string("t=<u+c"),3);
  (void)SP_register_atom(fd.functor_leqc);
  fd.functor_alldiff  = SetArity(SP_atom_from_string("all_distinct"),1);
  (void)SP_register_atom(fd.functor_alldiff);
  fd.functor_colored  = SetArity(SP_atom_from_string("colored"),1);
  (void)SP_register_atom(fd.functor_colored);
  table = fd.linkage_keys;
  table[0] = fd.functor_val;
  table[1] = functor_dom1;
  table[2] = fd.functor_min;
  table[3] = functor_dom1;
  table[4] = fd.functor_max;
  table[5] = functor_dom1;
  table[6] = fd.functor_minmax;
  table[7] = functor_dom1;
  table = fd.var_options;
  *table++ = SetArity(fd.functor_min,0);
  *table++ = SetArity(fd.functor_max,0);
  *table = SP_atom_from_string("ff");
  (void)SP_register_atom(*table++);
  *table = SP_atom_from_string("ffc");
  (void)SP_register_atom(*table++);
  *table = SP_atom_from_string("anti_first_fail");
  (void)SP_register_atom(*table++);
  *table = SP_atom_from_string("occurrence");
  (void)SP_register_atom(*table++);
  *table = SP_atom_from_string("max_regret");
  (void)SP_register_atom(*table++);
  fd.token_a         = SetArity(SP_atom_from_string("a"),1);
  (void)SP_register_atom(fd.token_a);
  fd.token_d         = SetArity(SP_atom_from_string("d"),1);
  (void)SP_register_atom(fd.token_d);
  fd.token_h         = SetArity(SP_atom_from_string("h"),1);
  (void)SP_register_atom(fd.token_h);
  fd.token_l         = SetArity(SP_atom_from_string("l"),1);
  (void)SP_register_atom(fd.token_l);
  fd.token_t         = SetArity(SP_atom_from_string("t"),1);
  (void)SP_register_atom(fd.token_t);
  fd.fd_module = find_module(SP_atom_from_string("clpfd"),TRUE);
  fd_define_predicates(wam, 1);

  fd.overflow_action2 = SP_predicate("overflow_action", 2, "clpfd");
  /* [PM] 4.4.0 happened from Distrib/buildbinaries.sh when clpfd.pl
     was newer than the *.po files (incorrect time on CentOS 6.5
     machine). Reproduce with touch library/clpfd.pl before running
     Suite/fdbasic. Fixed by defining overflow_action/2 before loading
     the clpfd foreign resource.
     
     Unfortunately we have no way to signal an error from a FLI init
     procedure.
  */
  SP_SOFT_ASSERT(fd.overflow_action2 != NULL);
  
  fd.dispatch = new_switch_on_key(32,NULL);
  (void)prolog_fd_evaluate_indexical(wam, 0);
  for (pt = pred_table; pt->name != NULL; pt++) {
    TAGGED key = SetArity(SP_atom_from_string(pt->name),pt->arity);
    
    SP_register_atom(key);
    dyn_puthash(&fd.dispatch,key)->value.arities = (SP_uinteger)pt->function;
  }
#if DBG>1
  dvar_validate(wam, );
#endif
}

/* Deinitialization upon unload_foreign_resource or before save/restore. */
void SPCDECL fd_deinitialize(Wam wam, int when)
{
  struct pred_spec *pt;
  
  (void)when;
  for (pt = pred_table; pt->name != NULL; pt++) {
    TAGGED key = SetArity(SP_atom_from_string(pt->name),pt->arity);
    
    SP_unregister_atom(key);
  }
  dispose_switch_on_key(fd.dispatch);
  (void)SP_unregister_atom(fd.functor_in_set2);
  (void)SP_unregister_atom(fd.functor_min);
  (void)SP_unregister_atom(fd.functor_max);
  (void)SP_unregister_atom(fd.functor_minmax);
  (void)SP_unregister_atom(fd.functor_val);
  (void)SP_unregister_atom(fd.functor_none);
  (void)SP_unregister_atom(fd.functor_call);
  (void)SP_unregister_atom(fd.functor_eq);
  (void)SP_unregister_atom(fd.functor_t_eq_u);
  (void)SP_unregister_atom(fd.functor_fdlists22);
  (void)SP_unregister_atom(fd.functor_leqc);
  (void)SP_unregister_atom(fd.functor_alldiff);
  (void)SP_unregister_atom(fd.functor_colored);
  (void)SP_unregister_atom(fd.token_a);
  (void)SP_unregister_atom(fd.token_d);
  (void)SP_unregister_atom(fd.token_h);
  (void)SP_unregister_atom(fd.token_l);
  (void)SP_unregister_atom(fd.token_t);
  fd_define_predicates(wam, 0);
  fd_dealloc(wam);
  
  {
    void *p = *SP_foreign_stash();
    *SP_foreign_stash()=NULL;
    SP_free(p);
  }
}

