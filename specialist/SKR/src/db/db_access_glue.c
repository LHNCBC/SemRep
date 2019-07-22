#define SP_IN_db_0x5Faccess_glue_c 1
#define SPAUX_H_DO_NOT_DECLARE_sp_GlobalSICStus 1
#ifndef SP_RESNAME
# define SP_RESNAME db_0x5Faccess
#endif
#if !SPDLL
# ifndef SP_STATIC_FOREIGN_RESOURCE
#  define SP_STATIC_FOREIGN_RESOURCE 1
# endif
#endif /* !SPDLL */
#include <sicstus/sicstus.h>
#include <stdlib.h>
#if ((SP_DYNAMIC_FOREIGN_RESOURCE || SPDLL) && !MULTI_SP_AWARE)
# ifndef sp_GlobalSICStus
#  if !SP_NO_MANGLE_sp_GlobalSICStus
#   define sp_GlobalSICStus SP_RESNAME_CATENATE(sp_GlobalSICStus,SP_RESNAME)
#   define sp_GlobalSICStus_MANGLED 1
#  endif
# endif /* !defined sp_GlobalSICStus */
#endif /* ((SP_DYNAMIC_FOREIGN_RESOURCE || SPDLL) && !MULTI_SP_AWARE) */
#if (sp_GlobalSICStus_MANGLED && sp_GlobalSICStus_MANGLED_declared)
 /* sp_GlobalSICStus_db_0x5Faccess already declared */
#elif (!sp_GlobalSICStus_MANGLED && sp_GlobalSICStus_declared)
 /* sp_GlobalSICStus (unmangled) already declared */
#else /* Need to declare it (sp_GlobalSICStus may be a mangled name) */
  SP_BEGIN_DECL
   extern SICSTUS_API_STRUCT_TYPE *sp_GlobalSICStus;
#  if sp_GlobalSICStus_MANGLED
#   define sp_GlobalSICStus_MANGLED_declared 1
#  else
#   define sp_GlobalSICStus_declared 1
#  endif
  SP_END_DECL
# endif
#ifndef SP_CONTEXT_SWITCH_HOOK
# define SP_CONTEXT_SWITCH_HOOK sp_context_switch_hook_db_0x5Faccess
#endif
SP_BEGIN_DECL
 extern void SPCDECL SP_CONTEXT_SWITCH_HOOK (int);
SP_END_DECL
SP_BEGIN_DECL
#ifdef SP_MANGLE
#define c_nls_db_exec_2_list_jgm SP_MANGLE(c_nls_db_exec_2_list_jgm)
#endif
extern SP_integer SPCDECL c_nls_db_exec_2_list_jgm PROTOTYPE(( SPAPI_ARG_PROTO_DECL
char SP_FLI_CONST *,
SP_term_ref));
SP_END_DECL
SP_BEGIN_DECL
#ifdef SP_MANGLE
#define exec_init_dbs SP_MANGLE(exec_init_dbs)
#endif
extern void SPCDECL exec_init_dbs PROTOTYPE(( SPAPI_ARG_PROTO_DECL
char SP_FLI_CONST *));
SP_END_DECL
SP_BEGIN_DECL
#ifdef SP_MANGLE
#define exec_destroy_dbs SP_MANGLE(exec_destroy_dbs)
#endif
extern void SPCDECL exec_destroy_dbs PROTOTYPE(( SPAPI_ARG_PROTO_DECL0 ));
SP_END_DECL
static SP_mutex sp_resource_mutex_db_0x5Faccess=SP_MUTEX_INITIALIZER;
#ifndef SPFUNCVARS
#define SPFUNCVARS 1
#include <stdio.h>
#include <sicstus/spaux.c>
#endif
SP_BEGIN_DECL
extern SPGLUEEXP SP_MainFun sp_main_SPENV_db_0x5Faccess;
SP_END_DECL
static const char *db_0x5Faccess_prednames[] = {
"c_nls_db_exec_2_list_jgm",
"exec_init_dbs",
"exec_destroy_dbs",
0};
static int db_0x5Faccess_arities[] = {
3,
1,
0,
-1};
#if !SP_FLI_APPLY_ASM_GENERIC
static SP_GlueFun *db_0x5Faccess_funcs[] = {
(SP_GlueFun *)c_nls_db_exec_2_list_jgm,
(SP_GlueFun *)exec_init_dbs,
(SP_GlueFun *)exec_destroy_dbs,
0};
#endif /* !SP_FLI_APPLY_ASM_GENERIC */
#if SP_FLI_APPLY_ASM_GENERIC
static void SPCDECL glue_c_nls_db_exec_2_list_jgm(sp_t_fli_call *a)
{
   (void) a; /* Avoid unused argument warnings. */
a->integer_return = (SP_integer) /* [-integer] */
c_nls_db_exec_2_list_jgm(
#if MULTI_SP_AWARE
   (SPEnv*)a->stash_arg, /* Cast should not be needed */
#endif /* MULTI_SP_AWARE */
   (char SP_FLI_CONST *) a->integer_arg[0] /* +string */
  , (SP_term_ref) a->integer_arg[1] /* -term */
);
}
static void SPCDECL glue_exec_init_dbs(sp_t_fli_call *a)
{
   (void) a; /* Avoid unused argument warnings. */

exec_init_dbs(
#if MULTI_SP_AWARE
   (SPEnv*)a->stash_arg, /* Cast should not be needed */
#endif /* MULTI_SP_AWARE */
   (char SP_FLI_CONST *) a->integer_arg[0] /* +string */
);
}
static void SPCDECL glue_exec_destroy_dbs(sp_t_fli_call *a)
{
   (void) a; /* Avoid unused argument warnings. */

exec_destroy_dbs(
#if MULTI_SP_AWARE
   (SPEnv*)a->stash_arg /* Cast should not be needed */
#endif /* MULTI_SP_AWARE */
);
}
static SP_GenericGlueFun *db_0x5Faccess_funcs[] = {
&glue_c_nls_db_exec_2_list_jgm,
&glue_exec_init_dbs,
&glue_exec_destroy_dbs,
0};
#endif /* SP_FLI_APPLY_ASM_GENERIC */
SPGLUEEXP1 int SPGLUEEXP2 SPCDECL sp_main_SPENV_db_0x5Faccess(SP_MAINFUN_PARAMS *params)
{
#if MULTI_SP_AWARE
   SPAPI_ARG_LOCAL_DECL
#endif
   return sp_main_helper(params, 0x30001, GetSICStusDISPATCHAddress(), &sp_resource_mutex_db_0x5Faccess, db_0x5Faccess_funcs, db_0x5Faccess_prednames, db_0x5Faccess_arities, NULL, NULL);
}
