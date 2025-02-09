#define SP_IN_qp_0x5Flexicon_glue_c 1
#define SPAUX_H_DO_NOT_DECLARE_sp_GlobalSICStus 1
#ifndef SP_RESNAME
# define SP_RESNAME qp_0x5Flexicon
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
 /* sp_GlobalSICStus_qp_0x5Flexicon already declared */
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
# define SP_CONTEXT_SWITCH_HOOK sp_context_switch_hook_qp_0x5Flexicon
#endif
SP_BEGIN_DECL
 extern void SPCDECL SP_CONTEXT_SWITCH_HOOK (int);
SP_END_DECL
SP_BEGIN_DECL
#ifdef SP_MANGLE
#define c_lex_cit SP_MANGLE(c_lex_cit)
#endif
extern SP_integer SPCDECL c_lex_cit PROTOTYPE(( SPAPI_ARG_PROTO_DECL
char SP_FLI_CONST *,
char SP_FLI_CONST *,
SP_integer,
SP_integer,
SP_integer,
SP_term_ref));
SP_END_DECL
SP_BEGIN_DECL
#ifdef SP_MANGLE
#define c_lex_form SP_MANGLE(c_lex_form)
#endif
extern SP_integer SPCDECL c_lex_form PROTOTYPE(( SPAPI_ARG_PROTO_DECL
char SP_FLI_CONST *,
char SP_FLI_CONST *,
SP_integer,
SP_integer,
SP_integer,
SP_term_ref));
SP_END_DECL
SP_BEGIN_DECL
#ifdef SP_MANGLE
#define c_lex_form SP_MANGLE(c_lex_form)
#endif
extern SP_integer SPCDECL c_lex_form PROTOTYPE(( SPAPI_ARG_PROTO_DECL
char SP_FLI_CONST *,
char SP_FLI_CONST *,
SP_integer,
SP_integer,
SP_integer,
SP_term_ref));
SP_END_DECL
SP_BEGIN_DECL
#ifdef SP_MANGLE
#define c_lex_form_cats SP_MANGLE(c_lex_form_cats)
#endif
extern SP_integer SPCDECL c_lex_form_cats PROTOTYPE(( SPAPI_ARG_PROTO_DECL
char SP_FLI_CONST *,
char SP_FLI_CONST *,
SP_integer,
SP_integer,
SP_term_ref));
SP_END_DECL
SP_BEGIN_DECL
#ifdef SP_MANGLE
#define c_lex_is_a_root SP_MANGLE(c_lex_is_a_root)
#endif
extern SP_integer SPCDECL c_lex_is_a_root PROTOTYPE(( SPAPI_ARG_PROTO_DECL
char SP_FLI_CONST *,
char SP_FLI_CONST *,
SP_integer,
SP_integer));
SP_END_DECL
SP_BEGIN_DECL
#ifdef SP_MANGLE
#define c_lex_is_a_form SP_MANGLE(c_lex_is_a_form)
#endif
extern SP_integer SPCDECL c_lex_is_a_form PROTOTYPE(( SPAPI_ARG_PROTO_DECL
char SP_FLI_CONST *,
char SP_FLI_CONST *,
SP_integer,
SP_integer));
SP_END_DECL
SP_BEGIN_DECL
#ifdef SP_MANGLE
#define c_lex_is_a_root_cats SP_MANGLE(c_lex_is_a_root_cats)
#endif
extern SP_integer SPCDECL c_lex_is_a_root_cats PROTOTYPE(( SPAPI_ARG_PROTO_DECL
char SP_FLI_CONST *,
char SP_FLI_CONST *,
SP_integer,
SP_integer,
SP_term_ref));
SP_END_DECL
SP_BEGIN_DECL
#ifdef SP_MANGLE
#define c_lex_form_input SP_MANGLE(c_lex_form_input)
#endif
extern SP_integer SPCDECL c_lex_form_input PROTOTYPE(( SPAPI_ARG_PROTO_DECL
char SP_FLI_CONST *,
SP_integer,
SP_term_ref,
SP_term_ref));
SP_END_DECL
SP_BEGIN_DECL
#ifdef SP_MANGLE
#define c_get_varlist SP_MANGLE(c_get_varlist)
#endif
extern SP_integer SPCDECL c_get_varlist PROTOTYPE(( SPAPI_ARG_PROTO_DECL
char SP_FLI_CONST *,
SP_integer,
SP_term_ref));
SP_END_DECL
static SP_mutex sp_resource_mutex_qp_0x5Flexicon=SP_MUTEX_INITIALIZER;
#ifndef SPFUNCVARS
#define SPFUNCVARS 1
#include <stdio.h>
#include <sicstus/spaux.c>
#endif
SP_BEGIN_DECL
extern SPGLUEEXP SP_MainFun sp_main_SPENV_qp_0x5Flexicon;
SP_END_DECL
static const char *qp_0x5Flexicon_prednames[] = {
"c_lex_cit",
"c_lex_form",
"c_lex_form",
"c_lex_form_cats",
"c_lex_is_a_root",
"c_lex_is_a_form",
"c_lex_is_a_root_cats",
"c_lex_form_input",
"c_get_varlist",
0};
static int qp_0x5Flexicon_arities[] = {
7,
7,
7,
6,
5,
5,
6,
5,
4,
-1};
#if !SP_FLI_APPLY_ASM_GENERIC
static SP_GlueFun *qp_0x5Flexicon_funcs[] = {
(SP_GlueFun *)c_lex_cit,
(SP_GlueFun *)c_lex_form,
(SP_GlueFun *)c_lex_form,
(SP_GlueFun *)c_lex_form_cats,
(SP_GlueFun *)c_lex_is_a_root,
(SP_GlueFun *)c_lex_is_a_form,
(SP_GlueFun *)c_lex_is_a_root_cats,
(SP_GlueFun *)c_lex_form_input,
(SP_GlueFun *)c_get_varlist,
0};
#endif /* !SP_FLI_APPLY_ASM_GENERIC */
#if SP_FLI_APPLY_ASM_GENERIC
static void SPCDECL glue_c_lex_cit(sp_t_fli_call *a)
{
   (void) a; /* Avoid unused argument warnings. */
a->integer_return = (SP_integer) /* [-integer] */
c_lex_cit(
#if MULTI_SP_AWARE
   (SPEnv*)a->stash_arg, /* Cast should not be needed */
#endif /* MULTI_SP_AWARE */
   (char SP_FLI_CONST *) a->integer_arg[0] /* +string */
  , (char SP_FLI_CONST *) a->integer_arg[1] /* +string */
  , (SP_integer) a->integer_arg[2] /* +integer */
  , (SP_integer) a->integer_arg[3] /* +integer */
  , (SP_integer) a->integer_arg[4] /* +integer */
  , (SP_term_ref) a->integer_arg[5] /* -term */
);
}
static void SPCDECL glue_c_lex_form(sp_t_fli_call *a)
{
   (void) a; /* Avoid unused argument warnings. */
a->integer_return = (SP_integer) /* [-integer] */
c_lex_form(
#if MULTI_SP_AWARE
   (SPEnv*)a->stash_arg, /* Cast should not be needed */
#endif /* MULTI_SP_AWARE */
   (char SP_FLI_CONST *) a->integer_arg[0] /* +string */
  , (char SP_FLI_CONST *) a->integer_arg[1] /* +string */
  , (SP_integer) a->integer_arg[2] /* +integer */
  , (SP_integer) a->integer_arg[3] /* +integer */
  , (SP_integer) a->integer_arg[4] /* +integer */
  , (SP_term_ref) a->integer_arg[5] /* -term */
);
}
static void SPCDECL glue_c_lex_form(sp_t_fli_call *a)
{
   (void) a; /* Avoid unused argument warnings. */
a->integer_return = (SP_integer) /* [-integer] */
c_lex_form(
#if MULTI_SP_AWARE
   (SPEnv*)a->stash_arg, /* Cast should not be needed */
#endif /* MULTI_SP_AWARE */
   (char SP_FLI_CONST *) a->integer_arg[0] /* +string */
  , (char SP_FLI_CONST *) a->integer_arg[1] /* +string */
  , (SP_integer) a->integer_arg[2] /* +integer */
  , (SP_integer) a->integer_arg[3] /* +integer */
  , (SP_integer) a->integer_arg[4] /* +integer */
  , (SP_term_ref) a->integer_arg[5] /* -term */
);
}
static void SPCDECL glue_c_lex_form_cats(sp_t_fli_call *a)
{
   (void) a; /* Avoid unused argument warnings. */
a->integer_return = (SP_integer) /* [-integer] */
c_lex_form_cats(
#if MULTI_SP_AWARE
   (SPEnv*)a->stash_arg, /* Cast should not be needed */
#endif /* MULTI_SP_AWARE */
   (char SP_FLI_CONST *) a->integer_arg[0] /* +string */
  , (char SP_FLI_CONST *) a->integer_arg[1] /* +string */
  , (SP_integer) a->integer_arg[2] /* +integer */
  , (SP_integer) a->integer_arg[3] /* +integer */
  , (SP_term_ref) a->integer_arg[4] /* -term */
);
}
static void SPCDECL glue_c_lex_is_a_root(sp_t_fli_call *a)
{
   (void) a; /* Avoid unused argument warnings. */
a->integer_return = (SP_integer) /* [-integer] */
c_lex_is_a_root(
#if MULTI_SP_AWARE
   (SPEnv*)a->stash_arg, /* Cast should not be needed */
#endif /* MULTI_SP_AWARE */
   (char SP_FLI_CONST *) a->integer_arg[0] /* +string */
  , (char SP_FLI_CONST *) a->integer_arg[1] /* +string */
  , (SP_integer) a->integer_arg[2] /* +integer */
  , (SP_integer) a->integer_arg[3] /* +integer */
);
}
static void SPCDECL glue_c_lex_is_a_form(sp_t_fli_call *a)
{
   (void) a; /* Avoid unused argument warnings. */
a->integer_return = (SP_integer) /* [-integer] */
c_lex_is_a_form(
#if MULTI_SP_AWARE
   (SPEnv*)a->stash_arg, /* Cast should not be needed */
#endif /* MULTI_SP_AWARE */
   (char SP_FLI_CONST *) a->integer_arg[0] /* +string */
  , (char SP_FLI_CONST *) a->integer_arg[1] /* +string */
  , (SP_integer) a->integer_arg[2] /* +integer */
  , (SP_integer) a->integer_arg[3] /* +integer */
);
}
static void SPCDECL glue_c_lex_is_a_root_cats(sp_t_fli_call *a)
{
   (void) a; /* Avoid unused argument warnings. */
a->integer_return = (SP_integer) /* [-integer] */
c_lex_is_a_root_cats(
#if MULTI_SP_AWARE
   (SPEnv*)a->stash_arg, /* Cast should not be needed */
#endif /* MULTI_SP_AWARE */
   (char SP_FLI_CONST *) a->integer_arg[0] /* +string */
  , (char SP_FLI_CONST *) a->integer_arg[1] /* +string */
  , (SP_integer) a->integer_arg[2] /* +integer */
  , (SP_integer) a->integer_arg[3] /* +integer */
  , (SP_term_ref) a->integer_arg[4] /* +term */
);
}
static void SPCDECL glue_c_lex_form_input(sp_t_fli_call *a)
{
   (void) a; /* Avoid unused argument warnings. */
a->integer_return = (SP_integer) /* [-integer] */
c_lex_form_input(
#if MULTI_SP_AWARE
   (SPEnv*)a->stash_arg, /* Cast should not be needed */
#endif /* MULTI_SP_AWARE */
   (char SP_FLI_CONST *) a->integer_arg[0] /* +string */
  , (SP_integer) a->integer_arg[1] /* +integer */
  , (SP_term_ref) a->integer_arg[2] /* +term */
  , (SP_term_ref) a->integer_arg[3] /* -term */
);
}
static void SPCDECL glue_c_get_varlist(sp_t_fli_call *a)
{
   (void) a; /* Avoid unused argument warnings. */
a->integer_return = (SP_integer) /* [-integer] */
c_get_varlist(
#if MULTI_SP_AWARE
   (SPEnv*)a->stash_arg, /* Cast should not be needed */
#endif /* MULTI_SP_AWARE */
   (char SP_FLI_CONST *) a->integer_arg[0] /* +string */
  , (SP_integer) a->integer_arg[1] /* +integer */
  , (SP_term_ref) a->integer_arg[2] /* -term */
);
}
static SP_GenericGlueFun *qp_0x5Flexicon_funcs[] = {
&glue_c_lex_cit,
&glue_c_lex_form,
&glue_c_lex_form,
&glue_c_lex_form_cats,
&glue_c_lex_is_a_root,
&glue_c_lex_is_a_form,
&glue_c_lex_is_a_root_cats,
&glue_c_lex_form_input,
&glue_c_get_varlist,
0};
#endif /* SP_FLI_APPLY_ASM_GENERIC */
SPGLUEEXP1 int SPGLUEEXP2 SPCDECL sp_main_SPENV_qp_0x5Flexicon(SP_MAINFUN_PARAMS *params)
{
#if MULTI_SP_AWARE
   SPAPI_ARG_LOCAL_DECL
#endif
   return sp_main_helper(params, 0x30001, GetSICStusDISPATCHAddress(), &sp_resource_mutex_qp_0x5Flexicon, qp_0x5Flexicon_funcs, qp_0x5Flexicon_prednames, qp_0x5Flexicon_arities, NULL, NULL);
}
