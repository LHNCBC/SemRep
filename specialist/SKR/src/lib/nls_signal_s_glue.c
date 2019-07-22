#define SP_IN_nls_0x5Fsignal_glue_c 1
#define SPAUX_H_DO_NOT_DECLARE_sp_GlobalSICStus 1
#ifndef SP_RESNAME
# define SP_RESNAME nls_0x5Fsignal
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
 /* sp_GlobalSICStus_nls_0x5Fsignal already declared */
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
# define SP_CONTEXT_SWITCH_HOOK sp_context_switch_hook_nls_0x5Fsignal
#endif
SP_BEGIN_DECL
 extern void SPCDECL SP_CONTEXT_SWITCH_HOOK (int);
SP_END_DECL
SP_BEGIN_DECL
#ifdef SP_MANGLE
#define C_establish_signal_handling SP_MANGLE(C_establish_signal_handling)
#endif
extern SP_integer SPCDECL C_establish_signal_handling PROTOTYPE(( SPAPI_ARG_PROTO_DECL0 ));
SP_END_DECL
static SP_mutex sp_resource_mutex_nls_0x5Fsignal=SP_MUTEX_INITIALIZER;
#ifndef SPFUNCVARS
#define SPFUNCVARS 1
#include <stdio.h>
#include <sicstus/spaux.c>
#endif
SP_BEGIN_DECL
extern SPGLUEEXP SP_MainFun sp_main_SPENV_nls_0x5Fsignal;
SP_END_DECL
static const char *nls_0x5Fsignal_prednames[] = {
"C_establish_signal_handling",
0};
static int nls_0x5Fsignal_arities[] = {
1,
-1};
#if !SP_FLI_APPLY_ASM_GENERIC
static SP_GlueFun *nls_0x5Fsignal_funcs[] = {
(SP_GlueFun *)C_establish_signal_handling,
0};
#endif /* !SP_FLI_APPLY_ASM_GENERIC */
#if SP_FLI_APPLY_ASM_GENERIC
static void SPCDECL glue_C_establish_signal_handling(sp_t_fli_call *a)
{
   (void) a; /* Avoid unused argument warnings. */
a->integer_return = (SP_integer) /* [-integer] */
C_establish_signal_handling(
#if MULTI_SP_AWARE
   (SPEnv*)a->stash_arg /* Cast should not be needed */
#endif /* MULTI_SP_AWARE */
);
}
static SP_GenericGlueFun *nls_0x5Fsignal_funcs[] = {
&glue_C_establish_signal_handling,
0};
#endif /* SP_FLI_APPLY_ASM_GENERIC */
SPGLUEEXP1 int SPGLUEEXP2 SPCDECL sp_main_SPENV_nls_0x5Fsignal(SP_MAINFUN_PARAMS *params)
{
#if MULTI_SP_AWARE
   SPAPI_ARG_LOCAL_DECL
#endif
   return sp_main_helper(params, 0x30001, GetSICStusDISPATCHAddress(), &sp_resource_mutex_nls_0x5Fsignal, nls_0x5Fsignal_funcs, nls_0x5Fsignal_prednames, nls_0x5Fsignal_arities, NULL, NULL);
}
