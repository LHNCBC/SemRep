#ifndef SP_FOREIGN_RESOURCE_db_0x5Faccess_H_INCLUDED
#define SP_FOREIGN_RESOURCE_db_0x5Faccess_H_INCLUDED 1
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
#endif /* SP_FOREIGN_RESOURCE_db_0x5Faccess_H_INCLUDED */
