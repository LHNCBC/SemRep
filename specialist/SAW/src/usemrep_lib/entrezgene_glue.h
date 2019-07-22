#ifndef SP_FOREIGN_RESOURCE_entrezgene_H_INCLUDED
#define SP_FOREIGN_RESOURCE_entrezgene_H_INCLUDED 1
#ifndef SP_RESNAME
# define SP_RESNAME entrezgene
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
 /* sp_GlobalSICStus_entrezgene already declared */
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
# define SP_CONTEXT_SWITCH_HOOK sp_context_switch_hook_entrezgene
#endif
SP_BEGIN_DECL
 extern void SPCDECL SP_CONTEXT_SWITCH_HOOK (int);
SP_END_DECL
SP_BEGIN_DECL
#ifdef SP_MANGLE
#define c_retrieve_gene SP_MANGLE(c_retrieve_gene)
#endif
extern void SPCDECL c_retrieve_gene PROTOTYPE(( SPAPI_ARG_PROTO_DECL
char SP_FLI_CONST *,
char SP_FLI_CONST **));
SP_END_DECL
SP_BEGIN_DECL
#ifdef SP_MANGLE
#define c_init_dbs SP_MANGLE(c_init_dbs)
#endif
extern void SPCDECL c_init_dbs PROTOTYPE(( SPAPI_ARG_PROTO_DECL0 ));
SP_END_DECL
SP_BEGIN_DECL
#ifdef SP_MANGLE
#define c_close_dbs SP_MANGLE(c_close_dbs)
#endif
extern void SPCDECL c_close_dbs PROTOTYPE(( SPAPI_ARG_PROTO_DECL0 ));
SP_END_DECL
#endif /* SP_FOREIGN_RESOURCE_entrezgene_H_INCLUDED */
