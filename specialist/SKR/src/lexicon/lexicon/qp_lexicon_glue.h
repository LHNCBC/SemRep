#ifndef SP_FOREIGN_RESOURCE_qp_0x5Flexicon_H_INCLUDED
#define SP_FOREIGN_RESOURCE_qp_0x5Flexicon_H_INCLUDED 1
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
#endif /* SP_FOREIGN_RESOURCE_qp_0x5Flexicon_H_INCLUDED */
