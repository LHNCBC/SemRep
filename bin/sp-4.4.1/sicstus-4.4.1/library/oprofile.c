#define SICSTUS_UNHIDE_SP_install_spti_hook 1

#include "oprofile_glue.h"
#include "oprofile_common.c"

void SPCDECL oprofile_init(SPAPI_ARG_PROTO_DECL int when)
{
  (void)when;
  register_spti(&SP_install_spti_hook);
}

void SPCDECL oprofile_deinit(SPAPI_ARG_PROTO_DECL int when)
{
  (void)when;
  unregister_spti(&SP_install_spti_hook);
}
