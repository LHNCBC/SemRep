#define SICSTUS_UNHIDE_SP_install_spti_hook 1

#include "perf_glue.h"
#include "perf_common.c"

void SPCDECL perf_init(SPAPI_ARG_PROTO_DECL int when)
{
  (void)when;
  register_spti(&SP_install_spti_hook);
}

void SPCDECL perf_deinit(SPAPI_ARG_PROTO_DECL int when)
{
  (void)when;
  unregister_spti(&SP_install_spti_hook);
}
