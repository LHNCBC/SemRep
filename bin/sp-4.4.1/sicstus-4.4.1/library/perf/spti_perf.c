
#include "spti_perf_glue.h"
#include "perf_common.c"

extern SPDLL_DECLARE_EXPORTED SP_spti_hook SPTI_OnLoad;

spio_t_error_code SPCDECL SPDLL_DEFINE_EXPORTED SPTI_OnLoad(sp_t_spti_event *event, void *reserved)
{
  (void)event; (void)reserved;
#if VERBOSE_SPTI
  {
    fprintf(stderr, "SPTI_OnLoad() PERF\n");
    fflush(stderr);
  }
#endif  /* VERBOSE_SPTI */

  register_spti(event->spenv->dispatch_API_SICSTUS_H->pSP_install_spti_hook);
  return SPIO_S_NOERR;
}

extern SPDLL_DECLARE_EXPORTED SP_spti_hook SPTI_OnUnLoad;

spio_t_error_code SPCDECL SPDLL_DEFINE_EXPORTED SPTI_OnUnLoad(sp_t_spti_event *event, void *reserved)
{
  (void)event; (void)reserved;
#if VERBOSE_SPTI
  {
    fprintf(stderr, "SPTI_OnUnLoad() PERF\n");
    fflush(stderr);
  }
#endif  /* VERBOSE_SPTI */
  unregister_spti(event->spenv->dispatch_API_SICSTUS_H->pSP_install_spti_hook);
  return SPIO_S_NOERR;
}
