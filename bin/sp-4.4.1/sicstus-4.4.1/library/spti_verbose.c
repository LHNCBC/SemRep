/* -*- Mode:C -*- */
/* A SPTI module that does nothing except write SPTI events to stderr. */
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include <sicstus/sicstus.h>
#include <sicstus/config.h>

#define BARF(CODE) do { code = (CODE); goto barf; } while(0)
#define CHECK(CODE) do { code = (CODE); if (SPIO_FAILED(code)) { spio_t_error_code tmp_code = code; BARF(tmp_code); } } while(0)

typedef struct spti_t_cookie_ spti_t_cookie;
struct spti_t_cookie_ {
  int inited;
};

#ifndef PREFIX
#define PREFIX "% SPTI: "
#endif  /* !PREFIX */
#ifndef ERROR_PREFIX
#define ERROR_PREFIX "! SPTI: "
#endif  /* !ERROR_PREFIX */

#define JIT_DEFINED_OPTION_DEFINED   0x00001
#define JIT_DEFINED_OPTION_UNDEFINED 0x00002
static void jit_defined(char const* name, SP_integer arity, char const *module, void const *addr, size_t size, spti_t_cookie *cookie, spio_t_bits options)
{
  int const defined = SPIO_MASK_IS_SET(options, JIT_DEFINED_OPTION_DEFINED);
  (void)defined;
  (void)name; (void)arity; (void)module; (void)addr; (void)size; (void)cookie;

  fprintf(stderr, "%s%s %s:%s/%d @[%p,%p] (%" SPRIdSZ " bytes)\n",
          PREFIX,
          (defined ? "JIT-ed" : "UnJIT-ed"),
          module, name, (int)(arity & 0xFF),
          (void*)addr, (void*)(((char*)addr)+size),  (size_t)size);
  fflush(stderr);
}

#define JIT_START_OPTION_START   0x00001
#define JIT_START_OPTION_END     0x00002
static void jit_start(char const* name, SP_integer arity, char const *module, spti_t_cookie *cookie, spio_t_bits options)
{
  int const start = SPIO_MASK_IS_SET(options, JIT_START_OPTION_START);
  (void)start;
  (void)name; (void)arity; (void)module; (void)cookie;

  fprintf(stderr, "%s%s %s:%s/%d\n",
          PREFIX,
          (start ? "BEGIN" : "END"),
          module, name, (int)(arity & 0xFF));
  fflush(stderr);
}

static spio_t_error_code SPCDECL spti_hook(sp_t_spti_event *event, void *cookie_)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  spti_t_cookie *cookie = (spti_t_cookie *)cookie_;
  SPIO_REFERENCE_BARF_LABEL();
  (void)event;

#if VERBOSE_SPTI
  {
    fprintf(stderr, "%sEnter spti_hook (\n", PREFIX);
    fflush(stderr);
  }
#endif  /* VERBOSE_SPTI */

  if (event->size != sizeof *event)
    {
      BARF(SPIO_E_PARAMETER_ERROR);
    }
  if (! (event->flags & SPTI_EVENT_FLAG_INITED))
    {
      BARF(SPIO_E_INVALID_STATE);
    }
  if (cookie == NULL)
    {
      BARF(SPIO_E_INVALID_STATE);
    }

  switch (event->request)
    {
    case SPTI_REQUEST_JIT_START:
    case SPTI_REQUEST_JIT_END:
      {
        int const start = (event->request == SPTI_REQUEST_JIT_START);
        spio_t_bits options;
        if (start) {
          options = JIT_START_OPTION_START;
        } else {
          options = JIT_START_OPTION_END;
        }
#if VERBOSE_SPTI
        {
          fprintf(stderr, "%sspti_hook request %s\n", PREFIX, (start ? "SPTI_REQUEST_JIT_START" : "SPTI_REQUEST_JIT_END"));
          fflush(stderr);
        }
#endif  /* VERBOSE_SPTI */

        if (event->params_len >= 3) {
          int i = 0;
          char const *name = event->params[i++].str;
          SP_integer arity = event->params[i++].integer;
          char const *module = event->params[i++].str;
          jit_start(name, arity, module, cookie, options);
        }
      }
      break;

    case SPTI_REQUEST_JIT_DEFINED:
    case SPTI_REQUEST_JIT_UNDEFINED:
      {
        int const defined = (event->request == SPTI_REQUEST_JIT_DEFINED);
        spio_t_bits options;
        if (defined) {
          options = JIT_DEFINED_OPTION_DEFINED;
        } else {
          options = JIT_DEFINED_OPTION_UNDEFINED;
        }
#if VERBOSE_SPTI
        {
          fprintf(stderr, "%sspti_hook request %s\n", PREFIX, (defined ? "SPTI_REQUEST_JIT_DEFINED" : "SPTI_REQUEST_JIT_UNDEFINED"));
          fflush(stderr);
        }
#endif  /* VERBOSE_SPTI */

        if (event->params_len >= 5) {
          int i = 0;
          char const *name = event->params[i++].str;
          SP_integer arity = event->params[i++].integer;
          char const *module = event->params[i++].str;
          void const *addr = event->params[i++].ptr;
          size_t size = event->params[i++].size;
          jit_defined(name, arity, module, addr, size, cookie, options);
        }
      }
      break;
    default:
      fprintf(stderr, "%sspti_hook Unhandled request %" SPRIdINTEGER "\n", ERROR_PREFIX, (SP_integer)event->request);
      fflush(stderr);
      break;
    }
  code = SPIO_S_NOERR;

 cleanup:
#if VERBOSE_SPTI
  {
    fprintf(stderr, "%sExit spti_hook )\n", PREFIX);
    fflush(stderr);
  }
#endif  /* VERBOSE_SPTI */
  return code;
 barf:
  goto cleanup;
}

static spti_t_cookie spti_cookie;

static void register_spti(tSP_install_spti_hook *install_spti_hook)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  SPIO_REFERENCE_BARF_LABEL();

#if VERBOSE_SPTI
  {
    fprintf(stderr, "%sRegisting spti hook (\n", PREFIX);
    fflush(stderr);
  }
#endif  /* VERBOSE_SPTI */

  spti_cookie.inited = 0;

  if (SPIO_FAILED(code = (*install_spti_hook)(spti_hook, &spti_cookie, SPIO_OPTION_NONE)))
    {
      fprintf(stderr, "%sSP_install_spti_hook() failed (0x%" SPRIxINTEGER ")\n", ERROR_PREFIX, (SP_uinteger)code);
      fflush(stderr);
    }
  CHECK(code);
  spti_cookie.inited = 1;
 cleanup:

#if VERBOSE_SPTI
  {
    fprintf(stderr, "%sRegistered spti hook )\n", PREFIX);
    fflush(stderr);
  }
#endif  /* VERBOSE_SPTI */

  return;
 barf:
  (void)code;
  goto cleanup;
}

static void unregister_spti(tSP_install_spti_hook *install_spti_hook)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  SPIO_REFERENCE_BARF_LABEL();
#if VERBOSE_SPTI
  {
    fprintf(stderr, "%sUnregisting spti hook (\n", PREFIX);
    fflush(stderr);
  }
#endif  /* VERBOSE_SPTI */

  if (SPIO_FAILED(code = (*install_spti_hook)(spti_hook, NULL, SP_INSTALL_SPTI_HOOK_OPTION_UNINSTALL)))
    {
      fprintf(stderr, "%sSP_install_spti_hook(UNINSTALL) failed (0x%" SPRIuINTEGER ")\n", ERROR_PREFIX, (SP_uinteger)code);
      fflush(stderr);
    }
  
  CHECK(code);
 cleanup:
  spti_cookie.inited = 0;
#if VERBOSE_SPTI
  {
    fprintf(stderr, "%sUnregistered spti hook )\n", PREFIX);
    fflush(stderr);
  }
#endif  /* VERBOSE_SPTI */

  return;
 barf:
  (void)code;
  goto cleanup;
}

extern SPDLL_DECLARE_EXPORTED SP_spti_hook SPTI_OnLoad;

spio_t_error_code SPDLL_DEFINE_EXPORTED SPCDECL SPTI_OnLoad(sp_t_spti_event *event, void *reserved)
{
  (void)event; (void)reserved;
#if VERBOSE_SPTI
  {
    fprintf(stderr, "%sSPTI_OnLoad()\n", PREFIX);
    fflush(stderr);
  }
#endif  /* VERBOSE_SPTI */
  register_spti(event->spenv->dispatch_API_SICSTUS_H->pSP_install_spti_hook);
  return SPIO_S_NOERR;
}

extern SPDLL_DECLARE_EXPORTED SP_spti_hook SPTI_OnUnLoad;

spio_t_error_code SPDLL_DEFINE_EXPORTED SPCDECL SPTI_OnUnLoad(sp_t_spti_event *event, void *reserved)
{
  (void)event; (void)reserved;
#if VERBOSE_SPTI
  {
    fprintf(stderr, "%sSPTI_OnUnLoad()\n", PREFIX);
    fflush(stderr);
  }
#endif  /* VERBOSE_SPTI */
  unregister_spti(event->spenv->dispatch_API_SICSTUS_H->pSP_install_spti_hook);
  return SPIO_S_NOERR;
}
