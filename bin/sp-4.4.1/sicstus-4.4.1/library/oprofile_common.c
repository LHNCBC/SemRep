#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include <sicstus/sicstus.h>
#include <sicstus/config.h>

#define BARF(CODE) do { code = (CODE); goto barf; } while(0)
#define CHECK(CODE) do { code = (CODE); if (SPIO_FAILED(code)) { spio_t_error_code tmp_code = code; BARF(tmp_code); } } while(0)
#define NULL_CHECK(PTR) do { void const * tmp_ptr = (PTR); if (tmp_ptr == NULL) { BARF(SPIO_E_OUT_OF_MEMORY); } } while(0)

#ifndef SPTI_USE_OPROFILE
#if HAVE_OPAGENT_H
#define SPTI_USE_OPROFILE 1
#endif  /* HAVE_OPAGENT_H */
#else
#error "SPTI_USE_OPROFILE is defined"
#endif  /* SPTI_USE_OPROFILE */

#if SPTI_USE_OPROFILE
#if HAVE_OPAGENT_H
#include <stdint.h> /* missing requirement in opagent.h */
#include <opagent.h>
#else  /* !HAVE_OPAGENT_H */
#error "SPTI_USE_OPROFILE requires HAVE_OPAGENT_H"
#endif  /* HAVE_OPAGENT_H */
#endif  /* SPTI_USE_OPROFILE */

typedef struct spti_t_cookie_ spti_t_cookie;
struct spti_t_cookie_ {
  int inited;
#if SPTI_USE_OPROFILE
  op_agent_t op_agent;
#endif  /* SPTI_USE_OPROFILE */
};

#if SPTI_USE_OPROFILE
static char *stralloc(char const *strings[])
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  char *s = NULL;
  char const *tmp;
  size_t len;
  int i;

  len = 0;
  i = 0;
  while((tmp = strings[i++]) != NULL)
      {
        size_t tmp_len = strlen(tmp);
        len += tmp_len;
      }

  s = malloc(len +1);        /* +1 for NUL */
  NULL_CHECK(s);

  len = 0;
  i = 0;
  while((tmp = strings[i++]) != NULL)
    {
      size_t tmp_len = strlen(tmp);
      memcpy(s+len, tmp, tmp_len);
      len += tmp_len;
    }
  s[len] = '\0';
 cleanup:
  return s;
 barf:
  (void)code;
  if (s != NULL)
    {
      free(s);
      s = NULL;
    }
  goto cleanup;
}
#endif /* SPTI_USE_OPROFILE */

#define JIT_DEFINED_OPTION_DEFINED   0x00001
#define JIT_DEFINED_OPTION_UNDEFINED 0x00002
static void jit_defined(char const* name, SP_integer arity, char const *module, void const *addr, size_t size, spti_t_cookie *cookie, spio_t_bits options)
{
  int const defined = SPIO_MASK_IS_SET(options, JIT_DEFINED_OPTION_DEFINED);
  (void)defined;
  (void)name; (void)arity; (void)module; (void)addr; (void)size; (void)cookie;

#if VERBOSE_SPTI
  {
    fprintf(stderr, "%s JITTED %s:%s/%d @%p +%" SPRIdSZ "\n",
            (defined ? "Registering" : "Unregistering"),
            module, name, (int)(arity & 0xFF),
            addr, size);
    fflush(stderr);
  }
#endif  /* VERBOSE_SPTI */

#if SPTI_USE_OPROFILE
  {
    if (cookie->op_agent == NULL) {
      cookie->op_agent = op_open_agent();
      if (cookie->op_agent == NULL) {
        int err = errno;
#if VERBOSE_SPTI
        perror("Error: op_open_agent()");
#endif	/* VERBOSE_SPTI */
        (void)err;
      }
    }
    if (cookie->op_agent != NULL) {
      char *symbol_name;
      char arity_name[4]; /* 255\0 */
      char const *components[6];
	  
      snprintf(arity_name, sizeof arity_name, "%u", (int)(arity&0xFF));
      arity_name[3] = '\0'; /* paranoia */

      components[0]=module;
      components[1] = ":";
      components[2] = name;
      components[3] = "/";
      components[4] = arity_name;
      components[5] = NULL;
      symbol_name = stralloc(components);

      if (symbol_name != NULL) {
        {
          uint64_t const vma = (uint64_t)(uintptr_t)addr;
          void const *code = addr; /* FIXME: Use NULL? How does oprofile use dumped code? */
          unsigned int const code_size = (unsigned int)size;
          if (defined) {

#if VERBOSE_SPTI>1
            {
              fprintf(stderr, "Calling op_write_native_code(%s, 0x%" SPRIxINTEGER ", 0x%" SPRIxINTEGER ", %ud)\n",
                      symbol_name,
                      (SP_uinteger)vma, (SP_uinteger)code, (unsigned int)code_size);
              fflush(stderr);
            }
#endif  /* VERBOSE_SPTI */

            if (op_write_native_code(cookie->op_agent, symbol_name, vma, code, code_size) == 0) {
              /* Could write line information here */
            } else {
              int err = errno;
#if VERBOSE_SPTI
              perror("Error: op_write_native_code()");
#endif	/* VERBOSE_SPTI */
              (void)err;
            }
          } else {
#if VERBOSE_SPTI>1
            {
              fprintf(stderr, "Calling op_unload_native_code(0x%" SPRIxINTEGER ")\n", (SP_uinteger)vma);
              fflush(stderr);
            }
#endif  /* VERBOSE_SPTI */
            op_unload_native_code(cookie->op_agent, vma);
          }
        }
        free(symbol_name);
      }
    }
  }
#endif  /* SPTI_USE_OPROFILE */
}

static spio_t_error_code SPCDECL spti_hook(sp_t_spti_event *event, void *cookie_)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  spti_t_cookie *cookie = (spti_t_cookie *)cookie_;
  SPIO_REFERENCE_BARF_LABEL();
  (void)event;
#if VERBOSE_SPTI
  {
    fprintf(stderr, "Enter spti_hook (\n");
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
      /* Not used by OProfile SPTI */
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
          fprintf(stderr, "  spti_hook request %s\n", (defined ? "SPTI_REQUEST_JIT_DEFINED" : "SPTI_REQUEST_JIT_UNDEFINED"));
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
#if VERBOSE_SPTI
      {
        fprintf(stderr, "  spti_hook Unhandled request %" SPRIdINTEGER "\n", (SP_integer)event->request);
        fflush(stderr);
      }
#endif  /* VERBOSE_SPTI */
      break;
    }
  code = SPIO_S_NOERR;

 cleanup:
#if VERBOSE_SPTI
  {
    fprintf(stderr, "Exit spti_hook )\n");
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
    fprintf(stderr, "Registing OPROFILE spti hook (\n");
    fflush(stderr);
  }
#endif  /* VERBOSE_SPTI */

  spti_cookie.inited = 0;
#if SPTI_USE_OPROFILE
  spti_cookie.op_agent = NULL;
#endif /* SPTI_USE_OPROFILE */

  if (SPIO_FAILED(code = (*install_spti_hook)(spti_hook, &spti_cookie, SPIO_OPTION_NONE)))
    {
#if VERBOSE_SPTI
      {
        fprintf(stderr, "SP_install_spti_hook() failed (%" SPRIdINTEGER ")\n", (SP_integer)code);
        fflush(stderr);
      }
#endif  /* VERBOSE_SPTI */
    }
  CHECK(code);
  spti_cookie.inited = 1;
 cleanup:

#if VERBOSE_SPTI
  {
    fprintf(stderr, "Registered spti hook )\n");
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
    fprintf(stderr, "Unregisting spti hook (\n");
    fflush(stderr);
  }
#endif  /* VERBOSE_SPTI */

  if (SPIO_FAILED(code = (*install_spti_hook)(spti_hook, NULL, SP_INSTALL_SPTI_HOOK_OPTION_UNINSTALL)))
    {
#if VERBOSE_SPTI
      fprintf(stderr, "SP_install_spti_hook(UNINSTALL) failed (0x%" SPRIdINTEGER")\n", (SP_integer)code);
      fflush(stderr);
#endif  /* VERBOSE_SPTI */
    }
  
  CHECK(code);
 cleanup:
  spti_cookie.inited = 0;
#if VERBOSE_SPTI
  {
    fprintf(stderr, "Unregistered spti hook )\n");
    fflush(stderr);
  }
#endif  /* VERBOSE_SPTI */

  return;
 barf:
  (void)code;
  goto cleanup;
}
