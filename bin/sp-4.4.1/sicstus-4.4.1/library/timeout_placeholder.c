/*
  [PM] 4.4.0 These are just placeholder definitions of the timeout
  foreign functions that existed prior to 4.4.0. The file and the
  foreign resource created from it are no longer used, but retained for
  backward compatibility with some third-party build scripts and the
  like.
*/

#include <sicstus/sicstus.h>
#include <timeout_glue.h>

SP_integer SPCDECL to_clocks_per_second(void)
{
  return 100;
}

void SPCDECL to_init(int when)
{
  SP_ASSERT(0);
  (void)when;
}

void SPCDECL to_deinit(int when)
{
  SP_ASSERT(0);
  (void) when;
}

SP_integer SPCDECL to_start_timer_a(SP_term_ref tContExpires, SP_term_ref tLimit)
{
  SP_ASSERT(0);
  {
    (void) tContExpires;
    (void) tLimit;
    return -1;
  }
}

SP_term_ref SPCDECL to_stop_timer_a(void)
{
  SP_ASSERT(0);
  {
    SP_term_ref term = SP_new_term_ref();
    SP_put_string(term, "off");
    return term;
  }
}

SP_term_ref SPCDECL to_timer_now(void)
{
  SP_ASSERT(0);
  {
    SP_term_ref term = SP_new_term_ref();
    SP_put_integer(term, 0);
    return term;
  }
}
