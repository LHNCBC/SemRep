/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"

SP_integer SPCDECL prolog_fd_statistics(Wam wam, SP_integer key)
{
  SP_integer l=0;
  
  switch (key) {
  case 0: l = fd.resumptions; fd.resumptions = 0; break;
  case 1: l = fd.entailments; fd.entailments = 0; break;
  case 2: l = fd.prunings; fd.prunings = 0; break;
  case 3: l = fd.failures; fd.failures = 0; break;
  case 4: l = fd.constraints; fd.constraints = 0; break;
  }
  return l;
}


SP_integer SPCDECL prolog_fd_batch(Wam wam,
				    SP_term_ref OldR,
				    SP_term_ref NewR)
{
  SP_term_ref old = SP_new_term_ref();
  SP_atom new;

  SP_put_atom(old,(fd.batching ? atom_on : atom_off));
  if (!SP_unify(OldR,old))
    return 0;
  SP_get_atom(NewR,&new);
  fd.batching = (new==atom_on);
  return 1;
}

SP_integer SPCDECL prolog_fd_hiding(Wam wam,
				    SP_term_ref OldR,
				    SP_term_ref NewR)
{
  SP_term_ref old = SP_new_term_ref();
  SP_atom new;

  SP_put_atom(old,(fd.hiding ? atom_on : atom_off));
  if (!SP_unify(OldR,old))
    return 0;
  SP_get_atom(NewR,&new);
  fd.hiding = (new==atom_on);
  return 1;
}

SP_integer SPCDECL prolog_fd_debug(Wam wam,
				    SP_term_ref OldR,
				    SP_term_ref NewR)
{
  SP_term_ref old = SP_new_term_ref();
  SP_atom new;

  SP_put_atom(old,(fd.debugging ? atom_on : atom_off));
  if (!SP_unify(OldR,old))
    return 0;
  SP_get_atom(NewR,&new);
  fd.debugging = (new==atom_on);
  return 1;
}

SP_integer SPCDECL prolog_fd_overflow(Wam wam,
				       SP_term_ref OldR,
				       SP_term_ref NewR)
{
  SP_term_ref old = SP_new_term_ref();
  SP_atom new;

  SP_put_atom(old,(fd.overflowing ? atom_error : atom_fail));
  if (!SP_unify(OldR,old))
    return 0;
  SP_get_atom(NewR,&new);
  fd.overflowing = (new==atom_error);
  return 1;
}
