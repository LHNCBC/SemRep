%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.3 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   nfq.pl                                                 %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module( nfq, 
	[
	    {}/1,
	    entailed/1,
	    wait_linear/3,

	    nf/2,
	    repair/2,
	    nf_constant/2,
	    split/3,
	    transg/3
	]).

:- use_module( arith_q).

:- use_module( clpq, '../clpq',
	[
	    'solve_<'/1,
	    'solve_=<'/1,
	    'solve_=\\='/1,
	    add_linear_11/3,
	    export_binding/2,
	    ineq_one/4,
	    ineq_one_n_n_0/1,
	    ineq_one_n_p_0/1,
	    ineq_one_s_n_0/1,
	    ineq_one_s_p_0/1,
	    log_deref/4,
	    normalize_scalar/2,
	    solve/1
	]).

:- ensure_loaded( nf).

transg( resubmit_eq(Nf)) -->
  {
    nf2term( [], Z),
    nf2term( Nf, Term)
  },
  [ clpq:{Term=Z} ].
transg( resubmit_lt(Nf)) -->
  {
    nf2term( [], Z),
    nf2term( Nf, Term)
  },
  [ clpq:{Term<Z} ].
transg( resubmit_le(Nf)) -->
  {
    nf2term( [], Z),
    nf2term( Nf, Term)
  },
  [ clpq:{Term=<Z} ].
transg( resubmit_ne(Nf)) -->
  {
    nf2term( [], Z),
    nf2term( Nf, Term)
  },
  [ clpq:{Term=\=Z} ].
transg( wait_linear_retry(Nf,Res,Goal)) -->
  {
    nf2term( Nf, Term)
  },
  [ clpq:{Term=Res}, Goal ].
