/*
SWI has inherited from hProlog a more liberal approach to attributed
variables than SICStus has. The current module simulates this approach.

In what follows, Module must always be an atom.

put_attr(Var,Module,Term)
	gives Var the attribute Term associated to module Module
	if Var had already an attribute associated to module Module, it
	   is replaced by Term - it is restored on backtracking
	attributes belonging to other modules are not affected
	no copy is made


get_attr(Var,Module,Term)
        retrieves the attribute Term of Var belonging to Module
	fails if there is none
	no copy is made


del_attr(Var,Module)
	removes the attribute Term of Var belonging to Module
	if there is none, succeeds


copy_term_nat(TermIn,TermOut)
	makes a copy of TermIn without attributes (and with systematic
	renaming of the variables)


Unification of attributed variables
===================================

Suppose X has attributes T1 for Mod1 and T2 for Mod2, and that X is
unified with a nonvariable N, then the following goals will called:

	Mod1:attr_unify_hook(T1,N),
	Mod2:attr_unify_hook(T2,N)

(but maybe not in this order) and X is already bound to N (see
verify_attributes below)


If X has attributes X1 for ModX1 and X2 for ModX2
and Y has attributes Y1 for ModY1 and Y2 for ModY2 and Y3 for ModY3
and X and Y are unified, then

either

     ModX1:attr_unify_hook(X1,Y),
     ModX2:attr_unify_hook(X2,Y)
is called in a state where X and Y have been unified already and the
result (Y) has the attributes Y original had

or

     ModY1:attr_unify_hook(Y1,X),
     ModY2:attr_unify_hook(Y2,X)
     ModY1:attr_unify_hook(Y1,X),

is called - the order and which not guaranteed.


Bart Demoen
K.U.Leuven
Wed Jan 18 14:08:13 CET 2006



This module will become obsolete if chr-code-generation for SICStus is
adapted to include appropriate declarations for the SICStus attvars.
*/




:- module(hpattvars,[
		     put_attr/3,
		     get_attr/3,
		     del_attr/2,
		     copy_term_nat/2
		    ]).

:- use_module(library(atts)).
:- use_module(hprolog).

:- attribute hpattvars/1.

del_attr(X,Mod) :-
	(get_atts(X,hpattvars(L1)) ->
	    att_del(L1,Mod,L2),
	    put_atts(X,hpattvars(L2))
	;
	    true
	).

put_attr(X,Mod,Val) :-
	(get_atts(X,hpattvars(L1)) ->
	    att_del(L1,Mod,L2),
	    put_atts(X,hpattvars([Mod,Val|L2]))
	;
	    put_atts(X,hpattvars([Mod,Val]))
	).

get_attr(X,Mod,Val) :-
	get_atts(X,hpattvars(L1)),
	att_mem(L1,Mod,Val).

verify_attributes(X,Y,[hpattvars:attr_unify_hook_iterator(XAttrs,Y)]) :-
	get_atts(X,hpattvars(XAttrs)).

attr_unify_hook_iterator([],_).
attr_unify_hook_iterator([Mod,Att|R],Y) :-
	Mod:attr_unify_hook(Att,Y),
	attr_unify_hook_iterator(R,Y).

att_mem([M,A|R],MIn,Val) :-
	(M == MIn ->
	    Val = A
	;
	    att_mem(R,MIn,Val)
	).

att_del([],_,[]).
att_del([M,A|R],MIn,Out) :-
	(M == MIn ->
	    Out = R
	;
	    Out = [M,A|O],
	    att_del(R,MIn,O)
	).

copy_term_nat(In,Out) :-
	chr_term_variables(In,Vars),
	delatts(Vars,Attvars,Atts),
	copy_term(In,Out),
	resetatts(Attvars,Atts).

delatts([],[],[]).
delatts([X|R],AttVars,Atts) :-
	(get_atts(X,hpattvars(Att)) ->
	    AttVars = [X|RAttVars],
	    Atts = [Att|RAtts],
	    put_atts(X,-hpattvars(_))
	;
	    AttVars = RAttVars,
	    Atts = RAtts
	),
	delatts(R,RAttVars,RAtts).

resetatts([],_).
resetatts([X|R],[A|As]) :-
	put_atts(X,hpattvars(A)),
	resetatts(R,As).

