/*
SICStus specific file - could be a model for other Prolog systems that
do not have "backtrackable global variables", but it is very unsofisticated.

SICStus has non-backtrackable global variables which need copying on
each access - supported by the blackboard.

"backtrackable global variables" work differently.

A backtrackable global variable (BGV) has a name that is an atom.
A BGV can have only one value at any time, but the value can be changed.

b_setval(+atom,?value)
	sets the value for the variable;
	on backtracking, the old value is reset;
	the value is not copied

b_getval(+atom,?value)
	gets the value of the variable; note that the actual value of
	the variable is unified with the second argument of b_getval !
	the value is not copied



The implementation uses global_term_ref_0/1 - SICStus specific.
Other parts of CHR use global_term_ref_1/1, so it is exported.

In the code below global_term_ref_0/1 gives access to an assoc. That's where
the atom-value pairs are stored. In SWI and hProlog, access to a NBV is
constant time - below will give you something worse.


Bart Demoen Thu Nov 17 10:36:23 EST 2005

Updated by Tom Schrijvers to use assoc rather than open-ended list.

Todo:
      error - in case better errors are needed - but only the CHR compiler
	    uses them and generates code using them, so unless this code
	    is available to other users, nothing much is needed

      better datastructure for GlobalVars - would be good for
	     performance reasons
*/

:- module(b_globval, [
		      b_setval/2,
		      b_getval/2,
		      global_term_ref_1/1
		     ]).

:- use_module(library(assoc3)).

b_setval(Atom,Value) :-
	atom(Atom),
	nonvar(Value),
	!,
	global_term_ref_0(GlobalVars),
	( var(GlobalVars) ->
		empty_assoc(Assoc0),
		put_assoc(Atom,Assoc0,Value,Assoc),
		create_mutable(Assoc,GlobalVars)
	;	
		get_mutable(Assoc0,GlobalVars),
		put_assoc(Atom,Assoc0,Value,Assoc),
		update_mutable(Assoc,GlobalVars)
	).
b_setval(Atom,Value) :-
	error(b_setval(Atom,Value)).

b_getval(Atom,Value) :-
	atom(Atom),
	!,
	global_term_ref_0(GlobalVars),
	nonvar(GlobalVars),
	get_mutable(Assoc,GlobalVars),
	get_assoc(Atom,Assoc,Value).
b_getval(Atom,Value) :-
	error(b_getval(Atom,Value)).

global_term_ref_0(X) :-
	prolog:'$term_ref'(0, X).

global_term_ref_1(X) :-
	prolog:'$term_ref'(1, X).

