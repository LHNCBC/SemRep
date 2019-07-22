:- use_module(library(chr)).

% handler modelgenerator.

:- chr_constraint attends/3, requires/2, less/2, leq/2.

:- op(700,xfx,less).
:- op(700,xfx,leq).


X less Y <=> nonvar(X), nonvar(Y) | X < Y.

X less Y \  X less Z <=>  Y =< Z | true.

X less Y, X leq Y <=>  fail.

X leq X <=> true.


attends(S, Y, TY), requires(Y, X) ==> attends(S, X, TX), TX less TY.

attends(john,C,T)  ==> true | (T leq 1996 ; T less 1994).


example :- 
	attends(john,constraintprogrammierung,1996),
	requires(constraintprogrammierung,logik).

/*
?- example.

   requires(constraintprogrammierung,logik),
   _A less 1994,
   attends(john,constraintprogrammierung,1996),
   attends(john,logik,_A) ?
*/
