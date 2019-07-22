% rational tree handler with OZ type constraints, see also tree.pl
% ECRC 1993, 950519; LMU 980211, 980312 Thom Fruehwirth

% Bart Demoen: this didn't run in SICStus 3, with CHR by Christian
% so included the ordering.pl directly and then it worked

:- use_module( library(chr)).

% handler type.

:- chr_constraint (~)/2, (':<')/2, ('&&')/2.
% T1 ~ T2 means: term T1 is syntactically equal to term T2
% T1 :< T2 means: term T1 has type T2, types are not to be cyclic (infinite)
% Z~X&&Y means: the type Z is the intersection of types X and Y 

:- op(100,xfx,(~)).	   % equality
:- op(100,xfx,(':<')).  % type constraint
:- op(110,xfx,('&&')).  % type intersection

% need global order on variables for equality with infinite (cyclic) terms
% :- use_module( library('chr/ordering'), [globalize/1,var_compare/3]).

:- use_module( library(atts)).

:- attribute id/1.

%
% The exception mechanism copies the thrown term.
% Thus we cannot pass the variable to the catcher ...
%
verify_attributes( X, Y, []) :-
	get_atts( X, id(Id)),
	!,
	( var(Y) ->
	    ( get_atts( Y, id(_)) ->
		true % raise_exception( binding_globalized_var)
	    ;
		put_atts( Y, id(Id))
	    )
	;
	    true % raise_exception( binding_globalized_var)
	).
verify_attributes( _, _, []).

globalize( Term) :-
	term_variables( Term, Vars),
	var_globalize( Vars).

var_globalize( X) :- var( X), !,		% indexing only
	( get_atts( X, id(_)) ->
	    true
	;
	    put_atts( X, id(_))
	).
var_globalize( []).
var_globalize( [X|Xs]) :-
	var_globalize( X),
	var_globalize( Xs).

unglobalize( Term) :-
	term_variables( Term, Vars),
	var_unglobalize( Vars).

var_unglobalize( X) :- var( X), !,		% indexing only
	put_atts( X, -id(_)).
var_unglobalize( []).
var_unglobalize( [X|Xs]) :-
	var_unglobalize( X),
	var_unglobalize( Xs).

var_compare( Rel, X, Y) :-
	(var(X),get_atts( X, id(IdX)) ->
	    true
	;
	    raise_exception( not_globalized)
	),
	(var(Y),get_atts( Y, id(IdY)) ->
	    true
	;
	    raise_exception( not_globalized)
	),
	compare( Rel, IdX, IdY).






% var is smaller than any non-var term
lt(X,Y):- (var(X),var(Y) -> globalize(X),globalize(Y),var_compare(<,X,Y) ; X@<Y).
le(X,Y):- (var(X) -> true ; X@=<Y).


% equality ~ -----------------------------------------------------------------

ident @ T ~ T <=> true.
decompose @ T1 ~ T2 <=> nonvar(T1),nonvar(T2) | 
		same_functor(T1,T2),     
		T1=..[F|L1],T2=..[F|L2],
		equate(L1,L2).
orient @ T ~ X <=> lt(X,T) | X ~ T.   
simplify @ X ~ T1 \ X ~ T2 <=> le(T1,T2) | T1 ~ T2.

  same_functor(T1,T2):- functor(T1,F,N),functor(T2,F,N).

  equate([],[]).
  equate([X|L1],[Y|L2]):- X ~ Y, equate(L1,L2).


% type constraint :< ---------------------------------------------------------

type_identity  @ XT :< XT <=> true.
type_decompose @ T1 :< T2 <=> nonvar(T1),nonvar(T2) | 
		same_functor(T1,T2),
		T1=..[_|L1],T2=..[_|L2],
		contain(L1,L2).
type_simplify1 @ X ~ T1 \ X :< T2 <=> var(X) | T1 :< T2.
type_simplify2 @ X ~ T1 \ T2 :< X <=> var(X) | T2 :< T1.
type_intersect @ X :< T1, X :< T2 <=> nonvar(T1),nonvar(T2) | 
		same_functor(T1,T2),
		T1=..[F|L1],T2=..[F|L2],
		type_intersect(L1,L2,L3),
		T3=..[F|L3],
		X :< T3.
type_transitiv @ T1 :< Y, Y :< T2 ==> var(Y) | T1 :< T2. 

  contain([],[]).
  contain([X|L1],[Y|L2]):- X :< Y,
	contain(L1,L2).

  type_intersect([],[],[]).
  type_intersect([X|L1],[Y|L2],[Z|L3]):- Z~X&&Y,
        type_intersect(L1,L2,L3).

% X~Y&&Z parses as (X~Y)&&Z, therefore it cannot match X~T
type_functional @ Z1~X&&Y \ Z2~X&&Y <=> Z1=Z2.
type_functional @ Z1~Y&&X \ Z2~X&&Y <=> Z1=Z2.
type_propagate  @ Z~X&&Y ==> Z :< X, Z :< Y.

/*
% Examples
:- f(a,b):<f(X,X).			% succeeds -  X is a "top" ('a hole')
   a:<X,b:<X.
:- Y~f(U),Z~f(X),X:<Y,X:<Z.		% succeeds
   Y~f(U),Z~f(X),UX~X&&U,X:<f(UX),UX:<X,UX:<U,UX:<f(UX)
:- Y~f(U),U~a,Z~f(X),X:<Y,X:<Z.	        % fails
:- X:<Y,X~f(X),X:<f(Y).
   X~f(X), f(X):<Y			% simplifies nicely
:- X:<Y,Y~f(U),U~a,Z~f(X),X:<Z.	        % fails
:- X~Y,U:<X,Z:<a,U:<Z,Y:<b.		% fails
:- X:<Y,X:<Z,Y~a,Z~b.			% fails
:- X:<Y,X:<Z,Y~f(Y,U),Z~f(Z,V),U~a,V~b. % fails
:- X:<f(X,Y), X~f(X1,U), X1~f(X11,U1), U1~g(U), a:<U, b:<U. % succeeds
:- X~ f(X,Y), X~f(X1,U), X1~f(X11,U1), U1~g(U), a:<U, b:<U. % fails
*/

% end of handler type =======================================================
