% Thom Fruehwirth ECRC 1991-1993
% 910528 started boolean,and,or constraints
% 910904 added xor,neg constraints
% 911120 added imp constraint
% 931110 ported to new release
% 931111 added card constraint 
% 961107 Christian Holzbaur, SICStus mods

% 8 febr 2006: Bart Demoen changed ?- to :- in some predicates 

:- use_module( library(chr)).

% handler bool.

:- chr_constraint boolean/1, and/3, or/3, xor/3, neg/2, imp/2.
:- chr_constraint labeling/0.


boolean(0) <=> true.
boolean(1) <=> true.

labeling, boolean(A)#Pc <=> 
	(A=0 ; A=1), 
	labeling
    pragma passive(Pc).


% and/3 specification
%and(0,0,0).
%and(0,1,0).
%and(1,0,0).
%and(1,1,1).

and(0,X,Y) <=> Y=0.
and(X,0,Y) <=> Y=0.
and(1,X,Y) <=> Y=X.
and(X,1,Y) <=> Y=X.
and(X,Y,1) <=> X=1,Y=1.
and(X,X,Z) <=> X=Z.
%and(X,Y,X) <=> imp(X,Y).
%and(X,Y,Y) <=> imp(Y,X).
and(X,Y,A) \ and(X,Y,B) <=> A=B.
and(X,Y,A) \ and(Y,X,B) <=> A=B.

labeling, and(A,B,C)#Pc <=> 
	label_and(A,B,C), 
	labeling
    pragma passive(Pc).

        label_and(0,X,0).
        label_and(1,X,X).


% or/3 specification
%or(0,0,0).
%or(0,1,1).
%or(1,0,1).
%or(1,1,1).

or(0,X,Y) <=> Y=X.
or(X,0,Y) <=> Y=X.
or(X,Y,0) <=> X=0,Y=0.
or(1,X,Y) <=> Y=1.
or(X,1,Y) <=> Y=1.
or(X,X,Z) <=> X=Z.
%or(X,Y,X) <=> imp(Y,X).
%or(X,Y,Y) <=> imp(X,Y).
or(X,Y,A) \ or(X,Y,B) <=> A=B.
or(X,Y,A) \ or(Y,X,B) <=> A=B.

labeling, or(A,B,C)#Pc <=> 
  label_or(A,B,C), 
    labeling
  pragma passive(Pc).

  label_or(0,X,X).
  label_or(1,X,1).


% xor/3 specification
%xor(0,0,0).
%xor(0,1,1).
%xor(1,0,1).
%xor(1,1,0).

xor(0,X,Y) <=> X=Y.
xor(X,0,Y) <=> X=Y.
xor(X,Y,0) <=> X=Y.
xor(1,X,Y) <=> neg(X,Y).
xor(X,1,Y) <=> neg(X,Y).
xor(X,Y,1) <=> neg(X,Y).
xor(X,X,Y) <=> Y=0.
xor(X,Y,X) <=> Y=0.
xor(Y,X,X) <=> Y=0.
xor(X,Y,A) \ xor(X,Y,B) <=> A=B.
xor(X,Y,A) \ xor(Y,X,B) <=> A=B.

labeling, xor(A,B,C)#Pc <=> 
	label_xor(A,B,C), 
	labeling
    pragma passive(Pc).

        label_xor(0,X,X).
        label_xor(1,X,Y):- neg(X,Y).


% neg/2 specification
%neg(0,1).
%neg(1,0).

neg(0,X) <=> X=1.
neg(X,0) <=> X=1.
neg(1,X) <=> X=0.
neg(X,1) <=> X=0.
neg(X,X) <=> fail.
neg(X,Y) \ neg(Y,Z) <=> X=Z.	
neg(X,Y) \ neg(Z,Y) <=> X=Z.	
neg(Y,X) \ neg(Y,Z) <=> X=Z.	
% Interaction with other boolean constraints
neg(X,Y) \ and(X,Y,Z) <=> Z=0.
neg(Y,X) \ and(X,Y,Z) <=> Z=0.
neg(X,Z) , and(X,Y,Z) <=> X=1,Y=0,Z=0.
neg(Z,X) , and(X,Y,Z) <=> X=1,Y=0,Z=0.
neg(Y,Z) , and(X,Y,Z) <=> X=0,Y=1,Z=0.
neg(Z,Y) , and(X,Y,Z) <=> X=0,Y=1,Z=0.
neg(X,Y) \ or(X,Y,Z) <=> Z=1.
neg(Y,X) \ or(X,Y,Z) <=> Z=1.
neg(X,Z) , or(X,Y,Z) <=> X=0,Y=1,Z=1.
neg(Z,X) , or(X,Y,Z) <=> X=0,Y=1,Z=1.
neg(Y,Z) , or(X,Y,Z) <=> X=1,Y=0,Z=1.
neg(Z,Y) , or(X,Y,Z) <=> X=1,Y=0,Z=1.
neg(X,Y) \ xor(X,Y,Z) <=> Z=1.
neg(Y,X) \ xor(X,Y,Z) <=> Z=1.
neg(X,Z) \ xor(X,Y,Z) <=> Y=1.
neg(Z,X) \ xor(X,Y,Z) <=> Y=1.
neg(Y,Z) \ xor(X,Y,Z) <=> X=1.
neg(Z,Y) \ xor(X,Y,Z) <=> X=1.
neg(X,Y) , imp(X,Y) <=> X=0,Y=1.
neg(Y,X) , imp(X,Y) <=> X=0,Y=1.

labeling, neg(A,B)#Pc <=> 
	label_neg(A,B), 
	labeling
    pragma passive(Pc).

        label_neg(0,1).
        label_neg(1,0).


% imp/2 specification (implication)
%imp(0,0).
%imp(0,1).
%imp(1,1).

imp(0,X) <=> true.
imp(X,0) <=> X=0.
imp(1,X) <=> X=1.
imp(X,1) <=> true.
imp(X,X) <=> true.
imp(X,Y),imp(Y,X) <=> X=Y.	

labeling, imp(A,B)#Pc <=> 
	label_imp(A,B), 
	labeling
    pragma passive(Pc).

        label_imp(0,X).
        label_imp(1,1).



% Boolean cardinality operator
% card(A,B,L,N) constrains list L of length N to have between A and B 1s

:- chr_constraint card/4.

card(A,B,L):- 
	length(L,N), 
	A=<B,0=<B,A=<N,%0=<N 	
	card(A,B,L,N).

% card/4 specification
%card(A,B,[],0):- A=<0,0=<B.
%card(A,B,[0|L],N):-
%		N1 is N-1,
%		card(A,B,L,N1).
%card(A,B,[1|L],N):-  
%		A1 is A-1, B1 is B-1, N1 is N-1,
%		card(A1,B1,L,N1).

triv_sat @ card(A,B,L,N) <=> A=<0,N=<B | true.		% trivial satisfaction
pos_sat @ card(N,B,L,N) <=> set_to_ones(L).		% positive satisfaction
neg_sat @ card(A,0,L,N) <=> set_to_zeros(L).		% negative satisfaction
pos_red @ card(A,B,L,N) <=> delete(X,L,L1),X==1 | 	% positive reduction
		A1 is A-1, B1 is B-1, N1 is N-1,
		card(A1,B1,L1,N1).
neg_red @ card(A,B,L,N) <=> delete(X,L,L1),X==0 | 	% negative reduction
		N1 is N-1,
		card(A,B,L1,N1).
% special cases with two variables
card2nand @ card(0,1,[X,Y],2) <=> and(X,Y,0).		
card2neg @ card(1,1,[X,Y],2) <=> neg(X,Y).		
card2or @ card(1,2,[X,Y],2) <=> or(X,Y,1).

delete( X, [X|L],  L).
delete( Y, [X|Xs], [X|Xt]) :-
	delete( Y, Xs, Xt).

labeling, card(A,B,L,N)#Pc <=> 
	label_card(A,B,L,N), 
	labeling
    pragma passive(Pc).

label_card(A,B,[],0):- A=<0,0=<B.
label_card(A,B,[0|L],N):-
		N1 is N-1,
		card(A,B,L).
label_card(A,B,[1|L],N):-  
		A1 is A-1, B1 is B-1, N1 is N-1,
		card(A1,B1,L).

  set_to_ones([]).
  set_to_ones([1|L]):-
	set_to_ones(L).

  set_to_zeros([]).
  set_to_zeros([0|L]):-
	set_to_zeros(L).



% Auxiliary predicates

:- op(100,fy,(~~)).
:- op(100,xfy,(#)).

  solve_bool(A,C) :- var(A), !, A=C.
  solve_bool(A,C) :- atomic(A), !, A=C.
  solve_bool(A * B, C) :- !,
        solve_bool(A,A1),
        solve_bool(B,B1),
        and(A1,B1,C).
  solve_bool(A + B, C) :- !,
        solve_bool(A,A1),
        solve_bool(B,B1),
        or(A1,B1,C).
  solve_bool(A # B, C) :- !,
        solve_bool(A,A1),
        solve_bool(B,B1),
        xor(A1,B1,C).
  solve_bool(~~A,C) :- !, 
        solve_bool(A,A1),
        neg(A1,C).
  solve_bool((A -> B), C) :- !,
        solve_bool(A,A1),
        solve_bool(B,B1),
        imp(A1,B1),C=1.
  solve_bool(A = B, C) :- !,
        solve_bool(A,A1),
        solve_bool(B,B1),
        A1=B1,C=1.

  % Labeling 
  label_bool([]).
  label_bool([X|L]) :-
	(X=0;X=1),
	label_bool(L).

/*					% no write macros in SICStus

  bool_portray(and(A,B,C),Out):- !, Out = (A*B = C).
  bool_portray(or(A,B,C),Out):- !, Out = (A+B = C).
  bool_portray(xor(A,B,C),Out):- !, Out = (A#B = C).
  bool_portray(neg(A,B),Out):- !, Out = (A= ~~B).
  bool_portray(imp(A,B),Out):- !, Out = (A -> B).
  bool_portray(card(A,B,L,N),Out):- !, Out = card(A,B,L).
	
  :- define_macro(type(compound),bool_portray/2,[write]).
*/

/* end of handler bool */



