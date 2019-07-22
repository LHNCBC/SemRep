% geomtric constraints following Tarskis axioms for geometry
% thom fruehwirth ECRC 950722
% thom fruehwirth LMU 980207, 980312 for Sicstus CHR

:- use_module(library(chr)).

% handler tarski.


:- chr_constraint b/3, ol/3. 
% b(X,Y,Z)  3 points are different and collinear, on same line in that order
% ol(X,Y,L) <=> X and Y are different points on line L

b(X,Y,Z) <=> ol(X,Y,L),ol(Y,Z,L).

irreflexivity  @ ol(X,X,L) <=> fail. % true.  % ol(X,L).
turn @ ol(X,Y,(-L)) <=> ol(Y,X,L).
same_line @ ol(X,Y,L1)\ol(X,Y,L2) <=> L1=L2.
same_line @ ol(X,Y,L1)\ol(Y,X,L2) <=> L1\==L2 | L2=(-L1). % turn direction of L2
antisymmetry @ ol(X,Y,L),ol(Y,X,L) ==> X=Y. % corresponds to axiom a1
% transitivity only for points on the same line
transitivity @ ol(X,Y,L),ol(Y,Z,L) ==> ol(X,Z,L). % corresponds to axiom a2


:- chr_constraint e/4, pd/3. 
% e(X,Y,U,V)  line segments X-Y and U-V have same nonzero length
% ls(X,Y,D) the different points X and Y have nonzero distance D from each other

e(X,Y,U,V) <=> pd(X,Y,D),pd(U,V,D).

orient_pd @ ol(X,Y,L)\pd(Y,X,D) <=> pd(X,Y,D).

% simple cases

idempotence @ pd(X,Y,D1)\pd(X,Y,D2)<=>D1=D2.  % corresponds to axiom a4 and a6
idempotence @ pd(X,Y,D1)\pd(Y,X,D2)<=>D1=D2.

zero @ pd(X,X,D) <=> fail. % corresponds to axiom a5

% more like that is missing
pd(X,Y,D),ol(X,Y,L),pd(X,Z,D),ol(X,Z,L) ==> Y=Z.
pd(X,Y,D),ol(X,Y,L),pd(Z,Y,D),ol(Z,Y,L) ==> Y=Z.


% EXAMPLES =============================================================

% simple 2-dimensional geometric objects (in German)

ld(X,Y,D):- ld(X,Y,D,_L).
ld(X,Y,D,L):- pd(X,Y,D),ol(X,Y,L).

polygon([]).
polygon([_]).
polygon([X,Y|P]):-
	ol(X,Y,L),
	polygon([Y|P]).

gleichseiter([],D).
gleichseiter([_],D).
gleichseiter([X,Y|P],D):-
	pd(X,Y,D),
	gleichseiter([Y|P],D).

dreieck(A,B,C):- polygon([A,B,C,A]).

gleichdreieck(A,B,C,W):- dreieck(A,B,C), gleichseiter([A,B,C,A],W).

viereck(A,B,C,D):- polygon([A,B,C,D,A]).

konkaves_viereck(A,B,C,D,E):-
     viereck(A,B,C,D),
     b(D,E,B), b(A,E,C).  %diagonals

quadrat(A,B,C,D,E,U,V):-
     konkaves_viereck(A,B,C,D,E),
     gleichseiter([A,B,C,D,A],U),
     gleichseiter([A,E,C],V),
     gleichseiter([D,E,B],V).

rechtdrei(A,B,C,U):-
	dreieck(A,B,C),
	gleichseiter([A,_,_,B],U), %3*U
	gleichseiter([B,_,_,_,C],U), %4*U
	gleichseiter([C,_,_,_,_,A],U). %5*U

/*
| ?- b(X,Y,X).

no
| ?- b(X,X,Y).

no
| ?- b(X,Y,Z),b(X,Z,Y).

no
| ?- b(X,Y,Z),b(Z,Y,X).

ol(X,Y,_A),
ol(Y,Z,_A),
ol(X,Z,_A) ? 

yes
| ?- b(X,Y,Z),b(X,Y,Z).

ol(X,Y,_A),
ol(Y,Z,_A),
ol(X,Z,_A) ?

| ?- b(X,Y,U),b(Y,Z,U).

ol(X,Y,_A),
ol(Y,U,_A),
ol(X,U,_A),
ol(Y,Z,_A),
ol(Z,U,_A),
ol(X,Z,_A) ? 

yes
| ?- b(X,Y,Z),b(X,Y,U).

ol(X,Y,_A),
ol(Y,Z,_A),
ol(X,Z,_A),
ol(Y,U,_A),
ol(X,U,_A) ? 

yes
| ?- ol(X,Y,L),ol(X,Z,L).

ol(X,Y,L),
ol(X,Z,L) ? 

yes
| ?- ol(X,Y,L),ol(X,Z,L), (ol(Y,Z,L);ol(Z,Y,L)).

ol(X,Y,L),
ol(X,Z,L),
ol(Y,Z,L) ? ;

ol(X,Y,L),
ol(X,Z,L),
ol(Z,Y,L) ? ;

no
| ?- ol(X,Y,L),ol(Y,U,L1),ol(U,V,L).

ol(X,Y,L),
ol(Y,U,L1),
ol(U,V,L) ? ;

no
| ?- ol(X,Y,L),ol(Y,U,L1),ol(U,V,L1).

ol(X,Y,L),
ol(Y,U,L1),
ol(U,V,L1),
ol(Y,V,L1) ? 

yes
| ?- ol(V,Y,L),ol(U,Y,L1),ol(U,V,L1).

ol(V,Y,L),
ol(U,Y,L1),
ol(U,V,L1) ? ;

no

*/



/*
| ?- e(X,Y,Y,X).

pd(X,Y,_A) ? 

yes
| ?- e(X,Y,X,Y).

pd(X,Y,_A) ? 

yes
| ?- e(X,Y,Z,Z).

no
| ?- e(X,Y,A,B),e(X,Y,A,B).

pd(X,Y,_A),
pd(A,B,_A) ? 

yes
| ?- e(X,Y,A,B),e(Y,X,B,A).

pd(X,Y,_A),
pd(A,B,_A) ? 

yes
| ?- e(X,Y,Z,U),e(X,Y,V,W).

pd(X,Y,_A),
pd(Z,U,_A),
pd(V,W,_A) ? 

yes
| ?- pd(X,Y,D1),pd(Y,Z,D2).

pd(X,Y,D1),
pd(Y,Z,D2) ? 

yes
| ?- pd(X,Y,D1),pd(Y,Z,D2),ol(X,Y,L),ol(Y,Z,L),pd(X,Z,D3).

pd(X,Y,D1),
pd(Y,Z,D2),
ol(X,Y,L),
ol(Y,Z,L),
ol(X,Z,L),
pd(X,Z,D3) ? 

yes
| ?- e(X,Y,U,V),e(X,Y,X,U).

pd(X,Y,_A),
pd(U,V,_A),
pd(X,U,_A) ? 

yes
| ?- e(X,Y,U,V),e(X,Y,X,U),b(X,Y,U).

no
| ?- e(X,Y,U,V),e(X,Y,X,U),b(X,Y,V).

pd(X,Y,_A),
pd(U,V,_A),
pd(X,U,_A),
ol(X,Y,_B),
ol(Y,V,_B),
ol(X,V,_B) ? 

yes
| ?- e(X,Y,U,V),e(X,Y,X,U),b(X,U,Y).

no

| ?- pd(X,Y,D),pd(Y,Z,D),pd(X,Z,D),ol(X,Y,L),ol(Y,Z,L).

no

| ?- pd(X,Y,D),ol(X,Y,L),pd(X,Z,D),ol(X,Z,L).

Z = Y,
pd(X,Y,D),
ol(X,Y,L) ? 

yes
| ?- e(X,Y,X,Z), b(X,Y,Z).

no

| ?- e(X,Y,X,Z), b(X,Y,U), b(X,Z,U).

Z = Y,
pd(X,Y,_A),
ol(X,U,_B),
ol(X,Y,_B),
ol(Y,U,_B) ? 

yes

*/



/*
| ?- gleichdreieck(A,B,C,W),gleichdreieck(A,C,D,V),gleichdreieck(B,C,E,U).
V = U,
W = U,
...
% cannot find out that D-C-E is on same line

| ?- konkaves_viereck(A,B,C,D,E),gleichdreieck(A,B,C,W),gleichdreieck(A,B,E,W).

E = C,

| ?- quadrat(A,B,C,D,E,U,V),gleichdreieck(A,B,C,W).

W = U,
% does not know that diagonal is longer than sides

| ?- quadrat(A,B,C,D,E,U,V),gleichdreieck(A,B,C,W), 
          e(A,D,A,D1), b(E,D1,C).

| ?- quadrat(A,B,C,D,E,U,V),gleichdreieck(A,B,C,W),gleichdreieck(A,B,E,W1).

no

| ?- rechtdrei(A,B,C,U),gleichdreieck(A,B,C,W).
% no constraining, since it is not known that U+U=/=U

*/


% end of file handler tarski.pl -------------------------------------