% INEQUALITIES with MINIMIUM and MAXIMUM on terms
% 920303, 950411 ECRC Thom Rruehwirth
% 961105 Christian Holzbaur, SICStus mods

:- use_module( library(chr)).

% handler minmax.

:- chr_option(check_guard_bindings, on).        % for ~=/2 with deep guards

:- op(700, xfx, lss). % less than
:- op(700, xfx, grt). % greater than
:- op(700, xfx, neq). % not equal to
:- op(700, xfx, geq). % greater or equal to
:- op(700, xfx, leq). % less or equal to
:- op(700, xfx, ~=).  % not identical

:- chr_constraint (~=)/2.

X ~= X <=> fail.
X ~= Y <=> ground(X),ground(Y) | X\==Y.

:- chr_constraint (leq)/2, (lss)/2, (neq)/2, minimum/3, maximum/3.

X geq Y :- Y leq X.
X grt Y :- Y lss X.


/* leq */

built_in     @ X leq Y <=> ground(X),ground(Y) | X @=< Y.
reflexivity  @ X leq X <=> true.

antisymmetry @ X leq Y, Y leq X <=> X = Y.

transitivity @ X leq Y, Y leq Z ==> X \== Y, Y \== Z, X \== Z | X leq Z.

subsumption  @ X leq N \ X leq M <=> N@<M | true.
subsumption  @ M leq X \ N leq X <=> N@<M | true.


/* lss */

built_in     @ X lss Y <=> ground(X),ground(Y) | X @< Y.
irreflexivity@ X lss X <=> fail.

transitivity @ X lss Y, Y lss Z ==> X \== Y, Y \== Z | X lss Z.
transitivity @ X leq Y, Y lss Z ==> X \== Y, Y \== Z | X lss Z.
transitivity @ X lss Y, Y leq Z ==> X \== Y, Y \== Z | X lss Z.

subsumption  @ X lss Y \ X leq Y <=> true.

subsumption  @ X lss N \ X lss M <=> N@<M | true.
subsumption  @ M lss X \ N lss X <=> N@<M | true.

subsumption  @ X leq N \ X lss M <=> N@<M | true.
subsumption  @ M leq X \ N lss X <=> N@<M | true.
subsumption  @ X lss N \ X leq M <=> N@<M | true.
subsumption  @ M lss X \ N leq X <=> N@<M | true.


/* neq */

built_in     @ X neq Y <=> X ~= Y | true.
irreflexivity@ X neq X <=> fail. 

subsumption  @ X neq Y \ Y neq X <=> true.
subsumption  @ X lss Y \ X neq Y <=> true.
subsumption  @ X lss Y \ Y neq X <=> true.

simplification @ X neq Y, X leq Y <=> X lss Y. 
simplification @ Y neq X, X leq Y <=> X lss Y. 



/* MINIMUM */

:- chr_constraint labeling/0.

labeling, minimum(X, Y, Z)#Pc <=> 
	(X leq Y, Z = X ; Y lss X, Z = Y), 
	labeling
    pragma passive(Pc).

built_in @ minimum(X, Y, Z) <=> ground(X),ground(Y) | (X@=<Y -> Z=X ; Z=Y).
built_in @ minimum(X, Y, Z) <=> Z~=X | Z = Y, Y lss X.
built_in @ minimum(Y, X, Z) <=> Z~=X | Z = Y, Y lss X.

min_eq @ minimum(X, X, Y) <=> X = Y.

min_leq @ Y leq X \ minimum(X, Y, Z) <=> Y=Z.
min_leq @ X leq Y \ minimum(X, Y, Z) <=> X=Z.
min_lss @ Z lss X \ minimum(X, Y, Z) <=> Y=Z.
min_lss @ Z lss Y \ minimum(X, Y, Z) <=> X=Z. 

functional @ minimum(X, Y, Z) \ minimum(X, Y, Z1) <=> Z1=Z.
functional @ minimum(X, Y, Z) \ minimum(Y, X, Z1) <=> Z1=Z.

propagation @ minimum(X, Y, Z) ==> X\==Y | Z leq X, Z leq Y.


/* MAXIMUM */

labeling, maximum(X, Y, Z)#Pc <=> 
	(X leq Y, Z = Y ; Y lss X, Z = X), 
	labeling
    pragma passive(Pc).

built_in @ maximum(X, Y, Z) <=> ground(X),ground(Y) | (Y@=<X -> Z=X ; Z=Y).
built_in @ maximum(X, Y, Z) <=> Z~=X | Z = Y, X lss Y.
built_in @ maximum(Y, X, Z) <=> Z~=X | Z = Y, X lss Y.

max_eq @ maximum(X, X, Y) <=> X = Y.

max_leq @ Y leq X \ maximum(X, Y, Z) <=> X=Z.
max_leq @ X leq Y \ maximum(X, Y, Z) <=> Y=Z.
max_lss @ X lss Z \ maximum(X, Y, Z) <=> Y=Z.
max_lss @ Y lss Z \ maximum(X, Y, Z) <=> X=Z. 

functional @ maximum(X, Y, Z) \ maximum(X, Y, Z1) <=> Z1=Z.
functional @ maximum(X, Y, Z) \ maximum(Y, X, Z1) <=> Z1=Z.

propagation @ maximum(X, Y, Z) ==> X\==Y | X leq Z, Y leq Z.



% end of handler minmax