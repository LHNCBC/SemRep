% fougau.pl =================================================================
% constraint handling rules for linear arithmetic
% fouriers algorithm for inequalities and gaussian elemination for equalities
% thom fruehwirth 950405-06, 980312
% 961107 Christian Holzbaur, SICStus mods

% CHOOSE one of the propagation rules below and comment out the others!
% completeness and termination depends on propagation rule used

:- use_module( library(chr)).
:- use_module( library(lists), [append/3]).
:- ensure_loaded('math-utilities').

handler fougau.

% auxiliary constraint to delay a goal G until it is ground
constraints check/1.
check(G) <=> ground(G) | G.

constraints {}/1.

{ C,Cs } <=> { C }, { Cs }.

{A =< B}  <=> ground(A),ground(B) | A=<B.
{A >= B}  <=> ground(A),ground(B) | A>=B.
{A < B}   <=> ground(A),ground(B) | A<B.
{A > B}   <=> ground(A),ground(B) | A>B.
{A =\= B} <=> ground(A),ground(B) | A=\=B.

{A =< B}  <=> normalize(B,A,P,C), eq(P,C,'>'('=')).
{A >= B}  <=> normalize(A,B,P,C), eq(P,C,'>'('=')).
{A < B}   <=> normalize(B,A,P,C), eq(P,C,'>'('>')).
{A > B}   <=> normalize(A,B,P,C), eq(P,C,'>'('>')).
%{A < B}   <=> normalize(B,A+1,P,C), eq(P,C,(>=)).  % adopt to integer
%{A > B}   <=> normalize(A,B+1,P,C), eq(P,C,(>=)).  % adopt to integer
{A =\= B} <=> normalize(A+X,B,P,C),  eq(P,C,(=:=)), check(X=\=0).  

{A =:= B} <=> ground(A),ground(B) | X is A-B, zero(X).  % handle imprecision of reals
{A =:= B} <=> var(A),   ground(B) | A is B.
{B =:= A} <=> var(A),   ground(B) | A is B.
{A =:= B} <=> unconstrained(A),var(B) | A=B.
{B =:= A} <=> unconstrained(A),var(B) | A=B.
{A =:= B} <=> normalize(A,B,P,C), eq(P,C,(=:=)).

constraints eq/3.
% eq(P,C,R) 
% P is a polynomial (list of monomials variable*coefficient), 
% C is a numeric constant and R is the relation between P and C 

% simplify single equation
zero @ eq([],C1,(=:=)) <=> zero(C1).
zero @ eq([],C1,'>'('=')) <=> C1>=0.
zero @ eq([],C1,'>'('>')) <=> C1>0.
unify @ eq([X*C2],C1,(=:=)) <=> nonground(X),nonzero(C2) | is_div(C1,C2,X).
	%, integer(X)   % if integers only
simplify @ eq(P0,C1,R) <=> delete(X*C2,P0,P),ground(X) |    % R any relation
	%, integer(X),   % if integers only	
	is_mul(X,C2,XC2),
	C is XC2+C1,
	eq(P,C,R).
/*
% must use if you unify variables of equations with each other
unified @ eq(P0,C1,R1) <=> 
	append(P1,[X*C2|P2],P0),var(X),delete(Y*C3,P2,P3),X==Y 
	|
	C23 is C2+C3,
	append(P1,[X*C23|P3],P4),
	sort1(P4,P5),		
	eq(P5,C1,R1).
*/

%(1) remove redundant inequation
% -1 (change in number of constraints)
red_poly @ eq([X*C1X|P1],C1,'>'(R1)) \ eq([X*C2X|P2],C2,'>'(R2)) <=> 
	C is C2X/C1X,		% explicit because of call_explicit bug
	C>0,	                % same sign
        C1C is C1*C,            
        C1C=<C2,                % remove right one
	stronger(C1X,C1C,R1,C2X,C2,R2),	% remove right one if C1C=:= C2
	same_poly(P1,P2,C)
	| 
	true.

%(2) equate opposite inequations
% -1
opp_poly @ eq([X*C1X|P1],C1,'>'(R1)), eq([X*C2X|P2],C2,'>'(R2)) <=> 
	C is C2X/C1X,
	C<0,	                % different sign
        C1C is C1*C,            
        C1C>=C2,                % applicable? 
	same_poly(P1,P2,C)
	| 
	Z is C1C-C2, zero(Z),	% must have identical constants
	(R1)=('='), (R2)=('='),	% fail if one of R's is ('>')
	eq([X*C1X|P1],C1,(=:=)).

%(3) usual equation replacement (like math-gauss.chr)
%  0 
/*
elimin_eager @ eq([X*C2X|PX],C1X,(=:=)) \ eq(P0,C1,R) <=>   % R any relation
	extract(X*C2,P0,P)
	| 
	is_div(C2,C2X,CX), 
	mult_const(eq0(C1X,PX),CX,P2),	
        add_eq0(eq0(C1,P),P2,eq0(C3,P3)),
	sort1(P3,P4),
	eq(P4,C3,R).
*/
elimin_lazy @ eq([X*C2X|PX],C1X,(=:=)) \ eq([X*C2|P],C1,R) <=> 
	is_div(C2,C2X,CX), 
	mult_const(eq0(C1X,PX),CX,P2),	
        add_eq0(eq0(C1,P),P2,eq0(C3,P3)),
	sort1(P3,P4),
	eq(P4,C3,R).

% choose one of the propagation rules below and comment out the others!
% completeness and termination depends on propagation rule used

%(4) propagate, transitive closure of inequations, various versions
% +1
/*
% complete, but too costly, propagate_lazy is as good, can loop
propagate_eager @ eq([X*C2X|PX],C1X,'>'(R1)), eq(P0,C1,'>'(R2)) ==> 
	extract(X*C2,P0,P),
	is_div(C2,C2X,CX),
        CX>0                    % different sign?
	| 
	combine_ineqs(R1,R2,R3),
	mult_const(eq0(C1X,PX),CX,P2),	
        add_eq0(eq0(C1,P),P2,eq0(C3,P3)),
	sort1(P3,P4),
	eq(P4,C3,'>'(R3)).

% complete, may loop
propagate_lazy @ eq([X*C2X|PX],C1X,'>'(R1)), eq([X*C2|P],C1,'>'(R2)) ==> 
	is_div(C2,C2X,CX),
        CX>0                    % different sign?
	| 
	combine_ineqs(R1,R2,R3),
	mult_const(eq0(C1X,PX),CX,P2),	
        add_eq0(eq0(C1,P),P2,eq0(C3,P3)),
	sort1(P3,P4),
	eq(P4,C3,'>'(R3)).	
*/
/*
% incomplete, number of variables does not increase, may loop
propagate_pair @ eq([X*C2X|PX],C1X,'>'(R1)), eq(P0,C1,'>'(R2)) ==> 
	not(PX=[_,_,_|_]),	% single variable or pair of variables only
	extract(X*C2,P0,P),
	is_div(C2,C2X,CX),
        CX>0                    % different sign?
	| 
	combine_ineqs(R1,R2,R3),
	mult_const(eq0(C1X,PX),CX,P2),	
        add_eq0(eq0(C1,P),P2,eq0(C3,P3)),
	sort1(P3,P4),
	eq(P4,C3,'>'(R3)).
*/
% incomplete, is interval reasoning, number of variables decreases, loop free
propagate_single @ eq([X*C2X],C1X,'>'(R1)), eq(P0,C1,'>'(R2)) ==> % single variable only
	(P0=[V*C2|P],V==X ; P0=[VC,V*C2|PP],V==X,P=[VC|PP]), % only first or second variable
	is_div(C2,C2X,CX),
        CX>0                    % different sign?
	| 
	combine_ineqs(R1,R2,R3),
	is_mul(C1X,CX,C1XCX),
	C3 is C1+C1XCX,
	eq(P,C3,'>'(R3)).
/*
% incomplete, ignore inequations until they are sufficiently simplified
%propagate_never @ eq([X*C2X|PX],C1X,'>'(R1)), eq([X*C2|P],C1,'>'(R2)) ==> 
%								fail | true.
*/

% handle nonlinear equations ------------------------------------------------
operator(450,xfx,eqnonlin).
constraints (eqnonlin)/2.
non_linear @ X eqnonlin A   <=> ground(A) | A1 is A, {X=:=A1}.
non_linear @ X eqnonlin A*B <=> ground(A) | A1 is A, {X=:=A1*B}.
non_linear @ X eqnonlin B*A <=> ground(A) | A1 is A, {X=:=A1*B}.


% labeling, useful really only for integers ---------------------------------
%label_with eq([XC],C1,'>'('=')) if true.
%eq([XC],C1,'>'('=')) :- eq([XC],C1,(=:=)) ; eq([XC],C1,'>'('>')).


% auxiliary predicates --------------------------------------------------------

% combine two inequalities
combine_ineqs(('='),('='),('=')):- !.
combine_ineqs(_,_,('>')).

same_poly([],[],C).
same_poly([X*C1|P1],[X*C2|P2],C) ?-
	%X==Y,
	C4 is C*C1-C2, zero(C4),
	same_poly(P1,P2,C).

stronger(C1X,C1C,R1,C2X,C2,R2):-
        C1C=:=C2 -> 
		\+ (R1=('='),R2=('>')),
		C1A is abs(C1X)+1/abs(C1X), C2A is abs(C2X)+1/abs(C2X),
		C1A=<C2A
		; 
		true.


/* end of file math-fougau.chr ----------------------------------------------*/
