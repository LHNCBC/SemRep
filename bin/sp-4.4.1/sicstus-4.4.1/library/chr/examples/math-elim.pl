% math-elim.pl================================================================
% constraint handling rules for linear polynomial (in)equalitions
% thom fruehwirth 910610,911213,920124,930518,931223,940308,950410-11,980312
% 961107 Christian Holzbaur, SICStus mods.

% CHOOSE one of the following elim-* named CHRs for variable elimination
% and comment out the others!

:- use_module( library(chr)).
:- ensure_loaded( 'math-utilities').

handler elim.

% auxiliary constraint to delay a goal G until it is ground
constraints check/1.
check(G) <=> ground(G) | G.

% handle inequalities (introduces slack variables)

constraints {}/1.

{ C,Cs } <=> { C }, { Cs }.

{A =< B}  <=> ground(A),ground(B) | A=<B.
{A >= B}  <=> ground(A),ground(B) | A>=B.
{A < B}   <=> ground(A),ground(B) | A<B.
{A > B}   <=> ground(A),ground(B) | A>B.
{A =\= B} <=> ground(A),ground(B) | A=\=B.

% transform inequations into equations by introducing slack variables
{A =< B}  <=> {A+slack(X) =:= B}, check(X>=0).
{A >= B}  <=> {B+slack(X) =:= A}, check(X>=0).
{A < B}   <=> {A+slack(X) =:= B}, check(X>0).
{A > B}   <=> {B+slack(X) =:= A}, check(X>0).
{A =\= B} <=> {A+      X  =:= B}, check(X=\=0).  

% some quick cases and the general case
{A =:= B} <=> ground(A),ground(B) | X is A-B, zero(X).  % handle imprecision
{A =:= B} <=> var(A),   ground(B) | A is B.
{B =:= A} <=> var(A),   ground(B) | A is B.
{A =:= B} <=> unconstrained(A),var(B) | A=B.
{B =:= A} <=> unconstrained(A),var(B) | A=B.
{A =:= B} <=> normalize(A,B,P,C), equals(P,C).

operator(100,xfx,equals).

constraints (equals)/2. 
% Poly equals Const, where Poly is list of monomials Variable*Coefficient 

% simplify single equation --------------------------------------------------
empty @ [] equals C1 <=> zero(C1).
unify @ [X*C2] equals C1 <=> nonground(X) | is_div(C1,C2,X). % nonzero(X)
simplify @ P0 equals C1 <=> delete(X*C2,P0,P), ground(X) |
	is_mul(X,C2,XC2),
	C is XC2+C1, 
	P equals C.
/*
% use only if you unify variables of equations with each other
% if rule is not used: may loop if variables of the equations are unified
unified @ P0 equals C1 <=> 
	append(P1,[X*C2|P2],P0),var(X),delete(Y*C3,P2,P3),X==Y 
	|
	C23 is C1+C2,
	append(P1,[X*C23|P3],P4),
	sort1(P4,P5),		% needed ?
	P5 equals C1.
*/

% CHOOSE one of the following elim-* CHRs for variable elimination 
% and comment out the others

% eliminate a variable ------------------------------------------------------
% lazy rule to replace a variable or slack (as used in math-lazy.chr)
elim_lazy @ [X*C2X|PX] equals C1X \ [X*C2|P] equals C1 <=> var(X) |
	is_div(C2,C2X,CX), 
	mult_const(eq0(C1X,PX),CX,P2),	
        add_eq0(eq0(C1,P),P2,eq0(C3,P3)),
	sort1(P3,P4),
	P4 equals C3.
/*
% not so lazy rule to replace a variable or slack
% should make all variable bindings explicit
% maybe even less efficient then eager rule?
elim_medium @ [X*C2X|PX] equals C1X \ P0 equals C1 <=> 
	(P0=[Y*C2|P] ; P0=[VC,Y*C2|P1],P=[VC|P1]),
	X==Y
	|
	is_div(C2,C2X,CX), 
	mult_const(eq0(C1X,PX),CX,P2),	
        add_eq0(eq0(C1,P),P2,eq0(C3,P3)),
	sort1(P3,P4),
	P4 equals C3.

% eager rule to replace a variable or slack (as used in math-eager.chr)
elim_eager @ [X*C2X|PX] equals C1X \ P0 equals C1 <=> %var(X) |
	delete(Y*C2,P0,P),X==Y 
	| 
	is_div(C2,C2X,CX), 
	mult_const(eq0(C1X,PX),CX,P2),	
        add_eq0(eq0(C1,P),P2,eq0(C3,P3)),
	P3 equals C3.
*/
/*
% handle slack variables, not complete ---------------------------------------
all_slacks @ P equals C <=> all_slacks(P,PS),sign(C,CS),(CS=0;CS=PS) | 
	CS=0,all_zeroes(P).
*/
% handle slack variables, complete? ------------------------------------------
zero_slacks @ P equals C <=> zero(C),all_slacks(P,_PS) | all_zeroes(P).

first_slack @ [S1*C1|P] equals C <=> nonvar(S1),sign(C,SC),sign(C1,SC1),SC=SC1 |
	(delete(S2*C2,P,P1),sign(C2,SC2),SC2 is -SC ->
	[S2*C2,S1*C1|P1] equals C).

elim_slack @ [X*C2X|PX] equals C1X \ P0 equals C1 <=> % P0 all_slacks, no?
	nonvar(X),  			              % slack variable
	sign(C1X,SC1X),sign(C2X,SC2X),SC2X\==SC1X,    % different sign	
	delete(Y*C2,P0,P),X==Y 
	| 
	is_div(C2,C2X,CX), 
	mult_const(eq0(C1X,PX),CX,P2),	
        add_eq0(eq0(C1,P),P2,eq0(C3,P3)),
	P3 equals C3.			% put P0 first slack first, yes?


% handle nonlinear equations -------------------------------------------------
operator(450,xfx,eqnonlin).
constraints (eqnonlin)/2.
linearize @ X eqnonlin A   <=> ground(A) | A1 is A, {X=:=A1}.
linearize @ X eqnonlin A*B <=> ground(A) | A1 is A, {X=:=A1*B}.
linearize @ X eqnonlin B*A <=> ground(A) | A1 is A, {X=:=A1*B}.

% pretty-print math-portray for equals/2 is defined in math-utilities.pl -----

/* end of file math-elim.pl -----------------------------------------------*/

