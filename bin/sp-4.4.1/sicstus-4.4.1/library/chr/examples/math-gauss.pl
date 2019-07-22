% solving linear polynomial equations by variable elimination a la Gauss
% thom fruehwirth 910610,911213,920124,930602,931223, 980311
% 961107 christian holzbaur for SICStus CHR 
% complete for equalities, leaves equalities implicit, slow
% may loop if variables of the equations are unified

:- use_module(library(chr)).
:- ensure_loaded('math-utilities').		% load auxiliary file

handler gauss.

option(check_guard_bindings, on).  % for delete(X...) in rule eliminate

operator(100,xfx,equals).

constraints (equals)/2.
% Poly equals Const, where Poly is list of monomials Variable*Coefficient 

eliminate(X) @ 
[X*Coeff1|P1] equals C1 \ P equals C2 <=> delete(X*Coeff2,P,P2) | 
	is_div(Coeff2,Coeff1,C), 
	mult_const(eq0(C1,P1),C,eq0(C1C,P1C)),	
        add_eq0(eq0(C2,P2),eq0(C1C,P1C),eq0(C3,P3)),
	P3 equals C3.


constraints {}/1.    
% curly brackets as wrapper to avoid name clash with built-in =:=

split @ { C, Cs } <=> { C }, { Cs }.

normalize @ {A =:= B} <=> 
	normalize(A,B,Poly,Const), 
	Poly equals Const.   	


/*
% uses math_portray pretty print defined in math-utilities.pl

?- {3 * X + 2 * Y - 4 * (3 + Z) =:= 2 * (X - 3) + (Y + Z) * 7 ,      
    2 * (X + Y + Z) =:= 3 * (X - Y - Z) , 
    5 * (X + Y) - 7 * X - Z =:= (2 + 1 + X) * 6}.

-(6*Z)=:=6,
-(35*Y)=:= -23,
X=:= -1.7142857142857144 ? 

?- {3 * X + 2 * Y - 4 * (3 + Z) =:= 2 * (X - 3) + (Y + Z) * 7 ,      
    2 * (X + Y + Z) =:= 3 * (X - Y - Z)}.

-(6*Z)=:=6,
-(5*Y)+X=:= -5 ?
*/

/* end of file gauss.chr ------------------------------------------------*/
