% math-utilities.pl ===========================================================
% auxiliary predicates for math*.pl constraint solvers
% thom fruehwirth 1991-92, revised 930518,931223,940304
% 961030 christian holzbaur, SICStus adaption

:- use_module( library('chr/getval')).
:- use_module( library('chr/matching')).
:- use_module( library('chr/ordering'), [globalize/1,var_compare/3]).


% SETTINGS --------------------------------------------------------------------

% for use in is/2: precision, slack variables, simulated infimum, etc.

% Code works with flag prefer_rationals on or off
% and with float_precision single or double

% adapt precision for zero/1 test
:- ( current_module(eclipse) -> 
	get_flag(float_precision,G)
   ;
	G = double
   ),
   (G==single -> setval(precision,1.0e-06),setval(mprecision,-1.0e-06)
   ;
    G==double -> setval(precision,1.0e-12),setval(mprecision,-1.0e-12)
   ).

slack(X,X).	% :- X>=0.

inf(   3.40282e38).
minf( -3.40282e38).
sup(   1.0e-45).
msup( -1.0e-45).

:- multifile portray/1.

portray( X) :- math_portray( X, Xp), print( Xp).


% PRETTY PRINT ---------------------------------------------------------------

% for math-gauss.pl and math-elim.pl
math_portray(equals(P,C),P1=:=0):- zero(C),!,
	make_poly(P,P1).
math_portray(equals(P,C),P1=:=C1):-!,
	MC is (-C),
	avoid_float(MC,C1),
	make_poly(P,P1).
% for math-fougau.pl
math_portray(eq(P,C,(=:=)),P1=:=C1):-!,
        MC is (-C),
        avoid_float(MC,C1),
        make_poly(P,P1).
math_portray(eq(P,C,'>'('=')),P1>=C1):-!,
        MC is (-C),
        avoid_float(MC,C1),
        make_poly(P,P1).
math_portray(eq(P,C,'>'('>')),P1>C1):-!,
        MC is (-C),
        avoid_float(MC,C1),
        make_poly(P,P1).
% for all three math*pl solvers
math_portray(eqnonlin(X,(E)),X=:=E):-!.


make_poly([],0).
make_poly([X*C],-CX):- C<0,!,
	C1 is (-C),
	avoid_float(C1,C2),
	make_mono(C2,X,CX).
make_poly([X*C],CX):-!,
	avoid_float(C,C1),
	make_mono(C1,X,CX).
make_poly([X*C|P],P1-CX):- C<0,!,
	C1 is (-C),
	avoid_float(C1,C2),
	make_mono(C2,X,CX),
	make_poly(P,P1).
make_poly([X*C|P],P1+CX):-
	avoid_float(C,C1),
	make_mono(C1,X,CX),
	make_poly(P,P1).

make_mono(C,X,CX):- nonvar(X),X=slack(Y),!,make_mono(C,Y,CX).
make_mono(C,X,CX1):- nonvar(X),number(X),!,CX is C*X,avoid_float(CX,CX1).
make_mono(1,X,X):-!.
% make_mono(1_1,X,X):-!.
make_mono(C,X,C*X).


% AUXILIARY PREDICATES -------------------------------------------------------

nonground( X) :- ground( X), !, fail.
nonground( _).

%
% sort X*K,slack(_)*K with globalized Xs
%
sort1(A,B):-
	msort(A,C),
	((C=[X*_|_],nonvar(X),X=slack(_))->A=B;B=C). % slacks unordered why?

msort( L, S) :-
	length( L, Len),
	msort( Len, L, [], S).

msort( 0, L,     L, []) :- !.
msort( 1, [X|L], L, [X]) :- !.
msort( N, L0, L2, S) :-
	P is N>>1,
	Q is N-P,
	msort( P, L0, L1, Sp),
	msort( Q, L1, L2, Sq),
	merge( Sp, Sq, S).

merge( [], B, B) :- !.
merge( A, [], A) :- !.
merge( [A|As], [B|Bs], Res) :-
	cmp( R, A, B),
	merge( R, A, As, B, Bs, Res).

merge( =, A, As, _, Bs, [A|Rest]) :- merge( As, Bs,     Rest).
merge( <, A, As, B, Bs, [A|Rest]) :- merge( As, [B|Bs], Rest).
merge( >, A, As, B, Bs, [B|Rest]) :- merge( [A|As], Bs, Rest).

cmp( R, X, Y) :- var(X), var(Y), !, var_compare( R, X, Y).
cmp( R, X, _) :- var(X), !, R = (<).
cmp( R, _, Y) :- var(Y), !, R = (>).
cmp( R, X, Y) :-
	functor( X, Fx, Ax),
	functor( Y, Fy, Ay),
	compare( Rr, Ax/Fx, Ay/Fy),
	( Rr = (=),
	  Ax > 0 ->
	    cmp_args( 1,Ax, X, Y, R)
	;   
	    R = Rr
        ).

cmp_args( N,M, _, _, R) :- N>M, !, R = (=).
cmp_args( N,M, X, Y, R) :-
	arg( N, X, Ax),
	arg( N, Y, Ay),
	cmp( Rr, Ax, Ay),
	( Rr = (=) ->
	    N1 is N+1,
	    cmp_args( N1,M, X, Y, R)
	;
	    R = Rr
	).


rev([],L,L).
rev([X|L1],L2,L3):- rev(L1,[X|L2],L3).

extract(X*C2,P0,P) ?- delete(Y*C2,P0,P),X==Y,!.

delete( X, [X|L],  L).
delete( Y, [X|Xs], [X|Xt]) :-
	delete( Y, Xs, Xt).

zero( slack(S)) ?- !, zero( S).
zero(C):-    
    float(C) -> 
	getval(precision,P),
	getval(mprecision,MP),
	MP < C,    % cope with imprecision
	C < P      
    ;
	C=:=0.

nonzero(C):- zero(C), !, fail.
nonzero(_).

unwrap( slack(S), X) ?- !, X=S.
unwrap( X,        X).

is_div( C1, C2, C3) :-
	unwrap( C1, C11),
	unwrap( C2, C21),
	unwrap( C3, C31),
	is_divu( C11, C21, C31).

is_divu(C1,C2,C3):- zero(C1),!,C3=0.
is_divu(C1,C2,C3):- X is -(C1/C2),  % minus here to get sign needed in handlers
	avoid_float(X,C3).

is_mul( C1, C2, C3) :-
	unwrap( C1, C11),
	unwrap( C2, C21),
	unwrap( C3, C31),
	is_mulu( C11, C21, C31).

is_mulu(C1,C2,C3):- zero(C1),!,C3=0.
is_mulu(C1,C2,C3):- zero(C2),!,C3=0.
is_mulu(C1,C2,C3):- X is C1*C2, 
	avoid_float(X,C3).

avoid_float(X,C3):-
	float(X) -> Y is round(X),Z is X-Y,(zero(Z)-> C3 is integer(Y);C3=X) ; C3=X.


simplifyable(X*C,P,P1):- delete(X*C,P,P1),ground(X),!.


% HANDLING SLACK VARIABLES ----------------------------------------------------

all_slacks([]).
all_slacks([slack(Sl)*C|P]) ?-			% check_slack(Sl),
	all_slacks(P).

all_slacks([],_).
all_slacks([slack(Sl)*C|P],S) ?-		% check_slack(Sl),
	sign(C,S),
	all_slacks(P,S).

check_slack( S) :- find_constraint( S, basic(_)#_), !.
check_slack( _) :- raise_exception( slack).

sign(C,0):- zero(C),!.
sign(C,S):- C>0 -> S=1 ; S=(-1).

all_zeroes([]).
all_zeroes([slack(0)*C|P]) :-
	all_zeroes(P).


% COMPUTING WITH POLYNOMIALS -------------------------------------------------

% gets rounded constant C from is_div/3
mult_const(eq0(C1,P1),C,eq0(0 ,[])):- C=:=0,!.
mult_const(eq0(C1,P1),C,eq0(C1,P1)):- C=:=1,!.
mult_const(eq0(C1,P1),C2,eq0(C,P)):-
	(zero(C1) -> C=0 ; C is C1*C2),
	mult_const1(P1,C2,P).
 mult_const1([],C,[]).
 mult_const1([Xi*Ci|Poly],C,PolyR):-
	(zero(Ci) -> PolyR=NPoly ; NCi is Ci*C,PolyR=[Xi*NCi|NPoly]),
	mult_const1(Poly,C,NPoly).

% gets input from const_mult/3
add_eq0(eq0(C1,P1),eq0(C2,P2),eq0(C,P0)):-
	Ci is C1+C2,
	(zero(Ci) -> C=0 ; C=Ci),
	add_eq1(P1,P2,P0).
%	sort(P,P0).
 add_eq1([],Poly,Poly):-!.
 add_eq1(Poly,[],Poly):-!.
 add_eq1([Xi1*Ci1|Poly1],Poly21,Poly):-
	delete(Xi2*Ci2,Poly21,Poly2),Xi2==Xi1,
	!,
	Ci is Ci1+Ci2,
	(zero(Ci) -> Poly=Poly3 ; Poly=[Xi1*Ci|Poly3]),
	add_eq1(Poly1,Poly2,Poly3).
 add_eq1([Xi1*Ci1|Poly1],Poly2,[Xi1*Ci1|Poly3]):-
	add_eq1(Poly1,Poly2,Poly3).



normalize(A,B,P2,C1):-
        normalize1(A-B,P),
	P=eq0(C1,P1),rev(P1,[],P1R),globalize(P1R),
	sort1(P1,P2).                                 

 normalize1(V,P) ?- var(V),!,
	P=eq0(0,[V*1]).
 normalize1(C,P) ?- ground(C),!,
	C1 is C,P=eq0(C1,[]).
 normalize1(slack(V),P) ?- !,
	P=eq0(0,[slack(V)*1]).
 normalize1((+E),P) ?-!,
	normalize1(E,P).
 normalize1((-E),P) ?-!,
	normalize1(E,P1),
	mult_const(P1,(-1),P).
 normalize1(A*B,C) ?- ground(A),!,
	normalize1(B,BN),
	mult_const(BN,A,C).
 normalize1(B*A,C) ?- ground(A),!,
	normalize1(B,BN),
	mult_const(BN,A,C).
 normalize1(B/A,C) ?- ground(A),!,
	normalize1(B,BN),
	A1 is 1/A,
	mult_const(BN,A1,C). 
 normalize1(A-B,C) ?- !,
	normalize1(A,AN),
	normalize1((-B),BN),
	add_eq0(AN,BN,C).
 normalize1(A+B,C) ?- !,
	normalize1(A,AN),
	normalize1(B,BN),
	add_eq0(AN,BN,C).
 normalize1(E,C) ?-
	C=eq0(0,[CX*1]),
	eqnonlin(CX,E).     % add a nonlinear equation constraint


% end of file math-utilities.pl -----------------------------------------------
