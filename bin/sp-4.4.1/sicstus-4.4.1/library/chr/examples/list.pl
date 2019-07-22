% 931129 ECRC, 980312 LMU thom fruehwirth
% 961106 Christian Holzbaur, SICStus mods

:- use_module( library(chr)).

% handler list.

:- chr_constraint eqlist/2, lenlist/2.
:- op(700,xfx,eqlist).
:- op(700,xfx,lenlist).

% Rs eqlist L: Rs is a list of lists, whose concatentation is the single list L

[] eqlist L <=> L=[].
[R] eqlist L <=> R=L.
[R|Rs] eqlist [] <=> R=[], Rs eqlist [].
[[X|R]|Rs] eqlist L <=> L=[X|L1], [R|Rs] eqlist L1.
Rs eqlist L <=> delete(R,Rs,Rs1),R==[] | Rs1 eqlist L.
Rs eqlist L <=> delete(R,Rs,Rs1),R==L | Rs1 eqlist [].

:- chr_constraint labeling/0.

labeling, ([R|Rs] eqlist L)#Ph <=> true |
	(var(L) -> length(L,_) ; true),
	(
	    R=[], Rs eqlist L
	;
	    L=[X|L1], R=[X|R1], [R1|Rs] eqlist L1
	),
	labeling
    pragma passive(Ph).


% L lenlist N: The length of the list L is N
% N can be an arithmetic expression

[]    lenlist N <=> true | (var(N) -> N=0 ; N=:=0).
[_|L] lenlist N <=> positive(N), plus(M,1,N), L lenlist M.
L     lenlist N <=> ground(N) | length(L,N). 


% auxiliary predicates ---------------------------------------------------

delete( X, [X|L],  L).
delete( Y, [X|Xs], [X|Xt]) :-
	delete( Y, Xs, Xt).

length([],0).
length([_|L],N1):- length(L,N), N1 is N+1.

:- block plus(-,-,?), plus(-,?,-), plus(?,-,-).
%
plus( A, B, C) :- var(C), !, C is A+B.
plus( A, B, C) :- var(B), !, B is C-A.
plus( A, B, C) :- var(A), !, A is C-B.
plus( A, B, C) :- C is A+B.

:- block positive(-).
%
positive( X) :- X>0.


% EXAMPLES ================================================================

% Inspired by LISTLOG, Z. Farkas, TAPSOFT 87, Pisa, Italy
% these predicates have better (more fair) enumeration properties

chr_member(X,L):- [_,[X],_] eqlist L.

chr_append(L1,L2,L3):- [L1,L2] eqlist L3.

chr_last(L,X):- [_,[X]] eqlist L.

/*
[6]: chr_member(1,L),chr_member(2,L),labeling.

L = [1, 2]     More? (;) 

L = [2, 1]     More? (;) 

L = [1, 2, _g1240]     More? (;) 

L = [1, _g1062, 2]     More? (;) 

L = [2, 1, _g1240]     More? (;) 

L = [2, _g1062, 1]     More? (;) 

[7]: member(1,L),member(2,L). % compare with usual member/2

L = [1, 2|_g282]     More? (;) 

L = [1, _g280, 2|_g288]     More? (;) 
 
L = [1, _g280, _g286, 2|_g294]     More? (;) 
*/

palindrome([]).
palindrome([X]).
palindrome(L):-
	X lenlist 1,
	[X,L1,X] eqlist L,
	palindrome(L1).


reverse([],[]).
reverse(R,L):-
	R lenlist N,
	L lenlist N,
	X lenlist 1,
	[X,R1] eqlist R,
	[L1,X] eqlist L,
	reverse(R1,L1).

/*
[19]: reverse(X,[a,b]).

X = [b, a]     		% does not loop like usual reverse/2
[10]: reverse([a,b|L],R).

L = []
R = [b, a]     More? (;) 

L = [_m1718]
R = [_m1718, b, a]     More? (;) 

L = [_m1718, _m2218]
R = [_m2218, _m1718, b, a]     More? (;) 


[11]: reverse(R,[a,b|L]).

R = [b, a]
L = []     More? (;) 

R = [_m754, b, a]
L = [_m754]     More? (;) 

R = [_m754, _m1274, b, a]
L = [_m1274, _m754]     More? (;) 


*/


% Done myself (thom)

permute([],[]).
permute(R,L):-
	R lenlist N,
	L lenlist N,
	X lenlist 1,
	[X,R1] eqlist R,
	[A,X,B] eqlist L,
	[A,B] eqlist L1,
	permute(R1,L1).

/*
[10]: permute(A,B).

A = []
B = []     More? (;) 

A = [_m970]
B = [_m970]     More? (;) 

A = [_m970, _m1994]
B = [_m2392, _m2416]
 
Constraints:
[_m946, [_m970], _m994] eqlist [_m2392, _m2416]
[_m946, _m994] eqlist [_m1994]
     More? (;) 

A = [_m970, _m1994, _m3194]
B = [_m3948, _m3972, _m3996]
 
Constraints:
[_m1970, [_m1994], _m2018] eqlist [_m3592, _m3616]
[_m946, _m994] eqlist [_m3592, _m3616]
[_m946, [_m970], _m994] eqlist [_m3948, _m3972, _m3996]
[_m1970, _m2018] eqlist [_m3194]
     More? (;) 


[11]: permute(A,B),labeling.

A = []
B = []     More? (;) 

A = [_m976]
B = [_m976]     More? (;) 

A = [_m976, _m2000]
B = [_m976, _m2000]     More? (;) 
 
A = [_m976, _m2000]
B = [_m2000, _m976]     More? (;) 

A = [_m976, _m2000, _m3200]
B = [_m976, _m2000, _m3200]     More? (;) 

A = [_m976, _m2000, _m3200]
B = [_m2000, _m976, _m3200]     More? (;) 

A = [_m976, _m2000, _m3200]
B = [_m2000, _m3200, _m976]     More? (;) 

A = [_m976, _m2000, _m3200]
B = [_m976, _m3200, _m2000]     More? (;) 

A = [_m976, _m2000, _m3200]
B = [_m3200, _m976, _m2000]     More? (;) 

A = [_m976, _m2000, _m3200]
B = [_m3200, _m2000, _m976]     More? (;) 

*/


% From Cohen, Koiran, Perrin "Meta-Level Interpretation of CLP(Lists)"
% in "CLP: Selected Research", eds Benhamou, Colmerauer, MIT Press 1993.

% tree(Preorder,Postorder,Tree).
tree([A],[A],A):- freeze(A,atomic(A)).
tree(Pre,Post,t(A,L,R)):-
%	Pre lenlist N,
%	Post lenlist N,
	[[A],X,Y] eqlist Pre,
	[Z,W,[A]] eqlist Post,
	tree(X,Z,L),
	tree(Y,W,R).

/*
[50]: tree([a, b, b, a, a], [b, a, a, b, a], T).

T = t(a, b, t(b, a, a))   
*/

% Inspired by talk by A. Colmerauer, WCLP Marseille, March 1993

transpose([],L):- [L,[[]]] eqlist [[]|L].	 % list of []'s
transpose([X|R],L):- first_column(L,X,L1), transpose(R,L1).

first_column([],[],[]).
first_column([[X|L]|R],[X|S],[L|T]):- first_column(R,S,T).

/*
[36]: transpose([[], [], [], []], L_g85).

L = []

[37]: transpose(L_g69, [[], [], [], []]).

L = []    

*/

/*
[18]:  [X,Y,Z,Z,Y,X] eqlist [a,b,b,c,c,c,c,c,c,b,b,a], labeling.

Z = [c, c, c]
Y = [b, b]
X = [a]     

[21]: [[a],X,[b],Y] eqlist L,
              [Y,[b],X,[a]] eqlist L    .

Y = Y_m654
X = X_m630
L = [a|_m678]
 
Constraints:
(3) [X_m630, [b], Y_m654] eqlist _m678
(4) [Y_m654, [b], X_m630, [a]] eqlist [a|_m678]


[4]: [[a],X,[b],Y] eqlist L,
     [Y,[b],X,[a]] eqlist L, labeling.

Y = [a]
X = []
L = [a, b, a]     More? (;) 

Y = [a]
X = [b]
L = [a, b, b, a]     More? (;) 

Y = [a, b, a]
X = []
L = [a, b, a, b, a]     More? (;) 

Y = [a, a]
X = [a]
L = [a, a, b, a, a]     More? (;) 

Y = [a]
X = [b, b]
L = [a, b, b, b, a]     More? (;) 

Y = [a]
X = [b, b, b]
L = [a, b, b, b, b, a]     More? (;) 

*/


/*

% Unsolvable equation
{2]: [[2],X] eqlist L,
     [X,[1]] eqlist L, 
     labeling.  
% if there is no more solution for longer lists L, labeling does not terminate

% Unsolvable equation from dissertation of J.-P. Pecuchet, 1981
[5]: [[2],X,Y,[1]] eqlist L,
     [X,[1],[2],X] eqlist L, 
     labeling.  
% if there is no more solution for longer lists L, labeling does not terminate

% Solvable equation from paper by K. Schulz, 1988
[11]: [[1],X,[2],Z,X] eqlist L,
      [Z,[3],Z,Y,Y,Y] eqlist L, 
      labeling.

X = [3, 1, 2, 1, 3, 1]
Z = [1]
Y = [2, 1, 3, 1]
L = [1, 3, 1, 2, 1, 3, 1, 2, 1, 3, 1, 2, 1, 3, 1]     More? (;) 

X = [A, 3, 1, A, 2, 1, A, A, 3, 1, A]
Z = [1, A]
Y = [2, 1, A, A, 3, 1, A]
L = [1, A, 3, 1, A, 2, 1, A, A, 3, 1, A, 2, 1, A, A, 3, 1, A, 2, 1, A, A, 3, 1, A]     More? (;) 

L = [1,_A,_B,3,1,_A,_B,2,1,_A|...],
X = [_A,_B,3,1,_A,_B,2,1,_A,_B|...],
Y = [2,1,_A,_B,_A,_B,3,1,_A,_B],
Z = [1,_A,_B],

etc.

% Solvable equation from talk by A. Colmerauer, WCLP Marseille, March 1993
[13]: X=[1,2,3,2,1],
      [X,[1]] eqlist L1, [[U],Y,[U,U]] eqlist L1,
      [Y,[2]] eqlist L2, [[V],Z,[V,V]] eqlist L2,
      labeling.

X = [1, 2, 3, 2, 1]
U = 1
L1 = [1, 2, 3, 2, 1, 1]
Y = [2, 3, 2]
Z = [3]
V = 2
L2 = [2, 3, 2, 2]    

*/


% end of handler list

