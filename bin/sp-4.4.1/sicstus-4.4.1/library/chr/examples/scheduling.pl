% 980312 Thom Fruehwirth, LMU

:- use_module(library(chr)).

% handler scheduling.


:- chr_constraint leq/2, optimize/0.
% leq(X+N,Y) means: task X starts at least N time units before task Y

% assumes leq-relation is non-circular

redundant @ leq(N,Y), leq(M,Y) <=> ground(M), ground(N), M=<N | leq(N,Y).

% optimize gives smallest possible value to a variable

optimize @  optimize#Id \ leq(X,Y) <=> 
	ground(X),var(Y),findall_constraints(Y,leq(_,Y),L),L=[]
        | 
	Y is X 
        pragma passive(Id).

% classical example --------------------------------------
	
build_house([Start,A,B,C,D,E,F,G,H,I,J,End]) :-
	leq(Start+0,A),
	leq(A+7,B),
	leq(A+7,D),
	leq(B+3,C),
	leq(C+1,E),
	leq(D+8,E),
	leq(C+1,G),
	leq(D+8,G),
	leq(D+8,F),
	leq(C+1,F),
	leq(F+1,H),
	leq(H+3,I),
	leq(G+1,J),
	leq(E+2,J),
	leq(I+2,J),
	leq(J+1,End),
	optimize,
	Start=0.	

/*
| ?- build_house([Start,A,B,C,D,E,F,G,H,I,J,End]).

A = 0,
B = 7,
C = 10,
D = 7,
E = 15,
F = 15,
G = 15,
H = 16,
I = 19,
J = 21,
End = 22,
Start = 0,
optimize ? 
*/
	
% end of handler scheduling
