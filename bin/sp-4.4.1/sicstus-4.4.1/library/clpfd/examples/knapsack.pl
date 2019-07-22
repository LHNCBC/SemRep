:- module(knapsack, [knapsack/2]).

% ?- bench([knapsack(1023,1524)]).

:-use_module(library(clpfd)).
:-use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

thedata([2,4,8,16,32,64,128,256,512,1024],
	[2,5,11,23,47,95,191,383,767,1535]).
            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
knapsack(Space,Profit) :-
    thedata(Weights,Costs),
    length(Weights,N),
    length(Vars,N),
    domain(Vars,0,Space),
    scalar_product(Weights,Vars,#=<,Space),
    scalar_product(Costs,Vars,#>=,Profit),
    reverse(Vars,VarsR),
    labeling([enum],VarsR),!,
    write(Vars),
    nl.
knapsack(_,_) :-
    write('No solution.'),
    nl.
          
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end_of_file.
    
all :-
	findall(knapsack(S,P), p(S,P), Benches),
	user:bench([true|Benches]).

p(255, 374).
p(255, 375).
p(511, 757).
p(511, 758).
p(1023,1524).
p(1023,1525).
p(2047,3059).
p(2047,3060).

ex(1, L) :-
	S in 10..12,
	length(L, 4),
	domain(L, 0, 1),
	knapsack([2,3,4,5], L, S).
ex(2, L) :-
	S in 80..82,
	length(L, 4),
	domain(L, 0, 3),
	knapsack([27,37,45,53], L, S).



alphakn(Lab,LD):-
	LD=[A,B,C,_D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z],

	domain(LD,1,26),
	all_different(LD),

	knapsack([1,1,1,1,1,1], [B,A,L,L,E,T], 45),
	knapsack([1,1,1,1,1], [C,E,L,L,O], 43),
	knapsack([1,1,1,1,1,1,1], [C,O,N,C,E,R,T], 74),
	knapsack([1,1,1,1,1], [F,L,U,T,E], 30),
	knapsack([1,1,1,1,1], [F,U,G,U,E], 50),
	knapsack([1,1,1,1], [G,L,E,E], 66),
	knapsack([1,1,1,1], [J,A,Z,Z], 58),
	knapsack([1,1,1,1], [L,Y,R,E], 47),
	knapsack([1,1,1,1], [O,B,O,E], 53),
	knapsack([1,1,1,1,1], [O,P,E,R,A], 65),
	knapsack([1,1,1,1,1], [P,O,L,K,A], 59),
	knapsack([1,1,1,1,1,1,1], [Q,U,A,R,T,E,T], 50),
	knapsack([1,1,1,1,1,1,1,1,1], [S,A,X,O,P,H,O,N,E], 134),
	knapsack([1,1,1,1,1], [S,C,A,L,E], 51),
	knapsack([1,1,1,1], [S,O,L,O], 37),
	knapsack([1,1,1,1], [S,O,N,G], 61),
	knapsack([1,1,1,1,1,1,1], [S,O,P,R,A,N,O], 82),
	knapsack([1,1,1,1,1], [T,H,E,M,E], 72),
	knapsack([1,1,1,1,1,1], [V,I,O,L,I,N], 100),
	knapsack([1,1,1,1,1], [W,A,L,T,Z], 34),
	labeling(Lab,LD).

