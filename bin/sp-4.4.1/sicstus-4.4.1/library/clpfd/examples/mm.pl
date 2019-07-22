%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  FILE
%    mm
%  AUTHOR
%    Greger Ottosson (greger@csd.uu.se)
%    Kristina Sirhuber (kiina@csd.uu.se)
%  PUPRPOSE
%   Implements the game of Mastermind.
%  HISTORY
%    greger - 1994-10-06 : Created.
% 

/* Expected output:
| ?- mm([6,5,6,3,2,3]).

My Guess: [1,1,1,1,1,1]  (Blacks,Whites) = 0,0
My Guess: [2,2,2,2,2,2]  (Blacks,Whites) = 1,0
My Guess: [2,3,3,3,3,3]  (Blacks,Whites) = 2,1
My Guess: [4,2,3,3,4,4]  (Blacks,Whites) = 1,2
My Guess: [5,2,5,5,3,3]  (Blacks,Whites) = 1,3
My Guess: [5,3,2,3,6,6]  (Blacks,Whites) = 1,5
My Guess: [6,5,3,2,3,6]  (Blacks,Whites) = 2,4
My Guess: [6,5,6,3,2,3]  (Blacks,Whites) = 6,0

Correct guess!!!
*/

:- module(mm, [mm/0, mm/1]).

:- use_module(library(clpfd)).
:- use_module(library(lists), [
	select/3
			     ]).

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % This is the main predicate that is called when we start the
 % program. It reads the secret code and  goes into a loop. It always
 % start guessing with a list of ones.
mm :-
	write('Enter the secret code, a list of integers among 1..6: '),
	read(Code),
	mm(Code).

mm(Code) :-
	(   foreach(_,Code),
	    foreach(1,Guess)
	do  true
	),
	colors(MaxColor),
	game_loop(Code, Guess, [], MaxColor).

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % This is the main loop, which starts with one guess, and generates i
 % new one after computing the Blacks and Whites for the old one. It has
 % reached the end when the number Blacks equals the length of the code.
game_loop(Code, CurGuess, OldGuesses, MaxColor) :-
	(   foreach(C,Code),
	    foreach(G1,CurGuess),
	    fromto(0,B0,B,Blacks)
	do  (C=:=G1 -> B is B0+1 ; B = B0)
	),
	(   fromto(Code,Code0,Code1,_),
	    foreach(G2,CurGuess),
	    fromto(0,W0,W,AllWhites)
	do  (   select(G2, Code0, Code1) -> W is W0+1
	    ;   Code1 = Code0,
		W = W0
	    )
	),
	Whites is AllWhites - Blacks,
	format('My Guess: ~w  (Blacks,Whites) = ~w\n', [CurGuess,(Blacks,Whites)]),
	(   length(Code,Blacks)
	->  write('Cracked it.\n')
	;   Guesses = [guess(CurGuess,CurOccs,Blacks,Whites)|OldGuesses],
	    (   foreach(G3,CurGuess),
		foreach(G3-1,L1)
	    do  true
	    ),
	    keysort(L1, L2),
	    keyfuse(L2, CurOccs),
	    (   foreach(guess(OldGuess,OldOccs,OldBlacks,OldWhites),Guesses),
		param([NewGuess,MaxColor])
	    do  (   foreach(O1,OldGuess),
		    foreach(B1,Bs),
		    foreach(N1,NewGuess),
		    param(MaxColor)
		do  N1 in 1..MaxColor,
		    O1 #= N1 #<=> B1
		),
		sum(Bs, #=, OldBlacks),
		new_occs(0, MaxColor, OldOccs, NewOccs, Terms),
		global_cardinality(NewGuess, NewOccs, [consistency(value)]),
		Sum is OldBlacks + OldWhites,
		sum(Terms, #=, Sum)
	    ),
	    lex_chain([CurGuess,NewGuess], [op(#<)]),
	    labeling([], NewGuess)
	->  game_loop(Code, NewGuess, Guesses, MaxColor)
	).


new_occs(NC, NC, [], [], []) :- !.
new_occs(I,  NC, [J-Count|Occs], [J-NewCount|NewOccs], [Term|Terms]) :-
	J is I+1, !,
	Term #= min(NewCount,Count),
	new_occs(J, NC, Occs, NewOccs, Terms).
new_occs(I,  NC, Occs, [J-_|NewOccs], Terms) :-
	J is I+1,
	new_occs(J, NC, Occs, NewOccs, Terms).

keyfuse([], []).
keyfuse([K1-I,K2-J|L1], L2) :- K1==K2, !, IJ is I+J, keyfuse([K1-IJ|L1], L2).
keyfuse([X|L1], [X|L2]) :- keyfuse(L1, L2).

 % The number of colors used.
colors(6).

