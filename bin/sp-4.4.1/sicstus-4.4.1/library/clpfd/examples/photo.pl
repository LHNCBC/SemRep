%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  FILE
%    photo
%  AUTHOR
%    Greger Ottosson (greger@csd.uu.se)
%  HISTORY
%    greger - 1996-05-24 : Created.
%			   (Adapted from Oz program)
% 

:- module(photo, [photo/5]).
:- use_module(library(clpfd)).

% [min] is the best heuristic, bb the best optimization scheme.
% Consistency = domain or bound are good, value is bad.
photo(bb, Lab, Size, NextTo, Consistency) :-
	problem(Size, NextTo, L, Benefit, Consistency),
	labeling([maximize(Benefit)|Lab], L),
	writeq(L-Benefit),
	nl.
photo(iter, Lab, Size, NextTo, Consistency) :-
	problem(Size, NextTo, L, Benefit, Consistency),
	labeling([down],[Benefit]),
	labeling(Lab, L),
	writeq(L-Benefit),
	nl.

problem(Size, NextTo, L, Benefit, Consistency) :-
	L = [Alice,Bert|_],
	problem(Size, L, Pairs),
	domain(L, 1, Size),
	(   foreach([X,Y],Pairs),
	    foreach(B,Bs),
	    param(NextTo)
	do  (   NextTo=:=1 -> D #= X-Y, D in -1..1 #<=> B
	    ;   NextTo=:=2 -> abs(X-Y) #=< 1 #<=> B
	    ;   NextTo=:=3 -> X#=Y+1 #\ Y#=X+1 #<=> B
	    )
	),
	sum(Bs, #=, Benefit),
	Alice #< Bert,
	all_different(L, [consistency(Consistency)]).

problem(5, [Alice,Bert,Chris,Deb,Evan], 
	  [[Alice,Chris], [Bert,Evan],
	   [Chris,Deb],   [Chris,Evan],  
	   [Deb,Alice],   [Deb,Evan],
	   [Evan,Alice],  [Evan,Bert]]).
problem(7, [Alain,Beatrice,Christian,Daniel,Eliane,Francois,Gerard],
	[[Beatrice,Gerard],
	 [Beatrice,Eliane],
	 [Beatrice,Christian],
	 [Francois,Eliane],
	 [Francois,Daniel],
	 [Francois,Alain],
	 [Alain,Daniel],
	 [Gerard,Christian]]).
problem(9, [A0,A1,A2,A3,A4,A5,A6,A7,A8],
	   [[A0,A2], [A0,A4], [A0,A7], [A1,A4], [A1,A8], [A2,A3], [A2,A4], [A3,A0], [A3,A4],
	    [A4,A5], [A4,A0], [A5,A0], [A5,A8], [A6,A2], [A6,A7], [A7,A8], [A7,A6]]).
