%   File       : wgcf.pl
%   Maintainer : Mats Carlsson
%   Updated    : November 2011
%   Purpose    : FD model of the wolf, goat, cabbage problem
%   Inspired by: SAT model given in the MiniZinc distribution

:- module(wgcf, [wgcf/0]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).

%-----------------------------------------------------------------------------%
% The wolf, goat, cabbage problem
%
% A farmer has to take a wolf, goat and cabbage across a bridge
% He can only take one thing at a time
% The wolf and goat can't be left together alone (without the farmer)
% The goat and cabbage can't be left alone together

% 1..3 represent the three locations:
% 1 is on the left bank of the river
% 2 is on the bridge
% 3 is on the right bank
%-----------------------------------------------------------------------------%

wgcf(Wolf, Goat, Cabbage, Farmer) :-
	(   foreach(Cur, [Wolf, Goat, Cabbage, Farmer])
	do  length(Cur, 15),
	    domain(Cur, 1, 3),
	    % All start on the right bank and finish on the left bank
	    % Things can only move from bank to bridge, or from bridge to bank, in one step
	    % The wolf can be on the bridge only if:
	    % (b) The farmer was previously on the same bank as the wolf
	    % (c) The farmer goes subsequently to the same bank as the wolf
	    % Similarly for the cabbage and goat
	    Cur = [3|_],
	    automaton(Cur, [source(3),sink(1)],
		      [arc(1,1,1), arc(1,2,r2),
		       arc(l2,1,1), arc(r2,3,3),
		       arc(3,2,l2), arc(3,3,3)])
	),
	transpose([Wolf, Goat, Cabbage, Farmer], Quads),
	% redundant constraint: never return to a previous state
	(   foreach(Quad,Quads),
	    foreach(Int,Ints)
	do  scalar_product([27,9,3,1], Quad, #=, Int)
	),
	all_different(Ints),
	findall(Tuple, wgcf_tuple(Tuple), Rel),
	table(Quads, Rel).

% The wolf and goat can't be left together alone (without the farmer)
% The goat and cabbage can't be left alone together
% The wolf can be on the bridge
% only if (a) the farmer is on the bridge, and neither the goat nor the cabbage is
% Similarly for the cabbage and goat
wgcf_tuple(Quad) :-
	Quad = [W,G,C,F],
	domain(Quad, 1, 3),
	(W#=G #=> W#=F),
	(G#=C #=> G#=F),
	(W#=2 #=> G#\=2 #/\ C#\=2 #/\ F#=2),
	(G#=2 #=> C#\=2 #/\ W#\=2 #/\ F#=2),
	(C#=2 #=> G#\=2 #/\ W#\=2 #/\ F#=2),
	labeling([], Quad).

wgcf/*(Wolf, Goat, Cabbage, Farmer)*/ :-
	wgcf(Wolf, Goat, Cabbage, Farmer),
	transpose([Wolf, Goat, Cabbage, Farmer], Quads),
	append(Quads, Vars),
	labeling([enum], Vars), !,
	(   foreach([W,G,C,F],Quads),
	    count(I,1,_)
	do  keysort([W-wolf,G-goat,C-cabbage,F-farmer], L1),
	    keyclumped(L1, L2),
	    Locs = f('Left','Bridge','Right'),
	    (   foreach(Loc-Clump,L2),
		fromto(Args,[Lab,Clump|Args1],Args1,[]),
		param(Locs)
	    do  arg(Loc, Locs, Lab)
	    ),
	    length(Args, N),
	    (   N=:=2 -> format('~d. ~w: ~w.\n', [I|Args])
	    ;   N=:=4 -> format('~d. ~w: ~w, ~w: ~w.\n', [I|Args])
	    ;   N=:=6 -> format('~d. ~w: ~w, ~w: ~w, ~w: ~w.\n', [I|Args])
	    )
	).

