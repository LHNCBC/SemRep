/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Configuration Problem
 * Author    : Mats Carlsson
 * 
 * There are two types of alveoles and four types of cards.  We want to plug
 * in all cards using at most 5 alveoles at minimal cost.
 * 
 * alveole type	power	slots	price
 * 1		0	0	0
 * 2		150	8	150
 * 3		200	16	200
 * 
 * card type	power	number
 * 1		20	10
 * 2		40	4
 * 3		50	2
 * 4		75	1
 */


:- module(config, [config/2]).

:- use_module(library(clpfd)).

config(bb, Lab) :-
	problem(Assignment, Cost, Alveoles, StaticOrder),
	append(Alveoles, StaticOrder, Variables),
	labeling([minimize(Cost)|Lab], Variables),
	writeq(Assignment-Cost),
	nl.
config(restart, Lab) :-
	problem(Assignment, Cost, Alveoles, StaticOrder),
	minimize(config_labeling(Lab,Alveoles,StaticOrder), Cost),
	writeq(Assignment-Cost),
	nl.

config_labeling(Lab, Alveoles, StaticOrder) :-
	labeling([], Alveoles),
	labeling(Lab, StaticOrder).

problem(Assignment, Cost, Alveoles, StaticOrder) :-
	Alveoles = [_A1,_A2,_A3,_A4,_A5],
	domain(Alveoles, 1, 3),	% alveole types
	domain_and_sum([C11,C21,C31,C41,C51], 10),
	domain_and_sum([C12,C22,C32,C42,C52], 4),
	domain_and_sum([C13,C23,C33,C43,C53], 2),
	domain_and_sum([C14,C24,C34,C44,C54], 1),
	Cards1I = [C11,C12,C13,C14],
	Cards2I = [C21,C22,C23,C24],
	Cards3I = [C31,C32,C33,C34],
	Cards4I = [C41,C42,C43,C44],
	Cards5I = [C51,C52,C53,C54],
	AllCards = [Cards1I,Cards2I,Cards3I,Cards4I,Cards5I],
	StaticOrder = [C14,C24,C34,C44,C54,C13,C23,C33,C43,C53,C12,C22,C32,C42,C52,C11,C21,C31,C41,C51],
	(   foreach(Cards,AllCards),
	    foreach(Alv1,Alveoles),
	    foreach(Cap,Slots),
	    foreach(Pow,Powers),
	    foreach(S,Assignment)
	do  Cap in 0..16,
	    slots(Alv1, Cap),
	    sum(Cards, #=<, Cap),
	    Pow in 0..200,
	    power(Alv1, Pow),
	    scalar_product([20,40,50,75], Cards, #=<, Pow),
	    S =.. [alv|Cards]
	),
				% break symmetries, redundant #1
	(   fromto(Alveoles,[X,Y|R],[Y|R],[_])
	do  X #=< Y
	),
	% order(Alveoles, [C11,C21,C31,C41,C51]), % did not help
	sum(Slots, #>=, 17),			% redundant #2
	sum(Powers, #>=, 535),			% redundant #3
	(   foreach(Alv2,Alveoles),
	    foreach(Cost1,Costs)
	do  Cost1 in 0..200,
	    cost(Alv2, Cost1)
	),
	sum(Costs, #=, Cost).

domain_and_sum(Vars, Sum) :-
	domain(Vars, 0, Sum),
	sum(Vars, #=, Sum).

slots(X, Y) +: element(X, [0,8,16], Y).

power(X, Y) +: element(X, [0,150,200], Y).

cost(X, Y) +: element(X, [0,150,200], Y).

% Idea borrowed from Gert's program didn't help
% order([_], _) :- !.
% order([Ai,Aj|Ajs], [Ci1,Cj1|Cj1s]) :- 
% 	Ai #=< Aj, 
% 	Ai #= Aj #=> Ci1 #>= Cj1,
% 	order([Aj|Ajs], [Cj1|Cj1s]).

end_of_file.

% genuine B&B search: 50 msec, 8 backtracks (9 nodes)
| ?- config_bb([],Ass,Cost), statistics(runtime,R), fd_statistics.
Tells detecting entailment: 107
Tells pruning: 253
Tells failing: 8
Total tells: 367
Asks detecting entailment: 17
Total asks: 89
Constraints created: 48

R = [113364300,50],
Ass = [alv(0,0,0,0),alv(0,0,0,0),alv(7,0,0,0),alv(2,4,0,0),alv(1,0,2,1)],
Cost = 550 ? 

yes

% B&B search with restart: 40 msec, 4 backtracks (5 nodes)
| ?- config_restart([],Ass,Cost), statistics(runtime,R), fd_statistics.
Tells detecting entailment: 128
Tells pruning: 275
Tells failing: 4
Total tells: 403
Asks detecting entailment: 21
Total asks: 96
Constraints created: 48

R = [113364530,40],
Ass = [alv(0,0,0,0),alv(0,0,0,0),alv(7,0,0,0),alv(2,4,0,0),alv(1,0,2,1)],
Cost = 550 ? 

yes
