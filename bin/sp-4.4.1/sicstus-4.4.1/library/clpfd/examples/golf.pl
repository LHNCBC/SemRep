/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Social Golfer Problem
 * Author    : Mats Carlsson
 *
 * We have 32 golfers, individual play.
 * We will golf for W weeks.
 * Set up the foursomes so that each person only golfs with the same
 * person once.
 */

% Best luck so far:
% | ?- golf(8,4,9,[min],bycolall).

:- module(golf, [golf/6]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).

golf(G, S, W, LabelOpt, VarOrder, Consistency) :-
	Opt = [consistency(Consistency)],
	golfer(G, S, W, Schedule, Byrow, Bycol, Opt),
	var_order(VarOrder, Byrow, Bycol, All),
	statistics(runtime, [T1,_]),
	(   (   foreach(Set,All),
		param(LabelOpt)
	    do  labeling(LabelOpt, Set)
	    )
	;   statistics(runtime, [T2,_]),
	    format('[labeling failed in ~d msec]', [T2-T1]),
	    flush_output,
	    fail
	),
	(   foreach(Round,Schedule),
	    count(Wk,1,_)
	do  format('Week ~d:\n', [Wk]),
	    (   foreach(Four,Round)
	    do  format('                    ~d ~d ~d ~d\n', Four)
	    )
	).

var_order(bycol, _, All, All).
var_order(byrow, All, _, All).
var_order(bycolall, _, Cols, [All]) :-
	append(Cols, All).
var_order(byrowall, Rows, _, [All]) :-
	append(Rows, All).

/* golfer(Number of Groups, GroupSize, NumberOfWeeks, ...) */
golfer(G, S, W, Schedule, PlayersByRow, PlayersByCol, Opt) :-
	schedule(0, G, S, W, Schedule, PlayersByRow, PlayersByCol, Opt),
	Schedule = [FirstS|RestS],
	append(FirstS, Players),
	labeling([enum], Players), !,
	(   foreach(Week,RestS),
	    param(S)
	do  (   foreach([P|Ps],Week),
		param(S)
	    do  P/S #= Q0,
		(   foreach(P1,Ps),
		    fromto(Q0,Q1,Q2,_),
		    param(S)
		do  P1/S #= Q2,
		    Q1 #< Q2
		)
	    ),
	    seed_week(0, S, Week)
	),
	ordered_players_by_week(PlayersByRow),
	players_meet_disjoint(Schedule, G, S, Opt),
	first_s_alldiff(0, S, RestS, Opt).


schedule(W, _, _, W, [], [], [], _) :- !.
schedule(I, G, S, W, [Week|Schedule], [ByRow|ByRows], [ByCol|ByCols], Opt) :-
	(   for(_,1,G),
	    foreach(Group,Week),
	    param([G,S])
	do  length(Group, S),
	    GS is G*S-1,
	    domain(Group, 0, GS)
	),
	append(Week, ByRow),
	all_different(ByRow, Opt),
	transpose(Week, WeekT),
	append(WeekT, ByCol),
	J is I+1,
	schedule(J, G, S, W, Schedule, ByRows, ByCols, Opt).

/* Version based on disjoint2/1. */

% players_meet_disjoint(Schedule, _, _) :-
% 	append(Schedule, Groups),
% 	groups_meets(Groups, Meets, []),
% 	disjoint2(Meets).

% groups_meets([]) --> [].
% groups_meets([Group|Groups]) -->
% 	group_meets(Group),
% 	groups_meets(Groups).

% group_meets([]) --> [].
% group_meets([P|Ps]) -->
% 	group_meets(Ps, P),
% 	group_meets(Ps).

% group_meets([], _) --> [].
% group_meets([Q|Qs], P) --> [r(P,1,Q,1)],
% 	group_meets(Qs, P).

/* Arc-consistent version. */

players_meet_disjoint(Schedule, G, S, Opt) :-
	append(Schedule, Groups),
	groups_meets(Groups, Tuples, [], MeetVars, []),
	GS is G*S,
	(   foreach([A,B,C],Tuples),
	    param([GS,Opt])
	do  scalar_product([GS,1], [A,B], #=, C, Opt)
	),
	all_distinct(MeetVars, Opt).

groups_meets([], Tuples, Tuples) --> [].
groups_meets([Group|Groups], Tuples1, Tuples3) -->
	group_meets(Group, Tuples1, Tuples2),
	groups_meets(Groups, Tuples2, Tuples3).

group_meets([], Tuples, Tuples) --> [].
group_meets([P|Ps], Tuples1, Tuples3) -->
	group_meets(Ps, P, Tuples1, Tuples2),
	group_meets(Ps, Tuples2, Tuples3).

group_meets([], _, Tuples, Tuples) --> [].
group_meets([Q|Qs], P, [[P,Q,PQ]|Tuples1], Tuples2) --> [PQ],
	group_meets(Qs, P, Tuples1, Tuples2).

seed_week(S, S, Week) :- !,
	S1 is S-1,
	seed_week(Week, S1).
seed_week(I, S, [[I|_]|Week]) :-
	J is I+1,
	seed_week(J, S, Week).

seed_week([], _).
seed_week([[J|_]|Week], I) :-
	I #< J,
	seed_week(Week, J).

ordered_players_by_week([W|Ws]) :-
	ordered_players_by_week(Ws, W).

ordered_players_by_week([], _).
ordered_players_by_week([W|Ws], V) :-
	W = [_,Y|_],
	V = [_,X|_],
	X #< Y,
	ordered_players_by_week(Ws, W).

first_s_alldiff(S, S, _Schedule, _) :- !.
first_s_alldiff(I, S, Schedule, Opt) :-
	(   foreach(Week,Schedule),
	    foreach(Ith,Part),
	    param(I)
	do  nth0(I, Week, [_|Ith])
	),
	append(Part, Conc),
	all_different(Conc, Opt),
	J is I+1,
	first_s_alldiff(J, S, Schedule, Opt).

% [PM] 4.3.1 avoid unused-predicate warning.
:- public dyn_solution/1.

% Posted in comp.constraints
:- dynamic dyn_solution/1.
dyn_solution([[[ 0, 1, 2, 3],[ 4, 5, 6, 7],[ 8,9,10,11],[12,13,14,15],[16,17,18,19],[20,21,22,23],[24,25,26,27],[28,29,30,31]] ,
	      [[ 0, 4,12,25],[ 1, 7, 8,30],[ 2, 6,9,29],[ 3,5,14,27],[10,19,20,28],[11,17,22,31],[13,18,23,26],[15,16,21,24]],
	      [[ 0, 5,10,31],[ 1, 6,13,24],[ 2, 7,15,26],[ 3, 4,11,28],[ 8,16,23,29],[9,18,21,30],[12,17,20,27],[14,19,22,25]],
	      [[ 0, 6,16,20],[ 1,10,12,23],[ 2, 5,17,21],[ 3,9,15,22],[ 4,19,24,29],[ 7,18,27,31],[ 8,13,25,28],[11,14,26,30]],
	      [[ 0, 7,17,23],[ 1,11,15,20],[ 2, 8,19,27],[ 3,10,16,26],[ 4,13,21,31],[ 5,18,25,29],[ 6,12,22,30],[9,14,24,28]],
	      [[ 0, 8,14,21],[ 1, 4,18,22],[ 2,11,16,25],[ 3, 7,12,24],[ 5,13,20,30],[ 6,17,26,28],[9,19,23,31],[10,15,27,29]],
	      [[ 0,9,13,27],[ 1, 5,19,26],[ 2,20,24,31],[ 3,23,25,30],[ 4, 8,15,17],[ 6,10,14,18],[ 7,16,22,28],[11,12,21,29]],
	      [[ 0,11,18,24],[ 1,9,17,25],[ 2,10,13,22],[ 3, 6,19,21],[ 4,16,27,30],[ 5,15,23,28],[ 7,14,20,29],[ 8,12,26,31]],
	      [[ 0,15,19,30],[ 1,14,16,31],[ 2,12,18,28],[ 3,13,17,29],[ 4,9,20,26],[ 5, 8,22,24],[ 6,11,23,27],[ 7,10,21,25]],
	      [[ 0,22,26,29],[ 1,21,27,28],[ 2, 4,14,23],[ 3, 8,18,20],[ 5,9,12,16],[ 6,15,25,31],[ 7,11,13,19],[10,17,24,30]]]).


end_of_file.

Occurrences in the four columns:
W weeks, G groups, P players, foursomes.

            Column 1        Column 2            Column 3        Column 4
            ********        ********            ********        ********

 0..3          W-1              0               Rev(Column 2)   Rev(Column 1)
 4..7    W-4..(P-8)/3     0..min(4,(P-8)/2)     
 8..11   W-8..(P-12)/3    0..min(8,(P-12)/2)    
12..15  W-12..(P-16)/3    0..min(12,(P-16)/2)   
16..19  W-16..(P-20)/3    0..min(16,(P-20)/2)   
20..23        0           0..min(20,(P-24)/2)   
24..27        0                 0               
28..31        0                 0               

