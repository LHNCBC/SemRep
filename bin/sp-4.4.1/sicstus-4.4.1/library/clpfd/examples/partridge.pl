:- module(partridge,[partridge/1]).
:- use_module(library(lists)).
:- use_module(library(trees)).
:- use_module(library(clpfd)).

/*

http://www.mathpuzzle.com/partridge.html

*/

partridge(N) :-
	Width is N*(N+1)//2,
	Height is Width,
	state_no_overlap(N,Width,Height,Rectangles),
	profile_labeling(Width,Height,Rectangles), !,
	format('placement space = ~dx~d\n', [Width,Height]),
	format('rectangles r(X,W,Y,H) = ~w\n', [Rectangles]).

state_no_overlap(N,Width,Height,Rectangles) :-
	(   fromto(N,Ni,Nj,0),
	    fromto(1,OId1,OId2,_),
	    foreach(sbox(Ni,[0,0],[Ni,Ni]),Shapes),
	    fromto(Objects,Obj1,Obj2,[]),
	    fromto(Rectangles,R1,R2,[]),
	    fromto(AllOptions,[lex(List)|Tail],Tail,TailOptions),
	    param(Width,Height)
	do  Nj is Ni-1,
	    OId2 is OId1+Ni,
	    (   fromto(Obj1,[object(OIdi,Ni,[X,Y])|S],S,Obj2),
	        fromto(R1,[r(X,Ni,Y,Ni)|R],R,R2),
		for(OIdi,OId1,OId2-1),
		foreach(OIdi,List),
		param(Ni,Width,Height)
	    do  X in 1..Width,
		Y in 1..Height,
		X+Ni-1 #=< Width,
		Y+Ni-1 #=< Height
	    )
	),
	Width1 is Width+1,
	Height1 is Height+1,
	TailOptions = [bounding_box([1,1],[Width1,Height1]),
		       cumulative(true),
		       disjunctive(true),
		       dynamic_programming(true)],
	geost(Objects,Shapes,AllOptions).

/*** Begin Nicolas's dual labeling with compulsory part profile reasonong. ***/

profile_labeling(Width, Height, Rectangles) :-
	(   foreach(r(X,W,Y,H),Rectangles),
	    foreach(task(X,W,H),TasksX),
	    foreach(task(Y,H,W),TasksY),
	    fromto(0,MW1,MW2,MaxW),
	    fromto(0,MH1,MH2,MaxH)
	do  MW2 is max(MW1,W),
	    MH2 is max(MH1,H)
	),
	label(TasksX, MaxW, MaxH, Width),
	label(TasksY, MaxH, MaxW, Height).

label(Tasks, MaxL, MaxH, Limit) :-
	(   for(_,1,MaxL),
	    foreach(0,LL)
	do  true
	),
	list_to_tree(LL, TL),
	(   for(_,1,MaxH),
	    foreach(0,LH)
	do  true
	),
	list_to_tree(LH, TH),
	(   foreach(task(_,Length,Height),Tasks),
	    fromto(TL,TL1,TL2,TL3),
	    fromto(TH,TH1,TH2,TH3)
	do  put_label(Length, TL1, NbOccL, TL2, NbOccL1),
	    NbOccL1 is NbOccL+1,
	    put_label(Height, TH1, NbOccH, TH2, NbOccH1),
	    NbOccH1 is NbOccH+1
	),
	label(Tasks, [], Limit, TL3, TH3).

label([], _, _, _, _).
label([Task|R], FixedTasks, Limit, L, H) :-
	(   foreach(task(Ori,_,_),[Task|R]),
	    fromto(1000000,E1,E2,EarliestStart)
	do  fd_min(Ori, MinOri),
	    E2 is min(E1,MinOri)
	),
	find_next_fixed_end_after_earliest_start(FixedTasks, EarliestStart, 1000000, NextEnd),
	find_sum_heights_fixed_tasks_at_earliest_start(FixedTasks, EarliestStart, 0, SumHeights),
	GapL is NextEnd-EarliestStart,
	GapH is Limit-SumHeights,
	order_tasks([Task|R], EarliestStart, GapL, GapH, L, H, OrderedTasks),
	fix_to_earliest_start(OrderedTasks, EarliestStart, S, L, H, NewL, NewH, FixedTask),
	label(S, [FixedTask|FixedTasks], Limit, NewL, NewH).

order_tasks(Tasks, EarliestStart, GapL, GapH, L, H, OrderedTasks) :-
	(   foreach(Task,Tasks),
	    foreach(Cost-Task,OTasks),
	    param(EarliestStart,GapL,GapH,L,H)
	do  get_cost(Task, EarliestStart, GapL, GapH, L, H, Cost)
	),
	keysort(OTasks, OOTasks),
	(   foreach(_-T,OOTasks),
	    foreach(  T,OrderedTasks)
	do  true
	).

get_cost(task(Ori,Length,Height), EarliestStart, GapL, GapH, L, H, Cost) :-
	fd_min(Ori, MinOri),
	get_label(Height, H, NbOccL),
	get_label(Length, L, NbOccH),
	(   MinOri =:= EarliestStart,
	    Length =:= GapL,
	    Height =:= GapH ->
	    Cost is 0-Length*Height
	;   MinOri =:= EarliestStart,
	    Length =:= GapL,
	    NbOccL > 0 ->
	    Cost is 1000000-Length*Height
	;   MinOri =:= EarliestStart,
	    Height =:= GapH,
	    NbOccH > 0 ->
	    Cost is 1000000-Length*Height
	;   MinOri =:= EarliestStart,
	    NbOccL > 0,
	    NbOccH > 0 ->
	    Cost is 2000000-Length*Height
	;   MinOri =:= EarliestStart,
	    NbOccL > 0 ->
	    Cost is 3000000-Length*Height
	;   MinOri =:= EarliestStart,
	    NbOccH > 0 ->
	    Cost is 3000000-Length*Height
	;   MinOri =:= EarliestStart ->
	    Cost is 4000000-Length*Height
	;   Cost is 5000000
	).

find_next_fixed_end_after_earliest_start([], _, NextEnd, NextEnd).
find_next_fixed_end_after_earliest_start([task(Ori,Length,_)|R], EarliestStart, Cur, NextEnd) :-
	End is Ori + Length,
	End > EarliestStart, !,
	E is min(End, Cur),
	find_next_fixed_end_after_earliest_start(R, EarliestStart, E, NextEnd).
find_next_fixed_end_after_earliest_start([_|R], EarliestStart, Cur, NextEnd) :-
	find_next_fixed_end_after_earliest_start(R, EarliestStart, Cur, NextEnd).

find_sum_heights_fixed_tasks_at_earliest_start([], _, SumHeights, SumHeights).
find_sum_heights_fixed_tasks_at_earliest_start([task(Ori,Length,Height)|R], EarliestStart, Cur, SumHeights) :-
	End is Ori + Length,
	Ori =< EarliestStart,
	EarliestStart < End, !,
	H is Cur+Height,
	find_sum_heights_fixed_tasks_at_earliest_start(R, EarliestStart, H, SumHeights).
find_sum_heights_fixed_tasks_at_earliest_start([_|R], EarliestStart, Cur, SumHeights) :-
	find_sum_heights_fixed_tasks_at_earliest_start(R, EarliestStart, Cur, SumHeights).

fix_to_earliest_start([task(Ori,Length,Height)|R], EarliestStart, R, L, H, NewL, NewH, task(Ori,Length,Height)) :-
	Ori #= EarliestStart,
	get_label(Length, L, NbOccL),
	NbOccL1 is NbOccL-1,
	put_label(Length, L, NbOccL1, NewL),
	get_label(Height, H, NbOccH),
	NbOccH1 is NbOccH-1,
	put_label(Length, H, NbOccH1, NewH).
fix_to_earliest_start([task(Ori,Length,Height)|R0], EarliestStart, [task(Ori,Length,Height)|S0], L, H, NewL, NewH, FixedTask) :-
	Ori #> EarliestStart,
	profile_skip(R0, R, S0, S, Length, Height),
	fix_to_earliest_start(R, EarliestStart, S, L, H, NewL, NewH, FixedTask).

profile_skip([task(Ori,Length,Height)|R0], R, [task(Ori,Length,Height)|S0], S, Length, Height) :- !,
	profile_skip(R0, R, S0, S, Length, Height).
profile_skip(R, R, S, S, _, _).

