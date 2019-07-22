%   File       : open_stacks.pl
%   Maintainer : Mats Carlsson
%   Updated    : November 2011
%   Purpose    : Solving the Open Stack Problem
%   Original model by Peter J. Stuckey

:- module(open_stacks, [open_stacks/1]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).

:- dynamic
	orders/2.

orders(p10_10_1,
       [[0, 0, 0, 0, 1, 0, 0, 1, 0, 0],
	[0, 0, 1, 0, 0, 1, 0, 0, 0, 1],
	[1, 0, 0, 1, 0, 0, 0, 0, 0, 1],
	[1, 0, 1, 0, 1, 0, 1, 1, 0, 0],
	[0, 0, 0, 0, 0, 0, 0, 1, 1, 0],
	[0, 0, 0, 0, 1, 0, 1, 0, 1, 0],
	[0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
	[0, 1, 0, 0, 0, 0, 0, 1, 0, 0],
	[0, 0, 0, 1, 0, 0, 1, 0, 0, 0],
	[0, 1, 0, 0, 0, 0, 0, 0, 1, 0]]).

open_stacks(ID) :-
	open_stacks(ID, Schedule, Objective),
	labeling([minimize(Objective)], Schedule),
	format('Max open stacks: ~d\nSchedule: ~w\n', [Objective,Schedule]).

open_stacks(ID, Schedule, Objective) :-
	orders(ID, Orders),
	Orders = [FirstOrder|_],
	length(FirstOrder, P),	% #products
	length(Schedule, P),	% schedule of products
	domain(Schedule, 1, P),
	(   foreach(Row,Orders),
	    foreach(N,NOrders),	% #orders per customer
	    foreach([0|ORow],OrdersFilled), % #orders filled after time t per customer
	    param(P,Schedule)
	do  sumlist(Row, N),
	    length(ORow, P),
	    (   foreach(Elt,Row),
		count(I,1,_),
		fromto([],S1,S2,Set)
	    do  (   Elt=:=0 -> S1 = S2
		;   fdset_add_element(S1, I, S2)
		)
	    ),
	    (   fromto([0|ORow],[Oprev,Onext|Os],[Onext|Os],[_]),
		foreach(S,Schedule),
		param(Set)
	    do  S in_set Set #<=> R,
		Oprev + R #= Onext
	    )
	),
	transpose(OrdersFilled,OrdersFilledT),
	(   fromto(OrdersFilledT,[RowP,RowN|Rows],[RowN|Rows],[_]),
	    foreach(Cost,Costs), % #open stacks per time point
	    param(NOrders)
	do  (   foreach(EltP,RowP),
		foreach(EltN,RowN),
		foreach(NO,NOrders),
		foreach(B,Bs)
	    do  open_stack(EltP,EltN,NO,B)
	    ),
	    sum(Bs, #=, Cost)
	),
	% objective is max open stacks at any time
	maximum(Objective, Costs),
	% each product is scheduled once
	all_distinct(Schedule, [consistency(bound)]).

open_stack(EltP,EltN,NO,B) +:
	EltP #< NO #/\ EltN #> 0 #<=> B.
