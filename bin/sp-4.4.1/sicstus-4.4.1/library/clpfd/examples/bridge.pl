/*
 * CHIP DEMONSTRATION PROGRAM
 * Converted to AKL by Bjorn Carlson
 * Converted to Prolog by Mats Carlsson
 *
 * IDENTIFICATION:	bridge.akl
 *
 * DESCRIPTION: 	scheduling problem with disjunctive constraints. The
 *			problem specification is taken from Bartusch's PhD
 *			thesis.
 *
 *
 * CONTENTS:		bridge is the program mainline.
 *
 * CHIP timings [JLP'90] for a Sun 3/160:
 * cost = 110 after 4.5 seconds
 * cost = 104 after 7.5 seconds
 * proof of optimality after 42.5 seconds
 *
SICStus(FD) timings, Brigitte:
** bridge/3: CD & restarting branch&bound over (choices, min_labeling)
small =   200 msec, 10 fails
big   =  2150 msec, 172 fails

** bridge_r/3: CD and reify and branch&bound **
small =   410 msec, 42 fails
big   =  5020 msec, 382 fails

** bridge_up/3: CD and reify and label End first **
small =   130 msec, 27 fails
big   = 11560 msec, 1243 fails


** bridge_c/3: CD and branch&bound labeling start times **
small =   650 msec, 149 fails
big   = A REALLY LONG TIME

*/

:- module(bridge, [
	bridge/3,
	bridge_r/3,
	bridge_up/3,
	bridge_c/3,
	bridge_p/1
		  ]).

:- use_module(library(clpfd)).

/* ordering jobs by backtracking

disjunctive	#failures	runtime
===========     =========	=======
diff (dom +)	78		2580
diff (int +)	111		2280
wcd(reif)	177 		1970
card(reif)	2951 		8170
none		4545		2540
*/

bridge(Size, Jobs, E) :-
	bridge(Size, wcd, Jobs, E).

bridge(Size, DType, Jobs, E) :-
	setup(DType, Size, E, Disj, Jobs),
	disjunctions(DType, Disj),		% deterministic
	(   foreach([_,_,V],Jobs),
	    foreach(V,Vs)
	do  true
	),
	min_max(DType, Disj, 201, E, [], Vs).


min_max(DType, Disj, E0, E, _, Vs) :-
	E #< E0,
	findall(Vs-E, p1(DType,Disj,Vs,E), [Vs1-E1]), !,
	min_max(DType, Disj, E1, E, Vs1, Vs).
min_max(_, _, E, E, Vs, Vs).

p1(_DType, Disj, Vs, E) :-
	(   foreach(d(_S1,_D1,_S2,_D2,Bool),Disj)
	do  (Bool=1; Bool=0)
	),
	min_labeling(Vs), !,
	fd_min(E, E).

/* ordering jobs by reified choices, b&b

labeling	disjunctive	#failures
========	===========     =========
leftmost	wcd		275
leftmost	card		3048
leftmost	none		3048
*/

bridge_r(Size, Jobs, E) :-
	bridge_r(Size, [], wcd, Jobs, E).

bridge_r(Size, _Label, DType, Jobs, E) :-	% Label not relevant here
	setup(DType, Size, E, Disj, Jobs),
	(   foreach([_,_,V],Jobs),
	    foreach(V,Vs)
	do  true
	),
	disjunctions(DType, Disj),		% deterministic
	(   foreach(disj(_,_,_,_,B),Disj),
	    fromto(Bs,[B|S],S,[E])
	do  true
	),
	labeling([minimize(E)], Bs),
	min_labeling(Vs).

/* ordering jobs by reified choices, increasing end dates

labeling	disjunctive	#failures
========	===========     =========
leftmost	wcd		1322
leftmost	card		15127
leftmost	none		15127
*/

bridge_up(Size, Jobs, E) :-
	bridge_up(Size, [], wcd, Jobs, E).

bridge_up(Size, _Label, DType, Jobs, E) :-
	setup(DType, Size, E, Disj, Jobs),
	(   foreach([_,_,V],Jobs),
	    foreach(V,Vs)
	do  true
	),
	disjunctions(DType, Disj),		% deterministic
	(   foreach(disj(_,_,_,_,B),Disj),
	    foreach(B,Bs)
	do  true
	),
	indomain(E),
	labeling([], Bs), !,
	min_labeling(Vs).


/* no ordering, just label start dates, b&b

labeling	disjunctive	#failures
========	===========     =========
ffc		wcd		>45000
ffc		wcd		117 [small]
ffc		card		>65000 [small]
*/

bridge_c(Size, Jobs, E) :-
	bridge_c(Size, [ff], wcd, Jobs, E).

% try increasing end dates
bridge_c(Size, Label, DType, Jobs, E) :-
	setup(DType, Size, E, Disj, Jobs),
	(   foreach([_,_,V],Jobs),
	    foreach(V,Vs)
	do  true
	),
	disjunctions(DType, Disj),		% deterministic
	% indomain(E),
	labeling([minimize(E)|Label], Vs).


/* ordering jobs by serialized(Ss, Ds, [edge_finder(true)]) +
   dichotomic labeling of start times until each task has a
   compulsory part */

bridge_p(Size) :-
	setup(ti, Size, E, Resources, Jobs),
	minimize(bridge_p_labeling(Resources,Jobs,E), E),
	writeq(Jobs-E),
	nl.

bridge_p_labeling(Resources, Jobs, E) :-
	(   foreach(R,Resources)
	do  label_p_resource(R)
	), !,
	(   foreach([_,_,X],Jobs)
	do  fd_min(X, V),
	    X#=V
	),
	fd_min(E, E).

label_p_resource(Res) :-
				% Order tasks by asc. est, desc. dur
				% Label start times until each task has a compulsory part
	(   foreach(T,Res),
	    foreach(R-T,Ranked)
	do  T = task(S,D,_),
	    fd_min(S, EST),
	    ND is -D,
	    R = (EST,ND)
	),
	keysort(Ranked, Keysorted),
	(   foreach(_-Task,Keysorted)
	do  label_p_task(Task, 1/*discrepancy*/)
	).

label_p_task(T, Dis0) :-
	T = task(S,D,_),
	fd_min(S, EST),
	fd_max(S, LST),
	EST+D =< LST, !,
	Mid is (EST+LST)>>1,
	(   Dis0=:=0 -> Dis = 0, S #=< Mid
	;   Dis = Dis0, S #=< Mid
	;   Dis is Dis0-1, S #> Mid
	),
	label_p_task(T, Dis).
label_p_task(_, _). 

% common stuff

min_labeling([]).
min_labeling([X|Xs]) :-
	fd_min(X, V),
	X#=V,
	min_labeling(Xs).

setup(DType, Size, Ende, Disj, K) :-
	jobs(Size, L),
	(   foreach(H,L),
	    foreach([H,D,A],K)
	do  duration(H, D),
	    A in 0..200
	),
	memberchk([start,_,Start], K),
	memberchk([stop,_,Ende], K),
	Start #= 0,
	precedence(M),
	make_precedence(M, K),
	max_nf(M1),
	make_max_nf(M1, K),
	max_ef(M2),
	make_max_ef(M2, K),
	min_af(M3),
	make_min_af(M3, K),
	min_sf(M4),
	make_min_sf(M4, K),
	min_nf(M5),
	make_min_nf(M5, K),
	resources(R),
	make_disj(R, K, DType, Disj, []).

make_precedence([], _).
make_precedence([[A,B]|R], L) :-
	memberchk([A,Ad,Aa], L),
	memberchk([B,_Bd,Ba], L), !,
	greatereqc(Ba, Aa, Ad),
	make_precedence(R, L).
make_precedence([_|R], L) :-
	make_precedence(R, L).

make_max_nf([], _).
make_max_nf([[A,B,C]|R], L) :-
	memberchk([A,Ad,Aa], L),
	memberchk([B,_Bd,Ba], L), !,
        C1 is C + Ad,
	smallereqc(Ba, Aa, C1),
	make_max_nf(R, L).
make_max_nf([_|R], L) :-
	make_max_nf(R, L).

make_max_ef([], _).
make_max_ef([[A,B,C]|R], L) :-
	memberchk([A,Ad,Aa], L),
	memberchk([B,Bd,Ba], L), !,
        C1 is Ad + C - Bd,
	smallereqc(Ba, Aa, C1),
	make_max_ef(R, L).
make_max_ef([_|R], L) :-
	make_max_ef(R, L).

make_min_af([], _).
make_min_af([[A,B,C]|R], L) :-
	memberchk([A,_Ad,Aa], L),
	memberchk([B,_Bd,Ba], L), !,
	greatereqc(Ba, Aa, C),
	make_min_af(R, L).
make_min_af([_|R], L) :-
	make_min_af(R, L).

make_min_sf([], _).
make_min_sf([[A,B,C]|R], L) :-
	memberchk([A,_Ad,Aa], L),
	memberchk([B,Bd,Ba], L), !,
        /*** C1 is C - Bd,
	     smallereqc(Ba,Aa,C1), ***/
	Ba #=< Aa + C - Bd,
	make_min_sf(R, L).
make_min_sf([_|R], L) :-
	make_min_sf(R, L).

make_min_nf([], _).
make_min_nf([[A,B,C]|R], L) :-
	memberchk([A,Ad,_Aa], L),
	memberchk([B,_Bd,Ba], L), !,
        C1 is C + Ad,
	greatereqc(Ba, Ad, C1),
	make_min_nf(R, L).
make_min_nf([_|R], L) :-
	make_min_nf(R, L).

/* This produces a list of terms corresponding to disjunctive choices
   "Bij #<=> Xi precedes Xj" in the order [B11,...,B1n,...,Bmn].
*/
make_disj([], _R, _) --> [].
make_disj([[_H,R]|T], K, DType) -->
	{el_list(R, K, R1)},
	(   {DType==ti_r}
	->  {   foreach(task(S,D,_),R1),
		foreach(S,Ss),
		foreach(D,Ds)
	    do  true
	    },
	    {serialized(Ss, Ds, [resource(Resource),edge_finder(true)])},
	    [Resource]
	;   {DType==ti}
	->  {disjoint1(R1, [/*global(true)*/])},
	    [R1]
	;   (   fromto(R1,[T1|R2],R2,[])
	    do  (   foreach(T2,R2),
		    param(T1)
		do  {T1 = task(A,B,_)},
		    {T2 = task(C,D,_)},
		    {reified_disj(A,B,C,D,Bool)},
		    [d(A,B,C,D,Bool)]
		)
	    )
	),
	make_disj(T, K, DType).

% for compatibility
serialized(Ss, Ds, Options) :-
	memberchk(resource(R), Options),
	(   foreach(O,Ss),
	    foreach(D,Ds),
	    foreach(task(O,D,E,1,I),Tasks),
	    count(I,0,_)
	do  E in 0..1000
	),	    
	cumulative(Tasks, [/*global(true),*/resource(R)]).

reified_disj(A, Ac, C, Cc, B1) :-
	greatereqc(C,A,Ac) #<=> B1,
	greatereqc(A,C,Cc) #<=> B2,
	B1+B2 #= 1.

el_list([],_,[]).
el_list([H|T],L,[task(A,D,H)|S]) :-
	memberchk([H,D,A],L), !,
	el_list(T,L,S).
el_list([_|T],L,S) :-
	el_list(T,L,S).

greatereqc(X,Y,C) +:
	X in (min(Y)+C)..sup,
	Y in inf..(max(X)-C).
greatereqc(X,Y,C) -:
	X in inf..(max(Y)+C-1),
	Y in (min(X)-C+1)..sup.
greatereqc(X,Y,C) +?
	X in (max(Y)+C)..sup.
greatereqc(X,Y,C) -:
	X in inf..(min(Y)+C-1).

smallereqc(X,Y,C) +:
	X in inf..(max(Y)+C),
	Y in (min(X)-C)..sup.

disjunctions(none, _).
disjunctions(card, _Disj) :- true /*card_disjunct(Disj)*/.
disjunctions(wcd , Disj) :-
	(   foreach(d(A,B,C,D,_),Disj)
	do  w_constructive_disj(A,B,C,D)
	).
disjunctions(ti_r, _).
disjunctions(ti, _).

w_constructive_disj(Aa,Ad,Ba,Bd) +:
	Ba in (min(Aa)+Ad..sup) \/ (inf..max(Aa)-Bd),
        Aa in (min(Ba)+Bd..sup) \/ (inf..max(Ba)-Ad).


/*
		BIG DATA
*/
jobs(small,[start,a1,a2,a3,p1,p2,ue,s1,s2,s3,
	    b1,b2,b3,ab1,ab2,ab3,m1,m2,m3,
	    l1,t1,t2,t3,ua,v1,v2,k1,k2,stop]).
jobs(big,[start,a1,a2,a3,a4,a5,a6,p1,p2,ue,s1,s2,s3,s4,s5,s6,
	  b1,b2,b3,b4,b5,b6,ab1,ab2,ab3,ab4,ab5,ab6,m1,m2,m3,m4,m5,m6,
	  l1,t1,t2,t3,t4,t5,ua,v1,v2,k1,k2,stop]).

% [A,B] denotes "A completes before B starts"
precedence([[start,a1],[start,a2],[start,a3],[start,a4],[start,a5],
	    [start,a6],[start,ue],[a1,s1],[a2,s2],[a5,s5],
	    [a6,s6],[a3,p1],[a4,p2],[p1,s3],[p2,s4],
	    [p1,k1],[p2,k1],[s1,b1],[s2,b2],
	    [s3,b3],[s4,b4],[s5,b5],[s6,b6],[b1,ab1],
	    [b2,ab2],[b3,ab3],[b4,ab4],[b5,ab5],[b6,ab6],
	    [ab1,m1],[ab2,m2],[ab3,m3],[ab4,m4],[ab5,m5],
	    [ab6,m6],[m1,t1],[m2,t1],[m2,t2],[m3,t2],
	    [m3,t3],[m4,t3],[m4,t4],[m5,t4],[m5,t5],
	    [m6,t5],[m1,k2],[m2,k2],[m3,k2],[m4,k2],
	    [m5,k2],[m6,k2],[l1,t1],[l1,t2],[l1,t3],
	    [l1,t4],[l1,t5],[ue,ua],[t1,v1],[t5,v2],
	    [t3,stop],[t2,stop],[t4,stop],[v1,stop],
	    [v2,stop],[ua,stop], [k2,stop]]).

% [A,B,C] denotes "B starts at most C units after A completes"
max_nf([[start,l1,30],[a1,s1,3],[a2,s2,3],[a5,s5,3],
	[a6,s6,3],[p1,s3,3],[p2,s4,3]]).

% [A,B,C] denotes "B completes at most C units after A starts"
min_sf([[ua,m1,2],[ua,m2,2],[ua,m3,2],[ua,m4,2],
		[ua,m5,2],[ua,m6,2]]).

% [A,B,C] denotes "B completes at most C units after A completes"
max_ef([[s1,b1,4],[s2,b2,4],[s3,b3,4],[s4,b4,4],[s5,b5,4],[s6,b6,4]]).

% [A,B,C] denotes "B starts at least C units after A completes"
min_nf([[start,l1,30]]).

% [A,B,C] denotes "B starts at least C units after A starts"
min_af([[ue,s1,6],[ue,s2,6],[ue,s3,6],[ue,s4,6],[ue,s5,6],[ue,s6,6]]).

resources([[crane,[l1,t1,t2,t3,t4,t5]],		% dur=62
	   [bricklaying,[m1,m2,m3,m4,m5,m6]],	% dur=68
	   [carpentry,[s1,s2,s3,s4,s5,s6]],	% dur=34
	   [excavator,[a1,a2,a3,a4,a5,a6]],	% dur=17
	   [pile_driver,[p1,p2]],		% dur=33
	   [concrete_mixer,[b1,b2,b3,b4,b5,b6]], % dur=6
	   [caterpillar,[v1,v2]]]		% dur=25
	 ).

/* "oracle" task orders
resources([[crane,[l1,t1,t2,t3,t5,t4]],		% dur=62
	   [bricklaying,[m5,m1,m2,m3,m4,m6]],	% dur=68
	   [carpentry,[s5,s1,s2,s3,s4,s6]],	% dur=34
	   [excavator,[a5,a1,a3,a2,a4,a6]],	% dur=17
	   [pile_driver,[p1,p2]],		% dur=33
	   [concrete_mixer,[b5,b1,b2,b3,b4,b6]], % dur=6
	   [caterpillar,[v1,v2]]]		% dur=25
	 ).
*/

/*
	SMALL DATA

precedence([[start,a1],[start,a2],[start,a3],
	[start,ue],[a1,s1],[a2,s2],[a3,p1],[p1,s3],
	[p1,k1],[p2,k1],[s1,b1],[s2,b2],[s3,b3],
	[b1,ab1],[b2,ab2],[b3,ab3],
	[ab1,m1],[ab2,m2],[ab3,m3],
	[m1,t1],[m2,t1],[m2,t2],[m3,t2],[m3,t3],
	[m1,k2],[m2,k2],[m3,k2],[l1,t1],[l1,t2],[l1,t3],[t1,v1],
        [t2,stop],[t3,stop],
	[v1,stop],[v2,stop],[ua,stop],
	[k2,stop]]).

max_nf([[start,l1,30],[a1,s1,3],[a2,s2,3],[p1,s3,3]]).

min_sf([[ua,m1,2],[ua,m2,2],[ua,m3,2]]).

max_ef([[s1,b1,4],[s2,b2,4],[s3,b3,4]]).

min_nf([[start,l1,30]]).

min_af([[ue,s1,6],[ue,s2,6],[ue,s3,6]]).

resources([
       [crane,[l1,t1,t2,t3]],
	[bricklaying,[m1,m2,m3]],
	[carpentry,[s1,s2,s3]],
	[excavator,[a1,a2,a3]],
	[pile_driver,[p1,p2]],
	[concrete_mixer,[b1,b2,b3]],
	[caterpillar,[v1,v2]]]).
*/


duration(start,0).
duration(a1,4).
duration(a2,2).
duration(a3,2).
duration(a4,2).
duration(a5,2).
duration(a6,5).
duration(p1,20).
duration(p2,13).
duration(ue,10).
duration(s1,8).
duration(s2,4).
duration(s3,4).
duration(s4,4).
duration(s5,4).
duration(s6,10).
duration(b1,1).
duration(b2,1).
duration(b3,1).
duration(b4,1).
duration(b5,1).
duration(b6,1).
duration(ab1,1).
duration(ab2,1).
duration(ab3,1).
duration(ab4,1).
duration(ab5,1).
duration(ab6,1).
duration(m1,16).
duration(m2,8).
duration(m3,8).
duration(m4,8).
duration(m5,8).
duration(m6,20).
duration(l1,2).
duration(t1,12).
duration(t2,12).
duration(t3,12).
duration(t4,12).
duration(t5,12).
duration(ua,10).
duration(v1,15).
duration(v2,10).
duration(k1,0).
duration(k2,0).
duration(stop,0).

