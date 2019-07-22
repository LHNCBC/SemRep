:- module(search, [
	search/8,
	by_intervals/3,
	by_interval/3,
	interval_labeling/2
	]).
:- use_module(library(clpfd)).

/*
search is just a dispatch routine for the actual code
*/

search(naive,Xs,Ys,Sizes,_Width,_Height,_Surf,_N):-
        disjoint_data(Xs,Ys,Sizes,Rs),
        naive_label(Rs).
search(xtheny,Xs,Ys,_Sizes,_Width,_Height,_Surf,_N):-
        labeling([enum],Xs),
        labeling([enum],Ys).
search(disj,Xs,Ys,Sizes,_Width,_Height,_Surf,_N):-
        disjoint_data(Xs,Ys,Sizes,Rs),
        disj_label(Rs),
        naive_label(Rs).
search(semantic,Xs,Ys,Sizes,_Width,_Height,_Surf,_N):-
        disjoint_data(Xs,Ys,Sizes,Rs),
        semantic_label(Rs),
        naive_label(Rs).
search(semantic4,Xs,Ys,Sizes,_Width,_Height,_Surf,_N):-
        disjoint_data(Xs,Ys,Sizes,Rs),
        semantic4_label(Rs),
        naive_label(Rs).
search(dual,Xs,Ys,_Sizes,Width,Height,_Surf,_N):-
        dual_labeling(Xs, 1, Width),
	dual_labeling(Ys, 1, Height).
search(dualyx,Xs,Ys,_Sizes,Width,Height,_Surf,_N):-
	dual_labeling(Ys, 1, Height),
        dual_labeling(Xs, 1, Width).
search(interval(Frac),Xs,Ys,Sizes,_Width,_Height,_Surf,_N):-
        disjoint_data(Xs,Ys,Sizes,Rs),
        interval_labeling(Rs,Frac).
search(split(Frac),Xs,Ys,Sizes,_Width,_Height,_Surf,_N):-
        disjoint_data(Xs,Ys,Sizes,Rs),
        interval_splitx(Rs,Frac,_),
        interval_splity(Rs,Frac,_),
        labeling([bisect],Xs),
        labeling([bisect],Ys).
search(xy(Frac),Xs,Ys,Sizes,_Width,_Height,_Surf,_N):-
        disjoint_data(Xs,Ys,Sizes,Rs),
        interval_splitxy(Rs,Frac,Vars),
        labeling([bisect],Vars).
search(yx(Frac),Xs,Ys,Sizes,_Width,_Height,_Surf,_N):-
        disjoint_data(Xs,Ys,Sizes,Rs),
        interval_splityx(Rs,Vars,Frac),
        labeling([bisect],Vars).

disjoint_data([], [], [], []).
disjoint_data([X|Xs], [Y|Ys], [S|Ss], [r(X,S,Y,S)|Rs]) :-
	disjoint_data(Xs, Ys, Ss, Rs).

/*

interval based labeling

*/


interval_labeling(Rect,Frac):-
        interval_splitx(Rect,Frac,Vars),
        labeling([bisect],Vars),
        interval_splity(Rect,Frac,Vars1),
        labeling([bisect],Vars1).

interval_splitx([],_,[]).
interval_splitx([r(X,W,_,_)|R],Frac,[X|X1]):-
        indomain(W),
        by_interval(X,W,Frac),
        interval_splitx(R,Frac,X1).

interval_splity([],_,[]).
interval_splity([r(_,_,Y,H)|R],Frac,[Y|Y1]):-
        by_interval(Y,H,Frac),
        interval_splity(R,Frac,Y1).

interval_splitxy([],_,[]).
interval_splitxy([r(X,W,Y,H)|R],Frac,[X,Y|Y1]):-
        by_interval(X,W,Frac),
        by_interval(Y,H,Frac),
        interval_splitxy(R,Frac,Y1).

interval_splityx([],_,[]).
interval_splityx([r(X,W,Y,H)|R],Frac,[X,Y|Y1]):-
        by_interval(Y,H,Frac),
        by_interval(X,W,Frac),
        interval_splityx(R,Frac,Y1).


by_intervals([], _, _).
by_intervals([X|Xs], [N|Ns], W) :-
	by_interval(X, N, W),
	by_intervals(Xs, Ns, W).

by_interval(X, _,   _) :-
	integer(X), !.
% Mats's version
by_interval(X, Len, W) :-
	fd_min(X, Min),
	fd_max(X, Max),
	Mid is (Min+Max)>>1,
	(   Max-Min < Len*W -> true
	;   X in Min..Mid,
	    by_interval(X, Len, W)
	;   Mid1 is Mid+1,
	    X in Mid1..Max,
	    by_interval(X, Len, W)
	).
% Helmut's version
% by_interval(X,_,_):-
%         integer(X),
%         !.
% by_interval(X,S,Frac):-
%         fd_min(X,Min),
%         Cut is Min+max(1,integer(S*Frac)),
%         Cut1 is Cut+1,
%         (
%             X in 1..Cut
%         ;
%             X in Cut1..sup,
%             by_interval(X,S,Frac)
%         ).

/*

simple enumeration

*/

naive_label([]).
naive_label([r(X,_,Y,_)|R]):-
        indomain(X),
        indomain(Y),
        naive_label(R).

/*

dual labeling, selecting variable as choice, fixing value constant

called with list of vars,1,width
*/


dual_labeling([], _, _) :- 
        !.
dual_labeling(L, I, Limit) :-
	dual_labeling(L, L1, I, Limit, J),
	dual_labeling(L1, J, Limit).

dual_labeling([], [], _, J, J).
dual_labeling([X|L1], L2, I, J0, J) :-
	(   integer(X) -> 
            dual_labeling(L1, L2, I, J0, J)
	;   
            X #= I, 
            dual_labeling(L1, L2, I, J0, J)
	;   
            X #> I,
	    fd_min(X, J1),
	    J2 is min(J0,J1),
	    L2 = [X|L3],
	    dual_labeling(L1, L3, I, J2, J)
	).


/*

disjunctive labeling variants

*/

disj_label([H|T]):-
        disj_label(T,[H]).

disj_label([],_).
disj_label([X|R],Larger):-
        disj_lp(X,Larger,Larger1),
        disj_label(R,Larger1).

disj_lp(X,[],[X]).
disj_lp(X,[Y|R],[Y|S]):-
        no_overlap_decide(X,Y),
        disj_lp(X,R,S).

no_overlap_decide(r(X1,W1,_Y1,_H1),r(X2,_W2,_Y2,_H2)) :-
    	X1 + W1 #=< X2.
no_overlap_decide(r(X1,_W1,_Y1,_H1),r(X2,W2,_Y2,_H2)) :-
    	X2 + W2 #=< X1.
no_overlap_decide(r(_X1,_W1,Y1,H1),r(_X2,_W2,Y2,_H2)) :-
    	Y1 + H1 #=< Y2.
no_overlap_decide(r(_X1,_W1,Y1,_H1),r(_X2,_W2,Y2,H2)) :-
    	Y2 + H2 #=< Y1.

semantic_label([H|T]):-
        semantic_label(T,[H]).

semantic_label([],_).
semantic_label([X|R],Larger):-
        semantic_lp(X,Larger,Larger1),
        semantic_label(R,Larger1).

semantic_lp(X,[],[X]).
semantic_lp(X,[Y|R],[Y|S]):-
        no_overlap_semantic(X,Y),
        semantic_lp(X,R,S).

no_overlap_semantic(r(X1,W1,_Y1,_H1),r(X2,_W2,_Y2,_H2)) :-
    	X1 + W1 #=< X2.
no_overlap_semantic(r(X1,W1,Y1,H1),r(X2,W2,Y2,H2)) :-
    	X1 + W1 #> X2,
        no_overlap_semantic1(r(X1,W1,Y1,H1),r(X2,W2,Y2,H2)).

no_overlap_semantic1(r(X1,_W1,_Y1,_H1),r(X2,W2,_Y2,_H2)) :-
    	X2 + W2 #=< X1.
no_overlap_semantic1(r(X1,W1,Y1,H1),r(X2,W2,Y2,H2)) :-
    	X2 + W2 #> X1,
        no_overlap_semantic2(r(X1,W1,Y1,H1),r(X2,W2,Y2,H2)).

no_overlap_semantic2(r(_X1,_W1,Y1,H1),r(_X2,_W2,Y2,_H2)) :-
    	Y1 + H1 #=< Y2.
no_overlap_semantic2(r(_X1,_W1,Y1,_H1),r(_X2,_W2,Y2,H2)) :-
%    	Y1 + H1 #> Y2,
    	Y2 + H2 #=< Y1.


semantic4_label([H|T]):-
        semantic4_label(T,[H]).

semantic4_label([],_).
semantic4_label([X|R],Larger):-
        semantic4_lp(X,Larger,Larger1),
        semantic4_label(R,Larger1).

semantic4_lp(X,[],[X]).
semantic4_lp(X,[Y|R],[Y|S]):-
        no_overlap_semantic4(X,Y),
        semantic4_lp(X,R,S).

no_overlap_semantic4(r(X1,W1,_Y1,_H1),r(X2,_W2,_Y2,_H2)) :-
    	X1 + W1 #=< X2.
no_overlap_semantic4(r(X1,_W1,_Y1,_H1),r(X2,W2,_Y2,_H2)) :-
    	X2 + W2 #=< X1.
no_overlap_semantic4(r(X1,W1,Y1,H1),r(X2,W2,Y2,_H2)) :-
    	X1 + W1 #> X2,
    	X2 + W2 #> X1,
    	Y1 + H1 #=< Y2.
no_overlap_semantic4(r(X1,W1,Y1,_H1),r(X2,W2,Y2,H2)) :-
    	X1 + W1 #> X2,
    	X2 + W2 #> X1,
    	Y2 + H2 #=< Y1.



