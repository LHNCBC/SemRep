/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)            INRIA Rocquencourt - ChLoE Project */
/*                                                                         */
/* Name           : partit.pl                                              */
/* Title          : integer partitionning                                  */
/* Original Source: Daniel Diaz - INRIA France                             */
/* Adapted by     :                                                        */
/* Date           : September 1993                                         */
/*                                                                         */
/* Partition numbers 1,2,...,N into two groups A and B such that:          */
/*   a) A and B have the same length,                                      */
/*   b) sum of numbers in A = sum of numbers in B,                         */
/*   c) sum of squares of numbers in A = sum of squares of numbers in B.   */
/*                                                                         */
/* This problem admits a solution if N is a multiple of 8.                 */
/*                                                                         */
/* Note: finding a partition of 1,2...,N into 2 groups A and B such that:  */
/*                                                                         */
/*     Sum (k^p) = Sum l^p                                                 */
/*   k in A      l in B                                                    */
/*                                                                         */
/* admits a solution if N mod 2^(p+1) = 0 (N is a multilple of 2^(p+1)).   */
/* Condition a) is a special case where p=0, b) where p=1 and c) where p=2.*/
/*                                                                         */
/* Two redundant constraints are used:                                     */
/*                                                                         */
/*   - in order to avoid duplicate solutions (permutations) we impose      */
/*     A1<A2<....<AN/2, B1<B2<...<BN/2 and A1<B1. This achieves much more  */
/*     pruning than only alldifferents(A) and alldifferents(B).            */
/*                                                                         */
/*   - the half sums are known                                             */
/*                              N                                          */
/*        Sum k^1 = Sum l^1 = (Sum i) / 2 = N*(N+1) / 4                    */
/*       k in A    l in B      i=1                                         */
/*                              N                                          */
/*        Sum k^2 = Sum l^2 = (Sum i^2)/2 = N*(N+1)*(2*N+1) / 12           */
/*       k in A    l in B      i=1                                         */
/*                                                                         */
/* Solution:                                                               */
/* N=8  A=[1,4,6,7]                                                        */
/*      B=[2,3,5,8]                                                        */
/*                                                                         */
/* N=16 A=[1,2,7,8,11,12,13,14]                                            */
/*      B=[3,4,5,6, 9,10,15,16]                                            */
/*                                                                         */
/* N=24 A=[1,2,3,8,13,14,15,16,18,19,20,21]                                */
/*      B=[4,5,6,7, 9,10,11,12,17,22,23,24]                                */
/*                                                                         */
/* N=32 A=[1,3,4, 7, 8, 9,16,20,21,22,23,24,25,26,27,28]                   */
/*      B=[2,5,6,10,11,12,13,14,15,17,18,19,29,30,31,32]                   */
/*-------------------------------------------------------------------------*/

:- module(partit,[partit/3, partit_global/2]).
:- use_module(library(clpfd)).

partit(Lab,N,Consistency) :-
	N2 is N//2,
	length(A, N2),
	domain(A, 1, N),
	length(B, N2),
	domain(B, 1, N),
	append(A, B, L),
	all_different(L, [consistency(Consistency)]),
	no_duplicate(A, B),			% redundant constraint 1
	half_sums(N, HS1, HS2),			% redundant constraint 2
	(   foreach(X,A),
	    foreach(Y,Asq)
	do  X*X #= Y
	),
	sum(A, #=, HS1),
	sum(Asq, #=, HS2),
	(   foreach(X1,B),
	    foreach(Y1,Bsq)
	do  X1*X1 #= Y1
	),
	sum(B, #=, HS1),
	sum(Bsq, #=, HS2),
	labeling(Lab, L),
	writeq(A), nl,
	writeq(B), nl.

partit_global(Lab,N) :-
	N2 is N//2,
	length(A, N2),
	domain(A, 1, N),
	length(B, N2),
	domain(B, 1, N),
	append(A, B, L),
	no_duplicate(A, B),			% redundant constraint 1
	half_sums(N, HS1, HS2),			% redundant constraint 2
	(   foreach(X,A),
	    foreach(X*X,Asq),
	    foreach(X1,B),
	    foreach(X1*X1,Bsq)
	do  true
	),
	plusify(A, SumA),
	plusify(B, SumB),
	plusify(Asq, SumAsq),
	plusify(Bsq, SumBsq),
	all_different(L, [SumA #= HS1,SumB #= HS1,SumAsq #= HS2,SumBsq #= HS2]),
	labeling(Lab, L),
	writeq(A), nl,
	writeq(B), nl.

plusify([], 0).
plusify([P|Ps], Conj) :-
	plusify(Ps, P, Conj).

plusify([], P, P).
plusify([P|Ps], Q, Conj) :-
	plusify(Ps, Q+P, Conj).

no_duplicate(A,B):-
	A=[X1|L1],
	B=[X2|L2],
	X1 #< X2,
	ascending_order(L1,X1),
	ascending_order(L2,X2).

ascending_order([],_).
ascending_order([Y|L],X):-
	X #< Y,
	ascending_order(L,Y).

half_sums(N,HS1,HS2):-
	S1  is N*(N+1)//2,
	S2  is S1*(2*N+1)//3,
	HS1 is S1//2,           
	HS2 is S2//2.


