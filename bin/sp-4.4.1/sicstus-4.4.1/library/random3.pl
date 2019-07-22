/* Copyright(C) 1989, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : random.pl                                                        %
%   Maintainer : Lena Flood                                                   %
%   Date   : 20 June 1989                                                     %
%   Purpose: To provide a random number generator                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(random3, [
        getrand/1,
        setrand/1,
        random/1,
        random/3,
        randseq/3,
        randset/3
	]).

:- use_module(library(random), [
        getrand/1,
        setrand/1,
        random/1,
        random/3
	]).

:- use_module(library(assoc3), [
	empty_assoc/1,
	get_assoc/3,
	put_assoc/4
	]).

%   randseq(K, N, L)
%   generates a random sequence of K integers in the range 1..N.
%   The result is in random order.
%   Courtesy of Christian Holzbaur.

randseq(K, N, Bag) :-
	K >= 0,
	K =< N,
	M is N+1,
        empty_assoc(A),
        randseq(K, M, A, Bag, []).

randseq(0, _, _) --> !.
randseq(N, M, A0) -->
        {random(1, M, R)},
        (   {get_assoc(R, A0, _)} ->
            randseq(N, M, A0)
        ;   [R],
            {N1 is N-1},
            {put_assoc(R, A0, R, A1)},
            randseq(N1, M, A1)
        ).


%   randset(K, N, S)
%   generates a random set of K integers in the range 1..N.
%   The result is an ordered list.
%   Courtesy of Christian Holzbaur.

randset(K, N, Set) :-
	randseq(K, N, Bag),
	sort(Bag, Set).

