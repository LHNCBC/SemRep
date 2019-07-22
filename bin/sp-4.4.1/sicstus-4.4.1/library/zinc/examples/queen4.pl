:- use_module(library(clpfd)).

query([q=[A,B,C,D]]) :-
        domain([A,B,C,D], 1, 4),
        scalar_product([1,-1], [A,B], #\=, -1),
        scalar_product([1,-1], [A,B], #\=, 1),
        A#\=B,
        scalar_product([1,-1], [A,C], #\=, -2),
        scalar_product([1,-1], [A,C], #\=, 2),
        A#\=C,
        scalar_product([1,-1], [A,D], #\=, -3),
        scalar_product([1,-1], [A,D], #\=, 3),
        A#\=D,
        scalar_product([1,-1], [B,C], #\=, -1),
        scalar_product([1,-1], [B,C], #\=, 1),
        B#\=C,
        scalar_product([1,-1], [B,D], #\=, -2),
        scalar_product([1,-1], [B,D], #\=, 2),
        B#\=D,
        scalar_product([1,-1], [C,D], #\=, -1),
        scalar_product([1,-1], [C,D], #\=, 1),
        C#\=D.
