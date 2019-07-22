include "globals.mzn";

% a stupid NFA/DFA: 1 or 2 followed by 3 or 4

array[1..2] of var 1..4: x;

array[1..3,1..4] of int: d = [| 2, 2, 0, 0, | 0, 0, 3, 3, | 0, 0, 0, 0, |];

constraint
    regular(x, 3, 4, d, 1, {3});

solve satisfy;

output [show(x) ++ "\n"];

