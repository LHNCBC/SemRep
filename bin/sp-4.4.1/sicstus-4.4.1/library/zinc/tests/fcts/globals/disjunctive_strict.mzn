include "globals.mzn";

array[1..2] of var 0..2: x;
array[1..2] of var {0,2}: d;


constraint
    disjunctive_strict(x, d);

solve satisfy;

output [show(x) ++ "\n" ++ show(d) ++ "\n"];

