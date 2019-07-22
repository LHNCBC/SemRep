include "globals.mzn";

array[1..4] of var 1..4: input;

constraint
    symmetric_all_different(input);

solve satisfy;

output [show(input)];

