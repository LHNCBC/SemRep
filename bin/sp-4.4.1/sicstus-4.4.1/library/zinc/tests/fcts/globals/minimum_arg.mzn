include "globals.mzn";

array[1..4] of int: input = [3,1,4,2];

var int: ans;

constraint
    minimum_arg(input, ans);

solve satisfy;

output [show(ans)];

