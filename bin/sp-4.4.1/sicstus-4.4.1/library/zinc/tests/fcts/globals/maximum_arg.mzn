include "globals.mzn";

array[1..4] of int: input = [3,1,4,2];

var int: ans;

constraint
    maximum_arg(input, ans);

solve satisfy;

output [show(ans)];

