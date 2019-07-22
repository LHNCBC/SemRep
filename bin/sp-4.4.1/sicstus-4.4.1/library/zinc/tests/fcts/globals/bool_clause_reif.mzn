include "globals.mzn";

array[1..4] of var bool: x;
var bool: y;

constraint
  x[1] \/ x[2] \/ not x[3] \/ not x[4] <-> y;

solve satisfy;

output ["X: \(x) Y: \(y)\n"];

