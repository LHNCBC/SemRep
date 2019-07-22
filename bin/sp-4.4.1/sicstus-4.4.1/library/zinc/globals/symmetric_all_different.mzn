include "inverse.mzn";

/** @group globals.alldifferent
  Requires the array of integers \a x to be all different, and for all \p i,
  \a x[\p i]=j \(\rightarrow\) \a x[\p j]=i.
*/

predicate symmetric_all_different(array[int] of var int:x) =
    inverse(x, x);
