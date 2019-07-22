/** @group globals.counting
  Requires that for all \p i, the value \a cover[\p i] appears at least \a lbound[\p i]
  and at most \a ubound[\p i] times in the array \a x.
*/
predicate global_cardinality_low_up_sicstus(array[int] of var int: x,
                                            array[int] of int: cover,
				            array[int] of int: lbound,
                                            array[int] of int: ubound,
					    int: closed);

predicate global_cardinality_low_up(array[int] of var int: x,
                                    array[int] of int: cover,
				    array[int] of int: lbound,
                                    array[int] of int: ubound) =
   global_cardinality_low_up_sicstus(x, cover, lbound, ubound, 0);
