/** @group globals.counting
  Requires that the number of occurrences of \a cover[\p i] in \a x is \a counts[\p i].
*/
predicate global_cardinality_sicstus(array[int] of var int: x,
                                     array[int] of int: cover,
                                     array[int] of var int: counts,
				     int: closed);

predicate global_cardinality(array[int] of var int: x,
	  		     array[int] of int: cover,
                             array[int] of var int: counts) =
    assert(index_set(cover) = index_set(counts),
        "global_cardinality: cover and counts must have identical index sets",
    global_cardinality_sicstus(x, cover, counts, 0));

