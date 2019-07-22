predicate circuit_sicstus(array[int] of var int: x, set of int: y);

/** @group globals
  Constrains the elements of \a x to define a circuit where \a x[\p i] = \p j means
  that \p j is the successor of \p i.
*/

predicate circuit(array[int] of var int: x) =
	    circuit_sicstus(x, index_set(x));
