/** @group globals.channeling
  Constrains two arrays of int variables, \a f and \a invf, to represent
  inverse functions.  All the values in each array must be within the index
  set of the other array.
*/
predicate inverse_sicstus(array[int] of var int: f, 
	                  array[int] of var int: invf, 
			  int: min_index_f, 
			  int: min_index_invf);

predicate inverse(array[int] of var int: f, array[int] of var int: invf) = 
    inverse_sicstus(f, invf, min(index_set(f)), min(index_set(invf)));

    
