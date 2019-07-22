%-----------------------------------------------------------------------------%
% Requires that the array 'x' is strictly lexicographically less than array 'y'.
% Compares them from first to last element, regardless of indices
%-----------------------------------------------------------------------------%

predicate lex_int_sicstus(array[int] of var int: x, 
	                  array[int] of var int: y, 
			  int: relop);

predicate lex_less_int(array[int] of var int: x, array[int] of var int: y) =
	  lex_int_sicstus(x, y, -2);