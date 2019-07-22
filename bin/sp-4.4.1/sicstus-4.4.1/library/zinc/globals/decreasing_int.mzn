%-----------------------------------------------------------------------------%
% Requires that the array 'x' is in decreasing order (duplicates are allowed).
%-----------------------------------------------------------------------------%

predicate pairwise_relation_int_sicstus(array[int] of var int: x, int: relop);

predicate decreasing_int(array[int] of var int: x) = 
	  pairwise_relation_int_sicstus(x, 1);

