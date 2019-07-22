%-----------------------------------------------------------------------------%
% Requires that the array 'x' is in decreasing order (duplicates are allowed).
%-----------------------------------------------------------------------------%

predicate pairwise_relation_bool_sicstus(array[int] of var bool: x, int: relop);

predicate decreasing_bool(array[int] of var bool: x) =
	  pairwise_relation_bool_sicstus(x, 1);

