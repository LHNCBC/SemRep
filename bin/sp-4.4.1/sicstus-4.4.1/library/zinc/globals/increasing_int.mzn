%-----------------------------------------------------------------------------%
% Requires that the array 'x' is in increasing order (duplicates are allowed).
%-----------------------------------------------------------------------------%

predicate pairwise_relation_int_sicstus(array[int] of var int: x, int: relop);

predicate increasing_int(array[int] of var int: x) =
	  pairwise_relation_int_sicstus(x, -1);
