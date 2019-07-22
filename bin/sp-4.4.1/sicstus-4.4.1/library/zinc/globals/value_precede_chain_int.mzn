%-----------------------------------------------------------------------------%
% Require that the value_precede constraint is true for every pair of adjacent
% integers in 'c' in the array 'x'.
%-----------------------------------------------------------------------------%

predicate value_precede_chain_sicstus(array[int] of int: c, 
	                              array[int] of var int: x);

predicate value_precede_chain_int(array[int] of int: c, array[int] of var int: x) =
    value_precede_chain_sicstus(c, x);
    % forall (i in min(index_set(c)) + 1 .. max(index_set(c))) (
    %     value_precede(c[i - 1], c[i], x)
    % );
