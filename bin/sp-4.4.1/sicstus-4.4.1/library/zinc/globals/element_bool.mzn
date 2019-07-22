%-----------------------------------------------------------------------------%
% Requires that 'y' is the ith element of the array 'x'.
%-----------------------------------------------------------------------------%

predicate element_bool_sicstus(var int: i, array[int] of var bool: x, var bool: y, int: min_index);

predicate element_bool(var int: i, array[int] of var bool: x, var bool: y) =
    element_bool_sicstus(i, x, y, min(index_set(x)));


