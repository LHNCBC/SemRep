predicate among_sicstus(var int: n, array[int] of var int: x, set of int: v);

%-----------------------------------------------------------------------------%
% Requires exactly 'n' variables in 'x' to take one of the values in 'v'.
%-----------------------------------------------------------------------------%
predicate among(var int: n, array[int] of var int: x, set of int: v) =
    among_sicstus(n, x, v);
    % n == sum(i in index_set(x)) ( bool2int(x[i] in v) );

