%-----------------------------------------------------------------------------%
% Requires that 'y' occurs in the array or set 'x'.
%-----------------------------------------------------------------------------%

predicate member_bool(array[int] of var bool: x, var bool: y);
predicate member_bool_reif(array[int] of var bool: x, var bool: y, var bool: b) = 
    b <-> exists(i in index_set(x)) ( x[i] == y );
