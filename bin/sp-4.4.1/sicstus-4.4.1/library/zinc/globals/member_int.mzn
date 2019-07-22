%-----------------------------------------------------------------------------%
% Requires that 'y' occurs in the array or set 'x'.
%-----------------------------------------------------------------------------%

predicate member_int(array[int] of var int: x, var int: y);
predicate member_int_reif(array[int] of var int: x, var int: y, var bool: b) = 
    b <-> exists(i in index_set(x)) ( x[i] == y );
