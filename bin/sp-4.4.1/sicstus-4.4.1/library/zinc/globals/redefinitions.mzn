% Define int_pow for CP solvers
% r = x ^ y
predicate int_pow( var int: x, var int: y, var int: r ) =
  if is_fixed(y) /\ 2==fix(y) then r==x*x
  elseif is_fixed(y) /\ 3==fix(y) then r==x*x*x
  else
    let {
      array[ int, int ] of int: x2y = array2d( lb(x)..ub(x), lb(y)..ub(y),
        [ pow( X, Y ) | X in lb(x)..ub(x), Y in lb(y)..ub(y) ] )
    } in
      r == x2y[ x, y ]
  endif;
