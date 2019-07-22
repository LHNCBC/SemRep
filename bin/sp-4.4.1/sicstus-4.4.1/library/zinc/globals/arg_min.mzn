/** @group globals
    Returns the index of the minimum value in the array \a x.
    When breaking ties the least index is returned.
*/
function var int: arg_min(array[int] of var int: x)  = 
                  let { constraint length(x) > 0; } in arg_min_total(x);

/** @group globals
    Returns the index of the minimum value in the array \a x.
    When breaking ties the least index is returned.
*/
function var int: arg_min_total(array[int] of var int: x) :: promise_total = 
                  if length(x) = 0 then 0 else
                  let { var min(index_set(x)) .. max(index_set(x)): i;
                        constraint minimum_arg(x, i); }
                  in i endif;

predicate minimum_arg_int(array[int] of var int: x, var int: i, int: base);

/** @group globals
    Constrain \a i to be the index of the minimum value in the array \a x.
    When breaking ties the least index is returned.
    
    Assumption: |x| > 0
*/
predicate minimum_arg(array[int] of var int: x, var int: i) =
          minimum_arg_int(x, i, min(index_set(x)));

