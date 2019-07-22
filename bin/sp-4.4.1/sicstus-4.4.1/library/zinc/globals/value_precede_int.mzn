%-----------------------------------------------------------------------------%
% Requires that 's' precede 't' in the array 'x'.
%
% If any element of 'x'
% is equal to 't', then another element of 'x' with a  lower index is equal
% to 's'.
%-----------------------------------------------------------------------------%

predicate value_precede_sicstus(int: s, 
	                        int: t, 
				array[int] of var int: x);

predicate value_precede_int(int: s, int: t, array[int] of var int: x) =
          value_precede_sicstus(s, t, x);
