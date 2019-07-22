predicate diffn_sicstus(array[int] of var int: x, array[int] of var int: y, array[int] of var int: dx, array[int] of var int: dy);

predicate inverse_sicstus(array[int] of var int: f, array[int] of var int: invf, int: min_index_f, int: min_index_invf);
   
predicate table_bool_sicstus(array[int] of var bool: x, array[int] of bool: t);
predicate table_int_sicstus(array[int] of var int: x, array[int] of int: t);

predicate pairwise_relation_bool_sicstus(array[int] of var bool: x, int: relop);
predicate pairwise_relation_int_sicstus(array[int] of var int: x, int: relop);

predicate global_cardinality_sicstus(array[int] of var int: x, array[int] of int: cover, array[int] of var int: counts, int: closed);
predicate global_cardinality_low_up_sicstus(array[int] of var int: x, array[int] of int: cover, array[int] of int: lbound, array[int] of int: ubound, int: closed);

predicate element_int_sicstus(var int: i, array[int] of var int: x, var int: y, int: min_index);
predicate element_bool_sicstus(var int: i, array[int] of var bool: x, var bool: y, int: min_index);

predicate lex_int_sicstus(array[int] of var int: x, array[int] of var int: y, int: relop);
predicate lex_bool_sicstus(array[int] of var bool: x, array[int] of var bool: y, int: relop);

predicate lex2_int_sicstus(array[int] of var int: x, int: nrows, int: ncolumns, int: relop);
predicate lex2_bool_sicstus(array[int] of var bool: x, int: nrows, int: ncolumns, int: relop);

predicate sum_pred_sicstus(var int: i, array[int] of set of int: sets, array[int] of int: cs, var int: s, int: min_index);

predicate bin_packing_sicstus(int: c,
                              array[int] of var int: bin,
                              array[int] of int: w);
predicate bin_packing_capa_sicstus(array[int] of int: c,
                           	   array[int] of var int: bin,
                           	   array[int] of int: w,
			   	   int: min_index);
predicate bin_packing_load_sicstus(array[int] of var int: l,
                                   array[int] of var int: bin,
                                   array[int] of int: w,
			   	   int: min_index);
predicate distribute_sicstus(array[int] of var int: card,
                     array[int] of var int: value,
                     array[int] of var int: base);
predicate value_precede_sicstus(int: s, int: t, array[int] of var int: x);
predicate value_precede_chain_sicstus(array[int] of int: c, array[int] of var int: x);
predicate regular_sicstus(array[int] of var int: x, int: Q, int: S,
                          array[int] of int: d, int: q0, set of int: F);
predicate among_sicstus(var int: n, array[int] of var int: x, set of int: v);
predicate circuit_sicstus(array[int] of var int: x, set of int: y);

