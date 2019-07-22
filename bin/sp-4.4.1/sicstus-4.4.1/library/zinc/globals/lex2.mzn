/** @group globals.lexicographic
  Require adjacent rows and adjacent columns in the array \a x to be
  lexicographically ordered.  Adjacent rows and adjacent columns may be equal.
*/
predicate lex2_int_sicstus(array[int] of var int: x, 
	                   int: nrows, 
			   int: ncolumns, 
			   int: relop);
predicate lex2_bool_sicstus(array[int] of var bool: x, 
	                    int: nrows,  
			    int: ncolumns, 
			    int: relop);

predicate lex2(array[int, int] of var int: x) =
    let {
            set of int: row_indexes = index_set_1of2(x),
            set of int: column_indexes = index_set_2of2(x),
	    int : n_rows = card(row_indexes),
	    int : n_columns = card(column_indexes),
            int : nn = n_rows * n_columns,
            array [1..nn] of var int : xx = [x[i,j] | i in row_indexes, 
                                                      j in column_indexes]
        }
	in
	    lex2_int_sicstus(xx, n_rows, n_columns, -1);

predicate lex2(array[int, int] of var bool: x) =
    let {
            set of int: row_indexes = index_set_1of2(x),
            set of int: column_indexes = index_set_2of2(x),
	    int : n_rows = card(row_indexes),
	    int : n_columns = card(column_indexes),
            int : nn = n_rows * n_columns,
            array [1..nn] of var bool : xx = [x[i,j] | i in row_indexes, 
                                                       j in column_indexes]
        }
	in
	    lex2_bool_sicstus(xx, n_rows, n_columns, -1);