/** @group globals.packing
 A global non-overlap constraint for \a k dimensional objects. It enforces that no two objects overlap.
    
@param k:            the number of dimensions
@param rect_size:    the size of each box in \a k dimensios
@param rect_offset:  the offset of each box from the base position in \a k dimensions
 
@param shape:        the set of rectangles defining the \p i-th shape. Assumption: Each pair of boxes in a shape must not overlap.

@param x:       the base position of each object. \a x[\p i,\p j] is the position of object \p i in. dimension \p j.
@param kind:      the shape used by each object.
*/

annotation geost_corners;
annotation geost_cumulative;
annotation geost_disjunctive;
annotation geost_dynamic_programming;
annotation geost_longest_hole;
annotation geost_pallet_loading;
annotation geost_parconflict;
annotation geost_polymorphism;
annotation geost_task_intervals;
annotation geost_visavis;
annotation geost_visavis_floating;
annotation geost_visavis_init;

predicate geost_sicstus(
                      int        : k           ,
    array[int    ] of int        : rect_size   ,
    array[int    ] of int        : rect_offset ,
    array[int    ] of 0..1       : shape01     , 
    array[int    ] of var int    : x           ,
    array[int    ] of var int    : kind        ,
    array[int    ] of var int    : l           ,
    array[int    ] of var int    : u
);

predicate geost(
                      int        : k           ,
    array[int,int] of int        : rect_size   ,
    array[int,int] of int        : rect_offset ,
    array[int    ] of set of int : shape       , 
    array[int,int] of var int    : x           ,
    array[int    ] of var int    : kind
) =
    assert(
        % Some sanity checks
        index_set_1of2( rect_size   ) = index_set_1of2(rect_offset) /\
        index_set_2of2( rect_size   ) = 1..k                        /\
        index_set_2of2( rect_offset ) = 1..k                        /\
        index_set(      shape       ) = 1..length(shape)            /\
        index_set_1of2( x           ) = index_set(kind)             /\
        index_set_2of2( x           ) = 1..k                        /\
        forall(i in index_set(shape))(
            shape[i] subset index_set_1of2(rect_size)
        ),
        % Error message
        "geost: index sets of arguments are incorrect",
    assert(
        % More sanity checks
        forall(i in index_set(shape))(card(shape[i]) > 0),
        % Error message
        "geost: sets in shape must be non-empty",

        % A few useful definitions
        let { 
            set of int: DIMS    = 1..k;
	    set of int: SBOXES  = index_set_1of2(rect_size);
            set of int: SHAPES  = 1..length(shape);
            set of int: OBJECTS = index_set(kind); 
        } in
        % Testing whether the rectangle for each shape are not overlapping
        forall(s in SHAPES)(
            forall(r1, r2 in shape[s] where r1 < r2)(
                assert(
                    geost_nonoverlap_k(
                        [ rect_offset[ r1, j] | j in DIMS ],
                        [ rect_size[   r1, j] | j in DIMS ],
                        [ rect_offset[ r2, j] | j in DIMS ],
                        [ rect_offset[ r2, j] | j in DIMS ]
                    ),
                    "geost: rectangles " ++ show(r1) ++ " and " ++ show(r2) ++
                    " in shape " ++ show(s) ++ " overlap!"
                )
            )
        )
        /\  % Posting the geost constraint
        let {array[DIMS] of var int: l, 
	     array[DIMS] of var int: u,
	     array[int] of 0..1: shape01 =
		     [b in shape[s] | s in SHAPES, b in SBOXES]} in (
          geost_sicstus(k, array1d(rect_size), array1d(rect_offset), shape01, array1d(x), kind, l, u)
	)
    )); % End assert statements


/** @group globals.packing
 A global non-overlap constraint for \a k dimensional objects. It enforces that no two objects overlap, and that all objects fit within a global \a k dimensional bounding box.
    
@param k:            the number of dimensions
@param rect_size:    the size of each box in \a k dimensios
@param rect_offset:  the offset of each box from the base position in \a k dimensions
 
@param shape:        the set of rectangles defining the \p i-th shape. Assumption: Each pair of boxes in a shape must not overlap.

@param x:       the base position of each object. \a x[\p i,\p j] is the position of object \p i in dimension \p j.
@param kind:      the shape used by each object.

@param l:            is an array of lower bounds, \a l[\p i] is the minimum bounding box for all objects in dimension \p i.
@param u:            is an array of upper bounds, \a u[\p i] is the maximum bounding box for all objects in dimension \p i.
*/

predicate geost_bb(
                      int        : k           ,
    array[int,int] of int        : rect_size   ,
    array[int,int] of int        : rect_offset ,
    array[int    ] of set of int : shape       , 
    array[int,int] of var int    : x           ,
    array[int    ] of var int    : kind        ,
    array[int    ] of var int    : l           ,
    array[int    ] of var int    : u
) =
    assert(
        % Sanity check
        index_set(l) = 1..k /\
	index_set(u) = 1..k /\
        index_set_1of2( rect_size   ) = index_set_1of2(rect_offset) /\
        index_set_2of2( rect_size   ) = 1..k                        /\
        index_set_2of2( rect_offset ) = 1..k                        /\
        index_set(      shape       ) = 1..length(shape)            /\
        index_set_1of2( x           ) = index_set(kind)             /\
        index_set_2of2( x           ) = 1..k                        /\
        forall(i in index_set(shape))(
            shape[i] subset index_set_1of2(rect_size)
        ),
        % Error message
        "geost: index sets of arguments are incorrect",
        % A few useful definitions
        let { 
            set of int: DIMS    = 1..k;
	    set of int: SBOXES  = index_set_1of2(rect_size);
            set of int: SHAPES  = 1..length(shape);
            set of int: OBJECTS = index_set(kind); 
        } in
        % Testing whether the rectangle for each shape are not overlapping
        forall(s in SHAPES)(
            forall(r1, r2 in shape[s] where r1 < r2)(
                assert(
                    geost_nonoverlap_k(
                        [ rect_offset[ r1, j] | j in DIMS ],
                        [ rect_size[   r1, j] | j in DIMS ],
                        [ rect_offset[ r2, j] | j in DIMS ],
                        [ rect_offset[ r2, j] | j in DIMS ]
                    ),
                    "geost: rectangles " ++ show(r1) ++ " and " ++ show(r2) ++
                    " in shape " ++ show(s) ++ " overlap!"
                )
            )
        )
        /\  % Posting the geost constraint
        let {array[int] of 0..1: shape01 =
		     [b in shape[s] | s in SHAPES, b in SBOXES],
	     array[DIMS] of var int: lhull,
	     array[DIMS] of var int: uhull} in (
          forall(j in DIMS)(l[j] <= lhull[j] /\ uhull[j] <= u[j]) /\
          geost_sicstus(k, array1d(rect_size), array1d(rect_offset), shape01, array1d(x), kind, lhull, uhull)
	)
    ); % End assert statement


/** @group globals.packing
 A global non-overlap constraint for \a k dimensional objects. It enforces that no two objects overlap, and that all objects fit within a global \a k dimensional bounding box. In addition, it enforces that the bounding box is the smallest one containing all objects, i.e., each of the \a 2k boundaries is touched by at least by one object.
    
@param k:            the number of dimensions
@param rect_size:    the size of each box in \a k dimensios
@param rect_offset:  the offset of each box from the base position in \a k dimensions
 
@param shape:        the set of rectangles defining the \p i-th shape. Assumption: Each pair of boxes in a shape must not overlap.

@param x:       the base position of each object. \a x[\p i,\p j] is the position of object \p i in dimension \p j.
@param kind:      the shape used by each object.

@param l:            is an array of lower bounds, \a l[\p i] is the minimum bounding box for all objects in dimension \p i.
@param u:            is an array of upper bounds, \a u[\p i] is the maximum bounding box for all objects in dimension \p i.
*/

predicate geost_smallest_bb(
                      int        : k           ,
    array[int,int] of int        : rect_size   ,
    array[int,int] of int        : rect_offset ,
    array[int    ] of set of int : shape       , 
    array[int,int] of var int    : x           ,
    array[int    ] of var int    : kind        ,
    array[int    ] of var int    : l           ,
    array[int    ] of var int    : u
) =
    assert(
        % Sanity check
        index_set(l) = 1..k /\
	index_set(u) = 1..k /\
        index_set_1of2( rect_size   ) = index_set_1of2(rect_offset) /\
        index_set_2of2( rect_size   ) = 1..k                        /\
        index_set_2of2( rect_offset ) = 1..k                        /\
        index_set(      shape       ) = 1..length(shape)            /\
        index_set_1of2( x           ) = index_set(kind)             /\
        index_set_2of2( x           ) = 1..k                        /\
        forall(i in index_set(shape))(
            shape[i] subset index_set_1of2(rect_size)
        ),
        % Error message
        "geost: index sets of arguments are incorrect",
        % A few useful definitions
        let { 
            set of int: DIMS    = 1..k;
	    set of int: SBOXES  = index_set_1of2(rect_size);
            set of int: SHAPES  = 1..length(shape);
            set of int: OBJECTS = index_set(kind); 
        } in
        % Testing whether the rectangle for each shape are not overlapping
        forall(s in SHAPES)(
            forall(r1, r2 in shape[s] where r1 < r2)(
                assert(
                    geost_nonoverlap_k(
                        [ rect_offset[ r1, j] | j in DIMS ],
                        [ rect_size[   r1, j] | j in DIMS ],
                        [ rect_offset[ r2, j] | j in DIMS ],
                        [ rect_offset[ r2, j] | j in DIMS ]
                    ),
                    "geost: rectangles " ++ show(r1) ++ " and " ++ show(r2) ++
                    " in shape " ++ show(s) ++ " overlap!"
                )
            )
        )
        /\  % Posting the geost constraint
        let {array[int] of 0..1: shape01 =
		     [b in shape[s] | s in SHAPES, b in SBOXES]} in (
          geost_sicstus(k, array1d(rect_size), array1d(rect_offset), shape01, array1d(x), kind, l, u)
	)
    ); % End assert statement


predicate geost_nonoverlap_k(
    array[int] of var int : x1, 
    array[int] of int     : w1, 
    array[int] of var int : x2, 
    array[int] of int     : w2
) = 
    assert(
        % Some sanity checks
        index_set( x1 ) = index_set( w1 ) /\ 
        index_set( x1 ) = index_set( x2 ) /\ 
        index_set( x1 ) = index_set( w2 ),
        % Error message
        "geost_nonoverlap_k: index sets of arguments do not match",
        % Non-overlap constraint
        exists(j in index_set(x1))(
            x1[j] + w1[j] <= x2[j] \/ x2[j] + w2[j] <= x1[j]
        )
    );


test geost_nonoverlap_k(
    array[int] of int: x1, 
    array[int] of int: w1, 
    array[int] of int: x2, 
    array[int] of int: w2
) = 
    assert(
        % Some sanity checks
        index_set( x1 ) = index_set( w1 ) /\ 
        index_set( x1 ) = index_set( x2 ) /\ 
        index_set( x1 ) = index_set( w2 ),
        % Error message
        "geost_nonoverlap_k: index sets of arguments do not match",
        % Non-overlap test
        exists(j in index_set(x1))(
            x1[j] + w1[j] <= x2[j] \/ x2[j] + w2[j] <= x1[j]
        )
    );

