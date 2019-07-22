%   Package: structs
%   Author : Peter Schachte
%   Updated: 12/10/98
%   Purpose: to allow access to C/Pascal data structures from Prolog

%   Copyright (C) 1990, Quintus Computer Systems, Inc.  All rights reserved.

% see accompanying README file for usage documentation.

:- module(structs, [
	(foreign_type)/2,
	get_contents/3,
	put_contents/3,
	get_address/3,
	new/2,
	new/3,
	dispose/1,
	cast/3,
	null_foreign_term/2,
	type_definition/2,
	type_definition/3,
	atomic_type/1,
	atomic_type/2,
	atomic_type/3
   ]).

:- use_module(library(alignments), [
	primitive_type_size/3
	]).
:- use_module(library(types), [
	illarg/3,
	illarg/4,
	must_be/4
	]).



:- multifile
	'SU_messages':typename/3.

'SU_messages':typename(part_of_foreign_object) --> !,
	['part of foreign object'-[]].
'SU_messages':typename(fixed_size_type) --> !,
	['fixed-size type'-[]].
'SU_messages':typename(foreign_object) --> !,
	['foreign object'-[]].
'SU_messages':typename(foreign_type) --> !,
	['foreign type'-[]].

/*
				Dynamic procedures
*/

%  Multifile procedures used by this module:
%
%	structs1:type_defn(Type_name, Simple_type_expr)
%		Type_name is the name of a user-defined type,
%		Simple_type_expr is a type expression defining
%		the type (see doc for new_atomic_type/8 for a
%		description of what a "simple" type expr is).
%
%	structs1:type_size(Type_name, Size, Alignment)
%		Size is the size of a Type_name, in bytes.
%		Alignment is the byte alignment required for this
%		type; that is, the address of the beginning of an
%		instance of Type_name must be divisible by Alignment.
%		Sadly, alignment is completely machine and compiler
%		specific.
%
%	structs1:get_contents(Datum, Part, Value)
%		This procedure is actually code used to get Part
%		of Datum, which is a compound datum.  Value is
%		bound to the contents of Part of Datum.
%
%	structs1:put_contents(Datum, Part, Value)
%		This procedure is actually code used to put Part
%		of Datum, which is a compound datum.  Part of Datum
%		is set to Value.
%
%	structs1:get_address(Datum, Part, Value)
%		This procedure is actually code used to get the
%		address of Part	of Datum, which is a compound datum.
%		Value is bound to the address of Part of Datum.
%
%  There are also three separate procedures for each struct defined,
%  using the names get_X_field, put_X_field, and get_X_address,
%  where X is the name of the structure.  Similarly, there are two
%  procedures for each union defined, named get_X_field and
%  get_X_address.  All of these procedures are 3-ary.
%

:- multifile
	structs1:type_defn/2,
	structs1:type_size/3,
	structs1:type_file/2,
	structs1:get_contents/3,
	structs1:put_contents/3,
	structs1:get_address/3.

%% [PM] 4.1.3 Make SPIDER see a reference
:- public structs1:type_file/2.

%  Start out knowing how to deal with the primitives

structs1:get_contents(integer(Ptr), contents, Value) :- !,
	get_integer(Ptr, 0, Value).
structs1:get_contents(integer_64(Ptr), contents, Value) :- !,
	get_integer_64(Ptr, 0, Value).
structs1:get_contents(integer_32(Ptr), contents, Value) :- !,
	get_integer_32(Ptr, 0, Value).
structs1:get_contents(integer_16(Ptr), contents, Value) :- !,
	get_integer_16(Ptr, 0, Value).
structs1:get_contents(integer_8(Ptr), contents, Value) :- !,
	get_integer_8(Ptr, 0, Value).
structs1:get_contents(unsigned(Ptr), contents, Value) :- !,
	get_unsigned(Ptr, 0, Value).
structs1:get_contents(unsigned_64(Ptr), contents, Value) :- !,
	get_unsigned_64(Ptr, 0, Value).
structs1:get_contents(unsigned_32(Ptr), contents, Value) :- !,
	get_unsigned_32(Ptr, 0, Value).
structs1:get_contents(unsigned_16(Ptr), contents, Value) :- !,
	get_unsigned_16(Ptr, 0, Value).
structs1:get_contents(unsigned_8(Ptr), contents, Value) :- !,
	get_unsigned_8(Ptr, 0, Value).
structs1:get_contents(float(Ptr), contents, Value) :- !,
	get_float(Ptr, 0, Value).
structs1:get_contents(float_32(Ptr), contents, Value) :- !,
	get_float_32(Ptr, 0, Value).
structs1:get_contents(atom(Ptr), contents, Value) :- !,
	get_atom(Ptr, 0, Value).
structs1:get_contents(string(Ptr), contents, Value) :- !,
	get_string(Ptr, 0, Value).


structs1:put_contents(integer(Ptr), contents, Value) :- !,
	put_integer(Ptr, 0, Value).
structs1:put_contents(integer_64(Ptr), contents, Value) :- !,
	put_integer_64(Ptr, 0, Value).
structs1:put_contents(integer_32(Ptr), contents, Value) :- !,
	put_integer_32(Ptr, 0, Value).
structs1:put_contents(integer_16(Ptr), contents, Value) :- !,
	put_integer_16(Ptr, 0, Value).
structs1:put_contents(integer_8(Ptr), contents, Value) :- !,
	put_integer_8(Ptr, 0, Value).
structs1:put_contents(unsigned(Ptr), contents, Value) :- !,
	put_unsigned(Ptr, 0, Value).
structs1:put_contents(unsigned_64(Ptr), contents, Value) :- !,
	put_unsigned_64(Ptr, 0, Value).
structs1:put_contents(unsigned_32(Ptr), contents, Value) :- !,
	put_unsigned_32(Ptr, 0, Value).
structs1:put_contents(unsigned_16(Ptr), contents, Value) :- !,
	put_unsigned_16(Ptr, 0, Value).
structs1:put_contents(unsigned_8(Ptr), contents, Value) :- !,
	put_unsigned_8(Ptr, 0, Value).
structs1:put_contents(float(Ptr), contents, Value) :- !,
	put_float(Ptr, 0, Value).
structs1:put_contents(float_32(Ptr), contents, Value) :- !,
	put_float_32(Ptr, 0, Value).
structs1:put_contents(atom(Ptr), contents, Value) :- !,
	put_atom(Ptr, 0, Value).
structs1:put_contents(string(Ptr), contents, Value) :- !,
	put_string(Ptr, 0, Value).


structs1:get_address(integer(Ptr), contents, integer(Ptr)) :- !.
structs1:get_address(integer_64(Ptr), contents, integer_64(Ptr)) :- !.
structs1:get_address(integer_32(Ptr), contents, integer_32(Ptr)) :- !.
structs1:get_address(integer_16(Ptr), contents, integer_16(Ptr)) :- !.
structs1:get_address(integer_8(Ptr), contents, integer_8(Ptr)) :- !.
structs1:get_address(unsigned(Ptr), contents, unsigned(Ptr)) :- !.
structs1:get_address(unsigned_64(Ptr), contents, unsigned_64(Ptr)) :- !.
structs1:get_address(unsigned_32(Ptr), contents, unsigned_32(Ptr)) :- !.
structs1:get_address(unsigned_16(Ptr), contents, unsigned_16(Ptr)) :- !.
structs1:get_address(unsigned_8(Ptr), contents, unsigned_8(Ptr)) :- !.
structs1:get_address(float(Ptr), contents, float(Ptr)) :- !.
structs1:get_address(float_32(Ptr), contents, float_32(Ptr)) :- !.
structs1:get_address(atom(Ptr), contents, atom(Ptr)) :- !.
structs1:get_address(string(Ptr), contents, string(Ptr)) :- !.


/****************************************************************
			    Public procedures
 ****************************************************************/

%  foreign_type(+Datum, -Type)
%  Datum is a data structure as returned by some of the procedures
%  in this module, and Type is its type.

foreign_type(Datum, Type) :-
	must_be(Datum, nonvar, foreign_type(Datum,Type), 1),
	functor(Datum, Type, 1),
	arg(1, Datum, Addr),
	integer(Addr),
	(   structs1:type_size(Type, _, _) -> true
	;   primitive_type_size(Type, _, _) -> true
	).


%  get_contents(+Datum, Part, -Value)
%  Value is unified with the contents of the Part part of Datum.
%  If Datum is an array, Part should be an integer index into the array,
%  where 0 is the first element.  For a pointer, Part should be the
%  atom 'contents' and Value will be what the pointer points to.  For
%  a struct, Part should be a field name, and Value will be the contents
%  of that field.  If Part is unbound, then get_contents will backtrack
%  through all the valid parts of Datum, binding both Part and Value.
%  A C programmer might thing of get_contents(Foo, Bar, Baz) as being
%  like Baz = Foo->Bar.
%
%  The hitch is that only atomic and pointer types can be returned.
%  This is because Prolog can only hold pointers to C structures, not
%  the structures themselves.  This isn't quite as bad as it might seem,
%  though, since usually structures contain pointers to other structures,
%  anyway.  When a structure directly contains another structure, Prolog
%  can get a pointer to it with get_address/3.

get_contents(Datum, Part, Value) :-
	(   var(Datum) ->
		illarg(var,get_contents(Datum,Part,Value),1)
	;   nonvar(Part) ->
		% if Part is bound at call, print error if it's invalid
		(   structs1:get_contents(Datum, Part, Value1) -> Value=Value1
		;   illarg(domain(term,part_of_foreign_object),get_contents(Datum,Part,Value),2, Part)
		)
	% if Part is unbound, backtrack through all valid Parts
	;   structs1:get_contents(Datum, Part, Value)
	).


% put_contents(+Datum, +Part, +Value)
% Value is put into the Part field of Datum.  Value is checked to make
% sure it is the expected type.  Interpretation of the arguments is as
% for get_contents/3 above.  Note that for put_contents, Part must be
% bound and cannont be backtracked through.
%  A C programmer might thing of put_contents(Foo, Bar, Baz) as being
%  like Foo->Bar = Baz.

put_contents(Datum, Part, Value) :-
	(   var(Datum) ->
		illarg(var,put_contents(Datum,Part,Value),1)
	% for put_contents, Part MUST be bound at call time
	;   var(Part) ->
		illarg(var,put_contents(Datum,Part,Value),2)
	;   structs1:put_contents(Datum, Part, Value) -> true
	;   illarg(domain(term,part_of_foreign_object),put_contents(Datum,Part,Value),2,Part)
	).


% get_address(+Datum, ?Part, -Value)
% Value is unified with a pointer to the Part part of Datum.
% Interpretation of the arguments is as for get_contents/3 above.
%  A C programmer might thing of get_address(Foo, Bar, Baz) as being
%  like Baz = &Foo->Bar.

get_address(Datum, Part, Value) :-
	(   var(Datum) ->
		illarg(var,get_address(Datum,Part,Value),1)
	;   nonvar(Part) ->
		% if Part is bound at call, print error if it's invalid
		(   structs1:get_address(Datum, Part, Value1) -> Value = Value1
		;   illarg(domain(term,part_of_foreign_object),get_address(Datum,Part,Value),2,Part)
		)
	% if Part is unbound, backtrack through all valid Parts
	;   structs1:get_address(Datum, Part, Value)
	).


% type_size(+Type, -Size)
% Type is user defined or a primitive type
type_size(Type, Size) :-
    ( structs1:type_size(Type, Size, _) -> true
    ; primitive_type_size(Type, Size, _)
    ).


%  new(+Type, -Datum)
%  Datum is a freshly allocated object of type Type.
%  Note, this uses calloc which initializes the returned memory to zeros

new(Type, Datum) :-
	(   var(Type) ->
		illarg(var,new(Type,Datum),1)
	;   type_size(Type, Size),
	    integer(Size) ->
		functor(Datum, Type, 1),
		arg(1, Datum, Ptr),
		calloc(1, Size, Ptr)
	;   illarg(domain(term,fixed_size_type),new(Type,Datum),1,Type)
	).


%  new(+Type, +Size, -Datum)
%  Datum is a freshly allocated object of type Type, which is an
%  unknown-size array type (as defined by array(Type), rather than
%  array(Type,Size)), and Size is the number of elements that should
%  be allocated.  The contents of Datum are zeroed.

new(Type, Size, Datum) :-
	Goal = new(Type,Size,Datum),
	must_be(Type, nonvar, Goal, 1),
	must_be(Size, integer, Goal, 2),
	structs1:type_defn(Type, array(Component_type)),
	(   (   structs1:type_size(Component_type, Elt_size, _) -> true
	    ;   primitive_type_size(Component_type, _, Elt_size)
	    ),
	    integer(Elt_size) ->
	        functor(Datum, Type, 1),
	        arg(1, Datum, Ptr),
	        calloc(Size, Elt_size, Ptr)
	    % shouldn't be possible for integer(Elt_size) to fail, since it
	    % shouldn't be possible to declare an array of non-fixed size elts.
	;   illarg(domain(term,fixed_size_type), Goal, 1, Type)
	).


%  dispose(+Datum)
%  Datum is a data structure as returned by some of the procedures in
%  this module.  After this call, Datum is deallocated, and MUST not
%  be referenced again.  No effort is made by this package to prevent
%  the horrible death that could follow using Datum after it has been
%  disposed.

dispose(Datum) :-
	Goal = dispose(Datum),
	must_be(Datum, nonvar, Goal, 1),
	(   functor(Datum, Type, 1),
	    type_size(Type, _) ->
		arg(1, Datum, Ptr),
		free(Ptr)
	;   illarg(domain(term,foreign_object), Goal, 1, Datum)
	).


%  cast(+Foreign0, +New_type, -Foreign)
%  Foreign is the foreign term which is the same data as Foreign0, only
%  is of foreign type New_type.  Foreign0 is not affected.  This also works
%  if Foreign0 is an address type (i.e., an integer address) or New_type
%  is an address type.  If New_type is an atomic type, Foreign will still
%  be a foreign term.

cast(Foreign0, New_type, Foreign) :-
	Goal = cast(Foreign0, New_type, Foreign),
	must_be(Foreign0, nonvar, Goal, 1),
	must_be(New_type, nonvar, Goal, 2),
	(   (   structs1:type_defn(New_type, New_type_defn)
	    ;   primitive_type_size(New_type, _, _), New_type_defn = New_type
	    ) ->
		(   integer(Foreign0) ->
			Addr = Foreign0		% casting FROM address type
		;   arg(1, Foreign0, Addr)	% casting FROM foreign term
		),
		(   New_type_defn == address ->
			Foreign = Addr		% casting TO address type
		;   functor(Foreign, New_type, 1),
						% casting TO foreign term
		    arg(1, Foreign, Addr)
		)
	;   illarg(domain(term,foreign_type),cast(Foreign0,New_type,Foreign),2,New_type)
	).


%   null_foreign_term(?Term, ?Type)
%   holds when Term is a foreign term of Type, but is NULL (the address
%   is 0).  At least one of Term and Type must be bound.  This can be
%   used to generate NULL foreign terms, or to check a foreign term to
%   determine whether or not it is NULL.

null_foreign_term(Term, Type) :-
	functor(Term, Type, 1),
	arg(1, Term, 0).


%  type_definition(?Type, -Definition)
%  type_definition(?Type, -Definition, -Size)
%  Type is a previously defined type, and Definition is its definition.
%  A definition looks much like the definition given when the type was
%  defined with type/1, except that it has been simplified.  Firstly,
%  intermediate type names have been elided.  For example, if foo is
%  defined as foo=integer, and bar as bar=foo, then
%  type_definition(bar, integer) would hold.  Also, in the definition
%  of a compound type, types of parts are always defined by type names,
%  rather than complex specifications.  So if the type of a field in
%  a struct was defined as pointer(fred), it will show up in the
%  definition as '$fred'.  Of course, type_definition('$fred', pointer(fred))
%  would hold, also.  Size is the size in bytes one of these objects
%  would occupy.

type_definition(Type, Definition) :-
	structs1:type_defn(Type, Definition0),
	cleanup_definition(Definition0, Definition).

type_definition(Type, Definition, Size) :-
	structs1:type_defn(Type, Definition0),
	cleanup_definition(Definition0, Definition),
	structs1:type_size(Type, Size, _).


%  atomic_type(?Type)
%  atomic_type(?Type, -Primitive_type)
%  atomic_type(?Type, -Primitive_type, -Size)
%  atomic_type(?Type, -Primitive_type, -Size, -Alignment)
%  Type is an atomic.  See the discussion above for the definition of
%  an atomic type.  Primitive_type is the primitive type that Type
%  is defined in terms of.  Size is the number of bytes occupied by
%  an object of type Type.  Alignment is the byte alignment expected
%  for things of this type.  That is, the address of one of these
%  things will be divisible by Alignment.

atomic_type(Type) :-
	atomic_type(Type, _, _, _).

atomic_type(Type, Primitive_type) :-
	atomic_type(Type, Primitive_type, _, _).

atomic_type(Type, Primitive_type, Size) :-
	atomic_type(Type, Primitive_type, Size, _).

atomic_type(Type, Primitive_type, Size, Alignment) :-
	atom(Type),
	(   primitive_type_size(Type, Size, Alignment) -> Primitive_type = Type
	;   structs1:type_defn(Type, Type_expr),
	    atomic_type(Type_expr, Primitive_type, Size, Alignment)
	).



/****************************************************************
			Procedures for all types
 ****************************************************************/

%  cleanup_definition(+Definition0, -Definition)
%  Definition is the definition we want to show users corresponding to
%  Definition0, a definition as we store them internally.
%  NB:  this is also defined in structs_decl.pl.

cleanup_definition(Definition0, Definition) :-
	(   Definition0 = struct(Fields0) ->
		Definition = struct(Fields),
	    (   foreach(field(N,T,_),Fields0),
		foreach(N:T,Fields)
	    do  true
	    )
	;   Definition = Definition0
	).

/****************************************************************
			Runtime support
 ****************************************************************/
%% [PM] 4.1.3 calls generated by term_expansion.
:- public between/3.
:- between/3 is nondet.
between(L, L, L) :- !.
between(L, _, L).		% between(L, U, L) :- L =< U.
between(L, U, N) :-		% between(L, U, N) :- L < U,
	M is L+1,		%	M is L+1,
	between(M, U, N).	%	between(M, U, N).

end_of_file.

/****************************************************************
			Interface to foreign accessors
 ****************************************************************/

get_unsigned(Address, Offset, Value) :-
	raw_get_unsigned(Address, Offset, Mask/* -1<<(wordsize-1) */, Raw),
	(Raw >= 0 -> Value = Raw ; Value is Raw-Mask-Mask).

get_unsigned_64(Address, Offset, Value) :-
	raw_get_unsigned_64(Address, Offset, Mask/* -1<<(wordsize-1) */, Raw),
	(Raw >= 0 -> Value = Raw ; Value is Raw-Mask-Mask).

get_unsigned_32(Address, Offset, Value) :-
	raw_get_unsigned_32(Address, Offset, Mask/* -1<<(wordsize-1) */, Raw),
	(Raw >= 0 -> Value = Raw ; Value is Raw-Mask-Mask).

put_integer(Address, Offset, Value) :-
	primitive_type_size(integer, Size, Size),
	put_integer(Size, Address, Offset, Value).

put_integer(8, Address, Offset, Value) :-
	(   (Value>>63)=:=(Value>>64) ->
	    raw_put_integer(Address, Offset, Value)
	;   illarg(representation(nofit(integer)), put_integer(Address, Offset, Value), 3)
	).

put_integer(4, Address, Offset, Value) :-
	(   (Value>>31)=:=(Value>>32) ->
	    raw_put_integer(Address, Offset, Value)
	;   illarg(representation(nofit(integer)), put_integer(Address, Offset, Value), 3)
	).

put_integer_64(Address, Offset, Value) :-
	(   (Value>>63)=:=(Value>>64) ->
	    raw_put_integer_64(Address, Offset, Value)
	;   illarg(representation(nofit(integer_64)), put_integer_64(Address, Offset, Value), 3)
	).

put_integer_32(Address, Offset, Value) :-
	(   (Value>>31)=:=(Value>>32) ->
	    raw_put_integer_32(Address, Offset, Value)
	;   illarg(representation(nofit(integer_32)), put_integer_32(Address, Offset, Value), 3)
	).

put_integer_16(Address, Offset, Value) :-
	(   (Value>>15)=:=(Value>>16) ->
	    raw_put_integer_16(Address, Offset, Value)
	;   illarg(representation(nofit(integer_16)), put_integer_16(Address, Offset, Value), 3)
	).

put_integer_8(Address, Offset, Value) :-
	(   (Value>>7)=:=(Value>>8) ->
	    raw_put_integer_8(Address, Offset, Value)
	;   illarg(representation(nofit(integer_8)), put_integer_8(Address, Offset, Value), 3)
	).

put_unsigned(Address, Offset, Value) :-
	primitive_type_size(unsigned, Size, Size),
	put_unsigned(Size, Address, Offset, Value).

put_unsigned(8, Address, Offset, Value) :-
	(   Value>=0,
	    (Value>>64)=:=(Value>>65) ->
	    raw_put_unsigned(Address, Offset, Value)
	;   illarg(representation(nofit(unsigned)), put_unsigned(Address, Offset, Value), 3)
	).

put_unsigned(4, Address, Offset, Value) :-
	(   Value>=0,
	    (Value>>32)=:=(Value>>33) ->
	    raw_put_unsigned(Address, Offset, Value)
	;   illarg(representation(nofit(unsigned)), put_unsigned(Address, Offset, Value), 3)
	).

put_unsigned_64(Address, Offset, Value) :-
	(   Value>=0,
	    (Value>>64)=:=(Value>>65) ->
	    raw_put_unsigned_64(Address, Offset, Value)
	;   illarg(representation(nofit(unsigned_64)), put_unsigned_64(Address, Offset, Value), 3)
	).

put_unsigned_32(Address, Offset, Value) :-
	(   Value>=0,
	    (Value>>32)=:=(Value>>33) ->
	    raw_put_unsigned_32(Address, Offset, Value)
	;   illarg(representation(nofit(unsigned_32)), put_unsigned_32(Address, Offset, Value), 3)
	).

put_unsigned_16(Address, Offset, Value) :-
	(   Value>=0,
	    (Value>>16)=:=(Value>>17) ->
	    raw_put_unsigned_16(Address, Offset, Value)
	;   illarg(representation(nofit(unsigned_16)), put_unsigned_16(Address, Offset, Value), 3)
	).

put_unsigned_8(Address, Offset, Value) :-
	(   Value>=0,
	    (Value>>8)=:=(Value>>9) ->
	    raw_put_unsigned_8(Address, Offset, Value)
	;   illarg(representation(nofit(unsigned_8)), put_unsigned_8(Address, Offset, Value), 3)
	).

foreign_resource(structs, ['_Sget_functor', '_Sget_atom', '_Sget_string',
'_Sget_integer', '_Sget_integer_64', '_Sget_integer_32', '_Sget_integer_16', '_Sget_integer_8',
'_Sget_unsigned', '_Sget_unsigned_64', '_Sget_unsigned_32', '_Sget_unsigned_16', '_Sget_unsigned_8',
'_Sget_float', '_Sget_float_32', '_Sput_atom', '_Sput_string',
'_Sput_integer', '_Sput_integer_64', '_Sput_integer_32', '_Sput_integer_16', '_Sput_integer_8',
'_Sput_unsigned', '_Sput_unsigned_64', '_Sput_unsigned_32', '_Sput_unsigned_16', '_Sput_unsigned_8',
'_Sput_float', '_Sput_float_32', '_Scalloc', '_Sfree']).


foreign('_Sget_functor', c, get_functor(+address(char), [-atom])).
foreign('_Sget_atom', c, get_atom(+address(char), +integer, [-atom])).
foreign('_Sget_string', c, get_string(+address(char), +integer, [-string])).
foreign('_Sget_integer', c, get_integer(+address(char), +integer, [-integer])).
foreign('_Sget_integer_64', c, get_integer_64(+address(char), +integer, [-integer])).
foreign('_Sget_integer_32', c, get_integer_32(+address(char), +integer, [-integer])).
foreign('_Sget_integer_16', c, get_integer_16(+address(char), +integer, [-integer])).
foreign('_Sget_integer_8', c, get_integer_8(+address(char), +integer, [-integer])).
foreign('_Sget_unsigned', c, raw_get_unsigned(+address(char), +integer, -integer, [-integer])).
foreign('_Sget_unsigned_64', c, raw_get_unsigned_64(+address(char), +integer, -integer, [-integer])).
foreign('_Sget_unsigned_32', c, raw_get_unsigned_32(+address(char), +integer, -integer, [-integer])).
foreign('_Sget_unsigned_16', c, get_unsigned_16(+address(char), +integer, [-integer])).
foreign('_Sget_unsigned_8', c, get_unsigned_8(+address(char), +integer, [-integer])).
foreign('_Sget_float', c, get_float(+address(char), +integer, [-float])).
foreign('_Sget_float_32', c, get_float_32(+address(char), +integer, [-float])).
foreign('_Sput_atom', c, put_atom(+address(char), +integer, +atom)).
foreign('_Sput_string', c, put_string(+address(char), +integer, +string)).
foreign('_Sput_integer', c, raw_put_integer(+address(char), +integer, +integer)).
foreign('_Sput_integer_64', c, raw_put_integer_64(+address(char), +integer, +integer)).
foreign('_Sput_integer_32', c, raw_put_integer_32(+address(char), +integer, +integer)).
foreign('_Sput_integer_16', c, raw_put_integer_16(+address(char), +integer, +integer)).
foreign('_Sput_integer_8', c, raw_put_integer_8(+address(char), +integer, +integer)).
foreign('_Sput_unsigned', c, raw_put_unsigned(+address(char), +integer, +integer)).
foreign('_Sput_unsigned_64', c, raw_put_unsigned_64(+address(char), +integer, +integer)).
foreign('_Sput_unsigned_32', c, raw_put_unsigned_32(+address(char), +integer, +integer)).
foreign('_Sput_unsigned_16', c, raw_put_unsigned_16(+address(char), +integer, +integer)).
foreign('_Sput_unsigned_8', c, raw_put_unsigned_8(+address(char), +integer, +integer)).
foreign('_Sput_float', c, put_float(+address(char), +integer, +float)).
foreign('_Sput_float_32', c, put_float_32(+address(char), +integer, +float)).

foreign('_Scalloc', c, calloc(+integer, +integer, [-address(opaque)])).
foreign('_Sfree', c, free(+address(opaque))).

:- load_foreign_resource(library(system(structs))).
