:- module(alignments, [
	primitive_type_size/3,
	pointer_alignment/2,
	endianness/1
   ]).

end_of_file.

/* [PM] 4.0.5 This used to be generated (into
library/<platform>/alignments.pl) by structs/str_align.c) */


%% primitive_type_size(+Type, -Size, -Alignment).
/* 
primitive_type_size(integer, 4, 4).
primitive_type_size(integer_64, 8, 8).
primitive_type_size(integer_32, 4, 4).
primitive_type_size(integer_16, 2, 2).
primitive_type_size(integer_8, 1, 1).
primitive_type_size(unsigned, 4, 4).
primitive_type_size(unsigned_64, 8, 8).
primitive_type_size(unsigned_32, 4, 4).
primitive_type_size(unsigned_16, 2, 2).
primitive_type_size(unsigned_8, 1, 1).
primitive_type_size(float, 8, 4).
primitive_type_size(float_32, 4, 4).
primitive_type_size(atom, 4, 4).
primitive_type_size(string, 4, 4).
primitive_type_size(address, 4, 4).
primitive_type_size(opaque, unknown, unknown).
*/
primitive_type_size(Type, Size, Alignment) :-
  prolog:'$primitive_type_size'(Type, Size, Alignment).

%% pointer_alignment(-Size, -Alignment).
/*
  pointer_alignment(4, 4).
*/
pointer_alignment(Size, Alignment) :-
  prolog:'$primitive_type_size'(address, Size, Alignment).

%% endianness(-Endianness).
endianness(Endianness) :-
  prolog:'$endianness'(Endianness).

