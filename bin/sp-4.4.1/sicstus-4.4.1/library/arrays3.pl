/* Copyright(C) 1988, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Name: arrays.pl
%  Maintainer: Lena Flood
%  Date: 8 November 1988
%  Purpose: Extendable arrays with logarithmic access time
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Adapted from shared code written by David Warren and Fernando Pereira.

%  Array extends from 0 to 2**Size - 1, where Size is a multiple of 2.
%  Note that 2**Size = 1<<Size.

:- module(arrays3, [
	new_array/1,
	is_array/1,
	aref/3,
	arefa/3,
	arefl/3,
	aset/4,
	array_to_list/2
	]).

:- use_module(library(logarr), [
	new_array/1,
	is_array/1,
	aref/3,
	arefa/3,
	arefl/3,
	aset/4,
	alist/2
	]).

array_to_list(Array, List) :-
	alist(Array, List).


