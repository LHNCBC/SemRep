/****************************************************************************
*
*                          PUBLIC DOMAIN NOTICE                         
*         Lister Hill National Center for Biomedical Communications
*                      National Library of Medicine
*                      National Institues of Health
*           United States Department of Health and Human Services
*                                                                         
*  This software is a United States Government Work under the terms of the
*  United States Copyright Act. It was written as part of the authors'
*  official duties as United States Government employees and contractors
*  and thus cannot be copyrighted. This software is freely available
*  to the public for use. The National Library of Medicine and the
*  United States Government have not placed any restriction on its
*  use or reproduction.
*                                                                        
*  Although all reasonable efforts have been taken to ensure the accuracy 
*  and reliability of the software and data, the National Library of Medicine
*  and the United States Government do not and cannot warrant the performance
*  or results that may be obtained by using this software or data.
*  The National Library of Medicine and the U.S. Government disclaim all
*  warranties, expressed or implied, including warranties of performance,
*  merchantability or fitness for any particular purpose.
*
***************************************************************************/

% File:	    debug.pl
% Module:   debug
% Author:   FML
% Purpose:  provide user-toggleable debugging facility

% ----- Module declaration and exported predicates

:- module(debug, [
%	dbh/0,
	sdp/0,
	sadp/0,
	ssdp/0,
	debug_call/3,
	set_debug_bit_vector/2,
	set_debug_points/0,
	show_debug_points/1,
	show_all_debug_points/0,
	show_set_debug_points/0
   ]).

:- use_module( usemrep_lib( portray_minimal_syntax ), [
	portray_minimal_syntax_structure/1
   ]).

:- use_module( skr_lib(nls_system), [
	control_option/1,
%	control_value/3
	control_value/2
   ]).


:- dynamic debug_point/1.


debug_data(1, 'usemrep.pl', perform_relational_analysis,
	    'Show analysis before and after call to intermediate_analysis').
debug_data(2, 'ssuppserv.pl', announce_check_relation,
	    'Checking type-relation-type').
debug_data(3, 'usemrep.pl', perform_referential_analysis,
	    'Announce METAMAP in/out').

set_debug_bit_vector(Mode, DebugBitVector) :-
	( Mode == runtime ->
	  get_debug_runtime_setting(DebugBitVector)
	; findall(SetDebugPoint, debug_point(SetDebugPoint), AllSetDebugPoints),
	  compute_bit_vector(AllSetDebugPoints, 0, DebugBitVector)
	).

get_debug_runtime_setting(DebugBitVector) :-
%	( control_value(debug_call, value, DebugBitVector) ->	  true
	( control_value(debug_call, DebugBitVector) ->	  true
	; DebugBitVector is 0
	).	
	
compute_bit_vector([], DebugBitVector, DebugBitVector).
compute_bit_vector([H|T], TempDebugBitVector, DebugBitVector) :-
	Hm1 is H - 1,
	%exp(2, Hm1, Power),
	Power is exp(2,Hm1),
	PowerInt is integer(Power),
	NextDebugBitVector is TempDebugBitVector + PowerInt,
	compute_bit_vector(T, NextDebugBitVector, DebugBitVector).
	
debug_call(DebugBitVector, DebugBit, DebugGoal) :-
	( DebugBitMinusOne is DebugBit - 1,
	  %exp(2, DebugBitMinusOne, Power),
	  Power is exp(2, DebugBitMinusOne),
	  PowerInt is integer(Power),
	  PowerInt /\ DebugBitVector > 0 ->
	  call_debug_goal(DebugGoal)
	; true
	).

call_debug_goal(format(FormatString, FormatArgs)) :-
	!,
	format(FormatString, FormatArgs).
call_debug_goal(portray_minimal_syntax_structure(Structure)) :-
	!,
	portray_minimal_syntax_structure(Structure).
call_debug_goal(OtherGoal) :-
	format('~n~n###Unexpected debug goal called:~n~q~n~n', OtherGoal),
	call(OtherGoal).

dbh :-
 	format('~nsdp:  set_debug_points/0~n', []),
 	format('sadp: show_all_debug_points/0~n', []),
 	format('ssdp: show_set_debug_points/0~n', []).

sdp :- set_debug_points.

set_debug_points :-
	show_all_debug_points,
	read(DebugTerm),
	comma_term_to_list(DebugTerm, DebugPointList),
	assert_all_debug_points(DebugPointList).

sadp :- show_all_debug_points.

show_all_debug_points :-
	show_debug_points('ALL').

ssdp :- show_set_debug_points.

show_set_debug_points :-
	show_debug_points('SET').

show_debug_points(Mode) :-
	( var(Mode) ->
	  Mode = 'ALL'
	; true
	),	
	format('~n~w debug Points:~n', [Mode]),
	debug_data(PointNum, File, Predicate, Description),
	require_debug_point_set(Mode, PointNum),
	nl,
	format('~w: In ~w (~w)~n   ~w~n', [PointNum, File, Predicate, Description]),
	fail
      ; nl.

require_debug_point_set('ALL', _PointNum).
require_debug_point_set('SET', PointNum) :- debug_point(PointNum).
	

comma_term_to_list((A,B), [A|Rest]) :-
	!,
	comma_term_to_list(B, Rest).
comma_term_to_list(B, [B]).

assert_all_debug_points(0) :- retractall(debug_point(_)).
assert_all_debug_points([]) :- show_set_debug_points.
assert_all_debug_points([DebugPoint|RestDebugPoints]) :-
	( \+ integer(DebugPoint) ->
	  format('~n>~w< is not an integer; allowable values are integers only:~n', [DebugPoint]),
	  show_all_debug_points
	; DebugPoint =:= 0 ->
	  retractall(debug_point(_))
	; \+ debug_data(DebugPoint, _, _, _) ->
	  format('~n~w is not a valid debug value:', [DebugPoint]),
	  show_all_debug_points
	; debug_point(DebugPoint) ->
	  true
	; assert(debug_point(DebugPoint))
	),
	assert_all_debug_points(RestDebugPoints).
