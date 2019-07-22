%   Comma-Separated Values (CSV) files and strings.
%   Copyright (C) 2016, Swedish Institute of Computer Science.  All rights reserved.
%   Author: Mats Carlsson

:- module(csv, [
	read_record/1,
	read_record/2,
	read_records/1,
	read_records/2,
	read_record_from_codes/2,
	read_record_from_codes/3,
	write_record/1,
	write_record/2,
	write_records/1,
	write_records/2,
	write_record_to_codes/2
   ]).

:- use_module(library(lists), [
	append/2
   ]).

%@  This library module provides some utilities for Comma-Separated Values (CSV) files and strings.
%@  In this context, a file is a sequence of @emph{records}, and a record is a sequence of @emph{fields}.
%@  In a CSV file, fields are separated by commas, and each record is terminated by @key{RET}.
%@
%@  This module does not report any syntax errors.  In the event of prematurely terminated input file,
%@  the current field and record will be terminated silently.
%@  
%@  Then a CSV record is read, it will yield a list of fields of the following form:
%@  
%@  @table @code
%@  @item integer(@var{Number},@var{Codes})
%@  Stands for the integer @var{Number}, where @code{number_codes(@var{Number},@var{Codes})} holds,
%@  and @var{Codes} is the list of character codes actually read.
%@  
%@  @item float(@var{Number},@var{Codes})
%@  Stands for the float @var{Number}, where @code{number_codes(@var{Number},@var{Codes})} holds,
%@  and @var{Codes} is the list of character codes actually read.
%@  
%@  @item string(@var{Codes})
%@  Stands for the text string (list of character codes) @var{Codes}, and @code{number_codes(@var{Number},@var{Codes})}
%@  does not hold.
%@  @end table
%@  
%@  When a CSV records is written, the @var{Codes} argument of the above terms is used, but
%@  the following fields are also allowed:
%@  
%@  @table @code
%@  @item integer(@var{Number})
%@  Stands for the integer @var{Number}.
%@  
%@  @item float(@var{Number})
%@  Stands for the float @var{Number}.
%@  
%@  @item atom(@var{Atom})
%@  Stands for the atom @var{Atom}.
%@  @end table
%@  
%@  Adapted to the conventions of this manual, RFC 4180 specifies the following.  Where this module relaxes
%@  the requirements, that is explicitly mentioned:
%@  
%@  @enumerate
%@  @item
%@  Each record is located on a separate line, delimited by a line
%@  break. For example:
%@  @example
%@  @group
%@  aaa,bbb,ccc @key{RET}
%@  zzz,yyy,xxx @key{RET}
%@  @end group
%@  @end example
%@  
%@  @item
%@  The last record in the file may or may not have an ending line
%@  break. For example:
%@  @example
%@  @group
%@  aaa,bbb,ccc @key{RET}
%@  zzz,yyy,xxx
%@  @end group
%@  @end example
%@  
%@  @item
%@  There may be an optional header line appearing as the first line
%@  of the file with the same format as normal record lines. This
%@  header will contain names corresponding to the fields in the file
%@  and should contain the same number of fields as the records in
%@  the rest of the file. For example:
%@  @example
%@  @group
%@  field_name,field_name,field_name @key{RET}
%@  aaa,bbb,ccc @key{RET}
%@  zzz,yyy,xxx @key{RET}
%@  @end group
%@  @end example
%@  This module does not attempt to detect a header line nor
%@  treat it in any special way.
%@  
%@  @item
%@  Within the header and each record, there may be one or more
%@  fields, separated by commas. Each record should contain the same
%@  number of fields throughout the file. Spaces are considered part
%@  of a field and should not be ignored. The last field in the
%@  record must not be followed by a comma, so if the record ends with a
%@  comma, the last field is treated as empty.  For example, the following
%@  is treated as four fields:
%@  @example
%@  @group
%@  aaa,bbb,ccc,
%@  @end group
%@  @end example
%@  This module does not require or check that each record contains
%@  the same number of fields.
%@  
%@  @item
%@  Each field may or may not be enclosed in double quotes.
%@  If fields contain line breaks (@key{RET}), double quotes or commas,
%@  then they should be enclosed in double quotes, otherwise the double quotes may be omitted.
%@  For example:
%@  @example
%@  @group
%@  "aaa","bbb","ccc" @key{RET}
%@  "aaa","b @key{RET}
%@  bb","ccc" @key{RET}
%@  zzz,yyy,xxx
%@  @end group
%@  @end example
%@  If an unenclosed field is immediately followed by a @kbd{"}, (or vice versa), then this module
%@  treats that as a new enclosed (or unenclosed) field to be read and appended to the field read so far.
%@  
%@  @item
%@  If double quotes are used to enclose fields, then a double quote
%@  appearing inside a field must be escaped by preceding it with
%@  another double quote. For example:
%@  @example
%@  @group
%@  "aaa","b""bb","ccc"
%@  @end group
%@  @end example
%@  @end enumerate
%@  
%@  Exported predicates:
%@  @table @code
%@  
%@  @item read_record(@var{-Record})
%@  @itemx read_record(@var{+Stream}, @var{-Record})
%@  @PLXindex {read_record/[1,2] (csv)}
%@  Reads a single record from the stream @var{Stream}, which defaults to the current input stream,
%@  and unifies it with @var{Record}.  On end of file, @var{Record} is unified with @code{end_of_file}.

read_record(Record) :-
	current_input(S),
	read_record(S, Record).

read_record(S, Record) :-
	read_cont(S, Line),
	(   Line = end_of_file -> Record = end_of_file
	;   read_record_from_codes(Record, Line)
	).

read_cont(S, Line) :-
	read_line(S, Sofar),
	(   Sofar = end_of_file -> Line = end_of_file
	;   countq(Sofar, N),
	    read_cont(S, Sofar, N, Line)
	).

read_cont(_, Front, N, Front) :-
	N mod 2 =:= 0, !.
read_cont(S, Front, _, Line) :-
	read_line(S, Next),
	(   Next = end_of_file -> Line = Front
	;   append(Front, [10|Next], Sofar),
	    countq(Sofar, N),
	    read_cont(S, Sofar, N, Line)
	).

countq(Codes, K) :-
	(   foreach(C,Codes),
	    fromto(0,I,J,K)
	do  (C = 0'" -> J is I+1 ; J = I)
	).

%@  @item read_records(@var{-Records})
%@  @itemx read_records(@var{+Stream}, @var{-Records})
%@  @PLXindex {read_records/[1,2] (csv)}
%@  Reads records from the stream @var{Stream}, which defaults to the current input stream,
%@  up to the end of the stream, and unifies them with @var{Records}.  

read_records(Records) :-
	current_input(S),
	read_records(S, Records).

read_records(S, [Record|Records]) :-
	read_record(S, Record), 
	Record \== end_of_file, !,
	read_records(S, Records).
read_records(_, []).

%@  @item read_record_from_codes(@var{-Record}, @var{+Codes})
%@  @itemx read_record_from_codes(@var{-Record}, @var{+Codes}, @var{-Suffix})
%@  @PLXindex {read_record_from_codes/[2,3] (csv)}
%@  Reads a record from the code-list @var{Codes}.  
%@  In the arity 2 variant, there must be no trailing character codes after the record.
%@  In the arity 3 variant, any trailing character codes are unified with @var{Suffix},
%@  which can be used for reading subsequent records.

read_record_from_codes(Record, Codes) :-
	read_record_from_codes(Record, Codes, []).

read_record_from_codes(Record, S0, S) :-
	parse(F, R, S0, S),
	tag_fields([F|R], Record).

%@  @item write_record(@var{+Record})
%@  @itemx write_record(@var{+Stream}, @var{+Record})
%@  @PLXindex {write_record/[1,2] (csv)}
%@  Writes a single record to the stream @var{Stream}, which defaults to the current output stream.

write_record(Record) :-
	current_output(S),
	write_record(S, Record).

write_record(S, Record) :-
	(   foreach(Field,Record),
	    fromto('',Sep,',',_),
	    param(S)
	do  write(S, Sep),
	    write_field(Field, S)
	),
	nl(S).

%@  @item write_records(@var{+Records})
%@  @itemx write_records(@var{+Stream}, @var{+Records})
%@  @PLXindex {write_records/[1,2] (csv)}
%@  Writes records to the stream @var{Stream}, which defaults to the current output stream.

write_records(Records) :-
	current_output(S),
	write_records(S, Records).

write_records(S, Records) :-
	(   foreach(Record,Records),
	    param(S)
	do  write_record(S, Record)
	).

%@  @item write_record_to_codes(@var{+Record}, @var{-Codes})
%@  @PLXindex {write_record_to_codes/2 (csv)}
%@  Writes a single record to the code-list @var{Codes}, without the terminating @key{RET}.

write_record_to_codes(Record, Codes) :-
	(   foreach(Field,Record),
	    fromto(Parts,[Sep,Part|Tail],Tail,[]),
	    fromto([],Sep,[0',],_)
	do  field_part(Field, Part)
	),
	append(Parts, Codes).

write_field(float(F,_), S) :-
	write(S, F).
write_field(float(F), S) :-
	write(S, F).
write_field(integer(I), S) :-
	write(S, I).
write_field(integer(I,_), S) :-
	write(S, I).
write_field(atom(Atom), S) :-
	atom_codes(Atom, Codes),
	write_field(string(Codes), S).
write_field(string(Codes), S) :-
	nonmember(10, Codes),
	nonmember(0'", Codes),
	nonmember(0',, Codes), !,
	format(S, '~s', [Codes]).
write_field(string(Codes), S) :-
	write(S, '"'),
	(   foreach(C,Codes),
	    param(S)
	do  (C = 0'" -> write(S, '""') ; put_code(S, C))
	),
	write(S, '"').

field_part(float(F), Codes) :-
	float(F),
	number_codes(F, Codes).
field_part(float(_,Codes), Codes).
field_part(integer(F), Codes) :-
	integer(F),
	number_codes(F, Codes).
field_part(integer(_,Codes), Codes).
field_part(atom(Atom), Codes) :-
	atom_codes(Atom, String),
	field_part(string(String), Codes).
field_part(string(Codes), Codes) :-
	nonmember(10, Codes),
	nonmember(0'", Codes),
	nonmember(0',, Codes), !.
field_part(string(Codes), [0'"|S0]) :-
	(   foreach(C,Codes),
	    fromto(S0,S1,S2,[0'"])
	do  (C = 0'" -> S1 = [C,C|S2] ; S1 = [C|S2])
	).

parse([], [F|R]) --> ",", !,
	parse(F, R).
parse(F, R) --> "\"", !,
	parseq(F, R).
parse([], []) --> [0'\n], !.
parse([C|F], R) --> [C], !,
	parse(F, R).
parse([], []) --> [].

parseq([0'"|F], R) --> "\"\"", !,
	parseq(F, R).
parseq(F, R) --> "\"", !,
	parse(F, R).
parseq([C|F], R) --> [C], !,
	parseq(F, R).
parseq([], []) --> [].

tag_fields(Raw, Tagged) :-
	(   foreach(X,Raw),
	    foreach(Y,Tagged)
	do  on_exception(error(_,_), number_codes(Number, X), true),
	    (   float(Number) -> Y = float(Number,X)
	    ;   integer(Number) -> Y = integer(Number,X)
	    ;   Y = string(X)
	    )
	).

%@  @end table

