/* Copyright (C) 1995, Swedish Institute of Computer Science. */

%   File       : codesio.pl
%   Author     : Stefan Andersson
%   Updated    : 3 September 1999
%   Purpose    : Defines predicates that read from, or write to, 
%              : a list of character codes.


:- module(codesio, [
	format_to_codes/3,
	format_to_codes/4,
	write_to_codes/2,
	write_to_codes/3,
	write_term_to_codes/3,
	write_term_to_codes/4,
%% [PM] 4.0 gone, use {atom,number}_codes instead.
%%	atom_to_codes/2,
%%	atom_to_codes/3,
%%	number_to_codes/2,
%%	number_to_codes/3,
	read_from_codes/2,
	read_term_from_codes/3,
	open_codes_stream/2,
	with_output_to_codes/2,
	with_output_to_codes/3,
	with_output_to_codes/4
		   ]).

:- meta_predicate	
	format_to_codes(?,:,?),
	format_to_codes(?,:,?,?),
	with_output_to_codes(0,?),
	with_output_to_codes(0,?,?),
	with_output_to_codes(0,?,?,?).

:- use_module(library(types), [
        illarg/3
	]).

:- meta_predicate
        call_to_codes(0, +, ?, ?).


%@  This package defines I/O predicates that read from, or write to, a
%@  code-list.  There are also predicates to open a stream
%@  referring to a code-list.  The stream may be used with
%@  general Stream I/O predicates.
%@  
%@  Exported predicates:
%@  
%@  @table @code

%@  @item format_to_codes(@var{+Format}, @var{:Arguments}, @var{-Codes})
%@  @itemx format_to_codes(@var{+Format}, @var{:Arguments}, @var{?S0}, @var{?S})
%@  @PLXindex {format_to_codes/[3,4] (codesio)}
%@  Prints @var{Arguments} into a code-list using @code{format/3}.  @var{Codes} is unified with the list,
%@  alternatively @var{S0} and @var{S} are unified with the list and its end, respectively.

format_to_codes(Fmt, Args, Codes) :-
	format_to_codes(Fmt, Args, Codes, []).

format_to_codes(Fmt, Args, S0, S) :-
	with_output_to_codes(format(Stream, Fmt, Args), Stream, S0, S).


%@  @item write_to_codes(@var{+Term}, @var{-Codes})
%@  @itemx write_to_codes(@var{+Term}, @var{?S0}, @var{?S})
%@  @PLXindex {write_to_codes/[2,3] (codesio)}
%@  A specialized @code{format_to_codes/[3,4]}. Writes @var{Term} into a
%@  code-list using @code{write/2}.  @var{Codes} is
%@  unified with the list.  Alternatively, @var{S0} and @var{S}
%@  are unified with the list and its end,
%@  respectively.

write_to_codes(Term, Codes) :-
	write_to_codes(Term, Codes, []).

write_to_codes(Term, S0, S) :-
        with_output_to_codes(write(Stream, Term), Stream, S0, S).



%@  @item write_term_to_codes(@var{+Term}, @var{-Codes}, @var{+Options})
%@  @itemx write_term_to_codes(@var{+Term}, @var{?S0}, @var{?S}, @var{+Options})
%@  @PLXindex {write_term_to_codes/[3,4] (codesio)}
%@  A specialized @code{format_to_codes/[3,4]}. Writes @var{Term} into a
%@  code-list using @code{write_term/3} and @var{Options}.  @var{Codes} is
%@  unified with the list.  Alternatively, @var{S0} and @var{S} are
%@  unified with the list and its end, respectively.

write_term_to_codes(Term, Codes, Options) :-
	write_term_to_codes(Term, Codes, [], Options).

write_term_to_codes(Term, S0, S, Options) :-
        with_output_to_codes(write_term(Stream,Term,Options), Stream, S0, S).



%% [PM] 4.0 gone
%% %@  @item atom_to_codes(@var{+Atom}, @var{-Codes})
%% %@  @item atom_to_codes(@var{+Atom}, @var{?S0}, @var{?S})
%% %@  @PLindex atom_to_codes/[2,3] (codesio)
%% %@  A specialized @code{format_to_codes/[3,4]}.  Converts @var{Atom} to the
%% %@  list of characters comprising its name.  @var{Codes} is
%% %@  unified with the list, alternatively @var{S0} and @var{S}
%% %@  are unified with the list and its end,
%% %@  respectively.
%%
%% atom_to_codes(Atom, Codes) :-   % a.k.a. atom_codes/2?
%%         atom_to_codes(Atom, Codes, []).
%%
%% atom_to_codes(Atom, S0, S) :-
%%         must_be(Atom, atom, atom_to_codes(Atom,S0,S), 1),
%%         '$atom_to_codes'(Atom, S0, S).
%%
%%
%%
%% %@  @item number_to_codes(@var{+Number}, @var{-Codes})
%% %@  @item number_to_codes(@var{+Number}, @var{?S0}, @var{?S})
%% %@  @PLindex number_to_codes/[2,3] (codesio)
%% %@  A specialized @code{format_to_codes/[3,4]}.  Converts @var{Number} to
%% %@  the list of characters comprising its name.  @var{Codes} is
%% %@  unified with the list, alternatively @var{S0} and @var{S}
%% %@  are unified with the list and its end,
%% %@  respectively.
%%
%% number_to_codes(Number, Codes) :- % a.k.a. number_codes/2?
%%         number_to_codes(Number, Codes, []).
%%
%% number_to_codes(Number, S0, S) :-
%%         must_be(Number, number, number_to_codes(Number,S0,S), 1),
%%         '$number_to_codes'(Number, S0, S).		% in C
%%

%@  @item read_from_codes(@var{+Codes}, @var{-Term})
%@  @PLXindex {read_from_codes/2 (codesio)}
%@  Reads @var{Term} from @var{Codes} using @code{read/2}.  The @var{Codes}
%@  must, as usual, be terminated by a @var{full-stop}, i.e.@: a @samp{.},
%@  possibly followed by @var{layout-text}.

read_from_codes(Codes, Term) :-
	open_codes_stream(Codes, Stream),
	call_cleanup(read(Stream, Term0),
                     close(Stream)),
	Term = Term0.


%@  @item read_term_from_codes(@var{+Codes}, @var{-Term}, @var{+Options})
%@  @PLXindex {read_term_from_codes/3 (codesio)}
%@  Reads @var{Term} from @var{Codes} using @code{read_term/3} and
%@  @var{Options}.  The @var{Codes} must, as usual, be terminated by a
%@  @var{full-stop}, i.e.@: a @samp{.}, possibly followed by
%@  @var{layout-text}.

read_term_from_codes(Codes, Term, Options) :-
    open_codes_stream(Codes, Stream),
    call_cleanup(read_term(Stream, Term0, Options),
		 close(Stream)),
    Term = Term0.


%@  @item open_codes_stream(@var{+Codes}, @var{-Stream})
%@  @PLXindex {open_codes_stream/2 (codesio)}
%@  @var{Stream} is opened as an input stream to an existing
%@  code-list.  The stream may be read with the Stream I/O
%@  predicates and must be closed using @code{close/1}.  The
%@  list is copied to an internal buffer when the stream is
%@  opened and must therefore be a ground code-list at that
%@  point.

open_codes_stream(Codes, Stream) :- !,
	'$codes_to_stream'(Codes, StreamCode),
	stream_code(Stream, StreamCode).



%@  @item with_output_to_codes(@var{:Goal}, @var{-Codes})
%@  @itemx with_output_to_codes(@var{:Goal}, @var{?S0}, @var{?S})
%@  @itemx with_output_to_codes(@var{:Goal}, @var{-Stream}, @var{?S0}, @var{?S})
%@  @PLXindex {with_output_to_codes/[2,3,4] (codesio)}
%@  @var{Goal} is called with the @code{current_output} stream set to
%@  a new stream. This stream writes to an internal buffer,
%@  which is, after the successful execution of @var{Goal}, converted to a
%@  list of character codes.  @var{Codes} is unified with the
%@  list, alternatively @var{S0} and @var{S} are unified with
%@  the list and its end, respectively.
%@  @code{with_output_to_codes/4} also passes the stream in the
%@  @var{Stream} argument. It can be used only by @var{Goal} for
%@  writing.

with_output_to_codes(Goal, Codes) :-
	with_output_to_codes(Goal, Codes, []).

with_output_to_codes(Goal, S0, S) :-
	with_output_to_codes(Goal, _, S0, S).

with_output_to_codes(Goal, Stream, S0, S) :-
	'$open_buf_stream'(StreamCode),
	StreamCode \== 0,
	stream_code(Stream, StreamCode),
	current_output(CurrOut),
	set_output(Stream),
	call_cleanup(call_to_codes(Goal,StreamCode,S0,S),
		     reset_stream(Stream,CurrOut)).

call_to_codes(Goal, StreamCode, S0, S) :-
	once(Goal),
	'$stream_to_codes'(StreamCode, S0_1, S_1),
        ( S0_1 == 'error' ->
           stream_code(Stream, StreamCode),
           illarg(system('Extracting codes from stream'), with_output_to_codes(Goal, Stream, S0, S), 0)
        ; otherwise ->
          S0 = S0_1,
          S = S_1
        ).


reset_stream(Stream, CurrOut) :-
	set_output(CurrOut),
	close(Stream).


%% foreign(xatom_to_codes, '$atom_to_codes'(+string,-term,-term)).
%% foreign(xnumber_to_codes, '$number_to_codes'(+term,-term,-term)).
foreign(codes_to_stream, '$codes_to_stream'(+codes,-address('SP_stream'))).
foreign(open_buf_stream, '$open_buf_stream'(-address('SP_stream'))).
foreign(stream_to_codes, '$stream_to_codes'(+address('SP_stream'),-term,-term)).

foreign_resource(codesio, [
%%	xatom_to_codes,
%%	xnumber_to_codes,
        init(codesio_init),
        deinit(codesio_deinit),
	codes_to_stream,
	open_buf_stream,
	stream_to_codes
			  ]).

:- load_foreign_resource(library(system(codesio))).

%@  @end table
