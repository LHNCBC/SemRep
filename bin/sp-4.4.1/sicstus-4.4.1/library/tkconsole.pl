/* tkconsole.pl; a simple console window written in Tcl/Tk. */

:- module(tkconsole,[tk_console/0]).

:- use_module(library(tcltk)).

tk_console :-
	absolute_file_name(library('tkconsole.tcl'),TkConSrc),
	tk_new([name('SICStus Prolog')], Interp),
	tcl_eval(Interp, [codes("source"),write(TkConSrc)], _),
	tcl_eval(Interp, codes("tk_console_window"), TextWidget),
	tk_terminal(Interp, TextWidget, InStream, OutStream, ErrStream),
	set_prolog_flag(user_input, InStream),
	set_prolog_flag(user_output, OutStream),
	set_prolog_flag(user_error, ErrStream),
	set_input(InStream),
	set_output(OutStream),
	current_prolog_flag(version,V),
	write(V),nl,
	true.
