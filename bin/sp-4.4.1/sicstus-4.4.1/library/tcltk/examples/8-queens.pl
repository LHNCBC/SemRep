:- use_module(library(tcltk)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(file_systems), [current_directory/2]).

setup :-
   TkOptions = [],
   setup(TkOptions).

setup(TkOptions) :- 
    tk_new([name('SICStus+Tcl/Tk - Queens')|TkOptions], Tcl),
    absolute_file_name(library('tcltk/examples'), ExDir),
    current_directory(WD, ExDir),

    %% [PM] 3.11.2 FIXME: need a way to pass the SICStus current
    %% directory to Tcl/Tk. Expecially on platforms (like WinCE) where
    %% there is no global working directory (and on other platforms
    %% cwd is process global which makes it pretty much a no-no to
    %% change in any process with multiple threads.
    %%
    %% CN did this for the WinCE port:
    %% tcl_eval(Tcl, [cd, '//mink/e$/sicstus/sicstus39_ce/library/tcltk/examples/'], _),

    tcl_eval(Tcl, [source,'8-queens.tcl'], _),
    current_directory(_, WD),
    tk_next_event(Tcl, Event),
    ( Event = next -> go(Tcl)
    ; closedown(Tcl)
    ).

closedown(Tcl) :-
    tcl_delete(Tcl).

go(Tcl) :- 
    tcl_eval(Tcl, 'clear_board', _),
    queens(8, Qs),
    show_solution(Tcl,Qs),
    tk_next_event(Tcl, Event),
    ( Event = next -> fail
    ; closedown(Tcl)
    ).
go(Tcl) :- 
    tcl_eval(Tcl, 'disable_next', _),
    tcl_eval(Tcl, 'clear_board', _),
    tk_next_event(Tcl, _Event),
    closedown(Tcl).

queens(N, Qs) :-
    range(1, N, Ns),
    queens(Ns, [], Qs).

:- queens/3 is nondet.
queens(UnplacedQs, SafeQs, Qs) :-
    select(Q, UnplacedQs, UnplacedQs1),
    \+ attack(Q, SafeQs),
    queens(UnplacedQs1, [Q|SafeQs], Qs).
queens([], Qs, Qs).

attack(X, Xs) :- attack(X, 1, Xs).

attack(X, N, [Y|_Ys]) :- X is Y + N.
attack(X, N, [Y|_Ys]) :- X is Y - N.
attack(X, N, [_Y|Ys]) :-
    N1 is N + 1,
    attack(X, N1, Ys).

:- range(+,+,-) is nondet.
range(M, N, [M|Ns]) :- 
    M < N,
    M1 is M + 1,
    range(M1, N, Ns).
range(N, N, [N]).

show_solution(Tcl, L) :-
    reverse(L, LR),
    tcl_eval(Tcl, [show_solution, br(LR)], _),
    tk_do_all_events.

tk_do_all_events :-
    tk_do_one_event, !,
    tk_do_all_events.
tk_do_all_events.

%% [PM] 3.10.1 Starting program as side effect of loading is evil.
%% :- setup.

