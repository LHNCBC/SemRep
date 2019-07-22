/* Copyright(C) 1999, Swedish Institute of Computer Science */

%   File       : timeout.pl
%   Author     : Mats Carlsson
%   Purpose    : Meta-calls with time-out

%@  This module contains utilities for computing a goal with limit on execution time.
%@
%@  As of release 4.4, this library no longer uses a foreign resource,
%@  and it can be used by more than one SICStus instance in the same process.
%@
%@  Exported predicates:
%@
%@  @table @code

:- module(timeout, [time_out/3]).

:- meta_predicate time_out(0,+,-).

%@  @item time_out(@var{:Goal}, @var{+Time}, @var{-Result})
%@  @PLXindex {time_out/3 (timeout)}
%@  The @var{Goal} is executed as if by @code{call/1}.  If computing any
%@  solution takes more than @var{Time} milliseconds, the goal will be
%@  aborted, as if by @code{throw/1}, and @var{Result} unified with the atom
%@  @code{time_out}.  If the goal succeeds within the specified time,
%@  @var{Result} is unified with the atom @code{success}.
%@  @var{Time} must be a positive integer, less than 2147483647.
%@
%@  Currently, time is measured in @use{runtime} (as opposed to @use{walltime}),
%@  i.e.@: the time does not increment while the program is waiting,
%@  e.g.@: during a blocking read.
%@
%@  Ideally, the measured runtime should be thread-specific, i.e.@: it should
%@  not be affected by computations done in other
%@  threads in the process (of course, thread-specific time is the same
%@  as process runtime for a single-threaded process). 
%@  Thread-specific runtime measurement is only implemented on Windows.
%@
%@  The precision of the timeout interval is usually not better than
%@  several tens of milliseconds. This is due to limitations in the timing
%@  mechanisms used to implement @file{library(timeout)}.

time_out(Call, Time, Flag) :-
        prolog:time_out(Call, Time, Flag).


%@
%@  @end table
%@

