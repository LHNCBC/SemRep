% -*- Mode:Prolog -*-
%   Package: types.pl
%   Author : Richard A. O'Keefe
%   Updated: 11 Oct 1991
%   Purpose: More and better type tests.

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1989, Quintus Computer Systems, Inc.  All rights reserved.

/*  This file adds the type tests through the predicates:

	must_be(+Term, +Type, +Goal, +ArgNo)

    Checks whether the Term in ArgNo of Goal belongs to the
    indicated Type.  If it isn't, there are two cases:
    The Term may not be instantiated enough to tell yet, in which
        case an instantiation_error will be raised
    The Term may be definitely not of the type, in which case a
	type_error is raised.

*/

:- module(types, [
	must_be/4,
	illarg/3,
	illarg/4
   ]).

%@  This library module provides more and better type tests.
%@  For the purposes of this library, we first define an abstract type
%@  @var{typeterm}, as follows:
%@  
%@  @multitable @columnfractions .25 .75
%@  @item @var{typeterm} @tab ::= @code{atom} 
%@  @item @tab | @code{atomic} 
%@  @item @tab | @code{boolean}
%@  @item @tab | @code{callable}
%@  @item @tab | @code{character}
%@  @item @tab | @code{character_code}
%@  @item @tab | @code{compound}
%@  @item @tab | @code{db_reference}
%@  @item @tab | @code{float}
%@  @item @tab | @code{float(@var{rangeterm})}
%@  @item @tab | @code{ground}
%@  @item @tab | @code{integer}
%@  @item @tab | @code{integer(@var{rangeterm})}
%@  @item @tab | @code{list}
%@  @item @tab | @code{list(@var{Type})}
%@  @item @tab | @code{mutable}
%@  @item @tab | @code{nonvar}
%@  @item @tab | @code{number}
%@  @item @tab | @code{number(@var{rangeterm})}
%@  @item @tab | @code{oneof(@var{L})}
%@  @item @tab | @code{order}
%@  @item @tab | @code{pair}
%@  @item @tab | @code{pred_spec}
%@  @item @tab | @code{pred_spec_tree}
%@  @item @tab | @code{proper_list}
%@  @item @tab | @code{proper_list(@var{Type})}
%@  @item @tab | @code{simple}
%@  @item @tab | @code{term}
%@  @item @tab | @code{var}
%@  @item @tab | @code{var_or(@var{Type})}
%@  @item
%@  @item @var{rangeterm} @tab ::= @code{between(L,U)}
%@  @item @tab | @code{>=(L)}
%@  @item @tab | @code{>(L)}
%@  @item @tab | @code{<(L)}
%@  @item @tab | @code{=<(L)}
%@  @item @tab | @code{=:=(L)}
%@  @item @tab | @code{=\=(L)}
%@  @end multitable
%@  
%@  Culprit information:
%@
%@  These predicates takes arguments that are used when reporting the reason and location of errors.
%@  The arguments are:
%@  @table @var
%@  @item Goal
%@  @c [PM] 4.3 document that Goal should lack module wrapping, even though we nowadays
%@  @c unwrap it. It did not always use to work and it is bad style, especially for illarg/3. 
%@   must be a callable term, without @code{(:)/2} module wrapping, with arity at least @var{ArgNo}.
%@  @item ArgNo
%@   must be a non-negative integer, where zero means no specific argument postition.
%@  @item Culprit
%@   the term that has the offending value.
%@  @end table
%@  
%@  Exported predicates:
%@  
%@  @table @code
%@  @item must_be(@var{+Term}, @var{+Type}, @var{+Goal}, @var{+ArgNo})
%@  @PLXindex {must_be/4 (types)}
%@  checks whether the @var{Term} belongs to the indicated @var{Type},
%@  which should be a @var{typeterm}.  If it doesn't, several different
%@  error exceptions can be thrown: the @var{Term} may not be instantiated
%@  enough to tell yet (Instantiation Error); it may be instantiated when
%@  an unbound variable was expected (Uninstantiation Error); it may be
%@  definitely not of the right type (Type Error); it may be of the right
%@  type but not representable (Representation Error); or it may be of the
%@  right type but in the wrong domain (Domain Error).  If an error
%@  exception is thrown, it will include @var{Goal} and @var{ArgNo} and,
%@  if possible, the line of code in the scope of which the error
%@  occurred.  @xref{ref-ere-err}.
%@
%@  @item illarg(@var{+ErrorTerm}, @var{+Goal}, @var{+ArgNo})
%@  @itemx illarg(@var{+ErrorTerm}, @var{+Goal}, @var{+ArgNo}, @var{+Culprit})
%@  @PLXindex {illarg/[3,4] (types)}
%@  is the way to raise an error exception, if you
%@  would like the exception to pinpoint the line of code in the scope of
%@  which the error occurs.  This is especially useful in the context of
%@  source-linked debugging. @var{Culprit} defaults to argument number
%@  @var{ArgNo} of @var{Goal}. These three arguments are passed to the
%@  exception being raised, if appropriate. @var{ErrorTerm} should be one
%@  of the following.  @xref{ref-ere-err}.
%@  
%@  @table @code
%@  @item var
%@  An Instantiation error is raised.
%@  
%@  @item type(@var{ErrorType})
%@  Same as @code{must_be(@var{Culprit}, @var{ErrorType}, @var{Goal}, @var{ArgNo})}.
%@  
%@  @item domain(@var{ErrorType},@var{ErrorDomain})
%@  First, the type is checked by @code{must_be(@var{Culprit}, @var{ErrorType}, @var{Goal}, @var{ArgNo})}.
%@  If the type is valid, a
%@  Domain Error is raised with the expected domain being
%@  @var{ErrorDomain}.
%@  
%@  @item force_type(@var{ExpType})
%@  A Type Error is raised.
%@  
%@  @item context(@var{ContextType},@var{CommandType})
%@  A Context Error is raised.
%@  
%@  @item existence(@var{ObjType},@var{Culprit},@var{Message})
%@  An Existence Error is raised.
%@  
%@  @item permission(@var{Operation},@var{ObjType},@var{Message})
%@  A Permission Error is raised.
%@  
%@  @item representation(@var{ErrorType})
%@  A Representation Error is raised.
%@  
%@  @item evaluation(@var{ErrorType})
%@  An Evaluation Error is raised.
%@  
%@  @item consistency(@var{Culprit1},@var{Culprit2},@var{Message})
%@  A Consistency Error is raised.
%@  
%@  @item syntax(@var{Pos},@var{Msg},@var{Tokens},@var{AfterError})
%@  A Syntax Error is raised.
%@  
%@  @item resource(@var{Resource})
%@  A Resource Error is raised.
%@  
%@  @item system(@var{Message})
%@  A System Error is raised.
%@  
%@  @end table
%@  @end table

must_be(Term, Type, Goal, ArgNo) :-
        % Verify arguments (else fail) and unwrap module (module wrapping is not allowed, but we are forgiving)
        normalize_culprit(Goal, ArgNo, Goal1, ArgNo1),
        !,
	prolog:must_be(Term, Type, Goal1, ArgNo1).
must_be(Term, Type, Goal, ArgNo) :-
        % Some argument is wrong. Use must_be/4 to report it
        ThisGoal = must_be(Term, Type, Goal, ArgNo),
        prolog:must_be(Goal, callable, ThisGoal, 3 /* Goal */),
        functor(Goal,_,Arity),
        prolog:must_be(ArgNo, integer(between(0,Arity)), ThisGoal, 4 /* ArgNo */),
        % not reached
        fail.

:- illarg/3 is documented_as(must_be/4).

illarg(ErrorTerm, Goal, ArgNo) :-
        % Verify arguments (else fail) and unwrap module (module wrapping is not allowed, but we are forgiving)
        normalize_culprit(Goal, ArgNo, Goal1, ArgNo1),
        % [PM] 4.3 Only some ErrorTerm values need a culprit (e.g. not var/0, context/2, ...) so we can not always require ArgNo >= 1.
        %          For now, always allow ArgNo == 0, since prolog:illarg/3 handles that (setting Culprit to entire Goal).
        %          objtest namedtes2 triggers this issue (SPRM 13637 compunded by incorrect LowLimit in next clause)
        % % must explicitly specify an argument position since this is where the Culprit is located
        % LowLimit = 1,
        LowLimit = 0,
        ArgNo1 >= LowLimit,
        !,
	prolog:illarg(ErrorTerm, Goal1, ArgNo1).
illarg(ErrorTerm, Goal, ArgNo) :-
        % Some argument is wrong. Use must_be/4 to report it
        ThisGoal = illarg(ErrorTerm, Goal, ArgNo),
        prolog:must_be(Goal, callable, ThisGoal, 2 /* Goal */),
        functor(Goal,_,Arity),
        % LowLimit = 1,
        LowLimit = 0,
        prolog:must_be(ArgNo, integer(between(LowLimit,Arity)), ThisGoal, 3 /* ArgNo */),
        % not reached
        fail.

:- illarg/4 is documented_as(must_be/4).

illarg(ErrorTerm, Goal, ArgNo, Culprit) :-
        % Verify arguments (else fail) and unwrap module (module wrapping is not allowed, but we are forgiving)
        normalize_culprit(Goal, ArgNo, Goal1, ArgNo1),
        !,
	prolog:illarg(ErrorTerm, Goal1, ArgNo1, Culprit).
illarg(ErrorTerm, Goal, ArgNo, Culprit) :-
        % Some argument is wrong. Use must_be/4 to report it
        ThisGoal = illarg(ErrorTerm, Goal, ArgNo, Culprit),
        prolog:must_be(Goal, callable, ThisGoal, 2 /* Goal */),
        functor(Goal,_,Arity),
        prolog:must_be(ArgNo, integer(between(0,Arity)), ThisGoal, 3 /* ArgNo */),
        % not reached
        fail.

% [PM] 4.3 Some of the underlying builtins are now stricter with what they accept, so we must check Culprit+ArgNo up front.
% [PM] 4.3 Unwrap any module wrapping M:G, and if ArgNo is out of bounds, use zero. Fail if malformed.
normalize_culprit(Goal, _ArgNo, _Goal1, _ArgNo1) :- var(Goal), !,
        fail.
normalize_culprit(_M:Goal, ArgNo, Goal1, ArgNo1) :- !,
        normalize_culprit(Goal, ArgNo, Goal1, ArgNo1).
normalize_culprit(Goal, ArgNo, Goal1, ArgNo1) :-
        callable(Goal),
        integer(ArgNo),
        functor(Goal,_,Arity),
        0 =< ArgNo, ArgNo =< Arity,
        Goal1 = Goal,
        ArgNo1 = ArgNo.
