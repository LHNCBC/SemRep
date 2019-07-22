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

% File:	    predications.pl
% Module:   predications
% Author:   psg
% Purpose:  Form predications from matched patterns


% ----- Module declaration and exported predicates

:- module( regexp,   [
% 	transformIteratively/3,  % Apply transformations repeatedly
	predications/3,          % Gather together resulting predications
	rearrangePredications/2,
	stripFunctors/2,         % stripFunctors([foo(bar), foo(car)], [bar, car])
	substitute/4,            % substitute([a,b,c],b,x,[a,x,c])
	joinLists/2              % joinLists([[a,b],[c,d]], [a,b,c,d])
   ]).

% ----- Imported predicates

:- use_module( library(lists),      [
	is_list/1,
	rev/2
   ]).

:- use_module( library(sets), [
	intersection/3
	]).

%:- library_directory( '/home/goetzp/specialist//SKR/src/lib').
% :- file_search_path(skr_lib,'/home/goetzp/specialist/SKR/src/lib').
:- use_module( skr_lib(nls_lists),	    [
	get_from_list/3
   ]). 

/*

% *****************************************************************
%          ******************* REGEXP ******************
% *****************************************************************

REGEXP matches regular expressions in the text, and produces
transformations of it, with a list of those matches that the
pattern asked to be saved for later use.

The way to use a pattern can be like this:

Transformations:
	Asserted:
		transform(postlex, Pattern1In, Pattern1Out).
		transform(postlex, Pattern2In, Pattern2Out).
	Call 
		transformIteratively(postlex, Sentence, NewSentence).

Predications:
	A predication transform works 1 of 2 ways:
	1. It returns a predication
	2. It returns a list of predications.  For instance, a comparitive
			transform must return a set of up to 3 'predicates' for the
			comparative, plus a coordination 'predicate'.
	The 'predications' predicate returns a list of all of the things
			returned by all of the transforms it applied.
	Hence, when using type 1 predications, you simply append the
			results returned in predications(_,_,RESULTS) to your predications.
			You may call consolidatePredications if you think it_s safer.
	When using type 2 predications, you get back a list of lists,
			and you call
				joinLists(RESULTS, Delisted)
			to convert, e.g.,
				[[compare(A,B), coord(A,B)], [compare(X,Y), coord(X,Y)]]
			into
				[compare(A,B), coord(A,B), compare(X,Y), coord(X,Y)]
			and then
				rearrangePredications(Delisted, ResultsByPred)
			to convert that into
				[compare([compare(A,B), compare(X,Y)]),
				 coord([coord(A,B), coord(X,Y)])]

	Example:
		Asserted:
			transform(comparison, Regexp1, PredicatesOut1).
			transform(comparison, Regexp2, PredicatesOut2).
		Call
			predications(comparison, SentenceIn, PredsOut),
			joinLists(PredsOut, Delisted),
			rearrangePredications(Delisted, [compare(COMP), coord(COORD)]),
			append(Preds, COMP, AllPreds),
			append(Coords, COORD, AllCoords).
*/


% Apply transformations repeatedly, until you can't apply any more
% WARNING: This will loop infinitely if there is a cycle in
%		transformations,
%		including if a single transformation matches its own output
% It has a good chance of looping infinitely
% transformIteratively(Module, In, Out) :-
% 	transform(Module, Pattern, T1),
%  	listMatch(In, Pattern, Saved),
% 	% Substitute saved variables into PredOut
% 	substSaved(T1, Saved, Transformed),
% 	% Avoid an infininte loop if transform makes no changes
% 	\+(Transformed = In),
% 	!,   % transforms aren't optional
% 	transformIteratively(Module, Transformed, Out).
% transformIteratively(_, In, In).


/*
% This doesn't recurse; it checks for a single pattern match.
	That's why we don't use it.
% +Module: An atom identifying the particular type of predication we're
%		looking for, such as comparative, conjunction, etc.
%		This organizes the code, and makes it easier to know what
%		order things happen in.
onePredication(Module, SentenceIn, PredOut) :-
	% transform/3 specifies a text and a resulting predication
	transform(Module, Pattern, Pred),
	% listMatch does quick & dirty bindingless test for a potential match
 	listMatch(SentenceIn, Pattern, Saved),
	% Substitute saved variables into PredOut
	substSaved(Pred, Saved, PredOut).
*/

% This returns a list of all predicates created by all patterns in Module
predications(Module, SentenceIn, PredsOut) :-
	findall(PredOut, (transform(Module, Pattern, Pred),
	                  % Each pattern may match only once
	                  oneListMatch(SentenceIn, Pattern, Saved),
										% Substitute saved matches from SentenceIn into Pred
	                  substSaved(Pred, Saved, PredOut)),
	        PredsOut).

% Don't try to find more than one way of matching each pattern
%		or the findall(...) will hang, or at least take a LONG time
oneListMatch(In, Pattern, Saved) :- listMatch(In, Pattern, Saved), !.

% listMatch(+Sentence, +Pattern, -Matches)
% -Matches is a list of terms s(name, value)
% NOTE: termMatch is more general than listMatch.
%		listMatch will not match two non-lists.
%		termMatch will match two lists.
listMatch([], [], []).
listMatch(Sentence, [Pattern | PTail], Saved) :-
	lm(Pattern, Sentence, Unmatched, S1),
	listMatch(Unmatched, PTail, S2),
	append(S1,S2,Saved).

% lm(+Pattern, +Sentence, -Unmatched, -Saved)
% Sentence = [Prefix | Unmatched], Prefix matches Pattern
% Sentence will contain no variables or specials.
% Pattern may contain both.
% Saved is the variables saved in the match.

% Reversing the order of the next 2 definitions would make star(dc) expensive
% Match star(X) to 0 items; must stop with star(X)
% It is IMPORTANT that star be the opposite of greedy:
%		It must try shortest match first.
%	Otherwise, patterns will try non-local arguments before local ones
lm(STAR_X, Tail, Tail, []) :-
	nonvar(STAR_X), STAR_X = star(_).
% Match star(X) to 1 item; may continue matching it to the right
lm(STAR_X, In, Unmatched, S) :-
	nonvar(STAR_X),      % a functor may put a variable in right-hand list
	STAR_X = star(STAR),
	lm(STAR, In, U1, S1),               % match some initial part of In
	lm(star(STAR), U1, Unmatched, S2),  % match the rest of In
	append(S1,S2,S).

/*
You can debug & uncomment these when you start using opt & plus
Leave until rest of regexp is debugged

% It is IMPORTANT that plus be the opposite of greedy:
%		It must try shortest match first.
%	Otherwise, patterns will try non-local arguments before local ones
listMatch([X | Tail], [PLUS_X | PatternTail], S) :-
	nonvar(PLUS_X),
	(	PLUS_X == plus(dc), !, S1 = []   % ! not needed since no vars in LHS
	;	PLUS_X = plus(PLUS), termMatch(X, PLUS, S1)
	),
	(	listMatch(Tail, PatternTail, S2)            % matched 1; stop
	;	listMatch(Tail, [PLUS_X | PatternTail], S2)  % match more
	),
	append(S1, S2, S).
*/

% Match optional(X) to 0 items; may not continue matching it to the right
lm(OPT_X, In, Unmatched, S) :-
	nonvar(OPT_X), OPT_X = optional(OPT),
	(	Unmatched = In, S = []		  % Match optional(X) to 0 items
	;	lm(OPT, In, Unmatched, S)   % Match optional(X) to 1 item
	).

lm(DC, [_|Tail], Tail, []) :- nonvar(DC), DC == dc.

% Save a matched term for later substitutions
% We list the variable name before the pattern for readability
% NOTE: It saves a LIST of the things matched, even if there's only one
% Hence, if Pattern = [x], you get s(Var, [[x]])
lm(SAVE, In, U, [s(Var, Term) | S]) :-
	nonvar(SAVE), SAVE = save(Var, Pattern),
	lm(Pattern, In, U, S),
	append(Term, U, In).     % Term is whatever was matched

% Match any one of the things inside or(X,Y,...)
lm(OR, In, U, S) :-
	nonvar(OR), OR =.. [or | Args],
	member(A, Args),
	lm(A, In, U, S).

/*
% Match all of the things inside and(X,Y,...)
lm(AND, In, U, S) :-
	nonvar(AND), AND =.. [and | Args], matchAll(A,Args,S).
*/

% If the pattern is a list, the entire list is to be matched against
%		the head of the Input.
% This requires going back to listMatch.
% If the head of the Input isn't a list, it's impossible to match.
lm(PatList, [IH | IT], IT, Saved) :-
	nonvar(PatList), PatList = [PH | PT],
	lists:is_list(IH),
	lm(PH, IH, Unmatched, S1),   % Match first item in pattern
	listMatch(Unmatched, PT, S2),   % Match rest of pattern
	append(S1, S2, Saved).

% Match functors, eg. foo(1,2,3) to foo(star(dc))
% Equivalent rule with nonvar on left not needed;
%		variables will occur only in the right (Pattern) side.
lm(FPat, [FIn | Unmatched], Unmatched, Saved) :-
	nonvar(FIn), \+(lists:is_list(FIn)),   % [] =.. [[]]
	FIn =.. [Functor | IArgs],
	\+(specialFunctor(Functor)),      % Functor isn't a regexp function
	(	nonvar(FPat),
		FPat =.. [Functor | PArgs],
		listMatch(IArgs, PArgs, Saved)  % match entire lists together
	; var(FPat), FPat = FIn, Saved = []
	).

% Match using Prolog unification
% This is the only rule that allows the pattern to be a variable
lm(Pat, [X | T], T, []) :- var(Pat), !, Pat = X.

% Pattern may not be a list, because they should use a prior rule
lm(X, [X|T], T, []) :- \+compound(X).


/*
% Match Sentence to one of the Patterns
matchOne(Sentence, [P | Patterns], Saved) :-
	termMatch(Sentence, P, Saved) ;
	matchOne(Sentence, Patterns, Saved).

% Match Sentence to all of the Patterns
% For use by special 'and'
matchAll(Sentence, [P | Patterns], Saved) :-
	termMatch(Sentence, P, S1),
	matchOne(Sentence, Patterns, S2),
	append(S1, S2, Saved).
*/


% Substitute saved variables into PredOut
% Because substSaved is used inside a findAll,
% it is very important for it to return only 1 answer.
% A saved(term) inside a list is left empty if 'term' wasn't saved

% substSaved(+In, +Saved, -Out)

% This is not needed, unless you call substSaved(saved(term), Saved, Val)
substSaved(saved(Var), Saved, Val) :-
	% fortunately we know by now that +Pred is nonvar
	!,
	member(s(Var,Val), Saved).

% We make a sub-predicate to avoid testing the previous rule again
substSaved(A, Saved, B) :- substSaved0(A, Saved, B).

substSaved0(X, _, X) :- var(X), !.

substSaved0([], _, []).

% A saved(term) inside a list is left empty if 'term' wasn't saved
%		or if s(term, []), meaning it was matched to nothing
substSaved0([saved(Var) | Tail], Saved, Out) :-
	!,
	(	member(s(Var,Val), Saved),
		!,    % don't allow an Out with this spot left blank
		substSaved0(Tail, Saved, TailOut),
		append(Val, TailOut, Out)
	;	% no value was saved for Var
		substSaved0(Tail, Saved, Out)
	).

substSaved0(Term, Saved, Out) :-
	compound(Term),            % requires nonvar(Term); allows is_list(Term)
	\+(lists:is_list(Term)),   % [] =.. [[]|[]]
	Term =.. [Functor | ArgsIn],
	substSaved0(ArgsIn, Saved, ArgsOut),
	Out =.. [Functor | ArgsOut].

substSaved0([H|T], Saved, [HO | TO]) :-
	substSaved0(H, Saved, HO),
	substSaved0(T, Saved, TO).

% No substitution in Term:
substSaved0(Term, _, Term) :- \+(compound(Term)), \+(lists:is_list(Term)).

% Declare special functors
% 'dc' is not a functor
specialFunctor(and).      % and([star(1,2)],not([star(1,2),2,2,star(1,2)]))
                          %    matches [star(1,2)] with no 2 2s in a row
specialFunctor(not).
specialFunctor(optional). % Note that optional may be used ONLY within a list
specialFunctor(or).       % Don't expect to ever have ';' in left/input
specialFunctor(plus).     % Note that plus may be used ONLY within a list
specialFunctor(save).     % save(foo,foo(X)) saves foo(X) under name foo
specialFunctor(star).     % Note that star may be used ONLY within a list


% joinLists( [[a, b, c], [d, e]] , [a,b,c,d,e] )
joinLists([], []).     % Happens only if called with an empty list
joinLists([[]], []).
joinLists([[]|T], Out) :-
	joinLists(T, Out).
joinLists([[H|T]|More], [H|OutTail]) :-
	joinLists([T | More], OutTail).


% See notes far above on this predicate.
% Assumes input is already delisted.
rearrangePredications([], []).
rearrangePredications([Result|MoreResults], ResultsByPred) :-
	nonvar(Result), Result =.. [Pred | _], nonvar(Pred),
	rearrangePredications(MoreResults, MoreRearranged),
	% NOTE: Result is an actual predicate foo(stuff, morestuff).
	% Collection is the predicate around a list of such predicates.
	( % Add one predicate into a collection already built of such predicates
		findPredicate(Result, MoreRearranged, Collection),
		!,
		Collection =.. [Pred | List],
		NewCollection =.. [Pred | [[Result | List]]],
		substitute(MoreRearranged, Collection, NewCollection, ResultsByPred)
	; % Create a new Collection if none exists
		Collection =.. [Pred | [[Result]]],
		ResultsByPred = [Collection | MoreRearranged]
	).

% findPredicate(Predication, [SamePred|Tail], Result)
% +Predication: foo(bar)
% +SamePred: foo(car)
% -Result: foo(car)
findPredicate(Predication, [SamePred | _], SamePred) :-
	nonvar(Predication), Predication =.. [Pred | _],
	nonvar(SamePred), SamePred =.. [Pred | _], !.
findPredicate(Predication, [_ | Tail], Result) :-
	findPredicate(Predication, Tail, Result).


% substitute(+In, +Original, +Replacement, -Out)
substitute([], _, _, []).
substitute([Old|IT], Old, New, [New|OT]) :-
	!,
	substitute(IT, Old, New, OT).
substitute([X|IT], Old, New, [X|OT]) :-
	substitute(IT, Old, New, OT).

% [foo(bar), foo(car)] -> [bar, car]
stripFunctors([], []).
stripFunctors([F | TailIn], [A | TailOut]) :-
	compound(F),
	F =.. [_,  A],
	stripFunctors(TailIn, TailOut).


/*
          PATTERNS

Most of the patterns in here are literal.
Special patterns are those enclosed in special regexp predicates.

Special patterns in the match pattern:
	star(X):  a list of 0 or more repetitions of things matching X
	optional(X): 0 or 1 things matching X
	plus(X): 1 or more things matching X
	not(X): nothing matching X
	or(X,Y,Z): something matching X or Y or Z
	dc: Equivalent to '_' in interactive Prolog
	save(varname, X): Match X, and save it under label 'varname'

	Note that star, optional, and plus may be used only inside lists.
	not(X), or dc, and save may be used anywhere.

Special patterns in the predicate pattern:
	saved(varname): Insert the pattern saved by save(X,varname)
*/

% This merely defines transform, so that the program won't crash
%		if there are no transforms defined.
transform(null, [], []).

% POSTLEX
% "<tmod>1 vs. <tmod>2" -> "<tmod>1 versus <tmod>2"
transform(
	postlex,
	[	save(star1, star(dc)),
		save(head1,
			[	star(dc),
				head([
              usemtype(U),
              ausemtype(AU),
              semgroup([star(dc), tmod, star(dc)]),
							star(dc)
				]),
				optional(confid(dc))
			]
		),
		save(star2, star(dc)),
		[	conj([
              lexmatch([vs]),
              inputmatch([vs]),
              tag(conj),
              bases(_),
              tokens([vs])
      ]),
			punc([
              inputmatch([.]),
              bases([.]),
              tokens([])
      ]),
			save(mods, star(dc)),
			save(head2,
				head([
              usemtype(U),
              ausemtype(AU),
              semgroup([star(dc), tmod, star(dc)]),
							star(dc)
				])
			),
			optional(
				save(punc, punc(dc))
			),
			confid(CONF)
		],
		save(star3, star(dc))
	],
	[	saved(star1),
		saved(head1),
		saved(star2),
		[	conj([
              lexmatch([versus]),
              inputmatch([versus]),
              tag(conj),
              bases([versus]),
              tokens([versus])
            ])
		],
 		[	saved(mods),
			saved(head2),
			saved(punc),
			confid(CONF)
		],
		saved(star3)
	]).

/*
This pattern was written for the comparative pattern
"comparative study * of X * and/versus Y"
However, there's already code inside identify_comparatives to do that.
Someday, we will rip that code out, and use this pattern (tested).

transform(comparison,
	[ star(dc),
		[ star(dc),
			head([
              index(INDR),
              star(dc),
              semgroup([conc]),
              lexmatch([comparative,study]),
              inputmatch(dc),
              tag(noun),
              bases([comparative,study]),
              tokens(dc),
              metaconc(dc)
            ]),
        confid(dc)
		],
		star(dc),
 		[ prep([
              index(dc),
              lexmatch([of]),
              inputmatch([of]),
              tag(prep),
              bases([of]),
              tokens([of])
            ]),
			star(dc),
			head(
				save(arg1,
				     [
              index(IND1),
              usemtype(dc),
              ausemtype([AUS1|AUS1R]),
              semgroup([tmod]),
							star(dc),
							metaconc([MC1:[AUS1|AUS1R]])
             ])
			),
			confid(dc)
		],
		star(dc),
		[ conj([
              index(dc),
              lexmatch(dc),
              inputmatch(dc),
              tag(conj),
              bases([save(conj,or(and,versus))]),
              tokens(dc)
            ])
		],
		[	star(dc),
			head(
				save(arg2, [
              index(IND2),
              usemtype(dc),
              ausemtype([AUS2|AUS2R]),
              semgroup([tmod]),
							star(dc),
							metaconc([MC2:[AUS2|AUS2R]]),
							star(dc)
            ])
			),
			optional(punc(dc)),
			confid(dc)
		],
		star(dc)
	],
	%compare(BASE1, BASE2)
	[	compare(_-_-IND1-MC1-[AUS1|AUS1R]-AUS1-compared_with-INDR-_-_-IND2-MC2-[AUS2|AUS2R]-AUS2),
		coord(and,arg,INDR,[saved(arg1)],[saved(arg2)])
	]
).
*/
