
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
*  For full details, please see the MetaMap Terms & Conditions, available at
*  https://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

% File:	    mincoman.pl
% Module:   mincoman
% Author:   tcr
% Purpose:  Underspecified syntactic analysis


% ----- Module declaration and exported predicates

:- module( mincoman,   [
	minimal_commitment_analysis/4,
	punc_mark1/1
   ]).

% ----- Imported predicates

:- use_module( skr_lib(nls_lists), [
	get_from_list/3
   ]).

:- use_module( skr_lib(nls_system), [
	control_option/1
   ]).

:- use_module( library(lists), [
	is_list/1,
	rev/2
   ]).

/*

% *****************************************************************
% ******************* MINIMAL_COMMITMENT_ANALYSIS *****************
% *****************************************************************

MINIMAL_COMMITMENT_ANALYSIS assigns underspecified syntactic analysis
to lexically analyzed input.  The current emphasis is on noun phrases;
however, the entire input string is bracketed. The analysis can be
thought of as the result of skimming the input to extract only NP's
and PrepP's. Subsequent analysis may require that the structures here
be included in higher level bracketings. However, it is claimed that
the bracketing assigned by MINIMAL_COMMITMENT_ANALYSIS will not have
to be erased.

The program proceeds in five phases, which can be thought of as a
series of filters (or transducers) that impose an ever finer degree
of analysis on an input string. The five phases are: MARK_BOUNDARIES,
ADJUST_BOUNDARIES, SEGMENT, IDENTIFY_HEADS, and IDENTIFY_LEFT_MODS.

When the Tagger is used, consult_tagged_text is used before
minimal_commitment_analysis to turn VarInfoList, which has elements in
the form "Label:[ InfoList ]", into LabeledText, which is a list with
elements in the form
"Tag([lexmatch(LexMatch),inputmatch(InputMatch),tag(Tag)])".

When the Tagger is not used, the predicate convert transforms
VarInfoList into a format comparable to that produced by
consult_tagged_text. It also "resolves" category label information
(see below).

*/

% First arg is TagList
minimal_commitment_analysis(TagList, VarInfoList, CatLabText, minimal_syntax(SyntAnalysis)) :-
	( TagList = [__|_] ->
	  LabeledText = CatLabText
	; convert(VarInfoList, LabeledText, [])
	),
	!,
	mark_boundaries(LabeledText, BoundedList, []),
	adjust_boundaries(BoundedList, NewBoundedList),
	segment(NewBoundedList, SegmentedList, []),
	identify_heads(SegmentedList, HeadedList, _Accum),
	identify_left_mods(HeadedList, SyntAnalysis, []).


/*

% *********************** CONVERT *******************************

This predicate is used when the tagger was not used. It converts
lexical entry format into a format comparable to that produced by
consult_tagged_text.  It also "resolves" category label ambiguity,
but it does not do so on the basis of context and is therefore not
as effective as the tagger.  Words having the label verb are 
particularly subject to error. The rules for these items are:

1. All potential past participles are tagged as adj
2. An exclusive past tense form is tagged as verb
3. Other verb forms are tagged as noun if they are N/V 
   ambiguous

Note that a consequence of (3) is that a non-past participial verb
form is tagged as a verb only if it is unambiguous.

not_in_lex is coverted to noun

*/

convert([], Gap, Gap) :- !.
convert([shapes:[inputmatch:ShapesList,features:FeatList|_] | MoreWords],
        [shapes([inputmatch(ShapesList),features(FeatList)]) | NewGap], Gap) :-
	!,
	convert(MoreWords,NewGap, Gap ).

convert([punctuation:PuncList|MoreWords],[NewItem|NewGap], Gap) :-
	!,
	get_from_list(inputmatch,PuncList,[Punk] ),
	NewItem = punc([inputmatch([Punk])]),
	convert(MoreWords,NewGap, Gap).

convert([(not):VarInfoList|MoreWords], 
        [adv([lexmatch(['not']),
         inputmatch(InputMatch),tag(adv)])|NewGap], Gap) :-
	!,
	get_variant( inputmatch, VarInfoList, InputMatch ),
	convert( MoreWords, NewGap, Gap ).

convert([ThisWord:VarInfoList|MoreWords], 
        [prep([lexmatch([ThisWord]), inputmatch(InputMatch),tag(prep)]) | NewGap], Gap ) :-
	get_variant( prep, VarInfoList, _Info ),
	!,
	get_variant( inputmatch, VarInfoList, InputMatch ),
	convert( MoreWords, NewGap, Gap ).

convert([ThisWord:VarInfoList|MoreWords], 
        [aux([lexmatch([ThisWord]), inputmatch(InputMatch),tag(aux)]) | NewGap], Gap) :-
	get_variant( aux, VarInfoList, _Info ),
	!,
	get_variant( inputmatch, VarInfoList, InputMatch ),
	convert( MoreWords, NewGap, Gap ).

convert([ThisWord:VarInfoList|MoreWords], 
        [modal([lexmatch([ThisWord]), inputmatch(InputMatch),tag(modal)]) | NewGap], Gap) :-
	get_variant( modal, VarInfoList, _Info ),
	!,
	get_variant( inputmatch, VarInfoList, InputMatch ),
	convert( MoreWords, NewGap, Gap ).

convert([ThisWord:VarInfoList|MoreWords], 
        [conj([lexmatch([ThisWord]), inputmatch(InputMatch),tag(conj)]) | NewGap], Gap) :-
	get_variant( conj, VarInfoList, _Info ),
	!,
	get_variant( inputmatch, VarInfoList, InputMatch ),
	convert( MoreWords, NewGap, Gap ).

convert([ThisWord:VarInfoList|MoreWords], 
        [compl([lexmatch([ThisWord]), inputmatch(InputMatch),tag(compl)]) | NewGap], Gap) :-
	get_variant( compl, VarInfoList, _Info ),
	!,
	get_variant( inputmatch, VarInfoList, InputMatch ),
	convert( MoreWords, NewGap, Gap ).

convert([ThisWord:VarInfoList|MoreWords], 
        [det([lexmatch([ThisWord]), inputmatch(InputMatch),tag(det)]) | NewGap], Gap) :-
	get_variant( det, VarInfoList, _Info ),
	!,
	get_variant( inputmatch, VarInfoList, InputMatch ),
	convert( MoreWords, NewGap, Gap ).

convert([ThisWord:VarInfoList|MoreWords], 
        [noun([lexmatch([ThisWord]), inputmatch(InputMatch),tag(noun)]) | NewGap], Gap) :-
	get_variant( noun, VarInfoList, _NounInfo ),
	get_variant( inputmatch, VarInfoList, InputMatch ),
	!,
	convert( MoreWords, NewGap, Gap ).

convert([ThisWord:VarInfoList|MoreWords], 
        [adj([lexmatch([ThisWord]), inputmatch(InputMatch),tag(adj)]) | NewGap], Gap ) :-
	get_variant( adj, VarInfoList, _NounInfo ),
	get_variant( inputmatch, VarInfoList, InputMatch ),
	!,
	convert( MoreWords, NewGap, Gap ).

convert([ThisWord:VarInfoList|MoreWords], 
        [adv([lexmatch([ThisWord]), inputmatch(InputMatch),tag(adv)]) | NewGap], Gap) :-
	get_variant( adv, VarInfoList, _AdvInfo ),
	!,
	get_variant( inputmatch, VarInfoList, InputMatch ),
	convert( MoreWords, NewGap, Gap ).

% Note: All potential pp's are tagged as adj

convert([ThisWord:VarInfoList|MoreWords], 
        [adj([lexmatch([ThisWord]), inputmatch(InputMatch),tag(adj)]) | NewGap], Gap) :-
	get_lex_feature( verb, pastpart, VarInfoList ),
	!,
	get_variant( inputmatch, VarInfoList, InputMatch ),
	convert( MoreWords, NewGap, Gap ).

convert([ThisWord:VarInfoList|MoreWords], 
        [verb([lexmatch([ThisWord]), inputmatch(InputMatch),tag(verb)]) | NewGap], Gap) :-
	get_variant( verb, VarInfoList, _Info ),
	!,
	get_variant( inputmatch, VarInfoList, InputMatch ),
	convert( MoreWords, NewGap, Gap ).

convert([ThisWord:VarInfoList|MoreWords], 
        [pron([lexmatch([ThisWord]), inputmatch(InputMatch),tag(pron)]) | NewGap], Gap) :-
	get_variant( pron, VarInfoList, _Info ),
	!,
	get_variant( inputmatch, VarInfoList, InputMatch ),
	convert( MoreWords, NewGap, Gap ).

convert([Unknown|MoreWords], [NewElement|NewGap], Gap) :-
	( punc_char1(Unknown) ->
	  NewElement = punc([inputmatch([Unknown])])
	; NewElement = noun([inputmatch([Unknown])])
	),
	!,
	convert(MoreWords, NewGap, Gap).

punc_char1('\\').
punc_char1('|').
punc_char1('"'). % " this comment is just to fake out Emacs's font code
punc_char1('`').
punc_char1('@').
punc_char1('#').
punc_char1('$').
punc_char1('%').
punc_char1('^').
punc_char1('&').
punc_char1('*').
punc_char1('+').
punc_char1('=').
punc_char1('[').
punc_char1(']').
punc_char1('<').
punc_char1('>').

/*

% ********************* MARK BOUNDARIES **************************

MARK_BOUNDARIES takes tagged input and marks some items as being
either potential heads or boundaries.  These boundaries mark minimal
syntactic units (MSU). These may form a part of some larger unit, but
it is claimed that these units are significant in that larger units do
not cross them.  It is also claimed that in general these units do not
need to be broken down further for later processing, although this is
wrong in the case of one construction, namely two simple NP's in a row
where the second is not introduced by a noun phrase introducer. For
example "games people" occurring in "games people play" ` needs to be
analyzed further.

The label boundary is assigned to: ":", "(", ")", prepositions,
auxiliaries, conjunctions, and verbs. The general principle is that an
item labelled as a boundary begins a new MSU. After an initial pass at
assigning boundaries during this predicate, boundary labels are
adjusted during the next predicate, ADJUST_BOUNDARIES.  Bracketing is
then assigned on the basis of boundary labels in SEGMENT.

Determiners, adverbs, and shapes are labelled as such.

Nouns and adjectives are labelled as potential heads.

Punctuation marks are labelled as punc.

Items not found in the lexicon are labeled as noun.

*/

mark_boundaries( [], Gap, Gap ) :- !.

% force a phrase break after the first of two consecutive shapes
mark_boundaries( [shapes(ShapesArg1), shapes(ShapesArg2)|MoreWords], 
                 [shapes(ShapesArg1), boundary(shapes(ShapesArg2))|NewGap], Gap ) :-
	control_option(num_break),
	!,
	mark_boundaries( MoreWords, NewGap, Gap ).

mark_boundaries( [shapes(ShapesArg)|MoreWords], 
                 [shapes(ShapesArg)|NewGap], Gap ) :-
	!,
	mark_boundaries( MoreWords, NewGap, Gap ).

mark_boundaries( [punc(PunkArg1), punc(PunkArg2)|MoreWords], 
                 [boundary(punc(PunkArg1)), boundary(punc(PunkArg2)) 
                |NewGap], Gap ) :-
	get_from_list( inputmatch, PunkArg1, ['-'] ),
	get_from_list( inputmatch, PunkArg2, ['-'] ),
	!,
	mark_boundaries( MoreWords, NewGap, Gap ).

mark_boundaries( [punc(PunkArg)|MoreWords], 
                 [NewItem|NewGap], Gap ) :-
	!,
	get_from_list( inputmatch, PunkArg, [PunkMark] ),
	( punc_mark1(PunkMark) ->
	  NewItem = boundary(punc(PunkArg))
	; NewItem = punc(PunkArg)
	),
	mark_boundaries( MoreWords, NewGap, Gap ).

mark_boundaries( [adv(AdvArg)|MoreWords], 
                 [boundary( adv(AdvArg) )|NewGap], Gap ) :-
	get_from_list( lexmatch, AdvArg, [Adv] ),
	Adv == 'not',
	!,
	mark_boundaries( MoreWords, NewGap, Gap ).

mark_boundaries( [noun(NounArg)|MoreWords], 
                 [noun(NounArg)|NewGap], Gap ) :-
	!,
	mark_boundaries( MoreWords, NewGap, Gap ).

mark_boundaries( [not_in_lex(NounArg)|MoreWords], 
                 [noun(NewNounArg)|NewGap], Gap ) :-
	!,
	append(NounArg,[tag(noun)],NewNounArg),
	mark_boundaries( MoreWords, NewGap, Gap ).

% past participles
mark_boundaries( [adj(AdjArg)|MoreWords], 
                 [boundary(pastpart(AdjArg))|NewGap], Gap ) :-
	get_from_list(tag,AdjArg,verb),!,
	mark_boundaries( MoreWords, NewGap, Gap ).

mark_boundaries( [adj(AdjArg)|MoreWords], 
                 [adj(AdjArg)|NewGap], Gap ) :-
	!,
	mark_boundaries( MoreWords, NewGap, Gap ).

mark_boundaries( [ThisItem|MoreItems], 
                 [boundary(ThisItem)|NewGap], Gap ) :-
	functor( ThisItem, Label, _ ),
	memberchk( Label, [aux, compl, conj, modal, prep, verb] ),
	!,
	mark_boundaries( MoreItems, NewGap, Gap ).

mark_boundaries( [AnythingElse|MoreWords], 
                 [AnythingElse|NewGap], Gap ) :-
	!,
	mark_boundaries( MoreWords, NewGap, Gap ).


punc_mark1(':').
punc_mark1('(').
punc_mark1(')').
% 01/03/2012: Per e-mail from Tom, we have changed the semantics of "[" and "]"
% to be phrase-breaking characters.
punc_mark1('[').
punc_mark1(']').
punc_mark1(';').
% 12/15/2011: Per e-mail discussion with Tom and Halil,
% we have changed the semantics of "/" to *not* be a phrase-breaking character.
% punc_mark1('/').
punc_mark1('<').
punc_mark1('>').
punc_mark1('=').
punc_mark1('*').


/*

% ******************** ADJUST_BOUNDARIES ******************************

ADJUST_BOUNDARIES looks at items which have been labelled by
MARK_BOUNDARIES.  Primarily, this predicate is concerned with adding
or erasing boundaries by examining the context in which the current
item occurs.

The label boundary is added to the second of two items in the
following instances:

    -when the first item has the label phead (potential head) and the
     second item ends in *ing* (which also has the label phead); (If
     the ing-word is not followed by a boundary and is not followed by
     punctuation, then it is an actual present participle (and not a
     head (gerund)).  For example, *the boys running laps*.  In such
     cases the participle introduces a new MSU.)

    -when the first item is NOT a boundary and the second item is
     either a determiner or a pronoun.

    -when the first item is a boundary which is also a conjunction,
     auxiliary, modal, verb, colon, or right parenthesis and the
     second item is not already a boundary and is also not a final
     punctuation mark. This has the effect of putting these items in
     their own MSU.

    -when the first item is NOT an adjective and the second is a
     comma.

The label boundary is removed (and the category label is adjusted) in
one instance: A word which has been labelled as a preposition and
which occurs immediately to the right of a determiner is considered
not to be functioning as a preposition in this sentence.

also changes boundary(punc(-)),boundary(punc(-)),Item -->
  boundary(punc(-)),punc(-), boundary(Item)

adjust_for_comma added. probably needs to be watched

*/


adjust_boundaries(BoundedList,NewBoundedList) :-
    adjust_for_comma(BoundedList,BoundedList0),   % quick fix !!!!
    adjust_boundaries_1(BoundedList0,NewBoundedList,[]).


% -----

adjust_for_comma([],[]) :- !.
adjust_for_comma([BeforeComma,Comma,AfterComma|More],
                 [BeforeComma,Comma|Gap]) :-
    \+ BeforeComma = adj( _ ),
    \+ AfterComma = boundary( _ ),
    Comma = punc([inputmatch([','])]),!,
    adjust_for_comma([boundary(AfterComma)|More], Gap ).
adjust_for_comma([Other|More],[Other|Gap]) :-
    adjust_for_comma(More, Gap).

% -----

adjust_boundaries_1( [], Gap, Gap ) :- !.
adjust_boundaries_1( [Anything|[]],[Anything|Gap], Gap) :- !.

adjust_boundaries_1( [PHead,ing(IngWord)|More], 
                   [PHead,boundary(ing(IngWord))|NewGap], Gap) :- 
     functor( PHead, Label, _ ),
     memberchk( Label, [noun,adj] ),
     More = [First|_Rest],
     \+ First = boundary( _ ),
     \+ First = punc( _ ), !,
     adjust_boundaries_1( More, NewGap, Gap ).
% The only adverb marking boundary is 'not'. However, in cases
% like 'the receptor positively regulates BRCA1', positively
% should be bracketed separately from 'the receptor'. This
% fixes the issue. --Halil
adjust_boundaries_1( [adv(Adv),boundary(Boundary)|More], 
                   [boundary(adv(Adv))|NewGap], Gap) :-
     functor( Boundary, Label, _ ),
     memberchk( Label, [modal,verb,aux] ),
     !,
     NewMore = [boundary( Boundary )|More],
     adjust_boundaries_1( NewMore, NewGap, Gap ).

adjust_boundaries_1([PreBoundary,Boundary|More],
                  [PreBoundary,boundary(Boundary)|NewGap], Gap) :-
    \+ PreBoundary = boundary(prep(_)), 
    ( Boundary = det(_)
     ;Boundary = pron(_)
    ), !,
    adjust_boundaries_1( More, NewGap, Gap ).

adjust_boundaries_1( [boundary(Boundary),PostBoundary|More], 
                   [boundary(Boundary)|NewGap], Gap) :-

% all these conditions should be cleaned up
    \+ PostBoundary = boundary(_),           
    \+ PostBoundary = punc([inputmatch(['.'])]),
    \+ PostBoundary = punc([inputmatch(['?'])]),
    \+ PostBoundary = punc([inputmatch(['!'])]),
    (  Boundary = conj( _ )
      ;Boundary = aux( _ )
      ;Boundary = modal( _ )
      ;Boundary = verb( _ )
      ;Boundary = punc([inputmatch([':'])])
      ;Boundary = punc([inputmatch([')'])])
    ),  !,
    NewMore = [boundary( PostBoundary )|More],
    adjust_boundaries_1( NewMore, NewGap, Gap ).

adjust_boundaries_1( [boundary( Boundary ),Item|More], 
                   [Boundary|NewGap], Gap ) :-
    Boundary = punc([inputmatch(['-'])]),  
    functor(Item,Label,_),
    \+ Label == boundary,!,
    NewMore = [boundary(Item)|More],
    adjust_boundaries_1( NewMore, NewGap, Gap ).

adjust_boundaries_1( [BeforeComma, Comma, AfterComma|More],
                   [BeforeComma, Comma|NewGap], Gap ) :-
    \+ BeforeComma = adj( _ ),
    \+ BeforeComma = boundary(adj(_)),
    \+ AfterComma = boundary( _ ),
    Comma = punc([inputmatch([','])]),!,
    NewMore = [boundary(AfterComma)|More],
    adjust_boundaries_1( NewMore, NewGap, Gap ).

adjust_boundaries_1( [PreBoundary,boundary(prep(ArgList))|More], 
                   [PreBoundary,ArgList|NewGap], Gap) :-
    PreBoundary = det( _ ),
    get_from_list( lexmatch, ArgList, [Word] ),
% this is adhoc and needs to be fixed
    \+ Word = of, !,                     
    adjust_boundaries_1( More, NewGap, Gap ).

adjust_boundaries_1( [Other|More], [Other|NewGap], Gap ) :-
    adjust_boundaries_1( More, NewGap, Gap ).


/*

% ************************** SEGMENT ********************************

On the basis of the labelled structure received from
ADJUST_BOUNDARIES, SEGMENT assigns bracketing.  Each bracketed
structure is a minimal syntactic unit (MSU). SEGMENT is analogous to
applying phrase structure rules, but is done on the basis of more
information than is normally available.

SEGMENT calls GET_MSU for each item that it encounters.  Items are put
into the current MSU until a boundary label is reached, whereupon
GET_MSU closes the current MSU and opens a new MSU beginning with the
current boundary (dropping the actual boundary label).

*/

segment([], Gap, Gap) :- !.

segment([boundary(Boundary)|More],[MSU|NewGap], Gap) :- 
    !,get_msu([Boundary|More],Remainder,MSU,[]),
    segment(Remainder,NewGap, Gap).

segment([Other|More],[MSU|NewGap], Gap) :-
    get_msu([Other|More],Remainder,MSU,[]),
    segment( Remainder, NewGap, Gap ).


% ---------- GET_MSU ----------

get_msu([],[], Gap, Gap) :- !.
get_msu([boundary(Boundary)|More],[Boundary|More], Gap, Gap) :- !.
get_msu([Other|More],Remainder,[Other|NewGap], Gap) :- 
    get_msu(More,Remainder,NewGap, Gap).


/*

% ************************ IDENTIFY HEADS ******************************

IDENTIFY_HEADS looks at each potential head (noun() or adj()) in each
MSU and determines whether it is actually a head.  This predicate only
looks at potential heads, and ignores everything else.  Recall that
nouns, adjectives and present participles have been labelled as
potential heads. When a potential head occurs as the final element of
an MSU it is identified as an actual head.  Certain types of right
modification are included in the MSU, with the consequence that a head
can occur further to the left than immediately at the end of the
structure.  A potential head which precedes a final past participle,
adverb, or punctuation is labelled as an actual head.  Both an adverb
and a past participle (and punctuation) are also allowed to follow a
head as in e.g "macromolecules removed directly" or "macromolecules
directly removed". Potential heads other than those discussed are
determined not to be heads.

At the conclusion of identify_heads, RESEGMENT is called, which checks
to see whether two heads have been assigned to this structure. (This
can happen anytime a potential head is followed by an adverb or a past
participle (or both).

*/

identify_heads([],HeadList,HeadList) :- !.
identify_heads([ThisMSUSubunit|More],HeadList,Accum) :-
    ident_heads(ThisMSUSubunit,SubunitHeadList),
    ( check_for_multiple_heads(SubunitHeadList)
      -> resegment(SubunitHeadList,NewSubunitHeadList),
         append(Accum,NewSubunitHeadList,NewAccum), !
      ;  append(Accum,[SubunitHeadList],NewAccum), !
    ),
    identify_heads( More, HeadList, NewAccum ).


%---------- IDENT HEADS ----------

% adj() is chosen only if there is no viable noun

ident_heads([], []).

ident_heads([PHead|More], [head(Head)|Gap]) :-
	functor(PHead, CurrLabel, _),
	memberchk(CurrLabel, [adj,noun]),
	length( More, MorLen),
	rev(More, RevMore),
	check_for_head_followers(MorLen, CurrLabel, More, RevMore),
	!,
	arg(1, PHead, Head),
	ident_heads(More, Gap).

ident_heads([ThisWord|More], [ThisWord|Gap]) :-
	ident_heads(More, Gap).


%----- CHECK FOR HEAD FOLLOWERS

check_for_head_followers(MorLen,CurrLabel,More, 
                         [punc(_)|RevTail]) :-
    !, NewMorLen is (MorLen - 1),
    check_for_head_followers(NewMorLen,CurrLabel,More,RevTail).


check_for_head_followers(0,_,_,_).

check_for_head_followers(1,noun,[Item|_],_RevMore) :-
   functor(Item,NexLabel,_),
   memberchk(NexLabel,[adj,adv,ing,pastpart]).

check_for_head_followers(2,noun,[adv(_)|_],[Second|_]) :-
   functor(Second,Label,_),
   memberchk(Label,[adj,pastpart]), !.

check_for_head_followers(2,noun,[First|_],[adv(_)|_]) :-
   functor(First,Label,_),
   memberchk(Label,[adj,pastpart]).


% ---------- CHECK_FOR_MULTIPLE_HEADS ----------

check_for_multiple_heads([]) :- !, fail.

check_for_multiple_heads([head(_)|More]) :-
     memberchk(head(_),More), !.

check_for_multiple_heads([_|More]) :-
     check_for_multiple_heads(More).


/*

% ---------- RESEGMENT ----------

In certain structures, ("cytotoxic T lymphocyte clones previously
found incapable", for example) SEGMENT assigns two heads.  RESEGMENT
determines whether structures with two heads should be split into two
MSU's or whether the first head should be downgraded to a mod(). (For
now, it always splits on two heads.)

*/

resegment(SubunitHeadList, NewSubunitHeadList) :-
	add_boundaries(SubunitHeadList, BoundaryList),
	segment(BoundaryList, NewSubunitHeadList, []).

% ---------- ADD_BOUNDARIES

add_boundaries(SubunitHeadList, BoundaryList) :-
    add_boundaries_1(SubunitHeadList, BoundaryList, []).


add_boundaries_1([], Gap, Gap) :- !.
add_boundaries_1([head(Head)], [head(Head)|Gap], Gap) :- !.
add_boundaries_1([head(Head), PostHead|More],
		 [head(Head), boundary(PostHead)|NewGap], Gap) :-
	!,
	%  \+ PostHead = punc(_),!,
	% \+ More = [], !,
    add_boundaries_1(More, NewGap, Gap).
add_boundaries_1([Other|More], [Other|NewGap], Gap) :-
    add_boundaries_1(More, NewGap, Gap).


/*

% ********************** IDENTIFY_LEFT_MODS *****************************

The underspecified nature of this analysis is seen most clearly in
IDENTIFY_LEFT_MODS. Anything that has not been previously labelled (as
either a head, past participle, adverb, or ing()) is labelled as a
modifier (mod()).

*/

identify_left_mods( [], Gap, Gap ).
identify_left_mods( [ThisMSUSubunit|More], 
                    [SubunitModList|NewGap], Gap ) :-
    ident_left_mods(ThisMSUSubunit, SubunitModList,[]),
    identify_left_mods( More, NewGap, Gap ).


% ---------- IDENT LEFT MODS ----------

ident_left_mods( [], Gap, Gap ) :- !.
ident_left_mods( [ThisWord|['-'|[pastpart(NextWord)|More]]], 
                 [mod([ThisWord,'-',pastpart(NextWord)])|NewGap], 
                 Gap ) :-
    !, ident_left_mods( More, NewGap, Gap ).

ident_left_mods([ThisWord|More],[mod(ThisWord)|NewGap], Gap) :-
    is_list( ThisWord ), !,
    ident_left_mods( More, NewGap, Gap ).

ident_left_mods( [noun( ThisWord )|More], 
                 [mod( ThisWord )|NewGap], Gap ) :-
    !, ident_left_mods( More, NewGap, Gap ).

ident_left_mods( [adj( ThisWord )|More], 
                 [mod( ThisWord )|NewGap], Gap ) :-
    !, ident_left_mods( More, NewGap, Gap ).

ident_left_mods( [ThisWord|More], 
                 [ThisWord|NewGap], Gap ) :-
    ident_left_mods( More, NewGap, Gap ).


% *********************** UTILITIES *******************************

% ----- GET_LEX_FEATURE

get_lex_feature( LexItem, Feature, LexItemList ) :-
    match_feat( LexItemList, LexItem, Feature ).


% ----- MATCH_FEAT

match_feat( [], _LexItem, _Feature  ) :- !, fail.
match_feat( [LexItem:[Feature]|_More], LexItem, Feature ) :- !.
match_feat( [_Other|More], LexItem,  Feature ) :-
     match_feat( More, LexItem, Feature ).


% ----- GET_VARIANT

get_variant( _Item, [], [] ) :- !, fail.
get_variant( Item, [Item:ItemInfo|_More], ItemInfo ) :- !.
get_variant( Item, [_Other|MoreItems], InfoList ) :-
    get_variant( Item, MoreItems, InfoList ).

