
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

% File:     nls_strings.pl
% Module:   NLS Strings
% Author:   Lan
% Purpose:  Provide miscellaneous string manipulation routines.
% Source:   strings_lra.pl


:- module(nls_strings, [
	atom_codes_list/2,
	atom_codes_list_list/2,
	concatenate_items_to_atom/2,
	concatenate_items_to_string/2,
	% OBSOLETE
	% eliminate_multiple_meaning_designator_string/2,
	% must be exported for mwi_utilities
	eliminate_nos_string/2,
	form_one_string/3,
	is_integer_string/1,
	is_print_string/1,
	normalized_syntactic_uninvert_string/2,
	number_codes_list/2,
	portray_strings_double_quoted/1,
	lex_stop_word/1,
	lex_stop_word_atom/1,
	% must be exported for mm_print and mwi_utilities 
	replace_all_substrings/4,
	replace_nonprints_in_strings/2,
	replace_tabs_in_strings/2,
	safe_number_codes/2,
	split_string/4,
	split_atom_completely/3,
	split_string_completely/3,
	split_string_backtrack/4,
	% must be exported for mm_print and mwi_utilities
	syntactic_uninvert_string/2,
	trim_and_compress_whitespace/2,
	trim_whitespace/2,
	% must be exported for mwi_utilities
	trim_whitespace_left/2,
	trim_whitespace_left_count/3,
	trim_whitespace_left_1/4,
	% must be exported for SemRep
	trim_whitespace_right/2
    ]).


:- use_module(metamap(metamap_tokenization), [
	local_alnum/1,
	local_alpha/1,
	local_digit/1,
	local_print/1,
	local_to_lower/2,
	local_ws/1,
	tokenize_text_more_lc/2
    ]).

:- use_module(skr(skr_utilities), [
        ensure_atom/2,
        fatal_error/2
   ]).

% :- use_module(skr_lib(ctypes), [
% 	is_alnum/1,
% 	is_print/1,
% 	is_digit/1,
% 	is_white/1,
% 	to_lower/2
%     ]).

:- use_module(skr_lib(nls_text), [
	string_uninvert/2
    ]).

:- use_module(skr_lib(sicstus_utils), [
	concat_strings_with_separator/3,
	lower/2,
	midstring/6
    ]).

:- use_module(library(lists), [
	append/2,
	rev/2
    ]).

:- use_module(library(lists3), [
	substitute/4
    ]).


% Use this predicate if you know that none of the strings
% to be converted to atoms is > 65K chars long.

/* atom_codes_list(?Atoms, ?Strings)
atom_codes_list/2 applies atom_codes/2 to lists. */

% The cut in the first clause is necessary because atom_codes_list/2 is bi-directional.
atom_codes_list([], []) :- !.
atom_codes_list([FirstAtom|RestAtoms], [FirstString|RestStrings]) :-
	atom_codes(FirstAtom, FirstString),
	atom_codes_list(RestAtoms, RestStrings).

% convert to numbers if the atoms are a representation of numbers
number_codes_list([], []).
number_codes_list([FirstNum|RestNums], [FirstString|RestStrings]) :-
	( var(FirstNum) ->
	  ( safe_number_codes(FirstNum, FirstString) ->
	    true
	  ; atom_codes(FirstNum, FirstString)
	  )
	; number(FirstNum) ->
	  number_codes(FirstNum, FirstString)
	; atom(FirstNum) ->
	  atom_codes(FirstNum, FirstString)
	),
	number_codes_list(RestNums, RestStrings).


/* atom_codes_list_list(?AtomListList, ?StringListList)
atom_codes_list_list/2 applies atom_codes/2 to lists of lists. */

atom_codes_list_list(AtomListList,StringListList) :-
    atom_codes_list_list_aux(AtomListList,StringListList),
    !.
atom_codes_list_list(AtomListList,StringListList) :-
    ( var(AtomListList) ->
	( var(StringListList) ->
	  fatal_error('atom_codes_list_list/2 failed (arguments uninstantiated).~n', [])
	; fatal_error('atom_codes_list_list/2 failed for ~p~n.', [StringListList])
	)
    ;   ( var(StringListList) ->
	  fatal_error('atom_codes_list_list/2 failed for ~p~n.', [AtomListList])
	; fatal_error('atom_codes_list_list/2 failed for ~p and ~p.~n', [AtomListList,StringListList])
	)
     ).

atom_codes_list_list_aux([], []).
atom_codes_list_list_aux([FirstAtomList|RestAtomListList],
			 [FirstStringList|RestStringListList]) :-
	atom_codes_list(FirstAtomList, FirstStringList),
	atom_codes_list_list_aux(RestAtomListList, RestStringListList).


/* concatenate_items_to_atom
Concatenate a list of items forming an atom.  The items must be strings, 
atoms or numbers. */

concatenate_items_to_atom([], '').
concatenate_items_to_atom([H|T], Atom) :-
	concatenate_items_to_string([H|T], String),
	atom_codes(Atom, String).


/* concatenate_items_to_string
Concatenate a list of items forming a string.  The items must be strings, 
atoms or numbers. */

concatenate_items_to_string([], "").
concatenate_items_to_string([Item|Rest], String) :-
	convert_item_to_string(Item, ItemString),
	append(ItemString,RestString, String),
	concatenate_items_to_string(Rest, RestString).

/* convert_item_to_string
Convert an item to a string.  The item must be a string, an atom or a number. */

convert_item_to_string(Item, String) :-
	( Item == [] ->
	  String = []
	; atom(Item) ->
	  atom_codes(Item, String)
	; number(Item) ->
	  number_codes(Item, String)
	; String = Item
	).

/* eliminate_multiple_meaning_designator_string(+String, -ModifiedString)

eliminate_multiple_meaning_designator_string/2 removes an expression of the
form <n> where n is an integer from String producing ModifiedString.  */

% OBSOLETE
% eliminate_multiple_meaning_designator_string(String, ModifiedString) :-
% 	split_string_backtrack(String, "<", Base, A1),
% 	split_string_backtrack(A1, ">", Integer, Tail),
% 	is_integer_string(Integer),
% 	trim_whitespace(Tail, ""),
% 	!,
% 	trim_whitespace(Base, ModifiedString).
% eliminate_multiple_meaning_designator_string(String, String).

eliminate_nos_string(String, NormString) :-
	eliminate_nos_acros(String, NormString0),
	eliminate_nos_expansion(NormString0, NormString).

eliminate_nos_acros(String,  NormString) :-
	( split_string_backtrack(String, "NOS", Left, Right) ->
	  true
	; split_string_backtrack(String, "nos", Left, Right)
	),
	\+ begins_with_alnum(Right),
	\+ ends_with_alpha(Left),
	% These "NOSDD" designations refer to the tests and filenames
	% in the 0doit2 script in the NEC_NOS directory
	split_a_string([
		   ".NOS",       % NOS04
		   "(NOS)",      % NOS05
		   ":NOS",       % NOS06
		   "/NOS",       % NOS07
		   "_NOS",       % NOS08
		   ",NOS",       % NOS09
		   "-NOS",       % NOS10
		   ")NOS",       % NOS11
		   ", NOS",      % NOS13
		   "; NOS",      % NOS14
		   " NOS",       % NOS15
		   " NOS ",      % NOS16
		   " NOS, ",     % NOS17
		   " NOS)",      % NOS18
		   " NOS]",      % NOS19
		   " NOS-",      % NOS20
		   " NOS; ",     % NOS21
		   " NOS/",      % NOS22
    		   "[NOS] or",   % NOS23
    		   "or [NOS]",   % NOS24
    		   "(& [NOS])",  % NOS25
    		   "NOS+",       % NOS26
    		   " NOS:",      % NOS27
    		   " NOS,",      % NOS28
    		   "[NOS]",      % NOS29
    		   ", nos",      % 2012AA.RRF contains five of these (see below)
    		   " nos"        % 2012AA.RRF contains > 100 of these (see below)
		      ], String, Left2, Substring, Right2),
	% Continuation of NOS15, testing for end of string
	( Substring == " NOS" ->
	  Right2 == []
	; Substring == ", nos" ->
	  Right2 == []
	; Substring == " nos" ->
	  Right2 == []
	; true
	),
	(   Substring == ")NOS" ->
	    append([Left2,")",Right2], NormString0)
	;   Substring == ".NOS" ->
	    append([Left2,".",Right2], NormString0)
	;   Substring == " NOS" ->
	    \+ abgn_form(Right2),
	    append(Left2, Right2, NormString0)
	;   Substring == "-NOS" ->
	    \+ short_last(Left2),
	    append(Left2, Right2, NormString0)
	;   append(Left2, Right2, NormString0)
	),
	!,
	eliminate_nos_acros(NormString0,NormString).
eliminate_nos_acros(String,String).

% Cases of ", nos":
% Mental health service, nos
% DME supply or accessory, nos
% Waiver service, nos
% Supply, nos
% Topical Ox Deliver sys, nos
% 
% Some cases of " nos":
% Abdominal mass nos
% Abnormal result investigation nos
% Abnormal urine test nos
% Alcohol/drug abuse svc nos
% Allergy/allergic reaction nos
% Anesth cranial surg nos
% Assay ph body fluid nos
% Bleeding/haemorrhage nos
% Bursitis/tendinitis/synovitis nos
% Cardiac arrhythmia nos
% Cardiovascular pain nos
% Cervical disease nos
% Chest pain nos
% DME supply or accessory, nos
% Extracorp shock wv tx ms nos
% Fear of cancer nos
% Fear of other disease nos
% Health education - general nos
% Health education - subject nos
% Heart valve disease nos
% Heart/arterial murmur nos

eliminate_nos_expansion(String, NormString) :-
	lower(String, LCString),
	atom_codes(Atom, String),
	atom_codes(LCAtom, LCString),
	eliminate_nos_expansion_aux(LCAtom, Atom, NormAtom),
	!,
	atom_codes(NormAtom, NormString).
eliminate_nos_expansion(String, String).

eliminate_nos_expansion_aux(LCAtom, Atom, NormAtom) :-
    nos_expansion(NE),
    midstring(LCAtom, NE, _, LenA, LenB, LenC),
    !,
    midstring(Atom, _, NormAtom, LenA, LenB, LenC).

% These designations refer to the tests and filenames
% in the 0doit5 script in the NEC_NOS directory
% sp01
nos_expansion(', not otherwise specified').
% sp02
nos_expansion('; not otherwise specified').
% sp03
nos_expansion(', but not otherwise specified').
% sp04
nos_expansion(' but not otherwise specified').
% sp05
nos_expansion('(not otherwise specified)').
% sp06
nos_expansion(' not otherwise specified').
% sp07
nos_expansion(' to be specified elsewhere').
% sp08
nos_expansion(', not elsewhere specified').
% sp09
nos_expansion('; not elsewhere specified').
% sp10
nos_expansion(' not elsewhere specified').
% sp11
nos_expansion(': not further specified').
% sp12
nos_expansion(', not further specified').
% sp13
nos_expansion(' not further specified').

/* 
   split_a_string(+Substrings, +String, -Left, -Substring, -Right)

split_a_string/5 does the same thing except that it returns the Left
and Right strings in addition to the matching Substring.*/

% split_a_string([], _String, _, _, _) :-
%     !,
%     fail.
split_a_string([First|_Rest], String, Left, First, Right) :-
	split_string(String, First, Left, Right),
	!.
split_a_string([_First|Rest],String, Left, Substring, Right) :-
	split_a_string(Rest, String, Left, Substring, Right).

%%% test_nos :-
%%% 	test_atom(Atom),
%%% 	atom_codes(Atom, String),
%%%         eliminate_nos_string(String, NormString),
%%% 	format('~nString: ~s~n(Norm): ~s~n', [String,NormString]),
%%% 	fail.
%%% test_nos.
%%% 
%%% test_atom('342 ACQUIRED STENOSES').
%%% test_atom('NOS1 gene product').
%%% test_atom('COLON NOS POLYPECTOMY COLONOSCOPY').
%%% test_atom('HoNOS item 1').
%%% test_atom('OPISTHOTONOS').
%%% test_atom('iNOS enzyme').
%%% test_atom('NOS').
%%% test_atom('NOS AB').
%%% test_atom('#Rib/sternum/larynx/trach.NOS').
%%% test_atom('Burning-watercr.NOS-other').
%%% test_atom('ALLERGIC REACTION (NOS)').
%%% test_atom('Lupus erythematosis (NOS) <2>').
%%% test_atom('NEUROPATHY - (NOS)').
%%% test_atom('BACTERIA:ACNC:PT:NOS:ORD:MICROSCOPY.LIGHT').
%%% test_atom('Bone/cartilage disord. OS/NOS').
%%% test_atom('Bran.cleft/preaur.anom.OS/NOS').
%%% test_atom('INFECTIOUS DISEASE OTHER/NOS').
%%% test_atom('BLEED_NOS').
%%% test_atom('BLEED_NOS PROBLEM').
%%% test_atom('Infect.endocarditis+dis EC,NOS').
%%% test_atom('ADVA-NOS-animal rider').
%%% test_atom('ANB-NOS').
%%% test_atom('HRO-NOS protein').
%%% test_atom('MVNTA-NOS').
%%% test_atom('Non-surgical biopsy (admin)NOS').
%%% test_atom('A VARIANT NOS AB').
%%% test_atom('A VARIANT NOS AG').
%%% test_atom('A VARIANT NOS ANTIBODY').
%%% test_atom('L (LITTLE U) NOS AG').
%%% test_atom('i NOS ANTIGEN <1>').
%%% test_atom('ACE inhibitor, NOS').
%%% test_atom('Ignition of clothing, NOS, from controlled fire, NOS, in building NOS').
%%% test_atom('Toxin, NOS (animal)').
%%% test_atom('10 year exam. NOS').
%%% test_atom('2-malig neop LN head/face NOS').
%%% test_atom('967-968 MALIGNANT LYMPHOMA, SPECIFIED TYPE, DIFFUSE OR NOS').
%%% test_atom('ANESTHESIA, PROC ON EYE; NOS').
%%% test_atom('Accidents NOS').
%%% test_atom('Bronchitis NOS, with tracheitis NOS').
%%% test_atom('ADDITION TO SPINAL ORTHOSIS, NOT OTHERWISE SPECIFIED').
%%% test_atom('Acute appendicitis, not otherwise specified').
%%% test_atom('FOR DIABETICS ONLY, NOT OTHERWISE SPECIFIED MODIFICATION (INCLUDING FITTING) OF OFF-THE-SHELF DEPTH-INLAY SHOE OR CUSTOM-MOLDED SHOE, PER SHOE').
%%% test_atom('IMMUNOASSAY, ANALYTE, QUANTITATIVE; NOT OTHERWISE SPECIFIED').
%%% test_atom('Anesthesia for procedures on eye; not otherwise specified').
%%% test_atom('INFECTIOUS AGENT ANTIGEN DETECTION BY IMMUNOFLUORESCENT TECHNIQUE; NOT OTHERWISE SPECIFIED, EACH ORGANISM').
%%% test_atom('Injury stated as accidentally inflicted, but not otherwise specified').
%%% test_atom('Killed, stated as accidentally inflicted but not otherwise specified').
%%% test_atom('ANESTHESIA FOR CLOSED CHEST PROCEDURES; (INCLUDING BRONCHOSCOPY) NOT OTHERWISE SPECIFIED').
%%% test_atom('Closed fracture of distal femur not otherwise specified').
%%% test_atom('1 VIEW:FINDING:POINT IN TIME:HIP.TO BE SPECIFIED ELSEWHERE:NARRATIVE:XR').
%%% test_atom('2-BUTANOL:MASS CONCENTRATION:POINT IN TIME:TO BE SPECIFIED ELSEWHERE:QUANTITATIVE').
%%% test_atom('ANTIBODY; BACTERIUM, NOT ELSEWHERE SPECIFIED').
%%% test_atom('Antibody; bacterium, not elsewhere specified').
%%% test_atom('IMMUNODIFFUSION; NOT ELSEWHERE SPECIFIED').
%%% test_atom('Immunodiffusion; not elsewhere specified').
%%% test_atom('ASSAY OF NEPHELOMETRY, EACH ANALYTE NOT ELSEWHERE SPECIFIED').
%%% test_atom('Chromatography, qualitative; thin layer, analyte not elsewhere specified').
%%% test_atom('Chromatography, quantitative, column (eg, gas liquid or HPLC); single analyte not elsewhere specified, single stationary and mobile phase').

begins_with_alnum([Char|_]) :-
    local_alnum(Char),
    !.

ends_with_alpha(String) :-
    rev(String,[Char|_]),
    local_alpha(Char),
    !.

abgn_form([0' ,0'A,Third|_]) :-  % " ANTIBODY", " ANTIGEN", " AB", " AG"
    memberchk(Third,[0'B,0'G,0'N]),
    !.

short_last(String) :-  % "HRO-NOS protein" but not "ADVA-NOS-animal rider"
    rev(String,RevString),
    (split_string(RevString," ",Last,_) ->
	length(Last,N),
	N<4
    ;   length(String,N),
	N<4
    ),
    !.


/* is_integer_string(+String)

is_integer_string/1 succeeds if String is a string of printable characters.  */

is_integer_string("").
is_integer_string([First|Rest]) :-
	local_digit(First),
	is_integer_string(Rest).

/* is_print_string(+String)

is_print_string/1 succeeds if String is a string of printable characters.  */

is_print_string(String) :-
	nonvar(String),
	is_print_string_aux(String).
is_print_string_aux([]).
is_print_string_aux([First|Rest]) :-
	nonvar(First),
 	local_print(First),
 	is_print_string_aux(Rest).

/* uninvert_string(+String, -UninvString)
   normalize_string(+String, -NormString)
   syntactic_uninvert_string(+String, -UninvString)
   normalized_syntactic_uninvert_string(+String, -NormUninvString)

uninvert_string/2 simply calls lexical:string_uninvert/2.
normalized_uninvert_text/2 first normalizes  and then uninverts String.
normalize_string/2 first eliminates multiple meaning designators (<n>) (OBSOLETE)
and then eliminates all forms of NOS.
syntactic_uninvert_string/2 calls uninvert_string/2 on String if it
contains ", " and does not contain a preposition or conjunction.
normalized_syntactic_uninvert_string/2 first normalizes and then
syntactically uninverts String.  */

uninvert_string(String,UninvString) :-
    string_uninvert(String,UninvString).

% eliminate_multiple_meaning_designator_string/2 is OBSOLETE,
% so normalize_string/2 is simply eliminate_nos_string now.
% normalize_string(String,NormString) :-
%     eliminate_multiple_meaning_designator_string(String,String1),
%     eliminate_nos_string(String1,NormString).

normalize_string(String, NormString) :-
	eliminate_nos_string(String, NormString).

normalized_syntactic_uninvert_string(String, NormSUninvString) :-
	normalize_string(String, NormString),
	syntactic_uninvert_string(NormString, NormSUninvString).

syntactic_uninvert_string(String, SUninvString) :-
	split_string(String, ", ", Before, After),
	!,
	( contains_comma(After) ->
	  SUninvString = String
	; contains_stop_word(String, StopWord),
	  StopWord \== 'a' ->
	  SUninvString = String
	; alnum_count(Before, 0, AlnumCount),
	  AlnumCount >= 3 ->
	  uninvert_string(String,SUninvString)
	; SUninvString = String
	).
syntactic_uninvert_string(String, String).


alnum_count([], AlnumCount, AlnumCount).
alnum_count([H|T], AlnumCountIn, AlnumCountOut) :-
	( local_alnum(H) ->
	  AlnumCountNext is AlnumCountIn + 1
	; AlnumCountNext is AlnumCountIn
	),
	alnum_count(T, AlnumCountNext, AlnumCountOut).


contains_comma(After) :-
	memberchk(0',, After).

contains_stop_word(String, StopWord) :-
    tokenize_text_more_lc(String,LCTokens),
    contains_stop_word_aux(LCTokens, StopWord).

contains_stop_word_aux([FirstString|Rest], StopWord) :-
	atom_codes(FirstAtom, FirstString),
	  % don't use the lexicon here for efficiency
	( lex_stop_word_atom(FirstAtom) ->
	  StopWord = FirstAtom
	; contains_stop_word_aux(Rest, StopWord)
	).

/* prep_or_conj(?PrepOrConj)

prep_or_conj/1 is a factual predicate of prepositions/conjunctions.
The data is derived from the 1999 lexicon and consists of any word without
special characters (blank, period, slash) that is either a preposition or
a conjunction. See .../Support/LexLab/ for details.  */

%% some clauses have been commented out to see if better results are obtained
%% The data were updated using the 2013 lex_form.txt file.
%% The data were updated using the 2014 lex_form.txt file.

% Use lex_stop_word_atom/1 instead

% prep_or_conj('aboard').
% prep_or_conj('about').
% prep_or_conj('above').
% prep_or_conj('across').
% prep_or_conj('after').
% prep_or_conj('against').
% prep_or_conj('aka').
% prep_or_conj('albeit').
% prep_or_conj('allover').
% prep_or_conj('along').
% prep_or_conj('alongside').
% prep_or_conj('although').
% prep_or_conj('amid').
% prep_or_conj('amidst').
% prep_or_conj('among').
% prep_or_conj('amongst').
% prep_or_conj('and').
% prep_or_conj('anti').
% prep_or_conj('around').
% prep_or_conj('as').
% prep_or_conj('astride').
% prep_or_conj('at').
% prep_or_conj('atop').
% % prep_or_conj('bar').
% prep_or_conj('because').
% prep_or_conj('before').
% prep_or_conj('behind').
% prep_or_conj('below').
% prep_or_conj('beneath').
% prep_or_conj('beside').
% prep_or_conj('besides').
% prep_or_conj('between').
% prep_or_conj('betwixt').
% prep_or_conj('beyond').
% prep_or_conj('but').
% prep_or_conj('by').
% prep_or_conj('circa').
% prep_or_conj('concerning').
% prep_or_conj('contra').
% prep_or_conj('despite').
% % prep_or_conj('down').
% prep_or_conj('during').
% prep_or_conj('ex').
% prep_or_conj('except').
% prep_or_conj('excluding').
% prep_or_conj('failing').
% prep_or_conj('following').
% prep_or_conj('for').
% prep_or_conj('from').
% prep_or_conj('given').
% prep_or_conj('however').
% prep_or_conj('if').
% prep_or_conj('in').
% prep_or_conj('inbetween').
% prep_or_conj('incl').
% prep_or_conj('including').
% prep_or_conj('inside').
% prep_or_conj('into').
% prep_or_conj('less').
% prep_or_conj('lest').
% % prep_or_conj('like').
% % prep_or_conj('mid').
% prep_or_conj('minus').
% prep_or_conj('modulo').
% % prep_or_conj('near').
% prep_or_conj('nearby').
% prep_or_conj('neath').
% prep_or_conj('nor').
% prep_or_conj('notwithstanding').
% prep_or_conj('of').
% % prep_or_conj('off').
% prep_or_conj('on').
% prep_or_conj('onboard').
% prep_or_conj('once').
% prep_or_conj('only').
% prep_or_conj('onto').
% prep_or_conj('or').
% prep_or_conj('out').
% prep_or_conj('outwith').
% prep_or_conj('over').
% prep_or_conj('overagainst').
% prep_or_conj('past').
% prep_or_conj('pending').
% prep_or_conj('per').
% prep_or_conj('plus').
% prep_or_conj('provided').
% prep_or_conj('providing').
% prep_or_conj('qua').
% prep_or_conj('regarding').
% prep_or_conj('respecting').
% prep_or_conj('round').
% prep_or_conj('sans').
% prep_or_conj('sensu').
% prep_or_conj('since').
% prep_or_conj('so').
% prep_or_conj('suppose').
% prep_or_conj('supposing').
% prep_or_conj('than').
% prep_or_conj('therefore').
% prep_or_conj('though').
% prep_or_conj('through').
% prep_or_conj('throughout').
% prep_or_conj('thru').
% prep_or_conj('til').
% prep_or_conj('till').
% prep_or_conj('to').
% prep_or_conj('toward').
% prep_or_conj('towards').
% prep_or_conj('under').
% prep_or_conj('underneath').
% prep_or_conj('unless').
% prep_or_conj('unlike').
% prep_or_conj('until').
% prep_or_conj('unto').
% prep_or_conj('up').
% prep_or_conj('upon').
% prep_or_conj('upside').
% prep_or_conj('upto').
% prep_or_conj('versus').
% prep_or_conj('via').
% prep_or_conj('vs').
% prep_or_conj('w').
% prep_or_conj('wanting').
% prep_or_conj('when').
% prep_or_conj('whenever').
% prep_or_conj('where').
% prep_or_conj('whereafter').
% prep_or_conj('whereas').
% prep_or_conj('whereat').
% prep_or_conj('whereby').
% prep_or_conj('wherefore').
% prep_or_conj('wherein').
% prep_or_conj('whereof').
% prep_or_conj('whereupon').
% prep_or_conj('wherever').
% prep_or_conj('whether').
% prep_or_conj('while').
% prep_or_conj('whilst').
% prep_or_conj('with').
% prep_or_conj('within').
% prep_or_conj('without').
% prep_or_conj('worth').
% prep_or_conj('yet').
 
/* 
    portray_strings_double_quoted(+String)

For example, portray_strings_double_quoted/1
prints strings double-quoted and fails on non-strings.  Note that the
empty list is not treated as a string.  Thus, it will be printed as
[] rather than "".  portray_strings_double_quoted/1 is suitable to be
used with addportray:add_portray/1 so that strings will be printed
double-quoted even outside the prolog and qui environments
(portray_strings_double_quoted works with qpc, whereas library(print_chars)
does not).  */

% portray_strings_double_quoted([]) :-
%     !,
%     fail.
portray_strings_double_quoted(String) :-
    is_print_string(String),
    put_print_string(String),
    !.

% Note: put_print_string/1 is at the end of this file because its syntax
% confuses emacs.


/* lex_stop_word_atom(?Word)
/* lex_stop_word(?Word)

lex_stop_word_atom/1 is a factual predicate of prepositions/conjunctions/
determiners. The data is derived from the 2006 lexicon and consists of any word
without special characters (blank, period, hyphen, slash) that is either a
preposition, a conjunction or a determiner. See .../Support/LexLab/ for details.
lex_stop_word/1 is the same predicate for strings. */

%% The data were updated using 2013 lex_form.txt file.

lex_stop_word_atom('''''d').
lex_stop_word_atom('''''m').
lex_stop_word_atom('''''re').
lex_stop_word_atom('''''s').
lex_stop_word_atom('''''ve').
lex_stop_word_atom('a').
lex_stop_word_atom('aboard').
lex_stop_word_atom('about').
lex_stop_word_atom('above').
lex_stop_word_atom('according as').
lex_stop_word_atom('according to').
lex_stop_word_atom('across').
lex_stop_word_atom('across from').
lex_stop_word_atom('after').
lex_stop_word_atom('against').
lex_stop_word_atom('ahead of').
lex_stop_word_atom('aka').
lex_stop_word_atom('albeit').
lex_stop_word_atom('all').
lex_stop_word_atom('all over').
lex_stop_word_atom('allover').
lex_stop_word_atom('along').
lex_stop_word_atom('along with').
lex_stop_word_atom('alongside').
lex_stop_word_atom('alongside of').
lex_stop_word_atom('although').
lex_stop_word_atom('am').
lex_stop_word_atom('amid').
lex_stop_word_atom('amidst').
lex_stop_word_atom('among').
lex_stop_word_atom('amongst').
lex_stop_word_atom('an').
lex_stop_word_atom('and').
lex_stop_word_atom('and/or').
lex_stop_word_atom('another').
% lex_stop_word_atom('anti').
lex_stop_word_atom('any').
lex_stop_word_atom('apart from').
lex_stop_word_atom('apropos of').
lex_stop_word_atom('are').
lex_stop_word_atom('aren''''t').
lex_stop_word_atom('around').
lex_stop_word_atom('as').
lex_stop_word_atom('as far as').
lex_stop_word_atom('as for').
lex_stop_word_atom('as if').
lex_stop_word_atom('as of').
lex_stop_word_atom('as regards').
lex_stop_word_atom('as though').
lex_stop_word_atom('as to').
lex_stop_word_atom('as well as').
lex_stop_word_atom('aside from').
lex_stop_word_atom('astride').
lex_stop_word_atom('at').
lex_stop_word_atom('at odds with').
lex_stop_word_atom('at risk of').
lex_stop_word_atom('at the behest of').
lex_stop_word_atom('at variance with').
lex_stop_word_atom('atop').
lex_stop_word_atom('away from').
lex_stop_word_atom('back of').
lex_stop_word_atom('bar').
lex_stop_word_atom('be').
lex_stop_word_atom('because').
lex_stop_word_atom('because of').
lex_stop_word_atom('been').
lex_stop_word_atom('before').
lex_stop_word_atom('behind').
lex_stop_word_atom('being').
lex_stop_word_atom('below').
lex_stop_word_atom('beneath').
lex_stop_word_atom('beside').
lex_stop_word_atom('besides').
lex_stop_word_atom('between').
lex_stop_word_atom('betwixt').
lex_stop_word_atom('beyond').
lex_stop_word_atom('both').
lex_stop_word_atom('but').
lex_stop_word_atom('but for').
lex_stop_word_atom('by').
lex_stop_word_atom('by comparison with').
lex_stop_word_atom('by dint of').
lex_stop_word_atom('by force of').
lex_stop_word_atom('by means of').
lex_stop_word_atom('by virtue of').
lex_stop_word_atom('by way of').
lex_stop_word_atom('ca.').
lex_stop_word_atom('certain').
lex_stop_word_atom('circa').
lex_stop_word_atom('concerning').
lex_stop_word_atom('contra').
lex_stop_word_atom('despite').
lex_stop_word_atom('did').
lex_stop_word_atom('didn''''t').
lex_stop_word_atom('do').
lex_stop_word_atom('does').
lex_stop_word_atom('doesn''''t').
lex_stop_word_atom('don''''t').
lex_stop_word_atom('down').
lex_stop_word_atom('downstream from').
lex_stop_word_atom('downstream of').
lex_stop_word_atom('due to').
lex_stop_word_atom('during').
lex_stop_word_atom('e.g.').
lex_stop_word_atom('each').
lex_stop_word_atom('either').
lex_stop_word_atom('enough').
lex_stop_word_atom('every').
lex_stop_word_atom('ex').
lex_stop_word_atom('except').
lex_stop_word_atom('except for').
lex_stop_word_atom('excluding').
lex_stop_word_atom('exclusive of').
lex_stop_word_atom('failing').
lex_stop_word_atom('few').
lex_stop_word_atom('fewer').
lex_stop_word_atom('following').
lex_stop_word_atom('for').
lex_stop_word_atom('for sake of').
lex_stop_word_atom('for want of').
lex_stop_word_atom('forasmuch as').
lex_stop_word_atom('from').
lex_stop_word_atom('from among').
lex_stop_word_atom('from want of').
lex_stop_word_atom('given').
lex_stop_word_atom('had').
lex_stop_word_atom('hadn''''t').
lex_stop_word_atom('has').
lex_stop_word_atom('hasn''''t').
lex_stop_word_atom('have').
lex_stop_word_atom('haven''''t').
lex_stop_word_atom('having').
lex_stop_word_atom('however').
lex_stop_word_atom('i.e.').
lex_stop_word_atom('if').
lex_stop_word_atom('in').
lex_stop_word_atom('in accordance with').
lex_stop_word_atom('in addition to').
lex_stop_word_atom('in aid of').
lex_stop_word_atom('in as much as').
lex_stop_word_atom('in back of').
lex_stop_word_atom('in behalf of').
lex_stop_word_atom('in between').
lex_stop_word_atom('in case of').
lex_stop_word_atom('in common with').
lex_stop_word_atom('in comparison to').
lex_stop_word_atom('in compliance with').
lex_stop_word_atom('in conformity with').
lex_stop_word_atom('in conjunction with').
lex_stop_word_atom('in contact with').
lex_stop_word_atom('in contrast to').
lex_stop_word_atom('in default of').
lex_stop_word_atom('in exchange for').
lex_stop_word_atom('in face of').
lex_stop_word_atom('in favor of').
lex_stop_word_atom('in favour of').
lex_stop_word_atom('in front of').
lex_stop_word_atom('in league with').
lex_stop_word_atom('in lieu of').
lex_stop_word_atom('in light of').
lex_stop_word_atom('in line with').
lex_stop_word_atom('in place of').
lex_stop_word_atom('in quest of').
lex_stop_word_atom('in reference to').
lex_stop_word_atom('in regard to').
lex_stop_word_atom('in relation to').
lex_stop_word_atom('in respect of').
lex_stop_word_atom('in respect to').
lex_stop_word_atom('in return for').
lex_stop_word_atom('in search of').
lex_stop_word_atom('in spite of').
lex_stop_word_atom('in step with').
lex_stop_word_atom('in terms of').
lex_stop_word_atom('in to').
lex_stop_word_atom('in view of').
lex_stop_word_atom('inasmuch as').
lex_stop_word_atom('inbetween').
lex_stop_word_atom('incl').
lex_stop_word_atom('incl.').
lex_stop_word_atom('including').
lex_stop_word_atom('inclusive of').
lex_stop_word_atom('independent of').
lex_stop_word_atom('independently of').
lex_stop_word_atom('inside').
lex_stop_word_atom('inside of').
lex_stop_word_atom('insofar as').
lex_stop_word_atom('insomuch as').
lex_stop_word_atom('instead of').
lex_stop_word_atom('into').
lex_stop_word_atom('irregardless of').
lex_stop_word_atom('irrespective of').
lex_stop_word_atom('is').
lex_stop_word_atom('isn''''t').
lex_stop_word_atom('last').
lex_stop_word_atom('less').
lex_stop_word_atom('lest').
lex_stop_word_atom('like').
lex_stop_word_atom('many').
lex_stop_word_atom('mid').
lex_stop_word_atom('minus').
lex_stop_word_atom('modulo').
lex_stop_word_atom('more').
lex_stop_word_atom('most').
lex_stop_word_atom('much').
lex_stop_word_atom('nary a').
lex_stop_word_atom('nary an').
lex_stop_word_atom('near').
lex_stop_word_atom('nearby').
lex_stop_word_atom('neath').
lex_stop_word_atom('neither').
lex_stop_word_atom('next to').
lex_stop_word_atom('no').
lex_stop_word_atom('nor').
lex_stop_word_atom('notwithstanding').
lex_stop_word_atom('of').
lex_stop_word_atom('off').
lex_stop_word_atom('off of').
lex_stop_word_atom('on').
lex_stop_word_atom('on account of').
lex_stop_word_atom('on behalf of').
lex_stop_word_atom('on board').
lex_stop_word_atom('on grounds of').
lex_stop_word_atom('on the basis of').
lex_stop_word_atom('on to').
lex_stop_word_atom('on top of').
lex_stop_word_atom('on-board').
lex_stop_word_atom('onboard').
lex_stop_word_atom('once').
% lex_stop_word_atom('only').
lex_stop_word_atom('onto').
lex_stop_word_atom('or').
lex_stop_word_atom('other').
lex_stop_word_atom('other than').
% lex_stop_word_atom('out').
lex_stop_word_atom('out of').
lex_stop_word_atom('outside of').
lex_stop_word_atom('outwith').
lex_stop_word_atom('over').
lex_stop_word_atom('over against').
lex_stop_word_atom('over and above').
lex_stop_word_atom('overagainst').
lex_stop_word_atom('owing to').
% lex_stop_word_atom('past').
lex_stop_word_atom('pending').
lex_stop_word_atom('per').
% lex_stop_word_atom('plus').
lex_stop_word_atom('previous to').
lex_stop_word_atom('prior to').
lex_stop_word_atom('provided').
lex_stop_word_atom('provided that').
lex_stop_word_atom('providing').
lex_stop_word_atom('providing that').
lex_stop_word_atom('pursuant to').
lex_stop_word_atom('qua').
lex_stop_word_atom('rather than').
lex_stop_word_atom('reg.').
lex_stop_word_atom('regarding').
lex_stop_word_atom('regardless of').
lex_stop_word_atom('respecting').
% lex_stop_word_atom('round').
lex_stop_word_atom('s/p').
lex_stop_word_atom('sans').
lex_stop_word_atom('sensu').
lex_stop_word_atom('several').
lex_stop_word_atom('short of').
lex_stop_word_atom('since').
lex_stop_word_atom('so').
lex_stop_word_atom('some').
lex_stop_word_atom('status post').
lex_stop_word_atom('subject to').
lex_stop_word_atom('subsequent to').
lex_stop_word_atom('such').
lex_stop_word_atom('such as').
lex_stop_word_atom('suchlike').
lex_stop_word_atom('suppose').
lex_stop_word_atom('supposing').
lex_stop_word_atom('than').
lex_stop_word_atom('that').
lex_stop_word_atom('the').
lex_stop_word_atom('therefore').
lex_stop_word_atom('these').
lex_stop_word_atom('this').
lex_stop_word_atom('those').
lex_stop_word_atom('though').
lex_stop_word_atom('through').
lex_stop_word_atom('throughout').
lex_stop_word_atom('thru').
lex_stop_word_atom('thy').
lex_stop_word_atom('til').
lex_stop_word_atom('till').
lex_stop_word_atom('to').
lex_stop_word_atom('to within').
lex_stop_word_atom('together with').
lex_stop_word_atom('toward').
lex_stop_word_atom('towards').
lex_stop_word_atom('unbeknown to').
lex_stop_word_atom('unbeknownst to').
lex_stop_word_atom('under').
lex_stop_word_atom('underneath').
lex_stop_word_atom('unless').
lex_stop_word_atom('unlike').
lex_stop_word_atom('until').
lex_stop_word_atom('unto').
lex_stop_word_atom('up').
lex_stop_word_atom('up to').
lex_stop_word_atom('upon').
lex_stop_word_atom('upside').
lex_stop_word_atom('upstream from').
lex_stop_word_atom('upstream of').
lex_stop_word_atom('upto').
lex_stop_word_atom('v.').
lex_stop_word_atom('versus').
lex_stop_word_atom('via').
lex_stop_word_atom('vis-a-vis').
lex_stop_word_atom('vs').
lex_stop_word_atom('vs.').
lex_stop_word_atom('w').
lex_stop_word_atom('w/o').
lex_stop_word_atom('wanting').
lex_stop_word_atom('was').
lex_stop_word_atom('wasn''''t').
lex_stop_word_atom('were').
lex_stop_word_atom('weren''''t').
lex_stop_word_atom('what').
lex_stop_word_atom('what with').
lex_stop_word_atom('whatever').
lex_stop_word_atom('when').
lex_stop_word_atom('whenever').
lex_stop_word_atom('where').
lex_stop_word_atom('whereafter').
lex_stop_word_atom('whereas').
lex_stop_word_atom('whereat').
lex_stop_word_atom('whereby').
lex_stop_word_atom('wherefore').
lex_stop_word_atom('wherein').
lex_stop_word_atom('whereof').
lex_stop_word_atom('whereupon').
lex_stop_word_atom('wherever').
lex_stop_word_atom('whether').
lex_stop_word_atom('which').
lex_stop_word_atom('whichever').
lex_stop_word_atom('while').
lex_stop_word_atom('whilst').
lex_stop_word_atom('with').
lex_stop_word_atom('with reference to').
lex_stop_word_atom('with regard to').
lex_stop_word_atom('with repect to').
lex_stop_word_atom('with respect to').
lex_stop_word_atom('within').
lex_stop_word_atom('without').
% lex_stop_word_atom('worth').
lex_stop_word_atom('yet').

lex_stop_word(String) :-
	ensure_atom(String, Atom),
	lex_stop_word_atom(Atom).


/* Note: right_parenthetical/1 has been removed to nls_strings_obs.pl. */


/* replace_all_substrings(+String, +OldSubString, +NewSubString, -NewString)
replace_all_substrings/4 replaces all occurrences of OldSubString to
NewSubString in String producing NewString. */

replace_all_substrings(String, OldSubString, NewSubString, NewString) :-
	split_string(String, OldSubString, Left, Right),
	replace_all_substrings(Right, OldSubString, NewSubString, NewRight),
	split_string(NewString, NewSubString, Left, NewRight),
	!.
replace_all_substrings(String, _, _, String).

/* replace_nonprints_in_strings/2(+Strings, -ModifiedStrings)
   replace_nonprints/2(+String, -ModifiedString)

replace_nonprints_in_strings/2 uses replace_nonprints/2 to replace each
nonprint character with a space in each String.  */

replace_nonprints_in_strings([], []).
replace_nonprints_in_strings([First|Rest], [ModifiedFirst|ModifiedRest]) :-
	replace_nonprints(First, ModifiedFirst),
	replace_nonprints_in_strings(Rest, ModifiedRest).

replace_nonprints([], []).
replace_nonprints([Char|Rest], [ModifiedChar|ModifiedRest]) :-
	( Char < 127,
	  local_print(Char) ->
	  ModifiedChar = Char
	; ModifiedChar = 32
	),
	replace_nonprints(Rest, ModifiedRest).

/* replace_tabs_in_strings/2(+Strings, -ModifiedStrings)
   replace_tabs/2(+String, -ModifiedString)

replace_tabs_in_strings/2 uses replace_tabs/2 to replace each tab character
with a space in each String.  */

replace_tabs_in_strings([], []).
replace_tabs_in_strings([First|Rest], [ModifiedFirst|ModifiedRest]) :-
	substitute(9, First, 32, ModifiedFirst),
	replace_tabs_in_strings(Rest, ModifiedRest).

/* split_string(?String, +Substring, ?Left, ?Right)
split_string/4 embodies the property that String is the concatenation of
Left, Substring and Right in that order.  Substring (and hence String) must be
non-null.  Backtracking is not allowed. */

split_string(String, SubString, Left, Right) :-
	SubString = [_|_],
	append(SubString, Right, S1),
	append(Left, S1, String),
	!.

split_atom_completely(Atom, SplitAtom, AtomList) :-
	( Atom == [] ->
	  AtomList = []
	; atom_codes(Atom, String),
	  atom_codes(SplitAtom, SplitString),
	  split_string_completely(String, SplitString, StringList),
	  atom_codes_list(AtomList, StringList)
	).

/* split_string_completely(+String, +Subtring, -StringList)
split_string_completely/ breaks String at each occurrence of Substring
forming StringList.  */

split_string_completely(String,Substring,[Left|SplitRight]) :-
    split_string(String,Substring,Left,Right),
    !,
    split_string_completely(Right,Substring,SplitRight).
split_string_completely(String,_Substring,[String]).

/* split_string_backtrack(?String, +Substring, ?Left, ?Right)
split_string/4 embodies the property that String is the concatenation of
Left, Substring and Right in that order.  Substring (and hence String) must be
non-null.  Backtracking is allowed. */

split_string_backtrack(String, SubString, Left, Right) :-
	\+ SubString = [],
	append(SubString, Right, S1),
	append(Left, S1, String).

form_one_string(Lines, InterLeaveString, InputString) :-
	concat_strings_with_separator(Lines, InterLeaveString, InputString).

/* put_print_string(+String)
   put_print_string_aux(+String)

put_print_string/1 uses put/1 to print String in double-quoted format, i.e.,
surrounded by double quotes and doubling internal double quotes.  */

put_print_string(String) :-
	put_code(0'"), %" this is just to fake out Emacs's colorization!!
	put_print_string_aux(String),
	put_code(0'"). %" this is just to fake out Emacs's colorization!!

put_print_string_aux([]).
put_print_string_aux([0'"|Rest]) :- %" this is just to fake out Emacs's colorization!!
	!,
	put_code(0'"),    %" this is just to fake out Emacs's colorization!!
	put_code(0'"),    %" this is just to fake out Emacs's colorization!!
	put_print_string_aux(Rest).
put_print_string_aux([Char|Rest]) :-
	put_code(Char),
	put_print_string_aux(Rest).

/* trim_whitespace([+WhichEnd,] +String, -TrimmedString)

trim_whitespace_left/right/both trims blanks from one or both ends of String depending on the
value of WhichEnd (left, right, left_and_right, or all).  */

trim_whitespace(String, TrimmedString) :-
	trim_whitespace_both(String, TrimmedString).

trim_whitespace_left_count(String, TrimmedString, NumBlanksTrimmed) :-
	trim_whitespace_left_1(String, 0, TrimmedString, NumBlanksTrimmed).

trim_whitespace_left(String, TrimmedString) :-
	trim_whitespace_left_1(String, 0, TrimmedString, _NumBlanksTrimmed).

trim_whitespace_left_1([FirstChar|RestString], TempNumBlanksTrimmed,
		       TrimmedString, NumBlanksTrimmed) :-
	local_ws(FirstChar),
	!,
	NextNumBlanksTrimmed is TempNumBlanksTrimmed + 1,
	trim_whitespace_left_1(RestString, NextNumBlanksTrimmed, TrimmedString, NumBlanksTrimmed).
trim_whitespace_left_1(String, NumBlanksTrimmed, String, NumBlanksTrimmed) :-
	!.

trim_whitespace_right(String, TrimmedString) :-
	trim_whitespace_right_count(String, TrimmedString, _NumBlanksTrimmed).

trim_whitespace_right_count(String, TrimmedString, NumBlanksTrimmed) :-
	rev(String, [FirstChar|RevString]),
	local_ws(FirstChar),
	!,
	trim_whitespace_left_1(RevString, 0, TrimmedRevString, NumBlanksTrimmed),
	rev(TrimmedRevString, TrimmedString).
trim_whitespace_right_count(String, String, 0) :- !.

trim_whitespace_both(String, TrimmedString) :-
	trim_whitespace_both_count(String, TrimmedString,
				   _NumLeftBlanksTrimmed, _NumRightBlanksTrimmed).
	
trim_whitespace_both_count(String, TrimmedString, NumLeftBlanksTrimmed, NumRightBlanksTrimmed) :-
	trim_whitespace_left_1(String, 0, String0, NumLeftBlanksTrimmed),
	trim_whitespace_right_count(String0, TrimmedString, NumRightBlanksTrimmed),
	!.

trim_and_compress_whitespace([], []).
trim_and_compress_whitespace([H|T], Compressed) :-
	trim_whitespace_both([H|T], Trimmed),
	( Trimmed == [] ->
	  Compressed = []
	; Trimmed = [TrimmedH|TrimmedT],
	  trim_and_compress_whitespace_1(TrimmedT, TrimmedH, Compressed)
	).

trim_and_compress_whitespace_1([], H, [H]).
trim_and_compress_whitespace_1([Next|Rest], First, Trimmed) :-
	( local_ws(First),
	  local_ws(Next) ->
	  Trimmed = RestTrimmed
	; Trimmed = [First|RestTrimmed]
	),
	trim_and_compress_whitespace_1(Rest, Next, RestTrimmed).

% safe_number_codes(?Number, +Codes)
% The SICStus version of number_codes/2 and number_chars/2 raises an error
% if the first arg is uninstantiated and the second arg is not a valid list of number codes.
safe_number_codes(Number, Codes) :-
	on_exception(_, number_codes(Number,Codes), fail).

