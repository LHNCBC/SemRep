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

% File:	    metamap_candidates.pl
% Module:   MetaMap
% Author:   Lan
% Purpose:  MetaMap candidate search capabilities

:- module(metamap_candidates, [
	add_candidates/9
    ]).

:- use_module(metamap(metamap_evaluation), [
	word_is_last_word_of_some_variant/2
    ]).

:- use_module(metamap(metamap_stop_phrase), [
	stop_phrase/2
    ]).

:- use_module(metamap(metamap_tokenization), [
	tokenize_text/2,
	tokenize_text_mm/2
    ]).

:- use_module(metamap(metamap_utilities), [
	wl/1
    ]).

:- use_module(skr_db(db_access), [
	db_get_mwi_word_count/3
    ]).

:- use_module(skr_lib(nls_strings), [
	lex_stop_word/1,
	lex_stop_word_atom/1
    ]).

:- use_module(skr_lib(nls_system), [
	control_option/1
    ]).

:- use_module(skr(skr_utilities), [
	debug_message/3,
	ensure_number/2
    ]).

:- use_module(skr_lib(sicstus_utils), [
	concat_atom/2,
	string_size/2
    ]).

:- use_module(skr_lib(word_index), [
	get_filtered_uscs_for_word/7
    ]).

:- use_module(library(avl), [
	avl_fetch/3,
	avl_store/4
    ]).

:- use_module(library(sets), [
	intersection/3,
	list_to_set/2
    ]).


/* ************************************************************************
   ************************************************************************
   ************************************************************************
                       MetaMap Candidate Search Predicates
   ************************************************************************
   ************************************************************************
   ************************************************************************ */


/* add_candidates(?GVCs, +CandidateCount, +AllVariants, +IgnoreSingleChars, +DebugFlags,
   		  +WordDataCacheIn, +USCCacheIn,
		  -WordDataCacheOut, -USCCacheOut)

add_candidates/4 instantiates the Cs argument of each GVC in GVCs.
Cs is a list of elements of the form
    usc(<canonical textwords>,<string text>,<concept text>)
AllVariants is an AVL tree used for determining which index to use in some cases.
WordDataCacheIn and WordDataCacheOut are used to cache results to avoid recomputation.
USCCacheIn and USCCacheOut are also used to cache results to avoid recomputation. */

add_candidates([], _CandidateCount, _AllVariants, _IgnoreSingleChars, _DebugFlags,
	       WordDataCache, USCCache, WordDataCache, USCCache).
add_candidates([gvc(Generator,Variants,Candidates)|RestGVCs],
	       CandidateCount, AllVariants, IgnoreSingleChars, DebugFlags,
	       WordDataCacheIn, USCCacheIn,
	       WordDataCacheOut, USCCacheOut) :-
	% If IgnoreSingleChars == 1,
	% then don't generate candidates for single-character tokens
	Generator = v(Token,_,LexCats,_,_Roots0,_WordCount),
%	format('GENERATOR|~w~n', [Generator]),
%	( LexCats = [verb] ->
%	  Candidates = [],
%	  WordDataCacheNext = WordDataCacheIn,
%	  USCCacheNext = USCCacheIn
%	; IgnoreSingleChars =:= 1,
	( IgnoreSingleChars =:= 1,
	  atom_codes(Token, [_SingleChar]) ->
	  Candidates = [],
	  WordDataCacheNext = WordDataCacheIn,
	  USCCacheNext = USCCacheIn
	; control_option(allow_overmatches),
	  stop_phrase(Token, StopPhraseLexCats),
	  intersection(LexCats, StopPhraseLexCats, Intersection),
	  Intersection \== [] ->
	  Candidates = [],
	  WordDataCacheNext = WordDataCacheIn,
	  USCCacheNext = USCCacheIn
	; control_option(allow_overmatches),
	  lex_stop_word_atom(Token)->
	  Candidates = [],
	  WordDataCacheNext = WordDataCacheIn,
	  USCCacheNext = USCCacheIn
	; % format(user_output, 'G:~q~n', [Generator]),
	  % format(user_output, 'V:~q~n', [Variants]),
	  % format(user_output, 'C:~q~n~n', [Candidates]),
	  extract_simple_variants(Variants, CandidateCount, SimpleVariants),
	  % append(SimpleVariants, Roots, VariantsWithRoots),
	  debug_message(trace, '~n### Generating candidates for ~q~n', [SimpleVariants]),
	  % get_meta_candidates_for_variants(SimpleVariants, AllVariants, Candidates, DebugFlags,
	  % get_meta_candidates_for_variants(VariantsWithRoots, AllVariants, Candidates, DebugFlags,
	  get_meta_candidates_for_variants(SimpleVariants, AllVariants, Candidates, DebugFlags,
					   WordDataCacheIn, USCCacheIn,
					   WordDataCacheNext, USCCacheNext)
	),
	NextCandidateCount is CandidateCount + 1,
	add_candidates(RestGVCs, NextCandidateCount, AllVariants, IgnoreSingleChars, DebugFlags,
		       WordDataCacheNext, USCCacheNext, WordDataCacheOut, USCCacheOut).
	
/* extract_simple_variants(+Variants, -SimpleVariants)
   stop_variant(+SimpleVariant, +VarLevel)

extract_simple_variants/2
stop_variant/2
For extract_simple_variants/2, SimpleVariants is a list of variant Words.
*/

extract_simple_variants(Vs, CandidateCount, SimpleVariants) :-
	extract_simple_variants_aux(Vs, CandidateCount, SimpleVariants0),
	list_to_set(SimpleVariants0, SimpleVariants).

extract_simple_variants_aux([], _CandidateCount, []).
extract_simple_variants_aux([v(Word,_,_,_,_,NFR)|Rest], CandidateCount, Result) :-
	% If it's the first candidate, don't check for stop words
	( CandidateCount =:= 1 ->
	  Result = [Word-NFR|ExtractedRest]
	; stop_variant(Word) ->
	  Result = ExtractedRest
	; Result = [Word-NFR|ExtractedRest]
	),
	extract_simple_variants_aux(Rest, CandidateCount, ExtractedRest).

stop_variant(Word) :-
	\+ control_option(allow_large_n),
	( lex_stop_word(Word) ->
	  true
	; string_size(Word, Length),
	  Length < 3,
	  db_get_mwi_word_count(first_words_counts, Word, Count),
	  ensure_number(Count, CountNumber),
	  test_count_and_length(CountNumber, Length)
	).

test_count_and_length(Count, Length) :-
	  ( Count > 4000 ->
	    true
	  ; Count > 2000,
	    Length < 2
	  ).

/* get_meta_candidates_for_variants(+Variants, +AllVariants, -Candidates, +DebugFlags,
   				    +WordDataCacheIn,  +USCCacheIn,
				    -WordDataCacheOut, -USCCacheOut)
   get_meta_candidates_for_variants_1(+Variants, +AllVariants,
                                    +CandidatesIn, -CandidatesOut, +DebugFlags,
   				    +WordDataCacheIn,  +USCCacheIn,
				    -WordDataCacheOut, -USCCacheOut)

get_meta_candidates_for_variants/6 does word index searches of the Metathesaurus
to find strings containing one or more of Variants.  AllVariants is used to
determine which index to use in some cases. Candidates is a list of terms
     usc(<canonical textwords>,<string text>,<concept text>)
(see get_meta_uscs_using_word_index/6).  get_meta_candidates_for_variants/6
is an auxiliary predicate with an accumulator for the candidates. */

get_meta_candidates_for_variants(Variants, AllVariants, Candidates, DebugFlags,
				 WordDataCacheIn, USCCacheIn,
				 WordDataCacheOut, USCCacheOut) :-
	get_meta_candidates_for_variants_1(Variants, AllVariants, [], Candidates0, DebugFlags,
					   WordDataCacheIn, USCCacheIn,
					   WordDataCacheOut, USCCacheOut),
	sort(Candidates0, Candidates).

get_meta_candidates_for_variants_1([], _AllVariants,
				   CandidatesIn, CandidatesIn, _DebugFlags,
				   WordDataCache, USCCache,
				   WordDataCache, USCCache).
get_meta_candidates_for_variants_1([Variant-NFR|Rest], AllVariants,
				   CandidatesIn, CandidatesOut, DebugFlags,
				   WordDataCacheIn, USCCacheIn,
				   WordDataCacheOut, USCCacheOut) :-
	debug_get_meta_candidates_1(DebugFlags, Variant),
	get_meta_uscs(Variant, AllVariants, NFR, DebugFlags, NewCandidates,
		      WordDataCacheIn, USCCacheIn,
		      WordDataCacheNext, USCCacheNext),
	debug_get_meta_candidates_2(DebugFlags, NewCandidates),
	append(NewCandidates, CandidatesIn, CandidatesInOut),
	get_meta_candidates_for_variants_1(Rest, AllVariants,
					  CandidatesInOut, CandidatesOut, DebugFlags,
					  WordDataCacheNext, USCCacheNext,
					  WordDataCacheOut, USCCacheOut).


/* get_meta_uscs(+ThisVariant, +AllVariants, +NFR, +DebugFlags, -MetaUSCs,
   		 +WordDataCacheIn, +USCCacheIn,
		 -WordDataCacheOut, -USCCacheOut)

get_meta_uscs/8 returns a list MetaUSCs of terms
usc(<canonical textwords>,<string text>,<concept text>) for ThisVariant using a
Metathesaurus word index where <canonical textwords> is the lowercased,
normalized, uninverted, tokenized form of <string text> and <concept text> is
<string text>'s concept string. */

get_meta_uscs(ThisVariant, AllVariants, NFR, DebugFlags, MetaUSCs,
	      WordDataCacheIn, USCCacheIn,
	      WordDataCacheOut, USCCacheOut) :-
	tokenize_text(ThisVariant, ThisVariantWords),
	tokenize_text_mm(ThisVariant, ThisVariantMMWords),
	% format(user_output, 'TT:  ~w-->~q~n', [ThisVariant, Words]),
 	% format(user_output, 'TTM: ~w-->~q~n', [ThisVariant, MMWords]),
	get_meta_uscs_1(ThisVariantMMWords, ThisVariantWords, NFR,
			AllVariants, DebugFlags, MetaUSCs,
			WordDataCacheIn, USCCacheIn,
			WordDataCacheOut, USCCacheOut).

/* 
Multi words CAN produce output (cf. "mentally ill" -> Mental illness),
but always choose the first word of multi-word items to search a word
index.
*/

get_meta_uscs_1([], _ThisVariantWords, _NFR, _AllVariants, _DebugFlags, [],
		WordDataCache, USCCache, WordDataCache, USCCache).
get_meta_uscs_1([Word|_RestWords], ThisVariantWords, NFR, AllVariants, DebugFlags, MetaUSCs,
		WordDataCacheIn, USCCacheIn,
		WordDataCacheOut, USCCacheOut) :-
	% WTF is this for?
	length(ThisVariantWords, ThisVariantWordCount),
 	% NewNFR is max(ThisVariantWordCount, NFR),???
	NewNFR is NFR + ThisVariantWordCount - 1,
	% format(user_output, '~d ~d : ~w~n', [NFR,NewNFR,Words]),
	% NewNFR is NFR,
	get_meta_uscs_2(NewNFR, Word, AllVariants, ThisVariantWords, DebugFlags, MetaUSCs,
			WordDataCacheIn, USCCacheIn,
			WordDataCacheOut, USCCacheOut).

% The next two predicates enable doing away with the
% dynamic goal instantiation formerly implemented via
% cache_get/5/6 in cache.pl; the cache.pl file no longer exists.

determine_table(Word, AllVariants, PhraseLength, Table) :-
	( control_option(allow_overmatches) ->
	  Table = all_words
	; ( control_option(allow_concept_gaps)
	  ; control_option(ignore_word_order)
	  ) ->
	  determine_first_word_index(Word, AllVariants, Table)
	; PhraseLength =:= 1 ->
	  Table = first_words_of_one
	; PhraseLength =:= 2 ->
	  Table = first_words_of_two
	; determine_first_word_index(Word, AllVariants, Table)
	).

get_from_table(Word, FilterWords, Table, NewNFR, DebugFlags, MetaUSCs,
	       WordDataCacheIn, USCCacheIn,
	       WordDataCacheOut, USCCacheOut) :-
	debug_message(db, '~n### Looking up ~q|~q|~q from ~q~n',
		          [Word,NewNFR,FilterWords,Table]),
	make_avl_key_2(Word, Table, FilterWords, AVLKey),
	( avl_fetch(AVLKey, USCCacheIn, MetaUSCs) ->
	  debug_message(db, '~N### USC CACHE FOUND ~q|~q from ~q~n', [Word,FilterWords,Table]),
	  WordDataCacheOut = WordDataCacheIn,
	  USCCacheOut = USCCacheIn
	; get_filtered_uscs_for_word(Table, Word, DebugFlags, FilterWords,
				     WordDataCacheIn, WordDataCacheOut, MetaUSCs) ->
  	  avl_store(AVLKey, USCCacheIn, MetaUSCs, USCCacheOut)
	; MetaUSCs = [],
	  WordDataCacheOut = WordDataCacheIn,
  	  avl_store(AVLKey, USCCacheIn, MetaUSCs, USCCacheOut)
	),
	debug_message(db, '~n### DONE looking up ~q|~q|~q from ~q~n',
		          [Word,NewNFR,FilterWords,Table]).


make_avl_key_2(Word, Table, FilterWords, AVLKey) :-
	concat_atom([Word,'-',Table], Functor),
	functor(AVLKey, Functor, 1),
	arg(1, AVLKey, FilterWords).

/*
   NFR == "Number From Right", the position of Word in the phrase counting from the right
   get_meta_uscs_2(+NewNFR, +Word, +AllVariants,
                   +FilterWords, +DebugFlags, -MetaUSCs,
		   +WordDataCacheIn, +USCCacheIn,
		   -WordDataCacheOut, -USCCacheOut)

*/

get_meta_uscs_2(1, Word, _AllVariants, FilterWords, DebugFlags, MetaUSCs,
		WordDataCacheIn, USCCacheIn,
		WordDataCacheOut, USCCacheOut) :-
	% special case for the last word of a phrase being processed
	% canonically allowing concept gaps but not overmatches;
	% in addition, word order must be respected
	\+ control_option(allow_overmatches),
	\+ control_option(ignore_word_order),
	!,
	get_from_table(Word, FilterWords, first_words_of_one, 1, DebugFlags, MetaUSCs,
		       WordDataCacheIn, USCCacheIn,
		       WordDataCacheOut, USCCacheOut),
	debug_get_uscs(DebugFlags, WordDataCacheIn, WordDataCacheOut).
get_meta_uscs_2(NewNFR, Word, AllVariants, FilterWords,
		DebugFlags, MetaUSCs,
		WordDataCacheIn, USCCacheIn,
		WordDataCacheOut, USCCacheOut) :-
	determine_table(Word, AllVariants, NewNFR, Table),
	% format(user_output, '~q (~d): ~w~n', [Word,NewNFR,Table]),
	get_from_table(Word, FilterWords, Table, NewNFR, DebugFlags, MetaUSCs,
		       WordDataCacheIn, USCCacheIn,
		       WordDataCacheOut, USCCacheOut),
	debug_get_uscs(DebugFlags, WordDataCacheIn, WordDataCacheOut).

/* determine_first_word_index(+Word, +AllVariants, -FirstWordIndex)
   frequent_first_word_pair(?Word1, ?Word2)

determine_first_word_index/3 determines which of the first_word indexes
(first_words or first_wordsb) should be used in retrieving candidates for Word.
The first_wordsb index is a subindex of first_words in which frequently occurring
(normalized) strings of the form <word1> ... <word2> are removed.
(See frequent_first_word_pair/2 for the definition of such strings)
This technique speeds up processing for the cases which do not involve
one of the frequent strings. */

determine_first_word_index(Word, AllVariants, Index) :-
	( frequent_first_word_pair(Word, Word2),
	  word_is_last_word_of_some_variant(Word2, AllVariants) ->
	  Index = first_words
	; Index = first_wordsb
	).


%dump_uscs(_,_,[]) :-
%    !.
%dump_uscs(Text,Word,MetaUSCs) :-
%    format('USCs for ~p, ~p:~n',[Text,Word]),
%    wl(MetaUSCs),
%    format('end USCs~n',[]).

debug_get_meta_candidates_1(DebugFlags, Variant) :-
	( memberchk(2, DebugFlags) ->
	  format(user_error, '~nVariant: ~p~n',[Variant])
	; true
	).

debug_get_meta_candidates_2(DebugFlags, NewCandidates) :-
	( memberchk(2, DebugFlags) ->
	  length(NewCandidates, NewCandidatesLength),
	  format(user_error, '~d candidates.~n', [NewCandidatesLength]),
	  wl(NewCandidates)
	; true
	).


debug_get_uscs(DebugFlags, WordDataCacheIn, WordDataCacheOut) :-
	( memberchk(2, DebugFlags),
	  WordDataCacheIn = WordDataCacheOut ->
	  format(user_error, '(by cache)~n', [])
	; true
	).


% :- use_module(skr_lib(addportray)).
% portray_node(Node) :-
% 	Node = node(_,_,_,_,_),
% 	writeq('NODE').
% :- add_portray(portray_node).
frequent_first_word_pair('2',                   'acid').
frequent_first_word_pair('3',                   'acid').
frequent_first_word_pair('4',                   'acid').
frequent_first_word_pair('accidental',          'poisoning').
frequent_first_word_pair('acetaminophen',       'tablet').
frequent_first_word_pair('activation',          'activity').
frequent_first_word_pair('anterior',            'nerve').
frequent_first_word_pair('anti',                'antibody').
frequent_first_word_pair('arabidopsis',         'protein').
frequent_first_word_pair('articular',           'vertebra').
frequent_first_word_pair('bacillus',            'protein').
frequent_first_word_pair('bacteria',            'protein').
frequent_first_word_pair('bony',                'bone').
frequent_first_word_pair('bony',                'vertebra').
frequent_first_word_pair('c',                   'protein').
frequent_first_word_pair('compact',             'bone').
frequent_first_word_pair('compact',             'vertebra').
frequent_first_word_pair('ctcae',               'injury').
frequent_first_word_pair('decreased',           '[pe]').
frequent_first_word_pair('down',                'activity').
frequent_first_word_pair('downregulation',      'activity').
frequent_first_word_pair('drosophila',          'protein').
frequent_first_word_pair('e',                   'protein').
frequent_first_word_pair('entire',              'artery').
frequent_first_word_pair('hemoglobin',          'phenotype').
frequent_first_word_pair('hla',                 'antigen').
frequent_first_word_pair('human',               '1').
frequent_first_word_pair('human',               '2').
frequent_first_word_pair('human',               'antigen').
frequent_first_word_pair('human',               'microrna').
frequent_first_word_pair('human',               'protein').
frequent_first_word_pair('increased',           '[pe]').
frequent_first_word_pair('inhibition',          'activity').
frequent_first_word_pair('intentional',         'poisoning').
frequent_first_word_pair('lateral',             'nerve').
frequent_first_word_pair('left',                'trunk').
frequent_first_word_pair('malignant',           'tumor').
frequent_first_word_pair('member',              'family').
frequent_first_word_pair('mouse',               '1').
frequent_first_word_pair('mouse',               'protein').
frequent_first_word_pair('negative',            'activity').
frequent_first_word_pair('negative',            'process').
frequent_first_word_pair('periosteum',          'bone').
frequent_first_word_pair('periosteum',          'vertebra').
frequent_first_word_pair('positive',            'activity').
frequent_first_word_pair('positive',            'process').
frequent_first_word_pair('posterior',           'nerve').
frequent_first_word_pair('rat',                 'protein').
frequent_first_word_pair('regulation',          'activity').
frequent_first_word_pair('regulation',          'process').
frequent_first_word_pair('right',               'artery').
frequent_first_word_pair('right',               'trunk').
frequent_first_word_pair('rnu6',                'gene').
frequent_first_word_pair('s',                   'protein').
frequent_first_word_pair('stage',               'cancer').
frequent_first_word_pair('stage',               'carcinoma').
frequent_first_word_pair('stage',               'lymphoma').
frequent_first_word_pair('stage',               'v7').
frequent_first_word_pair('structure',           'artery').
frequent_first_word_pair('surface',             'bone').
frequent_first_word_pair('surface',             'vertebra').
frequent_first_word_pair('synovial',            'joint').
frequent_first_word_pair('trabecular',          'bone').
frequent_first_word_pair('trabecular',          'vertebra').
frequent_first_word_pair('trunk',               'artery').
frequent_first_word_pair('trunk',               'nerve').
frequent_first_word_pair('trunk',               'vein').
frequent_first_word_pair('up',                  'activity').
frequent_first_word_pair('upregulation',        'activity').
frequent_first_word_pair('xenopus',             'protein').
frequent_first_word_pair('zebrafish',           'protein').
