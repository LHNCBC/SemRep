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

% File:     skr.pl
% Module:   SKR
% Author:   Lan
% Purpose:  Provide access to all SKR processing: MetaMap, MMI and SemRep


:- module(skr, [
	aev_print_version/2,
	compute_confidence_value/5,
	extract_phrases_from_aps/2,
	get_inputmatch_atoms_from_phrase/2,
	get_phrase_tokens/4,
	% called by MetaMap API -- do not change signature!
	initialize_skr/1,
	% print_candidate_grid/7 and print_duplicate_info/5 are not
	% explicitly called by any other module,
	% but they must still be exported because they're called via debug_call.
	print_candidate_grid/6,
	print_duplicate_info/4,
	skr_phrases/22,
	print_all_aevs/1,
	stop_and_halt/0,
	% called by MetaMap API -- do not change signature!
	stop_skr/0
    ]).

% :- use_module(lexicon(lex_access), [
% 	initialize_lexicon/2
%     ]).

:- use_module(metamap(metamap_candidates), [
	add_candidates/9
    ]).

:- use_module(metamap(metamap_evaluation), [
	% consolidate_matchmap/3,
	evaluate_all_GVCs/16,
	extract_components/3,
	component_intersects_components/2,
	compute_extra_meta/3,
	compute_match_value/8,
	connect_components/2,
	matching_token/3,
	merge_contiguous_components/2
    ]).

:- use_module(metamap(metamap_parsing), [
	collapse_syntactic_analysis/2,
	demote_heads/2,
	re_attach_apostrophe_s_to_prev_word/3
    ]).

:- use_module(metamap(metamap_stop_phrase), [
	stop_phrase/2
    ]).

:- use_module(metamap(metamap_tokenization), [
	add_tokens_to_phrases/2,
	extract_tokens_with_tags/2,
	get_phrase_item_feature/3,
	get_phrase_item_name/2,
	get_phrase_item_subitems/2,
	get_subitems_feature/3,
	linearize_components/2,
	linearize_phrase/4,
	local_alpha/1,
	local_digit/1,
	local_ws/1,
	new_phrase_item/3,
	parse_phrase_word_info/3,
	set_subitems_feature/4
    ]).

:- use_module(metamap(metamap_utilities), [
	candidate_term/16,
	extract_relevant_sources/3,
	extract_nonexcluded_sources/3,
	extract_name_in_source/2,
	wgvcs/1,
	wl/1,
	write_avl_list/1
    ]).

:- use_module(metamap(metamap_variants), [
	initialize_metamap_variants/1,
	compute_variant_generators/3,
	augment_GVCs_with_variants/1,
	gather_variants/4
    ]).

:- use_module(skr(skr_fe), [
	get_output_stream/1
    ]).

:- use_module(skr(skr_json), [
	json_output_format/1
   ]).

:- use_module(skr(skr_umls_info), [
	convert_to_root_sources/2,
	sab_tables_exist/0
    ]).

:- use_module(skr(skr_utilities), [
	debug_call/2,
	debug_message/3,
	ensure_number/2,
	expand_split_word_list/2,
	fatal_error/2,
	generate_EOT_output/1,
	get_candidate_feature/3,
	get_all_candidate_features/3,
	memberchk_var/2,
	replace_crs_with_blanks/4,
        send_message/2,
	split_word/3,
	token_template/5,
	token_template/6
    ]).

:- use_module(skr(skr_xml), [
	xml_output_format/1
    ]).

:- use_module(skr_db(db_access), [
	initialize_db_access/0,
	stop_db_access/0,
	% db_get_concept_sts/2,
	% db_get_cui_sts/2,
	db_get_cui_sourceinfo/2
	% db_get_string_sources/2
    ]).

% :- use_module(skr_lib(ctypes), [
% 	is_alpha/1
%     ]).

% :- use_module(skr_lib(efficiency), [
% 	maybe_atom_gc/3
%     ]).

:- use_module(skr_lib(negex), [
	compute_negex/5,
	final_negation_template/6
   ]).

:- use_module(skr_lib(dysonyms), [
	exclude_dysonyms/2
   ]).

:- use_module(skr_lib(nls_strings), [
	atom_codes_list/2,
	split_string_completely/3,
	trim_and_compress_whitespace/2
   ]).

:- use_module(skr_lib(nls_system), [
	add_to_control_options/1,
	control_option/1,
	control_value/2,
	set_control_options/1,
	subtract_from_control_options/1
    ]).

:- use_module(skr_lib(sicstus_utils), [
	concat_atom/3,
	interleave_string/3,
	lower/2,
	midstring/6,
	subchars/4,
	ttyflush/0,
	upper_list/2
    ]).

:- use_module(text(text_objects), [
	extract_token_atoms/2
    ]).

:- use_module(text(text_object_util), [
	an_tok/1,
	brackets/2,
	brackets/2,
	higher_order_or_annotation_tok/1,
	label_tok/1,
	pe_tok/1,
	pn_tok/1,
	ws_or_pn_tok/1,
	ws_tok/1
    ]).

:- use_module(wsd(wsdmod), [
	do_WSD/9
    ]).

:- use_module(library(avl), [
	avl_member/3,
	avl_to_list/2,
	empty_avl/1
    ]).

:- use_module(library(file_systems), [
	close_all_streams/0
    ]).

:- use_module(library(between), [
	between/3
    ]).

:- use_module(library(lists), [
	append/2,
	delete/3,
	keys_and_values/3,
	last/2,
	last/3,
	rev/2,
	select/3,
	sumlist/2
    ]).

:- use_module(library(lists3), [
	remove_duplicates/2
    ]).


:- use_module(library(sets), [
	del_element/3,
	intersection/3,
	subset/2,
	subtract/3,
	union/4
    ]).

/* 
   initialize_skr(+Options)
   stop_skr

initialize_skr/0 calls initialize_skr/1 to initialize modules that
it uses and to set Options.  (For skr_fe, Options is [] since it has
already set the options it uses.)
stop_skr/0 stops access to other modules and closes all streams.  */

initialize_skr(Options) :-
	set_control_options(Options),
	initialize_db_access,
	% initialize_lexicon(_, _),
	( control_option(dynamic_variant_generation) ->
	  initialize_metamap_variants(dynamic)
	; initialize_metamap_variants(static)
	),
	warn_if_no_sab_files,
	!.
initialize_skr(_) :-
	fatal_error('initialize_skr/1 failed.~n', []).

stop_and_halt :- stop_skr, halt.

stop_skr :-
	( stop_db_access ->
	  true
	; true
	),
	get_output_stream(OutputStream),
	generate_EOT_output(OutputStream),
	close_all_streams.

warn_if_no_sab_files :-
	% If the sab tables (sab_rv and sab_vr) exist, do not issue a warning.
	( sab_tables_exist ->
	  true
	% Otherwise, if the user has
	% * not specified silent mode, and
	% * has specified specified exclude_sources or restrict_to_sources,
	% then issue a warning.
	; \+ control_option(silent),
	  ( control_option(exclude_sources)
	  ; control_option(restrict_to_sources)
	  ) ->
	  send_message('### WARNING: Validation of UMLS sources specified on command line disabled!~n', [])
	% If neither of the above circumstances hold, then issue no warning.
	; true
	).

conditionally_collapse_syntactic_analysis(SyntAnalysis0, SyntAnalysis) :-
	( control_option(term_processing) ->
	  collapse_syntactic_analysis(SyntAnalysis0, SyntAnalysis)
	; SyntAnalysis = SyntAnalysis0
	).

skr_phrases(InputLabel, UtteranceText, OrigCitationTextAtom, CitationTextAtom,
	    AAs, UDAs, NoMapPairs, NoVarPairs, SyntAnalysis0, SyntAnalysis, TagList,
	    WordDataCacheIn, USCCacheIn, RawTokensIn,
	    ServerStreams, RawTokensOut, WordDataCacheOut, USCCacheOut,
	    % MMOPhrases, NegationTerms, ExtractedPhrases, SemRepPhrasesOut) :-
	    MMOPhrases, NegationTerms, ExtractedPhrases, SemRepPhrasesOut) :-
	conditionally_collapse_syntactic_analysis(SyntAnalysis0, SyntAnalysis),
	( skr_phrases_aux(InputLabel, UtteranceText, OrigCitationTextAtom, CitationTextAtom,
			  AAs, UDAs, NoMapPairs, NoVarPairs, SyntAnalysis, TagList,
			  WordDataCacheIn, USCCacheIn, RawTokensIn,
			  ServerStreams, RawTokensNext, WordDataCacheOut, USCCacheOut,
			  MMOPhrases, ExtractedPhrases, NegationTerms, SemRepPhrasesOut) ->
	  % clean up token list by removing un-consumed tokens
	  remove_tokens_until_label_token(RawTokensNext, RawTokensOut)
	  % skr_utilities:write_token_list(RawTokensOut, 0, 1)
        ; fatal_error('skr_phrases/14 failed for text ~p: ~p~n',
		      [InputLabel,UtteranceText])
        ).

skr_phrases_aux(InputLabel, UtteranceText, OrigCitationTextAtom, CitationTextAtom,
		AAs, UDAs, NoMapPairs, NoVarPairs, SyntacticAnalysis, TagList,
		WordDataCacheIn, USCCacheIn, RawTokensIn,
		ServerStreams, RawTokensOut, WordDataCacheOut, USCCacheOut,
		DisambiguatedMMOPhrases, ExtractedPhrases, NegationTerms,
		minimal_syntax(SemRepPhrasesWithWSD)) :-
	ServerStreams = _TaggerServerStream-WSDServerStream,
	SyntacticAnalysis = minimal_syntax(Syntax0),
	add_tokens_to_phrases(Syntax0, Syntax),
	% UtteranceText contains no AAs. E.g.,
	% "heart attack (HA)" will become simply "heart attack".
	skr_phrases_1(Syntax, InputLabel, UtteranceText,
		      AAs, UDAs, NoMapPairs, NoVarPairs,
		      OrigCitationTextAtom,
		      TagList,
		      WordDataCacheIn, USCCacheIn,
		      WordDataCacheOut, USCCacheOut,
		      RawTokensIn, RawTokensOut,
		      MMOPhrases, ExtractedPhrases),
	% current_output(OutputStream),
	% format(user_output, '~n### Current output before WSD is ~q', [OutputStream]),
	compute_negex(RawTokensIn, UtteranceText, MMOPhrases, NegationTerms, MMOPhrases1),
	do_WSD(UtteranceText, InputLabel, CitationTextAtom, AAs, RawTokensIn,
	       WSDServerStream, MMOPhrases1, DisambiguatedMMOPhrases, SemRepPhrasesWithWSD).
	% current_output(OutputStream),
	% format(user_output, '~n### Current output after WSD is ~q', [OutputStream]).

% sco(Tag) :-
% 	(control_option(O), format(user_output, '~wOPTION:~w~n', [Tag,O]), nl, fail; true),
% 	nl(user_output).

skr_phrases_1([], _InputLabel,
	      _AllUtteranceText, _AAs, _UDAs, _NoMapPairs, _NoVarPairs,
	      _OrigCitationTextAtom, _TagList,
	      WordDataCache, USCCache, WordDataCache, USCCache,
	      RawTokens, RawTokens, [], []).
skr_phrases_1([FirstSyntaxIn|RestSyntaxIn0], InputLabel, AllUtteranceText,
	      AAs, UDAs, NoMapPairs, NoVarPairs, OrigCitationTextAtom,
	      TagList,
	      WordDataCacheIn, USCCacheIn,
	      WordDataCacheOut, USCCacheOut,
	      RawTokensIn, RawTokensOut,
	      [FirstMMOPhrase|RestMMOPhrases],
	      [FirstPhraseOut|RestPhrasesOut]) :-
	% merge conjuncts beginning with FirstPhraseIn
	get_label_components(InputLabel, PMID, _TiOrAB, _UtteranceNum),
	merge_conjuncts(FirstSyntaxIn, RestSyntaxIn0, PMID,
			ConjoinedSyntax0, RestSyntaxIn1),
	% TotalPhraseCount is total the number of phrases glommed together
	% to form the composite phrase.
	% E.g., TotalPhraseCount for [ [pain] [on the left side] [of the chest] ] is 3
	% get_composite_phrases is intentionally non-deterministic
	% in order to allow undoing the phrase-gluing, in case the result is too long!
	get_composite_phrases([ConjoinedSyntax0|RestSyntaxIn1], CompositeSyntax,
			      TotalSyntaxCount, RestSyntaxIn2, CompositeOptions),
	% merge conjuncts beginning with CompositeSyntax
	merge_conjuncts(CompositeSyntax, RestSyntaxIn2, PMID,
			ConjoinedSyntax1, RestSyntaxIn3),
	maybe_print_inputmatches(ConjoinedSyntax1, PMID),
	% Merge consecutive phrases spanned by an AA
	% The original composite phrases are the list
	% [CompositeSyntax|NewRestSyntax0];
	% after merging phrases that are spanned by an AA, the phrases are
	% [MergedSyntax|RestMergedSyntax]
	merge_aa_phrases(RestSyntaxIn3, ConjoinedSyntax1, AAs, UDAs,
			 MergedSyntax, RestSyntaxIn4, MergeOptions),
	% merge_conjuncts(MergedSyntax, NewRestSyntax1, PMID,
	% 		  ConjoinedSyntax, NewRestSyntax2),
	get_inputmatch_atoms_from_phrase(MergedSyntax, InputMatchConjoinedSyntaxWords),
	length(InputMatchConjoinedSyntaxWords, Length),
	( TotalSyntaxCount > 0 ->
	  Length < 21
	; true
	),	    
	!,
	append(CompositeOptions, MergeOptions, AllAddedOptions0),
	sort(AllAddedOptions0, AllAddedOptions),
	add_to_control_options(AllAddedOptions),
	% AllUtteranceText is never used in skr_phrase; it's there only for gap analysis,
	% which is no longer used!!
	% format(user_output, 'Phrase ~w:~n', [ConjoinedSyntax]),
	skr_phrase(InputLabel, AllUtteranceText,
		   MergedSyntax, AAs, NoMapPairs, NoVarPairs,
		   OrigCitationTextAtom, TagList,
		   RawTokensIn, GVCs,
		   WordDataCacheIn, USCCacheIn,
		   WordDataCacheNext, USCCacheNext,
		   RawTokensNext, APhrases, FirstMMOPhrase),
	% maybe_atom_gc(1, _DidGC, _SpaceCollected),
	% format(user_output, 'Tokens Next:~n', []),
	% skr_utilities:write_token_list(RawTokensNext, 0, 1),
	set_var_GVCs_to_null(GVCs),
	extract_phrases_from_aps(APhrases, FirstPhraseOut),
	subtract_from_control_options(AllAddedOptions),
	% add_semtypes_to_phrases_if_necessary(Phrases0,FirstEPPhrases),
	skr_phrases_1(RestSyntaxIn4, InputLabel, AllUtteranceText,
		      AAs, UDAs, NoMapPairs, NoVarPairs, OrigCitationTextAtom,
		      TagList,
		      WordDataCacheNext, USCCacheNext, WordDataCacheOut, USCCacheOut,
		      RawTokensNext, RawTokensOut, RestMMOPhrases, RestPhrasesOut).

% Either
% (1) The last element of MergedPhrase is a head(_) or mod(_),
% or
% (2a) The last element of MergedPhrase is a punc(_),
% AND
% (2a) The next-to-last element of MergedPhrase is a head(_) or mod(_).

ends_with_head_or_mod_or_prep(MergedPhrase) :-
	last(AllButLast, Last, MergedPhrase),
	( Last = head(_) ->
	  true
	; Last = mod(_) ->
	  true
	; Last = prep(_) ->
	  true
	; Last = punc(_) ->
	  last(AllButLast, NextToLast),
	  ( NextToLast = head(_) ->
	    true
	  ; NextToLast = mod(_)
	  )
	).

relevant_conj('and').
relevant_conj('and/or').
relevant_conj('and or').
relevant_conj('as well as').
% "instead of" is a prep, not a conj
% relevant_conj('instead of').
relevant_conj('nor').
relevant_conj('or').
relevant_conj('rather than').
relevant_conj('versus').
relevant_conj('vs').

merge_conjuncts(FirstPhrase, RestPhrasesIn, PMID, ConjoinedPhrase, RestPhrasesOut) :-
	% MetaMap must have been called with "--conj".
	( control_option(conj),
	% If the phrases left in the utterance number fewer than 3, do nothing,
	% because we need at least 3 phrases to join conjuncts, i.e.,
	% Conjunct1, Conjunction, Conjunct2.
	  length([FirstPhrase|RestPhrasesIn], AllPhrasesLength),
	  AllPhrasesLength >= 3 ->
	  merge_conjuncts_aux(FirstPhrase, RestPhrasesIn,
			      PMID, ConjoinedPhrase, RestPhrasesOut)
	; ConjoinedPhrase = FirstPhrase,
	  RestPhrasesOut = RestPhrasesIn
	).

% CLAUSE 1 of 3 for merge_conjuncts_aux/5
% This clause handles plain vanilla "lung and liver cancer" conjunction.
merge_conjuncts_aux(FirstPhrase, RestPhrasesIn, PMID, ConjoinedPhrase, RestPhrasesOut) :-
	% Get the next two phrases, NextPhrase1 and NextPhrase2
	RestPhrasesIn = [ConjunctionPhrase,Conjunct2Phrase|_],
	% ConjunctionPhrase must begin (and end?) with a conj(_) element,
	% conj([lexmatch([and]),inputmatch([and]),tag(conj),tokens([and])])
	ConjunctionPhrase = [conj([lexmatch([ConjWord])|_])|_],
	% and the conjunction lexmatch must be a specific conjunction
	relevant_conj(ConjWord),
	% The first phrase (i.e., FirstPhrase) must be =< 7 elements long,
	length(FirstPhrase, FirstPhraseLength),
	FirstPhraseLength =< 7,
	% and end with a head(_) or mod(_) element.
	% Should this restriction be relaxed?
	ends_with_head_or_mod_or_prep(FirstPhrase),
	% The phrase after the conj(_) phrase (NextPhrase2) must be < 8 elements long
	length(Conjunct2Phrase, Conjunct2PhraseLength),
	Conjunct2PhraseLength < 8,
	% merge_conjoined_phrases/2 should be nondeterminate
	% so that it can be undone if the conjoined phrase it creates is too long.
	merge_conjoined_phrases(FirstPhrase, RestPhrasesIn, PMID,
				ConjoinedPhrase, RestPhrasesOut),
	length(ConjoinedPhrase, ConjoinedPhraseLength),
	ConjoinedPhraseLength < 15,
	!.

% Should eventually be merged into previous clause
% This clause handles series coordination, e.g., "stage i, ii, and iiia breast cancer"

% FirstPhrase = 
%   [mod([lexmatch([stage]),inputmatch([stage]),tag(noun),tokens([stage])])
%    head([lexmatch([i]),inputmatch([i]),tag(noun),tokens([i])])
%    punc([inputmatch([,]),tokens([])]) ]
% RestPhrasesIn =
%   [[head([inputmatch([ii]),tag(noun),tokens([ii])]),punc([inputmatch([',']),tokens([])])],
%    [conj([lexmatch([and]),inputmatch([and]),tag(conj),tokens([and])])],
%    [mod([inputmatch([iiia]),tag(noun),tokens([iiia])]),
%     head([lexmatch(['breast cancer']),inputmatch([breast,cancer]),tag(noun),tokens([breast,cancer])])]]

% PhrasesEndingWithConj =
%   [[head([inputmatch([ii]),tag(noun),tokens([ii])]),punc([inputmatch([',']),tokens([])])],
%    [conj([lexmatch([and]),inputmatch([and]),tag(conj),tokens([and])])]]

% Rest =
%   [[mod([inputmatch([iiia]),tag(noun),tokens([iiia])]),
%     head([lexmatch(['breast cancer']),inputmatch([breast,cancer]),tag(noun),tokens([breast,cancer])])]]

% FirstRestPhrases =
%   [[mod([inputmatch([iiia]),tag(noun),tokens([iiia])]),
%     head([lexmatch(['breast cancer']),inputmatch([breast,cancer]),tag(noun),tokens([breast,cancer])])]]

% TempConjoinedPhrase =
%   [mod([lexmatch([stage]),inputmatch([stage]),tag(noun),tokens([stage])]),
%    head([lexmatch([i]),inputmatch([i]),tag(noun),tokens([i])]),
%    punc([inputmatch([',']),tokens([])]),
%    head([inputmatch([ii]),tag(noun),tokens([ii])]),
%    punc([inputmatch([',']),tokens([])]),
%    conj([lexmatch([and]),inputmatch([and]),tag(conj),tokens([and])])]

% ConjoinedPhrase0    = 
%   [mod([lexmatch([stage]),inputmatch([stage]),tag(noun),tokens([stage])]),
%    head([lexmatch([i]),inputmatch([i]),tag(noun),tokens([i])]),
%    punc([inputmatch([',']),tokens([])]),
%    head([inputmatch([ii]),tag(noun),tokens([ii])]),
%    punc([inputmatch([',']),tokens([])]),
%    conj([lexmatch([and]),inputmatch([and]),tag(conj),tokens([and])]),
%    mod([inputmatch([iiia]),tag(noun),tokens([iiia])]),
%    head([lexmatch(['breast cancer']),inputmatch([breast,cancer]),tag(noun),tokens([breast,cancer])])]

% ConjoinedPhrase1 = 
%   [mod([lexmatch([stage]),inputmatch([stage]),tag(noun),tokens([stage])]),
%    mod([lexmatch([i]),inputmatch([i]),tag(noun),tokens([i])]),
%    punc([inputmatch([',']),tokens([])]),
%    mod([inputmatch([ii]),tag(noun),tokens([ii])]),
%    punc([inputmatch([',']),tokens([])]),
%    conj([lexmatch([and]),inputmatch([and]),tag(conj),tokens([and])]),
%    mod([inputmatch([iiia]),tag(noun),tokens([iiia])]),
%    mod([lexmatch(['breast cancer']),inputmatch([breast,cancer]),tag(noun),tokens([breast,cancer])])]
%   
% ConjoinedPhrase  =
%   [mod([lexmatch([stage]),inputmatch([stage]),tag(noun),tokens([stage])]),
%    mod([lexmatch([i]),inputmatch([i]),tag(noun),tokens([i])]),
%    punc([inputmatch([',']),tokens([])]),
%    mod([inputmatch([ii]),tag(noun),tokens([ii])]),
%    punc([inputmatch([',']),tokens([])]),
%    conj([lexmatch([and]),inputmatch([and]),tag(conj),tokens([and])]),
%    mod([inputmatch([iiia]),tag(noun),tokens([iiia])]),
%    head([lexmatch(['breast cancer']),inputmatch([breast,cancer]),tag(noun),tokens([breast,cancer])])]

% CLAUSE 2 of 3 for merge_conjuncts_aux/5
merge_conjuncts_aux(FirstPhrase, RestPhrasesIn, _PMID, ConjoinedPhrase, RestPhrasesOut) :-
	contains_relevant_conj([FirstPhrase|RestPhrasesIn], PhrasesEndingWithConj, Rest),
	% ensure series coordination comma(s) appear before the conjunction
	contains_series_coord(PhrasesEndingWithConj),
	length(PhrasesEndingWithConj, PhrasesEndingWithConjLen),
	PhrasesEndingWithConjLen < 6,
	Rest = [FirstRestPhrases|RestPhrasesOut],
	append(PhrasesEndingWithConj, TempConjoinedPhrase),
	append(TempConjoinedPhrase, FirstRestPhrases, ConjoinedPhrase0),
	length(ConjoinedPhrase0, ConjoinedPhrase0Length),
	ConjoinedPhrase0Length < 15,
	% maybe_print_inputmatches(ConjoinedPhrase0, PMID),
	demote_heads(ConjoinedPhrase0, ConjoinedPhrase1),
	promote_last_mod(ConjoinedPhrase1, ConjoinedPhrase),
	!.

% CLAUSE 3 of 3 for merge_conjuncts_aux/5
merge_conjuncts_aux(FirstPhrase, RestPhrasesIn, _PMID, ConjoinedPhrase, RestPhrasesOut) :-
	ConjoinedPhrase = FirstPhrase,
	RestPhrasesOut = RestPhrasesIn.

contains_relevant_conj([H|T], PhrasesEndingWithConj, Rest) :-
	% conj([lexmatch([and]),inputmatch([and]),tag(conj),tokens([and])])
	( H = [conj([lexmatch([ConjWord])|_])],
	  relevant_conj(ConjWord) ->
	  PhrasesEndingWithConj = [H],
	  Rest = T
	; H \= [prep(_)|_],
	  PhrasesEndingWithConj = [H|RestPhrasesEndingWithConj],
	  contains_relevant_conj(T, RestPhrasesEndingWithConj, Rest)
	).

contains_series_coord(PhrasesEndingWithConj) :-
	member(Phrase, PhrasesEndingWithConj),
	last(Phrase, PhraseElement),
	% punc([inputmatch([,]),tokens([])])
	PhraseElement = punc([inputmatch([','])|_]).

merge_conjoined_phrases(Conjunct1Phrase, RestPhrasesIn, PMID, ConjoinedPhrase2, RestPhrasesOut) :-
	merge_conjoined_phrases_once(Conjunct1Phrase, RestPhrasesIn, PMID,
				     ConjoinedPhrase0, RestPhrasesNext),
	% must recurse to glue phrases together multiple times, e.g.,
	% "inoperable, recurrent and metastatic endometrial or renal carcinoma"
	merge_conjuncts(ConjoinedPhrase0, RestPhrasesNext, PMID, ConjoinedPhrase1, RestPhrasesOut),
	promote_last_mod(ConjoinedPhrase1, ConjoinedPhrase2).

promote_last_mod(ConjoinedPhrase1, ConjoinedPhrase2) :-
	last(AllButLast, Last, ConjoinedPhrase1),
	( Last = mod(Args) ->
	  NewLast = head(Args)
	; NewLast = Last
	),
	last(AllButLast, NewLast, ConjoinedPhrase2).

merge_conjoined_phrases_once(Conjunct1Phrase, RestPhrasesIn,
			     _PMID, ConjoinedPhrase, RestPhrasesOut) :-
	RestPhrasesIn = [ConjunctionPhrase,Conjunct2Phrase|Rest],
        last_head_mod_or_shapes(Conjunct1Phrase, Conjunct1PhraseLast), 
        last_head_mod_or_shapes(Conjunct2Phrase, Conjunct2PhraseLast),
        functor(Conjunct1PhraseLast, F1, _N1),
        functor(Conjunct2PhraseLast, F2, _N2),
        compatible_tags(F1, F2),
	append([Conjunct1Phrase,ConjunctionPhrase,Conjunct2Phrase], TempConjoinedPhrase),
	% maybe_print_inputmatches(TempConjoinedPhrase, PMID),
	demote_heads(TempConjoinedPhrase, ConjoinedPhrase),
	RestPhrasesOut = Rest.	

last_head_mod_or_shapes(Conjunct1Phrase, Conjunct1PhraseLast) :-
	rev(Conjunct1Phrase, RevConjunct1Phrase),
	member(Conjunct1PhraseLast, RevConjunct1Phrase),
	functor(Conjunct1PhraseLast, F, _N),
	head_or_mod_or_shapes(F).
	

compatible_tags(F1, F2) :-
        ( F1 == F2 ->
          true
        ; head_or_mod_or_shapes(F1),
          head_or_mod_or_shapes(F2)
        ).

head_or_mod_or_shapes(head).
head_or_mod_or_shapes(mod).
head_or_mod_or_shapes(shapes).

maybe_print_inputmatches(TempConjoinedPhrase, PMID) :-
	( control_option(fielded_mmi_output),
	  control_option(conj) ->
	  format('~s|CONJ|', [PMID]),
	  print_inputmatches(TempConjoinedPhrase)
	; true
	).

print_inputmatches([]) :- nl.
print_inputmatches([FirstPhraseElement|RestPhraseElements]) :-
	arg(1, FirstPhraseElement, FeatureList),
	memberchk(inputmatch(TokenList), FeatureList),
	concat_atom(TokenList, ' ', TokenListAtom),
	format('~w|', [TokenListAtom]),
	print_inputmatches(RestPhraseElements).

% print_inputmatches(TempConjoinedPhrase) :-
% 	member(Element, TempConjoinedPhrase),
% 	arg(1, Element, FeatureList),
% 	memberchk(inputmatch(TokenList), FeatureList),
% 	concat_atom(TokenList, ' ', TokenListAtom),
% 	format('~w|', [TokenListAtom]),
% 	fail.
% print_inputmatches(_) :- nl.
 

remove_tokens_until_label_token([], []).
remove_tokens_until_label_token([FirstToken|RestTokens], ExpRawTokenListNext) :-
	( label_tok(FirstToken) ->
	  ExpRawTokenListNext = [FirstToken|RestTokens]
	; remove_tokens_until_label_token(RestTokens, ExpRawTokenListNext)
	).

% Determine if an AA Expansion spans two (or more) consecutive phrases.
% If so, collapse the spanned phrases into one.

merge_aa_phrases([], CompositePhrase, _AAs, _UDAs, CompositePhrase, [], []).
merge_aa_phrases([NextPhrase|RestPhrases], FirstPhrase, AAs, UDAs,
		 MergedPhrase, RestMergedPhrases, MergedOptions) :-
	( acronym_expansion_spans_phrases(AAs, UDAs,
					  [FirstPhrase,NextPhrase|RestPhrases],
					  PhrasesToMerge, RemainingPhrases) ->
	  merge_aa_phrases_1(PhrasesToMerge, MergedPhrase),
	  RestMergedPhrases = RemainingPhrases,
	  MergedOptions = [term_processing]            % -z
	; MergedPhrase = FirstPhrase,
	  RestMergedPhrases = [NextPhrase|RestPhrases],
	  MergedOptions = []
	).

merge_aa_phrases_1(PhrasesToMerge, [AppendedPhrase1|DemotedPhrases]) :-
	append(PhrasesToMerge, [AppendedPhrase1|RestAppendedPhrases]),
	demote_heads(RestAppendedPhrases, DemotedPhrases).

% We take the first AA or UDA to span multiple phrases,
% and assume it's the only one that does. Maybe too simple?
acronym_expansion_spans_phrases(AAs, UDAs, [FirstPhrase|RestPhrases],
				[FirstPhrasePrefix|PhrasesToMerge], RemainingPhrases) :-
	( member(_AATokens-ExpansionTokens, AAs)
	; avl_member(_AATokens, UDAs, [ExpansionTokens])
	),
	% extract_token_atoms(ExpansionTokens, ExpansionStrings),
	append(FirstPhrasePrefix, FirstPhraseSuffix, FirstPhrase),
	FirstPhraseSuffix \== [],
	% reversed order of args from QP library version!
	last(FirstPhraseSuffix, LastPhraseSuffix),
	not_non_hyphen_punc(LastPhraseSuffix),
	tokens_match_inputmatch(ExpansionTokens, FirstPhraseSuffix, RestPhrases,
				0, _NumPhrasesMerged, PhrasesToMerge, RemainingPhrases),
	!.

not_non_hyphen_punc(LastPhraseSuffix) :-
	% Either the LastPhraseSuffix syntax element is not a punc(_) structure....
	( \+ LastPhraseSuffix = punc(_) ->
	  true
	% ...or if it is, the punc char is not a hyphen
	; LastPhraseSuffix = punc(Features),
	  memberchk(inputmatch(InputMatch), Features),
	  % reversed order of args from QP library version!
	  last(InputMatch, '-')
	).

% tokens_match_inputmatch(+ExpansionTokens, +FirstPhrase, +RestPhrases,
% 			  +NumPhrasesMergedIn, -NumPhrasesMergedOut,
% 			  -PhrasesToMerge, -RemainingPhrases).

% Base case: If we've exhausted all the AA expansion tokens, we've succeded.
% There are no more phrases to merge, so return all leftover phrases as RemainingPhrases.

tokens_match_inputmatch([], FirstPhrase, RestPhrases,
			NumPhrasesMerged, NumPhrasesMerged,
			[], [FirstPhrase|RestPhrases]) :-
	% Merging phrases requires that > 1 phrase be merged!
	NumPhrasesMerged > 1.
% There are more AA expansion tokens to be matched with inputmatch words.
tokens_match_inputmatch([FirstExpansionTokens|RestExpansionTokens],
			FirstPhrase, RestPhrases,
			NumPhrasesMergedIn, NumPhrasesMergedOut,
			PhrasesToMerge, RemainingPhrases) :-
	% Match expansion tokens to inputmatch words from first phrase.
	% Either the Expansion Tokens or the first phrase's inputmatch tokens
	% must be completely consumed (possibly both).
	match_expansion_tokens_to_phrase([FirstExpansionTokens|RestExpansionTokens],
					 FirstPhrase, RemainingExpansionTokens),
	NumPhrasesMergedNext is NumPhrasesMergedIn + 1,
	% so add first phrase to list of phrases to merge
	% format(user_output, '~n~q~n', [PhrasesToMerge = [FirstPhrase|RestPhrasesToMerge]]),
	PhrasesToMerge = [FirstPhrase|RestPhrasesToMerge],
	% format(user_output, '~n~q~n', [RestPhrases = [NextPhrase|RestPhrases1]]),
	( RestPhrases == [] ->
	  RestPhrasesToMerge = [],
	  % There had better be no expansion tokens leftover, either!
	  RemainingExpansionTokens == [],
	  RemainingPhrases = [],
	  % Merging phrases requires that > 1 phrase be merged!
	  NumPhrasesMergedNext > 1
	; RestPhrases = [NextPhrase|RestPhrases1],
	  tokens_match_inputmatch(RemainingExpansionTokens, NextPhrase, RestPhrases1,
				  NumPhrasesMergedNext, NumPhrasesMergedOut,
				  RestPhrasesToMerge, RemainingPhrases)
	).

match_expansion_tokens_to_phrase([], _RestPhraseElements, []).
match_expansion_tokens_to_phrase([FirstExpansionToken|RestExpansionTokens],
				 PhraseElements, RemainingExpansionTokens) :-
	  % We've consumed the inputmatches of all the elements of the current phrase
	( PhraseElements == [] ->
	  RemainingExpansionTokens = [FirstExpansionToken|RestExpansionTokens]
	; PhraseElements = [FirstPhraseElement|RestPhraseElements],
	  % In matching expansion tokens to a phrase element's inputmatch tokens,
	  % either the expansion tokens or the inputmatch must be completely consumed (or both)
	  arg(1, FirstPhraseElement, FeatureList),
	  memberchk(inputmatch(InputMatchAtoms), FeatureList),
	  append(_InputMatchAtomsPrefix, InputMatchAtomsSuffix, InputMatchAtoms),
	  InputMatchAtomsSuffix \== [],
	  match_tokens_to_inputmatch([FirstExpansionToken|RestExpansionTokens],
				     InputMatchAtomsSuffix, NextExpansionTokens),
	  !,
	  match_expansion_tokens_to_phrase(NextExpansionTokens, RestPhraseElements,
					   RemainingExpansionTokens)
	).

match_tokens_to_inputmatch([], _RestInputMatchAtoms, []).
match_tokens_to_inputmatch([FirstExpansionToken|RestExpansionTokens],
			       InputMatchAtoms, RemainingExpansionTokens) :-
	  % We've consumed all the inputmatch atoms from this phrase element
	( InputMatchAtoms == [] ->
	  RemainingExpansionTokens = [FirstExpansionToken|RestExpansionTokens]
	  % Skip over ws tokens, because blank spaces aren't represented in inputmatch
	; ws_tok(FirstExpansionToken) ->
	  match_tokens_to_inputmatch(RestExpansionTokens,
				     InputMatchAtoms, RemainingExpansionTokens)
	; InputMatchAtoms = [FirstInputMatchAtom|RestInputMatchAtoms],
	  token_template(FirstExpansionToken, _TokenType, TokenAtom, _LCTokenAtom, _PosInfo),
	  TokenAtom == FirstInputMatchAtom,
	  match_tokens_to_inputmatch(RestExpansionTokens,
				     RestInputMatchAtoms, RemainingExpansionTokens)
	).

set_var_GVCs_to_null(GVCs) :-
	( var(GVCs) ->
	  GVCs=[]
	; true
	).

extract_phrases_from_aps([], []).
extract_phrases_from_aps([ap(_NegValue,Phrase,_PhraseMap,_Mapping)|Rest],
			 [Phrase|ExtractedRest]) :-
	extract_phrases_from_aps(Rest, ExtractedRest).

/* skr_phrase(+Label, +AllUtteranceText, +PhraseText, +Phrase, +AAs,
   	      +CitationTextAtom, +RawTokensIn
              -GVCs, -RawTokensOut, -APhrases, -MMOPhraseTerm)

skr_phrase/18 is the main predicate for finding Meta terms for
a Phrase, which is of the form [<item1>,...,<itemn>] where each item is
a (possibly nested) unary term containing a (possibly multi-word) atom.
Examples:
   [head(application)]
   [prep(of),head(computers)]
   [prep(in),mod(the),head(intensive care unit)]
PhraseWordInfoPair is a pair PhraseWordInfo:FilteredPhraseWordInfo where each
element of the pair is of the form
     pwi(PhraseWordL,PhraseHeadWordL,PhraseMap).
*/

skr_phrase(Label, UtteranceText, PhraseSyntax, AAs, NoMapPairs, NoVarPairs,
	   OrigCitationTextAtom, TagList,
	   RawTokensIn, GVCs,
	   WordDataCacheIn, USCCacheIn,
	   WordDataCacheOut, USCCacheOut,
	   RawTokensOut, APhrases, MMOPhraseTerm) :-
	( skr_phrase_1(Label, UtteranceText,
		       PhraseSyntax, AAs, NoMapPairs, NoVarPairs, RawTokensIn,
		       OrigCitationTextAtom, TagList, GVCs,
		       WordDataCacheIn, USCCacheIn,
		       WordDataCacheOut, USCCacheOut,
		       RawTokensOut, APhrases, MMOPhraseTerm) ->
	  true
        ; fatal_error('skr_phrase failed on ~w ~w~n~n', [Label, PhraseSyntax]),
	  abort
        ).

skr_phrase_1(Label, UtteranceTextString,
	     PhraseSyntax, AAs, NoMapPairs, NoVarPairs,
	     RawTokensIn, OrigCitationTextAtom, TagList, GVCs,
	     WordDataCacheIn, USCCacheIn,
	     WordDataCacheOut, USCCacheOut,
	     RawTokensOut, APhrases, MMOPhraseTerm) :-
	get_pwi_info(PhraseSyntax, PhraseWordInfoPair, TokenPhraseWords, TokenPhraseHeadWords),
	get_phrase_info(PhraseSyntax, AAs, InputMatchPhraseWords, RawTokensIn, OrigCitationTextAtom,
			TagList, PhraseTokens, RawTokensOut, PhraseStartPos, PhraseLength,
			OrigPhraseTextAtom, ReplacementPositions),
	% format(user_output, 'OPTA: ~w~n', [OrigPhraseTextAtom]),
	% format(user_output, '~q~n', [OrigPhraseTextAtom]),
	atom_codes(OrigPhraseTextAtom, OrigPhraseTextString),
	debug_phrase(Label, TokenPhraseWords, InputMatchPhraseWords),
	atom_codes(UtteranceTextAtom, UtteranceTextString),
	generate_initial_evaluations(Label, UtteranceTextAtom, OrigPhraseTextString, NoVarPairs,
				     PhraseSyntax, Variants, GVCs, WordDataCacheIn, USCCacheIn,
				     RawTokensOut, AAs,
				     InputMatchPhraseWords, PhraseTokens, TokenPhraseWords,
				     TokenPhraseHeadWords, WordDataCacheOut,
				     USCCacheOut, Evaluations0),

	filter_evaluations(Evaluations0, NoMapPairs, RefinedEvaluations, FinalEvaluations0),
        length(RefinedEvaluations, TotalCandidateCount),
        length(FinalEvaluations0, RefinedCandidateCount),
        ExcludedCandidateCount is TotalCandidateCount - RefinedCandidateCount,
        debug_message(candidates,
                      '~N### ~d Initial Candidates~n### ~d Refined Candidates~n',
                      [TotalCandidateCount, RefinedCandidateCount]),
        ( control_option(dysonym_processing) ->
          exclude_dysonyms(FinalEvaluations0,FinalEvaluations)
        ; FinalEvaluations = FinalEvaluations0
        ),
        debug_evaluations(FinalEvaluations),

	generate_best_mappings(FinalEvaluations, OrigPhraseTextString, PhraseSyntax, PhraseWordInfoPair,
			       Variants, APhrases, _BestCandidates, Mappings0, PrunedCandidateCount),
	RemainingCandidateCount is RefinedCandidateCount - PrunedCandidateCount,
	sort(Mappings0, Mappings),
	% Mappings = Mappings0,
	% length(Mappings, MappingsLength),
	% format(user_output, 'There are ~d Mappings~n', [MappingsLength]),
	% I have here the data structures to call disambiguate_mmo/2
	% format(user_output, 'Candidates: ~q~n', [candidates(FinalEvaluations)]),
	% format(user_output, 'Mappings:   ~q~n', [mappings(Mappings)]),
	% format(user_output, 'GVCs:       ~q~n', [gvcs(GVCs)]),
	% format(user_output, 'EV0:        ~q~n', [ev0(Evaluations0)]),
	% format(user_output, 'APhrases:   ~q~n', [aphrases(APhrases)]),
	mark_excluded_evaluations(RefinedEvaluations),
	% mark_excluded_evaluations(FinalEvaluations),
	%  format(user_output, 'Total=~d; Excluded=~d; Pruned=~d; Remaining=~d~n',
	%        [TotalCandidateCount,ExcludedCandidateCount,
	% 	PrunedCandidateCount,RemainingCandidateCount]),
	maybe_hide_evaluations(FinalEvaluations, FinalEvaluationsAfterHiding),
	maybe_hide_evaluations(RefinedEvaluations, RefinedEvaluationsAfterHiding),
	MMOPhraseTerm = phrase(phrase(OrigPhraseTextAtom,PhraseSyntax,
				      PhraseStartPos/PhraseLength,ReplacementPositions),
			       candidates(TotalCandidateCount,
					  ExcludedCandidateCount,
					  PrunedCandidateCount,
					  RemainingCandidateCount,
					  FinalEvaluationsAfterHiding),
			       mappings(Mappings),
			       pwi(PhraseWordInfoPair),
			       gvcs(GVCs),
			       % Change the next line to ev0(BestCandidates)
			       % to include best candidates only in output
			       ev0(RefinedEvaluationsAfterHiding),
			       aphrases(APhrases)).

proper_subset([], [_|_]).
proper_subset(SubSetList, SuperSetList) :-
	member(X, SubSetList),
	\+ memberchk(X, SuperSetList),
	!,
	fail.
proper_subset(SubSetList, SuperSetList) :-
	member(X, SuperSetList),
	\+ memberchk(X, SubSetList),
	!.



% proper_subset([H|T], ListIn) :-
% 	memberchk(H, ListIn),
% 	delete(ListIn, H, ListNext),
% 	proper_subset(T, ListNext).

maybe_hide_evaluations(Evaluations, EvaluationsAfterHiding) :-
	( control_option(show_candidates) ->
	  EvaluationsAfterHiding = Evaluations
	; EvaluationsAfterHiding = []
	).


mark_excluded_evaluations(RefinedEvaluations) :-
	(  foreach(Candidate, RefinedEvaluations)
	do get_candidate_feature(status, Candidate, Status),
	   % If the Status field is still uninstantiated,
	   % this candidate was filtered out,
	   % so the Status field can be set to 1.
	   % If the Status field is already set,
	   % "Status is 1" will fail, so just succeed anyway.
	   ( Status is 1 ->
	     true
	   ; true
	   )
	).

debug_phrase(Label, TokenPhraseWords, InputMatchPhraseWords) :-
	( phrase_debugging ->
	  get_label_components(Label, PMID, TiOrAB, UtteranceNum),
	  length(TokenPhraseWords, TokenPhraseLength),
	  format(user_error, 'Phrase|~s|~s|~s|~d|~q~n',
		 [PMID,TiOrAB,UtteranceNum,TokenPhraseLength,InputMatchPhraseWords]),
	  flush_output(user_error)
	; true
	).

phrase_debugging :-
	( control_value(debug, DebugFlags),
	  memberchk(phrases, DebugFlags) ->
	  true
	; control_option(phrases_only)
	).

get_label_components(Label, PMID, TiOrAB, UtteranceNum) :-
	  atom_codes(Label, LabelString),
	  % Label can contain an arbitrary number of ".",
	  % but the last two chunks will be the TiOrAB and the UtteranceNumber:
	  % E.g., if Label is "C0000726-L0000726-S0009053-HL7V2.5.ti.0",
	  % we need to isolate "ti" and "0".
	  ( split_string_completely(LabelString, ".", ComponentList),
	    append(PMID0, [TiOrAB,UtteranceNum], ComponentList),
	    interleave_string(PMID0, ".", PMID1),
	    append(PMID1, PMID) ->
	    true
	  ; PMID = "<>",
	    TiOrAB = "<>",
	    UtteranceNum = "<>"
	  ).

get_pwi_info(Phrase, PhraseWordInfoPair, TokenPhraseWords, TokenPhraseHeadWords) :-
	( control_option(term_processing) ->
	  parse_phrase_word_info(Phrase, unfiltered, PhraseWordInfoPair)
	; control_option(conj) ->
	  parse_phrase_word_info(Phrase, unfiltered, PhraseWordInfoPair)
	; parse_phrase_word_info(Phrase, filtered,   PhraseWordInfoPair)
	),
	PhraseWordInfoPair = _AllPhraseWordInfo:FilteredPhraseWordInfo,
	% format(user_output, 'ALL:      ~q~n', [AllPhraseWordInfo]),
	% format(user_output, 'FILTERED: ~q~n', [FilteredPhraseWordInfo]),
	FilteredPhraseWordInfo = pwi(FPhraseWordL,FPhraseHeadWordL,_FPhraseMap),
	FPhraseWordL = wdl(_,TokenPhraseWords),
	FPhraseHeadWordL = wdl(_,TokenPhraseHeadWords).

get_phrase_info(Phrase, AAs, InputMatchPhraseWords, RawTokensIn, CitationTextAtom, TagList,
		PhraseTokens, RawTokensOut, PhraseStartPos, PhraseLength,
		OrigPhraseTextAtom, ReplacementPositions) :-
	get_inputmatch_atoms_from_phrase(Phrase, InputMatchPhraseWords0),
	% InputMatchPhraseWords = InputMatchPhraseWords0,
	% format(user_output, 'Phrase: ~w~n', [Phrase]),
	re_attach_apostrophe_s_to_prev_word(InputMatchPhraseWords0,
					    TagList,
					    InputMatchPhraseWords),
	% InputMatchPhraseWords = InputMatchPhraseWords0,
	% For each word in InputMatchPhraseWords, extract the matching tokens from RawTokensIn.
	% We need to match the words in the raw tokens to get the correct pos info
	% and to get the phrase with all the blanks.

	% need to modify phrase tokens to discard field, label, and sn tokens
	get_phrase_tokens(InputMatchPhraseWords, RawTokensIn, PhraseTokens, RawTokensOut),
	% skr_utilities:write_token_list(RawTokensIn, 0, 1),
	% format(user_output, '---> ~w~n~n', [InputMatchPhraseWords]),
	% skr_utilities:write_token_list(RawTokensOut, 0, 1),
	get_phrase_startpos_and_length(PhraseTokens, PhraseStartPos, PhraseLength0),
	%% PhraseTokens = [FirstPhraseToken|_RestPhraseTokens],
	%% last(PhraseTokens, LastPhraseToken),
	%% % token_template(LastPhraseToken, _TokenType, _TokenString, _LCTokenString, _Pos1, RealPos),
	%% token_template(FirstPhraseToken, _FirstTokenType,
	%% 	       _FirstTokenString, _LCFirstTokenString, _FirstPos1, FirstRealPos),
	%% FirstRealPos = pos(PhraseStartPos, _),
	%% token_template(LastPhraseToken, _LastTokenType,
	%% 	       _LastTokenString, _LCLastTokenString, _LastPos1, LastRealPos),
	%% LastRealPos = pos(LastStartPos, LastLength),
	%% PhraseLength0 is LastStartPos + LastLength - PhraseStartPos,
	subchars(CitationTextAtom, PhraseTextStringWithCRs0, PhraseStartPos, PhraseLength0),
	add_AA_suffix(PhraseTextStringWithCRs0, AAs, PhraseTokens, PhraseLength0,
		      CitationTextAtom, PhraseTextStringWithCRs, PhraseLength),
	replace_crs_with_blanks(PhraseTextStringWithCRs, PhraseStartPos,
				OrigPhraseTextString, ReplacementPositions),
	atom_codes(OrigPhraseTextAtom, OrigPhraseTextString).

	
	% atom_codes(RealPhraseText, RealPhraseTextString).

% If the last phrase token matches the last expansion token of a given AA,
% then add " (" + AA + ")" to the phrase string, and modify the length accordingly.

add_AA_suffix(PhraseTextStringWithCRs0, AAs, PhraseTokens, PhraseLength0,
	      CitationTextAtom, PhraseTextStringWithCRs, PhraseLength) :-
	% reversed order of args from QP library version!
	( last(PhraseTokens, LastPhraseToken),
	  avl_member(AATokens, AAs, [ExpansionTokens]),
	  % reversed order of args from QP library version!
	  last(ExpansionTokens, LastExpansionToken),
	  % ExpansionTokens in the AA AVL tree are of the form
	  % tok(Type, String, LCString, PosInfo1), but
	  % Tokens in the Phrase Token List are of the form
	  % tok(Type, String, LCString, PosInfo1, PosInfo2).
	  % We require that the first 4 fields match.
	  matching_tokens_4(LastExpansionToken, LastPhraseToken),
	  get_AA_text(AATokens, AATextString0),
	  append(AATextString0, AATextString),
	  determine_brackets_enclosing_AA(PhraseTokens, CitationTextAtom,
					  LeftBracket, RightBracket),
	  append([PhraseTextStringWithCRs0,LeftBracket,AATextString,RightBracket], PhraseTextStringWithCRs) ->
	  % append([PhraseTextStringWithCRs0," (",AATextString,")"], PhraseTextStringWithCRs),
	  length(AATextString, AATextStringLength),
	  PhraseLength is PhraseLength0 + 2 + AATextStringLength + 1
	; PhraseTextStringWithCRs = PhraseTextStringWithCRs0,
	  PhraseLength is PhraseLength0
	).
	
% Determine the Left and Right brackets surrounding the AA:
% We can't automatically assume they are "(" and ")"!

determine_brackets_enclosing_AA(PhraseTokens, CitationTextAtom, MidStringPrefix, [RightBracket]) :-
	last(PhraseTokens, LastPhraseToken),
	token_template(LastPhraseToken, _TokenType, _TokenString, _LCTokenString, _Pos1, RealPos),
	RealPos = pos(LastPhraseTokenStartPos,LastPhraseTokenLength),
	PrefixLength is LastPhraseTokenStartPos + LastPhraseTokenLength,
	MidStringLength is 10,
	midstring(CitationTextAtom, MidString, _Fringes, PrefixLength, MidStringLength, _After),
	atom_codes(MidString, MidStringCodes),
	append(MidStringPrefix, _MidStringRest, MidStringCodes),
	last(MidStringPrefix, MidStringPrefixLastChar),
	brackets([MidStringPrefixLastChar], [RightBracket]),
	!.

	
	  
% First 4 fields must be identical	
matching_tokens_4(tok(TokenType, TokenString, TokenLCString, Pos1),
		  tok(TokenType, TokenString, TokenLCString, Pos1, _Pos2)).
		  
	
get_AA_text(AATokens, AATextString) :-
	extract_token_atoms(AATokens, AATextString).
	% extract_token_atoms(AATokens, AATokenStrings),
	% interleave_string(AATokenStrings, " ", AATextString).

generate_initial_evaluations(Label, UtteranceText,
			     PhraseTextString, NoVarPairs, Phrase, Variants,
			     GVCs, WordDataCacheIn, USCCacheIn, RawTokensOut, AAs,
			     InputMatchPhraseWords, PhraseTokens, TokenPhraseWords,
			     TokenPhraseHeadWords, WordDataCacheOut, USCCacheOut, Evaluations0) :-
	% If phrases_only is on, don't bother generating any evaluations,
	% because we've already computed and displayed the phrase lengths,
	% and that's all we care about if this option is on.
	% Moreover, setting Evaluations0 to [] will short-circuit
	% all subsequent evaluation processing.
	( ( control_option(aas_only)
	  ; control_option(phrases_only) ) ->
	  Evaluations0 = [],
	  WordDataCacheOut = WordDataCacheIn,
	  USCCacheOut = USCCacheIn
	; check_generate_initial_evaluations_control_options_1,
	  lower(PhraseTextString, LCPhraseTextString),
	  extract_syntactic_tags(Phrase, Tags),
	  atom_codes(LCPhraseAtom, LCPhraseTextString),
	  stop_analysis(LCPhraseAtom, LCPhraseTextString, Tags) ->
	  Evaluations0 = [],
	  WordDataCacheOut = WordDataCacheIn,
	  USCCacheOut = USCCacheIn
	; control_option(allow_overmatches),
	  control_value(min_length, MinLength),
	  length(PhraseTextString, PhraseTextStringLength),
	  PhraseTextStringLength < MinLength ->
	  Evaluations0 = [],
	  WordDataCacheOut = WordDataCacheIn,
	  USCCacheOut = USCCacheIn
	; check_generate_initial_evaluations_control_options_2 ->
 	  compute_evaluations(Label, UtteranceText, NoVarPairs,
 			      Phrase, Variants, GVCs,
 			      WordDataCacheIn, USCCacheIn, RawTokensOut, AAs,
 			      InputMatchPhraseWords,
 			      PhraseTokens, TokenPhraseWords, TokenPhraseHeadWords,
 			      WordDataCacheOut, USCCacheOut, Evaluations0)
	; Evaluations0 = [],
	  WordDataCacheOut = WordDataCacheIn,
	  USCCacheOut = USCCacheIn
	).

% generate_initial_evaluations_1(Label, UtteranceText,
% 			       PhraseTextString, Phrase, Variants,
% 			       GVCs, WordDataCacheIn, USCCacheIn, RawTokensOut, AAs,
% 			       InputMatchPhraseWords, PhraseTokens, TokenPhraseWords,
% 			       TokenPhraseHeadWords, WordDataCacheOut, USCCacheOut, Evaluations0) :-
% 	( check_generate_initial_evaluations_control_options_1,
% 	  lower(PhraseTextString, LCPhraseTextString),
% 	  extract_syntactic_tags(Phrase, Tags),
% 	  atom_codes(LCPhraseAtom, LCPhraseTextString),
% 	  stop_analysis(LCPhraseAtom, LCPhraseTextString, Tags) ->
% 	  Evaluations0 = [],
% 	  WordDataCacheOut = WordDataCacheIn,
% 	  USCCacheOut = USCCacheIn
% 	; check_generate_initial_evaluations_control_options_2 ->
% 	  % format(user_output, 'About to call compute_evaluations~n', []), ttyflush,
% 	  compute_evaluations(Label, UtteranceText,
% 			      Phrase, Variants, GVCs,
% 			      WordDataCacheIn, USCCacheIn, RawTokensOut, AAs,
% 			      InputMatchPhraseWords,
% 			      PhraseTokens, TokenPhraseWords, TokenPhraseHeadWords,
% 			      WordDataCacheOut, USCCacheOut, Evaluations0)
% 	  % format(user_output, 'Done with compute_evaluations~n', []), ttyflush
% 	;  Evaluations0 = [],
% 	   WordDataCacheOut = WordDataCacheIn,
% 	   USCCacheOut = USCCacheIn
% 	).

% Short-circuit the analysis if Atom is a stop phrase whose lexical categories
% overlap with the current phrase's StopTags.
stop_analysis(Atom, String, Tags) :-
	( stop_phrase(Atom, StopTags),
	  intersection(Tags, StopTags, [_|_]) ->
	  true
	; control_value(min_length, MinLength),
	  trim_and_compress_whitespace(String, StringWithNoBlanks),
	  length(StringWithNoBlanks, Length),
	  Length < MinLength
	).

filter_evaluations(InitialEvaluations, NoMapPairs, RefinedEvaluations, FinalEvaluations) :-
	filter_evaluations_by_user_exclusions(InitialEvaluations, NoMapPairs,
					      EvaluationsAfterUserExclusions),
	filter_evaluations_by_sources(EvaluationsAfterUserExclusions, EvaluationsAfterSources),
	% SemType filtering is now done after mapping construction in filter_mappings_by_semtypes/6
	% UNLESS mappings are off (i.e., hide_mappings is on).
	( \+ control_option(hide_mappings) ->
	  EvaluationsAfterSemTypes = EvaluationsAfterSources
	; filter_evaluations_by_semtypes(EvaluationsAfterSources, EvaluationsAfterSemTypes)
	),
	filter_numerical_evaluations(EvaluationsAfterSemTypes, EvaluationsAfterNumerical), 
	RefinedEvaluations = EvaluationsAfterNumerical,
	filter_evaluations_by_subsumption(RefinedEvaluations, FinalEvaluations).


filter_evaluations_by_user_exclusions(InitialEvaluations, NoMapPairs, EvaluationsAfterUserExclusions) :-
	( control_value(nomap, _) ->
	  filter_evaluations_by_user_exclusions_aux(InitialEvaluations,
						    NoMapPairs,
						    EvaluationsAfterUserExclusions)
	; EvaluationsAfterUserExclusions  = InitialEvaluations
	).
	  
filter_evaluations_by_sources(Evaluations0, EvaluationsAfterSources) :-
	( control_value(restrict_to_sources, RestrictedSources) ->
	  convert_to_root_sources(RestrictedSources, RestrictedRootSources),
	  upper_list(RestrictedRootSources, RestrictedRootSourcesUPPER),
	  filter_evaluations_restricting_to_sources(Evaluations0,
						    RestrictedRootSourcesUPPER,
						    EvaluationsAfterSources)
	; control_value(exclude_sources, ExcludedSources),
	  convert_to_root_sources(ExcludedSources, ExcludedRootSources),
	  upper_list(ExcludedRootSources, ExcludedRootSourcesUPPER),
	  filter_evaluations_excluding_sources(Evaluations0,
					       ExcludedRootSourcesUPPER,
					       EvaluationsAfterSources)
	; EvaluationsAfterSources = Evaluations0
	).

filter_numerical_evaluations(Evaluations0, EvaluationsAfterNumerical) :-
	( control_value(no_nums, NumSemTypes) ->
	  filter_numerical_evaluations_aux(Evaluations0,
					   NumSemTypes,
					   EvaluationsAfterNumerical)
	; EvaluationsAfterNumerical = Evaluations0
	).

filter_evaluations_by_semtypes(Evaluations0, RefinedEvaluations) :-
	( control_option(restrict_to_sts) ->
	  control_value(restrict_to_sts, STs),
	  filter_evaluations_restricting_to_sts(Evaluations0, STs,
						RefinedEvaluations0, Discarded),
	  maybe_discard_overlapping_evaluations(RefinedEvaluations0, Discarded, RefinedEvaluations)
	; control_option(exclude_sts) ->
	  control_value(exclude_sts, STs),
	  filter_evaluations_excluding_sts(Evaluations0, STs,
					   RefinedEvaluations, _Discarded)
	; RefinedEvaluations = Evaluations0
	).

maybe_discard_overlapping_evaluations(RefinedEvaluations0, Discarded, RefinedEvaluations) :-
	( control_option(cascade) ->
	  discard_overlapping_evaluations(RefinedEvaluations0, Discarded, RefinedEvaluations)
	; RefinedEvaluations = RefinedEvaluations0
	).

% This is an idea suggested by Dina.
% Suppose the text is "logistic regression", and we restict to DISO concepts.
% Then the concept
% 1000  C0206031:Logistic Regression  [Research Activity]
% is ruled out. However, we are left with
% 1000  Regression (Disease regression) [Pathologic Function]
% The two concepts share positional information.
% To rule out "Regression" use the "--cascade" option.

% We'd want to discard "regression" as well.
discard_overlapping_evaluations([], _DiscardedEvaluations, []).
discard_overlapping_evaluations([H|T], DiscardedEvaluations, KeptEvaluations) :-
	( get_candidate_feature(posinfo, H, PosInfoH),
	  member(Discarded, DiscardedEvaluations),
	  get_candidate_feature(posinfo, Discarded, PosInfoDiscarded),
	  posinfo_lists_overlap(PosInfoH, PosInfoDiscarded) ->
	  KeptEvaluations = RestKeptEvaluations
	; KeptEvaluations = [H|RestKeptEvaluations]
	),
	discard_overlapping_evaluations(T, DiscardedEvaluations, RestKeptEvaluations).


filter_evaluations_by_subsumption(RefinedEvaluations, FinalEvaluations) :-
 	( ( control_option(compute_all_mappings)
	  % ; control_option(hide_mappings)
	  ; control_option(allow_overmatches) ) ->
 	  FinalEvaluations = RefinedEvaluations
	; filter_out_subsumed_evaluations(RefinedEvaluations, FinalEvaluations)
 	).

debug_evaluations(Evaluations) :-
	( control_value(debug, DebugFlags),
	  memberchk(4, DebugFlags) ->
	  length(Evaluations, NEvals),
	  format(user_error, '~nNon-subsumed evaluations (~d):~n', [NEvals]),
	  wl(Evaluations)
	; true
	).

generate_best_mappings(Evaluations, PhraseTextString, Phrase, PhraseWordInfoPair,
		       Variants, APhrases, _BestCandidates, Mappings, PrunedCount) :-
	  % Construct mappings only if necessary
	( check_generate_best_mappings_control_options ->
	  % format(user_output, 'About to call construct_best_mappings~n', []), ttyflush,
	  construct_best_mappings(Evaluations, PhraseTextString,
				  Phrase, PhraseWordInfoPair, NPhraseWords,
				  Variants, APhrases0, _BestCandidates, Mappings0, PrunedCount),
	  maybe_filter_mappings_by_semtypes(Mappings0, APhrases0,
					    NPhraseWords, Variants,
					    Mappings1, APhrases1),
	  maybe_reduce_mappings_for_conj(Mappings1, Mappings1, APhrases1,
					 NPhraseWords, Variants,
					 Mappings, APhrases)
	  % length(Mappings0, Mappings0Length),
	  % format(user_output, 'Done with construct_best_mappings:~d ~n', [Mappings0Length]), ttyflush
	; APhrases = [],
          Mappings = [],
	  % If no mappings were constructed, no candidates were pruned;
	  % this next call will assign Status 0 to all candidates.
	  maybe_mark_pruned_evaluations(Evaluations, Evaluations),
	  % Don't prune any candidates if we don't generate mappings!
	  PrunedCount is 0
	).

extract_syntactic_tags([], []).
extract_syntactic_tags([First|Rest], [FirstSTag|RestSTags]) :-
	functor(First, FirstSTag, _Arity),
	extract_syntactic_tags(Rest, RestSTags).


get_all_generators_and_candidate_lengths([], []).
get_all_generators_and_candidate_lengths([GVC|RestGVCs], [G-CandidatesLength|RestGenerators]) :-
	GVC = gvc(Generator,_V,Candidates),
	Generator = v(G,_,_,_,_,_),
	length(Candidates, CandidatesLength),
	get_all_generators_and_candidate_lengths(RestGVCs, RestGenerators).

compute_evaluations(Label, UtteranceText, NoVarPairs,
		    Phrase, Variants, GVCs, WordDataCacheIn, USCCacheIn,
		    RawTokensOut, AAs, InputMatchPhraseWords,
		    PhraseTokens, TokenPhraseWords, TokenPhraseHeadWords,
		    WordDataCacheOut, USCCacheOut, Evaluations) :-
	% *******************
	% Expansion Algorithm
	% *******************
	get_debug_control_value(DebugFlags),
	debug_message(trace, '~N### Calling generate_variants: ~q~n', [TokenPhraseWords]),
	generate_variants(TokenPhraseWords, TokenPhraseHeadWords,
			  Phrase, DebugFlags, NoVarPairs, GVCs, Variants),
	% format(user_output, '~N### generate_variants DONE!~n', []),
	debug_compute_evaluations_2(DebugFlags, GVCs, Variants),

	debug_message(trace, '~N### Calling add_candidates: ~q~n', [TokenPhraseWords]),
	test_single_char_tokens(InputMatchPhraseWords, IgnoreSingleChars),
	CandidateCount is 1,
	maybe_ignore_GVCs(GVCs, GVCsToEvaluate),
	add_candidates(GVCsToEvaluate, CandidateCount, Variants, IgnoreSingleChars, DebugFlags,
		       WordDataCacheIn, USCCacheIn,
		       WordDataCacheOut, USCCacheOut),
	% format(user_output, '~N### add_candidates DONE!~n', []),
	debug_compute_evaluations_3(DebugFlags, GVCs),

	length(TokenPhraseWords, PhraseTokenLength),
	get_all_generators_and_candidate_lengths(GVCs, GeneratorsAndCandidateLengths),
	debug_message(trace,
		      '~N### Calling evaluate_all_GVCs: ~q~n',
		      [GeneratorsAndCandidateLengths]),
	empty_avl(CCsIn),
	evaluate_all_GVCs(GVCsToEvaluate, DebugFlags, Label, UtteranceText,
			  Variants, TokenPhraseWords,
			  PhraseTokenLength, TokenPhraseHeadWords,
			  PhraseTokens, RawTokensOut, AAs,
			  InputMatchPhraseWords,
			  CCsIn, _CCsOut, [], Evaluations0),
	% format(user_output, '~N### All GVCs DONE!~n', []),
	sort(Evaluations0, Evaluations1),
	maybe_filter_evaluations_by_threshold(Evaluations1, Evaluations2),
	debug_compute_evaluations_4(DebugFlags, Evaluations2),
	% length(Evaluations2, Length2),
	% format(user_output, '~d~n', [Length2]),
	% Evaluations = Evaluations2,
	% get_CUIs(Evaluations2, CUIs),
	% agg_count(CUIs, Res), ( Res = [First|_Counts] -> format('### ~w~n', [First]) ; true),
	filter_out_redundant_evaluations(Evaluations2, Evaluations),
	% This is now done in compute_one_evaluation in metamap_evaluation.pl
	% add_semtypes_to_evaluations(Evaluations),
	debug_compute_evaluations_5(DebugFlags, Evaluations).

% agg_count(List, Counts) :-
% 	sort(List, Sorted),
% 	count_all_instances(Sorted, List, Counts0),
% 	sort(Counts0, Counts).
% 
% count_all_instances([], _List, []).
% count_all_instances([H|T], List, [HCount-H|TCounts]) :-
% 	count_occurrences(List, H, 0, HCount),
% 	count_all_instances(T, List, TCounts).
% 
% count_occurrences([], _Element, Counts, Counts).
% count_occurrences([H|T], Element, CountIn, CountOut) :-
% 	( H == Element ->
% 	  CountNext is CountIn - 1
% 	; CountNext is CountIn
% 	),
% 	count_occurrences(T, Element, CountNext, CountOut).
% 
% 
% get_CUIs([], []).
% get_CUIs([H|T], [HCUI|TCUIs]) :-
% 	get_candidate_feature(cui, H, HCUI),
% 	get_CUIs(T, TCUIs).

maybe_ignore_GVCs(GVCs, GVCsToEvaluate) :-
	( control_option(tokenize_only) ->
	  GVCsToEvaluate = []
	% ; GVCsToEvaluate = GVCs
	; keep_only_useful_lexcats(GVCs, GVCsToEvaluate)
	).

% <flang@indlx6> 29 : grep cat= lexiconStatic2016 | count
% 404417    82.260    82.260  	cat=noun
%  65829    13.390    95.649  	cat=adj
%  11399     2.319    97.968  	cat=verb
%   9570     1.947    99.915  	cat=adv
%    217     0.044    99.959  	cat=prep
%     86     0.017    99.976  	cat=pron
%     67     0.014    99.990  	cat=conj
%     38     0.008    99.998  	cat=det
%      8     0.002    99.999  	cat=modal
%      3     0.001   100.000  	cat=aux
%      1     0.000   100.000  	cat=compl

% print_GVCs([]).
% print_GVCs([H|T]) :-
%  	H = gvc(v(_,_,LexCats,_,_,_),_, _),
% 	format(user_output, '~w~n', [LexCats]),
% 	print_GVCs(T).
	      
useful_lexcat(noun).
useful_lexcat(adj).
useful_lexcat(verb).
useful_lexcat(adv).

keep_only_useful_lexcats([], []).
keep_only_useful_lexcats([H|T], FilteredGVCs) :-
	H = gvc(v(_,_,LexCats,_,_,_),_, _),
	% If the token is not in the lexicon at all, LexCats will be [],
	% and we want to keep those. Filter out only tokens in the lexicon
	% that we know are not useful.
	( LexCats == [] ->
	  FilteredGVCs = [H|RestFilteredGVCs]	    
	; LexCats = [LexCat],	    
	  useful_lexcat(LexCat) ->
	  FilteredGVCs = [H|RestFilteredGVCs]
	; FilteredGVCs = RestFilteredGVCs
	),
	keep_only_useful_lexcats(T, RestFilteredGVCs).

% Determine if at least one-third of the words in the phrase are either
% single-char alphabetic tokens or hyphens. This special case handles cases like

% The sequence was (in the standard one-letter code)
%      A-N-S-F-L-X-X-L-R-P-G-N-V-X-R-X-C-S-X-X-V-C-X-F-X-X-A-R-X-I-F-Q-N-T-X-D-T-
%      M-A-F-W-S-K-Y-S-D-G-D-Q-C-E-D-R-P-S-G-S-P-C-D-L-P-C-C-G-R-G-K-C-I-H-G-L-G-
%      G-F-R-C-D-C-A-E-G-W-E-G-R-F-C-L-H-E-V-R-F-S-N-C-S-A-E-B-G-G-C-A-H-Y-C-M-E-
%      E-E-G-R-R-H-C-S-C-A-P-G-Y-R-L-E-D-D-H-Q-L-C-V-S-K-V-T-F-P-C-G-R-L-G-K-R-M-

% from PMID 282610 and
% the following sequence was obtained: A D T N A P L
%      C L C D E P G I L G R N Q L V T P E V K E K I E K A V E A V A E E S G V S
%      G R G F S L F S H H P V F R E C G K Y E C R T V R P E H T R C Y N F P P F
%      V H F T S E C P V S T R D C E P V F G Y T V A G E F R V I V Q A P R A G F
%      R Q C V W Q H K C R Y G S N N C G F S G R C T Q Q R S V V R L V T Y N L E

% from PMID 3905780.
% This determination will control the generation of candidates
% for single-character tokens in add_candidates/8 (see metamap_evaluation.pl).
% The idea is to block candidate generation for single-character tokens in
% phrases with a large number of single-char alphabetic tokens or hyphens.

test_single_char_tokens(InputMatchPhraseWords, IgnoreSingleChars) :-
	length(InputMatchPhraseWords, PhraseLength),
	( PhraseLength >= 10 ->
	  SingleCharAlphaOrHyphenCountIn is 0,
	  test_single_char_tokens_aux(InputMatchPhraseWords, PhraseLength,
				      SingleCharAlphaOrHyphenCountIn, _,
				      IgnoreSingleChars)
	; IgnoreSingleChars is 0
	).

test_single_char_tokens_aux([], PhraseLength, Count, Count, Result) :-
	( Count > PhraseLength * 0.75 ->
	  Result is 1
	; Result is 0
	).
test_single_char_tokens_aux([FirstWord|RestWords], PhraseLength, CountIn, CountOut, Result) :-
	next_single_alpha_or_hyphen_count(FirstWord, CountIn, CountNext),
	test_single_char_tokens_aux(RestWords, PhraseLength, CountNext, CountOut, Result).

% if the word consists of exactly one character, and that character is alphabetic,
% increment the count of consecutive single-character alphabetic tokens;
% otherwise reset the count to zero.
next_single_alpha_or_hyphen_count(Word, CountIn, CountNext) :-
	atom_codes(Word, WordCodes),
	( WordCodes = [_SingleChar] ->
	  % is_alpha_or_hyphen(SingleChar) ->
	  CountNext is CountIn + 1
	; CountNext is CountIn
	).

% is_alpha_or_hyphen(SingleChar) :-
% 	( is_alpha(SingleChar) ->
% 	  true
% 	; SingleChar =:= 45 % ASCII code for "-"
% 	).

generate_variants(PhraseWords, PhraseHeadWords, Phrase, 
		  DebugFlags, NoVarPairs, GVCsOut, Variants) :-
	expand_split_word_list(PhraseWords, DupPhraseWords),
	% expand_split_word_list(PhraseHeadWords, DupPhraseHeadWords),	
	compute_variant_generators(PhraseWords, DupPhraseWords, GVCs0),
	% format(user_output, '~n### ~q~n',
	%        [compute_variant_generators(PhraseWords, GVCs0)]),
	debug_compute_evaluations_1(DebugFlags, GVCs0),
	filter_variants_by_tags(GVCs0, Phrase, GVCs1),
	% format(user_output, '~n### ~q~n',
	%        [filter_variants_by_tags(GVCs0, Phrase, GVCs1)]),
	augment_GVCs_with_variants(GVCs1),
	% format(user_output, '~n### ~q~n',
	%        [augment_GVCs_with_variants(GVCs1)]),
	maybe_filter_out_dvars(GVCs1, GVCs2),
	maybe_filter_out_aas(GVCs2, GVCs3),
	maybe_filter_out_novars(NoVarPairs, GVCs3, GVCsOut),
	gather_variants(GVCsOut,
			PhraseWords, PhraseHeadWords, % GenWords, DupPhraseWords, DupPhraseHeadWords, DupGenWords,
			Variants).

	% format(user_output, '~n### ~q~n',
	%       [gather_variants(GVCs3, PhraseWords, PhraseHeadWords, Variants)]).

%%% The StartPos of a list of tokens = the minimum StartPos of all the tokens.
%%% The EndPos   of a list of tokens = the maximum StartPos of all the tokens
%%%                                  + the length of the token with the maximum StartPos
%%% The Length of a list of tokens   = EndPos = StartPos

get_phrase_startpos_and_length([], 0, 0).
get_phrase_startpos_and_length([FirstToken|RestTokens], PhraseStartPos, PhraseLength) :-
	% Initialize MinStartPos, MaxStartPos, and MaxLength
	% with the values from the first token
	get_token_startpos_and_length(FirstToken, FirstStartPos, FirstLength),
 	MinStartPos is FirstStartPos,
 	MaxStartPos is FirstStartPos,
 	MaxLength is FirstLength,
 	get_phrase_startpos_and_length_aux(RestTokens,
 					   MinStartPos, MaxStartPos, MaxLength,
 					   PhraseStartPos, PhraseLength).
 
% When there are no more tokens,
% use the current MinStartPos, MaxStartPos, and MaxLength
% to determine the entire phrase's StartPos and Length.
get_phrase_startpos_and_length_aux([],
				   MinStartPos, MaxStartPos, MaxLength,
				   PhraseStartPos, PhraseLength) :-
	PhraseStartPos is MinStartPos,
	PhraseEndPos is MaxStartPos + MaxLength,
	PhraseLength is PhraseEndPos - PhraseStartPos.
get_phrase_startpos_and_length_aux([Token|RestTokens],
				   MinStartPosIn, MaxStartPosIn, MaxLengthIn,
				   PhraseStartPos, PhraseLength) :-
	update_startpos_and_length(MinStartPosIn, MaxStartPosIn, MaxLengthIn,
				   Token,
				   MinStartPosNext, MaxStartPosNext, MaxLengthNext),
	get_phrase_startpos_and_length_aux(RestTokens,
					   MinStartPosNext, MaxStartPosNext, MaxLengthNext,
					   PhraseStartPos, PhraseLength).

update_startpos_and_length(MinStartPosIn, MaxStartPosIn, MaxLengthIn,
			   Token,
			   MinStartPosOut, MaxStartPosOut, MaxLengthOut) :-
	get_token_startpos_and_length(Token, TokenStartPos, TokenLength),
	update_min_startpos(MinStartPosIn, TokenStartPos, MinStartPosOut),
	update_max_startpos_and_length(MaxStartPosIn, MaxLengthIn,
				       TokenStartPos, TokenLength,
				       MaxStartPosOut, MaxLengthOut).

% If the current token's StartPos < the MinStartPos so far,
% update MinStartPos to the current token's StartPos;
% otherwise, keep the same MinStartPos.
update_min_startpos(MinStartPosIn, TokenStartPos, MinStartPosOut) :-
	( TokenStartPos < MinStartPosIn ->
	  MinStartPosOut is TokenStartPos
	; MinStartPosOut is MinStartPosIn
	). 

% If the current token's StartPos > the MaxStartPos so far,
% update MaxStartPos and Length to the current token's StartPos and Length;
% otherwise, keep the same MaxStartPos and Length.
update_max_startpos_and_length(MaxStartPosIn, MaxLengthIn,
			       TokenStartPos, TokenLength,
			       MaxStartPosOut, MaxLengthOut) :-
	( TokenStartPos > MaxStartPosIn ->
	  MaxStartPosOut is TokenStartPos,
	  MaxLengthOut is TokenLength
	; MaxStartPosOut is MaxStartPosIn,
	  MaxLengthOut is MaxLengthIn
	).

get_token_startpos_and_length(tok(_TokenType, _String, _LCString, _Pos1, pos(StartPos, Length)),
			       StartPos, Length).
get_token_startpos_and_length(tok(_TokenType, _String, _LCString, pos(StartPos, EndPos)),
			       StartPos, Length) :-
	Length is EndPos - StartPos.

% get_phrase_tokens(+PhraseWords, +RawTokensIn, -PhraseTokens, -RawTokensOut)
% 
% PhraseWords = [in,patients]
% RawTokensIn = [
% 	tok(lc,"in","in",pos(30,32),pos(183,2)),
% 	tok(ws," "," ",pos(32,33),pos(185,1)),
% 	tok(lc,"patients","patients",pos(33,41),pos(186,8)),
% 	tok(ws," "," ",pos(41,42),pos(194,1)),
% 	tok(lc,"with","with",pos(42,46),pos(195,4)),
% 	tok(ws," "," ",pos(46,47),pos(199,1)),
%         ...  ]
% 
% Extract from RawTokensIn the tokens matching the woreds in PhraseWords.


get_phrase_tokens(InputMatchPhraseWords, RawTokensIn, PhraseTokens, RawTokensOut) :-
	get_phrase_tokens_aux(InputMatchPhraseWords, 0, RawTokensIn, PhraseTokens, RawTokensOut).

get_phrase_tokens_aux([], _PrevTokenStartPos, RestRawTokens, [], RestRawTokens).
get_phrase_tokens_aux([H|T], PrevTokenStartPos, RawTokensIn, [TokenH|TokensT], RawTokensOut) :-
	remove_leading_hoa_ws_toks(RawTokensIn, [H|T], RawTokens1),
        get_word_token(H, PrevTokenStartPos, RawTokens1, TokenH, NewTokenStartPos, RawTokens2),
        get_phrase_tokens_aux(T, NewTokenStartPos, RawTokens2, TokensT, RawTokensOut).


% Remove as many tokens from the head of the list that are higher-order or ws.
% Moreover, if we encounter a PE (parenthetical expression), remove all the tokens
% to the end of the PE unless the InputMatchPhraseWords includ the PE itself.
% PE tokens will be removed in the case of an AA, but not if the text contains
% a real parenthetical expression, including a quoted string.
remove_leading_hoa_ws_toks([], _InputMatchPhraseWords, []).
remove_leading_hoa_ws_toks([FirstToken|RestTokens], InputMatchPhraseWords, FilteredTokens) :-
	( pe_tok(FirstToken),
	  InputMatchPhraseWords = [FirstPhraseWordAtom|_RestPhraseWordAtoms],
	  atom_codes(FirstPhraseWordAtom, FirstPhraseWordString),
	  FirstPhraseWordString = [FirstPhraseWordCode1|_FirstPhraseWordRestCodes],
	  \+ brackets([FirstPhraseWordCode1], _) ->
	  token_template(FirstToken, pe, _TokenString, _LCTokenString, PosInfo1, _PosInfo2),
	  PosInfo1 = pos(_PETokenStartPos, PETokenEndPos),
	  remove_tokens_to_end_of_pe(RestTokens, PETokenEndPos, RemainingTokens),
	  remove_leading_hoa_ws_toks(RemainingTokens, InputMatchPhraseWords, FilteredTokens)
	; higher_order_or_annotation_tok(FirstToken) ->
	  remove_leading_hoa_ws_toks(RestTokens, InputMatchPhraseWords, FilteredTokens)
	; ws_tok(FirstToken) ->
	  remove_leading_hoa_ws_toks(RestTokens, InputMatchPhraseWords, FilteredTokens)
	; FilteredTokens = [FirstToken|RestTokens]
	).

remove_tokens_to_end_of_pe([], _PETokenEndPos, []).
remove_tokens_to_end_of_pe([FirstToken|RestTokens], PETokenEndPos, RemainingTokens) :-
	token_template(FirstToken, _TokenType, _TokenString, _LCTokenString, PosInfo1, _PosInfo2),
	PosInfo1 = pos(_FirstTokenStartPos, FirstTokenEndPos),
	( FirstTokenEndPos =< PETokenEndPos ->
	  remove_tokens_to_end_of_pe(RestTokens, PETokenEndPos, RemainingTokens)
	; RemainingTokens = [FirstToken|RestTokens]
	).

% First, try to find a token that appears no earlier than the previous token
get_word_token(Word, PrevTokenStartPos, RawTokensIn, Token, NewTokenStartPos, RestRawTokens) :-
	% mc == matching case
        matching_token(mc, Word, Token),
	select(Token, RawTokensIn, RestRawTokens),
	token_template(Token, _TokenType, _TokenString, _LCTokenString, _PosInfo1, PosInfo2),
	PosInfo2 = pos(NewTokenStartPos, _Length),
	NewTokenStartPos >= PrevTokenStartPos,
	!.
% If that doesn't work, then take any matching token.
get_word_token(Word, _PrevTokenStartPos, RawTokensIn, Token, NewTokenStartPos, RestRawTokens) :-
        matching_token(mc, Word, Token),
	select(Token, RawTokensIn, RestRawTokens),
	token_template(Token, _TokenType, _TokenString, _LCTokenString, _PosInfo1, PosInfo2),
	PosInfo2 = pos(NewTokenStartPos, _Length),
	!.
% If that still doesn't work, then make up a new token on the fly.
get_word_token(Word, _PrevTokenStartPos, RawTokensIn, CreatedToken, NewTokenStartPos, RawTokensOut) :-
	create_new_token(Word, RawTokensIn, CreatedToken, RawTokensOut),
	token_template(CreatedToken, _TokenType, _String, _LCString, _Pos1, pos(NewTokenStartPos,_)).

create_new_token(Word, RawTokensIn, CreatedToken, RawTokensOut) :-
	RawTokensIn = [NextToken|_],
	token_template(NextToken, _TokenType, _Text, _LCText,
		       pos(StartPos1, Length1), pos(StartPos2, Length2)),
	lower(Word, LowerWord),
	atom_codes(Word, WordString),
	atom_codes(LowerWord, LowerWordString),
	token_template(CreatedToken, xx, WordString, LowerWordString,
		       pos(StartPos1, Length1),
		       pos(StartPos2, Length2)),
	format(user_error, '### Created new token for ~w before ~p~n', [Word, NextToken]),
	remove_tokens_matching_word(RawTokensIn, WordString, RawTokensOut).

remove_tokens_matching_word([], _WordString, []).
remove_tokens_matching_word([FirstRawTokenIn|RestRawTokensIn], WordString, RawTokensOut) :-
	token_template(FirstRawTokenIn, _TokenType, Text, _LCText, _Pos1, _Pos2),
	append(Text, RemainingWordString, WordString),
	!,
	remove_tokens_matching_word(RestRawTokensIn, RemainingWordString, RawTokensOut).
remove_tokens_matching_word(RawTokensIn, _WordString, RawTokensOut) :- RawTokensOut = RawTokensIn.


% % Check to see if the next token matches the word.
% get_word_token(Word, _PrevTokenStartPos, [Token|RestRawTokens], Token, RestRawTokens) :-
%         matching_token(mc, Word, Token),
%         !.
% % Skip non-text tokens and ws tokens.
% get_word_token(Word, _PrevTokenStartPos, [TokenToSkip|Rest], Token, RestRawTokens) :-
%         ( higher_order_or_annotation_tok(TokenToSkip) ->
%           true
%         ; ws_tok(TokenToSkip) ->
%           true
%         ),
%         !,
%         get_word_token(Word, Rest, Token, RestRawTokens).


% If we get to this next clause, there's a problem, presumably because
% the positional information re-tokenization failed to handle
% either an aa or an aadef token and simply skipped over it.

% This next clause is also used in term processing, because the syntactic uninversion
% done by that option changes, e.g., "cancer, lung" to "lung cancer".

% If next real (i.e., text) token doesn't match,
% which is the case if we've made it to this clause,
% then look for a matching token in the next 4 tokens.
% If one is found, then use that matching token,
% and skip over all tokens up to the match;
% otherwise (i.e., no match in the next 4 tokens)
% create a token on the fly, using the two PI terms
% of the next token, which is presumably an AA,
% and the non-matching words in the input stream
% are its expansion.

% Suppose the text includes "patients with acquired immunodeficiency syndrome (AIDS)."
% and the PI tokenization fails to process the AA.
% We'd have
% PhraseWords = [acquired,immunodeficiency,syndrome]
% and
% RawTokensIn = [
%     tok(uc,"AIDS","aids",pos(246,250),pos(439,4)),
%     tok(pn,")",")",pos(250,251),pos(443,1)),
%     tok(ws," "," ",pos(251,252),pos(444,1)),
%     tok(lc,"and","and",pos(252,255),pos(445,3))
%     ...       ]

% We don't find a match for "acquired" in the next 4 tokens,
% so assume (hope, pray...) that "AIDS" is the intended match,
% and use the "AIDS" token's PI.
% 
% get_word_token(Word, _PrevTokenStartPos, RestTokens, MatchingToken, NewRestTokens) :-
%         % send_message('### WARNING: Token mismatch for "~w"; ', [Word]),
%         ( find_next_matching_token(5, Word, RestTokens, MatchingToken, NewRestTokens) ->
% 	  true
%           % format(user_output, ' FOUND matching token.~n', [])
%         ; create_new_token(Word, RestTokens, MatchingToken),
%           % format(user_output, ' CREATING new token.~n', []),
%           NewRestTokens = RestTokens
%         ).
% 
% find_next_matching_token(Count, Word, Tokens, MatchingToken, RemainingTokens) :-
%         Count > 0,
%         Tokens = [NextToken|RestTokens],
%         ( matching_token(mc, Word, NextToken) ->
%           MatchingToken = NextToken,
%           RemainingTokens = RestTokens
%         ; decrement_count_if_an_tok(NextToken, Count, NextCount),
%           RemainingTokens = [NextToken|RestRemainingTokens],
%           find_next_matching_token(NextCount, Word, RestTokens, MatchingToken, RestRemainingTokens)
%         ).
% 
% decrement_count_if_an_tok(NextToken, Count, NextCount) :-
% 	( an_tok(NextToken) ->
% 	  NextCount is Count - 1
% 	; NextCount is Count
% 	).
 
	
% If the phrase represents an aadef, e.g.,
%      heart attack (HA)
% the AA text itself "(HA)" has been lost at this point of processing,
% but it's recoverable from the AAs AVL tree.
% Here, we determine if the PhraseTokens represent an AA;
% this is done by matching the PhraseTokens against all the
% ExpansionTokens in the AA AVL tree.
% If there is a match, we must calculate how many characters
% should be added to the Positional Info to account for the AA itself--e.g.,"(HA)".
% We cannot do this by using the PosInfo in the AVL tree, because that PosInfo
% does not take into account the actual spacing such as the 6-character indentation
% at the beginning of all lines in Medline citations.
% To get that, we must look in the RawTokens coming after the PhraseTokens
% and extract the pe token that comes after the PhraseTokens. Ugh...
%
% AAs = 
% node([tok(uc,"CPPV","cppv",pos(63,67))],
%       [[tok(ic,"Continuous","continuous",pos(21,31)),
%         tok(ws," "," ",pos(31,32)),
% 	tok(lc,"positive","positive",pos(32,40)),
% 	tok(ws," "," ",pos(40,41)),
% 	tok(lc,"pressure","pressure",pos(41,49)),
% 	tok(ws," "," ",pos(49,50)),
% 	tok(lc,"ventilation","ventilation",pos(50,61))]],
%       0,empty,empty))
% 
% PhraseTokens = [
% tok(ic,"Continuous","continuous",pos(21,31),pos(198,10)),
% tok(lc,"positive","positive",pos(32,40),pos(209,8)),
% tok(lc,"pressure","pressure",pos(41,49),pos(218,8)),
% tok(lc,"ventilation","ventilation",pos(50,61),pos(227,11))]
% 
% 
% RemainingTokens is the list of tokens that comes after the AAdef token,
% so its position identifies the position after the AAdef.
 
filter_out_dvars([], []).
filter_out_dvars([gvc(G,Vs,Cs)|Rest], [gvc(G,FilteredVs,Cs)|FilteredRest]) :-
	filter_out_dvars_aux(Vs, FilteredVs),
	filter_out_dvars(Rest, FilteredRest).

filter_out_dvars_aux([], []).
filter_out_dvars_aux([First|Rest], Filtered) :-
	First = v(_,_,_,History,_,_),
	( memberchk(0'd, History) ->
	  FilteredRest = Filtered
	; Filtered = [First|FilteredRest]
	),
	filter_out_dvars_aux(Rest, FilteredRest).

% Remove all variants whose history contains either "a" (AA) or "e" (AA expansion)
filter_out_aas([], []).
filter_out_aas([gvc(G,Vs,Cs)|Rest], [gvc(G,FilteredVs,Cs)|FilteredRest]) :-
	filter_out_aas_aux(Vs, FilteredVs),
	filter_out_aas(Rest, FilteredRest).

filter_out_aas_aux([], []).
filter_out_aas_aux([FirstVariant|RestVariants], FilteredRest) :-
	FirstVariant = v(_Word,_VarLevel,_LexCat,History,_Roots,_NFR),
	( memberchk(0'a, History) ->
	  filter_out_aas_aux(RestVariants, FilteredRest)
	; memberchk(0'e, History) ->
	  filter_out_aas_aux(RestVariants, FilteredRest)
	; FilteredRest = [FirstVariant|Tail],
	  filter_out_aas_aux(RestVariants, Tail)
	).

% Given
% (1) a list of GVC terms of the form
%     gvc(Generator, _Variants, _Candidates)
%     where Generator is of the form
%     v(Word, VarLevel, LexicalCategories, History, Roots, _NFR), and
% (2) the phrase parse,
% Remove from the list all GVC terms whose combination of Word and LecicalCategories
% does not appear in the phrase parse. E.g.,

% GVCList = [gvc(v(hydrophobic,0,[noun],[],[hydrophobic],_103295),_103309,_103310),
% 	     gvc(v(hydrophobic,0,[adj],[],[hydrophobic],_103295),_103332,_103333),
% 	     gvc(v(core,0,[verb],[],[core],_103456),_103470,_103471),
% 	     gvc(v(core,0,[noun],[],[core],_103456),_103493,_103494)]
% 
% contains hydrophobic-noun, hydrophobic-adj, core-verb, and core-noun. 
% 
% Phrase = [mod([lexmatch([hydrophobic]),inputmatch([hydrophobic]),tag(adj),tokens([hydrophobic])]),
% 	    head([lexmatch([core]),inputmatch([core]),tag(noun),tokens([core])]),
% 	    punc([inputmatch(['.']),tokens([])])]
% 
% However, in the phrase hydrophobic is only an adj, and core is only a noun,
% so we keep only only the first and last GVC terms.

filter_variants_by_tags(GVCsIn, Phrase, GVCsOut) :-
	% get the tokens and tags in the phrase
	extract_tokens_with_tags(Phrase, ToksTags),
	% convert the format
	convert_tokens_tags(ToksTags, WordTags0),
	% discard those wordtags that do not occur in GVCsIn
	filter_word_tags(WordTags0, GVCsIn, WordTags),
	% discard those GVCs that do not occur in WordTags
	filter_variants_by_tags_aux(GVCsIn, WordTags, GVCsOut).

% By a lucky accident, the GVCs formed from the alternate split-word forms
% will be kept, because memberchk(wordtags(Word,Tags), WordTags) won't succeed for them!
filter_variants_by_tags_aux([], _, []).
filter_variants_by_tags_aux([First|Rest], WordTags, Result) :-
	First = gvc(v(Word,_,[Tag],_,_,_),_,_),
	% find_tags(WordTags, Word, Tags),
	memberchk(wordtags(Word,Tags), WordTags),
	!,
	( memberchk(Tag, Tags) ->
	  Result = [First|FilteredRest]
	; Result = FilteredRest
	),
	filter_variants_by_tags_aux(Rest, WordTags, FilteredRest).
filter_variants_by_tags_aux([First|Rest], WordTags, [First|FilteredRest]) :-
	filter_variants_by_tags_aux(Rest, WordTags, FilteredRest).

% find_tags([],_,_) :-
%     !,
%     fail.
% find_tags([wordtags(Word,Tags)|_], Word, Tags) :- !.
% find_tags([_|Rest],Word, Tags) :-
% 	find_tags(Rest, Word, Tags).

convert_tokens_tags(ToksTags, WordTags) :-
	convert_tokens_tags_aux(ToksTags, WordTags0),
	merge_word_tags(WordTags0, WordTags).

convert_tokens_tags_aux([], []).
convert_tokens_tags_aux([tokenstag(Tokens,Tag)|RestTokensTags],
                        [wordtags(Word,[Tag])|RestWordTags]) :-
	concat_atom(Tokens, ' ', Word),
	convert_tokens_tags_aux(RestTokensTags, RestWordTags).

% If a word appears in several wordtags/2 structures, merge the lexical categories
merge_word_tags([], []).
merge_word_tags([First|Rest], [wordtags(Word,Tags)|MergedRest]) :-
	First = wordtags(Word,[Tag]),
	find_wordtags(Rest, Word, RestTags, NewRest),
	Tags = [Tag|RestTags],
	merge_word_tags(NewRest, MergedRest).

find_wordtags([], _, [], []).
find_wordtags([wordtags(Word,[Tag])|Rest], Word, [Tag|Tags], NewRest) :-
	!,
	find_wordtags(Rest, Word, Tags, NewRest).
find_wordtags([First|Rest], Word, Tags, [First|NewRest]) :-
	find_wordtags(Rest, Word, Tags, NewRest).


filter_word_tags([], _, []).
filter_word_tags([First|Rest], GVCs, [First|FilteredRest]) :-
	First = wordtags(Word,Tags),
	occurs_in_gvcs(GVCs, Word, Tags),
	!,
	filter_word_tags(Rest, GVCs, FilteredRest).
filter_word_tags([_|Rest], GVCs, FilteredRest) :-
	filter_word_tags(Rest, GVCs, FilteredRest).

% occurs_in_gvcs([],_,_) :-
%     !,
%     fail.
occurs_in_gvcs([gvc(v(Word,_,[Tag],_,_,_),_,_)|_Rest], Word, Tags) :-
	memberchk(Tag, Tags),
	!.
occurs_in_gvcs([_|Rest], Word, Tags) :-
	occurs_in_gvcs(Rest, Word, Tags).

filter_numerical_evaluations_aux([], _NoNumSemTypes, []).
filter_numerical_evaluations_aux([FirstCandidate|RestCandidates], NoNumSemTypes, Results) :-
	get_all_candidate_features([metaterm,semtypes],
				   FirstCandidate,
				   [MetaTermAtom, CandidateSemTypes]),
	( matching_semtypes(NoNumSemTypes, CandidateSemTypes),
	  atom_codes(MetaTermAtom, MetaTermString),
	  mostly_digits(MetaTermString) ->
	  Results = RestResults
	; Results = [FirstCandidate|RestResults]
	),
	filter_numerical_evaluations_aux(RestCandidates, NoNumSemTypes, RestResults).

matching_semtypes(NoNumSemTypes, CandidateSemTypes) :-
	( intersection([all,'ALL'], NoNumSemTypes, Int),
	  Int \== [] ->
	  true
	; intersection(NoNumSemTypes, CandidateSemTypes, CommonSemTypes),
	    CommonSemTypes \== []
	).
	
mostly_digits(MetaTermString) :-
	\+ contains_alphabetic_char(MetaTermString),
	remove_whitespace_chars(MetaTermString, MetaTermStringNoWS),
	length(MetaTermStringNoWS, Length),
	count_digits(MetaTermStringNoWS, 0, DigitCount),
	DigitCount/Length > 0.49.

contains_alphabetic_char(MetaTermString) :-
	member(Code, MetaTermString),
	local_alpha(Code),
	!.

remove_whitespace_chars([], []).
remove_whitespace_chars([H|T], CharsWithNoWS) :-
	( local_ws(H) ->
	  CharsWithNoWS = RestCharsWithNoWS
	; CharsWithNoWS = [H|RestCharsWithNoWS]
	),
	remove_whitespace_chars(T, RestCharsWithNoWS).



count_digits([], DigitCount, DigitCount).
count_digits([H|T], DigitCountIn, DigitCountOut) :-
	( local_digit(H) ->
	  DigitCountNext is DigitCountIn + 1
	; DigitCountNext is DigitCountIn
	),
	count_digits(T, DigitCountNext, DigitCountOut).


% ev(-1000,'C0596170','BED','Binge eating disorder',[bed],[mobd],[[[1,1],[1,1],0]],[1],1,yes,no,[],[0/3],_247965,_247967)],
filter_evaluations_by_user_exclusions_aux([], _UserExclusions, []).
filter_evaluations_by_user_exclusions_aux([FirstCandidate|RestCandidates],
				      UserExclusions, Results) :-
	get_all_candidate_features([cui,metaterm], FirstCandidate, [CUI, MetaTerm]),
	% This *must* be a not-not to prevent the instantiation of
	% any variable in the UserExclusions, in case a NoMap pair
	% is specified with either the string or CUI as empty,
	% which has the effiect of matching anything.
	( \+ \+ memberchk(MetaTerm:CUI, UserExclusions) ->
          Results = RestResults
	; Results = [FirstCandidate|RestResults]
	),
        filter_evaluations_by_user_exclusions_aux(RestCandidates, UserExclusions, RestResults).

/* filter_evaluations_restricting_to_sources(+EvaluationsIn, +Sources, -EvaluationsOut)

filter_evaluations_restricting_to_sources/3 removes those evaluations from EvaluationsIn
which do not represent terms from Sources. It produces EvaluationsOut
*** REPLACING THE PREFERRED CONCEPT NAME WITH A NAME FROM Sources ***
where necessary. At least temporarily, when this is done the source of
the name is included in curly braces. */

filter_evaluations_restricting_to_sources([], _, []).
filter_evaluations_restricting_to_sources([FirstCandidate|RestCandidates],
					  RestrictRootSources, Results) :-
        candidate_term(NegValue, CUI, _MetaTerm, MetaConcept, MetaWords, SemTypes,
                       MatchMap, LSComponents, TargetLSComponent, InvolvesHead,
                       IsOvermatch, CUISources, PosInfo, Status, Negated, FirstCandidate),
	% convert_to_root_sources(CUISources, CUIRootSources),
	% The sources in the concept are guaranteed to be the root sources,
	% so there's no need to check here!
	upper_list(CUISources, CUISourcesUPPER), 
	intersection(RestrictRootSources, CUISourcesUPPER, CommonSources),
	  % If none of this CUI's sources are in the sources to restrict to, discard the candidate.
        ( CommonSources == [] ->
          Results = ModRest
	; db_get_cui_sourceinfo(CUI, CUISourceInfo0),
	  extract_relevant_sources(CUISourceInfo0, RestrictRootSources, CUISourceInfo),
	  extract_name_in_source(CUISourceInfo, ConceptNameInSource),
          candidate_term(NegValue, CUI, ConceptNameInSource, MetaConcept, MetaWords, SemTypes,
                         MatchMap, LSComponents, TargetLSComponent, InvolvesHead,
                         IsOvermatch, CommonSources, PosInfo, Status, Negated, NewCandidate),
          Results = [NewCandidate|ModRest]
        ),
        filter_evaluations_restricting_to_sources(RestCandidates, RestrictRootSources, ModRest).

/* filter_evaluations_excluding_sources(+EvaluationsIn, +Sources,
                                        -EvaluationsOut)

filter_evaluations_excluding_sources/3 removes those evaluations from
EvaluationsIn which only represent terms from Sources. It produces
EvaluationsOut
*** REPLACING THE PREFERRED CONCEPT NAME IF IT IS FROM ONE OF Sources ***
where necessary.
filter_evaluations_excluding_sources_aux/3 performs the actual work on the
RootSources for Sources. */

filter_evaluations_excluding_sources([], _, []).
filter_evaluations_excluding_sources([FirstCandidate|RestCandidates], ExcludedRootSources, Results) :-
	candidate_term(NegValue, CUI, MetaTerm, _MetaConcept, MetaWords, SemTypes,
		       MatchMap, LSComponents, TargetLSComponent, InvolvesHead,
		       IsOvermatch, CUISources, PosInfo, Status, Negated, FirstCandidate),
	db_get_cui_sourceinfo(CUI, CUISourceInfo),
	get_string_sources(CUISourceInfo, MetaTerm, StringSources),
	convert_to_root_sources(StringSources, StringRootSources),
	upper_list(StringRootSources, StringRootSourcesUPPER),
	% remove_non_string_sources(StringRootSources, CUISourceInfo, StringSourceInfo),
	% RemainingStringSources is the result of deleting
	% all elements of ExcludedRootSources from StringRootSources
	difference(ExcludedRootSources, StringRootSourcesUPPER, RemainingStringSources),
	  % If all of this CUI's sources are excluded sources, then discard the candidate.
	( RemainingStringSources == [] ->
	  Results = RestResults
        ; convert_to_root_sources(CUISources, CUIRootSources),
	  db_get_cui_sourceinfo(CUI, CUISourceInfo),
	  extract_nonexcluded_sources(CUISourceInfo, ExcludedRootSources, ExtractedSourceInfo),
	  difference(ExcludedRootSources, CUIRootSources, RemainingCUISources),
	  extract_name_in_source(ExtractedSourceInfo, ConceptNameInSource),
          candidate_term(NegValue, CUI, MetaTerm, ConceptNameInSource, MetaWords, SemTypes,
                         MatchMap, LSComponents, TargetLSComponent, InvolvesHead,
                         IsOvermatch, RemainingCUISources, PosInfo, Status, Negated, NewCandidate),
          Results = [NewCandidate|RestResults]
        ),
        filter_evaluations_excluding_sources(RestCandidates, ExcludedRootSources, RestResults).


get_string_sources([], _MetaTerm, []).
get_string_sources([CUISourceInfo|RestCUISourceInfo], MetaTerm, StringSources) :-
	( CUISourceInfo = [_I,MetaTerm,StringRootSource,_TTY] ->
	  StringSources = [StringRootSource|RestStringSources]
	; StringSources = RestStringSources
	),
	get_string_sources(RestCUISourceInfo, MetaTerm, RestStringSources).

% filter_evaluations_excluding_sources([], _, []).
% filter_evaluations_excluding_sources([FirstCandidate|RestCandidates], ExcludedRootSources, Results) :-
% 	candidate_term(NegValue, CUI, MetaTerm, _MetaConcept, MetaWords, SemTypes,
% 		       MatchMap, LSComponents, TargetLSComponent, InvolvesHead,
% 		       IsOvermatch, CUISources, PosInfo, Status, Negated, FirstCandidate),
% 	db_get_cui_sourceinfo(CUI, CUISourceInfo0),
% 	extract_nonexcluded_sources(CUISourceInfo0, ExcludedRootSources, CUISourceInfo),
% 	% get_string_sources(CUISourceInfo, MetaTerm, StringSources),
% 	% convert_to_root_sources(StringSources, StringRootSources),
% 	% remove_non_string_sources(StringRootSources, CUISourceInfo, StringSourceInfo),
% 	% RemainingStringSources is the result of deleting
% 	% all elements of ExcludedRootSources from StringRootSources
% 	% difference(ExcludedRootSources, StringRootSources, RemainingStringSources),
% 	% If all of this CUI's sources are excluded sources, then discard the candidate.
% 	( CUISourceInfo == [] ->
% 	  Results = RestResults
%         ; convert_to_root_sources(CUISources, CUIRootSources),
% 	%  db_get_cui_sourceinfo(CUI, CUISourceInfo),
% 	%  extract_nonexcluded_sources(CUISourceInfo, ExcludedRootSources, ExtractedSourceInfo),
% 	  difference(ExcludedRootSources, CUIRootSources, RemainingCUISources),
% 	  extract_name_in_source(CUISourceInfo, ConceptNameInSource),
%           candidate_term(NegValue, CUI, MetaTerm, ConceptNameInSource, MetaWords, SemTypes,
%                          MatchMap, LSComponents, TargetLSComponent, InvolvesHead,
%                          IsOvermatch, RemainingCUISources, PosInfo, Status, Negated, NewCandidate),
%           Results = [NewCandidate|RestResults]
%         ),
%         filter_evaluations_excluding_sources(RestCandidates, ExcludedRootSources, RestResults).

% remove_non_string_sources([], StringSourceInfo, StringSourceInfo).
% remove_non_string_sources([StringRootSource|RestStringRootSources],
% 			  StringSourceInfoIn, StringSourceInfoOut) :-
% 	del_element([_I,_ConceptNameInSource,StringRootSource,_TTY],
% 		    StringSourceInfoIn, StringSourceInfoNext),
% 	remove_non_string_sources(RestStringRootSources, StringSourceInfoNext, StringSourceInfoOut).

difference([], Rest, Rest).
difference([H|T], Set0, Difference) :-
	del_element(H, Set0, Set1),
	difference(T, Set1, Difference).

/* filter_evaluations_restricting_to_sts(+EvaluationsIn, +STs, -EvaluationsOut)

filter_evaluations_restricting_to_sts/3 removes those evaluations from EvaluationsIn
which do not represent concepts with some ST in STs producing EvaluationsOut. */

filter_evaluations_restricting_to_sts([], _, [], []).
filter_evaluations_restricting_to_sts([FirstCandidate|RestCandidates], STs,
				      KeptEvaluations, DiscardedEvaluations) :-
	get_candidate_feature(semtypes, FirstCandidate, FirstCandidateSemTypes),
	intersection(FirstCandidateSemTypes, STs, Intersection),
	( Intersection == [] ->
	  KeptEvaluations = RestKeptEvaluations,
	  DiscardedEvaluations = [FirstCandidate|RestDiscardedEvaluations]	  
	; KeptEvaluations = [FirstCandidate|RestKeptEvaluations],
	  DiscardedEvaluations = RestDiscardedEvaluations
	),
	filter_evaluations_restricting_to_sts(RestCandidates, STs,
					      RestKeptEvaluations, RestDiscardedEvaluations).

/* filter_evaluations_excluding_sts(+EvaluationsIn, +STs, -EvaluationsOut)

filter_evaluations_excluding_sts/3 removes those evaluations from
EvaluationsIn which represent concepts with any ST in STs producing
EvaluationsOut. */

filter_evaluations_excluding_sts([], _, [], []).
filter_evaluations_excluding_sts([FirstCandidate|RestCandidates], STs,
				 KeptEvaluations, DiscardedEvaluations) :-
	get_candidate_feature(semtypes, FirstCandidate, FirstCandidateSemTypes),
	intersection(FirstCandidateSemTypes, STs, Intersection),
	( Intersection == [] ->
	  KeptEvaluations = [FirstCandidate|RestKeptEvaluations],
	  DiscardedEvaluations = RestDiscardedEvaluations
	; KeptEvaluations = RestKeptEvaluations,
	  DiscardedEvaluations = [FirstCandidate|RestDiscardedEvaluations]
	),
	filter_evaluations_excluding_sts(RestCandidates, STs,
					 RestKeptEvaluations, RestDiscardedEvaluations).

% construct_best_mappings(Evaluations, PhraseTextString, Phrase, PhraseWordInfoPair,
% 			_Variants, APhrases, Evaluations, BestMaps, PrunedCount) :-
% 	% special clause for detecting structures such as
% 	% amino acid sequences (e.g., His-Ala-Asp-Gly-...); and
% 	% other hyphenated structures (e.g., (D)-C-Q-W- A-V-G-H-L-C-NH2)
% 	% to prevent computing mappings
% 	length(Phrase, PhraseLength),
% 	PhraseLength > 5,
% 	PhraseWordInfoPair=_:pwi(wdl(_,LCFilteredWords),_,_),
% 	apply_shortcut_processing_rules(LCFilteredWords, PhraseTextString),
% 	!,
% 	% AllMappings = [],
% 	PrunedCount is 0,
% 	APhrases = [],
% 	BestMaps = [].
% 	% construct_best_mappings_1(Phrase, PhraseWordInfoPair, AllMappings,
% 	%			  Variants, APhrases, BestMaps).
construct_best_mappings(Evaluations, PhraseTextString, Phrase, PhraseWordInfoPair, NPhraseWords,
			Variants, APhrases0, _BestCandidates, BestMaps0, PrunedCount) :-
	% consider doing construction, augmentation and filtering more linearly
	% to increase control and to avoid keeping non-optimal results when
	% control_option(compute_all_mappings) is not in effect
	( check_construct_best_mappings_control_options ->
	  debug_call(trace, length(Evaluations, EvaluationsLength)), 
	  debug_message(trace,
			'~n### Calling construct_all_mappings on ~w AEvs~n',
			[EvaluationsLength]),
	  % write_aevs(AEvaluations),
	  compute_phrase_words_length(PhraseWordInfoPair, NPhraseWords),
	  construct_all_mappings(Evaluations, PhraseTextString, NPhraseWords, Variants,
				 _BestAEvs, DuplicateCandidates, AllMappings, PrunedCount),
	  % construct_all_mappings_OLD(AEvaluations, AllMappings)
	  % user:'=?'(AllMappings, OldAllMappings),
	  debug_call(trace, length(AllMappings, AllMappingsLength)),
	  debug_message(trace, '~n### DONE with construct_all_mappings: ~w~n', [AllMappingsLength])
	; AllMappings = []
	),
	debug_message(mappings, '~N### Adding duplicate candidates to mappings~n', []),
	% The next three predicates need to be merged!
	% AllMappings contains ev(_) structures, but DuplicateCandidates contains aev(_)s.
	add_dup_candidates_to_all_mappings(AllMappings, DuplicateCandidates, AllMappingsWithDups),
	debug_message(mappings, '~N### Distributing duplicate candidates over mappings~n', []),
	% I can avoid the next append by using difference lists in distribute_duplicate_candidates
	distribute_duplicate_candidates(AllMappingsWithDups, AllDistributedMappings0),
	append(AllDistributedMappings0, AllDistributedMappings),
	length(AllDistributedMappings, AllDistributedMappingsCount),
	debug_message(mappings, '~N### ~d Mappings with Duplicates.~n', [AllDistributedMappingsCount]),
	test_mappings_limit(AllDistributedMappingsCount),
	% Cut away all the choice points left by the pruning code!
	!,
	construct_best_mappings_1(Phrase, PhraseWordInfoPair, AllDistributedMappings,
				  Variants, APhrases0, BestMaps0).
%	maybe_filter_mappings_by_semtypes(BestMaps0, APhrases0,
%					  NPhraseWords, Variants,
%					  BestMaps, APhrases).

% filter_mappings_by_semtypes(BestMaps, APhrases, BestMaps, APhrases).

% maybe_reduce_mappings_for_conj(Mappings2, Mappings2, APhrases2, Mappings, APhrases)
maybe_reduce_mappings_for_conj(Mappings0, Mappings0, APhrases0,
			       NPhraseWords, Variants,
			       Mappings, APhrases) :-
	( \+ control_option(conj) ->
	  Mappings = Mappings0,
	  APhrases = APhrases0
	; reduce_mappings_for_conj(Mappings0, Mappings0, APhrases0,
				   NPhraseWords, Variants,
				   Mappings1, APhrases1),
	  % Must NOT call sort/2 because order must be preserved!
	  remove_duplicates(Mappings1, Mappings2),
	  ( Mappings2 = [map(MappingNegScore, _Map)|_] ->
	    update_mappings_scores_to_best_score(Mappings2, MappingNegScore, Mappings)
	  ; Mappings = []
	  ),
	  remove_duplicates(APhrases1, APhrases2),
	  ( APhrases2 = [ap(APhraseNegScore,_LPhrase,_LPhraseMap,_Mapping)|_] ->
	    update_aps_scores_to_best_score(APhrases2, APhraseNegScore, APhrases)
	  ; APhrases = []
	  )
	).		  

update_mappings_scores_to_best_score([], _NegScore, []).
update_mappings_scores_to_best_score([FirstMapping|RestMappings],
				     BestNegScore,
				     [ReScoredFirstMapping|ReScoredRestMappings]) :-
	FirstMapping = map(_OrigScore, ListOfConcepts),
	ReScoredFirstMapping = map(BestNegScore, ListOfConcepts),
	update_mappings_scores_to_best_score(RestMappings, BestNegScore, ReScoredRestMappings).

update_aps_scores_to_best_score([], _NegScore, []).
update_aps_scores_to_best_score([FirstAPhrase|RestAPhrases],
				BestNegScore,
				[ReScoredFirstAPhrase|ReScoredRestAPhrases]) :-
	FirstAPhrase = ap(_OrigNegScore,OrigLPhrase,LPhraseMap,Mapping),
	OrigLPhrase = [confid(_OrigConfidence)|RestLPhraseIn],
	ReScoredLPhrase = [confid(BestNegScore)|RestLPhraseIn],
	ReScoredFirstAPhrase = ap(BestNegScore,ReScoredLPhrase,LPhraseMap,Mapping),
	update_aps_scores_to_best_score(RestAPhrases, BestNegScore, ReScoredRestAPhrases).

% The initial implementation of --conj included all ancillary concepts.
% E.g., "lung and liver cancer" (with WSD) generated
% 1. Meta Mapping (824):
%    902   Lung cancer (Primary malignant neoplasm of lung) [neop]
%    645   LIVER (Liver) [bpoc]
% 2. Meta Mapping (824):
%    645   LUNG (Structure of parenchyma of lung) [tisu]
%    861   Cancer, Liver (Malignant neoplasm of liver) [neop]

reduce_mappings_for_conj([], _Mappings, [], _NPhraseWords, _Variants, [], []).
reduce_mappings_for_conj([FirstMapping|RestMappings], AllMappings,
			 [FirstAPhrase|RestAPhrases],
			 NPhraseWords, Variants,
			 FilteredMappings, FilteredAPhrases) :-
	FirstMapping = map(_Score, MappingEvaluations),
	filter_narrower_evals(MappingEvaluations, AllMappings, FilteredEvaluations),
	% If no candidates are left in this mapping, discard the mapping altogether.
	% This should not happen, but just in case, check for it!
	( FilteredEvaluations == [] ->
	  FilteredMappings = RestFilteredMappings,
	  FilteredAPhrases = RestFilteredAPhrases
	; Conj = 0,
	  % Compute a confidence value for the reduced mapping the traditional way.
	  compute_confidence_value(FilteredEvaluations, Conj, NPhraseWords, Variants, ModScore),
	  ModifiedFirstMapping = map(ModScore, FilteredEvaluations),
	  FilteredMappings = [ModifiedFirstMapping|RestFilteredMappings],
	  modify_aphrase(FirstAPhrase, FilteredEvaluations, ModScore, ModifiedFirstAPhrase),
	  FilteredAPhrases = [ModifiedFirstAPhrase|RestFilteredAPhrases]
	),
	reduce_mappings_for_conj(RestMappings, AllMappings, RestAPhrases,
				 NPhraseWords, Variants,
				 RestFilteredMappings, RestFilteredAPhrases).

% In the revised version of conj, requested by Halil and OK-ed by Jim,
% we discard all evals (i.e., candidates) in FirstMapping whose LSComponent
% (matched words) is a proper subset of an eval in any other mapping.
% For example, in "lung and liver cancer" (see above),
% "LIVER" in mapping 1 is a proper subset of "Cancer, Liver" in mappping 2.
% and
% "LUNG" in mapping 2 is a proper subset of "Lung Cancer" in mappping 1.
% So discard both those concepts, leaving only
% 1. Meta Mapping (824):
%    902   Lung cancer (Primary malignant neoplasm of lung) [neop]
% 2. Meta Mapping (824):
%    861   Cancer, Liver (Malignant neoplasm of liver) [neop]
% Finally, wsdmod.pl constructs the supermapping, which consists of
% only the remaining concepts,
% "Lung cancer" and "Cancer, Liver" in this example.


filter_narrower_evals([], _AllMappings, []).
filter_narrower_evals([Eval1|RestEvals], AllMappings, FilteredEvaluations) :-
	get_candidate_feature(lscomponents, Eval1, LSComponents1),
	( eval_is_narrow(AllMappings, LSComponents1) ->
	  FilteredEvaluations = RestFilteredEvaluations
	; FilteredEvaluations = [Eval1|RestFilteredEvaluations]
	),
	filter_narrower_evals(RestEvals, AllMappings, RestFilteredEvaluations).

% eval_is_narrow/2 must not have a base case with a [] first arg!
eval_is_narrow([FirstMapping|RestMappings], LSComponents1) :-
	( FirstMapping = map(_Score, MappingEvaluations),
	  member(Eval2, MappingEvaluations),
	  get_candidate_feature(lscomponents, Eval2, LSComponents2),
	  proper_subset(LSComponents1, LSComponents2) ->
	  true
	; eval_is_narrow(RestMappings, LSComponents1)
	).
	
% keep_best_evaluations_only([], _AllEvaluations, []).
% keep_best_evaluations_only([Eval1|RestEvals], AllEvaluations, BestEvaluations) :-
% 	get_candidate_feature(lscomponents, Eval1, LSComponents1),
% 	( member(Eval2, AllEvaluations),
% 	  get_candidate_feature(lscomponents, Eval2, LSComponents2),
% 	  proper_subset(LSComponents1, LSComponents2) ->
% 	  BestEvaluations = RestBestEvaluations
% 	; BestEvaluations = [Eval1|RestBestEvaluations]
% 	),
% 	keep_best_evaluations_only(RestEvals, AllEvaluations, RestBestEvaluations).

maybe_filter_mappings_by_semtypes(Mappings0, APhrases0, NPhraseWords,
				  Variants, Mappings, APhrases) :-
	( \+ control_option(restrict_to_sts),
	  \+ control_option(exclude_sts) ->
	  Mappings = Mappings0,
	  APhrases = APhrases0
	; filter_mappings_by_semtypes(Mappings0, APhrases0, NPhraseWords,
				      Variants, Mappings1, APhrases1),
	  % Must NOT call sort/2 because order must be preserved!
	  remove_duplicates(Mappings1, Mappings),
	  remove_duplicates(APhrases1, APhrases)
	    
	).		  

filter_mappings_by_semtypes([], [], _Variants, _NPhraseWords, [], []).
filter_mappings_by_semtypes([FirstMapping|RestMappings], [FirstAPhrase|RestAPhrases],
			    NPhraseWords, Variants,
			    FilteredMappings, FilteredAPhrases) :-
	FirstMapping = map(_Score, MappingEvaluations),
	filter_evaluations_by_semtypes(MappingEvaluations, FilteredEvaluations),
	  % If no candidates are left in this mapping, discard the mapping altogether
	( FilteredEvaluations == [] ->
	  FilteredMappings = RestFilteredMappings,
	  FilteredAPhrases = RestFilteredAPhrases
	; Conj = 0,
	    compute_confidence_value(FilteredEvaluations, Conj, NPhraseWords, Variants, ModScore),
	  ModifiedFirstMapping = map(ModScore, FilteredEvaluations),
	  FilteredMappings = [ModifiedFirstMapping|RestFilteredMappings],
	  modify_aphrase(FirstAPhrase, FilteredEvaluations, ModScore, ModifiedFirstAPhrase),
	  FilteredAPhrases = [ModifiedFirstAPhrase|RestFilteredAPhrases]
	),
	filter_mappings_by_semtypes(RestMappings, RestAPhrases,
				    NPhraseWords, Variants,
				    RestFilteredMappings, RestFilteredAPhrases).
	% deaugment_evaluations(BestAEvs, BestCandidates).

modify_aphrase(APhrase, FilteredEvaluations, ModScore, ModifiedAPhrase) :-
	APhrase = ap(_OrigScore,LPhraseOut,LPhraseMapOut,_OrigMapping),
	LPhraseOut = [_OrigConfid|OrigSyntax],
	modify_lphrase_syntax(OrigSyntax, FilteredEvaluations, ModSyntax),
	ModLPhraseOut = [confid(ModScore)|ModSyntax],
	ModifiedAPhrase = ap(ModScore,ModLPhraseOut,LPhraseMapOut,FilteredEvaluations).


% The Syntax elements are terms like these:
% mod([lexmatch([stent]),inputmatch([stent]),tag(noun),tokens([stent]),metaconc('Stent Device Component':'C1705817':[medd])])
% head([lexmatch([inferior,'vena caval',filter]),inputmatch([inferior,vena,caval,filter]),tag(noun),tokens([inferior,vena,caval,filter]),metaconc('Vena Cava Filters':'C0080306':[medd])])

% We want to discard syntax elements that contain a metaconc(_:CUI:_) structure
% whose CUI is NOT the CUI in a concept in FilteredEvaluations.

modify_lphrase_syntax([], _FilteredEvaluations, []).
modify_lphrase_syntax([FirstSyntaxElement|RestSyntaxElements],
		      FilteredEvaluations, ModLPhraseOut) :-
	arg(1, FirstSyntaxElement, FeatureList),
	( memberchk(metaconc(_MetaConcept:CUI:_SemTypes), FeatureList) ->
	    % If FirstSyntaxElement contains a metaconc(_) term,
	    % check if the CUI in the metaconc(_) structure appears in FilteredEvaluations.
	    % If it does, keep FirstSyntaxElement; otherwise, discard it.
	    ( member(Candidate, FilteredEvaluations),
	      get_candidate_feature(cui, Candidate, CUI) ->
	      ModLPhraseOut = [FirstSyntaxElement|RestModLPhraseOut]
	    ; ModLPhraseOut = RestModLPhraseOut
	    )
	  % If FirstSyntaxElement contains no metaconc(_) term, just pass it through
	  ; ModLPhraseOut = [FirstSyntaxElement|RestModLPhraseOut]
	),
	modify_lphrase_syntax(RestSyntaxElements, FilteredEvaluations, RestModLPhraseOut).

test_mappings_limit(MappingsCount) :-
	  % If no_prune is not set, then determine the max allowable number of mappings
	( \+ control_option(no_prune) ->
	   ( control_value(mappings_limit, MaxMappingsCount) ->
	     true
	   ; MaxMappingsCount is 500000
	   ),
	   MappingsCount < MaxMappingsCount
	  % If no_prune is set, simply succeed.	
	; true
	).	

compute_phrase_words_length(PhraseWordInfoPair, NPhraseWords) :-
	  PhraseWordInfoPair = _AllPhraseWordInfo:FilteredPhraseWordInfo,
	  % PhraseWordInfo = FilteredPhraseWordInfo,
	  FilteredPhraseWordInfo = pwi(PhraseWordL, _PhraseHeadWordL, _PhraseMap),
	  PhraseWordL = wdl(_, LCPhraseWords),
	  length(LCPhraseWords, NPhraseWords).
	

% apply_shortcut_processing_rules(LCFilteredWords, _PhraseTextString) :-
%	contains_n_amino_acids(LCFilteredWords, 0, 3).

% apply_shortcut_processing_rules(LCFilteredWords, PhraseTextString) :-
% 	( contains_n_amino_acids(LCFilteredWords, 3) ->
% 	  true
% 	; text_contains_n_hyphens_within_word(PhraseTextString, 4)
% 	).

construct_best_mappings_1(Phrase, PhraseWordInfoPair, AllMappings,
			  Variants, APhrases, BestMappings) :-
	debug_message(mappings, '~N### Calling augment_phrase_with_mappings~n', []),
 	sort(AllMappings, SortedAllMappings),
	augment_phrase_with_mappings(SortedAllMappings, Phrase,
				     PhraseWordInfoPair, Variants, APhrases0),
	% The sort is now done inside augment_phrase_with_mappings
	% sort(APhrases0,APhrases1),
	debug_message(mappings, '~N### Calling conditionally_filter_best_aphrases~n', []),
	conditionally_filter_best_aphrases(APhrases0, APhrases),
	% Aphrases is a list of terms of the form
	% ap(NegValue,LPhraseOut,LPhraseMapOut,Mapping).
	aphrases_maps(APhrases, BestMappings0),
	sort(BestMappings0, BestMappings),
	% BestMappings = BestMappings0,
	debug_call(mappings, length(BestMappings, BestMappingsCount)),
	debug_message(mappings, '~n### ~d Best Mappings~n', [BestMappingsCount]),
	debug_message(mappings, '~N### Done with aphrases_maps~n', []).

% Given a mapping consisting of N candidates
% [C1,C2,...,CN]
% and a list of duplicate concepts
% [C1:[C1D1,C1D2,...], C2:[C2D1,C2D2,...],...,CN:[CND1,CND2,...]]
% in which [C1D1,C1D2,...] are the duplicate concepts of C1, etc.,
% we want to replace each concept in the mapping
% with a list containing that concept and all its duplicates:
% The above mapping would be transformed into
% [[C1,C1D1,C1D2...], [C2,C2D1,C2D2,...],...,[CN,CND1,CND2,...]].

% For a simpler example, let's represent candidates as single chars.
% The list [a,b,c] therefore represents a mapping consisting of
% the three candidates a, b, and c.

% Suppose we have list of five mappings
% [ [a,b,c],  [a,c,e],  [a,b,d],  [b,c,d],  [b,c,e]]
% and the list of duplicate candidates is
% [a:[h,i,j],  c:[p,q,r],  d:[x,y,z]]
% thus,
% * a's duplicates are h, i, and j;
% * c's duplicates are p, q, and r; and
% * d's duplicates are x, y, and z.
% We first replace each concept in the mapping
% with a list containing that concept and all its duplicates (if any):
% That results in
% [ [[a,h,i,j], [b],       [c,p,q,r]],
%   [[a,h,i,j], [c,p,q,r], [e]],
%   [[a,h,i,j], [b],       [d,x,y,z]],
%   [[b],       [c,p,q,r], [d,x,y,z]],
%   [[b],       [c,p,q,r], [e]]        

% Then we distribute all the duplicate candidates to get
% [ [[a,b,c], [a,b,p], [a,b,q], [a,b,r], [h,b,c], [h,b,p], [h,b,q], [h,b,r],
%    [i,b,c], [i,b,p], [i,b,q], [i,b,r], [j,b,c], [j,b,p], [j,b,q], [j,b,r]],
%
%   [[a,c,e], [a,p,e], [a,q,e], [a,r,e], [h,c,e], [h,p,e], [h,q,e], [h,r,e],
%    [i,c,e], [i,p,e], [i,q,e], [i,r,e], [j,c,e], [j,p,e], [j,q,e], [j,r,e]],
%
%   [[a,b,d], [a,b,x], [a,b,y], [a,b,z], [h,b,d], [h,b,x], [h,b,y], [h,b,z],
%    [i,b,d], [i,b,x], [i,b,y], [i,b,z], [j,b,d], [j,b,x], [j,b,y], [j,b,z]],
%
%   [[b,c,d], [b,c,x], [b,c,y], [b,c,z], [b,p,d], [b,p,x], [b,p,y], [b,p,z],
%    [b,q,d], [b,q,x], [b,q,y], [b,q,z], [b,r,d], [b,r,x], [b,r,y], [b,r,z]],
%
%   [[b,c,e], [b,p,e], [b,q,e], [b,r,e]]]

% AllMappings is a list of terms of the form NegValue-[Candidate1, C2, ..., CN]
% and the candidate list is a mapping.
% DupCandidateList is a list of terms of the form Candidate:[Dup1, Dup2, ..., DupN]
% AllMappingsWithDups is a list of terms of the form NegValue-[DuplicateList1, DL2, ..., DLN]
% where DuplicateList1 contains all the duplicate candidates for Candididate1, etc.
add_dup_candidates_to_all_mappings(AllMappings, DupCandidateList, AllMappingsWithDups) :-
	(  foreach(NegValue-Mapping,         AllMappings),
	   foreach(NegValue-MappingWithDups, AllMappingsWithDups),
	   param(DupCandidateList)
	do (  foreach(Candidate,         Mapping),
	      foreach(CandidateWithDups, MappingWithDups),
	      param(DupCandidateList)
	   do ( memberchk(Candidate:Dups, DupCandidateList) ->
		CandidateWithDups = [Candidate|Dups]
	      ; CandidateWithDups = [Candidate]
	      )
	   )
	).

% As above,
% AllMappingsWithDups is a list of terms of the form NegValue-[DuplicateList1, DL2, ..., DLN]
% where DuplicateList1 contains all the duplicate candidates for Candididate1, etc.
distribute_duplicate_candidates(AllMappingsWithDups, AllDistributedMappings) :-
	(  foreach(NegValue-OneMappingWithDups,    AllMappingsWithDups),
	   foreach(OneDistributedMapping, AllDistributedMappings)
	do distribute_candidates(OneMappingWithDups, NegValue, OneDistributedMapping)
	).

% dist([[a,h,i,j], [b], [c,p,q,r]], Result).
% Result = [[a,b,c], [a,b,p], [a,b,q], [a,b,r],
%           [h,b,c], [h,b,p], [h,b,q], [h,b,r],
%	    [i,b,c], [i,b,p], [i,b,q], [i,b,r],
%	    [j,b,c], [j,b,p], [j,b,q], [j,b,r]]

% Distribute the candidates for ONE mapping.
distribute_candidates([H|T], NegValue, Dist) :- dist1(T, H, NegValue, Dist).

% Can this be made tail recursive?
dist1([], Last, NegValue, Dist) :- listify(Last, NegValue, Dist).
dist1([Next|Rest], First, NegValue, Dist) :-
	dist1(Rest, Next, NegValue, Dist1),
	prefix_all_lists(First, NegValue, Dist1, Dist).

% listify([[a,b,c,d], List) instantiates List to [[a], [b], [c], [d]].
listify([], _NegValue, []).
listify([First|Rest], NegValue, [NegValue-[First]|ModifiedRest]) :-
        listify(Rest, NegValue, ModifiedRest).

% prefix_all_lists(ListOfNewHeads, ListOfLists, NewListofLists)
% add each element of ListOfNewHeads as a new head to each list in ListOfLists,
% resulting in NewListsOfLists, e.g.,
% prefix_all_lists([#,*], [[a,b,c], [d,e,f], [g,h,i]], NewListOfLists).
% instatiates NewList to [[#,a,b,c],[#,d,e,f],[#,g,h,i],[*,a,b,c],[*,d,e,f],[*,g,h,i]].

prefix_all_lists(ListOfNewHeads, NegValue, ListOfLists, NewListofLists) :-
	prefix_all_lists_aux(ListOfNewHeads, NegValue, ListOfLists, NewListofLists, []).

prefix_all_lists_aux([], _NegValue, _ListOfLists, Result, Result).
prefix_all_lists_aux([H|T], NegValue, List, ResultIn, ResultOut) :-
	prefix_one_element(List, H, NegValue,ResultIn, ResultNext),
	prefix_all_lists_aux(T, NegValue, List, ResultNext, ResultOut).

prefix_one_element([], _First, _NegValue, Result, Result).
prefix_one_element([NegValue-H|T], First, NegValue, [NegValue-[First|H]|ResultNext], ResultOut) :-
	prefix_one_element(T, First, NegValue, ResultNext, ResultOut).

conditionally_filter_best_aphrases([], []).
conditionally_filter_best_aphrases([H|T], APhrases) :-
	APhrases0 = [H|T],
	( \+ control_option(compute_all_mappings) ->
	  H = ap(BestValue,_,_,_),
	  filter_best_aphrases(APhrases0, BestValue, APhrases)
	; APhrases = APhrases0
	).	

% contains_n_amino_acids(LCWords, N) :-
% 	contains_n_amino_acids(LCWords, 0, N).

% contains_n_amino_acids([First|Rest], I, N) :-
% 	( I =:= N ->
% 	  true
% 	; is_an_amino_acid(First) ->
% 	  J is I + 1,
% 	  contains_n_amino_acids(Rest, J, N)
% 	; contains_n_amino_acids(Rest, I, N)
% 	).
% 
% is_an_amino_acid(ala).
% is_an_amino_acid(arg).
% is_an_amino_acid(gly).
% is_an_amino_acid(leu).
% is_an_amino_acid(glu).
% is_an_amino_acid(lys).
% is_an_amino_acid(asp).
% is_an_amino_acid(ser).
% is_an_amino_acid(asn).
% is_an_amino_acid(gln).
% is_an_amino_acid(tyr).
% is_an_amino_acid(phe).
% is_an_amino_acid(ile).
% is_an_amino_acid(thr).
% is_an_amino_acid(pro).
% is_an_amino_acid(val).
% is_an_amino_acid(cys).
% is_an_amino_acid(his).
% is_an_amino_acid(met).
% is_an_amino_acid(trp).

% text_contains_n_hyphens_within_word(PhraseText, N) :-
% 	( atom(PhraseText) ->
% 	  atom_codes(PhraseText, PhraseString)
% 	; PhraseString = PhraseText
% 	),
% 	contains_n_hyphens_within_word(PhraseString, 0, N).
% 
% contains_n_hyphens_within_word(_,N,N) :-
%     !.
% contains_n_hyphens_within_word("",_,_) :-
%     !,
%     fail.
% contains_n_hyphens_within_word([0'-|Rest],I,N) :-
%     !,
%     J is I+1,
%     contains_n_hyphens_within_word(Rest,J,N).
% contains_n_hyphens_within_word([0' |Rest],_I,N) :-
%     !,
%     contains_n_hyphens_within_word(Rest,0,N).
% contains_n_hyphens_within_word([_|Rest],I,N) :-
%     contains_n_hyphens_within_word(Rest,I,N).

augment_evaluations([], []).
augment_evaluations([First|Rest], [AugmentedFirst|AugmentedRest]) :-
	augment_one_evaluation(First, AugmentedFirst),
	augment_evaluations(Rest, AugmentedRest).

% augmenting an evaluation
% * changes the functor from ev/11 to aev/14,
% * adds the merged phrase component of the MatchMap, and
% * adds the span of the entire MatchMap.
augment_one_evaluation(Candidate, AugmentedCandidate) :-
	candidate_term(NegValue, CUI, MetaTerm, MetaConcept, MetaWords, SemTypes,
		       MatchMap, LSComponents, TargetLSComponent, InvolvesHead,
		       IsOvermatch, SourceInfo, PosInfo, Status, Negated, Candidate),
	extract_components(MatchMap, PhraseComponents0, _MetaComponents),
	sort(PhraseComponents0, PhraseComponents1),
	merge_contiguous_components(PhraseComponents1, PhraseComponents),
	compute_component_span(PhraseComponents, Low, High),
	augmented_candidate_term(PhraseComponents, Low, High,
				 NegValue, CUI, MetaTerm, MetaConcept, MetaWords, SemTypes,
				 MatchMap, LSComponents, TargetLSComponent, InvolvesHead,
				 IsOvermatch, SourceInfo, PosInfo, Status, Negated,
				 AugmentedCandidate).

filter_best_aphrases([], _BestValue, []).
filter_best_aphrases([First|Rest], BestValue, Filtered) :-
	First = ap(Value,_,_,_),
	( Value =:= BestValue ->
	  Filtered = [First|FilteredRest],
	  filter_best_aphrases(Rest, BestValue, FilteredRest)
	; Filtered = []
	).

aphrases_maps([], []).
aphrases_maps([ap(-1000,_,_,[])|Rest], RestMaps) :-
	!,
	aphrases_maps(Rest, RestMaps).
aphrases_maps([ap(NegValue,_,_,Mapping)|Rest], [map(NegValue,Mapping)|RestMaps]) :-
	aphrases_maps(Rest, RestMaps).

print_all_aevs([]) :- nl(user_output).
print_all_aevs([H|T]) :-
	aev_print_version(H, Print),
	format(user_output, '~q~n', [Print]),
	print_all_aevs(T).

remove_duplicate_aevs(RemainingAEvaluations, DuplicateAEvs,
		      AEvaluationsNoDups, DuplicateAEvCount, NoDuplicatesCount) :-
	PrevDups = [],
	find_duplicate_aevs(RemainingAEvaluations, PrevDups, DuplicateAEvs, AEvaluationsNoDups),
	compute_total_dup_count(DuplicateAEvs, DuplicateAEvCount),
	length(AEvaluationsNoDups, NoDuplicatesCount),
	debug_call(dups, print_duplicate_info(DuplicateAEvs, DuplicateAEvCount,
					      AEvaluationsNoDups, NoDuplicatesCount)).

print_duplicate_info(DuplicateEvs, DuplicateEvCount, AEvaluationsNoDups, NoDuplicatesCount) :-
	% DuplicateEvs is the list of terms of the form C:[D1,D2,...,Dn] 
	format(user_output, '~n### DUPLICATES (~d)~n', [DuplicateEvCount]),
	print_duplicate_evs(DuplicateEvs),
	format(user_output, '~n### NON-DUPLICATES (~d):~n', [NoDuplicatesCount]),
	print_all_aevs(AEvaluationsNoDups).

compute_duplicate_counts(DuplicateEvs, DuplicateCountList, Product) :-
	(  foreach(Candidate:DuplicateList, DuplicateEvs),
	   foreach(Candidate:DuplicateCount1, DuplicateCountList)
	do length(DuplicateList, DuplicateCount),
	   DuplicateCount1 is DuplicateCount + 1
	),
	(  foreach(_Candidate2:DuplicateCount2, DuplicateCountList),
	   fromto(1, S0, S, Product)
	do S is S0 * DuplicateCount2
	).
	
calc_percent_removed(DiscardedAEvaluationsLength, AEvaluationsLength, PercentRemoved) :-
	( AEvaluationsLength =:= 0 ->
	  PercentRemoved is 0
	; PercentRemoved is (DiscardedAEvaluationsLength/AEvaluationsLength)*100
	).

maybe_prune_aevs(AEvaluations, PruningThreshold, PhraseTextString, KeptAEvaluations, PrunedCount) :-
 	( ( control_option(compute_all_mappings)
	  % ; control_option(hide_mappings)
	  ; control_option(allow_overmatches) ) ->
	  KeptAEvaluations = AEvaluations,
	  PrunedCount is 0	
	; maybe_prune_aevs_1(AEvaluations, PruningThreshold,
			     PhraseTextString, KeptAEvaluations, PrunedCount)
	).


maybe_prune_aevs_1(AEvaluations, PruningThreshold,
		   PhraseTextString, KeptAEvaluations, PrunedCount) :-	
	% If pruning been explicitly specified (via --prune), or
	% default pruning of 30 has not been explicitly disabled (via --no_prune),
	% then get the pruning threshold PruningThreshold.
	( length(AEvaluations, AEvaluationsLength),
	  AEvaluationsLength > PruningThreshold ->
	  debug_message(prune, '### PRUNING to ~d candidates for input "~s"~n',
			[PruningThreshold, PhraseTextString]),
	  debug_call(prune, print_all_aevs(AEvaluations)),
	  PruningLevel is 1,
	  MaxPruningLevel is 5,
 	  min_max_phrase_components(AEvaluations, 9999, _PhraseMin, 0, PhraseMax),
	  RestoreNum is 0,
  	  prune_aevs_all(PruningLevel, MaxPruningLevel, RestoreNum, PruningThreshold,
			 PhraseMax, AEvaluations, KeptAEvaluations),
	  length(KeptAEvaluations, KeptAEvaluationsLength),
	  debug_message(prune, '~n### ~d Final Kept Candidates~n', [KeptAEvaluationsLength]),
  	  debug_message(candidates, '~n### ~d Candidates ***PRUNED*** to ~d~n',
			[AEvaluationsLength,KeptAEvaluationsLength]),
	  PrunedCount is AEvaluationsLength - KeptAEvaluationsLength,
	  debug_call(prune, print_all_aevs(KeptAEvaluations))
	; KeptAEvaluations = AEvaluations,
	  PrunedCount is 0
	).

get_pruning_threshold(NoDuplicateCount, ActualPruningThreshold) :-
	  % If --no_prune has been specified on the command line,
	  % use NoDuplicateCount as the threshold, period.
	( control_option(no_prune) ->
	  ActualPruningThreshold is NoDuplicateCount
	  % If a pruning threshold has been explicitly specified on the command line
	  % using --prune, then use that value as the threshold, period.
	; control_value(prune, PruningThreshold) ->
	  ActualPruningThreshold is PruningThreshold
	  % Otherwise, the default action is to start with NoDuplicateCount,
	  % and allow backtracking, subtracting 1 at a time,
	  % until an acceptable number of candidates has been reached.
	; InitPruningThreshold is NoDuplicateCount,
	  between(0, NoDuplicateCount, Subtract),
	  ActualPruningThreshold is InitPruningThreshold - Subtract
	 ).

prune_aevs_all(PruningLevel, MaxPruningLevel, _RestoreNumIn,
	       PruningThreshold, PhraseMax, AEvaluationsIn, KeptAEvaluations) :-
	% Is the pruning level =< the maximum pruning level (4 -- set in maybe_prune_aevs/4)?
	( PruningLevel =< MaxPruningLevel,
	  % Continue pruning iff
	  % (1) We still have too many candidates, AND
	  % (2) No candidates were restored in the previous pruning round.
	  length(AEvaluationsIn, RemainingAEvCount),
	  RemainingAEvCount > PruningThreshold ->
	  % RestoreNumIn =:= 0 ->
	  debug_message(prune, '~n### Doing LEVEL-~d pruning on ~d Candidates~n~n',
			[PruningLevel,RemainingAEvCount]),
 	  AEvIndex is 0,
 	  NumPositions is 0,
  	  PositionsIn = [],
 	  PreviousPositionsCoveredAndScores = [],
	  prune_and_restore(PruningLevel, MaxPruningLevel, PruningThreshold, AEvaluationsIn,
			    PreviousPositionsCoveredAndScores, AEvIndex, NumPositions,
			    PhraseMax, PositionsIn, AEvaluationsNext, RestoreNumNext),
	  NextPruningLevel is PruningLevel + 1,
	  prune_aevs_all(NextPruningLevel, MaxPruningLevel, RestoreNumNext,
			 PruningThreshold, PhraseMax, AEvaluationsNext, KeptAEvaluations)
	  % Otherwise, we're done.
	; KeptAEvaluations = AEvaluationsIn
	).

% PPCS == PreviousPositionsCoveredAndScores
prune_and_restore(PruningLevel, _MaxPruningLevel, PruningThreshold, AEvaluationsIn, PPCS,
		  AEvIndex, NumPositions, PhraseMax, PositionsIn, AEvaluationsOut, MinRestoreNum) :-
	prune_LEVEL(AEvaluationsIn, PruningLevel, PruningThreshold, AEvIndex, NumPositions,
		    PhraseMax, PPCS, PositionsIn,
		    DiscardedAEvaluations, AEvaluationsNext),
	length(AEvaluationsIn, AEvaluationsInLength),
	length(DiscardedAEvaluations, DiscardedAEvaluationsLength),
	length(AEvaluationsNext, AEvaluationsNextLength),
	calc_percent_removed(DiscardedAEvaluationsLength, AEvaluationsInLength, PercentRemoved),
	debug_message(prune,
		      '~n### LEVEL-~d pruning removed ~d% (~d of ~d) candidates, leaving ~d~n',
		      [PruningLevel,PercentRemoved,DiscardedAEvaluationsLength,
		       AEvaluationsInLength,AEvaluationsNextLength]),

	MinRestoreNum is max(PruningThreshold-AEvaluationsNextLength, 0),
	debug_message(prune, '### Restoring at least ~d Candidates~n', [MinRestoreNum]),
	PreviousPositionsCoveredAndScores = [],
	restore_discarded_aevs(DiscardedAEvaluations, MinRestoreNum,
			       PreviousPositionsCoveredAndScores,
			       AEvaluationsNext, AEvaluationsOut),
	length(AEvaluationsOut, AEvaluationsOutLength),
	ActualRestoreNum is AEvaluationsOutLength - AEvaluationsNextLength,
	debug_message(prune, '### Restored ~d Pruned Candidates~n', [ActualRestoreNum]).

% Pruning level 1:
% If the aggregate phrase components of the current candidate's MatchMap
% has less phrase coverage than any previously seen candidate, exclude it.

% Pruning level 2:
% If the aggregate phrase components of the current candidate's MatchMap
% has less coverage than the phrase positions covered by all previous examined candidates,
% exclude it.

% Pruning level 3 (not sure this is useful):
% If the aggregate phrase components of the current candidate's MatchMap
% has less coverage than or the same coverage as the phrase positions
% covered by all previous examined candidates, exclude it.

% Pruning level 4:
% If the phrase positions covered by the current candidate's PhraseComponents
% overlaps the phrase positions covered by previous candidates,
% discard this candidate

% Pruning level 5:
% Sledgehammer: Take the first MAXNUM candidates, as well as any subsequent candidates
% whose score is equal to that of the MAXNUM-th candidate.

% TODO:
% (7) Add control option to control which levels of pruning are used.
% (7) Try to predict # of mappings by examining overlap of concepts

% DONE:
% (1) Implement 5th pruning level -- less than or equal to aggregate coverage.
% (2)  All pruning levels must keep later equivalent candidates.
%      Keep running list of phrase coverage/scores of kept candidates,
%      and don't exclude a candidate if a candidate with that same
%      phrase coverage/score has been previously kept
% (3) Improve restoring by restoring all candidates with same score and coverage
%     as last kept candidate; may not be necessary after (2).
% (4) Determine if corrected level-1 pruning ever excludes a best mapping:
%     YES: "suppressor T-cell component"
% (5) If there are two (or more) equivalent candidates, build initial mappings
%     with only one, and then expand final mappings with others.


prune_LEVEL([], _Level, _PruningThreshold, _AEvIndex, _NumPositions,
	      _PhraseMax, _PPCS, _Positions, [], []).
prune_LEVEL([FirstAEv|RestAEvs], Level, PruningThreshold, AEvIndexIn, NumPositionsIn, PhraseMax,
	      PPCSIn, PositionsIn, DiscardedAEvs, KeptAEvs) :-
	AEvIndexNext is AEvIndexIn + 1,
	test_phrase_components_LEVEL(Level, PruningThreshold, FirstAEv, PhraseMax,
				     PositionsIn, NumPositionsIn, PPCSIn,
				     PositionsNext, NumPositionsNext, AEvIndexNext,
				     PPCSNext,
				     DiscardedAEvs, RestDiscardedAEvs,
				     KeptAEvs, RestKeptAEvs),
	prune_LEVEL(RestAEvs, Level, PruningThreshold, AEvIndexNext, NumPositionsNext,
		    PhraseMax, PPCSNext, PositionsNext,
		    RestDiscardedAEvs, RestKeptAEvs).


test_phrase_components_LEVEL(1, _PruningThreshold, ThisAEv, PhraseMax, PositionsIn, NumPositionsIn, PPCSIn,
			     PositionsNext, NumPositionsNext, ThisAEvIndex, PPCSNext,
			     DiscardedAEvs, RestDiscardedAEvs, KeptAEvs, RestKeptAEvs) :-
	get_aev_info(ThisAEv, NegValue, ThisAEvPrintVersion, PhrasePositions, _MetaPositions),
	% If the the current candidate's phrase positions
	% are a proper subset of those of a previously seen candidate, exclude it
	( member(OtherPhrasePositions-_OtherScore, PPCSIn),
	  proper_subset(PhrasePositions, OtherPhrasePositions) ->
	  Message = 'TOSS',
	  PositionsNext = PositionsIn,
	  NumPositionsNext is NumPositionsIn,
	  PPCSNext = PPCSIn,
	  DiscardedAEvs = [ThisAEvIndex-ThisAEv|RestDiscardedAEvs],
	  KeptAEvs = RestKeptAEvs
	; Message = 'KEEP',
	  append(PhrasePositions, PositionsIn, PositionsNext0),
	  sort(PositionsNext0, PositionsNext),
	  length(PositionsNext, NumPositionsNext),
	  PPCSNext = [PhrasePositions-NegValue|PPCSIn],
	  DiscardedAEvs = RestDiscardedAEvs,
	  KeptAEvs = [ThisAEvIndex-ThisAEv|RestKeptAEvs]
	),
	debug_message(prune,
		      '~w ~d ~d/~d: ~q~n',
		      [Message,ThisAEvIndex,NumPositionsNext,PhraseMax,ThisAEvPrintVersion]).

test_phrase_components_LEVEL(2, _PruningThreshold, ThisAEv, PhraseMax, PositionsIn, NumPositionsIn, PPCSIn,
			     PositionsNext, NumPositionsNext, ThisAEvIndex, PPCSNext,
			     DiscardedAEvs, RestDiscardedAEvs, KeptAEvs, RestKeptAEvs) :-
	get_aev_info(ThisAEv, NegValue, ThisAEvPrintVersion, PhrasePositions, _MetaPositions),
	% If a previously kept candidate's phrase coverage and score
	% are the same as those of the current candidate, keep it.
	( memberchk(PhrasePositions-NegValue, PPCSIn) ->
	  Message = 'KEEP',
	  PositionsNext = PositionsIn,
	  NumPositionsNext is NumPositionsIn,
	  PPCSNext = PPCSIn,
	  DiscardedAEvs = RestDiscardedAEvs,
	  KeptAEvs = [ThisAEvIndex-ThisAEv|RestKeptAEvs]
	  % If the phrase positions covered by the current candidate's PhraseComponents
	  % is a proper subset of the phrase positions covered by all previous candidates,
	  % this candidate addes nothing
	; proper_subset(PhrasePositions, PositionsIn) ->
	  Message = 'TOSS',
	  PositionsNext = PositionsIn,
	  NumPositionsNext is NumPositionsIn,
	  PPCSNext = PPCSIn,
	  DiscardedAEvs = [ThisAEvIndex-ThisAEv|RestDiscardedAEvs],
	  KeptAEvs = RestKeptAEvs
	  % The current candidate has a new phrase coverage/score combination that should be kept
	; Message = 'KEEP',
	  union(PhrasePositions, PositionsIn, PositionsNext, Difference0),
	  sort(Difference0, Difference),
	  length(Difference, DifferenceLength),
	  NumPositionsNext is NumPositionsIn + DifferenceLength,
	  PPCSNext = [PhrasePositions-NegValue|PPCSIn],
	  DiscardedAEvs = RestDiscardedAEvs,
	  KeptAEvs = [ThisAEvIndex-ThisAEv|RestKeptAEvs]
	),
	debug_message(prune,
		      '~w ~d ~d/~d: ~q~n',
		      [Message,ThisAEvIndex,NumPositionsNext,PhraseMax,ThisAEvPrintVersion]).
	
test_phrase_components_LEVEL(3, _PruningThreshold, ThisAEv, PhraseMax, PositionsIn, NumPositionsIn, PPCSIn,
			     PositionsNext, NumPositionsNext, ThisAEvIndex, PPCSNext,
			     DiscardedAEvs, RestDiscardedAEvs, KeptAEvs, RestKeptAEvs) :-
	get_aev_info(ThisAEv, NegValue, ThisAEvPrintVersion, PhrasePositions, _MetaPositions),
	% If a previously kept candidate's phrase coverage and score
	% are the same as those of the current candidate, keep it.
	( memberchk(PhrasePositions-NegValue, PPCSIn) ->
	  Message = 'KEEP',
	  PositionsNext = PositionsIn,
	  NumPositionsNext is NumPositionsIn,
	  PPCSNext = PPCSIn,
	  DiscardedAEvs = RestDiscardedAEvs,
	  KeptAEvs = [ThisAEvIndex-ThisAEv|RestKeptAEvs]
	  % If the phrase positions covered by the current candidate's PhraseComponents
	  % is a subset of the phrase positions covered by all previous candidates,
	  % this candidate addes nothing
	; subset(PhrasePositions, PositionsIn) ->
	  Message = 'TOSS',
	  PositionsNext = PositionsIn,
	  NumPositionsNext is NumPositionsIn,
	  PPCSNext = PPCSIn,
	  DiscardedAEvs = [ThisAEvIndex-ThisAEv|RestDiscardedAEvs],
	  KeptAEvs = RestKeptAEvs
	  % If the phrase positions covered by the current candidate's PhraseComponents
	  % is a proper *superset* of the phrase positions covered by previous candidates,
	  % this candidate adds value
	  % This test is almost certainly redundant!
	; Message = 'KEEP',
	  union(PhrasePositions, PositionsIn, PositionsNext, Difference0),
	  sort(Difference0, Difference),
	  length(Difference, DifferenceLength) ->
	  NumPositionsNext is NumPositionsIn + DifferenceLength,
	  PPCSNext = [PhrasePositions-NegValue|PPCSIn],
	  DiscardedAEvs = RestDiscardedAEvs,
	  KeptAEvs = [ThisAEvIndex-ThisAEv|RestKeptAEvs]
	),
	debug_message(prune,
		      '~w ~d ~d/~d: ~q~n',
		      [Message,ThisAEvIndex,NumPositionsNext,PhraseMax,ThisAEvPrintVersion]).

test_phrase_components_LEVEL(4, _PruningThreshold, ThisAEv, PhraseMax, PositionsIn, NumPositionsIn, _PPCSIn,
			     PositionsNext, NumPositionsNext, ThisAEvIndex, _PPCSNext,
			     DiscardedAEvs, RestDiscardedAEvs, KeptAEvs, RestKeptAEvs) :-
	get_aev_info(ThisAEv, NegValue, ThisAEvPrintVersion, PhrasePositions, _MetaPositions),
	% If a previously kept candidate's phrase coverage and score
	% are the same as those of the current candidate, keep it.
	( memberchk(PhrasePositions-NegValue, PPCSIn) ->
	  Message = 'KEEP',
	  PositionsNext = PositionsIn,
	  NumPositionsNext is NumPositionsIn,
	  PPCSNext = PPCSIn,
	  DiscardedAEvs = RestDiscardedAEvs,
	  KeptAEvs = [ThisAEvIndex-ThisAEv|RestKeptAEvs]
	  % If the phrase positions covered by the current candidate's PhraseComponents
	  % overlaps the phrase positions covered by previous candidates,
	  % discard this candidate
	; intersection(PhrasePositions, PositionsIn, Intersection),
	  Intersection = [_|_] ->
	  Message = 'TOSS',
	  PositionsNext = PositionsIn,
	  NumPositionsNext is NumPositionsIn,
	  PPCSNext = PPCSIn,
	  DiscardedAEvs = [ThisAEvIndex-ThisAEv|RestDiscardedAEvs],
	  KeptAEvs = RestKeptAEvs
	; Message = 'KEEP',
	  union(PhrasePositions, PositionsIn, PositionsNext, Difference0),
	  sort(Difference0, Difference),
	  length(Difference, DifferenceLength),
	  NumPositionsNext is NumPositionsIn + DifferenceLength,
	  PPCSNext = [PhrasePositions-NegValue|PPCSIn],
	  DiscardedAEvs = RestDiscardedAEvs,
	  KeptAEvs = [ThisAEvIndex-ThisAEv|RestKeptAEvs]
	  % This branch should never be taken; if it is, something is grievously wrong
	),
	debug_message(prune,
		      '~w ~d ~d/~d: ~q~n',
		      [Message,ThisAEvIndex,NumPositionsNext,PhraseMax,ThisAEvPrintVersion]).

test_phrase_components_LEVEL(5, PruningThreshold, ThisAEv, PhraseMax, PositionsIn, NumPositionsIn, PPCSIn,
			     PositionsNext, NumPositionsNext, ThisAEvIndex, PPCSNext,
			     DiscardedAEvs, RestDiscardedAEvs, KeptAEvs, RestKeptAEvs) :-
	get_aev_info(ThisAEv, NegValue, ThisAEvPrintVersion, PhrasePositions, _MetaPositions),
	% If a previously kept candidate's phrase coverage and score
	% are the same as those of the current candidate, keep it.
	( memberchk(PhrasePositions-NegValue, PPCSIn) ->
	  Message = 'KEEP',
	  PositionsNext = PositionsIn,
	  NumPositionsNext is NumPositionsIn,
	  PPCSNext = PPCSIn,
	  DiscardedAEvs = RestDiscardedAEvs,
	  KeptAEvs = [ThisAEvIndex-ThisAEv|RestKeptAEvs]
	  % If the Index of this AEv is > PruningThreshold, toss it
	; ThisAEvIndex > PruningThreshold ->
	  Message = 'TOSS',
	  PositionsNext = PositionsIn,
	  NumPositionsNext is NumPositionsIn,
	  PPCSNext = PPCSIn,
	  DiscardedAEvs = [ThisAEvIndex-ThisAEv|RestDiscardedAEvs],
	  KeptAEvs = RestKeptAEvs
	  % If the Index of this AEv is > PruningThreshold AND
	  % this AEv has the same score as the previous candidate's, keep it
	  % Otherwise, simply keep it.
	; Message = 'KEEP',
	  NumPositionsNext is NumPositionsIn,
	  PPCSNext = [PhrasePositions-NegValue|PPCSIn],
	  DiscardedAEvs = RestDiscardedAEvs,
	  KeptAEvs = [ThisAEvIndex-ThisAEv|RestKeptAEvs]
	),
	debug_message(prune,
		      '~w ~d ~d/~d: ~q~n',
		      [Message,ThisAEvIndex,NumPositionsNext,PhraseMax,ThisAEvPrintVersion]).

% Need to restore all discarded candidates with the
% same score and phrase coverage as any previously restored one
% PPCS == PreviousPhrasePositionsAndScores

% No more discarded AEvs left to restore, so keep all the remaining kept ones
restore_discarded_aevs([], _RestoreNum, _PPCSIn, KeptAEvs, FinalAEvs) :-
	  keys_and_values(KeptAEvs, _Keys, FinalAEvs).

restore_discarded_aevs([DiscardedIndex-FirstDiscardedAEv|RestDiscardedAEvs],
		       RestoreNum, PPCSIn, KeptAEvs, FinalAEvs) :-
	get_aev_info(FirstDiscardedAEv, NegValue, PrintVersion, PhrasePositions, _MetaPositions),
	( memberchk(PhrasePositions-NegValue, PPCSIn) ->
	  NextRestoreNum is max(RestoreNum-1, 0),
	  PPCSNext = PPCSIn,
	  NextKeptAEvs = KeptAEvs,
	  debug_message(prune, 'RESTORED: ~q~n', [PrintVersion]),
	  NextDiscardedAEvs = RestDiscardedAEvs,
	  FinalAEvs = [FirstDiscardedAEv|RestFinalAEvs]
	  % We've restored enough candidates, and the current discarded candidate
	  % and the current discarded AEv 
	; RestoreNum =< 0 ->
	  NextRestoreNum is RestoreNum,
	  PPCSNext = PPCSIn,
	  NextKeptAEvs = KeptAEvs,
	  NextDiscardedAEvs = RestDiscardedAEvs,
	  RestFinalAEvs = FinalAEvs
	  % No more kept AEvs left to restore
	; KeptAEvs == [] ->
	  NextRestoreNum is RestoreNum,
	  PPCSNext = PPCSIn,
	  NextKeptAEvs = KeptAEvs,
	  NextDiscardedAEvs = RestDiscardedAEvs,
	  debug_message(prune, 'RESTORED: ~q~n', [PrintVersion]),
	  FinalAEvs = [FirstDiscardedAEv|RestFinalAEvs]
	  % DiscardedIndex is the index of the next discarded AEv.
	  % KeptIndex is the index of the next kept AEv.
	  % We need to know which of the two should be the next AEv
	  % to add to the FinalAEvs List, because that list should contain
	  % the AEvs in their original order.

	; update_aev_lists(RestoreNum, KeptAEvs, DiscardedIndex, FirstDiscardedAEv, PPCSIn,
			   RestDiscardedAEvs, NextDiscardedAEvs,
			   PPCSNext, NextKeptAEvs, NextRestoreNum, FinalAEvs, RestFinalAEvs)
	),
	restore_discarded_aevs(NextDiscardedAEvs, NextRestoreNum, PPCSNext, NextKeptAEvs, RestFinalAEvs).

update_aev_lists(RestoreNum, KeptAEvs, DiscardedIndex, FirstDiscardedAEv, PPCSIn,
		 RestDiscardedAEvs, NextDiscardedAEvs,
		 PPCSNext, NextKeptAEvs,  NextRestoreNum, FinalAEvs, RestFinalAEvs) :-
	  KeptAEvs = [KeptIndex-FirstKeptAEv|RestKeptAEvs],
	  % If DiscardedIndex < KeptIndex, then the next AEv to be added to FinalAEvs
	  % is a discarded AEv; in that case we decrement RestoreNum,
	  % because we just restored an AEv.
	  ( DiscardedIndex < KeptIndex ->
	    NextRestoreNum is RestoreNum - 1,
	    PPCSNext = [PhrasePositions-NegValue|PPCSIn],
	    NextDiscardedAEvs = RestDiscardedAEvs,
	    NextKeptAEvs = KeptAEvs,
	    FinalAEvs = [FirstDiscardedAEv|RestFinalAEvs],
	    get_aev_info(FirstDiscardedAEv, NegValue, PrintVersion, PhrasePositions, _MetaPositions),
	    debug_message(prune, 'RESTORED: ~q~n', [PrintVersion])
	    % Otherwise, we add the first KeptAEv to FinalAEvs; no need to decrement RestoreNum
	  ; NextRestoreNum is RestoreNum,
	    FinalAEvs = [FirstKeptAEv|RestFinalAEvs],
	    get_aev_info(FirstKeptAEv, NegValue, _PrintVersion, PhrasePositions, _MetaPositions),
	    PPCSNext = [PhrasePositions-NegValue|PPCSIn],
	    NextDiscardedAEvs = [DiscardedIndex-FirstDiscardedAEv|RestDiscardedAEvs],
	    NextKeptAEvs = RestKeptAEvs
	  ).

get_aev_info(ThisAEv, NegValue, ThisAEvPrintVersion, PhrasePositions, MetaPositions) :-
	get_all_aev_features([phrasecomponents,negvalue,matchmap],
			     ThisAEv,
			     [PhraseComponents,NegValue,MatchMap]),
	aev_print_version(ThisAEv, ThisAEvPrintVersion),
	extract_components(MatchMap, _PhraseComponents0, MetaComponents),
	positions_covered(MetaComponents, MetaPositions),
	positions_covered(PhraseComponents, PhrasePositions).

positions_covered(Components, Positions) :-
	phrase_components_positions(Components, Positions0),
	append(Positions0, Positions1),
	sort(Positions1, Positions).

phrase_components_positions([], []).
phrase_components_positions([H|T], [PositionsH|PositionsT]) :-
	H = [Low,High],
	( for(I, Low, High), foreach(I, PositionsH) do true ),
	phrase_components_positions(T, PositionsT).

% proper_subset(Set1, Set2) :-
% 	subset(Set1, Set2),
% 	member(Element, Set2),
% 	\+ memberchk(Element, Set1).

% determine the lowest and highest phrase components in the AEvs
min_max_phrase_components([], PhraseMin, PhraseMin, PhraseMax, PhraseMax).
min_max_phrase_components([FirstAEv|RestAEvs], PhraseMinIn, PhraseMin, PhraseMaxIn, PhraseMax) :-
	get_all_aev_features([low,high], FirstAEv, [Low,High]),
        update_phrase_components(Low, PhraseMinIn, PhraseMinNext,
                                 High, PhraseMaxIn, PhraseMaxNext),
        min_max_phrase_components(RestAEvs, PhraseMinNext, PhraseMin, PhraseMaxNext, PhraseMax).

update_phrase_components(Low, PhraseMinIn, PhraseMinNext,
                         High, PhraseMaxIn, PhraseMaxNext) :-
        ( Low < PhraseMinIn ->
          PhraseMinNext is Low
        ; PhraseMinNext is PhraseMinIn
        ),
        ( High > PhraseMaxIn ->
          PhraseMaxNext is High
        ; PhraseMaxNext is PhraseMaxIn
        ).

% Given a list of AEvs, create a list of terms of the form
% Candidate:DupList
% where DupList is a (possibly empty) list of candidates
% that have the same Score and PhraseComponents as Candidate.

% Uusing the portrayed representation of Candidates defined at the end of this file,
% one element of the list could be
% aev(C0175730):[aev(C1704731),aev(C1704730)a,ev(C1704474),aev(C1561954),aev(C1547937)]
% Most will be
% aev(C0019168):[]
% meaning that candidate aev(C0019168) had no other candidates
% with matching Score and PhraseComponents.

find_duplicate_aevs(AEvs, PrevDuplicates, Duplicates, NoDuplicates) :-
	  find_duplicate_aevs_aux(AEvs, PrevDuplicates, Duplicates0),
	  separate_dups(Duplicates0, Duplicates, NoDuplicates).

% Partition the list described above into terms showing duplicates, e.g.,
% aev(C0175730):[aev(C1704731),aev(C1704730),aev(C1704474),aev(C1561954),aev(C1547937)]
% and terms showing no duplicates, e.g.,
% aev(C0019168):[]
% Also, de-augment the aev(_) structures in the terms showing duplicates to
% ev(C0175730):[ev(C1704731),ev(C1704730),ev(C1704474),ev(C1561954),ev(C1547937)]
separate_dups([], [], []).
separate_dups([H|T], Duplicates, NoDuplicates) :-
	H = ThisAEv:ThisAEvDuplicates,
	( ThisAEvDuplicates == [] ->
	  NoDuplicates = [ThisAEv|RestNoDuplicates],
	  Duplicates = RestDuplicates
	; deaugment_evaluations([ThisAEv|ThisAEvDuplicates], [ThisEv|ThisEvDuplicates]),
	  DeaugmentedH = ThisEv:ThisEvDuplicates,
	  Duplicates = [DeaugmentedH|RestDuplicates],
	  NoDuplicates = [ThisAEv|RestNoDuplicates]
	),
	separate_dups(T, RestDuplicates, RestNoDuplicates).

% This is the looser version, which requires only two features to match:
% Score, and Phrase Component of MatchMap
find_duplicate_aevs_aux([], Dups, Dups).
find_duplicate_aevs_aux([FirstAEv|RestAEvs], DupsIn, DupsOut) :-
 	get_aev_info(FirstAEv, NegScore, _PrintVersion, PhrasePositions, _MetaPositions),
	update_dups(DupsIn, NegScore, PhrasePositions, FirstAEv, DupsNext),
	find_duplicate_aevs_aux(RestAEvs, DupsNext, DupsOut).


update_dups([], _NegScore, _PhrasePosCovered, ThisAEv, [ThisAEv:[]]).
update_dups([FirstDupAEv:DupsFound|RestDupAEvs], NegScore, PhrasePositions, ThisAEv, DupsOut) :-
	get_aev_info(FirstDupAEv, FirstDupNegScore,
		    _Print, FirstDupPhrasePositions, _FirstDupMetaPositions),
	( NegScore =:= FirstDupNegScore,
	  PhrasePositions == FirstDupPhrasePositions ->
	  DupsOut = [FirstDupAEv:[ThisAEv|DupsFound]|RestDupAEvs]
	; DupsOut = [FirstDupAEv:DupsFound|RestDupsOut],
	  update_dups(RestDupAEvs, NegScore, PhrasePositions, ThisAEv, RestDupsOut)
	).

print_duplicate_evs(DuplicateEvs) :-
	(  foreach(Ev:Duplicates, DuplicateEvs)
	do ev_print_version(Ev, PrintEv),
	   format(user_output, '~q~n', [PrintEv]),
	   (  foreach(Dup, Duplicates)
	   do ev_print_version(Dup, PrintDup),
	      format(user_output, '   ~q~n', [PrintDup])
	   )
	).

% Duplicates is a list whose elements are of the form Candidate:Dups, e.g.,
% [C0:Dups0, C1:Dups1, C2:Dups2, ... CN:DupsN], 
% we need to calculate the sum of the lenghts of all the Dups lists
% to know how many duplicate candidates we've accumulated.
compute_total_dup_count(Duplicates, DuplicatesCount) :-
	(  foreach(Dup, Duplicates),
	   fromto(0, In, Out, DuplicatesCount)
	do Dup = _:DupList,
	   length(DupList, DupListLength),
	   Out is In + DupListLength
	).

calculate_matrix_sparseness(AEvaluations, PhraseLength, Sparseness) :-
	length(AEvaluations, CandidateCount),
	(  foreach(Candidate, AEvaluations),
	   fromto(0, In, Out, Sum),
	   param(PhraseLength)
	do arg(2, Candidate, MinPos),
	   arg(3, Candidate, MaxPos),
	   Out is In + MaxPos - MinPos + 1
	),
	Sparseness is 100*Sum/(PhraseLength*CandidateCount).

print_candidate_grid(AEvaluations, DuplicateCount, CountWithDuplicates,
		     NoDuplicateCount, PhraseLength, Sparseness) :-
	print_candidate_grid_scale(PhraseLength, 0),
	length(AEvaluations, CandidateCount),
	( foreach(AugmentedCandidate, AEvaluations),
	  for(I, 1, CandidateCount),
	  param(PhraseLength)
	  do
	  get_all_aev_features([low,high,negvalue,cui,metawords],
			       AugmentedCandidate,
			       [Low,High,NegValue,CUI,MetaWords]),
	  number_codes(I, Codes),
	  length(Codes, CodesLength),
	  Padding is 3 - CodesLength,
	  format(user_output, '~*c~d ', [Padding,32,I]),
	  print_candidate_grid_coverage(0, Low, High, CUI, NegValue, MetaWords, PhraseLength)
	),
	print_candidate_grid_scale(PhraseLength, 0),
	announce_grid_data(DuplicateCount, CountWithDuplicates,
			   NoDuplicateCount, PhraseLength,Sparseness).



print_candidate_grid_scale(PhraseLength, PrevInteger) :-
	format(user_output, '    |', []),
	print_candidate_grid_scale_1(PhraseLength, PrevInteger).

print_candidate_grid_scale_1(PhraseLength, PrevInteger) :-
	( PhraseLength is 0 ->
	  format(user_output, '|~n', [])
	; NextInteger is ( PrevInteger + 1 ) rem 10,
	  ( NextInteger is 0 ->
	    Symbol = '|'
	  ; Symbol is NextInteger
	  ),
	  format(user_output, '~w', [Symbol]),
	  PhraseLengthMinus1 is PhraseLength - 1,
	  print_candidate_grid_scale_1(PhraseLengthMinus1, NextInteger)
	).

announce_grid_data(DuplicateCount, CountWithDuplicates, NoDuplicateCount, PhraseLength,Sparseness) :-
	TotalCount is DuplicateCount + NoDuplicateCount,
	format(user_output,
	       '### Total/# Dups/# w/Dups/# Base/PhraseLength/Sparseness: ', []),
	format(user_output, '~d/~d/~d/~d/~d/~2f...',
	       [TotalCount,DuplicateCount,CountWithDuplicates,
		NoDuplicateCount,PhraseLength,Sparseness]).	

evaluate_candidate_grid([], _DuplicateEvCount, _DuplicateEvsLength,
			_NoDuplicateCount, _NPhraseWords, _Sparseness).
% DuplicateCount   == number of total duplicate candidates
% CountWithDups    == number of candidates having a duplicate
% NoDuplicateCount == number of candidates after removing duplicates
evaluate_candidate_grid([H|T], DuplicateCount, CountWithDuplicates,
			NoDuplicateCount, PhraseLength, Sparseness) :-
	AEvaluations = [H|T],
	calculate_matrix_sparseness(AEvaluations, PhraseLength, Sparseness),
	debug_call(grid, print_candidate_grid(AEvaluations, DuplicateCount,
					      CountWithDuplicates, NoDuplicateCount,
					      PhraseLength, Sparseness)).

print_candidate_grid_coverage(CurrPos, MinPos, MaxPos, CUI, NegValue, Words, PhraseLength) :-	
	CurrPos =< PhraseLength,
	!,
	% first position: print '|'
	( CurrPos =:=  0 ->
	  Char = '|'
	% before MinPos: print ' '
	; CurrPos < MinPos ->
	  Char = ' '
	% within the interval: print '*'
	; CurrPos >= MinPos,
	  CurrPos =< MaxPos ->
	  Char = '*'
	% beyond MaxPos, but not yet at end of phrase: print ' '
	; CurrPos > MaxPos,
	  CurrPos =< PhraseLength ->
	  Char = ' '
	),
	format(user_output, '~w', [Char]),
	NextPos is CurrPos + 1,
	print_candidate_grid_coverage(NextPos, MinPos, MaxPos, CUI, NegValue, Words, PhraseLength).
print_candidate_grid_coverage(_CurrPos, _MinPos, _MaxPos, CUI, NegValue, Words, _PhraseLength) :-
	% reached the end of the phrase: print '|', words, and newline.
	% All done!
	PosScore is -1 * NegValue,
	( PosScore is 1000 ->
	  Padding = ''
	; Padding = ' '
	),	format(user_output, '|  ~w ~w~d ~q~n', [CUI,Padding,PosScore,Words]).

% 25/28/14.00 fails on
% Phrase: "r-proteins L16-L29-S17-L14-L24-L5-S14-S8-L6-L18-S5-L30-L15-SecY-adenylate
%          kinase (Adk)-methionine aminopeptidase (Map)-initiation factor 1 
%          (IF1)-L36-S13-S11-alpha subunit"
% from PMID 8635744.
% 24/16/15.36 fails on
% Phrase: "with ultra-high spatial resolution black blood inner volume three-dimensional
%         fast spin echo magnetic resonance imaging"
% from PMID 18080213 with JavaLex

test_candidate_grid_sparseness(_AEvaluations, _NoDuplicateCount, _Sparseness) :-
	control_option(allow_overmatches),
	!.
	
test_candidate_grid_sparseness(AEvaluations, NoDuplicateCount, Sparseness) :-	
	\+ control_option(no_prune),
	\+ control_option(prune),
	AEvaluations = [_|_],
	!,
	  % If NoDuplicateCount >= 45, fail regardless of anything else
	( NoDuplicateCount >= 45 ->
	  % print the "NO", but fail
 	  debug_message(grid, 'NO~n',  []), fail

	  % If NoDuplicateCount >= 25 and Sparseness is =< 22...
	; NoDuplicateCount >= 24,
	  Sparseness =< 22 ->
	  % print the "NO", but fail
 	  debug_message(grid, 'NO~n',  []), fail

	  % Otherwise, proceed
	; debug_message(grid, 'YES~n',  [])
	).
test_candidate_grid_sparseness(_AEvaluations,_NoDuplicateCount, _Sparseness) :-
	debug_message(grid, 'OK~n',  []).	
	
% RemainingCandidates is the list of candidates remaining after pruning
maybe_mark_pruned_evaluations(AllCandidates, RemainingCandidates) :-
 	( control_option(allow_overmatches) ->
	  (  foreach(Candidate, AllCandidates)
	  do get_candidate_feature(status, Candidate, Status),
	     Status is 0
	  )
	; mark_pruned_evaluations(AllCandidates, RemainingCandidates)
	).

mark_pruned_evaluations(AllCandidates, RemainingCandidates) :-
	(  foreach(Candidate, AllCandidates),
	   param(RemainingCandidates)
	do get_candidate_feature(status, Candidate, Status),
	   ( memberchk(Candidate, RemainingCandidates) ->
	     Status is 0
	   ; Status is 2
	   )
	).

construct_all_mappings(Evaluations, PhraseTextString, NPhraseWords, Variants,
		       _BestAEvaluations, DuplicateAEvs, FinalMappings, PrunedCount) :-
	augment_evaluations(Evaluations, AEvaluations), 
	length(Evaluations, EvaluationsCount),
	get_pruning_threshold(EvaluationsCount, PruningThreshold),
	maybe_prune_aevs(AEvaluations, PruningThreshold,
			 PhraseTextString, RemainingAEvaluations, PrunedCount),
	deaugment_evaluations(RemainingAEvaluations, RemainingEvs),
	mark_pruned_evaluations(Evaluations, RemainingEvs),	
	remove_duplicate_aevs(RemainingAEvaluations,
			      DuplicateAEvs, AEvaluationsNoDups,
			      DuplicateAEvCount, NoDuplicateCount),
	% compute_duplicate_counts(DuplicateAEvs, DuplicateCountList, Product),
	% format(user_output, '### PRODUCT = ~d~n', [Product]), ttyflush,	
	debug_call([candidates,grid], length(DuplicateAEvs, DuplicateAEvsCount)),
	debug_call([candidates,grid], length(RemainingAEvaluations, TotalAEvsCount)),
	debug_message(candidates, '~N### ~d Duplicates of ~d Candidates;',
		      [DuplicateAEvCount,DuplicateAEvsCount]),
	debug_message(candidates, ' ~d Base Candidates, ~d Total Candidates~n',
		      [NoDuplicateCount,TotalAEvsCount]),
	% BestAEvaluations = AEvaluationsNoDups,
	% Compute mappings' confidence value upstream -- where mappings are first constructed!
	evaluate_candidate_grid(AEvaluationsNoDups, DuplicateAEvCount, DuplicateAEvsCount,
				NoDuplicateCount, NPhraseWords, Sparseness),
	test_candidate_grid_sparseness(AEvaluationsNoDups, NoDuplicateCount, Sparseness),
	% !,
	Depth is 1,
	MappingsCountIn is 0,
	debug_call(candidates, length(AEvaluationsNoDups, NoDupsCount)),
	debug_message(candidates, '### Expanding ~d Candidates~n', [NoDupsCount]),
	expand_aevs(AEvaluationsNoDups, Depth, MappingsCountIn, MappingsTree, RawMappingsCount),
	debug_message(candidates, '~N### Assembling mappings from ~d Candidates~n', [NoDupsCount]),
	% This is the NEW way of flattening--order is preserved,
	% and no need for explicit flattening.
	% assemble_all_mappings_ds also computes the Confidence Value for each mapping
	assemble_all_mappings_dl(MappingsTree, NPhraseWords, Variants,
				 [], [], FlattenedNestedMappings1),
	debug_message(mappings, '### ~d Raw Mappings~n', [RawMappingsCount]),	
	debug_call(mappings, length(FlattenedNestedMappings1, FlattenedMappingsLength)),
	debug_message(mappings,
		      '~N### Deaugmenting and Reordering ~w mappings~n',
		      [FlattenedMappingsLength]),
	maybe_keep_best_mappings_only(FlattenedNestedMappings1, FlattenedNestedMappings2),
	% Prepending is just an efficiency measure to minimize
	% the amount of unification done in subsumption testing
	% deaugment_reorder_and_prepend_all_mappings(FlattenedNestedMappings1, Mappings3),
	reorder_and_prepend_all_mappings(FlattenedNestedMappings2, Mappings3),
	% We want to avoid calling filter_out_subsumed_mappings if possible,
	% because it's extremely computationally intensive.
	% Counterintuitively, we must call filter_out_subsumed_mappings
	% if and only if compute_all_mappings is on.
	% The reason is that conditionally_filter_best_aphrases/2
	% will keep only the best-scoring mappings anyway.
	% HUH?!
	% ( control_option(compute_all_mappings) ->
	ChunkSize = 1000,
	debug_message(mappings, '~N### Filtering out subsumed mappings.~n', []),
 	filter_out_subsumed_mappings_chunked(Mappings3, ChunkSize, FinalMappings0),
	remove_prepending_data(FinalMappings0, FinalMappings),
	% for debugging or profiling only
	% compute_duplicate_mapping_count(FinalMappings, DuplicateCountList, DuplicateMappingsCount),
	% format(user_output, 'DMC = ~d~n', [DuplicateMappingsCount]),
	% maybe_keep_best_mappings_only(FinalMappings1, FinalMappings),
	debug_call(mappings, length(FinalMappings, FinalMappingsCount)),
	debug_message(mappings,
		      '### ~d Initial Mappings; ~d Final Mappings~n',
		      [FlattenedMappingsLength,FinalMappingsCount]).

remove_prepending_data(MappingsIn, MappingsOut) :-
	( foreach(Score:_PrependData-Mapping, MappingsIn),
	  foreach(Score-Mapping, MappingsOut)
	do
	  true
	).

compute_duplicate_mapping_count(FinalMappings, DuplicateCountList, DuplicateMappingsCount) :-
	(  foreach(_Score-CandidateList, FinalMappings),
	   foreach(Multiplier, MultiplierList),
	   param(DuplicateCountList)
	do compute_duplicate_mapping_multiplier(CandidateList, DuplicateCountList, Multiplier)
	),
	sumlist(MultiplierList, DuplicateMappingsCount).

compute_duplicate_mapping_multiplier(Mapping, DuplicateCountList, Multiplier) :-
	(  foreach(Candidate, Mapping),
	   foreach(DuplicateCount, AllDuplicateCounts),
	   param(DuplicateCountList)
	do ( memberchk(Candidate:DuplicateCount, DuplicateCountList) ->
	     true
	   ; DuplicateCount is 1
	   )
	),
	list_product(AllDuplicateCounts, Multiplier).
	
list_product(List, Product) :-
	list_product_aux(List, 1, Product).

list_product_aux([], Product, Product).
list_product_aux([H|T], ProductIn, ProductOut) :-
	ProductNext is ProductIn * H,
	list_product_aux(T, ProductNext, ProductOut).

maybe_keep_best_mappings_only(FlattenedNestedMappings1, FlattenedNestedMappings2) :-
	( \+ control_option(compute_all_mappings) ->
	   compute_min_mapping_score(FlattenedNestedMappings1, 0, MinScore),
	   keep_mappings_with_min_score(FlattenedNestedMappings1, MinScore, FlattenedNestedMappings2)
	  % FlattenedNestedMappings2 = FlattenedNestedMappings1
	; FlattenedNestedMappings2 = FlattenedNestedMappings1
	).

compute_min_mapping_score([], MinScore, MinScore).
compute_min_mapping_score([FirstMappingWithScore|RestMappingsWithScore], MinScoreIn, MinScoreOut) :-
	FirstMappingWithScore = FirstScore-_FirstMapping,
	( FirstScore < MinScoreIn ->
	  MinScoreNext is FirstScore
	; MinScoreNext is MinScoreIn
	),
	compute_min_mapping_score(RestMappingsWithScore, MinScoreNext, MinScoreOut).

% Allowing mappings with a score that is within 95% of the MinScore is a fudge,
% but it's necessary because of "organized obstruction in upper lobe" (with term_processing)
% This mapping, consisting of three candidates, scores -762
%   637 C1300196:Organised (Organized) [Functional Concept]
%   804 C0028778:Obstruction [Pathologic Function]
%   637 C1428707:Lobe (AKT1S1 gene) [Gene or Genome]
% but this mapping, consisting of four candidates, scores only -757
%   637 C1300196:Organised (Organized) [Functional Concept]
%   804 C0028778:Obstruction [Pathologic Function]
%   637 C1282910:Upper [Spatial Concept]
%   637 C1428707:Lobe (AKT1S1 gene) [Gene or Genome]
% Even though it includes an extra word ("upper").
% The lower-scoring (-757) mapping subsumes the higher-scoring one.
% This behavior is anomalous and needs investigating.

% This doesn't happen any more. Go figure...

keep_mappings_with_min_score([], _MinScore, []).
keep_mappings_with_min_score([FirstMappingWithScore|RestMappingsWithScore], MinScore, KeptMappings) :-
	FirstMappingWithScore = FirstScore-_FirstMapping,
	get_map_thresh(MapThreshInteger),
	MapThresh is MapThreshInteger / 100,
	( FirstScore =< MapThresh * MinScore ->
	  KeptMappings = [FirstMappingWithScore|RestKeptMappings]
	; KeptMappings = RestKeptMappings
	),
	keep_mappings_with_min_score(RestMappingsWithScore, MinScore, RestKeptMappings).

get_map_thresh(MapThreshInteger) :-
	( control_value(map_thresh, MapThreshAtom) ->
	  ensure_number(MapThreshAtom, MapThreshInteger)
	; MapThreshInteger is 70
	).

% In the description below, "AEv" represents an aev/14 term.

% A Final Mapping is a term of the form
% AEv-[]

% A Temporary Mapping is either a final mapping or a term of the form
% AEev-ListofTempMappings

% Use the original algorithm to create InitialMappings,
% a list of terms of the form AEv-ListOfNonInteractingAEvs
% NonInteractingAEvs is a possibly empty list of AEvs.
% If NonInteractingAEvs == [],    the mapping is final;
% if NonInteractingAEvs == [_|_], the mapping needs to be expanded.
expand_aevs([], _Depth, MappingsCount, [], MappingsCount).
expand_aevs([H|T], Depth, MappingsCountIn, MappingsTree, MappingsCountOut) :-
	% length(AEvaluations, AEvaluationsLength),
	% debug_message(trace, '~N### Expanding ~w AEvs~n', [AEvaluationsLength]),
	create_initial_mappings([H|T], InitialMappings),
	length(InitialMappings, InitialMappingsLength), 
	% format(user_output, '~d: ~q~n~n', [InitialMappingsLength, InitialMappings]),
	Index is 1,
        expand_all_mappings(InitialMappings, Index, Depth, MappingsCountIn,
			    InitialMappingsLength, MappingsCountOut, MappingsTree).

create_initial_mappings([], []).
create_initial_mappings([FirstAEv|RestAEvs], [FirstInitMapping|RestInitMappings]) :-
	% Create a set of init mappings for only the top scoring candidates
	get_all_aev_features([phrasecomponents,low,high],
			     FirstAEv,
			     [PhraseComponents,Low,High]),
	% determine which AEvs in RestAEvs do not interact with FirstAEv
	find_non_interacting_aevs(RestAEvs, PhraseComponents, Low, High, NonInteractingAEvs),
	FirstInitMapping = FirstAEv-NonInteractingAEvs,
	create_initial_mappings(RestAEvs, RestInitMappings).

expand_all_mappings([], _Index, _Depth, MappingsCount, _NumMappings, MappingsCount, []).
expand_all_mappings([H|T], Index, Depth, MappingsCountIn, NumMappings, MappingsCountOut, [ExpandedH|ExpandedT]) :-
        H = AEv-AEvList,
	debug_call(expand, Padding is Depth*2),
	debug_call(expand, aev_print_version(AEv, PrintAEv)),
	debug_message(expand,
		      '~N### ~*cExpanding (depth ~d) ~w of ~w: ~q~n',
		      [Padding,32,Depth,Index,NumMappings,PrintAEv]),
        expand_one_mapping(AEvList, AEv, Depth, MappingsCountIn, MappingsCountNext, ExpandedH),
	Index1 is Index + 1,
        expand_all_mappings(T, Index1, Depth, MappingsCountNext, NumMappings, MappingsCountOut, ExpandedT).

% If X (i.e., AEvaluationsInOut) == [], the mapping is final.
% Otherwise, the mapping needs to be expanded.

% ExpandedMapping is
% AEv-[] or
% AEv-X, where X is a list of ExpandedMappings

expand_one_mapping([], AEv, _Depth, MappingsCountIn, MappingsCountOut, AEv-[]) :-
	MappingsCountOut is MappingsCountIn + 1.
expand_one_mapping([H|T], AEv, Depth, MappingsCountIn, MappingsCountOut, AEv-ExpandedMappings) :-
	Depth1 is Depth + 1,
        expand_aevs([H|T], Depth1, MappingsCountIn, ExpandedMappings, MappingsCountOut).

ev_print_version(Ev, PrintEv) :-
	get_all_candidate_features([negvalue,cui,metaterm,metaconcept,metawords],
				   Ev,
				   [NegValue,CUI,MetaTerm,MetaConcept,MetaWords]),
	PrintEv = ev(NegValue,CUI,MetaTerm,MetaConcept,MetaWords).

aev_print_version(AEv, PrintAEv) :-
	get_all_aev_features([phrasecomponents,low,high,negvalue,cui,
			      metaterm,metaconcept,metawords],
			    AEv,
			    [PhraseComponents,Low,High,NegValue,CUI,
			     MetaTerm,MetaConcept,MetaWords]),
	PrintAEv = aev(PhraseComponents,Low,High,
		       NegValue,CUI,MetaTerm,MetaConcept,MetaWords).

% % assemble_all_mappings/2 is called on a list of ExpandedMappings
% % to transform a tree structure into a nested list structure
% assemble_all_mappings([], _AEvs, []).
% assemble_all_mappings([H|T], AEvs, [AssembledH|AssembledT]) :-
%         H = X-Y,
%         assemble_mappings_1(Y, X, AEvs, AssembledH),
%         assemble_all_mappings(T, AEvs, AssembledT).
% 
% % A Mapping is a list of aev/14 terms
% assemble_mappings_1([], Y, AEvs, Result) :-
% 	% append([Y], AEvs, Result).
% 	Result = [Y|AEvs].
% assemble_mappings_1([H|T], CurrentMapping, AEvs, Mappings) :-
%         assemble_all_mappings([H|T], [CurrentMapping|AEvs], Mappings).

assemble_all_mappings_dl([], _NPhraseWords, _Variants, _AEvs, Mappings, Mappings).
assemble_all_mappings_dl([H|T], NPhraseWords, Variants, AEvs, MappingsIn, MappingsOut) :-
        H = X-Y,
        assemble_mappings_1_dl(Y, X, NPhraseWords, Variants, AEvs, MappingsIn, MappingsNext),
        assemble_all_mappings_dl(T, NPhraseWords, Variants, AEvs, MappingsNext, MappingsOut).

% A Mapping is a list of aev/14 terms
% assemble_mappings_1_dl([], Y, AEvs, [Result|Rest], Rest) :-
assemble_mappings_1_dl([], Y, NPhraseWords, Variants, AEvs, MappingsIn, MappingsOut) :-
	ThisMapping = [Y|AEvs],
	deaugment_evaluations(ThisMapping, DeaugmentedMapping),
	% ( member(Candidate, DeaugmentedMapping),
	%   arg(3, Candidate, Word),
	%   format(user_output, '~w ', [Word]),
	%   fail
	% ; nl(user_output)
	% ),
	Conj = 0,
	compute_confidence_value(DeaugmentedMapping, Conj, NPhraseWords, Variants, NegValue),
	MappingsOut = [NegValue-DeaugmentedMapping|MappingsIn].
assemble_mappings_1_dl([H|T], CurrentMapping, NPhraseWords,
		       Variants, AEvs, MappingsIn, MappingsOut) :-
        assemble_all_mappings_dl([H|T], NPhraseWords, Variants,
				 [CurrentMapping|AEvs], MappingsIn, MappingsOut).

aevaluations_interact(PhraseComponents0, Low0, High0,
		      PhraseComponents1, Low1, High1) :-
	( components_intersect_components(PhraseComponents0, PhraseComponents1) ->
	  true
	; spans_overlap(Low0, High0, Low1, High1) ->
	  true
	; is_proper_subspan(Low1, High1, Low0, High0),
	  component_intersects_components(PhraseComponents0, [Low1,High1]) ->
	  true
	; is_proper_subspan(Low0, High0, Low1, High1),
	  component_intersects_components(PhraseComponents1, [Low0,High0])
	).

/* find_non_interacting_aevs(+AEvaluationsIn, +FilterPhraseComponents,
			     +FilterLow, +FilterHigh, -AEvaluationsOut)

find_non_interacting_aevs/5
*/

find_non_interacting_aevs([], _FilterPhraseComponents, _FilterLow, _FilterHigh, []).
find_non_interacting_aevs([FirstAEv|RestAEvs], FilterPhraseComponents, FilterLow, FilterHigh, Result) :-
	get_all_aev_features([phrasecomponents,low,high],
			     FirstAEv,
			     [PhraseComponents,Low,High]),
	( aevaluations_interact(PhraseComponents, Low, High,
				FilterPhraseComponents, FilterLow, FilterHigh) ->
	  Result = FilteredRest
	; Result = [FirstAEv|FilteredRest]
	),
	find_non_interacting_aevs(RestAEvs, FilterPhraseComponents,
				  FilterLow, FilterHigh, FilteredRest).

/* components_intersect_components(+Components1, +Components2)

components_intersect_components/2
*/

components_intersect_components([First|Rest], Components) :-
	( component_intersects_components(Components, First) ->
	  true
	; components_intersect_components(Rest,Components)
	).

/* compute_component_span(+Components, -Low, -High)

compute_component_span/3
*/

compute_component_span(Components, Low, High) :-
	Components = [[Low,_]|_],
	% reversed order of args from QP library version!
	last(Components, [_,High]).

/* spans_overlap(+Low1, +High1, +Low2, +High2)

spans_overlap/4
*/

% There are two possibilities:
% (1)
%                 L2                H2
%                  |-----------------|
%        L1                H1
%         |-----------------|

% (2)
%                 L1                H1
%                  |-----------------|
%        L2                H2
%         |-----------------|

spans_overlap(Low1, High1, Low2, High2) :-
	( Low1 =< Low2,
	  Low2 =< High1,
	  High1 =< High2 ->
	  true
	; Low2 =< Low1,
	  Low1 =< High2,
	  High2 =< High1
	).

posinfo_lists_overlap(PosInfoList1, PosInfoList2) :-
	member(StartPos1/Length1, PosInfoList1),
	EndPos1 is StartPos1 + Length1,
	member(StartPos2/Length2, PosInfoList2),
	EndPos2 is StartPos2 + Length2,
	spans_overlap(StartPos1, EndPos1, StartPos2, EndPos2),
	!.

/* is_proper_subspan(+Low1, +High1, +Low2, +High2)

is_proper_subspan/4
*/

%   L2                        H2
%    |-------------------------|
%          L1          H1
%           |-----------|


is_proper_subspan(Low1, High1, Low2, High2) :-
	Low2 < Low1,
	High1 < High2.

/* filter_out_subsumed_mappings(+Mappings, -FilteredMappings)

filter_out_subsumed_mappings/2

A mapping M1 is subsumed by another M2 if all of M1's components occur in M2.

% filter_out_subsumed_mappings([], []).
% filter_out_subsumed_mappings([First|Rest], Result) :-
%       ( mapping_is_subsumed(Rest, First) ->
%         Result = FilteredRest
%       ; Result = [First|FilteredRest]
%       ),
%       filter_out_subsumed_mappings(Rest, FilteredRest).
*/

% The following is a more complicated, but more efficient way
% of filtering out subsumed mappings. It's more efficient because
% the old way called intersection/3 on the entire mappings, which required
% a lot of unification on the mappings terms.
% This method uses the same strategy, but unifies only CUIs and the MetaString.

filter_out_subsumed_mappings_chunked(MappingsIn, ChunkSize, FilteredMappings) :-
	length(MappingsIn, MappingsLength),
	( MappingsLength =< ChunkSize ->
	  filter_out_subsumed_mappings(MappingsIn, ChunkSize, 1, 1, FilteredMappings)
	; debug_message(trace, '~N### Splitting list of ~w~n', [MappingsLength]),
	  split_list(MappingsIn, 0, ChunkSize, MappingsChunked, 1, NumLists),
	  filter_out_each(MappingsChunked, ChunkSize, 1, NumLists, FilteredMappingsChunked),
	  append(FilteredMappingsChunked, TempFilteredMappings),
	  length(TempFilteredMappings, TempFilteredMappingsLength),
	  % filter_out_subsumed_mappings(TempFilteredMappings, 1, 1, FilteredMappings),
	  filter_out_subsumed_mappings_again(MappingsLength, ChunkSize,
					     TempFilteredMappingsLength,
					     TempFilteredMappings, FilteredMappings)
	).

% Continue splitting the list of mappings iff the previous split-and-filter pass
% reduced the number of mappings by at least 20%.
filter_out_subsumed_mappings_again(MappingsLength, ChunkSize, TempFilteredMappingsLength,
				   TempFilteredMappings, FilteredMappings) :-
	Ratio is MappingsLength / TempFilteredMappingsLength,
	debug_message(trace,
		      '~N### Ratio of ~w to ~w is ~w~n',
		      [MappingsLength,TempFilteredMappingsLength,Ratio]),
	( Ratio > 1.10 ->
	  filter_out_subsumed_mappings_chunked(TempFilteredMappings, ChunkSize, FilteredMappings)
	; filter_out_subsumed_mappings(TempFilteredMappings, ChunkSize, 1, 1, FilteredMappings)
	).	

split_list([], _N, _Limit, [[]], NumLists, NumLists).
split_list([H|T], N, Limit, ListOfLists, ListCount, NumLists) :-
	( N =:= Limit ->
	  ListOfLists = [[]|RestLists],
	  NextListCount is ListCount + 1,
	  split_list([H|T], 0, Limit, RestLists, NextListCount, NumLists)
	; ListOfLists = [[H|Rest]|RestLists],
	  N1 is N + 1,
	  split_list(T, N1, Limit, [Rest|RestLists], ListCount, NumLists)
	).

filter_out_each([], _ChunkSize, _ListCount, _NumLists, []).
filter_out_each([H|T], ChunkSize, ListCount, NumLists, [FilteredH|FilteredT]) :-
	filter_out_subsumed_mappings(H, ChunkSize, ListCount, NumLists, FilteredH),
	NextListCount is ListCount + 1,
	filter_out_each(T, ChunkSize, NextListCount, NumLists, FilteredT).

% This is the basic filtering predicate that operates on lists of mappings
filter_out_subsumed_mappings(Mappings, ChunkSize, ListCount, NumLists, FilteredMappings) :-
	debug_call(trace, length(Mappings, MappingsLength)),
	debug_message(trace, '~N### Filtering ~w of ~w: ~w',
		      		[ListCount, NumLists, MappingsLength]),
        filter_out_subsumed_mappings_aux(Mappings, ChunkSize, 0, FilteredMappings),
	debug_call(trace, length(FilteredMappings, FilteredMappingsLength)),
	debug_message(trace, ' --> ~w~n', [FilteredMappingsLength]).


filter_out_subsumed_mappings_aux([], _ChunkSize, _N, []).
filter_out_subsumed_mappings_aux([FirstScore:FirstData-FirstMapping|RestMappings],
				 ChunkSize, MappingsFiltered, Result) :-
	( mapping_is_subsumed(RestMappings, FirstData-FirstMapping) ->
          Result = FilteredRest
	  % Remove FirstData
        ; Result = [FirstScore:FirstData-FirstMapping|FilteredRest]
        ),
	( MappingsFiltered > 0,
	  0 is MappingsFiltered mod ChunkSize ->
	  debug_message(trace, '~N### Filtered ~w~n', [MappingsFiltered])
	; true
	),
	MappingsFiltered1 is MappingsFiltered + 1,
        filter_out_subsumed_mappings_aux(RestMappings, ChunkSize, MappingsFiltered1, FilteredRest).

/* mapping_is_subsumed(+Mappings, +Mapping)

mapping_is_subsumed/2
WATCH ORDER OF ARGS

% mapping_is_subsumed([First|Rest], Mapping) :-
%       ( intersection(Mapping, First, Mapping) ->
%         true
%       ; mapping_is_subsumed(Rest, Mapping)
%       ).

*/

mapping_is_subsumed([_Score:FirstMappingData-_FirstMapping|RestMappings], ThisMappingData-ThisMapping) :-
        ( intersection(ThisMappingData, FirstMappingData, ThisMappingData) ->
          true
        ; mapping_is_subsumed(RestMappings, ThisMappingData-ThisMapping)
        ).

% prepend_data([], []).
% prepend_data([FirstMapping|RestMappings], [Data-FirstMapping|RestMappingsWithMSs]) :-
%         get_data_for_mapping(FirstMapping, Data),
%         prepend_data(RestMappings, RestMappingsWithMSs).

get_data_for_mapping([], []).
get_data_for_mapping([FirstEval|RestEvals], [FirstCUI/FirstMS|RestData]) :-
	get_all_candidate_features([cui,metaterm], FirstEval, [FirstCUI,FirstMS]),
        get_data_for_mapping(RestEvals, RestData).


% This predicate combines the de-augmentation and re-ordering
% so that the entire list need not be traversed twice.
% Mappings come in as ConfidenceScore-Mapping, and they are transformed to
% ConfidenceScore:PrependingData-Mappinging (:/2 is the principal function symbol).
reorder_and_prepend_all_mappings([], []).
reorder_and_prepend_all_mappings([Score-H|T], [Score:HNew|TNew]) :-
	% sort(H, SortedH),
	% deaugment_mapping_evaluations(SortedH, DeAugmentedAndReorderedH),
	reorder_mapping(H, ReorderedH),
	get_data_for_mapping(ReorderedH, PrependingData),
	HNew = PrependingData-ReorderedH,
	reorder_and_prepend_all_mappings(T, TNew).
	

/* deaugment_evaluations(+Mapping, -DeaugmentedMapping)

deaugment_evaluations/2
*/

deaugment_evaluations([], []).
deaugment_evaluations([FirstAEv|RestAEvs], [FirstEv|RestEvs]) :-
	deaugment_one_evaluation(FirstAEv, FirstEv),
	deaugment_evaluations(RestAEvs, RestEvs).

deaugment_one_evaluation(AEvTerm, EvTerm) :-
	augmented_candidate_term(_PhraseComponents, _Low, _High, NegValue, CUI, MetaTerm,
				 MetaConcept, MetaWords, SemTypes, MatchMap,
				 LSComponents, TargetLSComponent, InvolvesHead,
				 IsOvermatch, SourceInfo, PosInfo, Status, Negated, AEvTerm),
	candidate_term(NegValue, CUI, MetaTerm, MetaConcept, MetaWords, SemTypes,
		       MatchMap, LSComponents, TargetLSComponent, InvolvesHead,
		       IsOvermatch, SourceInfo,  PosInfo, Status, Negated, EvTerm).

/* reorder_mapping(+Mapping, -OrderedMapping)

reorder_mapping/2
*/

reorder_mapping([], []).
reorder_mapping([H|T], OrderedMapping) :-
	prepend_phrase_maps([H|T], PrependedMapping),
	keysort(PrependedMapping, OrderedPrependedMapping),
	% to delete the phrase maps,
	% simply call prepend_phrase_maps/2 with the args reversed!
	prepend_phrase_maps(OrderedMapping, OrderedPrependedMapping).

/* prepend_phrase_maps(+Mapping, -PPMapping)

prepend_phrase_maps/2
*/

prepend_phrase_maps(MapsIn, MapsOut) :-
	( var(MapsIn) ->
	  prepend_phrase_maps_1(MapsOut, MapsIn)
	; prepend_phrase_maps_1(MapsIn, MapsOut)
	).

prepend_phrase_maps_1([], []).
prepend_phrase_maps_1([H|T], [PrePendedH|PrePendedT]) :-
	prepend_one_phrase_map(H, PrePendedH),
	prepend_phrase_maps_1(T, PrePendedT).

prepend_one_phrase_map(EvTerm, MatchMap-EvTerm) :-
	EvTerm \= _-_,
	!,
	get_candidate_feature(matchmap, EvTerm, MatchMap).
prepend_one_phrase_map(MatchMap-EvTerm, EvTerm) :-
	get_candidate_feature(matchmap, EvTerm, MatchMap).

/* augment_phrase_with_mappings(+Mappings, +Phrase, +PhraseWordInfoPair, -APhrases)
   augment_lphrase_with_mappings(+Mappings, +LPhrase, +LPhraseMap, +NPhraseWords, -APhrases)
   augment_lphrase_with_mapping(+Mapping, +LPhrase, +LPhraseMap, +NPhraseWords, -APhrase)

augment_phrase_with_mappings/4
augment_lphrase_with_mapping/5
*/

augment_phrase_with_mappings([], _Phrase, _PhraseWordInfoPair, _Variants, []).
augment_phrase_with_mappings([H|T], Phrase, PhraseWordInfoPair, Variants, APhrases) :-
	MappingsList = [H|T],
	PhraseWordInfoPair = _AllPhraseWordInfo:FilteredPhraseWordInfo,
	% PhraseWordInfo = FilteredPhraseWordInfo,
	FilteredPhraseWordInfo = pwi(PhraseWordL, _PhraseHeadWordL, PhraseMap),
	PhraseWordL = wdl(_, LCPhraseWords),
	linearize_phrase(Phrase, PhraseMap, LPhrase, LPhraseMap),
	length(LCPhraseWords, NPhraseWords),
	length(MappingsList, MappingsListLength),
	augment_lphrase_with_mappings(MappingsList, LPhrase, LPhraseMap, MappingsListLength,
				      NPhraseWords, Variants, APhrases).

augment_lphrase_with_mappings([], _LPhrase, _LPhraseMap, _MappingsCount, _NPW, _Vars, []).
augment_lphrase_with_mappings([FirstMapping|RestMappings], LPhrase, LPhraseMap, MappingsCount,
			      NPhraseWords, Variants, [AugmentedFirst|AugmentedRest]) :-
	augment_lphrase_with_one_mapping(FirstMapping, LPhrase, LPhraseMap,
					 NPhraseWords, Variants, AugmentedFirst),
	% !,
	% ( 0 is MappingsCount mod 1000 ->
	%   format(user_output, '~d~n', [MappingsCount]),
	%   ( 0 is MappingsCount mod 100000 ->
	%     garbage_collect,
	%     garbage_collect_atoms
	%   ; true
	%   )
	% ; true
	% ),
	NextCount is MappingsCount - 1,
	augment_lphrase_with_mappings(RestMappings, LPhrase, LPhraseMap, NextCount,
				      NPhraseWords, Variants, AugmentedRest).

augment_lphrase_with_one_mapping(NegValue-Mapping, LPhraseIn, LPhraseMapIn,
				 _NPhraseWords, _Variants, APhrase) :-
	augment_lphrase_with_meta_concepts(Mapping, LPhraseIn, LPhraseMapIn,
					   LPhraseInOut, LPhraseMapInOut),
	add_confidence_value(LPhraseInOut, NegValue, LPhraseOut),
	append(LPhraseMapInOut, [[0]], LPhraseMapOut),
	% format(user_output, 'NegValue == ~d~n', [NegValue]),
	APhrase = ap(NegValue,LPhraseOut,LPhraseMapOut,Mapping).

/* augment_lphrase_with_meta_concepts(+LPhraseIn, +LPhraseMapIn, +Evaluations,
                                      -LPhraseOut, -LPhraseMapOut)
   augment_lphrase_with_meta_concept(+LPhraseIn, +LPhraseMapIn, +Evaluation,
                                     -LPhraseOut, -LPhraseMapOut)

augment_lphrase_with_meta_concepts/5
augment_lphrase_with_meta_concept/5
*/

augment_lphrase_with_meta_concepts([], LPhraseIn, LPhraseMapIn,
                                   LPhraseIn, LPhraseMapIn).
augment_lphrase_with_meta_concepts([FirstConcept|RestConcepts], LPhraseIn, LPhraseMapIn,
                                   LPhraseOut, LPhraseMapOut) :-
	augment_lphrase_with_one_meta_concept(LPhraseIn, LPhraseMapIn, FirstConcept,
					      LPhraseInOut, LPhraseMapInOut),
	% format(user_output, '~q~n~q~n~n', [LPhraseIn, LPhraseInOut]),
	augment_lphrase_with_meta_concepts(RestConcepts, LPhraseInOut, LPhraseMapInOut,
					   LPhraseOut, LPhraseMapOut).

augment_lphrase_with_one_meta_concept(LPhraseIn, LPhraseMapIn, Evaluation,
				      LPhraseOut, LPhraseMapOut) :-
	%% AHA!!
	get_all_candidate_features([cui,metaconcept,semtypes,lscomponents,targetlscomponent],
				   Evaluation,
				   [CUI,MetaConcept,SemTypes,LSComponents,TargetLSComponent]),
	% compute_target_LS_component(MatchMap, LSComponents, TargetLSComponent),
	MetaInfo = MetaConcept:CUI:SemTypes,
	% Initialize accumulators
	% RevLexMatch   = [],
	% RevInputMatch = [],
	% RevTokens     = [],
	% RevBases      = [],
	% get_inputmatch_atoms_from_phrase(LPhraseIn, InputMatchAtoms),
	% format(user_output, 'Concept:         ~q~n',    [Evaluation]),
	% format(user_output, 'LSComponents:    ~q/~q~n', [LSComponents,TargetLSComponent]),
	% format(user_output, 'InputMatchAtoms: ~q~n',    [InputMatchAtoms]),
	% format(user_output, 'LPhraseMap:      ~q~n~n',  [LPhraseMapIn]),
	join_phrase_items(LPhraseIn, LPhraseMapIn, MetaInfo,
			  LSComponents, TargetLSComponent,
			  LPhraseOut, LPhraseMapOut).

% compute_target_LS_component(MatchMap, LSComponents, TargetLSComponent) :-
% 	extract_components(MatchMap, PhraseComponents, _MetaComponents),
% 	linearize_components(PhraseComponents, LSComponents0),
% 	% LSComponents is a list of integers representing
% 	% all phrase positions covered by the string in the ev/11 term.
% 	append(LSComponents0, LSComponents),
% 	% TargetLSComponent is the last phrase position covered.
% 	last(LSComponents, TargetLSComponent).

/* 

Suppose LexMatch starts out as LexMatch, LexMatchTail

   join_phrase_items(+LPhraseIn, +LPhraseMapIn, +MetaInfo, +LSComponents,
                     +TargetLSComponent, -LPhraseOut, -LPhraseMapOut)
   join_phrase_items(+LPhraseIn, +LPhraseMapIn,
                     +MetaInfo, +LSComponents, +TargetLSComponent,
                     +RevLexMatch, +RevInputMatch, +RevTokens,
                     -LPhraseOut, -LPhraseMapOut)

join_phrase_items/7
join_phrase_items/11
*/

join_phrase_items(LPhraseIn, LPhraseMapIn, MetaInfo,
		  LSComponents, TargetLSComponent,
		  LPhraseOut, LPhraseMapOut) :-
	join_phrase_items_aux(LPhraseIn, LPhraseMapIn, MetaInfo,
			      LSComponents, TargetLSComponent,
			      [[]|LexMatchTail],   LexMatchTail,
			      [[]|InputMatchTail], InputMatchTail,
			      [[]|TokensTail],     TokensTail,
			      [[]|BasesTail],      BasesTail,
			      LPhraseOut, LPhraseMapOut).

% LSComponents is the linearized phrase positions covered
% by the string for the concept being worked on, e.g., [1,2,3].
% TargetLSComponent is the *last* phrase position covered
% by the string for the concept being worked on, e.g., 3..

% LP == "Linearized Phrase"
% LS == "Linearized String"

% Loop through all the linearized phrase items and their linearized phrase components.

% Is the current linearized phrase position (FirstLPMapComponent) the same as
% the last linearized phrase position of the string (TargetLSComponent)?
join_phrase_items_aux([FirstLPItem|RestLPItems], [[TargetLSComponent]|RestLPMap],
		      MetaInfo, LSComponents,      TargetLSComponent,
		      LexMatchList, LexMatchTail, InputMatchList, InputMatchTail,
		      TokensList, TokensTail, _BasesList, _BasesTail,
		      [NewFirstLPItem|RestLPItems], [LSComponents|RestLPMap]) :-
	!,	
	% retrieve accumulated results and finish up
	% Suppose FirstLPItem is
	% head([lexmatch([role]),inputmatch([role]),tag(noun),tokens([role])])
	% Then
	% ItemName  = head (or mod, verb, etc.)
	% SubItemsIn = [lexmatch([role]),inputmatch([role]),tag(noun),tokens([role])]
	get_phrase_item_name(FirstLPItem,     ItemName),
	get_phrase_item_subitems(FirstLPItem, SubItemsIn),
	get_all_subitems_features(SubItemsIn, CurrLexMatch, CurrInputMatch, CurrTokens),
	% get_subitems_feature(SubItemsIn, lexmatch,   CurrLexMatch),
	% get_subitems_feature(SubItemsIn, inputmatch, CurrInputMatch),
	% get_subitems_feature(SubItemsIn, tokens,     CurrTokens),
	% get_subitems_feature(SubItemsIn, bases,      CurrBases),
	% get_subitems_feature(SubItemsIn, metaconc,   CurrMetaConc),

	( memberchk_var(CurrLexMatch, LexMatchList) ->
	  LexMatchTail = []
	; LexMatchTail = [CurrLexMatch]
	),
	append(LexMatchList, LexMatch),

	( memberchk_var(CurrInputMatch, InputMatchList) ->
	  InputMatchTail = []
	; InputMatchTail = [CurrInputMatch]
	),
	append(InputMatchList, InputMatch),

	TokensTail = [CurrTokens],
	append(TokensList, Tokens),

	% BasesTail = [CurrBases],
	% append(BasesList, Bases),

	% test_diff(lexmatch,   CurrLexMatch,   LexMatch),
	% test_diff(inputmatch, CurrInputMatch, InputMatch),
	% test_diff(tokens,     CurrTokens,     Tokens),
	% test_diff(bases,      CurrBases,      Bases),
	% test_diff(metaconc,   CurrMetaConc,   MetaInfo),

	set_all_subitems_features(SubItemsIn, LexMatch, InputMatch, Tokens, MetaInfo, SubItemsOut),
	new_phrase_item(ItemName, SubItemsOut, NewFirstLPItem).

% If the current linearized phrase position (FirstLPMapComponent)
% is one of the linearized phrase positions of the string (LSComponent),
% then add to the accumulators the current lexmatch, inputmatch, tokens, and bases.
join_phrase_items_aux([FirstLPItem|RestLPItems], [[FirstLPMapComponent]|RestLPMap],
		      MetaInfo, LSComponents, TargetLSComponent,
		      LexMatchList, LexMatchTail, InputMatchList, InputMatchTail,
		      TokensList, TokensTail, _BasesList, _BasesTail,
		      JoinedRestLPItems, JoinedRestLPMap) :-
	memberchk(FirstLPMapComponent, LSComponents),
	!,
	% accumulate
	get_phrase_item_subitems(FirstLPItem, SubItems),
	get_all_subitems_features(SubItems, CurrLexMatch, CurrInputMatch, CurrTokens),
	% get_subitems_feature(SubItems, lexmatch,   CurrLexMatch),
	% get_subitems_feature(SubItems, inputmatch, CurrInputMatch),
	% get_subitems_feature(SubItems, tokens,     CurrTokens),
	% get_subitems_feature(SubItems, bases,      CurrBases),

	( memberchk_var(CurrLexMatch, LexMatchList) ->
	  NewLexMatchTail = LexMatchTail
	; LexMatchTail = [CurrLexMatch|NewLexMatchTail]
	),

	( memberchk_var(CurrInputMatch, InputMatchList) ->
	  NewInputMatchTail = InputMatchTail
	; InputMatchTail = [CurrInputMatch|NewInputMatchTail]
	),

	TokensTail = [CurrTokens|NewTokensTail],

	% BasesTail = [CurrBases|NewBasesTail],

	join_phrase_items_aux(RestLPItems, RestLPMap,
			      MetaInfo, LSComponents, TargetLSComponent,
			      LexMatchList, NewLexMatchTail, InputMatchList, NewInputMatchTail,
			      TokensList, NewTokensTail, _BasesList, _NewBasesTail,
			      JoinedRestLPItems, JoinedRestLPMap).
join_phrase_items_aux([FirstLPItem|RestLPItems], [FirstLPMap|RestLPMap],
		      MetaInfo, LSComponents, TargetLSComponent,
		      LexMatchList, LexMatchTail, InputMatchList, InputMatchTail,
		      TokensList, TokensTail, _BasesList, _BasesTail,
		      [FirstLPItem|JoinedRestLPItems], [FirstLPMap|JoinedRestLPMap]) :-
	% Preserve the FirstLPItem and its FirstLPMap unchanged,
	% because this phrase item did not match the concept.
	join_phrase_items_aux(RestLPItems, RestLPMap,
			      MetaInfo, LSComponents, TargetLSComponent,
			      LexMatchList, LexMatchTail, InputMatchList, InputMatchTail,
			      TokensList, TokensTail, _BasesList, _BasesTail,
			      JoinedRestLPItems, JoinedRestLPMap).

% The SubItems list will be of one of these three forms:
% [lexmatch(_),   inputmatch(_), tag(_), tokens(_)]
% [inputmatch(_), features(_),   tokens(_)]
% [inputmatch(_), tag(_),        tokens(_)]

% The next two predicates are ugly, but avoid recursing down the SubItems list.
get_all_subitems_features(SubItems, LexMatch, InputMatch, Tokens) :-
	( SubItems = [lexmatch(LexMatch), inputmatch(InputMatch), tag(_), tokens(Tokens)] ->
	  true
	; SubItems = [inputmatch(InputMatch), features(_), tokens(Tokens)] ->
	  LexMatch = []
	; SubItems = [inputmatch(InputMatch), tag(_), tokens(Tokens)] ->
	  LexMatch = []
	  % This shouldn't be needed, but who knows...
	; get_subitems_feature(SubItems, lexmatch,   LexMatch),
	  get_subitems_feature(SubItems, inputmatch, InputMatch),
	  get_subitems_feature(SubItems, tokens,     Tokens)
	).
			  
set_all_subitems_features(SubItemsIn, LexMatch, InputMatch, Tokens, MetaConc, SubItemsOut) :-
	( SubItemsIn  = [lexmatch(_), inputmatch(_), tag(Tag), tokens(_)] ->	
	  SubItemsOut = [lexmatch(LexMatch), inputmatch(InputMatch), tag(Tag),
			 tokens(Tokens), metaconc(MetaConc)]
	; SubItemsIn  = [inputmatch(_), features(Features), tokens(_)] ->
	  SubItemsOut = [lexmatch(LexMatch), inputmatch(InputMatch), features(Features),
			 tokens(Tokens), metaconc(MetaConc)]
	; SubItemsIn  = [inputmatch(_), tag(Tag),tokens(_)] ->
	  SubItemsOut = [lexmatch(LexMatch), inputmatch(InputMatch), tag(Tag),
			 tokens(Tokens), metaconc(MetaConc)]
	; set_subitems_feature(SubItemsIn, lexmatch,   LexMatch,   SubItems1),
	  set_subitems_feature(SubItems1,  inputmatch, InputMatch, SubItems2),
	  set_subitems_feature(SubItems2,  tokens,     Tokens,     SubItems3),
	  set_subitems_feature(SubItems3,  metaconc,   [MetaConc], SubItemsOut)
	).

compute_confidence_value([], _Conj, _NPhraseWords, _Variants, -1000).
compute_confidence_value([H|T], Conj, NPhraseWords, Variants, NegValue) :-
	compute_conf_val_aux(Conj, [H|T], NPhraseWords, Variants, NegValue).

% This is the conj case.
compute_conf_val_aux(1, Mapping, _NPhraseWords, _Variants, NegValue) :-
	InitScoreSum = 0,
	InitEvalCount = 0,
	% format(user_output, 'MAPPING: ~w~n', [Mapping]),
	average_eval_scores(Mapping,
			    InitEvalCount, _FinalEvalCount,
			    InitScoreSum, _FinalScoreSum,
			    NegValue).

% This is the default non-conj case.
compute_conf_val_aux(0, Mapping, NPhraseWords, Variants, NegValue) :-
	glean_info_from_mapping(Mapping, [], MatchMap0, [], TermLengths,
				0, NMetaWords, no, InvolvesHead, ExtraMetaWords),
	% format(user_output, '~w~n', [MatchMap0]),
	sort(MatchMap0, MatchMap),
	% We no longer consolidate MatchMaps because of
	% the thorny issue of combining the lexical variation components.
	% MatchMap1 = [MatchMapHead|MatchMapTail],
	% consolidate_matchmap(MatchMapTail, MatchMapHead, MatchMap),
	% The connected components are computed in the normal fashion for
	% the phrase; but for Meta, the components are simply the lengths
	% of the terms participating in the mapping.
	extract_components(MatchMap, PhraseComponents, _MetaComponents),
	connect_components(PhraseComponents, PhraseCCs),
	% sort(MetaComponents, SortedMetaComponents),
	% connect_components(MetaComponents, NewMetaCCs),
	MetaCCs = TermLengths,
	MatchCCs = [PhraseCCs,MetaCCs],
	maybe_debug_mapping_1(Mapping, MatchMap, MatchCCs),
	compute_match_value(MatchMap, MatchCCs, NPhraseWords, NMetaWords,
			    ExtraMetaWords, Variants, InvolvesHead, Value),
	maybe_debug_mapping_2,
	NegValue is -Value.

% Simply average all candidate scores, because the original method of
% computing a mapping's score assumes that each word appears in exactly one candidate,
% and the logic computes suspicious values.
average_eval_scores([], EvalCount, EvalCount, ScoreSum, ScoreSum, MappingNegValue) :-
	MappingNegValue is floor(1*(ScoreSum/EvalCount)).
average_eval_scores([FirstCandidate|RestCandidates],
		    EvalCountIn, EvalCount,
		    ScoreSumIn, ScoreSum, MappingNegValue) :-
	get_candidate_feature(negvalue, FirstCandidate, CandidateNegValue),
	EvalCountNext is EvalCountIn + 1,
	ScoreSumNext is ScoreSumIn + CandidateNegValue,
	average_eval_scores(RestCandidates,
			    EvalCountNext, EvalCount,
			    ScoreSumNext, ScoreSum, MappingNegValue).

maybe_debug_mapping_1(Mapping, MatchMap, MatchCCs) :-
	( control_value(debug, DebugFlags),
	  memberchk(5, DebugFlags) ->
	  glean_concepts_from_mapping(Mapping, Concepts),
	  format(user_error, '~n',[]),
	  wl(Concepts),
	  format(user_error, '~p~n~p~n',[MatchMap,MatchCCs])
	; true
	).

maybe_debug_mapping_2 :-
	( control_value(debug, DebugFlags),
	  memberchk(5, DebugFlags) ->
	  format(user_error, '~n',[])
	; true
	).

/* glean_info_from_mapping(+Mapping, +MatchMapIn, -MatchMapOut,
                           +TermLengthsIn, -TermLengthsOut,
                           +NMetaWordsIn, -NMetaWordsOut,
                           +InvolvesHeadIn, -InvolvesHeadOut)
   glean_concepts_from_mapping(+Mapping, -Concepts)

glean_info_from_mapping/7
*/

glean_info_from_mapping([], MatchMapIn, MatchMapIn,
                        TermLengthsIn, TermLengthsIn,
                        NMetaWordsIn, NMetaWordsIn,
                        InvolvesHeadIn, InvolvesHeadIn, []).
glean_info_from_mapping([FirstCandidate|RestCandidates], MatchMapIn, MatchMapOut,
                        TermLengthsIn, TermLengthsOut,
                        NMetaWordsIn, NMetaWordsOut,
                        InvolvesHeadIn, InvolvesHeadOut, ExtraMetaWords) :-
	get_all_candidate_features([metawords,matchmap,involveshead],
				   FirstCandidate,
				   [MetaWords,MatchMap0,InvolvesHead]),
	maybe_modify_matchmap_for_concatenation(MatchMap0, MatchMapIn, NMetaWordsIn, MatchMapInOut),
	% modify_matchmap_for_concatenation(MatchMap0, NMetaWordsIn, MatchMap),
	% append(MatchMapIn, MatchMap, MatchMapInOut),
	( MatchMapIn == MatchMap0 ->
	  TermLengthsInOut = TermLengthsIn,
	  NMetaWordsInOut is NMetaWordsIn
	; length(MetaWords, NMetaWords),
	  append(TermLengthsIn, [NMetaWords], TermLengthsInOut),
	  NMetaWordsInOut is NMetaWordsIn + NMetaWords
	),
	( InvolvesHead == yes ->
	  InvolvesHeadInOut = yes
	; InvolvesHeadInOut = InvolvesHeadIn
	),
	compute_extra_meta(MatchMap0, MetaWords, FirstExtra),
	append(FirstExtra, RestExtra, ExtraMetaWords),
	glean_info_from_mapping(RestCandidates, MatchMapInOut, MatchMapOut,
				TermLengthsInOut, TermLengthsOut,
				NMetaWordsInOut, NMetaWordsOut,
				InvolvesHeadInOut, InvolvesHeadOut, RestExtra).

glean_concepts_from_mapping([], []).
glean_concepts_from_mapping([FirstCandidate|RestCandidates], [MetaConcept|RestConcepts]) :-
	get_candidate_feature(metaconcept, FirstCandidate, MetaConcept),
	glean_concepts_from_mapping(RestCandidates, RestConcepts).


/* modify_matchmap_for_concatenation(+MatchMapIn, +NMetaWords, -MatchMapOut)

modify_matchMap_for_concatenation/3
*/

maybe_modify_matchmap_for_concatenation(MatchMap0, MatchMapIn, NMetaWordsIn, MatchMapInOut) :-
	( MatchMap0 = MatchMapIn ->
	  MatchMapInOut = MatchMap0
	; MatchMapIn == [] ->
	  MatchMapInOut = MatchMap0
	; modify_matchmap_for_concatenation(MatchMap0, NMetaWordsIn, MatchMap),
	  append(MatchMapIn, MatchMap, MatchMapInOut)
	).
	% format(user_output, '~w~n~w~n~w~n~w~n~n', [MatchMapIn, MatchMap0, NMetaWordsIn, MatchMapInOut]).

modify_matchmap_for_concatenation(MatchMapIn, 0, MatchMapIn) :- !.
modify_matchmap_for_concatenation([], _NMetaWords, []).
modify_matchmap_for_concatenation([[PhraseComponent,MetaComponent,VarLevel]|Rest],
                                  NMetaWords,
                                  [[PhraseComponent,ModifiedMetaComponent,VarLevel]|ModifiedRest]) :-
	MetaComponent = [Begin,End],
	NewBegin is Begin + NMetaWords,
	NewEnd is End + NMetaWords,
	ModifiedMetaComponent = [NewBegin,NewEnd],
	modify_matchmap_for_concatenation(Rest, NMetaWords, ModifiedRest).

/* add_confidence_value(+LPhraseIn, +Value, -LPhraseOut)

add_confidence_value/3
*/

% Simply cons the confid(_) term rather than appending to save memory
add_confidence_value(LPhraseIn, Value, LPhraseOut) :-
	% append(LPhraseIn, [confid(Value)], LPhraseOut).
	LPhraseOut = [confid(Value)|LPhraseIn].

/* filter_evaluations_by_threshold(+Evaluations, -FilteredEvaluations)
   filter_evaluations_by_threshold(+Evaluations, +Threshold,
                                   -FilteredEvaluations)

filter_evaluations_by_threshold/2 retains only those Evaluations with
value Threshold or better.  */

% filter_evaluations_by_threshold(Evaluations, FilteredEvaluations) :-
% 	control_value(threshold, Threshold),
% 	NegThreshold is -Threshold,
% 	filter_evaluations_by_threshold(Evaluations, NegThreshold, FilteredEvaluations).

filter_evaluations_by_threshold([], _, []).
filter_evaluations_by_threshold([FirstCandidate|_Rest], NegThreshold, []) :-
	get_candidate_feature(negvalue, FirstCandidate, NegValue),
	NegValue > NegThreshold,
	!.
filter_evaluations_by_threshold([FirstCandidate|RestCandidates], NegThreshold,
				[FirstCandidate|FilteredRest]) :-
	filter_evaluations_by_threshold(RestCandidates, NegThreshold, FilteredRest).


/* filter_out_redundant_evaluations(+Evaluations, -FilteredEvaluations)
   filter_out_redundant_evaluations_aux(+Evaluations, -FilteredEvaluations)

filter_out_redundant_evaluations/2
filter_out_redundant_evaluations_aux/2

Evaluations are redundant if they involve the same concept and have the same
phrase involvement.
*/

filter_out_redundant_evaluations([], []).
filter_out_redundant_evaluations([First|Rest], FilteredEvaluations) :-
	prefix_with_CUI_and_pos_score([First|Rest], ListWithCUIPrefix),
	sort(ListWithCUIPrefix, SortedListWithCUIPrefix),
	% rev([First|Rest], RevEvaluations),
	filter_out_redundant_evaluations_aux(SortedListWithCUIPrefix, FilteredListWithCUIPrefix),
	remove_CUI_and_pos_score(FilteredListWithCUIPrefix, FilteredEvaluations0),
	sort(FilteredEvaluations0, FilteredEvaluations).

prefix_with_CUI_and_pos_score([], []).
prefix_with_CUI_and_pos_score([FirstIn|RestIn], [FirstOut|RestOut]) :-
	get_all_candidate_features([negvalue,cui], FirstIn, [NegValue,CUI]),
	PosValue is -NegValue,
	FirstOut = CUI-PosValue-FirstIn,
	prefix_with_CUI_and_pos_score(RestIn, RestOut).

remove_CUI_and_pos_score([], []).
remove_CUI_and_pos_score([_CUI-_PosScore-FirstIn|RestIn], [FirstIn|RestOut]) :-
	remove_CUI_and_pos_score(RestIn, RestOut).

filter_out_redundant_evaluations_aux([], []).
filter_out_redundant_evaluations_aux([First|Rest], Result) :-
	( evaluation_is_redundant(Rest, First) ->
	  Result = FilteredRest
	; Result = [First|FilteredRest]
	),
	filter_out_redundant_evaluations_aux(Rest, FilteredRest).


/* evaluation_is_redundant(+Evaluations, +Evaluation)

WATCH ORDER OF ARGS
evaluation_is_redundant/2 determines if Evaluation involves the same
concept and the same phrase involvement as one of Evaluations. */

evaluation_is_redundant([CUI2-_PosScore2-Candidate2|_Rest], CUI1-_PosScore1-Candidate1) :-
	get_candidate_feature(matchmap, Candidate2, MatchMap2),
	get_candidate_feature(matchmap, Candidate1, MatchMap1),

	( CUI1 == CUI2,
	  matchmaps_are_equivalent(MatchMap1, MatchMap2) ->
	  true
	; CUI1 \== CUI2 ->
	  !,
	  fail
	).
evaluation_is_redundant([_First|Rest], Evaluation) :-
	evaluation_is_redundant(Rest, Evaluation).

/* filter_out_subsumed_evaluations(+Evaluations, -FilteredEvaluations)
   filter_out_subsumed_evaluations_aux(+Evaluations, -FilteredEvaluations)

filter_out_subsumed_evaluations/2
filter_out_subsumed_evaluations_aux/2

An evaluation E1 is subsumed by another E2 if E1's score is strictly worse than E2's
and E1 and E2 have the same phrase involvement.

Subsumed evaluations are marked by "E" in MetaMap's human-readable output, e.g.,

  645   LIVER  (Liver Flavor) [food]          <------ NOT subsumed
  590 E Hepatic  [blor]                       <------ subsumed

*/

filter_out_subsumed_evaluations([], []).
filter_out_subsumed_evaluations([H|T], FilteredEvaluations) :-
        rev([H|T], RevEvaluations),
        filter_out_subsumed_evaluations_aux(RevEvaluations, RevFilteredEvaluations),
        rev(RevFilteredEvaluations, FilteredEvaluations).

filter_out_subsumed_evaluations_aux([], []).
filter_out_subsumed_evaluations_aux([First|Rest], Result) :-
        ( evaluation_is_subsumed(Rest, First) ->
          Result = FilteredRest
        ; Result = [First|FilteredRest]
        ),
        filter_out_subsumed_evaluations_aux(Rest, FilteredRest).


/* evaluation_is_subsumed(+Evaluations, +Evaluation)
   evaluation_is_subsumed_aux(+Evaluations, +NegValue, +MatchMap)
   evaluation_is_subsumed_aux(+Evaluations, +NegValue, +MatchMap, +SemTypes)

WATCH ORDER OF ARGS
evaluation_is_subsumed/2
evaluation_is_subsumed_aux/3,4

see filter_out_subsumed_evaluations/2 above.  */

evaluation_is_subsumed([H|T], Candidate) :-
	get_all_candidate_features([negvalue,metaconcept,matchmap],
				   Candidate,
				   [NegValue,Concept,MatchMap]),
	evaluation_is_subsumed_4([H|T], NegValue, Concept, MatchMap).

evaluation_is_subsumed_4([FirstCandidate|RestCandidates], NegValue1, Concept1, MatchMap1) :-
	get_all_candidate_features([negvalue,matchmap],
				   FirstCandidate,
				   [NegValue2,MatchMap2]),
        ( NegValue2 < NegValue1,
          matchmaps_are_equivalent(MatchMap1, MatchMap2) ->
          true
        ; evaluation_is_subsumed_4(RestCandidates, NegValue1, Concept1, MatchMap1)
        ).

/* matchmaps_are_equivalent(+MatchMap1, +MatchMap2)

matchmaps_are_equivalent/2 determines if the phrase components of MatchMap1
and MatchMap2 are the same.  */

matchmaps_are_equivalent(MatchMap1, MatchMap2) :-
	consolidate_matchmap_phrase_components(MatchMap1, SortedPhraseComponents1),
	consolidate_matchmap_phrase_components(MatchMap2, SortedPhraseComponents2),
	SortedPhraseComponents2 = SortedPhraseComponents1.

consolidate_matchmap_phrase_components(MatchMap, SortedComponents) :-
	extract_components(MatchMap, PhraseComponents, _),
	linearize_components(PhraseComponents, LPhraseComponents),
	append(LPhraseComponents, CompactComponents),
	sort(CompactComponents, SortedComponents).

/* add_semtypes_to_evaluations(?Evaluations)

add_semtypes_to_evaluations/1 instantiates the SemTypes argument of ev/8 terms
in Evaluations.  */

%%% add_semtypes_to_evaluations([]).
%%% add_semtypes_to_evaluations([Candidate|RestCandidates]) :-
%%% 	get_all_candidate_features([cui,semtypes], Candidate, [CUI,SemTypes]),
%%% 	% db_get_concept_sts(MetaConcept, SemTypes),
%%% 	db_get_cui_sts(CUI, SemTypes),
%%% 	!,
%%% 	add_semtypes_to_evaluations(RestCandidates).

get_inputmatch_atoms_from_phrase(PhraseElements, InputMatchAtoms) :-
	get_inputmatch_lists_from_phrase(PhraseElements, InputMatchLists),
	append(InputMatchLists, InputMatchAtoms).

get_inputmatch_lists_from_phrase([], []).
get_inputmatch_lists_from_phrase([FirstPhraseComponent|RestPhraseComponents],
				 [FirstInputMatchList|RestInputMatchLists]) :-
	arg(1, FirstPhraseComponent, FeatureList),
	memberchk(inputmatch(FirstInputMatchList), FeatureList),
	get_inputmatch_lists_from_phrase(RestPhraseComponents, RestInputMatchLists).
		
% get_composite_phrases/5 is intentionally nondeterminate to allow for undoing
% composite phrases if the result is too long a phrase!
get_composite_phrases([PhraseIn|RestPhrasesIn], CompositePhrase,
		      ActualPrepPhraseCount, NewRestPhrasesIn, CompositeOptions) :-
	control_value(composite_phrases, MaxPrepPhraseCount),
	MaxPrepPhraseCount > 0,
	begins_with_composite_phrase([PhraseIn|RestPhrasesIn], MaxPrepPhraseCount,
				     ActualPrepPhraseCount,
				     CompositePhrase0, NewRestPhrasesIn),
	collapse_syntactic_analysis(CompositePhrase0, CompositePhrase),
	% append(CompositePhrase1, CompositePhrase),
	CompositeOptions = [term_processing, ignore_word_order].
get_composite_phrases([PhraseIn|RestPhrases], PhraseIn, 0, RestPhrases, []).

/* begins_with_composite_phrase(+Phrases, -CompositePhrase, -Rest)

begins_with_composite_phrase/3 determines if Phrases begins with a CompositePhrase,
and returns it and the Rest of the phrases. A composite phrase is detected if
the phrase list begins with
(1)  a phrase containing a head(_) structure
(2)  followed by a prepositional phrase
(3)  followed by one or more prepositional phrases introduced by "of", e.g.,

(1) pain
(2) on the left side
(3) of the chest

*/

begins_with_composite_phrase([FirstPhrase,SecondPhrase|RestPhrases], MaxPrepPhraseCount0,
			     TotalPhraseCount, [FirstPhrase,SecondPhrase|RestComposite], NewRest) :-
	% We want to allow not just "pain on the left side of the chest",
	% but also "the patient presented with pain on the left side of the chest"!
	% \+ is_prep_phrase(FirstPhrase),
	% contains_head(FirstPhrase),
	% FirstPhrase = [FirstPhraseElement|_RestPhraseElements],
	% \+ FirstPhraseElement = conj(_),
        \+ ends_with_punc(FirstPhrase), % "pain"
        is_prep_phrase(SecondPhrase),   % "on the left side"
	% !,
	% MaxPrepPhraseCount is the total number of prepositional phrases
	% that can be glommed onto the initial phrase;
	MaxPrepPhraseCount is MaxPrepPhraseCount0 - 1,
	InitPrepPhraseCount is 0,
	initial_of_phrases(RestPhrases, MaxPrepPhraseCount, InitPrepPhraseCount,
			   _ActualOFPhraseCount0, _RestComposite0, _NewRest0),
	!,
	% This looks ugly, but it prevents needless backtracking.
	% ActualPrepPhraseCount0 is the max # of "of" prep phrases found,
	% so instead of backtracking from -4 (or whatever the limit is) to 0,
	% just backtrack from ActualPrepPhraseCount0 to 0.
	NegMaxPrepPhraseCount is -1 * MaxPrepPhraseCount,
%	NegActualPrepPhraseCount is -1 * ActualPrepPhraseCount0,
%	between(NegActualPrepPhraseCount, 0, TempNegActualPrepPhraseCount),
	between(NegMaxPrepPhraseCount, 0, TempNegActualPrepPhraseCount),
	TempActualPrepPhraseCount is -1 * TempNegActualPrepPhraseCount,
%	TempActualPrepPhraseCount is -1 * TempNegActualPrepPhraseCount,
	initial_of_phrases(RestPhrases, TempActualPrepPhraseCount, InitPrepPhraseCount,
			   ActualOFPhraseCount, RestComposite, NewRest),
	TotalPhraseCount is ActualOFPhraseCount + 2.

%%%%% begins_with_composite_phrase([First,Second|Rest],
%%%%% 			     [First,Second|RestComposite],
%%%%%                              NewRest) :-
%%%%% 	is_of_phrase(Second),
%%%%% 	!,
%%%%% 	initial_of_phrases(Rest, RestComposite, NewRest).

is_prep_phrase([PhraseItem|_]) :-
	get_phrase_item_name(PhraseItem, prep),
	!.

% contains_head(Phrase) :-
% 	member(PhraseItem, Phrase),
% 	get_phrase_item_name(PhraseItem, head),
% 	!.

ends_with_punc(PhraseItems) :-
	% reversed order of args from QP library version!
	last(PhraseItems, LastPhraseItem),
	get_phrase_item_name(LastPhraseItem, punc),
	!.

initial_of_phrases([], _MaxPrepPhraseCount,
		   ActualPrepPhraseCount, ActualPrepPhraseCount, [], []).
initial_of_phrases([First|Rest],
		   MaxPrepPhraseCountIn, ActualPrepPhraseCountIn, ActualPrepPhraseCountOut,
		   [First|RestOf], NewRest) :-
	MaxPrepPhraseCountIn > 0,
	is_of_phrase(First),
	!,
	MaxPrepPhraseCountNext is MaxPrepPhraseCountIn - 1,
	ActualPrepPhraseCountNext is ActualPrepPhraseCountIn + 1,
	initial_of_phrases(Rest, MaxPrepPhraseCountNext,
			   ActualPrepPhraseCountNext, ActualPrepPhraseCountOut,
			   RestOf, NewRest).
initial_of_phrases([H|T], _MaxPrepPhraseCount,
		   ActualPrepPhraseCount, ActualPrepPhraseCount, [], [H|T]).

is_of_phrase([PhraseItem|_]) :-
	get_phrase_item_name(PhraseItem, prep),
	get_phrase_item_feature(PhraseItem, lexmatch, [of]).

debug_compute_evaluations_1(DebugFlags, GVCs0) :-
	( memberchk(1, DebugFlags) ->
	  format(user_error, '~n~nGs:~n', []),
	  wgvcs(GVCs0)
	; true
	).

debug_compute_evaluations_2(DebugFlags, GVCs3, Variants) :-
	( memberchk(1, DebugFlags) ->
	  format(user_error, '~n~nGVs:~n', []),
	  wgvcs(GVCs3),
          format(user_error, '~n~nVariants:~n', []),
	  avl_to_list(Variants, VariantsList),
	  write_avl_list(VariantsList)
	; true
	).

debug_compute_evaluations_3(DebugFlags, GVCs) :-
	( memberchk(2, DebugFlags) ->
	  format(user_error, '~n~nGVCs:~n', []),
	  wgvcs(GVCs)
	; true
	).

debug_compute_evaluations_4(DebugFlags, Evaluations2) :-
	( memberchk(4, DebugFlags) ->
	  length(Evaluations2, NEvals2),
	  format(user_error, '~nPre-filtered evaluations (~d):~n', [NEvals2]),
	  wl(Evaluations2)
	; true
	).

debug_compute_evaluations_5(DebugFlags, Evaluations) :-
	( memberchk(4, DebugFlags) ->
	  length(Evaluations, NEvals),
	  format(user_error, '~nNon-redundant evaluations (~d):~n', [NEvals]),
	  wl(Evaluations)
	; true
	).


maybe_filter_out_dvars(GVCs1, GVCs2) :-
	( control_option(no_derivational_variants) ->
	  filter_out_dvars(GVCs1, GVCs2)
	; GVCs2 = GVCs1
	).

maybe_filter_out_aas(GVCs2, GVCs3) :-
	( \+ control_option(all_acros_abbrs),
	  \+ control_option(unique_acros_abbrs_only) ->
	  filter_out_aas(GVCs2, GVCs3)
	; GVCs3 = GVCs2
	).

maybe_filter_out_novars(NoVars, GVCsIn, GVCsOut) :-
	( control_option(novar) ->
	  filter_out_novars(NoVars, GVCsIn, GVCsOut)
	; GVCsOut = GVCsIn
	).


filter_out_novars([], GVCs, GVCs).
filter_out_novars([VariantWord1:VariantWord2|RestNoVarPairs], GVCsIn, GVCsOut) :-
	filter_out_novars_from_one_GVC(GVCsIn, VariantWord1, VariantWord2, GVCsNext),
	filter_out_novars(RestNoVarPairs, GVCsNext, GVCsOut).

filter_out_novars_from_one_GVC([], _VariantWord1, _VariantWord2, []).
filter_out_novars_from_one_GVC([GVCIn|RestGVCsIn], VariantWord1, VariantWord2, [GVCOut|RestGVCsOut]) :-
	GVCIn = gvc(Generator,VariantsIn,_Candidates),
	Generator = v(GeneratorWord,_,_,_,_,_),
	( generator_matches_variant_word(GeneratorWord,
					 VariantWord1, VariantWord2, OtherVariantWord) ->
	  filter_out_novar_variants(VariantsIn, OtherVariantWord, VariantsOut)
	; VariantsOut = VariantsIn
	),
	GVCOut = gvc(Generator,VariantsOut,_Candidates),
	filter_out_novars_from_one_GVC(RestGVCsIn, VariantWord1, VariantWord2, RestGVCsOut).

% If Generator == VariantWord1 or VariantWord2, then return the other variant word.
generator_matches_variant_word(Generator, VariantWord1, VariantWord2, OtherVariantWord) :-
	% Check for exact matches first
	( Generator == VariantWord1 ->
	  OtherVariantWord = VariantWord2
	; Generator == VariantWord2 ->
	  OtherVariantWord = VariantWord1
	;  \+ \+ ( Generator = VariantWord1 ) ->
	  OtherVariantWord = VariantWord2
	; \+ \+ ( Generator = VariantWord2 ) ->
	  OtherVariantWord = VariantWord1
	).

filter_out_novar_variants([], _OtherVariantWord, []).
filter_out_novar_variants([VariantTerm|RestVariantTerms], OtherVariantWord, VariantsOut) :-
	VariantTerm = v(VariantWord,VarLevel,_LexCat,_History,_Roots,_NFR),
	% Always allow a word to be its own variant (VarLevel == 0)!
	( VarLevel is 0 ->
	  VariantsOut = [VariantTerm|RestVariantsOut]
	; \+ \+ ( VariantWord = OtherVariantWord ) ->
	  VariantsOut = RestVariantsOut
	; VariantsOut = [VariantTerm|RestVariantsOut]
	),
	filter_out_novar_variants(RestVariantTerms, OtherVariantWord, RestVariantsOut).


maybe_filter_evaluations_by_threshold(Evaluations1, Evaluations2) :-
	( control_value(threshold, Threshold) ->
	  NegThreshold is -Threshold,
	  filter_evaluations_by_threshold(Evaluations1, NegThreshold, Evaluations2)
	; Evaluations2 = Evaluations1
	).

get_debug_control_value(DebugFlags) :-
	( control_value(debug, DebugFlags) ->
	  true
	; DebugFlags = []
	).

check_generate_best_mappings_control_options :-
	( \+ control_option(hide_mappings)   -> true
	; control_option(fielded_mmi_output) -> true
	; control_option(machine_output)     -> true
	; xml_output_format(_XMLFormat)      -> true
	; json_output_format(_JSONFormat)
	).

check_construct_best_mappings_control_options :-
	( \+ control_option(hide_mappings)    -> true
	; control_option(fielded_mmi_output)  -> true
	; control_option(machine_output)      -> true
	; xml_output_format(_XMLFormat)       -> true
	; json_output_format(_JSONFormat)
	).

check_generate_initial_evaluations_control_options_1 :-
	\+ control_option(allow_overmatches),
	\+ control_option(allow_concept_gaps),
	\+ control_option(ignore_stop_phrases),
	% -D and -a must be in force because
	% that's how stop phrases were computed
	\+ control_option(all_derivational_variants),
	\+ control_option(all_acros_abbrs).

% This predicate succeeds UNLESS show_candidates is off and hide_mappings is on.
% The checks for fielded_mmi_output, machine_output, and XML are redundant,
% because it is an error to have any of those three on if hide_mappings is on.
% The only way this predicate can fail is if hide_mappings is on and
% show_candidates is not set,
% in which case there is no reason to generate candidates!

check_generate_initial_evaluations_control_options_2 :-
	( control_option(show_candidates)    -> true
	; \+ control_option(hide_mappings)   -> true
	; control_option(fielded_mmi_output) -> true
	; control_option(machine_output)     -> true
	; xml_output_format(_XMLFormat)      -> true
	; json_output_format(_JSONFormat)
	).

% Create AEv term or access its features
augmented_candidate_term(PhraseComponents, Low, High,
			 NegValue, CUI, MetaTerm, MetaConcept, MetaWords, SemTypes,
			 MatchMap, LSComponents, TargetLSComponent, InvolvesHead,
			 IsOvermatch, SourceInfo, PosInfo, Status, Negated,
			 AugmentedCandidateTerm) :-
	AugmentedCandidateTerm = aev(PhraseComponents, Low, High,
				     NegValue, CUI, MetaTerm, MetaConcept, MetaWords, SemTypes,
				     MatchMap, LSComponents, TargetLSComponent, InvolvesHead,
				     IsOvermatch, SourceInfo, PosInfo, Status, Negated).

get_all_aev_features(FeatureList, AEvTerm, FeatureValueList) :-
	( foreach(ThisFeature, FeatureList),
	   foreach(ThisFeatureValue, FeatureValueList),
	   param(AEvTerm)
	do get_aev_feature(ThisFeature, AEvTerm, ThisFeatureValue)
	).

get_aev_feature(phrasecomponents, AEvTerm, PhraseComponents) :-
	AEvTerm = aev(PhraseComponents,_Low,_High,
		      _NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		      _MatchMap, _LSComponents, _TargetLSComponent, _InvolvesHead,
		      _IsOvermatch,_SourceInfo,_PosInfo,_Status,_Negated).
get_aev_feature(low, AEvTerm, Low) :-
	AEvTerm = aev(_PhraseComponents,Low,_High,
		      _NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		      _MatchMap, _LSComponents, _TargetLSComponent, _InvolvesHead,
      		      _IsOvermatch,_SourceInfo,_PosInfo,_Status,_Negated).
get_aev_feature(high, AEvTerm, High) :-
	AEvTerm = aev(_PhraseComponents,_Low,High,
		      _NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		      _MatchMap, _LSComponents, _TargetLSComponent, _InvolvesHead,
      		      _IsOvermatch,_SourceInfo,_PosInfo,_Status,_Negated).
get_aev_feature(negvalue, AEvTerm, NegValue) :-
	AEvTerm = aev(_PhraseComponents,_Low,_High,
		      NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		      _MatchMap, _LSComponents, _TargetLSComponent, _InvolvesHead,
      		      _IsOvermatch,_SourceInfo,_PosInfo,_Status,_Negated).
get_aev_feature(cui, AEvTerm, CUI) :-
	AEvTerm = aev(_PhraseComponents,_Low,_High,
		      _NegValue,CUI,_MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		      _MatchMap, _LSComponents, _TargetLSComponent, _InvolvesHead,
      		      _IsOvermatch,_SourceInfo,_PosInfo,_Status,_Negated).
get_aev_feature(metaterm, AEvTerm, MetaTerm) :-
	AEvTerm = aev(_PhraseComponents,_Low,_High,
		      _NegValue,_CUI,MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		      _MatchMap, _LSComponents, _TargetLSComponent, _InvolvesHead,
      		      _IsOvermatch,_SourceInfo,_PosInfo,_Status,_Negated).
get_aev_feature(metaconcept, AEvTerm, MetaConcept) :-
	AEvTerm = aev(_PhraseComponents,_Low,_High,
		      _NegValue,_CUI,_MetaTerm,MetaConcept,_MetaWords,_SemTypes,
		      _MatchMap, _LSComponents, _TargetLSComponent, _InvolvesHead,
      		      _IsOvermatch,_SourceInfo,_PosInfo,_Status,_Negated).
get_aev_feature(metawords, AEvTerm, MetaWords) :-
	AEvTerm = aev(_PhraseComponents,_Low,_High,
		      _NegValue,_CUI,_MetaTerm,_MetaConcept,MetaWords,_SemTypes,
		      _MatchMap, _LSComponents, _TargetLSComponent, _InvolvesHead,
      		      _IsOvermatch,_SourceInfo,_PosInfo,_Status,_Negated).
get_aev_feature(matchmap, AEvTerm, MatchMap) :-
	AEvTerm = aev(_PhraseComponents,_Low,_High,
		      _NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		      MatchMap, _LSComponents, _TargetLSComponent, _InvolvesHead,
      		      _IsOvermatch,_SourceInfo,_PosInfo,_Status,_Negated).

:- use_module(skr_lib(addportray)).
portray_candidate(Candidate) :-
	nonvar(Candidate),
	!,
 	get_candidate_feature(cui, Candidate, CUI),
 	writeq(ev(CUI)).

%%% 	( Candidate = []-[AEvTerm] ->
%%% 	  get_aev_feature(cui, AEvTerm, CUI),
%%% 	  writeq(final(CUI))
%%% 	; Candidate = AEvTerm,
%%% 	  get_aev_feature(cui, AEvTerm, CUI) ->
%%% 	  writeq(aev(CUI))
%%% 	; Candidate = EvTerm,
%%% 	  get_candidate_feature(cui, EvTerm, CUI) ->
%%% 	  writeq(ev(CUI))
%%% 	).
portray_candidate(Candidate) :- write(Candidate).
:- add_portray(portray_candidate).

%%% portray_aev(AEV) :-
%%% 	nonvar(AEV),
%%% 	!,
%%% 	( AEV = []-[AEvTerm] ->
%%% 	  get_aev_feature(cui, AEvTerm, CUI),
%%% 	  writeq(final(CUI))
%%% 	; get_aev_feature(cui, AEvTerm, CUI),
%%% 	  writeq(final(CUI))
%%% 	; write(AEV)
%%% 	).
%%% portray_aev(AEV) :- write(AEV).
%%% :- add_portray(portray_aev).
%%% 
%%% portray_ev(EvTerm) :-
%%% 	nonvar(EvTerm),
%%% 	!,
%%% 	get_candidate_feature(cui, EvTerm, CUI),
%%% 	writeq(ev(CUI)).
%%% portray_ev(EvTerm) :- write(EvTerm).
%%% 
%%% :- add_portray(portray_ev).
