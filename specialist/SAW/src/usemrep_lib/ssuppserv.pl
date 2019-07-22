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

% % File:	    ssuppserv.pl
% Module    ssuppserv    (saw_support_services)
% Author    tcr
% Purpose   Predicates of general utility throughout SAW


% ----- Module declaration and exported predicates

:- module( ssuppserv, [
	add_indexes_to_all_MSUs/3,
	% add_index_to_each_word/3,  %%% obsolete
	add_roles_to_conjuncts/7,
	add_msu_index/3,
	announce_check_relation/7,
% 	announce_conjlist/1,
	announce_initialize_db_access_for_genspec/1,
	announce_initialize_db_access_for_entrezgene/0,
	announce_initialize_db_access_for_profile_call/0,
	announce_initialize_metamap/1,
%	announce_predication/5,
	announce_stop_db_access_for_genspec/0,
	announce_stop_db_access_for_entrezgene/0,
	announce_stop_db_access_for_profile_call/0,
	announce_stop_metamap/0,
%	append_diff/6,
	assemble_format_control_string/2,
	atomize/2,
	check_any_relation/6,
	check_for_complementizer/1,
	check_for_passive/6,
	check_for_relativizer/1,
	check_relation/3,
	choose_utterance_list/4,
	clean_index/2,
% 	close_open_stream/1,
        collect_bases/2,
%	concat_with_blank_spaces/2,
	convert_dash_structure_to_list/2,
% 	convert_genphenom_function_list/2,
%	create_citation_text_atom/2,
	determine_utterance_types/2,
	determine_left_domain/4,
	determine_right_domain/4,
	find_the_next/6,
	find_next_non_adverb/2,	       
	fix_metamap_analysis/2,
%	flag_interesting_semtypes/4,
	get_abstract_id/2,
	get_abstract_id_1/3,
	get_all_bases/3,
	get_all_concept_cuis/2,
% 	get_all_semtypes/2,
	get_all_tokens/2,
	get_arguments/25,
%	get_arguments/19,
%	get_augmented_concept_cui/2,
	get_conjunct_data/9,
	get_base/4,
	get_entity_atom/5,
	get_from_list_if_possible/3,
%	get_head/2,
	get_index_start_and_end_pos/4,
	get_intervening_structure/3,
	get_last_indexes/3,
	get_left_partition/3,
	get_left_partition_1/3,
	get_lexical_category/3,
	get_lexrec_for_cat/3,
	get_list_indexes/6,
	get_matching_genphenom/5,
	get_mmo_text/4,
        is_conditional_msu_empty_head/2,
	is_empty_head/1,
	is_msu_empty_head/1,
	is_next_msu_head/3,
	is_adjectival_indicator/1,
	is_nominal_indicator/1,
%	get_print_semtype_list/2,
	get_sem_info/4,
	get_sem_info_and_type/5,
	get_term_with_metaconc/3,
	get_text/2,
	head_lists_are_not_conjuncts/3,
 	maybe_insert_domain_concepts/4,
%	insert_clinical_trials_semtypes/3,
%	insert_msu_token_lists/3,
	is_verb_indicator/1,
	get_transitive_cue/4,
	left_parenthesis/1,
	license_for_reuse/7,
	lower_list/2,
	main_verb/2,
%	merge_sentences/2,
	min_max/4,
%	no_conj_between_head_lists/3,
	no_conj_between_arguments/3,
	no_crossing_lines/5,
        non_lexical_status/1,
	no_np/1,
	no_verbal/1,
	npu_has_conjunct/6,
	locate_npu_head/3,
	locate_npu_and_head/5,
	locate_npu_from_head/3,
	look_for_pmid/1,
	override_control_options/0,
	override_file_args/2,
	potential_indicator/3,
	preceding_mod/3,
	processing_mode/1,
	remove_referential_analysis_if_necessary/2,
	remove_referential_features_from_element/2,
	remove_referential_features/2,
	remove_variables/3,
%	retokenize_for_period/2,
	right_parenthesis/1,
%	show_mods_and_heads_with_no_metaconcs/3,
% 	set_genphenom_or_disorder_fields/8,
	split_input_label/4,
	strings_to_atoms/2,
%	statistics/1,
	subordinating_conjunction/2,
	surface_role/3,
%	trim_all_whitespace/2,
	update_entity_fields/15
%	var_memberchk/2
%	var_list_to_set/2,
%	write_list_with_format/2,
%	write_list_with_format_to_stream/3
   ]).


:- load_files( usemrep_lib(module_version), [
		when(compile_time)
   ]).

% ----- Imported predicates

:- use_module( lexicon( qp_lookup ), [
	assemble_definitions/2
   ]).

:- use_module( metamap(metamap_tokenization), [
	local_upper/1
   ]).

:- use_module( skr(skr), [
        initialize_skr/1,
	% extract_phrases_from_aps/2,
	get_phrase_tokens/4,
%	skr_phrases/14,
	stop_skr/0
   ]).

:- use_module( skr(skr_text_processing), [
%	convert_all_utf8_to_ascii/4,
	get_skr_text/2
   ]).

:- use_module( skr_lib( ctypes), [
        is_lower/1,
	is_upper/1
   ]).

:- use_module( skr_lib( negex), [
        do_negex/0
   ]).

:- use_module( skr_lib(nls_lists), [
	get_from_list/3,
	get_from_list_nd/3
   ]).

:- use_module( skr_lib(nls_system), [
	control_value/2
   ]).

:- use_module( skr_lib( sicstus_utils), [
        concat_atom/2,
	concat_atom/3,
	lower/2,
	midstring/5,
	midstring/6,
	subchars/4,
	substring/4,
	ttyflush/0
   ]).

:- use_module( text(text_object_util), [
	an_tok/1
   ]).

:- use_module( usemrep_domain(exceptions_DOM), [
	conditional_empty_head_base_1_DOM/2,
	conditional_empty_head_base_2N_DOM/3,
	empty_head_base_1_DOM/1,
	empty_head_base_2N_DOM/2
   ]).

:- use_module( usemrep_domain(locsemnet_DOM), [
%  	local_relation_inverse_DOM/2,
  	local_semnet_DOM/3
%  	local_semnet_1_DOM/3
    ]).

:- use_module( usemrep_lib(abstract_section_header), [
	valid_abstract_section_header/2
   ]).

:- use_module( usemrep_lib(debug), [
	debug_call/3
   ]).

:- use_module( usemrep_lib(entrezgene), [
	initialize_db_access_for_entrezgene/0,
	stop_db_access_for_entrezgene/0
   ]).

:- use_module( usemrep_lib(exceptions_GEN), [
	conditional_empty_head_base_1_GEN/2,
	conditional_empty_head_base_2N_GEN/3,
	empty_head_base_1_GEN/1,
	empty_head_base_2N_GEN/2,
	empty_macro_np_head_list/1,
	non_compositional_phrase_list/1
   ]).

:- use_module( usemrep_lib(get_semtypes), [
	abbreviate_umls_semtypes/2,
	look_up_semantic_groups/3
   ]).

:- use_module( usemrep_lib(locsemnet_GEN), [
%	local_relation_inverse_GEN/2,
	local_semnet_GEN/3
%	local_semnet_1_GEN/3
   ]).

:- use_module( usemrep_lib(nominal_args), [
        get_args_of_nominalization/9
   ]).

:- use_module( usemrep_lib(mancmod), [
	initialize_db_access_for_genspec/1,
	stop_db_access_for_genspec/0
   ]).

:- use_module( usemrep_main(usemrep), [
	override_control_option/2,
	override_file/3
   ]).

%:- use_module( usemrep_lib(jdimod), [
%	initialize_db_access_for_profile_call/0,
%	stop_db_access_for_profile_call/0
%   ]).


%:- use_module( usemrep_lib(edgarsupport), [
%	adjust_bases_list/2
%   ]).

%:- use_module( usemrep_lib(edgarnonlex), [
%	non_lexical_status/1
%   ]).

%:- use_module( usemrep_domain(choose_domains), [
%	clinical_domain/1
%   ]).

:- use_module(usemrep_domain(domain_data), [
        domain_concept/2,
	domain_replace/2
%	domain_replace/6
   ]).

:- use_module(usemrep_lib(generic_domain), [
        domain_concept/2,
	domain_replace/2
   ]).

:- use_module( skr_lib(nls_strings), [
	form_one_string/3,
	split_string_completely/3,
	trim_whitespace/2
   ]).

:- use_module( skr_lib(nls_system), [
	control_option/1
   ]).

:- use_module( skr_db(db_access), [
        db_get_concept_cui/2
   ]).

:- use_module(usemrep_lib(module_version), [
	global_module_version/1
   ]).

%:- set_global_module_version('06').
%:- use_versioned_module( usemrep_lib( semnet_access),
%		ssuppserv, [
%        type_relation_type/3,
% 	 relation_inverse/2
%   ]).

:- use_module( usemrep_lib( semnet_access), [
	 type_relation_type/4,
	 relation_inverse/3
   ]).

:- use_module( library(lists), [
	append/2,
%	bagof/3,
	is_list/1,
	keys_and_values/3,
%	keysort/2,
	last/2,
	nextto/3,
%	nth1/3,
	rev/2,
	select/3,
	subseq0/2
   ]).

:- use_module( library(sets), [
	intersect/2,
	intersection/3,
	subtract/3
   ]).

%:- use_module( usemrep_lib( semnet_access_2012AA),  [
%	  type_relation_type/3,
%	  relation_inverse/2
%   ]).

%:- use_module( usemrep_lib( semnet_access06), [
%	  type_relation_type/3,
%	  relation_inverse/2
%   ]).

%:- use_module( skr_lib(nls_io), [
%         fget_non_ws_only_line/2,
%	  fget_lines_until_skr_break/2
%   ]).

% ---------- Add MSU Index ----------
add_msu_index(minimal_syntax(PhraseList),
	      IndexIn,
	      minimal_syntax(IndexedPhraseList)) :-
	add_msu_index(PhraseList, IndexIn, IndexedPhraseList).

add_msu_index([], _Index, []) :- !.

add_msu_index([ThisMSU|MoreMSUs], IndexIn, [IndexedMSU|IndexedList]) :-
	Index is IndexIn + 1,
	IndexedMSU = [msu_index(Index)|ThisMSU],
	add_msu_index(MoreMSUs, Index, IndexedList).

% ---------- Add Index to All MSUs ----------

add_indexes_to_all_MSUs(minimal_syntax(PhraseList),
		       IndexIn,
		       minimal_syntax(IndexedPhraseList)):-
	!,
	add_indexes_to_all_MSUs(PhraseList,IndexIn,IndexedPhraseList).

add_indexes_to_all_MSUs([], _Index, []).
add_indexes_to_all_MSUs([ThisMSU|RestMSUs], IndexIn, [IndexedMSU|IndexedList]) :-
	ThisMSU = [FirstMSUItem|RestMSUItems],
	add_indexes_to_one_MSU(RestMSUItems, FirstMSUItem, IndexIn, IndexOut, IndexedMSU),
	add_indexes_to_all_MSUs(RestMSUs, IndexOut, IndexedList).

% ---------- ADD_INDEXES_TO_ONE_MSU

add_indexes_to_one_MSU([], LastMSUItem, IndexIn, IndexOut, [IndexedLastMSUItem]) :-
	index_one_MSU_item(LastMSUItem, IndexIn, IndexOut, IndexedLastMSUItem).
add_indexes_to_one_MSU([NextMSUItem|RestMSUItems], ThisMSUItem,
		       IndexIn, IndexOut,
		       [IndexedThisMSUItem|IndexedRestMSUItems]) :-
	index_one_MSU_item(ThisMSUItem, IndexIn, IndexNext, IndexedThisMSUItem),
%	format(user_output,'Indexed MSU: ~q~n',[IndexedThisMSUItem]),
	add_indexes_to_one_MSU(RestMSUItems, NextMSUItem,
			       IndexNext, IndexOut,
			       IndexedRestMSUItems).

index_one_MSU_item(confid(Confid), Index,   Index,    confid(Confid)) :- !.
index_one_MSU_item(punc(Punc),     Index,   Index,    punc(Punc)) :- !.
index_one_MSU_item(OtherItem,      IndexIn, IndexOut, IndexedOtherItem) :-
	% e.g., OtherItem = head([_|_]), LexicalCategory = head
	functor(OtherItem, LexicalCategory, _Arity),
	arg(1, OtherItem, ItemArg),
	get_from_list(inputmatch, ItemArg, InputList),
	length(InputList, InputLen),
	( IndexIn = _OldIndex:_LowIndex/HighIndex ->
	  IndexOut is HighIndex + InputLen
	; IndexOut is IndexIn + InputLen
	),
	OtherItemArg = [index(IndexOut)|ItemArg],
	functor(IndexedOtherItem, LexicalCategory, 1),
	arg(1, IndexedOtherItem, OtherItemArg).

%%% MOVE THIS FROM HERE AND PROBABLY SIMPLIFY

% An MSU is a list that looks like
% [msu_index(9),
%  prep([index(21),lexmatch([of]),inputmatch([of]),tag(prep),
%        tokens([of]),position(314,316),bases([of])]),
%  head([index(22),inputmatch([shRNA]),tag(noun),tokens([shrna]),
%        position(317,322),bases([shrna])])]

% A sentence is a list of such MSUs:
% [ [msu_index(_), ... ], [msu_index(_), ... ], [msu_index(_), ... ] ]

% PrecedingAnalysis is a list of such sentences:
% [ [ [msu_index(FirstMSUINdex), ... ],
%     [msu_index(_), ... ],
%     [msu_index(_), ... ] ],
%   [ [msu_index(_), ... ],
%     [msu_index(_), ... ],
%     [msu_index(_), ... ] ],
%   ...
%   [ [msu_index(_), ... ],
%     [msu_index(_), ... ],
%     [msu_index(LastMSUIndex), ... ] ] ]

% Most of the MSU lists [msu_index(_), ... ] will contain an element like
% prep([index(21), ..., bases([of])])
% or
% head([index(22), ..., bases([shrna])])
% whose first (and only) argument is a list containing an index/1 term.

% We need to get the argument of
% (1) the last msu_index/1 term and
% (2) the last index/1 term
% in PrecedingAnalysis, but these two will not necessarily
% live in the same [msu_index(_), ... ] list because of lists like

% [[msu_index(52),punc([inputmatch(['.']),tokens([]),position(1018,1019),bases(['.'])])]]]

get_last_indexes([], 0, 0).
get_last_indexes([H|T], LastIndex, LastMSUIndex) :-
	PrecedingAnalysis = [H|T],
	% PrecedingAnalysis is a list of sentences, each of which is a list of MSUs.
	% Each MSU is a list like
	% [msu_index(47),conj([index(104),lexmatch([and]),inputmatch([and]),
	%  tag(conj),tokens([and]),position(946,949),bases([and])])]
	% First reverse the top-level list to get the sentences last-to-first.
	rev(PrecedingAnalysis, RevPrecedingAnalysis),
	get_last_MSU_index(RevPrecedingAnalysis, LastMSUIndex),
	get_last_index(RevPrecedingAnalysis, LastIndex).

get_last_MSU_index(RevPrecedingAnalysis, LastMSUIndex) :-
	% A sentence looks like
	% [ [msu_index(_), ... ], [msu_index(_), ... ], [msu_index(LastMSUIndex), ... ] ] 
	% Iterate through the sentences last-to-first,
	% looking for the last sentence headed by an msu_index/1 term.
	% Presumably all sentences are in fact headed by an msu_index/1 term,
	% but just in case, we don't just take the first one.
	member(Sentence, RevPrecedingAnalysis),
	% Reverse it to get the list of MSUs in the last sentence last-to-first
	% [ [msu_index(LastMSUIndex), ... ], [msu_index(_), ... ], [msu_index(_), ... ] ] 
	rev(Sentence, RevSentence), 
	% In that list, look front-to-back for an msu_index/1 term.
	% An MSU is a list that looks like
	% [msu_index(9),
	%  prep([index(21),lexmatch([of]),inputmatch([of]),tag(prep),
	%        tokens([of]),position(314,316),bases([of])]),
	%  head([index(22),inputmatch([shRNA]),tag(noun),tokens([shrna]),
	%        position(317,322),bases([shrna])])]
	member(MSU, RevSentence),
	get_from_list(msu_index, MSU, LastMSUIndex),
	!.

get_last_index(RevPrecedingAnalysis, LastIndex) :-
	% Then do the same thing, but looking for an index/1 term
	% inside a prep([...]) or a det([...]) or a head([...]) term
	% inside an MSU inside a sentence.
	% First, iterate through the sentences last to first.
	member(Sentence, RevPrecedingAnalysis),
	% Reverse the MSUs in the sentence.
	rev(Sentence, RevSentence),
	% Next, iterate through the MSUs last to first.
	member(MSU, RevSentence),
	% An MSU is a list that looks like
	% [msu_index(9),
	%  prep([index(21),lexmatch([of]),inputmatch([of]),tag(prep),
	%        tokens([of]),position(314,316),bases([of])]),
	%  head([index(22),inputmatch([shRNA]),tag(noun),tokens([shrna]),
	%        position(317,322),bases([shrna])])]
	% We want to examine each prep([...]), head([...]), etc. term in the MSU last-to-first,
	% so reverse the list of terms in the MSU.
	rev(MSU, RevMSU),
	% examine each one
	member(MSUElement, RevMSU),
	% grab its single list arguemnt,
	arg(1, MSUElement, MSUElementList),
	% and look for an index/1 term in the list.
	get_from_list(index, MSUElementList, LastIndex),
	!.
%if there is no index
get_last_index(_RevPrecedingAnalysis,0).

% Previous version
% get_last_indexes([],0,0) :- !.
% get_last_indexes(PrecedingAnalysis,Index,MSUIndex) :-
%         last(PrecedingAnalysis,PrecedingSentenceAnalysis),
%         rev(PrecedingSentenceAnalysis,RevSentenceAnalysis),
%         get_last_indexed(RevSentenceAnalysis,Index,MSUIndex).
% 
% get_last_indexed([MSU|_RestAnalysis],Index,MSUIndex) :-
%         get_from_list(msu_index,MSU,MSUIndex),
%         rev(MSU,RevMSU),
%         get_last_indexed_aux(RevMSU,Index,MSUIndex).
% get_last_indexed([_MSU|RestAnalysis],Index,MSUIndex) :-
%         get_last_indexed(RestAnalysis,Index,MSUIndex).
% 
% %get_last_indexed_aux([msu_index(MSUIndex)|Rest],Index,MSUIndex) :-
% %       !,
% %       get_last_indexed_aux(Rest,Index,MSUIndex).
% get_last_indexed_aux([MSUElement|_Rest],Index,_MSUIndex) :-
%         arg(1,MSUElement,MSUElementList),
%         get_from_list(index,MSUElementList,Index),
%         !.
% get_last_indexed_aux([_MSUElement|Rest],Index,MSUIndex) :-
%         get_last_indexed_aux(Rest,Index,MSUIndex).

% ----- APPEND_DIFF/6
%append with difference lists

% append_diff(Xs,Ys, Ys,Zs, Xs,Zs).

/*
% ---------- CHECK FOR RELATIVIZER

check_for_relativizer(+ListOfMSUs)

*/

check_for_relativizer([ThisMSU|_More]) :-
	get_from_list(pron, ThisMSU, PronList),
	get_from_list(lexmatch, PronList, LexList),
	intersect(LexList, [which,who,whom,that]),
	!.
check_for_relativizer([_|More]):-
	check_for_relativizer(More).

/*
% ---------- CHECK FOR COMPLEMENTIZER

check_for_complementizer(+ListOfMSUs)

*/

check_for_complementizer([ThisMSU|_More]) :-
        get_from_list(compl, ThisMSU, _ComplList),
        !.
check_for_complementizer([_|More]):-
        check_for_complementizer(More).

subordinating_conjunction(MSU, _OtherMSUs) :-
	get_from_list(conj,MSU,ConjList),
	get_from_list(lexmatch,ConjList,LexList),
	coordinating_conjunction_list(CoordConjList),
	\+ intersect(LexList,CoordConjList),
	!.
% this is supposed to handle multi word sub. conjunctions
%subordinating_conjunction(_MSU,_OtherMSUs) :-
%	fail.

coordinating_conjunction_list([and,but,either,or,neither,nor,for,yet,so]).
% very partial
% multi_subordinating_conjunction_list(['even though','even if','in order to','as if','as long as']).

/*
% ************************  Get All Bases ***********************
*/

get_all_bases(minimal_syntax(PhraseList),
	      Definitions,
              minimal_syntax(PhraseListWithBases)) :-
	!,
	get_all_bases_1(PhraseList, Definitions, PhraseListWithBases).
get_all_bases(PhraseList, Definitions, PhraseListWithBases) :-
	get_all_bases_1(PhraseList, Definitions, PhraseListWithBases).

get_all_bases_1([], _, []).
get_all_bases_1([ThisPhrase|MorePhrases], Definitions, [NewPhrase|Gap]) :-
	get_bases_for_all_items(ThisPhrase, Definitions, NewPhrase),
	get_all_bases_1(MorePhrases, Definitions, Gap).

% ---

get_bases_for_all_items([], _Definitions, []).
get_bases_for_all_items([ThisItem|MoreItems], Definitions, [NewItem|RestNewItems]) :-
	get_base_for_one_item(ThisItem, Definitions, NewItem),
	get_bases_for_all_items(MoreItems, Definitions, RestNewItems).
% --

get_base_for_one_item(ThisItem, Definitions, NewItem) :-
	% for SKR, which has an extra layer of brackets here
	( ThisItem = [_|_] ->
	  get_bases_for_all_items(ThisItem, Definitions, NewItem)
	; functor(ThisItem, Label, _),
	  arg(1, ThisItem, [H|T]),
	  !,
	  ItemList = [H|T],
	  set_item_base(ItemList, Definitions, ItemListWithBase),
	  functor(NewItem, Label, 1),
	  arg(1, NewItem, ItemListWithBase)
        ).
get_base_for_one_item(ThisItem, _Definitions, ThisItem).

set_item_base(ItemList, Definitions, ItemListWithBase) :-
	get_item_base(ItemList, Definitions, ItemBase),
	add_or_replace_base(ItemList, ItemBase, 0, _BaseFound, ItemListWithBase).

add_or_replace_base([], ItemBase, BaseFound, BaseFound, ItemListTail) :-
	add_base_if_not_found(BaseFound, ItemBase, ItemListTail).
add_or_replace_base([H|T], ItemBase, BaseFoundIn, BaseFoundOut, [NewH|NewT]) :-
	add_or_replace_base_1(H, ItemBase, BaseFoundIn, BaseFoundOut, NewH),
	add_or_replace_base(T, ItemBase, BaseFoundOut, _, NewT).

add_or_replace_base_1(bases(_), ItemBase, _BaseFoundIn, 1, bases(ItemBase)) :-
	!.
add_or_replace_base_1(X, _ItemBase, BaseFoundIn, BaseFoundIn, X).

add_base_if_not_found(0, ItemBase,  [bases(ItemBase)]).
add_base_if_not_found(1, _ItemBase, []).

get_item_base(ItemList, Definitions, ItemBase) :-
	get_from_list(tag, ItemList, LexicalCategory),
	!,
	get_from_list(inputmatch, ItemList, [FirstInputMatch|RestInputMatches]),
	replace_last_element_with_base(RestInputMatches, FirstInputMatch,
				       LexicalCategory, Definitions, ItemBase).
% normal word, not in lexicon
get_item_base(ItemList, _Definitions, BaseList) :-
	get_from_list(inputmatch, ItemList, InputList),
	lower_list(InputList, LowInputList),
	guess_base(LowInputList, BaseList),
	!.
% punc,etc.
get_item_base(ItemList, _Definitions, BaseList) :-
	get_from_list(inputmatch, ItemList, InputList),
	!,
	lower_list(InputList, BaseList).
get_item_base(ItemList, _Definitions, ItemList).

replace_last_element_with_base([], LastInput, LexicalCategory, Definitions, [LastInputBase]) :-
	lower(LastInput, LowerLastInput),
	get_base(LowerLastInput, LexicalCategory, Definitions, LastInputBase).
replace_last_element_with_base([Next|Rest], Input, LexicalCategory, Definitions, [LowerInput|RestBase]) :-
	lower(Input, LowerInput),
	replace_last_element_with_base(Rest, Next, LexicalCategory, Definitions, RestBase).
	
% -----
guess_base([], []).
guess_base([Atom|More], [BaseAtom|Gap]) :-
	atom_codes(Atom, AtomChars),
	rev(AtomChars, RevAtomChars),
	RevAtomChars = [0's|_],
	remove_plural_s(AtomChars, BaseChars),
	atom_codes(BaseAtom, BaseChars),
	guess_base(More, Gap).

% ---
% must not have hyphen or digits
remove_plural_s(AtomChars, AtomChars) :-
	( memberchk(0'-, AtomChars) ->
	  true
	; intersect(AtomChars, "1234567890")
	),
	!.
remove_plural_s(AtomChars, BaseChars) :-
	length(AtomChars, Len),
	Len >= 5,
	intersect(AtomChars, "aeiouy"),
	rev(AtomChars, RevChars),
	RevChars = [_S|RevBaseChars],
	RevBaseChars = [Penultimate|_],
	\+ memberchk(Penultimate, [0's,0'u]),
	memberchk(Penultimate, "bcdfghjklmnprtvwz"),
	rev(RevBaseChars, BaseChars).

% ----- GET_ALL_TOKENS
% get all tokens from an MSU; 

get_all_tokens(MSU, Tokens) :-
	get_all_tokens_1(MSU, Tokens, _Accum),
	!. %addded the cut, Halil
get_all_tokens_1([], Tokens, Tokens).
get_all_tokens_1([ThisItem|More], Tokens, Accum) :-
	functor(ThisItem, Label, _),
	memberchk(Label, [confid,num,gen,qgr]),
	!,
	get_all_tokens_1(More, Tokens, Accum).
get_all_tokens_1([ThisItem|More], Tokens, Accum) :-
	arg(1, ThisItem, ItemList),
	( get_from_list(orig_tokens, ItemList, TheseTokens) ->
	  true
	; get_from_list(tokens, ItemList, TheseTokens)
       ),
	( var(Accum) ->
	  TempAccum = TheseTokens
	;  append(Accum, TheseTokens, TempAccum)
	),
	get_all_tokens_1(More, Tokens, TempAccum).
get_all_tokens_1([_ThisItem|More], Tokens, Accum) :-
	get_all_tokens_1(More, Tokens, Accum).

% ---------- GET_BASE ----------
% get_base(+Word, +LexicalCategory, +Definitions, -Base)

% {base=lour
% spelling_variant=lower
% entry=E0038088
% 	cat=verb
% 	variants=reg
% 	intran
% 	tran=pphr(at,np)
% 	tran=pphr(on,np)
% 	tran=pphr(upon,np)
% }

get_base(Word, _LexicalCategory, [], Word) :- !.
get_base(Word, LexicalCategory, [lexicon:LexList|_MoreLexicon], Base) :-
	get_from_list(lexmatch, LexList, [LexMatch]),
	lower(LexMatch,Word),    % AAs are in lexicon in upper case
	get_from_list(records, LexList, RecList),
	get_lexrec_for_cat(RecList, LexicalCategory, LexRecList),
        % get_from_list(lexrec, RecList, LexRecList),
%	format(user_output,'Base: ~q~n', [LexRecList]),
	get_from_list(base, LexRecList, [Base]),
	% This is a hack designed to work around the lexical item in the comment above
        Base \== lour,
	!.
get_base(Word, LexicalCategory, [_NoMatch|MoreLexicon], Base) :-
	get_base(Word, LexicalCategory, MoreLexicon, Base).

% -----
% get_lexrec_for_cat([], _, _) :- !, fail.
get_lexrec_for_cat([lexrec:LexRecList|_MoreLexRec], LexicalCategory, LexRecList) :-
	get_from_list(entries, LexRecList, EntriesList),
	check_for_cat(LexicalCategory, EntriesList).
get_lexrec_for_cat([_NoMatch|MoreLexRec], LexicalCategory, LexRecList) :-
	get_lexrec_for_cat(MoreLexRec, LexicalCategory, LexRecList).

% -----
% check_for_cat(_, []) :- !, fail.
check_for_cat(LexicalCategory, [entry:Entry|_More]) :-
	memberchk(cat:[LexicalCategory], Entry),
	!.
check_for_cat(LexicalCategory, [_ThisEntry|More]) :-
	check_for_cat(LexicalCategory, More).

% ----- FIND_THE_NEXT 
% Find the next ListOut in ListIn which has structure with functor Label as its first component
% and which has Token as its lexmatch argument.
% Also return the list which follows ListOut in ListIn.

% find_the_next(+Label, +Token, +InterventionConstraint, +ListIn, -ListOut, -FollowsListOut)

find_the_next(_Label, _Token, _InterventionConstraint, [], _TokenList, _ListOut, _FollowsListOut) :-
	!,
	fail.
find_the_next(_Label, _Token, InterventionConstraint, [ThisNPU|_More], _TokenList, _ListOut, _More) :-
	memberchk(InterventionConstraint, ThisNPU),
	!,
	fail.
find_the_next(Label, Token, _InterventionConstraint, [ThisNPU|More], LabelList, ThisNPU, More) :-
	get_from_list(Label, ThisNPU, LabelList),
        %%% ThisNPU = [Introd|_],
        %%% functor(LabelList, Label, _Arity), 
        %%% arg(1, LabelList, LabelListArg),
	get_from_list(lexmatch, LabelList, [Token]),
	!.
find_the_next(Label, Token, InterventionConstraint, [_Other|More], TokenList, ListOut, StillMore) :-
	!,
	find_the_next(Label, Token, InterventionConstraint, More, TokenList, ListOut, StillMore).

% ---------- GET_HEAD ----------

% get_head(NPUnit, Head) :-
%     get_from_list(head, NPUnit, HeadList),
%     get_from_list(tokens, HeadList, TokensList),
%     last(TokensList,Head).


% ---------- NPU_HAS_CONJUNCT ----------

% npu_has_conjunct(_ThisHeadList, [], _LConjList, _RConj, _ModList) :- !, fail.
% npu_has_conjunct(ThisHeadList, [coord(_Conj, [no_lconj_found], 
%                    ThisHeadList, _)|_],_, _, _) :- !, fail.
% npu_has_conjunct(_ThisHeadList, [coord(_Conj, _LConjList, notfound, _)|_],
%                    _, _, _) :- !, fail.
%_ConjList, _LConjList, notfound, _ModList) :- !, fail.


% Case 1: ThisHeadList is coordinated in the first coord struct in the ConjList
npu_has_conjunct(ThisHeadList,
		 [FirstCoordStruct|_MoreCoordStrucs],
                 Conj, LConjList, RConj, ModList) :-
	find_conjunct_in_coord_struct(ThisHeadList, FirstCoordStruct,
				      Conj, LConjList, RConj, ModList),
	!.
% Case 2: Recurse on the rest of the coord structs in the ConjList
npu_has_conjunct(ThisNPU,
		 [_NoMatch|MoreCoordStrucs],
		 Conj, LConjList, RConj, ModList) :-
	npu_has_conjunct(ThisNPU, MoreCoordStrucs, Conj, LConjList, RConj, ModList).

% Case 1a: ThisHeadList is the RConj of the coord term
find_conjunct_in_coord_struct(ThisHeadList,
			      coord(Conj,_Type,_Index,LConjList,RConj,ModList),
			      Conj, LConjList, ThisHeadList, ModList) :-
%	format(user_output, 'Head: ~q~nConj: ~q~n',[ThisHeadList]),
	ThisHeadList = RConj,
	!.
% Handle potential negation differences 
find_conjunct_in_coord_struct(ThisHeadList,
			      coord(Conj,_Type,_Index,LConjList,RConj,ModList),
			      Conj, LConjList, ThisHeadList, ModList) :-
	append(ThisHeadMinus,[negated(_Negated)],ThisHeadList),
	append(RConjMinus,[negated(_RConjNeg)],RConj),
	ThisHeadMinus = RConjMinus,
	!.
% Case 1b: ThisHeadList is a member of the LConjList of coord term
find_conjunct_in_coord_struct(ThisHeadList,
			      coord(Conj,_Type,_Index,LConjList,RConj,ModList),
			      Conj, LConjList, RConj, ModList) :-
	memberchk(ThisHeadList, LConjList),
	!.
% Handle potential negation differences 
find_conjunct_in_coord_struct(ThisHeadList,
			      coord(Conj,_Type,_Index,LConjList,RConj,ModList),
			      Conj, LConjList, RConj, ModList) :-
	append(ThisHeadMinus,[negated(_Negated)],ThisHeadList),
	find_conjunct_in_coord_struct_aux(ThisHeadMinus,LConjList),
	!.

find_conjunct_in_coord_struct_aux(ThisHead,[LConj|_RestLConj]) :-
	append(LConjMinus,[negated(_Negated)],LConj),
	ThisHead = LConjMinus,
	!.
find_conjunct_in_coord_struct_aux(ThisHead,[_LConj|RestLConj]) :-
	find_conjunct_in_coord_struct_aux(ThisHead,RestLConj).



% ---------- Change Head to Mod

% change_head_to_mod([], []).
% change_head_to_mod([head(HeadList)|More], [mod(HeadList)|Gap]) :-
% 	!,
% 	change_head_to_mod(More, Gap).
% change_head_to_mod([NoHead|More], [NoHead|Gap]) :-
% 	change_head_to_mod(More, Gap).

look_for_pmid([ThisStr|_More]) :-
	atom_codes(ThisAtom,ThisStr),
	midstring(ThisAtom, 'PMID', _Back, 0, 4),
	!.
look_for_pmid([_NoPMID|More]) :-
	look_for_pmid(More).

/*
% ************************* Get Token Lists for SemGen****************

added '(' and ')' to the NoBreak list for insert_msu_token_lists_1
in order to handle "Xbai(-)"

*/

% insert_msu_token_lists(minimal_syntax(MSUsIn),
% 		  AnalysisWithSemTypes,
% 		  minimal_syntax(MSUsOut)) :-
%     NoBreak = ['/','-','+'],
%     insert_msu_token_lists_1(MSUsIn, AnalysisWithSemTypes, NoBreak, MSUsOut).
% insert_msu_token_lists(minimal_syntax(MSUsIn),_AnalysisWithSemTypes,
% 	               minimal_syntax(MSUsIn)).

% ---------- Get List for MSUs

% insert_msu_token_lists_1([], [], _, []).
% insert_msu_token_lists_1([ThisSyntMSU|MoreSyntMSUs],
% 			 [ThisSemMSU|MoreSemMSUs],
% 			 NoBreak,
% 			 [[msu_token_list(MinIndex, MaxIndex, TokenList)|ThisSyntMSU]|Gap]):-
% 	% If ThisSyntMSU is of one of the chosen types,
% 	% then identify the corresponding SemMSU to get the index.
% 	member(Type, [head,mod,not_in_lex,pron]),
% 	get_from_list(Type, ThisSyntMSU, _SyntTerm),
% 	get_from_list(Type, ThisSemMSU,  _SemTerm),
% 	!,
% 	%%% need to get index from get_np_atom_list instead!!
% 	get_np_atom_list(ThisSyntMSU, ThisSemMSU, NoBreak, MinIndex, MaxIndex, TokenList),
% 	insert_msu_token_lists_1(MoreSyntMSUs, MoreSemMSUs, NoBreak, Gap).
% insert_msu_token_lists_1([ThisSyntMSU|MoreSyntMSUs],
% 			 [_ThisSemMSU|MoreSemMSUs],
% 			 NoBreak,
% 			 [ThisSyntMSU|Gap]) :-
% 	insert_msu_token_lists_1(MoreSyntMSUs, MoreSemMSUs, NoBreak, Gap).
 
% get_np_atom_list(SyntMSU, SemMSU, NoBreak,
% 		 MinIndex, MaxIndex, ModifiedTokenList) :-
% 	get_word_lists_from_MSU(SyntMSU, SemMSU, MinIndex, MaxIndex, OrigTokenList),
% 	smart_tokenize_nps(OrigTokenList, NoBreak, ModifiedTokenList).
% get_np_atom_list(_SyntMSU,_SemMSU,_NoBreak,0,0,[]). %Halil

% get_word_lists_from_MSU(SyntList, SemList, MinIndex, MaxIndex, BasesList) :-
%     get_word_lists_from_MSU_1(SyntList, SemList, IndexList, [], [], BasesList),
%     IndexList = [MinIndex|_],
%     last(IndexList,MaxIndex).

%%% Pass in a Syntax MSU and a Semantics MSU.
%%% Generate a list of all the inputmatches or bases in the MSU
%%% (subject to certain conditions; e.g., up to the first punctuation mark?)
%%% and the indexes in the Semantics MSU spanned by those bases.

% get_word_lists_from_MSU_1([], _SemList, IndexList, IndexList, BasesList, BasesList).
% get_word_lists_from_MSU_1([ThisSyntItem|MoreSyntItems], SemList,
% 		IndexListIn, IndexListOut, BasesIn, BasesOut) :-
% 	functor(ThisSyntItem, Label, _),
% 	memberchk(Label, [head,mod,not_in_lex,pron,punc,shapes]),
% 	get_word_lists_from_MSU_item(ThisSyntItem, BasesList0, InputMatchList0),
% 	!,
% 	get_corresponding_index(BasesList0, InputMatchList0, SemList, IndexListIn, IndexListNext),
% 	adjust_bases_list(BasesList0, BasesList),
% 	( intersect(BasesList, [',', '.', ':', ';']),
% 	  MoreSyntItems == [] ->
% 	  BasesNext = BasesIn
% 	; append(BasesIn, BasesList, BasesNext)
% 	),
% 	!,
% 	get_word_lists_from_MSU_1(MoreSyntItems, SemList,
% 				  IndexListNext, IndexListOut,
% 				  BasesNext, BasesOut).
% get_word_lists_from_MSU_1([_ThisSyntItem|MoreSyntItems], SemList,
% 			  IndexListIn, IndexListOut, BasesIn, BasesOut) :-
% 	get_word_lists_from_MSU_1(MoreSyntItems, SemList,
% 				  IndexListIn, IndexListOut, BasesIn, BasesOut).

% ----- Adjust Bases List
% Moved from edgarsupport. --Halil 04/14/14.

% hack to deal with infelicities of lexical entries
% adjust_bases_list([],[]).
% adjust_bases_list([H|T], [AdjustedH|AdjustedT]) :-
% 	adjust_one_base(H, AdjustedH),
% 	adjust_bases_list(T, AdjustedT).
% 
% adjust_one_base(fo,     fos) :-   !.
% adjust_one_base(ra,     ras) :-   !.
% adjust_one_base('ra-s', ras) :-   !.
% adjust_one_base(alfa,   alpha) :- !.
% adjust_one_base(hra,    hras) :-  !.
% adjust_one_base(Base,   Base).

%%% Given the Syntax tree's BasesList and InputMatchList,
%%% determine the Indexes in the Semantics tree spanned by those words
%%% We have to check both the Syntax BaseList and InputMatchList
%%% against the Semantics BaseList and InputMatchList
%%% Because of Metamap shenanigans

% get_corresponding_index([], _InputMatchList, _SemList, IndexList, IndexList).
% get_corresponding_index([FirstBase|RestBases],
% 			[FirstInputMatch|RestInputMatches],
% 			SemList, IndexListIn, IndexListOut) :-
% 	get_one_base_index(FirstBase, FirstInputMatch, SemList, IndexListIn, IndexListNext),
% 	get_corresponding_index(RestBases, RestInputMatches, SemList, IndexListNext, IndexListOut).
% get_corresponding_index(_BasesList, _InputMatchList, _SemList, IndexList, IndexList).
% 
% get_one_base_index(Base, InputMatch, SemanticsMSUList, IndexListIn, IndexListNext) :-
% 	member(SemItem, SemanticsMSUList),
% 	get_word_lists_from_MSU_item(SemItem, SemBasesList, SemInputMatchList),
% 	( intersect([Base, InputMatch], SemBasesList) ->
% 	  true
% 	; intersect([Base, InputMatch], SemInputMatchList)
% 	),
% 	!,
% 	arg(1, SemItem, SemItemList),
% 	( get_from_list(index, SemItemList, Index) ->
% 	  IndexListIn = [Index|IndexListNext]
% 	; IndexListNext = IndexListIn
% 	).	
% 
% get_word_lists_from_MSU_item(MSUItem, BasesList, InputMatchList) :-
% 	arg(1, MSUItem, MSUList),
% 	get_from_list(bases, MSUList, BasesList),
% 	get_from_list(inputmatch, MSUList, InputMatchList).
	
/*
% -------------------- Smart Tokenize NP --------------------

This needs to be generalized to apply just to the token list, and then
needs to be incorporated into the (cleaned-up) predicates that find 
the NPs in the first place (in harvest_gene_and_cell_NPs)

*/

% smart_tokenize_nps(InputAtomList0, NoBreak, OutputAtomList) :-
% 	remove_external_hyphen(InputAtomList0, InputAtomList1),
% 	put_in_spaces(InputAtomList1, NoBreak, TempAtomList),
% 	rev(TempAtomList, TempAtomList0),
% 	put_space_before_cell(TempAtomList0, TempAtomList1),
% 	rev(TempAtomList1, TempAtomList2),
% 	lower_list(TempAtomList2, TempAtomList3),
% 	get_new_tokens(TempAtomList3, OutputAtomList).

/* 
% -------------------- Remove External Hyphen --------------------

This predicate is needed because hyphen is ambiguous as to whether it
is inside a word (as in "v-src") or external (as in "mock-transfected").

The rule is that if the token following the hyphen is in the lexicon,
the hyphen is replaced with a space.

*/

% remove_external_hyphen([], []).
% remove_external_hyphen(['-',Token|More], [Token|Gap]) :-
% 	\+ non_lexical_status(Token),
% 	!,
% 	remove_external_hyphen(More, Gap).
% remove_external_hyphen([Atom|More], [Atom|Gap]) :-
% 	remove_external_hyphen(More, Gap).


/*
% ----- NonLexical Status
% Fail if Token is in Lexicon
% Moved from edgarnonlex.pl -- Halil 04/14/14.

Unfortunately, another call has to be made to assembledefns due to the
possibility that the token in question is part of a multiword lexical
entry.  as with "suppressor gene" and "HeLa cells".  "Suppressor"
itself is in the lexicon, while "HeLa" is not. We only want "HeLa" as
a name, and this can only be determined by a separate call to
assembledefns for both "suppressor" and "HeLa".

Abbreviations are considered not to be in the lexicon.  Perhaps this
should be changed so that they can be considered either way.

*/

non_lexical_status(Token) :-
    assemble_definitions([[Token]],Definitions),
    (get_from_list(unknown,Definitions,_)
    ;get_from_list(shapes,Definitions,_)
    ;abbreviation(Definitions)
    ),!.

% ---------- Abbreviation

abbreviation(Definitions) :-
    get_from_list(lexicon,Definitions,Lexicon),
    get_from_list(records,Lexicon,Records),
    get_from_list(lexrec,Records,LexRec),
    get_from_list(entries,LexRec,Entries),
    get_from_list(entry,Entries,Entry),
    get_from_list(misc,Entry,Misc),
    (get_from_list(abbreviation_of,Misc,AbbrevList)
    ;get_from_list(acronym_of,Misc,AbbrevList)
    ),
    \+ AbbrevList == [], !.

% put_in_spaces([], _, []).
% put_in_spaces([Token|More], NoBreak, [Token|Gap]) :-
% 	More == [],
% 	!,
% 	put_in_spaces(More, NoBreak, Gap).
% put_in_spaces([ThisToken,NextToken|More], NoBreak, [ThisToken,NextToken,' '|Gap]) :-
% 	memberchk(ThisToken, NoBreak),
% 	!,
% 	put_in_spaces(More, NoBreak, Gap).
% put_in_spaces([ThisToken,NextToken|More], NoBreak, [ThisToken,NextToken|Gap]) :-
% 	memberchk(NextToken, NoBreak),
% 	!,
% 	put_in_spaces(More, NoBreak, Gap).
% put_in_spaces([Token|More], NoBreak, [Token,' '|Gap]) :-
% 	put_in_spaces(More, NoBreak, Gap).

% put_space_before_cell([], []).
% put_space_before_cell([Token|[]], [Token|[]]) :-
% 	memberchk(Token, [cell,cells]),
% 	!.
% put_space_before_cell([Token,' '|More], [Token,' '|Gap]) :-
% 	memberchk(Token, [cell,cells]),
% 	!,
% 	put_space_before_cell(More, Gap).
% put_space_before_cell([Token,Other|More], [Token,' ',Other|Gap]) :-
% 	memberchk(Token, [cell,cells]),
% 	!,
% 	put_space_before_cell(More, Gap).
% put_space_before_cell([Other|More], [Other|Gap]) :-
% 	put_space_before_cell(More, Gap).

%lower_all_tokens([], []).
%lower_all_tokens([ThisToken|More], [LowToken|Gap]) :-
%	lower(ThisToken, LowToken),
%	lower_all_tokens(More, Gap).

% get_new_tokens([], []) :- !.
% get_new_tokens(TempAtomList, [Token|Gap]) :-
% 	gather_upto_space_or_end(TempAtomList, Token, Remainder),
% 	get_new_tokens(Remainder, Gap).

% gather_upto_space_or_end(TempAtomList, Atom, Remainder) :-
% 	get_token(TempAtomList, TokenList, Remainder),
% 	concat_atom(TokenList, Atom).

% get_token([], [], []).
% get_token([' '|Remainder], [], Remainder) :- !.
% get_token([Atom|More], [Atom|Gap], Remainder) :-
% 	get_token(More, Gap, Remainder).

% ---------- List to Difference List

% list_to_diff_list([], Hole, Hole).
% list_to_diff_list([Head|More], [Head|Gap], Hole) :-
% 	list_to_diff_list(More, Gap, Hole).

% statistics(MSecs) :- 
%     statistics(runtime, [MSecs|_]).

% ----- GET TEXT
get_text(Text, TextID) :-
	get_skr_text(Text,TextID).


% ----- GET MMO TEXT -- Similar to get_text
% except it reads terms.
%get_mmo_text(Prompt, Stream, ID, Text) :-
%	issue_prompt(Prompt),
%	get_mmo_text_1(Stream, ID, Text).

%get_mmo_text_1(InputStream, AbstractID, [First|Rest]) :-
%	get_utterance(InputStream, Label, First),	
%	get_abstract_id_1(Label, AbstractID, _Type),
%	!,
%	get_terms_until_blank_line(InputStream, Rest).
%get_mmo_text_1(_,_,[]).

%get_terms_until_blank_line(Stream,[]) :-
%	at_end_of_stream(Stream),
%	!.
%get_terms_until_blank_line(Stream, Terms):-
%	% Make sure the first character is not a newline.
%	% which marks the end of citation
%	( peek_char(Stream,10) ->
%	  Terms=[]
%       ; get_utterance(Stream, _Label, Term),
%	  Terms=[Term|Rest],
%	  get_terms_until_blank_line(Stream,Rest)
%       ).
	
% write prompt IFF Prompt is not ''
%issue_prompt(Prompt) :-
%	( Prompt == '' ->
%	  true
%	; format('~n~w: ', [Prompt]),
%	  ttyflush
%	).

%get_abstract_id([tok(label,Label,_,_,_)|Rest],AbstractID) :-
%	get_abstract_id_1(Label,AbstractID,_Type),
%	!.
%get_abstract_id([_Token|Rest],AbstractID):-
%	get_abstract_id(Rest,AbstractID).

get_abstract_id(ExpandedUtterances, AbstractID) :-
	ExpandedUtterances = [utterance(Label,_,_,_)|_],
	get_abstract_id_1(Label, AbstractID, _Type).

get_abstract_id_1(Label, AbstractID, Type) :-
	atom_codes(Label, LabelStr),
	split_string_completely(LabelStr, ".", StrList),
	StrList = [AbstractIDStr,TypeStr|_],
	atom_codes(AbstractID, AbstractIDStr),
	atom_codes(Type,TypeStr).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ANNOUNCE predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

announce_initialize_db_access_for_genspec(DataYear) :-
	format(user_output, 'Initializing db access for UMLS Metathesaurus hierarchy for data year ~a...', [DataYear]),
	( initialize_db_access_for_genspec(DataYear) ->		    % from usemrep_main(mancmod)
	  format(user_output, 'successful.~n', [])
	; format(user_output, 'failed. ISA relations will not be extracted.~n', [])
	).

announce_stop_db_access_for_genspec :-
	format(user_output, 'Stopping db access for UMLS Metathesaurus hierarchy...', []),
	( stop_db_access_for_genspec ->		             % from usemrep_main(mancmod)
	  format(user_output, 'successful.~n', [])
	; format(user_output, 'failed.~n', [])
	).

announce_initialize_db_access_for_entrezgene :-
	format(user_output, 'Initializing db access for EntrezGene...', []),
	( initialize_db_access_for_entrezgene ->		    % from usemrep_lib(entrezgene)
	  format(user_output, 'successful.~n', [])
	; format(user_output, 'failed.~n', [])
	).

announce_stop_db_access_for_entrezgene :-
	format(user_output, 'Stopping db access for EntrezGene...', []),
	( stop_db_access_for_entrezgene ->		             % from usemrep_lib(entrezgene)
	  format(user_output, 'successful.~n', [])
	; format(user_output, 'failed.~n', [])
	).

%announce_initialize_db_access_for_profile_call :-
%	format(user_output, 'Initializing access for JDI...', []),
%	( initialize_db_access_for_profile_call ->	    % from usemrep_lib(jdimod)
%	  format(user_output, 'successful.~n', [])
%	; format(user_output, 'failed.~n', [])
%	).

%announce_stop_db_access_for_profile_call :-
%	format(user_output, 'Stopping access for JDI...', []),
%	( stop_db_access_for_profile_call ->		     % from usemrep_lib(jdimod)
%	  format(user_output, 'successful.~n', [])
%	; format(user_output, 'failed.~n', [])
%	).

announce_initialize_metamap(AllOptions) :-
	format(user_output, 'Initializing MetaMap with options...~n', []),
	format(user_output, '~w~n...', [AllOptions]),
%	( initialize_metamap(Options) ->		    % from skr(skr)
	( initialize_skr(AllOptions) ->		    % from skr(skr)
	  format(user_output, 'successful.~n', [])
	; format(user_output, 'failed.~n', [])
	).

announce_stop_metamap :-
	format(user_output, 'Stopping MetaMap...', []),
%	( stop_metamap ->				     % from skr(skr)
	( stop_skr ->				     % from skr(skr)
	  format(user_output, 'successful.~n', [])
	; format(user_output, 'failed.~n', [])
	).

choose_utterance_list(Domain, Utterances, ExpandedUtterances, ChosenUtterances) :-
	( Domain == genetics ->
	  ChosenUtterances = Utterances
	; ChosenUtterances = ExpandedUtterances
	).

% like memberchk/2, but won't loop if given a list with var tail as second argument
% var_memberchk(_, Var) :-
% 	var(Var),
% 	!,
% 	fail.
% var_memberchk(X, [X|_]) :- !.
% var_memberchk(X, [_|L]) :-
% 	nonvar(L),
% 	var_memberchk(X, L).


% like library(sets):list_to_set/2, but won't loop if given a list with var tail as second argument
% var_list_to_set(List, Set) :-
% 	( var(List) ->
% 	  Set = []
% 	; var_list_to_set_1(List, Set)
% 	).
% 
% var_list_to_set_1([Head|Tail], Set) :-
% 	var(Tail),
% 	!,
% 	Set = [Head].
% var_list_to_set_1([Head|Tail], Set) :-
% 	var_memberchk(Head, Tail),
% 	!,
% 	var_list_to_set_1(Tail, Set).
% var_list_to_set_1([Head|Tail], [Head|Set]) :-
%         var_list_to_set_1(Tail, Set).


% This predicate is not currently used, but can be called from
% perform_relational_analysis/9 after insert_clinical_trials_semtypes/3
% to generate output flagging certain semtypes of interest.
% A typical call would look like this:
% flag_interesting_semtypes(AnalysisWithSemTypes2, OutputStream, [dsyn,phsu,'CTGM'])

%flag_interesting_semtypes([], _OutputStream, _InputLabel, _InterestingSemTypes).
%flag_interesting_semtypes([H|T], OutputStream, InputLabel, InterestingSemTypes) :-
%	flag_interesting_semtypes_1(H, OutputStream, InputLabel, InterestingSemTypes),
%	flag_interesting_semtypes(T, OutputStream, InputLabel, InterestingSemTypes).

%flag_interesting_semtypes_1([], _OutputStream, _InputLabel, _InterestingSemTypes).
%flag_interesting_semtypes_1([H|T], OutputStream, InputLabel, InterestingSemTypes) :-
%	flag_interesting_semtypes_2(H, OutputStream, InputLabel, InterestingSemTypes),
%	flag_interesting_semtypes_1(T, OutputStream, InputLabel, InterestingSemTypes).

%flag_interesting_semtypes_2(Term, OutputStream, InputLabel, InterestingSemTypes) :-
%	arg(1, Term, List),
%	( memberchk(metaconc(MetaconcList), List) ->
%	  flag_interesting_semtypes_3(MetaconcList, OutputStream, InputLabel, InterestingSemTypes)
%	; true
%	).

%flag_interesting_semtypes_3([], _OutputStream, _InputLabel, _InterestingSemTypes).
%flag_interesting_semtypes_3([Concept:SemTypeList|Rest], OutputStream, InputLabel, InterestingSemTypes) :-
%	intersection(SemTypeList, InterestingSemTypes, [H|T]),
%	!,
%	format(OutputStream, '~n~w $$$$ FOUND: ~w ~w~n', [InputLabel, Concept,[H|T]]),
%        flag_interesting_semtypes_3(Rest, OutputStream, InputLabel, InterestingSemTypes).
%flag_interesting_semtypes_3([_Boring|Rest], OutputStream, InputLabel, InterestingSemTypes) :-
%        flag_interesting_semtypes_3(Rest, OutputStream, InputLabel, InterestingSemTypes).


% This predicate is not currently used, but can be called from
% perform_relational_analysis/9 after insert_clinical_trials_semtypes/3
% to generate output flagging mod(_) and head(_) structures that have no metaconc term.
% A typical call would look like this:
% show_mods_and_heads_with_no_metaconcs(AnalysisWithSemTypes1, InputLabel, OutputStream)

% show_mods_and_heads_with_no_metaconcs(minimal_syntax(Syntax), InputLabel, OutputStream) :-
% 	show_mods_and_heads_with_no_metaconcs(Syntax, InputLabel, OutputStream).
% 
% show_mods_and_heads_with_no_metaconcs([], _InputLabel, _OutputStream).
% show_mods_and_heads_with_no_metaconcs([H|T], InputLabel, OutputStream) :-
% 	show_mods_and_heads_with_no_metaconcs_1(H, InputLabel, OutputStream),
% 	show_mods_and_heads_with_no_metaconcs(T, InputLabel, OutputStream).
% 
% show_mods_and_heads_with_no_metaconcs_1(List, InputLabel, OutputStream) :-
% 	member(Element, List),
% 	functor(Element, Functor, 1),
% 	arg(1, Element, ArgList),
% 	memberchk(Functor, [mod,head]),	
% 	\+ memberchk(metaconc(_), ArgList),
% 	get_from_list(tokens, ArgList, Tokens),
% 	concat_with_blank_spaces(Tokens, Atom),
% 	format(OutputStream, '~n$$$$ ~w: ~w with NO Metaconc: ~w~n', [InputLabel,Functor,Atom]),
% 	flush_output(OutputStream),
% 	fail.
% show_mods_and_heads_with_no_metaconcs_1(_List, _InputLabel, _OutputStream).


% insert_clinical_trials_semtypes/3 takes a Minimal Syntax List as its first argument,
% and inserts a metaconc structure into head(_) and mod(_) terms that do not already have one
% if the concatenation of the tokens of the head/mod is a known clinical-trial lexical item.
% Known clinical-trial lexical items are defined in usemrep_lib/clinical_trials_data.pl

%insert_clinical_trials_semtypes(In, Domain, Out) :-
%	insert_clinical_trials_semtypes_into_parse_tree(In, Domain, Out).
%	% flag_diffs(In, Out).

%maybe_insert_domain_concepts(In, SyntIn, Out, SyntOut) :-
%	( control_value(domain, _Domain) ->
%	  insert_domain_concepts_into_parse_tree(In, Out),
%	  remove_entire_referential_analysis(Out,SyntOut)
%	; Out = In,
%	  SyntOut = SyntIn
%	).

maybe_insert_domain_concepts(In, _SyntIn, Out, SyntOut) :-
	insert_domain_concepts_into_parse_tree(In, Out),
	remove_entire_referential_analysis(Out,SyntOut).

insert_domain_concepts_into_parse_tree(In,Out) :-
	rev_all(In,RevIn),
	insert_domain_concepts_into_parse_tree_aux(RevIn,[],RevOut),
	!,
        rev_all(RevOut,Out).
insert_domain_concepts_into_parse_tree(In,In).

rev_all(In,Out) :-
	rev(In,RevIn),
	rev_all_aux(RevIn, Out).

rev_all_aux([],[]) :- !.
rev_all_aux([MSU|Rest],[MSUOut|RestOut]) :-
	rev(MSU,MSUOut),
	rev_all_aux(Rest,RestOut).

insert_domain_concepts_into_parse_tree_aux([],In,In).
insert_domain_concepts_into_parse_tree_aux([Phrase|Rest],In,Out) :-
	evaluate_phrases(Phrase,Phrase,Rest,NewPhrase,NewPhraseSpans),
	\+ NewPhraseSpans == [],
	find_largest_span(NewPhraseSpans,NewPhraseSpan,0),
	Out = [NewPhrase|Next],
	append(NewPhraseSpan,NewRest,[Phrase|Rest]),
	insert_domain_concepts_into_parse_tree_aux(NewRest,In,Next).
insert_domain_concepts_into_parse_tree_aux([Phrase|Rest],In,[Phrase|Next]) :-
	insert_domain_concepts_into_parse_tree_aux(Rest,In,Next).

% flag_diffs(In, Out) :-
% 	( In = Out ->
% 	  true
% 	; format('~n~n############## BEFORE CLIN TRIALS ##########~n~n', []),
% 	  portray_minimal_syntax:portray_minimal_syntax_structure(In),
% 	  format('~n~n############## AFTER CLIN TRIALS ##########~n~n', []),
% 	  portray_minimal_syntax:portray_minimal_syntax_structure(Out)
% 	).

%insert_clinical_trials_semtypes_into_parse_tree([], _Domain, []).
%insert_clinical_trials_semtypes_into_parse_tree([MSU|T], Domain, [NewMSU|NewT]) :-
%	MSU = [MSUHead|MSUTail],
%	Inserted is 0,
%	insert_clinical_trials_semtypes_into_MSU(MSUTail, MSUHead, Inserted, Domain, NewMSU),
%	insert_clinical_trials_semtypes_into_parse_tree(T, Domain, NewT).

%insert_clinical_trials_semtypes_into_MSU([], MSULast, InsertedSoFar, Domain, Rest) :-
%	insert_clinical_trials_semtypes_into_MSU_term(MSULast, InsertedHere, Domain, NewMSULast),
%	Inserted is InsertedSoFar \/ InsertedHere,
%	add_confidence_term_if_necessary(NewMSULast, Inserted, Rest).
%insert_clinical_trials_semtypes_into_MSU([Next|T], Current, InsertedSoFar, Domain, [NewCurrent|NewT]) :-
%	insert_clinical_trials_semtypes_into_MSU_term(Current, InsertedHere, Domain, NewCurrent),
%	Inserted is InsertedSoFar \/ InsertedHere,
%	insert_clinical_trials_semtypes_into_MSU(T, Next, Inserted, Domain, NewT).

evaluate_phrases([],_FullPhrase,_Rest,[],[]) :- !.
evaluate_phrases(Phrase,FullPhrase,Rest,[CandidateOut|PhraseOutRest],[CandidatePhrase|PhraseOutSpanRest]) :-
	Phrase = [H|_T],
	functor(H,F,1),
	memberchk(F,[mod,head,shapes,not_in_lex]),
	append([FullPhrase],Rest,All),
	generate_candidates(Phrase,Rest,All,Candidates,CandidatePhrases),
	evaluate_candidates(Candidates,Candidate,CandidatePhrases,CandidatePhrase,
	                        CandidateAtom,CandidateMetaConc),
	merge_into_macro_NP_if_necessary(Candidate,CandidateAtom,CandidateMetaConc,CandidateOut),
	reorganize_macro_NP_MSU_if_necessary(Phrase,Candidate,CandidatePhrase,CandidateOut,NewMSU),
	NewMSU = [CandidateOut|NewRest],
	evaluate_phrases(NewRest,FullPhrase,Rest,PhraseOutRest,PhraseOutSpanRest).
evaluate_phrases([H|T],FullPhrase,Rest,[H|PhraseOut],PhraseSpanOut) :-
	evaluate_phrases(T,FullPhrase,Rest,PhraseOut,PhraseSpanOut).

generate_candidates([],[],_,[],[]) :- !.
generate_candidates(Phrase,[],AllPhrase,[Phrase|Rest],[AllPhrase|APRest]) :-
	last(Phrase,Last),
	append(NewPhrase,[Last],Phrase),
	generate_candidates(NewPhrase,[],AllPhrase,Rest,APRest).
generate_candidates(Phrase,RestPhrases,AllPhrases,[Candidate|Rest],[AllPhrases|CPRest]) :-
	append(RestPhrases,RestPhrases0),
	append(Phrase,RestPhrases0,Candidate),
	last(RestPhrases,LastPhrase),
	append(AllButLastPhrase,[LastPhrase],RestPhrases),
	last(LastPhrase,Last),
	append(NewLastPhrase,[Last],LastPhrase),
	collect_bases(NewLastPhrase, NewLastPhraseBases),
	( NewLastPhraseBases == [] ->
	  % no more contributing items in the Phrase
	  NewRestPhrases = AllButLastPhrase,
	  AllPhrases = [H|_],
	  append([H],AllButLastPhrase,NewFullPhrase)
        ; append(AllButLastPhrase,[NewLastPhrase],NewRestPhrases),
	  NewFullPhrase = AllPhrases
        ),
	!,
	generate_candidates(Phrase,NewRestPhrases,NewFullPhrase,Rest,CPRest).

find_largest_span([],_Phrase,_Length) :- !.
find_largest_span([Phrase|Rest], Phrase, Length) :-
	length(Phrase,PhraseLen),
	PhraseLen > Length,
	find_largest_span(Rest,_Phrase,PhraseLen).
find_largest_span([_Phrase|Rest],_Phrase, Length) :-
	find_largest_span(Rest,_Phrase,Length).
 
reorganize_macro_NP_MSU_if_necessary(Phrase,Candidate,CandidatePhrase,CandidateOut,NewMSU) :-
	match_to_candidate_begin(Phrase,Candidate,PhraseUpToCandidate),
	last(CandidatePhrase,LastPhrase),
	rev(Candidate,RevCandidate),
	rev(LastPhrase,RevLastPhrase),
	match_to_candidate_begin(RevLastPhrase,RevCandidate,RevPhraseAfterCandidate),
	rev(RevPhraseAfterCandidate,PhraseAfterCandidate),
	append(PhraseUpToCandidate,[CandidateOut],X),
	append(X,PhraseAfterCandidate,NewMSU).

match_to_candidate_begin([Item|Rest],[Item|CRest],[]) :-
	(append(CRest,_,Rest)
        ; append(Rest,_,CRest)
        ), !.
match_to_candidate_begin([Item|Rest],Candidate,[Item|RestOut]) :-
	match_to_candidate_begin(Rest,Candidate,RestOut).
% evaluate_candidates([],_,[],_,_,_) :- !, fail.
evaluate_candidates([Candidate|_],Candidate,[CandidatePhrase|_],CandidatePhrase,
                                 CandidateAtom,NewMetaConc) :-
	control_value(domain, _Domain),
	collect_bases(Candidate,CandidateBases0),
	rev(CandidateBases0,RevCandidateBases0),
	append(RevCandidateBases0,CandidateBases),
	concat_atom(CandidateBases,' ',CandidateAtom),
	domain_concept(CandidateAtom,NewMetaConc),
	!.
% Only use concepts that are based on UMLS
evaluate_candidates([Candidate|_],Candidate,[CandidatePhrase|_],CandidatePhrase,
		    CandidateAtom,NewMetaConc) :-
	control_option(use_generic_domain_modification),
	collect_bases(Candidate,CandidateBases0),
	rev(CandidateBases0,RevCandidateBases0),
	append(RevCandidateBases0,CandidateBases),
	concat_atom(CandidateBases,' ',CandidateAtom),
	generic_domain:domain_concept(CandidateAtom,NewMetaConc),
	NewMetaConc = _PreferredName:UI:_SemTypes,
	substring(UI,'C',0,1),
	!.
evaluate_candidates([Candidate|_],Candidate,[CandidatePhrase|_],CandidatePhrase,
		    CandidateAtom,NewMetaConc) :-
	control_option(use_generic_domain_extension),
	collect_bases(Candidate,CandidateBases0),
	rev(CandidateBases0,RevCandidateBases0),
	append(RevCandidateBases0,CandidateBases),
	concat_atom(CandidateBases,' ',CandidateAtom),
	generic_domain:domain_concept(CandidateAtom,NewMetaConc),
	!.
evaluate_candidates([Candidate|_],Candidate,[CandidatePhrase|_],CandidatePhrase,
	                         _CandidateAtom,NewMetaConcs) :-
	length(Candidate,1),
	Candidate = [Item],
	functor(Item,_Name,1),
	arg(1,Item,List),
	get_from_list(metaconc,List,MetaConcs),
	replace_metaconcs_if_necessary(MetaConcs, NewMetaConcs),
	!.
evaluate_candidates([_|Rest],Candidate,[_|CPRest],CandidatePhrase,
                                 CandidateAtom,NewMetaConc) :-
	evaluate_candidates(Rest,Candidate,CPRest,CandidatePhrase,
                                         CandidateAtom,NewMetaConc).
replace_metaconcs_if_necessary([],[]) :- !.
replace_metaconcs_if_necessary([MetaConcIn|Rest],[MetaConcOut|RestOut]) :-
	control_value(domain, _Domain),
	domain_replace(MetaConcIn,MetaConcOut),
	!, 
	replace_metaconcs_if_necessary(Rest,RestOut).
%replace_metaconcs_if_necessary([MetaConcIn|Rest],[MetaConcOut|RestOut]) :-
%	MetaConcIn = StringIn:CUIIn:SemTypeListIn,
%	domain_replace(StringIn, CUIIn, SemTypeListIn,
%		       StringOut, CUIOut, SemTypeListOut),
%	!, % cut is still necessary because of catch-all domain_replace/6 clauses
%	MetaConcOut = StringOut:CUIOut:SemTypeListOut,
				%	replace_metaconcs_if_necessary(Rest,RestOut).
replace_metaconcs_if_necessary([MetaConcIn|Rest],[MetaConcOut|RestOut]) :-
        ( control_option(use_generic_domain_extension)
        ; control_option(use_generic_domain_modification)
        ),
	generic_domain:domain_replace(MetaConcIn,MetaConcOut),
	!, 
	replace_metaconcs_if_necessary(Rest,RestOut).
replace_metaconcs_if_necessary([MetaConc|Rest],[MetaConc|RestOut]) :-
	replace_metaconcs_if_necessary(Rest,RestOut).

collect_bases([],[]) :- !.
collect_bases([Item|Rest],[LBasesOut|RestOut]) :-
        functor(Item,_,1),
	arg(1,Item,List),
	get_from_list(bases,List,Bases),
	lower_list(Bases,LBases),
        ( is_base_exception(List,LBases,LBasesOut)
	; LBasesOut = LBases
        ), 
	!,
	collect_bases(Rest,RestOut).
collect_bases([_Item|Rest],Out) :-
	collect_bases(Rest,Out).

is_base_exception(List,Base,BaseOut) :-
	get_from_list(inputmatch,List,InputMatch),
	lower_list(InputMatch,LInputMatch),
	base_exception(Base,LInputMatch),
	BaseOut = LInputMatch.

base_exception([hh],[hhs]).
base_exception([dh],[dhs]).

remove_confid([],[]) :- !.
remove_confid([confid(_Confid)|Rest],RestOut) :-
	remove_confid(Rest,RestOut).
remove_confid([Item|Rest],[Item|RestOut]) :-
	remove_confid(Rest,RestOut).

%cleanup
merge_into_macro_NP_if_necessary(Candidate,_CandidateAtom,CandidateMetaConc,CandidateOut) :-
	rev(Candidate,RevCandidate0),
	Candidate = [H|_],
	functor(H,Name,1),
	arg(1,H,HList),
	remove_confid(RevCandidate0,RevCandidate),
	RevCandidate= [RH|_],
	functor(RH,_RName,1),
	arg(1,RH,RHList),
	get_feature_for_all_items(lexmatch,RevCandidate,CandidateLexMatch),
	(CandidateLexMatch \== [] -> 
	 append([],[lexmatch(CandidateLexMatch)],TempCandidate0)
        ; TempCandidate0 = []
        ),
	get_feature_for_all_items(inputmatch,RevCandidate,CandidateInputMatch),
        (CandidateInputMatch \== [] ->
	 append(TempCandidate0,[inputmatch(CandidateInputMatch)],TempCandidate1)
        ; TempCandidate1 = TempCandidate0
        ),
	( get_from_list(tag,HList,Tag) ->
	  append(TempCandidate1,[tag(Tag)],TempCandidate2)
        ; TempCandidate2 = TempCandidate1
        ),
	get_feature_for_all_items(tokens,RevCandidate,CandidateTokens),
	(CandidateTokens \== [] ->
	 append(TempCandidate2,[tokens(CandidateTokens)],TempCandidate3)
        ; TempCandidate3 = TempCandidate2
        ),
	get_feature_for_all_items(bases,RevCandidate,CandidateBases),
	(CandidateBases \== [] ->
	 append(TempCandidate3,[bases(CandidateBases)],TempCandidate4)
        ; TempCandidate4 = TempCandidate3
        ),
	( is_list(CandidateMetaConc) ->
	  append(TempCandidate4,[metaconc(CandidateMetaConc)],TempCandidate5)
        ; append(TempCandidate4,[metaconc([CandidateMetaConc])],TempCandidate5)
        ),
	( do_negex,
	  get_from_list(negated,HList,Negated) ->
	  append(TempCandidate5, [negated(Negated)],TempCandidate6)
	; TempCandidate6 = TempCandidate5
	),
	memberchk(position(_StartPos,EndPos),HList),
	memberchk(position(StartPosR,_EndPosR),RHList),
	append(TempCandidate6,[position(StartPosR,EndPos)],TempCandidate),
	( control_value(domain, DOMAIN) ->
	  add_other_MSU_components(1,TempCandidate,DOMAIN,CandidateOutList)
	; add_other_MSU_components(1,TempCandidate,'',CandidateOutList)
	),
	functor(CandidateOut,Name,1),
	arg(1,CandidateOut,CandidateOutList),
	!.

get_feature_for_all_items(Feature,List,ListOut) :-
	get_feature_for_all_items_aux(Feature,List,List0),
	( append(List0,ListOut)
	; ListOut = List0
	).

get_feature_for_all_items_aux(_Feature,[],[]) :- !.
get_feature_for_all_items_aux(Feature,[ThisItem|MoreItems],[NewItem|Rest]) :-
	functor(ThisItem,_,1),
	arg(1,ThisItem,FeatureList),
	get_from_list(Feature,FeatureList,NewItem),
	!,
	get_feature_for_all_items_aux(Feature, MoreItems,Rest).
get_feature_for_all_items_aux(Feature,[_ThisItem|MoreItems],FeatureOut) :-
	get_feature_for_all_items_aux(Feature,MoreItems,FeatureOut).

%insert_domain_concepts_into_MSU_terms([], MSULast, InsertedSoFar, Rest) :-
%	insert_domain_concepts_into_MSU_term(MSULast, InsertedHere, NewMSULast),
%	Inserted is InsertedSoFar \/ InsertedHere,
%	add_confidence_term_if_necessary(NewMSULast, Inserted, Rest).
%insert_domain_concepts_into_MSU_terms([Next|T], Current, InsertedSoFar, [NewCurrent|NewT]) :-
%	insert_domain_concepts_into_MSU_term(Current, InsertedHere, NewCurrent),
%	Inserted is InsertedSoFar \/ InsertedHere,
%	insert_domain_concepts_into_MSU_terms(T, Next, Inserted, NewT).

%insert_clinical_trials_semtypes_into_MSU_term(mod(ModList), Inserted, Domain, mod(NewModList)) :-
%	!,
%	ModList = [H|T],
%	insert_clinical_trials_semtypes_into_mod_or_head_list(T, H, _TokenList, Inserted, TempModList),
%	add_other_MSU_components(Inserted, TempModList, Domain, NewModList).

%insert_clinical_trials_semtypes_into_MSU_term(head(HeadList), Inserted, Domain, head(NewHeadList)) :-
%	!,
%	HeadList = [H|T],
%	insert_clinical_trials_semtypes_into_mod_or_head_list(T, H, _TokenList, Inserted, TempHeadList),
%	add_other_MSU_components(Inserted, TempHeadList, Domain, NewHeadList).
%insert_clinical_trials_semtypes_into_MSU_term(OtherTerm, 0, _Domain, OtherTerm).


/*
  The following code assumes that mod and head lists contain elements
  in the following order:
             index(3)
             usemtype(['Functional Concept'])
             ausemtype([ftcn])
             semgroup([conc])
             lexmatch([present])
             inputmatch([present])
             tag(adj)
             tokens([present])
	     position(1,7)
             bases([present])
             metaconc(['Present':[ftcn]])
*/


% If we get to the end of the list, that means no metaconc(_) term was found, so insert one.
%insert_clinical_trials_semtypes_into_mod_or_head_list([], LastElement, TokenList,
%						      Inserted, NewList) :-
%       add_metaconc_if_necessary(LastElement, TokenList, Inserted, NewList, ctrials).
%insert_clinical_trials_semtypes_into_mod_or_head_list([H|T], Term, TokenList,
%						      Inserted, NewList) :-
%	( % If we find a metaconc(_), there's nothing do to; just pass back the rest of the list.
%	  Term = metaconc(_) ->
%	  Inserted = 0,
%	  NewList = [Term,H|T]
%	; % If we find a tokens(_), pass in the TokenList
%	  % so we can know what tokens to look for in the CTG list.
%	  Term = tokens(TokenList) ->
%	  NewList = [Term|Rest],
%	  insert_clinical_trials_semtypes_into_mod_or_head_list(T, H, TokenList, Inserted, Rest)
%	; % If we find anything else, just keep on going.
%	  NewList = [Term|Rest],
%	  insert_clinical_trials_semtypes_into_mod_or_head_list(T, H, TokenList, Inserted, Rest)
%	).

% need to make sure list has the following components, in this order:
% usemtype(_)
% ausemtype(_)
% semgroup(_)
% lexmatch(_)     % usually already in structure
% inputmatch(_)   % usually already in structure
% tag(_)          % usually already in structure
% tokens(_)       % usually already in structure
% bases(_)        % usually already in structure
% metaconc(_)

add_other_MSU_components(0, ComponentList, _Domain, ComponentList).
add_other_MSU_components(1, ComponentList, Domain,  EnhancedComponentList) :-
	List0 = [usemtype(FullSemTypes),
		ausemtype(AUSemTypes),
		semgroup(SemGroups),
% This caused a bug in semantic_charpos in cases of mixed case.
% For instance, in "Islets ...", inputmatch became "islets"
%				    lexmatch(InputMatch),
		lexmatch(LexMatch),
		inputmatch(InputMatch),
		tag(_Tag),
		tokens(_Tokens),
		position(_Start,_End),
		bases(_Bases),
		metaconc(MetaConcs)],
	( do_negex, get_from_list(negated,ComponentList,_N) ->
	  append(List0, [negated(_Negated)],List)
	; List = List0
	),
%	add_other_MSU_components_1([usemtype(FullSemTypes),
%				    ausemtype(AUSemTypes),
%				    semgroup(SemGroups),
%% This caused a bug in semantic_charpos in cases of mixed case.
%% For instance, in "Islets ...", inputmatch became "islets"
%%				    lexmatch(InputMatch),
%				    lexmatch(LexMatch),
%				    inputmatch(InputMatch),
%				    tag(_Tag),
%				    tokens(_Tokens),
%                                   position(_Start,_End),
%				    negated(_Negated),
%				    bases(_Bases),
%				    metaconc(MetaConcs)],
	add_other_MSU_components_1(List,
				   ComponentList,
				   EnhancedComponentList),
% when no lexmatch exists, take the value of inputmatch
        ( var(LexMatch),
	  lower_list(InputMatch, LexMatch)
        ; true
        ),
	get_all_metaconc_semtypes(MetaConcs, AUSemTypes),
	abbreviate_umls_semtypes(AUSemTypes, FullSemTypes),
	look_up_semantic_groups(AUSemTypes,  SemGroups, Domain).

add_other_MSU_components_1([], _ComponentList, []).
add_other_MSU_components_1([FirstComponent|RestComponents], ComponentList,
			   [FirstComponent|RestComponents]) :-
	( memberchk(FirstComponent, ComponentList) ->
	  true
        ; true
        ),
	add_other_MSU_components_1(RestComponents, ComponentList, RestComponents).	

lower_list([], []) :- !.
lower_list([WordIn|Rest], [WordOut|Gap]) :-
	lower(WordIn, WordOut),
	lower_list(Rest, Gap).

get_all_metaconc_semtypes([], []).
get_all_metaconc_semtypes([_FirstMetaConc:_CUI:SemTypeList|RestMetaConcs], AllSemTypes) :-
	append(SemTypeList, Rest, AllSemTypes),
	get_all_metaconc_semtypes(RestMetaConcs, Rest).

% If there's already a confid(_) term, them simply return it unchanged.
% add_confidence_term_if_necessary(confid(C), _,        [confid(C)]) :- !.
% If a CTG metaconc term had to be added and there was no confid(_) term already,
% then add a confid(1000) term
% add_confidence_term_if_necessary(Other,     Inserted, [Other|Rest]) :-
% 	( var(Inserted) ->
% 	  Rest = []
% 	; Inserted =:= 1 ->
% 	  Rest = [confid(1000)]
% 	; Rest = []
% 	).

% When we get to here, we've reached the last element of the HeadList or ModList list
% (the first argument of the mod(_) or head(_) term).

% If the last term of the list is a metaconc(_) term,
% there's no need to create and insert a special clinical-trials metaconc,
% so just return the existing metaconc(_) term unchanged.

% If the last term of the list is anything else, no metaconc(_) term was found,
% so either take the existing TokenList or
% grab the TokenList from the tokens(_) term (if the last term is a tokens(_) term)
% and use the information in the TokenList to determine if we need to
% create and insert a special clinical-trials metaconc term.

% Why no metaconc(_) term was found earlier:
% if a metaconc(_) term had been found earlier,
% insert_clinical_trials_semtypes_into_mod_or_head_list/5 would have exited;
% if the last term was a metaconc(_) term, the first branch of
% add_metaconc_if_necessary/4 would have been taken).

%add_metaconc_if_necessary(LastTerm, TokenList, Inserted, NewModOrHeadList, ctrials) :-
%	( LastTerm = metaconc(_) ->
%	  Inserted = 0,
%	  NewModOrHeadList = [LastTerm]
%	; get_token_list(LastTerm, TokenList),
%	  concat_with_blank_spaces(TokenList, TokenAtom),
%%	  create_new_metaconc_term(TokenAtom, MetaConcList),
%	  create_new_metaconc_term(TokenAtom, MetaConcList, ctrials),
%	  set_metaconc_inserted(MetaConcList, Inserted),
%	  create_new_mod_or_head_list(MetaConcList, LastTerm, NewModOrHeadList)
%	).


%create_new_mod_or_head_list([], LastTerm, [LastTerm]).
%create_new_mod_or_head_list([metaconc(MC)], LastTerm, [LastTerm,metaconc(MC)]).

% If the last term is a tokens(_) term, grab its arguments as the TokenList;
% otherwise, if we have a nonvar TokenList, then just use it;
% otherwise, (no TokenList has been found yet), then we have a problem,
% so complain and fail.
% get_token_list(LastTerm, TokenList) :-
% 	( LastTerm = tokens(TokenList) ->
% 	  true
% 	; nonvar(TokenList) ->
% 	  true
% 	;  format('~n~n### No tokenlist(_) term found!~n~n', []),
% 	  fail
% 	).

% set_metaconc_inserted([],    0).
% It seems like Inserted variable is never updated.
% Causing problems with adding semtypes, semgroups, etc.
%set_metaconc_inserted([_|_], 0).
% set_metaconc_inserted([_|_], 1).

% If the TokenAtom created from the tokens(_) term is a known clinical-trials atom,
% then create and return a new metaconc(_) term;
% otherwise, simply return [].
%%create_new_metaconc_term(TokenAtom, MetaConcList) :-
%create_new_metaconc_term(TokenAtom, MetaConcList, ctrials) :-
%	  ( clinical_trials_data(TokenAtom, SemanticTypeList) ->
%	    % Add the special token 'CTGM' to signify Clinical Trials.Gov Metaconc
%	    MetaConcList = [metaconc([TokenAtom:'':SemanticTypeList])]
%	  ; MetaConcList = []
%         ).

% concat_with_blank_spaces([], '').
% concat_with_blank_spaces([H|T], Atom) :-
% 	add_blank_spaces(T, H, TokensWithSpaces),
% 	concat_atom(TokensWithSpaces, Atom).

% add_blank_spaces([], Head, [Head]).
% add_blank_spaces([Next|Tail], Head, [Head, ' '|Rest]) :-
% 	add_blank_spaces(Tail, Next, Rest).

head_lists_are_not_conjuncts([], _SubjectHeadList, _ObjectHeadList).
head_lists_are_not_conjuncts([FirstConjunct|RestConjuncts], SubjectHeadList, ObjectHeadList) :-
	head_lists_are_not_conjuncts_1(FirstConjunct, SubjectHeadList, ObjectHeadList),
	head_lists_are_not_conjuncts(RestConjuncts, SubjectHeadList, ObjectHeadList).

head_lists_are_not_conjuncts_1([], _SubjectHeadList, _ObjectHeadList).
head_lists_are_not_conjuncts_1(coord(_Conj, _Type, _Index, LConjList, RConj, []),
			       SubjectHeadList, ObjectHeadList) :-
	AllConjuncts = [RConj|LConjList],
	( memberchk(SubjectHeadList,  AllConjuncts) ->
	  \+ memberchk(ObjectHeadList, AllConjuncts)
	; true
	).

% Disallow conjunction between subject and object
% If there is a conjunction (including subordinating conjunctions) between two NPs, 
% the relevant concepts can't be arguments of the same predication. Except for reciprocals (like meet) 
% I believe this is true in general. The only exception (that I can think of now) is an object preposed 
% around a verb that takes two objects and has a coordinated subject as in 
% "the girl to whom John and Bill sent the letter". 
%But SemRep doesn?t interpret more than one object and doesn?t find preposed objects.
% I commented this out, because it seems too loose. I implemented no_conj_between_arguments/3 below.
% Existence of *anything* conj caused issues, so I limited this to actual realizations of coordination.
%no_conj_between_head_lists([],_SubjIndex,_ObjIndex) :- !.
%no_conj_between_head_lists([[msu_index(_Index)|MSU]|_RestMSU],SubjIndex,ObjIndex) :-
%	simplify_index(SubjIndex,SubjIndex0),
%	simplify_index(ObjIndex,ObjIndex0),
%	get_from_list(conj,MSU,ConjList),
%	get_from_list(index,ConjList,Index),
%	( Index > ObjIndex0,  SubjIndex0 > Index
%        ; Index > SubjIndex0, ObjIndex0 > Index
%        ),
%	!,
%	fail.
%no_conj_between_head_lists([_MSU|RestMSU],SubjIndex,ObjIndex) :-
%	no_conj_between_head_lists(RestMSU,ObjIndex,SubjIndex).

simplify_index(Index:_A/_B,Index) :- !.
simplify_index(Index,Index).

no_conj_between_arguments([],_SubjIndex,_ObjIndex) :- !.
no_conj_between_arguments([coord(_,_,Index,_,_,[])|_RestMSUs],
	                       SubjIndex,ObjIndex) :-
	simplify_index(SubjIndex,SubjIndex0),
	simplify_index(ObjIndex,ObjIndex0),
	( Index > SubjIndex0, Index > ObjIndex0
	; SubjIndex0 > Index, ObjIndex0 > Index
	),
	!.
no_conj_between_arguments([_Coord|RestConjList],SubjIndex,ObjIndex) :-
	no_conj_between_arguments(RestConjList,SubjIndex,ObjIndex).

/*
% ---------- LOCATE_NPU_HEAD ----------
The intent is to find the next NP (only NP's have heads). However, multi-phrase
anatomical terms cause problems. Because of the way they're processed by
MetaMap, they come back without a head.  They in fact probably should get
a head from MetaMap, but until the whole issue is resolved, locate_npu_head
will look for look for the right-most mod(), if the phrase doesn't have a
head.  It is also the case that only NP's have mods.
*/

locate_npu_head([ThisNPU|_MoreNPUs], Type, HeadList) :-
	rev(ThisNPU, RevThisNPU),
	get_right_most_element(RevThisNPU, Type, HeadList).
locate_npu_head([_NoHead|MoreNPUs], Type, HeadList) :-
	locate_npu_head(MoreNPUs, Type, HeadList).

%--- get the rightmost element that can be an argument (mod, head, shapes, not_in_lex)
get_right_most_element([head(HeadList)|_Rest], head, HeadList) :- !.
get_right_most_element([shapes(HeadList)|_Rest], shapes, HeadList) :- !.
get_right_most_element([mod(HeadList)|_Rest], mod, HeadList) :- !.
get_right_most_element([not_in_lex(HeadList)|_Rest], mod, HeadList) :- !.
get_right_most_element([_ThisElement|Rest], Type, HeadList) :-
	get_right_most_element(Rest, Type, HeadList).

% return both the head and the NPU itself
locate_npu_and_head([ThisNPU|MoreNPUs], Type, ThisNPU,HeadList,MoreNPUs) :-
	rev(ThisNPU, RevThisNPU),
	get_right_most_element(RevThisNPU, Type, HeadList).
locate_npu_and_head([_NoHead|MoreNPUs], Type, NPU,HeadList,Rest) :-
	locate_npu_and_head(MoreNPUs, Type, NPU,HeadList,Rest).

locate_npu_from_head([ThisNPU|_MoreNPUs],HeadList,ThisNPU) :-
	locate_npu_from_head_aux(ThisNPU,HeadList),
	!.
locate_npu_from_head([_NoHead|MoreNPUs], HeadList, NPU) :-
	locate_npu_from_head(MoreNPUs, HeadList, NPU).

locate_npu_from_head_aux([NPUElement|_Rest],HeadList) :-
	functor(NPUElement,_Functor,1),
	arg(1,NPUElement,HeadList).
locate_npu_from_head_aux([_NPUElement|Rest],HeadList) :-
	locate_npu_from_head_aux(Rest,HeadList).

%locate_npu_head([ThisNPU|_MoreNPUs], Type, FeatureList) :-
%	get_term_with_metaconc(ThisNPU, Type, FeatureList).

%locate_npu_head([_NoHead|MoreNPUs], Type, HeadList) :-
%	locate_npu_head(MoreNPUs, Type, HeadList).

% We want the first term in the MSU list
% (other than empties) that contains a metaconc
get_term_with_metaconc(ThisMSU, Functor, HeadList) :-
 	rev(ThisMSU, RevThisMSU),
 	member(HeadListTerm, RevThisMSU),
 	functor(HeadListTerm, Functor, 1),
 	Functor \== empty_head,
 	Functor \== empty_mod,
 	arg(1, HeadListTerm, HeadList),
 	memberchk(metaconc(_), HeadList).

% check_relation/3:
% check the local semnet to see if the relation is valid;
				% otherwise check the main semnet
check_relation(SubjectSemType, Relation, ObjectSemType) :-
	( midstring(Relation,'neg_',RelationNegRemoved, 0,4) -> 
	  check_relation_aux(SubjectSemType, RelationNegRemoved, ObjectSemType)
	; check_relation_aux(SubjectSemType,Relation,ObjectSemType)
	).

check_relation_aux(SubjectSemType, Relation, ObjectSemType) :-
%	(local_semnet(SubjectSemType, Relation, ObjectSemType) ->
%	    true
%	; bidirectional_type_relation_type(SubjectSemType, Relation, ObjectSemType)
%        ).
	( local_semnet_BOTH(SubjectSemType, Relation, ObjectSemType) ->
	  true
	; bidirectional_type_relation_type(SubjectSemType, Relation, ObjectSemType)
	).

bidirectional_type_relation_type(SubjectSemType, Relation, ObjectSemType) :-
	global_module_version(Version),
	( type_relation_type(Version,SubjectSemType, Relation, ObjectSemType) ->
	  true
	; relation_inverse(Version,Relation, RelationInverse) ->
	  type_relation_type(Version,ObjectSemType, RelationInverse, SubjectSemType)
	).

get_from_list_if_possible(Type, List, Result) :-
	( get_from_list(Type, List, Result) ->
	  true
	; Result = '<NONE FOUND>'
	).

% If Stream is an open stream, close it.
% Otherwise, do nothing.
% close_open_stream(Stream) :-
% 	( Stream == [] ->
% 	  true
% 	; current_stream(_File, read, Stream) ->
% 	  close(Stream)
% 	; current_stream(_File, write, Stream) ->
% 	  close(Stream)
% 	; true
% 	).

get_sem_info_and_type(HeadList, MetaConcList, CUI, SemTypeList2, SemType) :- 
        get_sem_info(HeadList,  MetaConcList,  CUI, SemTypeList),
        split_semtype_list(SemTypeList, _SemTypeList1, SemTypeList2),
        member(SemType, SemTypeList2).

% This hack handles genphenoms and disorders, whose SemTypeList is of the form
% [gngm]/FullSemTypeList
split_semtype_list(SemTypeList1/SemTypeList2, SemTypeList1, SemTypeList2).
split_semtype_list([H|T], [H|T], [H|T]).


% ---------- GET_SEM_INFO ----------

%  Given a Headlist as the first argument, e.g.,
% 
%  [index(4), usemtype(['Pharmacologic Substance']), ausemtype([phsu]),
%   isemtype([therapy]), lexmatch([analgesic]), inputmatch([analgesic]),
%   tag(adj), tokens([analgesic]), bases([]), metaconc(['Analgesics':[phsu]])]
%
%  return as the second argument a list containing the metaconc itself,
%  e.g., ['Analgesics'],
%  return as the third argument the list containing the associated Semantic Type(s),
%  e.g., [phsu]

%  get_from_list (defined in ssuppserv.pl) called with metaconc as first argument
%  will return the argument of the metaconc term, i.e., ['Analgesics':[phsu]]

get_sem_info(HeadList, [MetaConc], CUI, SemTypeList) :-
	get_from_list(metaconc, HeadList, MetaConcList),
	!,
	get_Meta_info(MetaConcList, MetaConc, CUI, SemTypeList).
get_sem_info(_HeadList, [], [], []).

% ---------- Get Meta info

%  Nondeterministcally return all combinations of
%  the metaconc itself and its associated Semantic Type list
%
%  e.g., given as first argument the list of metaconc colon terms
%  ['Cold Sensation':[qlco,sosy], 'Common Cold':[dsyn], 'cold temperature':[npop]]
%  which is itself the argument of a metaconc/1 term,
%  nondeterministically return in the second and third arguments
%
%  'Cold Sensation'    and  [qlco,sosy],
%  'Common Cold'       and  [dsyn],       and
%  'cold temperature'  and  [npop]

get_Meta_info([MetaConc:CUI:SemTypeList|_More], MetaConc, CUI, SemTypeList).
get_Meta_info([_NoMatch|More], MetaConc, CUI, SemType) :-
	get_Meta_info(More, MetaConc, CUI, SemType).

% get_all_semtypes('<NONE FOUND>', '<NONE FOUND>') :- !.
% get_all_semtypes(MetaConcList, AllSemTypes) :-
% 	get_all_semtypes_1(MetaConcList, AllSemTypesList),
% 	append(AllSemTypesList, AllSemTypes).	
% 
% get_all_semtypes_1([], []).
% get_all_semtypes_1([_MetaConc:SemTypeList|Rest], [RealSemTypeList|RestSemTypeLists]) :-
% 	get_print_semtype_list(SemTypeList, RealSemTypeList),	
% 	get_all_semtypes_1(Rest, RestSemTypeLists).

/*
% ---------- POTENTIAL INDICATOR ----------
"ing" appears here as a label, but I don't believe it is used anymore.
*/

potential_indicator(ThisNPU, UsedCuesIn, Indicator) :-
	look_for(head,     ThisNPU, Indicator)
      ; look_for(mod,      ThisNPU, Indicator)
      ; look_for(verb,     ThisNPU, Indicator)
      ; look_for(aux,      ThisNPU, Indicator)
      ; look_for(pastpart, ThisNPU, Indicator)
      ; look_for(empty_head, ThisNPU, Indicator)
      % basically for positively/negatively regulate
      ; look_for(adv,       ThisNPU, Indicator)
      ; look_for(empty_mod, ThisNPU, Indicator)
      ; look_for(prep,     ThisNPU, Indicator),
        \+ prep_has_been_used(Indicator, UsedCuesIn)
      ; look_for(ing,      ThisNPU, Indicator).

%----- look_for

look_for(ing, [ing(IngInfo)|_], ing(IngInfo)) :- !. 
look_for(ing, ThisNPU, ing(IngInfo)) :-
	!,
	rev(ThisNPU, [confid(_),ing(IngInfo)|_]). 
look_for(LexicalCategory, ThisNPU, LexicalCategoryStruct) :-
	get_from_list_nd(LexicalCategory, ThisNPU, LexicalCategoryList),
%	!,
	functor(LexicalCategoryStruct, LexicalCategory, 1),
	arg(1, LexicalCategoryStruct, LexicalCategoryList).

% ---------- Prep Has NOT Been Used ----------

%prep_has_not_been_used(prep(PrepList), UsedCuesIn) :-
%	!,
%	get_from_list(lexmatch, PrepList, LexList),
%	\+ intersect(LexList, UsedCuesIn).
%prep_has_not_been_used(_Prep, _UsedCuesIn).
prep_has_been_used(prep(PrepList), [UsedCueIn|_UsedCueRest]) :-
%        get_from_list(lexmatch, PrepList, LexList),
        ( UsedCueIn = [UsedCue1In-UsedCue2In] ->
	  ( UsedCue1In == PrepList
	  ; UsedCue2In == PrepList
	  )
	; UsedCueIn == PrepList
	),
	!.
prep_has_been_used(Prep, [_UsedCueIn|Rest]) :-
        prep_has_been_used(Prep, Rest).

determine_utterance_types(Utterances, UtteranceTypes) :-
	% assume that the zero-th utterance is of type ''
	determine_utterance_types_1(Utterances, UtteranceTypes, '').

determine_utterance_types_1([], [], _PreviousUtteranceType).
determine_utterance_types_1([utterance(InputLabel,ChosenInputText,_,_)|RestUtterances],
			    [UtteranceType|RestUtteranceTypes],
			    PreviousUtteranceType) :-
	determine_one_utterance_type(InputLabel, ChosenInputText, PreviousUtteranceType, UtteranceType),
	determine_utterance_types_1(RestUtterances, RestUtteranceTypes, UtteranceType).

/*tok(label,"12345678.ab.5","12345678.ab.5",pos(778,839)),tok(sn,[],0,pos(778,839)),tok(uc,"BACKGROUND","background",pos(778,788)),tok(pn,":",":",pos(788,789)), */
/*
UtteranceTypes are Section Headers that begin utterances such as
"RESULTS:", "METHODS:", "CONCLUSIONS:", "BACKGROUND:".

Section headers are always in full caps, and followed by a colon.

The default UtteranceType is ''.

If an utterance begins with a section header, then
that word is the utterance's UtteranceType.

If an utterance does not begin with a section header, then
  (1) If any previous sentence in the abstract did begin with a section header,
      use the UtteranceType of the most recent sentence having a section header.
  (2) Otherwise, use '' as the UtteranceType.

We may impose some distance constraints on how many subsequent sentences
one sentence's section header can include.

*/


determine_one_utterance_type(InputLabel, InputText, PreviousUtteranceType, UtteranceType) :-
	% get first word of utterance
				% InputText = [InputHead|InputTail],
	% Don't use labels in the title, like 'CASE REPORT' as an utterance type
	\+ midstring(InputLabel, 'ti', _Back, _BackLen,2),
	prefix_up_to_colon(InputText, PrefixUpToColon),
	!,
	( convert_abstract_section_header(PrefixUpToColon, TempUtteranceType) ->
	  true
	; TempUtteranceType = ''
	),
	choose_next_utterance_type(PreviousUtteranceType, TempUtteranceType, UtteranceType).

determine_one_utterance_type(_InputLabel,_ChosenInputText, PreviousUtteranceType, PreviousUtteranceType).

% determine_first_word(58, _RestUtterance, [58]) :-
% 	!.
% determine_first_word(FirstChar, [H|T], [FirstChar|RestFirstWord]) :-
% 	is_upper(FirstChar),
% 	!,
% 	determine_first_word(H, T, RestFirstWord).
% determine_first_word(_, _, []).	

prefix_up_to_colon(InputText, PrefixUpToColon) :-
	append(PrefixUpToColon, [58|_], InputText).

convert_abstract_section_header(FirstWordString, FirstWordAtom) :-
	FirstWordString = [H|T],
	remove_final_colon(T, H, FirstWordStringWithoutColon),
	atom_codes(FirstWordAtom, FirstWordStringWithoutColon),
	valid_abstract_section_header(FirstWordAtom, _FrequencyOfOccurrence).

remove_final_colon([], LastChar, WordWithoutColon) :-
	% ASCII for colon ":"
	( LastChar == 58 ->
	  WordWithoutColon = []
	; WordWithoutColon = [LastChar]
	).
remove_final_colon([NextChar|RestChars], CurrentChar, [CurrentChar|RestCharsWithoutColon]) :-
	remove_final_colon(RestChars, NextChar, RestCharsWithoutColon).

choose_next_utterance_type(Previous, Current, Next) :-
	( Previous == '' ->
	  Next = Current
	; Current == '' ->
	  Next = Previous
	; Next = Current
	).

% get_list_indexes(SubjectHeadList, ObjectHeadList,
% 		 SubjectIndex, ObjectIndex,
% 		 LowerIndex, HigherIndex) :-
% 	get_from_list(index, SubjectHeadList, SubjectIndex),
% 	get_from_list(index, ObjectHeadList,  ObjectIndex),
% 	min_max(SubjectIndex, ObjectIndex, LowerIndex, HigherIndex).

min_max(TempSubjectIndex, TempObjectIndex, LowerIndex, HigherIndex) :-
	get_base_index(TempSubjectIndex, SubjectIndex),
	get_base_index(TempObjectIndex,  ObjectIndex),
	( SubjectIndex > ObjectIndex ->
	  HigherIndex is SubjectIndex,
	  LowerIndex is ObjectIndex
	; HigherIndex is ObjectIndex,
	  LowerIndex is SubjectIndex
	).

% strip off the base index from multi-index indexes.
get_base_index(TempIndex, BaseIndex) :-
	( TempIndex = BaseIndex:_Low/_High ->
	  true
	; BaseIndex = TempIndex
	).

/*
% ---------- License for Multiple Use ----------

The first clause accommodates VP coordination with shared subject, as in
" John kissed Mary and hugged Sue".  The decision is made at the time
a subject is being sought for "hugged".
(Shared object with verb coordination hasn't been implemented yet.)

The second clause accommodates relativization; it was designed for 
shared subjects, as in
"The RCA is a large vessel which arises from the aorta"  The decision is
made at the time a subject is being sought for "arises".

"Shared objects not addressed yet". Actually, they have been now.

Either the conjunction or the relativizer has to appear in between the 
current indicator and the potential argument to its left.  This is
accomplished by checking in UpToHead, which is the list of MSU extracted
from LeftDomain by predicate up_to_head.

A noun phrase can be reused if it is the "subject" of a prepositional indicator
as in "X treats lymphedema of the arm":
lymphedema can be the both object of "treats" and subject of "of".

*/

/*

Rule 1 is for sentences like "aspirin for headache and for cancer".
Because
(1) "headache" and "cancer" will be identified as conjuncts
    by build_conjunct_list/2 in interanalysis.pl and
(2) both instances of "for" will produce the two predications
    (a) aspirin-TREATS-headache and
    (b) aspirin-TREATS cancer
we will get four predications: two each of (a) and (b).
We could simply call list_to_set/2 on the predications at the very top level
(and we may still!), but this is an inelegant, non-linguistic solution to the problem.
Here's how we do it: "aspirin" has already been used as the subject of the
predications arising from the first "for", so we need to prevent the reuse of "aspirin"
as the subject of predications arising from the second "for".

InterveningStructure is the list of MSUs between the Subject and the Indicator.
In our example sentence, when the Indicator is the second "for",
  * InterveningStructure is [ [and], [for, headache] ] (don't forget LeftPartition is reversed!)
  * UsedHead is [aspirin(subj)]
  * ConjList is [and(aspirin,cancer)]

We first find a conjunction in InterveningStructure,
and get its lexical item ("and") and index (5).
We then check to see if that conjunction/index pair is in the ConjList.
If so, allow the reuse only if it's a predicate conjunction, and not an arg conjunction.
Of course as of
FML 10/14/2005 Friday @ 10:45:16
Semrep doesn't do predicate conjunction, so we have effectively blocked the reuse
by considering this example to NOT be predicate conjunction, but only argument conjunction.

*/

% Rule 1

license_for_reuse(UsedHead, prep(_), ConjList, _SubordinatorPredicateList,
	         _Definitions, _VarInfoList, InterveningStructure) :-
	get_from_list(role, UsedHead, subj),
	find_the_next(conj, _Conj, none, InterveningStructure, _, ConjunctionTerm, _),
	get_from_list(conj, ConjunctionTerm, ConjunctionData),
	get_from_list(index,      ConjunctionData, Index),
	get_from_list(inputmatch, ConjunctionData, [Conj]),
	memberchk(coord(Conj, pred, Index, _LConjList, _RConj), ConjList),
	find_the_next(prep, _Prep, none, InterveningStructure, _,_, _),
	!.

/*

Rule 2:
A noun phrase can also be re-used in configurations such as
"patients with headache can be treated with aspirin".
The generalization is that if a noun phrase ("patients")
serves as an argument of a prepositional indicator ("with")
immediately to the right of the noun phrase,
the prepositional phrase is very likely to be a reduced relative clause.
The noun phrase in question is the head of that relative clause, and thus can be reused.

Also, if the indicator is a past participle, as in "treatment for X complicated by Y".

Rule 3:
"aspirin for patients with headache"
If the Indicator (i.e., "with") is a prep or a participle and
   the InterveningStructure is []

Interpretation of reduced relative clauses introduced by past participles is still problematic.

*/


% Rule 2
license_for_reuse(_UsedHead, _Indicator, _ConjList, _SubordinatorPredicateList,  
		  _Definitions, _VarInfoList, InterveningStructure) :-
%	get_from_list(role, UsedHead, obj),
	last(InterveningStructure,LastElement),
	get_from_list(prep, LastElement, _Prep),
	get_from_list(head, LastElement, _Head),
	!.	
license_for_reuse(UsedHead, Indicator, _ConjList, SubordinatorPredicateList,
		  _Definitions, _VarInfoList,InterveningStructure) :-
	full_rc_head(UsedHead,Indicator,SubordinatorPredicateList,InterveningStructure),	
	!.
% Rule 3
license_for_reuse(_UsedHead, Indicator, _ConjList,_SubordinatorPredicateList,
		  Definitions, VarInfoList, InterveningStructure) :-
	InterveningStructure == [],
	( Indicator = prep(_) ->
	  true
%	; Indicator = pastpart(_) ->
%	  true
	% in case the above statement fails.
	;  check_for_past_participle(Indicator, VarInfoList)
	; Indicator = verb(VerbList),
	  get_from_list(inputmatch, VerbList, InputMatchList),
  	  get_from_list(bases, VerbList, BasesList),
	  check_for_present_participle(InputMatchList, BasesList, Definitions, VarInfoList)
	).

% Ensure that the UsedHead considered as an argument here is was the head of 
% a RC (left-most argument), this will allow reuse
full_rc_head(UsedHead,Indicator,[Subordinator-Predicate|_RestSPList],InterveningStructure) :-
	Subordinator = [pron(_)],
	get_from_list(indicator,UsedHead,Indicator0),
	% RC is associated with an indicator already seen
	( memberchk(Indicator0,Predicate)
        % RC is associated with the current indicator
        ; memberchk(Indicator,Predicate)
        ),
%	get_relativizer(InterveningStructure,_Subordinator),
	check_for_relativizer(InterveningStructure),
	!.
full_rc_head(UsedHead,Indicator,[_Subordinator-_Predicate|RestSPList],InterveningStructure) :-
	full_rc_head(UsedHead,Indicator,RestSPList,InterveningStructure).


check_for_present_participle(InputMatchList, BasesList, _Definitions, VarInfoList) :-
	member(Word, InputMatchList),
	atom_codes(Word, WordChars),
	% ensure inputmatch word ends in "ing"
	append(_Prefix, "ing", WordChars),
	% One of the following must be true
	% (1) Word is not in BasesList
	( \+ memberchk(Word, BasesList) ->
	  true
	% (2) VarInfoList looks like the first example below, i.e.,
	% suggesting:[verb:[ing],inputmatch:[suggesting]],
	; member(Word:[verb:[ing]|_], VarInfoList) ->
	  true
	% (3) VarInfoList looks like the second example below, i.e.,
	% taking:[noun:[base],inputmatch:[taking]],
	; member(Word:[noun:[base]|_], VarInfoList) ->
	  true
	).

/*
  Examples of how "-ing" words show up in Definitions and VarInfoList structures:

  verb([
        index(2)
        usemtype(['Finding'])
        ausemtype([fndg])
        semgroup([diso])
        lexmatch([suggesting])
        inputmatch([suggesting])
        tag(verb)
        tokens([suggesting])
        bases([suggest])
        metaconc(['Suggestible':[fndg]])
      ])


VarInfoList:

suggesting:[verb:[ing],inputmatch:[suggesting]],

Definitions:

lexicon:[lexmatch:[suggesting],
         inputmatch:[suggesting],
	 records:[lexrec:[base:[suggest],
	          spelling_variants:[],
		  entries:[entry:[num:['E0058858'],
		                  cat:[verb],
				  variants:[reg],
				  complements:[tran:[compl:[fincomp:[s]],
				  	             part:[],
						     mvmt:[]],
					       tran:[compl:[ingcomp:[arbc]],
					             part:[],
						     mvmt:[]],
					       tran:[compl:[np],
					             part:[],
						     mvmt:[]],
					       tran:[compl:[whfincomp],
					             part:[],
						     mvmt:[]],
					       tran:[compl:[whinfcomp:[arbc]],
					             part:[],
						     mvmt:[]]],
				  nominalizations:['suggestion|noun|E0058860'],
				  misc:[]]],
		    annotations:[],
		    signature:[]]]],


patients taking aspirin.


  verb([
        index(2)
        lexmatch([taking])
        inputmatch([taking])
        tag(verb)
        tokens([taking])
        bases([taking])
      ])
 
VarInfoList:

taking:[noun:[base],inputmatch:[taking]],

Definitions:

lexicon:[lexmatch:[taking],
         inputmatch:[taking],
	 records:[lexrec:[base:[taking],
	                  spelling_variants:[],
			  entries:[entry:[num:['E0348240'],
			                  cat:[noun],
					  variants:[reg,uncount],
					  complements:[],
					  nominalizations_of:['take|verb|E0059816'],
					  proper:[],
					  misc:[]]],
			    annotations:[],
			    signature:[]]]],
*/


split_input_label(InputLabel, PMID, TiOrAb, SentenceID) :-
	atom_codes(InputLabel, InputLabelString),
	split_string_completely(InputLabelString,".",Elements),
	% It used to simply assume a PMID format, but as I found out,
	% users do not necessarily use MEDLINE and may use . in their IDs
	% which caused issues in writing the results in full fielded output format
	% This part of the code assumes that last two periods are the delimiters.
	length(Elements,Len),
	( Len = 3 ->
	  Elements = [PMIDString,TiOrAbString,SentenceIDString]
	; Len > 3,
	  rev(Elements,ElementsRev),
	  ElementsRev = [SentenceIDString,TiOrAbString|PMIDElementsRev],
	  rev(PMIDElements,PMIDElementsRev),
	  form_one_string(PMIDElements,".",PMIDString)
	),
	strings_to_atoms([PMIDString, TiOrAbString, SentenceIDString],
			 [PMID, TiOrAb, SentenceID]),
	!.
			

/*
Convert a term of the form a-b-c-d,
i.e., -(-(-(-(-(-(a,b),c),d),e),f),g)
in standard syntax, into a list [a,b,c,d].

This is more tricky than it seems because the '-' operator is yfx, that is,
left associative. For a right associative (i.e., xfy) operator, the code would be trivial:
% convert_to_list(A:B, [A|Rest]) :-
% 	!,
% 	convert_to_list(B, Rest).
% convert_to_list(A, [A]).
*/

% already a list
convert_dash_structure_to_list([H|T], [H|T]) :- !.

convert_dash_structure_to_list(A-B, List) :-
	convert_dash_structure_to_list_1(A, [B], List).

convert_dash_structure_to_list_1(A, Rest, [A|Rest]) :-
	var(A),
	!.
convert_dash_structure_to_list_1(A-B, Tail, List) :-
	!,
	convert_dash_structure_to_list_1(A, [B|Tail], List).
convert_dash_structure_to_list_1(A, Rest, [A|Rest]).

remove_variables(List, ListWithNoVariables, Length) :-
	remove_variables_1(List, ListWithNoVariables, 0, Length).

remove_variables_1([], [], Length, Length).
remove_variables_1([H|T], Rest, LengthSoFar, Length) :-
	var(H),
	!,
	remove_variables_1(T, Rest, LengthSoFar, Length).
remove_variables_1([H|T], [H|Rest], LengthSoFar, Length) :-
	Next is LengthSoFar + 1,
	remove_variables_1(T, Rest, Next, Length).

% For each element in each MSU in the analysis,
% remove that element's metaconc (and other referential analysis features)  if
% (1) the argument of its inputmatch(_) term is a one-element list, and 
% (not sure what this means - removed it -- Halil)
% (2) that one element is a one- or two-character atom.
% (3) or it's a three-character atom and the last char is a hyphen. (re-, up-, etc.)
% (4) the element is a verb.
% (5) the element is part of one of Graciela's phrases ('on the other hand' etc.)
remove_referential_analysis_if_necessary(minimal_syntax(AnalysisWithMetaConcs), 
	                                 minimal_syntax(AnalysisWithMetaConcsOut)) :-
	remove_referential_analysis_if_necessary_aux(AnalysisWithMetaConcs, 
	                                             AnalysisWithMetaConcsOut0),
        remove_entire_referential_analysis_if_necessary_aux(AnalysisWithMetaConcsOut0,
	                                                    AnalysisWithMetaConcsOut).

% This predicate looks at intra-MSUelement issues ((2), (3), (4)).
remove_referential_analysis_if_necessary_aux([],[]):- !.
remove_referential_analysis_if_necessary_aux([ThisMSU|RestMSUs],
	                         [ModifiedThisMSU|ModifiedRestMSUs]) :-
	remove_referential_analysis_from_MSU_elements(ThisMSU, ModifiedThisMSU),
	remove_referential_analysis_if_necessary_aux(RestMSUs, ModifiedRestMSUs).

remove_referential_analysis_from_MSU_elements([],[]).
remove_referential_analysis_from_MSU_elements([ThisMSUElement|RestMSUElements],
			                      [ModifiedThisMSUElement|ModifiedRestMSUElements]) :-
	remove_referential_analysis_from_MSU_element(ThisMSUElement, ModifiedThisMSUElement),
	remove_referential_analysis_from_MSU_elements(RestMSUElements, ModifiedRestMSUElements).

remove_referential_analysis_from_MSU_element(verb(FeatureList), verb(FeatureListOut)) :-
	get_from_list(metaconc, FeatureList,_Metaconc),
	!,
	remove_referential_features(FeatureList, FeatureListOut).
remove_referential_analysis_from_MSU_element(ThisMSUElement, ModifiedThisMSUElement) :-
	functor(ThisMSUElement, Functor, 1),
	arg(1, ThisMSUElement, ArgList),
	get_from_list(metaconc, ArgList, _Metaconc),
	get_from_list(inputmatch, ArgList, InputMatchList),
	concat_atom(InputMatchList, InputMatchAtom),
	atom_codes(InputMatchAtom, InputMatchString),
	length(InputMatchString, InputMatchStringLength),
	( InputMatchStringLength =< 2
        ; InputMatchStringLength == 3,
	  substring(InputMatchAtom,'-',2,1)
        ),
	!,
	remove_referential_features(ArgList, ModifiedArgList),
	functor(ModifiedThisMSUElement, Functor, 1),
	arg(1, ModifiedThisMSUElement, ModifiedArgList).
remove_referential_analysis_from_MSU_element(ThisMSUElement, ThisMSUElement).

% This predicate looks at the sentence-level. ((5))
% This is because some of the phrases span multiple MSU elements, even multiple MSUs.
remove_entire_referential_analysis_if_necessary_aux(AnalysisWithMetaConcs,
	                                            AnalysisWithMetaConcsOut) :-
	get_all_bases_and_indexes(AnalysisWithMetaConcs, BaseIndexList, []),
	append(BaseIndexList, BaseIndexes),
	non_compositional_phrase_list(NonCompList),
	empty_macro_np_head_list(EHList),
	append(NonCompList,EHList,PhraseList),
	match_phrases(PhraseList, BaseIndexes, IndexesList),
	append(IndexesList, IndexList),
	keysort(IndexList, SortedIndexList),
	remove_referential_features_with_index(AnalysisWithMetaConcs, SortedIndexList, 
	                                       AnalysisWithMetaConcsOut).


% Create a list of base-index pairs for the entire utterance.
get_all_bases_and_indexes([], BaseIndexList, BaseIndexList).
get_all_bases_and_indexes([ThisMSU|RestMSUs], BaseIndexListOut, BaseIndexListIn) :-
	get_bases_and_indexes(ThisMSU, BaseIndexList),
	BaseIndexListOut = [BaseIndexList|BaseIndexListNext],
	get_all_bases_and_indexes(RestMSUs, BaseIndexListNext, BaseIndexListIn).

get_bases_and_indexes([],[]) :- !.
get_bases_and_indexes([MSUElement|RestMSUElements], [Base-Index|RestBaseIndexes]) :-
	arg(1, MSUElement, ArgList),
	get_from_list(bases, ArgList, BaseList),
	concat_atom(BaseList, Base),
	get_from_list(index, ArgList, Index),
	!,
	get_bases_and_indexes(RestMSUElements, RestBaseIndexes).
get_bases_and_indexes([_MSUElement|RestMSUElements], RestBaseIndexes) :-
	get_bases_and_indexes(RestMSUElements, RestBaseIndexes).


% match Graciela's phrases. 
match_phrases([], _BaseIndexes, []).
match_phrases([Phrase|RestPhrases], BaseIndexes, Indexes) :-
	get_sublist_indexes(Phrase, BaseIndexes, IndexList),
	( IndexList == [] ->
	  Indexes = RestIndexes
	; Indexes = [IndexList|RestIndexes]
	),
	match_phrases(RestPhrases, BaseIndexes, RestIndexes).

% For each phrase, find the indexes for each occurrence in the utterance.
get_sublist_indexes(_Phrase,[],[]) :- !.
get_sublist_indexes(Phrase,[Base-Index|RestBaseIndexes],
	            [Index-LastIndex|RestIndexList] ) :-
	Phrase = [Base|RestPhrase],
	length(RestPhrase, Len),
	match_rest(RestPhrase, RestBaseIndexes, Index, LastIndex),
	LastIndex =:= Index + Len,
	!,
	get_sublist_indexes(Phrase, RestBaseIndexes, RestIndexList).
get_sublist_indexes(Phrase, [_BaseIndex|RestBaseIndexes], IndexList) :-
	get_sublist_indexes(Phrase, RestBaseIndexes, IndexList).

match_rest([],_BaseIndexes, Index, Index).	
match_rest([A|RestA],[A-Index|RestB], IndexIn, IndexOut) :-
	Index =:= IndexIn + 1,
	match_rest(RestA, RestB, Index, IndexOut).

% An adhoc fix for period issues in abbreviations. --Halil
% This came up in the context of "vs.".
% Phil was using regexp and transforming inputmatch fields
% which was not correct.
% For instance, in "aspirin vs. tylenol", the tokenization output becomes
% [aspirin, vs], [.], [tylenol]. We want [aspirin, vs, .], [tylenol], so that
% vs. can be recognized as a unit.
% May need more work, new abbreviations etc.
% retokenize_for_period([], []).
% retokenize_for_period([ThisList, [.]|MoreLists], [NewList|Gap]) :-
% 	MoreLists \== [],
% 	( last(ThisList,Last),
% 	  abbrv(Last)
% 	; concat_atom(ThisList, ThisAtom),
% 	  abbrv(ThisAtom)
% 	),
% 	append(ThisList, [.], NewList),
%         retokenize_for_period(MoreLists, Gap).
% retokenize_for_period([ThisList|MoreLists],[ThisList|Gap]) :-
% 	retokenize_for_period(MoreLists, Gap).

% dr, mr, mrs, ms rules don't seem to apply at the moment, 
% as they are recognized as sentence boundaries.
% abbrv(vs).
% abbrv(dr).
% abbrv(mr).
% abbrv(ms).
% abbrv(mrs).
% abbrv(al).
% abbrv('e.g').
% abbrv('i.e').
	

assemble_format_control_string(Length, String) :-
	( Length =:= 1 ->
	  String = "~w~n"
        ; Next is Length - 1,
          String = [126,119,124|Rest], % "~w|"
	  assemble_format_control_string(Next, Rest)
	).

% write_list_with_format(List, Format) :-
% 	write_list_with_format_to_stream(List, user_output, Format).
% 
% write_list_with_format_to_stream([], _Stream, _Format).
% write_list_with_format_to_stream([H|T], Stream, Format) :-
% 	format(Stream, Format, H),
% 	write_list_with_format_to_stream(T, Stream, Format).

strings_to_atoms([], []).
strings_to_atoms([String|More], [Atom|Gap]) :-
	atom_codes(Atom, String),
	strings_to_atoms(More, Gap).

% This functionality allows the developer to override the input filename
% without having to recompile the entire system from scratch.
override_file_args([], []).
override_file_args([OrigInputFile|Rest], [NewInputFile|Rest]) :-
	( override_file(infile, read, NewInputFile) ->
	  true
	; NewInputFile = OrigInputFile
	).

% This functionality allows the developer to override the control_option/1 values
% without having to recompile the entire system from scratch.
override_control_options :-
	nl,
	override_control_option(retract, ControlOption),
	retract(control_option(ControlOption)),
	format('~NRETRACTED override control option ~w~n', [ControlOption]),
	fail
      ; override_control_option(assert, ControlOption),
	\+ control_option(ControlOption),
	assert(control_option(ControlOption)),
	format('~NASSERTED override control option ~w~n', [ControlOption]),
	fail
      ; nl.


% add role(subj) or role(obj), as appropriate, to each RightConjunct
% and to each HeadList in [RightConjunct|LeftConjList]
% in order to mark them as used heads
add_roles_to_conjuncts(Indicator,[],RightConjunct, Role, SurfaceRole,
		       [[role(Role),srole(SurfaceRole),indicator(Indicator)|RightConjunct]|Gap], 
                       Gap).
add_roles_to_conjuncts(Indicator,[H|T], RightConjunct, Role,SurfaceRole,
		       [[role(Role),srole(SurfaceRole),indicator(Indicator)|H]|RestConjunctsWithRoles], 
		       Gap) :-
	 add_roles_to_conjuncts(Indicator,T, RightConjunct, Role, SurfaceRole,RestConjunctsWithRoles, Gap).

% find the surface role of an argument based on Indicator and Argument positions
surface_role(Indicator,ArgumentList,SurfaceRole) :-
	arg(1,Indicator,IndicatorList),
	get_from_list(index,IndicatorList,IndIndex),
	get_from_list(index,ArgumentList,ArgIndex),
	clean_index(ArgIndex,ArgIndex0),
	clean_index(IndIndex,IndIndex0),
	\+ IndIndex0 == ArgIndex0,
	( IndIndex0 > ArgIndex0 ->
	  SurfaceRole = subj
        ; SurfaceRole = obj
        ),
	!.

% ---------- GET_CONJUNCT_DATA ----------

% Return in CoordConcs a list of elements of the form Metaconc-SemType,
% one for each of the conjuncts of HeadList in [RConj|LConjList]
get_conjunct_data(HeadList, HeadSemTypeList, Relation,
		  HeadAtom, OtherSemTypeList,
		  Conj, LConjList, RConj, CoordConcs) :-
	% Conjuncts is all conjoined MSUs (whether left or right) other than HeadList
	select(HeadList, [RConj|LConjList], Conjuncts),
	!, % This (green) cut is necessary to prevent useless backtracking
	get_conjunct_data_1(Conj, RConj, Conjuncts, HeadSemTypeList, Relation, HeadAtom,
			    OtherSemTypeList, CoordConcs).

% ----- get_conjunct_data_1
% was modified by MF to check that semantic type St is part of SemTypes.
% The arity in this predicate is three because we send the St that matches.
% A typical problem in the predicate bellow would be in
% "haloperidol and clonidine for depression" where haloperidol is output as
% ASSAY of haloperidol (lbpr). In the new version of UMLS 03  this 

% The test above is no longer done.

% OriginalSemType is the semtype of the Head appearing in the predication in inter_npu_relations.
% We shold use that original semtype if the coordinated metaconcs have it;
% otherwise, just arbitrarily grab the first semtype of the coordinated metaconcs.

get_conjunct_data_1(_Conj, _RConj, [], _OriginalSemTypeList, _Relation, _HeadAtom, _OtherSemTypeList, []).
get_conjunct_data_1(Conj, RConj, [Conjunct|More], OriginalSemTypeList, Relation, HeadAtom,
		    OtherSemTypeList,
		    [ConjunctMetaConc-ConjunctCUI-ConjunctSemTypeList-ConjunctSemType-ConjunctIndex-Neg|MorePairs]) :-
	get_one_conjunct_data(Conj, RConj, Conjunct, OriginalSemTypeList, Relation, HeadAtom,
			      OtherSemTypeList,
			      ConjunctMetaConc, ConjunctCUI, ConjunctSemTypeList, ConjunctSemType,
			      ConjunctIndex,Neg),
	!,
	get_conjunct_data_1(Conj, RConj, More, OriginalSemTypeList,
			    Relation, HeadAtom, OtherSemTypeList, MorePairs).
% This clause is for no_lconj_found and other such oddities.
get_conjunct_data_1(Conj, RConj, [_Conjunct|More], OriginalSemTypeList,
		    Relation, HeadAtom, OtherSemTypeList, Pairs) :-
	get_conjunct_data_1(Conj, RConj, More, OriginalSemTypeList,
			    Relation, HeadAtom, OtherSemTypeList, Pairs).

% In determining which semtype to use for conjuncts,
% (1) use the same semtype as that of the original noun in the predication, if available.
% (2) use the first semtype that passes check_relation
% (3) use the first semtype--period.

get_one_conjunct_data('but not', RConj, Conjunct, OriginalSemTypeList,
		      Relation, HeadAtom,
		      OtherSemTypeList, ConjunctMetaConc, ConjunctCUI,
		      ConjunctSemTypeList, ConjunctSemType, ConjunctIndex, Neg) :-
	get_sem_info(Conjunct, [ConjunctMetaConc], ConjunctCUI, ConjunctSemTypeList),
	get_from_list(index, Conjunct, ConjunctIndex),
	get_from_list(index, RConj, RConjIndex),
	( RConjIndex == ConjunctIndex ->
	  Neg = 1
	; Neg = 0
	),
	( intersection(OriginalSemTypeList, ConjunctSemTypeList, [OriginalSemType|_]) ->
	  ConjunctSemType = OriginalSemType
	; member(ConjunctSemType, ConjunctSemTypeList),
	  determine_direction_and_check_relation(HeadAtom, ConjunctSemType,
						 Relation, OtherSemTypeList, _OtherSemType)
	).
get_one_conjunct_data(_Conj, _RConj, Conjunct, OriginalSemTypeList,
		      Relation, HeadAtom,
		      OtherSemTypeList, ConjunctMetaConc, ConjunctCUI,
		      ConjunctSemTypeList, ConjunctSemType, ConjunctIndex, Negated) :-
	get_sem_info(Conjunct, [ConjunctMetaConc], ConjunctCUI, ConjunctSemTypeList),
	get_from_list(index, Conjunct, ConjunctIndex),
	( get_from_list(negated,Conjunct,Negated)
	; Negated = 0
	),
	( intersection(OriginalSemTypeList, ConjunctSemTypeList, [OriginalSemType|_]) ->
	  ConjunctSemType = OriginalSemType
	; member(ConjunctSemType, ConjunctSemTypeList),
	  determine_direction_and_check_relation(HeadAtom, ConjunctSemType,
						 Relation, OtherSemTypeList, _OtherSemType)
	).

determine_direction_and_check_relation(subj, SubjectSemType, Relation, ObjectSemTypeList, ObjectSemType) :-
	member(ObjectSemType, ObjectSemTypeList),
	check_relation(SubjectSemType, Relation, ObjectSemType),
	!.
determine_direction_and_check_relation(obj, ObjectSemType, Relation, SubjectSemTypeList, SubjectSemType) :-
	member(SubjectSemType, SubjectSemTypeList),
	check_relation(SubjectSemType, Relation, ObjectSemType).

processing_mode(ProcessingMode) :-
	( control_option(genetics_processing) ->
	  ProcessingMode = genetics
	; ProcessingMode = generic
	),
	announce_processing_mode(ProcessingMode).

announce_processing_mode(ProcessingMode) :-
	format(user_output,'~nProcessing mode is ~a.~n~n', [ProcessingMode]).

update_entity_fields(IndexCharPositionList, GenPhenoms,
	             CitationTextAtom, UNExpandedInputText,
	             CUI, MetaConc, Index, SemTypeList,
	             RealCUI, RealMetaConc, GeneID, GeneName, 
		     EntityAtom, StartPos, EndPos):-
	get_matching_genphenom(GenPhenoms, Index, SemTypeList, GeneID, GeneName),
	modify_metaconc_if_necessary(CUI, MetaConc, GeneID, GeneName, RealCUI, RealMetaConc),	
	get_index_start_and_end_pos(Index, IndexCharPositionList, StartPos,EndPos),
	get_entity_atom(Index, IndexCharPositionList, 
                        CitationTextAtom, UNExpandedInputText, EntityAtom),
	!.

% first clause : ctrial concept
% second clause: entrezgene concept with no corresponding metaconc.
% could there be cases where a metaconc exists, however the metaconc actually 
% refers to an entrezgene concept? Then another clause is necessary
modify_metaconc_if_necessary('',MetaConc,'','','',MetaConc) :- !.
modify_metaconc_if_necessary(CUI,_MetaConc,_GeneID,_GeneName,'','') :-
	% midstring(CUI,Char,_Front,0,1),
	atom_codes(CUI,[Char|_]),
	% 10/28/2015: FML relaxed criterion because of e-mail exchange with
	% Swaroop Gantela (Swaroop.Gantela@uth.tmc.edu), whose custom data
	% had CUI-like unique identifiers beginning with 'P'.
	\+ local_upper(Char),
	% C for MT concepts, D for disaster management concepts, G for cardiac domain
	% \+ member(Char,['C','D','G']),
	!.
modify_metaconc_if_necessary(CUI,MetaConc,_GeneID,_GeneName,RealCUI,RealMetaConc) :-
	!,
	atomize(MetaConc,RealMetaConc),
	atomize(CUI, RealCUI).
	
% Ensure that we're looking at a genetic concept.
get_matching_genphenom(GenPhenoms, Index, SemTypeList,
	                GeneID, GeneName) :-
	intersect([aapp,gngm],SemTypeList),
%	!,
	match_genphenom(GenPhenoms, Index, GeneID, GeneName).
get_matching_genphenom(_GenPhenoms,_Index,_SemTypeList,'','').
	
match_genphenom([GeneID:GeneName:_GeneSemTypeList-Index|_Rest], 
	        Index, GeneID, GeneName) :-
	GeneID \== '',
	!.
match_genphenom([_GenPhenom|Rest], Index, GeneID, GeneName) :-
	match_genphenom(Rest, Index, GeneID, GeneName).

get_entity_atom(Index, IndexCharPositionList, 
	        CitationTextAtom, _UNExpandedInputText, EntityAtom) :-
	memberchk(Index-_InputMatchList-StartPos0-EndPos0, IndexCharPositionList),
	!,
	fix_indexes(Index, StartPos0, EndPos0, IndexCharPositionList, StartPos, EndPos),
%	EntityLen is EndPos - StartPos + 1,
	EntityLen is EndPos - StartPos,
%	LenA is StartPos - 1,
%	atom_codes(CitationTextAtom1, UNExpandedInputText),
%	( substring(CitationTextAtom, EntityAtom, StartPos, EntityLen)
	( sub_atom(CitationTextAtom,StartPos,EntityLen,_After,EntityAtom)
	; % Sometimes token list positions are wrong, this is just so that the entity is printed.
	  ( Index = Index0:_Index1/Index0
	  ; Index = Index0
	  ),	    
	  format('~n### WARNING in get_entity_atom: unexpected offset for item #~d',[Index0]),
	  EntityAtom = ''
	).
	
% for indicator indexes
get_index_start_and_end_pos(_OldIndex:LowerIndex/HigherIndex,
			    IndexCharPositionList,
			    StartPos, EndPos) :-
	!,
	get_index_start_and_end_pos(LowerIndex/HigherIndex,
				    IndexCharPositionList,
				    StartPos, EndPos).
get_index_start_and_end_pos(LowerIndex:HigherIndex, IndexCharPositionList,
	                    StartPos, EndPos) :-
	!,
	get_index_start_and_end_pos(LowerIndex/HigherIndex, IndexCharPositionList,
	                            StartPos, EndPos).
get_index_start_and_end_pos(LowerIndex/HigherIndex, IndexCharPositionList, StartPos, EndPos) :-
	!,
	get_one_index_start_and_end_pos(LowerIndex, IndexCharPositionList,
					StartPos, _LowerEndPos),
	get_one_index_start_and_end_pos(HigherIndex, IndexCharPositionList,
					_HigherStartPos, EndPos).
get_index_start_and_end_pos(Index, IndexCharPositionList, StartPos, EndPos) :-
	get_one_index_start_and_end_pos(Index, IndexCharPositionList, StartPos, EndPos).


get_one_index_start_and_end_pos(Index, IndexCharPositionList, StartPos, EndPos) :-
	memberchk(Index-_InputMatchList-StartPos-EndPos, IndexCharPositionList),
	!.
get_one_index_start_and_end_pos(Index, IndexCharPositionList, StartPos, EndPos) :-
	memberchk((_:Index/_H)-_InputMatchList-StartPos-EndPos, IndexCharPositionList),
	!.
get_one_index_start_and_end_pos(Index, IndexCharPositionList, StartPos, EndPos) :-
	memberchk((_:_/Index)-_InputMatchList-StartPos-EndPos, IndexCharPositionList),
	!.
% added for mmo problems
get_one_index_start_and_end_pos(_Index, _IndexCharPositionList, 0, 0).

% Just to ensure that start pos is less than the end pos
% Actually needs to done in IndexCharPositionList
% This is a temporary fix to prevent SemRep fail.
fix_indexes(Index, StartPos, EndPos, IndexCharPositionList, StartPosOut, EndPosOut) :-
	!,
	( StartPos > EndPos ->
%	  format('~nWARNING: Problem with word indexes.', []),
	  nextto(_PrevIndex-_PrevMatchList-_PrevStartPos-PrevEndPos, 
	         Index-_InputMatchList-StartPos-EndPos, IndexCharPositionList),	  
          NewStartPos is PrevEndPos + 1,
	  ( EndPos > NewStartPos -> 
	    StartPosOut is NewStartPos,
	    EndPosOut is EndPos 
          ; nextto(Index-_InputMatchList-StartPos-EndPos, 
	           _NextIndex-_NextMatchList-NextStartPos-_NextEndPos, IndexCharPositionList),
            NewEndPos is NextStartPos - 1,
	    NewEndPos > StartPos,
	    EndPosOut is NewEndPos,
	    StartPosOut is StartPos
	  )  
        ; StartPosOut is StartPos,
	  EndPosOut is EndPos
        ).

	
% set_genphenom_or_disorder_fields(NormGeneID-NormGeneName-GeneName-GenPhenomAtom-
% 				 GeneFunctionList-GenPhenomMetaConc,
% 
% 				 NormGeneID, NormGeneName, GeneName,
% 				 GenPhenomAtom, CUI, RealMetaConc,
% 				 ConvertedGeneFunctionList) :-
% 	!,
% 	get_augmented_concept_cui(GenPhenomMetaConc, CUI),
% 	atomize(GenPhenomMetaConc, RealMetaConc),
% 	convert_genphenom_function_list(GeneFunctionList, ConvertedGeneFunctionList).
% set_genphenom_or_disorder_fields(DisorderAtom-DisorderMetaConc,
% 
% 				 NormGeneID, NormGeneName, GeneName,
% 				 DisorderAtom, DisorderCUI, RealMetaConc,
% 				 ConvertedGeneFunctionListAtom) :-
% 	!,
% 	get_augmented_concept_cui(DisorderMetaConc, DisorderCUI),
% 	atomize(DisorderMetaConc, RealMetaConc),
% 	NormGeneID = '',
% 	NormGeneName = '',
% 	GeneName = '',
% 	ConvertedGeneFunctionListAtom = ''.
% set_genphenom_or_disorder_fields(MetaConc,
% 
% 				 NormGeneID, NormGeneName, GeneName,
% 				 DisorderAtom, CUI, MetaConc,
% 				 ConvertedGeneFunctionListAtom) :-
% 	!,
% 	get_augmented_concept_cui(MetaConc, CUI),
% 	DisorderAtom = '',
% 	NormGeneID = '',
% 	NormGeneName = '',
% 	GeneName = '',
% 	ConvertedGeneFunctionListAtom = ''.

get_all_concept_cuis(MetaConc, CUI) :-
	( MetaConc == '' ->
	  CUI = ''
	; is_list(MetaConc) ->
	  get_concept_cui_list(MetaConc, TempCUI)
	; db_get_concept_cui(MetaConc, TempCUI)
	),
	( TempCUI == 'C0000000' ->
	  CUI = ''
	; CUI = TempCUI
	).
	 

get_concept_cui_list(MetaConcList, CUI) :-
	get_concept_cui_list_1(MetaConcList, CuiList),
	concat_atom(CuiList, ',', CUI).

get_concept_cui_list_1([], []).
get_concept_cui_list_1([FirstMetaConc|RestMetaConcs], [FirstCUI|RestCUIs]) :-
	db_get_concept_cui(FirstMetaConc, FirstCUI),
	get_concept_cui_list_1(RestMetaConcs, RestCUIs).

% get_augmented_concept_cui(_SemGenConcept-RealConcept, RealConceptCUI) :-
% 	!,
% 	get_all_concept_cuis(RealConcept, RealConceptCUI).
% get_augmented_concept_cui(Concept, ConceptCUI) :-
% 	get_all_concept_cuis(Concept, ConceptCUI).

atomize(ListOrAtom, Atom) :-
	( atomic(ListOrAtom) ->
	  Atom = ListOrAtom
	; concat_atom(ListOrAtom, ',', Atom)
	).

/*

convert GeneFunctionListAtom by
 * changing all '|' to ';'
 * replacing any ';,' with ';'
 * deleting last character

This is necessary because all the '|' in the SemGen GeneFunctionString
will massively confuse the Dimitar output for SemGen.


E.g., this string

"MF:metal ion binding^transcription factor activity|BP:generation of precursor metabolites and energy^regulation of transcription, DNA-dependent^transcription|CC:nucleus|,|||,MF:3-alpha-hydroxysteroid dehydrogenase (B-specific) activity^aldo-keto reductase activity^bile acid transporter activity^chlordecone reductase activity^electron transporter activity^oxidoreductase activity|BP:androgen metabolism^bile acid transport|CC:cytoplasm|"

will become

"MF:metal ion binding^transcription factor activity;BP:generation of precursor metabolites and energy^regulation of transcription, DNA-dependent^transcription;CC:nucleus;;;;MF:3-alpha-hydroxysteroid dehydrogenase (B-specific) activity^aldo-keto reductase activity^bile acid transporter activity^chlordecone reductase activity^electron transporter activity^oxidoreductase activity;BP:androgen metabolism^bile acid transport;CC:cytoplasm"

*/

% convert_genphenom_function_list(GenPhenomFunctionList, ConvertedGenPhenomFunctionList) :-
% 	( GenPhenomFunctionList == '|||' ->
% 	  ConvertedGenPhenomFunctionList = ''
% 	; atom_codes(GenPhenomFunctionList, GenPhenomFunctionListString),
% 	  convert_genphenom_function_list_2(GenPhenomFunctionListString,
% 					    ConvertedGenPhenomFunctionListString),
% 	  atom_codes(ConvertedGenPhenomFunctionList, ConvertedGenPhenomFunctionListString)
% 	).
% 
% convert_genphenom_function_list_2([], []).
% convert_genphenom_function_list_2([H|T], Converted) :-
% 	convert_genphenom_function_list_3(T, H, Converted).
% 
% convert_genphenom_function_list_3([], _Last, []).
% convert_genphenom_function_list_3([NextChar|RestChars],
% 				  ThisChar,
% 				  [ConvertedThisChar|ConvertedRestChars]) :-
% 	convert_vertical_bar_to_semicolon(ThisChar, ConvertedThisChar),
% 	skip_following_comma(ConvertedThisChar, NextChar, RestChars,
% 			     RealNextChar, NewRestChars),
% 	convert_genphenom_function_list_3(NewRestChars, RealNextChar, ConvertedRestChars).

% skip_following_comma(ThisChar, OrigNextChar, RestChars, RealNextChar, NewRestChars) :-
% 	( ThisChar =:= 59,        % ';'
% 	  OrigNextChar =:= 44 ->  % ','
% 	  RestChars = [RealNextChar|NewRestChars]
% 	; NewRestChars = RestChars,
% 	  RealNextChar = OrigNextChar
% 	).

% convert_vertical_bar_to_semicolon(ThisChar, ConvertedThisChar) :-
% 	( ThisChar =:= 124 ->       % '|'
% 	  ConvertedThisChar is 59   % ';'
% 	; ConvertedThisChar is ThisChar
% 	).

% get_print_semtype_list(_GenDisSemTypeList/RealSemTypeList, RealSemTypeList).
% get_print_semtype_list([H|T], [H|T]).

announce_check_relation_1(DebugBitVector,
			  SubjectSemType, Relation, ObjectSemType) :-
	announce_check_relation(DebugBitVector,
				[], SubjectSemType, [], Relation, [], ObjectSemType).

announce_check_relation(DebugBitVector, SubjectInputMatch, SubjectSemType,
			IndicatorInputMatch, Relation,
			ObjectInputMatch, ObjectSemType) :-
	% hack to keep singleton variable checker quiet
	% when debugging code below is commented out
	SubjectInputMatch \== 'BOGUS',
	IndicatorInputMatch \== 'BOGUS',
	ObjectInputMatch \== 'BOGUS',
	% hack to keep singleton variable checker quiet
	% when debugging code below is commented out
	FormatList = [SubjectSemType, SubjectInputMatch,
		      Relation, IndicatorInputMatch,
		      ObjectSemType, ObjectInputMatch],
	FormatList \== 'BOGUS',
	debug_call(DebugBitVector, 2,
		     format('~nCHECKING TRT: ~w (~w)-~w (~w)-~w (~w)...', FormatList)),
	( check_relation(SubjectSemType, Relation, ObjectSemType) ->
	  debug_call(DebugBitVector, 2,
		     format('SUCCEEDED!~n', []))
	; midstring(Relation,'neg_',RelationNegRemoved, 0,4),
	  check_relation(SubjectSemType, RelationNegRemoved, ObjectSemType) ->
	  debug_call(DebugBitVector, 2,
		       format('SUCCEEDED!~n', []))	
  	; debug_call(DebugBitVector, 2,
		       format('failed!~n', [])),
	  fail
	).

% CP = Consequent Predication
check_any_relation(DebugBitVector,
		   CPSubjSemTypeList, DesiredCPSubjSemType,
		   CPRelation,
		   CPObjSemTypeList, DesiredCPObjSemType) :-
	member(DesiredCPSubjSemType, CPSubjSemTypeList),
	member(DesiredCPObjSemType,  CPObjSemTypeList),
	announce_check_relation_1(DebugBitVector,
				  DesiredCPSubjSemType, CPRelation, DesiredCPObjSemType),
	!.


get_lexical_category(ArgList, Label, LexicalCategory) :-
	( get_from_list(tag, ArgList, LexicalCategory) ->
	  true
	; map_label_to_lexical_category(Label, LexicalCategory)
	).

map_label_to_lexical_category(head,     noun) :- !.
map_label_to_lexical_category(mod,      adj) :- !.
map_label_to_lexical_category(verb,     verb) :- !.
map_label_to_lexical_category(aux,      aux) :- !.
map_label_to_lexical_category(pastpart, verb) :- !.
map_label_to_lexical_category(prep,     prep) :- !.
map_label_to_lexical_category(ing,      verb) :- !.
map_label_to_lexical_category(Label,    Label) :-
	format('~n~n####### UNEXPECTED LABEL in map_label_to_lexical_category: ~q~n~n', [Label]).

get_left_partition(ThisMSU, Analysis, LeftPartition) :-
	get_left_partition_1(ThisMSU, Analysis, AllLeft),
	check_for_seminterp_barrier(ThisMSU,AllLeft,LeftPartition),
	!.

get_left_partition_1(ThisMSU, [ThisMSU|LeftPartition], LeftPartition) :-
	!.
get_left_partition_1(ThisMSU, [_NoMatch|PrecedingNPUs], LeftNPU) :-
	get_left_partition_1(ThisMSU, PrecedingNPUs, LeftNPU).

% need to ensure that the colon is used as a barrier.
% IFF the preceding colon is a section header OR the first char following the colon is uppercase.
% The semicolon is always a barrier.
check_for_seminterp_barrier(_ThisMSU,[],[]) :- !.
check_for_seminterp_barrier(_ThisMSU,[[msu_index(_Index),MSU]|_PrecedingMSUs],[]) :- 
	functor(MSU,_Label,_Arity ),
	arg(1,MSU,Arg ),
	get_from_list(inputmatch,Arg,[;]),
	!.
% Fix -- 4/30/2012
% Added _RestMSU to the signature. Otherwise didn't unify when the MSU contained confid().
check_for_seminterp_barrier([msu_index(_IndexThis),ThisMSU|_RestMSU],[[msu_index(_Index),MSU]|PrecedingMSUs],[]) :- 
	functor(MSU,_Label,_Arity ),
	arg(1,MSU,Arg ),
	get_from_list(inputmatch,Arg,[:]),
	collect_inputmatches(PrecedingMSUs,InputMatches0),
	length(InputMatches0,InputMatchLen),
	( InputMatchLen == 1 ->
	    true
	; %append(InputMatches0,InputMatches),
	  rev(InputMatches0,RevInputMatches),
	  concat_atom(RevInputMatches,' ',InputMatchAtom)
	),
	( valid_abstract_section_header(InputMatchAtom,_Count)
        ; first_char_not_lower(ThisMSU)
	),  
	!.
check_for_seminterp_barrier(ThisMSU,[MSU|PrecedingMSUs],[MSU|PrecedingMSUsOut]) :-
	check_for_seminterp_barrier(ThisMSU,PrecedingMSUs,PrecedingMSUsOut).

collect_inputmatches([],[]) :- !.
collect_inputmatches([List|Rest],[InputMatchAtom|RestOut]) :-
	collect_inputmatches_aux(List,InputMatches0),
	append(InputMatches0,InputMatches),
	concat_atom(InputMatches,' ', InputMatchAtom),
	!,
	collect_inputmatches(Rest,RestOut).
collect_inputmatches([_Item|Rest],Out) :-
	collect_inputmatches(Rest,Out).

collect_inputmatches_aux([],[]) :- !.
collect_inputmatches_aux([Item|Rest],[InputMatch|RestOut]) :-
	functor(Item,_,1),
	arg(1,Item,List),
	get_from_list(inputmatch,List,InputMatch),
	!,
	collect_inputmatches_aux(Rest,RestOut).
collect_inputmatches_aux([_Item|Rest],RestOut) :-
	collect_inputmatches_aux(Rest,RestOut).

first_char_not_lower(MSU) :-
	functor(MSU,_Label,_Arity),
	arg(1,MSU,Arg),
	get_from_list(inputmatch,Arg,[InputMatchH|_InputMatchT]),
	atom_codes(InputMatchH,[Char|_InputMatchHStr]),
	\+ is_lower(Char),
	!.


% ---------- DETERMINE_LEFT_DOMAIN ----------

determine_left_domain(prep(PrepList), _ThisMSU, [LeftMSU|_Rest], [LeftMSU]) :-
	get_from_list(lexmatch, PrepList, [of] ),
	!.
determine_left_domain(pastpart(_), ThisMSU, LeftPartition, [ThisMSU|LeftPartition]) :-
	!.
determine_left_domain(ing(_), ThisMSU, LeftPartition, [ThisMSU|LeftPartition]) :-
	!.
determine_left_domain(_OtherIndicator, _ThisMSU, LeftPartition, LeftPartition).


% ---------- DETERMINE_RIGHT_DOMAIN ----------

determine_right_domain(prep(_),         ThisMSU, _RestNPUs, [ThisMSU]) :-
	!.
determine_right_domain(ing(_),          ThisMSU,  RestNPUs, [ThisMSU|RestNPUs]) :-
	!.
determine_right_domain(_OtherIndicator, _ThisMSU, RestNPUs, RestNPUs ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
% ---------- Find An Adverb Cue ----------

This is an adhoc predicate which is necessary for three reasons. 
a. prepositions which can be particles are listed in the Lexicon with
label "adverb" (in addition to "preposition"). 
b. the Tagger often incorrectly treats such a preposition as an adverb
c. if such an item follows an actual adverb, they are both put in the
   same MSU, such as " x arises [normally off] the y"

In such a case this predicate is the only way to determine that the
following NPU is an allowable argument of "arises"

*/
find_an_adverb_cue([ThisMSU,NextMSU|_], Cue, NextMSU, CueList) :-
	rev(ThisMSU, RevMSU),
	get_from_list(adv, ThisMSU, AdvList),
	get_from_list(adv, RevMSU, RevAdvList),
	( get_from_list(lexmatch, AdvList, [Cue]),
	  CueList = AdvList
	; get_from_list(lexmatch, RevAdvList, [Cue]),
	  CueList = RevAdvList
	),
	!.
find_an_adverb_cue([_ThisMSU|RestMSUs], Cue, NextMSU, CueList) :-
    find_an_adverb_cue(RestMSUs, Cue, NextMSU, CueList).



/*
% ---------- Get Args of Verb ----------

Differs from get_args_of_nominalization in that that the left argument is
not marked by a preposition
*/

get_args_of_verb(Cue, MoreNPUs, LeftPartition, CueList, LeftHeadList, RightHeadList) :-
	nonvar(Cue),
	!,
	( find_the_next(prep, Cue, none, MoreNPUs, CueList, RightNPU, _Remainder)
	; find_the_next(adv,  Cue, none, MoreNPUs, CueList, _ADVMSU,[RightNPU|_])
	; find_an_adverb_cue(MoreNPUs, Cue, RightNPU, CueList)
	),
	get_from_list(head, RightNPU, RightHeadList),
	locate_npu_head(LeftPartition, _LeftType, LeftHeadList).
    
get_args_of_verb(_Cue, MoreNPUs, LeftPartition, _CueList, LeftHeadList, RightHeadList) :-
	locate_npu_head(MoreNPUs,      _RightType, RightHeadList),
	locate_npu_head(LeftPartition, _LeftType,  LeftHeadList).

get_args_of_adj(_Indicator, Cue, _MSU, MoreNPUs, LeftPartition, CueList, LeftHeadList, RightHeadList) :-
	nonvar(Cue),
	!,
	( find_the_next(prep, Cue, none, MoreNPUs, CueList, RightNPU, _Remainder)
	; find_the_next(adv,  Cue, none, MoreNPUs, CueList, _ADVMSU,[RightNPU|_])
	; find_an_adverb_cue(MoreNPUs, Cue, RightNPU, CueList)
	),
	get_from_list(head, RightNPU, RightHeadList),
	locate_npu_head(LeftPartition, _LeftType, LeftHeadList).
% Special treatment for 'X-stimulated Y' type phrases.    
get_args_of_adj(Indicator,_Cue, MSU, _MoreNPUs, _LeftPartition, 
		_CueList, LeftHeadList, RightHeadList) :-
        get_hyphenated_left_argument(Indicator,MSU,LeftHeadList),
	!,
	get_adj_potential_right_argument(MSU,Indicator,RightHeadList,inclusive).
%	locate_npu_head([MSU],_Type,RightHeadList).
get_args_of_adj(Indicator,Cue, MSU, _MoreNPUs, LeftPartition, 
		_CueList, LeftHeadList, RightHeadList) :-
        \+ nonvar(Cue),
	( Indicator = mod(_)
	; Indicator = empty_mod(_)
	),
	!,
	locate_npu_head(LeftPartition, _LeftType,  LeftHeadList),
	get_adj_potential_right_argument(MSU,Indicator,RightHeadList,exclusive).
%	locate_npu_head([MSU],_Type,RightHeadList).
get_args_of_adj(Indicator,Cue, _MSU, MoreNPUs, LeftPartition, 
		_CueList, LeftHeadList, RightHeadList) :-
        \+ nonvar(Cue),
	( Indicator = head(_)
	; Indicator = empty_head(_)
	),
	locate_npu_head(MoreNPUs,      _RightType, RightHeadList),
	locate_npu_head(LeftPartition, _LeftType,  LeftHeadList).

%get_args_of_nominalization_baseline(Indicator, _Cue,
%                                   ThisMSU, MoreNPUs, LeftPartition,
%                                   LeftHeadList, RightHeadList ) :- 
%	locate_npu_head(MoreNPUs, _RightType, RightHeadList) ,
%	( preceding_mod(Indicator,ThisMSU,LeftHeadList)
%	; locate_npu_head(LeftPartition,_LeftType,LeftHeadList)
%	).

get_hyphenated_left_argument(Indicator,MSU,LeftHeadList) :-
    	( Indicator = mod(_)
	; Indicator = empty_mod(_)
	),
	nextto(punc(PossibleHyphenList),Indicator,MSU),
	get_from_list(inputmatch,PossibleHyphenList,[-]),
	nextto(PrecedingModifier,punc(PossibleHyphenList),MSU),
	arg(1,PrecedingModifier,LeftHeadList).

get_adj_potential_right_argument(MSU,Indicator,RightHeadList,InclusiveOrNot) :-
        rev(MSU,RevMSU),
        get_adj_potential_right_argument_aux(RevMSU,Indicator,RightHeadList,InclusiveOrNot),
	check_pos(Indicator,RightHeadList).
	
get_adj_potential_right_argument_aux([Indicator|_],Indicator,_RightHeadList,_) :- !, fail.
get_adj_potential_right_argument_aux([head(RightHeadList)|_Rest],_Indicator,RightHeadList,_).
get_adj_potential_right_argument_aux([mod(RightHeadList)|_Rest],_Indicator,RightHeadList,_).
get_adj_potential_right_argument_aux([shapes(RightHeadList)|_Rest],_Indicator,RightHeadList,_).
get_adj_potential_right_argument_aux([not_in_lex(RightHeadList)|_Rest],_Indicator,RightHeadList,_).
get_adj_potential_right_argument_aux([empty_head(RightHeadList)|_Rest],_Indicator,RightHeadList,inclusive).
get_adj_potential_right_argument_aux([empty_mod(RightHeadList)|_Rest],_Indicator,RightHeadList,inclusive).
get_adj_potential_right_argument_aux([_NonArgument|Rest],Indicator,RightHeadList,InclusiveOrNot) :-
        get_adj_potential_right_argument_aux(Rest,Indicator,RightHeadList,InclusiveOrNot).

check_pos(Indicator,HeadList) :-
    arg(1,Indicator,IndicatorList),
    memberchk(position(_IndicatorStart,IndicatorEnd),IndicatorList),
    memberchk(position(HeadListStart,_HeadListEnd),HeadList),
    HeadListStart > IndicatorEnd.

preceding_mod(HeadList,MSU,ModList) :-
	nextto(Mod,HeadList,MSU),
	functor(Mod,Label,_Arity),
	( Label == mod
	; Label == shapes
	; Label == not_in_lex
	; Label == head
	),
	!,
	arg(1,Mod,ModList).
	

% ---------- CHECK_FOR_PASSIVE ----------
% Passive occurs with both *be* and prep.  This prep should
% probably be lexically specified, but for now *by* and *with*
% are checked for.

check_for_passive(Indicator, _LeftPartition, VarInfoList, RestNPUs, _Definitions, PrepObjList) :-
	% check_for_aux(LeftPartition, Definitions, 0),
	check_for_past_participle(Indicator, VarInfoList),
	check_for_prep(Indicator, RestNPUs, PrepObjList),
	!.

% ----- check_for_aux
% No more than one constituent can intervene

% check_for_aux( [], _, _ ) :- !, fail.
% check_for_aux([ThisMSU|_Rest], Definitions, NIn) :-
% 	NOut is NIn + 1,
% 	NOut =< 2,
% 	get_from_list(aux, ThisMSU, AuxList),
% 	get_from_list(lexmatch, AuxList, [Word]),
% 	get_base(Word, aux, Definitions, be),
% 	!.
% check_for_aux([_NoMatch|RestMSUs], Definitions, NIn) :-
% 	NOut is NIn + 1,
% 	check_for_aux(RestMSUs, Definitions, NOut).

%----- check_for_past_participle
% ensure that the indicator (arg 1) is verb(VerbList) structure,
% and that VerbList's lexmatch term is either past or pastpart
check_for_past_participle(pastpart(_), _VarInfoList).
check_for_past_participle(verb(VerbList), VarInfoList) :-
	get_from_list(lexmatch, VerbList, [VerbWord]),
	get_from_list(VerbWord, VarInfoList, VerbWordList),
	get_from_list(verb, VerbWordList, [VerbForm]),
	memberchk(VerbForm, [pastpart,past]).


%----- check_for_prep
% this predicate allowed only  prep to  be in the next constituent
% It has been modified on 08/23/03 to allow  an adverb (only) is allowed to intervene
% an example treated with and treated successfully with are now allowed

% check_for_prep([]) :- !,fail.
check_for_prep(_Indicator,[ThisMSU|_], PrepObjList) :-
	get_from_list(prep, ThisMSU, PrepList),
	get_from_list(lexmatch, PrepList, [Prep]),
	get_from_list(head, ThisMSU, PrepObjList),
	memberchk(Prep,[by]),
	!.
% Only apply to certain verbs (associated, what else?)
% This is a bit iffy, not sure what the generalization is, predicates that involve an *instrument* subject?
check_for_prep(Indicator,[ThisMSU|_], PrepObjList) :-
	get_from_list(prep, ThisMSU, PrepList),
	get_from_list(lexmatch, PrepList, [Prep]),
	get_from_list(head, ThisMSU, PrepObjList),
	arg(1,Indicator,VerbList),
	get_from_list(lexmatch,VerbList,[Verb]),
	memberchk(Prep,[with,via]),
	!,
	memberchk(Verb,[associate,associated,treat,treated,pretreat,pretreated,control,controlled,
                       alleviate,alleviated,ameliorate,ameliorated,attenuate,attenuated,
			eliminate,eliminated,manage,managed,mitigate,mitigated,
		        immunize,immunized,prevent,prevented,transfected,transfect,cotransfect,cotransfected]).
check_for_prep(_Indicator,[ThisMSU|RestMSUs], PrepObjList) :-
	get_from_list(verb, ThisMSU, VerbList),
	get_from_list(lexmatch, VerbList, [Verb]),
	length(RestMSUs,Len),
	Len > 0,
	RestMSUs = [NextMSU|_],
	get_from_list(head, NextMSU, PrepObjList),
	!,
	memberchk(Verb,[using]).
check_for_prep(Indicator,[ThisMSU|RestMSUs], PrepObjList) :-
	( get_from_list(empty_head, ThisMSU, _) ->
	  true
	; get_from_list(empty_mod, ThisMSU, _) ->
	  true
	; get_from_list(adv, ThisMSU, _) ->
	  true
	),
	check_for_prep(Indicator, RestMSUs, PrepObjList).

% main_verb: finds if a word qualifies as a verb complex head.
% a verb complex is defined as a sequence of auxiliaries and a main verb.
% adverbs are allowed to intervene. Only one main verb is allowed in a verb complex.
% lone auxiliaries are considered main verb. To cover participles, a main verb is 
% defined as a nonauxiliary and nonmodal. This predicate is not complete, and
% is currently used for subordinator-predicate association. However, the notion
% of verb complex is useful in general (see Tom's dissertation) and could be exploited
% for various purposes.
main_verb(MSU,_RightAnalysis) :-
	get_from_list(verb,MSU,_Verb),
	!.
main_verb(MSU,RightAnalysis) :-
	get_from_list(aux,MSU,_Aux),
	!,
	rev(RightAnalysis,RevRightAnalysis),
	find_next_non_adverb(RevRightAnalysis,NextMSU),
	\+is_verb_indicator(NextMSU).
main_verb(MSU,_RightAnalysis) :-
	get_from_list(pastpart,MSU,_PastPart),
	!.

find_next_non_adverb([MSU|_Rest],First) :-
	\+ get_from_list(adv,MSU,_Adv),
%	MSU = [First|_],
	get_first(MSU,First),
	!.
find_next_non_adverb([_MSU|Rest],Out) :-
	find_next_non_adverb(Rest,Out).

get_first([msu_index(_MSUIndex)|Rest],First) :-
	get_first(Rest,First).
get_first([confid(_Confid)|Rest],First) :-
	get_first(Rest,First).
get_first([First|_Rest],First).

	

% ---------- Get Arguments ----------

% See if this can be cleaned up
%get_arguments(Indicator, UsedHeadsIn, ConjList, SubordinatorPredicateList,
%	      ThisMSU, LeftPartition, Definitions, VarInfoList, CueList,
%              SubjectHeadList, SubjectSemTypeList, SubjectSemType, SubjectMetaConcList, SubjectCUI,
%              ObjectHeadList,  ObjectSemTypeList,  ObjectSemType,  ObjectMetaConcList, ObjectCUI) :-
%	% hack to keep singleton variable checker quiet
%	% when announce_predication is commented out
%%	Relation \== 'BOGUS',
%	get_corresponding_relation(Indicator, ThisMSU, RestMSUs, Relation, Cue, 
%				   RemainingBases, UpdatedIndicator, IndicatorHead, NewRestMSUs),
%	determine_left_domain(Indicator, ThisMSU, LeftPartition, LeftDomain),
%	determine_right_domain(Indicator, ThisMSU, NewRestMSUs, RightDomain),
%	get_arguments_1(Indicator, Cue, Definitions, 


get_arguments(Indicator, RemainingBases, Cue, Relation, UsedHeadsIn,
	      ConjList, SubordinatorPredicateList,ThisMSU, LeftPartition, RestMSUs,
              LeftDomain, RightDomain, Definitions, VarInfoList, CueList,
              SubjectHeadList, SubjectSemTypeList, SubjectSemType, SubjectMetaConcList, SubjectCUI,
              ObjectHeadList,  ObjectSemTypeList,  ObjectSemType,  ObjectMetaConcList, ObjectCUI) :-
	% hack to keep singleton variable checker quiet
	% when announce_predication is commented out
	Relation \== 'BOGUS',
	get_arguments_1(Indicator, Cue, Definitions, 
	                ThisMSU, LeftPartition, RestMSUs,
			LeftDomain, RightDomain, 
			CueList, LeftHeadList, RightHeadList, Base),
	get_from_list(bases, LeftHeadList, LeftHeadListBases),
	\+ intersect(LeftHeadListBases, RemainingBases),
	get_from_list(bases, RightHeadList, RightHeadListBases),
	\+ intersect(RightHeadListBases, RemainingBases),
	check_for_used_head(UsedHeadsIn, LeftHeadList, ConjList,SubordinatorPredicateList,
			    Definitions, VarInfoList, LeftDomain, Indicator),
	( get_hyphenated_left_argument(Indicator,ThisMSU,LeftHeadList) ->
	  true
	;  \+ has_head_been_used(UsedHeadsIn, RightHeadList, _UsedHead)
	),
	set_head_lists(Indicator, LeftPartition, RestMSUs, Definitions, VarInfoList,
		       LeftHeadList, RightHeadList,
		       SubjectHeadList, ObjectHeadList,ActiveOrPassive),
	argument_consonance(ActiveOrPassive,LeftHeadList,UsedHeadsIn),
        %  If the indicator is transitive, make sure that either the object is immediately
        % to the right (if active).
        indicator_obligate_object(ActiveOrPassive,Indicator,Cue,Definitions,
	 	                  ObjectHeadList,RightDomain),
	check_prep_next_MSU(Indicator,RightDomain),
	check_prep_previous_subcat(Indicator,LeftDomain,LeftHeadList,Definitions),
        check_arguments_in_parentheses(Indicator,ThisMSU,Base,LeftDomain,LeftHeadList,
	                               RightDomain,RightHeadList),
        % Ensure the arguments is not an obligate object of some other verb.
%	arguments_non_obligate_objects(ActiveOrPassive,SubjectHeadList,ObjectHeadList,
%	                              Definitions,VarInfoList,LeftDomain,RightDomain),
	get_sem_info_and_type(SubjectHeadList,  SubjectMetaConcList, SubjectCUI,
			      SubjectSemTypeList, SubjectSemType),
	get_sem_info_and_type(ObjectHeadList, ObjectMetaConcList, ObjectCUI,
			      ObjectSemTypeList, ObjectSemType).

indicator_obligate_object(_ActiveOrPassive,Indicator,_Cue,_Definitions,_ObjectHeadList,_RightDomain) :-
	\+ is_verb_indicator(Indicator),
	!.
indicator_obligate_object(active,Indicator,_Cue,Definitions,ObjectHeadList,RightDomain) :-
	get_transitive_cue(interpreted,Indicator,Definitions,TranCue), 
	is_next_msu_head(ObjectHeadList,TranCue,RightDomain),
	!.
indicator_obligate_object(active,_Indicator,Cue,_Definitions,ObjectHeadList,RightDomain) :-
        is_next_msu_head(ObjectHeadList,Cue,RightDomain).
%       !.
indicator_obligate_object(passive,_Indicator,_Cue,_Definitions,_ObjectHeadList,_RightDomain).

% This is used to find the obligate objects. Checks whether the found argument satisfies
% the constraints of the obligate object rule for verbal indicators.
% possible_macro_np_head is used for sentences like " The patient receives several doses of 
% subcutaneous enoxaparin", where the next MSU checked is [several,doses] and since its head
% is empty, we look at the next MSU. This may not be totally right, need to check further with 
% Tom. 
is_next_msu_head(List,TranCue,[MSU|Domain]) :-
	\+ get_from_list(compl,MSU,_CList),
	( possible_macro_np_head(List,[MSU|Domain]) -> 
	  true
        ; locate_npu_head([MSU],_Type,List)
        ),
	( TranCue == empty ->
	  \+ get_from_list(prep,MSU,_PList)
	; get_from_list(prep,MSU,PrepList),
	  get_from_list(lexmatch,PrepList,[TranCue])
	),
	!.
is_next_msu_head(List,TranCue,[MSU|RestDomain]) :-
	get_from_list(punc,MSU,PuncList),
	get_from_list(bases,PuncList,[Base]),
	left_parenthesis(Base),
	!,
	consume_parenthesis(RestDomain,NewRestDomain,1),
	is_next_msu_head(List,TranCue,NewRestDomain).
is_next_msu_head(List,TranCue,[MSU|RestDomain]) :-
	get_from_list(adv,MSU,_AdvList),
	is_next_msu_head(List,TranCue,RestDomain).



possible_macro_np_head(Head,[DomainMSU|Rest]) :-
	get_from_list(adv,DomainMSU,_AdvList),
	!,
	possible_macro_np_head(Head,Rest).
possible_macro_np_head(Head,[DomainMSU|Rest]) :-
	( get_from_list(empty_head,DomainMSU,_EmptyHead)
	; get_from_list(empty_mod,DomainMSU,_EmptyMod)
	; get_from_list(head,DomainMSU,HeadList),
	  is_empty_head(HeadList)
        ),
	!,
	possible_macro_np_head_aux(Head,Rest).

possible_macro_np_head_aux(Head,[MSU|_Rest]) :-
	locate_npu_head([MSU],_Type,Head),
	!,
	get_from_list(prep,MSU,PrepList),
	get_from_list(lexmatch,PrepList,[of]).
possible_macro_np_head_aux(Head,[MSU|Rest]) :-
	get_from_list(adv,MSU,_AdvList),
	possible_macro_np_head_aux(Head,Rest).


is_conditional_msu_empty_head(MSU, Rest) :-
	Rest = [Left|_],
	arg(1,Left,LeftList),
	get_from_list(semgroup,LeftList,SemGroupList),
	!,
	is_conditional_msu_empty_head_aux(MSU,SemGroupList).

is_conditional_msu_empty_head_aux(MSU,SemGroupList) :-
	collect_bases(MSU,BasesList0),
	append(BasesList0,BasesList),
	BasesList = [FirstBase|RestBases],
	conditional_empty_head_base_2N_BOTH(FirstBase,RestBases,SemGroups),
	intersect(SemGroupList,SemGroups).
%	format(user_output,'CONDITIONAL EMPTY HEAD 2N: ~q~n',[MSU]).
is_conditional_msu_empty_head_aux(MSU,SemGroupList) :-
	MSU = [head(HeadList)|_],
	memberchk(bases(BasesList), HeadList),
	BasesList = [FirstBase|RestBases],
	( RestBases == [] ->
	  conditional_empty_head_base_1_BOTH(FirstBase,SemGroups)
	; conditional_empty_head_base_2N_BOTH(FirstBase,RestBases,SemGroups)
	),
	intersect(SemGroupList,SemGroups).
	
is_empty_head(HeadOrModList) :-
	memberchk(bases(BasesList), HeadOrModList),
	BasesList = [FirstBase|RestBases],
	empty_head_check(RestBases, FirstBase).

empty_head_check([], FirstBase) :-
	empty_head_base_1_BOTH(FirstBase).
empty_head_check([NextWord|RestWords], FirstBase) :-
	empty_head_base_2N_BOTH(FirstBase, [NextWord|RestWords]).

% To handle empty heads spanning multiple lexical items
is_msu_empty_head(MSU) :-
	collect_bases(MSU,BasesList0),
	append(BasesList0,BasesList),
	BasesList = [FirstBase|RestBases],
	empty_head_base_2N_BOTH(FirstBase,RestBases).
%	format(user_output,'EMPTY HEAD 2N: ~q~n',[MSU]).

% If this is the case, do not look for parentheses
check_arguments_in_parentheses(Indicator,MSU,_Base,_LeftDomain,LeftHeadList,
	                       _RightDomain,_RightHeadList) :-
        get_hyphenated_left_argument(Indicator,MSU,LeftHeadList).
check_arguments_in_parentheses(Indicator,_MSU,Base,LeftDomain,LeftHeadList,
	                       RightDomain,RightHeadList) :-
        ( is_nominal_indicator(Indicator) ->
	  \+ Base == '('
        ; no_incomplete_parenthetical_expression_intervenes(left,Indicator,LeftDomain,LeftHeadList),
	  no_incomplete_parenthetical_expression_intervenes(right,Indicator,RightDomain,RightHeadList)
        ),
	!.

% lexical information regarding indicators
get_indicator_lexical_info(_Indicator,[],[]) :- !.
get_indicator_lexical_info(Indicator,[lexicon:LexList|_MoreLexicon],LexInfo) :-
%	functor(Indicator,LexicalCategory,_Arity),
	functor(Indicator,_Func,_Arity),
	arg(1,Indicator,IndList),
	get_from_list(lexmatch,LexList,[LexMatch]),
	get_from_list(lexmatch,IndList,[LexMatch]),
	get_from_list(records,LexList,RecList),
	get_from_list(tag,IndList,LexicalCategory),
	get_lexrec_for_cat(RecList,LexicalCategory,LexInfo),
	!.
get_indicator_lexical_info(Indicator,[_NoMatch|MoreLexicon],LexInfo) :-
	get_indicator_lexical_info(Indicator,MoreLexicon,LexInfo).
	

get_subcategorization_info([entries:Entries|_MoreLexicon],SubcatInfo) :-
	get_subcategorization_info_aux(Entries,SubcatInfo),
	!.
get_subcategorization_info([_Lex|MoreLexicon],SubcatInfo) :-
	get_subcategorization_info(MoreLexicon,SubcatInfo).

get_subcategorization_info_aux([entry:Entry|_MoreEntry],SubcatInfo) :- 
        get_complement_info(Entry,SubcatInfo),
	!.
get_subcategorization_info_aux([_Entry|MoreEntry],SubcatInfo) :-
	get_subcategorization_info_aux(MoreEntry,SubcatInfo).


get_complement_info([complements:Complements|_MoreLexicon],Complements) :- !.
get_complement_info([_Lex|MoreLexicon],SubcatInfo) :-
	get_complement_info(MoreLexicon,SubcatInfo).

% get_nom_cuelist([],[]) :- !.
% get_nom_cuelist([pphr:[prep:[Cue],obj:[np]]|MoreCompl],[Cue|MoreCues]) :-
% 	!,
% 	get_nom_cuelist(MoreCompl,MoreCues).
% get_nom_cuelist([_Compl|MoreCompl],Cues) :-
% 	get_nom_cuelist(MoreCompl,Cues).

get_cue_from_subcat([pphr:[prep:[Cue],obj:[np]]|_MoreCompl],Cue) :-
	\+ memberchk(Cue,[of,by,with]).
get_cue_from_subcat([_Compl|MoreCompl],Cue) :-
	get_cue_from_subcat(MoreCompl,Cue).

determine_object_cue(_Indicator,Cue,_Definitions,Cue) :-
	nonvar(Cue), !.
determine_object_cue(Indicator,_Cue,Definitions,ObjectCue) :-
	get_indicator_lexical_info(Indicator,Definitions,LexInfo),
	get_subcategorization_info(LexInfo,SubcatInfo),
	get_cue_from_subcat(SubcatInfo,ObjectCue).
determine_object_cue(_Indicator,_Cue,_Definitions,of).
%determine_object_cue(_Indicator,_Cue,_Definitions,with).


% This is simplistic for now
% just checks if the verb is transitive and if it is, what is the object marking cue (if any)
% otherwise [empty].
% intransitives are not considered
% ditransitives may not be important. 
% Second clause handles "have", which gets tagged as "aux" and therefore without complement info.
% In this case fincomp and infcomp subcategorization is not considered. (To allow "reveal" as 
% an indicator, for example)..
get_transitive_cue(Status,Indicator,Definitions,Cue) :-
	get_indicator_lexical_info(Indicator,Definitions,LexInfo),
	get_subcategorization_info(LexInfo,SubcatInfo),
%	format(user_output,'Subcategorization: ~q~n~q~n',[Indicator,SubcatInfo]),
	( Status == interpreted ->
	  true
        ; \+ can_take_fincomp(SubcatInfo),
	  \+ can_take_infcomp(SubcatInfo)
        ),
	get_transitive_cue_aux(SubcatInfo,Cue).
%	!.
get_transitive_cue(_Status,aux(IndicatorList),_Definitions,empty) :-
	get_from_list(bases,IndicatorList,[have]).


get_transitive_cue_aux([tran:Compl|_MoreCompl],empty) :- 
        Compl = [compl:[np]|_].
get_transitive_cue_aux([tran:Compl|_MoreCompl],Cue) :-
	Compl = [compl:[pphr:[prep:[Cue]|_]]|_].
% Added this to handle prep subcategorization cases with adjectives. MAy need its own separate predicate. HK 10/27/2017
get_transitive_cue_aux([Compl|_MoreCompl],Cue) :-
	Compl = pphr:[prep:[Cue]|_].
get_transitive_cue_aux([_Compl|MoreCompl],Cue) :- 
	get_transitive_cue_aux(MoreCompl,Cue).

can_take_fincomp([tran:Compl|_MoreCompl]) :-
	Compl = [compl:[fincomp:_]|_],
	!.
can_take_fincomp([_Compl|MoreCompl]) :-
	can_take_fincomp(MoreCompl).

can_take_infcomp([tran:Compl|_MoreCompl]) :-
	Compl = [compl:[infcomp:_]|_],
	!.
can_take_infcomp([_Compl|MoreCompl]) :-
	can_take_infcomp(MoreCompl).

clean_index(_Index0:_Index1/Index2,Index2):- !.
clean_index((_Index0:Index1),Index1):- !.
clean_index(Index,Index).

% Ensure that predication do not cross dependencies
no_crossing_lines(SubjectIndex, IndicatorIndex, ObjectIndex, IndexIn, IndexOut) :-
	clean_index(SubjectIndex,SubjectIndex0),
	clean_index(ObjectIndex,ObjectIndex0),
	clean_index(IndicatorIndex,IndicatorIndex0),
	no_crossing_lines_aux(SubjectIndex0,IndicatorIndex0,ObjectIndex0,IndexIn),
	!,
	append(IndexIn,[SubjectIndex0-IndicatorIndex0-ObjectIndex0],IndexOut).

no_crossing_lines_aux(_,_,_,[]) :- !.
% S1-I1-O1, S2-I2-O2
no_crossing_lines_aux(SubjectIndex,IndicatorIndex, ObjectIndex,
	              [SubjectIndex0-IndicatorIndex0-ObjectIndex0|RestIndex]) :-
	IndicatorIndex > IndicatorIndex0,
	sort_indexes([SubjectIndex,IndicatorIndex,ObjectIndex],[MinIndex,_,_]),
	sort_indexes([SubjectIndex0,IndicatorIndex0,ObjectIndex0],[_,_,MaxIndex0]),
	MinIndex >= MaxIndex0,
	!,
	no_crossing_lines_aux(SubjectIndex,IndicatorIndex,ObjectIndex,RestIndex).
%S1-I1-S2-I2-O2-O1
no_crossing_lines_aux(SubjectIndex,IndicatorIndex,ObjectIndex,
	              [SubjectIndex0-IndicatorIndex0-ObjectIndex0|RestIndex]) :-
	IndicatorIndex > IndicatorIndex0,
	sort_indexes([SubjectIndex,IndicatorIndex,ObjectIndex],[MinIndex,_,MaxIndex]),
	sort_indexes([SubjectIndex0,IndicatorIndex0,ObjectIndex0],[_,MidIndex0,MaxIndex0]),
	MinIndex >= MidIndex0,
	MaxIndex0 >= MaxIndex,
	!,
	no_crossing_lines_aux(SubjectIndex,IndicatorIndex,ObjectIndex,RestIndex).
%S2-S1-I1-O1-S2-I2-O2
no_crossing_lines_aux(SubjectIndex,IndicatorIndex,ObjectIndex,
	              [SubjectIndex0-IndicatorIndex0-ObjectIndex0|RestIndex]) :-
	IndicatorIndex > IndicatorIndex0,
	sort_indexes([SubjectIndex,IndicatorIndex,ObjectIndex],[MinIndex,MidIndex,_]),
	sort_indexes([SubjectIndex0,IndicatorIndex0,ObjectIndex0],[MinIndex0,_,MaxIndex0]),
	MinIndex0 >= MinIndex,
	MidIndex >= MaxIndex0,
	!,
	no_crossing_lines_aux(SubjectIndex,IndicatorIndex,ObjectIndex,RestIndex).

sort_indexes([Index1,Index2,Index3],[IndexOut1,IndexOut2,IndexOut3]) :-
	remove_colon(Index1,Index1_0),
	remove_colon(Index2,Index2_0),
	remove_colon(Index3,Index3_0),
	sort([Index1_0,Index2_0,Index3_0],[IndexOut1,IndexOut2,IndexOut3]).

remove_colon(Index:_RestIndex,Index):- !.
remove_colon(Index,Index).

check_prep_next_MSU(Indicator,[MSU|_RestRightDomain]) :-
	Indicator = prep(_PrepList),
	nextto(Indicator,Modifier,MSU),
	\+ is_ing_modifier(Modifier),
	!.
check_prep_next_MSU(Indicator,_RightDomain) :-
	\+ Indicator = prep(_PrepList).
	
check_prep_previous_subcat(prep(PrepList),[PreviousMSU|_RestLeftDomain],LeftHeadList,_Definitions) :-
	get_from_list(lexmatch,PrepList,[Prep]),
	memberchk(Prep,[of,from,for,with]), % Right association
%	memberchk(Prep,[with]), % Right association
	% Clearly right association is a concept related to NPs, but I wonder if they can be applied in general.
	% Seems like for Graciela's examples, they work.
%	get_from_list(head,PreviousMSU,PreviousList),
%	get_from_list(tag,PreviousList,noun),
	!,
%	get_from_list(head,PreviousMSU,LeftHeadList).
        locate_npu_head([PreviousMSU],_Type,LeftHeadList).
%	memberchk(PreviousMSU,LeftHeadList).
check_prep_previous_subcat(prep(PrepList),[PreviousMSU|_RestLeftDomain],LeftHeadList,Definitions) :-
	get_from_list(lexmatch,PrepList,[Prep]),
	\+ Prep == of,
	( get_from_list(verb,PreviousMSU,PreviousList),
	    Term = verb(PreviousList)
	; locate_npu_head([PreviousMSU],_Type,LeftHeadList),
%	; get_from_list(head,PreviousMSU,PreviousList),
	  get_from_list(tag,PreviousList,Tag),
	  memberchk(Tag,[adj,noun]),  
	  Term = head(PreviousList)  
	),
	!,
%	( get_from_list(head,PreviousMSU,LeftHeadList)
        ( locate_npu_head([PreviousMSU],_Type,LeftHeadList)	
	; \+ get_transitive_cue(interpreted,Term,Definitions,Prep)
	).
check_prep_previous_subcat(_Indicator,_LeftDomain,_LeftHeadList,_Definitions).


get_arguments_1(verb(_), Cue, _Definitions, _ThisMSU, LeftPartition, RestMSUs,
              _LeftDomain, _RightDomain,
	      CueList, LeftHeadList, RightHeadList, none) :-
	get_args_of_verb( Cue, RestMSUs, LeftPartition,
			  CueList, LeftHeadList, RightHeadList ).

get_arguments_1(pastpart(_), Cue, _Definitions, _ThisMSU, LeftPartition, RestMSUs,
              _LeftDomain, _RightDomain,
	      CueList, LeftHeadList, RightHeadList, none) :-
	get_args_of_verb( Cue, RestMSUs, LeftPartition,
			  CueList, LeftHeadList, RightHeadList ).

get_arguments_1(aux(_), Cue, _Definitions, _ThisMSU, LeftPartition, RestMSUs,
              _LeftDomain, _RightDomain,
	      CueList, LeftHeadList, RightHeadList, none) :-
	get_args_of_verb( Cue, RestMSUs, LeftPartition,
			  CueList, LeftHeadList, RightHeadList ).
get_arguments_1(Indicator, Cue, Definitions, ThisMSU, LeftPartition,RestMSUs,
               _LeftDomain, _RightDomain, CueList, LeftHeadList, RightHeadList, Base) :-
	is_nominal_indicator(Indicator),
	\+is_ing_modifier(Indicator),
	determine_object_cue(Indicator,Cue,Definitions,ObjectCue),
	get_args_of_nominalization( Indicator, ObjectCue, ThisMSU, RestMSUs, 
	                            LeftPartition, CueList,LeftHeadList, 
				    RightHeadList,Base ).
get_arguments_1(Indicator, Cue, _Definitions, ThisMSU, LeftPartition,RestMSUs,
               _LeftDomain, _RightDomain, CueList, LeftHeadList, RightHeadList, _Base) :-
	is_nominal_indicator(Indicator),
	is_ing_modifier(Indicator),
	get_args_of_adj( Indicator, Cue, ThisMSU, RestMSUs, LeftPartition,
			  CueList, LeftHeadList, RightHeadList ).
get_arguments_1(Indicator, Cue, _Definitions, ThisMSU, LeftPartition, RestMSUs,
              _LeftDomain, _RightDomain,
	      CueList, LeftHeadList, RightHeadList, none) :-
	is_adjectival_indicator(Indicator),
	get_args_of_adj( Indicator, Cue, ThisMSU, RestMSUs, LeftPartition,
			  CueList, LeftHeadList, RightHeadList ).
get_arguments_1(prep(_), _Cue, _Definitions, _ThisMSU, _LeftPartition, _RestMSUs,
              LeftDomain, RightDomain,
	      _CueList, LeftHeadList, RightHeadList, none) :-
	locate_npu_head(LeftDomain,  _LeftType,  LeftHeadList),
	locate_npu_head(RightDomain, _RightType, RightHeadList).

set_head_lists(Indicator, LeftPartition, RestMSUs, Definitions, VarInfoList,
	       LeftHeadList, RightHeadList, SubjectHeadList, ObjectHeadList,passive) :-
	is_verb_indicator(Indicator),
	check_for_passive(Indicator, LeftPartition, VarInfoList, RestMSUs, Definitions, PrepObjList),
	PrepObjList = RightHeadList,
	!,
	SubjectHeadList = RightHeadList,
	ObjectHeadList = LeftHeadList.	
set_head_lists(_OtherIndicator, _LeftPartition, _RestMSUs, _Definitions, _VarInfoList,
	       LeftHeadList, RightHeadList, SubjectHeadList, ObjectHeadList,active) :-
	SubjectHeadList = LeftHeadList,
	ObjectHeadList = RightHeadList.

is_verb_indicator(pastpart(_)).
is_verb_indicator(verb(_)).
is_verb_indicator(aux(_)).

is_nominal_indicator(Indicator) :-
	( Indicator = head(IndicatorList)
	; Indicator = mod(IndicatorList)
	; Indicator = empty_head(IndicatorList)
	; Indicator = empty_mod(IndicatorList)
	),
	memberchk(tag(noun),IndicatorList).

is_ing_modifier(Modifier) :-
	( Modifier = mod(ModifierList)
	; Modifier = empty_mod(ModifierList)
	),
	get_from_list(tag,ModifierList,Tag),
	memberchk(Tag,[adj,noun]),
	get_from_list(inputmatch,ModifierList,[FirstInputMatch|_]),
	midstring(FirstInputMatch, 'ing', _Back, _BackLen,3,0),
	!.

is_adjectival_indicator(Indicator) :-
	( Indicator = head(IndicatorList)
	; Indicator = mod(IndicatorList)
	; Indicator = empty_head(IndicatorList)
	; Indicator = empty_mod(IndicatorList)
	),
	memberchk(tag(adj),IndicatorList).

% The potential argument should be in the same parenthetical expression as the 
% indicator (if any). There is an exception: a) Parenthetical expressions themselves
% act as indicators in the context of nominalizations, e.g., X inhibitor (Y). 
% I expect that this will increase precision, but possibly decrease recall. 
% It is still rough, and may need to be examined more closely based on examples.

% Extended 5/2/2012 - Need to consider the MSU with the HeadList as well,
% to consider that the parenthesis might be within the MSU. 
no_incomplete_parenthetical_expression_intervenes(left,Indicator,Partition,HeadList) :-
	get_intervening_structure(Partition,HeadList,InterveningStructure),
	expand_intervening_structure_with_MSU(left,Indicator,InterveningStructure,
					      Partition,HeadList,InterveningStructure0),
	complete_or_no_parenthetical_expression([')',']'],['(','['],
	                                        InterveningStructure0,0,0),
	!.
no_incomplete_parenthetical_expression_intervenes(right,Indicator,Partition,HeadList) :-
	get_intervening_structure(Partition,HeadList,InterveningStructure),
	expand_intervening_structure_with_MSU(right,Indicator,InterveningStructure,
					      Partition,HeadList,InterveningStructure0),
	complete_or_no_parenthetical_expression(['(','['],[')',']'], 
	                                        InterveningStructure0,0,0),
	!.
	

complete_or_no_parenthetical_expression(_,_,_,In,_Out) :-
	In < 0,
	!,
	fail.
complete_or_no_parenthetical_expression(_,_,[],In,In) :- !.
complete_or_no_parenthetical_expression(First,Second,[MSU|RestMSUs],In,Out) :-
	complete_or_no_parenthetical_expression_aux(First,Second,MSU,In,Out0),
	!,
	complete_or_no_parenthetical_expression(First,Second,RestMSUs,Out0,Out).

complete_or_no_parenthetical_expression_aux(_,_,_,In,_Out) :-
	In < 0,
	!,
	fail.
complete_or_no_parenthetical_expression_aux(_,_,[],In,In) :- !.
complete_or_no_parenthetical_expression_aux(First,Second,[punc(PuncList)|RestItems],In,Out) :-
	get_from_list(bases,PuncList,[Base]),
	( memberchk(Base,First) ->
	  Out0 is In + 1
        ; memberchk(Base,Second) ->
	  Out0 is In - 1
        ),
	!,
	complete_or_no_parenthetical_expression_aux(First,Second,RestItems, Out0, Out).
complete_or_no_parenthetical_expression_aux(First,Second,[_Item|RestItems],In,Out) :-
	complete_or_no_parenthetical_expression_aux(First,Second,RestItems,In,Out).

% This predicate searches for the closing parenthesis, assuming the opening parenthesis has already been found and returns the MSUList following the closing parenthesis.
consume_parenthesis([MSU|RestDomain], RestDomain,1) :-
	get_from_list(punc,MSU,PuncList),
	get_from_list(bases,PuncList,[Base]),
	right_parenthesis(Base),
	!.
consume_parenthesis([MSU|RestDomain], RestDomainOut,In) :-
	get_from_list(punc,MSU,PuncList),
	get_from_list(bases,PuncList,[Base]),
	right_parenthesis(Base),
	!,
	Out is In - 1,
	consume_parenthesis(RestDomain, RestDomainOut, Out).
consume_parenthesis([MSU|RestDomain], RestDomainOut, In) :-
	get_from_list(punc,MSU,PuncList),
	get_from_list(bases,PuncList,[Base]),
	left_parenthesis(Base),
	!,
	Out is In + 1,
	consume_parenthesis(RestDomain, RestDomainOut, Out).
consume_parenthesis([_MSU|RestDomain], RestDomainOut, In) :-
	consume_parenthesis(RestDomain, RestDomainOut, In).
	

left_parenthesis('[').
left_parenthesis('(').
right_parenthesis(']').
right_parenthesis(')').

% Given an MSU, get the elements preceding the onegiven in List.
% Exclude non-token elements, msu_index and confid, and Indicator, if any.
% 5/2/2012 - Ensure that a similar predicate does not already exist.
% If Indicator is in the MSU, only consider a sublist.
get_rest_of_msu(Indicator,MSU,List,Out):-
	memberchk(Indicator,MSU),
	( append(_,[Indicator|Rest],MSU),
	  get_rest_of_msu_aux([Rest],List,Out)
	; append(Prefix,[Indicator|_],MSU),
	  get_rest_of_msu_aux([Prefix],List,Out)
	),
	!.
get_rest_of_msu(_Indicator,MSU,List,Out) :-
	get_rest_of_msu_aux(MSU,List,Out).
	    
get_rest_of_msu_aux([],_,[]):- !.
get_rest_of_msu_aux(_,[],[]):- !.
get_rest_of_msu_aux([confid(_)|Rest],List,Out) :-
	get_rest_of_msu_aux(Rest,List,Out),
	!.
get_rest_of_msu_aux([msu_index(_)|Rest],List,Out) :-
	get_rest_of_msu_aux(Rest,List,Out),
	!.
get_rest_of_msu_aux([MSUElement|_Rest],List,[]) :-
	% The following line was added to prevent SemRep failure. 6/19/12
	MSUElement \== [],
	arg(1,MSUElement,List),
	!.
get_rest_of_msu_aux([MSUElement|Rest],List,[MSUElement|RestOut]) :-
	get_rest_of_msu_aux(Rest,List,RestOut).


check_for_used_head(UsedHeadsIn, SubjectHeadList, ConjList, SubordinatorPredicateList,
		    Definitions, VarInfoList, LeftDomain, Indicator) :-
	( has_head_been_used(UsedHeadsIn, SubjectHeadList, UsedHead) ->
	  get_intervening_structure(LeftDomain, UsedHead, InterveningStructure),
          % debug_check_for_used_head(UsedHead, ConjList, Indicator, InterveningStructure, LeftDomain),
	  license_for_reuse(UsedHead, Indicator, ConjList, SubordinatorPredicateList,
			    Definitions, VarInfoList, InterveningStructure)
	; true
	).


% debug_check_for_used_head(UsedHead, ConjList, Indicator, InterveningStructure, LeftDomain) :-
% 	( UsedHead = [_|_] ->
% 	  format('~nUSED HEAD:   ~q~n', [UsedHead]),
% 	  format('~nCONJLIST:    ~q~n', [ConjList]),
% 	  format('~nINDICATOR:   ~q~n', [Indicator]),
% 	  format('~nINTERVENING: ~q~n', [InterveningStructure]),
% 	  format('~nLEFT DOMAIN: ~q~n', [LeftDomain])
% 	; true
% 	).

% If a noun phrase is used more than twice, all uses have to be the same role (either subject or object)
% The girl who likes the boy who kicked Bill kissed Fred.
% Disallows "the boy" as the subject of "kissed".
% Ad hoc, and requires further look.
% For example, will not work "PARK7 which is stimulated by P53 which inhibits DISC1 stimulates PDE4B."
% P53 here should not be allowed as "stimulates", but will be with this rule.
argument_consonance(_ActiveOrPassive,_Argument,[]) :- !.
argument_consonance(ActiveOrPassive,Argument,UsedHeadList) :-
	( ActiveOrPassive == active ->
	  Role = subj
        ; Role = obj
        ), 
	argument_consonance_aux(Role,Argument,UsedHeadList).

argument_consonance_aux(Role,Argument,UsedHeadList) :-
	find_argument_use(Argument,UsedHeadList,ArgumentUses),
	length(ArgumentUses,UseCount),
	UseCount >= 2,
	!,
	same_role(ArgumentUses,Role).
argument_consonance_aux(_Role,_Argument,_UsedHeadList).
	 
find_argument_use(_Argument,[],[]):- !.
find_argument_use(Argument,[UsedHead|Rest],[SurfaceRole|RestRole]) :-
	UsedHead = [role(_LogicalRole),srole(SurfaceRole),indicator(_Indicator)|Argument],
	!,
	find_argument_use(Argument,Rest,RestRole).
find_argument_use(UsedHead,[_Head|Rest],Roles) :-
	find_argument_use(UsedHead,Rest,Roles).

same_role([],_Role) :- !.
same_role([Role|RestRole],Role) :-
	same_role(RestRole,Role).

no_np([]) :- !.
no_np([ThisNPU|Rest]) :-
	\+ locate_npu_head([ThisNPU],_Type,_Head), 
	no_np(Rest).

no_verbal([]) :- !.
no_verbal([ThisNPU|Rest]) :-
	\+ memberchk(verb(_),ThisNPU),
	\+ memberchk(aux(_),ThisNPU),
	\+ memberchk(pastpart(_),ThisNPU), 
	!,
	no_verbal(Rest).
	
/*
% ---------- Has Head Been Used ----------

A potentially reusable subject has to have a coordinator between it and the
current indicator.  It also must have been used before as a subject.

Need to use index() to insure the conj is in between the potential argument and
the current indicator.

Also need to compute reusability for objects

*/
% has_head_been_used([],_,_) :- !,fail.
has_head_been_used([ThisUsedHead|_RestUsedHeads], LeftHeadList, ThisUsedHead) :-
	get_from_list(index, ThisUsedHead, Index),
	get_from_list(index, LeftHeadList, Index),
	( get_from_list(lexmatch, ThisUsedHead, LexMatch) ->
	  get_from_list(lexmatch, LeftHeadList, LexMatch)
        ; get_from_list(inputmatch, ThisUsedHead, InputMatch),
	  get_from_list(inputmatch, LeftHeadList, InputMatch)
        ),
	!.
has_head_been_used([_ThisUsedHead|RestUsedHeads], LeftHeadList, UsedHead) :-
	has_head_been_used(RestUsedHeads, LeftHeadList, UsedHead).

/*
% ---------- Get Intervening Structure ----------
% From the current indicator leftwards up to the potential argument

*/

get_intervening_structure([], _UsedHead, []).

get_intervening_structure([ThisMSU|_Rest], UsedHead, []):-
	get_from_list(head, ThisMSU, ThisHead),
	get_from_list(index, UsedHead, Index),
	get_from_list(index, ThisHead, Index),
	!.
get_intervening_structure([ThisMSU|_Rest], UsedHead, []):-
	rev(ThisMSU, RevMSU),
	get_from_list(mod, RevMSU, ThisHead),
	get_from_list(index, UsedHead, Index),
	get_from_list(index, ThisHead, Index),
	!.
get_intervening_structure([ThisMSU|_Rest], UsedHead, []):-
        get_from_list(shapes, ThisMSU, ThisHead),
        get_from_list(index, UsedHead, Index),
        get_from_list(index, ThisHead, Index),
        !.
get_intervening_structure([ThisMSU|_Rest], UsedHead, []):-
        get_from_list(not_in_lex, ThisMSU, ThisHead),
        get_from_list(index, UsedHead, Index),
        get_from_list(index, ThisHead, Index),
	!.
get_intervening_structure([ThisMSU|Rest], UsedHead, [ThisMSU|Gap]):-
	get_intervening_structure(Rest, UsedHead, Gap).

% In some cases (parentheses, for example), it is necessary to
% expand the intervening_structure with a portion of the current MSU.
% Partition is expected to contain MSUElement.
expand_intervening_structure_with_MSU(left,Indicator,InterveningStructure,
				      Partition,HeadList,InterveningStructureOut):-
	append(InterveningStructure,[HeadListMSU0|_],Partition),
	!,
	rev(HeadListMSU0,HeadListMSU),
	get_rest_of_msu(Indicator,HeadListMSU,HeadList,InterveningStructureMSU),
	append([InterveningStructureMSU],InterveningStructure,InterveningStructureOut).
expand_intervening_structure_with_MSU(right,Indicator,InterveningStructure,
				      Partition,HeadList,InterveningStructureOut):-
	( InterveningStructure = Partition ->
	  InterveningStructureOut = InterveningStructure
	; append(InterveningStructure,[HeadListMSU|_],Partition),
	  get_rest_of_msu(Indicator,HeadListMSU,HeadList,InterveningStructureMSU),
	  append(InterveningStructure,[InterveningStructureMSU],InterveningStructureOut)
	).
	
% announce_predication(FormatString, Relation,
% 		     Indicator, LeftHeadList, RightHeadList) :-
% 	arg(1, Indicator, IndicatorList),
% 	get_from_list_if_possible(inputmatch,  IndicatorList,   IndicatorLexMatch),
% 	get_from_list_if_possible(inputmatch,  LeftHeadList,    LeftLexMatch),
% 	get_from_list_if_possible(inputmatch,  RightHeadList,   RightLexMatch),
% 	get_from_list_if_possible(metaconc,    LeftHeadList,    LeftMetaConc),
% 	get_all_semtypes(LeftMetaConc, LeftSemTypes),
% 	get_from_list_if_possible(metaconc,    RightHeadList,   RightMetaConc),
% 	get_all_semtypes(RightMetaConc, RightSemTypes),
% 	format(FormatString, [LeftLexMatch,LeftSemTypes,
% 			      Relation,
% 			      IndicatorLexMatch,RightLexMatch,RightSemTypes]),
% 	!.
% announce_predication(FormatString, Relation,
% 		     Indicator, LeftHeadList, RightHeadList) :-
% 	format('~n~n########announce_predication failed on ~q',
% 	       [[FormatString, Relation, Indicator, LeftHeadList, RightHeadList]]),
% 	!,
% 	fail.

% announce_conjlist(ConjList) :-
% 	format('~nConjList:~n', []),
% 	announce_conjlist_1(ConjList).
% 	
% announce_conjlist_1([]).
% announce_conjlist_1([H|T]) :-
% 	announce_one_conj_term(H),
% 	nl,
% 	announce_conjlist_1(T).

% announce_one_conj_term(coord(Conj, _Type, _Index, LConjList, RConj, [])) :-
% 	format('~q: [ ', [Conj]),
% 	LConjList = [H|T],
% 	announce_lconj(T, H),
% 	format(' ]', []),
% 	announce_rconj(RConj).
% 
% announce_lconj([], H) :-
% 	announce_one_conj_element(H).
% announce_lconj([Next|T], H) :-
% 	announce_one_conj_element(H),
% 	format(',', []),
% 	announce_lconj(T, Next).
% 
% announce_rconj(RConj) :-
% 	announce_one_conj_element(RConj).
% 
% announce_one_conj_element(ConjElement) :-
% 	atomic(ConjElement),
% 	!,
% 	format(' ~w', [ConjElement]).
% announce_one_conj_element(ConjElement) :-
% 	get_from_list_if_possible(inputmatch, ConjElement, LexMatch),
% 	format(' ~w', [LexMatch]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% OBSOLETE!! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ---------- Add Index to Each Word ----------

% add_index_to_each_word(minimal_syntax(PhraseList),IndexIn,minimal_syntax(IndexedPhraseList)):-
%     !,add_index_to_each_word(PhraseList,IndexIn,IndexedPhraseList).
% 
% add_index_to_each_word( [], _Index, [] ) :- !.
% 
% add_index_to_each_word( [ ThisMSU | MoreMSUs ], Index, [ IndexedMSU | IndexedList ] ) :-
%     index_this_msu( ThisMSU, Index, IndexOut, IndexedMSU ),
%     add_index_to_each_word( MoreMSUs, IndexOut, IndexedList ).
% 
% index_this_msu( [], IndexIn, IndexIn, [] ) :-  !.
% 
% index_this_msu( [ confid( Confid ) | MoreItems ], IndexIn, NewIndex, 
%                 [confid( Confid )|IndexedMSU] ) :-
%     index_this_msu( MoreItems, IndexIn, NewIndex, IndexedMSU ), !.
% 
% index_this_msu( [ punc( Confid ) | MoreItems ], IndexIn, NewIndex, 
%                 [punc( Confid )|IndexedMSU] ) :-
%     index_this_msu( MoreItems, IndexIn, NewIndex, IndexedMSU ), !.
% 
% index_this_msu( [ ThisItem | MoreItems ], IndexIn, NewIndex, 
%     [ NewItem | IndexedMSU ] ) :-
% 
%     functor( ThisItem, Label, _Arity ),
%     arg( 1, ThisItem, ItemArg ),
%     get_from_list(inputmatch,ItemArg,InputList),
%     length(InputList,InputLen),
%     Index is IndexIn + InputLen,
%     NewItemArg = [ index(Index)|ItemArg],
%     functor( NewItem, Label, 1),
%     arg( 1, NewItem, NewItemArg ),
%     index_this_msu(MoreItems,Index,NewIndex,IndexedMSU).

% remove MSU elements based on indexes
% used for Graciela's wsa phrases, such as 'on the other hand'. 'double-blind' etc.
remove_referential_features_with_index([], _IndexList, []) :- !.
remove_referential_features_with_index([ThisMSU|RestMSUs], IndexList, 
	                       [ModifiedMSU|RestModifiedMSUs]) :-
	remove_referential_features_with_index_aux(ThisMSU, IndexList, ModifiedMSU),
	remove_referential_features_with_index(RestMSUs, IndexList, RestModifiedMSUs).

remove_referential_features_with_index_aux([], _IndexList, []) :- !.
remove_referential_features_with_index_aux([ThisMSUElement|RestMSUElements], IndexList,
	                           [ModifiedMSUElement|RestModifiedMSUElements]) :-
	functor(ThisMSUElement, Functor,1),
	arg(1, ThisMSUElement, ArgList),
	get_from_list(index, ArgList, Index),
	index_match(Index, IndexList),
	!,
	remove_referential_features(ArgList, ModifiedArgList),
	functor(ModifiedMSUElement, Functor, 1),
	arg(1, ModifiedMSUElement, ModifiedArgList),
	remove_referential_features_with_index_aux(RestMSUElements, IndexList,
	                                   RestModifiedMSUElements).
remove_referential_features_with_index_aux([ThisMSUElement|RestMSUElements], IndexList,
	                           [ThisMSUElement|RestModifiedMSUElements]) :-
	remove_referential_features_with_index_aux(RestMSUElements, IndexList,
	                                   RestModifiedMSUElements).

index_match(IndexIn, [FirstIndex-LastIndex|_RestIndexList]) :-
        ( IndexIn = _Index:First/Last ->
	  First >= FirstIndex,
	  LastIndex >= Last
        ; IndexIn >= FirstIndex,
	  LastIndex >= IndexIn
        ),
	!.
index_match(IndexIn, [_FirstIndex-_LastIndex|RestIndexList]) :-
	index_match(IndexIn, RestIndexList).

% used  to remove metaconc and related features
% used before calling metamap again in domain processing and gene concept identification 
% how about shapes?
remove_referential_features_from_element([],[]).
remove_referential_features_from_element([mod(ModList)|More], [mod(NewModList)|Gap]) :-	!,
	remove_referential_features(ModList, NewModList),
	remove_referential_features_from_element(More, Gap).
remove_referential_features_from_element([head(HeadList)|More], [head(NewHeadList)|Gap]) :-
	!,
	remove_referential_features(HeadList, NewHeadList),
	remove_referential_features_from_element(More,Gap).
% MetaMap sometimes puts the conc in prep()!!
remove_referential_features_from_element([prep(PrepList)|More], [prep(NewPrepList)|Gap]) :-
	!,
	remove_referential_features(PrepList, NewPrepList),
	remove_referential_features_from_element(More, Gap).
% Added for macro_NP issue <Halil/>
remove_referential_features_from_element([verb(VerbList)|More], [verb(NewVerbList)|Gap]) :-
	!,
	remove_referential_features(VerbList,NewVerbList),
	remove_referential_features_from_element(More, Gap).
remove_referential_features_from_element([confid(_Confid)|More], Gap) :-
	!,
	remove_referential_features_from_element(More, Gap).
remove_referential_features_from_element([NoMetaConc|More], [NoMetaConc|Gap]) :-
	remove_referential_features_from_element(More, Gap).

% ---

remove_referential_features([], []).
remove_referential_features([metaconc(_MetaConcList)|More], Gap) :-
	!,
	remove_referential_features(More, Gap).
remove_referential_features([ausemtype(_AUSemTypeList)|More], Gap) :-
	!,
	remove_referential_features(More, Gap).
remove_referential_features([usemtype(_USemTypeList)|More], Gap) :-
	!,
	remove_referential_features(More, Gap).
remove_referential_features([semgroup(_SemGroup)|More], Gap) :-
	!,
	remove_referential_features(More, Gap).
% not sure why this is needed, ask Francois
remove_referential_features([tokens([])|More],Gap) :-
	!,
	remove_referential_features(More, Gap).
remove_referential_features([genphenom(_GenPhenom)|More], Gap) :-
	!,
	remove_referential_features(More, Gap).
remove_referential_features([NoMetaConc|More], [NoMetaConc|Gap]) :-
	remove_referential_features(More, Gap).

remove_entire_referential_analysis([],[]) :- !.
remove_entire_referential_analysis([MSU|More], [MSUOut|Gap]) :-
	remove_referential_features_from_element(MSU,MSUOut),
	remove_entire_referential_analysis(More,Gap).

% <Halil> moved from skr_fe.pl, new SKR does not have it. </Halil>
% create_citation_text_atom(Utterances, CitationTextAtom) :-
%         Utterances = [FirstUtterance|RestUtterances],
%         get_all_citation_text(RestUtterances, FirstUtterance, CitationTextStrings),
%         append(CitationTextStrings, CitationTextSingleString),
%         atom_codes(CitationTextAtom, CitationTextSingleString).

% get_all_citation_text([], LastUtterance, [LastUtteranceText]) :-
%         LastUtterance = utterance(_Label, LastUtteranceText).
% get_all_citation_text([NextUtterance|RestUtterances], Utterance,
%                       [UtteranceText, " "|RestUtteranceText]) :-
%         Utterance = utterance(_Label, UtteranceText),
%         get_all_citation_text(RestUtterances, NextUtterance, RestUtteranceText).
 
 
% <Halil> moved from metamap_utilities.pl </Halil>
% get_utterance(InputStream,TagLabel,Utterance) :-
%     repeat,
%     (fread_term(InputStream,UttTerm) ->
%         (UttTerm=utterance(Label,String,UttStartPos/_UttLen,_ReplPos) ->
%             get_phrases(InputStream,Phrases),
%             (Phrases==[] ->
%                 format('~NERROR: (get_utterance/3) No phrases found for ~p.~n',
%                        [Label]),
% 		!,
%                 fail
%             ;   true
%             ),
%             Label=TagLabel,
%             Utterance=utterance(Label,String,UttStartPos,Phrases)
% %        ;   format('~NERROR: (get_utterance/3) Expecting an utterance/2 term; found~n~p~n',
% %                   [UttTerm]),
% %	    !,
% %            fail
%         )
%     ;   (var(TagLabel) ->
%             true
%         ;   format('~NERROR: (get_utterance/3) Cannot find utterance for ~p.~n',[TagLabel])
%         ),
%         !,
%         fail
%     ),
%     !.

% get_phrases(InputStream,Phrases) :-
%     fread_term(InputStream,PhraseTerm),
%     (PhraseTerm=='EOU' ->
%         Phrases=[]
%     ;   PhraseTerm=phrase(Text,Syntax,_PosInfo,_ReplPos),
%         (fread_term(InputStream,candidates(Candidates)) ->
%             (fread_term(InputStream,mappings(Mappings)) ->
%                 Phrases=[phrase(Text,Syntax,Candidates,Mappings)|RestPhrases],
%                 get_phrases(InputStream,RestPhrases)
%             ;   format('~NERROR: (get_phrases/2) Missing mappings for "~p".~n',
%                        [Text]),
%                 fail
%             )
%         ;   format('~NERROR: (get_phrases/2) Missing candidates for "~p".~n',
%                    [Text]),
%             fail
%         )
%     ),
%     !.
 
% <Halil> from nls_io.pl </Halil>
/* fread_term(+Stream, -Term)

fread_term/2 reads a Term from Stream.  It fails at end-of-file.  */

% fread_term(Stream,_Term) :-
%     at_end_of_stream(Stream),
%     !, 
%     fail.
% fread_term(Stream,Term) :-
%     read(Stream,Term),
%     \+ Term==end_of_file,
%     !.

% <Halil> from text_objects.pl </Halil>
% merge_sentences(Sentences, MergedSentences) :-
% 	Sentences = [FirstToken|RestTokens],
% 	merge_sentences_aux(RestTokens, FirstToken, MergedSentences).
% 
% merge_sentences_aux([], LastToken, [LastToken]).
% merge_sentences_aux([NextToken|RestTokens], ThisToken, MergedTokens) :-
% 	% Two consecutive an tokens: merge them into the second.
% 	% Still need to merge token types!!
% 	( an_tok(ThisToken),
% 	  an_tok(NextToken) ->
% 	  merge_tokens(ThisToken, NextToken, NewNextToken),
% 	  merge_sentences_aux(RestTokens, NewNextToken, MergedTokens)
% 	; MergedTokens = [ThisToken|RestMergedTokens],
% 	  merge_sentences_aux(RestTokens, NextToken, RestMergedTokens)
% 	).


% merge_tokens(ThisToken, NextToken, NewNextToken) :-
% 	ThisToken = tok(ThisType, ThisTokenText, _ThisLCText,
% 			pos(ThisStart1,_ThisEnd1),
% 			pos(ThisStart2,_ThisEnd2)),
% 	NextToken = tok(NextType, NextTokenText, _NextLCText,
% 			pos(_NextStart1,NextEnd1),
% 			pos(_NextStart2,NextEnd2)),
% 	append(ThisTokenText, NextTokenText, MergedText),
% 	lower(MergedText, LCMergedText),
% 	NewStart1 is ThisStart1,
% 	NewEnd1 is NextEnd1,
% 	NewStart2 is ThisStart2,
% 	NewEnd2 is NextEnd2,
% 	merge_token_types(ThisType, NextType, NewType),
% 	NewNextToken = tok(NewType, MergedText, LCMergedText,
% 			   pos(NewStart1,NewEnd1),
% 			   pos(NewStart2,NewEnd2)).
	

% What is the resulting token type if two tokens
% of types ThisType and NextType are concatenated?
% merge_token_types(lc, NextType, NewType) :- merge_token_types_lc(NextType, NewType).
% merge_token_types(uc, NextType, NewType) :- merge_token_types_uc(NextType, NewType).
% merge_token_types(ic, NextType, NewType) :- merge_token_types_ic(NextType, NewType).
% merge_token_types(mc, NextType, NewType) :- merge_token_types_mc(NextType, NewType).
% merge_token_types(an, NextType, NewType) :- merge_token_types_an(NextType, NewType).
% merge_token_types(nu, NextType, NewType) :- merge_token_types_nu(NextType, NewType).
% 
% merge_token_types_lc(lc, lc).
% merge_token_types_lc(uc, mc).
% merge_token_types_lc(ic, mc).
% merge_token_types_lc(mc, an).
% merge_token_types_lc(an, an).
% merge_token_types_lc(nu, an).
% 
% merge_token_types_uc(lc, mc).
% merge_token_types_uc(uc, uc).
% merge_token_types_uc(ic, mc).
% merge_token_types_uc(mc, mc).
% merge_token_types_uc(an, an).
% merge_token_types_uc(nu, an).
% 
% merge_token_types_ic(lc, ic).
% merge_token_types_ic(uc, mc).
% merge_token_types_ic(ic, mc).
% merge_token_types_ic(mc, mc).
% merge_token_types_ic(an, an).
% merge_token_types_ic(nu, an).
% 
% merge_token_types_mc(lc, mc).
% merge_token_types_mc(uc, mc).
% merge_token_types_mc(ic, mc).
% merge_token_types_mc(mc, mc).
% merge_token_types_mc(an, an).
% merge_token_types_mc(nu, an).
% 
% merge_token_types_an(lc, an).
% merge_token_types_an(uc, an).
% merge_token_types_an(ic, an).
% merge_token_types_an(mc, an).
% merge_token_types_an(an, an).
% merge_token_types_an(nu, an).
% 
% merge_token_types_nu(lc, an).
% merge_token_types_nu(uc, an).
% merge_token_types_nu(ic, an).
% merge_token_types_nu(mc, an).
% merge_token_types_nu(an, an).
% merge_token_types_nu(nu, nu).

% trim_all_whitespace([],[]) :- !.
% trim_all_whitespace([First|Rest],[TrimmedFirst|TrimmedRest]) :-
% 	trim_whitespace(First,TrimmedFirst),
% 	!,
% 	trim_all_whitespace(Rest,TrimmedRest).

% We should not really need this, but metamap machine output keeps changing
% and we need to be able to convert it to the format SemRep expects.
% Currently (late 2012), this means making the confidence scores positive numbers,
% and ensuring that metaconc terms has as arguments a list of metaconcs.
% But this might change in the subsequent versions.

% fix_metamap_analysis needs to change because in wsdmod.pl,
% I now call extract_SemRep_phrases on APhrases, and not [APhrases0].

fix_metamap_analysis(minimal_syntax(Analysis0), minimal_syntax(Analysis)) :-
	fix_metamap_analysis_aux(Analysis0, Analysis).

fix_metamap_analysis_aux([], []).
fix_metamap_analysis_aux([[]|RestLists], RestModifiedLists) :-
	fix_metamap_analysis_aux(RestLists, RestModifiedLists).
fix_metamap_analysis_aux([ThisList|RestLists], [ThisModifiedList|RestModifiedLists]) :-
	fix_metamap_analysis_msu_list(ThisList, ThisModifiedList),
	fix_metamap_analysis_aux(RestLists, RestModifiedLists).

fix_metamap_analysis_msu_list([], []).
fix_metamap_analysis_msu_list([ThisMSU|RestMSUs], [FixedThisMSU|FixedRestMSUs]) :-
	fix_metamap_analysis_msu(ThisMSU, FixedThisMSU),
	fix_metamap_analysis_msu_list(RestMSUs, FixedRestMSUs).

fix_metamap_analysis_msu([], []).
fix_metamap_analysis_msu([confid(NegScore)|RestElement], [confid(Score)|RestModifiedElement]) :-
	!,
	Score is -NegScore,
	fix_metamap_analysis_msu(RestElement, RestModifiedElement).
fix_metamap_analysis_msu([ThisElement|RestElement], [ModifiedElement|RestModifiedElement]) :-
	functor(ThisElement, Functor, 1),
	arg(1, ThisElement, ArgList),
	get_from_list(metaconc, ArgList, _MetaConcList),
	!,
	fix_metaconcs(ArgList, ModifiedArgList),
	functor(ModifiedElement, Functor, 1),
	arg(1, ModifiedElement, ModifiedArgList),
	fix_metamap_analysis_msu(RestElement, RestModifiedElement).
fix_metamap_analysis_msu([ThisElement|RestElement], [ThisElement|RestModifiedElement]) :-
	fix_metamap_analysis_msu(RestElement, RestModifiedElement).

fix_metaconcs([], []) :- !.
fix_metaconcs([metaconc(MetaConcList)|RestArgs], 
	      [metaconc([MetaConcList])|RestModifiedArgs]) :-
	!,
	fix_metaconcs(RestArgs, RestModifiedArgs).
fix_metaconcs([metaconc(MetaConcList)|RestArgs], 
	      [metaconc(ModifiedMetaConcList)|RestModifiedArgs]) :-
	!,
	fix_metaconc(MetaConcList, ModifiedMetaConcList),
	fix_metaconcs(RestArgs, RestModifiedArgs).
fix_metaconcs([Feature|RestArgs], [Feature|RestModifiedArgs]) :-
	fix_metaconcs(RestArgs, RestModifiedArgs).

fix_metaconc([],[]) :- !.
fix_metaconc(['':Concept:SemTypeList|RestMetaConc],
	     [Concept:'':SemTypeList|RestModifiedMetaConcs]) :-
	!,
	fix_metaconc(RestMetaConc, RestModifiedMetaConcs).
fix_metaconc([MetaConc|RestMetaConc], [MetaConc|RestModifiedMetaConcs]) :-
	fix_metaconc(RestMetaConc,RestModifiedMetaConcs).

% set_global_module_version(Version) :- 
% 	( global_module_version(_) ->
% 	  retract(global_module_version(_))
% 	; true
% 	)
% 	,
% 	assert(global_module_version(Version)).

%%% DOMAIN-CHOICE LOGIC begins here
% There are three files that exist in both GENeric and DOMain flavors:
% exceptions, locsemnet, and semrules.

% (1) The logic for locsemnet and semrules is:
%  * If running DOMain SemRep, call the DOM version first,
%    and if that fails, call the GEN version.
%  * If running GENeric SemRep, call the GEN version only.

% (2) The logic for exceptions is
%  * If running DOMain SemRep,  call the DOM version only.
%  * If running GENeric SemRep, call the GEN version only.

% local_semnet is part of semnet, so the first set of rules applies.
local_semnet_BOTH(SubjectSemType, Relation, ObjectSemType) :-
	( control_value(domain, _Domain),
	  local_semnet_DOM(SubjectSemType, Relation, ObjectSemType) ->
	  true
	; local_semnet_GEN(SubjectSemType, Relation, ObjectSemType)
	).

% conditional_empty_head_base_1 is part of exceptions, so the second set of rules applies.
conditional_empty_head_base_1_BOTH(FirstBase,SemGroups) :-
	( control_value(domain, _Domain) ->
	  conditional_empty_head_base_1_DOM(FirstBase,SemGroups)
	; conditional_empty_head_base_1_GEN(FirstBase,SemGroups)
	).

% conditional_empty_head_base_2N is part of exceptions, so the second set of rules applies.
conditional_empty_head_base_2N_BOTH(FirstBase,RestBases,SemGroups) :-
	( control_value(domain, _Domain) ->
	  conditional_empty_head_base_2N_DOM(FirstBase,RestBases,SemGroups)
	; conditional_empty_head_base_2N_GEN(FirstBase,RestBases,SemGroups)
	).

% empty_head_base_1 is part of exceptions, so the second set of rules applies.
empty_head_base_1_BOTH(FirstBase) :-
	( control_value(domain, _Domain) ->
	  empty_head_base_1_DOM(FirstBase)
	; empty_head_base_1_GEN(FirstBase)
	).

% empty_head_base_2N is part of exceptions, so the second set of rules applies.
empty_head_base_2N_BOTH(FirstBase, [NextWord|RestWords]) :-
	( control_value(domain, _Domain) ->
	  empty_head_base_2N_DOM(FirstBase, [NextWord|RestWords])
	; empty_head_base_2N_GEN(FirstBase, [NextWord|RestWords])
	).
