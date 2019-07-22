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

% File:	    metamap_utilities.pl
% Module:   MetaMap
% Author:   Lan
% Purpose:  MetaMap utility predicates


:- module(metamap_utilities, [
	% must be exported for mm_print
	build_concept_name_1/5,
	candidate_term/16,
	% dump_aphrase_mappings/3,
	% dump_evaluations_indented/6,
	% must be exported for mm_print
	dump_evaluations_indented/7,
	dump_mappings/3,
	dump_variants_labelled/5,
	extract_nonexcluded_sources/3,
	extract_relevant_sources/3,
	extract_name_in_source/2,
	% extract_unique_sources/2,
	num_dump_evaluations_indented/8,
	positions_overlap/2,
	wgvcs/1,
	wl/1,
	write_avl_list/1,
	write_list_indented/1
    ]).

:- use_module(skr(skr_umls_info), [
	convert_to_root_sources/2
    ]).

:- use_module(skr(skr_utilities), [
	ensure_atom/2,
	fatal_error/2,
	get_candidate_feature/3,
	get_all_candidate_features/3
    ]).

:- use_module(library(codesio), [
	write_term_to_codes/3
   ]).

:- use_module(library(file_systems), [
	file_exists/1
   ]).

:-   absolute_file_name(skr_lib(semtype_translation_2017AA),
			AbsFileName,
			[extensions(['.pl'])]),
     file_exists(AbsFileName) ->
     use_module(skr_lib(semtype_translation_2017AA), [expand_semtypes/2]),
     format(user_error, 'File semtype_translation_2017AA.pl is loading ~w~n', [AbsFileName])
   ; format(user_error, 'File semtype_translation_2017AA.pl is NOT loading ~n', []).

:- use_module(skr_lib(nls_system), [
	control_option/1,
	control_value/2
    ]).

:- use_module(skr_lib(nls_strings), [
	concatenate_items_to_atom/2,
	split_string_completely/3,
	trim_whitespace/2
    ]).

:- use_module(skr_lib(sicstus_utils), [
	concat_atom/2,
	concat_atom/3
    ]).

:- use_module(library(lists), [
	append/2,
	is_list/1,
	rev/2
    ]).

:- use_module(library(lists3), [
	substitute/4
    ]).


/* ************************************************************************
   ************************************************************************
   ************************************************************************
                            MetaMap Utility Predicates
   ************************************************************************
   ************************************************************************
   ************************************************************************ */


/* positions_overlap(+Position1, +Position2)

positions_overlap/2
*/

positions_overlap([Begin1,End1], [Begin2,End2]) :-
	( Begin1 =:= 0,
	  End1 =:= -1 ->
	  fail
	; Begin2 =:= 0,
	  End2 =:= -1 ->
	  fail
	; Begin1 =< Begin2,
	  End1 >= Begin2 ->
	  true
	; Begin2 < Begin1,
	  End2 >= Begin1
	).

/* dump_aphrase_mappings(+APhrases, +Label)
   dump_aphrase_mappings_aux(+APhrases, +Label)
   dump_mapping(+Mapping, +Label, +Value)

dump_aphrase_mappings/2
dump_aphrase_mappings_aux/2
dump_mapping/2
*/

maybe_display_META_header(FormatString, Args) :-
	( control_option(pipe_output) ->
	  true
	; format(FormatString, Args)
	).

dump_mappings([], _UtteranceLabel, Label) :-
	% format('Meta ~a: <none>~n', [Label]).
	maybe_display_META_header('Meta ~a: <none>~n', [Label]).
dump_mappings([H|T], UtteranceLabel, Label) :-
	Mappings = [H|T],
	( control_option(number_the_mappings) ->
	  Counter is 1
	; Counter is 0
	),
	dump_mappings_aux(Mappings, UtteranceLabel, Label, Counter).

dump_mappings_aux([], _UtteranceLabel, _Label, _Counter).
dump_mappings_aux([Mapping|RestMappings], UtteranceLabel, Label, Counter) :-
	Mapping = map(NegValue, Evaluations),
	Score is -NegValue,
	get_display_and_increment_counter(Counter, CounterDisplay, NextCounter),
	dump_mapping(Evaluations, UtteranceLabel, Label, Score, CounterDisplay),
	dump_mappings_aux(RestMappings, UtteranceLabel, Label, NextCounter).

% dump_aphrase_mappings([], _UtteranceLabel, Label) :-
% 	% format('Meta ~a: <none>~n', [Label]).
% 	maybe_display_META_header('Meta ~a: <none>~n', [Label]).
% dump_aphrase_mappings([H|T], UtteranceLabel, Label) :-
% 	APhrases = [H|T],
% 	( control_option(number_the_mappings) ->
% 	  Counter is 1
% 	; Counter is 0
% 	),
% 	dump_aphrase_mappings_aux(APhrases, UtteranceLabel, Label, Counter).
% 
% dump_aphrase_mappings_aux([], _UtteranceLabel, _Label, _Counter).
% dump_aphrase_mappings_aux([ap(NegValue,_,_,Mapping)|Rest], UtteranceLabel, Label, Counter) :-
% 	Score is -NegValue,
% 	get_display_and_increment_counter(Counter, CounterDisplay, NextCounter),
% 	dump_mapping(Mapping, UtteranceLabel, Label, Score, CounterDisplay),
% 	dump_aphrase_mappings_aux(Rest, UtteranceLabel, Label, NextCounter).

get_display_and_increment_counter(Counter, CounterDisplay, NextCounter) :-
	( Counter =:= 0 ->
	  CounterDisplay = '',
	  NextCounter is 0
	; ensure_atom(Counter, CounterAtom),
	  % atom_concat/3 is a SICStus Prolog built-in predicate
	  atom_concat(CounterAtom, '. ', CounterDisplay),
	  NextCounter is Counter + 1
	).	  

dump_mapping([], _UtteranceLabel, Label, _Score, _DisplayCounter) :-
	% format('Meta ~a: <none>~n', [Label]).
	maybe_display_META_header('Meta ~a: <none>~n', [Label]).
dump_mapping([H|T], UtteranceLabel, Label, Score, DisplayCounter) :-
	Mapping = [H|T],
	% format('~wMeta ~a (~d):~n', [DisplayCounter,Label,Score]),
	maybe_display_META_header('~wMeta ~a (~d):~n', [DisplayCounter,Label,Score]),
	dump_evaluations(Mapping, UtteranceLabel).

dump_evaluations([], _UtteranceLabel).
dump_evaluations([FirstEV|RestEVs], UtteranceLabel) :-
	RelatedEvaluations = [],
	num_dump_one_evaluation(FirstEV:RelatedEvaluations, 0, UtteranceLabel, _),
	dump_evaluations(RestEVs, UtteranceLabel).

% MetaMap now has status symbols :-)

% choose_status_symbol(_Status, _Negated, Symbol) :-
% 	control_option(silent),
% 	!,
% 	Symbol = ''.
%  0 means the candidate was kept, but possibly negated
choose_status_symbol(0, Negated, Symbol) :-
	( Negated == 1 ->
	  Symbol = 'N'
	; Symbol = ' '
	).
%  1 means the candidate was Excluded
choose_status_symbol(1, _Negated, 'E').
%  2 means the candidate was Pruned
choose_status_symbol(2, _Negated, 'P').

%%% dump_one_evaluation(Candidate:RelatedCandidates) :-
%%% 	get_all_candidate_features([negvalue,cui,metaterm,metaconcept,
%%% 				    semtypes,sources,status,negated],
%%% 				   Candidate,
%%% 				   [NegValue,CUI,MetaTerm,PreferredName,
%%% 				    SemTypes0,SourceInfo,Status,Negated]),
%%% 	CandidateScore is -NegValue,
%%% 	build_concept_name_1(PreferredName, CUI, SourceInfo, PreferredNameDisplay),
%%% 	choose_status_symbol(Status, Negated, StatusSymbol),
%%% 	display_concept_info(MetaTerm, PreferredName, CandidateScore,
%%% 			     CUI, PreferredNameDisplay, StatusSymbol),
%%% 	conditionally_expand_semtypes(SemTypes0),
%%% 	format('~n', []),
%%% 	dump_related_evaluations(RelatedCandidates).

%%% display_concept_info(MetaTerm, PreferredName, CandidateScore,
%%% 		     CUI, PreferredNameDisplayIn, StatusSymbol) :-
%%% 	( control_option(show_cuis) ->
%%% 	  DisplayCUI = CUI,
%%% 	  Colon = ':'
%%% 	; DisplayCUI = '',
%%% 	  Colon = ''
%%% 	),
%%% 	compute_preferred_name_display(MetaTerm, PreferredName,
%%% 				       PreferredNameDisplayIn, PreferredNameDisplay),
%%% 	format('~t~d ~w~8| ~w~w~w~p',
%%% 	       [CandidateScore,StatusSymbol,DisplayCUI,Colon,MetaTerm,PreferredNameDisplay]).

compute_preferred_name_display(MetaTerm, PreferredName, SourcesAtom,
			       PreferredNameDisplayIn, PreferredNameDisplay) :-
	( MetaTerm == PreferredName,
	  control_option(sources) ->
	  PreferredNameDisplay = SourcesAtom
	; MetaTerm == PreferredName,
	  control_option(pipe_output) ->
	  PreferredNameDisplay = MetaTerm
	; control_option(pipe_output) ->
	  PreferredNameDisplay = PreferredName
	; MetaTerm == PreferredName,
	  PreferredNameDisplay = ''  
	% ; control_option(show_preferred_names_only) ->
	%   PreferredNameDisplay = ''
	; concat_atom([' (',PreferredNameDisplayIn,')'], PreferredNameDisplay)
	).

conditionally_expand_semtypes(ShortSemTypes, LongSemTypes) :-
    	( \+ control_option(short_semantic_types) ->
	  expand_semtypes(ShortSemTypes, LongSemTypes)
	% format(' ~p', [SemTypes])
	; LongSemTypes = ShortSemTypes
	).

/* build_concept_name_1(+PreferredName, +CUI, +Sources, -PreferredNameDisplay) :-

build_concept_name/4 constructs PreferredNameDisplay from PreferredName by optionally
appending slist, the list of sources for PreferredNameDisplay. slist is appended when
-G (--sources) is active, and it is affected by options
-R (--restrict_to_sources) and -e (--exclude_sources). */

% This part is no longer relevant because Sources are now based on the String, and not the CUI
build_concept_name_1(PreferredName, _CUI, _Sources, _SourcesAtom, PreferredName) :-
	\+ control_option(sources),
	!.
build_concept_name_1(PreferredName, _CUI, OrigSources, SourcesAtom, PreferredNameDisplay) :-
%%% 	( control_option(restrict_to_sources) ->
%%% 	  control_value(restrict_to_sources, RestrictSources),
%%% 	  convert_to_root_sources(RestrictSources, RestrictRootSources),
%%% 	  extract_relevant_simple_sources(OrigSources, RestrictRootSources, NewSources)
%%% 	; control_value(exclude_sources, ExcludedSources),
%%% 	  convert_to_root_sources(ExcludedSources, ExcludedRootSources),
%%% 	  extract_nonexcluded_simple_sources(OrigSources, ExcludedRootSources, NewSources)
%%% 	; NewSources = OrigSources
%%% 	),
	concat_atom(OrigSources, ',', SourcesAtom0),
	concat_atom([' {', SourcesAtom0, '}'], SourcesAtom),
	concat_atom([PreferredName, SourcesAtom], PreferredNameDisplay).
	% build_concept_name_1_aux(OrigSources, PreferredName, SourcesAtom, PreferredNameDisplay),
	% !.
% should never occur
build_concept_name_1(PreferredName, CUI, _Sources, _SourcesAtom, _PreferredNameDisplay) :-
	fatal_error('Unable to build concept name for ~q/~q~n', [CUI,PreferredName]).

% build_concept_name_1_aux(Sources, PreferredName, SourcesAtom, PreferredNameDisplay) :-
% 	concat_atom(Sources, ',', SourcesAtom0),
% 	concat_atom([' {', SourcesAtom0, '}'], SourcesAtom),
% 	concat_atom([PreferredName, SourcesAtom], PreferredNameDisplay).

%%% extract_unique_sources(SourceInfo, UniqueSources) :-
%%% 	(  foreach([_I, _Str, SRC, _TTY], SourceInfo),
%%% 	   foreach(SRC, Sources)
%%% 	do true
%%% 	),
%%% 	sort(Sources, UniqueSources).

% build_list([], ListIn, ListOut) :-
% 	rev(ListIn, ListOut).
% build_list([First|Rest], ListIn, ListOut) :-
% 	build_list(Rest,[First,', '|ListIn], ListOut).

% must be exported for mm_print
dump_evaluations_indented(Candidates, TotalCandidateCount,
			  ExcludedCandidateCount, PrunedCandidateCount,
			  RemainingCandidateCount, Label, OutputStream) :-
	current_output(CurrentOutput),
	set_output(OutputStream),
	StartNum is 0,
	num_dump_evaluations_indented(Candidates, TotalCandidateCount, Label,
				      ExcludedCandidateCount, PrunedCandidateCount,
				      RemainingCandidateCount, StartNum, Label),
	!,
	set_output(CurrentOutput).

% This should never be called,
% because generate_candidates_output/6 tests for Evaluations3 \== []
%%% dump_evaluations_indented([], _TotalCandidateCount,
%%% 			  _ExcludedCandidateCount, _PrunedCandidateCount,
%%% 			  _RemainingCandidateCount, Label) :-
%%% 	format('Meta ~a (0): <none>~n', [Label]).
%%% dump_evaluations_indented([H|T], TotalCandidateCount,
%%% 			  ExcludedCandidateCount, PrunedCandidateCount,
%%% 			  RemainingCandidateCount, Label) :-
%%% 	Candidates = [H|T],
%%% 	( control_option(silent) ->
%%% 	  format('Meta ~a (~d):~n', [Label,TotalCandidateCount])
%%% 	; format('Meta ~a (Total=~d; Excluded=~d; Pruned=~d; Remaining=~d)~n',
%%% 		 [Label,TotalCandidateCount,
%%% 		  ExcludedCandidateCount,PrunedCandidateCount,
%%% 		  RemainingCandidateCount])
%%% 	),
%%% 	construct_related_evaluations(Candidates, CandidatePairs),
%%% 	dump_evaluations_indented_aux(CandidatePairs).

maybe_construct_related_evaluations(Candidates, CandidatePairs) :-
	( control_option(allow_overmatches) ->
	  (  foreach(C, Candidates),
	     foreach(CP, CandidatePairs)
	  do CP = C:[]
	  )
	; construct_related_evaluations(Candidates, CandidatePairs)
	).

construct_related_evaluations([], []).
construct_related_evaluations([FirstCandidate|RestCandidates],
			      [FirstCandidate:FirstRelated|RestRelated]) :-
	% Look for other ev() terms with the same preferred name
	get_candidate_feature(metaconcept, FirstCandidate, Concept),
	construct_related_evaluations_6(RestCandidates, Concept,
					[], RevFirstRelated, [], RevNewRest),
	rev(RevFirstRelated, FirstRelated),
	rev(RevNewRest, NewRest),
	construct_related_evaluations(NewRest, RestRelated).

construct_related_evaluations_6([], _Concept, RelatedIn, RelatedIn, UnrelatedIn, UnrelatedIn).
construct_related_evaluations_6([FirstCandidate|RestCandidates], Concept,
				RelatedIn, RelatedOut,
				UnrelatedIn, UnrelatedOut) :-
	get_candidate_feature(metaconcept, FirstCandidate, Concept),
	!,
	construct_related_evaluations_6(RestCandidates, Concept,
					[FirstCandidate|RelatedIn], RelatedOut,
					UnrelatedIn, UnrelatedOut).
construct_related_evaluations_6([FirstCandidate|RestCandidates], Concept,
				RelatedIn, RelatedOut, UnrelatedIn, UnrelatedOut) :-
	construct_related_evaluations_6(RestCandidates, Concept,
					RelatedIn, RelatedOut,
					[FirstCandidate|UnrelatedIn], UnrelatedOut).

%%% dump_evaluations_indented_aux([]).
%%% dump_evaluations_indented_aux([FirstEV|RestEVs]) :-
%%% 	dump_one_evaluation(FirstEV),
%%% 	dump_evaluations_indented_aux(RestEVs).
%%% 
%%% dump_related_evaluations([]).
%%% dump_related_evaluations([FirstCandidate|RestCandidates]) :-
%%% 	get_all_candidate_features([metaterm,status,negated],
%%% 				   FirstCandidate,
%%% 				   [MetaTerm,Status,Negated]),
%%% 	choose_status_symbol(Status, Negated, StatusSymbol),
%%% 	format('       ~w   ~p~n', [StatusSymbol,MetaTerm]),
%%% 	dump_related_evaluations(RestCandidates).

% This should never be called,
% because generate_candidates_output/6 tests for Evaluations3 \== []
num_dump_evaluations_indented([], _TotalCandidateCount, _UtteranceLabel,
			      _ExcludedCandidateCount, _PrunedCandidateCount,
			      _RemainingCandidateCount, _StartNum, Label) :-
	 %  format('Meta ~a (0): <none>~n', [Label]).
	 maybe_display_META_header('Meta ~a (0): <none>~n', [Label]).
num_dump_evaluations_indented([H|T], TotalCandidateCount, UtteranceLabel,
			      ExcludedCandidateCount, PrunedCandidateCount,
			      RemainingCandidateCount, StartNum, Label) :-
	Candidates = [H|T],
	% format('Meta ~a (Total=~d; Excluded=~d; Pruned=~d; Remaining=~d)~n',
	maybe_display_META_header('Meta ~a (Total=~d; Excluded=~d; Pruned=~d; Remaining=~d)~n',
				  [Label,TotalCandidateCount,
				   ExcludedCandidateCount,PrunedCandidateCount,
				   RemainingCandidateCount]),
	maybe_construct_related_evaluations(Candidates, CandidatePairs),
	num_dump_evaluations_indented_aux(CandidatePairs, StartNum, UtteranceLabel).

num_dump_evaluations_indented_aux([], _Num, _UtteranceLabel).
num_dump_evaluations_indented_aux([Candidate:RelatedCandidates|Rest], N, UtteranceLabel) :-
	num_dump_one_evaluation(Candidate:RelatedCandidates, N, UtteranceLabel, NextN),
	num_dump_evaluations_indented_aux(Rest, NextN, UtteranceLabel).

num_dump_one_evaluation(Candidate:RelatedCandidates, N, UtteranceLabel, NextN) :-
	get_all_candidate_features([negvalue,cui,metaterm,metaconcept,
				    semtypes,sources,status,negated,posinfo],
				   Candidate,
				   [NegValue,CUI,MetaTerm,PreferredName,
				    ShortSemTypes,SourceInfo,Status,Negated,PosInfoList]),
	write_term_to_codes(PosInfoList, PosInfoCodesWithBrackets, []),
	append(["[", PosInfoCodes, "]"], PosInfoCodesWithBrackets),
	!,
	atom_codes(PosInfoAtom, PosInfoCodes),
	CandidateScore is -NegValue,
	build_concept_name_1(PreferredName, CUI, SourceInfo, SourcesAtom, PreferredNameDisplay),
	choose_status_symbol(Status, Negated, StatusSymbol),
	conditionally_expand_semtypes(ShortSemTypes, LongSemTypes),
	num_display_concept_info(N, Candidate, UtteranceLabel, MetaTerm, PreferredName, SourcesAtom,
				 CandidateScore, CUI, PreferredNameDisplay,
				 StatusSymbol, PosInfoAtom, LongSemTypes),
	% format('~n', []),
	get_next_N(N, N1),
	num_dump_related_evaluations(RelatedCandidates, N1, NextN).


get_next_N(N, N1) :-
	( N =:= 0 ->
	  N1 is 0
	; N1 is N + 1
	).

num_display_concept_info(N, _Candidate, UtteranceLabelAtom, MetaTerm, PreferredName,
			 SourcesAtom, CandidateScore, CUI, PreferredNameDisplayIn,
			 StatusSymbol, PosInfo, SemTypes) :-
	( control_option(show_cuis) ->
	  DisplayCUI = CUI,
	  Colon = ':'
	; control_option(pipe_output) ->
	  DisplayCUI = CUI,
	  Colon = ':'
	; DisplayCUI = '',
	  Colon = ''
	),
	compute_preferred_name_display(MetaTerm, PreferredName, SourcesAtom,
				       PreferredNameDisplayIn, PreferredNameDisplay),
	( control_option(pipe_output) ->
	  modify_utterance_label_display(UtteranceLabelAtom, ModUtteranceLabelAtom),
	  format('~w|~w|~w|~w|~w|~w|~w~n',
		 [MetaTerm,StatusSymbol,PreferredNameDisplay,
		  ModUtteranceLabelAtom,DisplayCUI,SemTypes,PosInfo])
	; N =:= 0 ->
	  format('~t~d ~w~8| ~w~w~w~p ~w~n',
		 [CandidateScore,StatusSymbol,DisplayCUI,Colon,
		  MetaTerm,PreferredNameDisplay,SemTypes])
	; format('~t~d~4|. ~t~d~10| ~t~w~12| ~p~w~p ~p ~w~n',
		 [N,CandidateScore,StatusSymbol,DisplayCUI,Colon,
		  MetaTerm,PreferredNameDisplay,SemTypes])
	).

% get_first_elements([], []).
% get_first_elements([H|T], [Interpolation|RestFirstElements]) :-
% 	H = [HFirstElement|_],
% 	HFirstElement = [FirstPos,LastPos],
% 	between:numlist(FirstPos, LastPos, Interpolation),
% 	get_first_elements(T, RestFirstElements).

modify_utterance_label_display(UtteranceLabelAtom, ModUtteranceLabelAtom) :-
        atom_codes(UtteranceLabelAtom, UtteranceLabelString),
        % Para will look like "para1.tx.2"; we care only about "para1"
        ( split_string_completely(UtteranceLabelString, "#", [Section,SubSection0,Para]) ->
          substitute(0'^, SubSection0, 0'., SubSection),
          split_string_completely(Para, ".", [ParaOnly|_]),
          append([Section, "|", SubSection, "|", ParaOnly], ModUtteranceLabelString),
          atom_codes(ModUtteranceLabelAtom, ModUtteranceLabelString)
        ; ModUtteranceLabelAtom = UtteranceLabelAtom
        ).


% modify_utterance_label_display(UtteranceLabelAtom, ModUtteranceLabelAtom) :-
%         atom_codes(UtteranceLabelAtom, UtteranceLabelString),
%         % Para will look like "para1.tx.2"; we care only about "para1"
%         % No more "para"s.
%         % Subsection will look like "6^3 Immunogenicity.tx.1";
% 	% we must get rid of the ".tx.1".
%         ( split_string_completely(UtteranceLabelString, "#", [Section,SubSection0]) ->
%           split_string_completely(SubSection0, ".", [SubSection1|_]),
%           substitute(0'^, SubSection1, 0'., SubSection),
%           append([Section, "|", SubSection], ModUtteranceLabelString),
%           atom_codes(ModUtteranceLabelAtom, ModUtteranceLabelString)
%         ; ModUtteranceLabelAtom = UtteranceLabelAtom
%         ).

num_dump_related_evaluations([], NIn, NIn).
num_dump_related_evaluations([FirstCandidate|RestCandidates], NIn, NOut) :-
	get_all_candidate_features([metaterm,status,negated],
				   FirstCandidate,
				   [MetaTerm,Status,Negated]),
	choose_status_symbol(Status, Negated, StatusSymbol),
	( NIn =:= 0 ->
	  format('       ~w   ~p~n',          [StatusSymbol,MetaTerm])
	; format('~t~d~4|.       ~w    ~p~n', [NIn,StatusSymbol,MetaTerm])
	),
	get_next_N(NIn, NInOut),
	num_dump_related_evaluations(RestCandidates, NInOut, NOut).

/* dump_variants_labelled(+Label, +Variants)

dump_variants_labelled/2
*/

dump_variants_labelled(Label, Variants, MaxNumDigits, CountIn, VariantsCount) :-
	length(Variants, VariantsCount),
	( VariantsCount =:= 0 ->
	  format('~a variants (n=0):~n<none>~n', [Label])
	; format('~a variants (n=~d):~n',[Label,VariantsCount]),
	  dump_variants(Variants, MaxNumDigits, CountIn)
	).

dump_variants([], _MaxNumDigits, _Count).
dump_variants([v(Word,VarLevel,Categories,History,_Roots,_NFR)|Rest], MaxNumDigits, CountIn) :-
	rev(History, RevHistory),
	number_codes(CountIn, CountInCodes),
	length(CountInCodes, CountInNumDigits),
	Padding is MaxNumDigits - CountInNumDigits,
	( Categories == [] ->
	  format('~d:~*c ~p{~d=~p}~n',
		 [CountIn,Padding,32,Word,VarLevel,RevHistory])
	; format('~d:~*c ~p{~p, ~d=~p}~n',
		 [CountIn,Padding,32,Word,Categories,VarLevel,RevHistory])
	),
	CountNext is CountIn + 1,
	dump_variants(Rest, MaxNumDigits, CountNext).

/* extract_relevant_sources(+SourceInfo, +Sources, -ExtractedSourceInfo)
   extract_nonexcluded_sources(+SourceInfo, +Sources, -ExtractedSourceInfo)

extract_relevant_sources/3 produces the subset ExtractedSourceInfo of
SourceInfo containing a source in Sources (which are root, or non-versioned,
sources).
extract_nonexcluded_sources/3 does the same thing except that it excludes
entries in SourceInfo containing one of Sources. */

extract_relevant_sources([], _, []).
extract_relevant_sources([[I,ConceptNameInSource,Source,TTY]|Rest],
			 Sources,
			 [[I,ConceptNameInSource,Source,TTY]|ExtractedRest]) :-
	memberchk(Source, Sources),
	!,
	extract_relevant_sources(Rest, Sources, ExtractedRest).
extract_relevant_sources([_|Rest], Sources, ExtractedRest) :-
	extract_relevant_sources(Rest, Sources, ExtractedRest).


% extract_relevant_simple_sources([], _, []).
% extract_relevant_simple_sources([FirstSource|RestSources], RestrictSources, ExtractedSources) :-
% 	( memberchk(FirstSource, RestrictSources) ->
% 	  ExtractedSources = [FirstSource|RestExtractedSources]
% 	; ExtractedSources = RestExtractedSources
% 	),
% 	extract_relevant_simple_sources(RestSources, RestrictSources, RestExtractedSources).
 
extract_nonexcluded_sources([], _, []).
extract_nonexcluded_sources([[I,ConceptNameInSource,Source,TTY]|Rest],
			    Sources,
			    [[I,ConceptNameInSource,Source,TTY]|ExtractedRest]) :-
	\+ memberchk(Source, Sources),
	!,
	extract_nonexcluded_sources(Rest, Sources, ExtractedRest).
extract_nonexcluded_sources([_|Rest], Sources, ExtractedRest) :-
	extract_nonexcluded_sources(Rest, Sources, ExtractedRest).

% extract_nonexcluded_simple_sources([] ,_, []).
% extract_nonexcluded_simple_sources([FirstSource|RestSources], ExcludeSources, ExtractedSources) :-
% 	( \+ memberchk(FirstSource, ExcludeSources) ->
% 	  ExtractedSources = [FirstSource|RestExtractedSources]
% 	; ExtractedSources = RestExtractedSources
% 	),
% 	extract_nonexcluded_simple_sources(RestSources, ExcludeSources, RestExtractedSources).
 
/* extract_name_in_source(+SourceInfo, -ConceptNameInSource)

extract_name_in_source/2 extracts ConceptNameInSource from the first entry in SourceInfo.
SourceInfo should never be empty; but if it is, ConceptNameInSource is set to
'<none>'. */

% 

% should never happen
extract_name_in_source([], '<none>').
extract_name_in_source([[_I,ConceptNameInSource,_Source,_TTY]|_Rest], ConceptNameInSource).

wgvcs([]).
wgvcs([First|Rest]) :-
    wgvc(First),
    wgvcs(Rest).

wgvc(gvc(G,Vs,Cs)) :-
    format(user_error, 'gvc:  ~p~n',[G]),
    wl(Vs),
    wl(Cs).

wl(X) :-
    var(X),
    !,
    format(user_error, '  <none>~n',[]).
wl([]).
wl([First|Rest]) :-
    format(user_error, '  ~p~n',[First]),
    wl(Rest).


write_avl_list([]).
write_avl_list([Key-Value|Rest]) :-
    format('~n~p~n',[Key]),
    (   is_vinfo_list(Value) ->
        write_vinfo_list(Value)
    ;   is_list(Value) ->
        wl(Value)
    ;   format('  ~p~n',[Value])
    ),
    write_avl_list(Rest).

is_vinfo_list([First|_]) :-
	First = vinfo(_Generator,_GenPos,_GenInvolvesHead,_Variant,_Words,_LastWord).

write_vinfo_list([]).
write_vinfo_list([vinfo(Generator,Position,InvolvesHead,Variant,Words,LastWord)|Rest]) :-
    format('  vinfo:~p~n',[Generator]),
    format('        ~p,~p~n',[Position,InvolvesHead]),
    format('        ~p~n',[Variant]),
    format('        ~p~n',[Words]),
    format('        ~p~n',[LastWord]),
    write_vinfo_list(Rest).

write_list_indented([]).
write_list_indented([First|Rest]) :-
    format('  ~p~n',[First]),
    write_list_indented(Rest).

% Create candidate term or extract features
candidate_term(NegValue, CUI, MetaTerm, PreferredName, MetaWords, SemTypes,
	       MatchMap, LSComponents, TargetLSComponent, InvolvesHead,
	       IsOvermatch, UniqueSources, PosInfo, Status, Negated, Evaluation) :-
	( Evaluation = ev(NegValue,CUI,MetaTerm,PreferredName,MetaWords,SemTypes,
			  MatchMap,LSComponents,TargetLSComponent,
			  InvolvesHead,IsOvermatch,UniqueSources,PosInfo,Status,Negated) ->
	  true
	  % This is simply for mm_print, which sees the printed representation of candidates,
	  % which does NOT include the LSComponents and TargetLSComponents fields.
	; Evaluation = ev(NegValue,CUI,MetaTerm,PreferredName,MetaWords,SemTypes,
			  MatchMap,InvolvesHead,IsOvermatch,UniqueSources,PosInfo,Status,Negated)
	).
