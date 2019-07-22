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

% File:	    interanalysis.pl
% Module:   interanalysis
% Author:   tcr
% Purpose:  Further syntactic analysis using real world knowledge


% ----- Module declaration and exported predicates

:- module( interanalysis, [
		intermediate_analysis/8,
		find_subordinator/3
   ]).

:- load_files( usemrep_lib(module_version),
               [ when(compile_time) ] ).

% ----- Imported predicates

:- use_module( skr_lib(nls_system), [
		control_option/1
   ]).

:- use_module( skr_lib(sicstus_utils), [
		concat_atom/2,
		concat_atom/3,			
		midstring/4
   ]).

:- use_module( library(lists), [
		is_list/1,
		last/2,
		nextto/3,
		rev/2
   ]).

:- use_module( library(sets), [
                intersect/2,
		intersection/3,
		list_to_set/2,
		union/3,
		subtract/3
   ]).

:- use_module( usemrep_domain(domain_data), [
                domain_semtype/3
   ]).

:- use_module( usemrep_lib(generic_domain), [
                domain_semtype/3
   ]).

:- use_module( usemrep_main(dumlex), [
		dummy_lexicon/4
   ]).


:- use_module( usemrep_lib(regexp), [
		predications/3,
		rearrangePredications/2,
		joinLists/2
	]).

:- use_module(usemrep_lib(module_version), [
		global_module_version/1
	]).

:- use_module( usemrep_lib(ssuppserv), [
		check_for_relativizer/1,
		check_for_complementizer/1,
		get_all_tokens/2,
		get_base/4,
%		get_head/2,
		get_left_partition/3,
		get_lexrec_for_cat/3,
		is_conditional_msu_empty_head/2,
		is_empty_head/1,
		is_msu_empty_head/1,
		locate_npu_head/3,
		main_verb/2,
		subordinating_conjunction/2
   ]).

% :- use_module( usemrep_lib(semrules), [
% 		word_corresponds_to_semnet_relation/4
%    ]).

:- use_module( skr_lib(nls_lists), [
		get_from_list/3
   ]).

:- use_module( skr_lib(nls_system), [
		control_value/2
   ]).

%:- use_versioned_module( usemrep_lib(semtype_translation),
%                         interanalysis, [
%                semtype_translation/2
%   ]).

:- use_module( usemrep_lib(semtype_translation), [
                semtype_translation/3
   ]).

% **************************************************************************************
% ************************************* INTERMEDIATE_ANALYSIS **************************
% **************************************************************************************

/*

INTERMEDIATE_ANALYSIS is intermediate between a strict syntactic and pure semantic analysis.
Essentially, it exploits semantic types to refine the underspecified syntactic analysis
imposed on the input during MINIMAL_COMMITMENT_ANALYSIS.  INTERMEDIATE_ANALYSIS is structure-
specific and currently concentrates on distinguishing present participles from gerunds and
on identifying left and right conjuncts in coordinate structures.

intermediate_analysis(+AnalysisWithSemTypes, +Definitions, +PrincipalDomain,
		      -ComparativePredications, -CompPredsTail,
		      -AdjustedAnalysisWithEmptyHeads, -ConjunctList)
*/


intermediate_analysis(AnalysisWithSemTypes, Definitions, PrincipalDomain,
		      ComparativePredications, CompPredsTail,
		      AdjustedAnalysisWithEmptyHeads, ConjunctList,
		      SubordinatorPredicateList)  :-
	AnalysisWithSemTypes = minimal_syntax(OrigAnalysis),
	reverse_EITHER_and_prep(OrigAnalysis, TweakedAnalysis),
	% The following call has been changed to include PrincipalDomain to take care of chem vs. tmod for aapp's.
	adjust_semtypes_in_analysis(TweakedAnalysis, PrincipalDomain, AdjustedAnalysis),
	PrevHeadEmpty is 0,
	TempPrevMSU = [],
	PrevMSU = _,
	AdjustedAnalysis = [H|T],
	set_empty_heads_in_analysis(T, H, PrevHeadEmpty,
	 			    TempPrevMSU, PrevMSU,
				    % There will always be a useless [] at the front of this list
	 			    [_|AdjustedAnalysisWithEmptyHeads]),
	% AdjustedAnalysisWithEmptyHeads = AdjustedAnalysis,
	rev(AdjustedAnalysisWithEmptyHeads, RevAdjustedAnalysisWithEmptyHeads),

	% This has been changed to traverse the entire sentence
	identify_comparatives(AdjustedAnalysisWithEmptyHeads, [],
			      RevAdjustedAnalysisWithEmptyHeads, Definitions,
			      CodeCompPreds, CompPredsTail, CodeConjunctList),
	% Now do the comparatives that are matched by patterns instead of code
	predications(comparison, AdjustedAnalysisWithEmptyHeads, CompPredSet),
	% In this case, predications returns a list of lists such as
	% CompPredSet = [[compare(a,b), coord(a,b)], [compare(x,y), coord(x,y)]]
	% First, join them all into one list
	joinLists(CompPredSet, DelistedPreds),
	% Then, -> [compare([compare(a,b),compare(x,y)]),coord([_,_])]
	rearrangePredications(DelistedPreds, PredsByPred),
	(	PredsByPred = [compare(COMPS), coord(RegexpCompCoords)],
		regexp:stripFunctors(COMPS, RegexpCompPreds),
		append(CodeCompPreds, RegexpCompPreds, ComparativePredications),
		append(CodeConjunctList, RegexpCompCoords, ConjunctList1)
	;
		PredsByPred == [],
		ComparativePredications = CodeCompPreds,
		ConjunctList1 = CodeConjunctList
	;
		format('~nERR: ~k should be of form [comparison(_), coord(_)]~n',
		       PredsByPred),
		fail
	),
	% format('~n~nComparativePredications: ~w~n~n', [ComparativePredications]),
	% format('~n~nBothConjuncLists:        ~w~n~n', [BothConjunctLists]),
	% identify_gerunds/2 is not capable of working, but it's been left in anyway
	% identify_gerunds(AnalysisWithSemTypes, EnhancedAnalysis0),
	EnhancedAnalysis0 = minimal_syntax(AdjustedAnalysisWithEmptyHeads),
	coordinate_adverbs_and_preps(EnhancedAnalysis0, EnhancedAnalysis1),
	consolidate_multi_word_conjunctions(EnhancedAnalysis1, EnhancedAnalysis2),
	% build_conjunct_list/2 used to be called coordination/2
	% -ConjunctList
	build_conjunct_list(EnhancedAnalysis2, PrincipalDomain, ConjunctList2),
	% Death to difference lists!
	append(ConjunctList1, ConjunctList2, ConjunctList),
	link_subordinators_to_predicates(RevAdjustedAnalysisWithEmptyHeads,
	                                 ConjunctList,SubordinatorPredicateList).

identify_comparatives([], _PrevMSU, _RevAnalysis, _Definitions,
		      ComparativePredications, ComparativePredications, []).
% CompPredsTail is a variable at this point.
% It will be bound to predications found later.
% CompConjunctList has been cured of this madness.
identify_comparatives([ThisMSU|RestMSUs], PrevMSU, RevAnalysis, Definitions,
		      ComparativePredications, CompPredsTail, CompConjunctList) :-
	% Is the current MSU a comparative indicator?
	% [compare|comparison]| [as|than|to] [versus|vs]
	% -NPArgType -Indicator -ComparisonType -OrigCueWord2
	% OrigCueWord2 will be set only for ComparisonType = np
	verify_comparative_indicator(ThisMSU, NP2ArgType, PrevMSU, RestMSUs,
%			Definitions, Indicator, ComparisonType,
	                Definitions, Indicator, IndicatorIndex, ComparisonType,
			% OrigCueWord2 returns the member of [as,than,to] found
			OrigCueWord2,
			% [as,than,to] is for type II: compared NP
			% "as" as in "Naproxin is as effective as aspirin"
			% Phil removed "same" after conferring with Marcello
			% "than" as in "Naproxen is more effective than aspirin"
			% "to" as in "Naproxen is superior to aspirin"
			[as,than,to,from]), % Bugzilla 22: added "from"
	% +ComparisonType +ThisMSU +PrevMSU +RestMSUs +NP2ArgType +OrigCueWord -PivotPoint
	% If ThisMSU is [as|than|to <head>], PivotPoint = ThisMSU
	% If ThisMSU is 'compare', PivotPoint = NP with cue in OrigCueWord2
	% If ThisMSU is 'versus', PivotPoint = next MSU, which must be an NP
	% Rest2 is the sentence including and to the right of the PivotPoint
	%		we want to resume scanning for comparatives after PivotPoint
	find_pivot_point(ComparisonType, ThisMSU, PrevMSU, RestMSUs,
	                 PivotPoint, NP2ArgType, OrigCueWord2, Rest2),
	get_left_partition(PivotPoint, RevAnalysis, LeftPartition),
	% Look for the next NP to the left of proper type: -NPArg1 -NP1ArgType
	find_previous_NP(LeftPartition, NPArg1, NP1ArgType),
	% NPArg2 := switch(CT) case compare: pivot point; case np: indicator MSU
	% +ComparisonType +PivotPoint +Indicator -NPArg2
	find_second_compared_NP(ComparisonType, PivotPoint, Indicator, NPArg2),
	% if comparative indicator is np, check for a comparative adj/adv immediately before
	% CueWord1 = higher/lower
	% +ComparisonType, +OrigCueWord2
	(ComparisonType \== np, CueWord1 = '<none>' ;
		(	verify_immediately_preceding_adj(LeftPartition, Definitions,
						_CompAdjLexMatch, CompAdjBase, CueWord1,
						OrigCueWord2, CueWord2),
			% "same as" or "<adj> than|to"
			verify_comparative_cueword_consistency(CueWord1, ComparisonType, CompAdjBase, CueWord2),
			!,
			% format('~n~nCueWord1: ~w~nCueWord2: ~w~n', [CueWord1,CueWord2]), 
			% translate e.g., "superior" to "good"
			scale_word_translation(CompAdjBase, ScaleWordTranslation),
			% translate e.g., "effective" to "effectiveness"
			get_scale_name(Definitions, ScaleWordTranslation, ScaleName)
		)
	),
% This causes a bug in the cases where Indicator is not the first token of a NP
% For instance, 'A immunogenicity comparative study', Index should be 4 not 1
%	get_first_index(Indicator, IndicatorIndex),

	% +NPArg1 +N1PArgType +II +NP2Arg2 +NP2ArgType +CueWord1 +CueWord2 +ScaleName
	% -ComparativePredications =CompPredsTail -CompConjunctList =CompConjList2
	form_comparative_results(NPArg1, NP1ArgType, IndicatorIndex, NPArg2, NP2ArgType,
				 CueWord1, CueWord2, ScaleName,
				 ComparativePredications, CompPreds2,
				 CompConjunctList, CompConjunctList2),
	!,     % go thru sentence only once
	% Continue looking for comparatives after the 2nd NP of this one
	% write_comparative_predications(ComparativePredications).
	% The last parm used to be CompConjunctListTail, but
	%		the deeply-nested difference lists caused me so much pain
	%		that I ripped them out. -psg
	identify_comparatives(Rest2, PivotPoint, RevAnalysis, Definitions,
			      CompPreds2, CompPredsTail, CompConjunctList2).

% If the current MSU is NOT a comparative indicator, just recurse
identify_comparatives([ThisMSU|RestMSUs], _PrevMSU, RevAnalysis, Definitions,
		      ComparativePredications, CompPredsTail, CompConjunctList) :-
	% pass ThisMSU in to recursive call, which will then before PrevMSU
	identify_comparatives(RestMSUs, ThisMSU, RevAnalysis, Definitions,
			      ComparativePredications, CompPredsTail, CompConjunctList).

% This predicate exists because of a recent change to metamap
% that causes "twice-daily clarithromycin" to be analyzed as
% [ punc([inputmatch([-]),tokens([])]),
%    mod([ ... lexmatch([twice,daily]),inputmatch([twice,daily]), ...]),
%    head([ ... lexmatch([aspirin]), ... ] ) ]
% We now have to find the first element in the indicator list that has an index
% in sentences such as
% 10399110.ab.6 This study showed that, in the treatment of acute sinusitis,
% daily levofloxacin therapy is as effective as twice-daily clarithromycin therapy
% with more complete clearing of symptoms and a more tolerable side-effect profile.

get_first_index(Indicator, IndicatorIndex) :-
	member(Element, Indicator),
	arg(1, Element, IndicatorList),
	get_from_list(index, IndicatorList, IndicatorIndex),
	!.

% write_comparative_predications(VarTail) :- var(VarTail), !.
% write_comparative_predications([H|T]) :-
% 	format('~n~w~n', [H]),
% 	write_comparative_predications(T).

% Try to determine if the adjective (e.g., "effective")
% has a corresponding nominalization (e.g., "effectiveness").
% If no nominalization is found, just use the adj lexmatch as the scale name
get_scale_name([], ScaleName, ScaleName).
get_scale_name([lexicon:LexList|_MoreLexicon], CompAdjBase, ScaleName) :-
	% get_from_list(lexmatch, LexList, [LexMatch]),
	% get the list of records for the lexical entry
	get_from_list(records, LexList, RecordList),
	% ensure that we have an adjective
	get_lexrec_for_cat(RecordList, adj, LexRecList),
	% whose base matches the base of the adjective in question
	get_from_list(base, LexRecList, [CompAdjBase]),
	!,
	get_from_list(entries, LexRecList, Entries),
	get_from_list(entry, Entries, Entry),
	get_from_list(nominalizations, Entry, NominalizationList),
	build_nominalization_if_possible(NominalizationList, CompAdjBase, ScaleName).
get_scale_name([_NoMatch|MoreLexicon], CompAdjLexMatch, ScaleName) :-
	get_scale_name(MoreLexicon, CompAdjLexMatch, ScaleName).

% NominzaliztionList comes in as a list of the form
% ['responsiveness|noun|E0053053','responsivity|noun|E0053054']
% from this list, create the atom 'responsiveness/responsivity'
build_nominalization_if_possible(NominalizationList, _CompAdjBase, Nominalization) :-
	get_nominalization_lexical_items(NominalizationList, [H|T]),
	!,
	% [H|T] is a list like [responsiveness, responsivity]
	% create the list [responsiveness, '/', responsivity]
	insert_char_between_atoms(T, H, '/', NominalizationTemp),
	% and just concatenate the atoms together
	% to get the atom 'responsiveness/responsivity'
	concat_atom(NominalizationTemp, Nominalization).
build_nominalization_if_possible(_, CompAdjBase, CompAdjBase).

get_nominalization_lexical_items([], []).
get_nominalization_lexical_items([NominalizationTerm|RestNominalizationTerms],
		     [NominalizationAtom|RestNominalizationAtoms]) :-
	get_nominalization_from_atom(NominalizationTerm, NominalizationAtom),
	get_nominalization_lexical_items(RestNominalizationTerms, RestNominalizationAtoms).

% from the atom 'responsiveness|noun|E0053053', extract the atom responsiveness
get_nominalization_from_atom(NominalizationTerm, Nominalization) :-
	atom_codes(NominalizationTerm, NominalizationTermChars),
	append(NominalizationChars, [124|_Rest], NominalizationTermChars),
	atom_codes(Nominalization, NominalizationChars).

% find_second_compared_NP(+ComparisonType, +PivotPoint, +Indicator, -NPArg2)
% If the ComparisonType is "compare" or "versus", then 2nd NP arg := pivot point
find_second_compared_NP(compare, NPArg2,      _Indicator, NPArg2).
find_second_compared_NP(versus,  NPArg2,      _Indicator, NPArg2).

% if the ComparisonType is "np",      then 2nd NP arg := the indicator 
find_second_compared_NP(np,      _PivotPoint, NPArg2,     NPArg2).

% If the ComparisonType is "np", then the Indicator is the Pivot Point;
%		resume scanning for comparatives after the Indicator.
find_pivot_point(np,       ThisMSU, _PrevMSU, RestMSUs, ThisMSU, _NPArgType,
_CueWord2, RestMSUs).

% If the ComparisonType IS "compare", then look for the next compared NP
% If the cue is a prep (with, to), it will be in the same MSU as the 2nd NP
% If the cue is a conj (and, vs), it will be in the MSU before the 2nd NP
find_pivot_point(compare, _ThisMSU, PrevMSU, RestMSUs,
                 NPArg2, NPArgType, CueWord2, Rest2) :-
	find_next_compared_NP(prep, RestMSUs, PrevMSU, NPArg2, NPArgType,
			      _SemanticGroup2, [with,to], CueWord2, Rest2);	
	find_next_compared_NP(conj, RestMSUs, PrevMSU, NPArg2, NPArgType,
			      _SemanticGroup2, [and,vs,versus], CueWord2, Rest2).


% For versus, only the very next MSU is an acceptable NP.
% This is because it doesn't have a cue word.
% NPArgType must be head
find_pivot_point(versus, _ThisMSU, _PrevMSU, [NPArg2 | RestMSUs],
                 NPArg2, NPArgType, _CueWord2, RestMSUs) :-
	is_an_NP(NPArg2, SemanticGroup, NPArgType),
	% Check that the NPU has a semgroup in this list
	% Perhaps either this, or the requirement that the pivot point
	%		be the very next MSU, could be eliminated.
	intersect([tmod], SemanticGroup).

% look for, e.g., "than aspirin" or "as aspirin"
% either in one MSU (prep) or two consecutive MSUs (prep/conj) + NP
% and then verify that an adj or adv is immediately before
% verify_comparative_indicator(ThisMSU, PrevMSU, [NextMSU1|RestMSUs],

% Type I of comparative indicator is some form of the word "compare"
verify_comparative_indicator(ThisMSU, _NPArgType, _PrevMSU, _RestMSUs, Definitions,
%			   ThisMSU, compare, _CueWord, _ValidCueWords) :-
			   ThisMSU, Index, compare, _CueWord, _ValidCueWords) :-
  % True iff base of ThisMSU is "compare" or "comparison"
	% (don't know why "compare" isn't the base of "comparison")
 	verify_comparative_compare_word(ThisMSU, Definitions, Index),
	!.

% Type II of comparative indicator is a compared NP
% -NPArgType +RestMSUs +Definitions -Indicator
verify_comparative_indicator(ThisMSU, NPArgType, PrevMSU, RestMSUs,
%			     Definitions, Indicator, np, CueWord2, ValidCueWords) :-
			     Definitions, Indicator, Index, np, CueWord2, ValidCueWords) :-
	verify_second_compared_NP(ThisMSU, NPArgType, PrevMSU, RestMSUs,
				  Definitions, Indicator, Index, SemanticGroup2, CueWord2),
	memberchk(CueWord2, ValidCueWords),
	intersect([tmod], SemanticGroup2).

% Type III of comparative indicator is the word "versus"
verify_comparative_indicator(ThisMSU, _NPArgType, _PrevMSU, _RestMSUs,
%		Definitions, ThisMSU, versus, _CueWord, _ValidCueWords) :-
		Definitions, ThisMSU, Index, versus, _CueWord, _ValidCueWords) :-
 	verify_comparative_versus(ThisMSU, Definitions, Index),
	!.


verify_immediately_preceding_adj(LeftPartition, Definitions,
					_CompAdjLexMatch, CompAdjBase, CueWord1,
					OrigCueWord2, CueWord2) :-
	!,
        LeftPartition = [MSU1,MSU2,MSU3|Rest],
	( Adj = MSU1, Prev = MSU2
	; Adj = MSU2, Prev = MSU3 )
	,
	( verify_comparative_standalone_prep_or_conj(Prev, OrigCueWord1),
	  verify_comparative_adjective1(Adj, Definitions, CompAdjBase, OrigCueWord0) ->
	  true
	; verify_comparative_adjective1(Adj, Definitions, CompAdjBase, OrigCueWord1)
	),
	% modified to handle strange cue word combinations like 'than-than', which occurs
	% with sentences like
	% 'The thermosensitivity follows the order Mg2+--greater than (Na+ + K+)--greater than Na+-ATPase.'
	( OrigCueWord1 == OrigCueWord2 ->
	    negate_cueword_if_necessary(MSU1, MSU2, [MSU3|Rest],
					OrigCueWord0, CueWord1,
					OrigCueWord2, CueWord2)
	;  negate_cueword_if_necessary(MSU1, MSU2, [MSU3|Rest],
					OrigCueWord1, CueWord1,
					OrigCueWord2, CueWord2)
	).


negate_cueword_if_necessary(MSU1, MSU2, Rest, OrigCueWord1, CueWord1, OrigCueWord2, CueWord2) :-
	adj_is_negated(MSU1, MSU2, Rest),
	!,
	( OrigCueWord1 == different,
	  memberchk(OrigCueWord2, [from,than,to]) ->
	  CueWord1 = 'same',
	  CueWord2 = as
	; OrigCueWord1 \= different ->
	  negate_cueword1(OrigCueWord1, CueWord1),
	  negate_cueword2(OrigCueWord2, CueWord2)
	).

negate_cueword_if_necessary(_MSU1, _MSU2, _Rest, OrigCueWord1, CueWord1, OrigCueWord2, CueWord2) :-
	CueWord1 = OrigCueWord1,
	CueWord2 = OrigCueWord2.

adj_is_negated(MSU1, MSU2, Rest) :-
	get_first_N_elements([MSU1, MSU2 |Rest], 5, Candidates),
	member(Element, Candidates),
	% Bugzilla 22: added det(Features) to get "no different from"
	% as well as "not different from": "not" is an adv, but "no" is a det.
	get_possible_negation(Element, Features),
	get_lex_or_inputmatch(Features, LexOrInputMatch),
	intersection(LexOrInputMatch, [no, not, non], [_|_]),
	!.

get_possible_negation(Element, Features) :-
	( member(adv(Features), Element)
	; member(det(Features), Element)
	; member(head(Features), Element)
	).

get_lex_or_inputmatch(Features, LexOrInputMatch) :-
	( get_from_list(inputmatch, Features, LexOrInputMatch) ->
	  true
	; get_from_list(lexmatch, Features, LexOrInputMatch)
	).
	
get_first_N_elements([], _N, []).
get_first_N_elements([H|T], N, [H|Rest]) :-
	N > 1,
	!,
	N1 is N - 1,
	get_first_N_elements(T, N1, Rest).
get_first_N_elements([H|_T], _N, [H]).


%%%% needs work!!!!!
verify_comparative_adjective1(CompAdj, Definitions, CompAdjBase, CueWord1) :-
	( get_from_list(head, CompAdj, CompAdjHead) ->
	  Tag = adj
	; get_from_list(pastpart, CompAdj, CompAdjHead) ->
	  Tag = verb
	; rev(CompAdj, RevCompAdj),
	  get_from_list(adv, RevCompAdj, CompAdjHead) ->
	  Tag = adv
	),
	get_from_list(tag, CompAdjHead, Tag),
	get_lexmatch(CompAdjHead, CompAdjLexMatchList, CompAdjLexMatchAtom),
	get_base(CompAdjLexMatchAtom, adj, Definitions, CompAdjBase),
	CompAdjBase \== 'compare',
	get_adj_cueword(CompAdj, CompAdjBase, CompAdjLexMatchList, CueWord1),
	!.

% look for, e.g., "than aspirin" or "as aspirin" in one MSU;
% in these cases, the "than" or "as" is a prep contained in the NP's MSU
% +Type +NPU -NPArgType +PrevMSU -Indicator -SemanticGroup -CueWord
% +Type is always 'prep' for this, since ComparisonType == 'np'
verify_second_compared_NP(NPU, NPArgType, PrevMSU, _RestMSUs,
%			  _Definitions, Indicator, SemanticGroup, CueWord) :-
			  _Definitions, Indicator, Index, SemanticGroup, CueWord) :-
	is_a_compared_NP(prep, NPU, NPArgType, PrevMSU, SemanticGroup, CueWord),
	!,
	memberchk(prep(CueWordList), NPU),
	memberchk(index(Index), CueWordList),	
	Indicator = NPU.

% look for (1) e.g., "than aspirin" or "as aspirin"
% in two consecutive MSUs (prep/conj) + NP	
%       or (2) e.g., "than is aspirin" or "as is aspirin"
% in two consecutive MSUs (prep/conj) + NP	
verify_second_compared_NP(ThisMSU, NPArgType, _PrevMSU, [NextMSU1|RestMSUs],
%			  _Definitions, Indicator, SemanticGroup, CueWord) :-
			  _Definitions, Indicator, Index, SemanticGroup, CueWord) :-
	verify_comparative_standalone_prep_or_conj(ThisMSU, CueWord),
	get_first_index(ThisMSU, Index),
	( % (1)
	  is_an_NP(NextMSU1, SemanticGroup, NPArgType) ->
	  Indicator = NextMSU1
	; % (2)
	  find_previous_NP([NextMSU1|RestMSUs], NextMSU2, NPArgType),
%	  verify_comparative_BE_verb(NextMSU1, Definitions),
%	  RestMSUs = [NextMSU2|_],
%	  is_an_NP(NextMSU2, SemanticGroup, NPArgType),
	  Indicator = NextMSU2
	).

find_previous_NP([NPU|_RestMSUs], NPU, NPArgType) :-
	is_an_NP(NPU, SemanticGroup, NPArgType),
	% Check that the NPU has a semgroup in this list
	intersect([tmod], SemanticGroup).
find_previous_NP([_MSU|RestMSUs], NPU, NPArgType) :-
	find_previous_NP(RestMSUs, NPU, NPArgType).

% It isn't great that we have both find_second_compared_NP
%		and find_next_compared_NP, which do similar things
% +Type = conj | prep
find_next_compared_NP(Type, MSUs, PrevMSU, NPU, NPArgType,
                      SemanticGroup, AllowedCueWords, CueWord, Rest2) :-
	find_next_compared_NP_1(Type, MSUs, PrevMSU, NPU, NPArgType,
	                        SemanticGroup, CueWord, Rest2),
	memberchk(CueWord, AllowedCueWords).

% RestMSUs is where to resume looking for comparatives
find_next_compared_NP_1(Type, [NPU|RestMSUs], PrevMSU, NPU,
                        NPArgType, SemanticGroup, CueWord, RestMSUs) :-
	is_a_compared_NP(Type, NPU, NPArgType, PrevMSU, SemanticGroup, CueWord),
	intersect([tmod], SemanticGroup).
find_next_compared_NP_1(Type, [MSU|RestMSUs], _PrevMSU, NPU,
                        NPArgType, SemanticGroup, CueWord, Rest2) :-
	find_next_compared_NP_1(Type, RestMSUs, MSU, NPU, NPArgType, SemanticGroup, CueWord, Rest2).

is_an_NP(NPU, SemanticGroup, NPArgType) :-
	get_from_list(head, NPU, HeadList),
	% get_from_list(tag, HeadList, noun),
	NPArgType = head,
	get_from_list(semgroup, HeadList, SemanticGroup).

% +Type: prep -> look for CueWord in NPU
% +Type: conj -> look for CueWord in PrevMSU
is_a_compared_NP(Type, NPU, NPArgType, PrevMSU, SemanticGroup, CueWord) :-
	is_an_NP(NPU, SemanticGroup, NPArgType),
	get_NPU_cueword(Type, NPU, PrevMSU, CueWord),
        CueWord \== '<none>'.

% for e.g., "than tylenol" or "as tylenol"
get_NPU_cueword(prep, NPU, _PrevMSU, CueWord) :-
	get_from_list(prep, NPU, CueWordStructure),
%	( get_from_list(prep, NPU, CueWordStructure)
%	; get_from_list(adv, NPU, CueWordStructure)
%	),
	get_lexmatch(CueWordStructure, _, CueWord),
	CueWord \== 'of',
	!.
get_NPU_cueword(prep, _NPU, PrevMSU, CueWord) :-
	verify_comparative_standalone_prep_or_conj(PrevMSU, CueWord),
	!.
get_NPU_cueword(conj, _NPU, PrevMSU, CueWord) :-
	get_from_list(conj, PrevMSU, CueWordStructure),
	get_lexmatch(CueWordStructure, _, CueWord),
	!.
get_NPU_cueword(_, _NPU, _RevAnalysis, '<none>').

% CompPredsTail is a variable at this point.
% Later, it will be bound to predications found after
%		identify_comparatives in intermediate_analysis.
% Likewise with CompConjunctListTail.
form_comparative_results(NPU1, NP1ArgType, IndicatorIndex, NPU2, NP2ArgType,
			 CueWord1, CueWord2, ScaleName,
			 ComparativePredications, CompPredsTail,
			 CompConjunctList, CompConjunctListTail) :-
	get_from_list(NP1ArgType, NPU1, Head1),
	% get_lexmatch(Head1, NPU1LexMatch, NPU1LexMatchAtom),
	get_from_list(metaconc,  Head1, [NPU1MetaConc:NPU1CUI:NPU1SemTypeList|_]),
	NPU1SemTypeList = [NPU1SemType|_],
	get_from_list(index,     Head1, NPU1Index),
	get_from_list(NP2ArgType, NPU2, Head2),
	% get_lexmatch(Head2, NPU1Lexmatch, NPU2LexMatchAtom),
	get_from_list(metaconc,  Head2, [NPU2MetaConc:NPU2CUI:NPU2SemTypeList|_]),
	(	NPU2SemTypeList = [NPU2SemType|_]
	;	NPU2SemTypeList = [NPU2SemType]/_
	),
	get_from_list(index, Head2, NPU2Index),
% for now, dist. fields are set to 0 to prevent SemRep failure in format_one_predication_full_fielded
% also added IndicatorType 'COMP'
% fix it later <Halil>	
%	ComparativePredications = [ 0-0-NPU1Index-
%				    NPU1MetaConc-NPU1SemTypeList-NPU1SemType-
%				    'COMP'-compared_with-IndicatorIndex-
%				    0-0-NPU2Index-
%				    NPU2MetaConc-NPU2SemTypeList-NPU2SemType|CompPredsNext],
	ComparativePredications = [ _SubjectMaxDist-_SubjectDist-NPU1Index-
				    NPU1MetaConc-NPU1CUI-NPU1SemTypeList-NPU1SemType-
				    _IndicatorType-compared_with-IndicatorIndex-
				    _ObjectMaxDist-_ObjectDist-NPU2Index-
				    NPU2MetaConc-NPU2CUI-NPU2SemTypeList-NPU2SemType|CompPredsNext],
	CompConjunctList = [coord(and,arg,IndicatorIndex, [Head1],Head2,[])|CompConjunctListTail],
	form_rest_comparative_predications(CueWord1, CueWord2, ScaleName,
					   NPU1Index, IndicatorIndex, NPU2Index,
					   NPU1MetaConc, NPU1CUI, NPU1SemTypeList, NPU1SemType,
					   NPU2MetaConc, NPU2CUI, NPU2SemTypeList, NPU2SemType,
					   CompPredsNext, CompPredsTail).

form_rest_comparative_predications('<none>', _CueWord2, _ScaleName,
				   _NPU1Index, _IndicatorIndex, _NPU2Index,
				   _NPU1LexMatch, _NPU1CUI, _NPU1SemTypeList, _NPU1SemType,
				   _NPU2LexMatch, _NPU2CUI, _NPU2SemTypeList, _NPU2SemType,
				   ComparativePredications, ComparativePredications) :-
	!.
form_rest_comparative_predications(CueWord1, CueWord2, ScaleName,
				   NPU1Index, IndicatorIndex, NPU2Index,
				   NPU1LexMatch, NPU1CUI, NPU1SemTypeList, NPU1SemType,
				   NPU2LexMatch, NPU2CUI, NPU2SemTypeList, NPU2SemType,
				   ComparativePredications, CompPredsTail) :-
	cueword_translation(CueWord1, CueWord1Translation),
	cueword_translation(CueWord2, CueWord2Translation),
	create_link_word(CueWord1Translation, CueWord2Translation, LinkWord),
% According to Marcelo, in this case indicator is always the word before the cue "better than"
% Make sure this works
	NewIndIndex is IndicatorIndex - 1,
	ComparativePredications = [ scale-ScaleName-NewIndIndex,
				    _SubjectMaxDist-_SubjectDist-NPU1Index-
				     NPU1LexMatch-NPU1CUI-NPU1SemTypeList-NPU1SemType-
				     _IndicatorType-LinkWord-IndicatorIndex- % This is the relation
				     _ObjectMaxDist-_ObjectDist-NPU2Index-
  				     NPU2LexMatch-NPU2CUI-NPU2SemTypeList-NPU2SemType
				  | CompPredsTail].

create_link_word('NEG_same', same, LinkWord) :-
	LinkWord = 'NEG_same_as'.
create_link_word(different, _CueWord2, LinkWord) :-
	LinkWord = 'NEG_same_as'.
%create_link_word(higher,from,'higher_than').
%create_link_word(lower,from,'lower_than').
%create_link_word(than,than,'same_as').
%create_link_word(than,than,LinkWord) :-
create_link_word(CueWord, CueWord, LinkWord) :-
	!,
	concat_atom([CueWord, '_as'], LinkWord).
create_link_word(CueWord1, CueWord2, LinkWord) :-
	!,
	concat_atom([CueWord1, '_', CueWord2], LinkWord).

scale_word_translation(well,     good) :- !.
scale_word_translation(superior, good) :- !.
scale_word_translation(inferior, good) :- !.
scale_word_translation(worse,    good) :- !.
scale_word_translation(different, same) :- !.

scale_word_translation(Word,     Word).

negate_cueword1(higher, 'NEG_higher').
negate_cueword1(lower,  'NEG_lower').
negate_cueword1(different, same). % Bugzilla 22: added different
negate_cueword1(same,   lower).
negate_cueword1(as,     lower).

negate_cueword2(as,     than).
negate_cueword2(from,   as). % Bugzilla 22: to get "[not] different from"
negate_cueword2(than,   than).

cueword_translation(and,  higher) :- !.
cueword_translation(as,   same)   :- !.
cueword_translation(to,   than)   :- !.
cueword_translation(than, than)   :- !.
cueword_translation(Word, Word).


% The variable is PrepOrConj because in "aspirin is as effective as is Tylenol",
% we get [ conj([lexmatch([as]),inputmatch([as]),tag(conj)]) ]
% as a standalone structure, but in "aspirin is more effective than is Tylenol",
% we get [ prep([lexmatch([than]),inputmatch([than]),tag(prep)]) ] !!!


% Requirements:
%  (1) MSU must be an aux(_) structure
%  (2) base must be "be"
% Not currently used.
% verify_comparative_BE_verb(BeVerbMSU, Definitions) :-
% 	get_from_list(aux, BeVerbMSU, Aux),
% 	get_from_list(tag, Aux, aux),
% 	get_lexmatch(Aux, _LexMatchList, LexMatchAtom),
% 	get_base(LexMatchAtom, aux, Definitions, be).	

% Requirements:
%  (1) base must be "compare" or "comparison"
%verify_comparative_compare_word(CompareWord, Definitions) :-
verify_comparative_compare_word(CompareWord, Definitions, Index) :- 
	( get_from_list(verb, CompareWord, CompareList) ->
	  LexCat = verb
	; get_from_list(head, CompareWord, CompareList) ->
	  LexCat = noun
	; get_from_list(pastpart, CompareWord, CompareList) ->
	  LexCat = verb
	),
	get_lexmatch(CompareList, _LexMatchList, LexMatchAtom),
	memberchk(index(Index), CompareList),
	get_base(LexMatchAtom, LexCat, Definitions, CompareBase),
	memberchk(CompareBase, [compare,comparison,comparative_study]).	

% Note that the base of "vs." is "vs." rather than "vs" or "versus"
verify_comparative_versus([conj(WordProperties)], _Definitions, Index) :-
	get_lexmatch(WordProperties, _LexMatchList, LexMatchAtom),
	memberchk(LexMatchAtom, [versus,vs,'vs.']),
	memberchk(index(Index), WordProperties).

% In the case of, e.g., lexmatch([iron,supplementation]),
% return iron_supplementation as a atom
get_lexmatch(Item, [H|T], LexMatchAtom) :-
	( get_from_list(lexmatch, Item, [H|T]) ->
	  true
	; get_from_list(tokens, Item, [H|T])
	),
	insert_char_between_atoms(T, H, '_', LexMatchList),
	concat_atom(LexMatchList, LexMatchAtom).

insert_char_between_atoms([], H, _Char, [H]).
insert_char_between_atoms([H|T], Prev,  Char, [Prev, Char|Rest]) :-
	insert_char_between_atoms(T, H, Char, Rest).
	

% Requirements:
% (1) CompAdj must have an adj as head.
% (2) NPU2 must have a noun as head.
% (3) If the adj is introduced by "as", then the second noun must be also.
% (4) Otherwise, the second NPU must be introduced by "than" or "to"
% e.g.,                              as effective    as Tylenol
% e.g.,                            more effective  than Tylenol

% If I already have the cueword, great. Just keep it.
get_adj_cueword(_AdjMSU, _AdjBase, _LexMatchList, CueWord) :-
	nonvar(CueWord),
	!.
% To avoid labeling 'more different' etc. as 'higher'.
get_adj_cueword(_AdjMSU,CueWord,_LexMatchList,CueWord) :-
	member(CueWord,[different,same]),
	!.
% Look for analytical comparatives: There is another word inside the adj MSU
% (e.g., "as good", "more effective", etc.) which is one of the cuewords
% identified in map_adj_cueword/3.
get_adj_cueword(AdjMSU, _AdjBase, LexMatchList, CueWord) :-
	member(WordTerm, AdjMSU),
	arg(1, WordTerm, WordTermAttributes),
	get_lexmatch(WordTermAttributes, _LexMatchList, LexMatchAtom),
	map_adj_cueword(LexMatchAtom, LexMatchList, CueWord),
	!.
% If we fall through to here, the lexical adjective must be in the comparative degree,
% i.e., an inflectional comparative (e.g., "safer", "better"), in which case
% the adjective's base will be different from the actual lexical item.
get_adj_cueword(_AdjMSU, AdjBase, LexMatchList, higher) :-
	\+ memberchk(AdjBase, LexMatchList).

map_adj_cueword(less,     _,            lower)  :- !.
map_adj_cueword(more,     _,            higher) :- !.
map_adj_cueword(inferior, _,            lower)  :- !.
map_adj_cueword(superior, _,            higher) :- !.
map_adj_cueword(better,   _,            higher) :- !.    % FML added 2015/09/01
map_adj_cueword(different,   _,         different) :- !. % FML added 2015/09/01 for Bugzilla #22
map_adj_cueword(worse,    _,            lower)  :- !.
map_adj_cueword(as,       _,            same)   :- !.
map_adj_cueword(same,     _,            same)   :- !.
map_adj_cueword(_Atom,    LexMatchList, same)   :-
	memberchk(equivalent, LexMatchList).

%            as effective    as [is] Tylenol.
%          more effective  than [is] Tylenol.
% ensure that every "as" is paired with "as"
% every "same" is paired with "as"
% and that every other adjective is paried with "than" or "to"
verify_comparative_cueword_consistency('<none>', _ComparisonType, _CompAdjBase, _CueWord2) :- !.
verify_comparative_cueword_consistency(as,     ComparisonType,  _CompAdjBase, CueWord2) :-
	!,
	ComparisonType == np,
	CueWord2 == as.
verify_comparative_cueword_consistency(same,     ComparisonType,  _CompAdjBase, CueWord2) :-
	!,
	ComparisonType == np,
	CueWord2 == as.
verify_comparative_cueword_consistency('NEG_same',     ComparisonType,  _CompAdjBase, CueWord2) :-
	!,
	ComparisonType == np,
	CueWord2 == as.
verify_comparative_cueword_consistency(_CueWord1, _ComparisonType, CompAdjBase, CueWord2) :-
	memberchk(CueWord2, [than,to,from]), % Bugzilla #22 to get "[not] different from"
	verify_comparative_cueword_consistency_1(CompAdjBase, CueWord2).

verify_comparative_cueword_consistency_1(superior, CueWord2) :-
	!,
	CueWord2 == to.
verify_comparative_cueword_consistency_1(inferior, CueWord2) :-
	!,
	CueWord2 == to.
 % Bugzilla #22 to get "[not] different from"
verify_comparative_cueword_consistency_1(different, CueWord2) :-
	!,
	memberchk(CueWord2, [from,than,to]).
verify_comparative_cueword_consistency_1(_CompAdjBase, than).

% Verify that argument is a one-element list, and that the element looks like
% conj([lexmatch([as]),inputmatch([as]),tag(conj)])
% or
% prep([lexmatch([than]),inputmatch([than]),tag(prep)])

verify_comparative_standalone_prep_or_conj([PrepOrConj], LexMatch) :-
	verify_comparative_standalone_prep_or_conj(PrepOrConj, LexMatch).

verify_comparative_standalone_prep_or_conj(PrepOrConj, LexMatch) :-
	functor(PrepOrConj, Functor, 1),
	arg(1, PrepOrConj, PrepOrConjList),
	memberchk(Functor, [prep,conj]),
	% This next check is redundant
	% get_from_list(tag, PrepOrConjList, Functor),
	get_lexmatch(PrepOrConjList, _, LexMatch),
	memberchk(LexMatch, [as,than]).

/*
% verify_comparative_standalone_prep_or_conj(prep(Prep), than) :-
% 	get_from_list(tag, Prep, prep),
% 	get_lexmatch(Prep, _, than).
% verify_comparative_standalone_prep_or_conj(conj(Conj), as) :-
% 	get_from_list(tag, Conj, conj),
% 	get_lexmatch(Conj, _, as).
% verify_comparative_standalone_prep_or_conj(conj(Conj), and) :-
% 	get_from_list(tag, Conj, conj),
% 	get_lexmatch(Conj, _, and).
*/

% ************************************* IDENTIFY_GERUNDS *******************************

/*

identify_gerunds/2 identifies gerunds in the following environment:

             boundary, Ving, ..., NP head

The factor determining whether the Ving is
(1) a gerund with the following NP head as it object (e.g., "predicting recurrences"), or
(2) a present participle modifying the following NP head (e.g., "predicting machines"),
is whether the following NP head refers to
(1) an event, in which case the Ving is indeed a gerund, or
(2) an entity, in whihc case the Ving is a participle.

It seems much more likely that "predicting" is a gerund in the first example than in the second.

When a Ving precedes a noun head referring to an event, the Ving is labelled as the head
and a new MSU is created out of the following NP; present participles remain labelled as ing().

Actually, events vs. entities is probably not quite the right criterion, but for now
this is simulated (infelicitously) in dumlex.pl with the feature "predicational".

When the NP head refers to entities, only the participial analysis is imposed for now;
the gerund analysis is not precluded, but is not yet handled.

*/

% FML 07/22/2005 Friday @ 11:07:30
% I don't think this predicate can ever succeed--for two reasons:
% (1) the tagger doesn't have "ing" as a category, and even if it did,
% (2) the ing(_) structure would have more in it than just tokens(_)

%%% identify_gerunds(minimal_syntax(SyntaxAnalysisList),
%%% 		 minimal_syntax(SyntaxAnalysisListWithGerunds)) :-
%%% 	identify_gerunds(SyntaxAnalysisList, SyntaxAnalysisListWithGerunds),
%%% 	( SyntaxAnalysisList \== SyntaxAnalysisListWithGerunds ->
%%% 	  format('\n\n###### GERUND!!\n\n', [])
%%% 	; true
%%% 	).
%%% 
%%% identify_gerunds([], []).
%%% identify_gerunds([ThisNPU|MoreNPUs], [PreComplement,Complement|Gap]) :-
%%% 	% I don't ever see ing(_) structures;
%%% 	% even if they were there, wouldn't they contain more elements than just tokens(_),
%%% 	% e.g., index(_), bases(_), usemtype(_), ausemtype(_), semgroup(_),
%%% 	% lexmatch(_), inputmatch(_), tag(_), metaconc(_), etc.?
%%% 	VingItem = ing([tokens([VingWord])]),
%%% 	memberchk(VingItem, ThisNPU),
%%% 	\+ VingWord = using,
%%% 	% PreComplement is head(X) where ing(X) = VingItem
%%% 	get_complement_of_ving(VingItem, ThisNPU, PreComplement, Complement),
%%% 	get_head(Complement, CompHeadString ),
%%% 	% currently, only "efficacy", "recurrence", and "scanning" are predicational,
%%% 	% as defined in dumlex.pl
%%% 	dummy_lexicon(CompHeadString, rnoun, predicational, _ArgCueList),
%%% 	!,
%%% 	identify_gerunds(MoreNPUs, Gap ).
%%% identify_gerunds([ThisNPU|MoreNPUs], [ThisNPU|Gap]) :-
%%% 	identify_gerunds(MoreNPUs, Gap).
%%% 
%%% % ----- GET_COMPLEMENT_OF_VING
%%% 
%%% % get_complement_of_ving(Ving, [ Ving | [] ], _, _, _ ) :- !, fail.
%%% 
%%% get_complement_of_ving(ing(IngInfo), [ing(IngInfo)|Complement],
%%% 		       [head(IngInfo)], Complement) :- !.
%%% 
%%% get_complement_of_ving(Ving, [NonVing|More], [NonVing|Gap], Complement ) :-
%%% 	get_complement_of_ving(Ving, More, Gap, Complement).

/*
% ***********************Coordinating Adverbs and Prepositions**********************

The idea behind this predicate is that certain adverbial expressions,
such as "prior to", "followed by", "before", and "after",
have syntactic consequences of coordinators, in that
they allow the reuse of arguments for semantic interpretation.
This predicate attempts to identify such expressions and restructures
the surrounding syntax with these labeled as coordinators.

"prior to" and "followed by" coordinate nouns; "after which" coordinates verbs.

The Xerox Tagger seems to always return "followed by" as a verb and preposition.
"prior to" is only a preposition in the Lexicon.
"Before" and "after" can both be prepositions, adverbs, or conjunctions in the Lexicon.

Because of this ambiguity, only "after which", "followed by" and "prior to"
will be handled at first.

The current assumption is that they should always be treated as conjunctions,
but this in fact may not be right.

Note that when "prior to" is turned into a conj(), the old PrepList
is left intact (with tag(prep)), so that this could be recovered if
needed for further semantic processing.
[But this sort of thing isn't done consistently.]

The ConjList for the restructured "followed by" is subject to revision.
index() needs to be kept.

Current List: "after which", "followed by", and "prior to".

coordinate_adverbs_and_preps(EnhancedAnalysis0, EnhancedAnalysis1)

FML 07/22/2005 Friday @ 11:35:14

Examples of "followed by":
* early administration of surfactant FOLLOWED BY extubation
* The patients will be treated with chemotherapy FOLLOWED BY heat treatment
* Infants were randomized to either receive dexamethasone for two weeks
  FOLLOWED BY saline placebo for two weeks,
Examples of "prior to":
* They will be instructed in a weight maintenance diet PRIOR TO beginning the exercise program
* Patients presenting within 30 days of HIV-1 infection undergo leukapheresis
  (where available) PRIOR TO starting ART.
* To evaluate the safety of intravenous magnesium sulfate PRIOR TO foscarnet infusion
Examples of "after which":
* The study will begin with one or two cycles of chemotherapy,
  AFTER WHICH the patient will receive injections of G-CSF.
* Two inhalations of isoproterenol were administered one minute apart
  AFTER WHICH three forced vital capacity maneuvers were performed.
* There will then be an 11-day washout period with no drugs,
  AFTER WHICH IDV will again be given for 4 doses...

*/

coordinate_adverbs_and_preps(minimal_syntax(OldAnalysis),
			     minimal_syntax(NewAnalysis)) :-
	coordinate_adverbs_and_preps(OldAnalysis, NewAnalysis).

coordinate_adverbs_and_preps([], []).

% change
% ... [prep('prior to') | Object] ...
% to
% ... [conj('prior to')] Object ... (Object is a list)
coordinate_adverbs_and_preps([ThisMSU|MoreMSUs],
			     [NewFirst,OtherItems|NewRest]) :-
	get_from_list(prep,ThisMSU,PrepList),
	append(Pre,[prep(PrepList)|Rest],ThisMSU),
%	ThisMSU = [prep(PrepList)|OtherItems],
	get_from_list(lexmatch,PrepList,['prior to']),
	!,
	PrepList = [H|T],
	% change tag(prep) to tag(conj)
	change_preplist_tag(T, H, NewPrepList),
	NewFirst = [conj(NewPrepList)],
	append(Pre,Rest,OtherItems),
	coordinate_adverbs_and_preps(MoreMSUs,NewRest).
% change
% ... [prep(after, pron(which) | Object] ...
% to
% ... [conj('after which')], Object ...
coordinate_adverbs_and_preps([ThisMSU|MoreMSUs],
			     [NewMSU,RestThisMSU|NewRest]) :-
	% nextto(prep(PrepList), pron(PronList), ThisMSU),
	next_to_rest(prep(PrepList), pron(PronList), RestThisMSU, ThisMSU),
	get_from_list(lexmatch, PrepList, [after]),
	get_from_list(lexmatch, PronList, [which]),
	!,
	get_from_list(index,  PrepList, Index),
	get_from_list(bases,  PrepList, Bases),
	get_from_list(tokens, PrepList, Tokens),
	NewMSU = [conj([index(Index),
			bases(Bases),
			lexmatch(['after which']),
                        inputmatch(['after which']),
			tag(conj),
		  	tokens(Tokens)])],
	coordinate_adverbs_and_preps(MoreMSUs,NewRest).
% change
% ... [conj([after]) ] [pron(which)|Rest] ...
% to
% ... [conj('after which')], Rest ...
coordinate_adverbs_and_preps([[conj(ConjList)],SecondMSU|MoreMSUs],
			     [NewMSU,NewSecondMSU|NewRest]) :-
	get_from_list(lexmatch, ConjList, [after]),
	SecondMSU = [pron(PronList)|NewSecondMSU],
	get_from_list(lexmatch, PronList, [which]),
	!,
	get_from_list(index,  ConjList, Index),
	get_from_list(bases,  ConjList, Bases),
	get_from_list(tokens, ConjList, Tokens),
	NewMSU = [conj([index(Index),
			bases(Bases),
			lexmatch(['after which']),
                        inputmatch(['after which']),
			tag(conj),
			tokens(Tokens)])],
	coordinate_adverbs_and_preps(MoreMSUs,NewRest).
% change
% ... [verb(followed)], [prep(by) | Object] ...
% to
% ... [conj('followed by')], Object ... (Object is a list)
coordinate_adverbs_and_preps([ThisMSU,SecondMSU|MoreMSUs],
			     [NewFirstMSU,NewSecondMSU|NewRest]) :-
	get_from_list(verb,ThisMSU,VerbList),
	get_from_list(lexmatch, VerbList, [followed]),
	%	SecondMSU = [prep(PrepList)|NewSecondMSU],
	get_from_list(prep,SecondMSU,PrepList),
	get_from_list(lexmatch, PrepList, [by]),
	!,
	get_from_list(index,  VerbList, Index),
	get_from_list(bases,  VerbList, Bases),
	get_from_list(tokens, VerbList, Tokens),
	NewFirstMSU = [conj([index(Index),
			     bases(Bases),
			     lexmatch(['followed by']),
			     inputmatch(['followed by']),
			     tag(conj),
			     tokens(Tokens)])],
	append(Pre,[prep(PrepList)|RestSecondMSU],SecondMSU),
	append(Pre,RestSecondMSU,NewSecondMSU),
	coordinate_adverbs_and_preps(MoreMSUs, NewRest).
coordinate_adverbs_and_preps([ThisMSU|MoreMSUs], [ThisMSU|NewRest]) :-
	coordinate_adverbs_and_preps(MoreMSUs,NewRest).


% change
% ... [prep(followed), head(combination) ... ], [prep(with) | Object] ...
% to
% ... [conj('in combination with')], Object ... (Object is a list)
consolidate_multi_word_conjunctions(minimal_syntax(OldAnalysis),
				    minimal_syntax(NewAnalysis)) :-
	consolidate_multi_word_conjunctions(OldAnalysis, NewAnalysis).

consolidate_multi_word_conjunctions([], []).
consolidate_multi_word_conjunctions([ThisMSU, SecondMSU|MoreMSUs],
				    [NewFirstMSU,NewSecondMSU|NewRest]) :-
	get_from_list(conj, ThisMSU, ConjList),
	get_from_list(lexmatch, ConjList, [but]),
	get_from_list(adv,SecondMSU, AdvList),
	get_from_list(lexmatch, AdvList, [not]),
	append(PreSecond,[adv(AdvList)|RestSecondMSU],SecondMSU),
	append(_Pre,[conj(ConjList)|Rest],ThisMSU),
	!,
	get_from_list(index,  ConjList, Index),
	memberchk(position(BeginPos,_EndPos0), ConjList),
	get_from_list(bases,  ConjList, _Bases),
	get_from_list(tokens, ConjList, _Tokens),
	memberchk(position(_BeginPos0,EndPos),AdvList),
	append([],[conj([index(Index),
			     bases([but,not]),
			     lexmatch(['but not']),
			     inputmatch(['but not']),
			     tag(conj),
			     tokens([but,not]),
			     position(BeginPos,EndPos)
			    ])|Rest], NewFirstMSU),
	append(PreSecond,RestSecondMSU,NewSecondMSU),
	consolidate_multi_word_conjunctions(MoreMSUs, NewRest).
consolidate_multi_word_conjunctions([ThisMSU, SecondMSU|MoreMSUs],
				     [NewFirstMSU,NewSecondMSU|NewRest]) :-
	get_from_list(prep,ThisMSU,PrepList1),
	get_from_list(head,ThisMSU,HeadList),
	get_from_list(lexmatch, PrepList1, [in]),
	get_from_list(lexmatch, HeadList,  [combination]),
	append(_Pre,[prep(PrepList1),head(HeadList)|Rest],ThisMSU),
	get_from_list(prep,SecondMSU,PrepList2),
	get_from_list(lexmatch, PrepList2, [with]),
	append(PreSecond,[prep(PrepList2)|RestSecondMSU],SecondMSU),
	!,
	get_from_list(index,  PrepList1, Index),
	get_from_list(bases,  PrepList1, _Bases),
	get_from_list(tokens, PrepList1, _Tokens),
	memberchk(position(BeginPos,_EndPos0), PrepList1),
	memberchk(position(_BeginPos0,EndPos), PrepList2),
	append([],[conj([index(Index),
			     bases([in,combination,with]),
			     lexmatch(['in combination with']),
			     inputmatch(['in combination with']),
			     tag(conj),
			     tokens([in,combination,with]),
			     position(BeginPos,EndPos)
			    ])|Rest],NewFirstMSU),
	append(PreSecond,RestSecondMSU,NewSecondMSU),
	consolidate_multi_word_conjunctions(MoreMSUs, NewRest).
consolidate_multi_word_conjunctions([ThisMSU|MoreMSUs], [ThisMSU|NewRest]) :-
	consolidate_multi_word_conjunctions(MoreMSUs,NewRest).
	

next_to_rest(X, Y, Rest, [X,Y|Rest]).
next_to_rest(X, Y, Rest, [_|List]) :-
	next_to_rest(X, Y, Rest, List).

change_preplist_tag([], Item, [NewItem]) :-
	modify_preplist_item(Item, NewItem).
change_preplist_tag([Next|Tail], Item, [NewItem|NewRest]) :-
	modify_preplist_item(Item, NewItem),
	change_preplist_tag(Tail, Next, NewRest).

modify_preplist_item(Item, NewItem) :-
	( Item = tag(prep) ->
	  NewItem = tag(conj)
	; NewItem = Item
	).

% ************************************* COORDINATION *******************************

/*

---- Coordination in Underspecified Syntactic Analysis ----

Coordinate structures pose a particular problem to NLP since virtually anything
(in synatctic terms) can be coordinated. Consequently, with a moderately complete grammar,
even sentences of only moderate apparent complexity engender an explosion of parses.

The approach taken here addresses this issue by using semantic information
to augment syntactic structure in limiting the possible structures participating
in coordinating structures. In the current implementation, the program does not
identify all coordinate structures; however, incorrect coordinations are almost never imposed.

In order to abstract away from the number of potential syntactic structures
involved in coordination, every coordinate structure in this approach
is considered to be based on three words:
(1) a coordinator,
(2) a primary left conjunct, and
(3) a primary right conjunct.
The identification of these entities  significantly constrains the possible structures
involved in the coordination, and once these entities have been found, further processing
imposes the interpretation which follows from the coordination.

----Definitions

Processing of coordinate structures proceeds in a number of steps,

(1) Identification of the the primary conjuncts of each coordinator.
    Primary conjuncts are separated from each other by a coordinator.
    In "cats, dogs, and horses", "cats" and "horses" are primary conjuncts,
    as well as "dogs" and "horses".
(2) Determination of the secondary conjuncts derivable from the primary conjuncts.
    In the above example, "cats" and "dogs" are secondary conjuncts,
    because they are both coordinate to a third conjunct, namely "horses".
(3) Determination of the further linguistic consequences involved in the coordination.

A coordination is said to be licensed only if the conjuncts are compatible.
Two conjuncts are compatible only if one of the following conditions apply:

(1) they are lexically identical
(2) both are relational nouns
(3) they have the same semantic type (internal)

Expansion of the definition of compatibility is an essential aspect of expanding
the linguistic coverage of this approach to coordination.

The coordination module is under development and has not yet stabilized.

*/


% ************************************* COORDINATION *******************************************

% build_conjunct_list/2 used to be called coordination/2
build_conjunct_list(minimal_syntax(SyntacticAnalysisList), PrincipalDomain, CoordList) :-
	!,
	build_conjunct_list(SyntacticAnalysisList, PrincipalDomain, CoordList).

build_conjunct_list(SyntacticAnalysisList, PrincipalDomain, CoordList) :-
	partition(SyntacticAnalysisList,
		  punc([bases([:]),inputmatch([:]),tokens([])]),
		  LeftOfColon, RightOfColon ),
	find_coordination(LeftOfColon,  [], PrincipalDomain, LOCCoordList, LOCCoordListTail),
	find_coordination(RightOfColon, [], PrincipalDomain, ROCCoordList, ROCCoordListTail),
	% These two unifications explicitly append the difference lists
	% created in the two previous calls
	LOCCoordListTail = ROCCoordList,
	ROCCoordListTail = [],
	% append(LOCCoordList, ROCCoordList, TempCoordList),
	merge_overlapping_coord_terms(LOCCoordList, CoordList0),
	sort_all_coord_lists(CoordList0, CoordList).

sort_all_coord_lists([], []).
sort_all_coord_lists([H|T], [SortedH|SortedT]) :-
	H = coord(A,B,C, List, E, F),
	sort(List, SortedList),
	SortedH  = coord(A,B,C, SortedList, E, F),
	sort_all_coord_lists(T, SortedT).

%	format(user_output, 'CoordList: ~q~n', [CoordList]).

/*
merge_overlapping_coordlists/2 combines overlapping lists of conjuncts.
Background: Coord terms are of the form

    coord(conj, arg, Index, [a, b, c, d], e, [])

where
(1) [a, b, c, d] is the list of Left Conjuncts,
(2) e            is the Right Conjunct,
(3) the [] fourth argument is doing something losts in the mists of time, and
(4) each of the individual conjuncts (i.e., a, b, c, d, and e) is a list of the form

  [index(10),
   bases([nucleus]),
   usemtype([Cell Component]),
   ausemtype([celc]),
   semgroup([anat]),
   lexmatch([nuclei]),
   inputmatch([nuclei]),
   tag(noun),
   tokens([nuclei]),
   metaconc([Cell Nucleus:[celc]])]

Suppose now that LOCCoordLlist looks like
C1:  and/or([[cytoplasm]],                         [nuclei], [])
C2:     and([[arteries, capillaries, adipocytes]], [veins],  [])
C3:     and([[veins]],                             [nerves], [])

Note that C2 and C3 share a conjunct: veins.
merge_overlapping_coordlists/2 combines C2 and C3 to create

C1a:  and/or([[cytoplasm]],                                [nuclei], [])
C2a:     and([[arteries, capillaries, adipocytes, veins]], [nerves],  [])

The shared conjunct X can appear in one of four combinations:

Case 1: RR
The RIGHT conjunct of C1, and the RIGHT conjunct of C2, e.g.,
C1: coord(conj, [a,b,c], X, [])
C2: coord(conj, [p,q,r], X, [])
Then, the structures are merged as follows:
C12: coord(conj, [a,b,c,p,q,r], X, [])

Case 2: RL
The RIGHT conjunct of C1, and one of the LEFT conjuncts of C2, e.g.,
C1: coord(conj, [a,b,c], X, [])
C2: coord(conj, [p,X,r], z, [])
Then, the structures are merged as follows:
C12: coord(conj, [a,b,c,p,X,r], z, [])

Case 3: LR
One of the LEFT conjuncts of C1, and the RIGHT conjunct of C2.
This is simply case (2) above, with C1 and C2 reversed.

Case 4: LL
One of the LEFT conjuncts of C1, and one of the LEFT conjuncts of C2, e.g.,
C1: coord(conj, [a,b,X], z, [])
C2: coord(conj, [p,X,r], w, [])
This case is not allowed.

*/

merge_overlapping_coord_terms([], []).
merge_overlapping_coord_terms([H|T], MergedListOfCoordTerms) :-
	merge_overlapping_coord_terms_1(T, H, [], MergedListOfCoordTerms).

merge_overlapping_coord_terms_1([], OneList, TempMergedListOfCoordTerms, MergedListOfCoordTerms) :-
	merge_one_coord_term(TempMergedListOfCoordTerms, OneList, MergedListOfCoordTerms).
merge_overlapping_coord_terms_1([NextList|RestLists], OneList,
				TempMergedListOfCoordTerms0, MergedListOfCoordTerms) :-
	merge_one_coord_term(TempMergedListOfCoordTerms0, OneList, TempMergedListOfCoordTerms1),
	merge_overlapping_coord_terms_1(RestLists, NextList,
					TempMergedListOfCoordTerms1, MergedListOfCoordTerms).

merge_one_coord_term([], OneCoordTerm, [OneCoordTerm]).
merge_one_coord_term([FirstCoordTerm|RestCoordTerms], OneCoordTerm,
		     [Union|RestCoordTerms]) :-
	coord_intersect(FirstCoordTerm, OneCoordTerm, Union),
	!.
merge_one_coord_term([FirstCoordTerm|RestCoordTerms], OneCoordTerm,
		     [FirstCoordTerm|RestMergedCoordTerms]) :-
	merge_one_coord_term(RestCoordTerms, OneCoordTerm, RestMergedCoordTerms).
	

% Case 1: RR
coord_intersect(coord(Conj, Type, _Index1, LConjList1,      RConj1, []),
	        coord(Conj, Type, Index2,  LConjList2,      RConj2, []),
		coord(Conj, Type, Index2,  MergedLConjList, RConj1, [])) :-
	RConj1 = RConj2,
	!,
	append(LConjList1, LConjList2, MergedLConjList).
% Case 2: RL
coord_intersect(coord(Conj, Type, _Index1, LConjList1,      RConj1, []),
	        coord(Conj, Type, Index2,  LConjList2,      RConj2, []),
		coord(Conj, Type, Index2,  MergedLConjList, RConj2, [])) :-
	memberchk(RConj1, LConjList2),
	!,
	append(LConjList1, LConjList2, MergedLConjList).
% Case 3: LR
coord_intersect(coord(Conj, Type, _Index1, LConjList1,      RConj1, []),
	        coord(Conj, Type, Index2,  LConjList2,      RConj2, []),
		coord(Conj, Type, Index2,  MergedLConjList, RConj1, [])) :-
	memberchk(RConj2, LConjList1),
	append(LConjList1, LConjList2, MergedLConjList).

% ---------- PARTITION ---------- 

partition([],                 _Boundary, [],                          []).
partition([Boundary|More],     Boundary, [],                          More) :- !.
partition([NonBoundary|More],  Boundary, [NonBoundary|LeftPartition], RightPartition) :-
	partition(More, Boundary, LeftPartition, RightPartition ).

% ---------- FIND_COORDINATION	 ----------

/*

find_coordination goes through the input stopping at each coordinator
and identifies its primary conjuncts (via identify_conjuncts).
Note: In identify_conjuncts all the words in the input preceding the current coordinator
are called the 'Left Partition', while those following are called the 'Right Partition'.
As find_coordination steps through the input, it reverses the partition it analyzes.

find_coordination(+SyntAnalysis, -RevLeftPartition, -ConjunctList ).

A typical Conjunct might look like this:
[coord(and,
   [[index(1), bases([aspirin]), usemtype([Organic Chemical,Pharmacologic Substance]),
     ausemtype([orch,phsu]), semgroup([tmod]), lexmatch([aspirin]), inputmatch([Aspirin]),
     tag(noun), tokens([aspirin]), metaconc([Aspirin:[orch,phsu]])]],
   [index(3), bases([iron]), usemtype([Pharmacologic Substance]),
    ausemtype([phsu]), semgroup([tmod]), lexmatch([iron]), inputmatch([iron]),
    tag(noun), tokens([iron]), metaconc([Ferrum metallicum, Homeopathic preparation:[phsu]])])]


NOTE:
Do not allow "either" to license a coord struct. If we did, sentences such as

	The BDNF gene is inhibited either by aspirin or APAP.

would end up with *two* coord terms:
	either(BDNF gene, aspirin)
and
	or(aspirin, APAP)
and that is wrong!

*/


find_coordination([], _RevLeftPartition, _PrincipalDomain, Tail, Tail).
find_coordination([H|T], RevLeftPartition, PrincipalDomain, ConjListIn, ConjListOut) :-
	( ( control_option(conj) ->
	    memberchk(conj(ConjInfo), H)
	  ; H = [conj(ConjInfo)]
	  ) ->
	  get_from_list(lexmatch, ConjInfo, [Conj]),
	  Conj \== neither,
	  Conj \== both,
	  Conj \== either -> % see NOTE above
	  get_from_list(index, ConjInfo, Index),
	  % find the elements conjoined by Conj and create ThisConjunctList
 	  ( control_option(conj) ->
 	    append(RevLeftPartition, [H], RevLeftPartitionPlus)
 	  ; RevLeftPartitionPlus = RevLeftPartition
 	  ),
% 	  identify_conjuncts(RevLeftPartition, Conj, PrincipalDomain, Index,
 	  identify_conjuncts(RevLeftPartitionPlus, Conj, PrincipalDomain, Index,
			     T, ThisConjunctList),
	  set_conjunct_lists(ThisConjunctList, ConjListIn, ConjListNext),
	  find_coordination(T, [conj(ConjInfo)|RevLeftPartition], PrincipalDomain,
			    ConjListNext, ConjListOut)
	  % If first MSU is not a conj(_) term
	  % or a conj(_) term with "either" as lexical item,
	  % then add that MSU to RevLeftPartiti
	  % to build up the reversed left partition
	; find_coordination(T, [H|RevLeftPartition], PrincipalDomain,
			    ConjListIn, ConjListOut)
	).

set_conjunct_lists([],    ConjunctList, ConjunctList).
set_conjunct_lists(coord(A,B,C,D,E,F), [coord(A,B,C,D,E,F)|RestConjunctList], RestConjunctList).

% ---------- IDENTIFY_CONJUNCTS ----------

/*

identify_conjuncts(+RevLeftPartition, +Conj, +PrincipalDomain, +RightPartition, -ConjunctList )

This predicate attempts to identify the primary conjuncts for the
current coordinator (identified by find_coordination).

identify_conjuncts first checks to see whether the coordinator is
flanked by single-word MSUs (e.g. "safety and effectiveness of epikeratophakia").
If this is the case, these MSUs are the conjuncts.

Next, it checks to see whether there is a finite verb (assuming a stochastic tagger)
on both the left and the right of the current coordinator.
(This is not currently fully implemented.)
If there is a finite verb in both the left and right partitions of this coordinator,
then this is either sentence coordination or some form of VP coordination.
However, if either of the partitions lacks a finite verb,
then some other form of coordination obtains, and, currently,
it is assumed that this is NP coordination.
(Other types of coordination have to be addressed.)

identify_conjuncts(+RevLeftPartition, +Conj, +PrincipalDomain, +RightPartition, -ConjunctList ),

The first clause of identify_conjuncts catches instances where the coordinator
is sentence initial and therefore var(RevLeftPartition) succeeds.

The third clause of identify conjuncts applies in cases when there is
(1) only one MSU to the left of the coordinator,
(2) no punctuation on the left, (and thus no series coordination), and
(3) no modifier in the MSU on the right
In this case, the head of the MSU on the left is the LeftConjunct,
and the head of the MSU on the right is the RightConjunct.

Examples??

*/


identify_conjuncts(initial_coordinator, _Conj, _PrincipalDomain, _Index, _RightPartition, []) :- !.
identify_conjuncts([], _Conj, _PrincipalDomain, _Index, _RightPartition,  []) :- !.
identify_conjuncts([OnlyMSUOnLeft], Conj, _PrincipalDomain, Index, [FirstMSUOnRight|_], 
                    coord(Conj,arg,Index,[LeftConjunct], RightConjunct, []) ) :-
	get_from_list(punc, OnlyMSUOnLeft,   notfound),
	get_from_list(mod,  FirstMSUOnRight, notfound),
	!,
	get_from_list(head, OnlyMSUOnLeft, LeftConjunct),
	get_from_list(head, FirstMSUOnRight, RightConjunct).


/* not done yet
%%% 
%%% identify_conjuncts(RevLeftPartition, Conj, [ FirstMSUOnRight | _ ], 
%%%                     coord(Conj, LeftConjunct, RightConjunct, []) ) :-
%%% 
%%%     has_finite_verb([ FirstMSUOnRight ] ),	
%%% 
%%% % if true this is not clausal coord & is VP coord
%%%     has_finite_verb(RevLeftPartition ), !, 
%%% 
%%% % VP coord; lconj is first finite verb to the left
%%%     get_vp_conjuncts(not_done_yet).         
%%% 
*/


identify_conjuncts(RevLeftPartition, Conj, PrincipalDomain, Index, RightPartition,
		   coord(Conj,arg,Index,LeftConjunct,RightConjunct,[])) :-
 	( % RightPartition == [],
 	  control_option(conj) ->
 	  find_conjuncts_in_MSU(RevLeftPartition, LeftConjunct, RightConjunct)

	; RightPartition = [FirstMSUOnRight|_],
	  ( check_for_clausal_coord(RevLeftPartition, RightPartition,
				    LeftConjunct, RightConjunct),
	    check_for_verb_phrase_coord(Conj,RevLeftPartition,RightPartition,
					LeftConjunct,RightConjunct)
	  ; noun_phrase_coord(Conj, PrincipalDomain, RevLeftPartition, FirstMSUOnRight,
			      LeftConjunct, RightConjunct )
	  )
	),
	!.
identify_conjuncts(_PreCoordAnalysis, _Conj, _PrincipalDomain, _Index, _RightPartition, []).

find_conjuncts_in_MSU(MSUList, [LeftConjunct], RightConjunct) :-
	member(MSU, MSUList),	
	head_mod_or_shapes_member(MSU, HeadOrMod1, Index1),
	head_mod_or_shapes_member(MSU, HeadOrMod2, Index3),
	HeadOrMod1 \= HeadOrMod2,
	conj_member(MSU, _Conj, Index2),
	Index1 < Index2,
	Index2 < Index3,
	sort([HeadOrMod1,HeadOrMod2], [LeftConjunct,RightConjunct]).

% This predicate MUST be backtrackable, so no cuts!
head_mod_or_shapes_member(MSU, List, Index) :-
	member(mod(List), MSU),
	List = [index(Index0)|_],
	get_numerical_index(Index0, Index).
head_mod_or_shapes_member(MSU, List, Index) :-
	member(head(List), MSU),
	List = [index(Index0)|_],
	get_numerical_index(Index0, Index).
head_mod_or_shapes_member(MSU, List, Index) :-
	member(shapes(List), MSU),
	List = [index(Index0)|_],
	get_numerical_index(Index0, Index).

conj_member(MSU, List, Index) :-
	member(conj(List), MSU),
	List = [index(Index0)|_],
	get_numerical_index(Index0, Index).

% In identify.pl, the predicate glom_together/3 can modify
% an integer index, say, 15, to 15:14/15,
% so we need to extract an integer from this structure.

get_numerical_index(Index0, Index) :-
	( integer(Index0) ->
	  Index = Index0
	; Index0 = Y:X/Y ->
	  member(Index, [X,Y])
	).

	
/*
% ---------- CHECK_FOR_CLAUSAL_COORD ----------

Not finished yet in the sense that it may assign clausal coordination where it doesn't exist.

If check_for_relativizer succeeds, then the presence of a finite verb
means there is NOT clausal coordination.

The intent is to indicate that if one of the partitions contains a relativizer,
then the relative clause thereby introduced might contain a finite verb,
and thus the presence of a finite verb does not necessarily signal clausal coordinate.
That is the conj may be coordinating two NPs, one of which is modified by
a finite relative clause.
This principle has been extended to complementizers, per Tom's email, dated 9/28/09).
Still we need ordination configuration for a full solution.
*/

check_for_clausal_coord(RevLeftPartition, RightPartition, LeftPartition, RightPartition) :-
	has_finite_verb(RevLeftPartition),
	( check_for_relativizer(RevLeftPartition) ->
	  fail
	; check_for_complementizer(RevLeftPartition) ->
	  fail
	; true
	),
	rev(RevLeftPartition, LeftPartition),
	has_finite_verb(RightPartition),
	( check_for_relativizer(RightPartition) ->
	  fail
	; check_for_complementizer(RightPartition) ->
	  fail
	; true
	).

/*
% ---------- HAS_FINITE_VERB

*/

% has_finite_verb([]) :- !, fail.

has_finite_verb([ThisMSU|_More]) :-
	( memberchk(aux(_),   ThisMSU ) -> true
	; memberchk(modal(_), ThisMSU ) -> true
	; memberchk(verb(_),  ThisMSU ) -> true
	).
has_finite_verb([_ThisMSU|More]) :-
	has_finite_verb(More).



% ---------- CHECK_FOR_VERB_PHRASE_COORD ----------
% if no vp coord then this is S coord
% (need to create two new minimal_syntax() units)
% not done yet


check_for_verb_phrase_coord(_Conj, RevLeftPartition, RightPartition, LeftConjunct, RightConjunct) :- 

    get_vp_conjuncts(fail )		% temporary for testing
    -> true
    ;  LeftConjunct = RevLeftPartition,	% Clausal (S) coordination
       RightConjunct = RightPartition.


% ---------- GET_VP_CONJUNCTS -----------

get_vp_conjuncts(TempFlag) :-

    TempFlag = true
    -> true
    ; fail.


% ---------- NOUN_PHRASE_COORD ----------

/*

The implementation is under development.
The current state of the algorithm to identify primary conjuncts in NP coordination
(i.e., either an NP head or a left modifier) is as follows:

For each coordinator in the input (sentence or noun phrase),
proceeding from left to right:

1 Identify the right conjunct as the first head to the right of the coordinator.

2  To identify the left conjunct:
   2.1 Check for unique potential left conjunct.
   2.2 Otherwise, proceeding leftward from the coordinator,
       identify the left conjunct as the first head compatible with the right conjunct,
       (considering compatibility in the following order:
       lexical identity, relational noun, and semantic type identity)
       (predicate FIND_LCONJ).

3 If no left conjunct was identified for the head as right conjunct,
and if there is a single modifier to the left of the head
and to the right of the coordinator,
then identify that modifier as right conjunct
and repeat 2.2.

4  Otherwise coordination is undetermined for this coordinator.

noun_phrase_coord(+Conj, +PrincipalDomain, +RevLeftPartition, +FirstMSUOnRight, 
                          -LeftConjunct, -RightConjunct )
*/

noun_phrase_coord(Conj, PrincipalDomain, RevLeftPartition, FirstMSUOnRight,
		  LeftConjunct, RightConjunct) :-
	% rconj is head of FirstMSUOnRight
	noun_phrase_conj(Conj),
	% To fix the problem with SHAPES in place of HEAD (IL-13, etc.)
	locate_npu_head([FirstMSUOnRight],_Type,HeadRConjList ),
%	get_from_list( head, FirstMSUOnRight, HeadRConjList ),
	find_lconj(head, PrincipalDomain, RevLeftPartition, Conj, HeadRConjList, HeadLConjList),
        ( HeadLConjList = [ no_lconj_found ] ->
	  look_for_mod_coord(FirstMSUOnRight, RevLeftPartition, PrincipalDomain,
			     Conj, HeadRConjList,
			     LeftConjunct, RightConjunct) 
        ; LeftConjunct  = HeadLConjList,
          RightConjunct = HeadRConjList
	).

noun_phrase_conj(and).
noun_phrase_conj(or).
noun_phrase_conj(nor).
noun_phrase_conj('followed by').
noun_phrase_conj('in combination with').
noun_phrase_conj('but not').
noun_phrase_conj('prior to').


look_for_mod_coord(FirstMSUOnRight, RevLeftPartition, PrincipalDomain,
		   Conj, HeadRConjList,
		   LeftConjunct, RightConjunct) :-
	noun_phrase_conj(Conj),
	get_from_list( mod, FirstMSUOnRight, ModRConjList ),
%	 ( get_from_list( mod, FirstMSUOnRight, ModRConjList ),
	   % rconj is first mod
	 ( is_list(ModRConjList) ->
	   find_lconj( head, PrincipalDomain, RevLeftPartition, Conj, ModRConjList, ModLConjList ),
	   LeftConjunct = ModLConjList,
	   RightConjunct = ModRConjList
	 ; LeftConjunct = [ no_lconj_found ],
           RightConjunct = HeadRConjList
	 ).

% ---------- FIND_LCONJ ----------

% FIND_LCONJ applies the compatibility rules discussed above.  
% At the end of each compatibility rule, a check is made for series coordination.

% find_lconj(+HeadOrMod, +RevLeftPartition, +Conj, +RConjList, -LConjList ),


find_lconj(HeadOrMod, PrincipalDomain, RevLeftPartition, Conj, RConjList, LConjList) :-
	compatibility2(HeadOrMod, PrincipalDomain, Conj, RConjList, RevLeftPartition, LConjList ).

% ---------- COMPATIBILITY

compatibility(HeadOrMod, PrincipalDomain, Conj, RConjList, RevLeftPartition, LConjList) :-
	  % lexical_identity_compatibility does not apply in SemGen
	( PrincipalDomain \== genetics,
	  lexical_identity_compatibility(HeadOrMod, Conj, RConjList,
					 RevLeftPartition, LConjList ) -> true
	  % relational_noun_compatibility does not apply in SemGen
	; PrincipalDomain \== genetics,
	  relational_noun_compatibility(HeadOrMod, Conj, RConjList,
					RevLeftPartition, LConjList ) -> true
	; semantic_type_compatibility(HeadOrMod, Conj, RConjList, PrincipalDomain,
				      RevLeftPartition, LConjList ) -> true
	; LConjList = [ no_lconj_found ]
	).

compatibility2(HeadOrMod,PrincipalDomain,Conj,RConjList,LeftPartition,LConjList) :-
	compatibility2_aux(HeadOrMod,PrincipalDomain,Conj,RConjList,LeftPartition,0,LConjListAux),
	( LConjListAux == [] ->
	    LConjList = [ no_lconj_found]
	;  LConjList = LConjListAux
	).

compatibility2_aux(_HeadOrMod,_PrincipalDomain,_Conj,_RConjList,0,[],[]) :- !.
compatibility2_aux(HeadOrMod, PrincipalDomain, Conj, RConjList, [FirstLeftMSU|MoreRevLeftPartition], 0,[ThisLConjList|MoreLConjList]) :-
	semantic_type_compatibility2(HeadOrMod, Conj, RConjList, PrincipalDomain,FirstLeftMSU, ThisLConjList ),
	( check_for_left_barriers(Conj,FirstLeftMSU) ->
	  compatibility2_aux(HeadOrMod,PrincipalDomain,Conj,RConjList,MoreRevLeftPartition,1,MoreLConjList)
	; compatibility2_aux(HeadOrMod,PrincipalDomain,Conj,RConjList,MoreRevLeftPartition,0,MoreLConjList)
	).
%	compatibility2_aux(HeadOrMod,PrincipalDomain,Conj,RConjList,MoreRevLeftPartition,MoreLConjList).
compatibility2_aux(HeadOrMod, PrincipalDomain, Conj, RConjList, [FirstLeftMSU|MoreRevLeftPartition], 0,[ThisLConjList|MoreLConjList]) :-
	  % lexical_identity_compatibility does not apply in SemGen
	PrincipalDomain \== genetics,
%	\+ check_for_left_barriers(Conj,FirstLeftMSU),
	lexical_identity_compatibility2(HeadOrMod, Conj, RConjList,FirstLeftMSU, ThisLConjList ),
	( check_for_left_barriers(Conj,FirstLeftMSU) ->
	  compatibility2_aux(HeadOrMod,PrincipalDomain,Conj,RConjList,MoreRevLeftPartition,1,MoreLConjList)
	; compatibility2_aux(HeadOrMod,PrincipalDomain,Conj,RConjList,MoreRevLeftPartition,0,MoreLConjList)
	).
compatibility2_aux(HeadOrMod, PrincipalDomain, Conj, RConjList, [FirstLeftMSU|MoreRevLeftPartition], 0,[ThisLConjList|MoreLConjList]) :-
	  % relational_noun_compatibility does not apply in SemGen
	PrincipalDomain \== genetics,
%	\+ check_for_left_barriers(Conj,FirstLeftMSU),
	relational_noun_compatibility2(HeadOrMod, Conj, RConjList,FirstLeftMSU, ThisLConjList ),
%	compatibility2_aux(HeadOrMod,PrincipalDomain,Conj,RConjList,MoreRevLeftPartition,MoreLConjList).
	( check_for_left_barriers(Conj,FirstLeftMSU) ->
	  compatibility2_aux(HeadOrMod,PrincipalDomain,Conj,RConjList,MoreRevLeftPartition,1,MoreLConjList)
	; compatibility2_aux(HeadOrMod,PrincipalDomain,Conj,RConjList,MoreRevLeftPartition,0,MoreLConjList)
	).
compatibility2_aux(_HeadOrMod, _PrincipalDomain, _Conj, _RConjList, _RevLeftPartition, _Barrier,[]). 


% ----- LEXICAL_IDENTITY_COMPATIBILITY -----


% lexical_identity_compatibility(_HeadOrMod, _Conj, _RConjList, [], _LConjList) :- !, fail.

lexical_identity_compatibility(HeadOrMod, Conj, RConjList,
			       [FirstLeftMSU|_MoreRevLeftPartition], LConjList) :-
	get_from_list(tokens, RConjList, RTokens ),
	last(RTokens,LastElement),
	get_from_list(HeadOrMod, FirstLeftMSU, ThisLConjList ),
	get_from_list(tokens, ThisLConjList, Tokens ),
	last(Tokens,LastElement),
	!,
	( check_for_series_coord_fail(HeadOrMod, Conj, RConjList, ThisLConjList,
				      FirstLeftMSU, SeriesConjList ) ->
          % append(SeriesConjList, [ ThisLConjList ], LConjList )
	  LConjList = [ ThisLConjList | SeriesConjList ]
	; LConjList = [ ThisLConjList ]
	).
lexical_identity_compatibility(HeadOrMod, Conj, RConjList,
			       [FirstLeftMSU|MoreRevLeftPartition], LConjList) :-
	% LCONJ can't be to the left of a barrier
	( check_for_left_barriers(Conj, FirstLeftMSU ) ->
          fail
	; lexical_identity_compatibility(HeadOrMod, Conj, RConjList, MoreRevLeftPartition, LConjList )
	).

lexical_identity_compatibility2(HeadOrMod, Conj, RConjList,
			       FirstLeftMSU, LConjList) :-
	get_from_list(tokens, RConjList, RTokens ),
	last(RTokens,LastElement),
	get_from_list(HeadOrMod, FirstLeftMSU, ThisLConjList ),
	get_from_list(tokens, ThisLConjList, Tokens ),
	last(Tokens,LastElement),
	!,
	( check_for_series_coord_fail(HeadOrMod, Conj, RConjList, ThisLConjList,
				      FirstLeftMSU, SeriesConjList ) ->
          % append(SeriesConjList, [ ThisLConjList ], LConjList )
	  LConjList = [ ThisLConjList | SeriesConjList ]
	; LConjList = [ ThisLConjList ]
	).

% ----- RELATIONAL_NOUN_COMPATIBILITY -----

% relational_noun_compatibility(_HeadOrMod, _Conj, _RConjList, [], _LConjList) :- !, fail.
relational_noun_compatibility(HeadOrMod, Conj, RConjList,
			      [FirstLeftMSU|_MoreRevLeftPartition], LConjList) :-
	get_from_list(tokens, RConjList, RTokens ),
	last(RTokens,LastRElement),
	dummy_lexicon(LastRElement, rnoun, _, _ ),
	get_from_list(HeadOrMod, FirstLeftMSU, ThisLConjList ),
	get_from_list(tokens, ThisLConjList, LTokens ),
	last(LTokens,LastLElement),
	dummy_lexicon(LastLElement, rnoun, _, _ ), !,
	( check_for_series_coord_fail(HeadOrMod, Conj, RConjList,
				      ThisLConjList, FirstLeftMSU, SeriesConjList ) ->
          % append( SeriesConjList, [ ThisLConjList ], LConjList )
	  LConjList =  [ ThisLConjList | SeriesConjList ]
	; LConjList = [ ThisLConjList ]
	).
relational_noun_compatibility(HeadOrMod, Conj, RConjList,
			      [FirstLeftMSU|MoreRevLeftPartition], LConjList) :-
	% LCONJ can't be to the left of a barrier
	( check_for_left_barriers(Conj, FirstLeftMSU) ->
	  fail
	; relational_noun_compatibility(HeadOrMod, Conj, RConjList, MoreRevLeftPartition, LConjList)
	).

relational_noun_compatibility2(HeadOrMod, Conj, RConjList,
			      FirstLeftMSU, LConjList) :-
	get_from_list(tokens, RConjList, RTokens ),
	last(RTokens,LastRElement),
	dummy_lexicon(LastRElement, rnoun, _, _ ),
	get_from_list(HeadOrMod, FirstLeftMSU, ThisLConjList ),
	get_from_list(tokens, ThisLConjList, LTokens ),
	last(LTokens,LastLElement),
	dummy_lexicon(LastLElement, rnoun, _, _ ), !,
	( check_for_series_coord_fail(HeadOrMod, Conj, RConjList,
				      ThisLConjList, FirstLeftMSU, SeriesConjList ) ->
          % append( SeriesConjList, [ ThisLConjList ], LConjList )
	  LConjList =  [ ThisLConjList | SeriesConjList ]
	; LConjList = [ ThisLConjList ]
	).


% ----- SEMANTIC_TYPE -----

% semantic_type_compatibility(_HeadOrMod, _Conj, _RConjList, [], _LConjList) :- !, fail.

semantic_type_compatibility(HeadOrMod, _Conj, RConjList, PrincipalDomain,
			    [FirstLeftMSU|MoreRevLeftPartition], LConjList) :-
	% get_from_list missed shapes, etc., which often appear in gene names.
	% IL-13, etc.
	( HeadOrMod == head ->
	  locate_npu_head([FirstLeftMSU],_Type,FirstLConj)
	; get_from_list(HeadOrMod, FirstLeftMSU, FirstLConj)
	),
	consonance(FirstLConj, RConjList, PrincipalDomain),
	check_for_series_coord(MoreRevLeftPartition, PrincipalDomain, RConjList, SeriesConjList),
	LConjList = [FirstLConj|SeriesConjList].

semantic_type_compatibility(HeadOrMod, Conj, RConjList, PrincipalDomain,
			    [FirstLeftMSU|MoreRevLeftPartition], LConjList) :-
	% LCONJ can't be to the left of a barrier
	( check_for_left_barriers(Conj, FirstLeftMSU )	->
          fail
	; semantic_type_compatibility(HeadOrMod, Conj, RConjList, PrincipalDomain,
				      MoreRevLeftPartition, LConjList )
	).

semantic_type_compatibility2(HeadOrMod, _Conj, RConjList, PrincipalDomain,
			     FirstLeftMSU, FirstLConj) :-
	% get_from_list missed shapes, etc., which often appear in gene names.
	% IL-13, etc.
	( HeadOrMod == head ->
	  locate_npu_head([FirstLeftMSU],_Type,FirstLConj)
	; get_from_list(HeadOrMod, FirstLeftMSU, FirstLConj)
	),
	consonance(FirstLConj, RConjList, PrincipalDomain).
%	check_for_series_coord(MoreRevLeftPartition, PrincipalDomain, RConjList, SeriesConjList),
%	LConjList = [FirstLConj|SeriesConjList].

% ----- New  Consonance predicate
% either UMLS ST or Semgroups

consonance(LConjList, RConjList, PrincipalDomain) :-
	consonance_1(LConjList, RConjList, PrincipalDomain, Intersection),
	domain_validate_consonance(PrincipalDomain, Intersection).

consonance_1(LConjList, RConjList, _PrincipalDomain, Intersection) :-
	get_from_list(ausemtype, LConjList, LCSTList),
	get_from_list(ausemtype, RConjList, RCSTList),
	intersection(LCSTList, RCSTList, Intersection),
	Intersection = [_|_],
	!.
consonance_1(LConjList, RConjList, PrincipalDomain, Intersection) :-
	PrincipalDomain \== genetics,
	get_from_list(semgroup, LConjList, LCSTList),
	get_from_list(semgroup, RConjList, RCSTList),
	intersection(LCSTList, RCSTList, Intersection),
	\+ Intersection = [conc],
	Intersection = [_|_].

% In SemGen, allow consonance (i.e., SemType matching) only if
% the two conjuncts share either dsyn or gngm.
domain_validate_consonance(PrincipalDomain, Intersection) :-
	( PrincipalDomain == genetics ->
	  ( memberchk(dsyn, Intersection) ->
	    true
	  ; memberchk(gngm, Intersection)
	  )
	; true
	).

/*
% ----- CHECK_FOR_LEFT_BARRIERS

The left conjunct for a certain coordinator cannot occur to the left
of certain words which  serve as barriers for that coordinator.
For example, in "A and B are combined with D and C",
the primary left conjunct for "C" cannot be to the left of "combined".

*/

% NEW version prolosed by FML 02/22/2016
% Changed to handle cases like 'such as' HK 10/20/17
check_for_left_barriers(Conj, ThisMSU) :-
	get_all_tokens(ThisMSU, Tokens),
	concat_atom(Tokens,' ',TokensAtom),
	left_barrier(Conj, Barrier),
	midstring(TokensAtom,Barrier,_,0),
%	memberchk(Barrier, Tokens),
	!. 

% OLD version that is very suspect:
% intersection/3 will never succeed because its first arg will never be a list!
% check_for_left_barriers(Conj, ThisMSU) :-
% 	left_barrier(Conj, Barriers),
% 	get_all_tokens(ThisMSU, Tokens),
% 	!,
% 	intersection(Barriers, Tokens, _Intersection).
 
% ----- LEFT_BARRIER

left_barrier(and, Barrier) :- left_barrier_and(Barrier).
left_barrier(or,  Barrier) :- left_barrier_or(Barrier).
left_barrier(nor, Barrier) :- left_barrier_nor(Barrier).

% This list probably needs more work.
% I think what is really needed is a prep(_) check.
left_barrier_and(and).
left_barrier_and(associated).
left_barrier_and(between).
left_barrier_and(against).
left_barrier_and(both).
left_barrier_and(combined).
left_barrier_and('such as').
left_barrier_and(including).
left_barrier_and(of).
left_barrier_and(by).

left_barrier_or(and).
left_barrier_or(either).
left_barrier_or(between).
left_barrier_or(against).
left_barrier_or(both).
left_barrier_or('such as').
left_barrier_or(including).
left_barrier_or(of).
left_barrier_or(by).

left_barrier_nor(and).
left_barrier_nor(neither).

/*
% ---------- CHECK_FOR_SERIES_COORD/3 ----------

Stop when the semantic type does not match that of the right conjunct.
Return [] when no series coord is found.

so far implemented for semantic_type_compatibility only;
need to implement for lexical_identity_compatibility and
relational_noun_compatibility as well.

OBS MF found mistake on punc in these situations (X, Y, Z).
It would miss X as coordinate with Y and Z becauseof the punc ['('].
Therefore the list is inverted first. 
*/

% check_for_series_coord/4 Modified for Bugzilla #24
% This next clause is a possible fix for the following FP (and others)
% 16141404.ab.8 PAR-1 stimulation by SFLLRN also increased the formation of capillary-like structures by EPC in Matrigel, and this effect was abrogated by anti-CXCR-4, anti-SDF-1, and MEK inhibitor pretreatment.
% >>> The following FP is generated. Again, coordination problem, I think. In both 1.6 and 1.7, erucylphosphocholine-COEXISTS_WITH-Matrigel are generated. I suspect Matrigel and SDF-1 are identified as conjuncts.
% 16141404.ab.8|relation|C0169014|erucylphosphocholine|opco,phsu|opco|||COEXISTS_WITH|C0218504|Stromal Cell-Derived Factor 1|aapp,gngm,imft|aapp||

% check_for_series_coord([NextMSU|_MoreToLeft], _PrincipalDomain, _RConj, Series) :-
% 	memberchk(verb(_), NextMSU),
% 	!,
% 	Series = [].

% check_for_series_coord([], _PrincipalDomain, _RConj, []).
check_for_series_coord([NextMSU|MoreToLeft], PrincipalDomain, RConj, [HeadList|Gap]) :-
        rev(NextMSU, NextMSUInv),
        get_from_list(punc, NextMSUInv, PuncList),
        get_from_list(inputmatch, PuncList, [',']),
        get_from_list(head, NextMSU, HeadList),
        consonance(HeadList,RConj, PrincipalDomain),
        !,
        check_for_series_coord(MoreToLeft,PrincipalDomain, RConj,Gap).
check_for_series_coord(_Other, _PrincipalDomain, _, []).

% This version was an attempt to fix the problem reported in Bugzilla #24,
% but it caused too many FPs.
% check_for_series_coord([], _PrincipalDomain, _RConj, []).
% check_for_series_coord([NextMSU|MoreToLeft], PrincipalDomain, RConj, Series) :-
% 	( rev(NextMSU, NextMSUInv),
% 	  get_from_list(punc, NextMSUInv, PuncList),
% 	  get_from_list(inputmatch, PuncList, [',']),
% 	  get_from_list(head, NextMSU, HeadList),
% 	  consonance(HeadList,RConj, PrincipalDomain) ->
% 	  Series = [HeadList|RestSeries]
% 	; Series = RestSeries
% 	),
% 	!,
% 	check_for_series_coord(MoreToLeft,PrincipalDomain, RConj,RestSeries).
 
% need to fix this for lexical identity and relational noun
% i.e. get rid of /6

check_for_series_coord_fail(_,_,_,_,_,_) :- !, fail.

/* OLD check_for_series_coord/6

%%% check_for_series_coord(HeadOrMod, Conj, RConjList, ThisLConjList, ThisMSU, SeriesConjList) :-
%%% 	functor(DeleteThis, HeadOrMod, 1 ),
%%% 	arg(1, DeleteThis, ThisLConjList ),
%%% 	rev(ThisMSU, RevThisMSU ),
%%% 	delete(RevThisMSU, DeleteThis, 1, PrecedesLConj ),
%%% 	( nextto(punc([inputmatch([',']),_]),
%%% 	  mod(_), PrecedesLConj ) ->
%%% 	  find_lconj(mod, [ PrecedesLConj ], Conj, RConjList, SeriesConjList )
%%% 	; fail
%%% 	).

*/

adjust_semtypes_in_analysis([], _Domain, []).
adjust_semtypes_in_analysis([ThisMSU|RestMSUs], Domain, [ThisAdjustedMSU|RestAdjustedMSUs]) :-
	adjust_semtypes_in_one_MSU(ThisMSU, Domain, ThisAdjustedMSU),
	adjust_semtypes_in_analysis(RestMSUs, Domain, RestAdjustedMSUs).

adjust_semtypes_in_one_MSU([], _Domain, []).
adjust_semtypes_in_one_MSU([ThisElement|RestElements], Domain,
			   [ThisAdjustedElement|RestAdjustedElements]) :-
	adjust_semtypes_in_one_MSU_element(ThisElement, Domain, ThisAdjustedElement),
	adjust_semtypes_in_one_MSU(RestElements, Domain, RestAdjustedElements).

adjust_semtypes_in_one_MSU_element(ThisElement, Domain, ThisAdjustedElement) :-
	( functor(ThisElement, Functor, 1),
	  arg(1, ThisElement, ArgList),
	  ArgList = [_|_] ->
	  ( memberchk(semgroup(SemGroupList), ArgList)
            ;
	    SemGroupList = []
	  ),
	  adjust_semtypes_in_one_MSU_element_list(ArgList, Domain, SemGroupList, AdjustedArgList),
	  functor(ThisAdjustedElement, Functor, 1),
	  arg(1, ThisAdjustedElement, AdjustedArgList)
	; ThisAdjustedElement = ThisElement
	).

adjust_semtypes_in_one_MSU_element_list([], _Domain, _SemGroupList, []).
adjust_semtypes_in_one_MSU_element_list([semgroup(_)|RestFeatures], Domain, SemGroupList,
	                                RestAdjustedFeatures):-
	adjust_semtypes_in_one_MSU_element_list(RestFeatures, Domain, SemGroupList,
	                                        RestAdjustedFeatures).
adjust_semtypes_in_one_MSU_element_list([ausemtype(AUSemTypeList)|RestFeatures], Domain,
	                                SemGroupList, [NewAUSemTypeList,NewSemGroupList|RestAdjustedFeatures]):-
	adjust_one_feature(ausemtype(AUSemTypeList), Domain, SemGroupList, AdjustedFeature),
	AdjustedFeature = [NewAUSemTypeList,NewSemGroupList],
	adjust_semtypes_in_one_MSU_element_list(RestFeatures, Domain, SemGroupList,
	                                        RestAdjustedFeatures).
adjust_semtypes_in_one_MSU_element_list([Feature|RestFeatures], Domain, SemGroupList,
					[AdjustedFeature|RestAdjustedFeatures]) :-
	adjust_one_feature(Feature, Domain, SemGroupList, AdjustedFeature),
	adjust_semtypes_in_one_MSU_element_list(RestFeatures, Domain, SemGroupList,
	                                        RestAdjustedFeatures).

adjust_one_feature(Feature, Domain, SemGroupList, AdjustedFeature) :-
	( Feature = metaconc(ConcList) ->
	  adjust_metaconc_list(ConcList, AdjustedConcList),
	  AdjustedFeature = metaconc(AdjustedConcList)
	; Feature = ausemtype(AUSemTypeList) ->
	  adjust_semtype_list(AUSemTypeList, AUSemTypeList, AdjustedAUSemTypeList),
	  AdjustedFeatures = [ausemtype(AdjustedAUSemTypeList)],
	  adjust_semgroup_list(AUSemTypeList, Domain, SemGroupList, 
	      AdjustedAUSemTypeList, AdjustedSemGroupList),
	  append(AdjustedFeatures, [semgroup(AdjustedSemGroupList)], AdjustedFeature)
	; Feature = usemtype(USemTypeList) ->
	  map_semtypes(USemTypeList, ua, AUSemTypeList),
	  adjust_semtype_list(AUSemTypeList, AUSemTypeList, AdjustedAUSemTypeList),
	  map_semtypes(AdjustedAUSemTypeList, au, AdjustedUSemTypeList),
	  AdjustedFeature = usemtype(AdjustedUSemTypeList)
        ; Feature = genphenom(GenPhenomList),
	  adjust_genphenom_list(GenPhenomList, AdjustedGenPhenomList),
	  AdjustedFeature = genphenom(AdjustedGenPhenomList)
        ; AdjustedFeature = Feature
        ).

adjust_semgroup_list(_AUSemTypeList, _Domain, [], _AdjustedAUSemTypeList, []).
adjust_semgroup_list(AUSemTypeList, Domain, SemGroupList, 
	             AdjustedAUSemTypeList, AdjustedSemGroupList) :-
	subtract(AdjustedAUSemTypeList, AUSemTypeList, SemTypeDiffList),
	( list_to_set(SemTypeDiffList,[gngm]),
	  union(SemGroupList, [gene], AdjustedSemGroupList)
	; list_to_set(SemTypeDiffList,[aapp]),
	  Domain == genetics,
	  union(SemGroupList, [chem], AdjustedSemGroupList)
	; list_to_set(SemTypeDiffList,[aapp]),
	  union(SemGroupList, [tmod], AdjustedSemGroupList)
	).
adjust_semgroup_list(_AUSemTypeList, _Domain,SemGroupList,
	             _AdjustedAUSemTypeList, SemGroupList).

adjust_genphenom_list(GenPhenomList, AdjustedGenPhenomList) :-
	rev(GenPhenomList, RevGenPhenomList),
	RevGenPhenomList = [SemTypeList|Rest],
	adjust_semtype_list(SemTypeList, SemTypeList, AdjustedSemTypeList),
	RevAdjGenPhenomList = [AdjustedSemTypeList|Rest],
	rev(RevAdjGenPhenomList, AdjustedGenPhenomList).
	
adjust_metaconc_list([], []) :- !.
adjust_metaconc_list([MetaConc:CUI:SemTypeList|RestMetaConcs],
		     [MetaConc:CUI:AdjustedSemTypeList|RestAdjustedMetaConcs]) :-
	!,
	adjust_semtype_list(SemTypeList, SemTypeList, AdjustedSemTypeList),
	adjust_metaconc_list(RestMetaConcs, RestAdjustedMetaConcs).

adjust_semtype_list(STList1/STList2, STList1/STList2, STList1/AdjustedSTList2) :-
	adjust_semtype_list(STList2, STList2, AdjustedSTList2).

adjust_semtype_list([], _OrigSemTypeList, []).
adjust_semtype_list([ThisSemType|RestSemTypes], OrigSemTypeList, AdjustedSemTypes) :-
	adjust_semtype(ThisSemType, OrigSemTypeList, AdjustedSemTypes, RestAdjustedSemTypes),
	adjust_semtype_list(RestSemTypes, OrigSemTypeList, RestAdjustedSemTypes).
	
adjust_semtype(aggp, _RestOrigSemTypes, [aggp,humn|RestAdjustedSemTypes], RestAdjustedSemTypes) :- !.
adjust_semtype(famg, _RestOrigSemTypes, [famg,humn|RestAdjustedSemTypes], RestAdjustedSemTypes) :- !.
adjust_semtype(grup, _RestOrigSemTypes, [grup,humn|RestAdjustedSemTypes], RestAdjustedSemTypes) :- !.
adjust_semtype(podg, _RestOrigSemTypes, [podg,humn|RestAdjustedSemTypes], RestAdjustedSemTypes) :- !.
adjust_semtype(popg, _RestOrigSemTypes, [popg,humn|RestAdjustedSemTypes], RestAdjustedSemTypes) :- !.
adjust_semtype(prog, _RestOrigSemTypes, [prog,humn|RestAdjustedSemTypes], RestAdjustedSemTypes) :- !.
adjust_semtype(humn, _RestOrigSemTypes, [grup,humn|RestAdjustedSemTypes], RestAdjustedSemTypes) :- !.
adjust_semtype(aapp,  RestOrigSemTypes, AdjustedSemTypes, RestAdjustedSemTypes) :-
	!,
	add_semtype_if_necessary(aapp, gngm, RestOrigSemTypes,
				 AdjustedSemTypes, RestAdjustedSemTypes).
adjust_semtype(gngm, RestOrigSemTypes, AdjustedSemTypes, RestAdjustedSemTypes) :-
	!,
	add_semtype_if_necessary(gngm, aapp, RestOrigSemTypes,
				 AdjustedSemTypes, RestAdjustedSemTypes).
adjust_semtype(OtherSemType, _RestOrigSemTypes,
	       [OtherSemType|RestAdjustedSemTypes],
	       RestAdjustedSemTypes).

add_semtype_if_necessary(SemType, OtherSemType, RestOrigSemTypes,
		 AdjustedSemTypes, RestAdjustedSemTypes) :-
	( memberchk(OtherSemType, RestOrigSemTypes) ->
	  AdjustedSemTypes = [SemType|RestAdjustedSemTypes]
	; AdjustedSemTypes = [SemType,OtherSemType|RestAdjustedSemTypes]
	).

map_semtypes([], _Direction, []).
map_semtypes([SemType|RestSemTypes], Direction, [MappedSemType|RestMappedSemTypes]) :-
	global_module_version(Version),
	( Direction = au ->
	  SemType = AbbreviatedSemType,
	  MappedSemType = LongSemType,
	  semtype_translation(Version,LongSemType, AbbreviatedSemType)
	; SemType = LongSemType,
	  MappedSemType = AbbreviatedSemType,
	  semtype_translation(Version,LongSemType, AbbreviatedSemType)
	),
	map_semtypes(RestSemTypes, Direction, RestMappedSemTypes).
% Not sure this is used or necessary
map_semtypes([SemType|RestSemTypes], Direction, [MappedSemType|RestMappedSemTypes]) :-
        ( control_option(use_generic_domain_extension)
        ; control_option(use_generic_domain_modification)
	),
        ( Direction = au ->
          SemType = AbbreviatedSemType,
          MappedSemType = LongSemType
%          generic_domain:domain_semtype(AbbreviatedSemType,LongSemType,_)
        ; SemType = LongSemType,
          MappedSemType = AbbreviatedSemType
%          generic_domain:domain_semtype(AbbreviatedSemType,LongSemType,_)
        ),
        map_semtypes(RestSemTypes, Direction, RestMappedSemTypes).
map_semtypes([SemType|RestSemTypes], Direction, [MappedSemType|RestMappedSemTypes]) :-
	control_value(domain, _),
        ( Direction = au ->
          SemType = AbbreviatedSemType,
          MappedSemType = LongSemType,
          domain_data:domain_semtype(AbbreviatedSemType,LongSemType,_)
        ; SemType = LongSemType,
          MappedSemType = AbbreviatedSemType,
          domain_data:domain_semtype(AbbreviatedSemType,LongSemType,_)
        ),
        map_semtypes(RestSemTypes, Direction, RestMappedSemTypes).



/*

set_empty_heads_in_analysis/4:

An MSU is a list such as [prep(1), det(2), mod(3), mod(4), head(5), punc(6), confid(7)].
Working from the end of an MSU to its front, an element in an MSU is an empty head if:
(1) it is a head or a mod, and
(2) its bases(_) component contains only words W such that empty_head_base(W) is true.
Also (again working from back to front), if an element is an empty head,
and the next element (i.e., the previous element working front to back) is NOT,
stop looking for empty heads. E.g., if head(5) is empty, and mod(4) is not empty,
stop right there, and don't even look at mod(3).

Special case: If we have a structure such as

     [ PREP1 ... HEAD1 ] [ PREP2 ... HEAD2 ]

where HEAD1 is an empty head, then change this to

     [ ... EMPTY_HEAD1 ] [ PREP1 EMPTY_PREP2... HEAD2 ]

*/

set_empty_heads_in_analysis([], LastMSU, PrevHeadEmpty, TempPrevMSU, PrevMSU,
			    [PrevMSU, LastMSUWithEmptyHead]) :-
	rev(LastMSU, RevLastMSU),
	possible_empty_head(RevLastMSU),
	EmptyHeadIn is 0,
	set_empty_heads_in_MSU(RevLastMSU, [], EmptyHeadIn, _EmptyHeadOut,
			       RevLastMSUWithEmptyHead, PrevHeadEmpty, EmptyHeadFound),
	rev(RevLastMSUWithEmptyHead, TempLastMSUWithEmptyHead),
	adjust_for_prep_phrases(PrevHeadEmpty, EmptyHeadFound,
				TempPrevMSU, TempLastMSUWithEmptyHead,
				PrevMSU, LastMSUWithEmptyHead),
	!.
set_empty_heads_in_analysis([],LastMSU,_PrevHeadEmpty,TempPrevMSU,_PrevMSU,
	                    [TempPrevMSU,LastMSU]) :- !.
set_empty_heads_in_analysis([NextMSU|RestMSUs],  ThisMSU, PrevHeadEmpty, TempPrevMSU, PrevMSU,
			    [PrevMSU|RestMSUsWithEmptyHeads]) :-
	rev(ThisMSU, RevThisMSU),
	possible_empty_head(RevThisMSU),
	% RevThisMSU = [FirstRevMSUElement|RestRevMSUElements],
	% EmptyHeadIn = 0 means that we have not yet seen an empty head in this MSU
	EmptyHeadIn is 0,
	set_empty_heads_in_MSU(RevThisMSU, [], EmptyHeadIn, _EmptyHeadOut,
			       RevThisMSUWithEmptyHead, PrevHeadEmpty, EmptyHeadFound),
	rev(RevThisMSUWithEmptyHead, TempThisMSUWithEmptyHead0),
	update_rest_with_empty_heads(TempThisMSUWithEmptyHead0,0,_EmptyOut,TempThisMSUWithEmptyHead),
%	format(user_output,'EMPTY HEAD MSU: ~q~n',[TempThisMSUWithEmptyHead]),
	adjust_for_prep_phrases(PrevHeadEmpty, EmptyHeadFound,
				TempPrevMSU, TempThisMSUWithEmptyHead,
			        PrevMSU, ThisMSUWithEmptyHead),
	set_empty_heads_in_analysis(RestMSUs, NextMSU, EmptyHeadFound,
				    ThisMSUWithEmptyHead, ThisMSUWithEmptyHead,
				    RestMSUsWithEmptyHeads).
set_empty_heads_in_analysis([NextMSU|RestMSUs],ThisMSU,_PrevHeadEmpty,TempPrevMSU,_PrevMSU,
	                    [TempPrevMSU|RestMSUsWithEmptyHeads]):-
	set_empty_heads_in_analysis(RestMSUs,NextMSU,0,ThisMSU,_,
	                            RestMSUsWithEmptyHeads).


% For an MSU to have an empty head, there must be a mod(_) if there is a head(_)
% or two mod(_)'s if there is no head.
possible_empty_head(MSU) :-
	memberchk(head(_HeadList),MSU),
	!,
	memberchk(mod(_ModList),MSU).
possible_empty_head(MSU) :-
	memberchk(mod(ModList1),MSU),
	memberchk(mod(ModList2),MSU),
	\+ ModList1 == ModList2.

% This predicate handles constructions such as "[with concentrations] [of tamoxifen]":
% PrevMSU contains prep phrase #1 whose object is an empty head ("concentrations");
% ThisMSU contains prep phrase #2 whose object is NOT an empty head ("tamoxifen").
% Moreover, if preposition #2 is not "of", the base of the empty head
% must be an indicator requiring preposition #2, e.g., "with response to lithium",
% and word_corresponds_to_semnet_relation(response, noun, to, interacts_with).

adjust_for_prep_phrases(PrevHeadEmpty, ThisHeadEmpty,
			TempPrevMSU, TempThisMSU,
			PrevMSU, ThisMSU) :-
	% format('~nPREV: ~w~n~w~nCURR: ~w~n~w~n',
	%        [PrevHeadEmpty, ThisHeadEmpty, TempPrevMSU, TempThisMSU]),
	( PrevHeadEmpty =:= 1,
	  ThisHeadEmpty =:= 0,
	  TempPrevMSU = [prep(PrevPrepList)|RestPrevMSU],
	  TempThisMSU = [prep(ThisPrepList)|RestThisMSU],
	  get_from_list(bases, ThisPrepList, [of]) ->	  
	 %  indicator_construction(RestPrevMSU, ThisPrep),
	 %  PrevMSU = [prep(PrevPrepList)|RestThisMSU],
	 %  ThisMSU = [prep(ThisPrepList)|RestPrevMSU]
	  PrevMSU = RestPrevMSU,
	  ThisMSU = [prep(PrevPrepList), empty_prep(ThisPrepList)|RestThisMSU]
        ; PrevMSU = TempPrevMSU,
	  ThisMSU = TempThisMSU
	).
	
% indicator_construction(RestPrevMSU, ThisPrep) :-
% 	( ThisPrep == of ->
% 	  true
% 	; get_from_list(empty_head, RestPrevMSU, HeadList),
% 	  get_from_list(bases, HeadList, [Base]),
% 	  word_corresponds_to_semnet_relation(Base, noun, ThisPrep, _)
% 	).

% EmptyHeadIn:    whether the previous element in this MSU was empty
% EmptyHeadOut:   whether the current  element in this MSU was empty
% PrevHeadEmpty : whether the previous MSU contained an empty head
% EmptyHeadFound: whether the current  MSU contained an empty head *anywhere*
set_empty_heads_in_MSU([], _EntireEmptyHead,EmptyHeadOut, EmptyHeadOut,
		       [], EmptyHeadFound, EmptyHeadFound).
set_empty_heads_in_MSU([ThisMSUElement|RestMSUElements],EmptyHeadSoFar,
		       EmptyHeadIn, EmptyHeadOut,
		       [ModifiedThisMSUElement|RestMSUElementsWithEmptyHeads],
		       _PrevHeadEmpty, EmptyHeadFound) :-
	% need to know if PrevHeadEmpty, and to have PrevHead itself!
	make_empty_head([ThisMSUElement|EmptyHeadSoFar],RestMSUElements,
			ThisMSUElementWithEmptyHead, ThisHeadIsEmpty),
	% If we've already encountered an empty head (EmptyHeadIn =:= 1),
	% but this element of the current MSU is NOT an empty head (ThisHeadIsEmpty =:= 0),
	% then we don't have to look for empty heads any further in this MSU,
	% but we do have to change the element that we just looked at
	% (which was NOT an empty head) to a head, regardless of what it was originally.
	( EmptyHeadIn =:= 1,
	  ThisHeadIsEmpty =:= 0 ->
	  EmptyHeadOut is 0,
	  % ensure that the closest mod(_) or head(_) before the empty_head(_)
	  % that has a Metaconc becomes the real head of the MSU
	  make_into_head([ThisMSUElement|RestMSUElements],
			 [ThisMSUElementAsHead|RestMSUElementsWithEmptyHeads]),
	  ModifiedThisMSUElement = ThisMSUElementAsHead,
	  EmptyHeadFound is 1
	; ModifiedThisMSUElement = ThisMSUElementWithEmptyHead,
	  set_empty_heads_in_MSU(RestMSUElements,[ThisMSUElementWithEmptyHead|EmptyHeadSoFar],
				 ThisHeadIsEmpty, EmptyHeadOut,
				 RestMSUElementsWithEmptyHeads,
				 ThisHeadIsEmpty, EmptyHeadFound)
	).

% set_empty_head_found(EmptyHeadFoundIn, EmptyHeadOut, EmptyHeadFoundOut) :-
% 	EmptyHeadFoundOut is EmptyHeadFoundIn \/ EmptyHeadOut.

make_into_head([], []).
make_into_head([H|T], [NewH|NewT]) :-
	( arg(1, H, FeatureList),
	  get_from_list(metaconc, FeatureList, _Metaconc) ->
	  NewH = head(FeatureList),
	  NewT = T
	; NewH = H,
	  make_into_head(T, NewT)
	).

% conditional empty heads are those that qualify as such
% if preceded by certain semantic groups. [gene] family, for example.
make_empty_head(MSU, RestMSUs, MSUElementWithEmptyHead, 1) :-
       MSU = [Head|_],
       is_conditional_msu_empty_head(MSU,RestMSUs),
       arg(1,Head,HeadList),
       MSUElementWithEmptyHead = empty_head(HeadList),
       !.
make_empty_head(MSU, _RestMSUs, MSUElementWithEmptyHead, EmptyHeadSeen) :-
       !,
       MSU = [Head|_],
       ( is_msu_empty_head(MSU) ->
 	  arg(1,Head,HeadList),
 	  MSUElementWithEmptyHead = empty_mod(HeadList),
	  EmptyHeadSeen is 1
	; make_empty_head_aux(Head, MSUElementWithEmptyHead, EmptyHeadSeen)
	).

make_empty_head_aux(head(HeadList), MSUElementWithEmptyHead, EmptyHeadSeen) :-
	!,
	( is_empty_head(HeadList) ->
	  MSUElementWithEmptyHead = empty_head(HeadList),
	  EmptyHeadSeen is 1
	; MSUElementWithEmptyHead = head(HeadList),
	  EmptyHeadSeen is 0
	).
	
make_empty_head_aux(mod(HeadList), MSUElementWithEmptyHead, EmptyHeadSeen) :-
	!,
	( is_empty_head(HeadList) ->
	  MSUElementWithEmptyHead = empty_mod(HeadList),
	  EmptyHeadSeen is 1
	; MSUElementWithEmptyHead = mod(HeadList),
	  EmptyHeadSeen is 0
	).
make_empty_head_aux(Other, Other, 0).

% If there is an empty_head_base_2N, whose head or some of its mod's are not empty by themselves,
% we need to go back and update them as empty, also.
% An example was 'therapy response'. 'response' is not empty, and only therapy was marked as 'empty_mod'.
update_rest_with_empty_heads([],Empty,Empty,[]) :- !.
update_rest_with_empty_heads([empty_mod(EmptyMod)|Rest],0,EmptyOut,[empty_mod(EmptyMod)|RestOut]) :-
	!,
	update_rest_with_empty_heads(Rest,1,EmptyOut,RestOut).
update_rest_with_empty_heads([empty_head(EmptyHead)|Rest],0,EmptyOut,[empty_head(EmptyHead)|RestOut]) :-
	!,
	update_rest_with_empty_heads(Rest,1,EmptyOut,RestOut).
update_rest_with_empty_heads([mod(ModList)|Rest],1,EmptyOut,[empty_mod(ModList)|RestOut]) :-
	!,
	update_rest_with_empty_heads(Rest,1,EmptyOut,RestOut).
update_rest_with_empty_heads([head(HeadList)|Rest],1,EmptyOut,[empty_head(HeadList)|RestOut]) :-
	!,
	update_rest_with_empty_heads(Rest,1,EmptyOut,RestOut).
update_rest_with_empty_heads([MSUElement|Rest],EmptyIn,EmptyOut,[MSUElement|RestOut]) :-
	update_rest_with_empty_heads(Rest,EmptyIn,EmptyOut,RestOut).


reverse_EITHER_and_prep([], []).
reverse_EITHER_and_prep([H|T], TweakedAnalysis) :-
	reverse_EITHER_and_prep_1(T, H, TweakedAnalysis).

reverse_EITHER_and_prep_1([], LastMSU, [LastMSU]).
reverse_EITHER_and_prep_1([NextMSU|TempRestMSUs], ThisMSU, TweakedMSUs) :-
	( ThisMSU = [prep(PrepList)],
	  NextMSU = [conj(ConjList)],
	  member(inputmatch([Word]), ConjList),
	  either(Word),	    
	  TempRestMSUs = [TempRestMSU1|RestRestMSUs],
	  EnhancedThisMSU = [prep(PrepList)|TempRestMSU1] ->
	  TweakedMSUs = [NextMSU,EnhancedThisMSU|RestTweakedMSUs],
	  RestMSUs = RestRestMSUs
	; TweakedMSUs = [ThisMSU|RestTweakedMSUs],
	  RestMSUs = [NextMSU|TempRestMSUs]
	),
	reverse_EITHER_and_prep(RestMSUs, RestTweakedMSUs).

	  
either(either).
either(neither).

link_subordinators_to_predicates(AdjustedAnalysis,_ConjunctList,SubordinatorPredicateList) :-
	link_subordinators_to_predicates_aux(AdjustedAnalysis,[],SubordinatorPredicateList),
% This was for VP coordination which is not addressed at the moment
% Should be taken up again later
%	adjust_with_conjuncts(SubordinatorPredicateList0,ConjunctList,[],SubordinatorPredicateList),
	!.

link_subordinators_to_predicates_aux([],_RightAnalysis,[]) :- !.
link_subordinators_to_predicates_aux([MSU|RestMSUs],RightAnalysisIn,[Subordinator-MSU|RestOut]) :-
%	get_from_list(verb,MSU,Verb),
	main_verb(MSU,RightAnalysisIn),
	find_subordinator(RestMSUs,Subordinator,NewRestMSU),
	!,
	append(RightAnalysisIn,[MSU],RightAnalysisOut),
	link_subordinators_to_predicates_aux(NewRestMSU,RightAnalysisOut,RestOut).
link_subordinators_to_predicates_aux([MSU|RestMSUs],RightAnalysisIn,RestOut) :-
	append(RightAnalysisIn,[MSU],RightAnalysisOut),
	link_subordinators_to_predicates_aux(RestMSUs,RightAnalysisOut,RestOut).

%adjust_with_conjuncts([],_ConjunctList,SubPredList,SubPredList).
%adjust_with_conjuncts([Subordinator-MSU|Rest],ConjunctList,
%	              SubPredListIn,SubPredListOut) :-
%	find_verb_conjuncts(MSU,ConjunctList,ConjunctMSUs),
%	link_subordinator_to_conjuncts(Subordinator,ConjunctMSUs, SubPredListConjunct),
%	append([Subordinator-MSU],SubPredListConjunct,SubPredListNext),
%        adjust_with_conjuncts(Rest,ConjunctList,SubPredListNext,SubPredListOut).

%find_verb_conjuncts(_MSU,[],[]) :- !.
%find_verb_conjuncts(MSU,[coord(_Coordinator,_,_,[MSU|Other])|RestConjunct],
%	            [Other|RestOut]) :-
%	find_verb_conjuncts(MSU,RestConjuncts,RestOut).
%find_verb_conjuncts(MSU,[coord(_Coordinator,_,_,[Other|MSU])|RestConjunct],
%	            [Other|RestOut]) :-
%	find_verb_conjuncts(MSU,RestConjuncts,RestOut).

%link_subordinator_to_conjuncts(_Subordinator,[],[]) :- !.
%link_subordinator_to_conjuncts(Subordinator,[ConjunctMSU|RestMSUs],
%                               [Subordinator-ConjunctMSU|RestSubPredOut]) :-
%	link_subordinator_to_conjuncts(Subordinator,RestMSUs,RestSubPredOut).
	                              
find_subordinator([MSU|RestMSUs],Relativizer,RestMSUs) :-
	check_for_relativizer([MSU]),
	!,
	get_from_list(pron,MSU,Relativizer0),
	Relativizer = [pron(Relativizer0)].
find_subordinator([MSU|RestMSUs],Complementizer,RestMSUs) :-
	get_from_list(compl, MSU, ComplList),
	!,
	Complementizer = [compl(ComplList)].
% This is not fully implemented yet,
% Multi-word subordinating conjunctions such as "in order to" need
% to be identified
% When that is done, get_from_list would be meaningless
find_subordinator([MSU|RestMSUs],SubConjunction,RestMSUs) :-
	get_from_list(conj,MSU,ConjList),
	subordinating_conjunction(MSU,RestMSUs),
	!,
	SubConjunction = [conj(ConjList)].
find_subordinator([MSU|RestMSUs],Subordinator,Rest) :-
	(get_from_list(adv,MSU,_AdvList)
        ; get_from_list(aux,MSU,_Aux)
        ),
	!,
	find_subordinator(RestMSUs,Subordinator,Rest).
