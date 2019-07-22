
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

% File:	    generate_varinfo.pl
% Module:   generate_varinfo
% Author:   tcr
% Purpose:  generate variant information from a lexical entry



% ----- Module declaration and exported predicates

:- module(generate_varinfo, [
	generate_variant_info/2
   ]).

% ----- Imported predicates

:- use_module(lexicon(lex_access), [
	get_im_varlist_for_all_forms/2
  ]).

:- use_module(lexicon(qp_lexicon), [
	normalize_token/2
  ]).

:- use_module(metamap(metamap_tokenization), [
	local_alnum/1
  ]).

:- use_module(skr(skr_utilities), [
	fatal_error/2,
	send_message/2
  ]).

% :- use_module(skr_lib(ctypes), [
%         is_alnum/1
%   ]).

:- use_module(skr_lib(nls_system), [
        control_option/1,
        control_value/2
  ]).

:- use_module(skr_lib(sicstus_utils), [
	lower/2,
	midstring/6,
	string_size/2
  ]).

:- use_module(library(lists), [
	append/2
  ]).


% ******************************* GENERATE_VARIANT_INFO *******************************

generate_variant_info([], []).
generate_variant_info([LexiconOrOther:List|RestDefinitions], Variants) :-
	generate_variant_info_1(LexiconOrOther, List, RestDefinitions, Variants).

generate_variant_info_1(unknown, UnkList, RestDefinitions, [ThisItem|NewGap]) :-
	!,
	get_lex_item(UnkList, inputmatch, InpMatch),
	InpMatch = [ThisItem],
	generate_variant_info(RestDefinitions, NewGap).
generate_variant_info_1(lexicon, [lexmatch:[LexMatch],
				  inputmatch:InputMatch,
				  records:Records|_],
			RestDefinitions, [LexMatch:ThisVarInfo|NewGap]) :-
	!,
	get_all_base_forms_from_lexical_records(Records, AllBaseForms0),
	append(AllBaseForms0, AllBaseForms1),
	sort(AllBaseForms1, AllBaseForms),
	% get_im_varlist(LexMatchLC, VarInfo),
	get_im_varlist_for_all_forms(AllBaseForms, VarInfo0),
	append(VarInfo0, VarInfo1),
	sort(VarInfo1, VarInfo),
	( VarInfo = [FirstVar|_] ->
	  FirstVar = _Token:[StopGap|_]
	; StopGap = []
	),
	VariantFound is 0,
	get_this_variant(VarInfo, VariantFound, StopGap,
			 LexMatch, InputMatch, ThisVarInfo, VariantTail),
	% If ThisVarInfo is still uninstantiated, we have a problem!
	
	% format(user_output, 'ThisVarInfo: ~q~n', [ThisVarInfo]),	
	% append(ThisVarInfo, [InputMatch], ThisVarInfoAndInputMatch),   % Lan needs InputMatch
	VariantTail = [inputmatch:InputMatch], 
	generate_variant_info(RestDefinitions, NewGap).

% This is for shapes, punctuation, and perhaps other stuff
generate_variant_info_1(Other, OtherList,
			RestDefinitions, [Other:OtherList|NewGap]) :-
	generate_variant_info(RestDefinitions, NewGap).


get_all_base_forms_from_lexical_records([], []).
get_all_base_forms_from_lexical_records([lexrec:[base:[CitationForm],
						 spelling_variants:SpVars|_]|RestLexRecs],
					[[CitationForm|SpVars]|RestBaseForms]) :-
	get_all_base_forms_from_lexical_records(RestLexRecs, RestBaseForms).
				       

% ----- GET_LEX_ITEM

get_lex_item([Item:ItemInfo|_More], Item, ItemInfo) :- !.
get_lex_item([_Other|More], Item, ItemInfo) :-
	get_lex_item(More, Item, ItemInfo).

% ----- GET_THIS_VARIANT
% LexKeys other than forms of *be* and *have* have the format: Entry:VarList
% LexKeys for forms of *be* have the format: 'VarForm;Agr':VarList

get_this_variant([], _VariantFound, _StopGap,
		 LexMatch, _InputMatch, [unknown:[base]|VarInfo], VarInfo) :-
	send_message('### WARNING: "~w" has no i-variants!~n', [LexMatch]).
get_this_variant([H|T], VariantFound, StopGap,
		 LexMatch, InputMatch, ThisVarInfo, VariantTail) :-	
	get_this_variant_1([H|T], VariantFound, StopGap,
			   LexMatch, InputMatch, ThisVarInfo, VariantTail).

get_this_variant_1([], VariantFound, _StopGap,
%		   LexMatch, InputMatch, [StopGap|ThisVarInfo], ThisVarInfo) :-
		   LexMatch, InputMatch, ThisVarInfo, ThisVarInfo) :-
	( VariantFound =:= 0 ->
	  send_message('### WARNING: Mismatch in LexMatch "~w" and InputMatch ~w ~n',
		 [LexMatch,InputMatch])
	; true
	).
get_this_variant_1([LexKey:[ThisList]|MoreVariants], _VariantFoundIn,
		   StopGap, LexMatch, InputMatch, [ThisList|Rest], Tail) :-
	lower(LexKey, LowerLexKey),
	% LexMatch can end in "'s"
%	( lower_apostrophe_s(LexMatch, LowerLexKey) ->
%	  true
%	; lower_apostrophe_s(LowerLexKey, LexMatch) ->
%	  true
	% The normalization is necessary for e.g., "St George's Respiratory Questionnaire"
	( normalize_token(LexMatch, NormalizedLexMatch),
	  normalize_token(LexKey,   NormalizedLexKey),
	  NormalizedLexMatch == NormalizedLexKey ->
	  true
	; lower(LexMatch, LexMatchLC),
	  atom_codes(LexMatchLC, LexMatchCodes),
	  atom_codes(LowerLexKey, LowerLexKeyCodes),
	  same_alnums(LexMatchCodes, LowerLexKeyCodes)
	),
        !,
	VariantFound is 1,
	% format(user_output, 'ThisList: ~q~n', [ThisList]),
	get_this_variant_1(MoreVariants, VariantFound,
			   StopGap,  LexMatch, InputMatch, Rest, Tail).
get_this_variant_1([_Other|MoreVariants], VariantFound,
		   StopGap, LexMatch, InputMatch, ThisVarInfo, Rest) :-
	get_this_variant_1(MoreVariants, VariantFound,
			   StopGap, LexMatch, InputMatch, ThisVarInfo, Rest).

% Either list can run out first,
% as long as what's left over in the other list doesn't begin with an alnum.
same_alnums([], []) :- !.
same_alnums([H1|T1], [H2|T2]) :-
	!,
	( H1 == H2 ->
	  same_alnums(T1, T2)
	; \+ local_alnum(H1) ->
	  same_alnums(T1, [H2|T2])
	; \+ local_alnum(H2) ->
	  same_alnums([H1|T1], T2)
	).
same_alnums([H|T], []) :- \+ contains_alnum([H|T]), !.
same_alnums([], [H|T]) :- \+ contains_alnum([H|T]).

contains_alnum([H|T]) :-
	( local_alnum(H) ->
	  true
	; contains_alnum(T)
	).

% lower_apostrophe_s(LexMatch, LowerLexKey) :-
% 	( lower(LexMatch, LowerLexKey) ->
% 	  true
% 	; midstring(LexMatch, '''s', LexMatchWithoutApostropheS, _Before, _Length, _After),
% 	  lower(LexMatchWithoutApostropheS, LowerLexKey)
% 	).
