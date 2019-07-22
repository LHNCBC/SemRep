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

% File:	    lex_access.pl
% Module:   Lexicon Access
% Author:   Lan
% Purpose:  Provide access to the new lexicon access facility

:- module(lex_access, [
	% get_base_forms_for_form/4,
        get_categories_for_form/2,
	get_base_forms_for_form_with_cats/3,
	get_derivational_variants_for_form/4,
	get_spellings_and_inflections_for_form/4,
	get_variants_for_form/2,
	get_im_varlist/2,
	get_im_varlist_with_cats/3,
	get_im_varlist_for_form/3,
	get_im_varlist_for_all_forms/2,
	% initialize_lexicon/2,
	% is_a_base_form/2,
	is_a_base_form_with_categories/2,
	is_a_form/1
    ]).

:- use_module(skr_db(db_access), [
	db_get_lex_base_forms/2,
	db_get_lex_base_forms_with_cat/3,
	db_get_lex_cats/2,
	db_get_lex_cats/2,
	db_get_lex_prefix_EUIs/2,
	db_get_lex_record_list/2,
	db_get_lex_im_varlist/2
   ]).

% :- use_module(lexicon(qp_lexicon), [
	% lex_init/2,
	% lex_cit_ci_vars/2,
	% lex_form_ci_cats/2,
	% lex_form_ci_recs/2,
	% lex_form_ci_vars/2,
	% lex_is_a_form_ci/1,
	% lex_is_a_root_ci/1,
	% lex_is_a_root_ci_cats/2,
	% remove_zero_EUI/2
%     ]).

:- use_module(lexicon(qp_lex_util), [
	lex_form_ci_ord/4
	% lex_get_base_from_record_3/3
	% lex_get_spvar_from_record/2
    ]).

% :- use_module(skr_lib(ctypes), [
%         is_punct/1
%    ]).

:- use_module(metamap(metamap_tokenization), [
	local_punct/1
   ]).

:- use_module(skr_lib(nls_system), [
        control_option/1,
        control_value/2
   ]).

:- use_module(skr_lib(sicstus_utils), [
	lower/2,
	midstring/6
   ]).

:- use_module(skr(skr_utilities), [
        fatal_error/2
   ]).

:- use_module(library(lists), [
        append/2
   ]).

/* ************************************************************************
   ************************************************************************
   ************************************************************************
                               Lexicon Access
   ************************************************************************
   ************************************************************************
   ************************************************************************ */


/* initialize_lexicon(-Lexicon, -Index)

initialize_lexicon/2 and initialize_lexicon_quietly/2 call lex_init/2 and
lex_init_quietly/2, respectively. They also call c_initialize_lexAccess/0
for the new lexicon access. This initialization will eventually allow
the caller to specify lexiconVersion but for now it will be fixed. */

% initialize_lexicon(L,I) :-
%     % temp
%     lex_init(L,I),
%     !.
% initialize_lexicon(_L, _I) :-
%     fatal_error('Cannot connect to a lexicon.~n', []).

% is_a_base_form_with_categories(Form, Categories) :-
% 	  lex_is_a_root_ci_cats(Form, Categories).

% get_im_varlist first computes the citation form(s) for the token,
% then computes the variants of the citation form(s).
get_im_varlist(BaseForm, VarList) :-
	get_im_varlist_TOGGLE(BaseForm, [], VarList).

get_im_varlist_with_cats(BaseForm, Categories, VarList) :-
	get_im_varlist_TOGGLE(BaseForm, Categories, VarList).

get_im_varlist_TOGGLE(BaseForm, _Categories, VarInfo) :-
	  get_im_varlist_for_form(BaseForm, VarInfo, []).
%	( control_value(lexicon, c) ->
%	  lex_form_ci_vars(BaseForm, VarInfo)
%	; control_value(lexicon, db) ->
%	  get_im_varlist_for_form(BaseForm, VarInfo, [])
%	; fatal_error('### ERROR: lexicon setting must be either "c" or "db".~n', [])
%	).

% get_im_varlist_for_all_forms([], []).
% get_im_varlist_for_all_forms([FirstBaseForm|RestBaseForms], VarList) :-
% 	get_im_varlist_for_form(FirstBaseForm, VarList, VarListTail),
% 	get_im_varlist_for_all_forms(RestBaseForms, VarListTail).

get_im_varlist_for_all_forms([], []).
get_im_varlist_for_all_forms([FirstBaseForm|RestBaseForms], [FirstVarList|RestVarLists]) :-
	get_variants_for_form_TOGGLE(FirstBaseForm, FirstVarList),
	get_im_varlist_for_all_forms(RestBaseForms, RestVarLists).


% % get_variants_for_form simply computes the variants of the given token,
% % and not the variants of the token's citation form(s).
% get_variants_for_form(Form, VarList) :-
% 	get_variants_for_form_TOGGLE(Form, VarList).
 
get_variants_for_form_TOGGLE(Form, VarList) :-
	get_im_varlist_for_form(Form, VarList, []).
% 	( control_value(lexicon, c) ->
% 	  lex_form_ci_vars(Form, VarList)
%  	; control_value(lexicon, db) ->
% 	  get_im_varlist_for_form(Form, VarList, [])
% 	; fatal_error('### ERROR: lexicon setting must be either "c" or "db".~n', [])
% 	).

get_im_varlist_for_form(BaseForm, VarList, VarListTail) :-
%	control_value(lexicon, db),
	lower(BaseForm, BaseFormLC),
	db_get_lex_im_varlist(BaseFormLC, Response),
        % Response is a list of lists of triples, e.g.,
	% [[['heart attack',noun,base],['heart attacks',noun,plural]]]
	convert_all_to_lexicon_format(Response, VarList, VarListTail).


% takes as input a list of lists of triples, e.g.,
% [[['heart attack',noun,base],['heart attacks',noun,plural]]]
convert_all_to_lexicon_format([], LexiconFormat, LexiconFormat).
convert_all_to_lexicon_format([H|T], LexiconFormatIn, LexiconFormatOut) :-
	H = [Token,LexicalCategory,Feature],
	LexiconFormatElement = Token:[LexicalCategory:[Feature]],
	LexiconFormatIn = [LexiconFormatElement|LexiconFormatNext],
	convert_all_to_lexicon_format(T, LexiconFormatNext, LexiconFormatOut).

% takes as input a list of triples, e.g.,
% [['heart attack',noun,base],['heart attacks',noun,plural]]
% convert_one_to_lexicon_format([], LexiconFormat, LexiconFormat).
% convert_one_to_lexicon_format([H|T], LexiconFormatIn, LexiconFormatOut) :-
% 	H = [Token,LexicalCategory,Feature],
% 	LexiconFormatElement = Token:[LexicalCategory:[Feature]],
% 	LexiconFormatIn = [LexiconFormatElement|LexiconFormatNext],
% 	convert_one_to_lexicon_format(T, LexiconFormatNext, LexiconFormatOut).

get_categories_for_form(Form, LexCats) :-
	get_categories_for_form_TOGGLE(Form, LexCats).

get_categories_for_form_TOGGLE(Form, LexCats) :-
	db_get_lex_cats(Form, LexCats).
% 	( control_value(lexicon, c) ->
% 	  lex_form_ci_cats(Form, LexCats)
%  	; control_value(lexicon, db) ->
% 	  db_get_lex_cats(Form, LexCats)
% 	; fatal_error('### ERROR: lexicon setting must be either "c" or "db".~n', [])
% 	).

get_spellings_and_inflections_for_form(Term, Categories, Spelling, Inflections) :-
	lex_form_ci_ord(Term, Categories, Spelling, Inflections).


% get_base_forms_for_form_apostrophe_s(FormAtom, Categories, BaseFormList) :-
% % First try to find FormAtom in the lex_form table.
% 	( Categories = [Category|_] ->
% 	  true
% 	; Category = []
% 	),
% 	( db_get_lex_base_forms_with_cat(FormAtom, Category, BaseFormList),
% 	  BaseFormList = [_|_] ->
% 	  true
% 	  % Next, try FormAtom without a trailing "'s" if there is one.
% 	; midstring(FormAtom, '''s', FormAtomWithoutApostropheS, _Before, 2, _After),
% 	  db_get_lex_base_forms_with_cat(FormAtomWithoutApostropheS, Category, BaseFormList),
% 	  BaseFormList = [_|_] ->
% 	  true
% 	  % Finally, find the lexical record(s), if there are any,
% 	  % and extract the citation forms and spvars.
% 	; db_get_lex_prefix_EUIs(FormAtom, EUIList0),
% 	  remove_zero_EUI(EUIList0, EUIList),
% 	  db_get_lex_record_list(EUIList, LexicalRecordList) ->
% 	  get_all_base_forms(LexicalRecordList, BaseFormList)
% 	; remove_punct_chars(FormAtom, FormAtomWithoutPunct),
% 	  FormAtom \== FormAtomWithoutPunct,
% 	  get_base_forms_for_form_apostrophe_s(FormAtomWithoutPunct, Categories, BaseFormList),
% 	  BaseFormList \== [] ->
% 	  true	  
% 	; BaseFormList = []
% 	).

% remove_punct_chars(Atom, AtomWithoutPunctChars) :-
%        atom_codes(Atom, CodeList),
%        (  foreach(Code, CodeList),
% 	  fromto(CodesWithoutPunctChars, S0, S, [])
%        do ( local_punct(Code) ->
% 	    S0 = S ;
% 	    S0 = [Code|S]
% 	  )
%        ),
%        atom_codes(AtomWithoutPunctChars, CodesWithoutPunctChars).
       
% get_all_base_forms(LexicalEntries, BaseForms) :-
% 	(  foreach(LE, LexicalEntries),
% 	   foreach(BF, BaseForms0)
% 	do LE = lexrec:[base:[CitationForm],spelling_variants:SpellingVariants|_],
% 	   BF = [CitationForm|SpellingVariants]
% 	),
% 	append(BaseForms0, BaseForms1),
% 	(  foreach(BF1,   BaseForms1),
% 	   foreach(LCBF1, BaseForms)
% 	do lower(BF1, LCBF1)
% 	).
 
get_base_forms_for_form_with_cats(Form, CategoryList, BaseFormList) :-
	get_base_forms_for_form_with_cats_TOGGLE(Form, CategoryList, BaseFormList).

get_base_forms_for_form_with_cats_TOGGLE(Form, CategoryList, BaseFormList) :-
	get_base_forms_for_form_with_cats_apostrophe_s(Form, CategoryList, BaseFormList).
%  	( control_value(lexicon, c) ->
%  	  lex_form_ci_recs(Form, LexRecords),
%  	   (findall(Cit,
%  		    (member(LexRecord,LexRecords),
%  		     lex_get_base_from_record_3(LexRecord, CategoryList, Cit)),
%  		    BaseFormList) ->
%  	             true
%  	   ;   BaseFormList=[]
%  	   )
% 	  % BDB version
%  	; control_value(lexicon, db) ->
% 	  get_base_forms_for_form_with_cats_apostrophe_s(Form, CategoryList, BaseFormList)
% 	; fatal_error('### ERROR: lexicon setting must be either "c" or "db".~n', [])
%  	).

get_base_forms_for_form_with_cats_apostrophe_s(FormAtom, CategoryList, BaseFormList) :-
	CategoryList = [Category],
	( db_get_lex_base_forms_with_cat(FormAtom, Category, BaseFormList),
	  BaseFormList = [_|_] ->
	  BaseFormList = BaseFormList
	; midstring(FormAtom, '''s', FormAtomWithoutApostropheS, _Before, 2, _After),
	  db_get_lex_base_forms_with_cat(FormAtomWithoutApostropheS, Category, BaseFormList),
	  BaseFormList = [_|_] ->
	  BaseFormList = BaseFormList
	; BaseFormList = []
	).

is_a_form(PossibleForm) :-
 	db_get_lex_base_forms(PossibleForm, Result),
	Result \== [].
% 	( control_value(lexicon, c) ->
% 	  lex_is_a_form_ci(PossibleForm)
% 	; db_get_lex_base_forms(PossibleForm, Result),
% 	  Result \== []
% 	).
