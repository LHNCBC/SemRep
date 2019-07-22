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

/* qp_lexicon.pl - all lexicon-related predicates.
*/

/*
    Some general comments regarding predicate names:

    - All retrieval predicates start with the prefix "lex_"
    - After that comes one of "cit|root|form".  This specifies
      whether the search is done for a citation form, a root form
      (lexical base) or any form in the lexicon resp.
    - After that comes "cs|ci" meaning case sensitive or
      case insensitive search.
    - Finally comes "recs|vars|cats" for retrieval of records,
      variants, categories etc.

    All predicates have matching predicates with small arity
    for which the lexicon and index default to the standard.
*/

:- module(qp_lexicon, [
	concat_atoms_intelligently/2,
	lex_form_ci_recs/2,	% lexical form, case insensitive
	lex_form_ci_cats/2,	% lexical item, case insensitive, returns cats
	% lex_cit_ci_vars/2,	% citation form, case insensitive, returns variants
	lex_form_ci_vars/2,	% lexical item, case insensitive, returns variants
	lex_form_ci_var_lists/3,
	% lex_is_a_root_ci/1,	% root form, case insensitive
	% lex_is_a_root_ci_cats/2,
	% lex_is_a_form_ci/1,	% lexical form, case insensitive
	lex_form_ci_recs_input/3,
        % lex_init/2,
	default_lexicon_file/1,
	default_index_file/1,
	normalize_token/2,
	reformat_list/2,
	remove_zero_EUI/2
    ]).

:- use_module(lexicon(lex_access), [
	get_im_varlist_with_cats/3,
	get_im_varlist_for_form/3
   ]).

:- use_module(lexicon(qp_fm_lexrec), [
	fm_lexical_record/4
   ]).

:- use_module(lexicon(qp_lookup), [
	punct_token/2
   ]).

:- use_module(lexicon(qp_recio), [
	read_lex_record/3
   ]).

:- use_module(lexicon(qp_token), [
	tokenize_string/2
   ]).

:- use_module(metamap(metamap_parsing), [
	re_attach_apostrophe_s_to_prev_word/3
   ]).

:- use_module(metamap(metamap_tokenization), [
	local_punct/1,
	tokenize_text/2
   ]).

:- use_module(skr(skr_utilities), [
	 check_valid_file_type/2,
	 fatal_error/2,
	 send_message/2
    ]).


:- use_module(skr_db(db_access), [
	db_get_lex_prefix_EUIs/2,
	db_get_lex_record/2,
	db_get_lex_record_list/2,
	db_get_lex_im_varlist/2,
	default_release/1
   ]).

% :- use_module(skr_lib(ctypes), [
% 	 is_punct/1
%     ]).

:- use_module(skr_lib(flatten), [
	 flatten/2
    ]).

:- use_module(skr_lib(mincoman), [
	punc_mark1/1
   ]).

:- use_module(skr_lib(nls_strings), [
	atom_codes_list/2,
	trim_and_compress_whitespace/2
   ]).

:- use_module(skr_lib(nls_system), [
	control_option/1,
	control_value/2
   ]).

:- use_module(skr_lib(sicstus_utils), [
	concat_atom/2,
	concat_atom/3,
	lower/2
   ]).

:- use_module(text(text_object_util), [
	ex_lbracket/1
    ]).

:- use_module(library(between), [
	between/3
    ]).

:- use_module(library(lists), [
	 append/2,
	 % delete/3,
	 last/2,
	 prefix_length/3
	 % rev/2,
	 % sublist/5
    ]).

% :- use_module(library(ordsets), [
% 	ord_symdiff/3
%    ]).

% :- use_module(library(sets), [
% 	 intersection/3
%     ]).
 
% :- use_module(library(system), [
% 	environ/2
%    ]).

% :- dynamic default_lexicon_file/1.
% :- dynamic default_index_file/1.
% :- dynamic lexicon_files/2.

 %%% Define the foreign interface
%  foreign_resource(qp_lexicon, [
% 	 % c_lex_cit, c_lex_root, c_lex_form,
% 	 c_lex_cit, c_lex_form,
% 	 c_lex_form,
% 	 % c_lex_cit_cats, c_lex_root_cats,
% 	 c_lex_form_cats,
% 	 c_lex_is_a_root, c_lex_is_a_form,
% 	 c_lex_is_a_root_cats,
% 	 c_lex_form_input,
% 	 c_get_varlist
%      ]).
% 
%  % Replaced with new lexAccess code
% foreign(c_lex_cit,            c,
% 	c_lex_cit(+string, +string, +integer, +integer, +integer, -term, [-integer])).
% 
%  % foreign(c_lex_root,           c,
%  % 	c_lex_root(+string, +string, +integer, +integer, +integer, -term, [-integer])).
% 
% foreign(c_lex_form,           c,
% 	c_lex_form(+string, +string, +integer, +integer, +integer, -term, [-integer])).
% 
% foreign(c_lex_form_cats,      c,
% 	c_lex_form_cats(+string, +string, +integer, +integer, -term, [-integer])).
% 
% foreign(c_lex_is_a_root,      c,
% 	c_lex_is_a_root(+string, +string, +integer, +integer, [-integer])).
% 
% foreign(c_lex_is_a_form,      c,
% 	c_lex_is_a_form(+string, +string, +integer, +integer, [-integer])).
% 
% foreign(c_lex_is_a_root_cats, c,
% 	c_lex_is_a_root_cats(+string, +string, +integer, +integer, +term, [-integer])).
% 
% foreign(c_lex_form_input,     c,
% 	c_lex_form_input(+string, +integer, +term, -term, [-integer])).
% 
% foreign(c_get_varlist,        c,
% 	c_get_varlist(+string, +integer, -term, [-integer])).

% :- load_foreign_resource('../../qp_lexicon').
% :- environ('DYNAMIC_LIB_DIR',DynamicLibDir),
%    atom_concat(DynamicLibDir,'/qp_lexicon',QpLexiconSo),
%    load_foreign_resource(QpLexiconSo).

%%% lex_init(-Lexicon, -Index)
%%% This predicate will return the names of the default location of the lexicon files
% lex_init(LexiconFile, LexiconIndex) :-
% 	lexicon_files(LexiconFile, LexiconIndex),
% 	!.  % already initialized
% lex_init(LexiconFile, LexiconIndex) :-
% 	lex_init_quietly(LexiconFile, LexiconIndex),
% 	conditionally_announce_lexicon(LexiconFile).
% 
% lex_init_quietly(LexiconFile, LexiconIndex) :-
% 	lexicon_files(LexiconFile, LexiconIndex),
% 	!.  % already initialized
% lex_init_quietly(LexiconFile, LexiconIndex) :-
% 	( control_value(lexicon, c) ->
% 	  get_lexicon_year(LexiconYear),
% 	  % environ('DEFAULT_LEXICON_FILE', Lexicon),
% 	  environ('LEXICON_DATA', LexiconDataDir),
% 	  concat_atom([LexiconDataDir, '/lexiconStatic', LexiconYear], LexiconFile),
% 	  check_valid_file_type(LexiconFile, 'Lexicon'),
% 	  % environ('DEFAULT_LEXICON_INDEX_FILE', Index),
% 	  concat_atom([LexiconFile, 'Ind'], LexiconIndex),
% 	  concat_atom([LexiconIndex, 'ByEui.dbx'], EuiIndexFile),
% 	  check_valid_file_type(EuiIndexFile, 'Lexicon Index'),
% 	  concat_atom([LexiconIndex, 'ByInfl.dbx'], InflIndexFile),
% 	  check_valid_file_type(InflIndexFile, 'Lexicon Index'),
% 	  retractall(default_lexicon_file(_)),
% 	  assert(default_lexicon_file(LexiconFile)),
% 	  retractall(default_index_file(_)),
% 	  assert(default_index_file(LexiconIndex)),
% 	  retractall(lexicon_files(_,_)),
% 	  assert(lexicon_files(LexiconFile,LexiconIndex))
% 	; LexiconFile = stub,
% 	  LexiconIndex = stub,
% 	  assert(default_lexicon_file(LexiconFile)),
% 	  assert(default_index_file(LexiconIndex)),
% 	  assert(lexicon_files(LexiconFile,LexiconIndex))
% 	).

% get_lexicon_year(NormalizedLexiconYear) :-
% 	default_release(DefaultRelease),
% 	atom_codes(DefaultRelease, Codes),
% 	Codes = [D1,D2,D3,D4,_AorB1,_AorB2],
% 	atom_codes(LexiconYear, [D1,D2,D3,D4]),
% 	normalize_lexicon_year(LexiconYear, NormalizedDefaultLexiconYear),
% 	% Is the lexicon year explicitly specified on the command line?
% 	( control_value(lexicon_year, SpecifiedLexiconYear),
% 	  normalize_lexicon_year(SpecifiedLexiconYear, NormalizedSpecifiedLexiconYear),
% 	  NormalizedDefaultLexiconYear \== NormalizedSpecifiedLexiconYear ->
% 	  send_message('### WARNING: Overriding default lexicon ~w with ~w.~n',
% 		       [NormalizedDefaultLexiconYear, SpecifiedLexiconYear]),
% 	  NormalizedLexiconYear = NormalizedSpecifiedLexiconYear
% 	; NormalizedLexiconYear = NormalizedDefaultLexiconYear
% 	).
% 
% normalize_lexicon_year(LexiconYear, NormalizedLexiconYear) :-
% 	( LexiconYear == '99' ->
% 	  NormalizedLexiconYear = '1999'
% 	; LexiconYear == '1999' ->
% 	  NormalizedLexiconYear = LexiconYear
% 	; atom_length(LexiconYear, LexiconLength),
% 	  ( LexiconLength =:= 2 ->
% 	    concat_atom(['20', LexiconYear], NormalizedLexiconYear)
% 	  ; NormalizedLexiconYear = LexiconYear
% 	  )
% 	).
% 
% conditionally_announce_lexicon(Lexicon) :-
% 	( control_value(lexicon, c),
% 	  \+ control_option(silent) ->
% 	  format('Accessing lexicon ~a.~n', [Lexicon])
% 	; true
% 	).

%%% Retrieve records given root form

% lex_form_ci_recs(Form, LexicalRecords) :-
% 	  default_lexicon_file(Lexicon),
% 	  default_index_file(Index),
% 	  lex_recs(form, Form, LexicalRecords, 1, 0, Lexicon, Index).

%%% Retrieves category given root form

% lex_form_ci_cats(Form, LexicalCategories) :-
% 	  default_index_file(Index),
% 	  lex_cats(form, Form, LexicalCategories, 1, Index).


%% Retrieves inflectional variant info for a citation form

%%% lex_cit_ci_vars(+Cit, -Vars)
% lex_cit_ci_vars(Cit, Vars) :-
% 	default_lexicon_file(Lexicon),
% 	default_index_file(Index),
% 	lex_vars(cit, Cit, Vars, 1, Lexicon, Index).

%%% Retrieves inflectional variant info for a lexical item

%%% lex_form_ci_vars(+Form, -Vars)
% lex_form_ci_vars(Form, Vars) :-
% 	default_lexicon_file(Lexicon),
% 	default_index_file(Index),
% 	lex_vars(form, Form, Vars, 1, Lexicon, Index).

lex_form_ci_var_lists(Form, Categories, VarLists) :-
	get_im_varlist_with_cats(Form, Categories, VarLists).
% 	( control_value(lexicon, c) ->
% 	  lex_var_lists(form, Form, VarLists0, 1, Lexicon, Index),
% 	  append(VarLists0, VarList)
% 	; control_value(lexicon, db) ->
% 	  get_im_varlist_with_cats(Form, Categories, VarList)
% 	; fatal_error('### ERROR: lexicon setting must be either "c" or "db".~n', [])
% 	).

%%% generic record retrieval predicate
% lex_recs(form, Form, Rec, LowerFlag, FlushFlag, Lexicon, Index) :-
% 	LexiconType = 0,
% 	c_lex_form(Index, Form, LexiconType, LowerFlag, FlushFlag, OfsList, 1),
% 	sort(OfsList, SortedOfsList),
% 	get_records_from_offsets(SortedOfsList, Rec, Lexicon).

%%% generic category retrieval predicate
% lex_cats(form, Form, Cats, LowerFlag, Index) :-
% 	LexiconType = 0,
% 	% LexiconType = 0,
% 	% This is now in an if-then-else because we no longer test for
% 	% is_a_form/1 in metamap_variants.pl
% 	( c_lex_form_cats(Index, Form, LexiconType, LowerFlag, Cats, 1) ->
% 	  true
% 	; Cats = []
% 	).
	% format(user_output, '~q~n', [c_lex_form_cats(Index, Form, LexiconType, LowerFlag, Cats, 1)]).

%%% generic variant retrieval predicate
% Replaced with new lexAccess code
% lex_vars(cit, Cit, Vars, LowerFlag, Lexicon, Index) :-
% 	lex_vars_cit(Cit, Vars, LowerFlag, Lexicon, Index).
% lex_vars(form, Form, Vars, LowerFlag, Lexicon, Index) :-
% 	lex_vars_form(Form, Vars, LowerFlag, Lexicon, Index).

% Replaced with new lexAccess code
% lex_vars_cit(Cit, Vars, LowerFlag, Lexicon, Index) :-
% 	LexiconType = 0,
% 	c_lex_cit(Index, Cit, LexiconType, LowerFlag, 0, OfsList, 1),
% 	sort(OfsList, SortedOfsList),
% 	lex_vars_aux(SortedOfsList, Vars, Lexicon).
% 
% lex_vars_form(Form, Vars, LowerFlag, Lexicon, Index) :-
% 	LexiconType = 0,
% 	( c_lex_form(Index, Form, LexiconType, LowerFlag, 0, OfsList, 1) ->
% 	  true
% 	; OfsList = []
% 	),
% 	sort(OfsList, SortedOfsList),
% 	lex_vars_aux(SortedOfsList, Vars, Lexicon).

%%% auxiliary
% lex_vars_aux(OfsList, Vars, Lexicon) :-
% 	lex_vars_aux_1(OfsList, Lexicon, Vars0),
% 	flatten(Vars0, FlattenedVars),
% 	reformat_list(FlattenedVars, ReformattedFlattenedVars),
% 	sort(ReformattedFlattenedVars, Vars).
% 
% lex_vars_aux_1([], _Lexicon, []).
% lex_vars_aux_1([Ofs|RestOfsList], Lexicon, [Vars|RestVars]) :-
% 	c_get_varlist(Lexicon, Ofs, Vars, 1),
% 	% format(user_output, '~q~n', [c_get_varlist(lexicon, Ofs, Vars, 1)]),
% 	lex_vars_aux_1(RestOfsList, Lexicon, RestVars).

%%% generic variant retrieval predicate
%%% LRA
% lex_var_lists(form, Form, VarLists, LowerFlag, Lexicon, Index) :-
% 	LexiconType = 0,
% 	c_lex_form(Index, Form, LexiconType, LowerFlag, 0, OfsList, 1),
% 	sort(OfsList, SortedOfsList),
% 	lex_var_lists_aux(SortedOfsList, Lexicon, VarLists).
% 
% %%% auxiliary
% lex_var_lists_aux([], _, []).
% lex_var_lists_aux([Ofs|Rest], Lexicon, [V|RestV]) :-
% 	c_get_varlist(Lexicon, Ofs, V0, 1),
% 	% format(user_output, '~q~n', [c_get_varlist(lexicon, Ofs, V0, 1)]),
% 	reformat_list(V0, V1),
% 	sort(V1, V),
% 	lex_var_lists_aux(Rest, Lexicon, RestV).

/* Simple queries */

%%% lex_is_a_root_ci(+Root)
%%% Queries for a root form.
% lex_is_a_root_ci(Root) :-
% 	default_index_file(Index),
% 	lex_is_a_root_ci_2(Root, Index).

% lex_is_a_root_ci_2(Root, Index) :-
% 	LexiconType = 0,
% 	c_lex_is_a_root(Index, Root, LexiconType, 1, 1).
% 	% format(user_output, '~q~n', [c_lex_is_a_root(Root, Return)]).

%%% lex_is_a_form_ci(+Form)
%%% Queries for a lexical item.
% lex_is_a_form_ci(Form) :-
% 	default_index_file(Index),
% 	lex_is_a_form_ci_2(Form, Index).

% lex_is_a_form_ci_2(Form, Index) :-
% 	LexiconType = 0,
% 	c_lex_is_a_form(Index, Form, LexiconType, 1, 1).
% 	% format(user_output, '~q~n', [c_lex_is_a_form(Form, Return)]).

%%% lex_is_a_root_ci_cats(+Root, +Cats)
%%% succeeds if +Root is a root form in any category in +Cats
% lex_is_a_root_ci_cats(Root, Cats) :-
% 	default_index_file(Index),
% 	lex_is_a_root_ci_cats_3(Root, Cats, Index).
% 
% lex_is_a_root_ci_cats_3(Root, Cats, Index) :-
% 	LexiconType = 0,
% 	c_lex_is_a_root_cats(Index, Root, LexiconType, 1, Cats, 1).
 
% If the call to db_get_lex_prefix_EUIs/2 in longest_prefix/6 succeeds,
% it will return a list of EUIs.
% Each element of EUIList is either the fake EUI '0' or an actual EUI, e.g., 'E0000001';
% moreover, if EUIList contains '0', it will be the first element (because results are sorted),
% and signifies that PrefixAtom is a (proper) prefix of a normalized lexical item.
% The data in EUIList can take one of three forms:
% (1) ['0']:          PrefixAtom is not a lexical item, but a prefix of a lexical item.
% (2) ['0'|RealEUIs]: PrefixAtom is both a lexical item and a prefix of a lexical item.
% (3) [RealEUIs]:     PrefixAtom is a lexical item, but not a prefix of a lexical item.

% How to handle each case:
% (1) Commit to recursing on a longer prefix; if no longer prefix matches
% the input, the entire clause will fail, and the logic will backtrack
% to the next shortest prefix.
% (2) Try recursing on a longer prefix, but don't commit to the choice
% in case no longer prefix matches the input; if a longer prefix is found,
% however, then commit, and retrieve the lexmatches correesponding to the (real) EUIs.
% (3) Don't look any further (i.e., don't recurse on a longer prefix),
% but just retrieve the lexmatches corresponding to the EUIs.

next_longest_prefix(CurrPrefixLength, InputTokenList,
                    LexicalEntryList, InputMatch, LexMatch, InputTokensRemaining) :-
	% length(ModTokenList, ModTokenListLength),
	length(InputTokenList, InputTokenListLength),
        NextPrefixLength is CurrPrefixLength + 1,
        NextPrefixLength =< InputTokenListLength,
        longest_prefix(InputTokenList, NextPrefixLength, LexicalEntryList,
                       InputMatch, LexMatch, InputTokensRemaining).

longest_prefix(InputTokenList, CurrPrefixLength, LexicalEntryList,
	       InputMatch, LexMatch, RemainingInputTokens) :-
	normalize_all_tokens(InputTokenList, NormInputTokenList),
	remove_punct_tokens(NormInputTokenList, ModInputTokenList),
        % Identify the first CurrPrefixLength elements of ModInputTokenList
	prefix_length(ModInputTokenList, CurrPrefixList, CurrPrefixLength),
	last(CurrPrefixList, LastElement),
	% Fail if we have something like the first two tokens in
	% [has, '', been] (because of retokenize/2)!	
	LastElement \== '',
	concat_atom(CurrPrefixList, ' ', PrefixAtom),
	db_get_lex_prefix_EUIs(PrefixAtom, EUIList),
	% What is this for?!
	% EUIList \== ['0'],
	( get_lexical_entry_list(EUIList, CurrPrefixLength,
				 InputTokenList, NormInputTokenList,
				 LexicalEntryList, InputMatch, LexMatch, RemainingInputTokens) ->
	  true
	; EUIList = [FirstEUI|_],
	  FirstEUI \== '0' ->
	  send_message('### WARNING: longest_prefix failed on ~w~n!!', [InputTokenList]),
	  fail
	).

get_lexical_entry_list(EUIList, CurrPrefixLength,
		       InputTokenList, NormInputTokenList,
		       LexicalEntryList, InputMatch, LexMatch, RemainingInputTokens) :-
	  % If the EUIList is ['0'], the input prefix is a prefix of a lexical item,
	  % BUT NOT a itself a lexical item, so look for a longer prefix.
	( EUIList == ['0'] ->
	  next_longest_prefix(CurrPrefixLength, InputTokenList,
			      LexicalEntryList, InputMatch, LexMatch, RemainingInputTokens)
	  % If the EUIList is ['0'|_], the input prefix is a prefix of a lexical item,
	  % AND a lexical item itself, so look for a longer prefix.
	; EUIList = ['0', _|_],
	  next_longest_prefix(CurrPrefixLength, InputTokenList,
			      LexicalEntryList, InputMatch, LexMatch, RemainingInputTokens),
	  !
	  % There could still be a '0' at the head of the list, e.g., for
	  % "the University of California Los Angeles":
	  % "the" generates an EUIList of ['0','E0060487','E0717225'],
	  % but "the university" fails altogether, so we fail back to the EUIlist of "the".
	  % So remove a '0' if if it's there.
	; remove_zero_EUI(EUIList, RealEUIList),
	  db_get_lex_record_list(RealEUIList, LexicalEntryList),
	  remove_null_atoms(NormInputTokenList, InputTokenListNoNulls),
	  get_all_lexmatches(LexicalEntryList, InputTokenListNoNulls, [], LexMatches),
	  sort(LexMatches, SortedLexMatches),
	  % choose_best_lexmatch(SortedLexMatches,
	  %		       NegMatchingCount-_MatchScore-LexMatchTokens-_RestInputTokens),
	  member(NegMatchingCount-_MatchScore-LexMatchTokens-_RestInputTokens, SortedLexMatches),
	  length(InputTokenList, InputTokenListLength),
	  length(LexMatchTokens, LexMatchTokensLength),
	  LexMatchTokensLength =< InputTokenListLength ->

	  MatchingCount is -NegMatchingCount,
	  % Still necessary?
	  % remove_final_blank(LexMatchTokens0, LexMatchTokens),
	  skip_n_tokens(MatchingCount, InputTokenList, InputMatch0, RemainingInputTokens),
	  % append(InputMatch0, RemainingInputTokens, InputTokenListNoNulls),
	  remove_null_atoms(InputMatch0, InputMatch),
	  % Still necessary?
	  % move_apostrophe_s_to_inputmatch(InputMatch0, RemainingInputTokens0,
	  %				  InputMatch, RemainingInputTokens),
	  % MatchingCount is -1 * NegMatchingCount,
	  % prefix_length(InputTokenList, InputMatch, MatchingCount),
	  % prefix_length(ModCurrPrefixList, InputMatch, MatchingCount),
	  % InputMatch           = ['P','.','(','R']
	  % InputTokenList       = ['P','.','(','R',')','System']
	  % We want InputTokensRemaining to be ['System'],
	  % so peel off all the InputMatch tokens followed by any punct tokens
	  % in whatever is left over in InputTokenList and add them to InputMatch
	  % append_plus_punct(InputMatch0, InputTokenList, InputMatch, InputTokensRemaining),
	  % remove_inputmatch_tokens(InputMatch, InputTokenList, RemainingInputTokens),
	  % append(InputMatch, InputTokensRemaining, InputTokenList),
	  concat_atoms_intelligently(LexMatchTokens, LexMatch0),
	  lower(LexMatch0, LexMatch)
	).

% remove_final_blank(LexMatchTokens0, LexMatchTokens) :-
% 	( append(LexMatchTokens, [' '], LexMatchTokens0) ->
% 	  true
% 	; LexMatchTokens = LexMatchTokens0
% 	).

%%% remove_inputmatch_tokens([], InputTokenList, InputTokensRemaining) :-
%%% 	InputTokensRemaining = InputTokenList.
%%% remove_inputmatch_tokens([FirstInputMatch|RestInputMatches],
%%% 			 [FirstInputToken|RestInputTokens], InputTokensRemaining) :-
%%% 	( FirstInputMatch == FirstInputToken ->
%%% 	  remove_inputmatch_tokens(RestInputMatches, RestInputTokens, InputTokensRemaining)
%%% 	; remove_inputmatch_tokens([FirstInputMatch|RestInputMatches], RestInputTokens, InputTokensRemaining)
%%% 	).


% If the call to append/3 immediately before calling move_apostrophe_s_to_inputmatch/4 leaves
% * an apostrophe at the end of InputMatch and
% * an "s" at the beginning of RemainingInputTokens, e.g.,
% InputTokenList        = ['Kayser',-,'Fleischer','\'',s,ring]
% InputMatch0           = ['Kayser',-,'Fleischer','\'']
% RemainingInputTokens0 = [s,ring]
% BECOMES ===>
% InputMatch            = ['Kayser',-,'Fleischer','\'',s]
% RemainingInputTokens  = [ring]

% OR 
% * an apostrophe + "s" at the beginning of RemainingInputTokens, e.g.,
% InputTokenList         = [cancer,'\'',s,cure]
% InputMatch0            = [cancer]
% RemainingInputTokens0  = ['\'',s,cure]
% BECOMES ===>
% InputMatch             = [cancer,'\'',s]
% RemainingInputTokens   = ['\'',s,cure]

%%% % move an apostrophe + "s" from the beginning of RemainingInputTokens to the end of InputMatch
%%% move_apostrophe_s_to_inputmatch(InputMatch0, RemainingInputTokens0,
%%% 				InputMatch, RemainingInputTokens) :-
%%% 	( RemainingInputTokens0 = ['''', 's'|RestRemainingInputTokens0] ->
%%% 	  append(InputMatch0, ['''', 's'], InputMatch),
%%% 	  RemainingInputTokens = RestRemainingInputTokens0
%%% 	; RemainingInputTokens0 = ['s'|RestRemainingInputTokens0],
%%% 	  last(InputMatch0, '''') ->
%%% 	  append(InputMatch0, ['s'], InputMatch),
%%% 	  RemainingInputTokens = RestRemainingInputTokens0
%%% 	; InputMatch = InputMatch0,
%%% 	  RemainingInputTokens = RemainingInputTokens0
%%% 	).

remove_zero_EUI([H|T], RealEUIList) :-
	( H == '0' ->
	  RealEUIList = T
	; RealEUIList = [H|T]
	).

%%% append_plus_punct([], RestInputTokenList, InputMatchOut, InputTokensRemaining) :-
%%% 	shift_punct_tokens(RestInputTokenList, InputMatchOut, InputTokensRemaining),
%%% 	( InputMatchOut == [] ->
%%% 	  true
%%% 	; last(InputMatchOut, LastToken),
%%% 	  atom_codes(LastToken, LastTokenChars),
%%% 	  \+ ex_lbracket(LastTokenChars)
%%% 	).
%%% append_plus_punct([H|RestInputMatchIn], [H|RestInputTokenList],
%%% 		  [H|RestInputMatchOut], InputTokensRemaining) :-
%%% 	append_plus_punct(RestInputMatchIn, RestInputTokenList,
%%% 			  RestInputMatchOut, InputTokensRemaining).
%%% 
%%% shift_punct_tokens([], [], []).
%%% shift_punct_tokens([FirstInputToken|RestInputTokens], InputMatchOut, InputTokensRemaining) :-
%%% 	( punct_token(FirstInputToken, _),
%%% 	  \+ no_glom_punct_token(FirstInputToken),
%%% 	  InputMatchOut = [FirstInputToken|RestInputMatchOut],
%%% 	  InputTokensRemaining = RestInputTokensRemaining,
%%% 	  shift_punct_tokens(RestInputTokens, RestInputMatchOut, RestInputTokensRemaining)
%%% 	; InputMatchOut = [],
%%% 	  InputTokensRemaining = [FirstInputToken|RestInputTokens]
%%% 	).
%%% 
%%% no_glom_punct_token('.').
%%% no_glom_punct_token(',').
%%% no_glom_punct_token(':').
%%% no_glom_punct_token(',').
%%% no_glom_punct_token('(').
%%% no_glom_punct_token('[').
%%% no_glom_punct_token('$').

concat_atoms_intelligently(AtomList, Result) :-
	AtomList = [H|T],
	insert_white_space_between_alnums(T, H, TokensWithWhiteSpace),
	concat_atom(TokensWithWhiteSpace, Result).	

% insert_white_space_between_alnums:
% Create a single atom from the input atoms,
% inserting whitespace between only alphanumeric tokens
insert_white_space_between_alnums([], LastToken, [LastToken]).

% need to special-case apostrophes!
% X, '''', Y --> X, '''', space, Y if X ends in "s" and Y \== 's'
	
insert_white_space_between_alnums(['''',Y|RestTokens], X, [X, '''', ' ' |NewTokens]) :-
	atom_codes(X, Codes),
	last(Codes, LastCode),
	LastCode == 0's,
	Y \== 's',
	!,
	insert_white_space_between_alnums(RestTokens, Y, NewTokens).
insert_white_space_between_alnums([NextToken|RestTokens], FirstToken, [FirstToken|NewTokens]) :-
	( atom_codes(FirstToken, FirstTokenCodes),
	  FirstTokenCodes = [FirstTokenCode],
	  local_punct(FirstTokenCode) ->
	  NewTokens = RestNewTokens
	; atom_codes(NextToken, NextTokenCodes),
	  NextTokenCodes = [NextTokenCode],
	  local_punct(NextTokenCode) ->
	  NewTokens = RestNewTokens
	; NewTokens = [' '|RestNewTokens]
	),
	insert_white_space_between_alnums(RestTokens, NextToken, RestNewTokens).


% choose_best_lexmatch([FirstLexMatch|_RestLexMatches], FirstLexMatch).

get_all_lexmatches([], _NormalizedInputTokenList, LexMatchesList, LexMatches) :-
	% Do this with difference lists!
	append(LexMatchesList, LexMatches0),
	sort(LexMatches0, LexMatches).
get_all_lexmatches([LexicalEntry|RestLexicalEntries],
		   NormalizedInputTokenList, LexMatchesIn, LexMatchesOut) :-
	get_lexmatches_for_one_entry(LexicalEntry, NormalizedInputTokenList, LexMatchesIn, LexMatchesNext),
	get_all_lexmatches(RestLexicalEntries, NormalizedInputTokenList, LexMatchesNext, LexMatchesOut).

get_variant_strings(RestEntry, VariantStrings) :-
	RestEntry = [entries:[entry:List]|_],
	memberchk(misc:Misc, List),
	get_variant_strings_1(Misc, VariantStrings).

get_variant_strings_1([], []).
get_variant_strings_1([H|T], VariantStrings) :-
	( H = variant:[variant-string:VS|_] ->
	  VariantStrings = [VS|RestVariantStrings]
	; VariantStrings = RestVariantStrings
	),
	get_variant_strings_1(T, RestVariantStrings).
	
get_irregs(RestEntry, Irregs) :-
	( RestEntry = [entries:[entry:List]|_],
	  memberchk(variants:Variants, List),
	  memberchk(irreg:Irregs, Variants) ->
	  true
	; Irregs = []
	).

% The irrreg of "LV" is [lv,'','','','']
% because in the lexicon flat file, the feature is "variants=irreg|lv|||||".
% Go figure...

remove_null_atoms([], []).
remove_null_atoms([H|T], Choices) :-
	( H == '' ->
	  Choices = RestChoices
	; Choices = [H|RestChoices]
	),
	remove_null_atoms(T, RestChoices).

get_all_im_variants_only(LexVariantsIn, LexVariantsOut) :-
	(  foreach(LVsIn,  LexVariantsIn),
	   foreach(LVsOut, LexVariantsOut0)
	do get_im_variants_only(LVsIn, LVsOut)
	),
	append(LexVariantsOut0, LexVariantsOut).	
	   

get_im_variants_only(CitationForm, InflectionalVariants) :-
	lower(CitationForm, CitationFormLC),
	db_get_lex_im_varlist(CitationFormLC, InflectionalVariants0),
	(  foreach(VariantData, InflectionalVariants0),
	   foreach(Variant, InflectionalVariants)
	do VariantData = [Variant|_]
	).

normalize_all_tokens(AllTokens, AllNormalizedTokens) :-
	(  foreach(Token, AllTokens),
	   foreach(NormalizedTokenAtom, AllNormalizedTokens)
	do normalize_token(Token, NormalizedTokenAtom)
	   % tokenize_text(NormalizedTokenString, TokenizedTokenString),
	   % atom_codes_list(TokenizedTokenAtoms, TokenizedTokenString)
	).

normalize_token(TokenAtom, NormalizedTokenAtom) :-
        atom_codes(TokenAtom, TokenString),
        ( TokenString = [Char],
          local_punct(Char) ->
          NormalizedTokenAtom = TokenAtom
        ; normalize_lex_string(TokenString, NormalizedTokenString),
          atom_codes(NormalizedTokenAtom, NormalizedTokenString)
        ).

% LexicalEntry is of the form
% lexrec:[base:['LDLC'],spelling_variants:['LDL-C']|_]
get_lexmatches_for_one_entry(LexicalEntry, NormalizedInputTokenList, LexMatchesIn, LexMatchesNext) :-
	LexicalEntry = lexrec:[base:[CitationForm],spelling_variants:SpellingVariants|RestEntry],
	% Inflect the citation form
	get_im_variants_only(CitationForm, InflectionalVariants),
	% Extract contractions ("aren't", "didn't", etc.), auxes and modals:
	% "can", "dare", "do", "have", "may", "must", "ought", "shall", "will", and "need".
	get_variant_strings(RestEntry, VariantStrings),
	% Extract irregular morphology
	get_irregs(RestEntry, Irregs),
	append([SpellingVariants,InflectionalVariants,VariantStrings,Irregs], AllLexVariants0),
	sort([CitationForm|AllLexVariants0], AllLexVariants1),
	remove_null_atoms(AllLexVariants1, AllLexVariants2),
	get_all_im_variants_only(AllLexVariants2, AllLexVariants3),
	sort(AllLexVariants3, AllLexVariants4),
	normalize_all_tokens(AllLexVariants4, AllNormalizedLexVariants),
	append(AllLexVariants4, AllNormalizedLexVariants, AllLexVariants5),
	sort(AllLexVariants5, AllLexVariants),
	tokenize_all(AllLexVariants, AllTokenizedLexVariants),
	% AllNormalizedLexVariants = AllLexVariants,
	count_all_matching_input_tokens(AllTokenizedLexVariants,
					NormalizedInputTokenList,
					AllTokenizedForms),
	LexMatchesNext = [AllTokenizedForms|LexMatchesIn].


count_all_matching_input_tokens(AllTokenizedLexVariants,
				NormalizedInputTokenList,
				AllTokenizedForms) :-
	(  foreach(TokenizedLexFormAtoms, AllTokenizedLexVariants),
	   foreach(MatchingTokenCount-MatchScore-MatchingTokens-RemainingInputTokens,
		   AllTokenizedForms),
	   param(NormalizedInputTokenList)
	do count_matching_input_tokens(TokenizedLexFormAtoms, NormalizedInputTokenList,
				       0, MatchingTokenCount,
				       0, MatchScore,
				       MatchingTokens, RemainingInputTokens)
	).


tokenize_all(LexVariants, TokenizedLexVariants) :-
	(  foreach(LV,  LexVariants),
	   foreach(TLV, TokenizedLexVariants)
	do atom_codes(LV, LVCodes),
	   tokenize_string(LVCodes, TLV0),
	   append(TLV0, TLV)
	).

% ### also need to generate all variants of each lex item and original input!

% norm_prefix file is created by
% (1) lowercase
% (2) change apostrophe-s-blank to blank
% (3) delete "(ies), (es)" and "(s)"
% (4) change each punctuation char to a blank
% (5) collapse multiple blanks to just onef
% (6) remove any leading and trailing blanks 

normalize_lex_string(LexFormString, NormalizedLexFormString) :-
	lower(LexFormString, LexFormString1),
	remove_unwanted_substrings(LexFormString1, LexFormString2),
%	( append(Prefix, "'s", LexFormString2) ->
%	  LexFormString3 = Prefix
%	; LexFormString3 = LexFormString2
%	),
	LexFormString3 = LexFormString2,
	change_puncts_to_blanks(LexFormString3, LexFormString4),
	trim_and_compress_whitespace(LexFormString4, LexFormString5),
	NormalizedLexFormString = LexFormString5.

remove_unwanted_substrings(LexFormStringIn, LexFormStringOut) :-
	( unwanted_string(Unwanted),
	  append([Before,Unwanted,After], LexFormStringIn) ->
	  append([Before,After], LexFormString1),
	  remove_unwanted_substrings(LexFormString1, LexFormStringOut)
	; LexFormStringOut = LexFormStringIn
	).

unwanted_string("(ies)").
unwanted_string("(es)").
unwanted_string("(s)").
unwanted_string("'s").

change_puncts_to_blanks([], []).
change_puncts_to_blanks([H1|T1], [H2|T2]) :-
	( local_punct(H1) ->
	  H2 = 0'    % there is a blank space after the "'"!
	; H2 = H1
	),
	change_puncts_to_blanks(T1, T2).
	

count_matching_input_tokens([], InputTokenList,
			  MatchingTokenCount, MatchingTokenCount,
			  MatchScore, MatchScore,
			  [], InputTokenList).
count_matching_input_tokens([FirstLexToken|_RestLexTokens], [],
			  MatchingTokenCountIn, MatchingTokenCountOut,
			  MatchScoreIn, MatchScoreOut,
			  MatchingTokenList, LeftOverInputTokens) :-
	!,
	punct_token(FirstLexToken, _),
	MatchingTokenCountOut is MatchingTokenCountIn + 1,
	MatchScoreOut is MatchScoreIn + 1,
	MatchingTokenList = [FirstLexToken],
	LeftOverInputTokens = [].	
count_matching_input_tokens([FirstLexToken|RestLexTokens],
			  [FirstInputToken|RestInputTokens],
			  MatchingTokenCountIn, MatchingTokenCountOut,
			  MatchScoreIn, MatchScoreOut,
			  MatchingTokenList, LeftOverInputTokens) :-

	( token_match([FirstLexToken|RestLexTokens], [FirstInputToken|RestInputTokens],
		      RemainingLexTokens, RemainingInputTokens,
		      AdditionalInputTokenCount, ThisMatchScore, MatchingLexTokens),
	  % update_matching_token_list(MatchingTokens, MatchingTokenList, RestMatchingTokenList),
	  append(MatchingLexTokens, RestMatchingTokenList, MatchingTokenList),
	  update_match_type(MatchScoreIn, ThisMatchScore, MatchScoreNext),
	  MatchingTokenCountNext is MatchingTokenCountIn - AdditionalInputTokenCount,
	  count_matching_input_tokens(RemainingLexTokens, RemainingInputTokens,
				    MatchingTokenCountNext, MatchingTokenCountOut,
				    MatchScoreNext, MatchScoreOut,
				    RestMatchingTokenList, LeftOverInputTokens) ->
	  true
	; MatchingTokenCountOut is MatchingTokenCountIn,
	  MatchingTokenList = [],
	  MatchScoreOut is MatchScoreIn,
	  LeftOverInputTokens = [FirstInputToken|RestInputTokens]
	).

% Update the match to the sum of
% * the match thus far (MatchScoreIn) and
% * the current match(MatchScore)
% In order to get an overall score taking into account all tokens!
update_match_type(MatchScoreIn, CurrentMatchScore, MatchScoreNext) :-
	% MatchScoreNext is max(MatchScoreIn, CurrentMatchScore).
	MatchScoreNext is MatchScoreIn + CurrentMatchScore.

% update_matching_token_list(MatchingToken, MatchingTokenList, RestMatchingTokenList) :-
% 	( MatchingToken == '' ->
% 	  MatchingTokenList = RestMatchingTokenList
% 	; MatchingTokenList = [MatchingToken|RestMatchingTokenList]
% 	).

% We must recognize the lexical item "heart attack" from the input "heart-attack",
% when the input contains a hyphen, but the lexical item does not.
% Conversely,
% we must recognize the lexical item "non-iron" from the input "non iron",
% when the lexical item contains a hyphen, but the input does not.

% There are many possible types of LexToken/InputToken matches.
% A negative score is a better match; this is done for sorting,
% so the best matches (with lower scores) will appear first in the sorted output.
% The possibilities for matching a LexToken and an InputToken are:
% Case (1):  exact match: score = -2
% Case (2):   case match: score = -1
% Case (3):  both punctuation, but different punctuation: score = 0
% Case (4):  LexToken list begins with apostrophe-s: score 0
%            If the input token list is      [alzheimer, disease],
%               the   lex token list will be ['Alzheimer','\'',s,disease].
%            If the input token list is      ['Pick', cells],
%               the   lex token list will be ['Pick','\'',s,cell].
%            If the input token list is      ['Fuchs',       dystrophy],
%               the   lex token list will be ['Fuchs', '\'', dystrophy].
% Case (5):  InputToken list begins with apostrophe-s: score 0
%            In this example, the 4th and 5th input tokens are relevant
%            If the input token list is      ['Kayser', -, 'Fleischer', '\'', s, ring],
%               the   lex token list will be ['Kayser' ,-, 'Fleischer',          ring].
% Case (6):  LexToken is punct and InputToken is not: score = 1
%            If the input token list is      [non,      iron],
%               the   lex token list will be [non,   -, iron].
% Case (9):  InputToken is punct, LexToken is not.
%             If the input token list is      [serum, beta, '(',' 2',' )', -, microglobulin],
%                the   lex token list will be [serum, beta,       '2',        microglobulin],

% Cases (7) and (8) are no longer necessary,
% because I no longer use tokenize_text_utterly, which added blank tokens:
% Case (7):  LexToken is ' ', InputToken is punct: score = 0
%            In this example, the 2nd tokens are relevant.
%            If the input token list is      [heart,   -, attack],
%               the   lex token list will be [heart, ' ', attack].
% Case (8):  LexToken is ' ', InputToken is alnum
%            Normal case, because the LexTokenList is created by tokenize_text_utterly,
%	     which creates whitespace tokens: score = -2
%            If the input token list is      [heart,      attack],
%               the   lex token list will be [heart, ' ', attack].
% Cases (10), (11) and (12) are no longer necessary,
% because I now generate the inflectional variants of the citation form!
% Case (10): InputToken and LexToken are variants.
%            If the input token list is      [hearts],
%               the   lex token list will be [heart]
% Case (11): This is tricky, and handles cases in which one token is not itself a lexical item,
%            and therefore can't be solved by case (10).
%             If the input token list is      ['Puerto',      'Ricans'],
%                the   lex token list will be ['Puerto', ' ', 'Rican'].
% Case (12): This is even trickier, and handles more complex cases
%            in which one token is not itself a lexical item,
%            and therefore can't be solved by case (10), but the morphology is irregular.
%             If the input token list is      [rami,       communicantes]
%                the   lex token list will be [ramus, ' ', communicans]

token_match([FirstLexToken|RestLexTokens], [FirstInputToken|RestInputTokens],
	    RemainingLexTokens, RemainingInputTokens,
	    AdditionalInputTokenCount, MatchScore, MatchingLexTokens) :-
 	( FirstLexToken == FirstInputToken ->
%	  atom_codes(FirstLexToken, [FirstLexTokenCode|_]),
%	  \+ local_punct(FirstLexTokenCode) ->
	  RemainingLexTokens = RestLexTokens,
	  RemainingInputTokens = RestInputTokens,
	  AdditionalInputTokenCount is 1,
	  MatchingLexTokens = [FirstLexToken],
	  MatchScore is -2
	; lower(FirstInputToken, FirstInputTokenLC),
	  lower(FirstLexToken, FirstLexTokenLC),
 	  FirstLexTokenLC == FirstInputTokenLC ->
 	  RemainingLexTokens = RestLexTokens,
 	  RemainingInputTokens = RestInputTokens,
 	  AdditionalInputTokenCount is 1,
 	  MatchingLexTokens = [FirstLexToken],
 	  MatchScore is -1
	; punct_token(FirstLexToken, _),
 	  punct_token(FirstInputToken, _) ->
 	  RemainingLexTokens = RestLexTokens,
 	  RemainingInputTokens = RestInputTokens,
	  MatchingLexTokens = [FirstLexToken],
 	  AdditionalInputTokenCount is 0,
 	  MatchScore is 0
	; punct_token(FirstLexToken, _) ->
 	  RemainingLexTokens = RestLexTokens,
	  RemainingInputTokens = [FirstInputToken|RestInputTokens],
	  MatchingLexTokens = [FirstLexToken],
 	  AdditionalInputTokenCount is 0,
 	  MatchScore is 1
	; punct_token(FirstInputToken, _) ->
	  RemainingLexTokens = [FirstLexToken|RestLexTokens],
	  RemainingInputTokens = RestInputTokens,
	  MatchingLexTokens = [],
 	  AdditionalInputTokenCount is 1,
 	  MatchScore is 1
%%% 	; % allow matching of "isn t" and "isn't"
%%% 	  match_modulo_ws_and_apostrophe(FirstLexToken, FirstInputToken) ->
%%% 	  RemainingLexTokens = RestLexTokens,
%%% 	  RemainingInputTokens = RestInputTokens,
%%% 	  AdditionalInputTokenCount is 1,
%%% 	  MatchingLexTokens = [FirstLexToken],
%%% 	  MatchScore is -2
	).
	  
%%% match_modulo_ws_and_apostrophe(FirstLexToken, FirstInputToken) :-
%%% 	  atom_codes(FirstLexToken, FirstLexTokenCodes),
%%% 	  atom_codes(FirstInputToken, FirstInputTokenCodes),
%%% 	  remove_ws_and_apostrophe(FirstLexTokenCodes, ModFirstLexTokenCodes),
%%% 	  remove_ws_and_apostrophe(FirstInputTokenCodes, ModFirstInputTokenCodes),
%%% 	  atom_codes(ModFirstLexToken, ModFirstLexTokenCodes),
%%% 	  atom_codes(ModFirstInputToken, ModFirstInputTokenCodes),
%%% 	  ModFirstLexToken == ModFirstInputToken.
%%% 
%%% remove_ws_and_apostrophe([], []).
%%% remove_ws_and_apostrophe([H|T], ModifiedToken) :-
%%% 	( ws_or_apostophe(H) ->
%%% 	  ModifiedToken = RestModifiedToken
%%% 	; ModifiedToken = [H|RestModifiedToken]
%%% 	),
%%% 	remove_ws_and_apostrophe(T, RestModifiedToken).
%%% 	
%%% ws_or_apostophe(32).
%%% ws_or_apostophe(39).

%	  % ## Case (1): Exact match; score is -2
% 	( FirstLexToken == FirstInputToken ->
% 	  RemainingLexTokens = RestLexTokens,
% 	  RemainingInputTokens = RestInputTokens,
% 	  AdditionalInputTokenCount is 1,
% 	  MatchingTokens = [FirstLexToken],
% 	  MatchScore is -2
% 	  % ## Case(2): Case match; score is -1
% 	; FirstLexTokenLC == FirstInputTokenLC ->
% 	  RemainingLexTokens = RestLexTokens,
% 	  RemainingInputTokens = RestInputTokens,
% 	  AdditionalInputTokenCount is 1,
% 	  MatchingTokens = [FirstLexToken],
% 	  MatchScore is -1
% 	  % ## Case (3): Both lex token and input token are punctuation,
% 	  % but different punctuation; score is 0
% 	; punct_token(FirstLexToken, _),
% 	  punct_token(FirstInputToken, _) ->
% 	  RemainingLexTokens = RestLexTokens,
% 	  ( FirstInputToken == '''',
% 	    RestInputTokens = [s|RemainingInputTokens] ->
% 	    % MatchingTokens = ['''', s, ' ']
% 	    MatchingTokens = [FirstLexToken]
% 	  ; RemainingInputTokens = RestInputTokens,
% 	    % MatchingTokens = ['''']
% 	    MatchingTokens = [FirstLexToken]
% 	  ),
% 	  AdditionalInputTokenCount is 0,
% 	  MatchScore is 0
% 	  % ## Case (4): LexTokens begin with apostrophe-s-space; score is 0
% 	; FirstLexToken == '''',
% 	  ( RestLexTokens = [s, ' '|RemainingLexTokens] ->
% 	    MatchingTokens = [' '],
% 	    MatchScore is 0
% 	  ; RestLexTokens = [' '|RemainingLexTokens] ->
% 	    MatchingTokens = [' '],
% 	    MatchScore is 0
% 	    % This prevents "one s allele" from matching "one's"...ugh...
% 	  ; % RestLexTokens \== [s],
% 	    MatchingTokens = [],
% 	    RemainingLexTokens = RestLexTokens,
% 	    MatchScore is 1
% 	  ),
% 	  % Do not enforce a match between the first token in
% 	  % RemainingLexTokens and FirstInputToken, because they could be variants.
%   	  AdditionalInputTokenCount is 1,
% 	  RemainingInputTokens = [FirstInputToken|RestInputTokens]
% 	  % ## Case (5): InputTokenList begins with apostrophe-s
% 	; FirstInputToken == '''',
% 	  RestInputTokens = ['s', NextInputToken|RestRestInputTokens],
% 	  NextInputToken = FirstLexToken ->
% 	  MatchingTokens = [NextInputToken],
% 	  MatchScore is 0,
% 	  RemainingLexTokens = RestLexTokens,
% 	  RemainingInputTokens = RestRestInputTokens,
% 	  AdditionalInputTokenCount is 3
% 	  % ## Case (6) LexToken is punct and InputToken is alnum
% 	; punct_token(FirstLexToken, _),
% 	  FirstLexToken  \== '''' ->
% 	  RemainingLexTokens = RestLexTokens,
% 	  RemainingInputTokens = [FirstInputToken|RestInputTokens],
% 	  AdditionalInputTokenCount is 0,
% 	  MatchingTokens = [FirstLexToken],
% 	  MatchScore is 0
% 	  % ## Case (7): whitespace vs. punct; score is 0
% 	; FirstLexToken = ' ',
% 	  punct_token(FirstInputToken, _) ->
% 	  % handle apostrophe-s here!
% 	  RemainingLexTokens = RestLexTokens,
% 	  ( FirstInputToken = '''',
% 	    RestInputTokens = [s|RemainingInputTokens] ->
% 	    true
% 	  ; RemainingInputTokens = RestInputTokens
% 	  ),
% 	  AdditionalInputTokenCount is 1,
% 	  MatchingTokens = [FirstLexToken],
% 	  MatchScore is 0
% 	  % ## Case (8): whitespace vs non-punct real token; score is -2
% 	; FirstLexToken = ' ' ->
% 	  RemainingLexTokens = RestLexTokens,
% 	  RemainingInputTokens = [FirstInputToken|RestInputTokens],
% 	  AdditionalInputTokenCount is 1,
% 	  MatchingTokens = [FirstLexToken],
% 	  MatchScore is 0
	  % Case (9): InputToken is punct, LexToken is not
% 	; punct_token(FirstInputToken, _) ->
% 	  RemainingLexTokens = [FirstLexToken|RestLexTokens],
% 	  RemainingInputTokens = RestInputTokens,
% 	  AdditionalInputTokenCount is 1,
% 	  MatchingTokens = [],
% 	  MatchScore is 1
	  % Case (10): variants match
%	; db_get_lex_im_varlist(FirstLexTokenLC, VarList),
%	  memberchk([FirstInputTokenLC,_,_], VarList) ->
%	  RemainingLexTokens = RestLexTokens,
%% format(user_output, '~n### LexTokens = ~w~n### InpTokens = ~w~n',
%%        [[FirstLexToken|RestLexTokens], [FirstInputToken|RestInputTokens]]),
%	  RemainingInputTokens = RestInputTokens,
%	  AdditionalInputTokenCount is 1,
%	  MatchingTokens = [FirstInputToken],
%	  MatchScore is -2
	  % Case (11): This case handles examples like
	  % "puerto ricans": LexTokens = ['Puerto', ' ', 'Rican']
	  % "hereditary hypophosphatemic rickets":
	  % LexTokens = [hereditary, hypophosphatemic, ricket]
	  % "tuco-tucos": LexTokens = [tuco, -, tuco]
	  % "do-it-yourselfers": LexTokens = [do, -, it, -, yourselfer]
	  % in which one lex token (Rican, ricket, tuco, yourselfer) has no variants
	  % for case (10) to match
	  % FirstInputToken is FirstLCToken + "s"
%	; concat_atom([FirstLexTokenLC,'s'], FirstInputTokenLC) ->
%	  RemainingLexTokens = RestLexTokens,
%	  RemainingInputTokens = RestInputTokens,
%	  AdditionalInputTokenCount is 1,
%	  MatchingTokens = [FirstInputToken],
%	  MatchScore is -1	  
	  % Case (12): This case handles examples like "rami communicantes",
	  % whose LexTokenList = [rami, ' ', communicans].
	  % make sure no more than 4 suffix chars in the longer token
	  % are not shared by the shorter token. Ugh...
% 	; atom_codes(FirstLexTokenLC, FirstLexTokenCodes),
% 	  atom_codes(FirstInputTokenLC, FirstInputTokenCodes),
% 	  length(FirstLexTokenCodes, LexTokenLength),
% 	  % don't let this apply to contractions; we don't want "are" and "aren" to match
% 	  LexTokenLength > 4,
% 	  length(FirstInputTokenCodes, InputTokenLength),
% 	  shorter_longer(FirstLexTokenCodes, LexTokenLength,
% 			 FirstInputTokenCodes, InputTokenLength,
% 			 Shorter, Longer),
% 	  common_prefix(Shorter, Longer, _Shared, LeftOverCodes),
% 	  length(LeftOverCodes, LeftOverCodesLength),
% 	  LeftOverCodesLength < 4,
% 	  RemainingLexTokens = RestLexTokens,
% 	  RemainingInputTokens = RestInputTokens,
% 	  AdditionalInputTokenCount is 1,
% 	  MatchingTokens = [FirstInputToken],
% 	  MatchScore is -1	  
%	).

% shorter_longer(Codes1, Codes1Length, Codes2, Codes2Length, Shorter, Longer) :-
% 	( Codes1Length < Codes2Length ->
% 	  Shorter = Codes1,
% 	  Longer = Codes2
% 	; Shorter = Codes2,
% 	  Longer = Codes1
% 	).
 
% common_prefix([], LeftOver, [], LeftOver).
% common_prefix([H1|T1], [H2|T2], Shared, LeftOver) :-
% 	( H1 == H2 ->
% 	  Shared = [H1|RestShared],
% 	  common_prefix(T1, T2, RestShared, LeftOver)
% 	; Shared = [],
% 	  LeftOver = [H2|T2]
% 	).
	

% If the first argument (InputTokenList) begins with an apostrophe, remove it;
% moreover, if the next token is an "s", remove it too!
remove_punct_tokens([], []).
remove_punct_tokens([H|RestIn], NonPunctTokens) :-
%	( H == '''' ->
%	  remove_next_s(RestIn, RestNext),
%	  NonPunctTokens = RestOut
	( punct_token(H, _) ->
	  NonPunctTokens = RestOut,
	  RestNext = RestIn
	; NonPunctTokens = [H|RestOut],
	  RestNext = RestIn
	),
	remove_punct_tokens(RestNext, RestOut).

% remove_next_s([], []).
% remove_next_s([H|T], Out) :-
% 	( H == s ->
% 	  Out = T
% 	; Out = [H|T]
% 	).
	
lex_form_ci_recs_input(InputTokens, LexicalEntries, RemainingInput) :-
	PrefixLength is 1,
	% separate_apostrophe(InputTokens, InputTokens1),
	longest_prefix(InputTokens, PrefixLength, LexRecs, InputMatch, LexMatch, RemainingInput),
	LexicalEntries = lexicon:[lexmatch:[LexMatch],
				  inputmatch:InputMatch,
				  records:LexRecs].
% 	LexiconType = 0,
% 	( control_value(lexicon, c) ->
% 	  c_lex_form_input(Index, LexiconType, InputTokens, Matches, 1),
% 	  sort_matches(Matches, SortedMatches),
% 	  get_best_match(SortedMatches, BestMatch),
% 	  BestMatch = match(LexMatch, OfsList, BestLength),
% 	  get_records_from_offsets(OfsList, LexRecs, Lexicon),
% 	  % LexRecs = [lexrec:[...], lexrec:[...], lexrec:[...]]
% 	  % (  foreach(LexRec, LexRecs)
%   	  % do LexRec = lexrec:List,
% 	  %    memberchk(entries:[entry:[num:[EUI]|_]|_], List),
% 	  %    format('### LEX    lex_rec:~w~n', [EUI])
% 	  % ),
% 	  skip_n_tokens(BestLength, InputTokens, InputMatch0, RemainingInput),
% 	  % InputMatch = InputMatch0,
% 	  re_attach_apostrophe_s_to_prev_word(InputMatch0, TagList, InputMatch),
% 	  % re-glue the apostrophe-s in InputMatch?
% 	  % just to avoid compiler warning about singleton var!
% 	  LexicalEntries = lexicon:[lexmatch:[LexMatch],
% 				    inputmatch:InputMatch,
% 				    records:LexRecs]
% 	  % format(user_output, 'LM: ~q~nIM: ~q~n~n', [LexMatch,InputMatch])
% 	; % Must remove all punct tokens because the norm_lex table,
% 	  % which is queried by db_get_prefix_EUIs/2, includes no punctuation chars.
% 	  PrefixLength is 1,
% 	  % separate_apostrophe(InputTokens, InputTokens1),
% 	  longest_prefix(InputTokens, PrefixLength, LexRecs, InputMatch, LexMatch, RemainingInput),
% 	  LexicalEntries = lexicon:[lexmatch:[LexMatch],
% 				    inputmatch:InputMatch,
% 				    records:LexRecs]
% 	  % format(user_output, 'LM: ~q~nIM: ~q~n~n', [LexMatch,InputMatch])
% 	).

%%% separate_apostrophe([], []).
%%% separate_apostrophe([H|RestInputTokensIn], InputTokensOut) :-
%%% 	atom_codes(H, HCodes),
%%% 	( append([BeforeCodes, "'", AfterCodes], HCodes) ->
%%% 	  atom_codes(BeforeAtom, BeforeCodes),
%%% 	  atom_codes(AfterAtom, AfterCodes),
%%% 	  InputTokensOut = [BeforeAtom, '''', AfterAtom|RestInputTokensOut]
%%% 	; InputTokensOut = [H|RestInputTokensOut]
%%% 	),
%%% 	separate_apostrophe(RestInputTokensIn, RestInputTokensOut).

%%% gets the best match, and collapses all offsets
% get_best_match([FirstMatch|RestMatches], N) :-
% 	FirstMatch = match(Type, Term, Ofs, Length),
% 	% The `C' lexicon code recognizes "HE's" as a lexical item. Go figure.
% 	Term \== 'HE''s',
% 	!,
% 	( findall(SomeOfs, member(match(Type, Term, SomeOfs, Length), RestMatches), MoreOfs) ->
% 	  N = match(Term, [Ofs|MoreOfs], Length)
% 	; N = match(Term, [Ofs], Length)
% 	).
% get_best_match([_|RestMatches], N) :-
% 	get_best_match(RestMatches, N).


%%% skip_n_tokens(+N, +List, -NTok, -Remain)
%%% returns the result of skipping 'n' tokens in a list

skip_n_tokens(N, List, Tokens, Remaining) :-
	( N =:= 0 ->
	 Tokens = [],
	 Remaining = List
	; N1 is N - 1,
	 List = [FirstTokenInList|RestTokensInList],
	 Tokens = [FirstTokenInList|RestTokensChosen],
	 skip_n_tokens(N1, RestTokensInList, RestTokensChosen, Remaining)
	).

%%% get_records_from_offsets(+OfsList, -RecsList, Lexicon)
% get_records_from_offsets([], [], _Lexicon).
% get_records_from_offsets([Ofs|R], [X|Y], Lexicon) :-
% 	read_lex_record(Lexicon, Ofs, Rec),
% 	fm_lexical_record(X, _EUI, Rec, []),
% 	get_records_from_offsets(R, Y, Lexicon).

%%% sort_matches(+In, -Out)
%%% sorts matches by decreasing match length.  Also, discards
%%% lower case matches in favor of and exact match of same length
%%% and record offset.
% sort_matches(In, Out) :-
% 	make_keys(In, InKeys),
% 	keysort(InKeys, OutKeys),
% 	rev(OutKeys, RevOutKeys),
% 	make_unkeys(RevOutKeys, UnOut),
% 	prune(UnOut, Out).

%%% constructs keys for sorting
% make_keys([], []).
% make_keys([F1|R1], [F2|R2]) :-
% 	F1 = match(Type, _Term, Ofs, Length),
% 	map_type(Type, MappedType),
% 	F2 = key(Length, MappedType, Ofs) - F1,
% 	make_keys(R1, R2).

% map_type(punct, 0).
% map_type(lower, 1).
% map_type(exact, 2).

%%% removes keys
% make_unkeys([], []).
% make_unkeys([F1|R1], [F2|R2]) :-
% 	F1 = _Key - Term,
% 	F2 = Term,
% 	make_unkeys(R1, R2).

%%% Between matches of the same length and offset, prefers the exact.
% prune([], []).
% prune([X], [X]) :- !.
% prune([X1, X2|R1], Y) :-
% 	X1 = match(exact, _T1, Ofs, Length),
% 	( X2 = match(lower, _T2, Ofs, Length)
% 	; X2 = match(punct, _T3, Ofs, Length)
%         ),
% 	!,
% 	prune([X1|R1], Y).
% prune([F|R1], [F|R2]) :-
% 	prune(R1, R2).

% reformat_list([], []).
% reformat_list([F|R], [X|Y]) :-
% 	functor(F, Term, 2),
% 	arg(1, F, Cat),
% 	arg(2, F, Feature),
% 	X = Term:[Cat:[Feature]],
% 	reformat_list(R, Y).
