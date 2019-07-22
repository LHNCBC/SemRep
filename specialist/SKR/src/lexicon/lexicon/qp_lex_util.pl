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

/* qp_lex_util.pl - some lexicon-related utility predicates
*/

:- module(qp_lex_util, [
	% lex_get_base_from_record_3/3,
	% lex_get_spvar_from_record/2,
	lex_form_ci_ord/4
    ]).

:- use_module(skr_lib(sicstus_utils), [
	lower/2
   ]).

:- use_module(library(lists), [
	append/2
   ]).

:- use_module(library(sets), [
	intersect/2
   ]).

:- use_module(lexicon(qp_lexicon), [
	lex_form_ci_var_lists/3
	% default_lexicon_file/1,
	% default_index_file/1
   ]).

%%% lex_get_base_from_record_3(+Record, +Categories, -Base)
%%% Extracts the base form of a record respecting Categories;
%%% it fails unless one of Entries has a category in Categories.
% lex_get_base_from_record_3(Record, Categories, Base) :-
%     lex_get_entries_from_record(Record, Entries),
%     entries_intersect_categories(Entries, Categories),
%     Record = lexrec:LexRecList,
%     ( memberchk(base:[Base], LexRecList)
%     ; memberchk(spelling_variants:SpVars, LexRecList),
%       member(Base, SpVars)
%     ).

% entries_intersect_categories([Entry|_Rest], Categories) :-
%     entry_intersects_categories(Entry, Categories),
%     !.
% %%% Note that there should be only one entry, but just in case
% entries_intersect_categories([_Entry|Rest], Categories) :-
%     entries_intersect_categories(Rest, Categories).
% 
% entry_intersects_categories(entry:Features, Categories) :-
%     get_entry_category(Features, CategorySet),
%     intersect(CategorySet, Categories).
% 
% get_entry_category([cat:CategorySet|_], CategorySet) :-
%     !.
% get_entry_category([_|Rest], CategorySet) :-
%     get_entry_category(Rest, CategorySet).

%%% lex_get_spvar_from_record(+Record, -Spvar)
%%% Extracts the spelling variants list for a lexical record.
%%% lex_get_spvar_from_record(Record, Spvar) :-
%%%     Record = lexrec:[base:[_Base], spelling_variants:Spvar|_].

%%% lex_get_entries_from_record(+Record, -Entries)
%%% Extracts the lexical entries list for a lexical record.
% lex_get_entries_from_record(Record, Entries) :-
%     Record = lexrec:[base:[_Base], spelling_variants:_Spvar, entries:Entries|_].

%%% lex_form_ci_ord(+Term, -Spelling, -Inflections)
%%% Given a query +Term, it returns two lists: one a complete list of all
%%% inflectional variants of the query term (-Inflections) and the other
%%% a subset of this list that has the same principal part as +Term.
%%% LRA--modified to use lex_form_ci_var_lists_4/4 to keep computations for
%%%      distinct lexical entries separate (otherwise, e.g., aid is
%%%      computed to be a spelling variant of AIDS)

lex_form_ci_ord(Term, Categories, Spelling, Inflections) :-
	lex_form_ci_var_lists(Term, Categories, VariantLists0),
	filter_by_categories([VariantLists0], Categories, VariantLists),
	%    format('     lfco: ~p ~p~n~p~n~p~n~n',
	%	   [Term,Categories,VariantLists0,VariantLists]),
	compute_all_variant_sps_infls(VariantLists, Term, SpLists, InflLists),
	append([[Term]|SpLists], Sps0),
	append(InflLists, Infls0),
	sort(Sps0, Spelling),
	sort(Infls0, Inflections).

filter_by_categories(X, [], X) :- !.
filter_by_categories([], _, []).
filter_by_categories([First|Rest], Categories, Result) :-
	filter_one_by_categories(First, Categories, FirstResult),
	( FirstResult == [] ->
	  filter_by_categories(Rest, Categories, Result)
	; Result = [FirstResult|RestResults],
	  filter_by_categories(Rest, Categories, RestResults)
	).

filter_one_by_categories([],_,[]).
filter_one_by_categories([First|Rest],Categories,[First|FilteredRest]) :-
	First = _W:[Cat:_Type],
	memberchk(Cat, Categories),
	!,
	filter_one_by_categories(Rest, Categories, FilteredRest).
filter_one_by_categories([_|Rest], Categories, FilteredRest) :-
	filter_one_by_categories(Rest, Categories, FilteredRest).


% Token is a lexical token, e.g., "cesarian".
% Variants is a list of lists of terms of the form Token:[LexCat:[Feature]], e.g.,
% [ [caesarean:[adj:[spvar]],
%    caesarian:[adj:[spvar]],
%    cesarean:[adj:[spvar]],
%    cesarian:[adj:[base]]]]
% We want SpVars to be all spelling variants
%      and Infls to be all inflections.
% Inflections is easy: Just take the atom to the left of the colon in each element of Variants.
% SpVars is more complicated:
% (1) Find all X:[LexCatX:[InflX]] terms in Variants in which X is a lowermatch of Token.
% (2) For each such term, find all Y:[LexCatX:[InflY]] terms in Variants such that
%     (a) InflX == InflY, or
%     (b) one of InflX and InflY is "base" and the other is "spvar".

% I spent quite some time devising a non-findall version of the logic below,
% and decided that the declarative readability of the findall version
% outweighed any possible efficiency gains, especially because this code
% is called only in dynamic variant generation (and therefore mm_variants),
% so it doesn't have to be blazingly fast (it's not called by normal MetaMap).

compute_all_variant_sps_infls([], _, [], []).
compute_all_variant_sps_infls([Variants|Rest], Token,
			      [SpVars|RestSpVars],
                              [Inflections|RestInflections]) :-
	get_all_inflections(Variants, Inflections0),
	sort(Inflections0, Inflections),
	findall(S, (   member(SomeToken:[LexCat:[Infl]], Variants),
		       lowermatch(Token, SomeToken),
		       get_synonym(LexCat:[Infl], Variants, S)), SpVars0),
	sort(SpVars0, SpVars),
	compute_all_variant_sps_infls(Rest, Token, RestSpVars, RestInflections).

get_all_inflections(Variants, Inflections) :-
	(  foreach(Infl:_, Variants),
	   foreach(Infl, Inflections)
	do true
	).

%%% does lowercase matching
lowermatch(Token, SomeToken) :-
	( Token == SomeToken ->
	  true
	; lower(Token, LToken),
	  lower(SomeToken, LSomeToken),
 	  LToken = LSomeToken
 	).

%%% looks for another +Token with the same principal part
get_synonym(Cat:[Feature], Variants, Synonym) :-
	member(Synonym:[Cat:[SomeFeature]], Variants),
	( Feature = SomeFeature ->
	  true
	; Feature     = base ->
	  SomeFeature = spvar
	; Feature     = spvar ->
	  SomeFeature = base
	).
