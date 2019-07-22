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

% File:	    metamap_tokenization.pl
% Module:   MetaMap
% Author:   Lan
% Purpose:  MetaMap tokenization routines


:- module(metamap_tokenization,[
	add_tokens_to_phrases/2,
	% must be exported for mwi_utilities
	ends_with_s/1,
	extract_tokens_with_tags/2,
	% must be exported for mm_print
	filter_tokens/3,
	% possessive handling
	% phrase predicates
	get_phrase_item_feature/3,
	get_phrase_item_name/2,
	get_phrase_item_subitems/2,
	get_subitems_feature/3,
	get_subitem_value/2,
	% must be exported for mwi_utilities
	is_ws/1,
	% must be exported for mwi_utilities
	is_ws_word/1,
	linearize_phrase/4,
	linearize_components/2,
	listify/2,
	local_alnum/1,
	local_alpha/1,
	local_ascii/1,
	local_digit/1,
	local_lower/1,
	local_print/1,
	local_punct/1,
	local_upper/1,
	local_to_lower/2,
	local_to_upper/2,
	local_ws/1,
	new_phrase_item/3,
	no_combine_pronoun/1,
	normalize_possessives/3,
	parse_phrase_word_info/3,
	% whitespace tokenization (break at whitespace and hyphens; ignore colons)
	tokenize_text/2,
	% wordind tokenization (maximal alphanumeric sequences)
	tokenize_text_more/2,
	tokenize_text_more_lc/2,
	% MetaMap tokenization (wordind - possessives)
	tokenize_text_mm/2,
	tokenize_text_mm_lc/2,
	% "complete" tokenization (wordind + punctuation)
	% "utter" tokenization (wordind + punctuation + whitespace)
	tokenize_fields_utterly/2,
	tokenize_text_utterly/2,
	set_subitems_feature/4,
	transparent_tag/1
    ]).

:- use_module(lexicon(qp_lookup),[
	punct_token/2
    ]).

% :- use_module(skr_lib(ctypes),[
% 	ctypes_bits/2
%     ]).

:- use_module(skr_lib(nls_lists),[
	first_n_or_less/3
    ]).

:- use_module(skr_lib(nls_strings),[
	atom_codes_list/2
    ]).

:- use_module(skr_lib(nls_text),[
	is_all_graphic_text/1
    ]).

:- use_module(skr_lib(sicstus_utils),[
	concat_strings_with_separator/3,
	lowercase_list/2,
	ttyflush/0
    ]).

:- use_module(skr(skr_utilities),[
	fatal_error/2,
	send_message/2
    ]).

:- use_module(text(text_object_util),[
	hyphen_punc_char/1
    ]).

:- use_module(library(lists),[
	append/2,
	last/2,
	rev/2
    ]).

/* ************************************************************************
   ************************************************************************
   ************************************************************************
                          MetaMap Tokenizing Predicates
   ************************************************************************
   ************************************************************************
   ************************************************************************ */





/*   add_tokens_to_phrases(+PhrasesIn, -PhrasesOut)
     add_tokens_to_phrase(+PhraseIn, -PhraseOut)

The tokens added by add_tokens_to_phrase/2 are normally obtained by
tokenizing the lexmatch/1 subitem (if any).  For shapes/2 and punc/1 items,
the newly created inputmatch/1 subitem is tokenized.  For example, text
"Fifty-six vena caval stent filters." produces the phrase

     [shapes([Fifty,-,six],[word_numeral]),
      mod([lexmatch(vena caval),inputmatch([vena,caval])]),
      mod([lexmatch(stent),inputmatch([stent])]),
      head([lexmatch(filters),inputmatch([filters])]),
      punc(.)]

which preprocesses to

     [shapes([inputmatch([Fifty,-,six]),features([word_numeral]),
              tokens([fifty,six])]),
      mod([lexmatch([vena caval]),inputmatch([vena,caval]),
           tokens([vena,caval])]),
      mod([lexmatch([stent]),inputmatch([stent]),tokens([stent])]),
      head([lexmatch([filters]),inputmatch([filters]),tokens([filters])]),
      punc([inputmatch([.]),tokens([])])]

*/

add_tokens_to_phrases([], []).
add_tokens_to_phrases([First|Rest], [ModifiedFirst|ModifiedRest]) :-
	add_tokens_to_one_phrase(First, ModifiedFirst),
	add_tokens_to_phrases(Rest, ModifiedRest).

add_tokens_to_one_phrase([], []).
add_tokens_to_one_phrase([First|Rest], [TokenizedFirst|TokenizedRest]) :-
	% add_lc_inputmatch_tokens_to_phrase_item(First, TokenizedFirst),
	add_tokens_to_phrase_item(First, TokenizedFirst),
	add_tokens_to_one_phrase(Rest, TokenizedRest).

add_lc_inputmatch_tokens_to_phrase_item(Item, ItemWithTokens) :-
	get_phrase_item_subitems(Item, SubItems),
	get_subitems_feature(SubItems, inputmatch, TextList),
	lowercase_list(TextList, TextListLC),
	set_phrase_item_feature(Item, tokens, TextListLC, ItemWithTokens).
	
% Existing version
% add_tokens_to_phrase_item(Item, TokenizedItem) :-
%         get_phrase_item_subitems(Item, SubItems),
%         ( get_non_null_lexmatch(SubItems, LexMatch) ->
%           make_list(LexMatch, TextList)
%         ; get_subitems_feature(SubItems,inputmatch,TextList)
%         ),
%         tokenize_all_text_mm_lc(TextList, TokenLists),
%         append(TokenLists, Tokens),
%         set_phrase_item_feature(Item, tokens, Tokens, TokenizedItem),
%         !.
% add_tokens_to_phrase_item(Item, Item).

% New version
add_tokens_to_phrase_item(Item, TokenizedItem) :-
	get_phrase_item_subitems(Item, SubItems),
	get_subitems_feature(SubItems, inputmatch, InputMatch),
	lowercase_list(InputMatch, InputMatchLC),
	remove_punct_tokens(InputMatchLC, InputMatchLCNoPunct),
	undo_possessives(InputMatchLCNoPunct, InputMatchLCNoPunctNoPoss),
	% InputMatchLCNoPunctNoPoss = InputMatchLCNoPunct,
	set_phrase_item_feature(Item, tokens, InputMatchLCNoPunctNoPoss, TokenizedItem),
	!.
add_tokens_to_phrase_item(Item, Item).


remove_punct_tokens([], []).
remove_punct_tokens([H|T], InputMatchLCNoPunct) :-
	( punct_token(H, _) ->
	  InputMatchLCNoPunct = InputMatchLCNoPunctRest
	; InputMatchLCNoPunct = [H|InputMatchLCNoPunctRest]
	),
	remove_punct_tokens(T, InputMatchLCNoPunctRest).

undo_possessives([], []).
undo_possessives([H|T], [HNoPoss|TNoPoss]) :-
	undo_one_possessive(H, HNoPoss),
	undo_possessives(T, TNoPoss).

undo_one_possessive(Token, TokenNoPoss) :-
	atom_codes(Token, TokenCodes),
	( append(TokenNoPossCodes, [0''', 0's], TokenCodes) ->
	  atom_codes(TokenNoPoss, TokenNoPossCodes)
	; TokenNoPoss = Token
	).

% get_non_null_lexmatch(SubItems, LexMatch) :-
% 	get_subitems_feature(SubItems, lexmatch, LexMatch),
% 	LexMatch \== [].

% make_list(LexMatch, TextList) :-
% 	( atom(LexMatch) ->
% 	  TextList = [LexMatch]
% 	; TextList = LexMatch
% 	).

/* 
   parse_phrase_word_info(+Phrase, +Option, -PhraseWordInfoPair)
   parse_phrase_word_info_aux(+Phrase, +FilterFlag, +WordsBegin
                          +PhraseWordsIn, -PhraseWordsOut,
                          +PhraseHeadWordsIn, -PhraseWordsOut,
                          +PhraseMapIn, -PhraseMapOut)

parse_phrase_word_info/3 extracts PhraseWordInfoPair from Phrase where
PhraseWordInfoPair is PhraseWordInfo:FilteredPhraseWordInfo and each element
of the pair is of the form
     pwi(PhraseWordL,PhraseHeadWordL,PhraseMap).
If Option is nofilter, then FilteredPhraseWordInfo is the same as PhraseWordInfo.
parse_phrase_word_info/2 calls parse_phrase_word_info/3 with Option filter.
Filtering consists of removing syntactic items with "opaque" tags
such as prepositions and determiners. See *input_match* family description for full list.
parse_phrase_word_info/9 is the auxiliary which does the work.  */

parse_phrase_word_info(Phrase, FilterChoice,
                       pwi(PhraseWordL,PhraseHeadWordL,PhraseMap)
                      :pwi(FilteredPhraseWordL,FilteredPhraseHeadWordL,FilteredPhraseMap)) :-
	parse_phrase_word_info_aux(Phrase, unfiltered, 1, [], PhraseWords,
				   [], PhraseHeadWords, [], PhraseMap0),
	create_word_list(PhraseWords, PhraseWordL),
	create_word_list(PhraseHeadWords, PhraseHeadWordL),
	rev(PhraseMap0, PhraseMap),
	( FilterChoice == filtered ->
	  parse_phrase_word_info_aux(Phrase, filtered, 1, [], FilteredPhraseWords,
				     [], FilteredPhraseHeadWords, [], FilteredPhraseMap0),
	  create_word_list(FilteredPhraseWords, FilteredPhraseWordL),
	  create_word_list(FilteredPhraseHeadWords, FilteredPhraseHeadWordL),
	  rev(FilteredPhraseMap0, FilteredPhraseMap)
	; FilterChoice == unfiltered ->
	  FilteredPhraseWordL = PhraseWordL,
	  FilteredPhraseHeadWordL = PhraseHeadWordL,
	  FilteredPhraseMap = PhraseMap
	).

parse_phrase_word_info_aux([], _FilterFlag, _WordsBegin,
			   PhraseWordsIn, PhraseWordsIn,
			   PhraseHeadWordsIn, PhraseHeadWordsIn,
			   PhraseMapIn, PhraseMapIn).
parse_phrase_word_info_aux([PhraseItem|Rest], FilterFlag, WordsBegin,
			   PhraseWordsIn, PhraseWordsOut,
			   PhraseHeadWordsIn, PhraseHeadWordsOut,
			   PhraseMapIn, PhraseMapOut) :-
	( FilterFlag == unfiltered ->
          % try tokens instead of input_match
	  extract_tokens(PhraseItem, IMWords, IMHeadWords)
	  % filter_tokens will exclude stop words,
	  % which is desirable, except not for the first word,
	  % so that we can identify concepts whose first word is a stop word,
	  % e.g., "in breathing" and "the uppermost part of the stomach", etc.
	  % ; WordsBegin =:= 1 ->
	  %   extract_tokens(PhraseItem, IMWords, IMHeadWords)
	; FilterFlag == filtered ->
	  filter_tokens(PhraseItem, IMWords, IMHeadWords)
	),
	!,
	( IMWords == [] ->
	  PhraseWordsInOut = PhraseWordsIn,
	  PhraseHeadWordsInOut = PhraseHeadWordsIn,
	  PhraseMapInOut = [[0,-1]|PhraseMapIn],
	  NewWordsBegin = WordsBegin
	; append(PhraseWordsIn, IMWords, PhraseWordsInOut),
	  append(PhraseHeadWordsIn, IMHeadWords, PhraseHeadWordsInOut),
	  length(IMWords, NIMWords),
	  NewWordsBegin is WordsBegin + NIMWords,
	  WordsEnd is NewWordsBegin - 1,
	  PhraseMapInOut = [[WordsBegin,WordsEnd]|PhraseMapIn]
	),
	parse_phrase_word_info_aux(Rest, FilterFlag, NewWordsBegin,
				   PhraseWordsInOut, PhraseWordsOut,
				   PhraseHeadWordsInOut, PhraseHeadWordsOut,
				   PhraseMapInOut, PhraseMapOut).
parse_phrase_word_info_aux([_|Rest], FilterFlag, WordsBegin,
			   PhraseWordsIn, PhraseWordsOut,
			   PhraseHeadWordsIn, PhraseHeadWordsOut,
			   PhraseMapIn, PhraseMapOut) :-
	parse_phrase_word_info_aux(Rest, FilterFlag, WordsBegin,
				   PhraseWordsIn, PhraseWordsOut,
				   PhraseHeadWordsIn, PhraseHeadWordsOut,
				   [[0,-1]|PhraseMapIn], PhraseMapOut).


/* create_word_list(+Words, -WordL)

create_word_list/2 forms wdl(Words,LCWords).  */

create_word_list(Words,wdl(Words,LCWords)) :-
    lowercase_list(Words,LCWords).

/* transparent_tag(?Tag)

transparent_tag/1 is a factual predicate of phrase tags which are essentially
ignored in determining the input match words for a phrase, i.e., processing
continues with the argument of the tag.  The only effect is that in
extract_input_match/3, IMHeadWords is always []. */

% Tagger tags
transparent_tag(adj).
transparent_tag(adv).
transparent_tag(noun).
transparent_tag(prefix).
transparent_tag(verb).
% Parser tags
transparent_tag(mod).
transparent_tag(pre).
transparent_tag(shapes).
transparent_tag(prefix).
transparent_tag(not_in_lex).  % Let unknowns through
transparent_tag(no_tag).      % Let unknowns through
transparent_tag(ing).         % Obsolete(?)
transparent_tag(pastpart).    % Obsolete(?)
% The "unknown" tag is added by get_this_variant/4 in generate_variant_info
% if no variants are found for a particular token. This is a hack.
transparent_tag(unknown).

/* opaque_tag(?Tag)

opaque_tag/1 is a factual predicate of phrase tags which prevent further
search for matching input.  */

opaque_tag(Tag) :- \+ transparent_tag(Tag).

%%% opaque_tag(error).    %  Unknown
%%% opaque_tag(unknown).    %  Unknown
%%% % Tagger tags
%%% opaque_tag(aux).
%%% opaque_tag(compl).
%%% opaque_tag(conj).
%%% opaque_tag(det).
%%% opaque_tag(modal).
%%% opaque_tag(prep).
%%% opaque_tag(pron).
%%% opaque_tag(punc).
%%% % Other tags
%%% opaque_tag(num).      %  digits only (Tagger tokenizer)
%%% opaque_tag(am).       %  & (Tagger tokenizer)
%%% opaque_tag(ap).       %  ' (Tagger tokenizer)
%%% opaque_tag(at).       %  @ (Tagger tokenizer)
%%% opaque_tag(ax).       %  * (Tagger tokenizer)
%%% opaque_tag(ba).       %  | (Tagger tokenizer)
%%% opaque_tag(bk).       %  [ or ] (Tagger tokenizer)
%%% opaque_tag(bl).       %  \ (Tagger tokenizer)
%%% opaque_tag(bq).       %  ` (Tagger tokenizer)
%%% opaque_tag(br).       %  { or } (Tagger tokenizer)
%%% opaque_tag(cl).       %  : (Tagger tokenizer)
%%% opaque_tag(cm).       %  , (Tagger tokenizer)
%%% opaque_tag(dl).       %  $ (Tagger tokenizer)
%%% opaque_tag(dq).       %  " (Tagger tokenizer)
%%% opaque_tag(eq).       %  = (Tagger tokenizer)
%%% opaque_tag(ex).       %  ! (Tagger tokenizer)
%%% opaque_tag(gr).       %  > (Tagger tokenizer)
%%% opaque_tag(hy).       %  - (Tagger tokenizer)
%%% opaque_tag(ls).       %  < (Tagger tokenizer)
%%% opaque_tag(nm).       %  # (Tagger tokenizer)
%%% opaque_tag(pa).       %  ( or ) (Tagger tokenizer)
%%% opaque_tag(pc).       %  % (Tagger tokenizer)
%%% opaque_tag(pd).       %  . (Tagger tokenizer)
%%% opaque_tag(pl).       %  + (Tagger tokenizer)
%%% opaque_tag(qu).       %  ? (Tagger tokenizer)
%%% opaque_tag(sc).       %  ; (Tagger tokenizer)
%%% opaque_tag(sl).       %  / (Tagger tokenizer)
%%% opaque_tag(tl).       %  ~ (Tagger tokenizer)
%%% opaque_tag(un).       %  _ (Tagger tokenizer)
%%% opaque_tag(up).       %  ^ (Tagger tokenizer)


/* extract_tokens(+PhraseItem, -TokenWords, -TokenHeadWords)
   extract_tokens_aux(+SubItemList, -TokenWords)
   extract_tokens_with_tags(+PhraseItemList, -IMsTags)
   filter_tokens(+PhraseItem, -TokenWords, -TokenHeadWords)
   filter_tokens_1(+PhraseItem, -TokenWords, -TokenHeadWords)
   filter_tokens_2(+PhraseItem, -TokenWords, -TokenHeadWords)

The *_tokens* family extracts tokens/1 information from
syntactic structures:

     PhraseItem ::= <tag>(<SubItemList>) | <primitivetag>(<atom>)

     SubItemList ::= <list> of <SubItem>

     SubItem ::= <unary term>  (e.g., tokens(_))

extract_tokens/3 finds the tokens/1 term, TokenWords, within PhraseItem.
If PhraseItem is a head, then TokenHeadWords=TokenWords.
extract_tokens_aux/2 finds the argument of the tokens/1 term in
SubItemList.
extract_tokens_with_tags/2 extracts the tokens and tag elements
from syntax.
filter_tokens/3 is similar to extract_tokens/3 except that only
"significant" tokens are reported where everything except prepositions,
determiners, conjunctions, punctuation and some modifiers (e.g., "-") is
significant.  filter_tokens/3 uses two filtering predicates,
filter_tokens_1/3 and filter_tokens_2/2.  */

extract_tokens(head(SubItemList), TokenWords, TokenWords) :-
	!,
	extract_tokens_aux(SubItemList, TokenWords).
% try this!
extract_tokens(shapes(ShapesList,_FeatList), ShapesList, []) :- !.
extract_tokens(PhraseItem, TokenWords, []) :-
	functor(PhraseItem, _Tag, 1),
	arg(1,PhraseItem, SubItemList),
	!,
	extract_tokens_aux(SubItemList, TokenWords).
extract_tokens(PhraseItem, [], []) :-
	fatal_error('extract_tokens/3 failed for ~p.~n', [PhraseItem]).

% for "primitive" tags
extract_tokens_aux(PhraseItems, TokenWords) :-
	( atom(PhraseItems) ->
	  TokenWords = [PhraseItems],
	  send_message('PRIMITIVE: ~q~n', [PhraseItems])
	; % PhraseItems = [H|T],
	  memberchk(tokens(TokenWords), PhraseItems)
	).

% For each syntactic element in arg 1 with non-null tokens(_) and tag(_), e.g.,
% [head([lexmatch(['heart attack']),inputmatch([heart,attack]),tag(noun),tokens([heart,attack])])]
% create a structure of the form tokenstag(Tokens,Tag).
% In this case, tokenstag([heart,attack], noun)
extract_tokens_with_tags([], []).
extract_tokens_with_tags([PhraseItem|RestPhraseItems], Result) :-
	arg(1, PhraseItem, SubItemList),
	extract_tokens_aux(SubItemList, Tokens),
	extract_tag(SubItemList, Tag),
	!,
	( Tokens == [] ->
	  Result = RestTokensTags
	; Tag == none ->
	  Result = RestTokensTags
	; Result = [tokenstag(Tokens,Tag)|RestTokensTags]
	),
	extract_tokens_with_tags(RestPhraseItems, RestTokensTags).
extract_tokens_with_tags([_|RestPhraseItems], Result) :-
	extract_tokens_with_tags(RestPhraseItems, Result).


extract_tag(SubItemList, Tag) :-
	( memberchk(tag(Tag), SubItemList) ->
	  true
	; Tag = none
	).

% extract_tag([], none).
% extract_tag([tag(Tag)|_], Tag) :-
% 	!.
% extract_tag([_|Rest], Tag) :-
% 	extract_tag(Rest,Tag).
 
filter_tokens(PhraseItem, TokenWords, TokenHeadWords) :-
	filter_tokens_1(PhraseItem, TokenWords0, TokenHeadWords0),
	filter_tokens_2(TokenWords0, TokenWords),
	filter_tokens_2(TokenHeadWords0, TokenHeadWords).

filter_tokens_1(head(SubItemList), TokenWords,TokenWords) :-
	!,
	extract_tokens_aux(SubItemList, TokenWords).
filter_tokens_1(PhraseItem, TokenWords, []) :-
	functor(PhraseItem, Tag, 1),
	( opaque_tag(Tag) ->        % stop
	  TokenWords = []
	; transparent_tag(Tag) ->   % continue
	  arg(1, PhraseItem, SubItemList),
	  extract_tokens_aux(SubItemList, TokenWords)
	; arg(1, PhraseItem, SubItemList),
	  extract_tokens_aux(SubItemList, TokenWords),
	  send_message('### WARNING: ~q is an unexpected phrase item!~n', [PhraseItem]),
	  ttyflush
	),
	!.
filter_tokens_1(PhraseItem, [], []) :-
	fatal_error('filter_tokens_1/3 failed for ~p.~n', [PhraseItem]).

filter_tokens_2([], []).
filter_tokens_2([First|Rest], [First|FilteredRest]) :-
	\+ is_all_graphic_text(First),
	!,
	filter_tokens_2(Rest, FilteredRest).
filter_tokens_2([_First|Rest], FilteredRest) :-
	filter_tokens_2(Rest, FilteredRest).

/* linearize_phrase(+Phrase, +PhraseMap, -LPhrase, -LPhraseMap)
   linearize_phrase_item(+MapComponent, +PhraseItem, -LItems, -LMap)
   linearize_phrase_item_aux(+PhraseItem, +LMapComponent, +InputMatches,
                             -LItems, -LMap)
   linearize_components(+Components, -LComponents)
   linearize_component(+Component, -LComponent)
   linearize_component(+Begin, +End, -LComponent)

linearize_phrase/4 chops up Phrase (and PhraseMap) into LPhrase (and LPhraseMap)
so that each element of the map is a singleton.  Chopping is performed on both
the map itself as well as on the inputmatch/1 and tokens/1 terms of the phrase
(but not the lexmatch/1 terms).

For example, given text "Fifty-six vena caval stent filters.",

     Phrase: [shapes([inputmatch(['Fifty',-,six]),
		      features([word_numeral]),
		      tokens([fifty,six])]),
	      mod([lexmatch(['vena caval']),
		   inputmatch([vena,caval]),
		   tag(adj),
		   tokens([vena,caval])]),
	      mod([lexmatch([stent]),
		   inputmatch([stent]),
		   tag(noun),
		   tokens([stent])]),
	      head([lexmatch([filters]),
		    inputmatch([filters]),
		    tag(noun),
		    tokens([filters])]),
	      punc([inputmatch(['.']),
		    tokens([])])]

     PhraseMap: [[1,2],[3,4],[5,5],[6,6],[0,-1]]
     is linearized to

     LPhrase: [shapes([inputmatch(['Fifty',-]),
		      features([word_numeral]),
		      tokens([fifty])]),
	      shapes([inputmatch([six]),
		      features([word_numeral]),
		      tokens([six])]),
	      mod([lexmatch(['vena caval']),
		   inputmatch([vena]),
		   tag(adj),
		   tokens([vena])]),
	      mod([lexmatch(['vena caval']),
		   inputmatch([caval]),
		   tag(adj),
		   tokens([caval])]),
	      mod([lexmatch([stent]),
		   inputmatch([stent]),
		   tag(noun),
		   tokens([stent])]),
	      head([lexmatch([filters]),
		    inputmatch([filters]),
		    tag(noun),
		    tokens([filters])]),
	      punc([inputmatch(['.']),
		    tokens([])])]

     LPhraseMap: [[1],[2],[3],[4],[5],[6],[0]]

linearize_phrase_item/4
linearize_phrase_item/5
linearize_components/2
linearize_component/2
linearize_component/3
*/

linearize_phrase([], [], [], []).
linearize_phrase([FirstItem|RestItems],  [[Begin,End]|RestMap],
                 [FirstItem|RestLItems], [[Begin]|RestLMap]) :-
	( Begin =:= 0
	; Begin =:= End),
	!,
	linearize_phrase(RestItems, RestMap, RestLItems, RestLMap).
linearize_phrase([FirstItem|RestItems], [FirstMap|RestMap], LItems, LMap) :-
	linearize_phrase_item(FirstMap, FirstItem, FirstLItems, FirstLMap),
	!,
	%temp
	%format('~nlp FirstItem~n~p~n',[FirstItem]),
	%format('lp FirstMap~n~p~n',[FirstMap]),
	%format('lp FirstLItems~n~p~n',[FirstLItems]),
	%format('lp FirstLMap~n~p~n',[FirstLMap]),
	append(FirstLItems, RestLItems, LItems),
	append(FirstLMap, RestLMap, LMap),
	linearize_phrase(RestItems, RestMap, RestLItems, RestLMap).

linearize_phrase_item(MapComponent, PhraseItem, LItems, LMap) :-
	get_phrase_item_feature(PhraseItem, tokens, Tokens),
	get_phrase_item_feature(PhraseItem, inputmatch, IM0),
	coordinate_tokens(Tokens, IM0, IM),
	linearize_component(MapComponent, LMapComponent),
	get_phrase_item_feature(PhraseItem, bases, Bases0),
	( Bases0 == [] ->
	  linearize_phrase_item_6(LMapComponent, PhraseItem, Tokens, IM, LItems, LMap)
	; coordinate_tokens(Tokens, Bases0, Bases),
	  linearize_phrase_item_7(LMapComponent, PhraseItem, Tokens, IM, Bases, LItems, LMap)
	).

linearize_phrase_item_6([], _PhraseItem, [], [], [], []) :- !.
linearize_phrase_item_6([FirstMap|RestMap], PhraseItem, [FirstToken|RestTokens],
			[FirstIM|RestIM], [FirstLItem|RestLItems], [[FirstMap]|RestLMap]) :-
	set_phrase_item_feature(PhraseItem, tokens, [FirstToken], FirstLItem0),
	set_phrase_item_feature(FirstLItem0, inputmatch, FirstIM, FirstLItem1),
	( RestMap == [] ->
	  FirstLItem = FirstLItem1
	; demote_phrase_item(FirstLItem1, FirstLItem)
	),
	!,
	linearize_phrase_item_6(RestMap, PhraseItem, RestTokens, RestIM, RestLItems, RestLMap).
linearize_phrase_item_6(_, PhraseItem, _, _, _, _) :-
	fatal_error('Cannot linearize ~p (mapping/input match)~n', [PhraseItem]).

linearize_phrase_item_7([], _PhraseItem, [], [], [], [], []) :- !.
linearize_phrase_item_7([FirstMap|RestMap], PhraseItem, [FirstToken|RestTokens],
			[FirstIM|RestIM], [FirstBase|RestBases],
			[FirstLItem|RestLItems], [[FirstMap]|RestLMap]) :-
	set_phrase_item_feature(PhraseItem,  tokens,     [FirstToken], FirstLItem0),
	set_phrase_item_feature(FirstLItem0, inputmatch, FirstIM,      FirstLItem1),
	set_phrase_item_feature(FirstLItem1, bases,      FirstBase,    FirstLItem2),
	( RestMap == [] ->
	  FirstLItem = FirstLItem2
	; demote_phrase_item(FirstLItem2,FirstLItem)
	),
	!,
	linearize_phrase_item_7(RestMap, PhraseItem, RestTokens, RestIM, RestBases, RestLItems, RestLMap).
linearize_phrase_item_7(_, PhraseItem, _, _, _, _, _) :-
	fatal_error('Cannot linearize ~p (mapping/input match)~n', [PhraseItem]).

coordinate_tokens(Tokens, IM0, [FirstIM|RestIM]) :-
	length(Tokens, NTok),
	length(IM0, NIM),
	NFirst is NIM - NTok + 1,
	first_n_or_less(IM0, NFirst, FirstIM),
	append(FirstIM, IM1, IM0),
	listify(IM1, RestIM).

listify([], []).
listify([First|Rest], [[First]|ModifiedRest]) :-
	listify(Rest, ModifiedRest).

linearize_components([], []).
linearize_components([First|Rest], [LFirst|LRest]) :-
	linearize_component(First, LFirst),
	linearize_components(Rest, LRest).

linearize_component([Begin,Begin], [Begin]) :- !.
linearize_component([Begin,End], [0]) :-
	Begin > End,
	!.
linearize_component([Begin,End], LComponent) :-
	linearize_component_aux(Begin, End, LComponent).

linearize_component_aux(End, End, [End]) :- !.
linearize_component_aux(Begin, End, [Begin|LComponent]) :-
	NewBegin is Begin + 1,
	linearize_component_aux(NewBegin, End, LComponent).

/* demote_phrase_item(+PhraseItem, -DemotedPhraseItem)

demote_phrase_item/2 transforms head/1 phrase items into mod/1 phrase items
leaving all other phrase items alone.
*/

demote_phrase_item(head(Subitems), mod(Subitems)) :- !.
demote_phrase_item(Item, Item).

/* 
   tokenize_text(+Text, -TokText)

tokenize_text/2 transforms Text (atom/string) into a list of tokens TokText
breaking at break characters (see break_character/1) including spaces
and hyphens and ignoring some characters (see ignore_character/1) such
as colons.
*/

tokenize_text(Text,TokText) :-
    (   atom(Text) ->
        atom_codes(Text,String),
        phrase(tt_string(TokString),String),
        atom_codes_list(TokText,TokString)
    ;   phrase(tt_string(TokText),Text)
    ),
    !.

/*  TOKENIZE TEXT GRAMMAR  */

tt_string(TS) --> tt_token(T), {T\==[]}, !, tt_string(S), {TS=[T|S]}

              ;   [_Char], !, tt_string(S), {TS=S}

              ;   {TS=[]}.

tt_token(T) --> [Char], {ignore_character(Char)}, !, tt_token(S), {T=S}

            ;   [Char], {\+break_character(Char)}, !, tt_token(S), {T=[Char|S]}

            ;   {T=[]}.


/* break_character(?Char)

break_character/1 is a factual predicate of token break characters. */

break_character(0' ).
break_character(0'-).


/* ignore_character(?Char)

ignore_character/1 is a factual predicate of token characters which are
ignored. */

ignore_character(0':).

tokenize_text_more(Text,TokText) :-
    (   atom(Text) ->
        atom_codes(Text,String),
        phrase(ttm_string(TokString),String),
        atom_codes_list(TokText,TokString)
    ;   phrase(ttm_string(TokText),Text)
    ),
    !.

tokenize_text_more_lc(Text,TokText) :-
    tokenize_text_more(Text,TokText0),
    lowercase_list(TokText0,TokText).



/*  TOKENIZE TEXT MORE GRAMMAR  */

ttm_string(TS) --> ttm_token(T), {T\==[]}, !, ttm_string(S), {TS=[T|S]}

               ;   [_Char], !, ttm_string(S), {TS=S}

               ;   {TS=[]}.

ttm_token(T) --> [Char], {local_alnum(Char)}, !, ttm_token(S), {T=[Char|S]}

             ;   {T=[]}.


/* 
   tokenize_text_mm(+Text, -TokText)
   tokenize_all_text_mm_lc(+TextList, -TokTextList)
   tokenize_text_mm_lc(+Text, -TokText)

tokenize_text_mm/2 transforms Text (atom/string) into a list of tokens TokText
similar to the wordind regime used in tokenize_text_more/2. The difference
is that tokenize_text_mm/2 respects possessives ("'s" at the end of a word).
tokenize_text_mm_lc/2 lowercases the result of tokenize_text_mm/2.
tokenize_all_.../2 tokenizes a list of text into a SINGLE list of tokens. */

tokenize_text_mm(Text, TokText) :-
	( atom(Text) ->
	  atom_codes(Text, String),
	  tokenize_text_utterly(String, StringToks0),
	  % normalize_possessives_and_remove_nonwords(StringToks0, StringToks),
	  remove_possessives_and_nonwords(StringToks0, StringToks),
	  atom_codes_list(TokText, StringToks)
	; tokenize_text_utterly(Text, TokText0),
	  remove_possessives_and_nonwords(TokText0, TokText)
	).


/* remove_possessives_and_nonwords(+UTokensIn, -UTokensOut)

remove_possessives_and_nonwords/2 filters out possessives and nonwords
from the results of tokenize_text_utterly/2. */

remove_possessives_and_nonwords([], []).
remove_possessives_and_nonwords([NonWord|Rest], FilteredRest) :-
	% \+ is_ws_word(NonWord),
	is_punct_or_ws(NonWord),
	!,
	remove_possessives_and_nonwords(Rest, FilteredRest).
% singular possessives
remove_possessives_and_nonwords([Word,"'","s"], [Word]) :- !.
remove_possessives_and_nonwords([Word,"'","s",WhiteSpace|Rest], [Word|FilteredRest]) :-
	is_ws(WhiteSpace),
	!,
	remove_possessives_and_nonwords(Rest, FilteredRest).
% plural possessives
remove_possessives_and_nonwords([Word,"'"], [Word]) :-
	ends_with_s(Word),
	!.
remove_possessives_and_nonwords([Word,"'",WhiteSpace|Rest], [Word|FilteredRest]) :-
	ends_with_s(Word),
	is_ws(WhiteSpace),
	!,
	remove_possessives_and_nonwords(Rest, FilteredRest).
remove_possessives_and_nonwords([First|Rest], [First|FilteredRest]) :-
	remove_possessives_and_nonwords(Rest, FilteredRest).


%%% normalize_possessives_and_remove_nonwords(TokensIn, TokensOut) :-
%%% 	KeepWhiteSpace = 0,
%%% 	normalize_possessives(TokensIn, KeepWhiteSpace, TokensInOut),
%%% 	remove_nonwords(TokensInOut, TokensOut).
%%% 
%%% remove_nonwords([], []).
%%% remove_nonwords([NonWord|Rest], FilteredRest) :-
%%% 	% \+ is_ws_word(NonWord),
%%% 	is_punct_or_ws(NonWord),
%%% 	!,
%%% 	remove_nonwords(Rest, FilteredRest).
%%% remove_nonwords([Word|Rest], [Word|FilteredRest]) :-
%%% 	remove_nonwords(Rest, FilteredRest).
%%% 
% singular possessives
%%% % remove_possessives_and_nonwords([Word,"'","s"], [Word]) :- !.
%%% normalize_possessives([], _KeepWhiteSpace, []).
%%% normalize_possessives([PossessiveWord], _KeepWhiteSpace, [Word]) :-
%%% 	% append(Word, "'s", WordApostropheS),
%%% 	append(Word, [C1,C2], PossessiveWord),
%%% 	atom_codes(Atom, [C1,C2]),
%%% 	apostrophe_s_or_s_apostrophe(Atom),
%%% 	!.
%%% % normalize_possessives([Word,"'","s",WhiteSpace|Rest], [Word|FilteredRest]) :-
%%% % normalize_possessives([WordApostropheS,WhiteSpace|Rest], KeepWhiteSpace, [Word|FilteredRest]) :-
%%% normalize_possessives([WordApostropheS,WhiteSpace|Rest], KeepWhiteSpace, NormalizedTokens) :-
%%% 
%%% 	% append(Word, "'s", WordApostropheS),
%%% 	append(Word, [C1,C2], WordApostropheS),
%%% 	atom_codes(Atom, [C1,C2]),
%%% 	apostrophe_s_or_s_apostrophe(Atom),
%%% 	is_ws(WhiteSpace),
%%% 	!,
%%% 	( KeepWhiteSpace =:= 1 ->
%%% 	  NormalizedTokens = [Word,WhiteSpace|FilteredRest]
%%% 	; NormalizedTokens = [Word|FilteredRest]
%%% 	),
%%% 	normalize_possessives(Rest, KeepWhiteSpace, FilteredRest).
%%% % plural possessives
%%% % normalize_possessives([Word,"'"], [Word]) :-
%%% % normalize_possessives([WordSApostrophe], [Word]) :-
%%% % 	append(Word, "s'", WordSApostrophe),
%%% %	% ends_with_s(Word),
%%% % 	!.
%%% % normalize_possessives([Word,"'",WhiteSpace|Rest], [Word|FilteredRest]) :-
%%% % normalize_possessives([WordSApostrophe,WhiteSpace|Rest], [Word|FilteredRest]) :-
%%% % 	append(Word, "s'", WordSApostrophe),
%%% % 	% ends_with_s(Word),
%%% % 	is_ws(WhiteSpace),
%%% % 	!,
%%% % 	normalize_possessives(Rest, FilteredRest).
%%% normalize_possessives([First|Rest], KeepWhiteSpace, [First|FilteredRest]) :-
%%% 	normalize_possessives(Rest, KeepWhiteSpace, FilteredRest).
%%% 
%%% apostrophe_s_or_s_apostrophe('''s').
%%% apostrophe_s_or_s_apostrophe('s''').


% ORIG
% is_ws_word([]).
% is_ws_word([AlNum|Rest]) :-
% 	is_alnum(AlNum),
% 	is_ws_word(Rest).

is_punct_or_ws([Char]) :-
	( local_punct(Char) ->
	  true
	; local_ws(Char)
	).

% MATS
is_ws_word([]).
is_ws_word([AlNum|Rest]) :-
	local_alnum(AlNum),
	is_ws_word(Rest).

ends_with_s(Word) :-
	% reversed order of args from QP library version!
	last(Word, 0's).

% ORIG
% is_ws([Char]) :-
% 	is_space(Char).

% MATS
is_ws([Char]) :-
	local_ws(Char).

% tokenize_all_text_mm_lc([], []).
% tokenize_all_text_mm_lc([First|Rest], [TokenizedFirst|TokenizedRest]) :-
% 	tokenize_text_mm_lc(First, TokenizedFirst),
% 	tokenize_all_text_mm_lc(Rest, TokenizedRest).
 
tokenize_text_mm_lc(Text, TokText) :-
	tokenize_text_mm(Text, TokText0),
	lowercase_list(TokText0, TokText).

/* tokenize_fields_utterly(+Fields, -TokFields)
/* tokenize_text_utterly(+Text, -TokText)

tokenize_fields_utterly/2 uses tokenize_text_utterly/2 to tokenize the text
in Fields which is a list of fields of the form
  [<field>,<lines>] where <field> is a string and <lines> is a list of strings.
TokFields is a list of tokenized fields of the form
  [<field>,<tokens>] where <field> is a string and <tokens> is a list of
strings.
Note that the <lines> for each <field> are concatenated together with a
blank separator between each line, forming a single string to pass off to
tokenize_text_utterly/2 (which retains the newline characters in the
tokenization it produces.

tokenize_text_utterly/2 transforms Text (atom/string) into a list of
tokens TokText breaking at, but keeping, all non-alphanumerics.  This is
like the current tokenization regime used by wordind except that whitespace
and punctuation are not ignored here.  */

tokenize_fields_utterly([], []).
tokenize_fields_utterly([First|Rest], [TokFirst|TokRest]) :-
	tokenize_one_field_utterly(First, TokFirst),
	tokenize_fields_utterly(Rest, TokRest).

tokenize_one_field_utterly([Field,Lines], [Field,TokField]) :-
	concat_strings_with_separator(Lines, " ", FieldText),
	tokenize_text_utterly(FieldText, TokField0),
	re_attach_apostrophe_s_tokens(TokField0, TokField),
	% TokField = TokField0,
	!.

% form_decimal_numbers([], []).
% form_decimal_numbers([Token1,Token2,Token3|RestTokensIn], [DecimalToken|RestTokensOut]) :-
% 	Token2 = ".",
% 	nls_strings:is_integer_string(Token1),
% 	nls_strings:is_integer_string(Token3),
% 	!,
% 	append([Token1, Token2, Token3], DecimalToken),
% 	form_decimal_numbers(RestTokensIn, RestTokensOut).
% form_decimal_numbers([H|RestIn], [H|RestOut]) :-
% 	form_decimal_numbers(RestIn, RestOut).

% The call to tokenize_text_utterly(FieldText, TokField0)
% will parse words ending in "'s" (apostrophe + s) into multiple tokens, e.g.,
% tokenize_text_utterly("finkelstein's test positive", TokField0)
% will instantiate TokField0 to ["finkelstein", "'", "s", " ", "test", " ", "positive"].
% MetaMap's (new!) default behavior is to reattach the "'s"  to the previous token.

% There are numerous cases of ill-formed text involving apostrophe-s
% that need to be handled via special cases.

% Special case for input like "area, wernicke's"
tokenized_field_in_out(["'", "s"], [], []).

tokenized_field_in_out(["'", "s", TokenAfterS           | RestTokenizedFieldIn],
		       TokenAfterS, RestTokenizedFieldIn).
% Ill-formed case 1:  e.g., "Crohn' s"
tokenized_field_in_out(["'", " ", "s", TokenAfterS      | RestTokenizedFieldIn],
		       TokenAfterS, RestTokenizedFieldIn).
% Ill-formed case 2:  e.g., "Crohn 's"
tokenized_field_in_out([" ", "'", "s", TokenAfterS      | RestTokenizedFieldIn],
		       TokenAfterS, RestTokenizedFieldIn).
% Ill-formed case 3:  e.g., "Crohn ' s"
tokenized_field_in_out([" ", "'", " ", "s", TokenAfterS | RestTokenizedFieldIn],
		       TokenAfterS, RestTokenizedFieldIn).


re_attach_apostrophe_s_tokens([], []).
re_attach_apostrophe_s_tokens(TokenizedFieldIn, TokenizedFieldOut) :-
	TokenizedFieldIn = [OrigString | TailTokenizedFieldIn],
	\+ no_reattach_string(OrigString),
	tokenized_field_in_out(TailTokenizedFieldIn, TokenAfterS, RestTokenizedFieldIn),
	% if the apostrophe-s appears as "'s'", as in, e.g.,
	% "more typical 's'-shaped VCs" (PMID 20444214), do not re-attach!
	% TokenAfterS \== [0'''],
	!,
	TokenizedFieldOut = [StringWithApostropheS|RestTokenizedFieldOut],
	append([OrigString, "'", "s"], StringWithApostropheS),
	% TokenizedFieldOut = [StringWithApostropheS | RestTokenizedField],
	% Special case for input like "area, wernicke's"
	( TokenAfterS == [],
	  RestTokenizedFieldIn == [] ->
	  RestTokenizedFieldOut = []
	; re_attach_apostrophe_s_tokens([TokenAfterS|RestTokenizedFieldIn], RestTokenizedFieldOut)
	).
re_attach_apostrophe_s_tokens([H|Rest], [H|NewRest]) :-
	re_attach_apostrophe_s_tokens(Rest, NewRest).


% succeeds if OrigString (the string representation of the previous token)
% is a string to which the following "'s" (apostrophe-s) should not be re-attached.
% That's true of any token ending in a non-alnum char, "he", "she", and "it".
no_reattach_string(OrigString) :-
	( last(OrigString, LastChar),
	  \+ local_alnum(LastChar) ->
	  true
	; atom_codes(Atom, OrigString),
	  no_combine_pronoun(Atom)
	).

no_combine_pronoun(he).
no_combine_pronoun(she).
no_combine_pronoun(it).

tokenize_text_utterly(Text, TokenizedText) :-
	% PrevChar = '',
	( atom(Text) ->
	  atom_codes(Text, String),
	  % ttu_string(String, PrevChar, TokenizedString, []),
	  ttu_string(String, TokenizedString, []),
	  atom_codes_list(TokenizedText, TokenizedString)
	% ; ttu_string(Text, PrevChar, TokenizedText, [])
	; ttu_string(Text, TokenizedText, [])
	),
	!.

% ORIG
% tokenize_text_utterly(Text,TokText) :-
% 	( atom(Text) ->
% 	  atom_codes(Text,String),
% 	  phrase(ttu_string(TokString),String),
% 	  atom_codes_list(TokText,TokString)
% 	; phrase(ttu_string(TokText),Text)
% 	),
% 	!.

% MATS
% tokenize_text_utterly(Text, TokenizedText) :-
% 	tokenize_text_utterly_1(Text, TokenizedText0),
% 	TokenizedText = TokenizedText0.
% 	% form_decimal_numbers(TokenizedText0, TokenizedText).

% tokenize_text_utterly_1(Text,TokText) :-
% 	( atom(Text) ->
% 	  atom_codes(Text,String),
% 	  ttu_string(TokString,String,[]),
% 	  atom_codes_list(TokText,TokString)
% 	; ttu_string(TokText,Text,[])
% 	),
% 	!.

/*  TOKENIZE TEXT UTTERLY GRAMMAR  */

% ORIG
% ttu_string(TS) -->
% 	( ttu_token(T),
% 	  { T\==[] },
% 	  !,
% 	  ttu_string(S),
% 	  { TS=[T|S] }
% 	; [Char],
% 	  !,
% 	  ttu_string(S),
% 	  { TS = [[Char]|S] }
% 	; { TS= [] }
% 	).

ttu_string([], Remainder, Remainder).
ttu_string([Char|RestStringIn], TokenizedString, Remainder) :-
	( local_alnum(Char) ->
	  TokenizedString = [[Char|RestToken]|RemainingTokens],
	  ttu_token(RestStringIn, RestToken, RestStringOut)
	; TokenizedString = [[Char]|RemainingTokens],
	  RestStringOut = RestStringIn
	),
	ttu_string(RestStringOut, RemainingTokens, Remainder).

ttu_token([], [], []).
ttu_token([Char|RestString], RestTokenIn, RestTokenOut) :-
	( local_alnum(Char) ->
	  RestTokenIn = [Char|RestTokenNext],
	  ttu_token(RestString, RestTokenNext, RestTokenOut)
	; RestTokenIn = [],
	  RestTokenOut = [Char|RestString]
	).

% No-DCG version to allow folding ttu_token2/3 into ttu_token/3
%%% ttu_string([], _PrevChar, Remainder, Remainder).
%%% ttu_string([Char|RestStringIn], PrevChar, TokenizedString, Remainder) :-
%%% 	( Char is 39,
%%% 	  local_alnum(PrevChar),
%%% 	  PrevChar \== 39,
%%% 	  RestStringIn = [NextChar|_],
%%% 	  local_alnum(NextChar),
%%% 	  NextChar \== 39 ->
%%% 	  TokenizedString = [[Char|RestToken]|RemainingTokens],
%%% 	  ttu_token(RestStringIn, Char, RestToken, LastChar, RestStringOut)
%%% 	; local_alnum(Char),
%%% 	  Char \== 39 ->
%%% 	  TokenizedString = [[Char|RestToken]|RemainingTokens],
%%% 	  ttu_token(RestStringIn, Char, RestToken, LastChar, RestStringOut)
%%% 	; TokenizedString = [[Char]|RemainingTokens],
%%% 	  RestStringOut = RestStringIn,
%%% 	  LastChar = Char
%%% 	),
%%% 	ttu_string(RestStringOut, LastChar, RemainingTokens, Remainder).
%%% 
%%% ttu_token([], PrevChar, [], PrevChar, []).
%%% ttu_token([Char|RestStringIn], PrevChar, RestTokenIn, LastChar, RestTokenOut) :-
%%% 	( Char is 39,
%%% 	  local_alnum(PrevChar),
%%% 	  PrevChar \== 39,
%%% 	  RestStringIn = [NextChar|_],
%%% 	  local_alnum(NextChar),
%%% 	  NextChar \== 39 ->
%%% 	  RestTokenIn = [Char|RestTokenNext],
%%% 	  ttu_token(RestStringIn, Char, RestTokenNext, LastChar, RestTokenOut)
%%% 	; local_alnum(Char),
%%% 	  Char \== 39 ->
%%% 	  RestTokenIn = [Char|RestTokenNext],
%%% 	  ttu_token(RestStringIn, Char, RestTokenNext, LastChar, RestTokenOut)
%%% 	; RestTokenIn = [],
%%% 	  LastChar = PrevChar,
%%% 	  RestTokenOut = [Char|RestStringIn]
%%% 	).

% MATS
% ttu_string(TS) -->
% 	[Char],
% 	{ local_alnum(Char) }, !,
% 	{ TS=[[Char|S]|R] },
% 	ttu_token(S),
% 	ttu_string(R).
% ttu_string(TS) -->
% 	[Char], !,
% 	{ TS = [[Char]|S] },
% 	ttu_string(S).
% ttu_string([]) --> [].
 
% ORIG
% ttu_token(T) -->
% 	( [Char],
% 	  { is_alnum(Char) },
% 	  !,
% 	  ttu_token(S),
% 	  { T=[Char|S] }
% 	; { T =[] }
% 	).

% MATS
% ttu_token(T) -->
% 	[Char],
% 	{ local_alnum(Char) }, !,
% 	{ T=[Char|S] },
% 	ttu_token(S).
% ttu_token([]) --> [].

% MATS 2
% ttu_token(T, S0, S) :-
% 	ttu_token2(S0, T, S).
% 
% ttu_token2([], [], []).
% ttu_token2([Char|S0], T, S) :-
%	ctypes_bits(Char, Bits),
%	% succeeds if Char is alnum
%	Mask is Bits /\ 3840,
%	(   Mask =\= 0 ->
% 	(   local_alnum(Char) ->
% 	    T = [Char|R],
% 	    ttu_token2(S0, R, S)
% 	;   T = [],
% 	    S = [Char|S0]
% 	).

/* ************************************************************************
   ************************************************************************
   ************************************************************************
                       MetaMap Phrase Access Predicates
   ************************************************************************
   ************************************************************************
   ************************************************************************ */


/* get_phrase_item_feature(+PhraseItem, +Feature, -FeatureValue)
   get_phrase_item_name(+PhraseItem, -ItemName)
   get_phrase_item_subitems(+PhraseItem, -Subitems)
   new_phrase_item(+ItemName, +Subitems, -PhraseItem)
   get_subitems_feature(+Subitems, +Feature, -FeatureValue)
   get_subitem_name(+Subitem, -Name)
   get_subitem_value(+Subitem, -Value)
   set_phrase_item_feature(+PhraseItem, +Feature, +FeatureValue,
                           -ModifiedPhraseItem)
   set_subitems_feature(+Subitems, +Feature, +FeatureValue, -ModifiedSubitems)

get_phrase_item_feature/3
get_phrase_item_name/2
get_phrase_item_subitems/2
new_phrase_item/3
get_subitems_feature/3
get_subitem_name/2
get_subitem_value/2
set_phrase_item_feature/4
set_subitems_feature/4
*/

get_phrase_item_feature(PhraseItem, Feature, FeatureValue) :-
	get_phrase_item_subitems(PhraseItem, Subitems),
	get_subitems_feature(Subitems, Feature, FeatureValue).

get_phrase_item_name(PhraseItem, ItemName) :-
	functor(PhraseItem, ItemName, 1).

get_phrase_item_subitems(PhraseItem, Subitems) :-
	arg(1, PhraseItem, Subitems).

new_phrase_item(ItemName,Subitems,PhraseItem) :-
	functor(PhraseItem, ItemName, 1),
	arg(1, PhraseItem, Subitems).

get_subitems_feature([], _, []).  % return [] for non-existing feature
get_subitems_feature([First|_Rest], Feature, FeatureValue) :-
	get_subitem_name(First, Feature),
	!,
	get_subitem_value(First, FeatureValue).
get_subitems_feature([_First|Rest], Feature, FeatureValue) :-
	get_subitems_feature(Rest, Feature, FeatureValue).

get_subitem_name(Subitem, Name) :-
	functor(Subitem, Name, 1).

get_subitem_value(Subitem, Value) :-
	arg(1, Subitem, Value).

set_phrase_item_feature(PhraseItem, Feature, FeatureValue, ModifiedPhraseItem) :-
	get_phrase_item_name(PhraseItem, ItemName),
	get_phrase_item_subitems(PhraseItem, Subitems),
	set_subitems_feature(Subitems, Feature, FeatureValue, ModifiedSubitems),
	new_phrase_item(ItemName, ModifiedSubitems, ModifiedPhraseItem).

set_subitems_feature([], Feature, FeatureValue, [NewSubitem]) :-
	functor(NewSubitem, Feature, 1),
	arg(1, NewSubitem, FeatureValue).
set_subitems_feature([First|Rest], Feature, FeatureValue, [NewSubitem|Rest]) :-
	functor(First, Feature, 1),
	!,
	functor(NewSubitem, Feature, 1),
	arg(1, NewSubitem, FeatureValue).
set_subitems_feature([First|Rest], Feature, FeatureValue, [First|ModifiedRest]) :-
	set_subitems_feature(Rest, Feature, FeatureValue, ModifiedRest).

local_ws( 9).
local_ws(10).
local_ws(11).
local_ws(12).
local_ws(13).
local_ws(31).
local_ws(32).

local_digit(48). % 0
local_digit(49). % 1
local_digit(50). % 2
local_digit(51). % 3
local_digit(52). % 4
local_digit(53). % 5
local_digit(54). % 6
local_digit(55). % 7
local_digit(56). % 8
local_digit(57). % 9

local_punct(33).  % !
local_punct(34).  % "
local_punct(35).  % #
local_punct(36).  % $
local_punct(37).  %  %
local_punct(38).  % &
local_punct(39).  % '
local_punct(40).  % (
local_punct(41).  % )
local_punct(42).  % *
local_punct(43).  % +
local_punct(44).  % ,
local_punct(45).  % -
local_punct(46).  % .
local_punct(47).  % /
local_punct(58).  % % 
local_punct(59).  % ;
local_punct(60).  % <
local_punct(61).  % =
local_punct(62).  % >
local_punct(63).  % ?
local_punct(64).  % @
local_punct(91).  % [
local_punct(92).  % \
local_punct(93).  % ]
local_punct(94).  % ^
local_punct(95).  % _
local_punct(96).  % `
local_punct(123). % {
local_punct(124). % |
local_punct(125). % }
local_punct(126). %  ~

% local_alnum(39).  % '
local_alnum(48).  % 0
local_alnum(49).  % 1
local_alnum(50).  % 2
local_alnum(51).  % 3
local_alnum(52).  % 4
local_alnum(53).  % 5
local_alnum(54).  % 6
local_alnum(55).  % 7
local_alnum(56).  % 8
local_alnum(57).  % 9
local_alnum(65).  % A
local_alnum(66).  % B
local_alnum(67).  % C
local_alnum(68).  % D
local_alnum(69).  % E
local_alnum(70).  % F
local_alnum(71).  % G
local_alnum(72).  % H
local_alnum(73).  % I
local_alnum(74).  % J
local_alnum(75).  % K
local_alnum(76).  % L
local_alnum(77).  % M
local_alnum(78).  % N
local_alnum(79).  % O
local_alnum(80).  % P
local_alnum(81).  % Q
local_alnum(82).  % R
local_alnum(83).  % S
local_alnum(84).  % T
local_alnum(85).  % U
local_alnum(86).  % V
local_alnum(87).  % W
local_alnum(88).  % X
local_alnum(89).  % Y
local_alnum(90).  % Z
local_alnum(97).  % a
local_alnum(98).  % b
local_alnum(99).  % c
local_alnum(100). % d
local_alnum(101). % e
local_alnum(102). % f
local_alnum(103). % g
local_alnum(104). % h
local_alnum(105). % i
local_alnum(106). % j
local_alnum(107). % k
local_alnum(108). % l
local_alnum(109). % m
local_alnum(110). % n
local_alnum(111). % o
local_alnum(112). % p
local_alnum(113). % q
local_alnum(114). % r
local_alnum(115). % s
local_alnum(116). % t
local_alnum(117). % u
local_alnum(118). % v
local_alnum(119). % w
local_alnum(120). % x
local_alnum(121). % y
local_alnum(122). % z


local_alpha(65).  % A
local_alpha(66).  % B
local_alpha(67).  % C
local_alpha(68).  % D
local_alpha(69).  % E
local_alpha(70).  % F
local_alpha(71).  % G
local_alpha(72).  % H
local_alpha(73).  % I
local_alpha(74).  % J
local_alpha(75).  % K
local_alpha(76).  % L
local_alpha(77).  % M
local_alpha(78).  % N
local_alpha(79).  % O
local_alpha(80).  % P
local_alpha(81).  % Q
local_alpha(82).  % R
local_alpha(83).  % S
local_alpha(84).  % T
local_alpha(85).  % U
local_alpha(86).  % V
local_alpha(87).  % W
local_alpha(88).  % X
local_alpha(89).  % Y
local_alpha(90).  % Z
local_alpha(97).  % a
local_alpha(98).  % b
local_alpha(99).  % c
local_alpha(100). % d
local_alpha(101). % e
local_alpha(102). % f
local_alpha(103). % g
local_alpha(104). % h
local_alpha(105). % i
local_alpha(106). % j
local_alpha(107). % k
local_alpha(108). % l
local_alpha(109). % m
local_alpha(110). % n
local_alpha(111). % o
local_alpha(112). % p
local_alpha(113). % q
local_alpha(114). % r
local_alpha(115). % s
local_alpha(116). % t
local_alpha(117). % u
local_alpha(118). % v
local_alpha(119). % w
local_alpha(120). % x
local_alpha(121). % y
local_alpha(122). % z

local_upper(65).  % A
local_upper(66).  % B
local_upper(67).  % C
local_upper(68).  % D
local_upper(69).  % E
local_upper(70).  % F
local_upper(71).  % G
local_upper(72).  % H
local_upper(73).  % I
local_upper(74).  % J
local_upper(75).  % K
local_upper(76).  % L
local_upper(77).  % M
local_upper(78).  % N
local_upper(79).  % O
local_upper(80).  % P
local_upper(81).  % Q
local_upper(82).  % R
local_upper(83).  % S
local_upper(84).  % T
local_upper(85).  % U
local_upper(86).  % V
local_upper(87).  % W
local_upper(88).  % X
local_upper(89).  % Y
local_upper(90).  % Z

local_lower(97).  % a
local_lower(98).  % b
local_lower(99).  % c
local_lower(100). % d
local_lower(101). % e
local_lower(102). % f
local_lower(103). % g
local_lower(104). % h
local_lower(105). % i
local_lower(106). % j
local_lower(107). % k
local_lower(108). % l
local_lower(109). % m
local_lower(110). % n
local_lower(111). % o
local_lower(112). % p
local_lower(113). % q
local_lower(114). % r
local_lower(115). % s
local_lower(116). % t
local_lower(117). % u
local_lower(118). % v
local_lower(119). % w
local_lower(120). % x
local_lower(121). % y
local_lower(122). % z

local_ascii(0).
local_ascii(1).
local_ascii(2).
local_ascii(3).
local_ascii(4).
local_ascii(5).
local_ascii(6).
local_ascii(7).
local_ascii(8).
local_ascii(9).
local_ascii(10).
local_ascii(11).
local_ascii(12).
local_ascii(13).
local_ascii(14).
local_ascii(15).
local_ascii(16).
local_ascii(17).
local_ascii(18).
local_ascii(19).
local_ascii(20).
local_ascii(21).
local_ascii(22).
local_ascii(23).
local_ascii(24).
local_ascii(25).
local_ascii(26).
local_ascii(27).
local_ascii(28).
local_ascii(29).
local_ascii(30).
local_ascii(31).
local_ascii(32).
local_ascii(33).
local_ascii(34).
local_ascii(35).
local_ascii(36).
local_ascii(37).
local_ascii(38).
local_ascii(39).
local_ascii(40).
local_ascii(41).
local_ascii(42).
local_ascii(43).
local_ascii(44).
local_ascii(45).
local_ascii(46).
local_ascii(47).
local_ascii(48).
local_ascii(49).
local_ascii(50).
local_ascii(51).
local_ascii(52).
local_ascii(53).
local_ascii(54).
local_ascii(55).
local_ascii(56).
local_ascii(57).
local_ascii(58).
local_ascii(59).
local_ascii(60).
local_ascii(61).
local_ascii(62).
local_ascii(63).
local_ascii(64).
local_ascii(65).
local_ascii(66).
local_ascii(67).
local_ascii(68).
local_ascii(69).
local_ascii(70).
local_ascii(71).
local_ascii(72).
local_ascii(73).
local_ascii(74).
local_ascii(75).
local_ascii(76).
local_ascii(77).
local_ascii(78).
local_ascii(79).
local_ascii(80).
local_ascii(81).
local_ascii(82).
local_ascii(83).
local_ascii(84).
local_ascii(85).
local_ascii(86).
local_ascii(87).
local_ascii(88).
local_ascii(89).
local_ascii(90).
local_ascii(91).
local_ascii(92).
local_ascii(93).
local_ascii(94).
local_ascii(95).
local_ascii(96).
local_ascii(97).
local_ascii(98).
local_ascii(99).
local_ascii(100).
local_ascii(101).
local_ascii(102).
local_ascii(103).
local_ascii(104).
local_ascii(105).
local_ascii(106).
local_ascii(107).
local_ascii(108).
local_ascii(109).
local_ascii(110).
local_ascii(111).
local_ascii(112).
local_ascii(113).
local_ascii(114).
local_ascii(115).
local_ascii(116).
local_ascii(117).
local_ascii(118).
local_ascii(119).
local_ascii(120).
local_ascii(121).
local_ascii(122).
local_ascii(123).
local_ascii(124).
local_ascii(125).
local_ascii(126).
local_ascii(127).

local_print(32).
local_print(33).
local_print(34).
local_print(35).
local_print(36).
local_print(37).
local_print(38).
local_print(39).
local_print(40).
local_print(41).
local_print(42).
local_print(43).
local_print(44).
local_print(45).
local_print(46).
local_print(47).
local_print(48).
local_print(49).
local_print(50).
local_print(51).
local_print(52).
local_print(53).
local_print(54).
local_print(55).
local_print(56).
local_print(57).
local_print(58).
local_print(59).
local_print(60).
local_print(61).
local_print(62).
local_print(63).
local_print(64).
local_print(65).
local_print(66).
local_print(67).
local_print(68).
local_print(69).
local_print(70).
local_print(71).
local_print(72).
local_print(73).
local_print(74).
local_print(75).
local_print(76).
local_print(77).
local_print(78).
local_print(79).
local_print(80).
local_print(81).
local_print(82).
local_print(83).
local_print(84).
local_print(85).
local_print(86).
local_print(87).
local_print(88).
local_print(89).
local_print(90).
local_print(91).
local_print(92).
local_print(93).
local_print(94).
local_print(95).
local_print(96).
local_print(97).
local_print(98).
local_print(99).
local_print(100).
local_print(101).
local_print(102).
local_print(103).
local_print(104).
local_print(105).
local_print(106).
local_print(107).
local_print(108).
local_print(109).
local_print(110).
local_print(111).
local_print(112).
local_print(113).
local_print(114).
local_print(115).
local_print(116).
local_print(117).
local_print(118).
local_print(119).
local_print(120).
local_print(121).
local_print(122).
local_print(123).
local_print(124).
local_print(125).
local_print(126).
local_print(161).
local_print(162).
local_print(163).
local_print(164).
local_print(165).
local_print(166).
local_print(167).
local_print(168).
local_print(169).
local_print(170).
local_print(171).
local_print(172).
local_print(173).
local_print(174).
local_print(175).
local_print(176).
local_print(177).
local_print(178).
local_print(179).
local_print(180).
local_print(181).
local_print(182).
local_print(183).
local_print(184).
local_print(185).
local_print(186).
local_print(187).
local_print(188).
local_print(189).
local_print(190).
local_print(191).
local_print(192).
local_print(193).
local_print(194).
local_print(195).
local_print(196).
local_print(197).
local_print(198).
local_print(199).
local_print(200).
local_print(201).
local_print(202).
local_print(203).
local_print(204).
local_print(205).
local_print(206).
local_print(207).
local_print(208).
local_print(209).
local_print(210).
local_print(211).
local_print(212).
local_print(213).
local_print(214).
local_print(215).
local_print(216).
local_print(217).
local_print(218).
local_print(219).
local_print(220).
local_print(221).
local_print(222).
local_print(223).
local_print(224).
local_print(225).
local_print(226).
local_print(227).
local_print(228).
local_print(229).
local_print(230).
local_print(231).
local_print(232).
local_print(233).
local_print(234).
local_print(235).
local_print(236).
local_print(237).
local_print(238).
local_print(239).
local_print(240).
local_print(241).
local_print(242).
local_print(243).
local_print(244).
local_print(245).
local_print(246).
local_print(247).
local_print(248).
local_print(249).
local_print(250).
local_print(251).
local_print(252).
local_print(253).
local_print(254).
local_print(255).

local_to_lower(0,0).
local_to_lower(1,1).
local_to_lower(2,2).
local_to_lower(3,3).
local_to_lower(4,4).
local_to_lower(5,5).
local_to_lower(6,6).
local_to_lower(7,7).
local_to_lower(8,8).
local_to_lower(9,9).
local_to_lower(10,10).
local_to_lower(11,11).
local_to_lower(12,12).
local_to_lower(13,13).
local_to_lower(14,14).
local_to_lower(15,15).
local_to_lower(16,16).
local_to_lower(17,17).
local_to_lower(18,18).
local_to_lower(19,19).
local_to_lower(20,20).
local_to_lower(21,21).
local_to_lower(22,22).
local_to_lower(23,23).
local_to_lower(24,24).
local_to_lower(25,25).
local_to_lower(26,26).
local_to_lower(27,27).
local_to_lower(28,28).
local_to_lower(29,29).
local_to_lower(30,30).
local_to_lower(31,31).
local_to_lower(32,32).
local_to_lower(33,33).
local_to_lower(34,34).
local_to_lower(35,35).
local_to_lower(36,36).
local_to_lower(37,37).
local_to_lower(38,38).
local_to_lower(39,39).
local_to_lower(40,40).
local_to_lower(41,41).
local_to_lower(42,42).
local_to_lower(43,43).
local_to_lower(44,44).
local_to_lower(45,45).
local_to_lower(46,46).
local_to_lower(47,47).
local_to_lower(48,48).
local_to_lower(49,49).
local_to_lower(50,50).
local_to_lower(51,51).
local_to_lower(52,52).
local_to_lower(53,53).
local_to_lower(54,54).
local_to_lower(55,55).
local_to_lower(56,56).
local_to_lower(57,57).
local_to_lower(58,58).
local_to_lower(59,59).
local_to_lower(60,60).
local_to_lower(61,61).
local_to_lower(62,62).
local_to_lower(63,63).
local_to_lower(64,64).
local_to_lower(65,97).
local_to_lower(66,98).
local_to_lower(67,99).
local_to_lower(68,100).
local_to_lower(69,101).
local_to_lower(70,102).
local_to_lower(71,103).
local_to_lower(72,104).
local_to_lower(73,105).
local_to_lower(74,106).
local_to_lower(75,107).
local_to_lower(76,108).
local_to_lower(77,109).
local_to_lower(78,110).
local_to_lower(79,111).
local_to_lower(80,112).
local_to_lower(81,113).
local_to_lower(82,114).
local_to_lower(83,115).
local_to_lower(84,116).
local_to_lower(85,117).
local_to_lower(86,118).
local_to_lower(87,119).
local_to_lower(88,120).
local_to_lower(89,121).
local_to_lower(90,122).
local_to_lower(91,91).
local_to_lower(92,92).
local_to_lower(93,93).
local_to_lower(94,94).
local_to_lower(95,95).
local_to_lower(96,96).
local_to_lower(97,97).
local_to_lower(98,98).
local_to_lower(99,99).
local_to_lower(100,100).
local_to_lower(101,101).
local_to_lower(102,102).
local_to_lower(103,103).
local_to_lower(104,104).
local_to_lower(105,105).
local_to_lower(106,106).
local_to_lower(107,107).
local_to_lower(108,108).
local_to_lower(109,109).
local_to_lower(110,110).
local_to_lower(111,111).
local_to_lower(112,112).
local_to_lower(113,113).
local_to_lower(114,114).
local_to_lower(115,115).
local_to_lower(116,116).
local_to_lower(117,117).
local_to_lower(118,118).
local_to_lower(119,119).
local_to_lower(120,120).
local_to_lower(121,121).
local_to_lower(122,122).
local_to_lower(123,123).
local_to_lower(124,124).
local_to_lower(125,125).
local_to_lower(126,126).
local_to_lower(127,127).
local_to_lower(128,128).
local_to_lower(129,129).
local_to_lower(130,130).
local_to_lower(131,131).
local_to_lower(132,132).
local_to_lower(133,133).
local_to_lower(134,134).
local_to_lower(135,135).
local_to_lower(136,136).
local_to_lower(137,137).
local_to_lower(138,138).
local_to_lower(139,139).
local_to_lower(140,140).
local_to_lower(141,141).
local_to_lower(142,142).
local_to_lower(143,143).
local_to_lower(144,144).
local_to_lower(145,145).
local_to_lower(146,146).
local_to_lower(147,147).
local_to_lower(148,148).
local_to_lower(149,149).
local_to_lower(150,150).
local_to_lower(151,151).
local_to_lower(152,152).
local_to_lower(153,153).
local_to_lower(154,154).
local_to_lower(155,155).
local_to_lower(156,156).
local_to_lower(157,157).
local_to_lower(158,158).
local_to_lower(159,159).
local_to_lower(160,160).
local_to_lower(161,161).
local_to_lower(162,162).
local_to_lower(163,163).
local_to_lower(164,164).
local_to_lower(165,165).
local_to_lower(166,166).
local_to_lower(167,167).
local_to_lower(168,168).
local_to_lower(169,169).
local_to_lower(170,170).
local_to_lower(171,171).
local_to_lower(172,172).
local_to_lower(173,173).
local_to_lower(174,174).
local_to_lower(175,175).
local_to_lower(176,176).
local_to_lower(177,177).
local_to_lower(178,178).
local_to_lower(179,179).
local_to_lower(180,180).
local_to_lower(181,181).
local_to_lower(182,182).
local_to_lower(183,183).
local_to_lower(184,184).
local_to_lower(185,185).
local_to_lower(186,186).
local_to_lower(187,187).
local_to_lower(188,188).
local_to_lower(189,189).
local_to_lower(190,190).
local_to_lower(191,191).
local_to_lower(192,224).
local_to_lower(193,225).
local_to_lower(194,226).
local_to_lower(195,227).
local_to_lower(196,228).
local_to_lower(197,229).
local_to_lower(198,230).
local_to_lower(199,231).
local_to_lower(200,232).
local_to_lower(201,233).
local_to_lower(202,234).
local_to_lower(203,235).
local_to_lower(204,236).
local_to_lower(205,237).
local_to_lower(206,238).
local_to_lower(207,239).
local_to_lower(208,240).
local_to_lower(209,241).
local_to_lower(210,242).
local_to_lower(211,243).
local_to_lower(212,244).
local_to_lower(213,245).
local_to_lower(214,246).
local_to_lower(215,247).
local_to_lower(216,248).
local_to_lower(217,249).
local_to_lower(218,250).
local_to_lower(219,251).
local_to_lower(220,252).
local_to_lower(221,253).
local_to_lower(222,254).
local_to_lower(223,223).
local_to_lower(224,224).
local_to_lower(225,225).
local_to_lower(226,226).
local_to_lower(227,227).
local_to_lower(228,228).
local_to_lower(229,229).
local_to_lower(230,230).
local_to_lower(231,231).
local_to_lower(232,232).
local_to_lower(233,233).
local_to_lower(234,234).
local_to_lower(235,235).
local_to_lower(236,236).
local_to_lower(237,237).
local_to_lower(238,238).
local_to_lower(239,239).
local_to_lower(240,240).
local_to_lower(241,241).
local_to_lower(242,242).
local_to_lower(243,243).
local_to_lower(244,244).
local_to_lower(245,245).
local_to_lower(246,246).
local_to_lower(247,247).
local_to_lower(248,248).
local_to_lower(249,249).
local_to_lower(250,250).
local_to_lower(251,251).
local_to_lower(252,252).
local_to_lower(253,253).
local_to_lower(254,254).
local_to_lower(255,255).

local_to_upper(0,0).
local_to_upper(1,1).
local_to_upper(2,2).
local_to_upper(3,3).
local_to_upper(4,4).
local_to_upper(5,5).
local_to_upper(6,6).
local_to_upper(7,7).
local_to_upper(8,8).
local_to_upper(9,9).
local_to_upper(10,10).
local_to_upper(11,11).
local_to_upper(12,12).
local_to_upper(13,13).
local_to_upper(14,14).
local_to_upper(15,15).
local_to_upper(16,16).
local_to_upper(17,17).
local_to_upper(18,18).
local_to_upper(19,19).
local_to_upper(20,20).
local_to_upper(21,21).
local_to_upper(22,22).
local_to_upper(23,23).
local_to_upper(24,24).
local_to_upper(25,25).
local_to_upper(26,26).
local_to_upper(27,27).
local_to_upper(28,28).
local_to_upper(29,29).
local_to_upper(30,30).
local_to_upper(31,31).
local_to_upper(32,32).
local_to_upper(33,33).
local_to_upper(34,34).
local_to_upper(35,35).
local_to_upper(36,36).
local_to_upper(37,37).
local_to_upper(38,38).
local_to_upper(39,39).
local_to_upper(40,40).
local_to_upper(41,41).
local_to_upper(42,42).
local_to_upper(43,43).
local_to_upper(44,44).
local_to_upper(45,45).
local_to_upper(46,46).
local_to_upper(47,47).
local_to_upper(48,48).
local_to_upper(49,49).
local_to_upper(50,50).
local_to_upper(51,51).
local_to_upper(52,52).
local_to_upper(53,53).
local_to_upper(54,54).
local_to_upper(55,55).
local_to_upper(56,56).
local_to_upper(57,57).
local_to_upper(58,58).
local_to_upper(59,59).
local_to_upper(60,60).
local_to_upper(61,61).
local_to_upper(62,62).
local_to_upper(63,63).
local_to_upper(64,64).
local_to_upper(65,65).
local_to_upper(66,66).
local_to_upper(67,67).
local_to_upper(68,68).
local_to_upper(69,69).
local_to_upper(70,70).
local_to_upper(71,71).
local_to_upper(72,72).
local_to_upper(73,73).
local_to_upper(74,74).
local_to_upper(75,75).
local_to_upper(76,76).
local_to_upper(77,77).
local_to_upper(78,78).
local_to_upper(79,79).
local_to_upper(80,80).
local_to_upper(81,81).
local_to_upper(82,82).
local_to_upper(83,83).
local_to_upper(84,84).
local_to_upper(85,85).
local_to_upper(86,86).
local_to_upper(87,87).
local_to_upper(88,88).
local_to_upper(89,89).
local_to_upper(90,90).
local_to_upper(91,91).
local_to_upper(92,92).
local_to_upper(93,93).
local_to_upper(94,94).
local_to_upper(95,95).
local_to_upper(96,96).
local_to_upper(97,65).
local_to_upper(98,66).
local_to_upper(99,67).
local_to_upper(100,68).
local_to_upper(101,69).
local_to_upper(102,70).
local_to_upper(103,71).
local_to_upper(104,72).
local_to_upper(105,73).
local_to_upper(106,74).
local_to_upper(107,75).
local_to_upper(108,76).
local_to_upper(109,77).
local_to_upper(110,78).
local_to_upper(111,79).
local_to_upper(112,80).
local_to_upper(113,81).
local_to_upper(114,82).
local_to_upper(115,83).
local_to_upper(116,84).
local_to_upper(117,85).
local_to_upper(118,86).
local_to_upper(119,87).
local_to_upper(120,88).
local_to_upper(121,89).
local_to_upper(122,90).
local_to_upper(123,123).
local_to_upper(124,124).
local_to_upper(125,125).
local_to_upper(126,126).
local_to_upper(127,127).
local_to_upper(128,128).
local_to_upper(129,129).
local_to_upper(130,130).
local_to_upper(131,131).
local_to_upper(132,132).
local_to_upper(133,133).
local_to_upper(134,134).
local_to_upper(135,135).
local_to_upper(136,136).
local_to_upper(137,137).
local_to_upper(138,138).
local_to_upper(139,139).
local_to_upper(140,140).
local_to_upper(141,141).
local_to_upper(142,142).
local_to_upper(143,143).
local_to_upper(144,144).
local_to_upper(145,145).
local_to_upper(146,146).
local_to_upper(147,147).
local_to_upper(148,148).
local_to_upper(149,149).
local_to_upper(150,150).
local_to_upper(151,151).
local_to_upper(152,152).
local_to_upper(153,153).
local_to_upper(154,154).
local_to_upper(155,155).
local_to_upper(156,156).
local_to_upper(157,157).
local_to_upper(158,158).
local_to_upper(159,159).
local_to_upper(160,160).
local_to_upper(161,161).
local_to_upper(162,162).
local_to_upper(163,163).
local_to_upper(164,164).
local_to_upper(165,165).
local_to_upper(166,166).
local_to_upper(167,167).
local_to_upper(168,168).
local_to_upper(169,169).
local_to_upper(170,170).
local_to_upper(171,171).
local_to_upper(172,172).
local_to_upper(173,173).
local_to_upper(174,174).
local_to_upper(175,175).
local_to_upper(176,176).
local_to_upper(177,177).
local_to_upper(178,178).
local_to_upper(179,179).
local_to_upper(180,180).
local_to_upper(181,181).
local_to_upper(182,182).
local_to_upper(183,183).
local_to_upper(184,184).
local_to_upper(185,185).
local_to_upper(186,186).
local_to_upper(187,187).
local_to_upper(188,188).
local_to_upper(189,189).
local_to_upper(190,190).
local_to_upper(191,191).
local_to_upper(192,192).
local_to_upper(193,193).
local_to_upper(194,194).
local_to_upper(195,195).
local_to_upper(196,196).
local_to_upper(197,197).
local_to_upper(198,198).
local_to_upper(199,199).
local_to_upper(200,200).
local_to_upper(201,201).
local_to_upper(202,202).
local_to_upper(203,203).
local_to_upper(204,204).
local_to_upper(205,205).
local_to_upper(206,206).
local_to_upper(207,207).
local_to_upper(208,208).
local_to_upper(209,209).
local_to_upper(210,210).
local_to_upper(211,211).
local_to_upper(212,212).
local_to_upper(213,213).
local_to_upper(214,214).
local_to_upper(215,215).
local_to_upper(216,216).
local_to_upper(217,217).
local_to_upper(218,218).
local_to_upper(219,219).
local_to_upper(220,220).
local_to_upper(221,221).
local_to_upper(222,222).
local_to_upper(223,223).
local_to_upper(224,192).
local_to_upper(225,193).
local_to_upper(226,194).
local_to_upper(227,195).
local_to_upper(228,196).
local_to_upper(229,197).
local_to_upper(230,198).
local_to_upper(231,199).
local_to_upper(232,200).
local_to_upper(233,201).
local_to_upper(234,202).
local_to_upper(235,203).
local_to_upper(236,204).
local_to_upper(237,205).
local_to_upper(238,206).
local_to_upper(239,207).
local_to_upper(240,208).
local_to_upper(241,209).
local_to_upper(242,210).
local_to_upper(243,211).
local_to_upper(244,212).
local_to_upper(245,213).
local_to_upper(246,214).
local_to_upper(247,215).
local_to_upper(248,216).
local_to_upper(249,217).
local_to_upper(250,218).
local_to_upper(251,219).
local_to_upper(252,220).
local_to_upper(253,221).
local_to_upper(254,222).
local_to_upper(255,255).
