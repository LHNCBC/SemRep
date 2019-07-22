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

% File:	    full_fielded_output.pl
% Module:   full_fielded_output
% Author:   FML
% Purpose:  predicates for full fielded output


% ----- Module declaration and exported predicates

:- module(full_fielded_output, [
	complex_token_type/1,
%	format_one_gendis_term/9,
% 	format_one_predication_full_fielded/10,
	get_genphenoms_from_tree/3,
	get_metaconcs_from_tree/4
%	semantics_tree_charpos/7,
%	syntax_tree_charpos/13,
%	write_full_fielded_output/12,
%	write_token_list/3
   ]).

:- load_files( usemrep_lib(module_version),
	       [ when(compile_time) ] ).

% ----- Imported predicates

% :- use_module( usemrep_lib(portray_minimal_syntax), [
% 		portray_minimal_syntax_structure/1
%    ]).

% :- use_module( usemrep_lib(write_results), [
%                 write_all_predications/10
%    ]).

% :- use_module( usemrep_lib(ssuppserv), [
%		add_indexes_to_all_MSUs/3,
%		add_msu_index/3,
%		atomize/2,
%		assemble_format_control_string/2,
% 		convert_dash_structure_to_list/2,
%	        convert_genphenom_function_list/2,
%		get_all_concept_cuis/2,
%		get_augmented_concept_cui/2,
%		get_entity_atom/5,
%		get_index_start_and_end_pos/4,
%		get_matching_genphenom/5,
%		get_print_semtype_list/2,
% 		min_max/4,
% 		remove_variables/3,
%		set_genphenom_or_disorder_fields/8,
% 		update_entity_fields/15,
%		split_input_label/4,
% 		write_list_with_format/2
%    ]).

% :- use_module( usemrep_lib(wrappers), [
% 		format_one_gendis_term_wrapper/12,
% 		semantics_tree_charpos_wrapper/10,
% 		syntax_tree_charpos_wrapper/14,
% 		format_one_predication_full_fielded_wrapper/13
%    ]).

:- use_module( skr_lib(nls_lists), [
		get_from_list/3
   ]).

% :- use_module( skr_lib(nls_strings), [
% 		split_string_completely/3
%    ]).
 
% :- use_module( skr_lib(nls_system), [
% 		control_option/1
%    ]).

% :- use_module( skr_db(db_access), [
% 		db_get_concept_cui/2
%    ]).

% :- use_module( library(sets), [
%                 intersect/2
%    ]).

% :- use_module( skr_lib(sicstus_utils), [
% 		concat_atom/2,
% 		concat_atom/3,
%		index/3,
% 		midstring/5,
% 		substring/4
%    ]).

% :- use_module( library(lists), [
%		keys_and_values/3,
% 		is_list/1,
%		last/2,
% 		select/3,
% 		selectchk/3,
% 		prefix/2,
% 		rev/2,
% 		reverse/2
%    ]).

% :- use_module( skr_lib(ctypes), [
% 		is_ascii/1
%    ]).


% write_full_fielded_output(OutputStream, CharOffset, Domain,
% 		     InputLabel, AAs, TokenListThisUtterance,
% 		     ChosenInputText, UNExpandedInputText,
% 		     UtteranceType,
% 		     minimal_syntax(SyntacticAnalysis),
% 		     minimal_syntax(SemanticAnalysis),
% 		     Predications) :-
% 	add_indexes_to_all_MSUs(SyntacticAnalysis, 0, IndexedSyntacticAnalysis),
% 	add_msu_index(SemanticAnalysis, 0, SemanticAnalysisWithMSUIndex),
% 	% split InputLabel (e.g., 15289345.ab.5) into its three components
% 	split_input_label(InputLabel, PMID, TiOrAb, SentenceID),
% 
% 	% MetaConcs is a list of terms of the form
% 	% (ConceptName:CUI:SemTypeList)-IndexNum-ConfidenceScore
% 	get_genphenoms_from_tree(SemanticAnalysis, GenPhenomList, []),
% 	get_metaconcs_from_tree(SemanticAnalysis, GenPhenomList, MetaConcList, []),
% 
% 	/*
% 	display_preliminary_info(InputLabel,
% 				 UNExpandedInputText, ChosenInputText,
% 				 IndexedSyntacticAnalysis, SemanticAnalysis,
% 				 MetaConcList, Predications,
% 				 TokenListThisUtterance, CharOffset),
% 	*/
% 
% 	AADefFoundIn = 0,
% 
% 	% IndexCharPositionList is a list of terms of the form
% 	% IndexID-InputMatchList-CharStartPos-CharEndPos
% 	% for each indexed structure in the syntax tree.
% 	% To generate these structures, use inputmatch words from Syntax tree
% 	% to capture original word order,
% 	% but index(_) terms from Semantics tree
% 	% Do this in two phases:
% 	% (1) syntax_tree_charpos_wrapper/15
% 	%     uses Syntax tree and tok(...) terms to associate
% 	%     a pos(Start,End) term with each inputmatch word, THEN
% 	% (2) semantics_tree_charpos/5
% 	%     sweeps through Semantics tree with these Pos terms
% 
% 	syntax_tree_charpos_wrapper(IndexedSyntacticAnalysis,
% 				    SemanticAnalysis,
% 				    CharOffset,
% 				    OutputStream, InputLabel,
% 				    AADefFoundIn, _AADefFoundOut,
% 				    ChosenInputText, UNExpandedInputText,
% 				    AAs,
% 				    TokenListThisUtterance,
% 				    _TokenListOut,
% 				    WordCharPositionList, []), %%% NEW
% 	semantics_tree_charpos_wrapper(SemanticAnalysis,
% 				       TokenListThisUtterance,
% 				       ChosenInputText, UNExpandedInputText,
% 				       OutputStream, InputLabel,
% 				       WordCharPositionList,
% 				       _WordCharPositionListOut,
% 				       IndexCharPositionList,
% 				       IndexCharPositionList),
%         update_predications(Predications, SemanticAnalysis, 
% 	                    SemanticAnalysisWithMSUIndex, 
% 	                    UpdatedPredications),
% 	format_all_predications_full_fielded(UpdatedPredications, Domain,
% 					     ChosenInputText, OutputStream,
% 					     MetaConcList, GenPhenomList,
% 					     IndexCharPositionList, UNExpandedInputText,
% 					     PMID, TiOrAb, SentenceID, UtteranceType,
% 					     FullFieldedPredications),
% 	atom_codes(InputTextAtom, UNExpandedInputText),
% 	SentencePredication = '1text'-(UtteranceType-TiOrAb-SentenceID-text-InputTextAtom),
% % Second MetaConcList will help in eliminating some of the Entrezgene concepts.	
% 	format_full_fielded_entity_list(MetaConcList, MetaConcList, GenPhenomList,
% 					IndexCharPositionList,
% 					UNExpandedInputText,
% 					TiOrAb, SentenceID, UtteranceType,
% 				        % FullFieldedEntityList is the entire list;
% 				        % its tail is [SentencePredication|FullFieldedPredications],
% 				        % which is instantiated when the recursion terminates
% 					FullFieldedEntityList,
% 					[SentencePredication|FullFieldedPredications]),
%         keysort(FullFieldedEntityList, TempSortedFullFieldedEntityList),
% 	keys_and_values(TempSortedFullFieldedEntityList, _Keys, SortedFullFieldedEntityList),
% 	determine_domain_indicator(Domain, DomainIndicator),
% 	FullFielded = 1,
% 	% In full fielded output, the DomainIndicator takes the place of the Utterance ID
% 	write_all_predications(SortedFullFieldedEntityList, Domain, FullFielded,
% 	                       GenPhenomList, IndexCharPositionList, _CitationTextAtom,UNExpandedInputText,
% 			       OutputStream, DomainIndicator, PMID),
% 	flush_output(OutputStream).

% determine_domain_indicator(Domain, Indicator) :-
% 	( Domain == genetics ->
% 	  Indicator = 'SG'
% 	; Indicator = 'SE'
% 	).       

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  BEGIN Full Fielded DISPLAY Predicates %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% display_preliminary_info(InputLabel,
% 			 UNExpandedInputText, ChosenInputText,
% 			 IndexedSyntacticAnalysis, SemanticAnalysis,
% 			 MetaConcList, Predications,
% 			 TokenListThisUtterance, CharOffset) :-
% 
% 	format('~n#### SYNTAX:~n', []),
% 	portray_minimal_syntax_structure(IndexedSyntacticAnalysis),
% 	format('~N#### SEMANTICS:~n', []),
% 	portray_minimal_syntax_structure(SemanticAnalysis),
% 	flag_expanded_difference(InputLabel, UNExpandedInputText, ChosenInputText),
% 	format('~nList of MetaConc-Index-ConfidenceScore:~n',   []),
% 	write_list_with_format(MetaConcList, '~w~n'),
% 	format('~nSortedPredications:~n',   []),
%  	sort(Predications, SortedPredications),
% 	write_list_with_format(SortedPredications, '~w~n'),
% 	nl, nl,
% 	format('~nTokens:~n', []),
% 	write_token_list(user_output, TokenListThisUtterance, CharOffset),
% 	flag_expanded_difference(InputLabel, UNExpandedInputText, ChosenInputText),
% 	nl.

% flag_expanded_difference(InputLabel, UNExpandedInputText, ChosenInputText) :-
% 	( UNExpandedInputText = ChosenInputText ->
% 	  format('~n~n~*c~n~w SAME UTTERANCES~n', [80,35,InputLabel])
% 	; format('~n~n~*c~n~w~nUN: ~s~nEX: ~s~n~n',
% 		 [80,35,InputLabel,UNExpandedInputText, ChosenInputText])
% 	).

% write_word_char_position_list([], _PrevStartPos).
% write_word_char_position_list([H|T], PrevStartPos) :-
% 	write_one_word_char_position_term(H, PrevStartPos, NewStartPos),
% 	write_word_char_position_list(T, NewStartPos).

% write_one_word_char_position_term(Word-StartPos-EndPos, PrevStartPos, StartPos) :-
% 	format('~w-~w-~w', [Word,StartPos,EndPos]),
% 	( StartPos >= EndPos ->
% 	  format('~n~n@@@@@@@@@@ OOPS! @@@@@@@@@@~n~n', [])
% 	% fail
% 	; EndPos < PrevStartPos ->
% 	  format('~n~n@@@@@@@@@@ OOPS! @@@@@@@@@@~n~n', [])
% 	% fail
% 	; nl
% 	).

% write_index_char_position_list([], _PrevStartPos).
% write_index_char_position_list([H|T], PrevStartPos) :-
% 	write_one_index_char_position_term(H, PrevStartPos, NewStartPos),
% 	write_index_char_position_list(T, NewStartPos).
% 
% write_one_index_char_position_term(Index-Word-StartPos-EndPos, PrevStartPos, StartPos) :-
% 	format('~w-~w-~w-~w', [Index,Word,StartPos,EndPos]),
% 	nonvar(StartPos),
% 	nonvar(EndPos),
% 	!,
% 	( StartPos >= EndPos ->
% 	  format('~n~n@@@@@@@@@@ OOPS! @@@@@@@@@@~n~n', [])
% 	% fail
% 	; EndPos < PrevStartPos ->
% 	  format('~n~n@@@@@@@@@@ OOPS! @@@@@@@@@@~n~n', [])
% 	%  fail
% 	; nl
% 	).
% write_one_index_char_position_term(_, _, _).

% display_matching_word_info(FirstInputMatch, PreviousIndexStartPos,
% 			   StartPosWithinAbstract, EndPosWithinAbstract,
% 			   UtteranceStartPos, UtteranceEndPos,
% 			   FirstToken, TokenType, TokensOut) :-
% 	format('~nMATCHING WORD:~n~q-~w-~w~n',
% 	       [FirstInputMatch,StartPosWithinAbstract, EndPosWithinAbstract]),
% 	format('~q-~w-~w~n',
% 	       [FirstInputMatch,UtteranceStartPos,UtteranceEndPos]),
% 	format('PREVIOUS UtteranceStartPos:~w~n',
% 	       [PreviousIndexStartPos]),
% 	format('MATCHING TOKEN:', []),
% 	token_type(FirstToken, TokenType),
% 	write_one_token(TokenType, FirstToken, 0, user_output),
% 	write_modified_first_token(TokenType, TokensOut).

% token_type(TokenTerm, TokenType) :-
% 	arg(1, TokenTerm, TokenType).

complex_token_type(aa).
complex_token_type(aadef).

% write_token_list(Stream, TokenList, CharOffSet) :-
% 	write_token_list_1(TokenList, CharOffSet, Stream).
% 
% 
% write_token_list_1([], _CharOffset, _Stream).
% write_token_list_1([FirstToken|RestTokens], CharOffset, Stream) :-
% 	token_type(FirstToken, TokenType),
% 	write_one_token(TokenType, FirstToken, CharOffset, Stream),
% 	write_token_list_1(RestTokens, CharOffset, Stream).

% write_one_token(TokenType, TokenTerm, CharOffset, Stream) :-
% 	\+ complex_token_type(TokenType),
% 	!,
% 	write_simple_token(TokenTerm, CharOffset, Stream),
% 	format(Stream, '.~n', []).
% write_one_token(aa, AATokenTerm, CharOffset, Stream) :-
% 	format(Stream, '~n', []),
% 	write_aa_token(AATokenTerm, CharOffset, Stream),
% 	format(Stream, '.~n~n', []).
% write_one_token(aadef, AADefTokenTerm, CharOffset, Stream) :-
% 	format(Stream, '.~n', []),
% 	write_aadef_token(AADefTokenTerm, CharOffset, Stream),
% 	format(Stream, '.~n~n', []).

% offset_simple_token(tok(Type,Arg1,Arg2,PosTerm),
% 		    CharOffset,
% 		    tok(Type,Arg1,Arg2,OffsetPosTerm)) :-
% 	offset_positions(PosTerm, CharOffset, OffsetPosTerm).

% write_simple_token(tok(Type,Arg1,Arg2,PosTerm), _CharOffset, Stream) :-
% 	% offset_positions(PosTerm, CharOffset, OffsetPosTerm),
% 	format(Stream, 'tok(~w,', [Type]),
% 	write_one_token_arg(Arg1, Stream),
% 	write_one_token_arg(Arg2, Stream),
% 	% format(Stream, '~w)', [OffsetPosTerm]).
% 	format(Stream, '~w)', [PosTerm]).

% offset_positions(pos(StartPos,EndPos),
% 		 CharOffset,
% 		 pos(OffsetStartPos, OffsetEndPos)) :-
% 	OffsetStartPos is StartPos + CharOffset,
% 	OffsetEndPos   is EndPos   + CharOffset.

% write_aa_token(tok(aa,AAAcronymList,[AADefToken],AAPos), CharOffset, Stream) :-
% 	format(Stream, 'tok(aa,~n', []),
% 	Indent1 = 5,
% 	write_simple_token_list(AAAcronymList, 1, Indent1, CharOffset, Stream),
% 	AADefToken = tok(aadef,
% 			AADefExpansion,
% 			AADefAcro,
% 			 AADefPos),
% 	format(Stream, '~*c[tok(aadef,~n', [Indent1,32]),
% 	Indent2 is 2*Indent1 + 1,
% 	write_simple_token_list(AADefExpansion, 1, Indent2, CharOffset, Stream),
% 	write_simple_token_list(AADefAcro,      1, Indent2, CharOffset, Stream),
% 	offset_positions(AADefPos, CharOffset, OffsetAADefPos),
% 	offset_positions(AAPos,    CharOffset, OffsetAAPos),
% 	format(Stream, '~*c~q)],~n~*c~q)', [Indent2,32,OffsetAADefPos,Indent1,32,OffsetAAPos]).
	    

% write_aadef_token(tok(aadef,ExpansionList,AcronymList,PosTerm), CharOffset, Stream) :-
% 	% ExpansionList = [ExpH|ExpT],
% 	% AcronymList   = [AcroH|AcroT],
% 	format(Stream, 'tok(aadef,~n', []),
% 	Indent = 5,
% 	write_simple_token_list(ExpansionList, 1, Indent, CharOffset, Stream),
% 	write_simple_token_list(AcronymList,   1, Indent, CharOffset, Stream),
% 	offset_positions(PosTerm, CharOffset, OffsetPosTerm),
% 	format(Stream, '~*c~w)', [Indent,32,OffsetPosTerm]).

	
% write_simple_token_list([], _Index, Indent, _CharOffset, Stream) :-
% 	format(Stream, '~*c~w', [Indent,32,[]]).
% write_simple_token_list([H|T], Index, Indent, CharOffset, Stream) :-
% 	write_simple_token_list_1(T, H, Index, Indent, CharOffset, Stream).
% 
% 
% write_simple_token_list_1([NextSimpleToken|RestSimpleTokens],
% 			FirstSimpleToken, Index, Indent, CharOffset, Stream) :-
% 	( Index > 1 ->
% 	  RealIndent is Indent + 1,
% 	  OpenBracket = ''
% 	; RealIndent is Indent,
% 	  OpenBracket = '['			 
% 	),
% 	format(Stream, '~*c~w', [RealIndent,32,OpenBracket]),
% 	write_simple_token(FirstSimpleToken, CharOffset, Stream),
% 	format(Stream, ',~n', []),
% 
% 	Index1 is Index + 1,
% 	write_simple_token_list_1(RestSimpleTokens, NextSimpleToken,
% 				  Index1, Indent, CharOffset, Stream).
% 
% write_simple_token_list_1([], LastSimpleToken, Index, Indent, CharOffset, Stream) :-
% 	( Index > 1 ->
% 	  RealIndent is Indent + 1,
% 	  OpenBracket = ''
% 	; RealIndent is Indent,
% 	  OpenBracket = '['			 
% 	),
% 	format(Stream, '~*c~w', [RealIndent,32,OpenBracket]),
% 	write_simple_token(LastSimpleToken, CharOffset, Stream),
% 	format(Stream, '],~n', []).

% write_one_token_arg([], Stream) :-
% 	!,
% 	format(Stream, '[],',[]).
% write_one_token_arg(Arg, Stream) :-
% 	atomic(Arg),
% 	!,
% 	format(Stream, '~q,',[Arg]).
% write_one_token_arg(Arg, Stream) :-
% 	is_text_string(Arg),
% 	atom_codes(Atom, Arg),
% 	!,
% 	format(Stream, '~q,', [Atom]).

% write_modified_first_token(aa, TokensOut) :-
% 	!,
% 	( TokensOut = [ModifiedFirstToken|_] ->
% 	  write_one_token(aa, ModifiedFirstToken, 0, user_output)
% 	; write([]), nl
% 	).
% write_modified_first_token(_, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  END Full Fielded DISPLAY Predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BEGIN Full Fielded SYNTAX Predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Use the (pre-metamap) Syntax tree to extract the char positions of each input word
% using the positions captured in the tok(_) terms.
% We *must* use a pre-Metamap tree here because
% of some distortions that Metamap introduces.

% syntax_tree_charpos([], _RemaningSemantics,
% 		    _CharOffset,
% 		    AADefFound, AADefFound,
% 		    _ChosenInputText, _AAs,
% 		    TokenList, TokenList,
% 		    UtteranceStartPos, UtteranceStartPos,
% 		    PositionList, PositionList).
% % not sure I'll ever need the two InputText variables
% syntax_tree_charpos([FirstSyntaxMSU|RestSyntaxMSUs],
% 		    _Semantics,
% 		    CharOffset,
% 		    AADefFoundIn, AADefFoundOut,
% 		    ChosenInputText, AAs,
% 		    TokenListIn, TokenListOut,
% 		    UtteranceStartPosIn, UtteranceStartPosOut,
% 		    PositionListIn, PositionListOut) :-
% 	syntax_MSU_charpos(FirstSyntaxMSU,
% 			   _FirstSemanticsMSU,
% 			   CharOffset,
% 			   AADefFoundIn, AADefFoundNext,
% 			   ChosenInputText, AAs,
% 			   TokenListIn, TokenListNext,
% 			   UtteranceStartPosIn, UtteranceStartPosNext,
% 			   PositionListIn, PositionListNext),
% 	% glom together pos(Start,End) terms!!
% 	syntax_tree_charpos(RestSyntaxMSUs,
% 			    _RestSemanticsMSUs,
% 			    CharOffset,
% 			    AADefFoundNext, AADefFoundOut,
% 			    ChosenInputText, AAs,
% 			    TokenListNext, TokenListOut,
% 			    UtteranceStartPosNext, UtteranceStartPosOut,
% 			    PositionListNext, PositionListOut).

% For a single MSU,
% find character positions in ChosenInputText of inputmatch words
% syntax_MSU_charpos([], _LeftoverSemantics,
% 		   _CharOffset,
% 		   AADefFound, AADefFound,
% 		   _ChosenInputText, _AAs,
% 		   TokenList, TokenList,
% 		   UtteranceStartPos, UtteranceStartPos,
% 		   PositionList, PositionList).
% syntax_MSU_charpos([FirstSyntaxMSUItem|RestSyntaxMSUItems],
% 		   _SemanticsMSUsIn,
% 		   CharOffset,
% 		   AADefFoundIn, AADefFoundOut,
% 		   ChosenInputText, AAs,
% 		   TokenListIn, TokenListOut,
% 		   UtteranceStartPosIn, UtteranceStartPosOut,
% 		   PositionListIn, PositionListOut) :-
% 	syntax_item_charpos(FirstSyntaxMSUItem,
% 			    _FirstSemanticsMSUItem,
% 			    CharOffset,
% 			    AADefFoundIn, AADefFoundNext,
% 			    ChosenInputText, AAs,
% 			    TokenListIn, TokenListNext,
% 			    UtteranceStartPosIn, UtteranceStartPosNext,
% 			    PositionListIn, PositionListNext),
% 	syntax_MSU_charpos(RestSyntaxMSUItems,
% 			   _RestSemanticsMSUItems,
% 			   CharOffset,
% 			   AADefFoundNext, AADefFoundOut,
% 			   ChosenInputText, AAs,
% 			   TokenListNext, TokenListOut,
% 			   UtteranceStartPosNext, UtteranceStartPosOut,
% 			   PositionListNext, PositionListOut).

% An MSU can be of the form [prep(...), head(...)].
% Each term in the MSU is an MSU item.

% Case 1: MSU item contains an inputmatch term
% syntax_item_charpos(SyntaxMSUItem,
% 		    _SemanticsMSUItem,
% 		    CharOffset,
% 		    AADefFoundIn, AADefFoundOut,
% 		    ChosenInputText, AAs,
% 		    TokenListIn, TokenListOut,
% 		    UtteranceStartPosIn, UtteranceStartPosOut,
% 		    PositionListIn, PositionListOut) :-
% 	arg(1, SyntaxMSUItem,    SyntaxItemList),
% 	get_from_list(inputmatch, SyntaxItemList, SyntaxInputMatchList),
% 	% arg(1, SemanticsMSUItem, SemanticsItemList),
% 	% get_from_list(index,     SemanticsItemList, SemanticsMSUIndex),
% 	!,
% 	% SyntaxInputMatchList = [H|T],
% 	syntax_inputmatch_LIST_charpos(SyntaxInputMatchList,
% 				       CharOffset,
% 				       AADefFoundIn, AADefFoundOut,
% 				       ChosenInputText, AAs,
% 				       TokenListIn, TokenListOut,
% 				       UtteranceStartPosIn, UtteranceStartPosOut,
% 				       PositionListIn, PositionListOut).
% 
% % Case 2: MSU item does not contain an inputmatch term
% syntax_item_charpos(_SyntaxMSUItem, _SemanticsMSUItem,
% 		    _CharOffset,
% 		    AADefFound, AADefFound,
% 		    _ChosenInputText, _AAs,
% 		    TokenList, TokenList,
% 		    UtteranceStartPos, UtteranceStartPos,
% 		    PositionList, PositionList).


% For a list of inputmatch words,
% find character positions in ChosenInputText of inputmatch words
% syntax_inputmatch_LIST_charpos([],
% 			       _CharOffset,
% 			       AADefFound, AADefFound,
% 			       _ChosenInputText, _AAs,
% 			       TokenList, TokenList,
% 			       UtteranceStartPos, UtteranceStartPos,
% 			       PositionList, PositionList).
% 
% syntax_inputmatch_LIST_charpos([FirstInputMatchWord|RestInputMatchWords],
% 			       CharOffset,
% 			       AADefFoundIn, AADefFoundOut,
% 			       ChosenInputText, AAs,
% 			       TokenListIn, TokenListOut,
% 			       UtteranceStartPosIn, UtteranceStartPosOut,
% 			       [FirstInputMatchWord-WordStartPos-WordEndPos|PosListIn],
% 			       PosListOut) :-
% 	syntax_inputmatch_charpos(FirstInputMatchWord,
% 				    CharOffset,
% 				    AADefFoundIn, AADefFoundNext,
% 				    ChosenInputText, AAs,
% 				    TokenListIn, TokenListNext,
% 				    UtteranceStartPosIn, UtteranceStartPosNext,
% 				    WordStartPos, WordEndPos),
% 	syntax_inputmatch_LIST_charpos(RestInputMatchWords,
% 				       CharOffset,
% 				       AADefFoundNext, AADefFoundOut,
% 				       ChosenInputText, AAs,
% 				       TokenListNext, TokenListOut,
% 				       UtteranceStartPosNext, UtteranceStartPosOut,
% 				       PosListIn, PosListOut).


% Ignore tokens of the following types:
% sn, ws, pe, pn, aadef
% Examine tokens of all other types, i.e.,
% lc: lowercase
% uc: uppercase
% ic: initial capital
% mc: mixed case
% an: alphanumeric
% nu: numeric
% aa: acronym

% We also want to ignore the first aa token after an aadef token,
% but only if the aa token corresponds to the previous aadef token.
% I had thought that an aadef token would always have a corresponding aa token,
% following soon afterwards, but that does not seem to be the case,
% so I have to keep track of the acronym defined by the aadef token
% (its third argument).

% token_type_to_ignore(TokenType, Token, AADefFoundIn, AADefFoundOut, TokensToConsume)
% AAdefFoundIn is passed in to token_type_to_ignore/5 as the value
% corresponding to the previous token to ignore. Its values are one of:
%   * 0: Either no AADef token has been encountered thus far,
%     or an AA token corresponsing to the most recent AADef token has been encountered.
%   * Abbreviation: the list of simple tokens found in the third argument
%     of the most recent AADef token encountered.

% We ignore an AA token if its second argument (its Abbreviation) is the same as
% the abbreviation passed in as AADefFoundIn.

% token_type_to_ignore(sn,    _Token,    AADefFound, AADefFound,   []).
% token_type_to_ignore(ws,    _Token,    AADefFound, AADefFound,   []).
% token_type_to_ignore(pe,    _Token,    AADefFound, AADefFound,   []).
% set Abbreviation here to be the third arg of the AADef token
% token_type_to_ignore(aadef,  Token,    _AADefFoundIn, Abbreviation, []) :-
% 	arg(3, Token, Abbreviation).
% Ignore an aa token iff its second argument (its abbreviation)
% matches the abbreviation of the most recent aadef token,
% which will be the third arg of the call to token_type_to_ignore.
% token_type_to_ignore(aa,    AAToken,   AADefAbbreviation, 0, AAAbbreviation) :-
% 	arg(2, AAToken, AAAbbreviation),
% 	AADefAbbreviation == AAAbbreviation.

% For a single inputmatch word,
% find character positions in ChosenInputText of inputmatch words
% Half-assed attempt to prevent SemRep failure. %Halil
%NCT00503841.ab.4 "receptornegative" - It's actually a tokenization problem
% syntax_inputmatch_charpos(_FirstInputMatch,
% 			  _CharOffset,
% 			  _AADefFoundIn, _AADefFoundOut,
% 			  _ChosenInputText, _AAs,
% 			  [], _TokensOut,
% 			  PreviousIndexStartPos, UtteranceStartPos,
% 			  UtteranceStartPos, UtteranceStartPos) :-
% 	UtteranceStartPos is PreviousIndexStartPos + 1.
% % Case 1: FirstToken should be ignored:
% % Consume any following tokens, and recurse on remainder.
% syntax_inputmatch_charpos(FirstInputMatch,
% 			  CharOffset,
% 			  AADefFoundIn, AADefFoundOut,
% 			  _ChosenInputText, _AAs,
% 			  [FirstToken|RestTokens], TokensOut,
% 			  PreviousIndexStartPos, UtteranceStartPos,
% 			  UtteranceStartPos, UtteranceEndPos) :-
% 	arg(1, FirstToken, FirstTokenType),
% 	token_type_to_ignore(FirstTokenType, FirstToken, 
% 			     AADefFoundIn, AADefFoundNext,
% 			     OtherTokensToConsume),
% 	!,
% 	consume_other_tokens(OtherTokensToConsume, RestTokens, NextTokens),
% 	syntax_inputmatch_charpos(FirstInputMatch,
% 				  CharOffset,
% 				  AADefFoundNext, AADefFoundOut,
% 				  _ChosenInputText, _AAs,
% 				  NextTokens, TokensOut,
% 				  PreviousIndexStartPos, UtteranceStartPos,
% 				  UtteranceStartPos, UtteranceEndPos).

% Case 2: FirstToken matches!
% syntax_inputmatch_charpos(FirstInputMatch,
% 			  CharOffset,
% 			  AADefFound, AADefFound,
% 			  _ChosenInputText, _AAs,
% 			  [FirstToken|RestTokens], TokensOut,
% 			  PreviousIndexStartPos, UtteranceStartPos,
% 			  UtteranceStartPos, UtteranceEndPos) :-
% 	arg(1, FirstToken, TokenType),
% 	% Must ensure that this token's StartPosWithinAbstract/EndPosWithinAbstract,
% 	% which are the Start/End positions within the entire Abstract,
% 	% are not less than the StartPosWithinAbstract/EndPosWithinAbstract of the previous match
% 	matching_token(TokenType, 
% 		       FirstToken, RestTokens,
% 		       FirstInputMatch, StartPosWithinAbstract, EndPosWithinAbstract,
% 		       TokensOut),
% 	% Add 1 because Lan's tokens' StartPos is the character position
% 	% *before* the first char of the token
% 	UtteranceStartPos is StartPosWithinAbstract - CharOffset + 1,
% 	UtteranceEndPos   is EndPosWithinAbstract   - CharOffset,
% 	% Must ensure that UtteranceStartPos here is >= previous UtteranceStartPos
% 	UtteranceStartPos >= PreviousIndexStartPos,
% 	!.
%	display_matching_word_info(FirstInputMatch, PreviousIndexStartPos,
%				   StartPosWithinAbstract, EndPosWithinAbstract,
%				   UtteranceStartPos, UtteranceEndPos,
%				   FirstToken, TokenType, TokensOut).

/*
  The next clause handles cases in which the order of the words appearing in
  (1) the inputmatch(_) terms in the parse tree and
  (2) the tok(_,_,_,_) terms from expand_utterances
  are not aligned.

  Example 1: "high blood cholesterol" has

  tok(lc, high, ...),
  tok(lc, blood, ...)
  tok(lc, cholesterol, ...)

  but Metamap analyzes this as
  [ mod(blood), head(high cholesterol) ]

  Example 2: "calcium-channel-blocking activity" has

  tok(lc, calcium, ...)
  tok(lc, channel, ...)
  tok(lc, blocking, ...)
  tok(lc, activity, ...)

  but Metamap analyzes this as
  [ mod(blocking), head(calcium-channel activity) ]

  ARRRGHHH!!!
  FML 11/18/2005 Friday @ 17:03:10

  In that case, we simply skip over, but keep, the non-matching token.

*/

% Case 3: Skip over this token, and continue looking for a matching token
% syntax_inputmatch_charpos(FirstInputMatch,
% 			  CharOffset,
% 			  AADefFoundIn, AADefFoundOut,
% 			  ChosenInputText, AAs,
% 			  [FirstToken|RestTokensIn],
% 			  [FirstToken|RestTokensOut],
% 			  PreviousIndexStartPos, UtteranceStartPos,
% 			  InputMatchStartPos, InputMatchEndPos) :-
% 	syntax_inputmatch_charpos(FirstInputMatch,
% 				  CharOffset,
% 				  AADefFoundIn, AADefFoundOut,
% 				  ChosenInputText, AAs,
% 				  RestTokensIn, RestTokensOut,
% 				  PreviousIndexStartPos, UtteranceStartPos,
% 				  InputMatchStartPos, InputMatchEndPos).

% matching_token(aa, tok(aa,Abbreviation1,Expansion,pos(StartPos,EndPos)),

/*

% Ignore the first tok(aa, ...) token occurring after each tok(aadef, ...) token.
% Also consume all tokens after the tok(aa, ...) token that appear
% in the tok(aa, ...) term's second argument.
% Here's why:

Most acronym appearances manifest themselves in the token list as follows.
Given the text "forced expiatory time (FET)", the expanded-utterance version
of the text will be "forced expiatory time"--without "(FET)".

The token list for this input string is

tok(aadef,
     [tok(lc,forced,forced,pos(0,6)),
      tok(ws,' ',' ',pos(6,7)),
      tok(lc,expiatory,expiatory,pos(7,16)),
      tok(ws,' ',' ',pos(16,17)),
      tok(lc,time,time,pos(17,21))],
     [tok(uc,'FET',fet,pos(23,26))],
     pos(0,21)).

tok(lc,forced,forced,pos(0,6)).
tok(ws,' ',' ',pos(6,7)).
tok(lc,expiatory,expiatory,pos(7,16)).
tok(ws,' ',' ',pos(16,17)).
tok(lc,time,time,pos(17,21)).
tok(ws,' ',' ',pos(21,22)).
tok(pe,[],1,pos(22,27)).
tok(pn,'(','(',pos(22,23)).

tok(aa,
     [tok(uc,'FET',fet,pos(23,26))],
     [tok(aadef,
           [tok(lc,forced,forced,pos(0,6)),
            tok(ws,' ',' ',pos(6,7)),
            tok(lc,expiatory,expiatory,pos(7,16)),
            tok(ws,' ',' ',pos(16,17)),
            tok(lc,time,time,pos(17,21))],
           [tok(uc,'FET',fet,pos(23,26))],
           pos(0,21))],
     pos(23,26)).

tok(uc,'FET',fet,pos(23,26)).
tok(pn,')',')',pos(26,27)).
tok(pn,'.','.',pos(27,28)).

We want to match the input tokens 'forced', 'expiatory', and 'time'
with their positions:

	tok(lc,forced,forced,pos(0,6))
	tok(lc,expiatory,expiatory,pos(7,16))
	tok(lc,time,time,pos(17,21))

To do this, we must ignore
(1) the aadef token,
(2) the following (and corresponding) aa token, and
(3) the following tok(uc,'FET',fet,pos(23,26))

The first tok(aa, ...) term after a tok(aadef, ...) term
should not be allowed to match up with the downstream tokens
when they finally appear.

*/

% consume_other_tokens(TokensToConsume, TokensIn, TokensOut)
% Remove from TokensIn all tokens in TokensToConsume
% and all other tokens encountered until all tokens in TokensToConsume are consumed.
% Unify TokensOut with list of all tokens remaining in TokensIn.

% All done. No more tokens to consume.
% consume_other_tokens([], RestTokens, RestTokens).
% consume_other_tokens([FirstTokenToConsume|RestTokensToConsume],
% 		     [FirstRestToken|RestTokensIn],
% 		     TokensOut) :-
% 	  % Current token matches first token to consume;
% 	  % discard it, and recurse.
% 	( FirstTokenToConsume = FirstRestToken ->
% 	  consume_other_tokens(RestTokensToConsume, RestTokensIn, TokensOut)
% 	  % Current token does NOT match first token to consume;
% 	  % so keep it, and recurse.
% 	; consume_other_tokens([FirstTokenToConsume|RestTokensToConsume], RestTokensIn, TokensOut)
% 	).

% matching_token(aa,
% 	       tok(aa,Abbrev1,ExpansionIn,pos(StartPos,EndPos)),
% 	       RestTokens,
% 	       InputMatch,
% 	       StartPos, EndPos,
% 	       [tok(aa,Abbrev1,ExpansionOut,pos(StartPos,EndPos))|RestTokens]) :-
% 	ExpansionIn = [tok(aadef,ExpansionList,Abbrev2,ExpansionPos)|RestExp],
% 	delete_inputmatch_from_aadef_expansion(ExpansionList, InputMatch, RestExpansionList),
% 	!,
%         ExpansionOut = [tok(aadef,RestExpansionList,Abbrev2,ExpansionPos)|RestExp].
% 
%       
% % consume the non-aa token if it matches
% matching_token(OtherTokenType,
% 	       tok(OtherTokenType,TextString,_LCText,pos(StartPos,EndPos),_),
% 	       RestTokens,
% 	       InputMatchAtom, StartPos, EndPos, RestTokens) :-
% 	% lower(InputMatchAtom, LowerInputMatchAtom),
% 	is_text_string(TextString),
% 	atom_codes(TextAtom, TextString),
% 	% lower(TextAtom, LowerTextAtom),
% 	% LowerInputMatchAtom == LowerTextAtom,
% 	( InputMatchAtom == TextAtom ->
% 	  true
% 	; % format('~n### MISMATCH ~w~n### MISMATCH ~w~n~n', [InputMatchAtom,TextAtom]),
% 	  atom_codes(InputMatchAtom, InputMatchString),
% 	  prefix(InputMatchString, TextString)
% 	).


% delete_inputmatch_from_aadef_expansion(ExpansionList, InputMatch, RestExpansionList)
% ExpansionList is the second argument of a tok(aadef,...) structure, e.g.,
% tok(aadef,ExpansionList,Abbrev2,ExpansionPos)
% which is itself the third argument of a tok(aa,...) structure.

% ExpansionList is a list of simple tokens, e.g.,

% [tok(lc,growth,growth,pos(272,278)),
%  tok(ws,' ',' ',pos(278,279)),
%  tok(lc,hormone,hormone,pos(279,286)),
%  tok(pn,-,-,pos(286,287)),
%  tok(lc,releasing,releasing,pos(287,296)),
%  tok(ws,' ',' ',pos(296,297)),
%  tok(lc,hormone,hormone,pos(297,304))]

% delete_inputmatch_from_aadef_expansion
% removes from ExpansionList the simple token containing InputMatchAtom
% as well as other tokens encountered before it.

% delete_inputmatch_from_aadef_expansion([FirstAADefToken|RestAADefTokens],
% 				       InputMatchAtom, RestAADefTokens) :-
% 	FirstAADefToken = tok(_Type,TextString,_LCText,_Pos),
% 	% lower(InputMatchAtom, LowerInputMatchAtom),
% 	is_text_string(TextString),
% 	atom_codes(TextAtom, TextString),
% 	% lower(TextAtom, LowerTextAtom),
% 	% LowerInputMatchAtom == LowerTextAtom,
% 	InputMatchAtom == TextAtom,
% 	!.
% delete_inputmatch_from_aadef_expansion([_FirstAADefToken|RestAADefTokens],
% 				       InputMatch, RestExpansion) :-
% 	delete_inputmatch_from_aadef_expansion(RestAADefTokens,
% 				      InputMatch, RestExpansion).

% is_text_string([]).
% is_text_string([H|T]) :-
% 	is_ascii(H),
% 	is_text_string(T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% END Full Fielded SYNTAX Predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BEGIN Full Fielded SEMANTICS Predicates %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Use the Semantics tree to find arguments,
% and the WordCharPositionList determined by syntax_tree_charpos/12
% to determine the character positions spanned by each MSU with an index(_) term.
% semantics_tree_charpos([],
% 		       _WordCharPositionListIn, _WordCharPositionListOut,
% 		       UtteranceStartPos, UtteranceStartPos,
% 		       [],   _IndexCharPositionList).
% 
% semantics_tree_charpos([FirstSemanticsMSU|RestSemanticsMSUs],
% 		       WordCharPosListIn, WordCharPosListOut,
% 		       UtteranceStartPosIn, UtteranceStartPosOut,
% 		       IndexCharPosListIn, IndexCharPosListOut) :-
% 	semantics_MSU_charpos(FirstSemanticsMSU,
% 			      WordCharPosListIn, WordCharPosListNext,
% 			      UtteranceStartPosIn, UtteranceStartPosNext,
% 			      IndexCharPosListIn, IndexCharPosListNext),
% 	semantics_tree_charpos(RestSemanticsMSUs,
% 			       WordCharPosListNext,  WordCharPosListOut,
% 			       UtteranceStartPosNext, UtteranceStartPosOut,
% 			       IndexCharPosListNext, IndexCharPosListOut).

% semantics_MSU_charpos([],
% 		      WordCharPosList,  WordCharPosList,
% 		      UtteranceStartPos, UtteranceStartPos,
% 		      IndexCharPosList, IndexCharPosList).
% 
% semantics_MSU_charpos([FirstSemanticsMSUItem|RestSemanticsMSUItems],
% 		      WordCharPositionListIn, WordCharPositionOut,
% 		      UtteranceStartPosIn, UtteranceStartPosOut,
% 		      IndexCharPositionListIn, IndexCharPositionListOut) :-
% 	get_semantics_item_data(FirstSemanticsMSUItem, IndexOrInputMatch,
% 				Index, SemanticsInputMatchWordList),
% 	semantics_item_charpos(IndexOrInputMatch, Index, SemanticsInputMatchWordList,
% 			       WordCharPositionListIn,  WordCharPositionListNext,
% 			       UtteranceStartPosIn, UtteranceStartPosNext,
% 			       IndexCharPositionListIn, IndexCharPositionListNext),
% 	semantics_MSU_charpos(RestSemanticsMSUItems,
% 			      WordCharPositionListNext, WordCharPositionOut,
% 			      UtteranceStartPosNext, UtteranceStartPosOut,
% 			      IndexCharPositionListNext, IndexCharPositionListOut).
% semantics_MSU_charpos([_FirstSemanticsMSUItem|RestSemanticsMSUItems],
% 	              WordCharPositionListIn, WordCharPositionListOut,
% 		      UtteranceStartPosIn, UtteranceStartPosOut,
% 		      IndexCharPositionListIn, IndexCharPositionListOut) :-
% 	semantics_MSU_charpos(RestSemanticsMSUItems, 
% 	                      WordCharPositionListIn, WordCharPositionListOut,
% 			      UtteranceStartPosIn, UtteranceStartPosOut,
% 			      IndexCharPositionListIn, IndexCharPositionListOut).

% determine if this SemanticsMSUItem contains
% (1) an index(_) term, in which case it must contain an inputmatch(_) term, or
% (2) no index(_) term, but an inputmatch(_) term, or
% (3) neither an index(_) nor an inputmatch(_) term

% get_semantics_item_data(SemanticsMSUItem, IndexOrInputMatch,
% 			Index, SemanticsInputMatchWordList) :-
% 	arg(1, SemanticsMSUItem, SemanticsItemList),
% 	( get_from_list(inputmatch, SemanticsItemList, SemanticsInputMatchWordList) ->
% 	  ( get_from_list(index, SemanticsItemList, Index) ->
% 	    IndexOrInputMatch = index
% 	  ; IndexOrInputMatch = inputmatch
% 	  )
% 	; IndexOrInputMatch = neither
% 	).

% SemanticsMSUItem here is a structure like det(_), head(_), prep(_), etc.
% Case 1: Item does contain an index(_) term.
% semantics_item_charpos(SemanticsMSUItem,
% semantics_item_charpos(index, Index, SemanticsInputMatchWordList,
% 		       WordCharPositionListIn,  WordCharPositionListOut,
% 		       PreviousIndexStartPos, IndexStartPos,
% 		       [Index-SemanticsInputMatchWordList-IndexStartPos-IndexEndPos
% 			| RestIndexCharPositionList], RestIndexCharPositionList) :-
% 	% must ensure that index increases, just as we did in Case 2 of
% 	% syntax_inputmatch_charpos
% 
% 	SemanticsInputMatchWordList = [Word|RestWords],
% 
% 	% Still need to ensure this is done:
% 	% If there are multiple instances of Word in the WordCharPositionList,
% 	% make sure the one chosen FOLLOWS the previous word, if possible.
% 	% If there's no occurrence of the word FOLLOWING the previous work,
% 	% then--and only then--take the last occurrence, even if it precedes
% 	% the previous word
% 
% 	semantics_inputmatch_word_charpos(WordCharPositionListIn,
% 					  PreviousIndexStartPos,
% 					  _TempWord,
% 					  Word, WordStartPos, WordEndPos,
% 					  WordCharPositionListNext),
% 	!,
% 	% Word is the first Word in the index, so the starting position
% 	% of Word is also the starting position of the entire index
% 	IndexStartPos = WordStartPos,
% 
% 	% format('~NSemanticsInputMatchWordList: ~q-~w-~w~n',
% 	%        [SemanticsInputMatchWordList,IndexStartPos,WordEndPos]),
% 
% 	semantics_inputmatch_LIST_charpos(RestWords,
% 					  IndexStartPos,
% 					  WordEndPos,
% 					  IndexEndPos,
% 					  WordCharPositionListNext,
% 					  WordCharPositionListOut).

% Case 2: Item does NOT contain an index(_) term, but does contain an inputmatch(_) term.
% Still remove the input words and their start/end pos from WordCharPositionList
% semantics_item_charpos(SemanticsMSUItem,
% semantics_item_charpos(inputmatch, _Index, SemanticsInputMatchWordList,
% 		       WordCharPositionListIn,  WordCharPositionListOut,
% 		       UtteranceStartPos,     UtteranceStartPos,
% 		       IndexCharPositionList, IndexCharPositionList) :-
% 	delete_from_word_char_position_list(SemanticsInputMatchWordList,
% 					    WordCharPositionListIn,
% 					    WordCharPositionListOut).
% 
% % Case 3: Item contain neither an index(_) term not an inputmatch(_) term; do nothing.
% % semantics_item_charpos(_SemanticsMSUItem,
% semantics_item_charpos(neither, _Index, _SemanticsInputMatchWordList,
%                        WordCharPositionList,  WordCharPositionList,
%                        UtteranceStartPos,     UtteranceStartPos,
%                        IndexCharPositionList, IndexCharPositionList).
% 

% delete_from_word_char_position_list([], WordCharPositionList, WordCharPositionList).
% delete_from_word_char_position_list([H|T], WordCharPositionListIn, WordCharPositionListOut) :-
% 	selectchk(H-_-_, WordCharPositionListIn, WordCharPositionListNext),
% 	delete_from_word_char_position_list(T, WordCharPositionListNext, WordCharPositionListOut).

% Given, e.g.,
% SemanticsInputMatchList = [gastroesophageal,reflux,disease]
% WordCharPositionListIn = [gastroesophageal-0-16,
%			    reflux-17-23,
% 			    disease-24-31,
% 			    to-39-41,
% 			    gastroesophageal-42-46,
% 			    reflux-42-46,
% 			    disease-42-46,
% 			    symptoms-47-55,
% 			    '.'-55-56]
% set
% IndexStartPos = 0,
% IndexEndPos = 31



% semantics_inputmatch_LIST_charpos([],
% 				  _PreviousIndexStartPos,
% 				  IndexEndPos, IndexEndPos,
% 				  WordCharPositionList,
% 				  WordCharPositionList).
% semantics_inputmatch_LIST_charpos([Word|RestWords],
% 				  PreviousIndexStartPos,
% 				  _IndexStartPos, IndexEndPos,
% 				  WordCharPositionListIn,
% 				  WordCharPositionListOut) :-
% 
% 	% Still need to ensure this is done:
% 	% If there are multiple instances of Word in WordCharPositionList,
% 	% choose the one whose StartPos is closest (whether before or after)
% 	% to PreviousIndexStartPos. Break ties in favor of later words.
% 
% 	semantics_inputmatch_word_charpos(WordCharPositionListIn,
% 					  PreviousIndexStartPos,
% 					  _TempWord,
% 					  Word, _WordStartPos, WordEndPos,
% 					  WordCharPositionListNext),
% 				     
% 	!,
% 	semantics_inputmatch_LIST_charpos(RestWords,
% 					  PreviousIndexStartPos,
% 					  WordEndPos, IndexEndPos,
% 					  WordCharPositionListNext,
% 					  WordCharPositionListOut).

/*

Inputs:
 * WordCharPositionList:  A list of term of the form Word-WordStartPos-WordEndPos
 * PreviousIndexStartPos: The Index of the previous indexed MSU
 * Word: The word we want to find in WordCharPositionList

Search WordCharPositionList for the triple W-S-E such that
 (1) W == Word
 (2) S is the closest to PreviousIndexStartPos
     (in either direction, but give preference to > rather than <)

*/

% semantics_inputmatch_word_charpos(WordCharPositionListIn,
% 				  PreviousIndexStartPos,
% 				  _TempWord,
% 				  Word, WordStartPos, WordEndPos,
% 				  WordCharPositionListOut) :-
% 	all_matching_words(WordCharPositionListIn, Word, PreviousIndexStartPos, MatchingWords),
% 	select_closest_word(MatchingWords, Word-WordStartPos-WordEndPos),
% 	selectchk(Word-WordStartPos-WordEndPos, WordCharPositionListIn, WordCharPositionListOut).

% all_matching_words([], _Word, _PreviousIndexStartPos, []).
% all_matching_words([Word-StartPos-EndPos|_RestWords],
% 		   Word, PreviousIndexStartPos, [Distance:Word-StartPos-EndPos]) :-
% 	StartPos >= PreviousIndexStartPos,
% 	!,
% 	Distance is abs(PreviousIndexStartPos - StartPos).
% %	Distance is PreviousIndexStartPos - StartPos.
% all_matching_words([Word-StartPos-EndPos|RestWords],
% 		   Word, PreviousIndexStartPos, [Distance:Word-StartPos-EndPos|RestMatches]) :-
% 	!,
% 	Distance is abs(PreviousIndexStartPos - StartPos),
% %	Distance is PreviousIndexStartPos - StartPos,
% 	all_matching_words(RestWords, Word, PreviousIndexStartPos, RestMatches).
% all_matching_words([_OtherWord-_StartPos-_EndPos|RestWords],
% 		   Word, PreviousIndexStartPos, Matches) :-
% all_matching_words(RestWords, Word, PreviousIndexStartPos, Matches).
	

% select_closest_word(MatchingWords, ClosestWord) :-
% 	select_closest_word_1(MatchingWords, 1000:_-_-_, ClosestWord).
% 
% select_closest_word_1([], _Distance:ClosestWord, ClosestWord).
% select_closest_word_1([CurrentDistance:Word-StartPos-EndPos|RestWords],
% 		      TempDistance:_TempWord-_TempStartPos-_TempEndPos, ClosestWord) :-
% 	CurrentDistance =< TempDistance,
% 	!,
% 	select_closest_word_1(RestWords, CurrentDistance:Word-StartPos-EndPos, ClosestWord).
% select_closest_word_1([CurrentDistance:_Word-_StartPos-_EndPos|_RestWords],
% 		      TempDistance:TempWord-TempStartPos-TempEndPos, ClosestWord) :-
% 	CurrentDistance > TempDistance,
% 	!,
% 	ClosestWord = TempWord-TempStartPos-TempEndPos.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% END Full Fielded SEMANTICS Predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% full_fielded_format_gendis %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% full_fielded_format_gendis([], _OutputStream, _InputLabel,
% 			   _ChosenInputText, _WordCharPositionList, _IndexCharPositionList,
% 			   _UtteranceType, _TiOrAb, _SentenceID, _MetaConcList,
% 			   FormattedGenDisList, FormattedGenDisList).
% full_fielded_format_gendis([FirstGenDisTerm|RestGenDisTerms], OutputStream, InputLabel,
% 			   ChosenInputText, WordCharPositionList, IndexCharPositionList,
% 			   UtteranceType, TiOrAb, SentenceID, MetaConcList,
% 			   FormattedGenDisTermsIn, FormattedGenDisTermsOut) :-
% 	format_one_gendis_term_wrapper(FirstGenDisTerm, OutputStream, InputLabel,
% 				       ChosenInputText,
% 				       WordCharPositionList, IndexCharPositionList,
% 				       UtteranceType,  TiOrAb, SentenceID, MetaConcList,
% 				       FormattedGenDisTermsIn, FormattedGenDisTermsNext),
% 	full_fielded_format_gendis(RestGenDisTerms, OutputStream, InputLabel,
% 				   ChosenInputText, WordCharPositionList, IndexCharPositionList,
% 				   UtteranceType, TiOrAb, SentenceID, MetaConcList,
% 				   FormattedGenDisTermsNext, FormattedGenDisTermsOut).

% format_one_gendis_term(disorder(MinIndex, MaxIndex, DisorderAtom,
% 				MetaConc, _DISSemTypeList/SemTypeList),
% 		        TiOrAb, SentenceID, _ChosenInputText,
% 		       _WordCharPositionList, IndexCharPositionList,
% 		       UtteranceType, MetaConcList,
% 
% 		       '3disorder'-(UtteranceType-TiOrAb-SentenceID-disorder-
% 		      MetaConc-SemTypeListAtom-CUI-
% 		      DisorderAtom-
% 		      Change-Degree-Negation-
% 		      Confidence-StartPos-EndPos)) :-
% 	get_all_concept_cuis(MetaConc, CUI),
% 	concat_atom(SemTypeList, ',', SemTypeListAtom),
% 	set_empty_semrep_value(change,     Change),
% 	set_empty_semrep_value(degree,     Degree),
% 	set_empty_semrep_value(negation,   Negation),
% 	get_gendis_confidence(MetaConcList, MetaConc, Confidence),
% 	get_index_start_and_end_pos(MinIndex/MaxIndex, IndexCharPositionList, StartPos, EndPos).
% 
% format_one_gendis_term(genphenom(MinIndex, MaxIndex, GeneList,
% 				 MetaConc, _GNGMSemTypeList/SemTypeList),
% 
% 		        TiOrAb, SentenceID, _ChosenInputText,
% 		       _WordCharPositionList, IndexCharPositionList,
% 		       UtteranceType, MetaConcList,
% 
% 		       '4genphenom'-(UtteranceType-TiOrAb-SentenceID-genphenom-
% 				     AtomizedMetaConc-SemTypeListAtom-CUI-
% 				     NormGeneID-NormGeneName-GeneName-
% 				     GenPhenomAtom-
% 				     ConvertedGeneFunctionList-
% 				     Change-Degree-Negation-
% 				     Confidence-GeneStartPos-GeneEndPos)) :-
% 	GeneList = [NormGeneID,NormGeneName,GeneName,GenPhenomAtom,GeneFunctionList],
% 	get_all_concept_cuis(MetaConc, CUI),
% 	concat_atom(SemTypeList, ',', SemTypeListAtom),
% 	atomize(MetaConc, AtomizedMetaConc),
% 	set_empty_semrep_value(change,      Change),
% 	set_empty_semrep_value(degree,      Degree),
% 	set_empty_semrep_value(negation,    Negation),
% 	get_gendis_confidence(MetaConcList, MetaConc, Confidence),
% 	convert_genphenom_function_list(GeneFunctionList, ConvertedGeneFunctionList),
% 	get_index_start_and_end_pos(MinIndex/MaxIndex, IndexCharPositionList,
% 				    GeneStartPos, GeneEndPos).
% 
% get_gendis_confidence([], _ThisMetaConc, '').
% get_gendis_confidence([(FirstMetaConc:_SemTypeList)-_Index-Confidence|_RestMetaConcTerms],
% 		      ThisMetaConc, Confidence) :-
% 	matching_metaconcs(ThisMetaConc, FirstMetaConc),
% 	!.
% get_gendis_confidence([_|RestMetaConcTerms], ThisMetaConc, Confidence) :-
% 	get_gendis_confidence(RestMetaConcTerms, ThisMetaConc, Confidence).
% 
% matching_metaconcs(MetaConc, MetaConc):- !.
% % the '-'/2 operator is right associative,
% % so there's no need to do two cases,
% % one for _-_-_-_-MetaConc and the other for _-MetaConc
% matching_metaconcs(MetaConc, _OtherStuff-MetaConc).

% update_predications([],_SemanticAnalysis,_SemanticAnalysisWithMSUIndex,[]) :- !.
% update_predications([scale-ScaleName-Index|Rest], SemanticAnalysis,
% 	            SemanticAnalysisWithMSUIndex,[scale-ScaleName-Index|RestUpdated]) :-
% 	update_predications(Rest, SemanticAnalysis, SemanticAnalysisWithMSUIndex,
% 	                    RestUpdated).
% update_predications([Predication|Rest], SemanticAnalysis,
% 	            SemanticAnalysisWithMSUIndex,
% 		    [PredicationUpdated|RestUpdated]) :-
% 	Predication = _SubjectMaxDist-_SubjectDist-SubjectIndex-
% 	              SubjectMetaConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-
% 	              IndicatorType-Relation-IndicatorIndex-
% 	              _ObjectMaxDist-_ObjectDist-ObjectIndex-
% 	              ObjectMetaConc-ObjectCUI-ObjectSemTypeList-ObjectSemType,
%         % This means distance fields need to be instantiated		      
%         ( var(IndicatorType)  ->
%           get_indicator(SemanticAnalysis, IndicatorIndex, _Indicator, IndType),
% 	  compute_distances(SemanticAnalysisWithMSUIndex, IndicatorIndex,  
% 	                    SubjectIndex, ObjectIndex, 
% 	                    SubjMaxDist, SubjDist, ObjMaxDist, ObjDist),
% 	  PredicationUpdated = SubjMaxDist-SubjDist-SubjectIndex-
% 	                       SubjectMetaConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-
% 	                       IndType-Relation-IndicatorIndex-
% 	                       ObjMaxDist-ObjDist-ObjectIndex-
% 	                       ObjectMetaConc-ObjectCUI-ObjectSemTypeList-ObjectSemType,
%           update_coordinated_predications(Rest, PredicationUpdated, RestUpdated0),	
% 	  update_predications(RestUpdated0, SemanticAnalysis, 
% 	                      SemanticAnalysisWithMSUIndex, RestUpdated)
% 	; PredicationUpdated = Predication,
% 	  update_predications(Rest, SemanticAnalysis,
% 	                      SemanticAnalysisWithMSUIndex, RestUpdated)
%         ).


% ---------- indicator_type ---------------------

% get_indicator([ThisMSU|_Rest], Index, MSUElement, Type) :-
% 	get_indicator_1(ThisMSU, Index, MSUElement, Type),
% 	!.
% get_indicator([_ThisMSU|Rest], Index, MSUElement, Type) :-
% 	get_indicator(Rest, Index, MSUElement, Type).
%         
% 
% get_indicator_1([MSUElement|_Rest], Index, MSUElement, Type) :-
% 	functor(MSUElement, _ElementType, 1),
% 	arg(1,MSUElement, ItemList),	
% 	memberchk(index(Index), ItemList),
% 	!,
% 	indicator_type(MSUElement, Type).
% get_indicator_1([_MSUElement|Rest], Index, MSUElement, Type) :-
% 	get_indicator_1(Rest, Index, MSUElement, Type).
	
% indicator_type(verb(_),     'VERB').
% indicator_type(prep(_),     'PREP').
% indicator_type(mod(_),      'NOM').
% indicator_type(head(_),     'NOM').
% indicator_type(aux(_),      'AUX').
% indicator_type(pastpart(_), 'PART').

% ---------- compute_distances ------------------
% moved over from semgeninterp.pl and modified
% can be simplified possibly
% compute_distances(SemanticAnalysis, IndicatorIndex, 
% 	          SubjectIndex, ObjectIndex, 
% 		  SubjectMaxDist, SubjectDist, ObjectMaxDist, ObjectDist) :-
% 	rev(SemanticAnalysis, RevAnalysis),
% 	get_msu_index(RevAnalysis, IndicatorIndex, IndicatorPos, Indicator),
% 	get_msu_index(RevAnalysis, SubjectIndex, SubjectPos, _Subject),
% 	get_msu_index(RevAnalysis, ObjectIndex, ObjectPos, _Object),
% 	compute_distances_1(Indicator, 
% 	                  (IndicatorPos, SemanticAnalysis, RevAnalysis, SubjectPos, ObjectPos),
% 			  (SubjectMaxDist, SubjectDist, ObjectMaxDist, ObjectDist)).
% 	
% 
% compute_distances_1(verb(_), InputArgs, OutputArgs) :-
% 	!,
% 	compute_distances_2(InputArgs, OutputArgs).
% compute_distances_1(aux(_), InputArgs, OutputArgs) :-
% 	!,
% 	compute_distances_2(InputArgs, OutputArgs).
% compute_distances_1(pastpart(_), InputArgs, OutputArgs) :-
% 	!,
% 	compute_distances_2(InputArgs, OutputArgs).
% compute_distances_1(prep(_), InputArgs, OutputArgs) :-
% 	!,
% 	compute_distances_3(InputArgs, OutputArgs).
% compute_distances_1(_,      _InputArgs, (0,0,0,0) ).
% 
% compute_distances_2( (IndicatorPos, Analysis, RevAnalysis, FirstPos, SecondPos),
% 		     (SubjectMaxDist, SubjectDist, ObjectMaxDist, ObjectDist ) ) :-
% 	% FirstPos and SecondPos are the msu_index values of the MSU containing
% 	% SubjectHeadList and ObjectHeadList.
% 	% The msu_index(_) terms are added (by domain_add_msu_index/3 in usemrep.pl)
% 	% iff the domain is genetics
% 	( FirstPos > IndicatorPos ->
% 	  get_intervening_npu_counts(RevAnalysis, FirstPos, IndicatorPos,
% 				     0, SubjectMaxDist, 0, SubjectDist1),
% 	  SubjectDist is SubjectMaxDist - SubjectDist1 + 1
% 	; get_intervening_npu_counts(Analysis, FirstPos, IndicatorPos,
% 				     0, SubjectMaxDist, 0, SubjectDist)
% 	),
% 	( SecondPos > IndicatorPos ->
% 	  get_intervening_npu_counts(RevAnalysis, SecondPos, IndicatorPos,
% 				     0, ObjectMaxDist, 0, ObjectDist1),
% 	  ObjectDist is ObjectMaxDist - ObjectDist1 + 1
% 	; get_intervening_npu_counts(Analysis, SecondPos, IndicatorPos,
% 				     0, ObjectMaxDist, 0, ObjectDist)
% 	).
% 
% compute_distances_3( (IndicatorPos, Analysis, RevAnalysis, FirstPos, SecondPos),
% 		     (SubjectMaxDist, SubjectDist, ObjectMaxDist, ObjectDist) ) :-
% 	  % Argument is in the same MSU as the indicator
% 	( FirstPos =:= IndicatorPos ->
% 	  SubjectDist is 1,
% 	  get_intervening_npu_counts(RevAnalysis, FirstPos, IndicatorPos,
% 				     0, SubjectMaxDist1, 0, _),
% 	  SubjectMaxDist is SubjectMaxDist1 + 1
% 	  % Argument's MSU follows the indicator's MSU
% 	; FirstPos > IndicatorPos ->
% 	  get_intervening_npu_counts(RevAnalysis, FirstPos, IndicatorPos,
% 				     0, SubjectMaxDist1, 0, SubjectDist1),
% 	  SubjectMaxDist is SubjectMaxDist1 + 1,
% 	  SubjectDist is SubjectMaxDist - SubjectDist1 + 1
% 	  % Argument's MSU precedes the indicator's MSU
% 	; get_intervening_npu_counts(Analysis, FirstPos, IndicatorPos,
% 				     0, SubjectMaxDist, 0, SubjectDist)
%         ),
% 	( SecondPos =:= IndicatorPos ->
% 	  ObjectDist is 1,
% 	  get_intervening_npu_counts(RevAnalysis, SecondPos, IndicatorPos,
% 				     0, ObjectMaxDist1, 0, _),
% 	  ObjectMaxDist is ObjectMaxDist1 + 1
% 	; SecondPos > IndicatorPos ->
% 	  get_intervening_npu_counts(RevAnalysis, SecondPos, IndicatorPos,
% 				     0, ObjectMaxDist1, 0, ObjectDist1),
% 	  ObjectMaxDist is ObjectMaxDist1 + 1,
% 	  ObjectDist is ObjectMaxDist - ObjectDist1 + 1
% 	; get_intervening_npu_counts(Analysis, SecondPos, IndicatorPos,
% 				     0, ObjectMaxDist, 0, ObjectDist)
%         ).

% ---------- Get Argument Position ---------------
% Get the msu_index of ThisMSU
% We should consider including the msu_index(_) term in the headlist returned by get_arguments
% get_msu_index([ThisMSU|_], ItemIndex, MSUIndex, Item) :-
% 	match_msu_element(ThisMSU, ItemIndex, Item), 
% 	!,
% 	get_from_list(msu_index, ThisMSU, MSUIndex).
% get_msu_index([_|MoreMSU], Index, MSUIndex, Item) :-
% 	get_msu_index(MoreMSU, Index, MSUIndex, Item).

% match_msu_element([MSUElement|_RestMSU], Index, MSUElement) :-
% 	functor(MSUElement, _ElementType, 1),
% 	arg(1,MSUElement, ItemList),	
% 	memberchk(index(Index), ItemList).
% match_msu_element([_|RestMSU], Index, MSUElement) :-
% 	match_msu_element(RestMSU, Index, MSUElement).

% ---------- Get Intervening NPU counts ----------
% compute the # of NPUs
% (1) on this side of the argument ThisMSU (TargetMaxDistOut), and
% (2) between the given argument ThisMSU and indicator (TargetDistOut)

% ask Halil: isn't TargetMaxDist the # of NPUs on this side of INDICATOR, and not ARGUMENT?

% If we get to the indicator position, then just return current distance counts.
% get_intervening_npu_counts([ThisMSU|RestMSUx], ArgPos, IndicatorPos,
%                            TargetMaxDistIn, TargetMaxDistOut,
%                            TargetDistIn, TargetDistOut) :-
% 	get_from_list(msu_index, ThisMSU, MSUIndex),
% 	get_intervening_npu_counts_1([ThisMSU|RestMSUx], ArgPos, IndicatorPos, MSUIndex,
%                            TargetMaxDistIn, TargetMaxDistOut,
%                            TargetDistIn, TargetDistOut).
% 
% get_intervening_npu_counts_1([ThisMSU|RestMSUs], ArgPos, IndicatorPos, MSUIndex,
%                            TargetMaxDistIn, TargetMaxDistOut,
%                            TargetDistIn, TargetDistOut) :-
%           % If we are at the indicator position, then just return current distance counts.
% 	( MSUIndex =:= IndicatorPos ->
% 	  TargetMaxDistIn = TargetMaxDistOut,
% 	  TargetDistIn = TargetDistOut
% 	  % Otherwise increment counts if we're at an MSU
% 	; increment_distances(ThisMSU, ArgPos, MSUIndex,
% 			    TargetMaxDistIn, TargetMaxDistNext,
% 			    TargetDistIn, TargetDistNext),
% 	  get_intervening_npu_counts(RestMSUs, ArgPos, IndicatorPos,
% 				       TargetMaxDistNext, TargetMaxDistOut,
% 				       TargetDistNext, TargetDistOut)
% 	).

% increment_distances(ThisMSU, ArgPos, MSUIndex,
% 		    TargetMaxDistIn, TargetMaxDistNext,
% 		    TargetDistIn, TargetDistNext) :-
% 	% If ThisMSU is an NPU, then increment TargetMaxDist (# of NPUs before argument)
% 	( get_from_list(head, ThisMSU, _) ->
%           TargetMaxDistNext is TargetMaxDistIn + 1,
% 	    % If we're reached the Argument's position,
% 	    % then start incrementing TargetDist (# of NPUs between argument and indicator)
% 	  ( MSUIndex >= ArgPos ->
%             TargetDistNext is TargetDistIn + 1
% 	  ; TargetDistNext is TargetDistIn
%           )
% 	  % If ThisMSU is NOT an NPU, then don't increase either count
%         ; TargetMaxDistNext is TargetMaxDistIn,
% 	  TargetDistNext is TargetDistIn
% 	).

% Instantiate the distance fields of the  coordinated predications
% Assumes that coordinated predications will appear consecutively. Is this always right?
% Coordinated subjects
% update_coordinated_predications([FirstPredication|Rest], Predication,
% 	                        [FirstPredicationUpdated|RestUpdated]) :-
% 	Predication = SubjMaxDist-SubjDist-_SubjectIndex-
% 	              _SubjectMetaConc-_SubjectCUI-_SubjectSemTypeList-_SubjectSemType-
% 	              IndType-Relation-IndicatorIndex-
% 	              ObjMaxDist-ObjDist-ObjectIndex-
% 	              ObjectMetaConc-ObjectCUI-ObjectSemTypeList-ObjectSemType,
%         FirstPredication = _SubjMaxDist1-_SubjDist1-SubjectIndex1-
% 	                   SubjectMetaConc1-SubjectCUI1-SubjectSemTypeList1-SubjectSemType1-
% 	                   _IndType1-Relation-IndicatorIndex-
% 	                   _ObjMaxDist1-_ObjDist1-ObjectIndex-
% 	                   ObjectMetaConc-ObjectCUI-ObjectSemTypeList-ObjectSemType,
%         FirstPredicationUpdated = SubjMaxDist-SubjDist-SubjectIndex1-
% 	                          SubjectMetaConc1-SubjectCUI1-SubjectSemTypeList1-SubjectSemType1-
% 	                          IndType-Relation-IndicatorIndex-
% 	                          ObjMaxDist-ObjDist-ObjectIndex-
% 	                          ObjectMetaConc-ObjectCUI-ObjectSemTypeList-ObjectSemType,
% 	update_coordinated_predications(Rest, Predication, RestUpdated). 
% 
% %Coordinated objects
% update_coordinated_predications([FirstPredication|Rest], Predication,
% 	                        [FirstPredicationUpdated|RestUpdated]) :-
% 	Predication = SubjMaxDist-SubjDist-SubjectIndex-
% 	              SubjectMetaConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-
% 	              IndType-Relation-IndicatorIndex-
% 	              ObjMaxDist-ObjDist-_ObjectIndex-
% 	              _ObjectMetaConc-_ObjectCUI-_ObjectSemTypeList-_ObjectSemType,
%         FirstPredication = _SubjMaxDist1-_SubjDist1-SubjectIndex-
% 	                   SubjectMetaConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-
% 	                   _IndType1-Relation-IndicatorIndex-
% 	                   _ObjMaxDist1-_ObjDist1-ObjectIndex1-
% 	                   ObjectMetaConc1-ObjectCUI1-ObjectSemTypeList1-ObjectSemType1,
%         FirstPredicationUpdated = SubjMaxDist-SubjDist-SubjectIndex-
% 	                          SubjectMetaConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-
% 	                          IndType-Relation-IndicatorIndex-
% 	                          ObjMaxDist-ObjDist-ObjectIndex1-
% 	                          ObjectMetaConc1-ObjectCUI1-ObjectSemTypeList1-ObjectSemType1,
% 	update_coordinated_predications(Rest, Predication, RestUpdated). 
% End of coordinated predications
% update_coordinated_predications(Rest, _Predication, Rest):- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% format_all_predications_full_fielded %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% format_all_predications_full_fielded([], _Domain, _InputText, _OutputStream,
% 				     _MetaConcs, _GenPhenoms, 
% 				     _IndexCharPositionList,
% 				     _UNExpandedInputText,
% 				     _PMID, _TiOrAb, _SentenceID, _UtteranceType,
% 				     []).
% format_all_predications_full_fielded([FirstPredication|RestPredications],
% 				     Domain, ChosenInputText, OutputStream,
% 				     MetaConcs, GenPhenoms, 
% 				     IndexCharPositionList,
% 				     UNExpandedInputText,
% 				     PMID, TiOrAb, SentenceID, UtteranceType,
% 				     [FormattedFirstPredication|FormattedRestPredications]) :-
% 	format_one_predication_full_fielded_wrapper(FirstPredication, Domain,
% 						    ChosenInputText, OutputStream,
% 						    MetaConcs, GenPhenoms, 
% 						    IndexCharPositionList,
% 						    UNExpandedInputText,
% 						    PMID, TiOrAb, SentenceID, UtteranceType,
% 						    FormattedFirstPredication),
% 	format_all_predications_full_fielded(RestPredications, Domain,
% 					     ChosenInputText, OutputStream,
% 					     MetaConcs, GenPhenoms, 
% 					     IndexCharPositionList,
% 					     UNExpandedInputText,
% 					     PMID, TiOrAb, SentenceID, UtteranceType,
% 					     FormattedRestPredications).

% format_one_predication_full_fielded(scale-ScaleName-Index, _Domain,
% 				    _MetaConcs, _GenPhenoms, 
% 				    IndexCharPositionList,
% 				    UNExpandedInputText,
% 				    TiOrAb, SentenceID, UtteranceType,
% 				    '2entity'-(UtteranceType-TiOrAb-SentenceID-entity-
% 					       CUI-MetaConc-SemTypeListAtom-
% 	                                       NormGeneID-NormGeneName-InputText-
% 	                                       Change-Degree-Negation-
% 					    Confidence-StartPos-EndPos)) :-
% 	!,
% 	set_empty_semrep_value(_, SemTypeListAtom),
% 	set_empty_semrep_value(_, NormGeneID),
% 	set_empty_semrep_value(_, NormGeneName),
% 	get_all_concept_cuis(ScaleName, CUI),
% 	MetaConc = FakeMetaConc,
%         % The next three fields are Carol-Friedman-specific
% 	set_empty_semrep_value(change, Change),
% 	set_empty_semrep_value(degree, Degree),
% 	set_empty_semrep_value(negation,    Negation),
% 	set_empty_semrep_value(confidence,  Confidence),
% 	get_index_start_and_end_pos(Index, IndexCharPositionList, StartPos, EndPos),
% 	get_entity_atom(Index, IndexCharPositionList, 
% 	                _CitationTextAtom, UNExpandedInputText, InputText),
% 	concat_atom(['<<', ScaleName, '>>'], FakeMetaConc).
% 
% format_one_predication_full_fielded(Predication, _Domain,
% 				    MetaConcs, GenPhenoms, 
% 				    IndexCharPositionList,
% 				    UNExpandedInputText,
% 				    TiOrAb, SentenceID, UtteranceType,
% 				    FormattedPredication) :-
% 	  format_semgen_predication(Predication,MetaConcs, GenPhenoms,
% 	                            IndexCharPositionList, 
% 				    UNExpandedInputText, 
% 				    TiOrAb, SentenceID, UtteranceType,
% 				    FormattedPredication).

% If Metaconc is a-b-c-d-e-f, it's a genphenom;
% if Metaconc is a-b,         it's a disorder.

% format_semgen_predication(SubjectMaxDist-SubjectDist-SubjectIndex-
% 			  SubjectMetaConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-
% 			  IndicatorType-Relation-IndicatorIndex-
% 			  ObjectMaxDist-ObjectDist-ObjectIndex-
% 			  ObjectMetaConc-ObjectCUI-ObjectSemTypeList-ObjectSemType,
% 			  MetaConcs, GenPhenoms, 
% 			  IndexCharPositionList,
% 			  UNExpandedInputText,
% 			  TiOrAb, SentenceID, UtteranceType,
% 			  FormattedPredication) :-
% 	!,
% 	FormattedPredication = '3relation'-(UtteranceType-TiOrAb-SentenceID-relation-
% 					    SubjectMaxDist-SubjectDist-
% 					    RealSubjectCUI-RealSubjectMetaConc-
% 					    SubjectSemTypeListAtom-SubjectSemType-
% 	                                    SubjectNormGeneID-SubjectNormGeneName-SubjectAtom-
% 					    SubjectChange-SubjectDegree-SubjectNegation-
% 					    SubjectConfidenceScore-SubjectStartPos-SubjectEndPos-
% 
% 					    IndicatorType-NewRelation-RelationNegation-
% 					    RelationStartPos-RelationEndPos-
% 
% 					    ObjectMaxDist-ObjectDist-
% 					    RealObjectCUI-RealObjectMetaConc-
% 					    ObjectSemTypeListAtom-ObjectSemType-
% 	                                    ObjectNormGeneID-ObjectNormGeneName-ObjectAtom-
% 					    ObjectChange-ObjectDegree-ObjectNegation-
% 					    ObjectConfidenceScore-ObjectStartPos-ObjectEndPos),
% 
%         % The next six fields are Carol-Friedman-specific
% 	set_empty_semrep_value(change,   SubjectChange),
% 	set_empty_semrep_value(degree,   SubjectDegree),
% 	set_empty_semrep_value(negation, SubjectNegation),
% 	set_empty_semrep_value(change,   ObjectChange),
% 	set_empty_semrep_value(degree,   ObjectDegree),
% 	set_empty_semrep_value(negation, ObjectNegation),
% 
% 	concat_atom(SubjectSemTypeList, ',', SubjectSemTypeListAtom),
% 	concat_atom(ObjectSemTypeList,  ',', ObjectSemTypeListAtom),
% 
% 	update_entity_fields(IndexCharPositionList, GenPhenoms,
% 	                     UNExpandedInputText,
% 	                     SubjectCUI, SubjectMetaConc, SubjectIndex, SubjectSemTypeList, 
% 			     RealSubjectCUI, RealSubjectMetaConc, 
% 			     SubjectNormGeneID, SubjectNormGeneName, SubjectAtom,
% 			     SubjectStartPos, SubjectEndPos),
% 	update_entity_fields(IndexCharPositionList, GenPhenoms,
%                              UNExpandedInputText,
% 	                     ObjectCUI, ObjectMetaConc, ObjectIndex, ObjectSemTypeList,
% 			     RealObjectCUI, RealObjectMetaConc,
% 			     ObjectNormGeneID, ObjectNormGeneName, ObjectAtom,
% 			     ObjectStartPos, ObjectEndPos),
% 
% 	get_index_start_and_end_pos(IndicatorIndex, IndexCharPositionList,
% 				    RelationStartPos, RelationEndPos),
% 
% 	memberchk((SubjectMetaConc:_SubjectCUI:_SubjectSemTypeList)-SubjectIndex-SubjectConfidenceScore,
% 		   MetaConcs),
%         memberchk((ObjectMetaConc:_ObjectCUI:_ObjectSemTypeList)-ObjectIndex-ObjectConfidenceScore,
% 	           MetaConcs),
% 	negate_relation_if_necessary(Relation, NewRelation, RelationNegation).
% 
% format_semgen_predication(_Predication,
% 			  _MetaConcs, _GenPhenoms_, 
% 			  _IndexCharPositionList,
% 			  _UNExpandedInputText,
% 			  _TiOrAb, _SentenceID, _UtteranceType,
% 			  ''-'').

%format_semrep_predication(SubjectMaxDist-SubjectDist-SubjectIndex-
%			  SubjectMetaConc-SubjectSemTypeList-SubjectSemType-
%			  Relation-IndicatorIndex-
%			  ObjectMaxDist-ObjectDist-ObjectIndex-
%			  ObjectMetaConc-ObjectSemTypeList-ObjectSemType,

%			  MetaConcs, _GenPhenoms, IndexCharPositionList,
%			  TiOrAb, SentenceID, UtteranceType,

%			  FormattedPredication) :-
%	FormattedPredication = '5relation'-(UtteranceType-TiOrAb-SentenceID-relation-
%					    SubjectMaxDist-SubjectDist-
%					    RealSubjectMetaConc-
%					    SubjectSemTypeListAtom-SubjectSemType-
%					    SubjectCUI-
%					    SubjectNormGeneID-SubjectNormGeneName-SubjectGeneName-
%					    SubjectGenPhenomOrDisorderAtom-
%					    SubjectGeneFunctionList-
%					    SubjectChange-SubjectDegree-SubjectNegation-
%					    SubjectConfidenceScore-SubjectStartPos-SubjectEndPos-
%					    IndicatorType-NewRelation-RelationNegation-
%					    RelationStartPos-RelationEndPos-
%					    ObjectMaxDist-ObjectDist-
%					    RealObjectMetaConc-
%					    ObjectSemTypeListAtom-ObjectSemType-
%					    ObjectCUI-
%					    ObjectNormGeneID-ObjectNormGeneName-ObjectGeneName-
%					    ObjectGenPhenomOrDisorderAtom-
%					    ObjectGeneFunctionList-
%					    ObjectChange-ObjectDegree-ObjectNegation-
%					    ObjectConfidenceScore-ObjectStartPos-ObjectEndPos),

        % The next fields are Carol-Friedman-specific
%	set_empty_semrep_value(_, SubjectMaxDist),
%	set_empty_semrep_value(_, SubjectDist),

%	set_empty_semrep_value(_,   ObjectMaxDist),
%	set_empty_semrep_value(_,   ObjectDist),

%	set_empty_semrep_value(change,   SubjectChange),
%	set_empty_semrep_value(degree,   SubjectDegree),
%	set_empty_semrep_value(negation, SubjectNegation),
%	set_empty_semrep_value(change,   ObjectChange),
%	set_empty_semrep_value(degree,   ObjectDegree),
%	set_empty_semrep_value(negation, ObjectNegation),

%	concat_atom(SubjectSemTypeList, ',', SubjectSemTypeListAtom),
%	concat_atom(ObjectSemTypeList,  ',', ObjectSemTypeListAtom),

%	set_genphenom_or_disorder_fields(SubjectMetaConc,
%					 SubjectNormGeneID, SubjectNormGeneName, SubjectGeneName,
%					 SubjectGenPhenomOrDisorderAtom,
%					 SubjectCUI, RealSubjectMetaConc, SubjectGeneFunctionList),
%	set_genphenom_or_disorder_fields(ObjectMetaConc,
%					 ObjectNormGeneID, ObjectNormGeneName, ObjectGeneName,
%					 ObjectGenPhenomOrDisorderAtom,
%					 ObjectCUI, RealObjectMetaConc, ObjectGeneFunctionList),

%	set_empty_semrep_value(_, IndicatorType), % for consistency with SemGen
	% convert_metaconc(SubjectMetaConc, ConvertedSubjectMetaConc),
	% convert_metaconc(ObjectMetaConc,  ConvertedObjectMetaConc),
%	negate_relation_if_necessary(Relation, NewRelation, RelationNegation),
%	get_index_start_and_end_pos(SubjectIndex, IndexCharPositionList,
%				    SubjectStartPos, SubjectEndPos),
%	memberchk((SubjectMetaConc:_SubjectSemTypeList)-SubjectIndex-SubjectConfidenceScore,
%		  MetaConcs),
%	get_augmented_concept_cui(SubjectMetaConc, SubjectCUI),

%	get_index_start_and_end_pos(IndicatorIndex, IndexCharPositionList,
%				    RelationStartPos, RelationEndPos),
%	get_index_start_and_end_pos(ObjectIndex, IndexCharPositionList,
%				    ObjectStartPos, ObjectEndPos),
%	get_augmented_concept_cui(ObjectMetaConc, ObjectCUI),
%	memberchk((ObjectMetaConc:_ObjectSemTypeList)-ObjectIndex-ObjectConfidenceScore,
%		  MetaConcs).

% split_subject_max_dist(SubjectMaxDist, IndicatorType, RealSubjectMaxDist) :-
% 	( var(SubjectMaxDist) ->
% 	  RealSubjectMaxDist = SubjectMaxDist,
% 	  set_empty_semrep_value(_, IndicatorType)
% 	; split_subject_max_dist_1(SubjectMaxDist, IndicatorType, RealSubjectMaxDist)
% 	).
% 
% split_subject_max_dist_1(IndicatorType-RealSubjectMaxDist, IndicatorType, RealSubjectMaxDist) :- !.
% split_subject_max_dist_1(SubjectMaxDist, '', SubjectMaxDist).

% negate_relation_if_necessary(Relation, NewRelation, RelationNegation) :-
% 	atom_codes(Relation, RelationChars),
% 	%                   N  E  G  _
% 	( RelationChars = [78,69,71,95|NewRelationChars] ->
% 	  atom_codes(NewRelation, NewRelationChars),
% 	  RelationNegation = 'negation'
% 	; NewRelation = Relation,
% 	  set_empty_semrep_value(_, RelationNegation)
% 	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% format_full_fielded_entity_list %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% format_full_fielded_entity_list([], _MetaConcList, _GenPhenomList, 
% 				_IndexCharPositionList, _UNExpandedInputText, 
% 				_TiOrAb, _SentenceID, _UtteranceType, Rest, Rest).
% format_full_fielded_entity_list([FirstMetaConc|RestMetaConcs], MetaConcList, GenPhenomList,
% 				IndexCharPositionList, UNExpandedInputText,
% 				TiOrAb, SentenceID, UtteranceType,
% 				[FirstFullFieldedConceptStructure|RestFullFieldedConceptStructure], Tail) :-
% 	format_one_full_fielded_entity_structure(FirstMetaConc, MetaConcList, GenPhenomList, 
% 						 IndexCharPositionList,
% 						 UNExpandedInputText,
% 						 TiOrAb, SentenceID, UtteranceType,
% 						 FirstFullFieldedConceptStructure),
% 	format_full_fielded_entity_list(RestMetaConcs, MetaConcList, GenPhenomList, 
% 					IndexCharPositionList,
% 					UNExpandedInputText,
% 					TiOrAb, SentenceID, UtteranceType,
% 					RestFullFieldedConceptStructure, Tail).
% format_full_fielded_entity_list([_FirstMetaConc|RestMetaConcs], MetaConcList, GenPhenomList,
% 				IndexCharPositionList,
% 				UNExpandedInputText,
% 				TiOrAb, SentenceID, UtteranceType,
% 				RestFullFieldedConceptStructure, Tail) :-
% 	format_full_fielded_entity_list(RestMetaConcs, MetaConcList, GenPhenomList, 
% 					IndexCharPositionList,
% 					UNExpandedInputText,
% 					TiOrAb, SentenceID, UtteranceType,
% 					RestFullFieldedConceptStructure, Tail).
% 
% format_one_full_fielded_entity_structure((MetaConc:CUI:SemTypeList)-Index-Confidence,
% 					 MetaConcList, GenPhenomList, 
% 					 IndexCharPositionList, 
% 					 UNExpandedInputText,
% 					 TiOrAb, SentenceID, UtteranceType,
% 					 '2entity'-(UtteranceType-TiOrAb-SentenceID-entity-
% 					     RealCUI-RealMetaConc-SemTypeListAtom-
% 	                                     NormGeneID-NormGeneName-InputText-
% 					     Change-Degree-Negation-
% 					     Confidence-StartPos-EndPos)) :-
%        % The next three fields are Carol-Friedman-specific
% 	set_empty_semrep_value(change, Change),
% 	set_empty_semrep_value(degree, Degree),
% 	set_empty_semrep_value(negation, Negation),
% 	concat_atom(SemTypeList, ',', SemTypeListAtom),
% 
% 	update_entity_fields(IndexCharPositionList, GenPhenomList,
% 	                     UNExpandedInputText, 
% 			     CUI, MetaConc, Index, SemTypeList, 
% 			     RealCUI, RealMetaConc, 
% 			     NormGeneID, NormGeneName, InputText,
% 			     StartPos, EndPos),	
%         % ensure that the concept created from Entrezgene is not repeated 
%         % if it is already used in addition to a MetaConc.
%         entrezgene_entity_not_used(MetaConcList, Index, CUI, NormGeneID).

% entrezgene_entity_not_used(MetaConcList, Index, CUI, NormGeneID) :-
% %	( CUI \== '' % umls mt concept
% 	( midstring(CUI,'C',_Front,0,1) 
%         ; NormGeneID == '' % ctrial concept
%         ; count_metaconcs_in_index(MetaConcList, Index, MetaConcLen, 0),
% 	  MetaConcLen == 1
%         ).

% count the metaconcs in the MSU item with the given index.
% count_metaconcs_in_index([], _Index, Len, Len) :- !.
% count_metaconcs_in_index([(_MetaConc:SemTypeList)-Index-_Confidence|RestMetaConc], Index,
% 	                LenOut, LenIn) :-
% 	intersect([aapp,gngm], SemTypeList),
% 	!,
% 	LenNext is LenIn + 1,
% 	count_metaconcs_in_index(RestMetaConc, Index, LenOut,LenNext).
% count_metaconcs_in_index([_|RestMetaConc],Index, LenOut, LenIn) :-
% 	count_metaconcs_in_index(RestMetaConc, Index, LenOut, LenIn).

% set_empty_semrep_value(_, Field) :-
% 	( var(Field) ->
% 	  Field = ''
% 	; true
% 	).

% The GenPhenoms argument in get_metaconcs_from_tree/4 is not used
get_metaconcs_from_tree([], _GenPhenoms, MetaConcs, MetaConcs).
get_metaconcs_from_tree([FirstMSU|RestMSUs], GenPhenoms, MetaConcsIn, MetaConcsOut) :-
	FirstMSU = [H|T],
	get_metaconcs_from_MSU(T, H, GenPhenoms, MetaConcsIn, MetaConcsNext, ConfidenceScore),
	set_unknown_confidence(MetaConcsIn, ConfidenceScore),
	get_metaconcs_from_tree(RestMSUs, GenPhenoms, MetaConcsNext, MetaConcsOut).

get_metaconcs_from_MSU([], Last, GenPhenoms, MetaConcsIn, MetaConcsOut, ConfidenceScore) :-
	% This call will get the confidence score
	get_metaconcs_from_MSU_element(Last, GenPhenoms, MetaConcsIn, MetaConcsOut, ConfidenceScore).
get_metaconcs_from_MSU([Next|T], First, GenPhenoms, MetaConcsIn, MetaConcsOut, ConfidenceScore) :-
	get_metaconcs_from_MSU_element(First, GenPhenoms, MetaConcsIn, MetaConcsNext, ConfidenceScore),
	get_metaconcs_from_MSU(T, Next, GenPhenoms, MetaConcsNext, MetaConcsOut, ConfidenceScore).

get_metaconcs_from_MSU_element(Element, _GenPhenoms, MetaConcsIn, MetaConcsOut, ConfidenceScore) :-
	( Element = confid(ConfidenceScore) ->
	  MetaConcsOut = MetaConcsIn
	; arg(1, Element, List),
	  get_from_list(metaconc, List, MetaConcList) ->
	  get_from_list(index,    List, Index),
	  get_negated_value(List, Negated),
	  add_index_confid_and_neg(MetaConcList,
				   Index, ConfidenceScore, Negated,
				   MetaConcsIn, MetaConcsOut)
	; MetaConcsOut = MetaConcsIn
	).

% Some MSU elements can have a metaconc(_) but no negated(_), e.g.,:
% det([index(90),lexmatch([all]),inputmatch(['All']),tag(det),tokens([all]),
%      position(629,632),bases([all]),usemtype(['Gene or Genome']),ausemtype([gngm]),
%      semgroup([gene]),metaconc(['BCR':'613':[gngm]]),genphenom([['613'],['BCR'],[gngm]])]),

get_negated_value(List, Negated) :-
	( get_from_list(negated,  List, Negated) ->
	  true
	; Negated = 0
	).

% Remove genphenoms posing as metaconcs from the metaconc list, 
% because they may mess up formatting the entities.
% remove_genphenom_from_metaconcs([], _Index, _GenPhenoms, []).
% remove_genphenom_from_metaconcs([MetaConc:_CUI:SemTypeList|RestMetaConc], Index, GenPhenoms, NewMetaConcs) :- 
% 	% if genphenom and metaconc index are the same and the names are same
% 	% is this sufficient? 
% 	get_matching_genphenom(GenPhenoms, Index, SemTypeList, _GeneID, MetaConc),
% 	remove_genphenom_from_metaconcs(RestMetaConc, Index, GenPhenoms, NewMetaConcs).
% remove_genphenom_from_metaconcs([MetaConc:CUI:SemTypeList|RestMetaConc], Index, GenPhenoms, 
% 	                        [MetaConc:CUI:SemTypeList|RestNewMetaConcs]) :-
% 	remove_genphenom_from_metaconcs(RestMetaConc, Index, GenPhenoms, RestNewMetaConcs).

% Each element in [H|T] is a term of the form 'Effect':[qlco].
% Add to each of these terms the index num of the parse tree and the confidence score.
% We end up with a list of terms of the form 'Effect':[qlco]-Index-Confidence.
% Note that the confidence score will be instantiated later, by the final call to
% get_metaconcs_from_MSU_element/4.

add_index_confid_and_neg([], _Index, _ConfidenceScore, _Negated, MetaConcs, MetaConcs).
add_index_confid_and_neg([H|T], Index, ConfidenceScore, Negated,
			 [H-Index-ConfidenceScore-Negated|RestMetaConcs], MetaConcs) :-
	add_index_confid_and_neg(T, Index, ConfidenceScore, Negated, RestMetaConcs, MetaConcs).


get_genphenoms_from_tree([], GenPhenoms, GenPhenoms) :- !.
get_genphenoms_from_tree([FirstMSU|RestMSUs], GenPhenomsIn, GenPhenomsOut) :-
	FirstMSU = [H|T],
	get_genphenoms_from_MSU(T,H, GenPhenomsIn, GenPhenomsNext),
	!,
	get_genphenoms_from_tree(RestMSUs, GenPhenomsNext, GenPhenomsOut).

get_genphenoms_from_MSU([],Last,GenPhenomsIn,GenPhenomsOut):-
	get_genphenoms_from_MSU_element(Last,GenPhenomsIn,GenPhenomsOut).
get_genphenoms_from_MSU([Next|T],First,GenPhenomsIn, GenPhenomsOut) :-
	get_genphenoms_from_MSU_element(First, GenPhenomsIn, GenPhenomsNext),
	get_genphenoms_from_MSU(T,Next,GenPhenomsNext,GenPhenomsOut).

get_genphenoms_from_MSU_element(Element, GenPhenomsIn, GenPhenomsOut) :-
	( Element = confid(_) ->
	  GenPhenomsOut = GenPhenomsIn
	; arg(1, Element, List),
	  get_from_list(genphenom, List, GenPhenomList) ->
	  get_from_list(index, List, Index),
	  add_index_num(GenPhenomList, Index, GenPhenomsIn, GenPhenomsOut)
        ; GenPhenomsOut = GenPhenomsIn
        ).

add_index_num([],_Index,GenPhenoms,GenPhenoms).
add_index_num(GenPhenomList, Index, [GenPhenomWithIndex|RestGenPhenoms], GenPhenoms) :-
	GenPhenomList = [GenPhenomIDList|Rest],
	GenPhenomIDList = [GenPhenomID],
	Rest = [GenPhenomNameList|Rest1],
	GenPhenomNameList = [GenPhenomName],
	Rest1 = [GenPhenomSemTypeList|T],
	GenPhenomWithIndex = GenPhenomID:GenPhenomName:GenPhenomSemTypeList-Index,
	add_index_num(T,Index,RestGenPhenoms,GenPhenoms).

% Now that Clinical Trials concepts have the full panoply
% of ausemtype(...) etc. elements, this predicate is longer neededfor CTG;
% however, it is still necessary for metaconcs created from genphenoms
set_unknown_confidence(MetaConcsIn, ConfidenceScore) :-
	( nonvar(MetaConcsIn),
	  var(ConfidenceScore) ->
	  ConfidenceScore = 1000
	; true
	).

