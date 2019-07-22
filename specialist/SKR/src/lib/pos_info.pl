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

% File:     pos_info.pl
% Module:   SKR Positional Information
% Author:   FML
% Purpose:  Calculate positional information for tokens


:- module(pos_info, [
	adjust_AAs_pos_info/4,
	collapse_pos_info/3,
	create_EXP_raw_token_list/7,
	create_UNEXP_raw_token_list/6,
        get_next_token_state/3
    ]).

:- use_module(metamap(metamap_tokenization), [
	local_ws/1
    ]).

:- use_module(skr_lib(nls_strings),[
	trim_whitespace_left/2,
	trim_whitespace_left_count/3,
	trim_whitespace_left_1/4
    ]).


:- use_module(skr(skr_text_processing),[
	text_field/1
    ]).

:- use_module(skr(skr_utilities),[
	fatal_error/2,
	send_message/2,
	token_template/5,
	token_template/6,
	write_token_list/3
    ]).

% :- use_module(skr_lib(ctypes),[
% 	is_space/1
%     ]).

:- use_module(text(text_object_util),[
	aa_tok/1,
	an_pn_xx_tok/1,
	aadef_tok/1,
	ex_lbracket_char/1,
	ex_lbracket_tok/1,
	ex_rbracket_char/1,
	ex_rbracket_tok/1,
	higher_order_or_annotation_tok/1,
	higher_order_or_annotation_type/1,
	hyphen_punc/1,
	pe_tok/1,
	punc_tok/1,
	token_sequence_length/4,
	ws_tok/1,
	ws_char/1
   ]).

:- use_module(text(text_objects), [
	maybe_dump_AAs/3
   ]).

:- use_module(library(avl), [
	avl_member/3
    ]).

:- use_module(library(lists),[
	append/2,
	append_length/3,
	last/2,
	rev/2,
	sublist/4,
	sublist/5
    ]).


% one-off: aa aadef ws
% hoa: field label sn pe
% an/pn: an ic lc mc nu pn uc
create_EXP_raw_token_list([], _UDA_AVL, _PreviousToken, _CurrentPos, _TokenState, _InputString, []).
create_EXP_raw_token_list([CurrentToken|RestTokensIn], UDA_AVL, PreviousToken,
			  CurrentPos, TokenState, InputStringIn, NewTokens) :-
	% CurrentToken  = tok(CurrentTokenType, _CurrentString, _CurrentLCString, _CurrentPI),
	% format(user_output, 'TOK: ~p~n', [CurrentToken]),
	token_template(CurrentToken, CurrentTokenType,
		       _CurrentString, _CurrentLCString, _CurrentPos),
	% format(user_output, '~w|~s|~nIN |~s|~nOUT|',
	%        [CurrentTokenType,CurrentString,InputStringIn]),
	handle_token_type(CurrentTokenType, UDA_AVL, PreviousToken, TokenState,
			  InputStringIn,   CurrentToken, CurrentPos, RestTokensIn,
			  InputStringNext, NewTokens,    NextPos,    RestTokensNext,
			  RestNewTokens),
	% write_token_list([CurrentToken], 0, 1),
	% format(user_output, 'IN |~s|~nOUT|~s|~n~n', [InputStringIn,InputStringNext]),
	% ( member(ThisToken, NewTokens),
	%   ( nonvar(ThisToken) ->
	%     skr_utilities:write_token(ThisToken, 0, 1)
	%   ; !, fail
	%   )
	% ; true
	% ),
	% format(user_output, '~s|~n', [InputStringNext]),
	get_next_token_state(TokenState, CurrentTokenType, NextTokenState),
	create_EXP_raw_token_list(RestTokensNext, UDA_AVL, CurrentToken, NextPos, NextTokenState,
				  InputStringNext, RestNewTokens).

create_UNEXP_raw_token_list([], _UDA_AVL,_CurrentPos, _TokenState, _InputString, []).
create_UNEXP_raw_token_list([CurrentToken|RestTokens], UDA_AVL, CurrentPos, TokenState,
			    InputStringIn, [NewCurrentToken|NewRestTokens]) :-
	% arg(1, CurrentToken, CurrentTokenType),
	token_template(CurrentToken, CurrentTokenType, _String, _LCString, _PosInfo),
  	( CurrentTokenType == 'ws' ->
 	  % NextPos is CurrentPos + NumBlanksRemoved,
	  InputStringIn = [WhiteSpaceChar|InputStringTemp],
	  local_ws(WhiteSpaceChar),
 	  add_raw_pos_info_1(CurrentToken, CurrentPos, NewCurrentToken),
	  % Handle blank spaces at the beginning of lines,
	  % e.g., in MedLine citations, that do not get tokenized
	  remove_untokenized_whitespace(RestTokens, InputStringTemp,
	  				InputStringNext, NumBlanksRemoved),
	  ( avl_member(_UDAShortFormToken, UDA_AVL, [UDALongFormTokenList]),
	    memberchk(CurrentToken, UDALongFormTokenList) ->
	    NextPos is CurrentPos + NumBlanksRemoved
	  ; NextPos is CurrentPos + 1 + NumBlanksRemoved
	  )
	; higher_order_or_annotation_type(CurrentTokenType) ->
	  % CurrentToken    = tok(Type, ExpTokens, AcroTokens, Pos1),
	  % NewCurrentToken = tok(Type, ExpTokens, AcroTokens, Pos1, Pos1),
	  add_raw_pos_info_1(CurrentToken, CurrentPos, NewCurrentToken),
	  % NextTokenState is TokenState,
	  NextPos is CurrentPos,
	  InputStringNext = InputStringIn
	  % for tokens that have content (uc, lc, etc.)
	; add_raw_pos_info_2(CurrentToken, an, UDA_AVL, _PrevTokenType, CurrentPos, InputStringIn, 
			     NewCurrentToken, NextPos, InputStringNext),
	  test_for_adjacency(TokenState, NewCurrentToken, CurrentPos)
        ),
	% format(user_output, 'Current Token: ~q~n', [CurrentToken]),
	% format(user_output, '    New Token: ~q~n', [NewToken]),
	get_next_token_state(TokenState, CurrentTokenType, NextTokenState),
	create_UNEXP_raw_token_list(RestTokens, UDA_AVL, NextPos, NextTokenState,
				    InputStringNext, NewRestTokens).


% aadef token:
% tok(aadef,
%     [tok(lc,heart,heart,pos(0,5)),
%      tok(ws,' ',' ',pos(5,6)),
%      tok(lc,attack,attack,pos(6,12))],
%     [tok(uc,'HA',ha,pos(14,16))],
%     pos(0,12))
% 
% aa token:
% tok(aa,
%     [tok(uc,'HA',ha,pos(26,28))],
%     [tok(aadef,
%         [tok(lc,heart,heart,pos(0,5)),
%          tok(ws,' ',' ',pos(5,6)),
%          tok(lc,attack,attack,pos(6,12))],
%         [tok(uc,'HA',ha,pos(17,19))],
%         pos(0,12))],
%     pos(26,28))

% In the clauses for handle_token_type/11, the variables are the following:

% TokenType:     The current token type.
% PrevToken:     The previous token.
% InputStringIn: The input string when handle_ws_token_is called.
% CurrentPos:    The current character position.
% RestTokens:    The list containing the remaining input tokens.
% TokenState:    The current state of the token stream: see get_next_token_state/3.
	           

% InputStringNext: The input string after processing this ws token.
% NewTokens:       The output token list.
% NextPos:         The character position afteer processing this ws token.
% RestTokensNext:  The input token list on which create_EXP_raw_token_list/5 should recurse.
%		   In this simple case, RestTokensNext is simply RestTokens.
% RestNewTokens:   The output token list on which create_EXP_raw_token_list/5 should recurse.
%		   In this simple case, RestNewTokens is simply the tail of NewTokens.

% Token types ws, aadef, and aa are handled specially.

% special_token_type(ws).
special_token_type(aa).
special_token_type(aadef).

handle_token_type(TokenType, UDA_AVL, PrevToken, TokenState, InputStringIn,
		  CurrentToken, CurrentPos, RestTokensIn,
		  InputStringNext, NewTokens,    NextPos, RestTokensNext, RestNewTokens) :-
	( TokenType == ws ->
	  handle_ws_token_type(TokenState, UDA_AVL, PrevToken,
			       InputStringIn,   CurrentToken, CurrentPos, RestTokensIn,
			       InputStringNext, NewTokens,    NextPos,    RestTokensNext,
			       RestNewTokens)
	; special_token_type(TokenType) ->
	  handle_special_token_type(TokenType, PrevToken,
				    InputStringIn,   CurrentToken, CurrentPos, RestTokensIn,
				    InputStringNext, NewTokens,    NextPos,    RestTokensNext,
				    RestNewTokens)
	; % aadef and aa are higher-order types, but they'll be caught by the first branch
	  higher_order_or_annotation_type(TokenType) ->
	  handle_hoa_type(InputStringIn,   CurrentToken, CurrentPos, RestTokensIn,
			  InputStringNext, NewTokens, NextPos, RestTokensNext, RestNewTokens)
	; handle_an_pn_type(InputStringIn, UDA_AVL, CurrentToken, CurrentPos,
			    RestTokensIn, TokenState, InputStringNext, NewTokens,
			    NextPos, RestTokensNext, RestNewTokens)
	),
	!.
handle_token_type(_TokenType, _UDA_AVL, PrevToken, _TokenState,
		  InputStringIn, CurrentToken, CurrentPos, _RestTokensIn,
		  _InputStringNext, _NewTokens, _NextPos, _RestTokensNext,
		  _RestNewTokens) :-
	fatal_error('Token ~p failed after token ~p at position ~d with input string "~s"~n',
		    [CurrentToken,PrevToken,CurrentPos,InputStringIn]).
	
% If ...
%   * the previous token was an aa whose final token is a ws, and
%   * the current input string does not begin with a ws,
% that means that the AA was something like "(TGF )" (e.g., from PMID 14704634),
% so allow that anyway.
handle_ws_token_type(_TokenState, _UDA_AVL, PrevToken, InputStringIn,
		     CurrentToken, CurrentPos, RestTokens,
		     InputStringNext, NewTokens,    NextPos,    RestTokensNext,
		     RestNewTokens) :-
	% If the input string does NOT begin with a ws char...
	InputStringIn \= [32|_],
	% ...and the previous token is an AA ...
	token_template(PrevToken, PrevTokenType,
		       _PrevTokenCurrentString, _PrevTokenCurrentLCString, _PrevTokenCurrentPos),
	PrevTokenType == aa,
	% ...whose last AA token matches the current token,
	arg(2, PrevToken, AATokens),
	last(AATokens, LastAAToken),
	token_template(LastAAToken, LastAATokenType,
		       _LastAATokenCurrentString, _LastAATokenCurrentLCString, _LastAATokenCurrentPos),
	LastAATokenType == ws,
	!,
	InputStringNext = InputStringIn,
	NextPos is CurrentPos,
	add_raw_pos_info_1(CurrentToken, CurrentPos, NewCurrentToken),
	NewTokens = [NewCurrentToken|RestNewTokens],
	RestTokensNext = RestTokens.	
handle_ws_token_type(_TokenState, UDA_AVL, _PrevToken, InputStringIn, CurrentToken, CurrentPos, RestTokens,
		     InputStringNext, NewTokens,    NextPos, RestTokensNext, RestNewTokens) :-
	InputStringIn = [WhiteSpaceChar|InputStringTemp],
	local_ws(WhiteSpaceChar),
 	add_raw_pos_info_1(CurrentToken, CurrentPos, NewCurrentToken),
	% Handle blank spaces at the beginning of lines,
	% e.g., in MedLine citations, that do not get tokenized
	remove_untokenized_whitespace(RestTokens, InputStringTemp,
				      InputStringNext, NumBlanksRemoved),
	( avl_member(_UDAShortFormToken, UDA_AVL, [UDALongFormTokenList]),	  
	  memberchk(CurrentToken, UDALongFormTokenList) ->
	  NextPos is CurrentPos
	; NextPos is CurrentPos + 1 + NumBlanksRemoved
	),
	% NewTokens is the top-level output;
	% the top-level predicate recurses on RestNewTokens.
	NewTokens = [NewCurrentToken|RestNewTokens],
	% The top-level predicate recurses on RestTokensNext,
	% which, in this case, happens to be just RestTokens.
	RestTokensNext = RestTokens.

% Here's what happens when we hit an aadef token. Suppose the text is
% With time-of-flight secondary ion mass-spectrometry (ToF
%       SIMS) measurements.
%
% The input string is
% "time-of-flight secondary ion mass-spectrometry (ToF       SIMS) measurements."
%
% The token list is
%
% [tok(aadef,
%      [tok(lc,"time","time",pos(14,18)),
%       tok(pn,"-","-",pos(18,19)),
%       tok(lc,"of","of",pos(19,21)),
%       tok(pn,"-","-",pos(21,22)),
%       tok(lc,"flight","flight",pos(22,28)),
%       tok(ws," "," ",pos(28,29)),
%       tok(lc,"secondary","secondary",pos(29,38)),
%       tok(ws," "," ",pos(38,39)),
%       tok(lc,"ion","ion",pos(39,42)),
%       tok(ws," "," ",pos(42,43)),
%       tok(lc,"mass","mass",pos(43,47)),
%       tok(pn,"-","-",pos(47,48)),
%       tok(lc,"spectrometry","spectrometry",pos(48,60))],
%      [tok(mc,"ToF","tof",pos(62,65)),
%       tok(ws," "," ",pos(65,66)),
%       tok(uc,"SIMS","sims",pos(66,70))],pos(14,60)),
%  tok(lc,"time","time",pos(14,18)),
%  tok(pn,"-","-",pos(18,19)),
%  tok(lc,"of","of",pos(19,21)),
%  tok(pn,"-","-",pos(21,22)),
%  tok(lc,"flight","flight",pos(22,28)),
%  tok(ws," "," ",pos(28,29)),
%  tok(lc,"secondary","secondary",pos(29,38)),
%  tok(ws," "," ",pos(38,39)),
%  tok(lc,"ion","ion",pos(39,42)),
%  tok(ws," "," ",pos(42,43)),
%  tok(lc,"mass","mass",pos(43,47)),
%  tok(pn,"-","-",pos(47,48)),
%  tok(lc,"spectrometry","spectrometry",pos(48,60)),
% ------------------------------------------------------- (1)
%  tok(ws," "," ",pos(60,61)),
% ------------------------------------------------------- (2)
%  tok(pe,[],1,pos(61,71)),
%  tok(pn,"(","(",pos(61,62)),
%  tok(aa,
%      [tok(mc,"ToF","tof",pos(62,65)),
%       tok(ws," "," ",pos(65,66)),
%       tok(uc,"SIMS","sims",pos(66,70))],
%      [tok(aadef,[tok(lc,"time","time",pos(14,18)),
% 		 tok(pn,"-","-",pos(18,19)),
% 		 tok(lc,"of","of",pos(19,21)),
% 		 tok(pn,"-","-",pos(21,22)),
% 		 tok(lc,"flight","flight",pos(22,28)),
% 		 tok(ws," "," ",pos(28,29)),
% 		 tok(lc,"secondary","secondary",pos(29,38)),
% 		 tok(ws," "," ",pos(38,39)),
% 		 tok(lc,"ion","ion",pos(39,42)),
% 		 tok(ws," "," ",pos(42,43)),
% 		 tok(lc,"mass","mass",pos(43,47)),
% 		 tok(pn,"-","-",pos(47,48)),
% 		 tok(lc,"spectrometry","spectrometry",pos(48,60))],
% 	        [tok(mc,"ToF","tof",pos(62,65)),
% 		 tok(ws," "," ",pos(65,66)),
% 		 tok(uc,"SIMS","sims",pos(66,70))],pos(14,60))],pos(62,70)),
%  tok(mc,"ToF","tof",pos(62,65)),
%  tok(ws," "," ",pos(65,66)),
%  tok(uc,"SIMS","sims",pos(66,70)),
%  tok(pn,")",")",pos(70,71)),
% ------------------------------------------------------- (3)
%  tok(ws," "," ",pos(71,72)),
%  tok(lc,"measurements","measurements",pos(72,84)),
%  tok(pn,".",".",pos(84,85))]
% 
% (1) create_new_tokens_and_consume_strings/9 and remove_tokens_no_consume/3
%     * matches up the AADef tokens in the
%       second arg of the aadef token ("time"..."spectrometry"),
%     * consume those tokens,
%     * create new ones with the new pos info, and
%     * remove those strings from the input string, leaving " (ToF       SIMS) measurements.";
% At this point, the remaining tokens are those below the "----- (1)" line.
% (2) consume_ws_tokens/4 removes the ws token;
% Now, the remaining tokens are those below the "----- (2)" line.
% (3) trim_whitespace_left/3 removes any whitespace
%     from the beginning of the input string, leaving "(ToF       SIMS) measurements.";
% (4) token_template/5 determines the end pos of the current pe token, i.e., 71;
% (5) consume_tokens_through_rbracket_end_pos/3 consumes all tokens through that position;
% Now, the remaining tokens are those below the "----- (3)" line.
% (6) consume_token_strings/5 then walks off the strings in the tokens
%     between "----- (2)" and "----- (3)", as follows:
%     * higher-order and annotation tokens (pe, aa) are ignored
%     * ws tokens consume the ws at the beginning of the string
%       PLUS any other leading whitespace
%     * an and pn tokens match the beginning of the input string,
%       and the tokens strings are walked off the input string.

handle_special_token_type(aadef, _PrevToken,
			  InputStringIn,   _CurrentToken, CurrentPos, RestTokensIn,
			  InputStringNext, NewTokens,    NextPos,    RestTokensNext,
			  RestNewTokens) :-
	% Special case for aadef(_) and aa(_) being consecutive tokens,
	% which means the AA is user-defined
	RestTokensIn = [FirstRestTokensIn|_],
	aa_tok(FirstRestTokensIn),
	% This next test is too strict, because of text such as in
	% PMID 16685652, simplified below:
	% DNA polymerase gamma (pol gamma ) is required.
	% pol gamma (POLG) has been linked.
	% See "### EXPLANATION ###" below for a fuller explanation.
	% arg(3, FirstRestTokensIn, [CurrentToken]),
	!,
	InputStringNext = InputStringIn,
	RestNewTokens = NewTokens,
	NextPos is CurrentPos,
	RestTokensNext = RestTokensIn.
handle_special_token_type(aadef, PrevToken,
			  InputStringIn,   CurrentToken, CurrentPos, RestTokensIn,
			  InputStringNext, NewTokens,    NextPos,    RestTokensNext,
			  RestNewTokens) :-
	arg(2, CurrentToken, AADefTokens0),
	% Add tokens in the first aadef list, e.g.,
	% tok(lc,heart,heart,pos(0,5))
	% tok(ws,' ',' ',pos(5,6))
	% tok(lc,attack,attack,pos(6,12))
	% to the list of newly created tokens -- with the addition
	% of raw positional information.
	% RestNewTokens is the uninstantiated tail of NewTokens.
	% Also, consume the strings contained in those tokens.
	% PrevToken = tok(PrevTokenType, _PrevString, _PrevLCString, _PrevPos),
	( token_template(PrevToken, PrevTokenType, _PrevString, _PrevLCString, _PrevPos) ->
	  true
	; PrevTokenType = ''
	),
	% This hack is intended to handle text like "urinary (u-) ..."
	remove_leading_hyphen_and_ws_tokens(AADefTokens0, AADefTokens1),
	% Removed this call to handle strings like
	% "Vasoactive Intestinal Peptide - (VIP) like immunoreactive nerves"
	% in PMID- 3891693
	% remove_final_hyphen_and_ws_tokens(AADefTokens1, AADefTokens),
	remove_final_ws_tokens(AADefTokens1, AADefTokens),
	% AADefTokens = AADefTokens1,
	% AADefTokens = AADefTokens0,
	create_new_tokens_and_consume_strings(AADefTokens, PrevTokenType, RestTokensIn,
					      CurrentPos, InputStringIn,
					      NewTokens, RestNewTokens, NextPos1,
					      InputString1),
	% Then, match and remove those aadef tokens from RestTokensIn;
	% the first argument must be AADefTokens1, and not AADefTokens.
	remove_tokens_no_consume(AADefTokens1, RestTokensIn, RestTokens1),
	InputString1 \== '',
	% consume_ws_tokens(RestTokens1, RestTokens2, 0, _NumWSTokens1),
	remove_leading_hyphen_and_ws_tokens(RestTokens1, RestTokens2),
	trim_ws_and_hyphens_left(InputString1, InputString2, 0, NumBlanksTrimmed1),
	NextPos2 is NextPos1 + NumBlanksTrimmed1,
	% Remove all following tokens up to and including the matching
	% exclusive right-bracket token, which can be done by
	% identifying PeTokenEndPos, the end pos of the pe token,
	% which should be the first token in RestTokens2,
	% and consuming all tokens until an ex_rbracket token
	% whose end pos is equal to PETokenEndPos.
	RestTokens2 = [PEToken|RestRestTokens2],
	% FirstRestTokens2 should be a pe tok
	% I cannot rely on the posinfo in the pe token,
	% because it's the orig pos info!
	token_template(PEToken, pe, _FirstString, _FirstLCString,
		       pos(_PETokenStartPos, PETokenEndPos)),
	consume_tokens_through_rbracket_end_pos(RestRestTokens2, PETokenEndPos, RestTokens3),
	append(TokensConsumed, RestTokens3, RestRestTokens2),
	consume_ws_tokens(RestTokens3, RestTokensNext, 0, _NumWSTokens3),
	% NumCharsToConsume is PETokenEndPos - PETokenStartPos,
	% sublist(InputString2, InputString3, NumCharsToConsume, _Length, 0),
	consume_token_strings(TokensConsumed, InputString2, InputString3, 0, NumCharsConsumed),
	NextPos3 is NextPos2 + NumCharsConsumed,
	trim_whitespace_left_count(InputString3, InputStringNext, NumBlanksTrimmed3),
	NextPos is NextPos3 + NumBlanksTrimmed3,
	!.
	% fail.
handle_special_token_type(aadef, _PrevToken,
			  InputStringIn,   CurrentToken, CurrentPos, RestTokensIn,
			  InputStringNext, NewTokens,    NextPos,    RestTokensNext,
			  RestNewTokens) :-
	send_message('### WARNING: aadef token failed:~n', []),
	write_token_list([CurrentToken], 0, 1),
	InputStringNext = InputStringIn,
	RestNewTokens = NewTokens,
	NextPos is CurrentPos,
	RestTokensNext = RestTokensIn.



% ### EXPLANATION ###: The two relevant tokens are below.
% The aadef token is CurrentToken, and the aa token is FirstRestTokensIn.
% Normally, the 3rd arg of the aa token is
% a 1-element list containing the aadef token (CurrentToken).
% However, in this case, because "pol gamma " is itself an AA for "polymerase gamma",
% the expansion of "pol gammma " is expanded to "polymerase gamma". Go figure...
% tok(aadef,
%     [tok(lc,pol,pol,pos(47,50)),
%      tok(ws,' ',' ',pos(50,51)),
%      tok(lc,gamma,gamma,pos(51,56))],
%     [tok(uc,'POLG',polg,pos(58,62))],
%     pos(47,56))
% tok(aa,
%     [tok(lc,pol,pol,pos(47,50)),
%      tok(ws,' ',' ',pos(50,51)),
%      tok(lc,gamma,gamma,pos(51,56)),
%      tok(ws,' ',' ',pos(56,57))],
%     [tok(aadef,
%         [tok(lc,polymerase,polymerase,pos(4,14)),
%          tok(ws,' ',' ',pos(14,15)),
%          tok(lc,gamma,gamma,pos(15,20))],
%         [tok(lc,pol,pol,pos(22,25)),
%          tok(ws,' ',' ',pos(25,26)),
%          tok(lc,gamma,gamma,pos(26,31)),
%          tok(ws,' ',' ',pos(31,32))],
%         pos(4,20))],
%     pos(47,57))

% Here's what happens when we hit an aa token. Suppose the text is
% AAdef:
% decolorization of diazo dye Acid Red 151 (AR
%      151) from simulated dye solution

% AA:
%     (4.0-7.0), and initial dye concentration (100-200 mg/L) on the
%     biodegradation of AR             151.


% Where "AR 151" is
% tok(aadef,
%     [tok(ic,"Acid","acid",pos(37,41)),
%      tok(ws," "," ",pos(41,42)),
%      tok(ic,"Red","red",pos(42,45)),
%      tok(ws," "," ",pos(45,46)),
%      tok(nu,"151","151",pos(46,49))],
%     [tok(uc,"AR","ar",pos(51,53)),
%      tok(ws," "," ",pos(53,54)),
%      tok(nu,"151","151",pos(54,57))],pos(37,49))
% 
% The input string is "AR 151."
% The token list is
% 
%  tok(aa,
%      [tok(uc,"AR","ar",pos(106,108)),
%       tok(ws," "," ",pos(108,109)),
%       tok(nu,"151","151",pos(121,124))],
%      [tok(aadef,
% 	  [tok(ic,"Acid","acid",pos(37,41)),
% 	   tok(ws," "," ",pos(41,42)),
% 	   tok(ic,"Red","red",pos(42,45)),
% 	   tok(ws," "," ",pos(45,46)),
% 	   tok(nu,"151","151",pos(46,49))],
% 	  [tok(uc,"AR","ar",pos(51,53)),
% 	   tok(ws," "," ",pos(53,54)),
% 	   tok(nu,"151","151",pos(54,57))],pos(37,49))],pos(106,124)),
%  tok(uc,"AR","ar",pos(106,108)),
%  tok(ws," "," ",pos(108,109)),
%  tok(ws," "," ",pos(109,110)),
%  tok(ws," "," ",pos(110,111)),
%  tok(ws," "," ",pos(111,112)),
%  tok(ws," "," ",pos(112,113)),
%  tok(ws," "," ",pos(113,114)),
%  tok(ws," "," ",pos(114,115)),
%  tok(ws," "," ",pos(115,116)),
%  tok(ws," "," ",pos(116,117)),
%  tok(ws," "," ",pos(117,118)),
%  tok(ws," "," ",pos(118,119)),
%  tok(ws," "," ",pos(119,120)),
%  tok(ws," "," ",pos(120,121)),
%  tok(nu,"151","151",pos(121,124)),
%  tok(pn,".",".",pos(124,125))]
% 
% and the current token is the aa token above.
% 
% (1) get_aa_tokens_start_and_end_pos/3 gets the starting and ending character positions
%     of the AA, e.g., 106-->124;
% (2) get_aa_tokens_length/5 gets the length of the AA, e.g., 18;
% (3) create_new_tokens_no_consume/7 creates new tokens with the updated pos info;
%     the new tokens correspond to the original tokens, but the updated pos info
%     reflects the token span of the AA;
% (4) remove_aa_tokens/4 removes the AA tokens from the remaining token list,
%     taking into account the possibility of multiple consecutive ws tokens;
% (5) consume_aa_token_strings/4 removes the strings in the AA tokens
%     from the input string.

handle_special_token_type(aa, _PrevToken,
			  InputStringIn,   CurrentToken, CurrentPos, RestTokensIn,
			  InputStringNext, NewTokens,    NextPos,    RestTokensNext,
			  RestNewTokens) :-
	  % AADefExpansionTokenList is a list containing these three tokens:
	  % tok(lc,heart,heart,pos(0,5))
	  % tok(ws,' ',' ',pos(5,6))
	  % tok(lc,attack,attack,pos(6,12))
	  arg(2, CurrentToken, AATokenList),
	  arg(3, CurrentToken, [AADefToken|_]),
	  arg(2, AADefToken,   AADefExpansionTokenList),

	  % calculate the length of the concatenation of all the AA tokens
	  % We need to march off characters in InputStringIn
	  % skipping over blanks, until we consume every string in AATokenList
	  get_aa_tokens_start_and_end_pos(AATokenList, AAStartPos, AAEndPos),          
	  % Not sure why I do this, rather than simply canculate AAEndPos - AAStartPos +1.
	  get_aa_tokens_length(AATokenList, InputStringIn, 0, 0, AALength),
	  % adjust_start_and_end_pos(InputStringIn, TempCurrentPos,
	  %			     TempAAStartPos, TempAAEndPos,
	  %			     AATokenList,
	  %			     CurrentPos, AAStartPos, AAEndPos),
	  % AALength is AAEndPos - AAStartPos,				  
	  % Create a new token for each token in the AADef Expansion
	  create_new_tokens_no_consume(AADefExpansionTokenList, CurrentPos,
	  			       AALength, AAStartPos, AAEndPos,
				       NewTokens, RestNewTokens),
	  arg(2, CurrentToken, AATokenList),
	  AATokenList = [AAH|AAT],
	  % remove from the remaining token list the AA tokens 
	  remove_aa_tokens(AAT, AAH, RestTokensIn, RestTokensNext),
	  % consume the text strings in the uc tokens from the InputString
	  consume_aa_token_strings(AAT, AAH, InputStringIn, InputStringNext),
	  % This cut can't be there because
	  % consume_aa_token_strings must be backtrackable. Ugh.
	  % !,
	  NextPos is CurrentPos + AALength.
% handle_special_token_type(aa, _PrevToken,
% 			  InputStringIn,   _CurrentToken, CurrentPos, RestTokensIn,
% 			  InputStringNext, NewTokens,     NextPos,    RestTokensNext,
% 			  RestNewTokens) :-
% 	send_message('### WARNING: aa token failed:~n', []),
% 	write_token_list([CurrentToken], 0, 1),
% 	ttyflush,
% 	InputStringNext = InputStringIn,
% 	RestNewTokens = NewTokens,
% 	NextPos is CurrentPos,
% 	RestTokensNext = RestTokensIn.

% Token types field, label, sn, pe are handled identically via handle_hoa_type.
% "hoa" == "higher-order or annotation type" (other than aadef and aa)
handle_hoa_type(InputStringIn, CurrentToken, CurrentPos, RestTokensIn,
		InputStringNext, NewTokens, NextPos, RestTokensNext, RestNewTokens) :-
	add_raw_pos_info_1(CurrentToken, CurrentPos, NewCurrentToken),
	InputStringNext = InputStringIn,
	RestTokensNext = RestTokensIn,
	NextPos is CurrentPos,
	NewTokens = [NewCurrentToken|RestNewTokens].

trim_ws_and_hyphens_left([H|T], InputStringOut, TrimmedCharCountIn, TrimmedCharCountOut) :-
	( H == 32 -> % blank space
	  TrimmedCharCountNext is TrimmedCharCountIn + 1,
	  trim_ws_and_hyphens_left(T, InputStringOut, TrimmedCharCountNext, TrimmedCharCountOut)
	; H == 45 -> % hyphen
	  TrimmedCharCountNext is TrimmedCharCountIn + 1,
	  trim_ws_and_hyphens_left(T, InputStringOut, TrimmedCharCountNext, TrimmedCharCountOut)
	; TrimmedCharCountOut is TrimmedCharCountIn,
	  InputStringOut = [H|T]
	).	


% Token types an, ic, lc, mc, nu, pn, uc are handled identically via handle_an_pn_type.
handle_an_pn_type(InputStringIn, UDA_AVL, CurrentToken, CurrentPos, RestTokensIn, TokenState,
		  InputStringNext, NewTokens, NextPos, RestTokensNext,
		  RestNewTokens) :-
	  add_raw_pos_info_2(CurrentToken, an, UDA_AVL, _PrevTokenType, CurrentPos, InputStringIn,
			     NewCurrentToken, NextPos, InputStringNext),
	  test_for_adjacency(TokenState, NewCurrentToken, NextPos),
	  NewTokens = [NewCurrentToken|RestNewTokens],
	  RestTokensNext = RestTokensIn.

consume_ws_tokens([], [], NumSpaces, NumSpaces).
consume_ws_tokens([FirstToken|RestTokens], RemainingTokens, NumSpacesIn, NumSpacesOut) :-
	( ws_tok(FirstToken) ->
	  NumSpacesNext is NumSpacesIn + 1,
	  consume_ws_tokens(RestTokens, RemainingTokens, NumSpacesNext, NumSpacesOut)
	; RemainingTokens = [FirstToken|RestTokens],
	  NumSpacesOut is NumSpacesIn
	).

remove_leading_hyphen_and_ws_tokens([H|T], AADefTokens) :-
	( hyphen_or_ws_token(H) ->
	  remove_leading_hyphen_and_ws_tokens(T, AADefTokens)
	; AADefTokens = [H|T]
	).

remove_leading_ws_tokens([H|T], AADefTokens) :-
	( ws_tok(H) ->
	  remove_leading_ws_tokens(T, AADefTokens)
	; AADefTokens = [H|T]
	).	    
	
% remove_final_hyphen_and_ws_tokens(AADefTokensIn, AADefTokensOut) :-
% 	rev(AADefTokensIn, RevAADefTokensIn0),
% 	remove_leading_hyphen_and_ws_tokens(RevAADefTokensIn0, RevAADefTokensIn1),
% 	rev(RevAADefTokensIn1, AADefTokensOut).

remove_final_ws_tokens(AADefTokensIn, AADefTokensOut) :-
	rev(AADefTokensIn, RevAADefTokensIn0),
	remove_leading_ws_tokens(RevAADefTokensIn0, RevAADefTokensIn1),
	rev(RevAADefTokensIn1, AADefTokensOut).

hyphen_or_ws_token(Token) :-
	( token_template(Token, pn, HyphenString, HyphenString, _PosInfo),
	  hyphen_punc(HyphenString) ->
	  true
	; token_template(Token, ws, _WSString, _WString, _PosInfo)
	).
	

% We have a list of tokens, e.g.,
% [tok(pn,"(","(",pos(61,62)),
%  tok(mc,"ToF","tof",pos(62,65)),
%  tok(ws," "," ",pos(65,66)),
%  tok(uc,"SIMS","sims",pos(66,70)),
%  tok(pn,")",")",pos(70,71))]
% and an input string, e.g., "(ToF       SIMS) measurements."

% We need to consume the strings in the tokens off the beginning of the input string,
% !!BUT!! a ws token needs special treatment, because the AA could span a line,
% and contain extra embedded white space, e.g.,

%       3-mercaptopropyl-silylated glass plates and capillaries. The brushes were
%       characterized with time-of-flight secondary ion mass-spectrometry (ToF
%       SIMS), atomic force microscopy and contact angle measurements. Fructose
%       caused a well-expressed drop spreading on the surface of copolymer-grafted

consume_token_strings([], InputString, InputString, NumCharsConsumed, NumCharsConsumed).
consume_token_strings([FirstToken|RestTokens], InputStringIn, InputStringOut,
		      NumCharsConsumedIn, NumCharsConsumedOut) :-
	consume_token_chars(FirstToken, InputStringIn, InputStringNext,
			    NumCharsConsumedIn, NumCharsConsumedNext),
	consume_token_strings(RestTokens, InputStringNext, InputStringOut,
			      NumCharsConsumedNext, NumCharsConsumedOut).

consume_token_chars(FirstToken, InputStringIn, InputStringNext,
		    NumCharsConsumedIn, NumCharsConsumedNext) :-
	( higher_order_or_annotation_tok(FirstToken) ->
	  InputStringNext = InputStringIn,
	  NumCharsConsumedNext is NumCharsConsumedIn	  
	; ws_tok(FirstToken) ->
	  trim_whitespace_left_count(InputStringIn, InputStringNext, NumBlanksTrimmed),
	  NumCharsConsumedNext is NumCharsConsumedIn + NumBlanksTrimmed
	; an_pn_xx_tok(FirstToken) ->
	  token_template(FirstToken, _TokenType, TokenString, _LCTokenString, pos(StartPos,EndPos)),
	  append(TokenString, InputStringNext, InputStringIn),
	  NumCharsConsumedNext is NumCharsConsumedIn + EndPos - StartPos
       ).

% If the next token is not a ws token, strip off any whitespace,
% and return the new InputString and the number of whitespace chars removed.
remove_untokenized_whitespace([], InputString, InputString, 0).
remove_untokenized_whitespace([NextToken|_], InputStringIn,
			      InputStringOut, NumBlanksRemoved) :-
	( ws_tok(NextToken) ->
	  InputStringOut = InputStringIn,
	  NumBlanksRemoved is 0
        ; remove_leading_whitespace(InputStringIn, 0, InputStringOut, NumBlanksRemoved)
        ).


% remove_parenthesized_chars([FirstToken|RestToken], InputStringIn, InputStringOut) :-
% 	( higher_order_or_annotation_tok(FirstToken) ->
% 	  remove_parenthesized_chars(RestToken, InputStringIn, InputStringOut)
% 	; arg(2, FirstToken, FirstTokenChars),
% 	  append(FirstTokenChars, InputStringNext, InputStringIn),
% 	  ( ex_rbracket_tok(FirstToken) ->
% 	    InputStringOut = InputStringNext
% 	  ; remove_parenthesized_chars(RestToken, InputStringNext, InputStringOut)
% 	  )
% 	).

% Given a list of AA tokens, which represent the unexpanded acronym, e.g.,
% [tok(uc,"HA","ha",pos(38,40)),tok(uc,"LC","lc",pos(41,43))]
% compute the length of the printed representation.
% This is done by subtracting the start position of the first AA token (38)
% from the end position of the last LC token (43). Here we get 5. Duh.

get_aa_tokens_start_and_end_pos([FirstAAToken|RestAATokens],
                                FirstAAStartPos, LastAAEndPos) :-
        arg(4, FirstAAToken, FirstPosTerm),
        arg(1, FirstPosTerm, FirstAAStartPos),
        arg(2, FirstPosTerm, FirstAAEndPos),
        get_last_aa_token_end_pos(RestAATokens, FirstAAEndPos, LastAAEndPos).

get_last_aa_token_end_pos([], LastAAEndPos, LastAAEndPos).
get_last_aa_token_end_pos([NextAAToken|RestAATokens], _PrevAAEndPos, LastAAEndPos) :-
        arg(4, NextAAToken, NextPosTerm),
        arg(2, NextPosTerm, NextAAEndPos),
        get_last_aa_token_end_pos(RestAATokens, NextAAEndPos, LastAAEndPos).

get_aa_tokens_length([], _InputStringIn, PrevNumLeftBlanksTrimmed, LengthSoFar, Length) :-
	Length is LengthSoFar - PrevNumLeftBlanksTrimmed.
get_aa_tokens_length([FirstAAToken|RestAATokens], InputStringIn, _PrevNumLeftBlanksTrimmed,
		     LengthSoFar, Length) :-
	get_one_token_pos_length(FirstAAToken, InputStringIn,
				 InputStringNext, TokenStringLength, NumLeftBlanksTrimmed),
	NextLength is LengthSoFar + TokenStringLength + NumLeftBlanksTrimmed,
	get_aa_tokens_length(RestAATokens, InputStringNext, NumLeftBlanksTrimmed, NextLength, Length).	

% calculate how many chars we peel off from InputString when we remove
% (1) the chars in TokenString, and then
% (2) all leading blanks (i.e., trim_left)
get_one_token_pos_length(AAToken, InputStringIn,
			 InputStringOut, StringLength, NumLeftBlanksTrimmed) :-
	( ws_tok(AAToken) ->
	  InputStringOut = InputStringIn,
	  StringLength is 0,
	  NumLeftBlanksTrimmed is 0
	; token_template(AAToken,
			 _TokenType, TokenString, _LCTokenString, _PosInfo) ->
	  append(Prefix, InputString0, InputStringIn),
	  append(TokenString, InputStringNext, InputString0),
	  !,
	  length(Prefix, PrefixLength),
	  length(TokenString, TokenStringLength),
	  StringLength is PrefixLength + TokenStringLength,
	  trim_whitespace_left_1(InputStringNext, 0, InputStringOut, NumLeftBlanksTrimmed)
	).
%%% get_aa_tokens_start_and_end_pos([FirstAAToken|RestAATokens], InputStringIn,
%%% 				FirstAAStartPos, LastAAEndPos) :-
%%% 	arg(4, FirstAAToken, FirstPosTerm),
%%% 	arg(1, FirstPosTerm, FirstAAStartPos),
%%% 	arg(2, FirstPosTerm, FirstAAEndPos),
%%% 	get_last_aa_token_end_pos(RestAATokens, FirstAAEndPos, LastAAEndPos).
%%% 
%%% get_last_aa_token_end_pos([], LastAAEndPos, LastAAEndPos).
%%% get_last_aa_token_end_pos([NextAAToken|RestAATokens], _PrevAAEndPos, LastAAEndPos) :-
%%% 	arg(4, NextAAToken, NextPosTerm),
%%% 	arg(2, NextPosTerm, NextAAEndPos),
%%% 	get_last_aa_token_end_pos(RestAATokens, NextAAEndPos, LastAAEndPos).

% remove_aa_tokens/4 is called while processing an aa token in the TokenList.
% The first argument passed to remove_aa_tokens is
% the second argument of the aa token.
% In the admittedly contrived case
% "heart attack lung cancer (HA LC) HA LC",
% the aa token is the following:

% tok(aa,
%     [tok(uc,'HA',ha,pos(33,35)),
%      tok(uc,'LC',lc,pos(37,39))],
%     [tok(aadef,
%         [tok(lc,heart,heart,pos(0,5)),
%          tok(ws,' ',' ',pos(5,6)),
%          tok(lc,attack,attack,pos(6,12)),
%          tok(ws,' ',' ',pos(12,13)),
%          tok(lc,lung,lung,pos(13,17)),
%          tok(ws,' ',' ',pos(17,18)),
%          tok(lc,cancer,cancer,pos(18,24))],
%         [tok(uc,'HA',ha,pos(26,28)),
%          tok(uc,'LC',lc,pos(29,31))],
%         pos(0,24))],
%     pos(33,39))

% We want to remove these two tokens from the TokenList:
%     tok(uc,'HA',ha,pos(33,35))
%     tok(uc,'LC',lc,pos(37,39))
% Unfortunately, the TokenList may have one or more
% intervening whitespace tokens, e.g.,
%     tok(ws," "," ",pos(49,50))
% so these must explicitly be removed via remove_next_whitespace_tokens,
% except for after the *final* AA token!!

% There are pathological cases in which an aa token intervenes between
% an aadef token and the individual expansion tokens;
% if one is encountered, just skip over it.

final_match([FirstToken|RestTokens0], LastAAToken, RestTokens) :-
	( FirstToken == LastAAToken ->
	  RestTokens = RestTokens0
	; aa_tok(FirstToken) ->
	  arg(2, FirstToken, [LastAAToken]),
	  RestTokens0 = [LastAAToken|RestTokens]
	).

remove_aa_tokens([], LastAAToken, [NextTokenIn|TempRestTokens], RestTokens) :-
	( LastAAToken == NextTokenIn ->
	  RestTokens = TempRestTokens
	; aa_tok(NextTokenIn) ->
	  arg(2, NextTokenIn, [LastAAToken|_]),
	  final_match(TempRestTokens, LastAAToken, RestTokens)
	; aadef_tok(NextTokenIn) ->
	  arg(2, NextTokenIn, [LastAAToken|_]),
	  TempRestTokens = [LastAAToken|RestTokens]
	  % When an AA appears in the text, there can be additional whitespace,
	  % e.g., the AA "(MSCT-CA)" can appear later in the text as "MSCT  -  CA".
	; ws_tok(NextTokenIn) ->
	  remove_aa_tokens([], LastAAToken, TempRestTokens, RestTokens)
	  % This case handles horrors such as "the apparent diffusivity (D )"
	; ws_tok(LastAAToken) ->
	  RestTokens = TempRestTokens
	).
	
remove_aa_tokens([NextAAToken|RestAATokens], AAToken, [NextTokenIn|RestTokensIn], RestTokensOut) :-
	( AAToken == NextTokenIn ->
	  remove_aa_tokens(RestAATokens, NextAAToken, RestTokensIn, RestTokensNext),
	  RestTokensOut = RestTokensNext
	  % remove_next_whitespace_tokens(RestTokensNext, RestTokensOut)
	; aa_tok(NextTokenIn) ->
	  remove_aa_tokens([NextAAToken|RestAATokens], AAToken, RestTokensIn, RestTokensOut)
	; ws_tok(NextTokenIn) ->
	  remove_aa_tokens([NextAAToken|RestAATokens], AAToken, RestTokensIn, RestTokensOut)
	; aadef_tok(NextTokenIn) ->
	  remove_aa_tokens([NextAAToken|RestAATokens], AAToken, RestTokensIn, RestTokensOut)
	  % If there's a ws token in the AA, remove all the next ws tokens in the token list
	; ws_tok(AAToken) ->
	  remove_next_whitespace_tokens([NextTokenIn|RestTokensIn], RestTokensNext),
	  remove_aa_tokens(RestAATokens, NextAAToken, RestTokensNext, RestTokensOut)
	).

remove_next_whitespace_tokens([], _RestTokensOut).
remove_next_whitespace_tokens([FirstToken|RestTokensIn], RestTokensOut) :-
	( ws_tok(FirstToken) ->
	  remove_next_whitespace_tokens(RestTokensIn, RestTokensOut)
	; RestTokensOut = [FirstToken|RestTokensIn]
	).

create_new_tokens_no_consume([], _CurrPos,
			     _AATokenLength, _AAStartPos, _AAEndPos,
			     NewTokens, NewTokens).
create_new_tokens_no_consume([FirstToken|RestTokens], CurrPos,
			     AATokenLength,  AAStartPos, AAEndPos,
			     [NewFirstToken|NewTokensNext], NewTokensOut) :-
	% FirstToken = tok(TokenType, Text, LCText, _OrigPosTerm),
	token_template(FirstToken, TokenType, Text, LCText, _OrigPosTerm),
	% add_pos_term_2(T, H, CurrPos, AATokenLength, NewPosTerms),
	% FirstNewToken  = tok(TokenType, Text, LCText,
	% 		     pos(AAStartPos,AAEndPos),
	% 		     pos(CurrPos, AATokenLength)),
	token_template(NewFirstToken, TokenType, Text, LCText,
		       pos(AAStartPos,AAEndPos),
		       pos(CurrPos, AATokenLength)),
	create_new_tokens_no_consume(RestTokens, CurrPos,
				     AATokenLength,  AAStartPos, AAEndPos,
				     NewTokensNext, NewTokensOut).

% consume_aa_token_strings/4 does for the input string
% exactly what remove_aa_tokens/4 does for the token list:
% consume the text strings in the uc tokens from the InputString,
% including any intervening blank spaces that are not in the token list
consume_aa_token_strings([], LastAAToken, InputStringIn, InputStringOut) :-
	arg(2, LastAAToken, LastAATokenString),
	( append(LastAATokenString, InputStringNext, InputStringIn) ->
	  true
	; trim_whitespace_left(InputStringIn, InputString1),
	  append(LastAATokenString, InputStringNext, InputString1)
	),
	InputStringOut = InputStringNext.
%	sublist(InputStringIn, FirstTokenString, PrefixLength, TextLength),
%	!,
%	% Don't allow skipping too far forward in input string!
%	limit_forward_skip(LastAAToken, PrefixLength),
%	NumCharsConsumed is PrefixLength + TextLength,
%	append_length(InputStringNext, InputStringIn, NumCharsConsumed),
%	( ws_tok(LastAAToken) ->
%	  trim_whitespace_left_count(InputStringNext, InputStringOut, _NumBlanksTrimmed)
%	; InputStringOut = InputStringNext
%	).
	% Can't simply append because of cases where the first token is an aa!
	% append(FirstTokenString, InputStringOut, InputStringIn).
consume_aa_token_strings([SecondAAToken|RestAATokens],
			 FirstAAToken, InputStringIn, InputStringOut) :-
	arg(2, FirstAAToken, FirstAATokenString),
	% Because text_objects:insert_all_aas/4 allows arbitrary whitespace between tokens
	% when inserting AAs into the token list (using match_tokens_ignore_ws_ho/4),
	% we must also allow removing arbitrary whitespace from the input string here too.
	( append(FirstAATokenString, InputStringNext, InputStringIn) ->
	  RemainingAATokens = RestAATokens,
	  NextAAToken = SecondAAToken
	; trim_whitespace_left(InputStringIn, InputStringNext),
	  RemainingAATokens = [SecondAAToken|RestAATokens],
	  NextAAToken = FirstAAToken
	),
	% arg(3, FirstAAToken, pos(StartPos,EndPos)),
	% NumCharsConsumed is EndPos - StartPos,
	% append(AATokenString, InputString1, InputStringIn),
	% remove_leading_whitespace(InputString1, 0, InputStringNext, _NumBlanksRemoved),
	consume_aa_token_strings(RemainingAATokens, NextAAToken, InputStringNext, InputStringOut).

% consume_aa_token_strings([NextAAToken|RestAATokens], AAToken, InputStringIn, InputStringOut) :-
% 	arg(2, AAToken, AATokenString),
% 	sublist(InputStringIn, AATokenString, PrefixLength, TextLength),
% 	!,
% 	NumCharsConsumed is PrefixLength + TextLength,
% 	% Don't allow skipping too far forward in input string,
% 	% unless we're at the very beginning of the citation!
% 	limit_forward_skip(AAToken, PrefixLength),
% 	append_length(InputString1, InputStringIn, NumCharsConsumed),
% 	% append(AATokenString, InputString1, InputStringIn),
% 	% remove_leading_whitespace(InputString1, 0, InputStringNext, _NumBlanksRemoved),
% 	consume_aa_token_strings(RestAATokens, NextAAToken, InputString1, InputStringOut).

% limit_forward_skip(AAToken, PrefixLength) :-
% 	% AAToken = tok(_TokenType, _String, _LCString, pos(StartPos,_EndPos)),
% 	token_template(AAToken, _TokenType, _String, _LCString, pos(StartPos,_EndPos)),
% 	% If StartPos < 500, we're probably somwhere in the metadata,
% 	% so don't enforce a 10-character limit on PrefixLength.
% 	( StartPos < 500 ->
% 	  true
% 	; PrefixLength < 10
% 	).

remove_leading_whitespace([H|T], NumBlanksRemovedIn,
			CharsWithNoWhiteSpace, NumBlanksRemovedOut) :-
	( ws_char(H) ->
	  NumBlanksRemovedNext is NumBlanksRemovedIn + 1,
	  remove_leading_whitespace(T, NumBlanksRemovedNext,
	  			  CharsWithNoWhiteSpace, NumBlanksRemovedOut)
        ; CharsWithNoWhiteSpace = [H|T],
	  NumBlanksRemovedOut is NumBlanksRemovedIn
        ).

% consume tokens though (and including) the first ex_bracket token
% whose endpos matches the original pe token's endpos
consume_tokens_through_rbracket_end_pos([FirstToken|RestTokens], TargetEndPos, NewRestTokens) :-
	token_template(FirstToken, _TokenType, _String, _LCString, pos(_StartPos,EndPos)),
	( EndPos =:= TargetEndPos,
	  ex_rbracket_tok(FirstToken) ->
	  NewRestTokens = RestTokens
	; consume_tokens_through_rbracket_end_pos(RestTokens, TargetEndPos, NewRestTokens)
	).

% There are pathological cases in which an aa token intervenes
% between an aadef token and the individual expansion tokens.

matching_tokens(FirstAAToken, NextToken, RestTokensIn1, RestTokensIn2) :-
	( FirstAAToken == NextToken ->
	  RestTokensIn2 = RestTokensIn1
	  % RestTokensIn1 = [WSToken|RestTokensInTemp],
	  % ( ws_tok(WSToken) ->
	  %   RestTokensIn2 = RestTokensInTemp
          % ; RestTokensIn2 = RestTokensIn1
	  % )
	; aa_tok(NextToken),
	  arg(2, NextToken, [FirstAAToken|_]),
	  RestTokensIn1 = [FirstAAToken|RestTokensIn2]
	).

remove_tokens_no_consume([], RestTokens, RestTokens).
remove_tokens_no_consume([FirstAADefToken|RestAADefTokens],
			 [FirstToken|RestTokensIn], RestTokensOut) :-
	( FirstAADefToken == FirstToken ->
	  remove_tokens_no_consume(RestAADefTokens, RestTokensIn, RestTokensOut)
	; ( aa_tok(FirstToken)
	  ; aadef_tok(FirstToken)
	  ) ->
	  arg(2, FirstToken, [FirstAAToken|RestAATokens]),
	  FirstAAToken == FirstAADefToken,
	  remove_tokens_no_consume([FirstAAToken|RestAATokens], RestTokensIn, _RestTokensOut1),
	  % NextToken is the simple token that matches the first AA token
	  % or an AA token 
	  RestTokensIn = [NextToken|RestTokensIn1],
	  matching_tokens(FirstAAToken, NextToken, RestTokensIn1, RestTokensIn2),
	  remove_tokens_no_consume(RestAADefTokens, RestTokensIn2, RestTokensOut)
	; ws_tok(FirstToken),
	  \+ ws_tok(FirstAADefToken) ->
	  remove_tokens_no_consume([FirstAADefToken|RestAADefTokens], RestTokensIn, RestTokensOut)
	; RestAADefTokens == [],
	  punc_tok(FirstAADefToken) ->
	  RestTokensOut = [FirstToken|RestTokensIn]
	).


% consume_tokens_up_to_ws_tok([FirstToken|RestTokens], PrevTokenType,
% 			    PosIn,  InputStringIn, NewTokensIn,
% 			    PosOut, InputStringOut, NewTokensOut) :-
% 	( ws_tok(FirstToken) ->
% 	  PosOut is PosIn,
% 	  InputStringOut = InputStringIn,
% 	  NewTokensIn = NewTokensOut
% 	; UDA_AVL = empty,
% 	  add_raw_pos_info_2(FirstToken, aadef, UDA_AVL, PrevTokenType, PosIn, InputStringIn, 
% 			     NewToken, PosNext, InputStringNext),
% 	  NewTokensIn = [NewToken|NewTokensNext],
% 	  consume_tokens_up_to_ws_tok(RestTokens, PrevTokenType,
% 				      PosNext,  InputStringNext, NewTokensNext,
% 				      PosOut, InputStringOut, NewTokensOut)
% 	).
 

% Create new tokens from the tokens in arg1
% and remove their corresponding strings from the InputString

% For really pathological cases such as "heart attack" (HA)
% (when the quotation marks appear in the text!!),
% we have to create new tokens for tokens occurring after the end
% of the AADef tokens and before the next whitespace token. GROSS.
create_new_tokens_and_consume_strings([], _PrevTokenType, _RestTokens, PosIn,
				      InputStringIn, NewTokensIn, NewTokensIn, PosIn,
				      InputStringIn).

%create_new_tokens_and_consume_strings([], PrevTokenType, RestTokens, PosIn,
%				      InputStringIn, NewTokensIn, NewTokensOut, PosOut,
%				      InputStringOut) :-	
%	consume_tokens_up_to_ws_tok(RestTokens, PrevTokenType,
%				    PosIn,  InputStringIn,  NewTokensIn,
%				    PosOut, InputStringOut, NewTokensOut).
	
create_new_tokens_and_consume_strings([FirstAADefToken|RestAADefTokens], PrevTokenType,
				      [NextToken|RestTokensIn],
				      PosIn, InputStringIn,
				      NewTokensIn, NewTokensOut, PosOut,
				      InputStringOut) :-
	( FirstAADefToken == NextToken ->
	  UDA_AVL = empty,
	  add_raw_pos_info_2(FirstAADefToken, aadef, UDA_AVL, PrevTokenType, PosIn,   InputStringIn, 
			     NewToken,   PosNext, InputStringNext),
	  NewTokensIn = [NewToken|NewTokensNext],
	  RemainingAADefTokens = RestAADefTokens,
	  RestTokensNext = RestTokensIn
	; RestAADefTokens == [],
	  punc_tok(FirstAADefToken) ->
	  RemainingAADefTokens = [],
	  PosNext = PosIn,
	  InputStringNext = InputStringIn,
	  NewTokensNext = NewTokensIn,
	  RestTokensNext = RestTokensIn
	  % If an aa token appears in the expansion of an aadef token,
	  % then the fun begins...
	; ( aa_tok(NextToken) ->
	    ArgNum is 2
	  ; aadef_tok(NextToken) ->
	    ArgNum is 2
	  ),
	  arg(ArgNum, NextToken, AATokens),
	  % Ensure that the first FirstAADefToken is also the first AA token;
	  % see the example below!
	  AATokens = [FirstAAToken|RestAATokens],
	  FirstAAToken == FirstAADefToken ->
	  % Can't simply append here, because RestAADefTokens can contain ws tokens
	  % that aren't in RestAATokens. GRRR.
	  match_aa_tokens(RestAATokens, RestAADefTokens, LeftOverAADefTokens),
	  match_aa_tokens(AATokens, RestTokensIn, RestTokensNext),
	  % The 3rg arg of the AA token is not the AADef token itself,
	  % but rather a singleton list CONTAINING the AADef token!
	  arg(3, NextToken, [AADefSubToken]),
	  ( aa_tok(NextToken) ->
	    arg(2, AADefSubToken, ExpansionTokens)
	  ; AADefSubToken = ExpansionTokens
	  ),
	  consume_strings(AATokens, 1, FirstPrefixLength, InputStringIn, InputStringNext),
	  % Can't just use PosIn here if the abstract begins with an aadef/aa,
	  % as in the example below, because PosIn will still be at the end of the title!
	  arg(4, FirstAAToken, pos(AAStartPos,_)),
	  % Not sure why the +1 was in there. I removed it because of Pawel's bug.
	  % RealPosIn is PosIn + FirstPrefixLength + 1,
	  RealPosIn is PosIn + FirstPrefixLength,
	  token_sequence_length(RestAATokens, FirstAAToken, AAStartPos, AATokensLength),
	  create_new_tokens(ExpansionTokens, RealPosIn, AATokensLength, NewTokensIn, NewTokensNext),
	  % create_new_tokens(AATokens, RealPosIn, AATokensLength, NewTokensIn, NewTokensNext),
	  PosNext is RealPosIn + AATokensLength,
	  RemainingAADefTokens = LeftOverAADefTokens
	),
	create_new_tokens_and_consume_strings(RemainingAADefTokens, PrevTokenType, RestTokensNext,
					      PosNext, InputStringNext,
					      NewTokensNext, NewTokensOut, PosOut,
					      InputStringOut).

%%% The code above matches the tokens in the aadef (TGF ... receptor)
%%% with the following tokens. Things get complicated when the next token
%%% is an AA tokens, and even more complicated when 2 consecutive AA tokens follow.

%%% match_aa_tokens/3 is called twice when the token after the AADef is an AA.
%%% 
%%% The first call to match_aa_tokens/3 matches
%%% (1) the tail of the first arg of the first AA token (beta, 1) and
%%% (2) the tail of AADef tokens (ws, beta, ... receptor).
%%% match_aa_tokens/3 is clever enough to ignore ws tokens in its second arg,
%%% so it matches the (beta, 1) tokens in arg 1 with (ws, beta, ws, 1) in arg 2.
%%% The head of both these lists is explicitly matched by
%%% 	  FirstAAToken = FirstAADefToken,
%%% The second call to match_aa_tokens/3 matches
%%% (1) all the AA tokens (TGF, beta, 1) and
%%% (2) the remaining tokens, beginning with the second AA token.

%%% Because the first remaining token is another AA,
%%% we call match_aa_tokens_1.

%%% AADef1: transforming growth factor beta 1 (TGF beta 1)
%%% AADef2: Transforming Growth Factor beta (TGF beta)
%%% AADef3: TGF beta 1 and its receptor (TGF beta RII)
%%% leads to the following token sequence:
%%% tok(aadef,
%%%     [tok(uc,'TGF',tgf,pos(1094,1097)),
%%%      tok(ws,' ',' ',pos(1097,1098)),     <--- tail of the AADef tokens
%%%      tok(lc,beta,beta,pos(1098,1102)),
%%%      tok(ws,' ',' ',pos(1102,1103)),
%%%      tok(nu,'1','1',pos(1103,1104)),
%%%      tok(ws,' ',' ',pos(1104,1105)),
%%%      tok(lc,and,and,pos(1105,1108)),
%%%      tok(ws,' ',' ',pos(1108,1109)),
%%%      tok(lc,its,its,pos(1109,1112)),
%%%      tok(ws,' ',' ',pos(1112,1113)),
%%%      tok(lc,receptor,receptor,pos(1113,1121))],
%%%     [tok(uc,'TGF',tgf,pos(1123,1126)),
%%%      tok(lc,beta,beta,pos(1127,1131)),
%%%      tok(uc,'RII',rii,pos(1132,1135))],
%%%     pos(1094,1121))
%%% tok(aa,
%%%     [tok(uc,'TGF',tgf,pos(1094,1097)),
%%%      tok(lc,beta,beta,pos(1098,1102)),   <--- tail of first arg
%%%      tok(nu,'1','1',pos(1103,1104))],         of the first AA token
%%%     [tok(aadef,
%%%         [tok(lc,transforming,transforming,pos(8,20)),
%%%          tok(ws,' ',' ',pos(20,21)),
%%%          tok(lc,growth,growth,pos(21,27)),
%%%          tok(ws,' ',' ',pos(27,28)),
%%%          tok(lc,factor,factor,pos(28,34)),
%%%          tok(ws,' ',' ',pos(34,35)),
%%%          tok(lc,beta,beta,pos(35,39)),
%%%          tok(ws,' ',' ',pos(39,40)),
%%%          tok(nu,'1','1',pos(40,41))],
%%%         [tok(uc,'TGF',tgf,pos(43,46)),
%%%          tok(lc,beta,beta,pos(47,51)),
%%%          tok(nu,'1','1',pos(52,53))],
%%%         pos(8,41))],
%%%     pos(1094,1104))
%%% tok(aa,                                  <--- the second AA token
%%%     [tok(uc,'TGF',tgf,pos(1094,1097)),
%%%      tok(lc,beta,beta,pos(1098,1102))],
%%%     [tok(aadef,
%%%         [tok(ic,'Transforming',transforming,pos(685,697)),
%%%          tok(ws,' ',' ',pos(697,698)),
%%%          tok(ic,'Growth',growth,pos(698,704)),
%%%          tok(ws,' ',' ',pos(704,705)),
%%%          tok(ic,'Factor',factor,pos(705,711)),
%%%          tok(ws,' ',' ',pos(711,712)),
%%%          tok(lc,beta,beta,pos(712,716))],
%%%         [tok(uc,'TGF',tgf,pos(718,721)),
%%%          tok(lc,beta,beta,pos(722,726))],
%%%         pos(685,716))],
%%%     pos(1094,1102))
%%% tok(uc,'TGF',tgf,pos(1094,1097))
%%% tok(ws,' ',' ',pos(1097,1098))
%%% tok(lc,beta,beta,pos(1098,1102))
%%% tok(ws,' ',' ',pos(1102,1103))
%%% tok(nu,'1','1',pos(1103,1104))

match_aa_tokens(_, [], []) :- !.
match_aa_tokens([], RestTokens, RestTokens).
match_aa_tokens([FirstAAToken|RestAATokens], [FirstToken|RestTokens], RestTokensNext) :-
	( FirstAAToken == FirstToken ->
	  match_aa_tokens(RestAATokens, RestTokens, RestTokensNext)
	; ws_tok(FirstToken) ->
	  match_aa_tokens([FirstAAToken|RestAATokens],
			  RestTokens, RestTokensNext)
	; aa_tok(FirstToken) ->
	  arg(2, FirstToken, AATokens2),
	  match_aa_tokens_1(AATokens2, RestTokens,
			    [FirstAAToken|RestAATokens],
			    RemainingTokens, RemainingAATokens),
	  match_aa_tokens(RemainingAATokens, RemainingTokens, RestTokensNext)			    
	).

% [FirstAAToken2|RestAATokens2] are the AA tokens from the second AA
% [FirstToken|RestTokens] are the rest of the top-level tokens
% [FirstAAToken1|RestAATokens1] are the AA tokens from the first AA
match_aa_tokens_1([], RemainingTokens, RemainingAATokens, RemainingTokens, RemainingAATokens).
match_aa_tokens_1([FirstAAToken2|RestAATokens2],
		  [FirstToken|RestTokens],
		  [FirstAAToken1|RestAATokens1],
		  RemainingTokens, RemainingAATokens) :-
	( ws_tok(FirstToken) ->
	  match_aa_tokens_1([FirstAAToken2|RestAATokens2],
	  		    RestTokens,
			    [FirstAAToken1|RestAATokens1],
			    RemainingTokens, RemainingAATokens)
	; FirstAAToken1 == FirstToken,
	  FirstAAToken2 == FirstToken,
	  match_aa_tokens_1(RestAATokens2, RestTokens, RestAATokens1,
	  		    RemainingTokens, RemainingAATokens)
	).

consume_strings([], _TokenIndex, _FirstPrefixLength, InputString, InputString).
consume_strings([FirstToken|RestTokens], TokenIndex, FirstPrefixLength,
		InputStringIn, InputStringOut) :-
	arg(2, FirstToken, TokenString),
	% remove_leading_whitespace(InputStringIn, 0, InputStringTemp, _NumBlanksRemoved),
	% Must change this call to append to a call to substring
	% append(TokenString, InputStringNext, InputStringTemp),
	InputStringTemp = InputStringIn,
	sublist(InputStringTemp, TokenString, PrefixLength, TextLength),
	NumCharsConsumed is PrefixLength + TextLength,
	set_first_prefix_length(TokenIndex, PrefixLength, FirstPrefixLength),
	append_length(InputStringNext, InputStringTemp, NumCharsConsumed),
	NextTokenIndex is TokenIndex + 1,
	consume_strings(RestTokens, NextTokenIndex, FirstPrefixLength,
			InputStringNext, InputStringOut).

set_first_prefix_length(TokenIndex, PrefixLength, FirstPrefixLength) :-
	( TokenIndex =:= 1 ->
	  FirstPrefixLength is PrefixLength
	; true
	).

create_new_tokens([], _StartPos, _EndPos, NewTokens, NewTokens).
create_new_tokens([FirstToken|RestTokens], StartPos, EndPos,
		  [NewFirstToken|NewTokensNext], NewTokensOut) :-
	% FirstToken = tok(Type, TokenString, LCTokenString, Pos),
	token_template(FirstToken, Type, TokenString, LCTokenString, Pos),
	% NewFirstToken = tok(Type, TokenString, LCTokenString, Pos, pos(StartPos,EndPos)),
	token_template(NewFirstToken, Type, TokenString, LCTokenString, Pos, pos(StartPos,EndPos)),
	create_new_tokens(RestTokens, StartPos, EndPos, NewTokensNext, NewTokensOut).

% Test if we are in a sentence (i.e., if a alphanumeric/punctuation token
% has been encountered since the most recent sn token).
% If not, I don't care how many characters have been skipped.
% Otherwise, ensure that we haven't skipped > 10 spaces.
% This test deals with the off chance that words like "PMID" or "MEDLINE"
% (typically found in the MEDLINE fields before the title/abstract)
% would appear in the actual text and mess up the character positions.
test_for_adjacency(TokenState, NewToken, CurrentPos) :-
	( TokenState =:= 0 ->
	  true
	; arg(5, NewToken, pos(StartPos, _StringLength)),
	  StartPos < CurrentPos + 10
        ).

% The token state is initialized to 0, meaning that we are NOT in a sentence.
% The state remains at 0 when a higher-order or annotation token (i.e., field,
% label, sn, pe, aa, aadef) found, but flips to 1 if a char-consuming token
% (i.e., an, ic, lc, mc, nu, pn, uc, ws), is found, because that means
% we've started a new sentence.
get_next_token_state(0, TokenType, NextTokenState) :-
	( higher_order_or_annotation_type(TokenType) ->
	  NextTokenState is 0
        ; NextTokenState is 1
         ).
% The token state flips from 1 (meaning that we are currently in a sentence)
% to 0 when an sn token is found, because that means the current sentence has ended.
% Otherwise a token state of 1 remains at 1.
get_next_token_state(1, TokenType, NextTokenState) :-
	( TokenType == sn ->
	  NextTokenState is 0
        ; NextTokenState is 1
        ).

% For tokens that consume characters (i.e., an, ic, lc, mc, nu, pn, uc, ws).
% CurrentTokenType is the type of token that is being handled when
% add_raw_pos_info_2/8 is called. Right now, it's only an (alphanumeric) or aadef.
add_raw_pos_info_2(CurrentToken, CurrentTokenType, UDA_AVL, PrevTokenType,
		   CurrentPos, InputString, NewToken, NextPos, RestInputString) :-
	token_template(CurrentToken, TokenType, CurrentTokenText,
		       LCCurrentTokenText, CurrentPosTerm),
	% CurrentTokenText is a substring of InputString.
	% There are PrefixLength chars in InputString before CurrentTokenText.
	% CurrentTokenTextLength is the length of CurrentTokenText.
	sublist_ws(TokenType, InputString, CurrentTokenText, PrefixLength, CurrentTokenTextLength),
	( CurrentTokenType == aadef,
	  PrevTokenType \== sn ->
	  PrefixLength < 20
	; true
	),
	% The call to sublist/4 must be backtrackable
	% in order to allow the token strings to match
	% multiple places in the text string!!
	% !,
	% proper_prefix_length(InputString, _Prefix, PrefixLength),
	% format(user_output, 'SKIPPED:>~q<~n', [Prefix]),
	% StartPos is the starting position of CurrentTokenText in InputString
	StartPos is PrefixLength + CurrentPos,	
	( avl_member([UDAShortFormToken], UDA_AVL, [UDALongFormTokenList]),	  
	  memberchk(CurrentToken, UDALongFormTokenList) ->
	  token_template(UDAShortFormToken, _UDATokenType,
			 _UDATokenText,_LCUDATokenText, UDAPosTerm),
	  UDAPosTerm = pos(UDAStartPos,UDAEndPos),
	  RealTokenTextLength is UDAEndPos - UDAStartPos,
	  % See comment below explaining this mess
	  ( last(UDALongFormTokenList, CurrentToken) ->
	    NextPos is CurrentPos + PrefixLength + RealTokenTextLength
	  ; NextPos is StartPos
	  )
	; RealTokenTextLength is CurrentTokenTextLength,
	  NextPos is CurrentPos + PrefixLength + RealTokenTextLength
	),
	create_new_pos_term_2(StartPos, RealTokenTextLength, NewPosTerm),
	token_template(NewToken, TokenType, CurrentTokenText,
		       LCCurrentTokenText, CurrentPosTerm, NewPosTerm),
	% NumCharsConsumed is the number of chars to lop off
	% the beginning of InputString to get RestInputString
	NumCharsConsumed is PrefixLength + CurrentTokenTextLength,
	append_length(RestInputString, InputString, NumCharsConsumed).

%% If CurrentToken is in a UDA: 
%%  * If CurrentToken IS the last token in the UDA expansion,
%%    then NextPos should be incremented by RealTokenTextLength.
%%  * If CurrentToken is NOT the last token in the UDA expansion,
%%    then NextPos should not change (i.e., it should be StartPos). 



% sublist_ws/4 succeeds if sublist/4 succeeds, or
% if the TokenType is xx and ends in "'s",
% allowing for an extra space before and/or after the "'" in InputString.

get_aa_text(TokenList, AAString) :-
	(  foreach(Token, TokenList),
	   foreach(TokenString, TokenStringList)
	do token_template(Token, _TokenType, TokenString, _LCTokenText, _PosTerm)
	),
	append(TokenStringList, AAString).

sublist_ws(CurrentTokenType, InputString, CurrentTokenText0,
	   InputStringPrefixLength, CurrentTokenTextLength) :-
	% If the current topken is an AA, Current TokenText0 is a list like
	% [tok(uc,"TGF","tgf",pos(33,36)),tok(ws," "," ",pos(36,37))],
	% so extract and concatenate the strings.
	( CurrentTokenType == aa ->
	  get_aa_text(CurrentTokenText0, CurrentTokenText)
	; CurrentTokenText = CurrentTokenText0
	),
	( sublist(InputString, CurrentTokenText, InputStringPrefixLength, CurrentTokenTextLength),
	  ( CurrentTokenType == xx ->
	    InputStringPrefixLength < 20
	  ; true
	  )
	; CurrentTokenType == xx,
	  % Set CurrentTokenPrefix to e.g., "Crohn" if CurrentTokenText is "Crohn's".
	  append(CurrentTokenPrefix, [0''', 0's], CurrentTokenText),
	  % length(CurrentTokenPrefix, CurrentTokenPrefixLength),
	  % If InputString is any of
	  %  * "Crohn' s disease",
	  %  * "Crohn 's disease", or even
	  %  * "Crohn ' s disease",
	  % we have to handle all those cases.
	  % We have to handle even cases like PMID 4729670:
	  % TI  - [Treatment of discogenic radiculitis by means of traction with the patient
          %        's own weight]
	  % Bottom line, we need to handle anything like
	  % "Crohn" + arbitrary whitespace + "'" + arbitrary whitespace + "s".
	  
	  % In the case above of "patient         's own weight",
	  % RestInputString = "         's own weight".

	  append([InputStringPrefix,CurrentTokenPrefix,RestInputString1], InputString),
	  length(CurrentTokenPrefix, CurrentTokenPrefixLength),
	  trim_whitespace_left_count(RestInputString1, RestInputString2, NumBlanks1),
	  RestInputString2 = [0'''|RestInputString3],
	  trim_whitespace_left_count(RestInputString3, RestInputString4, NumBlanks2),
	  length(InputStringPrefix, InputStringPrefixLength),
	  RestInputString4 = [0's|_RestInputString5],
	  % sublist(InputString, CurrentTokenPrefix, _InputStringPrefixLength,
	  %	  CurrentTokenPrefixLength, _RestInputStringLength),
	  % append([C1,C2,C3,C4], _TailInputString, RestInputString),
	  % CurrentTokenTextLength is InputStringPrefixLength + CurrentTokenPrefixLength + ExtraCount
          % The "+ 2" accounts for the "'s"			 
	  CurrentTokenTextLength is CurrentTokenPrefixLength + NumBlanks1 + NumBlanks2 + 2
	).
	  
% I want to handle tokens from both 
% Sentences, which have only one pos(X,Y) term, e.g.,
% tok(field,"TX","tx",pos(0,12))
% tok(label,"00000000.tx.1","00000000.tx.1",pos(0,12))
% tok(sn,[],0,pos(0,12))
% and CoordSentences, which have two pos(X,Y) terms, e.g.,
% tok(field,"TX","tx",pos(0,12),pos(0,12))
% tok(label,"00000000.tx.1","00000000.tx.1",pos(0,12),pos(0,12))
% tok(sn,[],0,pos(0,12),pos(0,12))

% Regardless of whether the token has one or two pos(X,Y) terms,
% we add another one to represent the position in the raw text.
% add_raw_pos_info_1 is used for ws, sn, and higher-order and annotation types
% (field, label, sn, pe, aa, and aadef).

add_raw_pos_info_1(OrigToken, CurrentPos, NewToken) :-
	% OrigToken = tok(TokenType, Text, LCText, OrigPosTerm),
	token_template(OrigToken, TokenType, Text, LCText, OrigPosTerm),
	create_new_pos_term_1(OrigPosTerm, CurrentPos, NewPosTerm2),
	% NewToken  = tok(TokenType, Text, LCText, OrigPosTerm, NewPosTerm2).
	token_template(NewToken, TokenType, Text, LCText, OrigPosTerm, NewPosTerm2).

create_new_pos_term_1(pos(X1,Y1), CurrentPos, pos(CurrentPos, TextLength)) :-
	TextLength is Y1 - X1.

create_new_pos_term_2(StartPos, CurrentTokenTextLength, pos(StartPos,CurrentTokenTextLength)).

% Collapse consecutive PosInfo tokens if they denote adjacent positions.
% E.g., 91:3,95:5 are adjacent, because 91:3 ends just before 95:5 begins.
collapse_pos_info([], X, [X]).
collapse_pos_info([Next|Rest], First, CollapsedPosInfo) :-
	First = FirstStartPos/FirstLength,
	Next = NextStartPos/NextLength,
	( NextStartPos is FirstStartPos + FirstLength + 1 ->
	  NewLength is FirstLength + NextLength + 1,
	  % format(user_output, 'Collapsed ~w and ~w into ~w~n',
	  %	   [First,Next,FirstStartPos/NewLength]),
	  collapse_pos_info(Rest, FirstStartPos/NewLength, CollapsedPosInfo)
	; CollapsedPosInfo = [First|RestCollapsed],
	  collapse_pos_info(Rest, Next, RestCollapsed)
	).


% The code below updates the pos info contained in AAs
% by using the corrected pos info in UnExpRawTokenList.

adjust_AAs_pos_info(AAList, UnExpRawTokenList, UniqueID, AdjustedAAs) :-
	sort_AAs_by_position(AAList, SortedAtomizedAAs),
	adjust_AAs_pos_info_aux(SortedAtomizedAAs, UnExpRawTokenList, AdjustedAAs),
	maybe_dump_AAs(UniqueID, AdjustedAAs, _).

sort_AAs_by_position(AAs, SortedAtomizedAAs) :-
	prefix_init_pos(AAs, PrefixedAAs),
	sort(PrefixedAAs, PrefixedSortedAAs),
	prefix_init_pos(SortedAAs, PrefixedSortedAAs),
	(  foreach(AATokens-ExpansionTokens,                 SortedAAs),
	   foreach(AtomizedAATokens-AtomizedExpansionTokens, SortedAtomizedAAs)
	do atomize_tokens(AATokens,        AtomizedAATokens),
	   ExpansionTokens = [ExpansionTokensList],
	   atomize_tokens(ExpansionTokensList, AtomizedExpansionTokens)
	).
	    
prefix_init_pos([], []).
prefix_init_pos([AA-Expansion|Rest], [PosInfo-AA-Expansion|PrefixedRest]) :-
	AA = [tok(_,_,_,PosInfo)|_],
	prefix_init_pos(Rest, PrefixedRest).

atomize_tokens(TokensWithStrings, TokensWithAtoms) :-
	(  foreach(StringToken, TokensWithStrings),
	   foreach(AtomToken,   TokensWithAtoms)
	do StringToken = tok(Type,String,StringLC,PosInfo),
	   maybe_atom_codes(Atom,   String),
	   maybe_atom_codes(AtomLC, StringLC),
	   AtomToken = tok(Type,Atom,AtomLC,PosInfo)
	).

maybe_atom_codes(Atom, String) :-
	( atomic(String) ->
	  Atom = String
	; atom_codes(Atom, String)
	).

adjust_AAs_pos_info_aux([], _UnExpRawTokenList, []).
% adjust_AAs_pos_info_aux([FirstAA|RestAAs], UnExpRawTokenListIn, [FirstAdjustedAA|RestAdjustedAAs]) :-
adjust_AAs_pos_info_aux([FirstAA|RestAAs], UnExpRawTokenList, [FirstAdjustedAA|RestAdjustedAAs]) :-
	FirstAA = AATokenList-ExpTokenList,
%	( earlier_pos(AATokenList, ExpTokenList) ->
%	  adjust_2_token_lists(AATokenList, ExpTokenList, UnExpRawTokenListIn,
%			       AdjustedAATokenList, AdjustedExpTokenList, UnExpRawTokenListNext)
%	; adjust_2_token_lists(ExpTokenList, AATokenList, UnExpRawTokenListIn,
%			       AdjustedExpTokenList, AdjustedAATokenList, UnExpRawTokenListNext)
%	),
	adjust_1_token_list(AATokenList, UnExpRawTokenList, AdjustedAATokenList),
	adjust_1_token_list(ExpTokenList, UnExpRawTokenList, AdjustedExpTokenList),
	FirstAdjustedAA = AdjustedAATokenList-AdjustedExpTokenList,
	adjust_AAs_pos_info_aux(RestAAs, UnExpRawTokenList, RestAdjustedAAs).

/*
adjust_2_token_lists(FirstList, SecondList, UnExpRawTokenListIn,
		     AdjustedFirstList, AdjustedSecondList, UnExpRawTokenListOut) :-
	adjust_1_token_list(FirstList, UnExpRawTokenListIn,
			    AdjustedFirstList, UnExpRawTokenListNext),
	adjust_1_token_list(SecondList, UnExpRawTokenListNext,
			    AdjustedSecondList, UnExpRawTokenListOut).


adjust_1_token_list([], UnExpRawTokenList, [], UnExpRawTokenList).
adjust_1_token_list([FirstAAToken|RestAATokens], [FirstUnExpToken|RestUnExpTokens],
		    [FirstAdjustedToken|RestAdjustedTokens], RemainingUnExpTokens) :-
		    FirstAAToken    = tok(Type, Atom, LCAtom, pos(StartPos1,EndPos1)),
		      % We've found the token in UnExpTokenList that matches the AA token!
		    ( FirstUnExpToken = tok(Type, _String, _LCString,
					    pos(StartPos1,EndPos1), pos(StartPos2,EndPos2)) ->
		      FirstAdjustedToken = tok(Type, Atom, LCAtom, pos(StartPos2,EndPos2)),
		      adjust_1_token_list(RestAATokens, RestUnExpTokens,
					  RestAdjustedTokens, RemainingUnExpTokens)
		    ; adjust_1_token_list([FirstAAToken|RestAATokens], RestUnExpTokens,
					  [FirstAdjustedToken|RestAdjustedTokens], RemainingUnExpTokens)
		    ).
*/

% adjust_1_token_list([], UnExpRawTokenList, [], UnExpRawTokenList).
% adjust_1_token_list([FirstAAToken|RestAATokens], UnExpTokenListIn,
% 		    [FirstAdjustedToken|RestAdjustedTokens], RemainingUnExpTokens) :-
% 	adjust_one_token(FirstAAToken, UnExpTokenListIn,
% 			 FirstAdjustedToken, UnExpTokenListNext),
% 	adjust_1_token_list(RestAATokens, UnExpTokenListNext,
% 			    RestAdjustedTokens, RemainingUnExpTokens).

adjust_1_token_list([], _UnExpRawTokenList, []).
adjust_1_token_list([FirstAAToken|RestAATokens], UnExpTokenListIn,
		    [FirstAdjustedToken|RestAdjustedTokens]) :-
	adjust_one_token(FirstAAToken, UnExpTokenListIn,
			 FirstAdjustedToken, UnExpTokenListNext),
	adjust_1_token_list(RestAATokens, UnExpTokenListNext,
			    RestAdjustedTokens).


adjust_one_token(FirstAAToken, [FirstUnExpToken|RestUnExpTokens],
		 FirstAdjustedToken, UnExpTokenListOut) :-
	FirstAAToken    = tok(Type, Atom, LCAtom, pos(StartPos1,EndPos1)),
	  % We've found the token in UnExpTokenList that matches the AA token!
	( FirstUnExpToken = tok(Type, _String, _LCString,
				pos(StartPos1,EndPos1), pos(StartPos2,EndPos2)) ->
	  FirstAdjustedToken = tok(Type, Atom, LCAtom, pos(StartPos2,EndPos2)),
	  UnExpTokenListOut = RestUnExpTokens
	; adjust_one_token(FirstAAToken, RestUnExpTokens,
			   FirstAdjustedToken, UnExpTokenListOut)
	).

% earlier_pos(AATokenList, ExpTokenList) :-
% 	AATokenList =  [tok(_,_,_,pos(StartPos1,_))|_],
% 	ExpTokenList = [tok(_,_,_,pos(StartPos2,_))|_],
% 	StartPos1 < StartPos2.

