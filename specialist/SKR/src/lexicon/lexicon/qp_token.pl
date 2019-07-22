
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

% File:     qp_token.pl
% Module:   qp_token
% Author:   Suresh, Lan
% Purpose:  To tokenize a string respecting barriers across which lexical
%           lookup is prohibited.

/* qp_token.pl - the tokenizer in prolog.

    The input stream is tokenized like the terms in the lexicon.
    The tokenizer returns a list each element of which is a list
    containing tokens.  Lexical lookahead is not permitted across
    these sublists.

    For example:

	"While in Europe, he saw London." ->
	    [['While',in,'Europe'],[','],[he,saw,'London'],['.']]
	"C3/C5 convertase" ->
	    [['C3','/','C5',convertase]]
*/

:- module(qp_token, [
    tokenize_string/2
    ]).

:- use_module(metamap(metamap_tokenization), [
	local_alnum/1,
	local_ws/1
    ]).

:- use_module(library(lists), [
    rev/2
    ]).

% :- use_module(skr_lib(ctypes), [
%     is_alnum/1,
%     is_space/1
%     ]).


/* tokenize_string(+String, -TokenLists)
   form_barrier_token_lists(+Tokens, -TokenLists)
   form_barrier_token_lists(+Tokens, +RevTokenList, -TokenLists)
   is_barrier(?BarrierChar, ?BarrierToken)

tokenize_string/2 tokenizes String into a list of token lists.  It is
equivalent to tokenize_input/2 (which seems to fail for isolated barrier
characters).  Rather than simply compute the tokens in a string,
tokenize_string/2 organizes the tokens into lists depending on "barrier"
characters/tokens.  tokenize_string/2 first computes a single list of
tokens which, for ease of computation, are either alphanumeric tokens or
raw characters (including whitespace).  It then calls
form_barrier_token_lists/2 to segment the tokens into lists where a new
list is signalled by a barrier character followed either by whitespace
or by nothing.  */

tokenize_string(String,TokenLists) :-
    phrase(ttw_tokens(Tokens),String),
    form_barrier_token_lists(Tokens,TokenLists),
    !.

% ---------- GRAMMAR FOR TOKENIZE TEXT WITH WHITESPACE

ttw_tokens(Ts) --> ttw_token(T), !, ttw_tokens(RestTs), {Ts=[T|RestTs]}

                ;  {Ts=[]}.

ttw_token(T) --> [C], {local_alnum(C)}, !, ttw_alnums(RestCs),
                 {atom_codes(T,[C|RestCs])}
              ;  [T].

ttw_alnums(Cs) --> [C], {local_alnum(C)}, !, ttw_alnums(RestCs), {Cs=[C|RestCs]}

                ;  {Cs=[]}.


form_barrier_token_lists(Tokens,TokenLists) :-
    form_barrier_token_lists(Tokens,[],TokenLists).

form_barrier_token_lists([],RevTokenList,Result) :-
    !,
    (RevTokenList==[] ->
        Result=[]
    ;   rev(RevTokenList,TokenList),
        Result=[TokenList]
    ).
% final barrier
form_barrier_token_lists([C],RevTokenList,Result) :-
    is_barrier(C,B),
    !,
    (RevTokenList==[] ->
        Result=[[B]]
    ;   rev(RevTokenList,TokenList),
        Result=[TokenList,[B]]
    ).
% barrier followed by whitespace
form_barrier_token_lists([C,W|Rest],RevTokenList,Result) :-
    is_barrier(C,B),
    local_ws(W),
    !,
    (RevTokenList==[] ->
        Result=[[B]|RestResult]
    ;   rev(RevTokenList,TokenList),
        Result=[TokenList,[B]|RestResult]
    ),
    form_barrier_token_lists([W|Rest],[],RestResult).
% barrier preceded by whitespace
%form_barrier_token_lists([W,C|Rest],RevTokenList,Result) :-
%    local_ws(W),
%    is_barrier(C,B),
%    !,
%    (RevTokenList==[] ->
%        Result=[[B]|RestResult]
%    ;   rev(RevTokenList,TokenList),
%        Result=[TokenList,[B]|RestResult]
%    ),
%    form_barrier_token_lists(Rest,[],RestResult).
% other whitespace
form_barrier_token_lists([W|Rest],RevTokenList,Result) :-
    local_ws(W),
    !,
    form_barrier_token_lists(Rest,RevTokenList,Result).
% everything else
form_barrier_token_lists([First|Rest],RevTokenList,Result) :-
    (atom(First) ->
        Token=First
    ;   atom_codes(Token,[First])
    ),
    form_barrier_token_lists(Rest,[Token|RevTokenList],Result).

is_barrier(0',,   ',').
is_barrier(0';,   ';').
is_barrier(0'.,   '.').
is_barrier(0':,   ':').
