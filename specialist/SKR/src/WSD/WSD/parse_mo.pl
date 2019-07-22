
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
*  http://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

:- module(parse_machine_output, [
	pmo/2,
	pmo_job/1
   ]).

:- use_module(skr_lib(nls_strings), [
	is_print_string/1
   ]).

:- use_module(library(directory), [
	file_member_of_directory/3
   ]).

:- use_module(library(strings), [
	concat_atom/2
   ]).

% Process all the output files in a Scheduler Mega job's ResultDir_01/results directory
pmo_job(JobID) :-
	concat_atom(['/net/nls9/Scheduler/foo/', JobID, '/ResultDir_01/results'], SchedulerDir),
	file_member_of_directory(SchedulerDir, _RelativeFileName, AbsoluteFileName),
	format('~nPROCESSING file ~w~n', [AbsoluteFileName]),
	pmo(AbsoluteFileName, _),
	fail
      ; true.

% Process all the machine output contained in a file
pmo(MOFile, Lists) :-
	parse_machine_output(MOFile,
			     UtteranceTerms, PhraseTerms, CandidatesTerms, MappingsTerms),
	Lists = (UtteranceTerms, PhraseTerms, CandidatesTerms, MappingsTerms).

parse_machine_output(MOFile,
		     UtteranceTerms, PhraseTerms, CandidatesTerms, MappingsTerms) :-
	open(MOFile, read, InputStream),
	process_mo_file(InputStream,
			[], UtteranceTerms,
			[], PhraseTerms,
			[], CandidatesTerms,
			[], MappingsTerms).

process_mo_file(InputStream,
		UtteranceTermsIn,  UtteranceTermsOut,
		PhraseTermsIn,     PhraseTermsOut,
		CandidatesTermsIn, CandidatesTermsOut,
		MappingsTermsIn,   MappingsTermsOut):-

	read(InputStream, NextTerm),
	( NextTerm == end_of_file ->
	  UtteranceTermsOut  = UtteranceTermsIn,
	  PhraseTermsOut     = PhraseTermsIn,
	  CandidatesTermsOut = CandidatesTermsIn,
	  MappingsTermsOut   = MappingsTermsIn
	; process_one_utterance_output(InputStream, NextTerm,
				       UtteranceTermsIn,  UtteranceTermsNext,
				       PhraseTermsIn,     PhraseTermsNext,
				       CandidatesTermsIn, CandidatesTermsNext,
				       MappingsTermsIn,   MappingsTermsNext),
	  process_mo_file(InputStream,
			  UtteranceTermsNext,  UtteranceTermsOut,
			  PhraseTermsNext,     PhraseTermsOut,
			  CandidatesTermsNext, CandidatesTermsOut,
			  MappingsTermsNext,   MappingsTermsOut)
	).

process_one_utterance_output(InputStream, UtteranceTerm,
			     UtteranceTermsIn,  UtteranceTermsOut,
			     PhraseTermsIn,     PhraseTermsOut,
			     CandidatesTermsIn, CandidatesTermsOut,
			     MappingsTermsIn,   MappingsTermsOut) :-

	verify_and_add_utterance_term(UtteranceTerm, UtteranceTermsIn, UtteranceTermsOut),
	process_rest_utterance_terms(InputStream,
				     PhraseTermsIn,     PhraseTermsOut,
				     CandidatesTermsIn, CandidatesTermsOut,
				     MappingsTermsIn,   MappingsTermsOut).

process_rest_utterance_terms(InputStream,
			     PhraseTermsIn,     PhraseTermsOut,
			     CandidatesTermsIn, CandidatesTermsOut,
			     MappingsTermsIn,   MappingsTermsOut) :-
	  read(InputStream, NextTerm),
	  ( NextTerm == 'EOU' ->
	    write('.'), ttyflush,
	    PhraseTermsOut     = PhraseTermsIn,
	    CandidatesTermsOut = CandidatesTermsIn,
	    MappingsTermsOut   = MappingsTermsIn
	  ; verify_and_add_phrase_term(NextTerm, PhraseTermsIn, PhraseTermsNext),
	    read(InputStream, CandidatesTerm),
	    verify_and_add_candidates_term(CandidatesTerm, CandidatesTermsIn, CandidatesTermsNext),
	    read(InputStream, MappingsTerm),
	    verify_and_add_mappings_term(MappingsTerm, MappingsTermsIn, MappingsTermsNext),
	    process_rest_utterance_terms(InputStream,
					 PhraseTermsNext,     PhraseTermsOut,
					 CandidatesTermsNext, CandidatesTermsOut,
					 MappingsTermsNext,   MappingsTermsOut)
	  ).

verify_and_add_utterance_term(UtteranceTerm, UtteranceTermsIn, UtteranceTermsNext) :-
	UtteranceTerm = utterance(UttIDAtom, UttString, _StartPos/Length, _ReplPos),
	% format('~nUTTERANCE: ~q~n', [UtteranceTerm]),
	atom(UttIDAtom),
	is_print_string(UttString),
	!,
	UtteranceTermsNext = [UtteranceTerm|UtteranceTermsIn].
verify_and_add_utterance_term(UtteranceTerm, _, _) :-
	complain(utterance, UtteranceTerm).

verify_and_add_phrase_term(PhraseTerm, PhraseTermsIn, PhraseTermsNext) :-
	PhraseTerm = phrase(PhraseAtom, SyntaxList, _PosInfo, _ReplPos),
	atom(PhraseAtom),
	verify_syntax_list(SyntaxList),
	!,
	PhraseTermsNext = [PhraseTerm|PhraseTermsIn].
verify_and_add_phrase_term(PhraseTerm, _, _) :-
	complain(phrase, PhraseTerm).

verify_and_add_candidates_term(CandidatesTerm, CandidatesTermsIn, CandidatesTermsNext) :-
	CandidatesTerm = candidates(EVList),
 	verify_ev_list(EVList),
	!,
	CandidatesTermsNext = [CandidatesTerm|CandidatesTermsIn].
verify_and_add_candidates_term(CandidatesTerm, _, _) :-
	complain(candidates, CandidatesTerm).

verify_and_add_mappings_term(MappingsTerm, MappingsTermsIn, MappingsTermsNext) :-
	MappingsTerm = mappings(MapList),
	verify_map_list(MapList),
	!,
	MappingsTermsNext   = [MappingsTerm|MappingsTermsIn].
verify_and_add_mappings_term(MappingsTerm, _, _) :-
	complain(mappings, MappingsTerm).

verify_syntax_list([]).
verify_syntax_list([H|T]) :-
	( verify_syntax_element(H) ->
	  true
	; complain(syntax, H)
	),
	verify_syntax_list(T).

verify_syntax_element(adv(_)).
verify_syntax_element(aux(_)).
verify_syntax_element(compl(_)).
verify_syntax_element(conj(_)).
verify_syntax_element(det(_)).
verify_syntax_element(head(_)).
verify_syntax_element(mod(_)).
verify_syntax_element(modal(_)).
verify_syntax_element(prep(_)).
verify_syntax_element(pastpart(_)).
verify_syntax_element(pron(_)).
verify_syntax_element(punc(_)).
verify_syntax_element(shapes(_)).
verify_syntax_element(verb(_)).


verify_ev_list([]).
verify_ev_list([H|T]) :-
	verify_ev_term(H),
	verify_ev_list(T).

verify_ev_term(EV) :-
	EV = ev(NegValue, CUI, MetaTerm, MetaConcept, MetaWords,
		SemTypes, MatchMap, InvolvesHead, IsOverMatch, Sources, PosInfo),
	verify_neg_int(NegValue),
	verify_CUI(CUI),
	verify_meta_term(MetaTerm),
	verify_meta_concept(MetaConcept),
	verify_meta_words(MetaWords),
	verify_semtypes(SemTypes),
	verify_matchmap(MatchMap),
	verify_involves_head(InvolvesHead),
	verify_is_overmatch(IsOverMatch),
	verify_sources(Sources),
	verify_is_pos_info(PosInfo).

verify_sources([]).
verify_sources([H|T]) :-
	atom(H),
	verify_sources(T).

verify_neg_int(NegValue) :-
	integer(NegValue),
	NegValue < 0.

verify_CUI(CUI) :-
	atom(CUI),
	atom_codes(CUI, [67|Rest]),
	number_chars(_, Rest).

verify_meta_term(MetaTerm) :- atom(MetaTerm).

verify_meta_concept(MetaConcept) :- atom(MetaConcept).

verify_meta_words([]).
verify_meta_words([H|T]) :-
	atom(H),
	verify_meta_words(T).

verify_semtypes([]).
verify_semtypes([H|T]) :-
	atom(H),
	verify_semtypes(T).

verify_matchmap([]).
verify_matchmap([H|T]) :-
	is_a_matchmap(H),
	verify_matchmap(T).

is_a_matchmap([[I1,I2],[I3,I4], I5]) :-
	integer(I1), I1 >  0,
	integer(I2), I2 >  0,
	integer(I3), I3 >  0,
	integer(I4), I4 >  0,
	integer(I5), I5 >= 0.

verify_involves_head(yes).
verify_involves_head(no).

verify_is_overmatch(yes).
verify_is_overmatch(no).

% Can't have terminating condition here,
% because PosInfo *must* have at least one StartPos/Length term.
verify_posinfo([H|T]) :-
	verify_one_posinfo_term(H),
	verify_rest_posinfo(T).

verify_one_posinfo_term(StartPos/Length) :-
	integer(StartPos),
	integer(Length).

verify_rest_posinfo([]).
verify_rest_posinfo([H|T]) :-
	verify_one_posinfo_term(H),
	verify_rest_posinfo(T).


verify_map_list([]).
verify_map_list([H|T]) :-
	verify_map_term(H),
	verify_map_list(T).

verify_map_term(map(NegInt, EVList)) :-
	verify_neg_int(NegInt),
	verify_ev_list(EVList).

complain(TermType, Term) :-
	format('~nThe ~w term~n~n~q~n~ncould not be analyzed.~n', [TermType, Term]),
	abort.
