
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

% File:     mmi_med_cit.pl
% Module:   MMI Medline Citation
% Author:   Lan
% Purpose:  Operates on Medline citations


:- module(mmi_med_cit,[
    construct_citation_from_utterances/3,
    citation_field/3,
    compute_field_value/3
    ]).


:- use_module(skr_lib(nls_system), [
    control_option/1
    ]).

:- use_module(skr_lib(nls_strings),[
    split_string_completely/3
    ]).

:- use_module(lexicon(lexical),[
    concatenate_strings/3
    ]).

:- use_module(library(lists),[
    rev/2
    ]).

/* construct_citation_from_utterances(+UtteranceTerms, -CitationRecord, -UIAtom)

construct_citation_from_utterances/3 constructs as much of CitationRecord as
possible from UtteranceTerms. It attempts to determine the UIAtom and fills
in the title and abstract lines of CitationRecord. The other fields
(au, mh, so and py) are left uninstantiated. */

construct_citation_from_utterances(UtteranceTerms,CitationRecord,UIAtom) :-
    extract_ui_ti_ab(UtteranceTerms,UI,TI,AB),
    CitationRecord=citation_record(UI,_,TI,AB,_,_,_),
    (UI=[UIString] ->
	atom_codes(UIAtom,UIString)
    ;   UIAtom=none
    ),
    !.

extract_ui_ti_ab([],[],[],[]).
extract_ui_ti_ab([First|Rest], UI, TI, AB) :-
	extract_ui(First, UI),
	extract_ti_ab([First|Rest], TI, AB).

extract_ui(utterance(LabelAtom,_Text,_PosInfo,_ReplPos), [UIString]) :-
	atom_codes(LabelAtom, Label),
	split_string_completely(Label, ".", Strings0),
	rev(Strings0, [_N,_Field|RevRest]),
	rev(RevRest, Rest),
	concatenate_strings(Rest, ".", UIString),
	!.
extract_ui(_, []).

extract_ti_ab([First|Rest], [FirstTI|RestTI], AB) :-
	is_title_utterance(First, FirstTI),
	!,
	extract_ti_ab(Rest,RestTI, AB).
extract_ti_ab(Utterances, [], AB) :-
	extract_lines(Utterances, AB).

is_title_utterance(utterance(LabelAtom,Text,_PosInfo,_ReplPos), Text) :-
	atom_codes(LabelAtom, Label),
	split_string_completely(Label, ".", Strings0),
	rev(Strings0, [_N,Field|_]),
	Field == "ti",
	!.

extract_lines([],[]).
extract_lines([utterance(_Label,Text,_PosInfo,_ReplPos)|Rest],[Text|ExtractedRest]) :-
    extract_lines(Rest,ExtractedRest).


/* citation_field(+FieldName, ?CitationRecord, ?Field)
   citation_field(+FieldName, +CitationRecordIn, -CitationRecordOut, +Value)
   compute_field_value(+FieldName, +Field, -Value)

citation_field/3 instantiates or retrieves the FieldName Field of
CitationRecord.  Field consists of a list of lines (strings).
For other FieldNames, citation_value/3 is
identical to citation_field/3.
E.g., citation_field(ui,CR,["93000000"])
      citation_value(ui,CR,"93000000").
compute_field_value/3 does the computation for citation_value/3.
*/

citation_field(ui, citation_record(UI,_,_,_,_,_,_), UI).
citation_field(au, citation_record(_,AU,_,_,_,_,_), AU).
citation_field(ti, citation_record(_,_,TI,_,_,_,_), TI).
citation_field(ab, citation_record(_,_,_,AB,_,_,_), AB).
citation_field(mh, citation_record(_,_,_,_,MH,_,_), MH).
citation_field(so, citation_record(_,_,_,_,_,SO,_), SO).
citation_field(py, citation_record(_,_,_,_,_,_,PY), PY).

compute_field_value(ui,[FirstLine|_],FirstLine) :-
    !.
compute_field_value(py,[FirstLine|_],FirstLine) :-
    !.
compute_field_value(mh,Field,Field) :-
    \+control_option(pre_parsed_mesh),
    !.
compute_field_value(mh,Field,Value) :-
    control_option(pre_parsed_mesh),
    !,
    parse_mesh_entries(normal,Field,Value).
compute_field_value(_FieldName,Field,Field).

/* ************************************************************************
   ************************************************************************
   ************************************************************************
         The following predicates are taken from dr_indexer_parse.pl
   ************************************************************************
   ************************************************************************
   ************************************************************************ */


/* parse_mesh_field(+Grammar, +MeshFields, -MeshTerms)
parse_mesh_field/3 parses the list of MeshFields into MeshTerms according
to Grammar, which is either 'normal' or 'printed'.
MeshFields is a list of lines consisting of mesh headings with optional
subheadings.  They can span two or more lines and are separated by ";".
MeshTerms is a list mesh terms.  See the examples below.

Mesh heading = "Animal"
Mesh term = [heading("Animal",nm)]

Mesh heading = "*Laser"
Mesh term = [heading("Laser",main)]

Mesh heading = "Choroid/*RADIATION EFFECTS/ULTRASTRUCTURE"
Mesh term = [heading("Choroid",main),
             sub("RADIATION EFFECTS",main),
             sub("ULTRASTRUCTURE",nm)]

Mesh heading = "Aminolevulinic Acid Synthetase/ANTAGONISTS & INHIBITORS"
Mesh term = [heading("Aminolevulinic Acid Synthetase",nm),
             sub("ANTAGONISTS & INHIBITORS",nm)]  */

/* parse_mesh_entries(+Grammar, +MeshEntries, -MeshTerms)
parse_mesh_entries/3 parses a list of MeshEntries into a list of MeshTerms
according to Grammar, which is either 'normal' or 'printed'.
See parse_mesh_field/3 above for examples.  */

parse_mesh_entries(_Grammar,[],[]) :-
    !.
parse_mesh_entries(Grammar,[First|Rest],[ParsedFirst|ParsedRest]) :-
    parse_mesh_entry(Grammar,First,ParsedFirst),
    parse_mesh_entries(Grammar,Rest,ParsedRest),
    !.


/* parse_mesh_entry(+Grammar, +MeshEntry, -ParsedMeshEntry)
parse_mesh_entry/3 is an auxiliary predicate for parse_mesh_entries/3. */

parse_mesh_entry(normal,MeshEntry,ParsedMeshEntry) :-
    phrase(m_entry(ParsedMeshEntry),MeshEntry),
    !.
parse_mesh_entry(printed,MeshEntry,ParsedMeshEntry) :-
    phrase(p_m_entry(ParsedMeshEntry),MeshEntry),
    !.
parse_mesh_entry(_,_,[]) :-
    !.


/*  MESH ENTRY GRAMMARs  */

/* Note that one version of this grammar forces a heading to be main if any
   of its subheadings is main in order to accurately reflect the heading's
   importance.  However, when the parsed entry must be used later to reconstruct
   the print version of the entry, this forcing must not be done.  Definitions
   for both normal and printed versions are given. */

/* NORMAL MeSH entry grammar */

m_entry(E) --> "*", m_heading(H), m_subs(Ss,_),
               {E=[heading(H,main)|Ss]}

            |  m_heading(H), m_subs(Ss,MNM),
               {E=[heading(H,MNM)|Ss]}.

m_heading(H) --> m_word(H).

m_subs(Ss,MNM) --> m_sub(S,MNM1), m_subs(Ts,MNM2),
                   {Ss=[S|Ts], ((MNM1==main; MNM2==main) -> MNM=main | MNM=nm)}

                |  {Ss=[], MNM=nm}.

% allow for intervening space
m_sub(S,MNM) --> "/ *", m_word(W),
                 {S=sub(W,main), MNM=main}

              |   "/ ", m_word(W),
                  {S=sub(W,nm), MNM=nm}

              |  "/*", m_word(W),
                 {S=sub(W,main), MNM=main}

              |   "/", m_word(W),
                  {S=sub(W,nm), MNM=nm}.

m_word(W) --> [C], {C=\="/", C=\="*"}, m_word(Cs),
              {W=[C|Cs]}

           |  {W=[]}.


/* PRINTED MeSH entry grammar */

p_m_entry(E) --> "*", m_heading(H), p_m_subs(Ss),
                 {E=[heading(H,main)|Ss]}

              |  m_heading(H), p_m_subs(Ss),
                 {E=[heading(H,nm)|Ss]}.

p_m_subs(Ss) --> p_m_sub(S), p_m_subs(Ts), {Ss=[S|Ts]}

              |  {Ss=[]}.

% allow for intervening space
p_m_sub(S) --> "/ *", m_word(W), {S=sub(W,main)}

            |  "/ ", m_word(W), {S=sub(W,nm)}

            |  "/*", m_word(W), {S=sub(W,main)}

            |  "/", m_word(W), {S=sub(W,nm)}.


