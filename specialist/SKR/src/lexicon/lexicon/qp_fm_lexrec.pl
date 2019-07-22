
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

/* qp_fm_lexrec.pl - file to memory grammar for lexical records
*/

:- module(qp_fm_lexrec, [
	fm_lexical_record/4
   ]).

:- use_module(lexicon(qp_fm_utils), [
	fm_to_rparen/3,
	fm_to_pipe/3,
	fm_to_newline/3,
	fm_spaces/2,
	fm_newline/2
   ]).

:- use_module(lexicon(qp_fm_misc), [
	fm_whinfcomp/3,
	fm_prep_phrase/3,
	fm_misc/3,
	fm_ingcomp/3,
	fm_infcomp/3,
	fm_fincomp/3,
	fm_edcomp/3,
	fm_binfcomp/3,
	fm_ascomp/3
   ]).

:- use_module(skr_lib(sicstus_utils), [
	string_size/2,
	substring/4
   ]).

%%% The token denoting a lexical entry is called "entries",
%%% and not "entry" for historical reasons.
fm_lexical_record(R, EUI) -->
	"{base=", fm_to_newline(BL), {atom_codes(B,BL) }, fm_newline,
	fm_spelling_variants(Ss),
	fm_lexical_entry(E, EUI),
	fm_annotations(A),
	fm_signature(S),
	"}", fm_newline,
	{ R = lexrec:[base:[B], spelling_variants:Ss, entries:E, annotations:A, signature:S] }.

fm_spelling_variants([S|R]) -->
	fm_spaces, "spelling_variant=", fm_to_newline(SL), { atom_codes(S, SL) }, fm_newline,
	fm_spelling_variants(R).
fm_spelling_variants([]) --> [].

%%% at least one lexical entry
fm_lexical_entry([E], EUI) -->
	(	fm_adj_entry(E, EUI)    -> { true }
	;	fm_adv_entry(E, EUI)    -> { true }
	;	fm_aux_entry(E, EUI)    -> { true }
	;	fm_compl_entry(E, EUI)  -> { true }
	;	fm_conj_entry(E, EUI)   -> { true }
	;	fm_det_entry(E, EUI)    -> { true }
	;	fm_modal_entry(E, EUI)  -> { true }
	;	fm_noun_entry(E, EUI)   -> { true }
	;	fm_prep_entry(E, EUI)   -> { true }
	;	fm_pron_entry(E, EUI)   -> { true }
	;	fm_verb_entry(E, EUI)   -> { true }
	).

fm_annotations([A|R]) -->
	fm_spaces, "annotation=", fm_to_newline(AL), { atom_codes(A, AL) }, fm_newline,
	fm_annotations(R).
fm_annotations([]) --> [].

fm_signature([S]) -->
	fm_spaces, "signature=", fm_to_newline(SL), { atom_codes(S, SL) }, fm_newline.
fm_signature([]) --> [].

%%% adjectives

fm_entry(EUI) -->
	fm_spaces, "entry=", fm_to_newline(NL), { !, fm_number_codes(EUI, NL), ! }, fm_newline.

fm_adj_entry(E, EUI) -->
	fm_entry(EUI),
	fm_spaces, "cat=adj", fm_newline,
	fm_adj_variants(Vs),
	fm_adj_positions(Ps),
	fm_adj_complements(Cs),
	fm_adj_stative(S),
	fm_adj_nominalizations(Ns),
	fm_misc(M),
	{ E = entry:[num:[EUI], cat:[adj], variants:Vs, positions:Ps,
	  complements:Cs, stative:S, nominalizations:Ns, misc:M] }.

%%% one or more
fm_adj_variants([V|R]) -->
    fm_spaces, "variants=", fm_adj_variant(V), fm_newline,
    (	fm_adj_variants(R)
    ;	{ R = [] }
    ),
    !.

fm_adj_variant(reg) --> "reg".
fm_adj_variant(regd) --> "regd".
fm_adj_variant(irreg:[Pos, Comp, Super]) -->
	"irreg|",
	fm_to_pipe(PosL),   { atom_codes(Pos,  PosL)   }, "|",
	fm_to_pipe(CompL),  { atom_codes(Comp, CompL)  }, "|",
	fm_to_pipe(SuperL), { atom_codes(Super,SuperL) }, "|".
fm_adj_variant(inv:[periph]) --> "inv;periph".
fm_adj_variant(inv) --> "inv".

%%% zero or more
fm_adj_positions([P|Ps]) -->
	fm_spaces, "position=", fm_adj_position(P), fm_newline,
	fm_adj_positions(Ps).
fm_adj_positions([]) --> [].

fm_adj_position(attrib:[1]) --> "attrib(1)".
fm_adj_position(attrib:[2]) --> "attrib(2)".
fm_adj_position(attrib:[3]) --> "attrib(3)".
fm_adj_position(pred) --> "pred".
fm_adj_position(attribc) --> "attribc".
fm_adj_position(post) --> "post".

%%% zero or more
fm_adj_complements([C|Cs]) -->
	fm_spaces, "compl=", fm_adj_complement(C), fm_newline,
	fm_adj_complements(Cs).
fm_adj_complements([]) --> [].

fm_adj_complement(V) --> fm_infcomp(V).
fm_adj_complement(V) --> fm_ascomp(V).
fm_adj_complement(V) --> fm_fincomp(V).
fm_adj_complement(V) --> fm_whinfcomp(V).
fm_adj_complement(whfincomp) --> "whfincomp".
fm_adj_complement(advbl) --> "advbl".
fm_adj_complement(P) --> fm_prep_phrase(P).

%%% optional
fm_adj_stative([yes]) -->
	fm_spaces, "stative", fm_newline.
fm_adj_stative([]) --> [].

%%% zero or more
fm_adj_nominalizations([N|Ns]) -->
	fm_spaces, "nominalization=", fm_to_newline(L), { atom_codes(N,L) }, fm_newline,
	fm_adj_nominalizations(Ns).
fm_adj_nominalizations([]) --> [].

%%% adverbs

fm_adv_entry(E, EUI) -->
	fm_entry(EUI),
	fm_spaces, "cat=adv", fm_newline,
	fm_adv_variants(Vs),
	fm_adv_interrogative(I),
	fm_adv_modification_types(Ms),
	fm_adv_negative(Neg),
	fm_misc(M),
	{ E = entry:[num:[EUI], cat:[adv], variants:Vs, interrogative:I,
	  modification_types:Ms, negative:Neg, misc:M] }.

%%% one or more variants
fm_adv_variants([V|R]) -->
	fm_spaces, "variants=", fm_adv_variant(V), fm_newline,
	(	fm_adv_variants(R)
	;	{ R = [] }
	),
	!.

fm_adv_variant(reg) --> "reg".
fm_adv_variant(regd) --> "regd".
fm_adv_variant(irreg:[Pos, Comp, Super]) -->
	"irreg|",
	fm_to_pipe(PosL),   { atom_codes(Pos,  PosL)   }, "|",
	fm_to_pipe(CompL),  { atom_codes(Comp, CompL)  }, "|",
	fm_to_pipe(SuperL), { atom_codes(Super,SuperL) }, "|".
fm_adv_variant(inv:[periph]) --> "inv;periph".
fm_adv_variant(inv) --> "inv".

fm_adv_interrogative([yes]) -->
	fm_spaces, "interrogative", fm_newline.
fm_adv_interrogative([]) --> [].

%%% one or more
fm_adv_modification_types([M|R]) -->
	fm_spaces, "modification_type=", fm_adv_modification_type(M), fm_newline,
	(	fm_adv_modification_types(R)
	;	{ R = [] }
	),
	!.

fm_adv_modification_type(particle) --> "particle".
fm_adv_modification_type(intensifier) --> "intensifier".
fm_adv_modification_type(sentence_modifier:[locative]) --> "sentence_modifier;locative".
fm_adv_modification_type(sentence_modifier:[temporal]) --> "sentence_modifier;temporal".
fm_adv_modification_type(sentence_modifier:[manner]) --> "sentence_modifier;manner".
fm_adv_modification_type(verb_modifier:[locative]) --> "verb_modifier;locative".
fm_adv_modification_type(verb_modifier:[temporal]) --> "verb_modifier;temporal".
fm_adv_modification_type(verb_modifier:[manner]) --> "verb_modifier;manner".

fm_adv_negative([broad_negative]) -->
	fm_spaces, "broad_negative", fm_newline.
fm_adv_negative([negative]) -->
	fm_spaces, "negative", fm_newline.
fm_adv_negative([]) --> [].

%%% auxiliaries

fm_aux_entry(E, EUI) -->
	fm_entry(EUI),
	fm_spaces, "cat=aux", fm_newline,
	fm_aux_variant(V),
	fm_misc(M),
	{ E = entry:[num:[EUI], cat:[aux], variants:V, misc:M] }.

fm_aux_variant(V) -->
	fm_spaces, "variants=", fm_aux_variant_val(V), fm_newline.
fm_aux_variant([]) -->[].

fm_aux_variant_val(reg) --> "reg".
fm_aux_variant_val(regd) --> "regd".
% There are currently no entries for irreg auxes
fm_aux_variant_val(irreg:[V1, V2, V3, V4, V5]) -->
	"irreg|",
	fm_to_pipe(VL1),  { atom_codes(V1, VL1) }, "|",
	fm_to_pipe(VL2),  { atom_codes(V2, VL2) }, "|",
	fm_to_pipe(VL3),  { atom_codes(V3, VL3) }, "|",
	fm_to_pipe(VL4),  { atom_codes(V4, VL4) }, "|",
	fm_to_pipe(VL5),  { atom_codes(V5, VL5) }, "|".

%%% complementizers

fm_compl_entry(E, EUI) -->
	fm_entry(EUI),
	fm_spaces, "cat=compl", fm_newline,
	fm_misc(M),
	{ E = entry:[num:[EUI], cat:[compl], misc:M] }.

%%% conjunctions

fm_conj_entry(E, EUI) -->
	fm_entry(EUI),
	fm_spaces, "cat=conj", fm_newline,
	fm_misc(M),
	{ E = entry:[num:[EUI], cat:[conj], misc:M] }.

%%% determiners

fm_det_entry(E, EUI) -->
	fm_entry(EUI),
	fm_spaces, "cat=det", fm_newline,
	fm_det_variant(V),
	fm_det_interrogative(I),
	fm_det_demonstrative(D),
	fm_misc(M),
	{ E = entry:[num:[EUI], cat:[det], variants:V, interrogative:I, demonstrative:D, misc:M]}.

fm_det_variant([V]) -->
    fm_spaces, "variants=", fm_det_variant_val(V), fm_newline.

fm_det_variant_val(free) --> "free".
fm_det_variant_val(singuncount) --> "singuncount".
fm_det_variant_val(pluruncount) --> "pluruncount".
fm_det_variant_val(sing) --> "sing".
fm_det_variant_val(plur) --> "plur".
fm_det_variant_val(uncount) --> "uncount".

fm_det_interrogative([yes]) -->
	fm_spaces, "interrogative", fm_newline.
fm_det_interrogative([]) --> [].

fm_det_demonstrative([yes]) -->
	fm_spaces, "demonstrative", fm_newline.
fm_det_demonstrative([]) --> [].

%%% modals

fm_modal_entry(E, EUI) -->
	fm_entry(EUI),
	fm_spaces, "cat=modal", fm_newline,
	fm_modal_variant(V),
	fm_misc(M),
	{ E = entry:[num:[EUI], cat:[modal], variants:V, misc:M] }.

%%% variant is optional
fm_modal_variant([[V]]) -->
    fm_spaces, "variants=", !, fm_to_newline(VL), { atom_codes(V, VL) }, fm_newline.
fm_modal_variant([[]]) --> [].

%%% nouns

fm_noun_entry(E, EUI) -->
	fm_entry(EUI),
	fm_spaces, "cat=noun", fm_newline,
	fm_noun_variants(Vs),
	fm_noun_complements(Cs),
	fm_noun_nominalizations(Ns),
	fm_noun_proper(P),
	fm_misc(M),
	{ E = entry:[num:[EUI], cat:[noun], variants:Vs, complements:Cs, nominalizations_of:Ns, proper:P, misc:M] }.

%%% one or more variants
fm_noun_variants([V|R]) -->
    fm_spaces, "variants=", fm_noun_variant(V),	fm_newline,
    (	fm_noun_variants(R)
    ;	{ R = [] }
    ),
    !.

fm_noun_variant(reg) --> "reg".
fm_noun_variant(glreg) --> "glreg".
fm_noun_variant(irreg:[Sing,Plu]) -->
	"irreg|",
	fm_to_pipe(SingL), { atom_codes(Sing, SingL) }, "|",
	fm_to_pipe(PluL),  { atom_codes(Plu,  PluL)  }, "|".
fm_noun_variant(inv) --> "inv".
fm_noun_variant(uncount) --> "uncount".
fm_noun_variant(group:[reg]) --> "group(reg)".
fm_noun_variant(group:[glreg]) --> "group(glreg)".
fm_noun_variant(group:[irreg:[Sing,Plu]]) -->
	"group(irreg|",
	fm_to_pipe(SingL), { atom_codes(Sing, SingL) }, "|",
	fm_to_pipe(PluL),  { atom_codes(Plu,  PluL)  }, "|)".
fm_noun_variant(group:[sing]) --> "group(sing)".
fm_noun_variant(group:[metareg]) --> "group(metareg)".
fm_noun_variant(groupuncount) --> "groupuncount".
fm_noun_variant(sing) --> "sing".
fm_noun_variant(plur) --> "plur".
fm_noun_variant(metareg) --> "metareg".

%%% zero or more
fm_noun_complements([C|R]) -->
    fm_spaces, "compl=", fm_noun_complement(C), fm_newline,
    fm_noun_complements(R).
fm_noun_complements([]) --> [].

fm_noun_complement(C) --> fm_infcomp(C).
fm_noun_complement(C) --> fm_ascomp(C).
fm_noun_complement(C) --> fm_fincomp(C).
fm_noun_complement(C) --> fm_whinfcomp(C).
fm_noun_complement(whfincomp) --> "whfincomp".
fm_noun_complement(P) --> fm_prep_phrase(P).

%%% zero or more
fm_noun_nominalizations([N|R]) -->
	fm_spaces, "nominalization_of=", fm_to_newline(NL), { atom_codes(N, NL) }, fm_newline,
	fm_noun_nominalizations(R).
fm_noun_nominalizations([]) --> [].

fm_noun_proper([yes]) -->
	fm_spaces, "proper", fm_newline.
fm_noun_proper([]) --> [].

%%% prepositions

fm_prep_entry(E, EUI) -->
	fm_entry(EUI),
	fm_spaces, "cat=prep", fm_newline,
	fm_misc(M),
	{ E = entry:[num:[EUI], cat:[prep], misc:M] }.

%%% pronouns

fm_pron_entry(E, EUI) -->
	fm_entry(EUI),
	fm_spaces, "cat=pron", fm_newline,
	fm_pron_variants(V),
	fm_pron_gender(G),
	fm_pron_interrogative(I),
	fm_pron_types(T),
	fm_misc(M),
	{ E = entry:[num:[EUI], cat:[pron], variants:V, gender:G, interrogative:I, type:T, misc:M] }.

%%% one or more variants
fm_pron_variants([V|R]) -->
	fm_spaces, "variants=", fm_pron_variant(V), fm_newline,
	(	fm_pron_variants(R)
	;	{ R = [] }
	),
	!.

fm_pron_variant(fst_sing) --> "fst_sing".
fm_pron_variant(fst_plur) --> "fst_plur".
fm_pron_variant(sec_sing) --> "sec_sing".
fm_pron_variant(sec_plur) --> "sec_plur".
fm_pron_variant(second) --> "second".
fm_pron_variant(thr_sing) --> "thr_sing".
fm_pron_variant(thr_plur) --> "thr_plur".
fm_pron_variant(third) --> "third".
fm_pron_variant(free) --> "free".

%%% single gender
fm_pron_gender([G]) -->
    fm_spaces, "gender=", !, fm_pron_gender_val(G), fm_newline.
fm_pron_gender([]) --> [].

fm_pron_gender_val(pers:[fem]) --> "pers(fem)".
fm_pron_gender_val(pers:[masc]) --> "pers(masc)".
fm_pron_gender_val(pers) --> "pers".
fm_pron_gender_val(neut) --> "neut".

%%% single interrogative
fm_pron_interrogative([yes]) -->
	fm_spaces, "interrogative", !, fm_newline.
fm_pron_interrogative([]) --> [].

%%% zero or more types
fm_pron_types([T|R]) -->
	fm_spaces, "type=", fm_pron_type(T), fm_newline,
	!,
	fm_pron_types(R).
fm_pron_types([]) --> [].

fm_pron_type(subj) --> "subj".
fm_pron_type(obj) --> "obj".
fm_pron_type(refl) --> "refl".
fm_pron_type(poss) --> "poss".
fm_pron_type(possnom) --> "possnom".
fm_pron_type(dem) --> "dem".
fm_pron_type(univ) --> "univ".
fm_pron_type(indef:[assert]) --> "indef(assert)".
fm_pron_type(indef:[nonassert]) --> "indef(nonassert)".
fm_pron_type(indef:[neg]) --> "indef(neg)".

%%% verbs

fm_verb_entry(E, EUI) -->
	fm_entry(EUI),
	fm_spaces, "cat=verb", fm_newline,
	fm_verb_variants(Vs),
	fm_verb_complements(Cs),
	fm_verb_nominalizations(Ns),
	fm_misc(M),
	{ E = entry:[num:[EUI], cat:[verb], variants:Vs, complements:Cs, nominalizations:Ns, misc:M] }.

%%% one or more
fm_verb_variants([V|R]) -->
	fm_spaces, "variants=", fm_verb_variant(V), fm_newline,
	(	fm_verb_variants(R)
	;	{ R = [] }
	),
	!.

fm_verb_variant(reg) --> "reg".
fm_verb_variant(regd) --> "regd".
% variants=irreg|resee|resees|resaw|reseen|reseeing|
fm_verb_variant(irreg:[V1, V2, V3, V4, V5]) -->
	"irreg|",
	fm_to_pipe(VL1), { atom_codes(V1, VL1) }, "|",
	fm_to_pipe(VL2), { atom_codes(V2, VL2) }, "|",
	fm_to_pipe(VL3), { atom_codes(V3, VL3) }, "|",
	fm_to_pipe(VL4), { atom_codes(V4, VL4) }, "|",
	fm_to_pipe(VL5), { atom_codes(V5, VL5) }, "|".

%%% complements include, intran, tran, etc at least one of which must be present
fm_verb_complements([C|R]) -->
    fm_verb_complement_value(C),    
    (	fm_verb_complements(R)
    ;	{ R = [] }
    ),
    !.

fm_verb_complement_value(V) -->
    (	fm_verb_intran(V)
    ;	fm_verb_tran(V)
    ;	fm_verb_link(V)
    ;	fm_verb_ditran(V)
    ;	fm_verb_cplxtran(V)
    ).

%%% intransitives
fm_verb_intran(intran:[compl:[], part:P, mvmt:[]]) -->
    fm_spaces, "intran", fm_verb_particle(P), fm_newline.

%%% transitives
fm_verb_tran(tran:[compl:[C], part:P, mvmt:M]) -->
    fm_spaces, "tran=",
	fm_verb_tran_value(C),
	fm_verb_particle(P),
	fm_verb_tran_movement(M), fm_newline.

fm_verb_tran_value(np) --> "np".
fm_verb_tran_value(np:[V]) -->
	"np|", fm_to_pipe(L), { atom_codes(V, L) }, "|".
fm_verb_tran_value(V) --> fm_prep_phrase(V).
fm_verb_tran_value(V) --> fm_binfcomp(V).
fm_verb_tran_value(V) --> fm_infcomp(V).
fm_verb_tran_value(V) --> fm_ingcomp(V).
fm_verb_tran_value(V) --> fm_ascomp(V).
fm_verb_tran_value(V) --> fm_fincomp(V).
fm_verb_tran_value(V) --> fm_whinfcomp(V).
fm_verb_tran_value(whfincomp) --> "whfincomp".

% Old version of fm_verb_tran_movement:
% fm_verb_tran_movement([nopass]) --> ";nopass".
% fm_verb_tran_movement([noprtmvt]) --> ";noprtmvt".
% These two clauses have been modified to handle lines such as
%        tran=pphr(on,np);part(off);nopass;noprtmvt
%        tran=pphr(over,np);part(off);nopass;noprtmvt
%        tran=pphr(against,np);part(off);nopass;noprtmvt
%        tran=pphr(between,np);part(off);nopass;noprtmvt

fm_verb_tran_movement([nopass|Rest])   --> ";nopass",    fm_verb_tran_movement(Rest).
fm_verb_tran_movement([noprtmvt|Rest]) --> ";noprtmvt" , fm_verb_tran_movement(Rest).
fm_verb_tran_movement([]) --> [].

%%% ditransitives
fm_verb_ditran(ditran:[compl:[F, S], part:P, mvmt:M]) -->
	fm_spaces, "ditran=", fm_verb_first_ditran_value(F), ",",
	fm_verb_second_ditran_value(S),
	fm_verb_particle(P),
	fm_verb_ditran_movement(M), fm_newline.

%%% first ditran object
fm_verb_first_ditran_value(np) --> "np".
fm_verb_first_ditran_value(np:[V]) -->
	"np|", fm_to_pipe(L), { atom_codes(V, L) }, "|".
fm_verb_first_ditran_value(V) --> fm_prep_phrase(V).
fm_verb_first_ditran_value(V) --> fm_fincomp(V).
fm_verb_first_ditran_value(V) --> fm_ingcomp(V).

%%% second ditran object
fm_verb_second_ditran_value(np) --> "np".
fm_verb_second_ditran_value(np:[V]) -->
	"np|", fm_to_pipe(L), { atom_codes(V, L) }, "|".
fm_verb_second_ditran_value(V) --> fm_prep_phrase(V).
fm_verb_second_ditran_value(V) --> fm_fincomp(V).
fm_verb_second_ditran_value(V) --> fm_whinfcomp(V).
fm_verb_second_ditran_value(whfincomp) --> "whfincomp".

%%% allowable ditran movement codes
fm_verb_ditran_movement([datmvt|Rest])   --> ";datmvt",    fm_verb_ditran_movement(Rest).
fm_verb_ditran_movement([nopass|Rest])   --> ";nopass",    fm_verb_ditran_movement(Rest).
fm_verb_ditran_movement([noprtmvt|Rest]) --> ";noprtmvt",  fm_verb_ditran_movement(Rest).
fm_verb_ditran_movement([]) --> [].

%%% linking verbs
fm_verb_link(link:[compl:[C], part:P, mvmt:M]) -->
	fm_spaces, "link=",
	fm_verb_link_value(C),
	fm_verb_particle(P),
	fm_verb_link_movement(M), fm_newline.

fm_verb_link_value(np) --> "np".
fm_verb_link_value(np:[V]) -->
	"np|", fm_to_pipe(L), { atom_codes(V, L) }, "|".
fm_verb_link_value(V) --> fm_prep_phrase(V).
fm_verb_link_value(adj) --> "adj".
fm_verb_link_value(advbl) --> "advbl".
fm_verb_link_value(V) --> fm_edcomp(V).
fm_verb_link_value(V) --> fm_infcomp(V).
fm_verb_link_value(V) --> fm_ingcomp(V).
fm_verb_link_value(V) --> fm_fincomp(V).
fm_verb_link_value(V) --> fm_whinfcomp(V).
fm_verb_link_value(whfincomp) --> "whfincomp".

%%% allowable movement codes for linking verbs
fm_verb_link_movement([noprtmvt]) --> ";noprtmvt".
fm_verb_link_movement([]) --> [].

%%% complex transitive verbs
fm_verb_cplxtran(cplxtran:[compl:[F, S], part:P, mvmt:M]) -->
	fm_spaces, "cplxtran=", fm_verb_first_cplxtran_value(F), ",",
	fm_verb_second_cplxtran_value(S),
	fm_verb_particle(P),
	fm_verb_cplxtran_movement(M), fm_newline.

%%% first cplxtran object
fm_verb_first_cplxtran_value(np) --> "np".
fm_verb_first_cplxtran_value(np:[V]) -->
	"np|", fm_to_pipe(L), { atom_codes(V, L) }, "|".
fm_verb_first_cplxtran_value(V) --> fm_prep_phrase(V).
fm_verb_first_cplxtran_value(V) --> fm_fincomp(V).
fm_verb_first_cplxtran_value(V) --> fm_ingcomp(V).

%%% second object
fm_verb_second_cplxtran_value(np) --> "np".
fm_verb_second_cplxtran_value(np:[V]) -->
	"np|", fm_to_pipe(L), { atom_codes(V, L) }, "|".
fm_verb_second_cplxtran_value(adj) --> "adj".
fm_verb_second_cplxtran_value(advbl) --> "advbl".
fm_verb_second_cplxtran_value(V) --> fm_binfcomp(V).
fm_verb_second_cplxtran_value(V) --> fm_edcomp(V).
fm_verb_second_cplxtran_value(V) --> fm_infcomp(V).
fm_verb_second_cplxtran_value(V) --> fm_ingcomp(V).
fm_verb_second_cplxtran_value(V) --> fm_ascomp(V).
fm_verb_second_cplxtran_value(V) --> fm_prep_phrase(V).

%%% allowable cplxtran movement codes
fm_verb_cplxtran_movement([nopass|Rest])   --> ";nopass",   fm_verb_cplxtran_movement(Rest).
fm_verb_cplxtran_movement([noprtmvt|Rest]) --> ";noprtmvt", fm_verb_cplxtran_movement(Rest).
fm_verb_cplxtran_movement([]) --> [].

%%% verb particles
fm_verb_particle([P]) -->
	";part(", fm_to_rparen(L), { atom_codes(P,L) }, ")".
fm_verb_particle([]) --> [].

%%% verb nominalizations
fm_verb_nominalizations([N|R]) -->
	fm_spaces, "nominalization=", fm_to_newline(L), { atom_codes(N,L) }, fm_newline,
	fm_verb_nominalizations(R).
fm_verb_nominalizations([]) --> [].


/*==========================================================
%FUNCTION NAME
        fm_number_codes
%PURPOSE
        Given a string of the form "E9999999",or an empty list, return 
        the atom version

          This routine should insure that there is an E as the
          first character, and that there are 7 digits to follow,
          otherwise this routine should spew out an error.

%I/O PARAMETER LIST
	-E_ATOM   ATOM (if the routine is true)
        +E_STRING CHAR LIST 

%EXAMPLE CALL
	fm_number_codes(E_ATOM,"E0065342").
%METHOD
                    Verify the E part

                       Now verify that the remaining seven positions 
                       only contain digits

%NOTES

   This routine has been tested on the following cases

  %  fm_number_codes('E0068931', E_STRING),   % Test for valid string
  %  fm_number_codes('E001234',  E_STRING1),  % Test for less than 7 digits
  %  fm_number_codes('E00A1234', E_STRING2),  % Test for alpha chars in digit 
                                              % space
  %  fm_number_codes('0067395',  E_STRING3),  % Test for No E at the beginning
  %  fm_number_codes('ABC'    ,  E_STRING4),  % Test for junk
  %  fm_number_codes([]       ,  E_STRING5),  % Test for []
  %  fm_number_codes('00E7395',  E_STRING3).  % Test for E Not at the beginning

%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
fm_number_codes([], [32]) :- !.
fm_number_codes([], []  ) :- !.
fm_number_codes(E_ATOM, E_STRING) :-
   (                          % 0
	    
    atom_codes(E_ATOM,E_STRING),
    %format(user_error,"in fm_number_codes with E_ATOM~q~n",E_ATOM),
    
 /* -------------------------------------
    Verify that the string is 8 chars long
    ------------------------------------- */
    string_size(E_ATOM,DIGITS_SIZE),
    DIGITS_SIZE =:= 8 ->
    ( % 1
      % format(user_error,"~q is small enough ~n", E_ATOM),
    /* -------------------------------------
       Verify the E part
       ------------------------------------- */
       substring(E_ATOM,'E',0, _ ) ->
       /* -------------------------------------------------------------
          Verify that the remaining seven positions only contain digits
          ------------------------------------------------------------- */
       ( % 2
          substring(E_ATOM,DIGITS,1,7),
	  atom_codes(DIGITS,CDIGITS),
          number_codes(_NUMBER,CDIGITS)->
          ( %3
	      atom_codes(E_ATOM,E_STRING)
	      %format(user_error,"~q is a good entry number",E_ATOM)
             
          ) % Verified that there are 7 digits                        3
          ;  % Else
	  (
              atom_codes(E_ATOM,E_STRING),
	      format(user_error,"~q Contains non numeric characters in it~n",
		     DIGITS),
	      fail
          )
       )  % Verified that there is an E                               2
       ;  % Else
       ( 
          atom_codes(E_ATOM,E_STRING),
          format(user_error,"~q does not contain an E at the beginning~n",E_ATOM),   
          fail 
       )

    )   % Verified that the string is the right size                 1
    ;  % Else
    (
       atom_codes(E_ATOM,E_STRING),
       format(user_error,"~q is not the right size~n",E_ATOM),
       fail
    ) 
  
    ). /*** End enumber_codes  0  */ 

