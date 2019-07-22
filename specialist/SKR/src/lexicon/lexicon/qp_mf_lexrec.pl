
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

/*==========================================================

%SOURCE FILE
	qp_mf_lexrec.pl

%DESCRIPTION OF FILE
	Quintus Prolog (.pl) source file. 
                             Memory to file grammar for lexical records

%REVISED
	31Mar94 divita -- Initial Version

%%
==========================================================*/

:- module(qp_mf_lexrec, [
	mf_lexical_record/3,
	enumber_codes/2
   ]).

:- use_module(lexicon(qp_mf_utils), [
	mf_tab/2,
	mf_newline/2
   ]).

:- use_module(lexicon(qp_mf_misc), [
	mf_whinfcomp/3,
	mf_prep_phrase/3,
	mf_misc/3,
	mf_ingcomp/3,
	mf_infcomp/3,
	mf_fincomp/3,
	mf_edcomp/3,
	mf_binfcomp/3,
	mf_ascomp/3
   ]).

:- use_module(library(strings), [
	substring/4
   ]).

/*==========================================================
%FUNCTION NAME
      mf_lexical_record  (GRAMMER)
%PURPOSE
      Checks to see if a term is a valid term	
%I/O PARAMETER LIST
      R
%EXAMPLE CALL
            	     
%METHOD
	
%NOTES
		
%FILES

%TABLES
	
%SYNTAX ARGS
==========================================================*/
mf_lexical_record(R) -->
    { R = lexrec:[base:[B], 
	spelling_variants:Ss, entries:Es, annotations:A, signature:S] },
    "{base=", { atom_codes(B, BL) }, BL, mf_newline,
    mf_spelling_variants(Ss),
    mf_lexical_entries(Es),
    mf_annotations(A),
    mf_signature(S),
    "}", mf_newline.


      /*** end mf_lexical_record */


/*==========================================================
%FUNCTION NAME
      mf_spelling_variants   (GRAMMER)
%PURPOSE
      Checks to see if R is a valid spelling variant 
%I/O PARAMETER LIST
      S|R
%EXAMPLE CALL
    mf_spelling_variants(Ss),
            	     
%METHOD
	
%NOTES
		
%FILES

%TABLES
	
%SYNTAX ARGS
==========================================================*/
mf_spelling_variants([S|R]) -->
    "spelling_variant=", { atom_codes(S, SL) }, SL, mf_newline,
    mf_spelling_variants(R).

mf_spelling_variants([]) --> [].

      /*** end mf_spelling_variants */

/*==========================================================
%FUNCTION NAME
      mf_lexical_entries   (GRAMMER)
%PURPOSE
      Checks to see if R is a valid lexical entry 
%I/O PARAMETER LIST
      E
%EXAMPLE CALL
    mf_spelling_variants(Ss),
            	     
%METHOD
	
%NOTES
		
%FILES

%TABLES
	
%SYNTAX ARGS
==========================================================*/
%%% at least one lexical entry
mf_lexical_entries([E]) -->
    mf_lexical_entry(E).
mf_lexical_entries([E|R]) -->
    mf_lexical_entry(E),
    mf_lexical_entries(R).

mf_lexical_entry(E) -->
    (	mf_adj_entry(E)
    ;	mf_adv_entry(E)
    ;	mf_aux_entry(E)
    ;	mf_compl_entry(E)
    ;	mf_conj_entry(E)
    ;	mf_det_entry(E)
    ;	mf_modal_entry(E)
    ;	mf_noun_entry(E)
    ;	mf_prep_entry(E)
    ;	mf_pron_entry(E)
    ;	mf_verb_entry(E)
    ).

mf_annotations([A|R]) -->
    "annotation=", { atom_codes(A, AL) }, AL, mf_newline,
    mf_annotations(R).
mf_annotations([]) --> [].

mf_signature([S]) -->
    "signature=", { atom_codes(S, SL) }, SL, mf_newline.
mf_signature([]) --> [].

%%% adjectives

mf_adj_entry(E) -->
    { E = entry:[num:[N], cat:[adj], variants:Vs, positions:Ps,
	complements:Cs, stative:S, nominalizations:Ns, misc:M] },
    "entry=", { enumber_codes(N, NL) }, NL, mf_newline,
    mf_tab, "cat=adj", mf_newline,
    mf_adj_variants(Vs),
    mf_adj_positions(Ps),
    mf_adj_complements(Cs),
    mf_adj_stative(S),
    mf_adj_nominalizations(Ns),
    mf_misc(M).

%%% generates one or more variants.
mf_adj_variants([V]) -->
    mf_tab, "variants=", mf_adj_variant(V),	mf_newline.
mf_adj_variants([V|R]) -->
    mf_tab, "variants=", mf_adj_variant(V),	mf_newline,
    mf_adj_variants(R).

mf_adj_variant(reg) --> "reg".
mf_adj_variant(regd) --> "regd".
mf_adj_variant(irreg:[Pos, Comp, Super]) -->
	{ atom_chars(Pos, PosL),
	  atom_chars(Comp, CompL),
	  atom_chars(Super, SuperL) },
	"irreg|", PosL, "|", CompL, "|", SuperL, "|".
mf_adj_variant(inv:[periph]) --> "inv;periph".
mf_adj_variant(inv) --> "inv".

%%% generates zero or more adjectival positions.
mf_adj_positions([]) --> [].
mf_adj_positions([P|R]) -->
    mf_tab, "position=", mf_adj_position(P), mf_newline,
    mf_adj_positions(R).

mf_adj_position(attrib:[1]) --> "attrib(1)".
mf_adj_position(attrib:[2]) --> "attrib(2)".
mf_adj_position(attrib:[3]) --> "attrib(3)".
mf_adj_position(pred) --> "pred".
mf_adj_position(attribc) --> "attribc".
mf_adj_position(post) --> "post".

%%% generates zero or more adjectival complements.
mf_adj_complements([]) --> [].
mf_adj_complements([C|R]) -->
    mf_tab, "compl=", mf_adj_complement(C), mf_newline,
    mf_adj_complements(R).

mf_adj_complement(V) --> mf_infcomp(V).
mf_adj_complement(V) --> mf_ascomp(V).
mf_adj_complement(V) --> mf_fincomp(V).
mf_adj_complement(V) --> mf_whinfcomp(V).
mf_adj_complement(whfincomp) --> "whfincomp".
mf_adj_complement(advbl) --> "advbl".
mf_adj_complement(P) --> mf_prep_phrase(P).

%%% optional
mf_adj_stative([yes]) --> mf_tab, "stative", mf_newline.
mf_adj_stative([]) --> [].

%%% generates zero or more nominalizations.
mf_adj_nominalizations([]) --> [].
mf_adj_nominalizations([N|R]) -->
    mf_tab, "nominalization=", mf_adj_nominalization(N), mf_newline,
    mf_adj_nominalizations(R).

mf_adj_nominalization(N) --> { atom_codes(N,L) }, L.

%%% adverbs

mf_adv_entry(E) -->
    { E = entry:[num:[N], cat:[adv], variants:Vs, interrogative:I,
	modification_types:Ms, negative:Neg, misc:M] },
    "entry=", { enumber_codes(N, NL) }, NL, mf_newline,
    mf_tab, "cat=adv", mf_newline,
    mf_adv_variants(Vs),
    mf_adv_interrogative(I),
    mf_adv_modification_types(Ms),
    mf_adv_negative(Neg),
    mf_misc(M).

%%% generates one or more variants.
mf_adv_variants([V]) -->
    mf_tab, "variants=", mf_adv_variant(V), mf_newline.
mf_adv_variants([V|R]) -->
    mf_tab, "variants=", mf_adv_variant(V), mf_newline,
    mf_adv_variants(R).

mf_adv_variant(reg) --> "reg".
mf_adv_variant(regd) --> "regd".
mf_adv_variant(irreg:[Pos,Comp,Super]) -->
    { atom_chars(Pos, PosL),
      atom_chars(Comp,CompL),
      atom_chars(Super,SuperL) },
    "irreg|", PosL, "|", CompL, "|", SuperL, "|".
mf_adv_variant(inv:[periph]) --> "inv;periph".
mf_adv_variant(inv) --> "inv".

%%% optional
mf_adv_interrogative([yes]) --> mf_tab, "interrogative", mf_newline.
mf_adv_interrogative([]) --> [].

%%% one or more
mf_adv_modification_types([M]) -->
    mf_tab, "modification_type=", mf_adv_modification_type(M), mf_newline.
mf_adv_modification_types([M|R]) -->
    mf_tab, "modification_type=", mf_adv_modification_type(M), mf_newline,
    mf_adv_modification_types(R).

mf_adv_modification_type(particle) --> "particle".
mf_adv_modification_type(intensifier) --> "intensifier".
mf_adv_modification_type(sentence_modifier:[locative]) --> "sentence_modifier;locative".
mf_adv_modification_type(sentence_modifier:[temporal]) --> "sentence_modifier;temporal".
mf_adv_modification_type(sentence_modifier:[manner]) --> "sentence_modifier;manner".
mf_adv_modification_type(verb_modifier:[locative]) --> "verb_modifier;locative".
mf_adv_modification_type(verb_modifier:[temporal]) --> "verb_modifier;temporal".
mf_adv_modification_type(verb_modifier:[manner]) --> "verb_modifier;manner".

%%% optional
mf_adv_negative([broad_negative]) --> mf_tab, "broad_negative", mf_newline.
mf_adv_negative([negative]) --> mf_tab, "negative", mf_newline.
mf_adv_negative([]) --> [].

%%% auxiliaries

mf_aux_entry(E) -->
    %{format(user_output,"mf_aux_entry~n",[]),
     {E = entry:[num:[N], cat:[aux], variants:V, misc:M] },
    "entry=", 
	{ enumber_codes(N, NL) }, 
	NL, 
	mf_newline,
	mf_tab, 
	"cat=aux", 
	mf_newline,
	%{format(user_output,"1 am willing to bet that I get to here~n",[])},
	mf_aux_variant(V),
	%{format(user_output,"2 am willing to bet that I get to herewith ~q~n",[M])},
	mf_misc(M)
	%{format(user_output,"3 am willing to bet that I dont get to here~n",[])}
    .

%%% single variant
mf_aux_variant(V) -->
    %{format(user_output,"4 mf_aux_variant~n",[])},
    mf_tab, "variants=",  mf_aux_variant_val(V), mf_newline.

mf_aux_variant(_) -->[].

mf_aux_variant_val(reg) --> 
	%{format(user_output,"5 mf_aux_variant_val~n",[])},
	"reg".


mf_aux_variant_val(regd) --> "regd".
mf_aux_variant_val(irreg:[V1,V2,V3,V4,V5]) -->
    { atom_chars(V1,L1),
      atom_chars(V2,L2),
      atom_chars(V3,L3),
      atom_chars(V4,L4),
      atom_chars(V5,L5) },
    "irreg|", L1, "|", L2, "|", L3, "|", L4, "|", L5, "|".

%%% complementizers

mf_compl_entry(E) -->
    { E = entry:[num:[N], cat:[compl], misc:M] },
    "entry=", { enumber_codes(N, NL) }, NL, mf_newline,
    mf_tab, "cat=compl", mf_newline,
    mf_misc(M).


%%% conjunctions

mf_conj_entry(E) -->
    { E = entry:[num:[N], cat:[conj], misc:M] },
    "entry=", { enumber_codes(N, NL) }, NL, mf_newline,
    mf_tab, "cat=conj", mf_newline,
    mf_misc(M).

%%% determiners

mf_det_entry(E) -->
    { E = entry:[num:[N], cat:[det], variants:V, interrogative:I, demonstrative:D, misc:M]},
    "entry=", { enumber_codes(N, NL) }, NL, mf_newline,
    mf_tab, "cat=det", mf_newline,
    mf_det_variant(V),
    mf_det_interrogative(I),
    mf_det_demonstrative(D),
    mf_misc(M).

%%% single value
mf_det_variant(V) -->
    mf_tab, "variants=", mf_det_variant_val(V), mf_newline.

mf_det_variant_val([free]) --> "free".
mf_det_variant_val([singuncount]) --> "singuncount".
mf_det_variant_val([pluruncount]) --> "pluruncount".
mf_det_variant_val([sing]) --> "sing".
mf_det_variant_val([plur]) --> "plur".
mf_det_variant_val([uncount]) --> "uncount".

mf_det_interrogative([yes]) -->
    mf_tab, "interrogative", mf_newline.
mf_det_interrogative([]) --> [].

mf_det_demonstrative([yes]) -->
    mf_tab, "demonstrative", mf_newline.
mf_det_demonstrative([]) --> [].

%%% modals

mf_modal_entry(E) -->
    %{ format(user_output," in mf_modal_entry with ~q ~n",[E])},
    { E = entry:[num:[N], cat:[modal], variants:V, misc:M] },
    "entry=", { enumber_codes(N, NL) }, NL, mf_newline,
    mf_tab, "cat=modal", mf_newline,
    mf_modal_variant(V),
    mf_misc(M).

%%% optional single variant
mf_modal_variant([[V]]) -->
	%{ format(user_output," in mf_modal_variant with ~q ~n",[V])},
	mf_tab, 
	"variants=", 
	{ atom_codes(V,L) }, 
	L, 
	mf_newline.

mf_modal_variant([[]]) --> [].

%%% nouns

mf_noun_entry(E) -->
    { E = entry:[num:[N], cat:[noun], variants:Vs, complements:Cs, nominalizations_of:Ns, proper:P, misc:M] },
    "entry=", { enumber_codes(N, NL) }, NL, mf_newline,
    mf_tab, "cat=noun", mf_newline,
    mf_noun_variants(Vs),
    mf_noun_complements(Cs),
    mf_noun_nominalizations(Ns),
    mf_noun_proper(P),
    mf_misc(M),!.

%%% one or more variants
mf_noun_variants([V]) -->
    mf_tab, "variants=", mf_noun_variant(V), mf_newline.
mf_noun_variants([V|R]) -->
    mf_tab, "variants=", mf_noun_variant(V), mf_newline,
    mf_noun_variants(R).

mf_noun_variant(reg) --> "reg".
mf_noun_variant(glreg) --> "glreg".
mf_noun_variant(irreg:[Sing,Plu]) -->
    { atom_chars(Sing, SingL),
      atom_chars(Plu, PluL) },
      "irreg|", SingL, "|", PluL, "|".
mf_noun_variant(inv) --> "inv".
mf_noun_variant(uncount) --> "uncount".
mf_noun_variant(group:[reg]) --> "group(reg)".
mf_noun_variant(group:[glreg]) --> "group(glreg)".
mf_noun_variant(group:[irreg:[Sing,Plu]]) -->
    { atom_chars(Sing, SingL),
      atom_chars(Plu,PluL) },
      "group(irreg|", SingL, "|", PluL, "|)".
mf_noun_variant(group:[sing]) --> "group(sing)".
mf_noun_variant(group:[metareg]) --> "group(metareg)".
mf_noun_variant(groupuncount) --> "groupuncount".
mf_noun_variant(sing) --> "sing".
mf_noun_variant(plur) --> "plur".
mf_noun_variant(metareg) --> "metareg".

%%% zero or more
mf_noun_complements([]) --> [].
mf_noun_complements([C|R]) -->
    mf_tab, "compl=", mf_noun_complement(C), mf_newline,
    mf_noun_complements(R).

mf_noun_complement(V) --> mf_infcomp(V).
mf_noun_complement(V) --> mf_ascomp(V).
mf_noun_complement(V) --> mf_fincomp(V).
mf_noun_complement(V) --> mf_whinfcomp(V).
mf_noun_complement(whfincomp) --> "whfincomp".
mf_noun_complement(P) --> mf_prep_phrase(P).

%%% zero or more
mf_noun_nominalizations([]) --> [].
mf_noun_nominalizations([N|R]) -->
    mf_tab, "nominalization_of=", { atom_codes(N, L) }, L, mf_newline,
    mf_noun_nominalizations(R).

%%% optional
mf_noun_proper([yes]) -->
    mf_tab, "proper", mf_newline.
mf_noun_proper([]) --> [].

%%% prepositions

mf_prep_entry(E) -->
    { E = entry:[num:[N], cat:[prep], misc:M] },
    "entry=", { enumber_codes(N, NL) }, NL, mf_newline,
    mf_tab, "cat=prep", mf_newline,
    mf_misc(M).

%%% pronouns

mf_pron_entry(E) -->
    { E = entry:[num:[N], cat:[pron], variants:V, gender:G, interrogative:I, type:T, misc:M] },
    "entry=", { enumber_codes(N, NL) }, NL, mf_newline,
    mf_tab, "cat=pron", mf_newline,
    mf_pron_variants(V),
    mf_pron_gender(G),
    mf_pron_interrogative(I),
    mf_pron_types(T),
    mf_misc(M).

%%% one or more
mf_pron_variants([V]) -->
    mf_tab, "variants=", mf_pron_variant(V), mf_newline.
mf_pron_variants([V|R]) -->
    mf_tab, "variants=", mf_pron_variant(V), mf_newline,
    mf_pron_variants(R).

mf_pron_variant(fst_sing) --> "fst_sing".
mf_pron_variant(fst_plur) --> "fst_plur".
mf_pron_variant(sec_sing) --> "sec_sing".
mf_pron_variant(sec_plur) --> "sec_plur".
mf_pron_variant(second) --> "second".
mf_pron_variant(thr_sing) --> "thr_sing".
mf_pron_variant(thr_plur) --> "thr_plur".
mf_pron_variant(third) --> "third".
mf_pron_variant(free) --> "free".

%%% single optional value
mf_pron_gender([]) --> [].
mf_pron_gender([G]) -->
	mf_tab, "gender=", mf_pron_gender_val(G), mf_newline.

mf_pron_gender_val(pers:[fem]) --> "pers(fem)".
mf_pron_gender_val(pers:[masc]) --> "pers(masc)".
mf_pron_gender_val(pers) --> "pers".
mf_pron_gender_val(neut) --> "neut".

%%% single optional
mf_pron_interrogative([yes]) -->
    mf_tab, "interrogative", mf_newline.
mf_pron_interrogative([]) --> [].

%%% zero or more
mf_pron_types([]) --> [].
mf_pron_types([T|R]) -->
    mf_tab, "type=", mf_pron_type(T), mf_newline,
    mf_pron_types(R).

mf_pron_type(subj) --> "subj".
mf_pron_type(obj) --> "obj".
mf_pron_type(refl) --> "refl".
mf_pron_type(poss) --> "poss".
mf_pron_type(possnom) --> "possnom".
mf_pron_type(dem) --> "dem".
mf_pron_type(univ) --> "univ".
mf_pron_type(indef:[assert]) --> "indef(assert)".
mf_pron_type(indef:[nonassert]) --> "indef(nonassert)".
mf_pron_type(indef:[neg]) --> "indef(neg)".

%%% verbs

mf_verb_entry(E) -->
    { E = entry:[num:[N], cat:[verb], variants:Vs, complements:Cs, nominalizations:Ns, misc:M] },
    "entry=", { enumber_codes(N, NL) }, NL, mf_newline,
    mf_tab, "cat=verb", mf_newline,
    mf_verb_variants(Vs),
    mf_verb_complements(Cs),
    mf_verb_nominalizations(Ns),
    mf_misc(M).

%%% generates one or more variants.
mf_verb_variants([V]) -->
    mf_tab, "variants=", mf_verb_variant(V), mf_newline.
mf_verb_variants([V|R]) -->
    mf_tab, "variants=", mf_verb_variant(V), mf_newline,
    mf_verb_variants(R).

mf_verb_variant(reg) --> "reg".
mf_verb_variant(regd) --> "regd".
mf_verb_variant(irreg:[V1, V2, V3, V4, V5]) -->
    { (V1 = [], L1 = ""; atom_chars(V1, L1)), 
      (V2 = [], L2 = ""; atom_chars(V2, L2)), 
      (V3 = [], L3 = ""; atom_chars(V3, L3)), 
      (V4 = [], L4 = ""; atom_chars(V4, L4)),
      (V5 = [], L5 = ""; atom_chars(V5, L5)) },
    "irreg|", L1, "|", L2, "|", L3, "|", L4, "|", L5, "|".

%%% at least one complement
mf_verb_complements([C]) --> mf_verb_complement_value(C).
mf_verb_complements([C|R]) -->
    mf_verb_complement_value(C),
    mf_verb_complements(R).

mf_verb_complement_value(C) -->
    (	mf_verb_intran(C)
    ;	mf_verb_tran(C)
    ;	mf_verb_link(C)
    ;	mf_verb_ditran(C)
    ;	mf_verb_cplxtran(C)
    ).

%%% intransitives
mf_verb_intran(intran:[compl:[], part:P, mvmt:[]]) -->
    mf_tab, "intran", mf_verb_particle(P), mf_newline.

%%% transitives
mf_verb_tran(tran:[compl:[C], part:P, mvmt:M]) -->
    mf_tab, "tran=", mf_verb_tran_value(C), mf_verb_particle(P), mf_verb_tran_mvmt(M), mf_newline.
	
mf_verb_tran_value(np) --> "np".
mf_verb_tran_value(np:[V]) -->
    "np|", { atom_codes(V,L) }, L, "|".
mf_verb_tran_value(V) --> mf_prep_phrase(V).
mf_verb_tran_value(V) --> mf_binfcomp(V).
mf_verb_tran_value(V) --> mf_infcomp(V).
mf_verb_tran_value(V) --> mf_ingcomp(V).
mf_verb_tran_value(V) --> mf_ascomp(V).
mf_verb_tran_value(V) --> mf_fincomp(V).
mf_verb_tran_value(V) --> mf_whinfcomp(V).
mf_verb_tran_value(whfincomp) --> "whfincomp".

mf_verb_tran_mvmt([nopass]) --> ";nopass".
mf_verb_tran_mvmt([noprtmvt]) --> ";noprtmvt".
mf_verb_tran_mvmt([]) --> [].

%%% ditransitives
mf_verb_ditran(ditran:[compl:[F, S], part:P, mvmt:M]) -->
    mf_tab, "ditran=",
	mf_verb_first_ditran_value(F), ",", mf_verb_second_ditran_value(S),
	mf_verb_particle(P), mf_verb_ditran_mvmt(M), mf_newline.

%%% first ditran values.
mf_verb_first_ditran_value(np) --> "np".
mf_verb_first_ditran_value(np:[V]) -->
    "np|", { atom_codes(V, L) }, L, "|".
mf_verb_first_ditran_value(V) --> mf_prep_phrase(V).
mf_verb_first_ditran_value(V) --> mf_fincomp(V).
mf_verb_first_ditran_value(V) --> mf_ingcomp(V).

%%% second ditran values.
mf_verb_second_ditran_value(np) --> "np".
mf_verb_second_ditran_value(np:[V]) -->
    "np|", { atom_codes(V, L) }, L, "|".
mf_verb_second_ditran_value(V) --> mf_prep_phrase(V).
mf_verb_second_ditran_value(V) --> mf_fincomp(V).
mf_verb_second_ditran_value(V) --> mf_whinfcomp(V).
mf_verb_second_ditran_value(whfincomp) --> "whfincomp".

%%% verb ditran movement.
mf_verb_ditran_mvmt([datmvt]) --> ";datmvt".
mf_verb_ditran_mvmt([nopass]) --> ";nopass".
mf_verb_ditran_mvmt([noprtmvt]) --> ";noprtmvt".
mf_verb_ditran_mvmt([]) --> [].

%%% Linking verbs
mf_verb_link(link:[compl:[C], part:P, mvmt:M]) -->
    mf_tab, "link=", mf_verb_link_value(C),
	mf_verb_particle(P), mf_verb_link_mvmt(M), mf_newline.
	
mf_verb_link_value(np) --> "np".
mf_verb_link_value(np(V)) -->
    "np|", { atom_codes(V, L) }, L, "|".
mf_verb_link_value(V) --> mf_prep_phrase(V).
mf_verb_link_value(adj) --> "adj".
mf_verb_link_value(advbl) --> "advbl".
mf_verb_link_value(V) --> mf_edcomp(V).
mf_verb_link_value(V) --> mf_infcomp(V).
mf_verb_link_value(V) --> mf_ingcomp(V).
mf_verb_link_value(V) --> mf_fincomp(V).
mf_verb_link_value(V) --> mf_whinfcomp(V).
mf_verb_link_value(whfincomp) --> "whfincomp".

%%% link movement codes.
mf_verb_link_mvmt([noprtmvt]) --> ";noprtmvt".
mf_verb_link_mvmt([]) --> [].

%%% complex transitive verbs
mf_verb_cplxtran(cplxtran:[compl:[F, S], part:P, mvmt:M]) -->
    mf_tab, "cplxtran=", mf_verb_first_cplxtran_value(F), ",", mf_verb_second_cplxtran_value(S),
	mf_verb_particle(P), mf_verb_cplxtran_mvmt(M), mf_newline.

%%% first cplxtran values.
mf_verb_first_cplxtran_value(np) --> "np".
mf_verb_first_cplxtran_value(np:[V]) -->
    "np|", { atom_codes(V, L) }, L, "|".
mf_verb_first_cplxtran_value(V) --> mf_prep_phrase(V).
mf_verb_first_cplxtran_value(V) --> mf_fincomp(V).
mf_verb_first_cplxtran_value(V) --> mf_ingcomp(V).

%%% second cplxtran values.
mf_verb_second_cplxtran_value(np) --> "np".
mf_verb_second_cplxtran_value(np:[V]) -->
    "np|", { atom_codes(V, L) }, L, "|".
mf_verb_second_cplxtran_value(adj) --> "adj".
mf_verb_second_cplxtran_value(advbl) --> "advbl".
mf_verb_second_cplxtran_value(V) --> mf_binfcomp(V).
mf_verb_second_cplxtran_value(V) --> mf_edcomp(V).
mf_verb_second_cplxtran_value(V) --> mf_infcomp(V).
mf_verb_second_cplxtran_value(V) --> mf_ingcomp(V).
mf_verb_second_cplxtran_value(V) --> mf_ascomp(V).
mf_verb_second_cplxtran_value(V) --> mf_prep_phrase(V).

%%% verb cplxtran movement.
mf_verb_cplxtran_mvmt([nopass]) --> ";nopass".
mf_verb_cplxtran_mvmt([noprtmvt]) --> ";noprtmvt".
mf_verb_cplxtran_mvmt([]) --> [].

%%% particles
mf_verb_particle([P]) -->
    ";part(", { atom_codes(P, L) }, L, ")".
mf_verb_particle([]) --> [].

%%% generates multiple nominalizations.
mf_verb_nominalizations([]) --> [].
mf_verb_nominalizations([N|R]) -->
    mf_tab, "nominalization=", { atom_codes(N, L) }, L, mf_newline,
    mf_verb_nominalizations(R).




/*==========================================================
%FUNCTION NAME
        enumber_codes
%PURPOSE
        Given an atom of the form "E9999999",or an empty list, return 
        the char version

          This routine should insure that there is an E as the
          first character, and that there are 7 digits to follow,
          otherwise this routine should spew out an error.

%I/O PARAMETER LIST
	+E_ATOM   ATOM
        -E_STRING CHAR LIST (If the routine is true)

%EXAMPLE CALL
	enumber_codes('E0065342',E_STRING).
%METHOD
                    Verify the E part

                       Now verify that the remaining seven positions 
                       only contain digits

%NOTES

   This routine has been tested on the following cases

  %  enumber_codes('E0068931', E_STRING),   % Test for valid string
  %  enumber_codes('E001234',  E_STRING1),  % Test for less than 7 digits
  %  enumber_codes('E00A1234', E_STRING2),  % Test for alpha chars in digit 
                                            % space
  %  enumber_codes('0067395',  E_STRING3),  % Test for No E at the beginning
  %  enumber_codes('ABC'    ,  E_STRING4),  % Test for junk
  %  enumber_codes([]       ,  E_STRING5),  % Test for []
  %  enumber_codes('00E7395',  E_STRING3).  % Test for E Not at the beginning

%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
enumber_codes([],[]) :- !.  % if no E number, rewrite with a no op.

enumber_codes(E_ATOM, E_STRING) :-
(
     % debug_print(FUNCTION_BEGIN,DEBUG_LEVEL_FLAG,enumber_codes),

		    atom_codes(E_ATOM,E_STRING),!,
                    
                    substring(E_ATOM,'E',0,_),!,
                       ( 
                    /* ---------------------------------------------
                       Now verify that the remaining seven positions 
                       only contain digits
                       --------------------------------------------- */
                         substring(E_ATOM,DIGITS,1,7) ->
                           (
                             atom_codes(DIGITS,CDIGITS),
                             number_codes(_NUMBER,CDIGITS) ->
                               (
				   true  % used to be a debug statement here
			       )  
                               ;
                               (
			         format(user_error,
			          "~q Contains non numeric characters in it~n",
			          [DIGITS]),
				 fail,!
                               )
                           )
                           ;
                           (
                             format(user_error,
			    "~q Contains less than 7 digit characters in it~n",
			           [DIGITS]),
			       fail,!
                           )
                       
  
                       )
                       ;
                       (
                         format(user_error,
		       "        ~q does not contain an E at the beginning~n",
		                E_ATOM),   
                         fail,!)

     % debug_print(FUNCTION_END,DEBUG_LEVEL_FLAG,enumber_codes),


). /*** End enumber_codes */

