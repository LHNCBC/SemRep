
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
	qp_fm_misc.pl

%DESCRIPTION OF FILE
	Quintus Prolog (.pl) source file.
        qp_fm_misc.pl - miscellaneous grammar predicates

%REVISED
	12Apr94 divita -- Initial Version

%%
==========================================================*/


/*----------------------------
%MODULE NAME
----------------------------*/
:- module(qp_fm_misc,[
	fm_infcomp/3,
	fm_ascomp/3,
	fm_ingcomp/3,
	fm_edcomp/3,
	fm_binfcomp/3,
	fm_whinfcomp/3,
	fm_fincomp/3,
	fm_prep_phrase/3,
	fm_misc/3
]).


/*----------------------------
%INCLUDES
----------------------------*/
:- use_module(lexicon(qp_fm_utils), [
	fm_to_pipe/3,
	fm_to_newline/3,
	fm_to_comma/3,
	fm_to_char/4,
	fm_spaces/2,
	fm_newline/2
   ]).
/*----------------------------
%STATIC FUNCTIONS
----------------------------*/

/*----------------------------
%STATIC GLOBAL VARIABLES
----------------------------*/

/*----------------------------
%DEBUG FLAGS
----------------------------*/




/*==========================================================
%FUNCTION NAME
	complement_codes
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	complement_codes(arg) :-
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/

fm_infcomp(infcomp:[C])     --> "infcomp:",   fm_interp_code(C).
fm_ascomp(ascomp:[C])       --> "ascomp:",    fm_interp_code(C).
fm_ingcomp(ingcomp:[C])     --> "ingcomp:",   fm_interp_code(C).
fm_edcomp(edcomp:[C])       --> "edcomp:",    fm_interp_code(C).
fm_binfcomp(binfcomp:[C])   --> "binfcomp:",  fm_interp_code(C).
fm_whinfcomp(whinfcomp:[C]) --> "whinfcomp:", fm_interp_code(C).

      /*** end complement_codes */


/*==========================================================
%FUNCTION NAME
	fm_interp_code
%PURPOSE
	Interpretation codes
%I/O PARAMETER LIST
	objc|objr|subjc|subjr|arbc|nsc|nsr
%EXAMPLE CALL
	fm_interp_code(objc).
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
fm_interp_code(objc)   --> "objc".
fm_interp_code(objr)   --> "objr".
fm_interp_code(subjc)  --> "subjc".
fm_interp_code(subjr)  --> "subjr".
fm_interp_code(arbc)   --> "arbc".
fm_interp_code(nsc)    --> "nsc".
fm_interp_code(nsr)    --> "nsr".

      /*** end fm_interp_code */



/*==========================================================
%FUNCTION NAME
	fm_fincomp
%PURPOSE
	Finite complements
%I/O PARAMETER LIST
	 F 
%EXAMPLE CALL
	fm_fincomp(F).
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
fm_fincomp(F) -->
    "fincomp(", fm_fincomp_val(X), ")", fm_fincomp_subj(S),
    { F = fincomp:[X|S] }.


 /*** end fm_fincomp */


/*==========================================================
%FUNCTION NAME
	fm_fincomp_val
%PURPOSE
	Finite complement values
%I/O PARAMETER LIST
	o|t|p|s|ts|sp|tsp
%EXAMPLE CALL
	fm_fincomp_val(o).
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/

fm_fincomp_val(o) --> "o".
fm_fincomp_val(t) --> "t".
fm_fincomp_val(p) --> "p".
fm_fincomp_val(s) --> "s".
fm_fincomp_val(ts) --> "ts".
fm_fincomp_val(tp) --> "tp".
fm_fincomp_val(sp) --> "sp".
fm_fincomp_val(tsp) --> "tsp".


      /*** end fm_fincomp_val */


/*==========================================================
%FUNCTION NAME
	fm_fincomp_subj
%PURPOSE
	Finite complement subject codes
%I/O PARAMETER LIST
	subj|[]
%EXAMPLE CALL
	fm_fincomp_subj(_).
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
fm_fincomp_subj([subj]) --> ":subj".
fm_fincomp_subj([]) --> [].

 /*** end fm_fincomp_subj */


/*==========================================================
%FUNCTION NAME
	fm_prep_phrase
%PURPOSE
	Prepositional phrase
%I/O PARAMETER LIST
	P
%EXAMPLE CALL
	fm_prep_phrase(P).
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
fm_prep_phrase(P) -->
	               (	fm_prep_phrase2(P)
    	               ;	fm_prep_phrase3(P)
    	               ).

      /*** end fm_prep_phrase */


/*==========================================================
%FUNCTION NAME
	fm_prep_phrase2
%PURPOSE
	Prep phrase, single object
%I/O PARAMETER LIST
	pphr - preposition
        obj  - object
%EXAMPLE CALL
	fm_prep_phrase2(pphr,obj).
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
fm_prep_phrase2(pphr:[prep:[P], obj:[O]]) -->
                                               "pphr(", 
                                       	       fm_to_comma(PL), 
                                               { atom_codes(P, PL) }, 
                                               ",", 
                                               fm_prep_object(O), 
                                               ")".

      /*** end fm_prep_phrase2 */


/*==========================================================
%FUNCTION NAME
	fm_prep_phrase3
%PURPOSE
	Prep phrase, double object
%I/O PARAMETER LIST
	pphr - preposition
        obj  - object
	P2   - another prep phrase of the single object form
%EXAMPLE CALL
	fm_prep_phrase3(pphr,obj,P2).

%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
fm_prep_phrase3(pphr:[prep:[P], obj:[O], P2]) -->
    "pphr(", fm_to_comma(PL), { atom_codes(P, PL) }, ",",
	fm_prep_object(O), ",",	fm_prep_phrase2(P2), ")".

      /*** end fm_prep_phrase3 */


/*==========================================================
%FUNCTION NAME
	fm_prep_object
%PURPOSE
	Prepositional Phrase Objects
%I/O PARAMETER LIST
	np|np(_)|adj|advb1|I|G|W|B|E|whfincomp
%EXAMPLE CALL
	fm_prep_object(np).
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/

fm_prep_object(np)       --> "np".
fm_prep_object(np:[T])   --> "np|", fm_to_pipe(L), { atom_codes(T, L) }, "|".
fm_prep_object(adj)      --> "adj".
fm_prep_object(advbl)    --> "advbl".
fm_prep_object(I)        --> fm_infcomp(I).
fm_prep_object(G)        --> fm_ingcomp(G).
fm_prep_object(W)        --> fm_whinfcomp(W).
fm_prep_object(whfincomp)--> "whfincomp".
fm_prep_object(B)        --> fm_binfcomp(B).
fm_prep_object(E)        --> fm_edcomp(E).


      /*** end fm_prep_object */


/*==========================================================
%FUNCTION NAME
	fm_misc
%PURPOSE
	miscellaneous slots, i.e., acronyms, abbreviations and keys
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	fm_misc(arg).
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
fm_misc([F|R]) -->  fm_misc_aux(F),
                    fm_misc(R).
		    

fm_misc([]) --> 
	[].

      /*** end fm_misc */


/*==========================================================
%FUNCTION NAME
	fm_misc_aux
%PURPOSE
	miscellaneous slots
%I/O PARAMETER LIST
	
%EXAMPLE CALL
	fm_misc_aux(arg).
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
fm_misc_aux(acronym_of:As)       --> fm_acronyms(As).
fm_misc_aux(abbreviation_of:As)  --> fm_abbreviations(As).
fm_misc_aux(keys:Ks)             --> fm_keys(Ks).
fm_misc_aux(variant:Vs)          --> fm_variant(Vs).
fm_misc_aux(trademark)           --> 
                                     fm_spaces, 
                                    "trademark",
                                     fm_to_newline(_L), 
                                     fm_newline.

	                             

      /*** end fm_misc_aux */


/*==========================================================
%FUNCTION NAME
	fm_acronyms
%PURPOSE
	Acronyms
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	fm_acronyms(arg) :-
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
fm_acronyms([A]) -->
                       fm_spaces, 
                       "acronym_of=", !, 
                       fm_to_newline(L), 
                       { atom_codes(A, L) }, 
                       fm_newline.

     /*** end fm_acronyms */


/*==========================================================
%FUNCTION NAME
	fm_abbreviations
%PURPOSE
	Abbreviations
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	fm_abbreviations(arg).
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
fm_abbreviations([A]) -->
                            fm_spaces, 
                            "abbreviation_of=", !, 
                            fm_to_newline(L), 
                            { atom_codes(A, L) }, 
                            fm_newline.


      /*** end fm_abbreviations */


/*==========================================================
%FUNCTION NAME
	fm_keys
%PURPOSE
	Keys
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	fm_keys(arg).
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
fm_keys([K]) -->
                   fm_spaces, 
                   "key=", !, 
                   fm_to_newline(L), 
                   { atom_codes(K, L) }, 
                   fm_newline.

        /*** end fm_keys */


/*==========================================================
%FUNCTION NAME
	fm_variant
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	fm_variant(arg) :-
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
        % For the cases of the form variant;tense(agreement):[negative]

fm_variant(R)  --> 
	       %{format(user_error,"In fm_variant variant:tense;agrmnt~n",[])},
		fm_spaces,
		"variant=",
		fm_to_char(0';,BL),
		{atom_codes(B,BL) },
		";",
		/* fm_spaces, */
		fm_tense(T),
		fm_modal_variant_agreement(Ag),
		fm_modal_negation(N),
		fm_to_newline(L), 
		{atom_codes(_, L) }, 
		fm_newline,
		{R = [variant-string:B, tense:T,agreement:Ag, negative:N]}.


        % For the cases of the form variant;tense:[negative]
fm_variant(R)  --> 
	       %{format(user_error,"In fm_variant variant:tense~n",[])},
		fm_spaces,
		"variant=", !,
		fm_to_char(0';,BL),
		{atom_codes(B,BL) },
		";",
		/* fm_spaces, */
		fm_tense(T),!,
		fm_modal_negation(N),
		fm_to_newline(L), 
		{!, atom_codes(_, L) }, 
		fm_newline,!,
		{R = [variant-string:B, tense:T,agreement:[], negative:N]}.


      /*** end fm_variant */

/*==========================================================
%FUNCTION NAME
	fm_tense
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	fm_tense(arg) :-
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
fm_tense(infinitive)--> "infinitive".
fm_tense( past_part)--> " past_part".
fm_tense(past_part)--> "past_part".
fm_tense( pres_part)--> " pres_part".
fm_tense(pres_part)--> "pres_part".
fm_tense( past)--> " past".
fm_tense(past)--> "past".
fm_tense( pres)--> " pres".
fm_tense(pres)--> "pres".


      /*** end fm_tense */


/*==========================================================
%FUNCTION NAME
	fm_modal_variant_agreement
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	fm_modal_variant_agreement(arg) :-
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
fm_modal_variant_agreement(V) -->  
	         %{format(user_error,"In fm_modal_variant_agreement~n",[])},
		  fm_spaces, 
		  fm_to_char(0'(, A),
		  {atom_codes(_,A)},
		  "(",
		  fm_modal_variant_agreement_list(V),
		  fm_to_char(0'), B),
		  {atom_codes(_,B)},
		  ")".


      /*** end fm_modal_variant_agreement_list */

/*==========================================================
%FUNCTION NAME
	fm_modal_variant_agreement_list
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	fm_modal_variant_agreement_list(arg) :-
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
fm_modal_variant_agreement_list([F|R]) -->
	       %{format(user_error,"In fm_modal_variant_agreement_list~n",[])},
		fm_modal_variant_agreement_list_aux(F),
		fm_to_char(0',, _),
		",",
		fm_modal_variant_agreement_list(R).

fm_modal_variant_agreement_list([V]) -->fm_modal_variant_agreement_list_aux(V).


      /*** end fm_modal_variant_agreement_list */
/*==========================================================
%FUNCTION NAME
	fm_modal_variant_agreement_list_aux
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	fm_modal_variant_agreement_list_aux(arg) :-
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
fm_modal_variant_agreement_list_aux(fst_sing) -->"fst_sing".
fm_modal_variant_agreement_list_aux(fst_plur) -->"fst_plur".
fm_modal_variant_agreement_list_aux(second)   -->"second".
fm_modal_variant_agreement_list_aux(sec_sing) -->"sec_sing".
fm_modal_variant_agreement_list_aux(sec_plur) -->"sec_plur".
fm_modal_variant_agreement_list_aux(third)    -->"third".
fm_modal_variant_agreement_list_aux(thr_sing) -->"thr_sing".
fm_modal_variant_agreement_list_aux(thr_plur) -->"thr_plur".


/*==========================================================
%FUNCTION NAME
	fm_modal_negation
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	fm_modal_variant_negation(arg):-
%METHOD
	?
%NOTES
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/

fm_modal_negation(V) --> 
	                  %{format(user_error,"negative 1~n",[])},
		           fm_to_char(0':, _),
			    ":",!,
			   "negative",
			   {V = yes }, !.

fm_modal_negation(V) --> 
             	          %{format(user_error,"negative 2~n",[])},
                           [],
                           { V = no }, !. 

