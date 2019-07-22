
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
	qp_mf_misc.pl

%DESCRIPTION OF FILE
	Quintus Prolog (.pl) source file.

%REVISED
	8Apr94 divita -- Initial Version

%%
==========================================================*/

:- module(qp_mf_misc,[
	mf_infcomp/3,
	mf_ascomp/3,
	mf_ingcomp/3,
	mf_edcomp/3,
	mf_binfcomp/3,
	mf_whinfcomp/3,
	mf_fincomp/3,
	mf_prep_phrase/3,
	mf_misc/3 
]).

:- use_module(lexicon(qp_mf_utils), [
	mf_tab/2,
	mf_newline/2
   ]).


/*==========================================================
%FUNCTION NAME
	interpretation_codes
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	interpretation_codes(arg) :-
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
mf_infcomp(infcomp:[C]) --> "infcomp:", mf_interp_code(C).
mf_ascomp(ascomp:[C]) --> "ascomp:", mf_interp_code(C).
mf_ingcomp(ingcomp:[C]) --> "ingcomp:", mf_interp_code(C).
mf_edcomp(edcomp:[C]) --> "edcomp:", mf_interp_code(C).
mf_binfcomp(binfcomp:[C]) --> "binfcomp:", mf_interp_code(C).
mf_whinfcomp(whinfcomp:[C]) --> "whinfcomp:", mf_interp_code(C).

%%% interpretation codes
mf_interp_code(objc) --> "objc".
mf_interp_code(objr) --> "objr".
mf_interp_code(subjc) --> "subjc".
mf_interp_code(subjr) --> "subjr".
mf_interp_code(arbc) --> "arbc".
mf_interp_code(nsc) --> "nsc".
mf_interp_code(nsr) --> "nsr".


/*** end interpretation_codes */


/*==========================================================
%FUNCTION NAME
	finite_complements
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	finite_complements(arg) :-
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
%%% finite complements
mf_fincomp(fincomp:[T]) -->
    "fincomp(", mf_fincomp_val(T), ")".
mf_fincomp(fincomp:[T, subj]) -->
    "fincomp(", mf_fincomp_val(T), "):subj".

mf_fincomp_val(o) --> "o".
mf_fincomp_val(t) --> "t".
mf_fincomp_val(p) --> "p".
mf_fincomp_val(s) --> "s".
mf_fincomp_val(ts) --> "ts".
mf_fincomp_val(tp) --> "tp".
mf_fincomp_val(sp) --> "sp".
mf_fincomp_val(tsp) --> "tsp".

 /*** end finite_complements */


/*==========================================================
%FUNCTION NAME
	prepositional_phrase
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	prepositional_phrase(arg) :-
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
mf_prep_phrase(P) -->
    (	mf_prep_phrase2(P)
    ;	mf_prep_phrase3(P)
    ).


 /*** end prepositional_phrase */


/*==========================================================
%FUNCTION NAME
	prep_phrase,_single_object
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	prep_phrase,_single_object(arg) :-
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
mf_prep_phrase2(pphr:[prep:[P], obj:[O]]) -->
                                              "pphr(", 
					      { atom_codes(P, PL) }, PL, 
					      ",", 
					      mf_prep_object(O), 
					      ")".

 /*** end prep_phrase,_single_object */



/*==========================================================
%FUNCTION NAME
	prep_phrase,_double_object
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	prep_phrase,_double_object(arg) :-
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
mf_prep_phrase3(pphr:[prep:[P], obj:[O], P2]) -->
                                                  "pphr(", 
						  { atom_codes(P, PL) }, PL, 
						  ",",
						  mf_prep_object(O), 
						  ",",	
						  mf_prep_phrase2(P2), 
						  ")".

 /*** end phrase,_double_object */

/*==========================================================
%FUNCTION NAME
	mf_prep_object
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	mf_prep_object() :-
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
mf_prep_object(np)        --> "np".
mf_prep_object(np:[T])    --> "np|", { atom_codes(T, L) }, L, "|".
mf_prep_object(adj)       --> "adj".
mf_prep_object(advbl)     --> "advbl".
mf_prep_object(I)         --> mf_infcomp(I).
mf_prep_object(G)         --> mf_ingcomp(G).
mf_prep_object(W)         --> mf_whinfcomp(W).
mf_prep_object(whfincomp) --> "whfincomp".
mf_prep_object(B)         --> mf_binfcomp(B).
mf_prep_object(E)         --> mf_edcomp(E).

 /*** end mf_prep_object */

/*==========================================================
%FUNCTION NAME
	miscellaneous_slots,_i.e.,_acronyms,_abbreviations_and_keys
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	miscellaneous_slots,_i.e.,_acronyms,_abbreviations_and_keys(arg) :-
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



/*==========================================================
%FUNCTION NAME
	mf_misc
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	mf_misc(arg) :-
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
mf_misc([F|R]) -->
                    %{format("In mf_misc/2 ~n",[])},
                    mf_misc_aux(F),
		    mf_misc(R).

mf_misc([])    --> [].

 /*** end mf_misc */


/*==========================================================
%FUNCTION NAME
	mf_misc_aux
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	mf_misc_aux(arg) :-
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
mf_misc_aux(acronym_of:As)      --> mf_acronyms(As).
mf_misc_aux(abbreviation_of:As) --> mf_abbreviations(As).
mf_misc_aux(keys:Ks)            --> %{format("misc aux keys",[])},
                                    mf_keys(Ks).
mf_misc_aux(variant:Vs)         --> 
                                    %{format("misc aux variant ~n",[])},
                                    mf_modal_variants(Vs).

mf_misc_aux(trademark)         -->  
                        mf_tab, 
			"trademark", 
			mf_newline.

 /*** end mf_misc_aux */


/*==========================================================
%FUNCTION NAME
	acronyms
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	acronyms(arg) :-
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
mf_acronyms([A|R]) -->
                        mf_tab, 
			"acronym_of=", 
			{ atom_codes(A, L) }, 
			L, 
			mf_newline,
			mf_acronyms(R).

mf_acronyms([])    --> [].


 /*** end acronyms */


/*==========================================================
%FUNCTION NAME
	abbreviations
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	abbreviations(arg) :-
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
mf_abbreviations([A|R]) -->
                              mf_tab, 
			      "abbreviation_of=", 
			      { atom_codes(A, L) }, 
			      L, 
			      mf_newline,
			      mf_abbreviations(R).

mf_abbreviations([])    --> [].


 /*** end abbreviations */


/*==========================================================
%FUNCTION NAME
	keys
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	keys(arg) :-
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
mf_keys([A|R]) -->
                   mf_tab, 
		   "key=", !, { atom_codes(A, L) }, L, mf_newline,
		   mf_keys(R).

mf_keys([])    --> [].


 /*** end keys */

/*==========================================================
%FUNCTION NAME
	mf_modal_variants
%PURPOSE
	?
%I/O PARAMETER LIST
	?
%EXAMPLE CALL
	mf_modal_variants(arg) :-
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
mf_modal_variants(R) -->
	        %{format(user_output,"In mf_modal_variant 1{~q ~q ~q ~q}~n",R)},
		!,
		{R = [variant-string:B, tense:T,agreement:Ag, negative:N]},!,
		mf_tab, 
		"variant=",!, {atom_codes(B,Bl)},Bl,
		";",!,
		mf_modal_tense(T),
		mf_modal_agreement(Ag),
  	        mf_modal_negative(N),
		mf_newline,!.


mf_modal_tense(infinitive) --> "infinitive".
mf_modal_tense(pres_part) --> "pres_part".
mf_modal_tense(past_part) --> "past_part".
mf_modal_tense(past) --> "past".
mf_modal_tense(pres) --> "pres".


mf_modal_agreement(R) --> 
	                   %{format(user_output,"In mf_modal_agreement~n",[])},
			    "(",
			    mf_modal_agreement_list(R),
			    ")".

mf_modal_agreement([]) --> [].
			    
mf_modal_agreement_list([F]) --> 
	             %{format(user_output,"In mf_modal_agreement_list with one~n",[])},
		      {atom_codes(F,Al)},!, Al .

mf_modal_agreement_list([F|R]) --> 
	             %{format(user_output,"In mf_modal_agreement_list with many~n",[])},
		      {atom_codes(F,Al)},!, Al ,
                      ",",
		      mf_modal_agreement_list(R).
		      
mf_modal_negative(yes) --> ":",
	                   "negative".

mf_modal_negative(no) --> [].

