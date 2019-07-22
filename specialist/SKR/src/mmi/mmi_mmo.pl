% File:     mmi_mmo.pl
% Module:   MMI MetaMap Output
% Author:   Lan
% Purpose:  Operates on machine-readable MetaMap output


:- module(mmi_mmo,[
    mmo_opt_mode/1,
    phrase_info/3
    ]).


:- dynamic mmo_opt_mode/1.

:- dynamic utterance_term/1.

/* 
   phrase_info(+FieldName, ?Phrase, ?Field)

*/

/* phrase_info(+FieldName, ?Phrase, ?Field)

phrase_info/3 instantiates or retrieves the FieldName Field of
Phrase.  */

phrase_info(phrase,          phrase(Value,_,_,_,_,_,_), Value).
phrase_info(candidates,      phrase(_,Value,_,_,_,_,_), Value).
phrase_info(mappings,        phrase(_,_,Value,_,_,_,_), Value).
phrase_info(pwi,             phrase(_,_,_,Value,_,_,_), Value).
phrase_info(gvcs,            phrase(_,_,_,_,Value,_,_), Value).
phrase_info(ev0,             phrase(_,_,_,_,_,Value,_), Value).
phrase_info(aphrases,        phrase(_,_,_,_,_,_,Value), Value).

