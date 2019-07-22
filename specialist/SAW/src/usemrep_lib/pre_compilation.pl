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

:- module(pre_compilation, [
	pre_compile_data_files/3
   ]).

:- use_module( skr_lib(ctypes), [
	       is_csym/1
   ]).

:- use_module( library(file_systems), [
	       file_exists/1
   ]).

:- use_module( skr_lib(nls_system), [
		control_value/2
   ]).

:- use_module( skr_lib(sicstus_utils), [
	       concat_atom/2
   ]).

:- use_module( library(lists), [
	       append/2,
	       last/2,
	       select_max/3
   ]).

:- use_module( usemrep_domain(semantic_groups), [
	       semgen_semantic_group/2,
	       semrep_semantic_group/2
   ]).

:- use_module( usemrep_lib(exceptions_GEN), [
		ignore_semnet_access_term_GEN/1,
%		ignore_type_relation_type_GEN/3,
		transform_semnet_access_term_GEN/2
   ]).


:- use_module( usemrep_domain(exceptions_DOM), [
		ignore_semnet_access_term_DOM/1,
%		ignore_type_relation_type_DOM/3,
		transform_semnet_access_term_DOM/2
   ]).


% :- use_module( skr_lib( semnet_access06 ), [
% 	        preferred_relation/1,
% 	        relation_inverse/2,
% 		type_relation_type/3
%    ]).
% 
% :- use_module( skr_lib( semnet_access_2012AA ), [
% 	        preferred_relation/1,
% 	        relation_inverse/2,
% 		type_relation_type/3
%    ]).
% 
% :- use_module( skr_lib( semnet_access_2014AA ), [
% 	        preferred_relation/1,
% 	        relation_inverse/2,
% 		type_relation_type/3
%    ]).

:- use_module( usemrep_lib( semnet_access ), [
	        preferred_relation/2,
	        relation_inverse/3,
		type_relation_type/4
   ]).
 
pre_compile_data_files(Application,Versions,Extensions) :-
	pre_compile_semnet_access(Versions,Extensions),
	format('~n### Compiled semnet_access ~n',[]),
	pre_compile_semtype_translation(Application,Versions,Extensions),
	format('~n### Compiled semtype_translation ~n',[]),
	pre_compile_semgroup_member,
	format('~n### Compiled semgroup_member ~n',[]).

pre_compile_semnet_access(Versions, Extensions) :-
	pre_compile_semnet_access(Versions, Extensions,
				  TempPreferredRelationTerms,
				  TempRelationInverseTerms,
				  TempTypeRelationTypeTerms),
	append(TempPreferredRelationTerms, PreferredRelationTerms),
	absolute_file_name(usemrep_lib(semnet_access), SawLibAbsoluteFileName,
			   [extensions(['.pl'])]),
	% open output file and write terms out to semnet_access in usemrep_lib directory
	open(SawLibAbsoluteFileName, write, OutputStream),
	format('~n### Writing Semantic Network file~n    ~w~n', [SawLibAbsoluteFileName]),
	write_semnet_access_header_atom(OutputStream),
	write_semnet_access_versions(Versions, OutputStream),
	write_semnet_access_terms(PreferredRelationTerms, OutputStream),
	format(OutputStream, '', []),
	append(TempRelationInverseTerms, RelationInverseTerms),
	write_semnet_access_terms(RelationInverseTerms, OutputStream),
	append(TempTypeRelationTypeTerms, TypeRelationTypeTerms),
	write_type_relation_type_terms(TypeRelationTypeTerms, OutputStream),
	close(OutputStream),
	compile(SawLibAbsoluteFileName),
	verify_consistency(SawLibAbsoluteFileName).

pre_compile_semnet_access([], [], [], [], []) :- !.
pre_compile_semnet_access([Version|RestVersions],
			  [Extension|RestExtensions],
			  [PreferredRelationTerms|RestPrefRelTerms],
			  [RelationInverseTerms|RestRelInverseTerms],
			  [TypeRelationTypeTerms|RestTypeRelTypeTerms]):-
	format(user_output,'~nUsing semnet_access module version=~a~n',[Version]),
	flush_output(user_output),
	pre_compile_semnet_access_aux(Extension,
				      TempPreferredRelationTerms,
				      TempRelationInverseTerms,
				      TempTypeRelationTypeTerms),
	length(TempPreferredRelationTerms, Len),
	format('Read ~w preferred_relation terms~n',[Len]),
	length(TempRelationInverseTerms, Len0),
	format('Read ~w relation_inverse terms~n', [Len0]),
	length(TempTypeRelationTypeTerms, Len1),
	format('Read ~w type_relation_type terms~n', [Len1]),
	modify_semnet_access_terms(TempPreferredRelationTerms, Version, PreferredRelationTerms),
	modify_semnet_access_terms(TempRelationInverseTerms, Version, RelationInverseTerms),
	modify_type_relation_type_terms(TempTypeRelationTypeTerms, Version, TypeRelationTypeTerms),
	!,
	pre_compile_semnet_access(RestVersions,
				  RestExtensions,
				  RestPrefRelTerms,
				  RestRelInverseTerms,
				  RestTypeRelTypeTerms).
pre_compile_semnet_access([_Version|RestVersions],
			  [_Extension|RestExtensions],
			  PreferredRelationTerms,
			  RelationInverseTerms,
			  TypeRelationTypeTerms):-
	pre_compile_semnet_access(RestVersions,
				  RestExtensions,
				  PreferredRelationTerms,
				  RelationInverseTerms,
				  TypeRelationTypeTerms).

pre_compile_semnet_access_aux(Extension,
			      PreferredRelationTerms,
			      RelationInverseTerms,
			      TypeRelationTypeTerms) :-
	% open input file and read terms in from semnet_access file in skr_lib directory
	concat_atom([semnet_access,Extension],FileNameWithYear),
	absolute_file_name(skr_lib(FileNameWithYear), SkrLibAbsoluteFileName,
			   [extensions(['.pl'])]),
	file_exists(SkrLibAbsoluteFileName),
	format('### Reading Semantic Network file    ~w~n', [SkrLibAbsoluteFileName]),
	flush_output(user_output),	
	open(SkrLibAbsoluteFileName, read, InputStream),
	read_all_terms_in_semnet_access_file(InputStream,
					     PreferredRelationTerms,
					     RelationInverseTerms,
					     TypeRelationTypeTerms).

verify_consistency(FileName) :-
	format('~n### Verifying consistency of facts in ~w...', [FileName]),
	verify_all_preferred_relations_have_inverses,
	verify_all_preferred_relations_are_inverses,
	verify_all_inverse_relations_are_preferred,
	verify_all_inverse_relations_are_inversed,
	verify_all_type_relations_are_inversed.

verify_all_preferred_relations_have_inverses :-
	preferred_relation(Version, R),
	\+ relation_inverse(Version, R, _),
	format('~n~n##### RELATION ~w does not have an inverse in ~w version.~n~n', [R,Version]),
	fail.
verify_all_preferred_relations_have_inverses.

verify_all_preferred_relations_are_inverses :- 
	preferred_relation(Version, R),
	\+ relation_inverse(Version, _, R),
	format('~n~n##### RELATION ~w is not an inverse in ~w version.~n~n', [R, Version]),
	fail.
verify_all_preferred_relations_are_inverses.

verify_all_inverse_relations_are_preferred :-
	relation_inverse(Version, Relation, Inverse),
	\+ preferred_relation(Version, Relation),
	\+ preferred_relation(Version, Inverse),
	format('~n~n##### NEITHER RELATION ~w nor its inverse ~w is preferred in ~w version.~n~n',
	       [Relation, Inverse, Version]),
	fail.
verify_all_inverse_relations_are_preferred.

verify_all_inverse_relations_are_inversed :-
	relation_inverse(Version, Relation, Inverse),
	\+ relation_inverse(Version, Inverse, Relation),
	format('~n~n##### RELATION INVERSE ~w/~w has no inverse in ~w version.~n~n',
	       [Relation, Inverse, Version]),
	fail.
verify_all_inverse_relations_are_inversed.

verify_all_type_relations_are_inversed :-
	type_relation_type(Version, T1, Rel, T2),
	relation_inverse(Version, Rel, Inv),
	\+ type_relation_type(Version, T2, Inv, T1),
%	format('~n~n##### TYPE RELATION ~w-~w-~w does not have an inverse.~n~n', [T1,Rel,T2]),
	fail.
verify_all_type_relations_are_inversed.

verify_relation_numbers :-
	setof(R1, preferred_relation(Version,R1), PreferredRelations),
	format('Verifying numbers for version ~w...~n', [Version]),
	length(PreferredRelations, PreferredRelationCount),
	setof(R2, Inverse^relation_inverse(Version,R2,Inverse), RelationInverses),
	setof(R3, Inverse^relation_inverse(Version,Inverse,R3), RelationInverses),
	length(RelationInverses, RelationInverseCount),
	setof(R4, relation_inverse(Version,R4,R4), SelfInverses),
	length(SelfInverses, SelfInverseCount),
	( ( (RelationInverseCount - SelfInverseCount)/2
	   =:=
	   PreferredRelationCount - SelfInverseCount) -> true
	; format('##### MISCOUNT IN RELATIONS:~n', []),
	  format('##### There are ~w preferred relations,~n',      [PreferredRelationCount]),
	  format('##### There are ~w inverse relations, and~n',    [RelationInverseCount]),
	  format('##### There are ~w self-inverse relations~n',    [SelfInverseCount]),
	  format('##### This relation should hold:~n', []),
	  format('##### (~w-~w)/2 == ~w - ~w~n',
		 [RelationInverseCount, SelfInverseCount,
		  PreferredRelationCount, SelfInverseCount]),
	  format('##### But it does not.~n~n', [])
	),
	fail.
verify_relation_numbers.

write_semnet_access_header_atom(OutputStream) :-
	semnet_access_header_atom(Atom),
	format(OutputStream, '~w', [Atom]),
	flush_output(OutputStream).

%% WARNING!!!!
%% This next clause is very difficult to read
%% because it quotes the header of the semnet_access file.
%% This clause continues until the comment marked "END OF CLAUSE!!"
semnet_access_header_atom(SemnetAccessHeaderAtom) :-
	concat_atom(['\n','\n', 
	              '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n',
                      '%%%                                    %%%\n',
                      '%%%           IMPORTANT:               %%%\n',
		      '%%%    DO NOT MODIFY THIS FILE:        %%%\n',
		      '%%%    IT IS MACHINE-GENERATED         %%%\n',
		      '%%%  from the semnet_access files      %%%\n',
                      '%%%         in $SKR/src/lib            %%%\n',
		      '%%%                                    %%%\n',
		      '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n',
		      '\n',
		      ':- module(semnet_access, [\n',
		      '    preferred_relation/2,\n',
		      '    relation_inverse/3,\n',
		      '    type_relation_type/4]).\n\n'], 
		      SemnetAccessHeaderAtom).
%% < ------------------------------------------------------------------END OF CLAUSE!!

% read in all terms from file
% return in RelationInverseList    a list of all the relation_inverse(X,Y) :- ! terms
% return in TypeRelationTypeList   a list of all the type_relation_type(X,Y,Z)  terms
% return in PreferredRelationList  a list of all the preferred_relation(X) :- ! terms
read_all_terms_in_semnet_access_file(InputStream,
				     PreferredRelationList,
				     RelationInverseList,
				     TypeRelationTypeList) :-
	% The first arg [] is just a bogus term that will not unify with end_of_file.
	r_a_t_in_s_a_f([], InputStream,
		       [], PreferredRelationList,
		       [], RelationInverseList,
		       [], TypeRelationTypeList).

% PRL  = PreferredRelationList
% RIL  = RelationInverseList
% TRTL = TypeRelationTypeList
% r_a_t_in_s_a_f = read_all_terms_in_semnet_access_file
r_a_t_in_s_a_f(end_of_file, InputStream, PRL, PRL, RIL, RIL, TRTL, TRTL) :-
	!,
	close(InputStream).

r_a_t_in_s_a_f(_PreviousTerm, InputStream, PRLIn, PRLOut, RILIn, RILOut, TRTLIn, TRTLOut) :-
	read(InputStream, NewTerm),
	add_term_to_lists(NewTerm, PRLIn, PRLNext, RILIn, RILNext, TRTLIn, TRTLNext),
	r_a_t_in_s_a_f(NewTerm, InputStream, PRLNext, PRLOut, RILNext, RILOut, TRTLNext, TRTLOut).

add_term_to_lists(end_of_file,
		  PRL, PRL,
		  RIL, RIL,
		  TRTL, TRTL) :-
	!.
add_term_to_lists((relation_inverse(Relation,RelationInverse) :- !),
		  PRL, PRL,
		  RIL, [(relation_inverse(Relation,RelationInverse) :- !)|RIL],
		  TRTL, TRTL) :-
	!.
add_term_to_lists((preferred_relation(Relation) :- !),
		  PRL, [(preferred_relation(Relation) :- !)|PRL],
		  RIL, RIL,
		  TRTL, TRTL) :-
	!.
add_term_to_lists(type_relation_type(Type1,Relation,Type2),
		  PRL, PRL,
		  RIL, RIL,
		  TRTL, [type_relation_type(Type1,Relation,Type2)|TRTL]) :-
	!.
add_term_to_lists(_OtherTerm,
		  PRL, PRL,
		  RIL, RIL,
		  TRTL, TRTL).

add_version(preferred_relation(Relation), Version,
	    preferred_relation(Version,Relation)).
add_version(relation_inverse(Relation,RelationInverse), Version,
	    relation_inverse(Version,Relation,RelationInverse)).
add_version(type_relation_type(Type1,Relation,Type2), Version,
	    type_relation_type(Type1,Relation,Type2,Version)).
add_version(semtype_translation_fact(SemType,Abbrev), Version,
	    semtype_translation_fact(Version,SemType,Abbrev)).
add_version(semtype_translation_reverse(Abbrev,SemType), Version,
	    semtype_translation_reverse(Version,Abbrev,SemType)).

%modify_and_write_semnet_access_terms([H|T], OutputStream) :-
%	modify_semnet_access_terms([H|T], TempModifiedSemnetAccessTerms),
%	sort(TempModifiedSemnetAccessTerms, SortedModifiedSemnetAccessTerms),
%	write_modified_semnet_access_terms(SortedModifiedSemnetAccessTerms, OutputStream).

modify_semnet_access_terms(Terms,Version,SortedModifiedTerms) :-
	modify_semnet_access_terms_aux(Terms,Version,ModifiedTerms),
	sort(ModifiedTerms,SortedModifiedTerms).

modify_semnet_access_terms_aux([], _Version,[]).
modify_semnet_access_terms_aux([(SemnetAccessTerm :- !)|Rest], Version, ModifiedSemnetAccessTerms) :-
	( ignore_semnet_access_term_BOTH(SemnetAccessTerm) ->
	  ModifiedSemnetAccessTerms = RestModifiedSemnetAccessTerms
        ; transform_semnet_access_term_BOTH(SemnetAccessTerm, TempTransformedSemnetAccessTerm),
	  add_version(TempTransformedSemnetAccessTerm, Version, TransformedSemnetAccessTerm),
	  ModifiedSemnetAccessTerms =
      		[TransformedSemnetAccessTerm|RestModifiedSemnetAccessTerms]
	),
	!,
	modify_semnet_access_terms_aux(Rest,Version,RestModifiedSemnetAccessTerms).
modify_semnet_access_terms_aux([(SemnetAccessTerm :- !)|Rest],Version,ModifiedSemnetAccessTerms) :-
	format('### Problem with modifying semnet_term ~q in version~a~n',[SemnetAccessTerm,Version]),
	modify_semnet_access_terms_aux(Rest,Version,ModifiedSemnetAccessTerms).

write_semnet_access_versions([], OutputStream) :-
	nl(OutputStream),
	nl(OutputStream).
write_semnet_access_versions([Version|RestVersions], OutputStream) :-
	VersionTerm = semnet_access_version(Version),
	format(OutputStream, '~q.~n', [VersionTerm]),
	flush_output(OutputStream),
	write_semnet_access_versions(RestVersions, OutputStream).	


write_semnet_access_terms([], OutputStream) :-	
	nl(OutputStream),
	nl(OutputStream).
write_semnet_access_terms([SemnetAccessTerm|RestTerms], OutputStream) :-
	format(OutputStream, '~q.~n', [SemnetAccessTerm]),
	flush_output(OutputStream),
	write_semnet_access_terms(RestTerms, OutputStream).

write_type_relation_type_terms([], OutputStream) :- nl(OutputStream).
write_type_relation_type_terms(SemRepTypeRelationTypeTermsIn, OutputStream) :-
	sort(SemRepTypeRelationTypeTermsIn,SemRepTypeRelationTypeTerms),
	compile_type_relation_type_3_to_2(OutputStream, SemRepTypeRelationTypeTerms),
	compile_type_relation_type_2_to_1(OutputStream, SemRepTypeRelationTypeTerms),
	compile_type_relation_type_1(OutputStream,      SemRepTypeRelationTypeTerms).

%modify_type_relation_type_terms(Terms,Version,SortedModifiedTerms) :-
%	modify_type_relation_type_terms_aux(Terms,Version,ModifiedTerms),
%	sort(ModifiedTerms,SortedModifiedTerms).

% Modify list of type_relation_type terms to conform to Caroline's ORIG_semnet_access file
modify_type_relation_type_terms([],_Version,[]) :- !.
modify_type_relation_type_terms([SkrTerm1|RestSkrTerms],Version,SemRepTerms) :-
	( ignore_semnet_access_term_BOTH(SkrTerm1) ->
	  RestSemRepTerms = SemRepTerms
        ; transform_semnet_access_term_BOTH(SkrTerm1, NewSkrTerm1),
	  NewSkrTerm1 \== SkrTerm1,
	  ( ignore_semnet_access_term_BOTH(NewSkrTerm1) ->
	    RestSemRepTerms = SemRepTerms
	  ; add_version(NewSkrTerm1,Version,NewSkrTerm),
	    SemRepTerms = [NewSkrTerm|RestSemRepTerms]
          )
        ; add_version(SkrTerm1,Version,SkrTerm),
	  SemRepTerms = [SkrTerm|RestSemRepTerms]
        ),	
	modify_type_relation_type_terms(RestSkrTerms,Version,RestSemRepTerms).
modify_type_relation_type_terms([SkrTerm|Rest],Version,ModifiedTypeRelTypeTerms) :-
	format('### Problem with modifying type_relation_type ~q in version ~a~n',[SkrTerm,Version]),
	modify_type_relation_type_terms(Rest,Version,ModifiedTypeRelTypeTerms).

% compile_type_relation_type_3_to_2
% creates clauses of this form:
% type_relation_type(aapp,A,B) :- type_relation_type_aapp(A,B).
% type_relation_type(acab,A,B) :- type_relation_type_acab(A,B).
% type_relation_type(acty,A,B) :- type_relation_type_acty(A,B).
% type_relation_type(aggp,A,B) :- type_relation_type_aggp(A,B).

compile_type_relation_type_3_to_2(OutputStream, List) :-
	format(OutputStream, '% level-1 conversion~n', []),
	% see (1) below for the logic of what's going on here
	setof(Length,
	      Clause^Relation^Name^Length^
	      ( member(Clause, List), arg(2, Clause, Relation),
	        name(Relation, Name), length(Name, Length)
	      ),
	      LengthList),
	last(LengthList, LongestLength),

	% create a list of all possible first arguments of type_relation_type_fact/3 terms
	setof(Type1, Clause^(member(Clause, List), arg(4,Clause,Version), arg(1, Clause, Type1)), TypeList),
	member(Type, TypeList),
	concat_atom([type_relation_type, '_', Type], BodyFunctor),
	name(Type, TypeName),
	length(TypeName, TypeNameLength),
	adjust_spacing_for_quoted_relation(TypeName, SpacingAdjustment),
	Blanks is LongestLength - TypeNameLength - SpacingAdjustment + 1,
	format(OutputStream,
	       'type_relation_type(~q,~q,~*cA, B) :- ~q(~q, A, B).~n',
	       [Version,Type,Blanks,32,BodyFunctor,Version  ]),
	flush_output(OutputStream),
	fail
      ; nl(OutputStream).


% The following simpler code is not correct, because the call to writeq/1,
% which is necessary for handling atoms like 'co-occurs_with',
% would end up writing clauses such as
% type_relation_type(aapp,'A','B') :- type_relation_type_aapp('A','B').

% An alternative would be to use anonymous variables and write clauses like
% type_relation_type(aapp,_1042588,_1042589) :- type_relation_type_aapp(_1042588,_1042589).
% but this approach leads to far less readable code.

% 	functor(RuleHead, type_relation_type, 3),
% 	arg(1, RuleHead, Type),
% 	arg(2, RuleHead, A),
% 	arg(3, RuleHead, B),
% 	functor(RuleBody, BodyFunctor, 2),
% 	arg(1, RuleBody, A),
% 	arg(2, RuleBody, B),
% 	writeq((RuleHead  :- RuleBody)),
% 	write('.'),
% 	nl,
% 	fail
%     ; nl.

% compile_type_relation_type_2_to_1
% creates clauses of this form:
% type_relation_type_aapp(affects,B)     :- type_relation_type_aapp_affects(B).
% type_relation_type_aapp(analyzed_by,B) :- type_relation_type_aapp_analyzed_by(B).
% type_relation_type_aapp(causes,B)      :- type_relation_type_aapp_causes(B).
% type_relation_type_aapp(consists_of,B) :- type_relation_type_aapp_consists_of(B).
% which are called by the clauses created by compile_type_relation_type_3_to_2/0

% (1) determine the length of the longest second argument
% of the type_relation_type/3 facts--for pretty-printing, e.g.,

% type_relation_type_aapp(affects,                 B) :- type_relation_type_aapp_affects(B).
% type_relation_type_aapp(analyzed_by,             B) :- type_relation_type_aapp_analyzed_by(B).
% type_relation_type_aapp(causes,                  B) :- type_relation_type_aapp_causes(B).
% type_relation_type_aapp(consists_of,             B) :- type_relation_type_aapp_consists_of(B).
% type_relation_type_aapp(constitutes,             B) :- type_relation_type_aapp_constitutes(B).
% type_relation_type_aapp(has_property,            B) :- type_relation_type_aapp_has_property(B).

% We worry about the second argument only because the first argument
% always has exactly four characters.

% (2) Create a list of all first/second argument pairs of the original type_relation-type/3 facts.
% This is necessary because of clauses such as

% type_relation_type_fact(aapp,affects,biof).
% type_relation_type_fact(aapp,affects,celf).
% type_relation_type_fact(aapp,affects,comd).
% type_relation_type_fact(aapp,affects,dsyn).
% type_relation_type_fact(aapp,affects,emod).

% which share common first and second arguments.

compile_type_relation_type_2_to_1(OutputStream, List) :-
	format(OutputStream, '% level-2 conversion~n', []),
	% (1): See above
	setof(CombinedLength,
	      Clause^Type1^Relation^Type1Name^Type1Length^RelationName^RelationLength^
	      ( member(Clause, List),
		arg(1, Clause, Type1),
		arg(2, Clause, Relation),
	        name(Type1, Type1Name),
	        name(Relation, RelationName),
		length(Type1Name, Type1Length),
		length(RelationName, RelationLength),
	        CombinedLength is Type1Length + RelationLength
	      ),
	      LengthList),
	select_max(LongestLength, LengthList, _Residue),
	% (2): See above
	setof(Type1-Relation-Version,
              Clause^(member(Clause, List), arg(4, Clause, Version), arg(1, Clause, Type1), arg(2, Clause, Relation)),
              TypeRelationList),
%	format('~nType Rel ~q~n',[TypeRelationList]),
        member(T-Rel-Version, TypeRelationList),
	name(Rel, RelName),
	length(RelName, RelNameLength),
	name(T, TName),
	length(TName, TNameLength),
	adjust_spacing_for_quoted_relation(RelName, SpacingAdjustment),
	Blanks is LongestLength - RelNameLength - TNameLength - SpacingAdjustment + 1,
	concat_atom([type_relation_type, '_', T], HeadFunctor),
	concat_atom([type_relation_type, '_', T,'_',Rel], BodyFunctor),
	format(OutputStream,
	       '~q(~q,~q,~*cB) :- ~q(~q,B).~n',
	       [HeadFunctor,Version,Rel,Blanks,32,BodyFunctor,Version]),
	flush_output(OutputStream),
	fail
      ; nl(OutputStream).


adjust_spacing_for_quoted_relation(RelName, SpacingAdjustment) :-
	( member(Char, RelName),
	  \+ is_csym(Char) ->
	  SpacingAdjustment is 2
        ; SpacingAdjustment is 0
        ).
   
% Again, this code would not work for the same reason as outlined above.
% 	functor(RuleHead, HeadFunctor, 2),
% 	arg(1, RuleHead, R),
% 	arg(2, RuleHead, 'B'),
% 	functor(RuleBody, BodyFunctor, 1),
% 	arg(1, RuleBody, 'B'),
% 	writeq((RuleHead :- RuleBody)),
% 	write('.'),
% 	nl,
% 	fail
%     ; nl.

% compile_type_relation_type_1
% creates clauses of the form
% type_relation_type_aapp_affects(biof).
% type_relation_type_aapp_affects(celf).
% type_relation_type_aapp_affects(comd).
% type_relation_type_aapp_affects(dsyn).
% which are called by the clauses created by compile_type_relation_type_1_to_1/0

compile_type_relation_type_1(OutputStream, List):-
	format(OutputStream, '% level-3 conversion~n', []),
	member(Clause, List), % type_relation_type_fact(A, B, C),
	arg(1, Clause, Type1),
	arg(2, Clause, Relation),
	arg(3, Clause, Type2),
	arg(4, Clause, Version),
	concat_atom([type_relation_type, '_', Type1, '_', Relation], Functor),
	functor(Fact, Functor, 2),
	arg(1, Fact, Version),
	arg(2, Fact, Type2),
	format(OutputStream, '~q.~n', [Fact]),
	flush_output(OutputStream),
	fail
      ; nl(OutputStream).

% Currently, we compile semtype_translation for individual UMLS releases.
% This becomes a problem when we give the ability to choose a UMLS release.
% What was done so far was to combine semtype_translation from individual UMLS releases
% into one file semtype_translation.pl. This needs to be automated for future releases.
% The same for semnet_access.pl.
pre_compile_semtype_translation(Application,Versions,Extensions) :- 
       absolute_file_name(usemrep_lib(semtype_translation),
			   SawLibAbsoluteFileName, [extensions(['.pl'])]),
       % open output file and write terms out to semtype_translation file in usemrep_lib directory
       open(SawLibAbsoluteFileName, write, OutputStream),
       format('~n### Writing semantic type translation file~n   ~w~n',[SawLibAbsoluteFileName]),
       write_semtype_translation_header_atom(OutputStream),
       flush_output(OutputStream),
       pre_compile_semtype_translation(Application,Versions,Extensions,
				       TempSemTypeTranslationTerms,
				       TempReverseSemTypeTranslationTerms,
				       TempRestTerms),
       append(TempSemTypeTranslationTerms,SemTypeTranslationTerms),
       write_terms(SemTypeTranslationTerms,OutputStream),
       nl(OutputStream),
       nl(OutputStream),
       append(TempReverseSemTypeTranslationTerms,ReverseSemTypeTranslationTerms),
       write_terms(ReverseSemTypeTranslationTerms,OutputStream),
       nl(OutputStream),
       nl(OutputStream),
       append(TempRestTerms,RestTerms),
       write_terms(RestTerms,OutputStream),
       nl(OutputStream),
       format(OutputStream, 'application_name(~a).~n', [Application]),
       close(OutputStream),
       compile(SawLibAbsoluteFileName).

pre_compile_semtype_translation(_Application,[],[],[],[],[]) :- !.
pre_compile_semtype_translation(Application,[Version|RestVersions],[Extension|RestExtensions],
				[SemTypeTranslationTerms|RestSemTypeTranslationTerms],
				[RevSemTypeTranslationTerms|RestRevSemTypeTranslationTerms],
				[OtherTerms|RestOtherTerms]) :-
	format(user_output,'Using semtype_translation module version=~a-~a~n',[Version,Extension]),
	flush_output(user_output),
	pre_compile_semtype_translation_aux(Application, Extension,
					    TempSemTypeTranslationTerms,
					    TempRevSemTypeTranslationTerms,TempOtherTerms),
	length(TempSemTypeTranslationTerms,Len),
	format('Read ~w semtype_translation terms~n',[Len]),
	flush_output(user_output),
	length(TempRevSemTypeTranslationTerms,Len0),
	format('Read ~w reverse_semtype_translation terms~n',[Len0]),
	flush_output(user_output),
	length(TempOtherTerms,Len1),
	format('Read ~w other terms~n',[Len1]),
	flush_output(user_output),
	modify_semtype_translation_terms(TempSemTypeTranslationTerms,
					 Version,
					 SemTypeTranslationTerms),
	modify_semtype_translation_terms(TempRevSemTypeTranslationTerms,
					 Version,
					 RevSemTypeTranslationTerms),
	modify_semtype_translation_terms(TempOtherTerms,Version,OtherTerms),
	!,
	pre_compile_semtype_translation(Application, RestVersions, RestExtensions,
					RestSemTypeTranslationTerms,
					RestRevSemTypeTranslationTerms,
					RestOtherTerms).
pre_compile_semtype_translation(Application,[_Version|RestVersions],[_Extension|RestExtensions],
				SemTypeTranslationTerms,
				RevSemTypeTranslationTerms,
				OtherTerms) :-
	pre_compile_semtype_translation(Application, RestVersions, RestExtensions,
					SemTypeTranslationTerms,
					RevSemTypeTranslationTerms,
				OtherTerms).

%pre_compile_semtype_translation(Application) :-
%	add_year_to_filename(semtype_translation,FileNameWithYear),
%	pre_compile_semtype_translation_aux(Application,FileNameWithYear).

pre_compile_semtype_translation_aux(_Application, Extension,
				    SemTypeTranslationTerms,
				    RevSemTypeTranslationTerms,
				    RestTerms) :-
	concat_atom([semtype_translation,Extension],FileNameWithYear),
	absolute_file_name(skr_lib(FileNameWithYear), SkrLibAbsoluteFileName,
	                   [extensions(['.pl'])]),
	file_exists(SkrLibAbsoluteFileName),
	% open input file and read terms in from semtype_translation file in the skr_lib directory
	open(SkrLibAbsoluteFileName, read, InputStream),
	format('~n~n### Reading SemType Translation file    ~w~n', [SkrLibAbsoluteFileName]),
	read_all_terms_in_semtype_translation_file(InputStream,
						   0,
						   SemTypeTranslationTerms,
						   RevSemTypeTranslationTerms,
						   RestTerms),
	close(InputStream).

% read in all terms from file
% return in SemTypeTranslationList         a list of all the semtype_translation(A,B) terms
% return in ReverseSemTypeTranslationList  a list of all the semtype_translation_reverse(B,A) terms
% return in OtherTerms                     a list of all the terms appearing
%        after the semtype_translation(A,B,C,D) terms
read_all_terms_in_semtype_translation_file(Stream,
					   SemTypeTranslationTermFound,
					   SemTypeTranslationList,
					   ReverseSemTypeTranslationList,
					   OtherTerms) :-
	read(Stream, Term),
	% If we've finished reading terms, close the two lists
	( Term == end_of_file ->
	  SemTypeTranslationList = [],
	  ReverseSemTypeTranslationList = [],
	  OtherTerms = []
	% If we've read a semtype_translation/4 term
        ; Term = ( semtype_translation(A,B,_,_) :- ! ) ->
	  SemTypeTranslationList =
	        [semtype_translation_fact(A,B)|RestSemTypeTranslationTerms],
	  ReverseSemTypeTranslationList =
	        [semtype_translation_reverse(B,A)|RestReverseSemTypeTranslationTerms],
	  read_all_terms_in_semtype_translation_file(Stream,
						     1,
						     RestSemTypeTranslationTerms,
						     RestReverseSemTypeTranslationTerms,
						     OtherTerms)
	% In 2012AA, last two arguments seem to be gone
	; Term = ( semtype_translation_fact(A,B)) ->
	  SemTypeTranslationList =
	        [semtype_translation_fact(A,B)|RestSemTypeTranslationTerms],
	  read_all_terms_in_semtype_translation_file(Stream,
						     1,
						     RestSemTypeTranslationTerms,
						     ReverseSemTypeTranslationList,
						     OtherTerms)
	; Term = ( semtype_translation_reverse(A,B)) ->
	  ReverseSemTypeTranslationList =
	        [semtype_translation_reverse(A,B)|RestReverseSemTypeTranslationTerms],
	  read_all_terms_in_semtype_translation_file(Stream,
						     1,
						     SemTypeTranslationList,
						     RestReverseSemTypeTranslationTerms,
						     OtherTerms)
	% If we've read any other term
	; ( SemTypeTranslationTermFound == 1 ->
	    OtherTerms = [Term|RestOtherTerms],
	    read_all_terms_in_semtype_translation_file(Stream,
						       1,
						       SemTypeTranslationList,
						       ReverseSemTypeTranslationList,
						       RestOtherTerms)
	  ; read_all_terms_in_semtype_translation_file(Stream,
						       SemTypeTranslationTermFound,
						       SemTypeTranslationList,
						       ReverseSemTypeTranslationList,
						       OtherTerms)
	  )
	).

write_semtype_translation_header_atom(OutputStream) :-
	semtype_translation_header_atom(Atom),
	write(OutputStream, Atom),
	flush_output(OutputStream).


%% WARNING!!!!
%% This next clause is very difficult to read
%% because it quotes the header of the semtype_translation file.
%% This clause continues until the comment marked "END OF CLAUSE!!"
semtype_translation_header_atom(SemTypeTranslationHeaderAtom) :-
	concat_atom(['\n',
	              '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n',
		      '%%%                                          %%%\n',
		      '%%%           IMPORTANT:                     %%%\n',
		      '%%%    DO NOT MODIFY THIS FILE:              %%%\n',
		      '%%%    IT IS MACHINE-GENERATED               %%%\n',
		      '%%%  from the semtype_translation files      %%%\n',
		      '%%%        in $SKR/src/lib                   %%%\n',
		      '%%%                                          %%%\n',
		      '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n',
		      '\n',
		      ':- module(semtype_translation, [\n',
%		      '     abbreviate_semtypes/3,\n',
		      '     application_name/1,\n',
%		      '     expand_semtypes/3,\n',
%		      '     is_semtype/2,\n',
%		      '     is_abbrev_semtype/2,\n',
		      '     semtype_translation/3\n',
%		      '     semtype_translation/5\n',
		      ']).\n',
		      '\n',
%		      'semtype_translation(Version, Name, Abbrev, _UI, _TreeNo) :-\n',
%		      'semtype_translation(Version, Name, Abbrev).\n',
%		      '\n',
		      'semtype_translation(Version, Name, Abbrev) :-\n',
		      '     ( nonvar(Name) ->\n',
		      '       semtype_translation_fact(Version, Name, Abbrev)\n',
		      '\n',          
		      '     ; semtype_translation_reverse(Version, Abbrev, Name)\n',
		      '     ),\n',
		      '     !.\n\n'], SemTypeTranslationHeaderAtom).
%% < ------------------------------------------------------------------END OF CLAUSE!!

%modify_semtype_translation_terms(Terms,Version,SortedModifiedTerms) :-
%	modify_semnet_access_terms_aux(Terms,Version,ModifiedTerms),
%	sort(ModifiedTerms,SortedModifiedTerms).

modify_semtype_translation_terms([],_Version,[]) :- !.
modify_semtype_translation_terms([SemTypeTranslationTerm|Rest], Version,
				 ModifiedSemTypeTranslationTerms) :-
	add_version(SemTypeTranslationTerm,Version,ModifiedSemTypeTranslationTerm),
	ModifiedSemTypeTranslationTerms =
      		[ModifiedSemTypeTranslationTerm|RestModifiedSemTypeTranslationTerms],
	!,
	modify_semtype_translation_terms(Rest,Version,RestModifiedSemTypeTranslationTerms).
modify_semtype_translation_terms([SemTypeTranslationTerm|Rest],Version,ModifiedSemTypeTranslationTerms) :-
	format('### Problem with modifying semtype_translation_term ~q in version ~a~n',[SemTypeTranslationTerm,Version]),
	modify_semtype_translation_terms(Rest,Version,ModifiedSemTypeTranslationTerms).

write_terms([], _OutputStream).
write_terms([H|T], OutputStream) :-
	format(OutputStream, '~q.~n', [H]),
	write_terms(T, OutputStream).

pre_compile_semgroup_member :-
	compiled_semgroups_filename(FileName),
	format('~n### Writing SemGroup Member file~n~w~n~n', [FileName]),
	open(FileName, write, OutputStream),
	compile_semgroup_member_aux(OutputStream).

compiled_semgroups_filename(SemGroupFileName) :-
	absolute_file_name(usemrep_main(semgroup_member), SemGroupFileName,
			   [extensions(['.pl'])]).
%compiled_semgroups_filename('semgroup_member.pl').

compile_semgroup_member_aux(OutputStream) :-
	semgroup_member_header_atom(SemGroupMemberHeaderAtom),
	format(OutputStream, '~n~w~n~n', [SemGroupMemberHeaderAtom]),
	( semgen_semantic_group(GroupName, Members),
	  Application = semgen
	; semrep_semantic_group(GroupName, Members),
	  Application = semrep
	),
	member(SemanticType, Members),
	% writing instead simply
	% write(semgroup_member(SemanticType, GroupName))
	% would be simpler, but the resulting code written this way
	% is more readable
	format(OutputStream,
	       '~w_semgroup_member(~q,~q).~n',
	       [Application,SemanticType,GroupName]),
	fail
      ; close(OutputStream).

%% WARNING!!!!
%% This next clause is very difficult to read
%% because it quotes the header of the semgroup_member file.
%% This clause continues until the comment marked "END OF CLAUSE!!"
semgroup_member_header_atom(SemGroupMemberHeaderAtom) :-
	concat_atom(['\n\n',
	              '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n',
		      '%%%                                    %%%\n',
		      '%%%           IMPORTANT:               %%%\n',
		      '%%%    DO NOT MODIFY THIS FILE:        %%%\n',
		      '%%%    IT IS MACHINE-GENERATED         %%%\n',
		      '%%%  from the semantic_groups file     %%%\n',
		      '%%% in $SAW/src/Usemrep/usemrep_domain %%%\n',
		      '%%%                                    %%%\n',
		      '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n',
		      '\n',
		      ':- module(semgroup_member, [semrep_semgroup_member/2, semgen_semgroup_member/2]).'],
			SemGroupMemberHeaderAtom). % <--- END OF CLAUSE


%%% DOMAIN-CHOICE LOGIC begins here
% There are three files that exist in both GENeric and DOMain flavors:
% exceptions, locsemnet, and semrules.

% (1) The logic for locsemnet and semrules is:
%  * If running DOMain SemRep, call the DOM version first,
%    and if that fails, call the GEN version.
%  * If running GENeric SemRep, call the GEN version only.

% (2) The logic for exceptions is
%  * If running DOMain SemRep,  call the DOM version only.
%  * If running GENeric SemRep, call the GEN version only.

% ignore_semnet_access_term is part of exceptions, so the second set of rules applies.
ignore_semnet_access_term_BOTH(Term) :-
	( control_value(domain, _Domain) ->
	  ignore_semnet_access_term_DOM(Term)
	; ignore_semnet_access_term_GEN(Term)
	).

% transform_semnet_access_term is part of exceptions, so the second set of rules applies.
transform_semnet_access_term_BOTH(Term, NewTerm) :-
	( control_value(domain, _Domain) ->
	  transform_semnet_access_term_DOM(Term, NewTerm)
	; transform_semnet_access_term_GEN(Term, NewTerm)
	).
