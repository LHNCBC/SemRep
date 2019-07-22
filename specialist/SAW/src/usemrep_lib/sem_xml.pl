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

% File:     sem_xml.pl
% Module:   Generate XML output
% Author:   Halil
% Purpose:  Generate XML output analogous to full fielded output

:- module(sem_xml,[
	conditionally_print_xml_header/2,
	conditionally_print_xml_footer/3,
	generate_and_print_utterance_xml/9,
	generate_document_header_xml/3
    ]).

:- use_module(skr_lib(nls_system), [
	control_option/1,
	control_value/2
   ]).

:- use_module(skr_lib(nls_strings),[
	atom_codes_list/2,
	form_one_string/3,
	split_string_completely/3
   ]).

:- use_module(skr_lib(sicstus_utils),[
	concat_atom/2,
	concat_atom/3,
	interleave_string/3,
	midstring/6,
	ttyflush/0
   ]).

:- use_module(skr_lib(xml),[
	xml_parse/3
   ]).

:- use_module(skr(skr_utilities), [
	replace_blanks_with_crs/4
   ]).

:- use_module(usemrep_lib(ssuppserv), [
	split_input_label/4
   ]).

:- use_module(library(lists),[
	append/2,
	delete/3,
	last/2,
	nth1/4
   ]).

:- use_module(library(system),[
	environ/2
   ]).


% This code creates Prolog structures that are fed to xml:xml_parse.
generate_and_print_utterance_xml(OutputStream, Position, UtteranceType, InputLabel,
				 UNExpandedInputText, AllEntities, Entities,
				 PredicationLen,Predications) :-
	XMLTerm = xml([],XMLUtteranceTerm),
	generate_utterance_xml(Position, UtteranceType, InputLabel, UNExpandedInputText,
			       AllEntities, Entities, PredicationLen, Predications, XMLUtteranceTerm),
	xml_parse(XMLChars, XMLTerm, [format(true)]),
	format(OutputStream, "~s", [XMLChars]).

% a hack to add Document element without the end tag.
generate_document_header_xml(DocumentID,DocumentTextAtom,XMLOut) :-
	XMLTerm = xml([],XMLDocumentTerm),
	concat_atom(['D',DocumentID],DID),
	atom_codes(DID,DocumentIDString),
	atom_codes(DocumentTextAtom,DocumentTextString),
	append([[id=DocumentIDString,text=DocumentTextString]],DocumentAttributeList),
	create_xml_element('Document',DocumentAttributeList,[],XMLDocumentTerm),
	xml_parse(XMLChars, XMLTerm, [format(true)]),
	atom_codes(XMLAtom,XMLChars),
	midstring(XMLAtom,XMLAtom0,'/>',0,_,2),
	atom_codes(XMLAtom0,XMLChars0),
	append(XMLChars0,">",XMLOut).
	
generate_utterance_xml(Position, UtteranceType, InputLabel, UNExpandedInputText,
		       AllEntities, Entities, PredicationLen, Predications, XMLUtteranceTerm) :-
	Position = StartPos/Length,
	EndPos is StartPos + Length,
	number_codes(StartPos,StartPosString),
	number_codes(EndPos,EndPosString),
	split_input_label(InputLabel, PMID, TiOrAb, UtteranceID),
	concat_atom(['D',PMID],DPMID),
	atom_codes(TiOrAb,TiOrAbString),
	atom_codes(UtteranceID,UtteranceIDString),
	concat_atom([DPMID, TiOrAb,UtteranceID],'.',TempUtteranceID),
	atom_codes(TempUtteranceID,InputLabelString),
%	append([TempUtteranceIDString,UtteranceIDString],InputLabelString),
	generate_entity_xml(AllEntities,1,AllEntityIDs,_AllXMLEntitiesTerm),
	get_first_entity_index(AllEntities,Entities,N),
	generate_entity_xml(Entities,N,_EntityIDs,XMLEntitiesTerm),
	generate_predication_xml(Predications,PredicationLen,AllEntityIDs,AllEntities,[],_Scales,XMLPredicationsTerm),
	( UtteranceType == '' ->
	  UtteranceTypeList = []
	; atom_codes(UtteranceType,UtteranceTypeString),
	  UtteranceTypeList = [type=UtteranceTypeString]
	),
	append([[id=InputLabelString,section=TiOrAbString,number=UtteranceIDString,begin=StartPosString,end=EndPosString],
		UtteranceTypeList,[text=UNExpandedInputText]], UtteranceAttributeList),
	create_xml_element('Utterance', UtteranceAttributeList,
			   [XMLEntitiesTerm,XMLPredicationsTerm],
			   XMLUtteranceTerm).


get_first_entity_index([],_,1) :- !.
get_first_entity_index(_,[],1) :- !.
get_first_entity_index(AllEntities,Entities,N) :-
	Entities = [First|_Rest],
	nth1(N,AllEntities,First,_).
			       
generate_entity_xml([],_,[],[]).
generate_entity_xml([FirstMetaConc|RestMetaConcs], IndexIn, [FirstEntityID|RestEntityIDs],
		    [FirstMetaConcXML|RestMetaConcXML]) :-
	generate_one_entity_xml(FirstMetaConc, IndexIn, FirstEntityID, FirstMetaConcXML),
	IndexOut is IndexIn + 1,
        !,				  
        generate_entity_xml(RestMetaConcs, IndexOut,RestEntityIDs, RestMetaConcXML).
generate_entity_xml([_FirstMetaConc|RestMetaConcs], Index, RestEntityIDs, RestMetaConcXML) :-
        generate_entity_xml(RestMetaConcs, Index, RestEntityIDs,RestMetaConcXML).

generate_one_entity_xml('2entity'-('SE'-PMID-_UtteranceType-_TiOrAb-_UtteranceID-entity-
		        CUI-MetaConc-SemTypeListAtom-NormGeneID-NormGeneName-InputText-
			_Change-_Degree-Negation-Confidence-StartPos-EndPos),
			Index,EntityID,XML) :-
	atom_codes(CUI,CUIString),
	atom_codes(MetaConc,MetaConcString),
	atom_codes(SemTypeListAtom,SemTypeListString),
	atom_codes(NormGeneID,NormGeneIDString),
	atom_codes(NormGeneName,NormGeneNameString),
	atom_codes(InputText,InputString),
	( Negation == 1 ->
	  NegationString = "true"
	; NegationString = "false" 
	),
%	number_codes(Negation,NegationString),
	number_codes(Confidence,ConfidenceString),
	number_codes(StartPos,StartPosString),
	number_codes(EndPos,EndPosString),
	number_codes(Index,IndexString),
%	concat_atom([PMID,TiOrAb,UtteranceID,'E'],'.',TempEntityID0),
%	concat_atom(['U',TempEntityID0],TempEntityID),
%	atom_codes(TempEntityID,TempEntityIDString),
	concat_atom(['D',PMID],DPMID),
	concat_atom([DPMID,'E'],'.',TempEntityID),
	atom_codes(TempEntityID,TempEntityIDString),
	append([TempEntityIDString,IndexString],EntityID),
	( CUI == '' ->
	  UMLSAttributeList = []
	; append([[cui=CUIString, name=MetaConcString]],UMLSAttributeList)
	),
	( NormGeneID == '' ->
	  EntrezAttributeList = []
	; append([[entrezID=NormGeneIDString, entrezName=NormGeneNameString]],EntrezAttributeList)
	),
	append([[id=EntityID],UMLSAttributeList,[semtypes=SemTypeListString],EntrezAttributeList,
		[text=InputString,
		 score=ConfidenceString,
		 negated=NegationString,
		 begin=StartPosString,
		 end=EndPosString]],
	       EntityAttributeList),
	create_xml_element('Entity', EntityAttributeList, [], XML).

generate_predication_xml([], _, _, _,Scale,Scale,[]) :- !.
generate_predication_xml([FirstPredication|RestPredications], IndexIn, EntityIDs, Entities, ScaleIn, ScaleOut,
			 [FirstPredicationXML|RestPredicationsXML]) :-
	generate_one_predication_xml(FirstPredication,IndexIn,EntityIDs, Entities, ScaleIn, ScaleNext, FirstPredicationXML),
	IndexOut is IndexIn + 1,
        !,				       
	generate_predication_xml(RestPredications, IndexOut, EntityIDs, Entities, ScaleNext, ScaleOut, RestPredicationsXML).
generate_predication_xml([_FirstPredication|RestPredications], Index, EntityIDs, Entities, ScaleIn, ScaleOut,RestPredicationsXML) :-
	generate_predication_xml(RestPredications, Index, EntityIDs, Entities, ScaleIn, ScaleOut,RestPredicationsXML).

generate_one_predication_xml('2entity'-('SE'-PMID-_UtteranceType-_TiOrAb-_UtteranceID-entity-
				       _CUI-MetaConc-_SemTypeListAtom-
				       _NormGeneID-_NormGeneName-InputText-
				       _Change-_Degree-_Negation-
				       _Confidence-StartPos-EndPos),
			     Index, _EntityIDs, _Entities, ScaleIn, ScaleOut, XML) :-
	number_codes(Index,IndexString),
%	concat_atom([PMID,TiOrAb,UtteranceID,'S'],'.',TempScaleID0),
	concat_atom(['D',PMID],DPMID),
	concat_atom([DPMID,'S'],'.',TempScaleID),
	atom_codes(TempScaleID,TempScaleIDString),
	append([TempScaleIDString,IndexString],ScaleID),
	% remove << and >>
	midstring(MetaConc,MetaConc0,'<<>>',2,_,2),
	atom_codes(MetaConc0,MetaConcString),
	atom_codes(InputText,InputString),
	number_codes(StartPos,StartPosString),
	number_codes(EndPos,EndPosString),
	append(ScaleIn,[ScaleID],ScaleOut),
	create_xml_element('Scale',
			   [id=ScaleID, name=MetaConcString, text=InputString,
			    begin=StartPosString, end=EndPosString],
			   [],
			   XML).
generate_one_predication_xml('3relation'-('SE'-PMID-_UtteranceType-_TiOrAb-_SentenceID-coreference-
					 SubjectCUI-SubjectMetaConc-_SubjectSemTypeListAtom-
					 SubjectNormGeneID-SubjectNormGeneName-SubjectAtom-
					 _SubjectChange-_SubjectDegree-_SubjectNegation-
					 SubjectConfidenceScore-SubjectStartPos-SubjectEndPos-
					 'COREF'-
					 ObjectCUI-ObjectMetaConc-_ObjectSemTypeListAtom-
					 ObjectNormGeneID-ObjectNormGeneName-ObjectAtom-
					 _ObjectChange-_ObjectDegree-_ObjectNegation-
					 ObjectConfidenceScore-ObjectStartPos-ObjectEndPos),
			      Index,EntityIDs,Entities,ScaleIn,ScaleOut,XML):-
	( get_entity_id(SubjectCUI-SubjectMetaConc-
		      SubjectNormGeneID-SubjectNormGeneName-SubjectAtom-
		      SubjectConfidenceScore-SubjectStartPos-SubjectEndPos,
		      EntityIDs, Entities, SubjectID)
	; get_entity_id_partial(SubjectCUI-SubjectMetaConc-
		      SubjectNormGeneID-SubjectNormGeneName-SubjectAtom-
		      SubjectConfidenceScore-SubjectStartPos-SubjectEndPos,
		      EntityIDs, Entities, SubjectID)
	),
%	atom_codes(SubjectSemType,SubjectSemTypeString),
	create_xml_element('Anaphor',
			   [entityID=SubjectID],
			   [],
			   SubjectXML),
	%%% ( IndicatorType == 'MOD/HEAD' ->
	%%%    IndicatorTypeString = "MOD_HEAD"
	%%% ; atom_codes(IndicatorType,IndicatorTypeString)
	%%% ),
	%%% number_codes(RelationStartPos,RelationStartPosString),
	%%% number_codes(RelationEndPos,RelationEndPosString),

	number_codes(Index,IndexString),
	concat_atom(['D',PMID],DPMID),
	concat_atom([DPMID,'C'],'.',TempCorefID),
	atom_codes(TempCorefID,TempCorefIDString),
	append([TempCorefIDString,IndexString],CorefID),
	AttributeList = [id=CorefID],
	%%% ( RelationNegation == negation ->
	%%%   NegAttributeList = [id=PredicationID,negated="true"]
	%%% ; NegAttributeList = [id=PredicationID]
	%%% ),
	%%% ( midstring(Relation,CoreRelation,'(SPEC)', 0,_,6) ->
	%%%   append([NegAttributeList,[inferred="true"]],PredicationAttributeList)
	%%% ; midstring(Relation,CoreRelation,'(INFER)', 0,_,7) ->
	%%%   append([NegAttributeList,[inferred="true"]],PredicationAttributeList)
	%%% ; PredicationAttributeList = NegAttributeList,
	%%%   CoreRelation = Relation
	%%% ),
	%%% atom_codes(CoreRelation,RelationString),
	%%% ( member(CoreRelation,[higher_than,lower_than,same_as]),
	%%%   ScaleIn \== [],
	%%%   last(ScaleIn,ScaleID),
	%%%   PredicateAttributeList = [type=RelationString, indicatorType=IndicatorTypeString,
	%%% 			    begin=RelationStartPosString, end=RelationEndPosString,
	%%% 			    scale=ScaleID]
	%%% ; PredicateAttributeList = [type=RelationString, indicatorType=IndicatorTypeString,
	%%% 			    begin=RelationStartPosString, end=RelationEndPosString]
	%%% ),
	%%% create_xml_element('Predicate', PredicateAttributeList,[],PredicateXML),
	ScaleOut = ScaleIn,

	get_entity_id(ObjectCUI-ObjectMetaConc-
		      ObjectNormGeneID-ObjectNormGeneName-ObjectAtom-
		      ObjectConfidenceScore-ObjectStartPos-ObjectEndPos,
		      EntityIDs, Entities, ObjectID),
%	number_codes(ObjectMaxDist,ObjectMaxDistString),
%	number_codes(ObjectDist,ObjectDistString),
%	atom_codes(ObjectSemType,ObjectSemTypeString),
	create_xml_element('Antecedent',[entityID=ObjectID],[],ObjectXML),
	create_xml_element('Coreference',AttributeList,[SubjectXML, ObjectXML],XML).
generate_one_predication_xml('3relation'-
			    ('SE'-PMID-_UtteranceType-_TiOrAb-_UtteranceID-relation-
			    SubjectMaxDist-SubjectDist-
			    SubjectCUI-SubjectMetaConc-
			    _SubjectSemTypeListAtom-SubjectSemType-
			    SubjectNormGeneID-SubjectNormGeneName-SubjectAtom-
			    _SubjectChange-_SubjectDegree-_SubjectNegation-
			    SubjectConfidenceScore-SubjectStartPos-SubjectEndPos-
			    IndicatorType-Relation-RelationNegation-
			    RelationStartPos-RelationEndPos-
			    ObjectMaxDist-ObjectDist-
			    ObjectCUI-ObjectMetaConc-
			    _ObjectSemTypeListAtom-ObjectSemType-
			    ObjectNormGeneID-ObjectNormGeneName-ObjectAtom-
			    _ObjectChange-_ObjectDegree-_ObjectNegation-
			    ObjectConfidenceScore-ObjectStartPos-ObjectEndPos),
			    Index,EntityIDs, Entities, ScaleIn, ScaleOut, XML) :-
	get_entity_id(SubjectCUI-SubjectMetaConc-
		      SubjectNormGeneID-SubjectNormGeneName-SubjectAtom-
		      SubjectConfidenceScore-SubjectStartPos-SubjectEndPos,
		      EntityIDs, Entities, SubjectID),
	number_codes(SubjectMaxDist,SubjectMaxDistString),
	number_codes(SubjectDist,SubjectDistString),
	atom_codes(SubjectSemType,SubjectSemTypeString),
	create_xml_element('Subject',
			   [maxDist=SubjectMaxDistString, dist=SubjectDistString,
			    entityID=SubjectID, relSemType=SubjectSemTypeString],
			   [],
			   SubjectXML),
	( IndicatorType == 'MOD/HEAD' ->
	   IndicatorTypeString = "MOD_HEAD"
	; atom_codes(IndicatorType,IndicatorTypeString)
	),
	number_codes(RelationStartPos,RelationStartPosString),
	number_codes(RelationEndPos,RelationEndPosString),

	number_codes(Index,IndexString),
%	concat_atom([PMID,TiOrAb,UtteranceID,'P'],'.',TempPredicationID0),
	concat_atom(['D',PMID],DPMID),
	concat_atom([DPMID,'P'],'.',TempPredicationID),
	atom_codes(TempPredicationID,TempPredicationIDString),
	append([TempPredicationIDString,IndexString],PredicationID),
	( RelationNegation == negation ->
	  NegAttributeList = [id=PredicationID,negated="true"]
	; NegAttributeList = [id=PredicationID,negated="false"]
	),
	( midstring(Relation,CoreRelation,'(SPEC)', 0,_,6) ->
	  append([NegAttributeList,[inferred="true"]],PredicationAttributeList)
	; midstring(Relation,CoreRelation,'(INFER)', 0,_,7) ->
	  append([NegAttributeList,[inferred="true"]],PredicationAttributeList)
	; append([NegAttributeList,[inferred="false"]],PredicationAttributeList),
	  CoreRelation = Relation
	),
	atom_codes(CoreRelation,RelationString),
	( member(CoreRelation,[higher_than,lower_than,same_as]),
	  ScaleIn \== [],
	  last(ScaleIn,ScaleID),
	  PredicateAttributeList = [type=RelationString, indicatorType=IndicatorTypeString,
				    begin=RelationStartPosString, end=RelationEndPosString,
				    scale=ScaleID]
	; PredicateAttributeList = [type=RelationString, indicatorType=IndicatorTypeString,
				    begin=RelationStartPosString, end=RelationEndPosString]
	),
	create_xml_element('Predicate', PredicateAttributeList,[],PredicateXML),
	ScaleOut = ScaleIn,

	get_entity_id(ObjectCUI-ObjectMetaConc-
		      ObjectNormGeneID-ObjectNormGeneName-ObjectAtom-
		      ObjectConfidenceScore-ObjectStartPos-ObjectEndPos,
		      EntityIDs, Entities, ObjectID),
	number_codes(ObjectMaxDist,ObjectMaxDistString),
	number_codes(ObjectDist,ObjectDistString),
	atom_codes(ObjectSemType,ObjectSemTypeString),
	create_xml_element('Object',
			   [maxDist=ObjectMaxDistString, dist=ObjectDistString,
			    entityID=ObjectID, relSemType=ObjectSemTypeString],
			   [],
			   ObjectXML),
	create_xml_element('Predication',
			   PredicationAttributeList,
			   [SubjectXML, PredicateXML, ObjectXML],
			   XML).


get_entity_id(CUI-MetaConc-NormGeneID-NormGeneName-Text-ConfidenceScore-StartPos-EndPos,
	      [ID|_RestEntityIDs], [Entity|_RestEntities], ID):-
	Entity = '2entity'-('SE'-_PMID-_UtteranceType-_TiOrAb-_UtteranceID-entity-
			   CUI-MetaConc-_SemtypeListAtom-NormGeneID-NormGeneName-Text-
			   _Change-_Degree-_Negation-ConfidenceScore-StartPos-EndPos),
	!.
get_entity_id(Entity, [_ID|RestEntityIDs], [_Entity|RestEntities], EntityID) :-
	get_entity_id(Entity, RestEntityIDs, RestEntities, EntityID).

get_entity_id_partial(CUI-MetaConc-NormGeneID-NormGeneName-_Text-ConfidenceScore-StartPos-EndPos,
	      [ID|_RestEntityIDs], [Entity|_RestEntities], ID):-
	Entity = '2entity'-('SE'-_PMID-_UtteranceType-_TiOrAb-_UtteranceID-entity-
			   CUI-MetaConc-_SemtypeListAtom-NormGeneID-NormGeneName-_EntityText-
			   _Change-_Degree-_Negation-ConfidenceScore-EntityStartPos-EntityEndPos),
	overlap(StartPos,EndPos,EntityStartPos,EntityEndPos),
	!.
get_entity_id_partial(Entity, [_ID|RestEntityIDs], [_Entity|RestEntities], EntityID) :-
	get_entity_id_partial(Entity, RestEntityIDs, RestEntities, EntityID).
	
overlap(StartA,EndA,StartB,EndB) :-
	ALen is EndA - StartA,
	BLen is EndB - StartB,
	( StartA >= StartB, EndB > StartA 
	; EndA > StartB, EndB >= EndA
	; ALen > BLen, StartB >= StartA, EndA >= EndB
	).
	
conditionally_print_CR(Format, OutputStream) :-
	( Format == 'XMLf1' ->
	  format(OutputStream, "~n", []),
	  flush_output(OutputStream)
	; Format == 'XMLf' ->
	  format(OutputStream, "~n", []),
	  flush_output(OutputStream)
	; true
	).


create_xml_element(ElementName, AttributeList, Components, XMLStructure) :-
	XMLStructure = element(ElementName, AttributeList, Components).

conditionally_print_xml_header(PrintSetting, OutputStream) :-
	( PrintSetting =:= 1 ->
	  environ('XML_VERSION',XMLVersion),
	  environ('SEMREP_XML_DOCTYPE', XMLDocType),
	  environ('SEMREP_XML_DOCNAME', XMLDocName),
	  environ('SEMREP_XML_DTD', XMLDTD),
	  concat_atom(['<!', XMLDocType, ' "', XMLDocName, '" "', XMLDTD, '">'], DocType),
	  format(OutputStream, '~w~n~w', [XMLVersion,DocType]),
	  format(OutputStream, '~n<SemRepAnnotation>', []),
	  flush_output(OutputStream)
	; true
	).

conditionally_print_xml_footer(PrintSetting, XMLSetting, OutputStream) :-
	( PrintSetting =:= 1 ->
	  conditionally_print_CR(XMLSetting, OutputStream),
	  format(OutputStream, '</SemRepAnnotation>~n', []),
	  flush_output(OutputStream)
	; true
	).

%xml_header_footer_print_setting(InnerOrOuter, XMLFormat, PrintSetting) :-
%( xml_output_format(XMLFormat),
%	  get_xml_format_mode(XMLFormat, FormatMode, _TrueOrFalse) ->
%	  % This is bitwise XOR
%	  PrintSetting is \(InnerOrOuter, FormatMode)
%	; PrintSetting is 0
%	).

% Why can we use XOR here?
% Let's assume XML is on -- otherwise this is all irrelevant anyway.
% If we're at the outer header/footer (InnerOrOuter =:= 1)
% we want to print the header iff the format mode is 0 (XMLf1/XMLn1).
% If we're at the inner header/footer (InnerOrOuter =:= 0)
% we want to print the header iff the format mode is 1 (XMLf/XMLn).

% The overall structure of MetaMap XML output is the following.
% Note that there will be exactly one of inner and outer headers/footers.
% If the user has requested one XML document for the entire input file
% (XMLf1/XMLn1), only the outer header and footer will be printed.
% If the user has requested multiple XML documents, i.e., one per citation
% (format/noformat), only the inner headers and footers will be printed.
% 
% outer XML header                     (for XMLf1/XMLn1 only)
% 
%     inner XML header                 (for XMLf/XMLn only)
%     inner XML footer                 (for XMLf/XMLn only)
% 
%     inner XML header                 (for XMLf/XMLn only)
%     inner XML footer                 (for XMLf/XMLn only)
% 
%     inner XML header                 (for XMLf/XMLn only)
%     inner XML footer                 (for XMLf/XMLn only)
% 
% outer XML footer                     (for XMLf1/XMLn1 only)
% 


% get_xml_format_mode(XMLFormatMode, OneOrZero, TrueOrFalse)
% OneOrZero controls the printing of the inner header/footer
% TrueOrFalse controls whether XML output is formatted (true) or unformatted (false)
%get_xml_format_mode('XMLf1', 0, true).
%get_xml_format_mode('XMLn1', 0, false).
%get_xml_format_mode('XMLf',  1, true).
%get_xml_format_mode('XMLn',  1, false).

%xml_output_format(XMLFormat) :-
%	( control_option('XMLf')  ->
%	  XMLFormat = 'XMLf'
%	; control_option('XMLf1') ->
%	  XMLFormat = 'XMLf1'
%	; control_option('XMLn')  ->
%	  XMLFormat = 'XMLn'
%	; control_option('XMLn1') ->
%	  XMLFormat = 'XMLn1'
%	).

% :- use_module(library(addportray)).
% portray_mm_output(candidates(_)) :- write('CANDIDATES').
% portray_mm_output(mappings(_)) :- write('MAPPINGS').
% portray_mm_output(phrase(_,_,_,_)) :- write('PHRASE').
% :- add_portray(portray_mm_output).
  
% doit :-
% 	open(xml_call, read, Stream),
% 	read(Stream, Term),
% 	close(Stream),
% 	call(Term).
