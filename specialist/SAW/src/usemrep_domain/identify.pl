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

% File:	    identify.pl
% Module:   identify
% Author:   tcr, NLS
% Purpose:  Code to Identify Genetic/Disease Concepts

% ----- Module declaration and exported predicates

:- module( identify, [
                identify_genetic_concepts/4
   ]).

:- use_module( skr_lib(nls_lists), [
		get_from_list/3
   ]).

:- use_module( skr_lib( nls_strings ), [
	        split_string_completely/3,
		split_string/4,
		trim_whitespace_right/2
   ]).

:- use_module( usemrep_domain(semantic_groups), [
		semantic_group/3
   ]).

:- use_module( usemrep_lib(ssuppserv), [
		get_sem_info/4,
		locate_npu_head/3,
		min_max/4,
		non_lexical_status/1,
		strings_to_atoms/2
   ]).

%:- use_module( usemrep_lib(edgarsupport), [
%	        get_from_list/3
%   ]).

%:- use_module( usemrep_lib(edgarnonlex), [
%		non_lexical_status/1
%   ]).

:- use_module( usemrep_lib(entrezgene), [
		retrieve_gene_symbol/2
   ]).

:- use_module( skr_lib(ctypes), [
		is_upper/1,
		is_digit/1
   ]).


:- use_module( library(lists), [
	        append/2,
	        is_list/1,
	        last/2,
	        rev/2,
		nth0/3,
		nth0/4,
		segment/2,
		selectchk/3
   ]).

:- use_module( library(sets), [
		add_element/3,
	        intersect/2,
		intersection/3,
%		selectchk/3,
		subset/2,
		subtract/3,
		union/3
   ]).

:- use_module( skr_lib(sicstus_utils), [
	        concat_atom/2,
		concat_atom/3,
		lower/2,
		midstring/5,
		midstring/6,
		substring/4
   ]).

/* 
% ************************* Identify Genetic Concepts ******************************
Metamap rearranges the analysis under certain circumstances (especially with hyphens).
Thus the token_lists are generated from the syntactic analysis (before metamap)
and sent down here as MSUList

The MetaMap and Semtype list is SemtypeList.

Need to use semantic groups for disorders: disorder

May need to expand semantic types for genetic phenomenon: genphenom

adjacent NPs with genphenom are coalesced in a macroNP

<Halil>Renamed identify_genetic_and_disease_concepts to identify_genetic_concepts.
The goal is to remove SemGen-only features and simplify the processing.
Also remove macroNP coalescence code, since this should be handled by the
empty head machinery. </Halil>

*/
identify_genetic_concepts(Domain,SemanticsParse, ABGenes, 
			  minimal_syntax(SemanticsParseWithGenPhenoms)) :-
	semantic_group(Domain, gene, GeneticsSemTypes),
	ident_gene_concepts(SemanticsParse, ABGenes, GeneticsSemTypes,
			    SemanticsParseWithGenPhenoms, []).

ident_gene_concepts([],_ABGenes,_GeneticsSemTypes,
	            SemanticsParseWithGenPhenoms,SemanticsParseWithGenPhenoms).
ident_gene_concepts([ThisSemMSU|MoreSem],ABGenes, GeneticsSemTypes,
		    SemParseWiGenPhenomsOut, SemParseWiGenPhenomsIn) :-
	glom_multi_element_genes_in_MSU(ThisSemMSU, GlommedSemMSU),
	ident_gene_concepts_1(GlommedSemMSU, ABGenes, GeneticsSemTypes,
	           SemParseWithGenPhenoms,[]),
        SemParseWiGenPhenomsOut = [SemParseWithGenPhenoms|SemParseWiGenPhenomsNext],
        ident_gene_concepts(MoreSem, ABGenes, GeneticsSemTypes,
	           SemParseWiGenPhenomsNext, SemParseWiGenPhenomsIn).

ident_gene_concepts_1([], _ABGenes, _GeneticsSemTypes,
	              SemParseWithGenPhenoms, SemParseWithGenPhenoms) :- !.
ident_gene_concepts_1([MSUElement|RestMSUElement], ABGenes,GeneticsSemTypes,
	              SemParseWithGenPhenomsOut,SemParseWithGenPhenomsIn):-
	ident_gene_concepts_2(MSUElement,ABGenes,GeneticsSemTypes,MSUElementOut),
        !,		      
        SemParseWithGenPhenomsOut = [MSUElementOut|SemParseWithGenPhenomsNext],
        ident_gene_concepts_1(RestMSUElement, ABGenes, GeneticsSemTypes, 
	                      SemParseWithGenPhenomsNext, SemParseWithGenPhenomsIn).
ident_gene_concepts_1([MSUElement|RestMSUElement],ABGenes,GeneticsSemTypes,
	              SemParseWithGenPhenomsOut, SemParseWithGenPhenomsIn) :-
	SemParseWithGenPhenomsOut = [MSUElement|SemParseWithGenPhenomsNext],
	ident_gene_concepts_1(RestMSUElement, ABGenes, GeneticsSemTypes, 
	                      SemParseWithGenPhenomsNext, SemParseWithGenPhenomsIn). 

ident_gene_concepts_2(MSUElement, ABGenes, GeneticsSemTypes, MSUElementOut) :-
	may_have_genphenom(MSUElement,ElementType, ItemList),
	genphenom_semtype_test(ItemList, GeneticsSemTypes, MetaConc, SemTypeList),
	create_genphenom_term(ABGenes, ItemList, MetaConc, SemTypeList, GenPhenomTerm),
	!,
	modify_item(ItemList, ElementType, GenPhenomTerm, MSUElementOut).
ident_gene_concepts_2(MSUElement, ABGenes, _GeneticsSemTypes, MSUElementOut) :-
	functor(MSUElement, ElementType, 1),
	arg(1,MSUElement, ItemList),
	no_metaconc_item(ItemList),
	memberchk(tokens(ModifiedTokenList), ItemList),
	memberchk(index(Index),ItemList),
	concat_atom(ModifiedTokenList, ModifiedTokenAtom),
	ModifiedTokens = [ModifiedTokenAtom],
	remove_punc(ModifiedTokenList, ItemTokenList),
	concat_atom(ItemTokenList, ItemTokenAtom),
	ItemTokens = [ItemTokenAtom],
	% Test for genphenom not involving semtypes
	genphenom_non_semtype_test(ABGenes, ItemTokens),
	% create synthetic genphenom
	mangle_genphenom_format(ModifiedTokens, ItemTokens, GenPhenomAtom),
	get_gene_name(GenPhenomAtom, ABGenes, GeneName),	
	create_enhanced_genphenom_term(GeneName, ABGenes, Index, Index,
				       '', [gngm], GenPhenomTerm),
        !,				       
        modify_item(ItemList, ElementType, GenPhenomTerm, MSUElementOut).	
ident_gene_concepts_2(MSUElement, _ABGenes, _GeneticsSemTypes, MSUElement).

modify_item(ItemList, ElementType, Term, ItemListOut) :-
	Term = genphenom([EntrezIDList,EntrezNameList,_]),
	concat_atom(EntrezNameList, EntrezNameAtom),
	EntrezNameAtom \== 'None',
	!,
	concat_atom(EntrezIDList, EntrezIDAtom),
	modify_item_1(ItemList, EntrezIDAtom, EntrezNameAtom, NewItemList),
	append(NewItemList, [Term], NewItemList1),
	functor(ItemListOut, ElementType, 1),
	arg(1, ItemListOut, NewItemList1).	    
modify_item(ItemList, ElementType, Term, ItemListOut) :-
	append(ItemList, [Term], NewItemList),
	functor(ItemListOut, ElementType, 1),
	arg(1, ItemListOut, NewItemList).

modify_item_1([],_,_,[]):- !.
modify_item_1(ItemList, EntrezIDAtom, EntrezNameAtom, NewItemList) :-
	no_metaconc_item(ItemList),
	MetaConc = metaconc([EntrezNameAtom:EntrezIDAtom: [gngm] ]),
	USemType = usemtype(['Gene or Genome']),
	AUSemType = ausemtype([gngm]),
	SemGroup = semgroup([gene]),
	modify_item_2(ItemList, EntrezIDAtom, EntrezNameAtom, NewItemList0),
	append(NewItemList0, [USemType], NewItemList1),
	append(NewItemList1, [AUSemType], NewItemList2),
	append(NewItemList2, [SemGroup], NewItemList3),
	append(NewItemList3, [MetaConc], NewItemList).
modify_item_1(ItemList, EntrezIDAtom, EntrezNameAtom, NewItemList) :-
	modify_item_2(ItemList, EntrezIDAtom, EntrezNameAtom, NewItemList).
	
modify_item_2([],_,_,[]):-!.
modify_item_2([metaconc(MetaConcList)|More], EntrezIDAtom, EntrezNameAtom, 
	      [metaconc(NewMetaConcList)|MoreOut]):-
	union(MetaConcList, [EntrezNameAtom:EntrezIDAtom: [gngm] ], NewMetaConcList),
	modify_item_2(More, EntrezIDAtom, EntrezNameAtom, MoreOut).
modify_item_2([usemtype(USemTypeList)|More], EntrezIDAtom, EntrezNameAtom, 
	      [usemtype(NewUSemTypeList)|MoreOut]):-
	union(USemTypeList, ['Gene or Genome'], NewUSemTypeList),
	modify_item_2(More, EntrezIDAtom, EntrezNameAtom, MoreOut).	
modify_item_2([ausemtype(AUSemTypeList)|More], EntrezIDAtom, EntrezNameAtom, 
	      [ausemtype(NewAUSemTypeList)|MoreOut]):-
	union(AUSemTypeList, [gngm], NewAUSemTypeList),
	modify_item_2(More, EntrezIDAtom, EntrezNameAtom, MoreOut).
modify_item_2([semgroup(SemGroupList)|More], EntrezIDAtom, EntrezNameAtom, 
	      [semgroup(NewSemGroupList)|MoreOut]):-
	union(SemGroupList, [gene], NewSemGroupList),
	modify_item_2(More, EntrezIDAtom, EntrezNameAtom, MoreOut).
modify_item_2([Item|More], EntrezIDAtom, EntrezNameAtom, [Item|MoreOut]) :-
	modify_item_2(More, EntrezIDAtom, EntrezNameAtom, MoreOut).

may_have_genphenom(head(Elements),head, Elements).
may_have_genphenom(mod(Elements),mod, Elements).
may_have_genphenom(shapes(Elements), shapes, Elements).
may_have_genphenom(not_in_lex(Elements), not_in_lex, Elements).


% If we're running SemRep, the MSU *must* contain an msu_token_list term
% require_msu_token_list(Domain, ThisSyntMSU) :-
% 	( Domain == generic ->
% 	  memberchk(msu_token_list(_MinIndex, _MaxIndex, _ModifiedTokenList), ThisSyntMSU)
% 	; true
% 	).

%%% Keep the macro genphenoms and those genphenoms
%%% that did not get combined into a macro genphenom
% keep_genphenoms(macro,
% 		_OrigGenPhenoms, _OrigGenPhenomsTail,
% 		MacroGenPhenoms, MacroGenPhenomsTail,
% 		Disorders,
% 		GenPhenomAndDisorderList) :-
% 	GenPhenomAndDisorderList = MacroGenPhenoms,
% 	MacroGenPhenomsTail = Disorders.
% 
% %%% Keep the original genphenoms; just ignore results of coalesce_macro_NPs
% keep_genphenoms(micro,
% 		OrigGenPhenoms, OrigGenPhenomsTail,
% 		_MacroGenPhenoms, _MacroGenPhenomsTail,
% 		Disorders,
% 		GenPhenomAndDisorderList) :-
% 	GenPhenomAndDisorderList = OrigGenPhenoms,
% 	OrigGenPhenomsTail = Disorders.

% collect_gendis_terms(disorder(A,B,C,D,E),
% 		     GenPhenoms, GenPhenoms,
% 		     [disorder(A,B,C,D,E)|DisordersOut], DisordersOut).
% collect_gendis_terms(genphenom(A,B,C,D,E),
% 		     [genphenom(A,B,C,D,E)|GenPhenomsOut], GenPhenomsOut,
% 		     Disorders, Disorders).

% In SemRep, require that an MSU contain a head that does NOT have a MetaConc
% FML 09/05/2006 Tuesday @ 18:17:29
% Halil requested calling entrezgene even if there IS a MetaThesaurus concept

/*
NEW:
*/

% get_head_itemlist(_Domain, ThisSemMSU, HeadType, ItemList) :-
% 	% ( Domain == generic ->
% 	%   get_from_list(head, ThisSemMSU, HeadList),
% 	%   \+ get_from_list(metaconc, HeadList, _MC)
% 	( locate_npu_head([ThisSemMSU], HeadType, ItemList)
% 	; get_from_list(not_in_lex, ThisSemMSU, ItemList)
% 	).
% 

/*
OLD:


get_head_itemlist(Domain, ThisSemMSU, HeadType, ItemList) :-
	 ( Domain == generic ->
	   get_from_list(head, ThisSemMSU, HeadList),
	   \+ get_from_list(metaconc, HeadList, _MC)
	; locate_npu_head([ThisSemMSU], HeadType, ItemList) ->
	  true
        ; get_from_list(not_in_lex, ThisSemMSU, ItemList)
        ).
*/

no_metaconc_item(MSUElement) :-
	\+ get_from_list(metaconc, MSUElement, _MetaConc).

% Succeed only if Domain \== generic, or if ThisSemMSU has no metaconc.
% PSG revised to Domain == genetics, or ThisSemMSU has no metaconc.
% no_metaconc_in_MSU(Domain, ThisSemMSU) :-
% 	Domain == genetics ;
% 	no_metaconc_in_MSU_1(ThisSemMSU).
% 
% no_metaconc_in_MSU_1([]).
% no_metaconc_in_MSU_1([MSUElement|RestMSUElements]) :-
% 	arg(1, MSUElement, ItemList),    % extract first member of MSUElement
% 	\+ get_from_list(metaconc, ItemList, _MetaConc), % it has no metaconc
% 	no_metaconc_in_MSU_1(RestMSUElements).

% <Halil> This might still need to change.
% If there is a metaconc in the form of 'X gene or X protein, human', 
% X could be  a gene symbol. This can be expanded possibly.
% Use it to retrieve an EntrezGene ID.
create_genphenom_term(ABGenes,ItemList,MetaConc,SemTypeList, GenPhenomTerm) :-	
	(midstring(MetaConc,GeneName,' gene',0,_,5),
	 memberchk(gngm,SemTypeList);
	 midstring(MetaConc,GeneName,' protein, human',0,_,15),
	 intersect([aapp,bacs,enzy,imft,rcpt],SemTypeList)
        ),
	memberchk(index(Index),ItemList),
	create_enhanced_genphenom_term(GeneName, ABGenes, Index, Index,
	    MetaConc, SemTypeList, GenPhenomTerm).
create_genphenom_term(ABGenes,ItemList,MetaConc,SemTypeList, GenPhenomTerm) :-
	memberchk(tokens(ModifiedTokenList), ItemList),
	memberchk(index(Index),ItemList),
	remove_punc(ModifiedTokenList, ModifiedTokenListNoPunc),
	mangle_genphenom_format(ModifiedTokenList, ModifiedTokenListNoPunc, GenPhenomAtom),
	get_gene_name(GenPhenomAtom, ABGenes, GeneName),
	create_enhanced_genphenom_term(GeneName, ABGenes, Index, Index,
	    MetaConc, SemTypeList, GenPhenomTerm).
	
create_enhanced_genphenom_term(GeneName, _ABGenes, _MinIndex, _MaxIndex,
                               _MetaConc, _SemTypeList, GenPhenomTerm) :-
	%format('~n######## CHECKING Entrezgene for ~w~n', [GeneName]),
	check_entrezgene(GeneName, NormGeneID, NormGeneName, _GeneFunctionList),
	GenPhenomTerm = genphenom([[NormGeneID],[NormGeneName],[gngm]]).

genphenom_semtype_test(ItemList, GeneticsSemTypes,
	               MetaConc, SemTypeList) :-
	get_sem_info(ItemList, [MetaConc], _CUI, SemTypeList),
	intersect(SemTypeList, GeneticsSemTypes).

% <Halil> From here on, we might need more refinement and cleanup.</Halil>
genphenom_non_semtype_test(ABGenes, MSUTokenList) :-
	( compare_abgenes(ABGenes, MSUTokenList)
	;	% Any 1 token in MSUTokenList satisfies one of 5 protein shape rules
		shape_rules(MSUTokenList, _ShapeProt)
	;	% Check that MSUTokenList contains eg allele, chromosomal, exon ...
		member(Element, MSUTokenList),
		genetics_signal(Element)
	).

mangle_genphenom_format(MSUTokenList0, MSUTokenList, GenPhenomAtom) :-
	( check_for_period(MSUTokenList0, MSUTokenList, MSUTokenListWithPeriod) ->
          concat_atom_with_period(MSUTokenListWithPeriod, ' ', [], GenPhenomStr),
	  trim_whitespace_right(GenPhenomStr, TrimmedGenStr),
	  atom_codes(GenPhenomAtom, TrimmedGenStr)
        ; concat_atom(MSUTokenList, ' ', GenPhenomAtom)
        ).

% checks for periods in MSUTokenList
check_for_period(MSUTokenList, MSUTokenList, _) :- !, fail.
check_for_period(MSUTokenList, MSUTokenListNoPunc, MSUTokenListWithPeriod) :-
	memberchk('.', MSUTokenList),
	nth0(Index, MSUTokenList, '.'),
	nth0(Index, MSUTokenListWithPeriod, '.', MSUTokenListNoPunc).

% handles the periods in gene symbols. 
concat_atom_with_period([], _, GenList, GenList).
concat_atom_with_period(['.'|More], Separator, GenListIn, GenListOut) :-
	atom_codes(Separator, SeparatorStr),
	append(GenAtomNoSpaceStr, SeparatorStr, GenListIn),
	!,
	append(GenAtomNoSpaceStr, ".", GenAtom2Str),
	TempGenListIn = GenAtom2Str,
	concat_atom_with_period(More, Separator, TempGenListIn, GenListOut).
concat_atom_with_period([Other|More], Separator, GenListIn, GenListOut) :-
	atom_codes(Other, OtherStr),
	atom_codes(Separator, SeparatorStr),
	append(OtherStr, SeparatorStr, OtherStrWithSpace),
	append(GenListIn, OtherStrWithSpace, GenAtom2Str),
	TempGenListIn = GenAtom2Str,
	concat_atom_with_period(More, Separator, TempGenListIn, GenListOut).

gene_np_signal(activated).
gene_np_signal(expression).
gene_np_signal(expressions).
gene_np_signal(gene).
gene_np_signal(genes).
gene_np_signal(mutant).
gene_np_signal(mutants).
gene_np_signal(mutated).
gene_np_signal(mutation).
gene_np_signal(mutations).
gene_np_signal(overexpression).
gene_np_signal('suppressor gene').
gene_np_signal('suppressor genes').
gene_np_signal(oncogene).
gene_np_signal(oncogenes).
gene_np_signal(protein).
gene_np_signal(proteins).
gene_np_signal(underexpression).

% ----- Genetic Phenomenon Signal Words


genetics_signal(amino).
genetics_signal(allele).
genetics_signal(chromosomal).
genetics_signal(chromosome).
genetics_signal(codon).
genetics_signal(deletion).
genetics_signal(duplication).
genetics_signal(exon).
genetics_signal(gene).
genetics_signal(heterozygous).
genetics_signal(hemizygous).
genetics_signal(homozygous).
genetics_signal(intron).
genetics_signal(inversion).
genetics_signal(kinase).
genetics_signal(mutant).
genetics_signal(mutation).
genetics_signal(polymorphic).
genetics_signal(polymorphism).
genetics_signal(recessive).
genetics_signal(sequence).
genetics_signal(variant).
genetics_signal(nucleotide).
genetics_signal(dinucleotide).
genetics_signal(terminal).
genetics_signal(trinucleotide).
genetics_signal(oligonucleotide).
genetics_signal(polynucleotide).
genetics_signal(translocation).

% ----- Genetic Phenomenon Ignore Words

genetics_delete(non).
genetics_delete(mrna).
genetics_delete(cdna).
genetics_delete(dna).
genetics_delete(rna).
genetics_delete(et).
genetics_delete(al).

digit_name(one).
digit_name(two).
digit_name(three).
digit_name(four).
digit_name(five).
digit_name(six).
digit_name(seven).
digit_name(eight).
digit_name(nine).
digit_name(ten).
digit_name(first).
digit_name(second).
digit_name(third).
digit_name(fourth).
digit_name(fifth).
digit_name(sixth).
digit_name(seventh).
digit_name(eighth).
digit_name(ninth).
digit_name(tenth).

% --------- Remove Punctuation
% punc had been saved for other reasons; needs to be deleted here

remove_punc([], []).
remove_punc([Punc|More], Gap) :-
	( punc_to_remove(Punc) ->
	  remove_punc(More, Gap)
	; Gap = [Punc|RestNoPunc],
	  remove_punc(More, RestNoPunc)
        ).

punc_to_remove(',').
punc_to_remove('.').
punc_to_remove(':').
punc_to_remove(';').
% punc_to_remove('(').
% punc_to_remove(')').

% ---------- Compare ABGenes ----------

compare_abgenes([ABGeneAtomList|_More], ItemTokenList) :-
	lower_list(ABGeneAtomList, LowABGeneAtomList),
	compare_abgenes_1(LowABGeneAtomList, ItemTokenList).
compare_abgenes([_NoMatch|More], ItemTokenList) :-
	compare_abgenes(More, ItemTokenList).

compare_abgenes_1([],_) :- !, fail.
compare_abgenes_1([ABGeneAtom|_More],[ABGeneAtom]).
compare_abgenes_1([_NoMatch|More],ItemTokenList) :-
	compare_abgenes_1(More, ItemTokenList).
	
% ---
lower_list([], []).
lower_list([Word|More], [LowWord|Gap]) :-
	lower(Word, LowWord),
	lower_list(More, Gap).

% ---------- Coalesce Macro NPs

% If there are at least MSUs left in the MSU list,
% and the first two MSUs both contain genphenom(_) terms, e.g.,
% genphenom('hl-a-dr3/dq2') and genphenom(gad65), then
% (1) add the coalesced genphenom to beginning of the second MSU, and
% (2) delete the two original genphenoms.
% The coalesced genphenom is the concatenation of the two original
% genphenom atoms separated by a blank space, e.g.,
% genphenom('hl-a-dr3/dq2 gad65')

% MacroGenPhenomTerms is a list containing
% (1) All the macro GenPhenom terms and
% (2) All the original GenPhenom terms that were not
%     absorbed into a macro GenPhenom

% coalesce_macro_NPs([], _ABGenes, MacroGenPhenomTerms, MacroGenPhenomTerms, []).
% coalesce_macro_NPs([ThisMSU|MoreMSUs], ABGenes,
% 		   MacroGenPhenomTermsIn,    MacroGenPhenomTermsOut,
% 		   MacroMSUs) :-
% 	% We know that if an MSU list contains a GenPhenom term,
% 	% then the GenPhenom term will be the first element in the MSU list.
% 	GenPhenomTerm1 = genphenom(_,_,_,_,_),
% 	ThisMSU = [GenPhenomTerm1|ThisMSUWithoutGenPhenom],
% 	MoreMSUs = [NextMSU|RestMSUs],
% 	GenPhenomTerm2 = genphenom(_,_,_,_,_),
% 	NextMSU = [GenPhenomTerm2|NextMSUWithoutGenPhenom],
% 	!,
% 	% format('~n###############MSU 1: ~q~n', [ThisMSU]),
% 	% format('~n###############MSU 2: ~q~n', [NextMSU]),
% 	append(ThisMSUWithoutGenPhenom, NextMSUWithoutGenPhenom, CoalescedMSU),
% 	get_genphenom_components(GenPhenomTerm1, MinIndex1, MaxIndex1,
% 				 _List1, GenPhenomAtom1,
% 				 MetaConc1, SemTypeList1),
% 	get_genphenom_components(GenPhenomTerm2, MinIndex2, MaxIndex2,
% 				 _List2, GenPhenomAtom2,
% 				 MetaConc2, SemTypeList2),
% 	concat_atom([GenPhenomAtom1,GenPhenomAtom2], ',', MacroGenPhenomAtom),
% 	min_max(MinIndex1, MinIndex2, MacroMinIndex, _),
% 	min_max(MaxIndex1, MaxIndex2, _, MacroMaxIndex),
%         union_semtype_lists(SemTypeList1, SemTypeList2, MacroSemTypeList),
% 	create_macro_metaconc(MetaConc1, MetaConc2, MacroMetaConc),
% 	create_enhanced_genphenom_term(MacroGenPhenomAtom, ABGenes,
% 				       MacroMinIndex, MacroMaxIndex,
% 				       MacroMetaConc, MacroSemTypeList,
% 				       MacroGenPhenomTerm),
% 
% 	% format('~n~n### Coalesced1: ~q~n', [GenPhenomTerm1]),
% 	% format('### Coalesced2: ~q~n',     [GenPhenomTerm2]),
% 	% format('### MACRO: ~q~n~n',        [MacroGenPhenomTerm]),
% 
% 	MacroNP = [MacroGenPhenomTerm|CoalescedMSU],
% 	coalesce_macro_NPs([MacroNP|RestMSUs], ABGenes,
% 			   MacroGenPhenomTermsIn, MacroGenPhenomTermsOut,
% 			   MacroMSUs).
% coalesce_macro_NPs([ThisMSU|RestMSUs], ABGenes,
% 		   MacroGenPhenomTermsIn, MacroGenPhenomTermsOut,
% 		   [ThisMSU|RestMacroMSUs]) :-
% 	ThisMSU = [H|_T],
% 	collect_genphenoms(H, MacroGenPhenomTermsIn, MacroGenPhenomTermsNext),
% 	coalesce_macro_NPs(RestMSUs, ABGenes,
% 			   MacroGenPhenomTermsNext, MacroGenPhenomTermsOut,
% 			   RestMacroMSUs).
% 
% collect_genphenoms(genphenom(A,B,C,D,E),
% 		   [genphenom(A,B,C,D,E)|RestMacroGenPhenomTerms],
% 		   RestMacroGenPhenomTerms) :-
% 	!.
% collect_genphenoms(_NonGenPhenom,
% 		   MacroGenPhenomTerms, MacroGenPhenomTerms).

/*

  To coalesce two MetaConcs or GenPhenomAtoms there are several possible cases.
  Each MetaConc can be
  (1) '',
  (2) [metaconc1, metaconc2], or
  (3) metaconc (atomic)

  Case 1: If either MetaConc is '', then just unify MacroMetaConc with the other MetaConc
  Case 2: If both MetaConcs are lists, then union them to get MacroMetaConc
  Case 3: If exactly one MetaConc is a list
	  (neither can be [], because that case was caught in Case 1),
  	  then insert the other into the list to get MacroMetaConc
  Case 4: Both MetaConcs are atomic, so MacroMetaConc is a list of the two 

  We must also ensure that the resulting MacroMetaConc does not contain duplicates.
  Finally, ensure that the MacroMetaConc or MacroGenPhenom is an atom.
*/

% create_macro_metaconc(MetaConc1, MetaConc2, MacroMetaConc) :-
% 	( MetaConc1 == '' ->         % case 1a
% 	  MacroMetaConc = MetaConc2
% 	; MetaConc2 == '' ->         % case 1b
% 	  MacroMetaConc = MetaConc1
% 	; is_list(MetaConc1),
% 	  is_list(MetaConc2) ->      % case 2
% 	  union(MetaConc1, MetaConc2, MacroMetaConc)
% 	; is_list(MetaConc1) ->      % case 3a
% 	  % \+ is_list(MetaConc2)
% 	  add_element(MetaConc2, MetaConc1, MacroMetaConc)
% 	; is_list(MetaConc2) ->      % case 3b
% 	  % \+ is_list(MetaConc2)
% 	  add_element(MetaConc1, MetaConc2, MacroMetaConc)
% 	; MetaConc1 == MetaConc2 ->
% 	  MacroMetaConc = MetaConc1
% 	; MacroMetaConc = [MetaConc1,MetaConc2]
% 	).

% union_semtype_lists(SemTypeList1A/SemTypeList1B,
%                     SemTypeList2A/SemTypeList2B,
%                     UnionSemTypeListA/UnionSemTypeListB) :-
%         union(SemTypeList1A, SemTypeList2A, UnionSemTypeListA),
%         union(SemTypeList1B, SemTypeList2B, UnionSemTypeListB).

% get_genphenom_components(genphenom(GenPhenomAtom),
% 			 _MinIndex, _MaxIndex, _GenPhenomList, GenPhenomAtom, _MetaConc, _SemTypeList).
%

% get_genphenom_components(genphenom(MinIndex, MaxIndex, GenPhenomList, MetaConc, SemTypeList),
% 			 MinIndex, MaxIndex,
% 			 GenPhenomList, GenPhenomAtom,
% 			 MetaConc, SemTypeList) :-
% 	GenPhenomList = [_NormGeneID,_NormGeneName,_GeneName, GenPhenomAtom,_GeneFunctionList].

% ---------- Check Entrezgene
% <Halil>If a gene name cannot even be isolated, we don't want to create a genphenom field, so fail. 
% If "None" is returned, still create a genphenom, since it might be useful later on; however, we 
% probably still don't want to print that out.</Halil>
check_entrezgene('n_g_n_f',_,_,_) :- !, fail.
check_entrezgene(Name, EntrezGeneIDAtom, EntrezGeneNameAtom, EntrezGeneFunctionAtom) :-
	% Atom is of the form                 IDList|NameList|Functionlist
	% where FunctionList is of the form   MF:xxx|BP:xxx|CC:xxx|
	% These are Gene Ontology annotation types:
	% MF = Molecular Function
	% BP = Biological Process
	% CC = Cell Component
	retrieve_gene_symbol(Name, TempEntrezGeneAtom),
	fix_entrezgene_symbol(TempEntrezGeneAtom, EntrezGeneAtom),
	atom_codes(EntrezGeneAtom, EntrezGeneStr),
	split_string_completely(EntrezGeneStr, [10], EntrezGeneList),
	get_entrezgene_elements(EntrezGeneList,
				EntrezGeneIDList, [],
				EntrezGeneNameList, [],
				EntrezGeneFunctionList, []),
	concat_atom(EntrezGeneIDList,       ',', EntrezGeneIDAtom),
	concat_atom(EntrezGeneNameList,     ',', EntrezGeneNameAtom),
	concat_atom(EntrezGeneFunctionList, ',', EntrezGeneFunctionAtom).


% Temporary fix for problem with retrieve_gene_symbol's returning ''
fix_entrezgene_symbol(TempSymbol, Symbol) :-
	( TempSymbol == '' ->
	  Symbol = '|None||||'
	; Symbol = TempSymbol
	).

get_entrezgene_elements([],
			IDList, IDList,
			NameList, NameList,
			FunctionList, FunctionList).
get_entrezgene_elements([FirstEntrezGeneString|MoreEntrezGeneStrings],
			[IDAtom|IDInList],             IDOutList,
			[NameAtom|NameInList],         NameOutList,
			[FunctionAtom|FunctionInList], FunctionOutList) :-
     split_string(FirstEntrezGeneString, "|", IDString,   RestString),
     split_string(RestString,            "|", NameString, FunctionString),
     atom_codes(IDAtom,       IDString),
     atom_codes(NameAtom,     NameString),
     atom_codes(FunctionAtom, FunctionString),
     get_entrezgene_elements(MoreEntrezGeneStrings,
			     IDInList,       IDOutList,
			     NameInList,     NameOutList,
			     FunctionInList, FunctionOutList).


% -----
/*
% get_gene_name(GPAtom,ABGenes,GeneName) :-
%     atom_codes(GPAtom,GPString),
%     split_string_completely(GPString," ",GPStrList),
%     strings_to_atoms(GPStrList,GPAtmList0),
%     remove_nonnames(GPAtmList0,GPAtmList),
%     get_gene_symbol(GPAtmList,GPStrList,ABGenes,GeneName).
*/

get_gene_name(GPAtom, ABGenes, GeneName) :-
	atom_codes(GPAtom, GPString),
	split_string_completely(GPString, " ", GPStrList),
	strings_to_atoms(GPStrList, GPAtmList),
	get_gene_symbol(GPAtmList, GPStrList, ABGenes, GeneName).


% ---

remove_nonnames([], []).
remove_nonnames([Atom|More], Rest) :-
	( gene_np_signal(Atom) ->
	  true
	; genetics_signal(Atom) ->
	  true
	; genetics_delete(Atom) ->
	  true
	; digit_name(Atom) ->
	  true
	),
	!,
	remove_nonnames(More, Rest).
remove_nonnames([Keep|More], [Keep|Gap]) :-
	remove_nonnames(More, Gap).

% --------- Remove Non Alphanumeric Characters
remove_nonalpha([], []).
remove_nonalpha([NonAlpha|More], Gap) :-
	check_nonalpha(NonAlpha),
	!,
	remove_nonalpha(More, Gap).
remove_nonalpha([Other|More], [Other|Gap]) :-
	remove_nonalpha(More, Gap).

% check_nonalpha([]) :- !, fail.
check_nonalpha([Char|_More]) :-
	memberchk(Char,"+<>%="),
	!.
check_nonalpha([_|More]) :-
	check_nonalpha(More).


% --- Get Gene Symbol

get_gene_symbol(GPAtmList, GPStrList, ABGenes, GeneName) :-
    ( check_abgene(ABGenes, GPAtmList, GeneName0),
      atom_codes(GeneName0, GeneNameStr0),
      split_string_completely(GeneNameStr0, " ", GeneNameStrList0),
      remove_nonalpha(GeneNameStrList0, GeneNameStrList),
      strings_to_atoms(GeneNameStrList, GeneNameAtmList0),
      remove_nonnames(GeneNameAtmList0, GeneNameAtmList),
      concat_atom(GeneNameAtmList, ' ', GeneName)
    ; remove_nonnames(GPAtmList,GPAtmList0),
      rev(GPAtmList0, RevGPAtmList0),
      shape_rules(RevGPAtmList0, GeneName0),
      atom_codes(GeneName0, GeneNameStr0),
      split_string_completely(GeneNameStr0, " ", GeneNameStrList0),
      remove_nonalpha(GeneNameStrList0, GeneNameStrList),
      strings_to_atoms(GeneNameStrList, GeneNameAtmList0),
      rev(GeneNameAtmList0, GeneNameAtmList),
      concat_atom(GeneNameAtmList, ' ', GeneName)
    ; remove_nonnames(GPAtmList, GPAtmList0),
      rev(GPAtmList0, RevGPAtmList0),
      check_hyphen(RevGPAtmList0,GPStrList, GeneName0),
      atom_codes(GeneName0, GeneNameStr0),
      split_string_completely(GeneNameStr0, " ", GeneNameStrList0),
      remove_nonalpha(GeneNameStrList0, GeneNameStrList),
      strings_to_atoms(GeneNameStrList, GeneNameAtmList0),
      rev(GeneNameAtmList0, GeneNameAtmList),
      concat_atom(GeneNameAtmList, ' ', GeneName)   
    ),
    atom_codes(GeneName, GeneNameStr),
    check_letter(GeneNameStr),
    !.
get_gene_symbol(_GPAtmList, _GPStrList, _ABGenes, n_g_n_f).

% --
check_abgene([ABGeneNameList|_More], GPAtomList, GeneName) :-
	lower_list(ABGeneNameList,LowABGeneAtomList),
	convert_tokens_to_str(LowABGeneAtomList,[],LowABGeneStrList),
	trim_whitespace_right(LowABGeneStrList,TrimmedLowABGeneStrList),
	convert_tokens_to_str(GPAtomList,[],GPStrList),
	trim_whitespace_right(GPStrList,TrimmedGPStrList),
	atom_codes(TrimmedLowABGeneAtom,TrimmedLowABGeneStrList),
	atom_codes(TrimmedGPAtom,TrimmedGPStrList),
	length(TrimmedLowABGeneStrList,ABGeneAtomLen),
	length(TrimmedGPStrList,GPAtomLen),
        ( substring(TrimmedGPAtom,TrimmedLowABGeneAtom, _,ABGeneAtomLen),
	  GeneName = TrimmedLowABGeneAtom
	; substring(TrimmedLowABGeneAtom,TrimmedGPAtom, _,GPAtomLen),
	  GeneName = TrimmedGPAtom
	).
check_abgene([_NoMatch|More], GPAtomList, GeneName) :-
	check_abgene(More, GPAtomList, GeneName).

% -
convert_tokens_to_str([],ListIn,ListIn) :- !.
convert_tokens_to_str([ListAtom|More],ListIn,ListStrOut) :-
     atom_codes(ListAtom,ListStr),
     append(ListIn,ListStr,ListStrOut1),
     append(ListStrOut1," ",ListStrOut2),
     convert_tokens_to_str(More,ListStrOut2,ListStrOut).


check_hyphen([],[],_) :- !,fail.
check_hyphen([Atom|_MoreA],[String|_MoreS],Atom) :-
    memberchk(0'-,String),!.
check_hyphen([_Atom|MoreA],[_String|MoreS],GeneName) :-
    check_hyphen(MoreA,MoreS,GeneName).

% - Shape Rules

% shape_rules(+List, -GeneName): Succeeds iff List contains anything
%		that satisfies protein_shape_rule_X.
% shape_rules([],_) :- !, fail.
shape_rules([Atom|_More], Atom) :-
	TokenList = [Atom],
	( protein_shape_rule_1(TokenList) -> true
	; protein_shape_rule_2(TokenList) -> true
	; protein_shape_rule_3(TokenList) -> true
	; protein_shape_rule_4(TokenList) -> true
	; protein_shape_rule_5(TokenList) -> true
	).
shape_rules([_NoMatch|More], GeneName) :-
	shape_rules(More, GeneName).
    
% ---------- Protein name shape rules ----------
% assume the head is a single token; check this out

/*
version 1.2. Enhancements for argument identification. 

-incorporate Jay Rajan's shape rules for protein names in chbt.pl

(Note currently implemented rules are marked by "+". Implemented in assess_head in chbt.pl)

(1,2,& 3 collapsed as protein_shape_rule_1)
+1. Any series of three or more letters (all caps) immediately followed by 1 or 2 
digits.
+2. Any series of three or more letters (all lowercase) immediately followed by 1 
or 2 digits.
+3. Same as above, except only first letter is uppercase.


-4. modify 1-3 such that the digits can be separated from the
preceding letters by a space (not very common) or a hyphen

(protein_shape_rule_2) 
+5. eliminate the digit requirement, but the
term should not be a normal English word.

(protein_shape_rule_3)
 +6. p (only lower case) immediately followed by
2 or more digits; the p does not need to come at a word boundary (you
also cover gp this way).

We may want to 
eliminate rule #5, as it has the potential to be a bit leaky. We may want to 
introduce some reasoning to rule out cell lines, but I tend to think that it is 
in such cases that the ontology comes to play.

Another rule we may add is:

7. any entity where "c-" or "v-" is immediately followed by any series of three 
letters. Many oncogenes were originally referred to in this way (and still are).

*/

% Three or more letters followed by 1 or 2 digits
%-----
protein_shape_rule_1([Token|_]) :-
	atom_codes(Token, String),
	length(String, StrLen),
	StrLen >= 3,
	initial_letters(Token),
	final_digits(Token),
	!.
protein_shape_rule_1([Token|_]) :-
    ( midstring(Token, NewToken, alpha, _, _) -> true
    ; midstring(Token, NewToken, beta,  _, _) -> true
    ; midstring(Token, NewToken, gamma, _, _) -> true
    ; midstring(Token, NewToken, delta, _, _) -> true
    ),
    protein_shape_rule_1([NewToken]).


% Three or more letters, not a normal word (has no vowels)
%-----
protein_shape_rule_2(TokenList) :-
	TokenList = [Token|_],
	lower(Token, LowToken),
	atom_codes(LowToken, String),
	length(String, StrLen),
	StrLen >= 3,
	intersect(String, "bcdfghjklmnpqrstvwxyz"),
	( intersect(String,"1234567890") -> true
	; \+ intersect(String,"aeiou")
	).

%    not_normal_word(TokenList).

% "gp" or "p" immediately followed by 2 or more digits
%-----
protein_shape_rule_3([Token|_]) :-
	midstring(Token, Front, Back, 0, 1),
	Front == p,
	atom_codes(Back,BackStr),
	length(BackStr,BLen),
	BLen >= 2,
	subset(BackStr,"1234567890").
protein_shape_rule_3([Token|_]) :-
	midstring(Token, Front, Back, 0, 2),
	Front == gp,
	atom_codes(Back, BackStr),
	length(BackStr, BLen),
	BLen >= 2,
	subset(BackStr, "1234567890").

% 4 to 6 characters with both a hyphen and a slash (tcr)
%-----
protein_shape_rule_4([Token|_]) :-
	atom_codes(Token, Chars),
	length(Chars, Len),
	Len >= 4,
	Len =< 6,
	check_letter(Chars),
	memberchk(0'-, Chars),
	memberchk(0'/, Chars).


% 3 characters, not in lexicon (abbrevs and acros count as NIL)
% Added 12/31/03: 4 characters, not in lexicon
%-----

protein_shape_rule_5([Token|_]) :-
	atom_codes(Token, Chars),
	subset(Chars, "abcdefghijklmnopqrstuvwxyz"),
	length(Chars, Len),
	( Len =:= 3 -> true
	; Len =:= 4
	),
	non_lexical_status(Token).

% Make sure a string has at least one letter in it.
check_letter([Char|_]) :- 
    memberchk(Char,"abcdefghijklmnopqrstuvwxyz"), !.
check_letter([_|More]) :- check_letter(More).

%-----
initial_letters(Token) :-
   lower(Token,LowToken),
   midstring(LowToken,Front,_Back,0,3),
   atom_codes(Front,FrontStr),
   subset(FrontStr,"abcdefghijklmnopqrstuvwxyz").

%-----
final_digits(Token) :-
    lower(Token,LowToken),
    midstring(LowToken,Front,Back,0,_,1),
    atom_codes(Front,FrontStr),
    subset(FrontStr,"abcdefghijklmnopqrstuvwxyz"),
    atom_codes(Back,BackStr),
    subset(BackStr,"1234567890").
final_digits(Token) :-
    lower(Token,LowToken),
    midstring(LowToken,Front,Back,0,_,2),
    atom_codes(Front,FrontStr),
    subset(FrontStr,"abcdefghijklmnopqrstuvwxyz"),
    atom_codes(Back,BackStr),
    subset(BackStr,"1234567890").

% If gene name contains an
% glom_multi_element_genes([], []).
% glom_multi_element_genes([ThisMSU|RestMSUs], [GlommedThisMSU|RestGlommedMSUs]) :-
% 	glom_multi_element_genes_in_MSU(ThisMSU, GlommedThisMSU),
% 	glom_multi_element_genes(RestMSUs, RestGlommedMSUs).

glom_multi_element_genes_in_MSU([], []).
glom_multi_element_genes_in_MSU([H|T], GlommedMSU) :-
	( append([BeforeSequence, [BeforeHyphen,punc(HyphenList),AfterHyphen], AfterSequence],
		  [H|T]),
	  memberchk(inputmatch([-]), HyphenList),
	  probable_gene_term(BeforeHyphen),
	  probable_gene_term(AfterHyphen),
          glom_together(BeforeHyphen, AfterHyphen, GlommedTerm),
	  append(BeforeSequence, [GlommedTerm], NewPrefix),
	  append(NewPrefix, AfterSequence, GlommedMSU)
	; GlommedMSU = [H|T]
	).

probable_gene_term(Term) :-
	arg(1, Term, ArgList),
	memberchk(inputmatch(InputMatchList), ArgList),
	concat_atom(InputMatchList, InputMatchAtom),
	atom_codes(InputMatchAtom, InputMatchString),
        probable_half_of_gene_name(InputMatchString).

% A token probably represents a gene name if it
% consists of nothing but uppercase letters and digits
probable_half_of_gene_name([]).
probable_half_of_gene_name([H|T]) :-
	( is_upper(H) ->
	  true
	; is_digit(H)
	),
	probable_half_of_gene_name(T).

glom_together(BeforeHyphen, AfterHyphen, GlommedTerm) :-
	arg(1, BeforeHyphen, BeforeHyphenList),
	memberchk(index(BeforeIndex),               BeforeHyphenList),
	memberchk(inputmatch(BeforeInputMatchList), BeforeHyphenList),
	memberchk(bases(BeforeBasesList),           BeforeHyphenList),
	memberchk(tokens(BeforeTokenList),          BeforeHyphenList),
	memberchk(position(Start,_),                BeforeHyphenList),

	arg(1, AfterHyphen, AfterHyphenList),
	memberchk(index(AfterIndex),               AfterHyphenList),
	memberchk(inputmatch(AfterInputMatchList), AfterHyphenList),
	memberchk(bases(AfterBasesList),           AfterHyphenList),
	memberchk(tokens(AfterTokenList),          AfterHyphenList),
	memberchk(position(_,End),                 AfterHyphenList),

	append([BeforeInputMatchList, [-], AfterInputMatchList], GlommedInputMatchList),
	append([BeforeBasesList,      [-], AfterBasesList],      GlommedBasesList),
	append([BeforeTokenList,      [-], AfterTokenList],      GlommedTokenList),

	reconcile_features(BeforeHyphen, AfterHyphen, GlommedFunctor, GlommedList),
	functor(GlommedTerm, GlommedFunctor, 1),
	append([[index(AfterIndex:BeforeIndex/AfterIndex)], 
	        GlommedList,
		[inputmatch(GlommedInputMatchList),bases(GlommedBasesList),tokens(GlommedTokenList),position(Start,End)]],
		GlommedTermList),

	union_items(metaconc,  BeforeHyphenList,  AfterHyphenList, GlommedMetaConc),
	( GlommedMetaConc \== [],
	  union_items(usemtype,  BeforeHyphenList,  AfterHyphenList, GlommedUSemtype),
	  union_items(ausemtype, BeforeHyphenList,  AfterHyphenList, GlommedAUSemtype),
	  union_items(semgroup,  BeforeHyphenList,  AfterHyphenList, GlommedSemGroup),
	  append([GlommedTermList,
		  [usemtype(GlommedUSemtype), ausemtype(GlommedAUSemtype), semgroup(GlommedSemGroup), 
                   metaconc(GlommedMetaConc)]],
		 GlommedTermList1),
          arg(1, GlommedTerm, GlommedTermList1)
        ; arg(1, GlommedTerm, GlommedTermList)
        ).

reconcile_features(mod(_BeforeHyphenList), head(AfterHyphenList), head, GlommedList) :-
	rest_of_features(AfterHyphenList, GlommedList, []).
reconcile_features(head(BeforeHyphenList), _AfterHyphen, head, GlommedList) :-
	rest_of_features(BeforeHyphenList, GlommedList, []).
reconcile_features(mod(BeforeHyphenList), _AfterHyphen, mod, GlommedList) :-
	rest_of_features(BeforeHyphenList, GlommedList, []).
reconcile_features(_BeforeHyphen, AfterHyphen, AfterHyphenFunctor, GlommedList) :-
	arg(1, AfterHyphen, AfterHyphenList),
	functor(AfterHyphen, AfterHyphenFunctor, 1),
	rest_of_features(AfterHyphenList, GlommedList, []).

rest_of_features([], GlommedList, GlommedList).
rest_of_features([Feature|Rest], GlommedListIn, GlommedListOut) :-
	functor(Feature, FeatureType, 1),
	\+ memberchk(FeatureType, 
	             [metaconc, usemtype, ausemtype, semgroup, 
		     index, inputmatch, bases, tokens]),
        GlommedListIn = [Feature|GlommedListNext],		     
	rest_of_features(Rest, GlommedListNext, GlommedListOut).
rest_of_features([_Feature|Rest], GlommedListIn, GlommedListOut) :-
	rest_of_features(Rest, GlommedListIn, GlommedListOut).


union_items(Type, MSU1, MSU2, GlommedItem) :-
	( get_from_list(Type, MSU1, Item1)
        ; Item1 = []
        ),
	( get_from_list(Type, MSU2, Item2)
        ; Item2 = []
        ),
	( is_list(Item1),
	  is_list(Item2) ->
	  union(Item1, Item2, GlommedItem)
        ; is_list(Item1) ->
	  add_element(Item2, Item1, GlommedItem)
        ; is_list(Item2) ->
	  add_element(Item1, Item2, GlommedItem)
        ).
	

