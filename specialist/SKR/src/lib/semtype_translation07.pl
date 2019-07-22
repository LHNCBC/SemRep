% File:     semtype_translation07.pl
% Module:   semtype_translation08
% Author:   Lan
% Purpose:  Provide translation among
%             full semantic type names (e.g., Vitamin),
%             their four-character equivalents (e.g., vita),
%             their UI (e.g., T127), and
%             their tree numbers (e.g., A1.4.1.1.3.4).


:- module(semtype_translation07,[
    semtype_translation/2,
    semtype_translation/4,
    abbreviate_semtypes/2,
    expand_semtypes/2,
    is_semtype/1,
    is_abbrev_semtype/1,
    application_name/1  % for compatibility with Semrep only
    ]).


/* semtype_translation(?Name, ?Abbrev)
   semtype_translation(?Name, ?Abbrev, ?UI, ?TreeNo)

semtype_translation/2 is a factual predicate based on semtype_translation/4
which defines semantic type Names, their 4-character Abbrev(iations), their
UI and their TreeNo.  The data was derived from the 2007 Semantic Network.
The 1994 semantic type, Macromolecular Structure, was dropped in the
process of updating the data to 1996.  No changes occurred in 1997.
The following changes were made in 1998:
        2 name changes
            Element or Ion (eloi)
              -> Element, Ion, or Isotope (elii)
            Indicator or Reagent (inre)
              -> Indicator, Reagent, or Diagnostic Aid (irda)
        5 deletions
            Alkaloid (alkd)
            Inorganic Compound (inco)
            Isotope (isot)
            Lactam (lact)
            Prostaglanndin (prgl)
        2 additions
            Antibiotic (antb)
            Archaeon (arch)
Note that several changes in semantic type tree numbers also occurred in 1998.
And the following changes were made in 1999:
        2 additions
            Clinical Attribute (clna)
            Clinical Drug (clnd)
*/

semtype_translation(Name,Abbrev) :-
    semtype_translation(Name,Abbrev,_UI,_TreeNo).

semtype_translation('Acquired Abnormality','acab','T020','A1.2.2.2') :- !.
semtype_translation('Activity','acty','T052','B1') :- !.
semtype_translation('Age Group','aggp','T100','A2.9.4') :- !.
semtype_translation('Alga','alga','T003','A1.1.1.1') :- !.
semtype_translation('Amino Acid Sequence','amas','T087','A2.1.5.3.2') :- !.
semtype_translation('Amino Acid, Peptide, or Protein','aapp','T116','A1.4.1.2.1.7') :- !.
semtype_translation('Amphibian','amph','T011','A1.1.7.2.1') :- !.
semtype_translation('Anatomical Abnormality','anab','T190','A1.2.2') :- !.
semtype_translation('Anatomical Structure','anst','T017','A1.2') :- !.
semtype_translation('Animal','anim','T008','A1.1.7') :- !.
semtype_translation('Antibiotic','antb','T195','A1.4.1.1.1.1') :- !.
semtype_translation('Archaeon','arch','T194','A1.1.6') :- !.
semtype_translation('Bacterium','bact','T007','A1.1.5') :- !.
semtype_translation('Behavior','bhvr','T053','B1.1') :- !.
semtype_translation('Biologic Function','biof','T038','B2.2.1') :- !.
semtype_translation('Biologically Active Substance','bacs','T123','A1.4.1.1.3') :- !.
semtype_translation('Biomedical Occupation or Discipline','bmod','T091','A2.6.1') :- !.
semtype_translation('Biomedical or Dental Material','bodm','T122','A1.4.1.1.2') :- !.
semtype_translation('Bird','bird','T012','A1.1.7.2.2') :- !.
semtype_translation('Body Location or Region','blor','T029','A2.1.5.2') :- !.
semtype_translation('Body Part, Organ, or Organ Component','bpoc','T023','A1.2.3.1') :- !.
semtype_translation('Body Space or Junction','bsoj','T030','A2.1.5.1') :- !.
semtype_translation('Body Substance','bdsu','T031','A1.4.2') :- !.
semtype_translation('Body System','bdsy','T022','A2.1.4.1') :- !.
semtype_translation('Carbohydrate','carb','T118','A1.4.1.2.1.8') :- !.
semtype_translation('Carbohydrate Sequence','crbs','T088','A2.1.5.3.3') :- !.
semtype_translation('Cell','cell','T025','A1.2.3.3') :- !.
semtype_translation('Cell Component','celc','T026','A1.2.3.4') :- !.
semtype_translation('Cell Function','celf','T043','B2.2.1.1.3') :- !.
semtype_translation('Cell or Molecular Dysfunction','comd','T049','B2.2.1.2.2') :- !.
semtype_translation('Chemical','chem','T103','A1.4.1') :- !.
semtype_translation('Chemical Viewed Functionally','chvf','T120','A1.4.1.1') :- !.
semtype_translation('Chemical Viewed Structurally','chvs','T104','A1.4.1.2') :- !.
semtype_translation('Classification','clas','T185','A2.4.1') :- !.
semtype_translation('Clinical Attribute','clna','T201','A2.3.1') :- !.
semtype_translation('Clinical Drug','clnd','T200','A1.3.3') :- !.
semtype_translation('Conceptual Entity','cnce','T077','A2') :- !.
semtype_translation('Congenital Abnormality','cgab','T019','A1.2.2.1') :- !.
semtype_translation('Daily or Recreational Activity','dora','T056','B1.2') :- !.
semtype_translation('Diagnostic Procedure','diap','T060','B1.3.1.2') :- !.
semtype_translation('Disease or Syndrome','dsyn','T047','B2.2.1.2.1') :- !.
semtype_translation('Drug Delivery Device','drdd','T203','A1.3.1.1') :- !.
semtype_translation('Educational Activity','edac','T065','B1.3.4') :- !.
semtype_translation('Eicosanoid','eico','T111','A1.4.1.2.1.9.2') :- !.
semtype_translation('Element, Ion, or Isotope','elii','T196','A1.4.1.2.3') :- !.
semtype_translation('Embryonic Structure','emst','T018','A1.2.1') :- !.
semtype_translation('Entity','enty','T071','A') :- !.
semtype_translation('Environmental Effect of Humans','eehu','T069','B2.1.1') :- !.
semtype_translation('Enzyme','enzy','T126','A1.4.1.1.3.3') :- !.
semtype_translation('Event','evnt','T051','B') :- !.
semtype_translation('Experimental Model of Disease','emod','T050','B2.2.1.2.3') :- !.
semtype_translation('Family Group','famg','T099','A2.9.3') :- !.
semtype_translation('Finding','fndg','T033','A2.2') :- !.
semtype_translation('Fish','fish','T013','A1.1.7.2.3') :- !.
semtype_translation('Food','food','T168','A1.4.3') :- !.
semtype_translation('Fully Formed Anatomical Structure','ffas','T021','A1.2.3') :- !.
semtype_translation('Functional Concept','ftcn','T169','A2.1.4') :- !.
semtype_translation('Fungus','fngs','T004','A1.1.2') :- !.
% The following is a pseudo-semantic type for use with gene terminology
% in which genes and proteins are not necessarily distinguishable
semtype_translation('Gene or Gene Product','gngp','Tx01','A1.2.3.x') :- !.
semtype_translation('Gene or Genome','gngm','T028','A1.2.3.5') :- !.
semtype_translation('Genetic Function','genf','T045','B2.2.1.1.4.1') :- !.
semtype_translation('Geographic Area','geoa','T083','A2.1.5.4') :- !.
semtype_translation('Governmental or Regulatory Activity','gora','T064','B1.3.3') :- !.
semtype_translation('Group','grup','T096','A2.9') :- !.
semtype_translation('Group Attribute','grpa','T102','A2.8') :- !.
semtype_translation('Hazardous or Poisonous Substance','hops','T131','A1.4.1.1.5') :- !.
semtype_translation('Health Care Activity','hlca','T058','B1.3.1') :- !.
semtype_translation('Health Care Related Organization','hcro','T093','A2.7.1') :- !.
semtype_translation('Hormone','horm','T125','A1.4.1.1.3.2') :- !.
semtype_translation('Human','humn','T016','A1.1.7.2.5.1') :- !.
semtype_translation('Human-caused Phenomenon or Process','hcpp','T068','B2.1') :- !.
semtype_translation('Idea or Concept','idcn','T078','A2.1') :- !.
semtype_translation('Immunologic Factor','imft','T129','A1.4.1.1.3.5') :- !.
semtype_translation('Indicator, Reagent, or Diagnostic Aid','irda','T130','A1.4.1.1.4') :- !.
semtype_translation('Individual Behavior','inbe','T055','B1.1.2') :- !.
semtype_translation('Injury or Poisoning','inpo','T037','B2.3') :- !.
semtype_translation('Inorganic Chemical','inch','T197','A1.4.1.2.2') :- !.
semtype_translation('Intellectual Product','inpr','T170','A2.4') :- !.
semtype_translation('Invertebrate','invt','T009','A1.1.7.1') :- !.
semtype_translation('Laboratory Procedure','lbpr','T059','B1.3.1.1') :- !.
semtype_translation('Laboratory or Test Result','lbtr','T034','A2.2.1') :- !.
semtype_translation('Language','lang','T171','A2.5') :- !.
semtype_translation('Lipid','lipd','T119','A1.4.1.2.1.9') :- !.
semtype_translation('Machine Activity','mcha','T066','B1.4') :- !.
semtype_translation('Mammal','mamm','T015','A1.1.7.2.5') :- !.
semtype_translation('Manufactured Object','mnob','T073','A1.3') :- !.
semtype_translation('Medical Device','medd','T074','A1.3.1') :- !.
semtype_translation('Mental Process','menp','T041','B2.2.1.1.1.1') :- !.
semtype_translation('Mental or Behavioral Dysfunction','mobd','T048','B2.2.1.2.1.1') :- !.
semtype_translation('Molecular Biology Research Technique','mbrt','T063','B1.3.2.1') :- !.
semtype_translation('Molecular Function','moft','T044','B2.2.1.1.4') :- !.
semtype_translation('Molecular Sequence','mosq','T085','A2.1.5.3') :- !.
semtype_translation('Natural Phenomenon or Process','npop','T070','B2.2') :- !.
semtype_translation('Neoplastic Process','neop','T191','B2.2.1.2.1.2') :- !.
semtype_translation('Neuroreactive Substance or Biogenic Amine','nsba','T124','A1.4.1.1.3.1') :- !.
semtype_translation('Nucleic Acid, Nucleoside, or Nucleotide','nnon','T114','A1.4.1.2.1.5') :- !.
semtype_translation('Nucleotide Sequence','nusq','T086','A2.1.5.3.1') :- !.
% The following is a pseudo-semantic type for use with terminology
% that is not classified as a kind of entity or event
semtype_translation('Object','objt','T000','X') :- !.
semtype_translation('Occupation or Discipline','ocdi','T090','A2.6') :- !.
semtype_translation('Occupational Activity','ocac','T057','B1.3') :- !.
semtype_translation('Organ or Tissue Function','ortf','T042','B2.2.1.1.2') :- !.
semtype_translation('Organic Chemical','orch','T109','A1.4.1.2.1') :- !.
semtype_translation('Organism','orgm','T001','A1.1') :- !.
semtype_translation('Organism Attribute','orga','T032','A2.3') :- !.
semtype_translation('Organism Function','orgf','T040','B2.2.1.1.1') :- !.
semtype_translation('Organization','orgt','T092','A2.7') :- !.
semtype_translation('Organophosphorus Compound','opco','T115','A1.4.1.2.1.6') :- !.
semtype_translation('Pathologic Function','patf','T046','B2.2.1.2') :- !.
semtype_translation('Patient or Disabled Group','podg','T101','A2.9.5') :- !.
semtype_translation('Pharmacologic Substance','phsu','T121','A1.4.1.1.1') :- !.
semtype_translation('Phenomenon or Process','phpr','T067','B2') :- !.
semtype_translation('Physical Object','phob','T072','A1') :- !.
semtype_translation('Physiologic Function','phsf','T039','B2.2.1.1') :- !.
semtype_translation('Plant','plnt','T002','A1.1.1') :- !.
semtype_translation('Population Group','popg','T098','A2.9.2') :- !.
semtype_translation('Professional Society','pros','T094','A2.7.2') :- !.
semtype_translation('Professional or Occupational Group','prog','T097','A2.9.1') :- !.
semtype_translation('Qualitative Concept','qlco','T080','A2.1.2') :- !.
semtype_translation('Quantitative Concept','qnco','T081','A2.1.3') :- !.
semtype_translation('Receptor','rcpt','T192','A1.4.1.1.3.6') :- !.
semtype_translation('Regulation or Law','rnlw','T089','A2.4.2') :- !.
semtype_translation('Reptile','rept','T014','A1.1.7.2.4') :- !.
semtype_translation('Research Activity','resa','T062','B1.3.2') :- !.
semtype_translation('Research Device','resd','T075','A1.3.2') :- !.
semtype_translation('Rickettsia or Chlamydia','rich','T006','A1.1.4') :- !.
semtype_translation('Self-help or Relief Organization','shro','T095','A2.7.3') :- !.
semtype_translation('Sign or Symptom','sosy','T184','A2.2.2') :- !.
semtype_translation('Social Behavior','socb','T054','B1.1.1') :- !.
semtype_translation('Spatial Concept','spco','T082','A2.1.5') :- !.
semtype_translation('Steroid','strd','T110','A1.4.1.2.1.9.1') :- !.
semtype_translation('Substance','sbst','T167','A1.4') :- !.
semtype_translation('Temporal Concept','tmco','T079','A2.1.1') :- !.
semtype_translation('Therapeutic or Preventive Procedure','topp','T061','B1.3.1.3') :- !.
semtype_translation('Tissue','tisu','T024','A1.2.3.2') :- !.
semtype_translation('Vertebrate','vtbt','T010','A1.1.7.2') :- !.
semtype_translation('Virus','virs','T005','A1.1.3') :- !.
semtype_translation('Vitamin','vita','T127','A1.4.1.1.3.4') :- !.

/* abbreviate_semtypes(+SemTypes, -SemTypeAbbrevs)
   expand_semtypes(+SemTypeAbbrevs, -SemTypes)

abbreviate_semtypes/2 converts a list of semantic types to their abbreviations.
expand_semtypes/2 does the converse.  */

abbreviate_semtypes([],[]).
abbreviate_semtypes([First|Rest],[FirstAbbrev|RestAbbrevs]) :-
    (semtype_translation(First,FirstAbbrev) ->
        true
    ;   FirstAbbrev=First
    ),
    abbreviate_semtypes(Rest,RestAbbrevs).

expand_semtypes([],[]).
expand_semtypes([FirstAbbrev|RestAbbrevs],[First|Rest]) :-
    (semtype_translation(First,FirstAbbrev) ->
        true
    ;   FirstAbbrev=First
    ),
    expand_semtypes(RestAbbrevs,Rest).



is_semtype(Semtype) :-
    semtype_translation(Semtype,_).


is_abbrev_semtype(Semtype) :-
    semtype_translation(_,Semtype).


application_name(dummy).  % for compatibility with Semrep only
