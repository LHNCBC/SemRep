
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                          %%%
%%%           IMPORTANT:                     %%%
%%%    DO NOT MODIFY THIS FILE:              %%%
%%%    IT IS MACHINE-GENERATED               %%%
%%%  from the semtype_translation files      %%%
%%%        in $SKR/src/lib                   %%%
%%%                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(semtype_translation, [
     application_name/1,
     semtype_translation/3
]).

semtype_translation(Version, Name, Abbrev) :-
     ( nonvar(Name) ->
       semtype_translation_fact(Version, Name, Abbrev)

     ; semtype_translation_reverse(Version, Abbrev, Name)
     ),
     !.

semtype_translation_fact('06','Acquired Abnormality',acab).
semtype_translation_fact('06','Activity',acty).
semtype_translation_fact('06','Age Group',aggp).
semtype_translation_fact('06','Alga',alga).
semtype_translation_fact('06','Amino Acid Sequence',amas).
semtype_translation_fact('06','Amino Acid, Peptide, or Protein',aapp).
semtype_translation_fact('06','Amphibian',amph).
semtype_translation_fact('06','Anatomical Abnormality',anab).
semtype_translation_fact('06','Anatomical Structure',anst).
semtype_translation_fact('06','Animal',anim).
semtype_translation_fact('06','Antibiotic',antb).
semtype_translation_fact('06','Archaeon',arch).
semtype_translation_fact('06','Bacterium',bact).
semtype_translation_fact('06','Behavior',bhvr).
semtype_translation_fact('06','Biologic Function',biof).
semtype_translation_fact('06','Biologically Active Substance',bacs).
semtype_translation_fact('06','Biomedical Occupation or Discipline',bmod).
semtype_translation_fact('06','Biomedical or Dental Material',bodm).
semtype_translation_fact('06','Bird',bird).
semtype_translation_fact('06','Body Location or Region',blor).
semtype_translation_fact('06','Body Part, Organ, or Organ Component',bpoc).
semtype_translation_fact('06','Body Space or Junction',bsoj).
semtype_translation_fact('06','Body Substance',bdsu).
semtype_translation_fact('06','Body System',bdsy).
semtype_translation_fact('06','Carbohydrate',carb).
semtype_translation_fact('06','Carbohydrate Sequence',crbs).
semtype_translation_fact('06','Cell',cell).
semtype_translation_fact('06','Cell Component',celc).
semtype_translation_fact('06','Cell Function',celf).
semtype_translation_fact('06','Cell or Molecular Dysfunction',comd).
semtype_translation_fact('06','Chemical',chem).
semtype_translation_fact('06','Chemical Viewed Functionally',chvf).
semtype_translation_fact('06','Chemical Viewed Structurally',chvs).
semtype_translation_fact('06','Classification',clas).
semtype_translation_fact('06','Clinical Attribute',clna).
semtype_translation_fact('06','Clinical Drug',clnd).
semtype_translation_fact('06','Conceptual Entity',cnce).
semtype_translation_fact('06','Congenital Abnormality',cgab).
semtype_translation_fact('06','Daily or Recreational Activity',dora).
semtype_translation_fact('06','Diagnostic Procedure',diap).
semtype_translation_fact('06','Disease or Syndrome',dsyn).
semtype_translation_fact('06','Drug Delivery Device',drdd).
semtype_translation_fact('06','Educational Activity',edac).
semtype_translation_fact('06','Eicosanoid',eico).
semtype_translation_fact('06','Element, Ion, or Isotope',elii).
semtype_translation_fact('06','Embryonic Structure',emst).
semtype_translation_fact('06','Entity',enty).
semtype_translation_fact('06','Environmental Effect of Humans',eehu).
semtype_translation_fact('06','Enzyme',enzy).
semtype_translation_fact('06','Event',evnt).
semtype_translation_fact('06','Experimental Model of Disease',emod).
semtype_translation_fact('06','Family Group',famg).
semtype_translation_fact('06','Finding',fndg).
semtype_translation_fact('06','Fish',fish).
semtype_translation_fact('06','Food',food).
semtype_translation_fact('06','Fully Formed Anatomical Structure',ffas).
semtype_translation_fact('06','Functional Concept',ftcn).
semtype_translation_fact('06','Fungus',fngs).
semtype_translation_fact('06','Gene or Gene Product',gngp).
semtype_translation_fact('06','Gene or Genome',gngm).
semtype_translation_fact('06','Genetic Function',genf).
semtype_translation_fact('06','Geographic Area',geoa).
semtype_translation_fact('06','Governmental or Regulatory Activity',gora).
semtype_translation_fact('06','Group',grup).
semtype_translation_fact('06','Group Attribute',grpa).
semtype_translation_fact('06','Hazardous or Poisonous Substance',hops).
semtype_translation_fact('06','Health Care Activity',hlca).
semtype_translation_fact('06','Health Care Related Organization',hcro).
semtype_translation_fact('06','Hormone',horm).
semtype_translation_fact('06','Human',humn).
semtype_translation_fact('06','Human-caused Phenomenon or Process',hcpp).
semtype_translation_fact('06','Idea or Concept',idcn).
semtype_translation_fact('06','Immunologic Factor',imft).
semtype_translation_fact('06','Indicator, Reagent, or Diagnostic Aid',irda).
semtype_translation_fact('06','Individual Behavior',inbe).
semtype_translation_fact('06','Injury or Poisoning',inpo).
semtype_translation_fact('06','Inorganic Chemical',inch).
semtype_translation_fact('06','Intellectual Product',inpr).
semtype_translation_fact('06','Invertebrate',invt).
semtype_translation_fact('06','Laboratory Procedure',lbpr).
semtype_translation_fact('06','Laboratory or Test Result',lbtr).
semtype_translation_fact('06','Language',lang).
semtype_translation_fact('06','Lipid',lipd).
semtype_translation_fact('06','Machine Activity',mcha).
semtype_translation_fact('06','Mammal',mamm).
semtype_translation_fact('06','Manufactured Object',mnob).
semtype_translation_fact('06','Medical Device',medd).
semtype_translation_fact('06','Mental Process',menp).
semtype_translation_fact('06','Mental or Behavioral Dysfunction',mobd).
semtype_translation_fact('06','Molecular Biology Research Technique',mbrt).
semtype_translation_fact('06','Molecular Function',moft).
semtype_translation_fact('06','Molecular Sequence',mosq).
semtype_translation_fact('06','Natural Phenomenon or Process',npop).
semtype_translation_fact('06','Neoplastic Process',neop).
semtype_translation_fact('06','Neuroreactive Substance or Biogenic Amine',nsba).
semtype_translation_fact('06','Nucleic Acid, Nucleoside, or Nucleotide',nnon).
semtype_translation_fact('06','Nucleotide Sequence',nusq).
semtype_translation_fact('06','Object',objt).
semtype_translation_fact('06','Occupation or Discipline',ocdi).
semtype_translation_fact('06','Occupational Activity',ocac).
semtype_translation_fact('06','Organ or Tissue Function',ortf).
semtype_translation_fact('06','Organic Chemical',orch).
semtype_translation_fact('06','Organism',orgm).
semtype_translation_fact('06','Organism Attribute',orga).
semtype_translation_fact('06','Organism Function',orgf).
semtype_translation_fact('06','Organization',orgt).
semtype_translation_fact('06','Organophosphorus Compound',opco).
semtype_translation_fact('06','Pathologic Function',patf).
semtype_translation_fact('06','Patient or Disabled Group',podg).
semtype_translation_fact('06','Pharmacologic Substance',phsu).
semtype_translation_fact('06','Phenomenon or Process',phpr).
semtype_translation_fact('06','Physical Object',phob).
semtype_translation_fact('06','Physiologic Function',phsf).
semtype_translation_fact('06','Plant',plnt).
semtype_translation_fact('06','Population Group',popg).
semtype_translation_fact('06','Professional Society',pros).
semtype_translation_fact('06','Professional or Occupational Group',prog).
semtype_translation_fact('06','Qualitative Concept',qlco).
semtype_translation_fact('06','Quantitative Concept',qnco).
semtype_translation_fact('06','Receptor',rcpt).
semtype_translation_fact('06','Regulation or Law',rnlw).
semtype_translation_fact('06','Reptile',rept).
semtype_translation_fact('06','Research Activity',resa).
semtype_translation_fact('06','Research Device',resd).
semtype_translation_fact('06','Rickettsia or Chlamydia',rich).
semtype_translation_fact('06','Self-help or Relief Organization',shro).
semtype_translation_fact('06','Sign or Symptom',sosy).
semtype_translation_fact('06','Social Behavior',socb).
semtype_translation_fact('06','Spatial Concept',spco).
semtype_translation_fact('06','Steroid',strd).
semtype_translation_fact('06','Substance',sbst).
semtype_translation_fact('06','Temporal Concept',tmco).
semtype_translation_fact('06','Therapeutic or Preventive Procedure',topp).
semtype_translation_fact('06','Tissue',tisu).
semtype_translation_fact('06','Vertebrate',vtbt).
semtype_translation_fact('06','Virus',virs).
semtype_translation_fact('06','Vitamin',vita).
semtype_translation_fact('12','Acquired Abnormality',acab).
semtype_translation_fact('12','Activity',acty).
semtype_translation_fact('12','Age Group',aggp).
semtype_translation_fact('12','Amino Acid Sequence',amas).
semtype_translation_fact('12','Amino Acid, Peptide, or Protein',aapp).
semtype_translation_fact('12','Amphibian',amph).
semtype_translation_fact('12','Anatomical Abnormality',anab).
semtype_translation_fact('12','Anatomical Structure',anst).
semtype_translation_fact('12','Animal',anim).
semtype_translation_fact('12','Antibiotic',antb).
semtype_translation_fact('12','Archaeon',arch).
semtype_translation_fact('12','Bacterium',bact).
semtype_translation_fact('12','Behavior',bhvr).
semtype_translation_fact('12','Biologic Function',biof).
semtype_translation_fact('12','Biologically Active Substance',bacs).
semtype_translation_fact('12','Biomedical Occupation or Discipline',bmod).
semtype_translation_fact('12','Biomedical or Dental Material',bodm).
semtype_translation_fact('12','Bird',bird).
semtype_translation_fact('12','Body Location or Region',blor).
semtype_translation_fact('12','Body Part, Organ, or Organ Component',bpoc).
semtype_translation_fact('12','Body Space or Junction',bsoj).
semtype_translation_fact('12','Body Substance',bdsu).
semtype_translation_fact('12','Body System',bdsy).
semtype_translation_fact('12','Carbohydrate Sequence',crbs).
semtype_translation_fact('12','Carbohydrate',carb).
semtype_translation_fact('12','Cell Component',celc).
semtype_translation_fact('12','Cell Function',celf).
semtype_translation_fact('12','Cell or Molecular Dysfunction',comd).
semtype_translation_fact('12','Cell',cell).
semtype_translation_fact('12','Chemical Viewed Functionally',chvf).
semtype_translation_fact('12','Chemical Viewed Structurally',chvs).
semtype_translation_fact('12','Chemical',chem).
semtype_translation_fact('12','Classification',clas).
semtype_translation_fact('12','Clinical Attribute',clna).
semtype_translation_fact('12','Clinical Drug',clnd).
semtype_translation_fact('12','Conceptual Entity',cnce).
semtype_translation_fact('12','Congenital Abnormality',cgab).
semtype_translation_fact('12','Daily or Recreational Activity',dora).
semtype_translation_fact('12','Diagnostic Procedure',diap).
semtype_translation_fact('12','Disease or Syndrome',dsyn).
semtype_translation_fact('12','Drug Delivery Device',drdd).
semtype_translation_fact('12','Educational Activity',edac).
semtype_translation_fact('12','Eicosanoid',eico).
semtype_translation_fact('12','Element, Ion, or Isotope',elii).
semtype_translation_fact('12','Embryonic Structure',emst).
semtype_translation_fact('12','Entity',enty).
semtype_translation_fact('12','Environmental Effect of Humans',eehu).
semtype_translation_fact('12','Enzyme',enzy).
semtype_translation_fact('12','Eukaryote',euka).
semtype_translation_fact('12','Event',evnt).
semtype_translation_fact('12','Experimental Model of Disease',emod).
semtype_translation_fact('12','Family Group',famg).
semtype_translation_fact('12','Finding',fndg).
semtype_translation_fact('12','Fish',fish).
semtype_translation_fact('12','Food',food).
semtype_translation_fact('12','Fully Formed Anatomical Structure',ffas).
semtype_translation_fact('12','Functional Concept',ftcn).
semtype_translation_fact('12','Fungus',fngs).
semtype_translation_fact('12','Gene or Gene Product',gngp).
semtype_translation_fact('12','Gene or Genome',gngm).
semtype_translation_fact('12','Genetic Function',genf).
semtype_translation_fact('12','Geographic Area',geoa).
semtype_translation_fact('12','Governmental or Regulatory Activity',gora).
semtype_translation_fact('12','Group Attribute',grpa).
semtype_translation_fact('12','Group',grup).
semtype_translation_fact('12','Hazardous or Poisonous Substance',hops).
semtype_translation_fact('12','Health Care Activity',hlca).
semtype_translation_fact('12','Health Care Related Organization',hcro).
semtype_translation_fact('12','Hormone',horm).
semtype_translation_fact('12','Human',humn).
semtype_translation_fact('12','Human-caused Phenomenon or Process',hcpp).
semtype_translation_fact('12','Idea or Concept',idcn).
semtype_translation_fact('12','Immunologic Factor',imft).
semtype_translation_fact('12','Indicator, Reagent, or Diagnostic Aid',irda).
semtype_translation_fact('12','Individual Behavior',inbe).
semtype_translation_fact('12','Injury or Poisoning',inpo).
semtype_translation_fact('12','Inorganic Chemical',inch).
semtype_translation_fact('12','Intellectual Product',inpr).
semtype_translation_fact('12','Laboratory Procedure',lbpr).
semtype_translation_fact('12','Laboratory or Test Result',lbtr).
semtype_translation_fact('12','Language',lang).
semtype_translation_fact('12','Lipid',lipd).
semtype_translation_fact('12','Machine Activity',mcha).
semtype_translation_fact('12','Mammal',mamm).
semtype_translation_fact('12','Manufactured Object',mnob).
semtype_translation_fact('12','Medical Device',medd).
semtype_translation_fact('12','Mental Process',menp).
semtype_translation_fact('12','Mental or Behavioral Dysfunction',mobd).
semtype_translation_fact('12','Molecular Biology Research Technique',mbrt).
semtype_translation_fact('12','Molecular Function',moft).
semtype_translation_fact('12','Molecular Sequence',mosq).
semtype_translation_fact('12','Natural Phenomenon or Process',npop).
semtype_translation_fact('12','Neoplastic Process',neop).
semtype_translation_fact('12','Neuroreactive Substance or Biogenic Amine',nsba).
semtype_translation_fact('12','Nucleic Acid, Nucleoside, or Nucleotide',nnon).
semtype_translation_fact('12','Nucleotide Sequence',nusq).
semtype_translation_fact('12','Object',objt).
semtype_translation_fact('12','Occupation or Discipline',ocdi).
semtype_translation_fact('12','Occupational Activity',ocac).
semtype_translation_fact('12','Organ or Tissue Function',ortf).
semtype_translation_fact('12','Organic Chemical',orch).
semtype_translation_fact('12','Organism Attribute',orga).
semtype_translation_fact('12','Organism Function',orgf).
semtype_translation_fact('12','Organism',orgm).
semtype_translation_fact('12','Organization',orgt).
semtype_translation_fact('12','Organophosphorus Compound',opco).
semtype_translation_fact('12','Pathologic Function',patf).
semtype_translation_fact('12','Patient or Disabled Group',podg).
semtype_translation_fact('12','Pharmacologic Substance',phsu).
semtype_translation_fact('12','Phenomenon or Process',phpr).
semtype_translation_fact('12','Physical Object',phob).
semtype_translation_fact('12','Physiologic Function',phsf).
semtype_translation_fact('12','Plant',plnt).
semtype_translation_fact('12','Population Group',popg).
semtype_translation_fact('12','Professional Society',pros).
semtype_translation_fact('12','Professional or Occupational Group',prog).
semtype_translation_fact('12','Qualitative Concept',qlco).
semtype_translation_fact('12','Quantitative Concept',qnco).
semtype_translation_fact('12','Receptor',rcpt).
semtype_translation_fact('12','Regulation or Law',rnlw).
semtype_translation_fact('12','Reptile',rept).
semtype_translation_fact('12','Research Activity',resa).
semtype_translation_fact('12','Research Device',resd).
semtype_translation_fact('12','Self-help or Relief Organization',shro).
semtype_translation_fact('12','Sign or Symptom',sosy).
semtype_translation_fact('12','Social Behavior',socb).
semtype_translation_fact('12','Spatial Concept',spco).
semtype_translation_fact('12','Steroid',strd).
semtype_translation_fact('12','Substance',sbst).
semtype_translation_fact('12','Temporal Concept',tmco).
semtype_translation_fact('12','Therapeutic or Preventive Procedure',topp).
semtype_translation_fact('12','Tissue',tisu).
semtype_translation_fact('12','Vertebrate',vtbt).
semtype_translation_fact('12','Virus',virs).
semtype_translation_fact('12','Vitamin',vita).
semtype_translation_fact('14','Acquired Abnormality',acab).
semtype_translation_fact('14','Activity',acty).
semtype_translation_fact('14','Age Group',aggp).
semtype_translation_fact('14','Amino Acid Sequence',amas).
semtype_translation_fact('14','Amino Acid, Peptide, or Protein',aapp).
semtype_translation_fact('14','Amphibian',amph).
semtype_translation_fact('14','Anatomical Abnormality',anab).
semtype_translation_fact('14','Anatomical Structure',anst).
semtype_translation_fact('14','Animal',anim).
semtype_translation_fact('14','Antibiotic',antb).
semtype_translation_fact('14','Archaeon',arch).
semtype_translation_fact('14','Bacterium',bact).
semtype_translation_fact('14','Behavior',bhvr).
semtype_translation_fact('14','Biologic Function',biof).
semtype_translation_fact('14','Biologically Active Substance',bacs).
semtype_translation_fact('14','Biomedical Occupation or Discipline',bmod).
semtype_translation_fact('14','Biomedical or Dental Material',bodm).
semtype_translation_fact('14','Bird',bird).
semtype_translation_fact('14','Body Location or Region',blor).
semtype_translation_fact('14','Body Part, Organ, or Organ Component',bpoc).
semtype_translation_fact('14','Body Space or Junction',bsoj).
semtype_translation_fact('14','Body Substance',bdsu).
semtype_translation_fact('14','Body System',bdsy).
semtype_translation_fact('14','Carbohydrate Sequence',crbs).
semtype_translation_fact('14','Carbohydrate',carb).
semtype_translation_fact('14','Cell Component',celc).
semtype_translation_fact('14','Cell Function',celf).
semtype_translation_fact('14','Cell or Molecular Dysfunction',comd).
semtype_translation_fact('14','Cell',cell).
semtype_translation_fact('14','Chemical Viewed Functionally',chvf).
semtype_translation_fact('14','Chemical Viewed Structurally',chvs).
semtype_translation_fact('14','Chemical',chem).
semtype_translation_fact('14','Classification',clas).
semtype_translation_fact('14','Clinical Attribute',clna).
semtype_translation_fact('14','Clinical Drug',clnd).
semtype_translation_fact('14','Conceptual Entity',cnce).
semtype_translation_fact('14','Congenital Abnormality',cgab).
semtype_translation_fact('14','Daily or Recreational Activity',dora).
semtype_translation_fact('14','Diagnostic Procedure',diap).
semtype_translation_fact('14','Disease or Syndrome',dsyn).
semtype_translation_fact('14','Drug Delivery Device',drdd).
semtype_translation_fact('14','Educational Activity',edac).
semtype_translation_fact('14','Eicosanoid',eico).
semtype_translation_fact('14','Element, Ion, or Isotope',elii).
semtype_translation_fact('14','Embryonic Structure',emst).
semtype_translation_fact('14','Entity',enty).
semtype_translation_fact('14','Environmental Effect of Humans',eehu).
semtype_translation_fact('14','Enzyme',enzy).
semtype_translation_fact('14','Eukaryote',euka).
semtype_translation_fact('14','Event',evnt).
semtype_translation_fact('14','Experimental Model of Disease',emod).
semtype_translation_fact('14','Family Group',famg).
semtype_translation_fact('14','Finding',fndg).
semtype_translation_fact('14','Fish',fish).
semtype_translation_fact('14','Food',food).
semtype_translation_fact('14','Fully Formed Anatomical Structure',ffas).
semtype_translation_fact('14','Functional Concept',ftcn).
semtype_translation_fact('14','Fungus',fngs).
semtype_translation_fact('14','Gene or Gene Product',gngp).
semtype_translation_fact('14','Gene or Genome',gngm).
semtype_translation_fact('14','Genetic Function',genf).
semtype_translation_fact('14','Geographic Area',geoa).
semtype_translation_fact('14','Governmental or Regulatory Activity',gora).
semtype_translation_fact('14','Group Attribute',grpa).
semtype_translation_fact('14','Group',grup).
semtype_translation_fact('14','Hazardous or Poisonous Substance',hops).
semtype_translation_fact('14','Health Care Activity',hlca).
semtype_translation_fact('14','Health Care Related Organization',hcro).
semtype_translation_fact('14','Hormone',horm).
semtype_translation_fact('14','Human',humn).
semtype_translation_fact('14','Human-caused Phenomenon or Process',hcpp).
semtype_translation_fact('14','Idea or Concept',idcn).
semtype_translation_fact('14','Immunologic Factor',imft).
semtype_translation_fact('14','Indicator, Reagent, or Diagnostic Aid',irda).
semtype_translation_fact('14','Individual Behavior',inbe).
semtype_translation_fact('14','Injury or Poisoning',inpo).
semtype_translation_fact('14','Inorganic Chemical',inch).
semtype_translation_fact('14','Intellectual Product',inpr).
semtype_translation_fact('14','Laboratory Procedure',lbpr).
semtype_translation_fact('14','Laboratory or Test Result',lbtr).
semtype_translation_fact('14','Language',lang).
semtype_translation_fact('14','Lipid',lipd).
semtype_translation_fact('14','Machine Activity',mcha).
semtype_translation_fact('14','Mammal',mamm).
semtype_translation_fact('14','Manufactured Object',mnob).
semtype_translation_fact('14','Medical Device',medd).
semtype_translation_fact('14','Mental Process',menp).
semtype_translation_fact('14','Mental or Behavioral Dysfunction',mobd).
semtype_translation_fact('14','Molecular Biology Research Technique',mbrt).
semtype_translation_fact('14','Molecular Function',moft).
semtype_translation_fact('14','Molecular Sequence',mosq).
semtype_translation_fact('14','Natural Phenomenon or Process',npop).
semtype_translation_fact('14','Neoplastic Process',neop).
semtype_translation_fact('14','Neuroreactive Substance or Biogenic Amine',nsba).
semtype_translation_fact('14','Nucleic Acid, Nucleoside, or Nucleotide',nnon).
semtype_translation_fact('14','Nucleotide Sequence',nusq).
semtype_translation_fact('14','Object',objt).
semtype_translation_fact('14','Occupation or Discipline',ocdi).
semtype_translation_fact('14','Occupational Activity',ocac).
semtype_translation_fact('14','Organ or Tissue Function',ortf).
semtype_translation_fact('14','Organic Chemical',orch).
semtype_translation_fact('14','Organism Attribute',orga).
semtype_translation_fact('14','Organism Function',orgf).
semtype_translation_fact('14','Organism',orgm).
semtype_translation_fact('14','Organization',orgt).
semtype_translation_fact('14','Organophosphorus Compound',opco).
semtype_translation_fact('14','Pathologic Function',patf).
semtype_translation_fact('14','Patient or Disabled Group',podg).
semtype_translation_fact('14','Pharmacologic Substance',phsu).
semtype_translation_fact('14','Phenomenon or Process',phpr).
semtype_translation_fact('14','Physical Object',phob).
semtype_translation_fact('14','Physiologic Function',phsf).
semtype_translation_fact('14','Plant',plnt).
semtype_translation_fact('14','Population Group',popg).
semtype_translation_fact('14','Professional Society',pros).
semtype_translation_fact('14','Professional or Occupational Group',prog).
semtype_translation_fact('14','Qualitative Concept',qlco).
semtype_translation_fact('14','Quantitative Concept',qnco).
semtype_translation_fact('14','Receptor',rcpt).
semtype_translation_fact('14','Regulation or Law',rnlw).
semtype_translation_fact('14','Reptile',rept).
semtype_translation_fact('14','Research Activity',resa).
semtype_translation_fact('14','Research Device',resd).
semtype_translation_fact('14','Self-help or Relief Organization',shro).
semtype_translation_fact('14','Sign or Symptom',sosy).
semtype_translation_fact('14','Social Behavior',socb).
semtype_translation_fact('14','Spatial Concept',spco).
semtype_translation_fact('14','Steroid',strd).
semtype_translation_fact('14','Substance',sbst).
semtype_translation_fact('14','Temporal Concept',tmco).
semtype_translation_fact('14','Therapeutic or Preventive Procedure',topp).
semtype_translation_fact('14','Tissue',tisu).
semtype_translation_fact('14','Vertebrate',vtbt).
semtype_translation_fact('14','Virus',virs).
semtype_translation_fact('14','Vitamin',vita).
semtype_translation_fact('15','Acquired Abnormality',acab).
semtype_translation_fact('15','Activity',acty).
semtype_translation_fact('15','Age Group',aggp).
semtype_translation_fact('15','Amino Acid Sequence',amas).
semtype_translation_fact('15','Amino Acid, Peptide, or Protein',aapp).
semtype_translation_fact('15','Amphibian',amph).
semtype_translation_fact('15','Anatomical Abnormality',anab).
semtype_translation_fact('15','Anatomical Structure',anst).
semtype_translation_fact('15','Animal',anim).
semtype_translation_fact('15','Antibiotic',antb).
semtype_translation_fact('15','Archaeon',arch).
semtype_translation_fact('15','Bacterium',bact).
semtype_translation_fact('15','Behavior',bhvr).
semtype_translation_fact('15','Biologic Function',biof).
semtype_translation_fact('15','Biologically Active Substance',bacs).
semtype_translation_fact('15','Biomedical Occupation or Discipline',bmod).
semtype_translation_fact('15','Biomedical or Dental Material',bodm).
semtype_translation_fact('15','Bird',bird).
semtype_translation_fact('15','Body Location or Region',blor).
semtype_translation_fact('15','Body Part, Organ, or Organ Component',bpoc).
semtype_translation_fact('15','Body Space or Junction',bsoj).
semtype_translation_fact('15','Body Substance',bdsu).
semtype_translation_fact('15','Body System',bdsy).
semtype_translation_fact('15','Carbohydrate Sequence',crbs).
semtype_translation_fact('15','Cell Component',celc).
semtype_translation_fact('15','Cell Function',celf).
semtype_translation_fact('15','Cell or Molecular Dysfunction',comd).
semtype_translation_fact('15','Cell',cell).
semtype_translation_fact('15','Chemical Viewed Functionally',chvf).
semtype_translation_fact('15','Chemical Viewed Structurally',chvs).
semtype_translation_fact('15','Chemical',chem).
semtype_translation_fact('15','Classification',clas).
semtype_translation_fact('15','Clinical Attribute',clna).
semtype_translation_fact('15','Clinical Drug',clnd).
semtype_translation_fact('15','Conceptual Entity',cnce).
semtype_translation_fact('15','Congenital Abnormality',cgab).
semtype_translation_fact('15','Daily or Recreational Activity',dora).
semtype_translation_fact('15','Diagnostic Procedure',diap).
semtype_translation_fact('15','Disease or Syndrome',dsyn).
semtype_translation_fact('15','Drug Delivery Device',drdd).
semtype_translation_fact('15','Educational Activity',edac).
semtype_translation_fact('15','Element, Ion, or Isotope',elii).
semtype_translation_fact('15','Embryonic Structure',emst).
semtype_translation_fact('15','Entity',enty).
semtype_translation_fact('15','Environmental Effect of Humans',eehu).
semtype_translation_fact('15','Enzyme',enzy).
semtype_translation_fact('15','Eukaryote',euka).
semtype_translation_fact('15','Event',evnt).
semtype_translation_fact('15','Experimental Model of Disease',emod).
semtype_translation_fact('15','Family Group',famg).
semtype_translation_fact('15','Finding',fndg).
semtype_translation_fact('15','Fish',fish).
semtype_translation_fact('15','Food',food).
semtype_translation_fact('15','Fully Formed Anatomical Structure',ffas).
semtype_translation_fact('15','Functional Concept',ftcn).
semtype_translation_fact('15','Fungus',fngs).
semtype_translation_fact('15','Gene or Gene Product',gngp).
semtype_translation_fact('15','Gene or Genome',gngm).
semtype_translation_fact('15','Genetic Function',genf).
semtype_translation_fact('15','Geographic Area',geoa).
semtype_translation_fact('15','Governmental or Regulatory Activity',gora).
semtype_translation_fact('15','Group Attribute',grpa).
semtype_translation_fact('15','Group',grup).
semtype_translation_fact('15','Hazardous or Poisonous Substance',hops).
semtype_translation_fact('15','Health Care Activity',hlca).
semtype_translation_fact('15','Health Care Related Organization',hcro).
semtype_translation_fact('15','Hormone',horm).
semtype_translation_fact('15','Human',humn).
semtype_translation_fact('15','Human-caused Phenomenon or Process',hcpp).
semtype_translation_fact('15','Idea or Concept',idcn).
semtype_translation_fact('15','Immunologic Factor',imft).
semtype_translation_fact('15','Indicator, Reagent, or Diagnostic Aid',irda).
semtype_translation_fact('15','Individual Behavior',inbe).
semtype_translation_fact('15','Injury or Poisoning',inpo).
semtype_translation_fact('15','Inorganic Chemical',inch).
semtype_translation_fact('15','Intellectual Product',inpr).
semtype_translation_fact('15','Laboratory Procedure',lbpr).
semtype_translation_fact('15','Laboratory or Test Result',lbtr).
semtype_translation_fact('15','Language',lang).
semtype_translation_fact('15','Machine Activity',mcha).
semtype_translation_fact('15','Mammal',mamm).
semtype_translation_fact('15','Manufactured Object',mnob).
semtype_translation_fact('15','Medical Device',medd).
semtype_translation_fact('15','Mental Process',menp).
semtype_translation_fact('15','Mental or Behavioral Dysfunction',mobd).
semtype_translation_fact('15','Molecular Biology Research Technique',mbrt).
semtype_translation_fact('15','Molecular Function',moft).
semtype_translation_fact('15','Molecular Sequence',mosq).
semtype_translation_fact('15','Natural Phenomenon or Process',npop).
semtype_translation_fact('15','Neoplastic Process',neop).
semtype_translation_fact('15','Nucleic Acid, Nucleoside, or Nucleotide',nnon).
semtype_translation_fact('15','Nucleotide Sequence',nusq).
semtype_translation_fact('15','Object',objt).
semtype_translation_fact('15','Occupation or Discipline',ocdi).
semtype_translation_fact('15','Occupational Activity',ocac).
semtype_translation_fact('15','Organ or Tissue Function',ortf).
semtype_translation_fact('15','Organic Chemical',orch).
semtype_translation_fact('15','Organism Attribute',orga).
semtype_translation_fact('15','Organism Function',orgf).
semtype_translation_fact('15','Organism',orgm).
semtype_translation_fact('15','Organization',orgt).
semtype_translation_fact('15','Pathologic Function',patf).
semtype_translation_fact('15','Patient or Disabled Group',podg).
semtype_translation_fact('15','Pharmacologic Substance',phsu).
semtype_translation_fact('15','Phenomenon or Process',phpr).
semtype_translation_fact('15','Physical Object',phob).
semtype_translation_fact('15','Physiologic Function',phsf).
semtype_translation_fact('15','Plant',plnt).
semtype_translation_fact('15','Population Group',popg).
semtype_translation_fact('15','Professional Society',pros).
semtype_translation_fact('15','Professional or Occupational Group',prog).
semtype_translation_fact('15','Qualitative Concept',qlco).
semtype_translation_fact('15','Quantitative Concept',qnco).
semtype_translation_fact('15','Receptor',rcpt).
semtype_translation_fact('15','Regulation or Law',rnlw).
semtype_translation_fact('15','Reptile',rept).
semtype_translation_fact('15','Research Activity',resa).
semtype_translation_fact('15','Research Device',resd).
semtype_translation_fact('15','Self-help or Relief Organization',shro).
semtype_translation_fact('15','Sign or Symptom',sosy).
semtype_translation_fact('15','Social Behavior',socb).
semtype_translation_fact('15','Spatial Concept',spco).
semtype_translation_fact('15','Substance',sbst).
semtype_translation_fact('15','Temporal Concept',tmco).
semtype_translation_fact('15','Therapeutic or Preventive Procedure',topp).
semtype_translation_fact('15','Tissue',tisu).
semtype_translation_fact('15','Vertebrate',vtbt).
semtype_translation_fact('15','Virus',virs).
semtype_translation_fact('15','Vitamin',vita).
semtype_translation_fact('18','Acquired Abnormality',acab).
semtype_translation_fact('18','Activity',acty).
semtype_translation_fact('18','Age Group',aggp).
semtype_translation_fact('18','Amino Acid Sequence',amas).
semtype_translation_fact('18','Amino Acid, Peptide, or Protein',aapp).
semtype_translation_fact('18','Amphibian',amph).
semtype_translation_fact('18','Anatomical Abnormality',anab).
semtype_translation_fact('18','Anatomical Structure',anst).
semtype_translation_fact('18','Animal',anim).
semtype_translation_fact('18','Antibiotic',antb).
semtype_translation_fact('18','Archaeon',arch).
semtype_translation_fact('18','Bacterium',bact).
semtype_translation_fact('18','Behavior',bhvr).
semtype_translation_fact('18','Biologic Function',biof).
semtype_translation_fact('18','Biologically Active Substance',bacs).
semtype_translation_fact('18','Biomedical Occupation or Discipline',bmod).
semtype_translation_fact('18','Biomedical or Dental Material',bodm).
semtype_translation_fact('18','Bird',bird).
semtype_translation_fact('18','Body Location or Region',blor).
semtype_translation_fact('18','Body Part, Organ, or Organ Component',bpoc).
semtype_translation_fact('18','Body Space or Junction',bsoj).
semtype_translation_fact('18','Body Substance',bdsu).
semtype_translation_fact('18','Body System',bdsy).
semtype_translation_fact('18','Carbohydrate Sequence',crbs).
semtype_translation_fact('18','Cell Component',celc).
semtype_translation_fact('18','Cell Function',celf).
semtype_translation_fact('18','Cell or Molecular Dysfunction',comd).
semtype_translation_fact('18','Cell',cell).
semtype_translation_fact('18','Chemical Viewed Functionally',chvf).
semtype_translation_fact('18','Chemical Viewed Structurally',chvs).
semtype_translation_fact('18','Chemical',chem).
semtype_translation_fact('18','Classification',clas).
semtype_translation_fact('18','Clinical Attribute',clna).
semtype_translation_fact('18','Clinical Drug',clnd).
semtype_translation_fact('18','Conceptual Entity',cnce).
semtype_translation_fact('18','Congenital Abnormality',cgab).
semtype_translation_fact('18','Daily or Recreational Activity',dora).
semtype_translation_fact('18','Diagnostic Procedure',diap).
semtype_translation_fact('18','Disease or Syndrome',dsyn).
semtype_translation_fact('18','Drug Delivery Device',drdd).
semtype_translation_fact('18','Educational Activity',edac).
semtype_translation_fact('18','Element, Ion, or Isotope',elii).
semtype_translation_fact('18','Embryonic Structure',emst).
semtype_translation_fact('18','Entity',enty).
semtype_translation_fact('18','Environmental Effect of Humans',eehu).
semtype_translation_fact('18','Enzyme',enzy).
semtype_translation_fact('18','Eukaryote',euka).
semtype_translation_fact('18','Event',evnt).
semtype_translation_fact('18','Experimental Model of Disease',emod).
semtype_translation_fact('18','Family Group',famg).
semtype_translation_fact('18','Finding',fndg).
semtype_translation_fact('18','Fish',fish).
semtype_translation_fact('18','Food',food).
semtype_translation_fact('18','Fully Formed Anatomical Structure',ffas).
semtype_translation_fact('18','Functional Concept',ftcn).
semtype_translation_fact('18','Fungus',fngs).
semtype_translation_fact('18','Gene or Gene Product',gngp).
semtype_translation_fact('18','Gene or Genome',gngm).
semtype_translation_fact('18','Genetic Function',genf).
semtype_translation_fact('18','Geographic Area',geoa).
semtype_translation_fact('18','Governmental or Regulatory Activity',gora).
semtype_translation_fact('18','Group Attribute',grpa).
semtype_translation_fact('18','Group',grup).
semtype_translation_fact('18','Hazardous or Poisonous Substance',hops).
semtype_translation_fact('18','Health Care Activity',hlca).
semtype_translation_fact('18','Health Care Related Organization',hcro).
semtype_translation_fact('18','Hormone',horm).
semtype_translation_fact('18','Human',humn).
semtype_translation_fact('18','Human-caused Phenomenon or Process',hcpp).
semtype_translation_fact('18','Idea or Concept',idcn).
semtype_translation_fact('18','Immunologic Factor',imft).
semtype_translation_fact('18','Indicator, Reagent, or Diagnostic Aid',irda).
semtype_translation_fact('18','Individual Behavior',inbe).
semtype_translation_fact('18','Injury or Poisoning',inpo).
semtype_translation_fact('18','Inorganic Chemical',inch).
semtype_translation_fact('18','Intellectual Product',inpr).
semtype_translation_fact('18','Laboratory Procedure',lbpr).
semtype_translation_fact('18','Laboratory or Test Result',lbtr).
semtype_translation_fact('18','Language',lang).
semtype_translation_fact('18','Machine Activity',mcha).
semtype_translation_fact('18','Mammal',mamm).
semtype_translation_fact('18','Manufactured Object',mnob).
semtype_translation_fact('18','Medical Device',medd).
semtype_translation_fact('18','Mental Process',menp).
semtype_translation_fact('18','Mental or Behavioral Dysfunction',mobd).
semtype_translation_fact('18','Molecular Biology Research Technique',mbrt).
semtype_translation_fact('18','Molecular Function',moft).
semtype_translation_fact('18','Molecular Sequence',mosq).
semtype_translation_fact('18','Natural Phenomenon or Process',npop).
semtype_translation_fact('18','Neoplastic Process',neop).
semtype_translation_fact('18','Nucleic Acid, Nucleoside, or Nucleotide',nnon).
semtype_translation_fact('18','Nucleotide Sequence',nusq).
semtype_translation_fact('18','Object',objt).
semtype_translation_fact('18','Occupation or Discipline',ocdi).
semtype_translation_fact('18','Occupational Activity',ocac).
semtype_translation_fact('18','Organ or Tissue Function',ortf).
semtype_translation_fact('18','Organic Chemical',orch).
semtype_translation_fact('18','Organism Attribute',orga).
semtype_translation_fact('18','Organism Function',orgf).
semtype_translation_fact('18','Organism',orgm).
semtype_translation_fact('18','Organization',orgt).
semtype_translation_fact('18','Pathologic Function',patf).
semtype_translation_fact('18','Patient or Disabled Group',podg).
semtype_translation_fact('18','Pharmacologic Substance',phsu).
semtype_translation_fact('18','Phenomenon or Process',phpr).
semtype_translation_fact('18','Physical Object',phob).
semtype_translation_fact('18','Physiologic Function',phsf).
semtype_translation_fact('18','Plant',plnt).
semtype_translation_fact('18','Population Group',popg).
semtype_translation_fact('18','Professional Society',pros).
semtype_translation_fact('18','Professional or Occupational Group',prog).
semtype_translation_fact('18','Qualitative Concept',qlco).
semtype_translation_fact('18','Quantitative Concept',qnco).
semtype_translation_fact('18','Receptor',rcpt).
semtype_translation_fact('18','Regulation or Law',rnlw).
semtype_translation_fact('18','Reptile',rept).
semtype_translation_fact('18','Research Activity',resa).
semtype_translation_fact('18','Research Device',resd).
semtype_translation_fact('18','Self-help or Relief Organization',shro).
semtype_translation_fact('18','Sign or Symptom',sosy).
semtype_translation_fact('18','Social Behavior',socb).
semtype_translation_fact('18','Spatial Concept',spco).
semtype_translation_fact('18','Substance',sbst).
semtype_translation_fact('18','Temporal Concept',tmco).
semtype_translation_fact('18','Therapeutic or Preventive Procedure',topp).
semtype_translation_fact('18','Tissue',tisu).
semtype_translation_fact('18','Vertebrate',vtbt).
semtype_translation_fact('18','Virus',virs).
semtype_translation_fact('18','Vitamin',vita).


semtype_translation_reverse('06',acab,'Acquired Abnormality').
semtype_translation_reverse('06',acty,'Activity').
semtype_translation_reverse('06',aggp,'Age Group').
semtype_translation_reverse('06',alga,'Alga').
semtype_translation_reverse('06',amas,'Amino Acid Sequence').
semtype_translation_reverse('06',aapp,'Amino Acid, Peptide, or Protein').
semtype_translation_reverse('06',amph,'Amphibian').
semtype_translation_reverse('06',anab,'Anatomical Abnormality').
semtype_translation_reverse('06',anst,'Anatomical Structure').
semtype_translation_reverse('06',anim,'Animal').
semtype_translation_reverse('06',antb,'Antibiotic').
semtype_translation_reverse('06',arch,'Archaeon').
semtype_translation_reverse('06',bact,'Bacterium').
semtype_translation_reverse('06',bhvr,'Behavior').
semtype_translation_reverse('06',biof,'Biologic Function').
semtype_translation_reverse('06',bacs,'Biologically Active Substance').
semtype_translation_reverse('06',bmod,'Biomedical Occupation or Discipline').
semtype_translation_reverse('06',bodm,'Biomedical or Dental Material').
semtype_translation_reverse('06',bird,'Bird').
semtype_translation_reverse('06',blor,'Body Location or Region').
semtype_translation_reverse('06',bpoc,'Body Part, Organ, or Organ Component').
semtype_translation_reverse('06',bsoj,'Body Space or Junction').
semtype_translation_reverse('06',bdsu,'Body Substance').
semtype_translation_reverse('06',bdsy,'Body System').
semtype_translation_reverse('06',carb,'Carbohydrate').
semtype_translation_reverse('06',crbs,'Carbohydrate Sequence').
semtype_translation_reverse('06',cell,'Cell').
semtype_translation_reverse('06',celc,'Cell Component').
semtype_translation_reverse('06',celf,'Cell Function').
semtype_translation_reverse('06',comd,'Cell or Molecular Dysfunction').
semtype_translation_reverse('06',chem,'Chemical').
semtype_translation_reverse('06',chvf,'Chemical Viewed Functionally').
semtype_translation_reverse('06',chvs,'Chemical Viewed Structurally').
semtype_translation_reverse('06',clas,'Classification').
semtype_translation_reverse('06',clna,'Clinical Attribute').
semtype_translation_reverse('06',clnd,'Clinical Drug').
semtype_translation_reverse('06',cnce,'Conceptual Entity').
semtype_translation_reverse('06',cgab,'Congenital Abnormality').
semtype_translation_reverse('06',dora,'Daily or Recreational Activity').
semtype_translation_reverse('06',diap,'Diagnostic Procedure').
semtype_translation_reverse('06',dsyn,'Disease or Syndrome').
semtype_translation_reverse('06',drdd,'Drug Delivery Device').
semtype_translation_reverse('06',edac,'Educational Activity').
semtype_translation_reverse('06',eico,'Eicosanoid').
semtype_translation_reverse('06',elii,'Element, Ion, or Isotope').
semtype_translation_reverse('06',emst,'Embryonic Structure').
semtype_translation_reverse('06',enty,'Entity').
semtype_translation_reverse('06',eehu,'Environmental Effect of Humans').
semtype_translation_reverse('06',enzy,'Enzyme').
semtype_translation_reverse('06',evnt,'Event').
semtype_translation_reverse('06',emod,'Experimental Model of Disease').
semtype_translation_reverse('06',famg,'Family Group').
semtype_translation_reverse('06',fndg,'Finding').
semtype_translation_reverse('06',fish,'Fish').
semtype_translation_reverse('06',food,'Food').
semtype_translation_reverse('06',ffas,'Fully Formed Anatomical Structure').
semtype_translation_reverse('06',ftcn,'Functional Concept').
semtype_translation_reverse('06',fngs,'Fungus').
semtype_translation_reverse('06',gngp,'Gene or Gene Product').
semtype_translation_reverse('06',gngm,'Gene or Genome').
semtype_translation_reverse('06',genf,'Genetic Function').
semtype_translation_reverse('06',geoa,'Geographic Area').
semtype_translation_reverse('06',gora,'Governmental or Regulatory Activity').
semtype_translation_reverse('06',grup,'Group').
semtype_translation_reverse('06',grpa,'Group Attribute').
semtype_translation_reverse('06',hops,'Hazardous or Poisonous Substance').
semtype_translation_reverse('06',hlca,'Health Care Activity').
semtype_translation_reverse('06',hcro,'Health Care Related Organization').
semtype_translation_reverse('06',horm,'Hormone').
semtype_translation_reverse('06',humn,'Human').
semtype_translation_reverse('06',hcpp,'Human-caused Phenomenon or Process').
semtype_translation_reverse('06',idcn,'Idea or Concept').
semtype_translation_reverse('06',imft,'Immunologic Factor').
semtype_translation_reverse('06',irda,'Indicator, Reagent, or Diagnostic Aid').
semtype_translation_reverse('06',inbe,'Individual Behavior').
semtype_translation_reverse('06',inpo,'Injury or Poisoning').
semtype_translation_reverse('06',inch,'Inorganic Chemical').
semtype_translation_reverse('06',inpr,'Intellectual Product').
semtype_translation_reverse('06',invt,'Invertebrate').
semtype_translation_reverse('06',lbpr,'Laboratory Procedure').
semtype_translation_reverse('06',lbtr,'Laboratory or Test Result').
semtype_translation_reverse('06',lang,'Language').
semtype_translation_reverse('06',lipd,'Lipid').
semtype_translation_reverse('06',mcha,'Machine Activity').
semtype_translation_reverse('06',mamm,'Mammal').
semtype_translation_reverse('06',mnob,'Manufactured Object').
semtype_translation_reverse('06',medd,'Medical Device').
semtype_translation_reverse('06',menp,'Mental Process').
semtype_translation_reverse('06',mobd,'Mental or Behavioral Dysfunction').
semtype_translation_reverse('06',mbrt,'Molecular Biology Research Technique').
semtype_translation_reverse('06',moft,'Molecular Function').
semtype_translation_reverse('06',mosq,'Molecular Sequence').
semtype_translation_reverse('06',npop,'Natural Phenomenon or Process').
semtype_translation_reverse('06',neop,'Neoplastic Process').
semtype_translation_reverse('06',nsba,'Neuroreactive Substance or Biogenic Amine').
semtype_translation_reverse('06',nnon,'Nucleic Acid, Nucleoside, or Nucleotide').
semtype_translation_reverse('06',nusq,'Nucleotide Sequence').
semtype_translation_reverse('06',objt,'Object').
semtype_translation_reverse('06',ocdi,'Occupation or Discipline').
semtype_translation_reverse('06',ocac,'Occupational Activity').
semtype_translation_reverse('06',ortf,'Organ or Tissue Function').
semtype_translation_reverse('06',orch,'Organic Chemical').
semtype_translation_reverse('06',orgm,'Organism').
semtype_translation_reverse('06',orga,'Organism Attribute').
semtype_translation_reverse('06',orgf,'Organism Function').
semtype_translation_reverse('06',orgt,'Organization').
semtype_translation_reverse('06',opco,'Organophosphorus Compound').
semtype_translation_reverse('06',patf,'Pathologic Function').
semtype_translation_reverse('06',podg,'Patient or Disabled Group').
semtype_translation_reverse('06',phsu,'Pharmacologic Substance').
semtype_translation_reverse('06',phpr,'Phenomenon or Process').
semtype_translation_reverse('06',phob,'Physical Object').
semtype_translation_reverse('06',phsf,'Physiologic Function').
semtype_translation_reverse('06',plnt,'Plant').
semtype_translation_reverse('06',popg,'Population Group').
semtype_translation_reverse('06',pros,'Professional Society').
semtype_translation_reverse('06',prog,'Professional or Occupational Group').
semtype_translation_reverse('06',qlco,'Qualitative Concept').
semtype_translation_reverse('06',qnco,'Quantitative Concept').
semtype_translation_reverse('06',rcpt,'Receptor').
semtype_translation_reverse('06',rnlw,'Regulation or Law').
semtype_translation_reverse('06',rept,'Reptile').
semtype_translation_reverse('06',resa,'Research Activity').
semtype_translation_reverse('06',resd,'Research Device').
semtype_translation_reverse('06',rich,'Rickettsia or Chlamydia').
semtype_translation_reverse('06',shro,'Self-help or Relief Organization').
semtype_translation_reverse('06',sosy,'Sign or Symptom').
semtype_translation_reverse('06',socb,'Social Behavior').
semtype_translation_reverse('06',spco,'Spatial Concept').
semtype_translation_reverse('06',strd,'Steroid').
semtype_translation_reverse('06',sbst,'Substance').
semtype_translation_reverse('06',tmco,'Temporal Concept').
semtype_translation_reverse('06',topp,'Therapeutic or Preventive Procedure').
semtype_translation_reverse('06',tisu,'Tissue').
semtype_translation_reverse('06',vtbt,'Vertebrate').
semtype_translation_reverse('06',virs,'Virus').
semtype_translation_reverse('06',vita,'Vitamin').
semtype_translation_reverse('12',acab,'Acquired Abnormality').
semtype_translation_reverse('12',acty,'Activity').
semtype_translation_reverse('12',aggp,'Age Group').
semtype_translation_reverse('12',amas,'Amino Acid Sequence').
semtype_translation_reverse('12',aapp,'Amino Acid, Peptide, or Protein').
semtype_translation_reverse('12',amph,'Amphibian').
semtype_translation_reverse('12',anab,'Anatomical Abnormality').
semtype_translation_reverse('12',anst,'Anatomical Structure').
semtype_translation_reverse('12',anim,'Animal').
semtype_translation_reverse('12',antb,'Antibiotic').
semtype_translation_reverse('12',arch,'Archaeon').
semtype_translation_reverse('12',bact,'Bacterium').
semtype_translation_reverse('12',bhvr,'Behavior').
semtype_translation_reverse('12',biof,'Biologic Function').
semtype_translation_reverse('12',bacs,'Biologically Active Substance').
semtype_translation_reverse('12',bmod,'Biomedical Occupation or Discipline').
semtype_translation_reverse('12',bodm,'Biomedical or Dental Material').
semtype_translation_reverse('12',bird,'Bird').
semtype_translation_reverse('12',blor,'Body Location or Region').
semtype_translation_reverse('12',bpoc,'Body Part, Organ, or Organ Component').
semtype_translation_reverse('12',bsoj,'Body Space or Junction').
semtype_translation_reverse('12',bdsu,'Body Substance').
semtype_translation_reverse('12',bdsy,'Body System').
semtype_translation_reverse('12',crbs,'Carbohydrate Sequence').
semtype_translation_reverse('12',carb,'Carbohydrate').
semtype_translation_reverse('12',celc,'Cell Component').
semtype_translation_reverse('12',celf,'Cell Function').
semtype_translation_reverse('12',comd,'Cell or Molecular Dysfunction').
semtype_translation_reverse('12',cell,'Cell').
semtype_translation_reverse('12',chvf,'Chemical Viewed Functionally').
semtype_translation_reverse('12',chvs,'Chemical Viewed Structurally').
semtype_translation_reverse('12',chem,'Chemical').
semtype_translation_reverse('12',clas,'Classification').
semtype_translation_reverse('12',clna,'Clinical Attribute').
semtype_translation_reverse('12',clnd,'Clinical Drug').
semtype_translation_reverse('12',cnce,'Conceptual Entity').
semtype_translation_reverse('12',cgab,'Congenital Abnormality').
semtype_translation_reverse('12',dora,'Daily or Recreational Activity').
semtype_translation_reverse('12',diap,'Diagnostic Procedure').
semtype_translation_reverse('12',dsyn,'Disease or Syndrome').
semtype_translation_reverse('12',drdd,'Drug Delivery Device').
semtype_translation_reverse('12',edac,'Educational Activity').
semtype_translation_reverse('12',eico,'Eicosanoid').
semtype_translation_reverse('12',elii,'Element, Ion, or Isotope').
semtype_translation_reverse('12',emst,'Embryonic Structure').
semtype_translation_reverse('12',enty,'Entity').
semtype_translation_reverse('12',eehu,'Environmental Effect of Humans').
semtype_translation_reverse('12',enzy,'Enzyme').
semtype_translation_reverse('12',euka,'Eukaryote').
semtype_translation_reverse('12',evnt,'Event').
semtype_translation_reverse('12',emod,'Experimental Model of Disease').
semtype_translation_reverse('12',famg,'Family Group').
semtype_translation_reverse('12',fndg,'Finding').
semtype_translation_reverse('12',fish,'Fish').
semtype_translation_reverse('12',food,'Food').
semtype_translation_reverse('12',ffas,'Fully Formed Anatomical Structure').
semtype_translation_reverse('12',ftcn,'Functional Concept').
semtype_translation_reverse('12',fngs,'Fungus').
semtype_translation_reverse('12',gngp,'Gene or Gene Product').
semtype_translation_reverse('12',gngm,'Gene or Genome').
semtype_translation_reverse('12',genf,'Genetic Function').
semtype_translation_reverse('12',geoa,'Geographic Area').
semtype_translation_reverse('12',gora,'Governmental or Regulatory Activity').
semtype_translation_reverse('12',grpa,'Group Attribute').
semtype_translation_reverse('12',grup,'Group').
semtype_translation_reverse('12',hops,'Hazardous or Poisonous Substance').
semtype_translation_reverse('12',hlca,'Health Care Activity').
semtype_translation_reverse('12',hcro,'Health Care Related Organization').
semtype_translation_reverse('12',horm,'Hormone').
semtype_translation_reverse('12',humn,'Human').
semtype_translation_reverse('12',hcpp,'Human-caused Phenomenon or Process').
semtype_translation_reverse('12',idcn,'Idea or Concept').
semtype_translation_reverse('12',imft,'Immunologic Factor').
semtype_translation_reverse('12',irda,'Indicator, Reagent, or Diagnostic Aid').
semtype_translation_reverse('12',inbe,'Individual Behavior').
semtype_translation_reverse('12',inpo,'Injury or Poisoning').
semtype_translation_reverse('12',inch,'Inorganic Chemical').
semtype_translation_reverse('12',inpr,'Intellectual Product').
semtype_translation_reverse('12',lbpr,'Laboratory Procedure').
semtype_translation_reverse('12',lbtr,'Laboratory or Test Result').
semtype_translation_reverse('12',lang,'Language').
semtype_translation_reverse('12',lipd,'Lipid').
semtype_translation_reverse('12',mcha,'Machine Activity').
semtype_translation_reverse('12',mamm,'Mammal').
semtype_translation_reverse('12',mnob,'Manufactured Object').
semtype_translation_reverse('12',medd,'Medical Device').
semtype_translation_reverse('12',menp,'Mental Process').
semtype_translation_reverse('12',mobd,'Mental or Behavioral Dysfunction').
semtype_translation_reverse('12',mbrt,'Molecular Biology Research Technique').
semtype_translation_reverse('12',moft,'Molecular Function').
semtype_translation_reverse('12',mosq,'Molecular Sequence').
semtype_translation_reverse('12',npop,'Natural Phenomenon or Process').
semtype_translation_reverse('12',neop,'Neoplastic Process').
semtype_translation_reverse('12',nsba,'Neuroreactive Substance or Biogenic Amine').
semtype_translation_reverse('12',nnon,'Nucleic Acid, Nucleoside, or Nucleotide').
semtype_translation_reverse('12',nusq,'Nucleotide Sequence').
semtype_translation_reverse('12',objt,'Object').
semtype_translation_reverse('12',ocdi,'Occupation or Discipline').
semtype_translation_reverse('12',ocac,'Occupational Activity').
semtype_translation_reverse('12',ortf,'Organ or Tissue Function').
semtype_translation_reverse('12',orch,'Organic Chemical').
semtype_translation_reverse('12',orga,'Organism Attribute').
semtype_translation_reverse('12',orgf,'Organism Function').
semtype_translation_reverse('12',orgm,'Organism').
semtype_translation_reverse('12',orgt,'Organization').
semtype_translation_reverse('12',opco,'Organophosphorus Compound').
semtype_translation_reverse('12',patf,'Pathologic Function').
semtype_translation_reverse('12',podg,'Patient or Disabled Group').
semtype_translation_reverse('12',phsu,'Pharmacologic Substance').
semtype_translation_reverse('12',phpr,'Phenomenon or Process').
semtype_translation_reverse('12',phob,'Physical Object').
semtype_translation_reverse('12',phsf,'Physiologic Function').
semtype_translation_reverse('12',plnt,'Plant').
semtype_translation_reverse('12',popg,'Population Group').
semtype_translation_reverse('12',pros,'Professional Society').
semtype_translation_reverse('12',prog,'Professional or Occupational Group').
semtype_translation_reverse('12',qlco,'Qualitative Concept').
semtype_translation_reverse('12',qnco,'Quantitative Concept').
semtype_translation_reverse('12',rcpt,'Receptor').
semtype_translation_reverse('12',rnlw,'Regulation or Law').
semtype_translation_reverse('12',rept,'Reptile').
semtype_translation_reverse('12',resa,'Research Activity').
semtype_translation_reverse('12',resd,'Research Device').
semtype_translation_reverse('12',shro,'Self-help or Relief Organization').
semtype_translation_reverse('12',sosy,'Sign or Symptom').
semtype_translation_reverse('12',socb,'Social Behavior').
semtype_translation_reverse('12',spco,'Spatial Concept').
semtype_translation_reverse('12',strd,'Steroid').
semtype_translation_reverse('12',sbst,'Substance').
semtype_translation_reverse('12',tmco,'Temporal Concept').
semtype_translation_reverse('12',topp,'Therapeutic or Preventive Procedure').
semtype_translation_reverse('12',tisu,'Tissue').
semtype_translation_reverse('12',vtbt,'Vertebrate').
semtype_translation_reverse('12',virs,'Virus').
semtype_translation_reverse('12',vita,'Vitamin').
semtype_translation_reverse('14',acab,'Acquired Abnormality').
semtype_translation_reverse('14',acty,'Activity').
semtype_translation_reverse('14',aggp,'Age Group').
semtype_translation_reverse('14',amas,'Amino Acid Sequence').
semtype_translation_reverse('14',aapp,'Amino Acid, Peptide, or Protein').
semtype_translation_reverse('14',amph,'Amphibian').
semtype_translation_reverse('14',anab,'Anatomical Abnormality').
semtype_translation_reverse('14',anst,'Anatomical Structure').
semtype_translation_reverse('14',anim,'Animal').
semtype_translation_reverse('14',antb,'Antibiotic').
semtype_translation_reverse('14',arch,'Archaeon').
semtype_translation_reverse('14',bact,'Bacterium').
semtype_translation_reverse('14',bhvr,'Behavior').
semtype_translation_reverse('14',biof,'Biologic Function').
semtype_translation_reverse('14',bacs,'Biologically Active Substance').
semtype_translation_reverse('14',bmod,'Biomedical Occupation or Discipline').
semtype_translation_reverse('14',bodm,'Biomedical or Dental Material').
semtype_translation_reverse('14',bird,'Bird').
semtype_translation_reverse('14',blor,'Body Location or Region').
semtype_translation_reverse('14',bpoc,'Body Part, Organ, or Organ Component').
semtype_translation_reverse('14',bsoj,'Body Space or Junction').
semtype_translation_reverse('14',bdsu,'Body Substance').
semtype_translation_reverse('14',bdsy,'Body System').
semtype_translation_reverse('14',crbs,'Carbohydrate Sequence').
semtype_translation_reverse('14',carb,'Carbohydrate').
semtype_translation_reverse('14',celc,'Cell Component').
semtype_translation_reverse('14',celf,'Cell Function').
semtype_translation_reverse('14',comd,'Cell or Molecular Dysfunction').
semtype_translation_reverse('14',cell,'Cell').
semtype_translation_reverse('14',chvf,'Chemical Viewed Functionally').
semtype_translation_reverse('14',chvs,'Chemical Viewed Structurally').
semtype_translation_reverse('14',chem,'Chemical').
semtype_translation_reverse('14',clas,'Classification').
semtype_translation_reverse('14',clna,'Clinical Attribute').
semtype_translation_reverse('14',clnd,'Clinical Drug').
semtype_translation_reverse('14',cnce,'Conceptual Entity').
semtype_translation_reverse('14',cgab,'Congenital Abnormality').
semtype_translation_reverse('14',dora,'Daily or Recreational Activity').
semtype_translation_reverse('14',diap,'Diagnostic Procedure').
semtype_translation_reverse('14',dsyn,'Disease or Syndrome').
semtype_translation_reverse('14',drdd,'Drug Delivery Device').
semtype_translation_reverse('14',edac,'Educational Activity').
semtype_translation_reverse('14',eico,'Eicosanoid').
semtype_translation_reverse('14',elii,'Element, Ion, or Isotope').
semtype_translation_reverse('14',emst,'Embryonic Structure').
semtype_translation_reverse('14',enty,'Entity').
semtype_translation_reverse('14',eehu,'Environmental Effect of Humans').
semtype_translation_reverse('14',enzy,'Enzyme').
semtype_translation_reverse('14',euka,'Eukaryote').
semtype_translation_reverse('14',evnt,'Event').
semtype_translation_reverse('14',emod,'Experimental Model of Disease').
semtype_translation_reverse('14',famg,'Family Group').
semtype_translation_reverse('14',fndg,'Finding').
semtype_translation_reverse('14',fish,'Fish').
semtype_translation_reverse('14',food,'Food').
semtype_translation_reverse('14',ffas,'Fully Formed Anatomical Structure').
semtype_translation_reverse('14',ftcn,'Functional Concept').
semtype_translation_reverse('14',fngs,'Fungus').
semtype_translation_reverse('14',gngp,'Gene or Gene Product').
semtype_translation_reverse('14',gngm,'Gene or Genome').
semtype_translation_reverse('14',genf,'Genetic Function').
semtype_translation_reverse('14',geoa,'Geographic Area').
semtype_translation_reverse('14',gora,'Governmental or Regulatory Activity').
semtype_translation_reverse('14',grpa,'Group Attribute').
semtype_translation_reverse('14',grup,'Group').
semtype_translation_reverse('14',hops,'Hazardous or Poisonous Substance').
semtype_translation_reverse('14',hlca,'Health Care Activity').
semtype_translation_reverse('14',hcro,'Health Care Related Organization').
semtype_translation_reverse('14',horm,'Hormone').
semtype_translation_reverse('14',humn,'Human').
semtype_translation_reverse('14',hcpp,'Human-caused Phenomenon or Process').
semtype_translation_reverse('14',idcn,'Idea or Concept').
semtype_translation_reverse('14',imft,'Immunologic Factor').
semtype_translation_reverse('14',irda,'Indicator, Reagent, or Diagnostic Aid').
semtype_translation_reverse('14',inbe,'Individual Behavior').
semtype_translation_reverse('14',inpo,'Injury or Poisoning').
semtype_translation_reverse('14',inch,'Inorganic Chemical').
semtype_translation_reverse('14',inpr,'Intellectual Product').
semtype_translation_reverse('14',lbpr,'Laboratory Procedure').
semtype_translation_reverse('14',lbtr,'Laboratory or Test Result').
semtype_translation_reverse('14',lang,'Language').
semtype_translation_reverse('14',lipd,'Lipid').
semtype_translation_reverse('14',mcha,'Machine Activity').
semtype_translation_reverse('14',mamm,'Mammal').
semtype_translation_reverse('14',mnob,'Manufactured Object').
semtype_translation_reverse('14',medd,'Medical Device').
semtype_translation_reverse('14',menp,'Mental Process').
semtype_translation_reverse('14',mobd,'Mental or Behavioral Dysfunction').
semtype_translation_reverse('14',mbrt,'Molecular Biology Research Technique').
semtype_translation_reverse('14',moft,'Molecular Function').
semtype_translation_reverse('14',mosq,'Molecular Sequence').
semtype_translation_reverse('14',npop,'Natural Phenomenon or Process').
semtype_translation_reverse('14',neop,'Neoplastic Process').
semtype_translation_reverse('14',nsba,'Neuroreactive Substance or Biogenic Amine').
semtype_translation_reverse('14',nnon,'Nucleic Acid, Nucleoside, or Nucleotide').
semtype_translation_reverse('14',nusq,'Nucleotide Sequence').
semtype_translation_reverse('14',objt,'Object').
semtype_translation_reverse('14',ocdi,'Occupation or Discipline').
semtype_translation_reverse('14',ocac,'Occupational Activity').
semtype_translation_reverse('14',ortf,'Organ or Tissue Function').
semtype_translation_reverse('14',orch,'Organic Chemical').
semtype_translation_reverse('14',orga,'Organism Attribute').
semtype_translation_reverse('14',orgf,'Organism Function').
semtype_translation_reverse('14',orgm,'Organism').
semtype_translation_reverse('14',orgt,'Organization').
semtype_translation_reverse('14',opco,'Organophosphorus Compound').
semtype_translation_reverse('14',patf,'Pathologic Function').
semtype_translation_reverse('14',podg,'Patient or Disabled Group').
semtype_translation_reverse('14',phsu,'Pharmacologic Substance').
semtype_translation_reverse('14',phpr,'Phenomenon or Process').
semtype_translation_reverse('14',phob,'Physical Object').
semtype_translation_reverse('14',phsf,'Physiologic Function').
semtype_translation_reverse('14',plnt,'Plant').
semtype_translation_reverse('14',popg,'Population Group').
semtype_translation_reverse('14',pros,'Professional Society').
semtype_translation_reverse('14',prog,'Professional or Occupational Group').
semtype_translation_reverse('14',qlco,'Qualitative Concept').
semtype_translation_reverse('14',qnco,'Quantitative Concept').
semtype_translation_reverse('14',rcpt,'Receptor').
semtype_translation_reverse('14',rnlw,'Regulation or Law').
semtype_translation_reverse('14',rept,'Reptile').
semtype_translation_reverse('14',resa,'Research Activity').
semtype_translation_reverse('14',resd,'Research Device').
semtype_translation_reverse('14',shro,'Self-help or Relief Organization').
semtype_translation_reverse('14',sosy,'Sign or Symptom').
semtype_translation_reverse('14',socb,'Social Behavior').
semtype_translation_reverse('14',spco,'Spatial Concept').
semtype_translation_reverse('14',strd,'Steroid').
semtype_translation_reverse('14',sbst,'Substance').
semtype_translation_reverse('14',tmco,'Temporal Concept').
semtype_translation_reverse('14',topp,'Therapeutic or Preventive Procedure').
semtype_translation_reverse('14',tisu,'Tissue').
semtype_translation_reverse('14',vtbt,'Vertebrate').
semtype_translation_reverse('14',virs,'Virus').
semtype_translation_reverse('14',vita,'Vitamin').
semtype_translation_reverse('15',acab,'Acquired Abnormality').
semtype_translation_reverse('15',acty,'Activity').
semtype_translation_reverse('15',aggp,'Age Group').
semtype_translation_reverse('15',amas,'Amino Acid Sequence').
semtype_translation_reverse('15',aapp,'Amino Acid, Peptide, or Protein').
semtype_translation_reverse('15',amph,'Amphibian').
semtype_translation_reverse('15',anab,'Anatomical Abnormality').
semtype_translation_reverse('15',anst,'Anatomical Structure').
semtype_translation_reverse('15',anim,'Animal').
semtype_translation_reverse('15',antb,'Antibiotic').
semtype_translation_reverse('15',arch,'Archaeon').
semtype_translation_reverse('15',bact,'Bacterium').
semtype_translation_reverse('15',bhvr,'Behavior').
semtype_translation_reverse('15',biof,'Biologic Function').
semtype_translation_reverse('15',bacs,'Biologically Active Substance').
semtype_translation_reverse('15',bmod,'Biomedical Occupation or Discipline').
semtype_translation_reverse('15',bodm,'Biomedical or Dental Material').
semtype_translation_reverse('15',bird,'Bird').
semtype_translation_reverse('15',blor,'Body Location or Region').
semtype_translation_reverse('15',bpoc,'Body Part, Organ, or Organ Component').
semtype_translation_reverse('15',bsoj,'Body Space or Junction').
semtype_translation_reverse('15',bdsu,'Body Substance').
semtype_translation_reverse('15',bdsy,'Body System').
semtype_translation_reverse('15',crbs,'Carbohydrate Sequence').
semtype_translation_reverse('15',celc,'Cell Component').
semtype_translation_reverse('15',celf,'Cell Function').
semtype_translation_reverse('15',comd,'Cell or Molecular Dysfunction').
semtype_translation_reverse('15',cell,'Cell').
semtype_translation_reverse('15',chvf,'Chemical Viewed Functionally').
semtype_translation_reverse('15',chvs,'Chemical Viewed Structurally').
semtype_translation_reverse('15',chem,'Chemical').
semtype_translation_reverse('15',clas,'Classification').
semtype_translation_reverse('15',clna,'Clinical Attribute').
semtype_translation_reverse('15',clnd,'Clinical Drug').
semtype_translation_reverse('15',cnce,'Conceptual Entity').
semtype_translation_reverse('15',cgab,'Congenital Abnormality').
semtype_translation_reverse('15',dora,'Daily or Recreational Activity').
semtype_translation_reverse('15',diap,'Diagnostic Procedure').
semtype_translation_reverse('15',dsyn,'Disease or Syndrome').
semtype_translation_reverse('15',drdd,'Drug Delivery Device').
semtype_translation_reverse('15',edac,'Educational Activity').
semtype_translation_reverse('15',elii,'Element, Ion, or Isotope').
semtype_translation_reverse('15',emst,'Embryonic Structure').
semtype_translation_reverse('15',enty,'Entity').
semtype_translation_reverse('15',eehu,'Environmental Effect of Humans').
semtype_translation_reverse('15',enzy,'Enzyme').
semtype_translation_reverse('15',euka,'Eukaryote').
semtype_translation_reverse('15',evnt,'Event').
semtype_translation_reverse('15',emod,'Experimental Model of Disease').
semtype_translation_reverse('15',famg,'Family Group').
semtype_translation_reverse('15',fndg,'Finding').
semtype_translation_reverse('15',fish,'Fish').
semtype_translation_reverse('15',food,'Food').
semtype_translation_reverse('15',ffas,'Fully Formed Anatomical Structure').
semtype_translation_reverse('15',ftcn,'Functional Concept').
semtype_translation_reverse('15',fngs,'Fungus').
semtype_translation_reverse('15',gngp,'Gene or Gene Product').
semtype_translation_reverse('15',gngm,'Gene or Genome').
semtype_translation_reverse('15',genf,'Genetic Function').
semtype_translation_reverse('15',geoa,'Geographic Area').
semtype_translation_reverse('15',gora,'Governmental or Regulatory Activity').
semtype_translation_reverse('15',grpa,'Group Attribute').
semtype_translation_reverse('15',grup,'Group').
semtype_translation_reverse('15',hops,'Hazardous or Poisonous Substance').
semtype_translation_reverse('15',hlca,'Health Care Activity').
semtype_translation_reverse('15',hcro,'Health Care Related Organization').
semtype_translation_reverse('15',horm,'Hormone').
semtype_translation_reverse('15',humn,'Human').
semtype_translation_reverse('15',hcpp,'Human-caused Phenomenon or Process').
semtype_translation_reverse('15',idcn,'Idea or Concept').
semtype_translation_reverse('15',imft,'Immunologic Factor').
semtype_translation_reverse('15',irda,'Indicator, Reagent, or Diagnostic Aid').
semtype_translation_reverse('15',inbe,'Individual Behavior').
semtype_translation_reverse('15',inpo,'Injury or Poisoning').
semtype_translation_reverse('15',inch,'Inorganic Chemical').
semtype_translation_reverse('15',inpr,'Intellectual Product').
semtype_translation_reverse('15',lbpr,'Laboratory Procedure').
semtype_translation_reverse('15',lbtr,'Laboratory or Test Result').
semtype_translation_reverse('15',lang,'Language').
semtype_translation_reverse('15',mcha,'Machine Activity').
semtype_translation_reverse('15',mamm,'Mammal').
semtype_translation_reverse('15',mnob,'Manufactured Object').
semtype_translation_reverse('15',medd,'Medical Device').
semtype_translation_reverse('15',menp,'Mental Process').
semtype_translation_reverse('15',mobd,'Mental or Behavioral Dysfunction').
semtype_translation_reverse('15',mbrt,'Molecular Biology Research Technique').
semtype_translation_reverse('15',moft,'Molecular Function').
semtype_translation_reverse('15',mosq,'Molecular Sequence').
semtype_translation_reverse('15',npop,'Natural Phenomenon or Process').
semtype_translation_reverse('15',neop,'Neoplastic Process').
semtype_translation_reverse('15',nnon,'Nucleic Acid, Nucleoside, or Nucleotide').
semtype_translation_reverse('15',nusq,'Nucleotide Sequence').
semtype_translation_reverse('15',objt,'Object').
semtype_translation_reverse('15',ocdi,'Occupation or Discipline').
semtype_translation_reverse('15',ocac,'Occupational Activity').
semtype_translation_reverse('15',ortf,'Organ or Tissue Function').
semtype_translation_reverse('15',orch,'Organic Chemical').
semtype_translation_reverse('15',orga,'Organism Attribute').
semtype_translation_reverse('15',orgf,'Organism Function').
semtype_translation_reverse('15',orgm,'Organism').
semtype_translation_reverse('15',orgt,'Organization').
semtype_translation_reverse('15',patf,'Pathologic Function').
semtype_translation_reverse('15',podg,'Patient or Disabled Group').
semtype_translation_reverse('15',phsu,'Pharmacologic Substance').
semtype_translation_reverse('15',phpr,'Phenomenon or Process').
semtype_translation_reverse('15',phob,'Physical Object').
semtype_translation_reverse('15',phsf,'Physiologic Function').
semtype_translation_reverse('15',plnt,'Plant').
semtype_translation_reverse('15',popg,'Population Group').
semtype_translation_reverse('15',pros,'Professional Society').
semtype_translation_reverse('15',prog,'Professional or Occupational Group').
semtype_translation_reverse('15',qlco,'Qualitative Concept').
semtype_translation_reverse('15',qnco,'Quantitative Concept').
semtype_translation_reverse('15',rcpt,'Receptor').
semtype_translation_reverse('15',rnlw,'Regulation or Law').
semtype_translation_reverse('15',rept,'Reptile').
semtype_translation_reverse('15',resa,'Research Activity').
semtype_translation_reverse('15',resd,'Research Device').
semtype_translation_reverse('15',shro,'Self-help or Relief Organization').
semtype_translation_reverse('15',sosy,'Sign or Symptom').
semtype_translation_reverse('15',socb,'Social Behavior').
semtype_translation_reverse('15',spco,'Spatial Concept').
semtype_translation_reverse('15',sbst,'Substance').
semtype_translation_reverse('15',tmco,'Temporal Concept').
semtype_translation_reverse('15',topp,'Therapeutic or Preventive Procedure').
semtype_translation_reverse('15',tisu,'Tissue').
semtype_translation_reverse('15',vtbt,'Vertebrate').
semtype_translation_reverse('15',virs,'Virus').
semtype_translation_reverse('15',vita,'Vitamin').
semtype_translation_reverse('18',acab,'Acquired Abnormality').
semtype_translation_reverse('18',acty,'Activity').
semtype_translation_reverse('18',aggp,'Age Group').
semtype_translation_reverse('18',amas,'Amino Acid Sequence').
semtype_translation_reverse('18',aapp,'Amino Acid, Peptide, or Protein').
semtype_translation_reverse('18',amph,'Amphibian').
semtype_translation_reverse('18',anab,'Anatomical Abnormality').
semtype_translation_reverse('18',anst,'Anatomical Structure').
semtype_translation_reverse('18',anim,'Animal').
semtype_translation_reverse('18',antb,'Antibiotic').
semtype_translation_reverse('18',arch,'Archaeon').
semtype_translation_reverse('18',bact,'Bacterium').
semtype_translation_reverse('18',bhvr,'Behavior').
semtype_translation_reverse('18',biof,'Biologic Function').
semtype_translation_reverse('18',bacs,'Biologically Active Substance').
semtype_translation_reverse('18',bmod,'Biomedical Occupation or Discipline').
semtype_translation_reverse('18',bodm,'Biomedical or Dental Material').
semtype_translation_reverse('18',bird,'Bird').
semtype_translation_reverse('18',blor,'Body Location or Region').
semtype_translation_reverse('18',bpoc,'Body Part, Organ, or Organ Component').
semtype_translation_reverse('18',bsoj,'Body Space or Junction').
semtype_translation_reverse('18',bdsu,'Body Substance').
semtype_translation_reverse('18',bdsy,'Body System').
semtype_translation_reverse('18',crbs,'Carbohydrate Sequence').
semtype_translation_reverse('18',celc,'Cell Component').
semtype_translation_reverse('18',celf,'Cell Function').
semtype_translation_reverse('18',comd,'Cell or Molecular Dysfunction').
semtype_translation_reverse('18',cell,'Cell').
semtype_translation_reverse('18',chvf,'Chemical Viewed Functionally').
semtype_translation_reverse('18',chvs,'Chemical Viewed Structurally').
semtype_translation_reverse('18',chem,'Chemical').
semtype_translation_reverse('18',clas,'Classification').
semtype_translation_reverse('18',clna,'Clinical Attribute').
semtype_translation_reverse('18',clnd,'Clinical Drug').
semtype_translation_reverse('18',cnce,'Conceptual Entity').
semtype_translation_reverse('18',cgab,'Congenital Abnormality').
semtype_translation_reverse('18',dora,'Daily or Recreational Activity').
semtype_translation_reverse('18',diap,'Diagnostic Procedure').
semtype_translation_reverse('18',dsyn,'Disease or Syndrome').
semtype_translation_reverse('18',drdd,'Drug Delivery Device').
semtype_translation_reverse('18',edac,'Educational Activity').
semtype_translation_reverse('18',elii,'Element, Ion, or Isotope').
semtype_translation_reverse('18',emst,'Embryonic Structure').
semtype_translation_reverse('18',enty,'Entity').
semtype_translation_reverse('18',eehu,'Environmental Effect of Humans').
semtype_translation_reverse('18',enzy,'Enzyme').
semtype_translation_reverse('18',euka,'Eukaryote').
semtype_translation_reverse('18',evnt,'Event').
semtype_translation_reverse('18',emod,'Experimental Model of Disease').
semtype_translation_reverse('18',famg,'Family Group').
semtype_translation_reverse('18',fndg,'Finding').
semtype_translation_reverse('18',fish,'Fish').
semtype_translation_reverse('18',food,'Food').
semtype_translation_reverse('18',ffas,'Fully Formed Anatomical Structure').
semtype_translation_reverse('18',ftcn,'Functional Concept').
semtype_translation_reverse('18',fngs,'Fungus').
semtype_translation_reverse('18',gngp,'Gene or Gene Product').
semtype_translation_reverse('18',gngm,'Gene or Genome').
semtype_translation_reverse('18',genf,'Genetic Function').
semtype_translation_reverse('18',geoa,'Geographic Area').
semtype_translation_reverse('18',gora,'Governmental or Regulatory Activity').
semtype_translation_reverse('18',grpa,'Group Attribute').
semtype_translation_reverse('18',grup,'Group').
semtype_translation_reverse('18',hops,'Hazardous or Poisonous Substance').
semtype_translation_reverse('18',hlca,'Health Care Activity').
semtype_translation_reverse('18',hcro,'Health Care Related Organization').
semtype_translation_reverse('18',horm,'Hormone').
semtype_translation_reverse('18',humn,'Human').
semtype_translation_reverse('18',hcpp,'Human-caused Phenomenon or Process').
semtype_translation_reverse('18',idcn,'Idea or Concept').
semtype_translation_reverse('18',imft,'Immunologic Factor').
semtype_translation_reverse('18',irda,'Indicator, Reagent, or Diagnostic Aid').
semtype_translation_reverse('18',inbe,'Individual Behavior').
semtype_translation_reverse('18',inpo,'Injury or Poisoning').
semtype_translation_reverse('18',inch,'Inorganic Chemical').
semtype_translation_reverse('18',inpr,'Intellectual Product').
semtype_translation_reverse('18',lbpr,'Laboratory Procedure').
semtype_translation_reverse('18',lbtr,'Laboratory or Test Result').
semtype_translation_reverse('18',lang,'Language').
semtype_translation_reverse('18',mcha,'Machine Activity').
semtype_translation_reverse('18',mamm,'Mammal').
semtype_translation_reverse('18',mnob,'Manufactured Object').
semtype_translation_reverse('18',medd,'Medical Device').
semtype_translation_reverse('18',menp,'Mental Process').
semtype_translation_reverse('18',mobd,'Mental or Behavioral Dysfunction').
semtype_translation_reverse('18',mbrt,'Molecular Biology Research Technique').
semtype_translation_reverse('18',moft,'Molecular Function').
semtype_translation_reverse('18',mosq,'Molecular Sequence').
semtype_translation_reverse('18',npop,'Natural Phenomenon or Process').
semtype_translation_reverse('18',neop,'Neoplastic Process').
semtype_translation_reverse('18',nnon,'Nucleic Acid, Nucleoside, or Nucleotide').
semtype_translation_reverse('18',nusq,'Nucleotide Sequence').
semtype_translation_reverse('18',objt,'Object').
semtype_translation_reverse('18',ocdi,'Occupation or Discipline').
semtype_translation_reverse('18',ocac,'Occupational Activity').
semtype_translation_reverse('18',ortf,'Organ or Tissue Function').
semtype_translation_reverse('18',orch,'Organic Chemical').
semtype_translation_reverse('18',orga,'Organism Attribute').
semtype_translation_reverse('18',orgf,'Organism Function').
semtype_translation_reverse('18',orgm,'Organism').
semtype_translation_reverse('18',orgt,'Organization').
semtype_translation_reverse('18',patf,'Pathologic Function').
semtype_translation_reverse('18',podg,'Patient or Disabled Group').
semtype_translation_reverse('18',phsu,'Pharmacologic Substance').
semtype_translation_reverse('18',phpr,'Phenomenon or Process').
semtype_translation_reverse('18',phob,'Physical Object').
semtype_translation_reverse('18',phsf,'Physiologic Function').
semtype_translation_reverse('18',plnt,'Plant').
semtype_translation_reverse('18',popg,'Population Group').
semtype_translation_reverse('18',pros,'Professional Society').
semtype_translation_reverse('18',prog,'Professional or Occupational Group').
semtype_translation_reverse('18',qlco,'Qualitative Concept').
semtype_translation_reverse('18',qnco,'Quantitative Concept').
semtype_translation_reverse('18',rcpt,'Receptor').
semtype_translation_reverse('18',rnlw,'Regulation or Law').
semtype_translation_reverse('18',rept,'Reptile').
semtype_translation_reverse('18',resa,'Research Activity').
semtype_translation_reverse('18',resd,'Research Device').
semtype_translation_reverse('18',shro,'Self-help or Relief Organization').
semtype_translation_reverse('18',sosy,'Sign or Symptom').
semtype_translation_reverse('18',socb,'Social Behavior').
semtype_translation_reverse('18',spco,'Spatial Concept').
semtype_translation_reverse('18',sbst,'Substance').
semtype_translation_reverse('18',tmco,'Temporal Concept').
semtype_translation_reverse('18',topp,'Therapeutic or Preventive Procedure').
semtype_translation_reverse('18',tisu,'Tissue').
semtype_translation_reverse('18',vtbt,'Vertebrate').
semtype_translation_reverse('18',virs,'Virus').
semtype_translation_reverse('18',vita,'Vitamin').



application_name(usemrep).
