
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                    %%%
%%%           IMPORTANT:               %%%
%%%    DO NOT MODIFY THIS FILE:        %%%
%%%    IT IS MACHINE-GENERATED         %%%
%%%  from the semnet_access file       %%%
%%% in $NLS/specialist/SKR/src/skr_lib %%%
%%%                                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(semtype_translation_2012AA, [
     abbreviate_semtypes/2,
     application_name/1,
     expand_semtypes/2,
     is_semtype/1,
     is_abbrev_semtype/1,
     semtype_translation/2,
     semtype_translation/4
]).

semtype_translation(Name, Abbrev, _UI, _TreeNo) :-
semtype_translation(Name, Abbrev).

semtype_translation(Name, Abbrev) :-
     ( nonvar(Name) ->
        semtype_translation_fact(Name, Abbrev)

       ; semtype_translation_reverse(Abbrev, Name)
        ),
       !.
semtype_translation_fact('Acquired Abnormality',acab).
semtype_translation_fact('Activity',acty).
semtype_translation_fact('Age Group',aggp).
semtype_translation_fact('Amino Acid Sequence',amas).
semtype_translation_fact('Amino Acid, Peptide, or Protein',aapp).
semtype_translation_fact('Amphibian',amph).
semtype_translation_fact('Anatomical Abnormality',anab).
semtype_translation_fact('Anatomical Structure',anst).
semtype_translation_fact('Animal',anim).
semtype_translation_fact('Antibiotic',antb).
semtype_translation_fact('Archaeon',arch).
semtype_translation_fact('Bacterium',bact).
semtype_translation_fact('Behavior',bhvr).
semtype_translation_fact('Biologic Function',biof).
semtype_translation_fact('Biologically Active Substance',bacs).
semtype_translation_fact('Biomedical Occupation or Discipline',bmod).
semtype_translation_fact('Biomedical or Dental Material',bodm).
semtype_translation_fact('Bird',bird).
semtype_translation_fact('Body Location or Region',blor).
semtype_translation_fact('Body Part, Organ, or Organ Component',bpoc).
semtype_translation_fact('Body Space or Junction',bsoj).
semtype_translation_fact('Body Substance',bdsu).
semtype_translation_fact('Body System',bdsy).
semtype_translation_fact('Carbohydrate Sequence',crbs).
semtype_translation_fact('Carbohydrate',carb).
semtype_translation_fact('Cell Component',celc).
semtype_translation_fact('Cell Function',celf).
semtype_translation_fact('Cell or Molecular Dysfunction',comd).
semtype_translation_fact('Cell',cell).
semtype_translation_fact('Chemical Viewed Functionally',chvf).
semtype_translation_fact('Chemical Viewed Structurally',chvs).
semtype_translation_fact('Chemical',chem).
semtype_translation_fact('Classification',clas).
semtype_translation_fact('Clinical Attribute',clna).
semtype_translation_fact('Clinical Drug',clnd).
semtype_translation_fact('Conceptual Entity',cnce).
semtype_translation_fact('Congenital Abnormality',cgab).
semtype_translation_fact('Daily or Recreational Activity',dora).
semtype_translation_fact('Diagnostic Procedure',diap).
semtype_translation_fact('Disease or Syndrome',dsyn).
semtype_translation_fact('Drug Delivery Device',drdd).
semtype_translation_fact('Educational Activity',edac).
semtype_translation_fact('Eicosanoid',eico).
semtype_translation_fact('Element, Ion, or Isotope',elii).
semtype_translation_fact('Embryonic Structure',emst).
semtype_translation_fact('Entity',enty).
semtype_translation_fact('Environmental Effect of Humans',eehu).
semtype_translation_fact('Enzyme',enzy).
semtype_translation_fact('Eukaryote',euka).
semtype_translation_fact('Event',evnt).
semtype_translation_fact('Experimental Model of Disease',emod).
semtype_translation_fact('Family Group',famg).
semtype_translation_fact('Finding',fndg).
semtype_translation_fact('Fish',fish).
semtype_translation_fact('Food',food).
semtype_translation_fact('Fully Formed Anatomical Structure',ffas).
semtype_translation_fact('Functional Concept',ftcn).
semtype_translation_fact('Fungus',fngs).
semtype_translation_fact('Gene or Gene Product',gngp).
semtype_translation_fact('Gene or Genome',gngm).
semtype_translation_fact('Genetic Function',genf).
semtype_translation_fact('Geographic Area',geoa).
semtype_translation_fact('Governmental or Regulatory Activity',gora).
semtype_translation_fact('Group Attribute',grpa).
semtype_translation_fact('Group',grup).
semtype_translation_fact('Hazardous or Poisonous Substance',hops).
semtype_translation_fact('Health Care Activity',hlca).
semtype_translation_fact('Health Care Related Organization',hcro).
semtype_translation_fact('Hormone',horm).
semtype_translation_fact('Human',humn).
semtype_translation_fact('Human-caused Phenomenon or Process',hcpp).
semtype_translation_fact('Idea or Concept',idcn).
semtype_translation_fact('Immunologic Factor',imft).
semtype_translation_fact('Indicator, Reagent, or Diagnostic Aid',irda).
semtype_translation_fact('Individual Behavior',inbe).
semtype_translation_fact('Injury or Poisoning',inpo).
semtype_translation_fact('Inorganic Chemical',inch).
semtype_translation_fact('Intellectual Product',inpr).
semtype_translation_fact('Laboratory Procedure',lbpr).
semtype_translation_fact('Laboratory or Test Result',lbtr).
semtype_translation_fact('Language',lang).
semtype_translation_fact('Lipid',lipd).
semtype_translation_fact('Machine Activity',mcha).
semtype_translation_fact('Mammal',mamm).
semtype_translation_fact('Manufactured Object',mnob).
semtype_translation_fact('Medical Device',medd).
semtype_translation_fact('Mental Process',menp).
semtype_translation_fact('Mental or Behavioral Dysfunction',mobd).
semtype_translation_fact('Molecular Biology Research Technique',mbrt).
semtype_translation_fact('Molecular Function',moft).
semtype_translation_fact('Molecular Sequence',mosq).
semtype_translation_fact('Natural Phenomenon or Process',npop).
semtype_translation_fact('Neoplastic Process',neop).
semtype_translation_fact('Neuroreactive Substance or Biogenic Amine',nsba).
semtype_translation_fact('Nucleic Acid, Nucleoside, or Nucleotide',nnon).
semtype_translation_fact('Nucleotide Sequence',nusq).
semtype_translation_fact('Object',objt).
semtype_translation_fact('Occupation or Discipline',ocdi).
semtype_translation_fact('Occupational Activity',ocac).
semtype_translation_fact('Organ or Tissue Function',ortf).
semtype_translation_fact('Organic Chemical',orch).
semtype_translation_fact('Organism Attribute',orga).
semtype_translation_fact('Organism Function',orgf).
semtype_translation_fact('Organism',orgm).
semtype_translation_fact('Organization',orgt).
semtype_translation_fact('Organophosphorus Compound',opco).
semtype_translation_fact('Pathologic Function',patf).
semtype_translation_fact('Patient or Disabled Group',podg).
semtype_translation_fact('Pharmacologic Substance',phsu).
semtype_translation_fact('Phenomenon or Process',phpr).
semtype_translation_fact('Physical Object',phob).
semtype_translation_fact('Physiologic Function',phsf).
semtype_translation_fact('Plant',plnt).
semtype_translation_fact('Population Group',popg).
semtype_translation_fact('Professional Society',pros).
semtype_translation_fact('Professional or Occupational Group',prog).
semtype_translation_fact('Qualitative Concept',qlco).
semtype_translation_fact('Quantitative Concept',qnco).
semtype_translation_fact('Receptor',rcpt).
semtype_translation_fact('Regulation or Law',rnlw).
semtype_translation_fact('Reptile',rept).
semtype_translation_fact('Research Activity',resa).
semtype_translation_fact('Research Device',resd).
semtype_translation_fact('Self-help or Relief Organization',shro).
semtype_translation_fact('Sign or Symptom',sosy).
semtype_translation_fact('Social Behavior',socb).
semtype_translation_fact('Spatial Concept',spco).
semtype_translation_fact('Steroid',strd).
semtype_translation_fact('Substance',sbst).
semtype_translation_fact('Temporal Concept',tmco).
semtype_translation_fact('Therapeutic or Preventive Procedure',topp).
semtype_translation_fact('Tissue',tisu).
semtype_translation_fact('Vertebrate',vtbt).
semtype_translation_fact('Virus',virs).
semtype_translation_fact('Vitamin',vita).


semtype_translation_reverse(acab,'Acquired Abnormality').
semtype_translation_reverse(acty,'Activity').
semtype_translation_reverse(aggp,'Age Group').
semtype_translation_reverse(amas,'Amino Acid Sequence').
semtype_translation_reverse(aapp,'Amino Acid, Peptide, or Protein').
semtype_translation_reverse(amph,'Amphibian').
semtype_translation_reverse(anab,'Anatomical Abnormality').
semtype_translation_reverse(anst,'Anatomical Structure').
semtype_translation_reverse(anim,'Animal').
semtype_translation_reverse(antb,'Antibiotic').
semtype_translation_reverse(arch,'Archaeon').
semtype_translation_reverse(bact,'Bacterium').
semtype_translation_reverse(bhvr,'Behavior').
semtype_translation_reverse(biof,'Biologic Function').
semtype_translation_reverse(bacs,'Biologically Active Substance').
semtype_translation_reverse(bmod,'Biomedical Occupation or Discipline').
semtype_translation_reverse(bodm,'Biomedical or Dental Material').
semtype_translation_reverse(bird,'Bird').
semtype_translation_reverse(blor,'Body Location or Region').
semtype_translation_reverse(bpoc,'Body Part, Organ, or Organ Component').
semtype_translation_reverse(bsoj,'Body Space or Junction').
semtype_translation_reverse(bdsu,'Body Substance').
semtype_translation_reverse(bdsy,'Body System').
semtype_translation_reverse(crbs,'Carbohydrate Sequence').
semtype_translation_reverse(carb,'Carbohydrate').
semtype_translation_reverse(celc,'Cell Component').
semtype_translation_reverse(celf,'Cell Function').
semtype_translation_reverse(comd,'Cell or Molecular Dysfunction').
semtype_translation_reverse(cell,'Cell').
semtype_translation_reverse(chvf,'Chemical Viewed Functionally').
semtype_translation_reverse(chvs,'Chemical Viewed Structurally').
semtype_translation_reverse(chem,'Chemical').
semtype_translation_reverse(clas,'Classification').
semtype_translation_reverse(clna,'Clinical Attribute').
semtype_translation_reverse(clnd,'Clinical Drug').
semtype_translation_reverse(cnce,'Conceptual Entity').
semtype_translation_reverse(cgab,'Congenital Abnormality').
semtype_translation_reverse(dora,'Daily or Recreational Activity').
semtype_translation_reverse(diap,'Diagnostic Procedure').
semtype_translation_reverse(dsyn,'Disease or Syndrome').
semtype_translation_reverse(drdd,'Drug Delivery Device').
semtype_translation_reverse(edac,'Educational Activity').
semtype_translation_reverse(eico,'Eicosanoid').
semtype_translation_reverse(elii,'Element, Ion, or Isotope').
semtype_translation_reverse(emst,'Embryonic Structure').
semtype_translation_reverse(enty,'Entity').
semtype_translation_reverse(eehu,'Environmental Effect of Humans').
semtype_translation_reverse(enzy,'Enzyme').
semtype_translation_reverse(euka,'Eukaryote').
semtype_translation_reverse(evnt,'Event').
semtype_translation_reverse(emod,'Experimental Model of Disease').
semtype_translation_reverse(famg,'Family Group').
semtype_translation_reverse(fndg,'Finding').
semtype_translation_reverse(fish,'Fish').
semtype_translation_reverse(food,'Food').
semtype_translation_reverse(ffas,'Fully Formed Anatomical Structure').
semtype_translation_reverse(ftcn,'Functional Concept').
semtype_translation_reverse(fngs,'Fungus').
semtype_translation_reverse(gngp,'Gene or Gene Product').
semtype_translation_reverse(gngm,'Gene or Genome').
semtype_translation_reverse(genf,'Genetic Function').
semtype_translation_reverse(geoa,'Geographic Area').
semtype_translation_reverse(gora,'Governmental or Regulatory Activity').
semtype_translation_reverse(grpa,'Group Attribute').
semtype_translation_reverse(grup,'Group').
semtype_translation_reverse(hops,'Hazardous or Poisonous Substance').
semtype_translation_reverse(hlca,'Health Care Activity').
semtype_translation_reverse(hcro,'Health Care Related Organization').
semtype_translation_reverse(horm,'Hormone').
semtype_translation_reverse(humn,'Human').
semtype_translation_reverse(hcpp,'Human-caused Phenomenon or Process').
semtype_translation_reverse(idcn,'Idea or Concept').
semtype_translation_reverse(imft,'Immunologic Factor').
semtype_translation_reverse(irda,'Indicator, Reagent, or Diagnostic Aid').
semtype_translation_reverse(inbe,'Individual Behavior').
semtype_translation_reverse(inpo,'Injury or Poisoning').
semtype_translation_reverse(inch,'Inorganic Chemical').
semtype_translation_reverse(inpr,'Intellectual Product').
semtype_translation_reverse(lbpr,'Laboratory Procedure').
semtype_translation_reverse(lbtr,'Laboratory or Test Result').
semtype_translation_reverse(lang,'Language').
semtype_translation_reverse(lipd,'Lipid').
semtype_translation_reverse(mcha,'Machine Activity').
semtype_translation_reverse(mamm,'Mammal').
semtype_translation_reverse(mnob,'Manufactured Object').
semtype_translation_reverse(medd,'Medical Device').
semtype_translation_reverse(menp,'Mental Process').
semtype_translation_reverse(mobd,'Mental or Behavioral Dysfunction').
semtype_translation_reverse(mbrt,'Molecular Biology Research Technique').
semtype_translation_reverse(moft,'Molecular Function').
semtype_translation_reverse(mosq,'Molecular Sequence').
semtype_translation_reverse(npop,'Natural Phenomenon or Process').
semtype_translation_reverse(neop,'Neoplastic Process').
semtype_translation_reverse(nsba,'Neuroreactive Substance or Biogenic Amine').
semtype_translation_reverse(nnon,'Nucleic Acid, Nucleoside, or Nucleotide').
semtype_translation_reverse(nusq,'Nucleotide Sequence').
semtype_translation_reverse(objt,'Object').
semtype_translation_reverse(ocdi,'Occupation or Discipline').
semtype_translation_reverse(ocac,'Occupational Activity').
semtype_translation_reverse(ortf,'Organ or Tissue Function').
semtype_translation_reverse(orch,'Organic Chemical').
semtype_translation_reverse(orga,'Organism Attribute').
semtype_translation_reverse(orgf,'Organism Function').
semtype_translation_reverse(orgm,'Organism').
semtype_translation_reverse(orgt,'Organization').
semtype_translation_reverse(opco,'Organophosphorus Compound').
semtype_translation_reverse(patf,'Pathologic Function').
semtype_translation_reverse(podg,'Patient or Disabled Group').
semtype_translation_reverse(phsu,'Pharmacologic Substance').
semtype_translation_reverse(phpr,'Phenomenon or Process').
semtype_translation_reverse(phob,'Physical Object').
semtype_translation_reverse(phsf,'Physiologic Function').
semtype_translation_reverse(plnt,'Plant').
semtype_translation_reverse(popg,'Population Group').
semtype_translation_reverse(pros,'Professional Society').
semtype_translation_reverse(prog,'Professional or Occupational Group').
semtype_translation_reverse(qlco,'Qualitative Concept').
semtype_translation_reverse(qnco,'Quantitative Concept').
semtype_translation_reverse(rcpt,'Receptor').
semtype_translation_reverse(rnlw,'Regulation or Law').
semtype_translation_reverse(rept,'Reptile').
semtype_translation_reverse(resa,'Research Activity').
semtype_translation_reverse(resd,'Research Device').
semtype_translation_reverse(shro,'Self-help or Relief Organization').
semtype_translation_reverse(sosy,'Sign or Symptom').
semtype_translation_reverse(socb,'Social Behavior').
semtype_translation_reverse(spco,'Spatial Concept').
semtype_translation_reverse(strd,'Steroid').
semtype_translation_reverse(sbst,'Substance').
semtype_translation_reverse(tmco,'Temporal Concept').
semtype_translation_reverse(topp,'Therapeutic or Preventive Procedure').
semtype_translation_reverse(tisu,'Tissue').
semtype_translation_reverse(vtbt,'Vertebrate').
semtype_translation_reverse(virs,'Virus').
semtype_translation_reverse(vita,'Vitamin').



application_name(usemrep).
