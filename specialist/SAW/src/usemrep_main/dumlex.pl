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

% File:	    dumlex.pl
% Module:   dumlex
% Author:   tcr
% Purpose:  Temporary lexicon for SAW semantic interpretation


:- module( dumlex, [ dummy_lexicon/4 ] ).


%****************************** Temporary Lexicon ******************************


% dummy_lexicon( Entry, RNoun, Predicational, ArgCueList )


dummy_lexicon( adding,		rnoun, none, [ argcue( np,cotheme,noselrestr ), 
					       argcue( to,cotheme,noselrestr )] ).

dummy_lexicon( activation,	rnoun, none, [ argcue( of,theme,noselrestr ), 
					       argcue( by,agent,noselrestr )] ).

dummy_lexicon( adjunct,		rnoun, none, [ argcue( to,cotheme,noselrestr )] ).

dummy_lexicon( analysis,	rnoun, none, [ argcue( of,theme,noselrestr )] ).

dummy_lexicon( amplification,	rnoun, none, [ argcue( of,theme,noselrestr ),
	                                       argcue( by,nom,noselrestr )] ).

dummy_lexicon( amplifications,	rnoun, none, [ argcue( of,theme,noselrestr ),
	                                       argcue( by,nom,noselrestr )] ).

dummy_lexicon( application,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( to,goal,noselrestr ),
					       argcue( by,agent,human )] ).

dummy_lexicon( applications,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( to,goal,noselrestr ),
					       argcue( by,agent,human )] ).

dummy_lexicon( assessing,       rnoun, none, [ argcue( np, theme,noselrestr) ] ).

dummy_lexicon( assessment,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,agent,human ),
					       argcue( mod,instr,diagnosis)] ).

dummy_lexicon( associated,	rnoun, none, [ argcue( with,cotheme,noselrestr ),
					       argcue( by,nom,noselrestr ) ] ).

dummy_lexicon( binding, 	rnoun, none, [ argcue( of,cotheme,noselrestr),
					       argcue(to,cotheme,noselrestr) ] ).

dummy_lexicon( biosynthesis,	rnoun, none, [ argcue( of,theme,noselrestr )] ).

dummy_lexicon( caused,	        rnoun, none, [ argcue( by,nom,noselrestr )] ).

dummy_lexicon( change,		rnoun, none, [ argcue( in,nom,noselrestr ),
					       argcue( from,sourc,noselrestr ),
					       argcue( into,goal,noselrestr ),
					       argcue( to,goal,noselrestr ) ] ).

dummy_lexicon( changes,		rnoun, none, [ argcue( in,nom,noselrestr ),
					       argcue( from,sourc,noselrestr ),
					       argcue( into,goal,noselrestr ),
					       argcue( to,goal,noselrestr ) ] ).

dummy_lexicon( characteristic,		rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( characteristics,		rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( characterisation, rnoun, none, [ argcue( of,theme,noselrestr ), 
					 	argcue( by,agent,noselrestr ) ] ).

dummy_lexicon( characterization, rnoun, none, [ argcue( of,theme,noselrestr ), 
					 	argcue( by,agent,noselrestr ) ] ).

dummy_lexicon( clearance,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( cleavage,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( cloning,	        rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( comparing,       rnoun, none, [ argcue( np,theme,noselrestr ) ] ).

dummy_lexicon( comparison,	rnoun, none, [ argcue( of,cotheme,noselrestr ), 
					       argcue( with,cotheme,noselrestr ) ] ).

dummy_lexicon( compliance,	rnoun, none, [ argcue( with,theme,noselrestr ) ] ).

dummy_lexicon( complications,	rnoun, none, [ argcue( of,theme,noselrestr ), 
					       argcue( by,nom,noselrestr ) ] ).

dummy_lexicon( construction,    rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( control,		rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( correlate,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( correlates,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( correlation,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( cloned,	        rnoun, none, [ argcue( from,sourc,noselrestr ) ] ).

dummy_lexicon( cloning,	        rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( critique,	rnoun, none, [ argcue( of,theme,noselrestr ), 
					       argcue( by,agent,human ) ] ).

dummy_lexicon( decline,		rnoun, none, [ argcue( in,theme,noselrestr ),
					       argcue( from,sourc,noselrestr ),
					       argcue( into,goal,noselrestr ),
					       argcue( to,goal,noselrestr ) ] ).

dummy_lexicon( definition,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( demonstrating,   rnoun, none, [ argcue( np, theme,noselrestr) ] ).

dummy_lexicon( density,		rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( derived,		rnoun, none, [ argcue( from,sourc,noselrestr ) ] ).

dummy_lexicon( detection,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,agent,human ) ] ).

dummy_lexicon( determination,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,agent,human ) ] ).

dummy_lexicon( determined,	rnoun, none, [ argcue( by,nom,noselrestr ) ] ).

dummy_lexicon( detoxification,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,nom,noselrestr ) ] ).

dummy_lexicon( development,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,agent,human ) ] ).

dummy_lexicon( diagnosis,       rnoun, none, [ argcue( of,theme,disorder ),
	                                       argcue( of,patn,medpat ),
	                                       argcue( mod,instr,diagnosis ),
					       argcue( mod,theme,disorders )] ).

dummy_lexicon( diagnostics,     rnoun, none, [ none ] ).

dummy_lexicon( effect,		rnoun, none, [ argcue( of,nom,noselrestr ),
					 	argcue( on,theme,noselrestr ),
						argcue( upon,theme,noselrestr )] ).

dummy_lexicon( effectiveness,	rnoun, none, [ argcue( of,theme,noselrestr )]).

dummy_lexicon( effects,		rnoun, none, [ argcue( of,nom,noselrestr ),
					       argcue( on,theme,noselrestr ),
					       argcue( upon,theme,noselrestr )] ).

dummy_lexicon( efficacy,	rnoun, predicational, [ argcue( of,nom,noselrestr )]).

dummy_lexicon( enhancement,	rnoun, none, [ argcue( of,theme,noselrestr ),
                                               argcue( by,nom,noselrestr ) ] ).

dummy_lexicon( establishment,	rnoun, none, [ argcue( of,nom,noselrestr )]).

dummy_lexicon( evaluation,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,nom,noselrestr ),
					       argcue( mod,instr,diagnosis ),
					       argcue( mod,theme,anat )] ).

dummy_lexicon( examination,     rnoun, none, [ argcue( of,theme,noselrestr ),
                                               argcue( mod,instr,diagnosis ) ] ).

dummy_lexicon( examinations,    rnoun, none, [ argcue( mod,instr,diagnosis ) ] ).

dummy_lexicon( experience,	rnoun, none, [ argcue( with,theme,noselrestr ) ] ).

dummy_lexicon( expression,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,agent,human ) ] ).

dummy_lexicon( fate,	        rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( flanking,        rnoun, none, [ argcue( np,theme,noselrestr ) ] ).

dummy_lexicon( formation,	rnoun, none, [ argcue( of,theme,noselrestr ),
	                                       argcue( by,nom,noselrestr )] ).

dummy_lexicon( function,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( growth,	        rnoun, none, [ argcue( of,nom,noselrestr ) ] ).

dummy_lexicon( haemostasis,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( hemodialysis,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,agent,human ) ] ).

dummy_lexicon( hemofiltration,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,agent,human ) ] ).

dummy_lexicon( hemostasis,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( mod,instr,diagnosis ) ] ).

dummy_lexicon( hyperpolarization,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( 'continuous arteriovenous hemofiltration',	
				rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,agent,human ) ] ).

dummy_lexicon( identification,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,agent,human ) ] ).

dummy_lexicon( immunodiagnosis,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( implementation,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( improvement,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,nom,noselrestr ) ] ).

dummy_lexicon( influence,	rnoun, none, [ argcue( of,nom,noselrestr ),
					       argcue( on,theme,noselrestr ) ] ).

dummy_lexicon( infusion,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( with,theme,noselrestr ),
					       argcue( mod,locat,anat ),
					       argcue( mod,theme,therapy )] ).

dummy_lexicon( inhibition,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,agent,human ) ] ).

dummy_lexicon( injection,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( with,theme,noselrestr ),
	                                       argcue( mod,locat,anat ) ] ).

dummy_lexicon( interaction,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( intervention,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( investigation,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,agent,human ) ] ).

dummy_lexicon( isolation,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( lacking, 	rnoun, none, [ argcue( np,theme,noselrestr ) ] ).

dummy_lexicon( lesion, 	        rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( localization,    rnoun, none, [ argcue( of,nom,noselrestr ),
	                                       argcue( in,locat,noselrestr )] ).

dummy_lexicon( management,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,agent,human ) ] ).

dummy_lexicon( measurement,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,agent,human ) ] ).

dummy_lexicon( measurements,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,agent,human ) ] ).

dummy_lexicon( measuring,	rnoun, none, [ argcue( np,theme,noselrestr ) ] ).

dummy_lexicon( method,		rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( methods,		rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( microstimulation,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( mineralization, 	rnoun, none, [ argcue( np,theme,noselrestr ) ] ).

dummy_lexicon( mimicking, 	rnoun, none, [ argcue( np,theme,noselrestr ) ] ).

dummy_lexicon( model,		rnoun, none, [ argcue( of,theme,noslelect ),
    					       argcue( for,theme,noselect ) ] ). % this should be a disjunction

dummy_lexicon( monitoring,	rnoun, none, [ argcue( np,theme,noselrestr ),
					       argcue( of,theme,noselrestr ) ] ). % this should be a disjunction

dummy_lexicon( partitioning,	rnoun, none, [ argcue( np,theme,noselrestr ),
					       argcue( of,theme,noselrestr ),
					       argcue( into,result,noselrestr ) ] ).

dummy_lexicon( perception,	rnoun, none, [ argcue( of,theme,noselrestr ),
	                                       argcue( by,nom,noselrestr )] ).

dummy_lexicon( permeability,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( potential,	rnoun, none, [ argcue( of,theme,noselrestr ),
                                               argcue( for,theme,noselrestr ) ] ).

dummy_lexicon( potentiation,	rnoun, none, [ argcue( of,theme,noselrestr ), 
					       argcue( by,nom,drug ) ] ).

dummy_lexicon( 'posttetanic potentiation',	
				rnoun, none, [ argcue( of,theme,noselrestr ), 
					       argcue( by,nom,drug ) ] ).

dummy_lexicon( predicting,	rnoun, none, [ argcue( np,theme,noselrestr ) ] ).

dummy_lexicon( predictor,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( prevalence,	rnoun, none, [ argcue( of,nom,noselrestr  ) ] ).

dummy_lexicon( prevention,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( processing,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( production,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( prognosis,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( proliferation,	rnoun, none, [ argcue( of,theme,noselrestr ),
	                                       argcue( by,nom,noselrestr )] ).

dummy_lexicon( quality,		rnoun, none, [ argcue( of,nom,noselrestr  ) ] ).

dummy_lexicon( quantification,	rnoun, none, [ argcue( of,nom,noselrestr  ) ] ).

dummy_lexicon( quantitation,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( reactivity,	rnoun, none, [ argcue( of,cotheme,noselrestr ),
                                               argcue( with,cotheme,noselrestr ) ] ).

dummy_lexicon( receiving,	rnoun, none, [ argcue( np,theme,noselrestr ) ] ).

dummy_lexicon( recurrence,	rnoun, 
				predicational, [ argcue( of,nom,noselrestr  ) ] ).

dummy_lexicon( reference,	rnoun, none, [ argcue( to,theme,noselrestr  ) ] ).

dummy_lexicon( relationship,	rnoun, none, [ argcue( of,cotheme,noselrestr ),
                                               argcue( to,cotheme,noselrestr ) ] ).

dummy_lexicon( removal,		rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( from,sourc,noselrestr),
					       argcue( by,nom,noselrestr ) ] ).

dummy_lexicon( report,		rnoun, none, [ argcue( on,theme,noselrestr  ),
					       argcue( upon,theme,noselrestr  ),
					       argcue( by,agent,human ),
					       argcue( to,benefactive,human )] ).

dummy_lexicon( result,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( results,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( resumption,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,nom,noselrestr ) ] ).

dummy_lexicon( reversal,	rnoun, none, [ argcue( of,theme,noselrestr ),
					       argcue( by,nom,noselrestr ) ] ).

dummy_lexicon( safety,	        rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( scanning,	rnoun, 
				predicational, [ argcue( np,theme,noselrestr ),
						 argcue( of,theme,noselrestr ),
					 	 argcue( by,nom,noselrestr ) ] ).

dummy_lexicon( secretion,	rnoun, none, [ argcue( of,theme,noselrestr ),
                                               argcue( mod,theme,biosub )  ] ).

dummy_lexicon( sedation,	rnoun, none, [ argcue( of,theme,medpat ) ] ).

dummy_lexicon( selecting,	rnoun, none, [ argcue( np,theme,noselrestr ) ] ).

dummy_lexicon( separation,	rnoun, none, [ argcue( of,cotheme,noselrestr ), 
					       argcue( from,cotheme,noselrestr ),
					       argcue( by,nom,noselrestr ) ] ).

dummy_lexicon( sequence,	rnoun, 	none,[ argcue( of,nom,noselrestr )] ).

dummy_lexicon( severity,	rnoun, 	none,[ argcue( of,nom,noselrestr )] ).

dummy_lexicon( source,		rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( spasm,		rnoun, none, [ argcue( of,nom,noselrestr ) ] ).

dummy_lexicon( stimulation,	rnoun, none, [ argcue( of,theme,noselrestr ),
                                               argcue( by,nom,noselrestr ) ] ).

dummy_lexicon( study,		rnoun, none, [ argcue( of,theme,noselrestr ),
                                               argcue( on,theme,noselrestr ) ] ).

dummy_lexicon( studies,		rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( supplying, 	rnoun, none, [ argcue( np,theme,noselrestr ) ] ).

dummy_lexicon( symptom,		rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( symptoms,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( synthesis,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( testing, 	rnoun, none, [ argcue( np,theme,noselrestr ),
					       argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( transplantation,	rnoun, none, [ argcue( of,nom,noselrestr )]).

dummy_lexicon( treated,		rnoun, none, [ argcue( with,instr,therapy ),
					       argcue( by,nom,noselrestr ),
					       argcue( for,theme,disorder)] ).

dummy_lexicon( treatment,	rnoun, none, [ argcue( of,theme,disorder ),
	                                       argcue( of,patn,medpat ),
					       argcue( by,nom,noselrestr ),
					       argcue( for,theme,disorder ),
					       argcue( with,instr,therapy )] ).

dummy_lexicon( type,		rnoun, none, [ argcue( of,nom,noselrestr  ) ] ).

dummy_lexicon( types,		rnoun, none, [ argcue( of,nom,noselrestr  ) ] ).

dummy_lexicon( undergoing, 	rnoun, none, [ argcue( np,theme,therapy ) ] ).

dummy_lexicon( understanding,	rnoun, none, [ argcue( of,theme,noselrestr ),
                                               argcue( np,theme,noselrestr ) ] ). % this should be a disjunction

dummy_lexicon( utilization,	rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

dummy_lexicon( value,		rnoun, none, [ argcue( of,theme,noselrestr ) ] ).

