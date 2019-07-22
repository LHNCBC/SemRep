
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
*  http://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

/*
% File:	    nls_signal.c
% Authors:  Lan
% Purpose:  Provides signal handling
*/

#include <signal.h>
#include "sicstus/sicstus.h"
#include "nls_signal_glue.h" 

/* External interface */

/* long C_establish_signal_handling(void); */
void abort_handler(int arg);


/*****************************************************************************/
/* Establish signal handlers.
*/

long C_establish_signal_handling(void)
{
	signal(SIGTERM, abort_handler);
	signal(SIGPIPE, abort_handler);

	return 1;

} /* C_establish_signal_handling */

void abort_handler(int arg)
{
	/* SP_raise_fault("ABORT"); */
	abort();

} /* abort_handler */
