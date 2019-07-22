
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

static Derror _derror [] =
{
	{  D_S_SUCCESS,         SUCCESS, "D_S_SUCCESS",          "Success"},
	{  D_E_ERROR,           ERROR,   "D_E_ERROR",            "Error"},
	{  D_F_MALLOC,          FATAL,   "D_E_MALLOC",           "Call to malloc failed"},
	{  D_E_FILE,            ERROR,   "D_E_FILE",             "Error trying to open or close a file"},
	{  D_E_FSEEK,           ERROR,   "D_E_FSEEK",            "Call to fseek failed"},
	{  D_E_FTELL,           ERROR,   "D_E_FTELL",            "Call to ftell failed"},
	{ D_E_FPUTS,            ERROR,   "D_E_FPUTS",            "Call to fputs failed"},
	{ D_F_ERROR,            FATAL,   "D_F_ERROR",            "Fatal Error"},
	{ D_S_NUMBER_EXISTS,    SUCCESS, "D_S_NUMBER_EXISTS",    "The Entry is not a new entry"},
	{ D_S_EOF,              SUCCESS, "D_S_EOF",              "End of File Hit"},
	{ D_S_NOT_FOUND,        SUCCESS, "D_S_NOT_FOUND",        "Not found"},
	{ D_E_ERRLOG,           ERROR,   "D_E_ERRLOG",           "Unable to write to the error log"},
	{ D_E_DBGLOG,           ERROR,   "D_E_DBGLOG",           "Unable to write to the debug log"},
	{ D_F_VALNODENEW,       FATAL,   "D_F_VALNODENEW",       "Unable to create a new valnode"},
	{ D_W_UNEXPECTED_VALUE, WARNING, "D_W_UNEXPECTED_VALUE", "Unexpected or Unknown value has been seen by this routine"},
	{ D_E_UNEXPECTED_VALUE, ERROR,   "D_E_UNEXPECTED_VALUE", "An Unexpected or Unknown value has been seen by this routine"},
	{ D_W_DESIGN_DEFECT,    WARNING, "D_W_DESIGN_DEFECT",    "A Design defect"},
	{ D_F_ASNLOAD,          FATAL,   "D_F_ASNLOAD",          "Unable to initialize asn api s"},
	{ D_F_ASNIOOPEN,        FATAL,   "D_F_ASNIOOPEN",        "Unable to open ASN encoded file"}
};

