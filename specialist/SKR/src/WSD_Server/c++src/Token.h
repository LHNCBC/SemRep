
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

#ifndef TOKEN_H
#define TOKEN_H

#include <stdlib.h>
#include <string>

#define EOS '\0'
/*
#define punctsym   1
#define stringsym  2
#define numericsym 3
#define hyphensym  4
#define periodsym  5
#define spacesym   6
#define eofsym     7
#define lambda     99
#define fatalsym   100
*/

using namespace std;

class Token {
 public:
  int tokentype;
  string text;
  Token(int type, string strbuf);
  Token(int type, const char* strbuf);
  Token(int type, const char ch);
  int getType();
  string getString();
  string getTypeString();
};

enum symbols { lambda, punctsym, stringsym, numericsym, hyphensym,
	       periodsym, spacesym, eofsym, fatalsym, };


#endif /*TOKEN_H*/
