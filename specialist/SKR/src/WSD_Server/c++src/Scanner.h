
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

#ifndef SCANNER_H
#define SCANNER_H
#include "Token.h"

/*
  example of use:

  list<string> tokens;
  Token* token;
  Scanner scan(buffer);
  token = scan.getNextToken();
  while (token->getType() != eofsym) {
    switch (token->getType())
      {
      case stringsym: 
	tokens.push_back(token->getString());	
	break;
      case numericsym:
	tokens.push_back(token->getString());	
	break;
      }
    token = scan.getNextToken();
*/

class Scanner {
  const char *buffer;
  const char *bp;
  char getch();
  char nextch();
  void scan_errmsg_unexpch(char);
  void scan_errmsg_line();
  void scanfatal(string);
public:
  Scanner(string);
  void reset(string);
  Token* getNextToken();
  Token* getToken();
};

#endif /*SCANNER_H*/
