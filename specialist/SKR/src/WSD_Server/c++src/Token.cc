#include "Token.h"

char* tokenstrings[] = { "lambda", "punctsym", "stringsym", "numericsym", 
			 "hyphensym", "periodsym", "spacesym", "eofsym", 
			 "fatalsym" };

Token::Token(int type, string strbuf)
{
  tokentype = type;
  text = strbuf;
}

Token::Token(int type, const char* strbuf)
{
  tokentype = type;
  text = strbuf;
}

Token::Token(int type, const char ch)
{
  tokentype = type;
  text[0] = ch;
  text[1] = '\0';
}


int
Token::getType()
{
  return tokentype;
}

string
Token::getString()
{
  return string(text);
}


string
Token::getTypeString()
{
  return tokenstrings[tokentype];
}

