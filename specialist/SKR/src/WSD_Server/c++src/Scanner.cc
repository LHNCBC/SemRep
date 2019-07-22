#include <iostream>
#include "Scanner.h"

// $Id: Scanner.cc,v 1.2 2004/04/15 13:36:56 wrogers Exp $
// predefined regular expressions

// compounds   "([a-zA-Z])-([a-zA-Z])"
// numbers     "(\d)\.(\d)"  "(\s)\.(\d)" "(\d)\,(\d)"
// NonAlphaNum "\W"

// finite state machine
//
//  state   input   next state
//  -------------------------
//    0     [0-9]       1
//    0     \.          1
//    1     [0-9]       1
//    1     \,          1
//    1     \.          1
//


Scanner::Scanner(string str)
{
  buffer = bp = str.data();
}

void 
Scanner::reset(string str)
{
  buffer = bp = str.data();
}

Token*
Scanner::getNextToken()
{
  return getToken();
}

char 
Scanner::getch()
{
  return *bp;
}

char 
Scanner::nextch()
{
  //  input_len++;
  bp++;
  if (*bp == '\0') return EOS;
  return *bp;
}

void 
Scanner::scan_errmsg_unexpch(char ch)
{
  cerr << "unexplained character: " << ch << "\n";
}

void 
Scanner::scan_errmsg_line()
{
  // TBI
}

// we probably should not exit in this case.
void 
Scanner::scanfatal(string str)
{
  cerr << str << "\n";
}

Token*
Scanner::getToken()
{
  char ch;
  int tokentype = 0;
  char* tokenbuf = new char[4096];
  char* tbp = tokenbuf;
  char* tbend = tokenbuf + 4096;
  tokenbuf[0] = '\0';
  ch = getch();
  switch (ch)
    {
    case '<':
    case '`':        case '"':    case '#':
    case '$':    case '%':    case '&':    case '\'':
    case '(':    case ')':    case '*':    case '+':
    case ',':    case '/':    case ':':    case ';':
    case '=':    case '?':    case '@':    case '[':
    case '\\':   case ']':    case '^':    case '_':
    case '{':    case '|':    case '}':    case '~':
    case '!':
      tokentype = punctsym;
      tokenbuf[0] = ch;
      tokenbuf[1] = '\0';
      nextch();
      break;
    case '.':
    case '0':    case '1':    case '2':    case '3':    case '4':    
    case '5':    case '6':    case '7':    case '8':    case '9':
      tokentype = numericsym;
      while (isdigit(ch) || ch == '.' || ch == ',' && tbp < tbend)
	{
	  *tbp++ = ch;
	  ch = nextch();
	}
      *tbp = '\0';
      if (tokenbuf[0] == '.' && tokenbuf[1] == '\0') tokentype = punctsym;
      break;
    case 'A':    case 'B':    case 'C':    case 'D':    case 'E':    case 'F':
    case 'G':    case 'H':    case 'I':    case 'J':    case 'K':    case 'L':
    case 'M':    case 'N':    case 'O':    case 'P':    case 'Q':    case 'R':
    case 'S':    case 'T':    case 'U':    case 'V':    case 'W':    case 'X':
    case 'Y':    case 'Z':
    case 'a':    case 'b':    case 'c':    case 'd':    case 'e':    case 'f':
    case 'g':    case 'h':    case 'i':    case 'j':    case 'k':    case 'l':
    case 'm':    case 'n':    case 'o':    case 'p':    case 'q':    case 'r':
    case 's':    case 't':    case 'u':    case 'v':    case 'w':    case 'x':
    case 'y':    case 'z':
      tokentype = stringsym;
      while ( ! iscntrl(ch) && 
	      ! isspace(ch) && 
	      ! ispunct(ch) &&
	      ch != '\n' && tbp < tbend)
	{
	  *tbp++ = ch;
	  ch = nextch();
	}
      *tbp = '\0';
      break;
    case '-':
      tokentype = hyphensym;
      tokenbuf[0] = ch;
      tokenbuf[1] = '\0';
      nextch();
      break;
    case ' ':
    case '\12':
    case '\14':
      tokentype = spacesym;
      tokenbuf[0] = ch;
      tokenbuf[1] = '\0';
      nextch();
      break;
    case '\177':
      tokentype = eofsym;
      tokenbuf[0] = ch;
      tokenbuf[1] = '\0';
      break;
    case '\325':
    case '\345':
      tokentype = lambda;
      tokenbuf[0] = '\0';
      tokenbuf[1] = '\0';
      nextch();
      break;
    case EOS:
      tokentype = eofsym;
      tokenbuf[0] = '\0';
      tokenbuf[1] = '\0';
      break;
    default:
      tokentype = fatalsym;
      scan_errmsg_unexpch(ch);
      scan_errmsg_line();
      scanfatal("gettoken");
      nextch();
      break;
    }
  return new Token(tokentype, tokenbuf);
}

