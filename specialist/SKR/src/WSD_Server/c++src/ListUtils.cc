#include "ListUtils.h"
/*$Id: ListUtils.cc,v 1.2 2004/04/15 13:38:05 wrogers Exp $*/
/* split string into string tokens delimited by separator */
list<string> 
split(string aString, char separator)
{
  list<string> l;
  unsigned int i = 0;
  unsigned int pos = 0;
  string tok;
  unsigned int end = aString.length();
  while (i < end) {
    pos = aString.find(separator, i);
    if (pos == string::npos) pos = end;
    tok = aString.substr(i, pos-i);
    l.push_back(tok);
    if (pos == end) {
      return l;
    }
    i = pos + 1;
  }
  return l;
}

// join list of string tokens into single string with tokens delimited
// by separator
string
join(list<string> aList, char separator)
{
  string result;
  for (list<string>::iterator it = aList.begin();
       it != aList.end();
       ++it) {
    if (it != aList.begin()) {
      result += separator;
    }
    result += *it;
  }
  return result;
}
