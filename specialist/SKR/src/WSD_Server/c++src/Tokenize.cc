// tokenize tr
// The Perl code:
// # Last edited Aug 3, 2001
//
// require Exporter;
// @ISA = qw(Exporter);
// @EXPORT = qw(tokenize);
// 
// sub tokenize {
//     $_ = shift;
//     # put special - sign for compounds 
//     s/([a-zA-Z])-([a-zA-Z])/$1CCCOMPOUNDSIGNNN$2/g;    # com-pound
//     # put special , .  signs within numbers
//     s/(\d)\.(\d)/$1DDDOTSIGNNN$2/g;         # 12.34
//     s/(\s)\.(\d)/$1DDDOTSIGNNN$2/g;         #  .001
//     s/(\d),(\d)/$1CCCOMMASIGNNN$2/g;        # 1,121
// 
//     # replace all non-alphanumeric characters with spaces
//     s/\W/ /g;
// 
//     # put compound and dot sign back in
//     s/CCCOMPOUNDSIGNNN/-/g;
//     s/DDDOTSIGNNN/\./g;
//     s/CCCOMMASIGNNN/,/g;
// 
//     # delete leading and trailing spaces
//     s/^\s+//;
//     s/\s+$//;
// 
//     # replace multiple spaces and return
//     s/\s+/ /g;
//     return split / /, $_;
// }
// 
// 1;
//

#include <list>
#include "Scanner.h"
#include "Token.h"
#include "Tokenize.h"

list<string> tokenize(string buffer)
{
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
  }
  return tokens;
}
