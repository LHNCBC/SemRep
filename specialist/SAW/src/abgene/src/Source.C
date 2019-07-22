#include "Source.h"

Source::Source( string name, int id, int rank) {
  myName = name;
  myId = id;
  myRank = rank;
}

void Source::print(ostream& out) {
  out << "Id: " << myId << " Source: " << myName <<
    " Rank: " << myRank;
}

ostream& operator<<( ostream& out, Source s) {
  s.print(out);
  return out;
}

