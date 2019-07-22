#include <iostream>
#include <fstream>
#include <cstdlib>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <cmath>
#include <cstring>
#include <cassert>
#include "runn.h"
#include "Btree.h"
#include "Filter.h"
#include "Word.h"
#include <string>
#include <algorithm>
#include <vector>
#include <Hash.h>
#include <Blist.h>
#include <pcre.h>

#define DEBUG 0
#define MAXTAGLEN 256     // max char length of pos tags 
#define MAXWORDLEN 512    // max char length of words 

using namespace std;
namespace iret {
  
Filter::Filter(void){
}

Filter::Filter(const char *nam){
  int lxn=strlen(nam);
  name=new char[lxn+1];
  strcpy(name,nam);
}

Filter::~Filter(){  
  if(name!=NULL)delete [] name;
}

void Filter::change_name(char *nam){
  if(name!=NULL)delete [] name;
  int lxn=strlen(nam);
  name=new char[lxn+1];
  strcpy(name,nam);
}

void Filter::gopen_filter(void){
  ifstream fin;
  int pflag=get_qflag(),flag=1;
  char cnam[max_str];
  int  i;
  const char* errptr;
  const char* study_error;
  int erroffset;
  int offsets[max_str];
  int r; 
  char* cptr;
  pcre* ppre = NULL;
  pcre_extra* pextra;

  NotNoun.change_name("nn");
  NotNoun.gopen_htable_map();
  Intran.change_name("it");
  Intran.gopen_htable_map();
  Tran.change_name("tr");
  Tran.gopen_htable_map();
  Ditran.change_name("di");
  Ditran.gopen_htable_map();
  Bad_Before_Num.change_name("bbn");
  Bad_Before_Num.gopen_htable_map();
  Bad_After_Num.change_name("ban");
  Bad_After_Num.gopen_htable_map();
  Good_Before_Adj_Adv.change_name("gba");
  Good_Before_Adj_Adv.gopen_htable_map();
  Stopwords.change_name("s");
  Stopwords.gopen_htable_map();
  Umls.change_name("u");
  Umls.gopen_htable_map();
  //Good_Suffixes.change_name("gs");
  //Good_Suffixes.gopen_htable_map();
  Good_Num_Words.change_name("gn");
  Good_Num_Words.gopen_htable_map();
  Numbers.change_name("n");
  Numbers.gopen_htable_map();
  plex = new Lexicon("test");
  plex->gopen_bmatrix();

  get_pathw(cnam,"filterset",name,"rstr");
  fin.open(cnam, ios::in);
  if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}

  // Moved outside while loop to prevent memory leak -- Halil
  ppre    = (pcre*) pcre_malloc (max_str);
  pextra  = (pcre_extra*) pcre_malloc (max_str);

  while(fin.getline(cnam, max_str, '\n')) { 
    ppre    = pcre_compile(cnam, PCRE_CASELESS, 
			   &errptr, &erroffset, pcre_maketables());
    pextra  = pcre_study(ppre, 0, &study_error);
    Re.push_back(ppre);
    Extra.push_back(pextra);
  }
  // Added to prevent memory leak -- Halil
  pcre_free(ppre);
  pcre_free(pextra);
  fin.close();
}

char* Filter::get_str(void) {
  return instr;
}

void Filter::filter_string(char* str) {
  char *strpair, *loc, *aptr;
  char strc[max_str];
  long i;
  int ans;

  strcpy(instr,str);

  // make the words and tags vector for this string
  strpair = strtok(str, " ");
  strcpy(strc,strpair);
  loc = strrchr(strc, '/'); 
  loc++;
  strcat(tagstr, loc);
  strcat(tagstr, " ");
  Tags.push_back((plex->Btags).find(loc));
  *(--loc) = '\0';
  aptr = new char[strlen(strc)+1];
  strcpy(aptr,strc);
  Words.push_back(aptr); 
  strcat(no_tagstr, aptr);
  strcat(no_tagstr, " ");
 

  while((strpair = strtok(NULL, " ")) != NULL) {
    strcpy(strc,strpair);
    loc = strrchr(strc, '/');     
    loc++;
    strcat(tagstr, loc);
    strcat(tagstr, " ");
    Tags.push_back((plex->Btags).find(loc));
    *(--loc) = '\0';
    aptr = new char[strlen(strc)+1];
    strcpy(aptr,strc);
    Words.push_back(aptr);
    strcat(no_tagstr, aptr);
    strcat(no_tagstr, " ");
  }

  no_tagstr[strlen(no_tagstr)-1] = '\0';
  if(strcmp(Words[Words.size()-1], ".") == 0) {
    no_tagstr[strlen(no_tagstr)-2] = '\0';
    strcat(no_tagstr, ".");
  }
  tagstr[strlen(tagstr)-1] = '\0';
  if(strcmp((plex->Btags).strmap+(plex->Btags).addr[Tags[Tags.size()-1]-1], ".") == 0) {
    tagstr[strlen(tagstr)-2] = '\0';
  }

  /*if(DEBUG) {
    for(i=0;i<Words.size();i++) {
      cout << Words[i] << " " 
	   << (plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1] << endl;
    }
    }*/

  // apply filter to string and print result 
  apply_filter();
  cout << get_flag() << " | ";
  if(DEBUG) {
    cout << Rules[Rules.size()-1] << instr << " | ";
  }
  cout << last_no_tagstr << endl;
}

void Filter::filter_file(ifstream& ifs) {
  char s[max_str];
  long i;

  while(ifs.getline(s, max_str,'\n')) {
    filter_string(s);    
  }
}

void Filter::apply_filter() {
  long i,j;
  const char* errptr;
  const char* study_error;
  int erroffset;
  int offsets[max_str];
  int cs,r;   
  pcre* pre = NULL;
  pcre_extra* pextra;
  char* cptr;

  set_special_words_and_tags();

  // fflag set to 0 if phrase is bad

  if(first_rules()) {
    return;
  }

  r = pcre_exec(Re[7], Extra[7], tlastw, 
		strlen(tlastw), 0, 0, offsets, max_str);
  if(r > 0) {
    ends_in_verb();
    return;
  }

  r = pcre_exec(Re[10], Extra[10], tlastw, 
		strlen(tlastw), 0, 0, offsets, max_str);
  if(r > 0) {
    ends_in_adv();
    return;
  }

  r = pcre_exec(Re[11], Extra[11], tlastw, 
		strlen(tlastw), 0, 0, offsets, max_str);
  if(r > 0) {
    ends_in_adj();
    return;
  }

  last_rules();
}



void Filter::ends_in_adv() {
  long i,j;
  const char* errptr;
  const char* study_error;
  int erroffset;
  int offsets[max_str];
  int r;   
  pcre* pre = NULL;
  pcre_extra* pextra;
  char* cptr;
  int found = 0;

  r = pcre_exec(Re[12], Extra[12], tlastw, 
		strlen(tlastw), 0, 0, offsets, max_str);
  if(r > 0) {
    Rules.push_back("compare, super adv | ");
    fflag = 0; found = 1; 
  }
  
  // OK IF UMLS
  if(!found && Umls.find(wlastw)) {
    if(!NotNoun.find(wlastw)) {
      r = pcre_exec(Re[8], Extra[8], wlastw, 
		    strlen(wlastw), 0, 0, offsets, max_str);
      if (r > 0 || strlen(wlastw) <= 3) {
	Rules.push_back("umls, ends in adv | ");
	fflag = 1; found = 1; 
      }
      else {
	r = pcre_exec(Re[13], Extra[13], wlastw, 
		      strlen(wlastw), 0, 0, offsets, max_str);
	if (r > 0) {
	  r = pcre_exec(Re[16], Extra[16], tnlastw, 
			strlen(tnlastw), 0, 0, offsets, max_str);
	  if (r > 0) {
	    Rules.push_back("umls, ends in adv | ");
	    fflag = 1; found = 1; 
	  }
	}
      }
    }
  }
	
  // OK IF PRECEDED BY VERB, ADVERB, NUMBER
  r = pcre_exec(Re[7], Extra[7], tnlastw, 
		strlen(tnlastw), 0, 0, offsets, max_str);
  if(!found && r > 0) {
    //cout << "1 preceded by verb, ends in adv | " << instr << endl;
    Rules.push_back("preceded by verb, ends in adv | ");
    fflag = 1; found = 1; 
  }
  r = pcre_exec(Re[10], Extra[10], tnlastw, 
		strlen(tnlastw), 0, 0, offsets, max_str);
  if(!found && r > 0) {
    //cout << "2 preceded by verb, ends in adv | " << instr << endl;
    Rules.push_back("preceded by adv, ends in adv | ");
    fflag = 1; found = 1; 
  }
  if(!found && strcmp(tnlastw, "CD") == 0) {
    //cout << "3 preceded by verb, ends in adv | " << instr << endl;
    Rules.push_back("preceded by num, ends in adv | ");
    fflag = 1; found = 1; 
  }
  r = pcre_exec(Re[7], Extra[7], tagstr, 
		strlen(tagstr), 0, 0, offsets, max_str);
  if(!found && r > 0) {
    //cout << "4 preceded by verb, ends in adv | " << instr << endl;
    Rules.push_back("first tag is verb, ends in adv | ");
    fflag = 1; found = 1; 
  }

  // OK IF COULD BE A VERB IF NOT TAGGED AS A VERB
  if(!found && (Tran.find(wnlastw) || Intran.find(wnlastw) 
		|| Ditran.find(wnlastw))) {
    Rules.push_back("found in tran,intran or ditran | ");
    fflag = 1; found = 1; 
  }
  
  // OK IF COULD BE AA NAME ETC.
  if(!found && strlen(wlastw) <= 3) {    
    Rules.push_back("(could be) noun tagged as adverb | ");
    fflag = 1; found = 1; 
  }

  // OK IF PREV WORD ENDS in I,O,U AND LASTWORD ENDS IN LY
  r = pcre_exec(Re[14], Extra[14], wnlastw, 
		strlen(wnlastw), 0, 0, offsets, max_str);
  if(!found && r > 0) {
    r = pcre_exec(Re[15], Extra[15], wlastw, 
		  strlen(wlastw), 0, 0, offsets, max_str);
    if(!found && r > 0) {
      Rules.push_back("prev word ends in i,o,u | ");
      fflag = 1; found = 1; 
    }
  }
  
  if(!found) {
    // DEFAULT BAD IF ENDS IN ADVERB
    Rules.push_back("adverb at end | ");
    fflag = 0; found = 1;
  }
  
  clear_all();     
}

void Filter::ends_in_adj() {
  long i,j;
  const char* errptr;
  const char* study_error;
  int erroffset;
  int offsets[max_str];
  int r;   
  pcre* pre = NULL;
  pcre_extra* pextra;
  char* cptr;
  int found = 0;

  r = pcre_exec(Re[12], Extra[12], tlastw, 
		strlen(tlastw), 0, 0, offsets, max_str);
  if(r > 0) {
    Rules.push_back("compare, super adj | ");
    fflag = 0; found = 1; 
  }
  
  // OK IF UMLS
  if(!found && Umls.find(wlastw)) {
    if(!NotNoun.find(wlastw)) {
      r = pcre_exec(Re[8], Extra[8], wlastw, 
		    strlen(wlastw), 0, 0, offsets, max_str);
      if (r > 0 || strlen(wlastw) <= 3) {
	Rules.push_back("umls, ends in adj | ");
	fflag = 1; found = 1; 
      }
      else {
	r = pcre_exec(Re[11], Extra[11], tnlastw, 
		      strlen(tnlastw), 0, 0, offsets, max_str);
	if (r > 0) {
	    Rules.push_back("umls, ends in adj, jj | ");
	    fflag = 1; found = 1; 
	}
	if(!found) {
	  r = pcre_exec(Re[16], Extra[16], tnlastw, 
			strlen(tnlastw), 0, 0, offsets, max_str);
	  if (r > 0) {
	    Rules.push_back("umls, ends in adj, nn | ");
	    fflag = 1; found = 1; 
	  }
	}
	if(!found) {
	  r = pcre_exec(Re[17], Extra[17], tnlastw, 
			strlen(tnlastw), 0, 0, offsets, max_str);
	  if (r > 0) {
	    Rules.push_back("umls, ends in adj, cd | ");
	    fflag = 1; found = 1; 
	  }
	}
      }
    }
  }

  // OK IF 2 WORDS AND ENDS IN ING AND FIRST IS VERB
  if(!found && Words.size() == 2) {
    r = pcre_exec(Re[19], Extra[19], wlastw, 
		  strlen(wlastw), 0, 0, offsets, max_str);
    if(r > 0) {
      r = pcre_exec(Re[18], Extra[18], tnlastw, 
		    strlen(tnlastw), 0, 0, offsets, max_str);
      if (r > 0) {
	Rules.push_back("verb, adjective at end | ");
	fflag = 1; found = 1; 
      }
    }
    // OK IF 2 WORDS AND FIRST IS AN ADVERB
    if(!found) {
      if(strcmp(tnlastw, "RB") == 0) {
	Rules.push_back("adverb adjective at end | ");
	fflag = 1; found = 1; 
      }
    }
    // OK IF ENDS IN LY AND PREV ENDS in I,O,U
    if(!found) {
      r = pcre_exec(Re[15], Extra[15], wlastw, 
		    strlen(wlastw), 0, 0, offsets, max_str);
      if (r > 0) {
	r = pcre_exec(Re[14], Extra[14], wnlastw, 
		      strlen(wnlastw), 0, 0, offsets, max_str);
	if (r > 0) {
	  Rules.push_back("prev word ends in i,o,u | ");
	  fflag = 1; found = 1;
	} 
      }
    }
  }

  // OK IF INTRAN
  if(!found) {
    if(Intran.find(wlastw)) {
      if(!NotNoun.find(wlastw)) {
	r = pcre_exec(Re[8], Extra[8], wlastw, 
		      strlen(wlastw), 0, 0, offsets, max_str);
	if (r > 0 || strlen(wlastw) <= 3) {
	  Rules.push_back("found in intran | ");
	  fflag = 1; found = 1;
	} 
      }
    }
  }
    
  if(!found) {
    // DEFAULT BAD IF ENDS IN ADJ
    Rules.push_back("adj at end | ");
    fflag = 0; found = 1;
  }

  clear_all();  
}

void Filter::last_rules() {
  long i,j;
  const char* errptr;
  const char* study_error;
  int erroffset;
  int offsets[max_str];
  int r;   
  pcre* pre = NULL;
  pcre_extra* pextra;
  char* cptr;
  int found = 0;
  
  // OK IF ENDS IN PREP AND <= 3 CHARS
  r = pcre_exec(Re[20], Extra[20], tlastw, 
		strlen(tlastw), 0, 0, offsets, max_str);
  if (r > 0 && strlen(wlastw) <= 3) {
    Rules.push_back("ends in prp | ");
    fflag = 1; found = 1; clear_all(); return;
  } 

  // OK IF ENDS IN ONE CHAR
  if(!found) {
    if(strlen(wlastw) == 1) {
      Rules.push_back("one char end | ");
      fflag = 1; found = 1; clear_all(); return;
    } 
  }

  // NOT OK IF ON NOT NOUN LIST
  if(!found) {
    if(NotNoun.find(wlastw)) {
      Rules.push_back("on not noun list | ");
      fflag = 0; found = 1; clear_all(); return;
    } 
  }

  if(!found) {
    // default is a good phrase
    Rules.push_back("default good | ");
    fflag = 1; clear_all(); return;
  }   
}

int Filter::first_rules() {
  long i,j;
  const char* errptr;
  const char* study_error;
  int erroffset;
  int offsets[max_str];
  int cs,r;   
  pcre* pre = NULL;
  pcre_extra* pextra;
  char* cptr;

  // NUM BAD AFTER
  r      = pcre_exec(Re[0], Extra[0], wfirstw, 
		     strlen(wfirstw), 0, 0, offsets, max_str);
  if (r > 0 && Bad_After_Num.find(wlastw) > 0) {
    Rules.push_back("num badafter | ");
    fflag = 0; clear_all(); return 1;
  }

  // NUM BAD BEFORE
  r      = pcre_exec(Re[1], Extra[1], wlastw, 
		     strlen(wlastw), 0, 0, offsets, max_str);
  if (r > 0 && (Bad_Before_Num.find(wnlastw) ||
		Bad_Before_Num.find(wfirstw) ||
		strchr(tnlastw, 'V'))) { 
    Rules.push_back("num badbefore | ");
    fflag = 0; clear_all(); return 1;
  }

  // DOT PATTERN
  r      = pcre_exec(Re[2], Extra[2], no_tagstr, 
		     strlen(no_tagstr), 0, 0, offsets, max_str);
  if (r > 0 && (Bad_Before_Num.find(wnlastw) ||
		Bad_Before_Num.find(wfirstw) ||
		strchr(tnlastw, 'V'))) { 
    Rules.push_back("dot pattern | ");
    fflag = 0; clear_all(); return 1;
  }

  // ca TAGGED AS MD
  if (strcmp(tlastw, "MD") == 0) {
     r      = pcre_exec(Re[3], Extra[3], no_tagstr, 
		     strlen(no_tagstr), 0, 0, offsets, max_str);
     if (r > 0) {
       Rules.push_back("ca tagged as md | ");
       fflag = 1; clear_all(); return 1;
     }
     else {
       Rules.push_back("md at end | ");
       fflag = 0; clear_all(); return 1;
     }
  }

  //  GOOD NUMBER
  for(i=0;i<Words.size();i++) {
    if(Numbers.find(Words[i])) {
      for(j=0;j<Words.size();j++) {
	if(Good_Num_Words.find(Words[j])) {
	  Rules.push_back("good number | ");
	  fflag = 1; clear_all(); return 1;
	}
      }
       Rules.push_back("not good number | ");
      fflag = 0; clear_all(); return 1;
    }
  }

  // BAD NUMBER PATTERN - 4 mol l, 4 x 4 etc.
  r      = pcre_exec(Re[4], Extra[4], wfirstw, 
		     strlen(wfirstw), 0, 0, offsets, max_str);
  if (r > 0) {
    if(DEBUG) {
      for(i=0;i<(r*2);i+=2) {
	//cout << offsets[i] << offsets[i+1] << endl;
      }
    }
    // get offset of last match from offsets
    cptr = wfirstw; 
    cptr += offsets[(r*2)-2];

    if (Bad_After_Num.find(cptr) 
	&& (Words.size() <= 2 || Bad_After_Num.find(wnlastw))) {
      Rules.push_back("bad number pattern | ");
      fflag = 0; clear_all(); return 1;
    }
  }
    
  // STOPWORD
  if(Stopwords.find(wfirstw) || Stopwords.find(wlastw)) {
    Rules.push_back("stopword | ");
    fflag = 0; clear_all(); return 1;
  }

  // GOOD SUFFIX
  r      = pcre_exec(Re[5], Extra[5], wlastw, 
		     strlen(wlastw), 0, 0, offsets, max_str);
  if(r > 0) {
    Rules.push_back("good suffix | ");
    fflag = 1; clear_all(); return 1;
  }

  // OK IF BEGINS WITH GOOD WORD
  if(Words.size() == 2) {
    r = pcre_exec(Re[21], Extra[21], wnlastw, 
		  strlen(wnlastw), 0, 0, offsets, max_str);
    if (r > 0) {
      Rules.push_back("first word good | ");
      fflag = 1; clear_all(); return 1;
    }
  }

  // CD usually OK if no verb/adv in rest of phrase
  if(strcmp(tlastw, "CD") == 0) {
    r      = pcre_exec(Re[6], Extra[6], tagstr, 
		       strlen(tagstr), 0, 0, offsets, max_str);
    if(r == -1) {  
      Rules.push_back("cd, no v | ");
      fflag = 1; clear_all(); return 1;
    }
  }

  // default don't return yet
  return 0;
}

void Filter::ends_in_verb() {
  long i,j;
  const char* errptr;
  const char* study_error;
  int erroffset;
  int offsets[max_str];
  int r;   
  pcre* pre = NULL;
  pcre_extra* pextra;
  char* cptr;
  int found = 0;

  
  // GERUNDS OK UNLESS NOT NOUN
  if(strcmp(tlastw, "VBG") == 0 && 
     !NotNoun.find(wlastw)) {
    Rules.push_back("gerund at end | ");
    fflag = 1; found = 1;  
  }
  
  // OK IF IN UMLS ENDING ED or ING
  r      = pcre_exec(Re[6], Extra[6], tfirstw, 
		     strlen(tfirstw), 0, 0, offsets, max_str);
  if(!found && r == -1) {
    if(Umls.find(wlastw)) {
      if(!NotNoun.find(wlastw)) {
	r      = pcre_exec(Re[8], Extra[8], wlastw, 
			   strlen(wlastw), 0, 0, offsets, max_str);
	if(r > 0 || (strlen(wlastw) <= 3)) {
	  Rules.push_back("found in umls | ");
	  fflag = 1; found = 1; 
	  
	}
      }
    }
  }
  
  // OK IF INTRAN 
  if(!found && Intran.find(wlastw)) {
    if(!NotNoun.find(wlastw)) {
      r      = pcre_exec(Re[8], Extra[8], wlastw, 
			 strlen(wlastw), 0, 0, offsets, max_str);
      if(r > 0 || (strlen(wlastw) <= 3)) {
	Rules.push_back("found in intran | ");
	fflag = 1; found = 1;     
      }
    }
  }
  
  // OK IF MODIFIED BY RB - check for intran
  r      = pcre_exec(Re[9], Extra[9], tnlastw, 
		     strlen(tnlastw), 0, 0, offsets, max_str);
  if(!found && r > 0) {
    if(strcmp(tlastw, "VBZ") != 0) {
      if(Intran.find(wlastw)) {
	Rules.push_back("verb adverb intran | ");
	fflag = 1; found = 1;     
      }
    }
  }
  
  // OK IF MODIFIED BY RB - 2 words long
  r      = pcre_exec(Re[9], Extra[9], tnlastw, 
		     strlen(tnlastw), 0, 0, offsets, max_str);
  if(!found && r > 0 && Words.size() == 2) {
    Rules.push_back("verb adverb 2 words | ");
    fflag = 1; found = 1;     
  }
  
  // OK IF MODIFIED BY CD AND NOT VBZ AND INTRAN
  if(!found && strcmp(tnlastw, "CD") == 0) {
    if(strcmp(tlastw, "VBZ") != 0) {
      if(Intran.find(wlastw)) {
	Rules.push_back("cd and intran not vbz | ");
	fflag = 1; found = 1;     
      }
    }
  }
  
  if(!found) {
    // DEFAULT BAD IF ENDS IN VERB
    Rules.push_back("verb at end | ");
    fflag = 0; found = 1;
  }

  clear_all();     
}

void Filter::set_special_words_and_tags() {
  const char* noothing = "NOOTHING";
    
  // set first, last, next-to-last words from Words
  // if there is a period at the end of the sentence,
  // lastw is set to the word before the period

  wfirstw     = new char[strlen(Words[0])+1];
  strcpy(wfirstw, Words[0]);
  if(Words.size() > 1) {
    wlastw    = new char[strlen(Words[Words.size()-1])+1];
    strcpy(wlastw, Words[Words.size()-1]);
  }
  else {
    wlastw = new char[9];
    strcpy(wlastw, noothing);
  }
  if(Words.size() > 2) {
    wnlastw   = new char[strlen(Words[Words.size()-2])+1];
    strcpy(wnlastw, Words[Words.size()-2]);
  }
  else {
    wnlastw = new char[9];
    wnlastw = wfirstw;
  }
  if(*wlastw == '.') {
    if(Words.size() > 2) {
      wlastw  = new char[strlen(Words[Words.size()-2])+1];
      strcpy(wlastw, Words[Words.size()-2]);
    }
    else {
      wlastw = new char[9];
      strcpy(wlastw, noothing);
    }
    if(Words.size() > 3) {
      wnlastw = new char[strlen(Words[Words.size()-3])+1];
      strcpy(wnlastw, Words[Words.size()-3]);
    }
    else {
      wnlastw = new char[9];
      wnlastw = wfirstw;
    }
  }

  // set first, last, next-to-last tags from Tags
  
  tfirstw    = new char[strlen((plex->Btags).strmap+(plex->Btags).addr[Tags[0]-1])+1];
  strcpy(tfirstw,  (plex->Btags).strmap+(plex->Btags).addr[Tags[0]-1]);
  if(Tags.size() > 1) {
    tlastw   = new char[strlen((plex->Btags).strmap+(plex->Btags).addr[Tags[Tags.size()-1]-1])+1];
    strcpy(tlastw, (plex->Btags).strmap+(plex->Btags).addr[Tags[Tags.size()-1]-1]);
  }
  else {
    tlastw = new char[9];
    strcpy(tlastw, noothing);
  }
  if(Tags.size() > 2) {
    tnlastw  = new char[strlen((plex->Btags).strmap+(plex->Btags).addr[Tags[Tags.size()-2]-1])+1];
    strcpy(tnlastw, (plex->Btags).strmap+(plex->Btags).addr[Tags[Tags.size()-2]-1]);
  }
  else {
    tnlastw = new char[9];
    tnlastw = tfirstw;
  }
  if(*tlastw == '.') {
    if(Tags.size() > 2) {
      tlastw  = new char[strlen((plex->Btags).strmap+(plex->Btags).addr[Tags[Tags.size()-2]-1])+1];
      strcpy(tlastw, (plex->Btags).strmap+(plex->Btags).addr[Tags[Tags.size()-2]-1]);
    }
    else {
      tlastw = new char[9];
      strcpy(tlastw, noothing);
    }
    if(Tags.size() > 3) {
      tnlastw = new char[strlen((plex->Btags).strmap+(plex->Btags).addr[Tags[Tags.size()-3]-1])+1];
      strcpy(tnlastw, (plex->Btags).strmap+(plex->Btags).addr[Tags[Tags.size()-3]-1]);
    }
    else {
      tnlastw = new char[9];
      tnlastw = tfirstw;
    }
  }
}

void Filter::clear_all() {
  // clear words and tags vectors and firstw etc.

  Words.clear();
  Tags.clear();
  delete(wlastw); 
  delete(wnlastw);
  delete(tlastw);
  delete(tnlastw);
  delete(wfirstw); 
  delete(tfirstw);
  strcpy(last_no_tagstr, no_tagstr);
  strcpy(last_tagstr, tagstr);
  strcpy(no_tagstr, "");
  strcpy(tagstr, "");
}

int Filter::get_flag() {
  return fflag;
}

// Added to prevent memory leak -- Halil
void Filter::gclose_filter(void){
  NotNoun.gclose_htable_map();
  Intran.gclose_htable_map();
  Tran.gclose_htable_map();
  Ditran.gclose_htable_map();
  Bad_Before_Num.gclose_htable_map();
  Bad_After_Num.gclose_htable_map();
  Good_Before_Adj_Adv.gclose_htable_map();
  Stopwords.gclose_htable_map();
  Umls.gclose_htable_map();
  //Good_Suffixes.gclose_htable_map();
  Good_Num_Words.gclose_htable_map();
  Numbers.gclose_htable_map();
  plex->gclose_bmatrix();
}
}
