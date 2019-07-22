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
#include <runn.h>
#include <Btree.h>
#include <PostBrill_FP.h>
#include <Word.h>
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
  
PostBrill_FP::PostBrill_FP(void){
}

PostBrill_FP::PostBrill_FP(const char *nam){
  int lxn=strlen(nam);
  name=new char[lxn+1];
  strcpy(name,nam);
}

PostBrill_FP::~PostBrill_FP(){  
  if(name!=NULL)delete [] name;
  if (plex!=NULL) delete plex;
  free_vec_pcre(Re);
  free_vec_pcre_extra(Extra);
}

void PostBrill_FP::change_name(char *nam){
  if(name!=NULL)delete [] name;
  int lxn=strlen(nam);
  name=new char[lxn+1];
  strcpy(name,nam);
}

void PostBrill_FP::gopen_PostBrill_FP(void){
  ifstream fin;
  int pflag=get_qflag(),flag=1;
  char cnam[MAX_STR];
  int  i;
  const char* errptr;
  const char* study_error;
  int erroffset;
  int offsets[MAX_STR];
  int r; 
  char* cptr;
  pcre* ppre = NULL;
  pcre_extra* pextra;

  Gen.change_name("gen");
  Gen.gopen_htable_map();
  Aa.change_name("aa");
  Aa.gopen_htable_map();
  Ren.change_name("re");
  Ren.gopen_htable_map();
  Ce.change_name("ce");
  Ce.gopen_htable_map();
  Org.change_name("org");
  Org.gopen_htable_map();
  Bad_After_Num.change_name("bann");
  Bad_After_Num.gopen_htable_map();
  Genes.change_name("single");
  Genes.gopen_htable_map();
  GoodTags.change_name("gt");
  GoodTags.gopen_htable_map();
  Freq.change_name("freq");
  Freq.gopen_htable_map();
  plex = new Lexicon(name);
  plex->gopen_bmatrix();

  get_pathw(cnam,"postbrillset",name,"rstr_fp");
  fin.open(cnam, ios::in);
  if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}

  // removed to prevent memory leak -- Halil
//   ppre    = (pcre*) pcre_malloc (MAX_STR);
//   pextra  = (pcre_extra*) pcre_malloc (MAX_STR);

  while(fin.getline(cnam, MAX_STR, '\n')) { 
    // modified to prevent memory leak -- Halil
//      ppre    = pcre_compile(cnam, PCRE_CASELESS, 
//     			   &errptr, &erroffset, pcre_maketables());
    ppre    = pcre_compile(cnam,PCRE_CASELESS,
			   &errptr, &erroffset, NULL);
    pextra  = pcre_study(ppre, 0, &study_error);
    Re.push_back(ppre);
    Extra.push_back(pextra);
  }
  // removed to prevent memory leak -- Halil
//   pcre_free(ppre);
//   pcre_free(pextra);
  fin.close();
  tagstr[0] = '\0';
  no_tagstr[0] = '\0';
}

char* PostBrill_FP::get_str(void) {
  return instr;
}

// Added by Halil
// Tokenize input and call filter_string
// strchr() is used, since strtok somehow didn't work
string PostBrill_FP::filter_str_sub(char *str) {
   string output;
   char *p = str; 
   char *q;

   while ((q = strchr(p, '\n')) != NULL) {
     *q++ = '\0'; 
     output += filter_string_sub(p) + "\n";
     p = q;
   }
  output += filter_string_sub(p);
  return output;
}

// Added by Halil
// the version of filter_string that returns a string
string PostBrill_FP::filter_string_sub(char* str) {
  char *strpair, *loc, *aptr;
  char strc[MAX_STR];
  long i;
  int ans;
  string filtered_str;
  char *line;

  strcpy(instr,str);
  // make the words and tags vector for this string
  if(strpbrk(str,"abcdefghijklmnopqrstuvwxzyABCDEFGHIJKLMNOPQRSTUVWXYZ")) {
    strpair = strtok(str, " ");
    strcpy(strc,strpair);
    loc = strrchr(strc, '/'); 
    loc++;
    strcat(tagstr, loc);
    strcat(tagstr, " ");
    if(GoodTags.find(loc)) {
      Tags.push_back((plex->Btags).find(loc));
    }
    else {
      Tags.push_back((plex->Btags).find("NN"));
    }
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
      if(GoodTags.find(loc)) {
	Tags.push_back((plex->Btags).find(loc));
      }
      else {
	Tags.push_back((plex->Btags).find("NN"));
      }
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

    if(DEBUG) {
       for(i=0;i<Words.size();i++) {
 	cout << Words[i] << " " 
 	     << (plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1] << endl;
       }
    } 
  
    // apply filter to string and print result 
    filtered_str = apply_filter_sub();
    //cout << get_flag() << " | ";
    // if(DEBUG) {
    //cout << Rules[Rules.size()-1] << instr << " | ";
    // }
    //cout << last_no_tagstr << endl;
    
  }
  return filtered_str;
}

// Added by Halil 
// Version to return a string
string PostBrill_FP::apply_filter_sub() {
  long i,j;
  const char* errptr;
  const char* study_error;
  int erroffset;
  int offsets[MAX_STR];
  int cs,r;   
  pcre* pre = NULL;
  pcre_extra* pextra;
  char* cptr;
  string result_str;

  set_special_words_and_tags();
  result_str = apply_rules_sub();
  clear_all();

  return result_str;
}

// Added by Halil
// Version to return a string
string PostBrill_FP::apply_rules_sub() {
  string result_str;
  long i,j,k,found=0;
  const char* errptr;
  const char* study_error;
  int erroffset;
  int offsets[MAX_STR];
  int cs,r;   
  pcre* pre = NULL;
  pcre_extra* pextra;
  char* cptr;
  char* pptr;
  char* ccptr;
  const char** stringptr;
  char lcw[MAX_STR];
  char cw [MAX_STR];
  char rn [MAX_STR];

  if(Words.size() == Tags.size()) {
    vector<char*>::iterator p = Words.begin();
    vector<long>::iterator pp = Tags.begin();
    
    while(p != Words.end()) {
      while (pp != Tags.end()) {
	found=0;
	if(strcmp((plex->Btags).strmap+(plex->Btags).addr[*pp-1],"GENE")==0) {
	  j=k=0; 
	  strcpy(lcw,*p);
	  strcpy(cw,*p);
	 
	  for(i=0;lcw[i] != '\0';i++) {
	    // get lower case current word = lcw
	    if(lcw[i] >= 65 && lcw[i] <= 90) { 
	      (lcw[i])+=32;
	    }
	    // get cur word without / . -    = cw  
	    if(lcw[i] != 47 && lcw[i] != 46 && lcw[i] != 45) {
	      cw[k] = lcw[i]; k++;
	    }
	  }
	  cw[k] = '\0';

	  // remove if likely grant/contract numbers
	  if(!found && strstr(instr,"grant") || strstr(instr,"Grant") ||
	     strstr(instr,"contract") || strstr(instr,"Contract")) {
	    result_str += string(*p) + "/NN ";
//	    cout << *p << "/NN "; 
	    found = 1;  
	  }

	  // remove .I and .D
	  r      = pcre_exec(Re[0], Extra[0], lcw, 
			     strlen(lcw), 0, 0, offsets, MAX_STR);
	  if(!found && r > 0) {
	    result_str += string(*p) + "/CD ";
// 	    cout << *p << "/CD "; 
	    found = 1;  
	  }

	  // remove stats etc.
	  if(!found && p < (Words.end()-2)) {
	    cptr = *(p+=1);  p--;
	    r      = pcre_exec(Re[1], Extra[1], cptr, 
			       strlen(cptr), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cptr = *(p+=2);  p-=2;
	      r      = pcre_exec(Re[2], Extra[2], cptr, 
				 strlen(cptr), 0, 0, offsets, MAX_STR);
	      if(r > 0) {
		result_str += string(*p) + "/NN ";
// 		cout << *p << "/NN "; 
		found = 1;  
	      }
	    }
	  }

	  // filter reagents etc.
	  if(!found && (Freq.find(lcw) || Freq.find(cw))
	     && !(Genes.find(lcw) || Genes.find(cw))) {
	    result_str += string(*p) + "/NN ";
//            cout << *p << "/NN "; 
	    found = 1; 
	  }

	  // try to filter cell names
	  if(!found && Ce.find(cw) && !Genes.find(lcw)) {	  
	    cptr = new char[MAX_STR];
	    strcpy(cptr, *p);
	    strcat(cptr, "-transfected");
	    pptr = new char[MAX_STR];
	    strcpy(pptr, *p);
	    strcat(pptr, " transfected");
	    ccptr = new char[MAX_STR];
	    strcpy(ccptr, *p);
	    strcat(ccptr, " -transfected");
	    if(strstr(no_tagstr, "cell") ||
	       strstr(no_tagstr, "target")) {
	      if(!strstr(no_tagstr, cptr) &&
		 !strstr(no_tagstr, pptr) &&
		 !strstr(no_tagstr, ccptr)) {
		 result_str += string(*p) + "/CELL ";
// 		cout << *p << "/CELL ";
		*pp = (plex->Btags).find("CELL");
		found=1;
	      }
	    }
	    delete cptr; delete pptr; delete ccptr;
	  }
	  if(!found && p > (Words.begin()+3)) {
	    cptr = *(p-=3);  p+=3;
	    r      = pcre_exec(Re[3], Extra[3], cptr, 
			       strlen(cptr), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cptr = *(p-=2);  p+=2;
	      r      = pcre_exec(Re[4], Extra[4], cptr, 
				 strlen(cptr), 0, 0, offsets, MAX_STR);
	      if(r > 0) {
// 		cout << *p << "/CELL ";
		result_str += string(*p) + "/CELL ";
		*pp = (plex->Btags).find("CELL");
		found=1;
	      }
	    }
	  }
	  if(!found && p > (Words.begin()+2)) {
	    cptr = *(p-=2);  p+=2;
	    r      = pcre_exec(Re[3], Extra[3], cptr, 
			       strlen(cptr), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cptr = *(p-=1);  p+=1;
	      r      = pcre_exec(Re[4], Extra[4], cptr, 
				 strlen(cptr), 0, 0, offsets, MAX_STR);
	      if(r > 0) {
// 		cout << *p << "/CELL ";
		result_str += string(*p) + "/CELL ";
		*pp = (plex->Btags).find("CELL");
		found=1;
	      }
	    }
	  }
	  if(!found && p > (Words.begin()+2)) {
	    cptr = *(p-=2);  p+=2;
	    r      = pcre_exec(Re[5], Extra[5], cptr, 
			       strlen(cptr), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cptr = *(p-=1);  p+=1;
	      r      = pcre_exec(Re[6], Extra[6], cptr, 
				 strlen(cptr), 0, 0, offsets, MAX_STR);
	      if(r > 0) {
// 		cout << *p << "/CELL ";
	        result_str += string(*p) + "/CELL ";
		*pp = (plex->Btags).find("CELL");
		found=1;
	      }
	    }
	  }
	  if(!found && p > (Words.begin()+1)) {
	    cptr = *(p-=1);  p+=1;
	    r      = pcre_exec(Re[5], Extra[5], cptr, 
			       strlen(cptr), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
// 	      cout << *p << "/CELL ";
	      result_str += string(*p) + "/CELL ";
	      *pp = (plex->Btags).find("CELL");
	      found=1;
	    }
	  }
	  if(!found && p > (Words.begin()+1)) {
	    cptr = *(p-=1);  p+=1;
	    r      = pcre_exec(Re[7], Extra[7], cptr, 
			       strlen(cptr), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
// 	      cout << *p << "/CELL ";
	      result_str += string(*p) + "/CELL ";
	      *pp = (plex->Btags).find("CELL");
	      found=1;
	    }
	  }

	  // drug names/chemicals
	  if(!found) {
	    r      = pcre_exec(Re[8], Extra[8], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
// 	      cout << *p << "/CHEM ";
	      result_str += string(*p) + "/CHEM ";
	      found=1;
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[9], Extra[9], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      result_str += string(*p) + "/CHEM ";
// 	      cout << *p << "/CHEM ";
	      found=1;
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[10], Extra[10], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      result_str += string(*p) + "/CHEM ";
// 	      cout << *p << "/CHEM ";
	      found=1;
	    }
	  }

	  // roman numerals
	  if(!found) {
	    k=0;
	    for(i=0;lcw[i] != '\0';i++) {
	      if(lcw[i] != 'i' && lcw[i] != 'v' && lcw[i] != 'x') {
		rn[k] = lcw[i]; k++;
	      }
	    }
	    rn[k] = '\0';
	    r      = pcre_exec(Re[11], Extra[11], rn, 
			       strlen(rn), 0, 0, offsets, MAX_STR);
	    if(r == -1) {
// 	      cout << *p << "/NN ";
	      result_str += string(*p) + "/NN ";
	      found=1;
	    }
	  }

	  // restriction enzymes
	  if(!found && Ren.find(cw)) {
// 	    cout << *p << "/RENZYME ";
	      result_str += string(*p) + "/RENZYME ";
	      found=1;
	  }
	
	  // amino acids
	  if(!found) {
	    r      = pcre_exec(Re[12], Extra[12], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	       r      = pcre_exec(Re[13], Extra[13], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	       if(r == -1) {
		 cptr = new char[MAX_STR];
		 strcpy(cptr,lcw);
		 cptr[offsets[3]] = '\0';
		 cptr += offsets[2];
		 if(Aa.find(cptr)) {
// 		   cout << *p << "/NN ";
		   result_str += string(*p) + "/NN ";
		   found=1;
		 }
		 cptr -= offsets[2];
		 delete cptr;
	       }
	    }
	  }
	  
	  // chromosomal locations
	  if(!found) {
	    r      = pcre_exec(Re[14], Extra[14], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
// 	      cout << *p << "/CD ";
	      result_str += string(*p) + "/CD ";
	      found=1;
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[15], Extra[15], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
// 	      cout << *p << "/CD ";
	      result_str += string(*p) + "/CD ";
	      found=1;
	    }
	  }
	  
	  // too general
	  if(!found) {
	    r      = pcre_exec(Re[16], Extra[16], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cptr = new char[MAX_STR];
	      strcpy(cptr,lcw);
	      cptr[offsets[2]] = '\0';
	      if(Gen.find(cptr)) {
// 		cout << *p << "/NN ";
		result_str += string(*p) + "/NN ";
		found=1;
	      }
	      delete cptr;
	    }
	  }
	  if(!found && Gen.find(lcw)) {
	    result_str += string(*p) + "/NN ";
// 	    cout << *p << "/NN ";
	    found=1;
	  }

	  // ends in dot
	  if(!found) {
	    r      = pcre_exec(Re[17], Extra[17], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
// 	      cout << *p << "/NN ";
	      result_str += string(*p) + "/NN ";
	      found=1;
	    }
	  }
	  
	  if(!found && strchr(lcw,'=')) {
// 	    cout << *p << "/NN ";
	    result_str += string(*p) + "/NN ";
	    found=1;
	  }
	  
	  // dashes etc.
	  if(!found) {
	    r      = pcre_exec(Re[18], Extra[18], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
// 	      cout << *p << "/NN ";
	      result_str += string(*p) + "/NN ";
	      found=1;
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[19], Extra[19], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
// 	      cout << *p << "/NN ";
	      result_str += string(*p) + "/NN ";
	      found=1;
	    }
	  }    
	  if(!found) {
	    r      = pcre_exec(Re[20], Extra[20], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
// 	      cout << *p << "/NN ";
	      result_str += string(*p) + "/NN ";
	      found=1;
	    }
	  }    
	  if(!found) {
	    r      = pcre_exec(Re[21], Extra[21], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
// 	      cout << *p << "/NN ";
	      result_str += string(*p) + "/NN ";
	      found=1;
	    }
	  }    
	  if(!found) {
	    r      = pcre_exec(Re[22], Extra[22], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
// 	      cout << *p << "/NN ";
	      result_str += string(*p) + "/NN ";
	      found=1;
	    }
	  }    

	  // numbers etc.
	  if(!found) {
	    r      = pcre_exec(Re[23], Extra[23], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
// 	      cout << *p << "/CD ";
	      result_str += string(*p) + "/CD ";
	      found=1;
	    }
	  }    
	  if(!found) {
	    r      = pcre_exec(Re[24], Extra[24], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
// 	      cout << *p << "/CD ";
	      result_str += string(*p) + "/CD ";
	      found=1;
	    }
	  }    
	  if(!found) {
	    r      = pcre_exec(Re[25], Extra[25], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
// 	      cout << *p << "/NN ";
	      result_str += string(*p) + "/NN ";
	      found=1;
	    }
	  }    
	  if(!found && Bad_After_Num.find(lcw)) {
	    r      = pcre_exec(Re[2], Extra[2], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      if(strlen(lcw)>=3) {
// 		cout << *p << "/NN ";
	        result_str += string(*p) + "/NN ";
		found=1;
	      }
	    }    
	  }

	  // unbalanced parens
	  if(!found) {
	    r      = pcre_exec(Re[26], Extra[26], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      r      = pcre_exec(Re[27], Extra[27], lcw, 
				 strlen(lcw), 0, 0, offsets, MAX_STR);
	      if(r == -1) {
// 		cout << *p << "/NN ";
	        result_str += string(*p) + "/NN ";
		found=1;
	      }
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[28], Extra[28], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      r      = pcre_exec(Re[29], Extra[29], lcw, 
				 strlen(lcw), 0, 0, offsets, MAX_STR);
	      if(r == -1) {
// 		cout << *p << "/NN ";
	        result_str += string(*p) + "/NN ";
		found=1;
	      }
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[27], Extra[27], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      r      = pcre_exec(Re[30], Extra[30], lcw, 
				 strlen(lcw), 0, 0, offsets, MAX_STR);
	      if(r == -1) {
// 		cout << *p << "/NN ";
	        result_str += string(*p) + "/NN ";
		found=1;
	      }
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[29], Extra[29], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      r      = pcre_exec(Re[31], Extra[31], lcw, 
				 strlen(lcw), 0, 0, offsets, MAX_STR);
	      if(r == -1) {
// 		cout << *p << "/NN ";
	        result_str += string(*p) + "/NN ";
		found=1;
	      }
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[32], Extra[32], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      r      = pcre_exec(Re[34], Extra[34], lcw, 
				 strlen(lcw), 0, 0, offsets, MAX_STR);
	      if(r == -1) {
// 		cout << *p << "/NN ";
	        result_str += string(*p) + "/NN ";
		found=1;
	      }
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[33], Extra[33], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      r      = pcre_exec(Re[35], Extra[35], lcw, 
				 strlen(lcw), 0, 0, offsets, MAX_STR);
	      if(r == -1) {
// 		cout << *p << "/NN ";
	        result_str += string(*p) + "/NN ";
		found=1;
	      }
	    }
	  }
	  
	  // dots, pluses
	  if(!found) {
	    r      = pcre_exec(Re[36], Extra[36], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
// 	      cout << *p << "/NN ";
	      result_str += string(*p) + "/NN ";
	      found=1;
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[37], Extra[37], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
// 	      cout << *p << "/NN ";
	      result_str += string(*p) + "/NN ";
	      found=1;
	    }
	  }

	  // bad after number
	  if(!found) {
	    r      = pcre_exec(Re[38], Extra[38], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cptr = new char[MAX_STR];
	      strcpy(cptr,lcw);
	      cptr += offsets[2];
	      cptr[offsets[3]] = '\0';
	      if(Bad_After_Num.find(cptr)) {
		r      = pcre_exec(Re[41], Extra[41], lcw, 
				   strlen(lcw), 0, 0, offsets, MAX_STR);
		if(r > 0) {
// 		  cout << *p << "/NN ";
		  result_str += string(*p) + "/NN ";
		  found=1;
		}
	      }
	      cptr -= offsets[2];
	      delete cptr;
	    }
	  }
	  if(!found) {
	      r      = pcre_exec(Re[39], Extra[39], lcw,
			       strlen(lcw), 0, 0, offsets, MAX_STR);      
	      if(r > 0) {
		 ccptr = new char[MAX_STR];
		 strcpy(ccptr,lcw);
		 ccptr[offsets[2]] = '\0';	
		 //cout << "---- " << ccptr << endl;
		 if(Bad_After_Num.find(ccptr)) {
		   r      = pcre_exec(Re[2], Extra[2], lcw,
			       strlen(lcw), 0, 0, offsets, MAX_STR);      
		   if(r > 0) {
// 		     cout << *p << "/NN ";
		     result_str += string(*p) + "/NN ";
		     found=1;
		   }
		 }
		 delete ccptr;
	      }      
	  }
	      
	  // try to get other FP CELLS in groups of cells
	  if(!found && p > (Words.begin()+2)) {
	    pp-=2; 
	    cptr = (plex->Btags).strmap+(plex->Btags).addr[(*pp-1)];
	    //cout << "---- " << *p << " " << *pp << " " << cptr << endl;
	    pp+=2;
	    r      = pcre_exec(Re[40], Extra[40], cptr,
			       strlen(cptr), 0, 0, offsets, MAX_STR);      
	    if(r > 0) {
// 	      cout << *p << "/CELL ";
	      result_str += string(*p) + "/CELL ";
	      *pp = (plex->Btags).find("CELL");
	      found=1;
	    }
	  }
	  if(!found && p > (Words.begin()+1)) {
	    pp-=1; 
	    cptr = (plex->Btags).strmap+(plex->Btags).addr[(*pp-1)];
	    pp+=1;
	    r      = pcre_exec(Re[40], Extra[40], cptr,
			       strlen(cptr), 0, 0, offsets, MAX_STR);      
	    if(r > 0) {
// 	      cout << *p << "/CELL ";
	      result_str += string(*p) + "/CELL ";
	      *pp = (plex->Btags).find("CELL");
	      found=1;
	    }
	  }    	  
	  if(!found && p < (Words.end()-2)) {
	    pp+=2;
	    cptr = (plex->Btags).strmap+(plex->Btags).addr[(*pp-1)];
	    pp-=2;
	    r      = pcre_exec(Re[40], Extra[40], cptr,
			       strlen(cptr), 0, 0, offsets, MAX_STR);      
	    if(r > 0) {
// 	      cout << *p << "/CELL ";
	      result_str += string(*p) + "/CELL ";
	      *pp = (plex->Btags).find("CELL");
	      found=1;
	    }
	  }    
	  if(!found && p < (Words.end()-1)) {
	    pp+=1;
	    cptr = (plex->Btags).strmap+(plex->Btags).addr[(*pp-1)];
	    pp-=1;
	    r      = pcre_exec(Re[40], Extra[40], cptr,
			       strlen(cptr), 0, 0, offsets, MAX_STR);      
	    if(r > 0) {
// 	      cout << *p << "/CELL ";
	      result_str += string(*p) + "/CELL ";
	      *pp = (plex->Btags).find("CELL");
	      found=1;
	    }
	  }
    
	  // try to get ORGs
	   if(!found && Org.find(lcw)) {
// 	     cout << *p << "/ORG ";
	     result_str += string(*p) + "/ORG ";
	     found=1;
	   }
	}

	if (!found) {
// 	  cout << *p << "/" << (plex->Btags).strmap+(plex->Btags).addr[*pp-1] << " ";
	 result_str += string(*p) + "/" + string((plex->Btags).strmap+(plex->Btags).addr[*pp-1]) + " ";
	}
	p++;pp++;
      }
    }
//     cout << endl;
  }
  return result_str;
}


void PostBrill_FP::filter_string(char* str) {
  char *strpair, *loc, *aptr;
  char strc[MAX_STR];
  long i;
  int ans;

  strcpy(instr,str);
  
  // make the words and tags vector for this string
  if(strpbrk(str,"abcdefghijklmnopqrstuvwxzyABCDEFGHIJKLMNOPQRSTUVWXYZ")  &&
     (strstr(str, "/NN") || strstr(str, "/V") || strstr(str, "/CD")
      || strstr(str,"/GENE") || strstr(str,"/JJ") || strstr(str,"/DT"))) {
    strpair = strtok(str, " ");
    strcpy(strc,strpair);
    loc = strrchr(strc, '/'); 
    loc++;
    strcat(tagstr, loc);
    strcat(tagstr, " ");
    if(GoodTags.find(loc)) {
      Tags.push_back((plex->Btags).find(loc));
    }
    else {
      Tags.push_back((plex->Btags).find("NN"));
    }
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
      if(GoodTags.find(loc)) {
	Tags.push_back((plex->Btags).find(loc));
      }
      else {
	Tags.push_back((plex->Btags).find("NN"));
      }
      *(--loc) = '\0';
      aptr = new char[strlen(strc)+1];
      strcpy(aptr,strc);
      //cout << aptr << endl;
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
    
    if(DEBUG) {
      for(i=0;i<Words.size();i++) {
	cout << Words[i] << " " 
	     << (plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1] << endl;
      }
    }
  
    // apply filter to string and print result 
    apply_filter();
    //cout << get_flag() << " | ";
    /*if(DEBUG) {
      cout << Rules[Rules.size()-1] << instr << " | ";
      }*/
    //cout << last_no_tagstr << endl;
  }
}

void PostBrill_FP::filter_file(ifstream& ifs) {
  char s[MAX_STR];
  long i;

  while(ifs.getline(s, MAX_STR,'\n')) {
    if(s) {filter_string(s);}    
  }
}

void PostBrill_FP::apply_filter() {
  long i,j;
  const char* errptr;
  const char* study_error;
  int erroffset;
  int offsets[MAX_STR];
  int cs,r;   
  pcre* pre = NULL;
  pcre_extra* pextra;
  char* cptr;
  
  set_special_words_and_tags();
  apply_rules();
  clear_all();
}

void PostBrill_FP::apply_rules() {
  long i,j,k,found=0;
  const char* errptr;
  const char* study_error;
  int erroffset;
  int offsets[MAX_STR];
  int cs,r;   
  pcre* pre = NULL;
  pcre_extra* pextra;
  char* cptr;
  char* pptr;
  char* ccptr;
  const char** stringptr;
  char lcw[MAX_STR];
  char cw [MAX_STR];
  char rn [MAX_STR];

  if(Words.size() == Tags.size()) {
    vector<char*>::iterator p = Words.begin();
    vector<long>::iterator pp = Tags.begin();
    
    while(p != Words.end()) {
      while (pp != Tags.end()) {
	found=0;
	if(strcmp((plex->Btags).strmap+(plex->Btags).addr[*pp-1],"GENE")==0) {
	  j=k=0; 
	  strcpy(lcw,*p);
	  strcpy(cw,*p);
	 
	  for(i=0;lcw[i] != '\0';i++) {
	    // get lower case current word = lcw
	    if(lcw[i] >= 65 && lcw[i] <= 90) { 
	      (lcw[i])+=32;
	    }
	    // get cur word without / . -    = cw  
	    if(lcw[i] != 47 && lcw[i] != 46 && lcw[i] != 45) {
	      cw[k] = lcw[i]; k++;
	    }
	  }
	  cw[k] = '\0';
	  
	  // remove if likely grant/contract numbers
	  if(!found && strstr(instr,"grant") || strstr(instr,"Grant") ||
	     strstr(instr,"contract") || strstr(instr,"Contract")) {
	    cout << *p << "/NN "; 
	    found = 1;  
	  }

	  // remove .I and .D
	  r      = pcre_exec(Re[0], Extra[0], lcw, 
			     strlen(lcw), 0, 0, offsets, MAX_STR);
	  if(!found && r > 0) {
	    cout << *p << "/CD "; 
	    found = 1;  
	  }
  
	  // remove stats etc.
	  if(!found && p < (Words.end()-2)) {
	    cptr = *(p+=1);  p--;
	    r      = pcre_exec(Re[1], Extra[1], cptr, 
			       strlen(cptr), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cptr = *(p+=2);  p-=2;
	      r      = pcre_exec(Re[2], Extra[2], cptr, 
				 strlen(cptr), 0, 0, offsets, MAX_STR);
	      if(r > 0) {
		cout << *p << "/NN "; 
		found = 1;  
	      }
	    }
	  }
	  // filter reagents etc.
	  if(!found && (Freq.find(lcw) || Freq.find(cw))
	     && !(Genes.find(lcw) || Genes.find(cw))) {
	    cout << *p << "/NN "; 
	    found = 1; 
	  }

	  // try to filter cell names
	  if(!found && Ce.find(cw) && !Genes.find(lcw)) {	  
	    cptr = new char[MAX_STR];
	    strcpy(cptr, *p);
	    strcat(cptr, "-transfected");
	    pptr = new char[MAX_STR];
	    strcpy(pptr, *p);
	    strcat(pptr, " transfected");
	    ccptr = new char[MAX_STR];
	    strcpy(ccptr, *p);
	    strcat(ccptr, " -transfected");
	    if(strstr(no_tagstr, "cell") ||
	       strstr(no_tagstr, "target")) {
	      if(!strstr(no_tagstr, cptr) &&
		 !strstr(no_tagstr, pptr) &&
		 !strstr(no_tagstr, ccptr)) {
		cout << *p << "/CELL ";
		*pp = (plex->Btags).find("CELL");
		found=1;
	      }
	    }
	    delete [] cptr; delete [] pptr; delete [] ccptr;
	  }
	  if(!found && p > (Words.begin()+3)) {
	    cptr = *(p-=3);  p+=3;
	    r      = pcre_exec(Re[3], Extra[3], cptr, 
			       strlen(cptr), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cptr = *(p-=2);  p+=2;
	      r      = pcre_exec(Re[4], Extra[4], cptr, 
				 strlen(cptr), 0, 0, offsets, MAX_STR);
	      if(r > 0) {
		cout << *p << "/CELL ";
		*pp = (plex->Btags).find("CELL");
		found=1;
	      }
	    }
	  }
	  if(!found && p > (Words.begin()+2)) {
	    cptr = *(p-=2);  p+=2;
	    r      = pcre_exec(Re[3], Extra[3], cptr, 
			       strlen(cptr), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cptr = *(p-=1);  p+=1;
	      r      = pcre_exec(Re[4], Extra[4], cptr, 
				 strlen(cptr), 0, 0, offsets, MAX_STR);
	      if(r > 0) {
		cout << *p << "/CELL ";
		*pp = (plex->Btags).find("CELL");
		found=1;
	      }
	    }
	  }
	  if(!found && p > (Words.begin()+2)) {
	    cptr = *(p-=2);  p+=2;
	    r      = pcre_exec(Re[5], Extra[5], cptr, 
			       strlen(cptr), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cptr = *(p-=1);  p+=1;
	      r      = pcre_exec(Re[6], Extra[6], cptr, 
				 strlen(cptr), 0, 0, offsets, MAX_STR);
	      if(r > 0) {
		cout << *p << "/CELL ";
		*pp = (plex->Btags).find("CELL");
		found=1;
	      }
	    }
	  }
	  if(!found && p > (Words.begin()+1)) {
	    cptr = *(p-=1);  p+=1;
	    r      = pcre_exec(Re[5], Extra[5], cptr, 
			       strlen(cptr), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cout << *p << "/CELL ";
	      *pp = (plex->Btags).find("CELL");
	      found=1;
	    }
	  }
	  if(!found && p > (Words.begin()+1)) {
	    cptr = *(p-=1);  p+=1;
	    r      = pcre_exec(Re[7], Extra[7], cptr, 
			       strlen(cptr), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cout << *p << "/CELL ";
	      *pp = (plex->Btags).find("CELL");
	      found=1;
	    }
	  }

	  // drug names/chemicals
	  if(!found) {
	    r      = pcre_exec(Re[8], Extra[8], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cout << *p << "/CHEM ";
	      found=1;
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[9], Extra[9], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cout << *p << "/CHEM ";
	      found=1;
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[10], Extra[10], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cout << *p << "/CHEM ";
	      found=1;
	    }
	  }

	  // roman numerals
	  if(!found) {
	    k=0;
	    for(i=0;lcw[i] != '\0';i++) {
	      if(lcw[i] != 'i' && lcw[i] != 'v' && lcw[i] != 'x') {
		rn[k] = lcw[i]; k++;
	      }
	    }
	    rn[k] = '\0';
	    r      = pcre_exec(Re[11], Extra[11], rn, 
			       strlen(rn), 0, 0, offsets, MAX_STR);
	    if(r == -1) {
	      cout << *p << "/NN ";
	      found=1;
	    }
	  }

	  // restriction enzymes
	  if(!found && Ren.find(cw)) {
	    cout << *p << "/RENZYME ";
	      found=1;
	  }
	
	  // amino acids
	  if(!found) {
	    r      = pcre_exec(Re[12], Extra[12], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	       r      = pcre_exec(Re[13], Extra[13], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	       if(r == -1) {
		 cptr = new char[MAX_STR];
		 strcpy(cptr,lcw);
		 cptr[offsets[3]] = '\0';
		 cptr += offsets[2];
		 if(Aa.find(cptr)) {
		   cout << *p << "/NN ";
		   found=1;
		 }
		 cptr -= offsets[2];
		 if(cptr) {delete [] cptr;}
	       }
	    }
	  }
	  
	  // chromosomal locations
	  if(!found) {
	    r      = pcre_exec(Re[14], Extra[14], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cout << *p << "/CD ";
	      found=1;
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[15], Extra[15], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cout << *p << "/CD ";
	      found=1;
	    }
	  }
	  
	  // too general
	  if(!found) {
	    r      = pcre_exec(Re[16], Extra[16], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cptr = new char[MAX_STR];
	      strcpy(cptr,lcw);
	      cptr[offsets[2]] = '\0';
	      if(Gen.find(cptr)) {
		cout << *p << "/NN ";
		found=1;
	      }
	      if(cptr) {delete [] cptr;}
	    }
	  }
	  if(!found && Gen.find(lcw)) {
	    cout << *p << "/NN ";
	    found=1;
	  }

	  // ends in dot
	  if(!found) {
	    r      = pcre_exec(Re[17], Extra[17], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cout << *p << "/NN ";
	      found=1;
	    }
	  }
	  
	  if(!found && strchr(lcw,'=')) {
	    cout << *p << "/NN ";
	    found=1;
	  }
	  
	  // dashes etc.
	  if(!found) {
	    r      = pcre_exec(Re[18], Extra[18], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cout << *p << "/NN ";
	      found=1;
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[19], Extra[19], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cout << *p << "/NN ";
	      found=1;
	    }
	  }    
	  if(!found) {
	    r      = pcre_exec(Re[20], Extra[20], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cout << *p << "/NN ";
	      found=1;
	    }
	  }    
	  if(!found) {
	    r      = pcre_exec(Re[21], Extra[21], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cout << *p << "/NN ";
	      found=1;
	    }
	  }    
	  if(!found) {
	    r      = pcre_exec(Re[22], Extra[22], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cout << *p << "/NN ";
	      found=1;
	    }
	  }    

	  // numbers etc.
	  if(!found) {
	    r      = pcre_exec(Re[23], Extra[23], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cout << *p << "/CD ";
	      found=1;
	    }
	  }    
	  if(!found) {
	    r      = pcre_exec(Re[24], Extra[24], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cout << *p << "/CD ";
	      found=1;
	    }
	  }    
	  if(!found) {
	    r      = pcre_exec(Re[25], Extra[25], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cout << *p << "/NN ";
	      found=1;
	    }
	  }    
	  if(!found && Bad_After_Num.find(lcw)) {
	    r      = pcre_exec(Re[2], Extra[2], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      if(strlen(lcw)>=3) {
		cout << *p << "/NN ";
		found=1;
	      }
	    }    
	  }

	  // unbalanced parens
	  if(!found) {
	    r      = pcre_exec(Re[26], Extra[26], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      r      = pcre_exec(Re[27], Extra[27], lcw, 
				 strlen(lcw), 0, 0, offsets, MAX_STR);
	      if(r == -1) {
		cout << *p << "/NN ";
		found=1;
	      }
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[28], Extra[28], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      r      = pcre_exec(Re[29], Extra[29], lcw, 
				 strlen(lcw), 0, 0, offsets, MAX_STR);
	      if(r == -1) {
		cout << *p << "/NN ";
		found=1;
	      }
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[27], Extra[27], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      r      = pcre_exec(Re[30], Extra[30], lcw, 
				 strlen(lcw), 0, 0, offsets, MAX_STR);
	      if(r == -1) {
		cout << *p << "/NN ";
		found=1;
	      }
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[29], Extra[29], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      r      = pcre_exec(Re[31], Extra[31], lcw, 
				 strlen(lcw), 0, 0, offsets, MAX_STR);
	      if(r == -1) {
		cout << *p << "/NN ";
		found=1;
	      }
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[32], Extra[32], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      r      = pcre_exec(Re[34], Extra[34], lcw, 
				 strlen(lcw), 0, 0, offsets, MAX_STR);
	      if(r == -1) {
		cout << *p << "/NN ";
		found=1;
	      }
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[33], Extra[33], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      r      = pcre_exec(Re[35], Extra[35], lcw, 
				 strlen(lcw), 0, 0, offsets, MAX_STR);
	      if(r == -1) {
		cout << *p << "/NN ";
		found=1;
	      }
	    }
	  }
	  
	  // dots, pluses
	  if(!found) {
	    r      = pcre_exec(Re[36], Extra[36], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cout << *p << "/NN ";
	      found=1;
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[37], Extra[37], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cout << *p << "/NN ";
	      found=1;
	    }
	  }

	  // bad after number
	  if(!found) {
	    r      = pcre_exec(Re[38], Extra[38], lcw, 
			       strlen(lcw), 0, 0, offsets, MAX_STR);
	    if(r > 0) {
	      cptr = new char[MAX_STR];
	      strcpy(cptr,lcw);
	      cptr += offsets[2];
	      cptr[offsets[3]] = '\0';
	      if(Bad_After_Num.find(cptr)) {
		r      = pcre_exec(Re[41], Extra[41], lcw, 
				   strlen(lcw), 0, 0, offsets, MAX_STR);
		if(r > 0) {
		  cout << *p << "/NN ";
		  found=1;
		}
	      }
	      cptr -= offsets[2];
	      if(cptr) {delete [] cptr;}
	    }
	  }
	  if(!found) {
	      r      = pcre_exec(Re[39], Extra[39], lcw,
			       strlen(lcw), 0, 0, offsets, MAX_STR);      
	      if(r > 0) {
		 ccptr = new char[MAX_STR];
		 strcpy(ccptr,lcw);
		 ccptr[offsets[2]] = '\0';	
		 //cout << "---- " << ccptr << endl;
		 if(Bad_After_Num.find(ccptr)) {
		   r      = pcre_exec(Re[2], Extra[2], lcw,
			       strlen(lcw), 0, 0, offsets, MAX_STR);      
		   if(r > 0) {
		     cout << *p << "/NN ";
		     found=1;
		   }
		 }
		 delete [] ccptr;
	      }      
	  }
	      
	  // try to get other FP CELLS in groups of cells
	  if(!found && p > (Words.begin()+2)) {
	    pp-=2; 
	    cptr = (plex->Btags).strmap+(plex->Btags).addr[(*pp-1)];
	    //cout << "---- " << *p << " " << *pp << " " << cptr << endl;
	    pp+=2;
	    r      = pcre_exec(Re[40], Extra[40], cptr,
			       strlen(cptr), 0, 0, offsets, MAX_STR);      
	    if(r > 0) {
	      cout << *p << "/CELL ";
	      *pp = (plex->Btags).find("CELL");
	      found=1;
	    }
	  }
	  if(!found && p > (Words.begin()+1)) {
	    pp-=1; 
	    cptr = (plex->Btags).strmap+(plex->Btags).addr[(*pp-1)];
	    pp+=1;
	    r      = pcre_exec(Re[40], Extra[40], cptr,
			       strlen(cptr), 0, 0, offsets, MAX_STR);      
	    if(r > 0) {
	      cout << *p << "/CELL ";
	      *pp = (plex->Btags).find("CELL");
	      found=1;
	    }
	  }    	  
	  if(!found && p < (Words.end()-2)) {
	    pp+=2;
	    cptr = (plex->Btags).strmap+(plex->Btags).addr[(*pp-1)];
	    pp-=2;
	    r      = pcre_exec(Re[40], Extra[40], cptr,
			       strlen(cptr), 0, 0, offsets, MAX_STR);      
	    if(r > 0) {
	      cout << *p << "/CELL ";
	      *pp = (plex->Btags).find("CELL");
	      found=1;
	    }
	  }    
	  if(!found && p < (Words.end()-1)) {
	    pp+=1;
	    cptr = (plex->Btags).strmap+(plex->Btags).addr[(*pp-1)];
	    pp-=1;
	    r      = pcre_exec(Re[40], Extra[40], cptr,
			       strlen(cptr), 0, 0, offsets, MAX_STR);      
	    if(r > 0) {
	      cout << *p << "/CELL ";
	      *pp = (plex->Btags).find("CELL");
	      found=1;
	    }
	  }
    
	  // try to get ORGs
	   if(!found && Org.find(lcw)) {
	     cout << *p << "/ORG ";
	     found=1;
	   }
	}

	if (!found) {
	  cout << *p << "/" << (plex->Btags).strmap+(plex->Btags).addr[*pp-1] << " ";
	}
	p++;pp++;
      }
    }
    cout << endl;
  }
}

void PostBrill_FP::set_special_words_and_tags() {
  char* noothing;
  noothing = new char[10];
  strcpy(noothing,"NOOTHING");
    
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
    wlastw = new char[10];
    strcpy(wlastw, noothing);
  }
  if(Words.size() > 2) {
    wnlastw   = new char[strlen(Words[Words.size()-2])+1];
    strcpy(wnlastw, Words[Words.size()-2]);
  }
  else {
    wnlastw = new char[strlen(wfirstw)+1];
    strcpy(wnlastw,wfirstw);
  }
  if(*wlastw == '.') {
    if(Words.size() > 2) {
      if(wlastw) {delete [] wlastw;}
      wlastw  = new char[strlen(Words[Words.size()-2])+1];
      strcpy(wlastw, Words[Words.size()-2]);
    }
    else {
      if(wlastw) {delete [] wlastw;}
      wlastw = new char[10];
      strcpy(wlastw, noothing);
    }
    if(Words.size() > 3) {
      if(wnlastw) {delete [] wnlastw;}
      wnlastw = new char[strlen(Words[Words.size()-3])+1];
      strcpy(wnlastw, Words[Words.size()-3]);
    }
    else {
       if(wnlastw) {delete [] wnlastw;}
      wnlastw = new char[strlen(wfirstw)+1];
      strcpy(wnlastw,wfirstw);
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
    tlastw = new char[10];
    strcpy(tlastw, noothing);
  }
  if(Tags.size() > 2) {
    tnlastw  = new char[strlen((plex->Btags).strmap+(plex->Btags).addr[Tags[Tags.size()-2]-1])+1];
    strcpy(tnlastw, (plex->Btags).strmap+(plex->Btags).addr[Tags[Tags.size()-2]-1]);
  }
  else {
    tnlastw = new char[strlen(tfirstw)+1];
    strcpy(tnlastw,tfirstw);
  }
  if(*tlastw == '.') {
    if(Tags.size() > 2) {
      if(tlastw) {delete [] tlastw;}
      tlastw  = new char[strlen((plex->Btags).strmap+(plex->Btags).addr[Tags[Tags.size()-2]-1])+1];
      strcpy(tlastw, (plex->Btags).strmap+(plex->Btags).addr[Tags[Tags.size()-2]-1]);
    }
    else {
      if(tlastw) {delete [] tlastw;}
      tlastw = new char[10];
      strcpy(tlastw, noothing);
    }
    if(Tags.size() > 3) {
      if(tnlastw) {delete [] tnlastw;}
      tnlastw = new char[strlen((plex->Btags).strmap+(plex->Btags).addr[Tags[Tags.size()-3]-1])+1];
      strcpy(tnlastw, (plex->Btags).strmap+(plex->Btags).addr[Tags[Tags.size()-3]-1]);
    }
    else {
      if(tnlastw) {delete [] tnlastw;}
      tnlastw = new char[strlen(tfirstw)+1];
      strcpy(tnlastw,tfirstw);
    }
  }
  //cout << wlastw << " " << wnlastw << " " << tlastw << " " << tnlastw << " " << wfirstw << " " << tfirstw << endl;
  delete [] noothing;
}

void PostBrill_FP::clear_all() {
  // clear words and tags vectors and firstw etc.
  free_vec_str(Words);
  Tags.clear();
  if(wlastw)  {delete [] wlastw;} 
  if(wnlastw) {delete [] wnlastw;}
  if(tlastw)  {delete [] tlastw;}
  if(tnlastw) {delete [] tnlastw;}
  if(wfirstw) {delete [] wfirstw;} 
  if(tfirstw) {delete [] tfirstw;}
  strcpy(last_no_tagstr, no_tagstr);
  strcpy(last_tagstr, tagstr);
  strcpy(no_tagstr, "");
  strcpy(tagstr, "");
}

void PostBrill_FP::free_vec_str(vector<char*> &vc) {
  vector<char*>::iterator p = vc.begin();
  while(p!=vc.end()){
    if(*p)delete [] *p;
    p++;
  }
  vc.clear();
}

// Added by Halil
// Prevents memory leak 
void PostBrill_FP::free_vec_pcre(vector<pcre*> &vc) {
  vector<pcre*>::iterator p = vc.begin();
  while(p!=vc.end()){
    if(*p) pcre_free(*p);
    p++;
  }
  vc.clear();
}

// Added by Halil
// Prevents memory leak 
void PostBrill_FP::free_vec_pcre_extra(vector<pcre_extra*> &vc) {
  vector<pcre_extra*>::iterator p = vc.begin();
  while(p!=vc.end()){
    if(*p) pcre_free(*p);
    p++;
  }
  vc.clear();
}

// Added by Halil
// Prevents memory leak 
void PostBrill_FP::gclose_PostBrill_FP(void) {
  Gen.gclose_htable_map();
  Aa.gclose_htable_map();
  Ren.gclose_htable_map();
  Ce.gclose_htable_map();
  Org.gclose_htable_map();
  Bad_After_Num.gclose_htable_map();
  Genes.gclose_htable_map();
  Freq.gclose_htable_map();
  GoodTags.gclose_htable_map();
  plex->gclose_bmatrix(); 
}
 
}
