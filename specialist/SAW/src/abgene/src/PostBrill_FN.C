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
#include "PostBrill_FN.h"
#include "Word.h"
#include <string>
#include <algorithm>
#include <vector>
#include <Hash.h>
#include <Blist.h>
#include <pcre.h>
#include <Thes.h>
#include "LinClass.h"
#include "Post.h"
#include "Regist.h"
#include "Nabor.h"
#include "BnBayes.h"

#define DEBUG 0
#define MAXTAGLEN 256     // max char length of pos tags 
#define MAXWORDLEN 100000    // max char length of words 

using namespace std;
namespace iret { 
 
PostBrill_FN::PostBrill_FN(void) :
  LC("geneterms")
  {
    Wrd=new Word; Wsw= new Word; 
    Wrd->byte_lim=64;
    Wrd->back_lim=48;
    Wrd->set_map(".,:;!?",'\024',LOWERCASE);
    Wrd->restore_map("\'");
    Wsw->byte_lim=0;
    Wsw->back_lim=32;
    Wsw->set_map("",'\024',LOWERCASE);
    Wsw->stop=0;
    Wsw->restore_map("<>&-,;:*!@%+={}[]()'./");
  }
  
PostBrill_FN::PostBrill_FN(const char *nam) :
  LC("geneterms")
{
  int lxn=strlen(nam);
  name=new char[lxn+1];
  strcpy(name,nam);
  Wrd = new Word(max_str,nam);
  Wsw = new Word(max_str,nam);
  Wrd->byte_lim=64;
  Wrd->back_lim=48;
  Wrd->set_map(".,:;!?",'\024',LOWERCASE);
  Wrd->restore_map("\'");
  Wsw->byte_lim=0;
  Wsw->back_lim=32;
  Wsw->set_map("",'\024',LOWERCASE);
  Wsw->stop=0;
  Wsw->restore_map("<>&-,;:*!@%+={}[]()'./");
 
}

PostBrill_FN::~PostBrill_FN(){  
  if(name!=NULL)delete [] name;
  if(Wrd!=NULL) delete Wrd;
  if(Wsw!=NULL) delete Wsw;
  if(plex!=NULL) delete plex;
  free_vec_pcre(Re);
  free_vec_pcre_extra(Extra);
  free_vec_str(Ids);
}

void PostBrill_FN::change_name(char *nam){
  if(name!=NULL)delete [] name;
  int lxn=strlen(nam);
  name=new char[lxn+1];
  strcpy(name,nam);
}


void PostBrill_FN::gopen_PostBrill_FN(void){
  ifstream fin_multi;
  ifstream fin_mg;
  ifstream fin_rstr_fn;
  int pflag=get_qflag(),flag=1;
  char cnam[XSTR];
  int  i;
  const char* errptr;
  const char* study_error;
  int erroffset;
  int offsets[XSTR];
  int r;
  char s[max_str];
  char* cptr;
  char* loc;
  char* ptr;
  pcre* ppre = NULL;
  pcre_extra* pextra;

  tagstr[0]='\0';
  no_tagstr[0]='\0';

  get_pathw(cnam,"postbrillset",name,"multi");
  // Modified fin to fin_multi, coredumped otherwise -- Halil
  fin_multi.open(cnam, ios::in);
  if(!fin_multi.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
  while(fin_multi.getline(cnam, max_str, '\n')) {  
    PM.add_key(cnam);
  }
  fin_multi.close();

  // Modified fin to fin_mg, coredumped otherwise -- Halil
  fin_mg.open("postbrillset_gene.mg",ios::in);
  while(fin_mg.getline(s, XSTR, '\n')) {
    loc = new char[strlen(s)+1];
    strcpy(loc,s);
    Ten_PM.add_key_count(loc);
  }
  fin_mg.close(); 

  LC.gopen_operate();
  Mg.change_name("mg");
  Mg.gopen_htable_map();
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
  J.change_name("j");
  J.gopen_htable_map();
  Lj.change_name("lj");
  Lj.gopen_htable_map();
  Freq.change_name("freq");
  Freq.gopen_htable_map();
  Gin.change_name("gin");
  Gin.gopen_htable_map();
  Lt.change_name("lt");
  Lt.gopen_htable_map();
  Stop.change_name("stop");
  Stop.gopen_htable_map();
  Gaft.change_name("gaft");
  Gaft.gopen_htable_map();
  Gbef.change_name("gbef");
  Gbef.gopen_htable_map();
  GoodTags.change_name("gt");
  GoodTags.gopen_htable_map();
  Bani.change_name("bani");
  Bani.gopen_htable_map();

  plex = new Lexicon(name);
  plex->gopen_bmatrix();

  get_pathw(cnam,"postbrillset",name,"rstr_fn");
  // Modified fin to fin_rstr_fn, coredumped otherwise -- Halil
  fin_rstr_fn.open(cnam, ios::in);
  if(!fin_rstr_fn.is_open()){cout << cnam << " failed to open" << endl;exit(0);}

  // Removed to prevent memory leak --Halil
//   ppre    = (pcre*) pcre_malloc (XSTR);
//   pextra  = (pcre_extra*) pcre_malloc (XSTR);
  while(fin_rstr_fn.getline(cnam,XSTR,'\n') != NULL) { 
    // Modified to prevent memory leak -- Halil
//       ppre    = pcre_compile(cnam, 0, &errptr, &erroffset, pcre_maketables()); 
      ppre = pcre_compile(cnam, 0, &errptr, &erroffset, NULL);
    pextra  = pcre_study(ppre, 0, &study_error);
    Re.push_back(ppre);
    Extra.push_back(pextra);
  }
  fin_rstr_fn.close();
}

char* PostBrill_FN::get_str(void) {
  return instr;
}

// Added by Halil
// Version that returns a string
// strtok() didn't somehow work, used strchr() instead
string PostBrill_FN::filter_str_sub(char *str) {
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
// Version that returns a string
string PostBrill_FN::filter_string_sub(char* str) {
  char *strpair, *loc, *aptr;
  char strc[XSTR];
  long i;
  int ans;
  string filtered_str;

  strcpy(instr,str);

  // make the words and tags vector for this string
  if(strpbrk(str,"abcdefghijklmnopqrstuvwxzyABCDEFGHIJKLMNOPQRSTUVWXYZ") &&
     (strpbrk(str, "/NN") || strpbrk(str, "/V") || strpbrk(str, "/CD"))) {
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
    
    
//    for(i=0;i<Words.size();i++) {
//      cout << Words[i] << " " 
//      << (plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1] << endl;
//    }
    
    filtered_str = apply_filter_sub();
   
//     cout << get_flag() << " | ";
//     if(DEBUG) {
//        cout << Rules[Rules.size()-1] << instr << " | ";
//     }
//     cout << last_no_tagstr << endl;
  }
  return filtered_str;
}


// Added by Halil
// Version that returns string
string PostBrill_FN::apply_filter_sub() {
  string result_str;
  
  //set_special_words_and_tags();
  result_str = apply_rules_sub();
  clear_all();

  return result_str;
}

// Added by Halil
// Version that returns string
string PostBrill_FN::apply_rules_sub() {
  string result_str;

  get_newgenes();   // single word known genes with good bef/aft
  get_knowngenes(); // multiword known genes with good bef/aft
  get_multigenes();
  result_str = get_contextgenes_sub();  

  return result_str;
}

// Added by Halil
// Version that returns string
string PostBrill_FN::get_contextgenes_sub() {
  string result_str;
  char *lcw, *tptr, *cptr,*aptr;
  int i,j,k,r,r1,r2,r3,r4,r5,r6,r7;
  const char* errptr;
  const char* study_error;
  int erroffset;
  int offsets[XSTR]; 
  ofstream ofs;
  vector<char*> ids;
  vector<char*> gene;
  char* beforeg;
  char* afterg;
  vector<char*> CBefore;
  vector<char*> CAfter;
  char *ptr;
  char *pptr;
  //cout << "context" << endl;
  //cout << fname << endl;
  
  /** weakest rules are 1,7,8 then 5,9 
      Brill tagger V-->N errors cause cascading problems **/

  //try some Brill contextual rules 
  if(Words.size() == Tags.size()) {
    for(i=0;i<Words.size();i++) { 
      lcw = lc(Words[i]);
      
      // ^[A-Z]\.$
      r      = pcre_exec(Re[63], Extra[63], Words[i],
			 strlen(Words[i]), 0, 0, offsets, XSTR);
      // \d|[A-Z][A-Z]|\S\S\S\Sin$|\S\S\ase$|[A-Z]$|[^aeiou]|^[A-Z][^aeiou]|^\S\S\S$
      r1      = pcre_exec(Re[64], Extra[64], Words[i],
			  strlen(Words[i]), 0, 0, offsets, XSTR);
      // ^[a-z]ase$|[a-z]\.[a-z]...ate$|chlors*$|ed$|ing$
      r2      = pcre_exec(Re[65], Extra[65], Words[i],
			  strlen(Words[i]), 0, 0, offsets, XSTR);
      // [A-Z]|-|\d
      r3      = pcre_exec(Re[66], Extra[66], Words[i],
			  strlen(Words[i]), 0, 0, offsets, XSTR);
      // ^et\.*$|^and$|\,|=|^19\d\d$|^20\d\d$
      r4      = pcre_exec(Re[63], Extra[63], lcw,
			  strlen(lcw), 0, 0, offsets, XSTR);
      // NN|CD|JJ|IN
      r5      = pcre_exec(Re[69], Extra[69], 
      (plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],
      strlen((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1]), 0, 0, offsets, XSTR);

      // ^et\.*$|^and$|\,|=|^19\d\d$|^20\d\d$
      if(Words.size()>0 && i<Words.size()-1) {
	ptr=lc(Words[i+1]);
	r6      = pcre_exec(Re[63], Extra[63], ptr,
			    strlen(ptr), 0, 0, offsets, XSTR);
	if (ptr) {delete [] ptr;}
      }
      // bad suffixes
      r7      = pcre_exec(Re[70], Extra[70], lcw,
			  strlen(lcw), 0, 0, offsets, XSTR);

      if(Ids.size()>0) {
	if((NotGene.search(Ids[Ids.size()-1])==1
	    && NotGene.exs_pair(Ids[Ids.size()-1],lcw)==0 
	    || NotGene.search(Ids[Ids.size()-1])==0) && 
	   strpbrk(lcw,"abcdefghijklmnopqrstuvwxyz") 
	   && r<0 && r1>0 && r2<0 && strlen(lcw)>1 
	   && !Bad_After_Num.find(lcw) 
	   && !Stop.find(lcw) && !Aa.find(lcw) 
	   && !Gen.find(lcw) && !Freq.find(lcw) 
	   && !Ce.find(lcw) && !Ren.find(lcw) 
	   && strpbrk(Words[i],"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") 
	   && !Org.find(lcw) 
	   && strcmp(Words[i], "a") && !strstr(lcw,"prime") 
	   && !NotGene.search(Ids[Ids.size()-1])==1
	   && NotGene.exs_pair(Ids[Ids.size()-1],lcw)==0
	   && strpbrk(lcw,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
	   && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"GENE") ) {
	  //GENE , x NOTVERB
	  if(Words.size()>0 && i>2 && i < Words.size()-1 && r7<0) { 
	      ptr=lc(Words[i-2]);
	      // remove r7<0 to be less conservative 
	      if (strcmp(Words[i-1],",")==0 
		  && strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i-2]-1],"GENE") 
		  && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"V") 
		  && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+1]-1],"IN") 
		  && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+1]-1],"MD")
		  && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"RB") 
		  && !Gen.find(ptr) 
		  && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+1]-1],"V")
		  && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+1]-1],"SYM")) {
		  // cout << "1 " << Words[i] << " changed to CONTEXTGENE" << endl;
		  Tags[i] = (plex->Btags).find("CONTEXTGENE");
		  if(r7<0) {NewContextGene.add_element(Ids[Ids.size()-1],lcw);}
	      }
	      if (ptr) {delete [] ptr;}
	  }

	  // x CC GENE .
	  if(Words.size()>2 && i<Words.size()-3 && r5>0  && r7<0) {
	      ptr=lc(Words[i+2]);
	      // remove r7<0 to be less conservative   
	      if (strcmp((plex->Btags).strmap+(plex->Btags).addr[Tags[i+1]-1],"CC")==0
		  && strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+2]-1],"GENE") 
		  && !Gen.find(ptr)
		  && strcmp(Words[i+3],".")==0) {
		  // cout << "2 " << Words[i] << " changed to CONTEXTGENE" << endl;
		  Tags[i] = (plex->Btags).find("CONTEXTGENE");
		  if(r7<0) {NewContextGene.add_element(Ids[Ids.size()-1],lcw);}
	      }
	      if (ptr) {delete [] ptr;}
	  }

	  // GENE CC x . 
	  if(i>2 && r3>0  && Words.size()> 0 && i<Words.size()-1 && r7<0) {
	      ptr=lc(Words[i-2]);
	      // remove r7<0 to be less conservative   
	      if (strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i-2]-1],"GENE") 
		  && strcmp((plex->Btags).strmap+(plex->Btags).addr[Tags[i-1]-1],"CC")==0
		  && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"V") 
		  && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"RB") 
		  && !Gen.find(ptr)
		  && strcmp(Words[i+1],".")==0) {
		  // cout << "3 " << Words[i] << " changed to CONTEXTGENE" << endl;
		  Tags[i] = (plex->Btags).find("CONTEXTGENE");
		  if(r7<0) {NewContextGene.add_element(Ids[Ids.size()-1],lcw);}
	      }
	      if (ptr) { delete [] ptr;}
	  }

	  // GENE , CC x
	  if(i>3 && r3>0 && Words.size()>1 && i<Words.size()-1  && r7<0) {
	      ptr=lc(Words[i-2]);
	      // remove r7<0 to be less conservative   
	      if (strcmp((plex->Btags).strmap+(plex->Btags).addr[Tags[i-1]-1],"CC")==0
		  && strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i-3]-1],"GENE") 
		  && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"V") 
		  && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"RB") 
		  && !Gen.find(ptr)
		  && strcmp((plex->Btags).strmap+(plex->Btags).addr[Tags[i-2]-1],",")==0) {
		  // cout << "4 " << Words[i] << " changed to CONTEXTGENE" << endl;
		  Tags[i] = (plex->Btags).find("CONTEXTGENE");
		  if(r7<0) {NewContextGene.add_element(Ids[Ids.size()-1],lcw);}
	      }
	      if (ptr) {delete [] ptr;}
	  }

	  // x ( GENE NOT=
	  if(Words.size()>2 && i<Words.size()-3 && r4<0  && r7<0) {
	      ptr=lc(Words[i+2]);
	      // remove r7<0 to be less conservative   
	      if (strpbrk(Words[i], "ABCDEFGHIJKLMNOPQRSTUVWXYZ-*'")
		  && strcmp(Words[i+1], "(")==0
		  && strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+2]-1],"GENE") 
		  && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"V") 
		  && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"RB") 
		  && !Gen.find(ptr)) {
		  // cout << "5 " << Words[i] << " changed to CONTEXTGENE" << endl;
		  Tags[i] = (plex->Btags).find("CONTEXTGENE");
		  if(r7<0) {NewContextGene.add_element(Ids[Ids.size()-1],lcw);}
	      }
	      if (ptr) {delete [] ptr;}
	  }

	  // GENE ( x NOT=
	  if(i>2 && r4<0 && Words.size()>1 && i<Words.size()-1 && r6<0 && r3>0  && r7<0 // remove r7<0 to be less conservative     
	     && strcmp(Words[i-1], "(")==0
	     && strcmp(lcw, "figure") && strcmp(lcw, "fig.") && strcmp(lcw,"table") && strcmp(lcw,"see")
	     && strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i-2]-1],"GENE") 
	     && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"V") 
	     && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"RB")) {
	    // cout << "6 " << Words[i] << " changed to CONTEXTGENE" << endl;
	    Tags[i] = (plex->Btags).find("CONTEXTGENE");
	    if(r7<0) {NewContextGene.add_element(Ids[Ids.size()-1],lcw);}
	  }
	  // GENE x )
	  if(Words.size()>0 && i<Words.size()-1 && i>1 && r4<0 && r7<0 // remove r7<0 to be less conservative
	     && strcmp(Words[i+1], ")")==0
	     && strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i-1]-1],"GENE")
	     && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"V") 
	     && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"RB")) {
	    // cout << "7 " << Words[i] << " changed to CONTEXTGENE" << endl;
	    Tags[i] = (plex->Btags).find("CONTEXTGENE");
	    if(r7<0) {NewContextGene.add_element(Ids[Ids.size()-1],lcw);}
	  }
	  // x GENE )
	  if(Words.size()>1 && i<Words.size()-2 && r7<0) {
	      ptr=lc(Words[i+1]);
	      // remove r7<0 to be less conservative
	      if (strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+1]-1],"GENE")
		  && strcmp(Words[i+2], ")")==0
		  && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"V") 
		  && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"RB") 
		  && !Gen.find(ptr)) {   
		  // cout << "8 " << Words[i] << " changed to CONTEXTGENE" << endl;
		  Tags[i] = (plex->Btags).find("CONTEXTGENE");
		  if(r7<0) {NewContextGene.add_element(Ids[Ids.size()-1],lcw);}
	      }
	      if (ptr) {delete [] ptr;}
	  }

	  // x , GENE
	  if(Words.size()>1 && i<Words.size()-2  && r7<0) {
	      ptr=lc(Words[i+2]);
	      // remove r7<0 to be less conservative   
	      if (strpbrk(Words[i], "ABCDEFGHIJKLMNOPQRSTUVWXYZ-*'")
		  && strcmp((plex->Btags).strmap+(plex->Btags).addr[Tags[i+1]-1], ",")==0
		  && strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+2]-1],"GENE")
		  && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"V") 
		  && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"RB") 
		  && !Gen.find(ptr)) {
		  // cout << "9 " << Words[i] << " changed to CONTEXTGENE" << endl;
		  Tags[i] = (plex->Btags).find("CONTEXTGENE");
		  if(r7<0) {NewContextGene.add_element(Ids[Ids.size()-1],lcw);}
	      }
	      if (ptr) {delete [] ptr;}
	  }
	}

	// pick up previously found genes in this abstract	
	// added gbef and gaft for FULL TEXT to decrease false positives
	if(!Stop.find(lcw) && i>0 && i<Words.size()-1) {
	    ptr=lc(Words[i-1]);
	    pptr=lc(Words[i+1]);
	    if ((Gbef.find(ptr) || Gaft.find(pptr))
		&& (NewContextGene.search(Ids[Ids.size()-1])==1
		&& NewContextGene.exs_pair(Ids[Ids.size()-1],lcw)==1)
		&& !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"GENE")
		&& (NotGene.search(Ids[Ids.size()-1])==1
		    && NotGene.exs_pair(Ids[Ids.size()-1],lcw)==0 
		    || NotGene.search(Ids[Ids.size()-1])==0)
		&& !Bad_After_Num.find(lcw) 
		&& strcmp(Words[i], "a") && strlen(Words[i])>1) {
		//cout << "10 " << Ids[Ids.size()-1] 
		//   << " " << Words[i] << " changed to CONTEXTGENE" << endl;
		Tags[i] = (plex->Btags).find("CONTEXTGENE");
	    }
	    if (ptr) {delete [] ptr;}
	    if (pptr) {delete [] pptr;}
	}
      }
      if (lcw) {delete [] lcw;}
    }
  }

  //Genes on big.sco >=0 , sentence sco >= 2 using linclass
  float sco;
  Count* lptr;
  char *strt;
  sco=1;
  sco = get_score(lc(no_tagstr));
  if(sco) { 
    //lptr = new Count();
    lptr = set_substrings(lc(no_tagstr));
    if(lptr) {
      //cout << lptr->total << " --- matches for: " << endl << lc(no_tagstr) << endl;
      lptr->node_first();
      // each unique gene name
      while(lptr->node_next()) {
	//cout << lptr->show_str() << " " << lptr->count() << endl;
	// make vector of words in this gene name
	if(Ten_PM.search(lptr->show_str())) {
	  cptr = new char[strlen(Ten_PM.show_str())+1];
	  strcpy(cptr,Ten_PM.show_str());
	  strt = strtok(cptr, " ");
	  gene.push_back(strt);
	  while((strt = strtok(NULL, " ")) != NULL) {
	    gene.push_back(strt);
	  }
	
	  // change the tag 
	  if(Words.size() == Tags.size()) {
	    // for each occurrence of this gene in the sentence
	    //for(j=0;j<lptr->count();j++) {
	      for(i=0;i<Words.size();i++) {
		k=0;
		
		if (strcmp(ptr=lc(Words[i]), gene[k])==0) {
		  k++;
		  while(k<gene.size()) {
		    if (strcmp(pptr=lc(Words[i+k]), gene[k])==0) {
		      k++;
		    }
		    else { k = 9999; }
		    delete [] pptr;
		  }
		}		
		delete [] ptr;
		if(k == gene.size()) {
		  if(i<Words.size()-1) {
		    afterg = new char[strlen(Words[i+k])+1];
		    strcpy(afterg, Words[i+k]);
		    for(j=0;afterg[j] != '\0';j++) {
		      if(afterg[j] >= 65 && afterg[j] <= 90) { 
			(afterg[j])+=32;
		      }
		    }
		    CAfter.push_back(afterg);
		  }
		  if(i>0) {
		    beforeg = new char[strlen(Words[i-1])+1];
		    strcpy(beforeg, Words[i-1]);
		    for(j=0;beforeg[j] != '\0';j++) {
		      if(beforeg[j] >= 65 && beforeg[j] <= 90) { 
			(beforeg[j])+=32;
		      }
		    }  
		    CBefore.push_back(beforeg);
		  }
		  
		  k=0;
		  while(k<gene.size()) {
		    tptr = new char[15];
		    strcpy(tptr, (plex->Btags).strmap+(plex->Btags).addr[Tags[i+k]]);
		    if (!strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+k]-1],"GENE")){
		      //if((afterg && Gaft.find(afterg)) || (beforeg && Gbef.find(beforeg))) {
			pptr=lc(no_tagstr); ptr=lc(Words[i+k]);
			if(!(strstr(pptr, "cell") ||
			     strstr(pptr, "target") ||
			     strstr(pptr, "blast")) && !Stop.find(ptr)
			   && !Gen.find(ptr) 
			   && !strstr(tptr,"V")) {
			  Tags[i+k]= (plex->Btags).find("NEWGENE");
			}
			delete [] pptr; delete [] ptr;
		      //}// end if afterg
		    }
		    k++;
		  } // end while k<gene.size
		  if(tptr)    {delete [] tptr;}
		  k=0;
		}
	      }
	      //	      gene.clear();
	  }// end while node next
	  if (cptr) {delete [] cptr;}
	  gene.clear();
	}
      }
      if(lptr) {delete lptr;}
    } 
  }  //end if sco
  else {
     for(i=0;i<Tags.size();i++) {
       if(strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"GENE")) {
	 Tags[i]= (plex->Btags).find("NN");
       }
     }
  }
  //ofs.open(fname, ios::app);
  vector<char*>::iterator p = Words.begin();
  vector<long>::iterator pp = Tags.begin();
  while(p != Words.end()) {
    while (pp != Tags.end()) {
      r      = pcre_exec(Re[12], Extra[12], *p,
			   strlen(*p), 0, 0, offsets, XSTR);      
      /*if(r > 0) { 
	cptr = new char[strlen(*p)+1];
	strcpy(cptr,*p);
	cptr += offsets[2];
	cptr[offsets[3]] = '\0';
	ids.push_back(cptr);
      }*/
      if(strstr((plex->Btags).strmap+(plex->Btags).addr[*pp-1],"GENE")
	 && Ids.size()>0) {
	//cout << Ids[Ids.size()-1] << " " << *p << endl;
	HasGene.add_key_count(Ids[Ids.size()-1]);
      }
      result_str += string(*p) + "/" + string((plex->Btags).strmap+(plex->Btags).addr[*pp-1]) + " ";
      if (DEBUG)  
	  cout << *p << "/" << (plex->Btags).strmap+(plex->Btags).addr[*pp-1] << " " ;
      p++;pp++;
      
    }
  }
//   cout << endl;
  //ofs.close();
  ids.clear(); 
  free_vec_str(CAfter); free_vec_str(CBefore);

  return result_str;

}


void PostBrill_FN::filter_string(char* str) {
  char *strpair, *loc, *aptr;
  char strc[XSTR];
  long i;
  int ans;

  strcpy(instr,str);

  // make the words and tags vector for this string
  if(strpbrk(str,"abcdefghijklmnopqrstuvwxzyABCDEFGHIJKLMNOPQRSTUVWXYZ") &&
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
    
    
   /* for(i=0;i<Words.size();i++) {
	cout << Words[i] << " " 
	     << (plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1] << endl;
	     } */

    // apply filter to string and print result 
    apply_filter();
  }
}

void PostBrill_FN::filter_file(ifstream& ifs) {
  char s[XSTR];
  long i;

  while(ifs.getline(s, XSTR,'\n')) {
    filter_string(s);    
  }
}

void PostBrill_FN::apply_filter() {
  long i,j;
  const char* errptr;
  const char* study_error;
  int erroffset;
  int offsets[XSTR];
  int cs,r;   
  pcre* pre = NULL;
  pcre_extra* pextra;
  char* cptr;
 
  apply_rules();
  clear_all();
}

void PostBrill_FN::apply_rules() {
  get_newgenes();   // single word known genes with good bef/aft
  get_knowngenes(); // multiword known genes with good bef/aft
  get_multigenes();
  get_contextgenes();  
}

void PostBrill_FN::get_knowngenes() {
  ifstream fin;
  char cnam[max_str];
  int m=0;
  Count M;
  int i,j,k;
  char *cptr,*strt,*lcw,*ptr,*pptr;
  vector<char*> gene; 
  float sco;

  

  // Known multiple word genes
  PM.all_match(strt=lc(no_tagstr),M,1);
  if(strt) {delete [] strt;}
  if(M.total > 1) {
    M.node_first();
    // each unique gene name
    while(M.node_next()) {
      // make vector of words in this gene name
      cptr = new char[strlen(M.show_str())+1];
      strcpy(cptr,M.show_str());
      strt = strtok(cptr, " ");
      gene.push_back(strt);
      while((strt = strtok(NULL, " ")) != NULL) {
	gene.push_back(strt);
      }
      
      // change the tag 
      if(Words.size() == Tags.size()) {
	// for each occurrence of this gene in the sentence
	for(j=0;j<M.count();j++) {
	  for(i=0;i<Words.size();i++) {
	    k=0;
	    ptr=lc(Words[i]);
	    if (strcmp(ptr, gene[k])==0) {
	      k++;
	      while(k<gene.size()) {
		pptr=lc(Words[i+k]);
		if (strcmp(pptr, gene[k])==0) {
		  k++;
		}
		else { k = 9999; }
		delete [] pptr;
	      }
	    }
	    delete [] ptr;

	    if(k == gene.size()) {
	      k=0;
	      while(k<gene.size()) {
		if (!strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+k]-1],"GENE")) {
		  Tags[i+k]= (plex->Btags).find("NEWGENE");
		}
		 k++;
	      }
	    }
	    k=0;
	  }
	}
      }
      if(cptr) {delete [] cptr;}
      gene.clear();
    }// end while node next
  }

}

char* PostBrill_FN::lc(char* lcw) {
  int j;
  char* c;
  if(lcw != NULL) {
    c = new char[strlen(lcw)+1];
    strcpy(c, lcw);
    for(j=0;c[j]!='\0';j++) {
      if(c[j] >= 65 && c[j] <= 90) { 
	(c[j])+=32;
      }
    }
    return c;
  }
  else {
    return NULL;
  }
 
}

void PostBrill_FN::get_contextgenes() {
  char *lcw, *tptr, *cptr,*aptr;
  int i,j,k,r,r1,r2,r3,r4,r5,r6,r7;
  const char* errptr;
  const char* study_error;
  int erroffset;
  int offsets[XSTR]; 
  ofstream ofs;
  vector<char*> ids;
  vector<char*> gene;
  char* beforeg = NULL;
  char* afterg = NULL;
  vector<char*> CBefore;
  vector<char*> CAfter;
  char* ptr;
  char* pptr;

  /** weakest rules are 1,7,8 then 5,9 
      Brill tagger V-->N errors cause cascading problems **/

  //try some Brill contextual rules 
  if(Words.size() == Tags.size()) {
    for(i=0;i<Words.size();i++) { 
      lcw = lc(Words[i]);
      
      // ^[A-Z]\.$
      r      = pcre_exec(Re[63], Extra[63], Words[i],
			 strlen(Words[i]), 0, 0, offsets, XSTR);
      // \d|[A-Z][A-Z]|\S\S\S\Sin$|\S\S\ase$|[A-Z]$|[^aeiou]|^[A-Z][^aeiou]|^\S\S\S$
      r1      = pcre_exec(Re[64], Extra[64], Words[i],
			  strlen(Words[i]), 0, 0, offsets, XSTR);
      // ^[a-z]ase$|[a-z]\.[a-z]...ate$|chlors*$|ed$|ing$
      r2      = pcre_exec(Re[65], Extra[65], Words[i],
			  strlen(Words[i]), 0, 0, offsets, XSTR);
      // [A-Z]|-|\d
      r3      = pcre_exec(Re[66], Extra[66], Words[i],
			  strlen(Words[i]), 0, 0, offsets, XSTR);
      // ^et\.*$|^and$|\,|=|^19\d\d$|^20\d\d$
      r4      = pcre_exec(Re[63], Extra[63], lcw,
			  strlen(lcw), 0, 0, offsets, XSTR);
      // NN|CD|JJ|IN
      r5      = pcre_exec(Re[69], Extra[69], 
      (plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],
      strlen((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1]), 0, 0, offsets, XSTR);

      // ^et\.*$|^and$|\,|=|^19\d\d$|^20\d\d$
      if(Words.size()>0 && i<Words.size()-1) {
	ptr=lc(Words[i+1]);
	r6      = pcre_exec(Re[63], Extra[63], ptr,
			    strlen(ptr), 0, 0, offsets, XSTR);
	if(ptr){delete [] ptr;}
      }
      // bad suffixes
      r7      = pcre_exec(Re[70], Extra[70], lcw,
			  strlen(lcw), 0, 0, offsets, XSTR);

      if(Ids.size()>0) {
	if((NotGene.search(Ids[Ids.size()-1])==1
	    && NotGene.exs_pair(Ids[Ids.size()-1],lcw)==0 
	    || NotGene.search(Ids[Ids.size()-1])==0) && 
	   strpbrk(lcw,"abcdefghijklmnopqrstuvwxyz") 
	   && r<0 && r1>0 && r2<0 && strlen(lcw)>1 
	   && !Bad_After_Num.find(lcw) 
	   && !Stop.find(lcw) && !Aa.find(lcw) 
	   && !Gen.find(lcw) && !Freq.find(lcw) 
	   && !Ce.find(lcw) && !Ren.find(lcw) 
	   && strpbrk(Words[i],"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") 
	   && !Org.find(lcw) 
	   && strcmp(Words[i], "a") && !strstr(lcw,"prime") 
	   && !NotGene.search(Ids[Ids.size()-1])==1
	   && NotGene.exs_pair(Ids[Ids.size()-1],lcw)==0
	   && strpbrk(lcw,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
	   && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"GENE") ) {
	  //GENE , x NOTVERB
	  if(Words.size()>0 && i>2 && i < Words.size()-1 && r7<0) {
	    ptr=lc(Words[i-2]);
	    // remove r7<0 to be less conservative   
	    if(strcmp(Words[i-1],",")==0 
	       && strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i-2]-1],"GENE") 
	       && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"V") 
	       && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+1]-1],"IN") 
	       && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+1]-1],"MD")
	       && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"RB") 
	       && !Gen.find(ptr) 
	       && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+1]-1],"V")
	       && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+1]-1],"SYM")) {
	      Tags[i] = (plex->Btags).find("CONTEXTGENE");
	      if(r7<0) {NewContextGene.add_element(Ids[Ids.size()-1],lcw);}
	    }
	    if(ptr) {delete [] ptr;}
	  }
	  // x CC GENE .
	  if(Words.size()>2 && i<Words.size()-3 && r5>0  && r7<0) {
	    ptr=lc(Words[i+2]);
	    // remove r7<0 to be less conservative   
	    if(strcmp((plex->Btags).strmap+(plex->Btags).addr[Tags[i+1]-1],"CC")==0
	       && strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+2]-1],"GENE") 
	       && !Gen.find(ptr)
	       && strcmp(Words[i+3],".")==0) {
	      Tags[i] = (plex->Btags).find("CONTEXTGENE");
	      if(r7<0) {NewContextGene.add_element(Ids[Ids.size()-1],lcw);}
	    }
	    if(ptr) {delete [] ptr;}
	  }
	  // GENE CC x . 
	  if(i>2 && r3>0  && Words.size()> 0 && i<Words.size()-1 && r7<0) {
	    ptr=lc(Words[i-2]);
	    // remove r7<0 to be less conservative   
	    if(strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i-2]-1],"GENE") 
	       && strcmp((plex->Btags).strmap+(plex->Btags).addr[Tags[i-1]-1],"CC")==0
	       && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"V") 
	       && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"RB") 
	       && !Gen.find(ptr)
	       && strcmp(Words[i+1],".")==0) {
	      Tags[i] = (plex->Btags).find("CONTEXTGENE");
	      if(r7<0) {NewContextGene.add_element(Ids[Ids.size()-1],lcw);}
	    }
	    if(ptr) {delete [] ptr;}
	  }
	  // GENE , CC x
	  if(i>3 && r3>0 && Words.size()>1 && i<Words.size()-1  && r7<0) {
	    ptr=lc(Words[i-2]);
	    // remove r7<0 to be less conservative   
	    if(strcmp((plex->Btags).strmap+(plex->Btags).addr[Tags[i-1]-1],"CC")==0
	       && strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i-3]-1],"GENE") 
	       && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"V") 
	       && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"RB") 
	       && !Gen.find(ptr)
	       && strcmp((plex->Btags).strmap+(plex->Btags).addr[Tags[i-2]-1],",")==0) {
	      Tags[i] = (plex->Btags).find("CONTEXTGENE");
	      if(r7<0) {NewContextGene.add_element(Ids[Ids.size()-1],lcw);}
	    }
	    if(ptr) {delete [] ptr;}
	  }
	  // x ( GENE NOT=
	  if(Words.size()>2 && i<Words.size()-3 && r4<0  && r7<0) {
	    ptr=lc(Words[i+2]);
	    // remove r7<0 to be less conservative   
	    if(strpbrk(Words[i], "ABCDEFGHIJKLMNOPQRSTUVWXYZ-*'")
	       && strcmp(Words[i+1], "(")==0
	       && strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+2]-1],"GENE") 
	       && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"V") 
	       && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"RB") 
	       && !Gen.find(ptr)) {
	      Tags[i] = (plex->Btags).find("CONTEXTGENE");
	      if(r7<0) {NewContextGene.add_element(Ids[Ids.size()-1],lcw);}
	    }
	    if(ptr) {delete [] ptr;}
	  }
	  // GENE ( x NOT=
	  if(i>2 && r4<0 && Words.size()>1 && i<Words.size()-1 && r6<0 && r3>0  && r7<0 
	     // remove r7<0 to be less conservative     
	     && strcmp(Words[i-1], "(")==0
	     && strcmp(lcw, "figure") && strcmp(lcw, "fig.") && strcmp(lcw,"table") 
	     && strcmp(lcw,"see")
	     && strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i-2]-1],"GENE") 
	     && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"V") 
	     && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"RB")) {
	    Tags[i] = (plex->Btags).find("CONTEXTGENE");
	    if(r7<0) {NewContextGene.add_element(Ids[Ids.size()-1],lcw);}
	  }
	  // GENE x )
	  if(Words.size()>0 && i<Words.size()-1 && i>1 && r4<0 && r7<0 
	     // remove r7<0 to be less conservative
	     && strcmp(Words[i+1], ")")==0
	     && strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i-1]-1],"GENE")
	     && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"V") 
	     && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"RB")) {
	    Tags[i] = (plex->Btags).find("CONTEXTGENE");
	    if(r7<0) {NewContextGene.add_element(Ids[Ids.size()-1],lcw);}
	  }
	  // x GENE )
	  if(Words.size()>1 && i<Words.size()-2 && r7<0) {
	    ptr=lc(Words[i+1]);
	    // remove r7<0 to be less conservative
	    if(strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+1]-1],"GENE")
	       && strcmp(Words[i+2], ")")==0
	       && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"V") 
	       && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"RB") 
	       && !Gen.find(ptr)) {   
	      Tags[i] = (plex->Btags).find("CONTEXTGENE");
	      if(r7<0) {NewContextGene.add_element(Ids[Ids.size()-1],lcw);}
	    }
	    if(ptr) {delete [] ptr;}
	  }
	  // x , GENE
	  if(Words.size()>1 && i<Words.size()-2  && r7<0) {
	    ptr=lc(Words[i+2]);
	    // remove r7<0 to be less conservative   
	    if(strpbrk(Words[i], "ABCDEFGHIJKLMNOPQRSTUVWXYZ-*'")
	       && strcmp((plex->Btags).strmap+(plex->Btags).addr[Tags[i+1]-1], ",")==0
	       && strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+2]-1],"GENE")
	       && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"V") 
	       && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"RB") 
	       && !Gen.find(ptr)) {
	      Tags[i] = (plex->Btags).find("CONTEXTGENE");
	      if(r7<0) {NewContextGene.add_element(Ids[Ids.size()-1],lcw);}
	    }
	    if(ptr) {delete [] ptr;}
	  }
	}
	// pick up previously found genes in this abstract	
	// added gbef and gaft for FULL TEXT to decrease false positives
	if(!Stop.find(lcw) && i>0 && i<Words.size()-1) {
	  ptr=lc(Words[i-1]);
	  pptr=lc(Words[i+1]);
	  if((Gbef.find(ptr) || Gaft.find(pptr))
	     && (NewContextGene.search(Ids[Ids.size()-1])==1
		 && NewContextGene.exs_pair(Ids[Ids.size()-1],lcw)==1)
	     && !strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"GENE")
	     && (NotGene.search(Ids[Ids.size()-1])==1
		 && NotGene.exs_pair(Ids[Ids.size()-1],lcw)==0 
		 || NotGene.search(Ids[Ids.size()-1])==0)
	     && !Bad_After_Num.find(lcw) 
	     && strcmp(Words[i], "a") && strlen(Words[i])>1) {
	    Tags[i] = (plex->Btags).find("CONTEXTGENE");
	  }
	  if(ptr) {delete [] ptr;}
	  if(pptr) {delete [] pptr;}
	}
      }
      if(lcw) {delete [] lcw;}
    }
  }
	  
    
  //Genes on big.sco >=0 , sentence sco >= 2 using linclass
  float sco;
  Count* lptr;
  char *strt;
 
  sco=1;
  sco = get_score(lc(no_tagstr));
  if(sco) { 
    lptr = set_substrings(lc(no_tagstr));
    if(lptr) {
      lptr->node_first();
      // each unique gene name
      while(lptr->node_next()) {
	// make vector of words in this gene name
	if(Ten_PM.search(lptr->show_str())) {
	  cptr = new char[strlen(Ten_PM.show_str())+1];
	  strcpy(cptr,Ten_PM.show_str());
	  strt = strtok(cptr, " ");
	  gene.push_back(strt);
	  while((strt = strtok(NULL, " ")) != NULL) {
	    gene.push_back(strt);
	  }
	
	  // change the tag 
	  if(Words.size() == Tags.size()) {
	    // for each occurrence of this gene in the sentence
	      for(i=0;i<Words.size();i++) {
		k=0;
		
		if (strcmp(ptr=lc(Words[i]), gene[k])==0) {
		  k++;
		  while(k<gene.size()) {
		    if (strcmp(pptr=lc(Words[i+k]), gene[k])==0) {
		      k++;
		    }
		    else { k = 9999; }
		    delete [] pptr;
		  }
		}
		delete [] ptr;
		if(k == gene.size()) {
		  if(i<Words.size()-1) {
		    afterg = new char[strlen(Words[i+k])+1];
		    strcpy(afterg, Words[i+k]);
		    for(j=0;afterg[j] != '\0';j++) {
		      if(afterg[j] >= 65 && afterg[j] <= 90) { 
			(afterg[j])+=32;
		      }
		    }
		    CAfter.push_back(afterg);
		  }
		  if(i>0) {
		    beforeg = new char[strlen(Words[i-1])+1];
		    strcpy(beforeg, Words[i-1]);
		    for(j=0;beforeg[j] != '\0';j++) {
		      if(beforeg[j] >= 65 && beforeg[j] <= 90) { 
			(beforeg[j])+=32;
		      }
		    }  
		    CBefore.push_back(beforeg);
		  }
		  
		  k=0;
		  tptr = new char[15];
		  while(k<gene.size()) {
		    strcpy(tptr, (plex->Btags).strmap+(plex->Btags).addr[Tags[i+k]]);
		    if (!strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i+k]-1],"GENE")){
		      pptr=lc(no_tagstr); ptr =lc(Words[i+k]);
			if(!(strstr(pptr, "cell") ||
			     strstr(pptr, "target") ||
			     strstr(pptr, "blast")) && !Stop.find(ptr)
			   && !Gen.find(ptr) 
			   && !strstr(tptr,"V")) {
			  Tags[i+k]= (plex->Btags).find("NEWGENE");
			}
			delete [] pptr; delete [] ptr;
		    }
		    k++;
		  }// end while k<gene.size
		  if(tptr)    {delete [] tptr;}
		  k=0;
		}
	      }
	     
	  }// end while node next
	  if(cptr) {delete [] cptr;}
	  gene.clear();
	}
      }
      if(lptr) {delete lptr;}
    } 
  }  //end if sco
  else {
     for(i=0;i<Tags.size();i++) {
       if(strstr((plex->Btags).strmap+(plex->Btags).addr[Tags[i]-1],"GENE")) {
	 Tags[i]= (plex->Btags).find("NN");
       }
     }
  }
  vector<char*>::iterator p = Words.begin();
  vector<long>::iterator pp = Tags.begin();
  while(p != Words.end()) {
    while (pp != Tags.end()) {
      r      = pcre_exec(Re[12], Extra[12], *p,
			   strlen(*p), 0, 0, offsets, XSTR);      
    
      if(strstr((plex->Btags).strmap+(plex->Btags).addr[*pp-1],"GENE")
	 && Ids.size()>0) {
	HasGene.add_key_count(Ids[Ids.size()-1]);
      }
      cout << *p << "/" << (plex->Btags).strmap+(plex->Btags).addr[*pp-1]
	  << " " ;
      p++;pp++;
      
    }
  }
  cout << endl;
  ids.clear(); 
  free_vec_str(CAfter); free_vec_str(CBefore);

}

void PostBrill_FN::get_multigenes() {
  long i,j,k,found,jj=0,jjj=0;
  const char* errptr;
  const char* study_error;
  int erroffset;
  int offsets[XSTR];
  int r, r1, r2, r3, r4, remove, rem, start, y, yy, removedlast;   
  pcre* pre = NULL;
  pcre_extra* pextra;
  char* cptr= NULL;
  char* bptr=NULL;
  char* xptr;
  char* lptr= NULL;
  char* t= NULL;
  char* lcg= NULL;
  char l1[XSTR]= "";
  char l2[XSTR]= "";
  char l3[XSTR]= "";
  char l4[XSTR]= "";
  char tt[XSTR]= "";
  char* l2p= NULL;
  char* l4p= NULL;
  char* l3p= NULL;
  char paa[XSTR]= "";
  char npaa[XSTR]= "";
  char* paap = NULL;
  char* npaap = NULL;
  char afternum[XSTR]= "";
  char capafternum[XSTR]= "";
  char* tempptr= NULL;
  char* aptr = NULL;
  char rems[XSTR]= "";
  char* ag= NULL;
  char* bg= NULL;
  char* tptr= NULL;
  char   g[XSTR] = "";
  char ggg[XSTR] = "";
  vector<char*> gg;
  vector<char*> lgg;
  char* lggptr=NULL;
  char lcw[XSTR]= "";
  char ca [XSTR]= "";
  char cw[XSTR]= "";
  char ccw[XSTR]= "";
  char* first = NULL;
  char* lastw= NULL;
  char* nlastw= NULL;
  char* nnlastw= NULL;
  char* capfirst= NULL;
  char* capnlastw = NULL;
  char* caplast = NULL;
  char clastw[XSTR]= "";
  char cnlastw[XSTR]= "";
  char* gptr= NULL;
  char* ntptr= NULL;
  vector<long>::iterator ti = Tags.begin();
  vector<char*>::iterator wi = Words.begin();
  char cl[XSTR]= "";
  char z[XSTR] = "0";

  ntptr=lc(no_tagstr); 
  if(Words.size() == Tags.size()) {
    vector<char*>::iterator p = Words.begin();
    vector<long>::iterator pp = Tags.begin();
    j=-1;
    while(p != Words.end()) {
      while (pp != Tags.end()) {
	j++;
	found=k=0;
	strcpy(lcw,*p);
	strcpy(cw,*p);
	for(i=0;lcw[i] != '\0';i++) {
	  if(lcw[i] >= 65 && lcw[i] <= 90) { 
	    (lcw[i])+=32;
	  }
	  if(lcw[i] != 45) {
	    cw[k] = lcw[i]; k++;
	  }
	}
	cw[k] = '\0';
       	
	r      = pcre_exec(Re[12], Extra[12], *p,
			   strlen(*p), 0, 0, offsets, XSTR);      
	if(r > 0) {
	  cptr = new char[offsets[2]+offsets[3]+strlen(*p) + 1];
	  strcpy(cptr,*p);
	  // added to prevent memory leak -- Halil
	  xptr = cptr;
	  xptr += offsets[2];
	  xptr[offsets[3]] = '\0';
// 	  strcpy(cl,cptr);
// 	  while(strlen(cl) < 8) {
// 	    strcat(z,cptr);
// 	    cptr = z;
// 	    strcpy(cl,cptr);
// 	    cptr = cl;
// 	    strcpy(z,"0");
// 	    }
// 	  strcpy(cptr,cl);
	  bptr = new char[strlen(xptr)+1];
	  strcpy(bptr,xptr);
	  Ids.push_back(bptr);
	  if (cptr!=NULL) delete [] cptr;
	}
	r=0;

	r      = pcre_exec(Re[13], Extra[13], *p,
			   strlen(*p), 0, 0, offsets, XSTR);
	if(r < 0) {
	  r      = pcre_exec(Re[14], Extra[14], *p,
			      strlen(*p), 0, 0, offsets, XSTR);
	  if(r < 0) {
	    r      = pcre_exec(Re[15], Extra[15], *p,
			   strlen(*p), 0, 0, offsets, XSTR);
	  }
	}
	// build gene name   as long as the parts are OK
        if(g){strcpy(ggg,g); strcat(ggg,lcw); /*cout << lc(ggg) << endl;*/}
	gptr=lc(ggg);
	if((g && Genes.find(gptr)) || Genes.find(lcw) || Genes.find(cw) 
	   || Gin.find(lcw) || r>0) {  
	  // continuing a gene name
	  if(gg.size()>0) {
	    tempptr=lc(gg[0]);
	    r      = pcre_exec(Re[16], Extra[16], tempptr,
			       strlen(gg[0]), 0, 0, offsets, XSTR); 
	    if(tempptr) {delete [] tempptr;}
	    if(r < 0) {
	      if(gg.size()>0) {
		tempptr = lc(gg[gg.size()-1]);
		r      = pcre_exec(Re[17], Extra[17], tempptr,
				   strlen(gg[gg.size()-1]), 0, 0, offsets, XSTR); 
		if(tempptr) {delete [] tempptr;}
		if(r < 0) {
		  if(strcmp(lcw,"line") && strcmp(lcw,"serotype")) {		   
		    if(Ids.size()>0 && NotGene.search(Ids[Ids.size()-1])==1
		       && NotGene.exs_pair(Ids[Ids.size()-1],lcw)==0) {
		      if(!Stop.find(lcw) && !Freq.find(lcw)) {
			if(g) {strcat(g,*p); strcat(g, " ");}
			else  {strcpy(g,*p); strcat(g, " ");}
			if(DEBUG) { cout << g << endl; }
			gg.push_back(*p);
			lptr = new char[strlen(lcw)+1];
			strcpy(lptr,lcw);
			lgg.push_back(lptr);			
			goto cont;
		      }
		    }
		    else if(Ids.size()>0 && 
			    NotGene.search(Ids[Ids.size()-1])==0) {
		      if(!Stop.find(lcw) && !Freq.find(lcw)) {
			if(g) {strcat(g,*p); strcat(g, " ");}
			else  {strcpy(g,*p); strcat(g, " ");}
			if(DEBUG) { cout << g << endl; }
			gg.push_back(*p); 
			lptr = new char[strlen(lcw)+1];
			strcpy(lptr,lcw);
			lgg.push_back(lptr);			
			goto cont;
		      }
		    }
		  }
		}
	      }
	    }
	  }
	  
	  // the first word in a multiword gene name
	  else  {
	    if(strcmp(lcw,"line") && strcmp(lcw,"serotype")) {
	     
	      if(Ids.size()>0 && NotGene.search(Ids[Ids.size()-1])==1
		 && NotGene.exs_pair(Ids[Ids.size()-1],lcw)==0) {
		if(!Stop.find(lcw) && !Freq.find(lcw)) {
		  if(g) {strcat(g,*p); strcat(g, " ");}
		  else  {strcpy(g,*p); strcat(g, " ");}
		  if(DEBUG) { cout << "first: " << g << endl; }
		  gg.push_back(*p); 
		  lptr = new char[strlen(lcw)+1];
		  strcpy(lptr,lcw);
		  lgg.push_back(lptr);
		  
		  goto cont;
		}
	      }
	      else if(Ids.size()>0 && NotGene.search(Ids[Ids.size()-1])==0){
		if(!Stop.find(lcw) && !Freq.find(lcw)) {
		  if(g) {strcat(g,*p); strcat(g, " ");}
		  else  {strcpy(g,*p); strcat(g, " ");} 
		  if(DEBUG) { cout << g << endl; }
		  gg.push_back(*p); 
		  lptr = new char[strlen(lcw)+1];
		  strcpy(lptr,lcw);
		  lgg.push_back(lptr);
		  
		  goto cont;
		}
	      }
	    }
	  }
	}
	
	// try long regexp
	else {
	  r      = pcre_exec(Re[18], Extra[18], lcw,
			     strlen(lcw), 0, 0, offsets, XSTR);      
	  if(r>0) {
	    if(gg.size()>0) {
	      tempptr=lc(gg[0]);
	      r      = pcre_exec(Re[16], Extra[16], tempptr,
				 strlen(gg[0]), 0, 0, offsets, XSTR); 
	      if(tempptr) {delete [] tempptr;}
	      if(r < 0) {
		
		if(gg.size()>0) {
		  tempptr=lc(gg[gg.size()-1]);
		  r      = pcre_exec(Re[17], Extra[17], tempptr,
				     strlen(gg[gg.size()-1]), 0, 0, offsets, XSTR); 
		  if(tempptr) {delete [] tempptr;}
		  if(r < 0) {
		    if(strcmp(lcw,"line") && strcmp(lcw,"serotype")) {
		     
		      if(Ids.size()>0 && NotGene.search(Ids[Ids.size()-1])==1
			 && NotGene.exs_pair(Ids[Ids.size()-1],lcw)==0) {
			if(!Stop.find(lcw) && !Freq.find(lcw)) {
			  if(g) {strcat(g,*p); strcat(g, " ");}
			  else  {strcpy(g,*p); strcat(g, " ");} 
			  if(DEBUG) { cout << g << endl; }
			  gg.push_back(*p); 
			  lptr = new char[strlen(lcw)+1];
			  strcpy(lptr,lcw);
			  lgg.push_back(lptr);
			  
			  goto cont;
			}
		      }
		      else if(Ids.size()>0 && NotGene.search(Ids[Ids.size()-1])==0){
			if(!Stop.find(lcw) && !Freq.find(lcw)) {
			  if(g) {strcat(g,*p); strcat(g, " ");}
			  else  {strcpy(g,*p); strcat(g, " ");}  
			  if(DEBUG) { cout << g << endl; }
			  gg.push_back(*p); 
			  lptr = new char[strlen(lcw)+1];
			  strcpy(lptr,lcw);
			  lgg.push_back(lptr);
			  
			  goto cont;
			}
		      }
		    }
		  }
		}
	      }
	    }
	    else  {
	      if(strcmp(lcw,"line") && strcmp(lcw,"serotype")) {
		if(Ids.size()>0 && NotGene.search(Ids[Ids.size()-1])==1
		   && NotGene.exs_pair(Ids[Ids.size()-1],lcw)==0) {
		  if(!Stop.find(lcw) && !Freq.find(lcw)) {
		    if(g) {strcat(g,*p); strcat(g, " ");}
		    else  {strcpy(g,*p); strcat(g, " ");}  
		    if(DEBUG) { cout << g << endl; }
		    gg.push_back(*p); 
		    lptr = new char[strlen(lcw)+1];
	    	    strcpy(lptr,lcw);
		    lgg.push_back(lptr);
		    
		    goto cont;
		  }
		}
		else if(Ids.size()>0 && NotGene.search(Ids[Ids.size()-1])==0){
		  if(!Stop.find(lcw) && !Freq.find(lcw)) {
		    if(g) {strcat(g,*p); strcat(g, " ");}
		    else  {strcpy(g,*p); strcat(g, " ");} 
		    if(DEBUG) { cout << g << endl; } 
		    gg.push_back(*p); 
 		    lptr = new char[strlen(lcw)+1];
		    strcpy(lptr,lcw);
		    lgg.push_back(lptr);
		    
		    goto cont;
		  }
		}
	      }
	    }
	  }
	}
      filter:
	// current term was not good
	// now filter this multiword gene name before continuing
	
	if((int)lgg.size() == (int)gg.size() && (int)gg.size() > 1) {
	  r = pcre_exec(Re[71], Extra[71], lcw,
			strlen(lcw), 0, 0, offsets, XSTR);
	  if(r > 0) {
	    g[strlen(g)-1] = '\0';
	  }
	  
	  removedlast=0;
	  first = lgg[0]; 
	  lastw  = lgg[(int)gg.size()-1];
	  nlastw = lgg[(int)gg.size()-2];
	  capfirst = gg[0];
	  caplast  = gg[(int)gg.size()-1];
          capnlastw = gg[(int)gg.size()-2];          		
	 
	  //throw out name if cell words present
	  r = pcre_exec(Re[19], Extra[19], lcw,
			strlen(lcw), 0, 0, offsets, XSTR);
	  if(r > 0 || 
	     (strstr(ntptr, "diabet") && strstr(g, "type") &&
	      first && lastw && nlastw)) {
	    NotGene.add_element(Ids[Ids.size()-1],first);
	    NotGene.add_element(Ids[Ids.size()-1],lastw);
	    NotGene.add_element(Ids[Ids.size()-1],nlastw);
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL;  goto cont;
          }
	  
	  // otherwise see if the current g is a good name 
	  // no unbalanced parens
	  r      = pcre_exec(Re[62], Extra[62], g, 
			     strlen(g), 0, 0, offsets, XSTR);
	  if(r > 0) {
	    r      = pcre_exec(Re[0], Extra[0], g, 
			       strlen(g), 0, 0, offsets, XSTR);
	    if(r > 0) {
	      r      = pcre_exec(Re[1], Extra[1], g, 
				 strlen(g), 0, 0, offsets, XSTR);
	      if(r == -1 && (int)gg.size()>1) {
		
		removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
		g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
		first=lastw=nlastw=capfirst=caplast=
		  capnlastw=paap=npaap=nnlastw=aptr=NULL;  goto cont;
	      }
	    }
	    r      = pcre_exec(Re[2], Extra[2], g, 
			       strlen(g), 0, 0, offsets, XSTR);
	    if(r > 0) {
	      r      = pcre_exec(Re[3], Extra[3], g, 
				 strlen(g), 0, 0, offsets, XSTR);
	      if(r == -1 && (int)gg.size()>1) {
		
		removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
		g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
		first=lastw=nlastw=capfirst=caplast=
		  capnlastw=paap=npaap=nnlastw=aptr=NULL;  goto cont;
	      }
	    }
	    r      = pcre_exec(Re[1], Extra[1], g, 
			       strlen(g), 0, 0, offsets, XSTR);
	    if(r > 0) {
	      r      = pcre_exec(Re[4], Extra[4], g, 
				 strlen(g), 0, 0, offsets, XSTR);
	      if(r == -1 && (int)gg.size()>1) {
		
		removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
		g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
		first=lastw=nlastw=capfirst=caplast=
		  capnlastw=paap=npaap=nnlastw=aptr=NULL;  goto cont;
	      }
	    }
	    r      = pcre_exec(Re[3], Extra[3], g, 
			       strlen(g), 0, 0, offsets, XSTR);
	    if(r > 0) {
	      r      = pcre_exec(Re[5], Extra[5], g, 
				 strlen(g), 0, 0, offsets, XSTR);
	      if(r == -1 && (int)gg.size()>1) {
		
		removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
		g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
		first=lastw=nlastw=capfirst=caplast=
		  capnlastw=paap=npaap=nnlastw=aptr=NULL;  goto cont;
	      }
	    }
	    r      = pcre_exec(Re[6], Extra[6], g, 
			       strlen(g), 0, 0, offsets, XSTR);
	    if(r > 0) {
	      r      = pcre_exec(Re[8], Extra[8], g, 
				 strlen(g), 0, 0, offsets, XSTR);
	      if(r == -1 && (int)gg.size()>1) {
		
		removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
		g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
		first=lastw=nlastw=capfirst=caplast=
		  capnlastw=paap=npaap=nnlastw=aptr=NULL;  goto cont;
	      }
	    }
	    r      = pcre_exec(Re[7], Extra[7], g, 
			       strlen(g), 0, 0, offsets, XSTR);
	    if(r > 0) {
	      r      = pcre_exec(Re[9], Extra[9], g, 
				 strlen(g), 0, 0, offsets, XSTR);
	      if(r == -1 && (int)gg.size()>1) {
		
		removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
		g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
		first=lastw=nlastw=capfirst=caplast=
		  capnlastw=paap=npaap=nnlastw=aptr=NULL;  goto cont;
	      }
	    }
	  }

	  // remove verb at beginning
          if((int)gg.size()>1) {	 
	    pp-=1;
	    k = (int)gg.size()-1;
	    pp-=k;
	    if(strstr((plex->Btags).strmap+(plex->Btags).addr[*pp-1], "V")) {
	      g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	      first=lastw=nlastw=capfirst=caplast=
				   capnlastw=paap=npaap=nnlastw=aptr=NULL;
	      vector<char*>::iterator temp = gg.begin(); 
	      gg.erase(temp);
	      temp = gg.begin(); 
	      while(temp != gg.end()) {
		if(g) {strcat(g,*temp); strcat(g, " ");}
		else  {strcpy(g,*temp); strcat(g, " ");}
		temp++;
	      } 
	      temp = lgg.begin();
	      lggptr = *temp;
	      lgg.erase(temp); if (lggptr) delete [] lggptr;	      
	      if((int)gg.size()>1) {
		first = lgg[0];
		lastw  = lgg[(int)gg.size()-1];
		nlastw = lgg[(int)gg.size()-2];
		capfirst = gg[0];
		caplast  = gg[(int)gg.size()-1];
		capnlastw = gg[(int)gg.size()-2];
		jj++;
	      }
	    }
	    pp+=k;
	    pp+=1;
	  }
	  if((int)gg.size()>1) {
	    pp-=1;
	    k = (int)gg.size()-1;
	    pp-=k;
	    if(strstr((plex->Btags).strmap+(plex->Btags).addr[*pp-1], "V")) {
	      g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	      first=lastw=nlastw=capfirst=caplast=
				   capnlastw=paap=npaap=nnlastw=aptr=NULL;
	      vector<char*>::iterator temp = gg.begin(); 
	      gg.erase(temp);
	      temp = gg.begin(); 
	      while(temp != gg.end()) {
		if(g) {strcat(g,*temp); strcat(g, " ");}
		else  {strcpy(g,*temp); strcat(g, " ");}
		temp++;
	      } 
	      temp = lgg.begin();
	      lggptr = *temp;
	      lgg.erase(temp);
	      if (lggptr) delete [] lggptr;
	      if((int)gg.size()>1) {
		first = lgg[0];
		lastw  = lgg[(int)gg.size()-1];
		nlastw = lgg[(int)gg.size()-2];
		capfirst = gg[0];
		caplast  = gg[(int)gg.size()-1];
		capnlastw = gg[(int)gg.size()-2];
		jj++;
	      }
	    }
	    pp+=k;
	    pp+=1;
	  }
	  
	  //  remove verb number
	  if((int)gg.size()>1) {
	    pp-=2;
	    r      = 
	      pcre_exec(Re[20], Extra[20], 
	      (plex->Btags).strmap+(plex->Btags).addr[*pp-1],
	      strlen((plex->Btags).strmap+(plex->Btags).addr[*pp-1]),
			0, 0, offsets, XSTR);
	    r1     =  pcre_exec(Re[21], Extra[21], nlastw,
				strlen(nlastw), 0, 0, offsets, XSTR);
	   
	   
	    if(r > 0 || r1 > 0 && strlen(nlastw)>2) {
	      r2     =  pcre_exec(Re[22], Extra[22], lastw,
				  strlen(lastw), 0, 0, offsets, XSTR);
	      if(r2 > 0) {
		pp+=2;
		
		removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
		g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
		first=lastw=nlastw=capfirst=caplast=
		  capnlastw=paap=npaap=nnlastw=aptr=NULL;  goto cont;
	      }
	    } 
	    pp+=2;
	  }
	}

	// remove generic num
	if((int)gg.size()>1) {
	  if(Gen.find(first)) {
	    r     =  pcre_exec(Re[22], Extra[22], lastw,
			       strlen(lastw), 0, 0, offsets, XSTR);
	    if(r>0) {
	      removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	      g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	      first=lastw=nlastw=capfirst=caplast=
		capnlastw=paap=npaap=nnlastw=aptr=NULL;   goto cont;
	    }
	  }
	}

	// remove bad terms from beginning of gene name - 
	// HAS TO BE DONE BEFORE NEXT remove .I and .D
	if(first) {
	  r     =  pcre_exec(Re[23], Extra[23], first,
		   strlen(first), 0,0, offsets, XSTR);
	
	  if(r>0) {
	    if(!strstr(g,"affinity")) {
	      g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	      first=lastw=nlastw=capfirst=caplast=
				   capnlastw=paap=npaap=nnlastw=aptr=NULL;
	      vector<char*>::iterator temp = gg.begin(); 
	      gg.erase(temp);
	      temp = gg.begin(); 
	      while(temp != gg.end()) {
		if(g) {strcat(g,*temp); strcat(g, " ");}
		else  {strcpy(g,*temp); strcat(g, " ");}
		temp++;
	      } 
	      // added to prevent memory leak -- Halil	  
	      temp = lgg.begin();
	      lggptr = *temp;
	      lgg.erase(temp);
	      if (lggptr) delete [] lggptr;
	      if((int)gg.size()>1) {
		first = lgg[0];
		lastw  = lgg[(int)gg.size()-1];
		nlastw = lgg[(int)gg.size()-2];
		capfirst = gg[0];
		caplast  = gg[(int)gg.size()-1];
		capnlastw = gg[(int)gg.size()-2];
		jj++;
	      }
	    }
	  }
	}
	if(first) {
	  r     =  pcre_exec(Re[24], Extra[24], first,
			     strlen(first), 0, 0, offsets, XSTR);
	  if(r>0) {
	    if(!strstr(g,"affinity")) {
	      g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	      first=lastw=nlastw=capfirst=caplast=
				   capnlastw=paap=npaap=nnlastw=aptr=NULL;
	      vector<char*>::iterator temp = gg.begin(); 
	      gg.erase(temp);
	      temp = gg.begin(); 
	      while(temp != gg.end()) {
		if(g) {strcat(g,*temp); strcat(g, " ");}
		else  {strcpy(g,*temp); strcat(g, " ");}
		temp++;
	      } 
	      // added to prevent memory leak -- Halil	  
	      temp = lgg.begin();
	      lggptr = *temp;
	      lgg.erase(temp);
	      if (lggptr) delete [] lggptr;
	      if((int)gg.size()>1) {
		first = lgg[0];
		lastw  = lgg[(int)gg.size()-1];
		nlastw = lgg[(int)gg.size()-2];
		capfirst = gg[0];
		caplast  = gg[(int)gg.size()-1];
		capnlastw = gg[(int)gg.size()-2];
		jj++;
	      }
	    }
	  }    
	} 

        // remove .I and .D from gene name
	if(lastw) {
	  r     =  pcre_exec(Re[25], Extra[25], lastw,
			     strlen(lastw), 0, 0, offsets, XSTR);
	  if(r>0) {
	    gg.pop_back();
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
				 capnlastw=paap=npaap=nnlastw=aptr=NULL;
	    vector<char*>::iterator temp = gg.begin(); 
	    temp = gg.begin(); 
	    while(temp != gg.end()) {
	      if(g) {strcat(g,*temp); strcat(g, " ");}
	      else  {strcpy(g,*temp); strcat(g, " ");}
	      temp++;
	    } 	    
	      // added to prevent memory leak -- Halil	  
	    temp = lgg.end(); temp--;
	    lggptr = *temp;
	    lgg.pop_back();
	    if (lggptr) delete [] lggptr;
	    if((int)gg.size()>1) {
	      first = lgg[0];
	      lastw  = lgg[(int)gg.size()-1];
	      nlastw = lgg[(int)gg.size()-2];
	      capfirst = gg[0];
	      caplast  = gg[(int)gg.size()-1];
	      capnlastw = gg[(int)gg.size()-2];
	      jjj++;
	    }
	    if(lastw) { 
	    r     =  pcre_exec(Re[26], Extra[26], lastw,
			       strlen(lastw), 0, 0, offsets, XSTR);
	    if(r>0) {
	      gg.pop_back();      
	      g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	      first=lastw=nlastw=capfirst=caplast=
				   capnlastw=paap=npaap=nnlastw=aptr=NULL;
	      vector<char*>::iterator temp = gg.begin(); 
	      temp = gg.begin(); 
	      while(temp != gg.end()) {
		if(g) {strcat(g,*temp); strcat(g, " ");}
		else  {strcpy(g,*temp); strcat(g, " ");}
		temp++;
	      }	     
	      // added to prevent memory leak -- Halil	     
	    temp = lgg.end(); temp--;
	    lggptr = *temp;
	    lgg.pop_back();
	    if (lggptr) delete [] lggptr;
	      if((int)gg.size()>1) {
		first = lgg[0];
		lastw  = lgg[(int)gg.size()-1];
		nlastw = lgg[(int)gg.size()-2];
		capfirst = gg[0];
		caplast  = gg[(int)gg.size()-1];
		capnlastw = gg[(int)gg.size()-2];
		jjj++;
	      }
	    }
	  }
	  
	  }	
	}
	// if last word ends in one of these, throw out name
	if(lastw && (int)gg.size()>1) {
	  r     =  pcre_exec(Re[27], Extra[27], lastw,
			     strlen(lastw), 0, 0, offsets, XSTR);
	  r1     =  pcre_exec(Re[28], Extra[28], caplast,
			     strlen(caplast), 0, 0, offsets, XSTR);
	  if(r>0 || r1>0 || Freq.find(lastw) || Freq.find(first)) {
	    if(DEBUG) {
	      cout<< "GCSF: " << Ids[Ids.size()-1] << " " << g << endl;
	    }
	    
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg); 
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL;   goto cont;
	  }
	}
	
	// remove words that end in -ing or -ed from end of gene name
	if(lastw && (int)gg.size()>1) {
	  r     =  pcre_exec(Re[29], Extra[29], lastw,
			      strlen(lastw), 0, 0, offsets, XSTR);
	  if(r>0) {
	    gg.pop_back();      
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
				 capnlastw=paap=npaap=nnlastw=aptr=NULL;
	    vector<char*>::iterator temp = gg.begin(); 
	    temp = gg.begin(); 
	    while(temp != gg.end()) {
	      if(g) {strcat(g,*temp); strcat(g, " ");}
	      else  {strcpy(g,*temp); strcat(g, " ");}
	      temp++;
	    } 
	    if(DEBUG) {
	      cout<< Ids[Ids.size()-1] << " ing or ed: " << g << endl;	
	    }     
	      // added to prevent memory leak -- Halil	  
	    temp = lgg.end(); temp--;
	    lggptr = *temp;
	    lgg.pop_back();
	    if (lggptr) delete [] lggptr;
	    if((int)gg.size()>1) {
	      first = lgg[0];
	      lastw  = lgg[(int)gg.size()-1];
	      nlastw = lgg[(int)gg.size()-2];
	      capfirst = gg[0];
	      caplast  = gg[(int)gg.size()-1];
	      capnlastw = gg[(int)gg.size()-2];
	      jjj++;
	    }
	    if(lastw) { 
	      r     =  pcre_exec(Re[29], Extra[29], lastw,
				 strlen(lastw), 0, 0, offsets, XSTR);
	      if(r>0) {
		gg.pop_back();      
		g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
		first=lastw=nlastw=capfirst=
			       caplast=capnlastw=paap=npaap=nnlastw=aptr=NULL;
		vector<char*>::iterator temp = gg.begin(); 
		temp = gg.begin(); 
		while(temp != gg.end()) {
		  if(g) {strcat(g,*temp); strcat(g, " ");}
		  else  {strcpy(g,*temp); strcat(g, " ");}
		  temp++;
		} 
		if(DEBUG) {
		  cout<< Ids[Ids.size()-1] << " ing or ed: " << g << endl;
		}
	      // added to prevent memory leak -- Halil	  
	    temp = lgg.end(); temp--;
	    lggptr = *temp;
	    lgg.pop_back();
	    if (lggptr) delete [] lggptr;
		if((int)gg.size()>1) {
		  first = lgg[0];
		  lastw  = lgg[(int)gg.size()-1];
		  nlastw = lgg[(int)gg.size()-2];
		  capfirst = gg[0];
		  caplast  = gg[(int)gg.size()-1];
		  capnlastw = gg[(int)gg.size()-2];
		  jj = 3;
		}   
	      }
	    }
	  }
	}

	//  use paa and npaa to check for AA later
	if(first) {
	  r     =  pcre_exec(Re[30], Extra[30], first,
			     strlen(first), 0, 0, offsets, XSTR);
	  if(r>0) {
	    strcpy(paa,first);
	    paap=paa;
	    paap+=offsets[2];
	    paa[offsets[3]]=clastw[0]=cnlastw[0]='\0';
	  }
	}
	
	if(lastw) {
	  r     =  pcre_exec(Re[30], Extra[30], lastw,
			     strlen(lastw), 0, 0, offsets, XSTR);
	  if(r>0) {
	    strcpy(npaa,lastw);
	    npaap=npaa;
	    npaap+=offsets[2];
	    npaa[offsets[3]]=clastw[0]=cnlastw[0]='\0';
	  }
	}
	
	if(first) {
	  r     =  pcre_exec(Re[31], Extra[31], first,
			     strlen(first), 0, 0, offsets, XSTR);
	  if(r>0 && lastw) {
	    if(Aa.find(lastw)) {
	      removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	      g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	      first=lastw=nlastw=capfirst=caplast=
		capnlastw=paap=npaap=nnlastw=aptr=NULL;   goto cont;
	    }
	  }
	  }

	//get the piece of gene name occurring after numbers 
	//which can also have , - ( ) +
	if(g) {
	   r     =  pcre_exec(Re[32], Extra[32], g,
			     strlen(g), 0, 0, offsets, XSTR);
	   if(r>0) {
	     strcpy(afternum,g);
	     aptr = afternum;
	     aptr += offsets[2];
	     afternum[offsets[3]] = '\0';
	     strcpy(capafternum,aptr);
	     for(i=0;afternum[i] != '\0';i++) {
	       if(afternum[i] >= 65 && afternum[i] <= 90) { 
		 (afternum[i])+=32;
	       }
	     }
	     if(strpbrk(aptr,"abcdefghijklmnopqrstuvwxyz")) {
	     }
	     
	   }
	}
	 
	// appears to be dilution or dimensions
	if(g && (int)gg.size()>1) {
	 
	  strcpy(tt,g);
	  for(i=0;tt[i] != '\0';i++) {
	    if(tt[i] >= 65 && tt[i] <= 90) { 
	      (tt[i])+=32;
	    }
	  }
	  r     =  pcre_exec(Re[33], Extra[33], tt,
			     strlen(tt), 0, 0, offsets, XSTR);
	  if(r>0) {
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL;   goto cont;
	  }
	
	}

	// throw out if appears to be an AA
	if(first && lastw && (int)gg.size()>1) {
	  if(Aa.find(first)) {
	    r     =  pcre_exec(Re[34], Extra[34], lastw,
			       strlen(lastw), 0, 0, offsets, XSTR);
	    if(r>0) {
	      removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	      g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	      first=lastw=nlastw=capfirst=caplast=
		capnlastw=paap=npaap=nnlastw=aptr=NULL;   goto cont;
	    }
	  }
	}
	if(paap) {
	  if(Aa.find(paap)) {	    
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL;   goto cont;
	  }
	}
	if(npaap) {
	  if(Aa.find(npaap)) {
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL;   goto cont;
	  }
	}
       
	if(DEBUG && first && lastw && g && (int)gg.size()>1) {
	  cout << first << "," << lastw << "," << g << endl;
	  }

	// non-gene indicators anywhere in the gene name
	if((int)gg.size()>1) {
	 
	  strcpy(tt,g);
	  for(i=0;tt[i] != '\0';i++) {
	    if(tt[i] >= 65 && tt[i] <= 90) { 
	      (tt[i])+=32;
	    }
	  }
	  r     =  pcre_exec(Re[35], Extra[35], tt,
			     strlen(tt), 0, 0, offsets, XSTR);
	  if(r>0) {
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL;   goto cont;
	  }
	
	}
	
	// pH or pi  numbers
	if(first && lastw && (int)gg.size()>1) {
	  r     =  pcre_exec(Re[36], Extra[36], lastw,
			     strlen(lastw), 0, 0, offsets, XSTR);
	  if(r>0 && strcmp(first,"ph")==0 || strcmp(first,"pi")==0) {
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL;   goto cont;
	  }
	}

	// e.g., i.v., etc.
	if(first && lastw && (int)gg.size()>1) {
	  r     =  pcre_exec(Re[37], Extra[37], lastw,
			     strlen(lastw), 0, 0, offsets, XSTR);
	  if(r>0) {
	    r     =  pcre_exec(Re[37], Extra[37], first,
			     strlen(first), 0, 0, offsets, XSTR);
	    if(r>0) {
	      removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	      g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	      first=lastw=nlastw=capfirst=caplast=
		capnlastw=paap=npaap=nnlastw=aptr=NULL;   goto cont;
	    }
	  }
	}

	// mg/ml --> mg
	if(aptr) {
	  r     =  pcre_exec(Re[39], Extra[39], aptr,
			     strlen(aptr), 0, 0, offsets, XSTR);
	  if(r>0) {
	    strcpy(rems,aptr);
	    rems[offsets[2]]=clastw[0]=cnlastw[0]='\0';
	  }
	}
	   
	// first word or next to last word is a number
	if(first && nlastw) {
	  r     =  pcre_exec(Re[38], Extra[38], first,
			     strlen(first), 0, 0, offsets, XSTR);
	  r1     =  pcre_exec(Re[38], Extra[38], nlastw,
		   strlen(nlastw), 0, 0, offsets, XSTR);

	  if(r>0 || r1>0) {
	    
	    // gene name only 2 words long
	    if((int)gg.size()<3) {
	      removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	      g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	      first=lastw=nlastw=capfirst=caplast=
		capnlastw=paap=npaap=nnlastw=aptr=NULL;   goto cont;
	    }
	    // gene name ends in Ban
	    else if(aptr && rems && (int)gg.size()>1 && 
		    (Bad_After_Num.find(aptr) || Bad_After_Num.find(rems))) {
	      r     =  pcre_exec(Re[40], Extra[40], aptr,
			     strlen(aptr), 0, 0, offsets, XSTR);
	      if(r<0) {
		removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
		g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
		first=lastw=nlastw=capfirst=caplast=
		  capnlastw=paap=npaap=nnlastw=aptr=NULL;   goto cont;
	      }
	    }
	    else {
	      if(aptr) {
		r     =  pcre_exec(Re[41], Extra[41], aptr,
			     strlen(aptr), 0, 0, offsets, XSTR);
		if(r>0) {
		  *(aptr+offsets[2])=clastw[0]=cnlastw[0]='\0';
		  capafternum[offsets[2]]=clastw[0]=cnlastw[0]='\0';
		}
	      }
	      if(lastw) {
		nlastw=lastw;
		r     =  pcre_exec(Re[41], Extra[41], nlastw,
			     strlen(nlastw), 0, 0, offsets, XSTR);
		if(r>0) {
		   *(nlastw+offsets[2])=clastw[0]=cnlastw[0]='\0';
		}
	      }
	   
	      // throw if gene name ends in Ban-1
	      if(aptr && Bad_After_Num.find(aptr)) {
		t = strstr(g,capafternum);
		if(strcmp(t,capafternum)==0) {
		  t--; 
		  if(strpbrk(t, " \t\n,.;!?-/:)]}")) {
		    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
		    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
		    first=lastw=nlastw=capfirst=caplast=
		      capnlastw=paap=npaap=nnlastw=aptr=NULL;   goto cont; 
		  }
		}
		else if(nlastw && Bad_After_Num.find(nlastw)) {
		  removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
		  g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
		  first=lastw=nlastw=capfirst=caplast=
		    capnlastw=paap=npaap=nnlastw=aptr=NULL;   goto cont; 
		}
	      }
	      
	    }
	    // gene name has number at beginning, but may still be 
	    //OK (240 kd immunoglobin heavy chain)
	  }
	}
 
	if((int)gg.size()>1) {
	  r     =  pcre_exec(Re[42], Extra[42], g,
			     strlen(g), 0, 0, offsets, XSTR);
	  if(r>0) {
	    strcpy(l1,g);
	    *(l1+offsets[3])=clastw[0]=cnlastw[0]='\0';
	    strcpy(l2,g);
	    l2p=l2;
	    l2p += offsets[4];
	    
	    *(l2+offsets[5])=clastw[0]=cnlastw[0]='\0';
	    for(i=0;l1[i] != '\0';i++) {
	      if(l1[i] >= 65 && l1[i] <= 90) { 
		(l1[i])+=32;
	      }
	    }
	    for(i=0;l2[i] != '\0';i++) {
	      if(l2[i] >= 65 && l2[i] <= 90) { 
		(l2[i])+=32;
	      }
	    }
	    // throw if gene name ends in Ban-1
	    if(Bad_After_Num.find(l1) && Bad_After_Num.find(l2p)) {
	      removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	      g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	      first=lastw=nlastw=capfirst=caplast=
		capnlastw=paap=npaap=nnlastw=aptr=NULL; 
		goto cont; 
	    }
	 
	  }
	}
	if((int)gg.size()>1) {
	  r     =  pcre_exec(Re[43], Extra[43], g,
			     strlen(g), 0, 0, offsets, XSTR);
	  if(r>0) {
	    strcpy(l3,g);
	    l3p=l3;
	    l3p += offsets[2];
	    *(l3+offsets[3])=clastw[0]=cnlastw[0]='\0';
	    strcpy(l4,g);
	    l4p=l4;
	    l4p += offsets[4];
	    
	    *(l4+offsets[5])=clastw[0]=cnlastw[0]='\0';
	    for(i=0;l3[i] != '\0';i++) {
	      if(l3[i] >= 65 && l3[i] <= 90) { 
		(l3[i])+=32;
	      }
	    }
	    for(i=0;l4[i] != '\0';i++) {
	      if(l4[i] >= 65 && l4[i] <= 90) { 
		(l4[i])+=32;
	      }
	    }
	    // throw if gene name ends in Ban-1
	    if(Bad_After_Num.find(l3p) && Bad_After_Num.find(l4p)) {
	      removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	      g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	      first=lastw=nlastw=capfirst=caplast=
		capnlastw=paap=npaap=nnlastw=aptr=NULL; 
		goto cont; 
	    }	 
	  }
	}
	
	/*if((int)gg.size()>1) {
	  cout << Ids[Ids.size()-1] << " 2 " << first << "," 
	       << lastw << "," << g << endl;
	       }*/

	// fp's that end in chain
	if((int)gg.size()>1 && lastw) {
	 
	  strcpy(tt,g);
	  for(i=0;tt[i] != '\0';i++) {
	    if(tt[i] >= 65 && tt[i] <= 90) { 
	      (tt[i])+=32;
	    }
	  }
	  r     =  pcre_exec(Re[44], Extra[44], tt,
			     strlen(tt), 0, 0, offsets, XSTR);
	  if(r>0 && strcmp(lastw,"chain")==0) {
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL; 
	    goto cont;
	  }
	
	}
 
	// et al 
	if((int)gg.size()>1 && first && lastw) {
	  if(strcmp(first,"et")==0 && strcmp(lastw,"al")==0) {
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL; 
	    goto cont;
	  }
	}
	
	// lane 1, ID
	if((int)gg.size()>1 && first) {
	  if(strcmp(first,"lane")==0) {
	    
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL; 
	    goto cont;
	  }
	}
	if((int)gg.size()>1 && lastw) {
	  if(strcmp(lastw,"id")==0) {
	    
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL; 
	    goto cont;
	  }
	}
	// molecular weights
	if((int)gg.size()>1 && first && lastw) {
	  if(strcmp(first,"mw")==0) {
	    r     =  pcre_exec(Re[45], Extra[45], lastw,
			     strlen(lastw), 0, 0, offsets, XSTR);
	    if(r>0) {
	      removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	      g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	      first=lastw=nlastw=capfirst=caplast=
		capnlastw=paap=npaap=nnlastw=aptr=NULL; 
	      goto cont;
	    }
	  }
	}
	// IC50 etc.
	if((int)gg.size()>1) {
	 
	  strcpy(tt,g);
	  for(i=0;tt[i] != '\0';i++) {
	    if(tt[i] >= 65 && tt[i] <= 90) { 
	      (tt[i])+=32;
	    }
	  }
	  r     =  pcre_exec(Re[46], Extra[46], tt,
			     strlen(tt), 0, 0, offsets, XSTR);
	  if(r>0) {
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL; 
	    goto cont;
	  }
	
	}
	// stat
	if((int)gg.size()>1) {	
	  strcpy(tt,ntptr);
	  for(i=0;tt[i] != '\0';i++) {
	    if(tt[i] >= 65 && tt[i] <= 90) { 
	      (tt[i])+=32;
	    }
	  }
	  r     =  pcre_exec(Re[48], Extra[48], tt,
			     strlen(tt), 0, 0, offsets, XSTR);
	  if(r>0) {
	    r     =  pcre_exec(Re[47], Extra[47], tt,
			     strlen(tt), 0, 0, offsets, XSTR);
	    if(r>0) {
	      removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	      g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	      first=lastw=nlastw=capfirst=caplast=
		capnlastw=paap=npaap=nnlastw=aptr=NULL; 
	      goto cont;
	    }
	  }
	
	}
	if((int)gg.size()>1) {
	 
	  strcpy(tt,g);
	  for(i=0;tt[i] != '\0';i++) {
	    if(tt[i] >= 65 && tt[i] <= 90) { 
	      (tt[i])+=32;
	    }
	  }
	  r     =  pcre_exec(Re[49], Extra[49], tt,
			     strlen(tt), 0, 0, offsets, XSTR);
	  if(r>0) {
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL; 
	    goto cont;
	  }
	 
	}
	if((int)gg.size()>1 && first && lastw) {
	  if(strcmp(first,"chi")==0 && strcmp(lastw,"2")==0) {
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL; 
	    goto cont;
	  }
	  if(strcmp(first,"p")==0 && strcmp(lastw,"i")==0) {
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL; 
	    goto cont;
	  }
	  r     =  pcre_exec(Re[50], Extra[50], first,
			     strlen(first), 0, 0, offsets, XSTR);
	  if(r>0) {
	    r     =  pcre_exec(Re[51], Extra[51], lastw,
			       strlen(lastw), 0, 0, offsets, XSTR);
	    if(r>0) {
	      removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	      g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	      first=lastw=nlastw=capfirst=caplast=
		capnlastw=paap=npaap=nnlastw=aptr=NULL; 
	      goto cont;
	    }
	  }
	}
	if((int)gg.size()>1) {
	  if(strstr(g,"ATCC") && strpbrk(g, "0123456789")) {
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL; 
	    goto cont;
	  }
	 
	  strcpy(tt,g);
	  for(i=0;tt[i] != '\0';i++) {
	    if(tt[i] >= 65 && tt[i] <= 90) { 
	      (tt[i])+=32;
	    }
	  }
	  r     =  pcre_exec(Re[52], Extra[52], tt,
			     strlen(tt), 0, 0, offsets, XSTR);
	  if(r>0) {
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL; 
	    goto cont;
	  }
	}
	
	if(DEBUG && (int)gg.size()>1) {
	  cout << Ids[Ids.size()-1] << " 3 " << first 
	  << "," << lastw << "," << g << endl;
	  }
      fin:
	remove=0;
	if((int)gg.size()) {
	  // try to remove general names	  
	  for(rem=0;rem<(int)gg.size();rem++) {
	    if(Gen.find(lgg[rem])) {
	      remove++;
	    }
	  }
	} 
	if(g) {
	  strcpy(tt,g);
	  for(i=0;tt[i] != '\0';i++) {
	    if(tt[i] >= 65 && tt[i] <= 90) { 
	      (tt[i])+=32;
	    }
	  }
	  if(lastw && first) {
	    r      =  pcre_exec(Re[53], Extra[53], lastw,
				strlen(lastw), 0, 0, offsets, XSTR); 
	    r1     =  pcre_exec(Re[54], Extra[54], lastw,
				strlen(lastw), 0, 0, offsets, XSTR);
	    r2     =  pcre_exec(Re[55], Extra[55], first,
				strlen(first), 0, 0, offsets, XSTR);
	    r3     =  pcre_exec(Re[56], Extra[56], tt,
				strlen(tt), 0, 0, offsets, XSTR);
	    r4     =  pcre_exec(Re[57], Extra[57], tt,
				strlen(tt), 0, 0, offsets, XSTR);
	  }
	}
	// finally see if gene name has good last word
	if(lastw) {
	  strcpy(clastw,lastw);
	  k=0;
	  for(i=0;lastw[i] != '\0';i++) {
	    if(lastw[i] != 45) {
	      clastw[k] = lastw[i]; k++;
	    }
	  }
	  clastw[k] = '\0';
	}
	if((int)gg.size()>1 && g && (remove < (int)gg.size()) 
	   && lastw && first && caplast && nlastw 
	   && capnlastw && !Stop.find(first) 
	   && !Stop.find(lastw) && 
	   (r>0 || strcmp(caplast,"A")==0 || 
	    (Genes.find(lastw)||Genes.find(clastw))
	    || Lt.find(lastw)) && r1<0 && r2<0 && r3<0 && r4<0) { 
	  
	  if(DEBUG) {cout << Ids[Ids.size()-1] << " 4 " 
			  << first << "," << lastw << "," << g << endl;}
	
	  if(lgg.size()>2) {
	    nnlastw = lgg[lgg.size()-3];
	  }
	  // filter out pmoles 25 mg-1 protein etc
	  if(nnlastw && nlastw && lastw && Gen.find(lastw)
	     && !strpbrk(nlastw,"ABCDEFGHIJKLMNOPQRSTUVWXYZ")) {  
	    if(Bani.find(nlastw) && Bad_After_Num.find(nnlastw)){
	      for(i=0;i<Words.size();i++) {
		if(Bad_After_Num.find(Words[i])) {
		  removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
		  g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
		  first=lastw=nlastw=capfirst=caplast=
		    capnlastw=paap=npaap=nnlastw=aptr=NULL; 
		  goto cont;
		}
	      }
	    }	   
	  }
	  else if(nlastw && lastw && Gen.find(lastw)) {  
	    if(Bani.find(nlastw)){
	      for(i=0;i<Words.size();i++) {
		if(Bad_After_Num.find(Words[i])) {
		  removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
		  g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
		  first=lastw=nlastw=capfirst=caplast=
		    capnlastw=paap=npaap=nnlastw=aptr=NULL; 
		  goto cont;
		}
	      }
	    }	   
	  }
	  int x = 0;
	  // get the word just before the startof gene name
	  while(x < (gg.size()+1)) {
	    vector<char*>::iterator ppp = Words.begin()+x;
	    if(p==ppp) { x = 9999; }
	    // "else" added by Halil - 04/16/2012
	    // otherwise, segmentation fault on p-=(gg.size()+1) below.
	    // does not make much sense without an "else", anyway.
	    // x != 9999 below would never be true.
	    else {x++;}
	  }
	  if(x != 9999 && first && lastw && nlastw
	     && (Bani.find(first)||Bani.find(lastw)||Bani.find(nlastw))) {
	    p-=(gg.size()+1);
	    if(*p && Bad_After_Num.find(*p)) {
	      p+=(gg.size()+1);
	      
	      removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	      g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	      first=lastw=nlastw=capfirst=caplast=
		capnlastw=paap=npaap=nnlastw=aptr=NULL; 
	      goto cont;
	    }
	    else {p+=(gg.size()+1);}
	  }
	
	  // remove a from beginning of name
	  strcpy(tt,g);
	  for(i=0;tt[i] != '\0';i++) {
	    if(tt[i] >= 65 && tt[i] <= 90) { 
	      (tt[i])+=32;
	    }
	  }
	  r     =  pcre_exec(Re[58], Extra[58], tt,
			     strlen(tt), 0, 0, offsets, XSTR);
	  if(r>0) {
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=capnlastw=
	      paap=npaap=nnlastw=aptr=NULL;
	    vector<char*>::iterator temp = gg.begin(); 
	    gg.erase(temp);
	    temp = gg.begin(); 
	    while(temp != gg.end()) {
	      if(g) {strcat(g,*temp); strcat(g, " ");}
	      else  {strcpy(g,*temp); strcat(g, " ");}
	      temp++;
	    } 
	    if(DEBUG) {cout<< Ids[Ids.size()-1] << " a at beg " << g << endl;}
	    temp = lgg.begin();
	    lggptr = *temp;
	    lgg.erase(temp);
	    if (lggptr) delete [] lggptr;
	    if((int)gg.size()>1) {
	      first = lgg[0];
	      lastw  = lgg[(int)gg.size()-1];
	      nlastw = lgg[(int)gg.size()-2];
	      capfirst = gg[0];
	      caplast  = gg[(int)gg.size()-1];
	      capnlastw = gg[(int)gg.size()-2];
	      jj++;
	    }
	  }
	  strcpy(ca,tt);
	  strcpy(ccw,tt);
	  k=0;
	  for(i=0;ca[i] != '\0';i++) {
	    if(ca[i] >= 65 && ca[i] <= 90) { 
	      (ca[i])+=32;
	    }
	    if(ca[i] != 39 && ca[i] != 40 && ca[i] != 41 &&
	       ca[i] != 34 && ca[i] != 45 && ca[i] != 47 &&
	       ca[i] != 91 && ca[i] != 93 && ca[i] != 60 &&
	       ca[i] != 62 && ca[i] != 58 && ca[i] != 34 ) {
	      ccw[k] = ca[i]; k++;
	    }
	  }
	  ccw[k] = '\0';
	    
	  // throw if orgs, re, cells, journals, AA, Gen 
	  if(Org.find(ccw) || Ren.find(ccw) || Ce.find(ccw) ||
	     Aa.find(ccw) || Gen.find(ccw)) {
	    if(DEBUG) {cout <<  Ids[Ids.size()-1] 
			    << "  Found in bad list:  " << g << endl;}
	    
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL; 
	    goto cont;
	  }
	  // throw out journal names, keeping case information
	  if(J.find(g)) {
	    if(DEBUG) {cout<<  Ids[Ids.size()-1] 
	         << " long journal " << g << endl;
	    }
	    
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL; 
	    goto cont;
	  }
	    
	  //  throw out partial journal entries
	  if(nlastw) {
	    strcpy(cnlastw,nlastw);
	    k=0;
	    for(i=0;nlastw[i] != '\0';i++) {
	      if(nlastw[i] != 45) {
		cnlastw[k] = nlastw[i]; k++;
	      }
	    }
	    cnlastw[k] = '\0';
	  }
	  if(lastw && nlastw && capnlastw && Lj.find(capnlastw) 
	     && strpbrk(lastw, "0123456789") && !Genes.find(nlastw)
	     && !Genes.find(cnlastw)) {
	    r     =  pcre_exec(Re[61], Extra[61], lastw,
			       strlen(lastw), 0, 0, offsets, XSTR);
	  
	    if(r>0) {
	     
	      
	      removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	      g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	      first=lastw=nlastw=capfirst=caplast=
		capnlastw=paap=npaap=nnlastw=aptr=NULL; 
	      goto cont;
	    }
	    else { goto cont; }
	  }
	  else {
	    start = (j-jjj) - ((int)gg.size()-jj) -jj;
	    
	    for(y=start,yy=0;yy<(int)gg.size();y++,yy++) {
	      if(gg.size()>1) {
		/*if(y<0) {
		  cout << y << ":" << g << "|" <<start 
		       << "|" << jj << "|" << j 
		       << "|" <<instr << endl; exit(0);}*/
		       Tags[y] = (plex->Btags).find("MULTIGENE");
		       if(DEBUG){
		  cout << Words[y] << " changed to GENE " << g << endl;
		  }
	      }
	    } 
	    // cout<< g << endl;
	    
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);  
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL; 
	    goto cont;
	  }
	}
	
	else {
	  if(g && remove == (int)gg.size() && (int)gg.size()>1) {
	    if(DEBUG) {cout<< Ids[Ids.size()-1] 
			   << " general! " << g << endl;}
	  }
	  else if(g && (int)gg.size()>1) {
	   
	    jjj++;
	   
	    removedlast++;
	    gg.pop_back();
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=caplast=
	      capnlastw=paap=npaap=nnlastw=aptr=NULL;
	    vector<char*>::iterator temp = gg.begin(); 
	    temp = gg.begin(); 
	    while(temp != gg.end()) {
	      if(g) {strcat(g,*temp); strcat(g, " ");}
	      else  {strcpy(g,*temp); strcat(g, " ");}
	      temp++;
	    } 
	    if(DEBUG) {cout << Ids[Ids.size()-1] << " ID: " << g << endl;}
	    temp = lgg.end(); temp--;
	    lggptr = *temp;
	    lgg.pop_back();
	    if (lggptr) delete [] lggptr;	    
	    if((int)gg.size()>1) {
	      first = lgg[0];
	      lastw  = lgg[(int)gg.size()-1];
	      nlastw = lgg[(int)gg.size()-2];
	      capfirst = gg[0];
	      caplast  = gg[(int)gg.size()-1];
	      capnlastw = gg[(int)gg.size()-2];
	     
	    }
	    if(lastw) {
	      r     =  pcre_exec(Re[60], Extra[60], lastw,
				 strlen(lastw), 0, 0, offsets, XSTR);
	      if(r<0) {
		if((int)gg.size()>1 && DEBUG) { cout << Ids[Ids.size()-1] 
		  << " last removed: " << g << endl;
		}
		goto fin;
	      }
	    }
	    else {
	      g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	      first=lastw=nlastw=capfirst=
		caplast=capnlastw=paap=
		npaap=nnlastw=aptr=NULL; 
	      removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);
		if((int)gg.size()>1 && DEBUG) {cout << Ids[Ids.size()-1]
		  << " last " << g << endl;
		} 
		goto cont;
	    }
	  }
	  else {
	    g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	    first=lastw=nlastw=capfirst=
	      caplast=capnlastw=paap=
	      npaap=nnlastw=aptr=NULL; 
	    removedlast=jj=jjj=0; gg.clear(); free_vec_str(lgg);
	      if((int)gg.size()>1 && DEBUG) {cout << Ids[Ids.size()-1] 
		<< " last FINAL " << g << endl;
	      } 
	      goto cont;
	  }
	
	  
	  g[0]=rems[0]=clastw[0]=cnlastw[0]='\0'; 
	  first=lastw=nlastw=capfirst=
	    caplast=capnlastw=paap=
	    npaap=nnlastw=aptr=NULL; jj=jjj=0; gg.clear(); free_vec_str(lgg);
	    removedlast=0;
	    cont : p++;pp++; 
	}
	if(gptr) {delete [] gptr;} 
      }
    }
  }  
  if(ntptr) {delete [] ntptr;} 
  if(lgg.size()>0) {free_vec_str(lgg);}

}
  	
void PostBrill_FN::get_newgenes() {
  long i,j,k,found=0;
  const char* errptr;
  const char* study_error;
  int erroffset;
  int offsets[XSTR];
  int r,r1;   
  pcre* pre = NULL;
  pcre_extra* pextra;
  char* cptr;
  char* afterg = NULL;
  char* beforeg = NULL;
  char* tptr;
  char lcw[XSTR];
  char cw [XSTR];
  char rn [XSTR];
  char* tempptr;

  if(Words.size() == Tags.size()) {
    vector<char*>::iterator p = Words.begin();
    vector<long>::iterator pp = Tags.begin();
    vector<char*>::iterator ppp = Words.end()-1;
   
    while(p != Words.end()) {
      while (pp != Tags.end()) {
	found=0;
	j=k=0;	
	strcpy(lcw,*p);
	strcpy(cw,*p);
	
	for(i=0;lcw[i] != '\0';i++) {
	  // get lower case current word = lcw
	  if(lcw[i] >= 65 && lcw[i] <= 90) { 
	    (lcw[i])+=32;
	  }
	  // get cur word without -    = cw  
	  if(lcw[i] != 45) {
	    cw[k] = lcw[i]; k++;
	  }
	}
	cw[k] = '\0';

	tptr = new char[15];
	strcpy(tptr, (plex->Btags).strmap+(plex->Btags).addr[*pp-1]);
	
	// unbalanced parens
	r      = pcre_exec(Re[62], Extra[62], lcw, 
			   strlen(lcw), 0, 0, offsets, XSTR);
	if(r > 0) {
	  if(!found) {
	    r      = pcre_exec(Re[0], Extra[0], lcw, 
			       strlen(lcw), 0, 0, offsets, XSTR);
	    if(r > 0) {
	      r      = pcre_exec(Re[1], Extra[1], lcw, 
				 strlen(lcw), 0, 0, offsets, XSTR);
	      if(r == -1) {
		*pp = (plex->Btags).find("NN");
		found=1;
	      }
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[2], Extra[2], lcw, 
			       strlen(lcw), 0, 0, offsets, XSTR);
	    if(r > 0) {
	      r      = pcre_exec(Re[3], Extra[3], lcw, 
				 strlen(lcw), 0, 0, offsets, XSTR);
	      if(r == -1) {
		*pp = (plex->Btags).find("NN");
		found=1;
	      }
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[1], Extra[1], lcw, 
			       strlen(lcw), 0, 0, offsets, XSTR);
	    if(r > 0) {
	      r      = pcre_exec(Re[4], Extra[4], lcw, 
				 strlen(lcw), 0, 0, offsets, XSTR);
	      if(r == -1) {
		*pp = (plex->Btags).find("NN");
		found=1;
	      }
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[3], Extra[3], lcw, 
			       strlen(lcw), 0, 0, offsets, XSTR);
	    if(r > 0) {
	      r      = pcre_exec(Re[5], Extra[5], lcw, 
				 strlen(lcw), 0, 0, offsets, XSTR);
	      if(r == -1) {
		*pp = (plex->Btags).find("NN");
		found=1;
	      }
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[6], Extra[6], lcw, 
			       strlen(lcw), 0, 0, offsets, XSTR);
	    if(r > 0) {
	      r      = pcre_exec(Re[8], Extra[8], lcw, 
				 strlen(lcw), 0, 0, offsets, XSTR);
	      if(r == -1) {
		*pp = (plex->Btags).find("NN");
		found=1;
	      }
	    }
	  }
	  if(!found) {
	    r      = pcre_exec(Re[7], Extra[7], lcw, 
			       strlen(lcw), 0, 0, offsets, XSTR);
	    if(r > 0) {
	      r      = pcre_exec(Re[9], Extra[9], lcw, 
				 strlen(lcw), 0, 0, offsets, XSTR);
	      if(r == -1) {
		*pp = (plex->Btags).find("NN");
		found=1;
	      }
	    }
	  }
	
	  // genes on big list with number in name or good before or after
	  if(!found) {	
	    if(!(strstr(tptr,"GENE") || strstr(tptr,"CELL")
		 || strstr(tptr,"RENZYME"))) {
	      if((Genes.find(lcw)||Genes.find(cw))
		 && !Ce.find(cw) && !Gen.find(cw) && !Stop.find(cw)
		 && strlen(cw) >= 3 && !strchr(tptr,'V')) {
		r      = pcre_exec(Re[11], Extra[11], cw, 
				   strlen(cw), 0, 0, offsets, XSTR);
		if(r < 0) {
		  r      = pcre_exec(Re[10], Extra[10], cw, 
				     strlen(cw), 0, 0, offsets, XSTR);
		  if(r > 0) {
		    if(p!=Words.begin()) {
		      p--;
		      r     = pcre_exec(Re[41], Extra[41], *p, 
					strlen(*p), 0, 0, offsets, XSTR);
		      r1     = pcre_exec(Re[61], Extra[61], *p, 
					strlen(*p), 0, 0, offsets, XSTR);
		      p++;
		      if(r<0 && p!=ppp && r1<0 && !Bani.find(*p)) {
			p++;
			r     = pcre_exec(Re[41], Extra[41], *p, 
					strlen(*p), 0, 0, offsets, XSTR);
			if(r<0) {
			  *pp = (plex->Btags).find("NEWGENE");
			  found=1;
			}
			p--;
		      }
		    }
		    else if(p!=ppp) {
		      p++;
		      r     = pcre_exec(Re[41], Extra[41], *p, 
					strlen(*p), 0, 0, offsets, XSTR);
		      if(r<0) {
			*pp = (plex->Btags).find("NEWGENE");
			found=1;
		      }
		      p--;
		    }
		  }
		}
	      }
	    }
	  }
	}
	// set before/after potential gene name
	if(!found && p != ppp) {
	  
	  p+=1;
	  afterg = new char[strlen(*p)+1];
	  strcpy(afterg, *p);
	  p-=1;
	  for(i=0;afterg[i] != '\0';i++) {
	    if(afterg[i] >= 65 && afterg[i] <= 90) { 
	      (afterg[i])+=32;
	    }
	  }
	  After.push_back(afterg);
	}
	
	if(!found && p != Words.begin()) {
	
	  p-=1;
	  beforeg = new char[strlen(*p)+1];
	  strcpy(beforeg, *p);
	  p+=1;
	  for(i=0;beforeg[i] != '\0';i++) {
	    if(beforeg[i] >= 65 && beforeg[i] <= 90) { 
	      (beforeg[i])+=32;
	    }
	  }
	  Before.push_back(beforeg);
	}

	tempptr=lc(no_tagstr);
	
	if(!found && !strchr(tptr,'V') && !(strstr(tptr,"GENE"))) {
	  if(p!=ppp && afterg && Gaft.find(afterg) && (Genes.find(cw)||Genes.find(lcw))
	     && !Stop.find(cw)) {
	    if(!(strstr(tempptr, "cell") ||
	       strstr(tempptr, "target"))) {
	      *pp = (plex->Btags).find("NEWGENE");
	      found=1;
	    }
	  }

	  if(p != Words.begin()) {
	    if(!found && Gbef.find(beforeg)
	       && (Genes.find(cw)||Genes.find(lcw)) && !Stop.find(cw)) {
	      if(!(strstr(tempptr, "cell") ||
		   strstr(tempptr, "target"))) {
		*pp = (plex->Btags).find("NEWGENE");
		found=1;
	      }
	    }
	  }

	  // contains low trigram and good before or after
	  if(!found && strlen(lcw) >=3) {
	    for(i=0; i<strlen(lcw)-2; i++) {
	      rn[0] = lcw[i];
	      rn[1] = lcw[i+1];
	      rn[2] = lcw[i+2];
	      rn[3] = '\0';
  
	      if(Lt.find(rn) && !Gen.find(lcw) && !Stop.find(lcw)
		 && !Ren.find(cw) && strpbrk(rn,"abcdefghijklmnopqrstuvwxyz-*/':;.")) {
		if(!(strstr(tempptr, "cell") ||
		     strstr(tempptr, "target") ||
		     strstr(tempptr, "blast"))) {
		  if(!found && afterg) {
		    if(Gaft.find(afterg)) {
		      *pp = (plex->Btags).find("NEWGENE");
		      found=1;
		    }
		  }
		  else if(!found && beforeg) {
		    if(Gbef.find(beforeg)) {
		      *pp = (plex->Btags).find("NEWGENE");
		      found=1;
		    }
		  }
		  
		}
	      
	    }
	  }
	}
	
	  /*if (!found) {
	  // NEW STUFF HERE!!!!
	  if(!found && strpbrk(*p,"()[]:")) {
	    if(beforeg) {
	      if(Gbef.find(beforeg)) {
		*pp = (plex->Btags).find("NEWGENE");
		found=1;
	      }
	    }
	    else if(afterg) {
	      if(Gaft.find(beforeg)) {
		*pp = (plex->Btags).find("NEWGENE");
		found=1;
	      }
	    }
	    }*/
	  
	}	
	if(tempptr) {delete [] tempptr;} 	
	if(tptr)    { delete [] tptr; }
	p++;pp++; 		
      }
    }
  }
}

void PostBrill_FN::clear_all() {
  free_vec_str(Words);
  free_vec_str(Before);
  free_vec_str(After);
  free_vec_str(lcwords);
  Tags.clear();
  strcpy(last_no_tagstr, no_tagstr);
  strcpy(last_tagstr, tagstr);
  strcpy(no_tagstr, "");
  strcpy(tagstr, "");
}

Count* PostBrill_FN::set_substrings(char* st) {
  char *ptr, *p;
  int j,k;
  Count* lptr;
  lptr = new Count();
  Wsw->convert(st,strlen(st));
  j=1;
  ptr=st;
  while(p=strchr(ptr,' ')) {
    ptr=p; ptr++;
    j++;
  }
  while(j>=1) {  
    Wsw->multiple(j);
    for(k=0;k<Wsw->cnt;k++) {
      lptr->add_count(*(Wsw->list+k),(long)1);
    }
    j--;  
    Wsw->clear_list();
  }  
  if(st) {delete [] st;}
  return lptr;
  
}

float PostBrill_FN::get_score(char* s) {
  float sss;
  char* ptr;
  List* lsptr;
  int i,k,len;
  char chp[max_str];
  lsptr = new List;
  
  Wrd->convert(s,i=strlen(s)); 
  Wrd->modify('\'');
  Wrd->single();
  for(k=0;k<Wrd->cnt;k++){
    strcpy(chp,*(Wrd->list+k)); 
    len=strlen(chp);
    *(chp+len)='!';
    *(chp+len+1)='!';
    *(chp+len+2)='t';
    *(chp+len+3)='\0';
    lsptr->add_key(chp); 
  }
  Wrd->clear_list();
  Wrd->multiple(2);
 
  for(k=0;k<Wrd->cnt;k++){
    strcpy(chp,*(Wrd->list+k));
    len=strlen(chp);
    *(chp+len)='!';
    *(chp+len+1)='!';
    *(chp+len+2)='p';
    *(chp+len+3)='\0';
    lsptr->add_key(chp);
  }
  Wrd->clear_list();
  
  sss=0;
 
  lsptr->node_first();
  
  while(lsptr->node_next()) {
    sss+=LC.weight(ptr=lsptr->show_str());
  }
  delete lsptr;
  delete []s;
  if(sss>=0.0) {
    return 1;
  }
  else {
    return 0;
  }
 
}
 
void PostBrill_FN::free_vec_str(vector<char*> &vc) {
  vector<char*>::iterator p = vc.begin();
  while(p!=vc.end()){
    if(*p) delete [] *p;
    p++;
  }
  vc.clear();
}

// Added by Halil
// Prevents memory leak
void PostBrill_FN::free_vec_pcre(vector<pcre*> &vc) {
  vector<pcre*>::iterator p = vc.begin();
  while(p!=vc.end()){
    if(*p) pcre_free(*p);
    p++;
  }
  vc.clear();
}

// Added by Halil
// Prevents memory leak
void PostBrill_FN::free_vec_pcre_extra(vector<pcre_extra*> &vc) {
  vector<pcre_extra*>::iterator p = vc.begin();
  while(p!=vc.end()){
    if(*p) pcre_free(*p);
    p++;
  }
  vc.clear();
}

// Added by Halil
// Prevents memory leak
void PostBrill_FN::gclose_PostBrill_FN(void) {
  LC.gclose_operate();
  Mg.gclose_htable_map();
  Gen.gclose_htable_map();
  Aa.gclose_htable_map();
  Ren.gclose_htable_map();
  Ce.gclose_htable_map();
  Org.gclose_htable_map();
  Bad_After_Num.gclose_htable_map();
  Genes.gclose_htable_map();
  J.gclose_htable_map();
  Lj.gclose_htable_map();
  Freq.gclose_htable_map();
  Gin.gclose_htable_map();
  Lt.gclose_htable_map();
  Stop.gclose_htable_map();
  Gaft.gclose_htable_map();
  Gbef.gclose_htable_map();
  GoodTags.gclose_htable_map();
  Bani.gclose_htable_map();
  plex->gclose_bmatrix();
  
}
  
}











