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
#include <Brill.h>
#include <Word.h>
#include <string>
#include <algorithm>
#include <vector>
#include <Hash.h>


#define PENN 0               // assumes input text tokenized 
#define DEBUG 0
#define MAXTAGLEN 256        // max char length of pos tags 
#define MAXWORDLEN 100000    // max char length of words 
#define PHRASE_MODE 0        // if set to 1, don't break on periods
#define RESTRICT_MOVE 1      /* if this is set to 1, then a rule "change a tag
			     from x to y" will only apply to a word if:
			     a) the word was not in the training set or
			     b) the word was tagged with y at least once in
			     the training set  
			     When training on a very small corpus, better
			     performance might be obtained by setting this to
			     0, but for most uses it should be set to 1 */
using namespace std;
namespace iret {
  
Brill::Brill(void){
  Wrd = new Word;
  Wrd->byte_lim=0;
  Wrd->back_lim=0;
  Wrd->all_print_map();
  Wrd->stop=0;
  Wrd->set_map(".,:;!?", '\024',1);
  Wrd->pre_punct(); 
}

Brill::Brill(const char *nam){
  int lxn=strlen(nam);
  name=new char[lxn+1];
  strcpy(name,nam); 
  Wrd = new Word(max_str,nam);
  Wrd->byte_lim=0;
  Wrd->back_lim=0;
  Wrd->all_print_map();
  Wrd->stop=0;
  Wrd->set_map(".,:;!?", '\024',1);
  Wrd->pre_punct();
}

Brill::~Brill(){  
  if(name!=NULL)delete [] name;
  if(plex) delete plex; 
  if(Wrd) delete Wrd;
  free_vec_str(Crv);
  free_vec_str(Lrv);
}

void Brill::change_name(char *nam){
  if(name!=NULL)delete [] name;
  int lxn=strlen(nam);
  name=new char[lxn+1];
  strcpy(name,nam);
}

void Brill::open_tagger(void){
  ifstream fin_lrv, fin_crv;
  int pflag=get_qflag(),flag=1;
  char cnam[max_str];
  char *nothing;
  char *aptr, *ptr, *pptr, c, **trm;
  long cr_size,lr_size;
  int  i,num;
  char *rule;
  nothing = new char[10];
  strcpy(nothing,"NOOTHING");

  plex = new Lexicon(name);
  plex->gopen_bmatrix();
  // Modified fin's to fin_lrv to prevent coredumps -- Halil
  get_pathw(cnam,"tagset",name,"lr");
  fin_lrv.open(cnam, ios::in);
  if(!fin_lrv.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
  while(fin_lrv.getline(cnam, max_str, '\n')) { 
    aptr= new char[strlen(cnam)+1];
    strcpy(aptr,cnam);
    Lrv.push_back(aptr);
  }
  fin_lrv.close();
  num = Lrv.size();
  LOld.resize(num, 0); LNewt.resize(num,0); 
  LWhen.resize(num,nothing); LLen.resize(num, 0); 
  LWord.resize(num,nothing);

  LWord.assign(num,nothing); LLen.assign(num,  0);

  LrvCount.add_count("char",1);        LrvCount.add_count("fchar",2); 
  LrvCount.add_count("deletepref",3);  LrvCount.add_count("fdeletepref",4); 
  LrvCount.add_count("haspref",5);     LrvCount.add_count("fhaspref",6); 
  LrvCount.add_count("deletesuf",7);   LrvCount.add_count("fdeletesuf",8); 
  LrvCount.add_count("hassuf",9);      LrvCount.add_count("fhassuf",10); 
  LrvCount.add_count("addpref",11);    LrvCount.add_count("faddpref",12); 
  LrvCount.add_count("addsuf",13);     LrvCount.add_count("faddsuf",14); 
  LrvCount.add_count("goodleft",15);   LrvCount.add_count("fgoodleft",16); 
  LrvCount.add_count("goodright",17);  LrvCount.add_count("fgoodright",18); 

  for(i=0;i<Lrv.size();i++) {
    rule = Lrv[i];
    // moved cptr to Brill.h and removed cptr = new statement to prevent memory leak -- Halil
//     cptr= new char[strlen(rule)+1];
//     strcpy(cptr,"");
//     strcpy(cptr,rule);
    cptr = rule;

    // rule is either the second or third elem
    aptr = strtok(cptr, " "); 
    ptr  = strtok(NULL, " "); 
    pptr = strtok(NULL, " ");

    if(LrvCount.count(ptr)) {
	LWhen[i] = ptr;
    }
    else if (LrvCount.count(pptr)) {
	LWhen[i] = pptr;
    }
    else {
      cout << "Error LrvCount\n";
    }
    num = LrvCount.count(LWhen[i]);
    switch(num) {
    case 1:
      LWord[i] = aptr;
      LNewt[i] = (plex->Btags).find(pptr);
      break;
    case 2:
      LOld[i]  = (plex->Btags).find(aptr);
      LWord[i] = ptr;
      LNewt[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 3:
      LWord[i] = aptr;
      LLen[i]  = atoi(pptr);
      LNewt[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 4:
      LOld[i]  = (plex->Btags).find(aptr);
      LWord[i] = ptr;
      LLen[i]  = atoi(strtok(NULL, " "));
      LNewt[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 5:
      LWord[i] = aptr;
      LLen[i]  = atoi(pptr);
      LNewt[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 6:
      LOld[i]  = (plex->Btags).find(aptr);
      LWord[i] = ptr;
      LLen[i]  = atoi(strtok(NULL, " "));
      LNewt[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 7:
      LWord[i] = aptr;
      LLen[i]  = atoi(pptr);
      LNewt[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 8:
      LOld[i]  = (plex->Btags).find(aptr);
      LWord[i] = ptr;
      LLen[i]  = atoi(strtok(NULL, " "));
      LNewt[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 9:
      LWord[i]  = aptr;
      LLen[i]   = atoi(pptr);
      LNewt[i]  = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 10:
      LOld[i]  = (plex->Btags).find(aptr);
      LWord[i] = ptr;
      LLen[i]  = atoi(strtok(NULL, " "));
      LNewt[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 11:
      LWord[i] = aptr;
      LLen[i]  = atoi(pptr);
      LNewt[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 12:
      LOld[i]  = (plex->Btags).find(aptr);
      LWord[i] = ptr;
      LLen[i]  = atoi(strtok(NULL, " "));
      LNewt[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 13:
      LWord[i] = aptr;
      LLen[i]  = atoi(pptr);
      LNewt[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 14:
      LOld[i]  = (plex->Btags).find(aptr);
      LWord[i] = ptr;
      LLen[i]  = atoi(strtok(NULL, " "));
      LNewt[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 15:
      LWord[i] = aptr;
      LNewt[i] = (plex->Btags).find(pptr);
      break;
    case 16:
      LOld[i]  = (plex->Btags).find(aptr);
      LWord[i] = ptr;
      LNewt[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 17:
      LWord[i] = aptr;
      LNewt[i] = (plex->Btags).find(pptr);
      break;
    case 18:
      LOld[i]  = (plex->Btags).find(aptr);
      LWord[i] = ptr;
      LNewt[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    default:
      cout << "FOUND AN UNKNOWN LEXICAL RULE\n";
      break;
    }
  }

//   for(i=0;i<Lrv.size();i++) {
//      cout << i << " " << LOld[i] << " " << LNewt[i] << " " 
// 	  << LWhen[i] << " " << LWord[i] << " " << LLen[i] << endl;
// 	  }

  Lw.change_name(name);
  Lw.gopen_htable_map();

  get_pathw(cnam,"tagset",name,"cr");
  // Modified fin's to fin_crv to prevent coredumps -- Halil
  fin_crv.open(cnam, ios::in);
  if(!fin_crv.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
  while(fin_crv.getline(cnam, max_str, '\n')) { 
   aptr= new char[strlen(cnam)+1];
    strcpy(aptr,cnam);
    Crv.push_back(aptr);
  } 
  fin_crv.close();
  
  strcpy(cnam,name);
  strcat(cnam,"_rb");
  Rb.change_name(cnam);
  strcpy(cnam,name);
  strcat(cnam,"_lb");
  Lb.change_name(cnam);
  Rb.gopen_bmatrix();
  Lb.gopen_bmatrix();

  num = Crv.size();

  Old.resize(num,  0); Newt.resize(num,    0); 
  Lft.resize(num,  0); Rght.resize(num,    0); 
  Prev1.resize(num,0); Prev2.resize(num,   0); 
  Next1.resize(num,0); Next2.resize(num,   0); 
  Prev1t.resize(num,0); Prev2t.resize(num,   0); 
  Next1t.resize(num,0); Next2t.resize(num,   0); 
  When.resize(num, nothing); TheWord.resize(num, nothing); 
  Tag.resize(num,  0);
  
  Lft.assign(num,     0);  Rght.assign(num, 0);
  Prev1.assign(num,   nothing); Prev2.assign(num, nothing);
  Next1.assign(num,   nothing); Next2.assign(num, nothing);
  Prev1t.assign(num,   0); Prev2t.assign(num, 0);
  Next1t.assign(num,   0); Next2t.assign(num, 0);
  TheWord.assign(num, nothing);  Tag.assign(num,  0);

  CrvCount.add_count("NEXTTAG",1);        CrvCount.add_count("NEXT2TAG",2);      
  CrvCount.add_count("NEXT1OR2TAG",3);    CrvCount.add_count("NEXT1OR2OR3TAG",4); 
  CrvCount.add_count("PREVTAG",5);        CrvCount.add_count("PREV2TAG",6);       
  CrvCount.add_count("PREV1OR2TAG",7);    CrvCount.add_count("PREV1OR2OR3TAG",8); 

  CrvCount.add_count("NEXTWD",9);         CrvCount.add_count("CURWD",10);          
  CrvCount.add_count("NEXT2WD",11);       CrvCount.add_count("NEXT1OR2WD",12);    
  CrvCount.add_count("NEXT1OR2OR3WD",13); CrvCount.add_count("PREVWD",14);        
  CrvCount.add_count("PREV2WD",15);       CrvCount.add_count("PREV1OR2WD",16);    
  CrvCount.add_count("PREV1OR2OR3WD",17);

  CrvCount.add_count("SURROUNDTAG",18);   
  CrvCount.add_count("PREVBIGRAM",19);
  CrvCount.add_count("NEXTBIGRAM",20);    

  CrvCount.add_count("LBIGRAM",21);       CrvCount.add_count("WDPREVTAG",22);
  CrvCount.add_count("RBIGRAM",23);       CrvCount.add_count("WDNEXTTAG",24); 
  CrvCount.add_count("WDAND2BFR",25);     CrvCount.add_count("WDAND2TAGBFR",26);  
  CrvCount.add_count("WDAND2AFT",27);     CrvCount.add_count("WDAND2TAGAFT",28);  

  for(i=0;i<Crv.size();i++) {
    rule = Crv[i];
    // moved cptr to Brill.h and removed cptr = new statement to prevent memory leak -- Halil
//     cptr= new char[strlen(rule)+1];
//     strcpy(cptr,"");
//     strcpy(cptr,rule);
    cptr = rule;

    Old[i]  = (plex->Btags).find(strtok(cptr, " ")); 
    Newt[i] = (plex->Btags).find(strtok(NULL, " ")); 
    When[i] = strtok(NULL, " "); 
    num = CrvCount.count(When[i]);
    
    switch(num) {
    case 1:
      Tag[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 2:
      Tag[i] = (plex->Btags).find(strtok(NULL, " "));
      break;  
    case 3:
      Tag[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 4:
      Tag[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 5:
      Tag[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 6:
      Tag[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 7:
      Tag[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 8:
      Tag[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 9:
      TheWord[i] = strtok(NULL, " ");
      break;
    case 10:
      TheWord[i] = strtok(NULL, " ");
      break;
    case 11:
      TheWord[i] = strtok(NULL, " ");
      break;
    case 12:
      TheWord[i] = strtok(NULL, " ");
      break;
    case 13:
      TheWord[i] = strtok(NULL, " ");
      break;
    case 14:
      TheWord[i] = strtok(NULL, " ");
      break;
    case 15:
      TheWord[i] = strtok(NULL, " ");
      break;
    case 16:
      TheWord[i] = strtok(NULL, " ");
      break;
    case 17:
      TheWord[i] = strtok(NULL, " ");
      break;
    case 18:
      Lft[i]  = (plex->Btags).find(strtok(NULL, " "));
      Rght[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 19:
      Prev1t[i] = (plex->Btags).find(strtok(NULL, " "));
      Prev2t[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 20:
      Next1t[i] = (plex->Btags).find(strtok(NULL, " "));
      Next2t[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 21:
      Prev1[i] = strtok(NULL, " ");
      TheWord[i] = strtok(NULL, " ");
      break;
    case 22:
      Prev1t[i] = (plex->Btags).find(strtok(NULL, " "));
      TheWord[i] = strtok(NULL, " ");
      break;
    case 23:
      TheWord[i] = strtok(NULL, " ");
      Next1[i] = strtok(NULL, " ");
      break;
    case 24:
      TheWord[i] = strtok(NULL, " ");
      Next1t[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    case 25:
      Prev2[i] = strtok(NULL, " ");
      TheWord[i] = strtok(NULL, " ");
      break;
    case 26:
      Prev2t[i] = (plex->Btags).find(strtok(NULL, " "));
      TheWord[i] = strtok(NULL, " ");
      break;
    case 27:
      TheWord[i] = strtok(NULL, " ");
      Next2[i] = strtok(NULL, " ");
      break;
    case 28:
      TheWord[i] = strtok(NULL, " ");
      Next2t[i] = (plex->Btags).find(strtok(NULL, " "));
      break;
    default:
      cout << "FOUND AN UNKNOWN CONTEXTUAL RULE\n";
      break;
    }
  }
//   for(i=0;i<Crv.size();i++) {
//     cout << i << " " << Old[i] << " " << Newt[i] << " " 
// 	 << When[i] << " " << Tag[i] << " " << TheWord[i] << " " 
// 	 << Lft[i] << " " << Rght[i] << " " << Prev1[i] << " " 
//          << Prev2[i] << " " << Next1[i] << " " << Next2[i] << endl;
// 	 }

  // added to prevent memory leak -- Halil
  delete [] nothing;
}

char* Brill::get_str(void) {
  return tagstr; cout << tagstr << endl;
}

// -- Added by Halil
void Brill::read_and_tok_sub(char* str) {
    char *line;
    line = strtok(str,"\n");
    read_and_tok(line);
    while((line = strtok(NULL,"\n")) != NULL) {
	read_and_tok(line);
    }
}

void Brill::read_and_tok(char* str) {
  tagstr = str;
  //ifstream fin;
  //ofstream fout;
  //char cnam [XSTR];
  char* nnstr = new char[XSTR];
  const char* nstr;
  char *aptr, *ptr, *uptr, *pptr, *temp;
  syn_lk* sptr;
  long len, k;
  char a[128];
  int flag   = 1;
  int begin  = 1;
  int eflag  = 0;
  int eeflag = 0;
  int bflag  = 0;
  int bbflag = 0;
  int i = 0;
  char c,c1,c2,c3;
  string s;
  int pos;
  string::size_type npos;
  char *staart;
  char *staart1;
  staart = new char[7];
  staart1 = new char[7];
  strcpy(staart,"STAART");
  strcpy(staart1,"STAART");
  
  Terms.push_back(staart);
  Terms.push_back(staart1);

  for(k=0;k<128;k++) {
    if (k <= 57 && k >= 48) { a[k] = 1;}
    else { a[k]=0;}
  }
  a[44]=1; a[46]=1;
  
 
  // protect *#$ from punct function
  s = (string) str;
  replace(s.begin(), s.end(), '$', (char)1);
  replace(s.begin(), s.end(), '#', (char)2);
  replace(s.begin(), s.end(), '*', (char)3);
  nstr = const_cast<char*> (s.c_str());
  
  // use punct function
  strcpy(nnstr, nstr);
  len=strlen(nnstr);
  Wrd->punct(len,nnstr);
  uptr=cptr=nnstr;

  while(*cptr) {
    uptr=cptr;
    flag=1;
   
    // skip space and tabs at beginning
     while(((c=*uptr) == ' ' || c == '\t') && c != '\0') { 
      if (c == ' ' || strchr(uptr, '-') == 0) {
	//if(strchr(uptr, '-') == 0) {
	  eflag=bflag=1;	
	  //}
      }
      uptr++;
    } 
    
    if(*uptr != '\0') {    // a word starts here
      if(begin) {          // lc first word of sentence if second letter is lc
	// COMMENT OUT TO SYNC WITH ORIGINAL BRILL TAGGER AND LEXICONS
	// To and to are different in the LEXICON, for ex.

        /*if (*uptr >= 65 && *uptr <= 90) {
	  aptr=uptr;
	  aptr++;
	  if(!(*aptr >= 65 && *aptr <= 90)) { 
	    *uptr+=32;
	  }
	  }*/
	eflag=bflag=1;
        begin=0;
      }
    
      switch(*uptr) {
      case '!':
	temp = new char[2];
	strcpy(temp,"!");
	Terms.push_back(temp); flag=0; cptr=uptr+1;
	break;
      case ',':
	temp = new char[2];
	strcpy(temp,",");
	Terms.push_back(temp); flag=0; cptr=uptr+1;
	break;
      case '.':
	temp = new char[2];
	strcpy(temp,".");
	Terms.push_back(temp); flag=0; cptr=uptr+1;
	break;
      case ':':
	temp = new char[2];
	strcpy(temp,":");
	Terms.push_back(temp); flag=0; cptr=uptr+1;
	break;
      case ';':
	temp = new char[2];
	strcpy(temp,";");
	Terms.push_back(temp); flag=0; cptr=uptr+1;
	break;
      case '?':
	temp = new char[2];
	strcpy(temp,"?");
	Terms.push_back(temp); flag=0; cptr=uptr+1;
	break;
      case '(':
	temp = new char[2];
	strcpy(temp,"(");
	if(eflag) {
	  Terms.push_back(temp); flag=0; cptr=uptr+1;
	  eeflag=1;
	} 
        break;
      case '<':
	temp = new char[2];
	strcpy(temp,"<");
	Terms.push_back(temp); flag=0; cptr=uptr+1;
        break;
      case '>':
	temp = new char[2];
	strcpy(temp,">");
	Terms.push_back(temp); flag=0; cptr=uptr+1;
        break;
      
      case ')':
	temp = new char[2];
	strcpy(temp,")");
	if(eeflag) { 
	  Terms.push_back(temp); flag=0; cptr=uptr+1;
	}
	else { delete [] temp; }
        break;
	/*case '[':
	temp = new char[2];
	strcpy(temp,"[");
	if(bflag) {
	  Terms.push_back(temp); flag=0; cptr=uptr+1;
	  bbflag=1;
	}
        break;
	case ']':
	temp = new char[2];
	strcpy(temp,"]");
	if(bbflag) {
	  Terms.push_back(temp); flag=0; cptr=uptr+1;
	  }
	  break;*/
      }
  
      if(flag) {
	ptr=uptr;
	// look for end of word
	while(flag) {
	  switch(*ptr) {
	  case '!':
	    flag=0;
	    break;
	  case ',':
	    if(PENN) {
	      if(strlen(uptr)>1) {}
	      else { flag=0;}
	    }
	    else { flag=0; }
	    break;
	  case ')':
	    if(eeflag) {
	      flag=0; 
	      eeflag=0;
	    }
	    break;
	    /*case ']':
	    if(bbflag) {
	      flag=0;
	      bbflag=0;
	      }*/
	    break;
	  case '.':
	    // need to allow e.g. and i.v.
	    if(strlen(uptr)>3) {
	      pptr=ptr;
	      c1 = *(pptr-=1);
	      c2 = *(pptr-=1);
	      c3 = *(pptr-=1);
	      if(c1 == 'g' && c2 == '.' && c3 == 'e' ) {}
	      else if (c1== 'g' && c2 == 'i' && (c3== 'f' || c3 == 'F')) {}
	      else if (c1== 'v' && c2 == '.' && (c3== 'i' || c3 == 'I')) {}
	      if(PENN) {
		if (c1 != ' ') {}
	      }
	      else {
		flag=0;
	      }
	    }
	    else if (PHRASE_MODE) {}
	    else if (PENN && strlen(uptr)>1) {}
	    else {
		flag=0;
	    }
	    break;
	  case ':':
	    if(PENN) {
	      if(strlen(uptr)>1) {}
	      else { flag=0;}
	    }
	    else { flag=0;}
	    break;
	  case ';':
	    if(PENN) {
	      if(strlen(uptr)>1) {}
	      else { flag=0;}
	    }
	    else { flag=0;}
	    break;
	  case '?':
	    flag=0;
	    break;
	  case ' ':
	    flag=0;
	    break;
	  case '\t':
	    flag=0;
	    break;
	  case '*':
	    *ptr='.';
	    break;
	  case '#':
	    *ptr=',';
	    break;
	  case '$':
	    *ptr='\'';
	    break;
	  case '\0':
	    flag=0;
	    break;
	  case (char)1:
	    *ptr='$';
	    break;
	  case (char)2:
	    *ptr='#';
	    break;
	  case (char)3:
	    *ptr='*';
	  }
	  if(flag) { ptr++;  }
	  else {
	    c=*ptr;
	    *ptr='\0';
	     aptr= new char[strlen(uptr)+1];
	    strcpy(aptr,uptr);
	    Terms.push_back(aptr);
	    *ptr=c;
	    cptr=ptr;
	    eflag=bflag=0;
	  }
	}                   // end while flag
      }                     // end if flag
    }                       // end if *uptr ne NULL
    else {                  // get out of while *cptr loop
      cptr=uptr; 
    }
  }                         // end while *cptr 

  if(DEBUG) {
    for(i=0;i<Terms.size();i++) {
      cout << Terms[i] << endl;
    }  
  }
  if(nnstr) delete [] nnstr;
}

void Brill::read_and_tok(ifstream& ifs) {
  char s[XSTR];
  int i;

  while(ifs.getline(s, XSTR,'\n')) {
    read_and_tok(s);
  }
  /*for(i=0;i<Terms.size();i++) {
    cout << Terms[i] << endl;
    }*/
}

void Brill::start_state_tagger(void) {
  int i,j,lr_num,psize;
  long nn,nnp,st;
  char ap[XSTR];
  char tp[XSTR];
  char ppt[XSTR];
  char *aptr,*tptr,*pptr;
  syn_lk* sptr;
  char *staart;
  char *staart1;
  staart = new char[7];
  staart1 = new char[7];
  strcpy(staart, "STAART");
   strcpy(staart1, "STAART");
  // removed to prevent memory leak -- Halil
//   char *pretag;
//   pretag = new char[3];
//   strcpy(pretag, "//");

  // first fill Tags as NN  
  /*for(i=0;i<Terms.size();i++) {
    cptr=Terms[i]; 
    if(strcmp(cptr,staart) != 0) {
      Tags.push_back("NN");
    }
     else {
      Tags.push_back(staart);
    }
  }*/

  nnp = (plex->Btags).find("NNP");
  nn  = (plex->Btags).find("NN");
  st  = (plex->Btags).find(staart);

  // first fill Tags as NN or NNP
  for(i=0;i<Terms.size();i++) {
    // removed to prevent memory leak -- Halil
//     cptr = new char[strlen(Terms[i])+1];
    cptr=Terms[i];
    if(strcmp(cptr,staart) != 0) {
      if (*cptr >= 65 && *cptr <= 90) {
	Tags.push_back(nnp);
      }
      else {
	Tags.push_back(nn);
      }
    }
    else {
      Tags.push_back(st);
    }
  }
  
  // apply rules to Terms, changing Tags if necessary
  
  for(i=0;i<Lrv.size();i++) {
    lr_num = LrvCount.count(LWhen[i]);
    switch(lr_num) {
    case 1:            // char
      for(j=0;j<Terms.size();j++) {
	if(Tags[j] != LNewt[i]) {
	  if(strpbrk(Terms[j], LWord[i]) != NULL) {
	    Tags[j] = LNewt[i];
	    if(DEBUG) {
	      cout << Lrv[i] << endl;
	      cout << Terms[j] << " to " <<  (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
	    }
	  }
	}
      }
      break;
    case 2:            // fchar
      for(j=0;j<Terms.size();j++) {
	if(Tags[j] == LOld[i]) {
	  if(strpbrk(Terms[j], LWord[i]) != NULL) {
	    Tags[j] = LNewt[i];
	    if(DEBUG) {
	      cout << Lrv[i] << endl;
	      cout << Terms[j] << " to " <<  (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
	    }
	  }
	}
      }
      break;
    case 3:            // deletepref
      for(j=0;j<Terms.size();j++) {
        if(Tags[j] != LNewt[i]) {
	  cptr = Terms[j];
	  strcpy(ap,cptr);
	  psize = LLen[i]; 
	  if(strlen(cptr) > psize) {
	    ap[psize] = '\0';
	    if(strcmp(ap, LWord[i]) == 0) {
	      // get the part after removing the prefix
	      cptr += psize; 
	      if((plex->Bterms).find(cptr) || Lw.find(cptr)) {
		Tags[j] = LNewt[i];
		if(DEBUG) {
		  cout << Lrv[i] << endl;
		  cout << Terms[j] << " to " <<  (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      }
	    }
	  }
	}
      }
      break;
    case 4:            // fdeletepref  
      for(j=0;j<Terms.size();j++) {
        if(Tags[j] != LNewt[i]) {
	  if(Tags[j] == LOld[i]) {
	    cptr = Terms[j];
	    strcpy(ap,cptr);
	    psize = LLen[i]; 
	    if(strlen(cptr) > psize) {
	      ap[psize] = '\0';
	      if(strcmp(ap, LWord[i]) == 0) {
		cptr += psize; 
		if((plex->Bterms).find(cptr) || Lw.find(cptr)) {
		  Tags[j] = LNewt[i];
		  if(DEBUG) {
		    cout << Lrv[i] << endl;
		    cout << Terms[j] << " to " <<  (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		  }
		}
	      }
	    }
	  }
	}
      }
      break;
    case 5:            // haspref  
      for(j=0;j<Terms.size();j++) {
	if(Tags[j] != LNewt[i]) {
	   cptr = Terms[j];
	   strcpy(ap,cptr);
	   psize = LLen[i];
	   if(strlen(cptr) > psize) {
	     ap[psize] = '\0';
	     cptr += psize;  
	     if(strcmp(ap, LWord[i]) == 0) {
	       Tags[j] = LNewt[i];
	       if(DEBUG) {
		 cout << Lrv[i] << endl;
		 cout << Terms[j] << " to " <<  (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
	       }
	     }
	   }
	}
      }
      break;
    case 6:            // fhaspref 
      for(j=0;j<Terms.size();j++) {
	if(Tags[j] != LNewt[i]) {
	  if(Tags[j] == LOld[i]) {
	    cptr = Terms[j];
	    strcpy(ap,cptr);
	    psize = LLen[i];
	    if(strlen(cptr) > psize) {
	      ap[psize] = '\0';
	      cptr += psize;  
	      if(strcmp(ap, LWord[i]) == 0) {
		Tags[j] = LNewt[i];
		if(DEBUG) {
		  cout << Lrv[i] << endl;
		  cout << Terms[j] << " to " <<  (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      }
	    }
	  }
	}
      }
      break;
    case 7:            // deletesuf
      for(j=0;j<Terms.size();j++) {
	if(Tags[j] != LNewt[i]) {
	  cptr = Terms[j];
	  strcpy(ap,cptr); strcpy(tp,cptr);
	  pptr=ap;
	  psize = LLen[i];
	  if(strlen(cptr) > psize) {
	    // get the suffix
	    pptr += (strlen(cptr) - psize);    
	    // get the part after removing the suffix
	    tp[(strlen(cptr) - psize)] = '\0';	    
	    if(strcmp(pptr, LWord[i]) == 0) {
	      if((plex->Bterms).find(tp) || Lw.find(tp)) {
		Tags[j] = LNewt[i];
		if(DEBUG) {
		  cout << Lrv[i] << endl;
		  cout << Terms[j] << " to " <<  (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      }
	    }
	  }
	}
      }
      break;
    case 8:            // fdeletesuf 
      for(j=0;j<Terms.size();j++) {
	if(Tags[j] != LNewt[i]) {
	  if(Tags[j] == LOld[i]) {
	    cptr = Terms[j];
	    strcpy(ap,cptr); strcpy(tp,cptr);
	    pptr=ap;
	    psize = LLen[i];
	    if(strlen(cptr) > psize) {
	      pptr += (strlen(cptr) - psize);
	      tp[(strlen(cptr) - psize)] = '\0';	      
	      if(strcmp(pptr, LWord[i]) == 0) {
		if((plex->Bterms).find(tp) || Lw.find(tp)) {
		  Tags[j] = LNewt[i];
		  if(DEBUG) {
		    cout << Lrv[i] << endl;
		    cout << Terms[j] << " to " <<  (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		  }
		}
	      }
	    }
	  }
	}
      }
      break;
    case 9:            // hassuf 
      for(j=0;j<Terms.size();j++) {
	if(Tags[j] != LNewt[i]) {
	  cptr = Terms[j];
	  strcpy(ap,cptr); strcpy(tp,cptr);
	  pptr=ap;
	  psize = LLen[i];
	  if(strlen(cptr) > psize) {
	    pptr += (strlen(cptr) - psize);
	    tp[(strlen(cptr) - psize)] = '\0'; 
	    if(strcmp(pptr, LWord[i]) == 0) {
	      Tags[j] = LNewt[i];
	      if(DEBUG) {
		cout << Lrv[i] << endl;
		cout << Terms[j] << " to " <<  (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
	      }
	    }
	  }
	}
      }
      break;
    case 10:           // fhassuf 
      for(j=0;j<Terms.size();j++) {
	if(Tags[j] != LNewt[i]) {
	  if(Tags[j] == LOld[i]) {
	    cptr = Terms[j];
	    strcpy(ap,cptr); strcpy(tp,cptr);
	    pptr=ap;
	    psize = LLen[i];
	    if(strlen(cptr) > psize) {
	      pptr += (strlen(cptr) - psize);
	      tp[(strlen(cptr) - psize)] = '\0'; 
	      if(strcmp(pptr, LWord[i]) == 0) {
		Tags[j] = LNewt[i];
		if(DEBUG) {
		  cout << Lrv[i] << endl;
		  cout << Terms[j] << " to " <<  (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      }
	    }
	  }
	}
      }
      break;
    case 11:           // addpref 
      for(j=0;j<Terms.size();j++) {
	if(Tags[j] != LNewt[i]) {
	  cptr = Terms[j];
	  psize = LLen[i];
	  strcpy(tp,LWord[i]);
	  strcpy(ppt,tp); 
	  strcat(ppt,cptr); 
	  if((plex->Bterms).find(ppt) || Lw.find(ppt)) {
	    Tags[j] = LNewt[i];
	    if(DEBUG) {
	      cout << Lrv[i] << endl;
	      cout << Terms[j] << " to " <<  (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
	    }
	  }
	}
      }
      break;
    case 12:           // faddpref 
      for(j=0;j<Terms.size();j++) {
	if(Tags[j] != LNewt[i]) {
	  if(Tags[j] == LOld[i]) {
	    cptr = Terms[j];
	    psize = LLen[i];
	    strcpy(tp,LWord[i]);
	    strcpy(ppt,tp); 
	    strcat(ppt,cptr); 
	    if((plex->Bterms).find(ppt) || Lw.find(ppt)) {
	      Tags[j] = LNewt[i];
	      if(DEBUG) {
		cout << Lrv[i] << endl;
		cout << Terms[j] << " to " <<  (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
	      }
	    }
	  }
	}
      }
      break;
    case 13:           // addsuf 
      for(j=0;j<Terms.size();j++) {
	if(Tags[j] != LNewt[i]) {
	  cptr = Terms[j];
	  psize = LLen[i];
	  strcpy(tp,LWord[i]);
	  strcpy(ppt,cptr); 
	  strcat(ppt,tp); 
	  if((plex->Bterms).find(ppt) || Lw.find(ppt)) {
	    Tags[j] = LNewt[i];
	    if(DEBUG) {
	      cout << Lrv[i] << endl;
	      cout << Terms[j] << " to " <<  (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
	    }
	  }
	}
      }
      break;
    case 14:           // faddsuf 
      for(j=0;j<Terms.size();j++) {
	if(Tags[j] != LNewt[i]) {
	  if(Tags[j] == LOld[i]) {
	    cptr = Terms[j];
	    psize = LLen[i];
	    strcpy(tp,LWord[i]);
	    strcpy(ppt,cptr); 
	    strcat(ppt,tp); 
	    if((plex->Bterms).find(ppt) || Lw.find(ppt)) {
	      Tags[j] = LNewt[i];
	      if(DEBUG) {
		cout << Lrv[i] << endl;
		cout << Terms[j] << " to " <<  (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
	      }
	    }
	  }
	}
      }
      break;
    case 15:           // goodleft  
      for(j=0;j<Terms.size();j++) {
	if(Tags[j] != LNewt[i]) {
	  // the pattern - anything in bigrams 
	  // that occurs to the left of this
	  // and is in wordlist will be changed to the new tag
	  if (Lb.exs_pair(LWord[i],Terms[j])) {
	    Tags[j] = LNewt[i];
	    if(DEBUG) {
	      cout << Lrv[i] << endl;
	      cout << Terms[j] << " to " <<  (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
	    }
	  }
	}
      }
      break;
    case 16:           // fgoodleft  
      for(j=0;j<Terms.size();j++) {
	if(Tags[j] != LNewt[i]) {
	  if(Tags[j] == LOld[i]) {
	    if (Lb.exs_pair(LWord[i],Terms[j])) {
	      Tags[j] = LNewt[i];
	      if(DEBUG) {
		cout << Lrv[i] << endl;
		cout << Terms[j] << " to " <<  (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
	      }
	    }
	  }
	}
      }
      break;
    case 17:           // goodright  
      for(j=0;j<Terms.size();j++) {
	if(Tags[j] != LNewt[i]) {
	  // the pattern - anything in bigrams 
	  // that occurs to the left of this
	  // and is in wordlist will be changed to the new tag
	  if (Rb.exs_pair(LWord[i],Terms[j])) {
	    Tags[j] = LNewt[i];
	    if(DEBUG) {
	      cout << Lrv[i] << endl;
	      cout << Terms[j] << " to " <<  (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
	    }
	  }
	}
      }
      break;
    case 18:           // fgoodright
      for(j=0;j<Terms.size();j++) {
	if(Tags[j] != LNewt[i]) {
	  if(Tags[j] == LOld[i]) {
	    if (Rb.exs_pair(LWord[i],Terms[j])) {
	      Tags[j] = LNewt[i];
	      if(DEBUG) {
		cout << Lrv[i] << endl;
		cout << Terms[j] << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
	      }
	    }
	  }
	}
      }
      break;
    default:
      break;
    }
  }
 
  /* Now for each term, assign it a tag from Lex if it's there,
     or the tag from Tags if it isn't */

  vector<char*>::iterator p = Terms.begin();
  vector<long>::iterator pp = Tags.begin();

  while(p != Terms.end()) {
    while (pp != Tags.end()) {
      // if a word is pretagged, use the pretagging indicated by '//'
       cptr = *p;
      /*if(strstr(cptr, pretag)) {
	tptr = strstr(cptr, pretag);
	*pp = (plex->Btags).find(tptr+=2);
	pptr = cptr;
	aptr = new char[MAXTAGLEN];
	pptr = strstr(cptr, pretag);
	*pptr = NULL; 
	strcpy(aptr,cptr);
	*p = aptr;
	if(DEBUG) {cout << *p << " ";} 
	}*/
      if(i=(plex->Bterms).find(*p)) {
	*pp = plex->ptag[i];
	if(DEBUG) {cout << *p << "/" << (plex->Btags).strmap+(plex->Btags).addr[*pp-1] << " ";}
      }

      // if a word is unknown, use the tags found from applying lex rules 
      else {
	if(DEBUG) {cout << *p << "/" << (plex->Btags).strmap+(plex->Btags).addr[*pp-1] << " ";}
      }
      if(DEBUG) {cout << endl;}
      p++;pp++;
    }
  }
  
  Terms.push_back(staart);
  Tags.push_back(st);
  Terms.push_back(staart1);
  Tags.push_back(st);

}

void Brill::final_state_tagger() {
  int  i,j,inlex,cr_num,tempcount,tempcount1;
  char *curwd,*curtag;
  char *newtag; 
  long term_num;
  syn_lk* sptr;
  inlex=0;
//   char *staart;
//   staart = new char[7];
//   strcpy(staart,"STAART");
//   ofstream fout;

  // apply cr rules to input text
  for(i=0;i<Crv.size();i++) {
    cr_num = CrvCount.count(When[i]);
    for (j=0;j<Terms.size();j++) {
      term_num = 0;
      if (Tags[j] == Old[i]) {
	term_num = (plex->Bterms).find(Terms[j]);
	plex->set_wrd(term_num);
	if (plex->exs_tag(Newt[i])) { 
	  inlex = 1; 
	} 
	if (!RESTRICT_MOVE || inlex || !term_num) {
	  inlex=0;
	  switch(cr_num) {
	  case 1:              // NEXTTAG
	    if (Tag[i] == Tags[j+1] && j < Terms.size()) {  
		Tags[j] = Newt[i];
		if(DEBUG) {
		  cout << Crv[i] << endl;
		  cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		       << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	    }
	    break;
	  case 2:              // NEXT2TAG
	    if (Tag[i] == Tags[j+2] && 
		j < (Terms.size()-1) ) {
	      Tags[j] = Newt[i];
		if(DEBUG) {
		  cout << Crv[i] << endl;
		  cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		       << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	    }
	    break;  
	  case 3:              // NEXT1OR2TAG
	    if(j < Terms.size()) {
	      if(j < Terms.size()-1) 
		tempcount = j+2;
	      else
		tempcount = j+1;
	      if (Tag[i] == Tags[j+1] ||
		  Tag[i] == Tags[tempcount]) {
		Tags[j] = Newt[i];
		if(DEBUG) {
		  cout << Crv[i] << endl;
		  cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		       << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      }
	    }
	    break;
	  case 4:              // NEXT1OR2OR3TAG
	    if (j < Terms.size()) {
	      if (j < Terms.size()-1)
		tempcount = j+2;
	      else 
		tempcount = j+1;
	      if (j < Terms.size()-2)
		tempcount1 = j+3;
	      else 
		tempcount1 =j+1;
	      if (Tag[i] == Tags[j+1] ||
		  Tag[i] == Tags[tempcount] ||
		  Tag[i] == Tags[tempcount1]) {
		Tags[j] = Newt[i];
		if(DEBUG) {
		  cout << Crv[i] << endl;
		  cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		       << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      }
	    }
	    break;
	  case 5:              // PREVTAG
	    if (Tag[i] == Tags[j-1]
		&& j > 0) {
	      Tags[j] = Newt[i];
	      if(DEBUG) {
		cout << Crv[i] << endl;
		cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		     << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
	      }
	    }
	    break;
	  case 6:              // PREV2TAG 
	    if (Tag[i] == Tags[j-2]
		&& j > 1) {
	      Tags[j] = Newt[i];
	      if(DEBUG) {
		cout << Crv[i] << endl;
		cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		     << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
	      }
	    }
	    break;
	  case 7:              // PREV1OR2TAG
	    if (j > 0) {
	      if (j > 1) 
		tempcount = j-2;
	      else
		tempcount = j-1;
	      if (Tag[i] == Tags[j-1] ||
		  Tag[i] == Tags[tempcount]) {
		Tags[j] = Newt[i];
		if(DEBUG) {
		  cout << Crv[i] << endl;
		  cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		       << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      }
	    }
	    break;
	  case 8:              // PREV1OR2OR3TAG
	    if (j > 0) {
	      if (j > 1) 
		tempcount = j-2;
	      else 
		tempcount = j-1;
	      if (j > 2) 
		tempcount1 = j-3;
	      else 
		tempcount1 = j-1;
	      if (Tag[i] == Tags[j-1] ||
		  Tag[i] == Tags[tempcount] ||
		  Tag[i] == Tags[tempcount1]) {
		Tags[j] = Newt[i];
		if(DEBUG) {
		  cout << Crv[i] << endl;
		  cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		       << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      }
	    }
	    break;
	  case 9:              // NEXTWD
	    if(strcmp(TheWord[i], Terms[j+1]) == 0 && j < Terms.size()) {
	      Tags[j] = Newt[i];
	      if(DEBUG) {
		cout << Crv[i] << endl;
		cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		     << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
	      }
	    }
	    break;
	  case 10:             // CURWD
	    if (strcmp(TheWord[i], Terms[j]) == 0) {
	       Tags[j] = Newt[i];
		 if(DEBUG) {
		   cout << Crv[i] << endl;
		   cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
			<< " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		 }
	    }
	    break;
	  case 11:             // NEXT2WD
	    if (strcmp(TheWord[i], Terms[j+2]) == 0
		&& j < (Terms.size()-1)) {
	      Tags[j] = Newt[i];
		 if(DEBUG) {
		   cout << Crv[i] << endl;
		   cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
			<< " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		 }
	    }      
	    break;
	  case 12:             // NEXT1OR2WD
	    if(j < Terms.size()) {
	      if(j < Terms.size()-1) 
		tempcount = j+2;
	      else
		tempcount = j+1;
	      if (strcmp(TheWord[i], Terms[j+1]) == 0 ||
		  strcmp(TheWord[i], Terms[tempcount]) == 0) {
		Tags[j] = Newt[i];
		 if(DEBUG) {
		   cout << Crv[i] << endl;
		   cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
			<< " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		 }
	      }
	    }
	    break;
	  case 13:             // NEXT1OR2OR3WD
	    if (j < Terms.size()) {
	      if (j < Terms.size()-1)
		tempcount = j+2;
	      else 
		tempcount = j+1;
	      if (j < Terms.size()-2)
		tempcount1 = j+3;
	      else 
		tempcount1 =j+1;
	      if (strcmp(TheWord[i], Terms[j+1]) == 0 ||
		  strcmp(TheWord[i], Terms[tempcount]) == 0 ||
		  strcmp(TheWord[i], Terms[tempcount1]) == 0) {
		Tags[j] = Newt[i];
		 if(DEBUG) {
		   cout << Crv[i] << endl;
		   cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
			<< " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		 }
	      }
	    }
	    break;
	  case 14:             // PREVWD
	    if (strcmp(TheWord[i], Terms[j-1]) == 0
		&& j > 0) {
	      Tags[j] = Newt[i];
	      if(DEBUG) {
		cout << Crv[i] << endl;
		cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		     << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
	      }
	    }
	    break;
	  case 15:             // PREV2WD
	    if (strcmp(TheWord[i], Terms[j-2]) == 0
		&& j > 1) {
	      Tags[j] = Newt[i];
	      if(DEBUG) {
		cout << Crv[i] << endl;
		cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		     << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
	      }
	    }
	    break;
	  case 16:             // PREV1OR2WD
	    if (j > 0) {
	      if (j > 1) 
		tempcount = j-2;
	      else
		tempcount = j-1;
	      if (strcmp(TheWord[i], Terms[j-1]) == 0 ||
		  strcmp(TheWord[i], Terms[tempcount]) == 0) {
		Tags[j] = Newt[i];
		if(DEBUG) {
		  cout << Crv[i] << endl;
		  cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		       << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      }
	    }
	    break;
	  case 17:             // PREV1OR2OR3WD
	    if (j > 0) {
	      if (j > 1) 
		tempcount = j-2;
	      else 
		tempcount = j-1;
	      if (j > 2) 
		tempcount1 = j-3;
	      else 
		tempcount1 = j-1;
	      if (strcmp(TheWord[i], Terms[j-1]) == 0 ||
		  strcmp(TheWord[i], Terms[tempcount]) == 0 ||
		  strcmp(TheWord[i], Terms[tempcount1]) == 0) {
		Tags[j] = Newt[i];
		if(DEBUG) {
		  cout << Crv[i] << endl;
		  cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		       << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      }
	    }
	    break;
	  case 18:             // SURROUNDTAG
	    if(j > 0 && j < Terms.size()) {
	       if (Lft[i] == Tags[j-1] &&
		   Rght[i]== Tags[j+1]) {
		 Tags[j] = Newt[i];
		 if(DEBUG) {
		   cout << Crv[i] << endl;
		   cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
			<< " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		 }
	       }
	    }
	    break;
	  case 19:             // PREVBIGRAM
	    if (Prev2t[i] == Tags[j-1]) {
	      if (Prev1t[i] == Tags[j-2]
		&& j > 1) {
		Tags[j] = Newt[i];
		if(DEBUG) {
		  cout << Crv[i] << endl;
		  cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		       << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      }
	    }
	    break;
	  case 20:             // NEXTBIGRAM
	    if (Next1t[i] == Tags[j+1]) {
	      if (Next2t[i] == Tags[j+2] && j < (Terms.size()-1)) {
		Tags[j] = Newt[i];
		if(DEBUG) {
		  cout << Crv[i] << endl;
		  cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		       << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      }
	    }
	    break;
	  case 21:             // LBIGRAM
	    if (strcmp(TheWord[i], Terms[j]) == 0) {
	      if (strcmp(Prev1[i], Terms[j-1]) == 0
		&& j > 0) {
		Tags[j] = Newt[i];
		if(DEBUG) {
		  cout << Crv[i] << endl;
		  cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		       << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      }
	    }
	    break;
	  case 22:             // WDPREVTAG
	    if (strcmp(TheWord[i], Terms[j]) == 0) {
	      if (Prev1t[i] == Tags[j-1]
		  && j > 0) {
		Tags[j] = Newt[i];
		if(DEBUG) {
		  cout << Crv[i] << endl;
		  cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		       << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      }
	    }
	    break;
	  case 23:             // RBIGRAM
	     if (strcmp(TheWord[i], Terms[j]) == 0) {
	       if (strcmp(Next1[i], Terms[j+1]) == 0 &&
		   j < Terms.size()) {
		 Tags[j] = Newt[i];
		 if(DEBUG) {
		   cout << Crv[i] << endl;       
		   cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
			<< " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		 }
	       } 
	     }
	    break;
	  case 24:             // WDNEXTTAG
	    if (strcmp(TheWord[i], Terms[j]) == 0) {
	      if (Next1t[i] ==  Tags[j+1] && 
		  j < Terms.size()) {
		Tags[j] = Newt[i];
		if(DEBUG) {
		  cout << Crv[i] << endl;
		  cout << Terms[j] << " ********** " << Terms[j+1] << endl;
		  cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		       << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      } 
	    }
	    break;
	  case 25:             // WDAND2BFR
	    if (strcmp(TheWord[i], Terms[j]) == 0) {
	      if (strcmp(Prev2[i], Terms[j-2]) == 0
		  && j > 1) {
		Tags[j] = Newt[i];
		if(DEBUG) {
		  cout << Crv[i] << endl;
		  cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		       << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      } 
	    }
	    break;
	  case 26:             // WDAND2TAGBFR
	    if (strcmp(TheWord[i], Terms[j]) == 0) {
	      if (Prev2t[i] == Tags[j-2]
		  && j > 1) {
		Tags[j] = Newt[i];
		if(DEBUG) {
		  cout << Crv[i] << endl;
		  cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		       << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      }  
	    }
	    break;
	  case 27:             // WDAND2AFT
	    if (strcmp(TheWord[i], Terms[j]) == 0) {
	      if (strcmp(Next2[i], Terms[j+2]) == 0 && 
		  j < (Terms.size()-1)) {
		Tags[j] = Newt[i];
		if(DEBUG) {
		  cout << Crv[i] << endl;
		  cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		       << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      } 
	    }
	    break;
	  case 28:             // WDAND2TAGAFT
	    if (strcmp(TheWord[i], Terms[j]) == 0) {
	      if (Next2t[i] == Tags[j+2] 
		  && j < (Terms.size()-1)) {
	        Tags[j] = Newt[i];
		if(DEBUG) {
		  cout << Crv[i] << endl;
		  cout << TheWord[i] << " ********** " << 
		    Terms[j] << " ********** " << Terms[j+1] << endl;
		  cout << Terms[j] << " " << (plex->Btags).strmap+(plex->Btags).addr[Old[i]-1] 
		       << " to " << (plex->Btags).strmap+(plex->Btags).addr[Tags[j]-1] << endl;
		}
	      } 
	    }
	    break;
	  default:
	    cout << "ERROR CR RULES FINAL STATE TAGGER\n";
	    break;
	  }
	} // end if restrict_move
      }   // end if strcmp Old
    }     // end for j Terms
  }       // end for i Crv	    
	   



  // print results to screen, moved to get_results()  -- Halil
  //  if(Terms.size() == Tags.size()) {
  //    vector<char*>::iterator p = Terms.begin();
  //    vector<long>::iterator pp = Tags.begin();

      // remove STAART from front and end
  //    Tags.pop_back();
  //    Terms.pop_back();
  //    Tags.pop_back();
  //    Terms.pop_back();
  //    Tags.erase(pp);
  //    Terms.erase(p);

      // COMMENT OUT TO SYNC WITH ORIGINAL BRILL TAGGER

  //    if(!PHRASE_MODE) {
  //	if(**p >= 97 && **p <= 122) {
  //	  (**p)-=32;
  //	}
  //    }
  //    while(p != Terms.end()) {
  //	while (pp != Tags.end()) {
  //	  if(strcmp(*p,staart) != 0) {
  //	    if(cptr=(plex->Btags).strmap+(plex->Btags).addr[*pp-1]) {
  //	      cout << *p << "/" 
  //		   << cptr << " ";
  //	    }
  //	    else {
  //	      cout << *p << "/" 
  //		   << "NN ";
  //	    }
  //	  }
  //	  else {
  //	    p++;
  //	    if(strcmp(*p,staart) == 0) {
  //	      cout << endl;
  //	    }
  //	    p--;
  //	  }
  //	  p++;pp++;
  //	}
  //      }
  //  }
  //  cout << endl;
  //  free_vec_str(Terms);
  //  Tags.clear();
  
  //fout.close();
}

void Brill::free_vec_str(vector<char*> &vc) {
  vector<char*>::iterator p = vc.begin();
  while(p!=vc.end()){
    if(*p)delete [] *p;
    p++;
  }
  vc.clear();
}

// Added by Halil
string Brill::get_results() {
  string result_str;
  char *xptr[3];
  char staart[] = "STAART";

  if(Terms.size() == Tags.size()) {
      vector<char*>::iterator p = Terms.begin();
      vector<long>::iterator pp = Tags.begin();
      vector<char*>::iterator pr = Terms.end();

      // remove STAART from front and end
      Tags.pop_back();
      // added tp prevent memory leak -- Halil
      pr--; xptr[0] = *pr;
      Terms.pop_back();  
      Tags.pop_back();
      // added tp prevent memory leak -- Halil
      pr--; xptr[1] = *pr;
      Terms.pop_back();  
      Tags.erase(pp);
      // added tp prevent memory leak -- Halil
      xptr[2] = *p;
      Terms.erase(p);  

      // COMMENT OUT TO SYNC WITH ORIGINAL BRILL TAGGER

      if(!PHRASE_MODE) {
	if(**p >= 97 && **p <= 122) {
	  (**p)-=32;
	}
      }
      while(p != Terms.end()) {
	while (pp != Tags.end()) {
	  if(strcmp(*p,staart) != 0) {
	    if(cptr=(plex->Btags).strmap+(plex->Btags).addr[*pp-1]) {
	      result_str += string(*p) + "/" + string(cptr) + " ";	      		
	    }
	    else {
	      result_str += string(*p) + "/NN ";
	    }
	  }
	  else {
	    p++;
	    if(strcmp(*p,staart) == 0) {
		result_str += "\n";
	    }
	    p--;
	  }
	  p++;pp++;
	}
      }
  }
  // added to prevent memory leak -- Halil
  if (xptr[0]) delete [] xptr[0];
  if (xptr[1]) delete [] xptr[1];
  if (xptr[2]) delete [] xptr[2];

  free_vec_str(Terms);
  Tags.clear();

  return result_str;
}

// added to prevent memory leak -- Halil
void Brill::close_tagger(void){

   Lw.gclose_htable_map();
   Rb.gclose_bmatrix();
   Lb.gclose_bmatrix();
   plex->gclose_bmatrix();
}

}




 

