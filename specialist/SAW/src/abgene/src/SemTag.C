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
#include <SemTag.h>
#include <Word.h>
#include <cstring>
#include <algorithm>
#include <vector>
#include <Hash.h>


#define PENN 0               // assumes input text tokenized 
#define DEBUG 1
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
  
SemTag::SemTag(void){
  Wrd = new Word;
  Wrd->byte_lim=0;
  Wrd->back_lim=0;
  Wrd->all_print_map();
  Wrd->stop=0;
  Wrd->set_map(".,:;!?", '\024',1);
  Wrd->pre_punct(); 
}

SemTag::SemTag(const char *nam){
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

SemTag::~SemTag(){  
  if(name!=NULL)delete [] name;
  if(Wrd) delete Wrd;
}

void SemTag::change_name(char *nam){
  if(name!=NULL)delete [] name;
  int lxn=strlen(nam);
  name=new char[lxn+1];
  strcpy(name,nam);
}

void SemTag::open_tagger(void){
  ifstream fin;
  int pflag=get_qflag(),flag=1;
  char cnam[max_str];
  char *nothing;
  char *cptr, *ptr, *aptr, *pptr, c, **trm;
  long cr_size,lr_size;
  int  i,num;
  LexPrb* lexptr;

  Stop.change_name("stop");
  Stop.gopen_htable_map();

  lexptr = new LexPrb(3,"aap"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"maap");
  MLPv.push_back(lexptr); 
  lexptr = new LexPrb(3,"biologicfunctionp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mbiologicfunctionp");
  MLPv.push_back(lexptr);  
  lexptr = new LexPrb(3,"bodylocationp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mbodylocationp");
  MLPv.push_back(lexptr); 
  lexptr = new LexPrb(3,"bodypartp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mbodypartp");
  MLPv.push_back(lexptr); 
  lexptr = new LexPrb(3,"carbohydratep"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mcarbohydratep");
  MLPv.push_back(lexptr);   
  lexptr = new LexPrb(3,"cellcomponentp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mcellcomponentp");
  MLPv.push_back(lexptr);  
  lexptr = new LexPrb(3,"cellfunctionp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mcellfunctionp");
  MLPv.push_back(lexptr);   
  lexptr = new LexPrb(3,"cellmoldysfunctionp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mcellmoldysfunctionp");
  MLPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"cellp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mcellp");
  MLPv.push_back(lexptr); 
  lexptr = new LexPrb(3,"chemicalp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mchemicalp");
  MLPv.push_back(lexptr);  
  lexptr = new LexPrb(3,"chromosomalregionp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mchromosomalregionp");
  MLPv.push_back(lexptr);  
  lexptr = new LexPrb(3,"clinicaldrugp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mclinicaldrugp");
  MLPv.push_back(lexptr);  
  lexptr = new LexPrb(3,"diagnosticp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mdiagnosticp");
  MLPv.push_back(lexptr);  
  lexptr = new LexPrb(3,"diseasep"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mdiseasep");
  MLPv.push_back(lexptr);  
  lexptr = new LexPrb(3,"dnadomainp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mdnadomainp");
  MLPv.push_back(lexptr);  
  lexptr = new LexPrb(3,"dnamoleculep"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mdnamoleculep");
  MLPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"findingp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mfindingp");
  MLPv.push_back(lexptr);      
  lexptr = new LexPrb(3,"foodp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mfoodp");
  MLPv.push_back(lexptr);
  lexptr = new LexPrb(3,"geographicp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mgeographicp");
  MLPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"healthcarep"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mhealthcarep");
  MLPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"hormonep"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mhormonep");
  MLPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"humancausedp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mhumancausedp");
  MLPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"injuryp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"minjuryp");
  MLPv.push_back(lexptr);   
  lexptr = new LexPrb(3,"inorganicp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"minorganicp");
  MLPv.push_back(lexptr);      
  lexptr = new LexPrb(3,"intellectualp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mintellectualp");
  MLPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"journalp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mjournalp");
  MLPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"labprocedurep"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mlabprocedurep");
  MLPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"labtestp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mlabtestp");
  MLPv.push_back(lexptr);     
  lexptr = new LexPrb(3,"lipidp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mlipidp");
  MLPv.push_back(lexptr);     
  lexptr = new LexPrb(3,"manufacturedobjectp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mmanufacturedobjectp");
  MLPv.push_back(lexptr);     
  lexptr = new LexPrb(3,"medicaldevicep"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mmedicaldevicep");
  MLPv.push_back(lexptr);     
  lexptr = new LexPrb(3,"mentalprocessp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mmentalprocessp");
  MLPv.push_back(lexptr);     
  lexptr = new LexPrb(3,"molecularfunctionp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mmolecularfunctionp");
  MLPv.push_back(lexptr);     
  lexptr = new LexPrb(3,"naturalprocessp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mnaturalprocessp");
  MLPv.push_back(lexptr);      
  lexptr = new LexPrb(3,"neoplasticprocessp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mneoplasticprocessp");
  MLPv.push_back(lexptr);      
  lexptr = new LexPrb(3,"occupationalgroupp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"moccupationalgroupp");
  MLPv.push_back(lexptr);      
  lexptr = new LexPrb(3,"organicp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"morganicp");
  MLPv.push_back(lexptr);      
  lexptr = new LexPrb(3,"organismp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"morganismp");
  MLPv.push_back(lexptr);      
  lexptr = new LexPrb(3,"pathologicfunctionp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mpathologicfunctionp");
  MLPv.push_back(lexptr);      
  lexptr = new LexPrb(3,"patientgroupp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mpatientgroupp");
  MLPv.push_back(lexptr);       
  lexptr = new LexPrb(3,"personp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mpersonp");
  MLPv.push_back(lexptr);         
  lexptr = new LexPrb(3,"professionalgroupp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mprofessionalgroupp");
  MLPv.push_back(lexptr);      
  lexptr = new LexPrb(3,"proteincomplexp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mproteincomplexp");
  MLPv.push_back(lexptr);        
  lexptr = new LexPrb(3,"proteinfamilyp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mproteinfamilyp");
  MLPv.push_back(lexptr);       
  lexptr = new LexPrb(3,"proteinmoleculep"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mproteinmoleculep");
  MLPv.push_back(lexptr);        
  lexptr = new LexPrb(3,"proteinsubunitp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mproteinsubunitp");
  MLPv.push_back(lexptr);           
  lexptr = new LexPrb(3,"quantitativep"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mquantitativep");
  MLPv.push_back(lexptr);         
  lexptr = new LexPrb(3,"researchactivityp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mresearchactivityp");
  MLPv.push_back(lexptr);          
  lexptr = new LexPrb(3,"researchdevicep"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mresearchdevicep");
  MLPv.push_back(lexptr);          
  lexptr = new LexPrb(3,"researchtechniquep"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mresearchtechniquep");
  MLPv.push_back(lexptr);
  lexptr = new LexPrb(3,"signp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"msignp");
  MLPv.push_back(lexptr);             
  lexptr = new LexPrb(3,"steroidp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"msteroidp");
  MLPv.push_back(lexptr);               
  lexptr = new LexPrb(3,"temporalp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mtemporalp");
  MLPv.push_back(lexptr);
  lexptr = new LexPrb(3,"therapeuticp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mtherapeuticp");
  MLPv.push_back(lexptr);  
  lexptr = new LexPrb(3,"commonp"); 
  LPv.push_back(lexptr);    
  lexptr = new LexPrb(3,"mcommonp");
  MLPv.push_back(lexptr);  
}

char* SemTag::get_str(void) {
  return tagstr; cout << tagstr << endl;
}

void SemTag::read_and_tok(char* str) {
  tagstr = str;
  //ifstream fin;
  //ofstream fout;
  //char cnam [XSTR];
  char* nnstr = new char[XSTR];
  const char* nstr;
  char *cptr, *ptr, *uptr, *aptr, *pptr, *temp;
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

  s = (string) str;
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

void SemTag::read_and_tok(ifstream& ifs) {
  char s[XSTR];
  int i;

  cout << "OK" << endl;
  while(ifs.getline(s, XSTR,'\n')) {
    cout << "OK" << endl;
    cout << s << endl;
    read_and_tok(s);
  }
  /*for(i=0;i<Terms.size();i++) {
    cout << Terms[i] << endl;
    }*/
}

void SemTag::tagger(void) {
  int i,j,k;
  char *temp,*pch,*tag,*tag1,*tag2,*tag3;
  float xx,yy,zz,max1,max2,max3;
  LexPrb* lexptr;
  LexPrb* mlexptr;
  char lcw[max_str];
 

  // assign the most probable tag
  for(i=0;i<Terms.size();i++) {
    max1=max2=max3=0;
    tag  = new char[150];
    tag1 = new char[50];
    tag2 = new char[50];
    tag3 = new char[50];    
    strcpy(tag,"NO_CAT");    
    strcpy(tag1,"NO_CAT");    
    strcpy(tag2,"NO_CAT");    
    strcpy(tag3,"NO_CAT");
    pch=Terms[i];	
    strcpy(lcw,pch);	
    for(k=0;lcw[k] != '\0';k++) {
      // get lower case current word = lcw
      if(lcw[k] >= 65 && lcw[k] <= 90) { 
	(lcw[k])+=32;
      }
    }
    if(strspn(pch,"bcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") < 1) {
 
    }
    else if (Stop.find(lcw)) {
      strcpy(tag,"STOP");
    }
    else {
      lexptr = LPv[0];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[0];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);
      if(xx)zz=1.0/(1.0+(yy/xx)*608.60); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz; 
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"AA_MONOMER");
	//cout << zz << " " << pch << "|" << "AA_MONOMER" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz; 
	strcpy(tag3,tag2); strcpy(tag2,"AA_MONOMER");
	//cout << zz << " " << pch << "|" << "AA_MONOMER" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"AA_MONOMER");
	//cout << zz << " " << pch << "|" << "AA_MONOMER" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();

      lexptr = LPv[1];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[1];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*95.95); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"BIOLOGIC_FUNCTION");
	//cout << zz << " " << pch << "|" << "BIOLOGIC_FUNCTION" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"BIOLOGIC_FUNCTION");
	//cout << zz << " " << pch << "|" << "BIOLOGIC_FUNCTION" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"BIOLOGIC_FUNCTION");
	//cout << zz << " " << pch << "|" << "BIOLOGIC_FUNCTION" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  

      lexptr = LPv[2];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[2];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*30.17); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"BODY_LOCATION");
	//cout << zz << " " << pch << "|" << "BODY_LOCATION" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"BODY_LOCATION");
	//cout << zz << " " << pch << "|" << "BODY_LOCATION" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"BODY_LOCATION");
	//cout << zz << " " << pch << "|" << "BODY_LOCATION" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  

      lexptr = LPv[3];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[3];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*4.94); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"BODY_PART");
	//cout << zz << " " << pch << "|" << "BODY_PART" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"BODY_PART");
	//cout << zz << " " << pch << "|" << "BODY_PART" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"BODY_PART");
	//cout << zz << " " << pch << "|" << "BODY_PART" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  

      lexptr = LPv[4];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[4];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*24.79); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"CARBOHYDRATE");
	//cout << zz << " " << pch << "|" << "CARBOHYDRATE" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"CARBOHYDRATE");
	//cout << zz << " " << pch << "|" << "CARBOHYDRATE" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"CARBOHYDRATE");
	//cout << zz << " " << pch << "|" << "CARBOHYDRATE" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();   

      lexptr = LPv[5];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[5];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*168.53); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"CELL COMPONENT");
	//cout << zz << " " << pch << "|" << "CELL COMPONENT" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"CELL COMPONENT");
	//cout << zz << " " << pch << "|" << "CELL COMPONENT" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"CELL COMPONENT");
	//cout << zz << " " << pch << "|" << "CELL COMPONENT" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  

      lexptr = LPv[6];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[6];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*137.14); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"CELL FUNCTION");
	//cout << zz << " " << pch << "|" << "CELL FUNCTION" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"CELL FUNCTION");
	//cout << zz << " " << pch << "|" << "CELL FUNCTION" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"CELL FUNCTION");
	//cout << zz << " " << pch << "|" << "CELL FUNCTION" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  

      lexptr = LPv[7];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[7];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*519.41); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"CELL_OR_MOLECULAR_DYSFUNCTION");
	//cout << zz << " " << pch << "|" << "CELL_OR_MOLECULAR_DYSFUNCTION" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"CELL_OR_MOLECULAR_DYSFUNCTION");
	//cout << zz << " " << pch << "|" << "CELL_OR_MOLECULAR_DYSFUNCTION" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"CELL_OR_MOLECULAR_DYSFUNCTION");
	//cout << zz << " " << pch << "|" << "CELL_OR_MOLECULAR_DYSFUNCTION" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  

      lexptr = LPv[8];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[8];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*115.27); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"CELL");
	//cout << zz << " " << pch << "|" << "CELL" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"CELL");
	//cout << zz << " " << pch << "|" << "CELL" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"CELL");
	//cout << zz << " " << pch << "|" << "CELL" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb(); 

      /*lexptr = LPv[9];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[9];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*1.54); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"CHEMICAL");
	//cout << zz << " " << pch << "|" << "CHEMICAL" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"CHEMICAL");
	//cout << zz << " " << pch << "|" << "CHEMICAL" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"CHEMICAL");
	//cout << zz << " " << pch << "|" << "CHEMICAL" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();*/   

      lexptr = LPv[10];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[10];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*1215.73); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"CHROMOSOMAL_REGION");
	//cout << zz << " " << pch << "|" << "CHROMOSOMAL_REGION" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"CHROMOSOMAL_REGION");
	//cout << zz << " " << pch << "|" << "CHROMOSOMAL_REGION" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"CHROMOSOMAL_REGION");
	//cout << zz << " " << pch << "|" << "CHROMOSOMAL_REGION" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  

      lexptr = LPv[11];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[11];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*2.31); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"CLINICAL_DRUG");
	//cout << zz << " " << pch << "|" << "CLINICAL_DRUG" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"CLINICAL_DRUG");
	//cout << zz << " " << pch << "|" << "CLINICAL_DRUG" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"CLINICAL_DRUG");
	//cout << zz << " " << pch << "|" << "CLINICAL_DRUG" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();   

      lexptr = LPv[12];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[12];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*16.45); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"DIAGNOSTIC_PROCEDURE");
	//cout << zz << " " << pch << "|" << "DIAGNOSTIC_PROCEDURE" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"DIAGNOSTIC_PROCEDURE");
	//cout << zz << " " << pch << "|" << "DIAGNOSTIC_PROCEDURE" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"DIAGNOSTIC_PROCEDURE");
	//cout << zz << " " << pch << "|" << "DIAGNOSTIC_PROCEDURE" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  

      lexptr = LPv[13];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[13];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*3.16); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"DISEASE");
	//cout << zz << " " << pch << "|" << "DISEASE" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"DISEASE");
	//cout << zz << " " << pch << "|" << "DISEASE" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"DISEASE");
	//cout << zz << " " << pch << "|" << "DISEASE" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  

      lexptr = LPv[14];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[14];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*193.67); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"DNA_DOMAIN");
	//cout << zz << " " << pch << "|" << "DNA_DOMAIN" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"DNA_DOMAIN");
	//cout << zz << " " << pch << "|" << "DNA_DOMAIN" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"DNA_DOMAIN");
	//cout << zz << " " << pch << "|" << "DNA_DOMAIN" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  

      lexptr = LPv[15];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[15];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*1.66); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"DNA_MOLECULE");
	//cout << zz << " " << pch << "|" << "DNA_MOLECULE" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"DNA_MOLECULE");
	//cout << zz << " " << pch << "|" << "DNA_MOLECULE" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"DNA_MOLECULE");
	//cout << zz << " " << pch << "|" << "DNA_MOLECULE" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();   

      lexptr = LPv[16];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[16];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*7.43); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"FINDING");
	//cout << zz << " " << pch << "|" << "FINDING" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"FINDING");
	//cout << zz << " " << pch << "|" << "FINDING" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"FINDING");
	//cout << zz << " " << pch << "|" << "FINDING" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  

      lexptr = LPv[17];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[17];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*130.72); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"FOOD");
	//cout << zz << " " << pch << "|" << "FOOD" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"FOOD");
	//cout << zz << " " << pch << "|" << "FOOD" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"FOOD");
	//cout << zz << " " << pch << "|" << "FOOD" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  

      lexptr = LPv[18];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[18];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*510.07); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"GEOGRAPHIC_AREA");
	//cout << zz << " " << pch << "|" << "GEOGRAPHIC_AREA" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"GEOGRAPHIC_AREA");
	//cout << zz << " " << pch << "|" << "GEOGRAPHIC_AREA" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"GEOGRAPHIC_AREA");
	//cout << zz << " " << pch << "|" << "GEOGRAPHIC_AREA" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();   

      lexptr = LPv[19];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[19];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*19.01); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"HEALTHCARE_ACTIVITY");
	//cout << zz << " " << pch << "|" << "HEALTHCARE_ACTIVITY" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"HEALTHCARE_ACTIVITY");
	//cout << zz << " " << pch << "|" << "HEALTHCARE_ACTIVITY" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"HEALTHCARE_ACTIVITY");
	//cout << zz << " " << pch << "|" << "HEALTHCARE_ACTIVITY" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb(); 

      lexptr = LPv[20];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[20];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*105.72); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"HORMONE");
	//cout << zz << " " << pch << "|" << "HORMONE" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"HORMONE");
	//cout << zz << " " << pch << "|" << "HORMONE" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"HORMONE");
	//cout << zz << " " << pch << "|" << "HORMONE" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb(); 
      
      lexptr = LPv[21];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[21];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*184.73); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"HUMAN_CAUSED_PHENOMENON");
	//cout << zz << " " << pch << "|" << "HUMAN_CAUSED_PHENOMENON" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"HUMAN_CAUSED_PHENOMENON");
	//cout << zz << " " << pch << "|" << "HUMAN_CAUSED_PHENOMENON" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"HUMAN_CAUSED_PHENOMENON");
	//cout << zz << " " << pch << "|" << "HUMAN_CAUSED_PHENOMENON" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();       
      lexptr = LPv[22];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[22];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*4.16); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"INJURY");
	//cout << zz << " " << pch << "|" << "INJURY" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"INJURY");
	//cout << zz << " " << pch << "|" << "INJURY" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"INJURY");
	//cout << zz << " " << pch << "|" << "INJURY" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  
     
      lexptr = LPv[23];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[22];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*62.23); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"INORGANIC_CHEMICAL");
	//cout << zz << " " << pch << "|" << "INORGANIC_CHEMICAL" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"INORGANIC_CHEMICAL");
	//cout << zz << " " << pch << "|" << "INORGANIC_CHEMICAL" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"INORGANIC_CHEMICAL");
	//cout << zz << " " << pch << "|" << "INORGANIC_CHEMICAL" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb(); 
      
      lexptr = LPv[24];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[24];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*58.62); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"INTELLECTUAL_PRODUCT");
	//cout << zz << " " << pch << "|" << "INTELLECTUAL_PRODUCT" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"INTELLECTUAL_PRODUCT");
	//cout << zz << " " << pch << "|" << "INTELLECTUAL_PRODUCT" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"INTELLECTUAL_PRODUCT");
	//cout << zz << " " << pch << "|" << "INTELLECTUAL_PRODUCT" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb(); 
      
      lexptr = LPv[25];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[25];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*787.08); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"JOURNAL");
	//cout << zz << " " << pch << "|" << "JOURNAL" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"JOURNAL");
	//cout << zz << " " << pch << "|" << "JOURNAL" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"JOURNAL");
	//cout << zz << " " << pch << "|" << "JOURNAL" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb(); 
      
      lexptr = LPv[26];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[26];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*26.65); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"LAB_PROCEDURE");
	//cout << zz << " " << pch << "|" << "LAB_PROCEDURE" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"LAB_PROCEDURE");
	//cout << zz << " " << pch << "|" << "LAB_PROCEDURE" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"LAB_PROCEDURE");
	//cout << zz << " " << pch << "|" << "LAB_PROCEDURE" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb(); 
      
      lexptr = LPv[27];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[27];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*90.37); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"LAB_TEST");
	//cout << zz << " " << pch << "|" << "LAB_TEST" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"LAB_TEST");
	//cout << zz << " " << pch << "|" << "LAB_TEST" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"LAB_TEST");
	//cout << zz << " " << pch << "|" << "LAB_TEST" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb(); 
      
      lexptr = LPv[28];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[28];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*42.17); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"LIPID");
	//cout << zz << " " << pch << "|" << "LIPID" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"LIPID");
	//cout << zz << " " << pch << "|" << "LIPID" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"LIPID");
	//cout << zz << " " << pch << "|" << "LIPID" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb(); 
      
      lexptr = LPv[29];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[29];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*158.18); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"MANUFACTURED_OBJECT");
	//cout << zz << " " << pch << "|" << "MANUFACTURED_OBJECT" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"MANUFACTURED_OBJECT");
	//cout << zz << " " << pch << "|" << "MANUFACTURED_OBJECT" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"MANUFACTURED_OBJECT");
	//cout << zz << " " << pch << "|" << "MANUFACTURED_OBJECT" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb(); 
      
      lexptr = LPv[30];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[30];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*7.84); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"MEDICAL_DEVICE");
	//cout << zz << " " << pch << "|" << "MEDICAL_DEVICE" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"MEDICAL_DEVICE");
	//cout << zz << " " << pch << "|" << "MEDICAL_DEVICE" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"MEDICAL_DEVICE");
	//cout << zz << " " << pch << "|" << "MEDICAL_DEVICE" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb(); 
      
      lexptr = LPv[31];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[31];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*243.04); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"MENTAL_PROCESS");
	//cout << zz << " " << pch << "|" << "MENTAL_PROCESS" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"MENTAL_PROCESS");
	//cout << zz << " " << pch << "|" << "MENTAL_PROCESS" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"MENTAL_PROCESS");
	//cout << zz << " " << pch << "|" << "MENTAL_PROCESS" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb(); 
      
      lexptr = LPv[32];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[32];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*41.05); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"MOLECULAR_FUNCTION");
	//cout << zz << " " << pch << "|" << "MOLECULAR_FUNCTION" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"MOLECULAR_FUNCTION");
	//cout << zz << " " << pch << "|" << "MOLECULAR_FUNCTION" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"MOLECULAR_FUNCTION");
	//cout << zz << " " << pch << "|" << "MOLECULAR_FUNCTION" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb(); 
      
      lexptr = LPv[33];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[33];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*582.52); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"NATURAL_PROCESS");
	//cout << zz << " " << pch << "|" << "NATURAL_PROCESS" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"NATURAL_PROCESS");
	//cout << zz << " " << pch << "|" << "NATURAL_PROCESS" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"NATURAL_PROCESS");
	//cout << zz << " " << pch << "|" << "NATURAL_PROCESS" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb(); 
      
      lexptr = LPv[34];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[34];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*10.45); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"NEOPLASTIC_PROCESS");
	//cout << zz << " " << pch << "|" << "NEOPLASTIC_PROCESS" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"NEOPLASTIC_PROCESS");
	//cout << zz << " " << pch << "|" << "NEOPLASTIC_PROCESS" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"NEOPLASTIC_PROCESS");
	//cout << zz << " " << pch << "|" << "NEOPLASTIC_PROCESS" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb(); 
      
      lexptr = LPv[35];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[35];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*531.53); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"OCCUPATIONAL_GROUP");
	//cout << zz << " " << pch << "|" << "OCCUPATIONAL_GROUP" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"OCCUPATIONAL_GROUP");
	//cout << zz << " " << pch << "|" << "OCCUPATIONAL_GROUP" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"OCCUPATIONAL_GROUP");
	//cout << zz << " " << pch << "|" << "OCCUPATIONAL_GROUP" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb(); 
      
      lexptr = LPv[36];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[36];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*61.63); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"ORGANIC_CHEMICAL");
	//cout << zz << " " << pch << "|" << "ORGANIC_CHEMICAL" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"ORGANIC_CHEMICAL");
	//cout << zz << " " << pch << "|" << "ORGANIC_CHEMICAL" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"ORGANIC_CHEMICAL");
	//cout << zz << " " << pch << "|" << "ORGANIC_CHEMICAL" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb(); 
      
      lexptr = LPv[37];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[37];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*2.89); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"ORGANISM");
	//cout << zz << " " << pch << "|" << "ORGANISM" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"ORGANISM");
	//cout << zz << " " << pch << "|" << "ORGANISM" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"ORGANISM");
	//cout << zz << " " << pch << "|" << "ORGANISM" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  
      
      lexptr = LPv[38];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[38];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*29.86); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"PATHOLOGIC_FUNCTION");
	//cout << zz << " " << pch << "|" << "PATHOLOGIC_FUNCTION" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"PATHOLOGIC_FUNCTION");
	//cout << zz << " " << pch << "|" << "PATHOLOGIC_FUNCTION" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"PATHOLOGIC_FUNCTION");
	//cout << zz << " " << pch << "|" << "PATHOLOGIC_FUNCTION" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  
      
      lexptr = LPv[39];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[39];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*478.33); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"PATIENT_GROUP");
	//cout << zz << " " << pch << "|" << "PATIENT_GROUP" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"PATIENT_GROUP");
	//cout << zz << " " << pch << "|" << "PATIENT_GROUP" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"PATIENT_GROUP");
	//cout << zz << " " << pch << "|" << "PATIENT_GROUP" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  
      
      lexptr = LPv[40];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[40];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*310.77); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"PERSON");
	//cout << zz << " " << pch << "|" << "PERSON" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"PERSON");
	//cout << zz << " " << pch << "|" << "PERSON" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"PERSON");
	//cout << zz << " " << pch << "|" << "PERSON" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  
      
      lexptr = LPv[41];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[41];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*74.87); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"PROFESSIONAL_GROUP");
	//cout << zz << " " << pch << "|" << "PROFESSIONAL_GROUP" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"PROFESSIONAL_GROUP");
	//cout << zz << " " << pch << "|" << "PROFESSIONAL_GROUP" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"PROFESSIONAL_GROUP");
	//cout << zz << " " << pch << "|" << "PROFESSIONAL_GROUP" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  
      
      lexptr = LPv[42];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[42];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*354.86); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"PROTEIN_COMPLEX");
	//cout << zz << " " << pch << "|" << "PROTEIN_COMPLEX" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"PROTEIN_COMPLEX");
	//cout << zz << " " << pch << "|" << "PROTEIN_COMPLEX" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"PROTEIN_COMPLEX");
	//cout << zz << " " << pch << "|" << "PROTEIN_COMPLEX" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  
      
      lexptr = LPv[43];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[43];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*288.37); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"PROTEIN_FAMILY");
	//cout << zz << " " << pch << "|" << "PROTEIN_FAMILY" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"PROTEIN_FAMILY");
	//cout << zz << " " << pch << "|" << "PROTEIN_FAMILY" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"PROTEIN_FAMILY");
	//cout << zz << " " << pch << "|" << "PROTEIN_FAMILY" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  
      
      lexptr = LPv[44];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[44];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*1.03); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"PROTEIN_MOLECULE");
	//cout << zz << " " << pch << "|" << "PROTEIN_MOLECULE" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"PROTEIN_MOLECULE");
	//cout << zz << " " << pch << "|" << "PROTEIN_MOLECULE" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"PROTEIN_MOLECULE");
	//cout << zz << " " << pch << "|" << "PROTEIN_MOLECULE" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  
      
      lexptr = LPv[45];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[45];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*52.86); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"PROTEIN_SUBUNIT");
	//cout << zz << " " << pch << "|" << "PROTEIN_SUBUNIT" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"PROTEIN_SUBUNIT");
	//cout << zz << " " << pch << "|" << "PROTEIN_SUBUNIT" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"PROTEIN_SUBUNIT");
	//cout << zz << " " << pch << "|" << "PROTEIN_SUBUNIT" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  
      
      lexptr = LPv[46];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[46];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*117.75); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"QUANTITATIVE_CONCEPT");
	//cout << zz << " " << pch << "|" << "QUANTITATIVE_CONCEPT" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"QUANTITATIVE_CONCEPT");
	//cout << zz << " " << pch << "|" << "QUANTITATIVE_CONCEPT" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"QUANTITATIVE_CONCEPT");
	//cout << zz << " " << pch << "|" << "QUANTITATIVE_CONCEPT" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  
      
      lexptr = LPv[47];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[47];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*262.23); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"RESEARCH_ACTIVITY");
	//cout << zz << " " << pch << "|" << "RESEARCH_ACTIVITY" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"RESEARCH_ACTIVITY");
	//cout << zz << " " << pch << "|" << "RESEARCH_ACTIVITY" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"RESEARCH_ACTIVITY");
	//cout << zz << " " << pch << "|" << "RESEARCH_ACTIVITY" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  
      
      lexptr = LPv[48];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[48];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*541.33); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"RESEARCH_DEVICE");
	//cout << zz << " " << pch << "|" << "RESEARCH_DEVICE" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"RESEARCH_DEVICE");
	//cout << zz << " " << pch << "|" << "RESEARCH_DEVICE" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"RESEARCH_DEVICE");
	//cout << zz << " " << pch << "|" << "RESEARCH_DEVICE" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  
      
      lexptr = LPv[49];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[49];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*360.75); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"RESEARCH_TECHNIQUE");
	//cout << zz << " " << pch << "|" << "RESEARCH_TECHNIQUE" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"RESEARCH_TECHNIQUE");
	//cout << zz << " " << pch << "|" << "RESEARCH_TECHNIQUE" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"RESEARCH_TECHNIQUE");
	//cout << zz << " " << pch << "|" << "RESEARCH_TECHNIQUE" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  
      
      lexptr = LPv[50];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[50];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*40.28); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"SIGN_OR_SYMPTOM");
	//cout << zz << " " << pch << "|" << "SIGN_OR_SYMPTOM" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"SIGN_OR_SYMPTOM");
	//cout << zz << " " << pch << "|" << "SIGN_OR_SYMPTOM" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"SIGN_OR_SYMPTOM");
	//cout << zz << " " << pch << "|" << "SIGN_OR_SYMPTOM" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  
      
      lexptr = LPv[51];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[51];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*25.46); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"STEROID");
	//cout << zz << " " << pch << "|" << "STEROID" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"STEROID");
	//cout << zz << " " << pch << "|" << "STEROID" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"STEROID");
	//cout << zz << " " << pch << "|" << "STEROID" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  
      
      lexptr = LPv[52];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[52];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*329.88); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"TEMPORAL_CONCEPT");
	//cout << zz << " " << pch << "|" << "TEMPORAL_CONCEPT" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"TEMPORAL_CONCEPT");
	//cout << zz << " " << pch << "|" << "TEMPORAL_CONCEPT" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"TEMPORAL_CONCEPT");
	//cout << zz << " " << pch << "|" << "TEMPORAL_CONCEPT" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  
      
      lexptr = LPv[53];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[53];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*2.88); 
      else zz=0;
      if(zz>max1) {
	
	max3=max2; max2=max1; max1=zz;  
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"THERAPEUTIC_OR_PREVENTIVE");
	//cout << zz << " " << pch << "|" << "THERAPEUTIC_OR_PREVENTIVE" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"THERAPEUTIC_OR_PREVENTIVE");
	//cout << zz << " " << pch << "|" << "THERAPEUTIC_OR_PREVENTIVE" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"THERAPEUTIC_OR_PREVENTIVE");
	//cout << zz << " " << pch << "|" << "THERAPEUTIC_OR_PREVENTIVE" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb();  
      
      lexptr = LPv[54];
      lexptr->gopen_LexPrb();
      lexptr->set_fst('!');
      mlexptr = MLPv[54];
      mlexptr->gopen_LexPrb();
      mlexptr->set_fst('!');
      xx=lexptr->prob_ext_fst(pch);
      yy=mlexptr->prob_ext_fst(pch);      
      if(xx)zz=1.0/(1.0+(yy/xx)*17.34); 
      else zz=0;
      if(zz>max1) {
	    
	max3=max2; max2=max1; max1=zz; 
	strcpy(tag3,tag2); strcpy(tag2,tag1); strcpy(tag1,"COMMON_WORD");
	//cout << zz << " " << pch << "|" << "COMMON_WORD" << endl;
      }
      else if(zz>max2) {
	max3=max2; max2=zz;  
	strcpy(tag3,tag2); strcpy(tag2,"COMMON_WORD");
	//cout << zz << " " << pch << "|" << "COMMON_WORD" << endl;
      }
      else if(zz>max3) {
	max3=zz; strcpy(tag3,"COMMON_WORD");
	//cout << zz << " " << pch << "|" << "COMMON_WORD" << endl;
      }
      lexptr->gclose_LexPrb();
      mlexptr->gclose_LexPrb(); 
    } 

    strcpy(tag, "[");
    if(strcmp(tag1,"NO_CAT") && strcmp(tag1,"STOP")) {
      strcat(tag,tag1);
      gcvt(max1,2,temp);
      strcat(tag, " (");
      strcat(tag, temp);
      strcat(tag, ")");
    }
    if(strcmp(tag2,"NO_CAT") && strcmp(tag2,"STOP")) {
      gcvt(max2,2,temp);
      strcat(tag,"|");
      strcat(tag,tag2);
      strcat(tag, " (");
      strcat(tag, temp);
      strcat(tag, ")");
    }
    if(strcmp(tag3,"NO_CAT") && strcmp(tag3,"STOP")) {
      gcvt(max3,2,temp);
      strcat(tag,"|");
      strcat(tag,tag3);
      strcat(tag, " (");
      strcat(tag, temp);
      strcat(tag, ")");
    }
    strcat(tag,"]");
    Tags.push_back(tag);
  }
  //free_vec_str(Terms);
  //free_vec_str(Tags);
}

void SemTag::write_tagged(void) {
  int i;
  if(Terms.size()==Tags.size()) {
    //cout << Terms[0] << "/" << Tags[0];
    for(i=1;i<Terms.size();i++) {
      if(strcmp(Tags[i],"NO_CAT") && strcmp(Tags[i],"STOP")) {
	cout << " " << Terms[i] << "/" << Tags[i];
      }
      else {
	cout << " " << Terms[i];
      }
	   
    }
  }
  cout << endl;
  free_vec_str(Terms);
  free_vec_str(Tags);
}

void SemTag::free_vec_str(vector<char*> &vc) {
  vector<char*>::iterator p = vc.begin();
  while(p!=vc.end()){
    if(*p)delete [] *p;
    p++;
  }
  vc.clear();
}


}




 

