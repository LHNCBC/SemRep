#include <cstdio>
#include <cstring>
#include <iostream.h>
#include <fstream>
#include <Btree.h>
#include <runn.h>
#include <Thes.h>
#include <vector>
#include <string>
#include <Blist.h>

#define XSTR 100000

using namespace std;                                                        
using namespace iret;
char* lc(char*);

main(int argc, char** argv) {
  ifstream ifs;
  Count* GeneNames=NULL;
  ifs.open(argv[1], ios::in);
  char s[XSTR];
  int ii;
  char *strpair, *loc, *aptr, *str;
  char strc[XSTR];
  long i;
  int ans;
  int gflag=0;
  char gene[XSTR];
  long id;

  //if(ifs.is_open()) { cout << "OPEN" << endl; }
  while(ifs.getline(s, XSTR,'\n')) {
    str=s;
    if(strpbrk(str,"@@") && strpbrk(str, "/CD")) {
      str+=2;
      loc = strrchr(str, '/'); 
      if(loc) {
	loc++;
	*(--loc) = '\0';
      }
      id = atoi(str);

      if(GeneNames) {
	if(GeneNames->cnt_key > 0) {
 	GeneNames->node_first();	
	while(GeneNames->node_next()) {
	  //cout << GeneNames.show_str() << " " << GeneNames.count() << endl;
	  cout << GeneNames->show_str() << endl;
	}
	}
	delete GeneNames;
      }
      cout << "----- ITAME DOC ID: " << id << " -----" << endl;
      GeneNames = new Count;
    }
    else if(strpbrk(str,"abcdefghijklmnopqrstuvwxzyABCDEFGHIJKLMNOPQRSTUVWXYZ")  &&
       (strpbrk(str, "/NN") || strpbrk(str, "/V") || strpbrk(str, "/CD"))) {
      strpair = strtok(str, " ");
      strcpy(strc,strpair);
      loc = strrchr(strc, '/'); 
      if(loc) {loc++;}
      if(loc && strstr(loc,"GENE")) {
	*(--loc) = '\0';
	if(!gflag) {
	  strcpy(gene, strc);
	  gflag=1;
	}
	else {
	  strcat(gene," ");
	  strcat(gene,strc);
	}
      }
      else if(gflag) {
	aptr = lc(gene);
	//aptr=gene;
	GeneNames->add_count2(aptr,1);
	gflag=0;
      }
      while((strpair = strtok(NULL, " ")) != NULL) {
	strcpy(strc,strpair);
	loc = strrchr(strc, '/');     
	if(loc) {loc++;}
	if(loc && strstr(loc,"GENE")) {
	  *(--loc) = '\0';
	  if(!gflag) {
	    strcpy(gene, strc);
	    gflag=1;
	  }
	  else {
	    strcat(gene," ");
	    strcat(gene,strc);
	  }
	}
	else if(gflag) {
	  aptr = lc(gene);
	  //aptr=gene;
	  GeneNames->add_count2(aptr,1);
	  gflag=0;
	}
      }  
    }
  }
  if(GeneNames) {
    if(GeneNames->cnt_key > 0) {
      GeneNames->node_first();	
      while(GeneNames->node_next()) {
	//cout << GeneNames.show_str() << " " << GeneNames.count() << endl;
	cout << GeneNames->show_str() << endl;
      }
    }
    delete GeneNames;
  }
  ifs.close();
 
}

char* lc(char* lcw) {
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










