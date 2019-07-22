#include "Bnum.h"
namespace iret {

Bnum::Bnum(long nm){
   num=nm;
   mm=new long[num];
} 

Bnum::Bnum(Bnum &Anm){
   long i;
   num=Anm.num;
   mm=new long[num];
   for(i=0;i<num;i++)mm[i]=Anm.mm[i];
}
   
Bnum::~Bnum(){
    if(mm!=NULL)delete [] mm;
}  
 
long Bnum::index(long qn){
   long i,j,k,x,y;

   if(qn<mm[0]){cout << qn << " Below range!" << endl;return(0);}
   if(qn>=mm[num-1]){cout << qn << " Above range!" << endl;return(num-1);}
   x=0;
   y=num-1;
   while(y-x>1){
      i=(y+x)/2;
      if(qn>=mm[i])x=i;
      else y=i;
   }
   return(x);
}
   
} 
