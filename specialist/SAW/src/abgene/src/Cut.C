#include "Cut.h"

namespace iret {

Cut::Cut(const char *nam_slice){
   mm=NULL;
   rev=NULL;
   char cnam[100];
   ifstream fin;
   get_pathw(cnam,"slice",nam_slice,"d");
   fin.open(cnam,ios::in);
   fin >> ndoc;
   fin.close();
   hyper = new Hyper(ndoc+10,1.0e-8);
   get_pathw(cnam,"slice",nam_slice,"n");
   fin.open(cnam,ios::in);
   fin >> nwrd;
   fin.close();
} 

Cut::Cut(void){
   mm=NULL;
   rev=NULL;
}  

Cut::~Cut(){
    if(mm)delete []mm;
    if(rev)delete []rev;
    if(hyper) delete hyper;
}

void Cut::set_get_M(long *frq){
   long i, j, k;
   freq = frq;
   mm = new long[ndoc];
   for(i= 0; i < ndoc; i++) mm[i] = 0;
   for(i= 0; i < nwrd; i++){
   	 k = freq[i];
         
         mm[k]++;
   }  
   rev = new long[ndoc];
   j = 0;
   for(i = 0; i < ndoc; i++){
   	 if(mm[i] !=0) {
           rev[j] = i; 
           j++;
         }
   }
   f_max  = 1;
   non_zero = j;
   i = 0;
   while(i < non_zero) {
   	if(f_max < rev[i]) f_max = rev[i];
        i++; 
  }
   cout << "end of reading" <<endl;
}  
 
double Cut::get_M(long n_s){
	
        double pr_not_null;	
        double m_sum = 0;
        double M;
        for(long i = 2; i < non_zero; i++){
                pr_not_null = 1.0 - pow(10.0, hyper->log_prob(0, n_s, rev[i], ndoc));
                M= pr_not_null*mm[rev[i]];
                m_sum = m_sum + M;
        } 
        
        return (m_sum);
}


void Cut::create_cut_array(long ns){
   int pflag=get_qflag();
   long i,j,k; 
   double *Mxx, *pz;
   mm = new long[ndoc+1];
   rev= new long[ndoc+1];
   for(i= 0; i < (ndoc+1); i++) mm[i] = 0;
   
   for(i= 0; i < nwrd; i++){
   	 k = freq[i];
         mm[k]++;
   }  
   
   j = 0;
   for(i = 0; i < ndoc; i++){
   	 if(mm[i] !=0) {
           rev[j] = i; 
           j++;
         }
   }
   non_zero = j;
   cout <<"Non Zero "<<j<<endl;
   Mxx=new double[ns+1];
   pz=new double[ns+1];
   
   double pr_not_null;	
   for(i=2;i<=ns;i++) Mxx[i]=0.0;
   for(j = 2; j <non_zero; j++){
      pz[1]=double(ndoc-rev[j])/double(ndoc);
      for(i=2;i<=ns;i++){
         pz[i] = pz[i-1]*(double(ndoc-rev[j]-i+1)/double(ndoc-i+1));
         pr_not_null=1.0-pz[i];
         Mxx[i]+=pr_not_null*mm[rev[j]];
      }
      mark(pflag,j,1000,"frequency computed");
   }
   for(i=2;i<=ns;i++) Mxx[i]=log10(Mxx[i]);
   delete [] mm;
   mm=NULL;
   delete [] rev;
   rev=NULL;
   delete []pz;
   pz=NULL;
   FBase Fb("alpha","cut");
   ofstream *pfout=Fb.get_Ostr("n",ios::out);
   *pfout << ns+1 << endl;
   Fb.dst_Ostr(pfout);
   Fb.bin_Writ("c",sizeof(double)*(ns+1),(char*)Mxx);
   delete []Mxx;
   
}

void Cut::gopen_cut_map(void){
   FBase Fb("alpha","cut");
   Txx=(double*)Fb.get_Mmap("c");
   ifstream fin("alpha_cut.n", ios::in);
   fin>>snum;
   fin.close();
   hyper=new Hyper(snum+2000, 0.01);

}

long Cut::signif(long n_st, long n_s, long n_t, long N){
     if(hyper->nlog_pval_appx(n_st,n_s,n_t,N)> Txx[n_s+1]) return(1);
     else return(0);
}

}
