#include <Brill.h>
#include <PostBrill_FP.h>
#include <PostBrill_FN.h>

#define DEBUG 0

using namespace std;                                                        
using namespace iret;

// Calls ABGENE modules
extern "C" char* Process(char *input_str) {
   string brill_str;
   const char *brill_char;
   string postbrill_fp_str;
   const char *postbrill_fp_char;
   string postbrill_fn_str;
   const char *postbrill_fn_char;
   char *str1, *str2;
   static char *return_str=NULL;

   // Brill tagging 
   Brill b("gene");
   b.open_tagger();
   fprintf(stderr, "OPENED BRILL TAGGER\n");
   b.read_and_tok_sub(input_str);
   fprintf(stderr,"READ AND TOKENIZED\n");    
   b.start_state_tagger();
   b.final_state_tagger();
   brill_str = b.get_results();
   b.close_tagger();
   fprintf(stderr, "CLOSED BRILL TAGGER\n");
   brill_char = brill_str.c_str();
   if (DEBUG) cout << "OUTPUT FROM BRILL TAGGER: " << brill_char << endl;
   str1 = (char *)malloc(strlen(brill_char) + 1);
   strcpy(str1,""); strcpy(str1,brill_char);

   // FP filtering 
   PostBrill_FP pb_fp("gene");
   pb_fp.gopen_PostBrill_FP(); 
   fprintf(stderr, "OPENED FP FILTER\n");
   postbrill_fp_str = pb_fp.filter_str_sub(str1);
   postbrill_fp_char = postbrill_fp_str.c_str();
   pb_fp.gclose_PostBrill_FP();
   fprintf(stderr, "CLOSED FP FILTER\n");
   if (DEBUG) cout << "OUTPUT FROM POST-BRILL FP FILTER: " << postbrill_fp_char << endl;
   str2 = (char *)malloc(strlen(postbrill_fp_char) + 1);
   strcpy(str2,""); strcpy(str2,postbrill_fp_char);

   // FN filtering
   PostBrill_FN pb_fn("gene");  
   pb_fn.gopen_PostBrill_FN(); 
   fprintf(stderr, "OPENED FN FILTER\n");
   postbrill_fn_str = pb_fn.filter_str_sub(str2);
   if (DEBUG) cout << "OUTPUT FROM POST-BRILL PRELIMINARY FILTER: " << postbrill_fn_str << endl;
   postbrill_fn_char = postbrill_fn_str.c_str();  
   pb_fn.gclose_PostBrill_FN();
   fprintf(stderr, "CLOSED FN FILTER\n");
   if (DEBUG) cout << "OUTPUT FROM POST-BRILL FN FILTER: " << postbrill_fn_char << endl;   

   if (return_str!=NULL) free(return_str);
   return_str = (char *)malloc(strlen(postbrill_fn_char)+1);
   strcpy(return_str,postbrill_fn_char);

   free(str1);
   free(str2);

   return return_str;   
}












