#include <stdio.h>
#include <string.h>
#include <pcre.h>
#include <abgene.h>

#define MAXLINE 400096 
#define OVECCOUNT 300

#define TRUE 1
#define FALSE 0
#define DEBUG 0


char *Trim_String(char *input_text);
char *c_abgene(char *input_str);

char *c_abgene(char *input_str) {
    static char *output_str = NULL;    
    pcre *re;
    const char *error;
    int erroffset;
    int ovector[OVECCOUNT];
    int rc, i;
    const char *pmid_pattern = "^PMID\\s*\\-\\s*(\\d+)$";
    const char *da_pattern = "^DA\\s*\\-\\s*(\\d+)$";
    const char *title_pattern = "^TI\\s*\\-\\s*([\\w\\W]+)$";
    const char *abstract_pattern = "^AB\\s*\\-\\s*([\\w\\W]+)$";
    const char *next_item_pattern = "^(\\w+)\\s*\\-\\s*";
    int input_len;
    char *intermediate_str = NULL;
    int intermediate_str_len;
    char *intermediate_str_abgene = NULL;
    char *pmid_str = NULL;
    char *da_str = NULL;
    char *temp_title_str = NULL;
    char *title_str = NULL;
    char *temp_abstract_str = NULL;
    char *abstract_str = NULL;
    int pmid_len, da_len, title_len, title_ind, abstract_len, abstract_ind;
    int title_end_ind, abstract_end_ind;

    input_len = (int)strlen(input_str);
    if (DEBUG) printf("INPUT STRING: %d|%s\n", input_len, input_str);

    /* Find PMID */
    re = pcre_compile(pmid_pattern,PCRE_MULTILINE,&error,&erroffset,NULL);
    if (re == NULL) {
	printf("PCRE compilation for PMID failed at offset %d: %s\n", erroffset, error);
	return NULL;
    }
    rc = pcre_exec(re,NULL,input_str,input_len,0,0,ovector,OVECCOUNT);
    if (rc < 0) {
	switch(rc) {
	case PCRE_ERROR_NOMATCH: printf("No PMID found.\n"); break;
	                default: printf("Matching error %d at PMID.\n", rc); break;
	}
	//return NULL;
    }

    if (rc == 0) {
	rc = OVECCOUNT/3;
    }
 
    pmid_len = ovector[3] - ovector[2];    
    pmid_str = (char *)calloc(1,pmid_len + 1); 
    strncpy(pmid_str, input_str + ovector[2], pmid_len);
    if (DEBUG) printf("PMID: %d|%d|%d|%s\n", ovector[2], ovector[3], pmid_len, pmid_str);
    pcre_free(re); 
   
    /* Find DA */
    re = pcre_compile(da_pattern,PCRE_MULTILINE,&error,&erroffset,NULL);
    if (re == NULL) {
	printf("PCRE compilation for DA failed at offset %d: %s\n", erroffset, error);
	return NULL;
    }

    rc = pcre_exec(re,NULL,input_str,input_len,0,0,ovector,OVECCOUNT);
    if (rc < 0) {
	switch(rc) {
	case PCRE_ERROR_NOMATCH: printf("No DA found.\n"); break;
	                default: printf("Matching error %d at DA.\n", rc); break;
	}
	//return NULL;
    }

    if (rc == 0) {
	rc = OVECCOUNT/3;
    }
    
    da_len = ovector[3] - ovector[2]; 
    da_str = (char *)calloc(1,da_len + 1); 
    strncpy(da_str, input_str + ovector[2], da_len);
    
    if (DEBUG) printf("DA: %d|%d|%d|%s\n", ovector[2], ovector[3], da_len, da_str);
    pcre_free(re); 

    /* find TI */
    re = pcre_compile(title_pattern,PCRE_MULTILINE,&error,&erroffset,NULL);
    if (re == NULL) {
	printf("PCRE compilation for TI failed at offset %d: %s\n", erroffset, error);
	return NULL;
    }

    rc = pcre_exec(re,0,input_str,input_len,0,0,ovector,OVECCOUNT);
    if (rc < 0) {
	switch(rc) {
	case PCRE_ERROR_NOMATCH: printf("No TI found.\n"); break;
	                default: printf("Matching error %d at TI.\n", rc); break;
	}
    }

    if (rc == 0) {
	rc = OVECCOUNT/3;
    }
    
    title_ind = ovector[2];
    pcre_free(re);

    re = pcre_compile(next_item_pattern,PCRE_MULTILINE,&error,&erroffset,NULL);
    if (re == NULL) {
	printf("PCRE compilation for next item(TI) failed at offset %d: %s\n", erroffset, error);
	return NULL;
    }

    rc = pcre_exec(re,0,input_str,input_len,title_ind+1,0,ovector,OVECCOUNT);
    if (rc < 0) {
	switch(rc) {
	case PCRE_ERROR_NOMATCH: printf("No next item(TI) found.\n"); title_end_ind = input_len-1; break;
	                default: printf("Matching error %d at next item(TI).\n", rc); break;
	}
    }

    if (rc == 0) {
	rc = OVECCOUNT/3;
    }

    if (rc >= 0) title_end_ind = ovector[2];
    title_len = title_end_ind - title_ind;  
    temp_title_str = (char *)calloc(1,title_len + 1); 
    strncpy(temp_title_str, input_str + title_ind, title_len);
    title_str = (char *)calloc(1,title_len + 1);
    strcpy(title_str, Trim_String(temp_title_str));
    if (DEBUG) printf("TI: %d|%d|%d|%s\n", title_ind, title_end_ind, title_len, title_str);
    pcre_free(re); 
    
    /* Find AB */
    re = pcre_compile(abstract_pattern,PCRE_MULTILINE,&error,&erroffset,NULL);
    if (re == NULL) {
	printf("PCRE compilation for AB failed at offset %d: %s\n", erroffset, error);
	return NULL;
    }

    rc = pcre_exec(re,0,input_str,input_len,0,0,ovector,OVECCOUNT);
    if (rc < 0) {
	switch(rc) {
	case PCRE_ERROR_NOMATCH: printf("No AB found.\n"); break;
	                default: printf("Matching error %d at AB.\n", rc); break;
	}
    }

    if (rc == 0) {
	rc = OVECCOUNT/3;
    }
    

    abstract_ind = ovector[2];
    pcre_free(re);

    re = pcre_compile(next_item_pattern,PCRE_MULTILINE,&error,&erroffset,NULL);
    if (re == NULL) {
	printf("PCRE compilation for next item(AB) failed at offset %d: %s\n", erroffset, error);
	return NULL;
    }

    rc = pcre_exec(re,0,input_str,input_len,abstract_ind+1,0,ovector,OVECCOUNT);
    if (rc < 0) {
	switch(rc) {
	case PCRE_ERROR_NOMATCH: printf("No next item(AB) found.\n"); abstract_end_ind = input_len-1;break;
	                default: printf("Matching error %d at AB.\n", rc); break;
	}
    }

    if (rc == 0) {
	rc = OVECCOUNT/3;
    }
    pcre_free(re);

    if (rc >= 0)  abstract_end_ind = ovector[2];
    abstract_len = abstract_end_ind - abstract_ind;  
    temp_abstract_str = (char *)calloc(1,abstract_len + 1); 
    strncpy(temp_abstract_str, input_str + abstract_ind, abstract_len-1);
    abstract_str = (char *)calloc(1, abstract_len + 1);
    strcpy(abstract_str, Trim_String(temp_abstract_str));
    if (DEBUG) printf("AB: %d|%d|%d|%s\n", abstract_ind, abstract_end_ind, abstract_len, abstract_str);

    intermediate_str_len = strlen(abstract_str) + strlen(title_str) + 100;
    intermediate_str = (char *)calloc(1,intermediate_str_len+1);
    sprintf(intermediate_str,"@@%s \n&&%s \n%s%s", pmid_str,da_str,title_str,abstract_str);

    if (DEBUG) printf("STRING TO ABGENE: %d|%s\n", strlen(intermediate_str),intermediate_str);
    intermediate_str_abgene = Process(intermediate_str); 
    if (DEBUG) printf("STRING FROM ABGENE: %d|%s\n", strlen(intermediate_str_abgene), intermediate_str_abgene);
    if (output_str != NULL)
    { 
	free(output_str);
    }
    output_str = (char *)malloc(strlen(intermediate_str_abgene)+2);
    strcpy(output_str,""); 
    strcpy(output_str,intermediate_str_abgene);
    strcat(output_str,"\n"); 
    if (DEBUG) printf("Freeing variables..,");
    if (temp_title_str != NULL) free(temp_title_str);
    if (temp_abstract_str != NULL) free(temp_abstract_str);
    if (pmid_str != NULL) free(pmid_str);
    if (da_str != NULL) free(da_str);
    if (title_str != NULL) free(title_str);
    if (abstract_str != NULL) free(abstract_str);
    if (intermediate_str != NULL) free(intermediate_str);
    if (DEBUG) printf("Completed freeing variables.\n");
    return output_str;
}

/* Trim TI and AB sections. Remove extra spaces and tokenize 
   the sentences. */
char *Trim_String(char *input_text)
{
       int i=0;
       int j=0;
       int end_of_sentence = FALSE;
       int space=0;
       static char trimmed_text[MAXLINE + 1];

       if (DEBUG) printf("STRING TO TRIM: %d|%s\n", strlen(input_text),input_text);
       strcpy(trimmed_text,"");

       /* Break into sentences and remove extra space */
       while (input_text[i] != '\0' && i < strlen(input_text)-1) {
	 /* possibly the end of the sentence */
	 if (input_text[i] == '.' || input_text[i] == '?')
         {
	     if (space > 0)
	     {
		 trimmed_text[j] = 32; j++;
	     }
	     space = 0;
	     end_of_sentence = TRUE;
             trimmed_text[j] = input_text[i]; j++;
         }
	 /* space encountered. */
         else if (input_text[i] == 32)
	 {
	     space++;	     
	 }
	 else if (end_of_sentence && space > 0)
	 {
	     /* [A-Z][0-9]\[\) encountered after end of sentence and spaces */
	     /* Start of a sentence */
	     if ((input_text[i]  > 64 && input_text[i]  < 92) ||
	           (input_text[i] > 47 &&  input_text[i] < 58) ||
                   input_text[i] == 40)
	     {
		 space = 0;
		 trimmed_text[j] = 10; j++;
	     }
	     /* other characters after the end of sentence and spaces */
	     /* not the start of a sentence */
	     else 
	     {
		 while (space>0)
		 {
		     trimmed_text[j] = 32; j++;
		     space--;
		 }

	     }
	     trimmed_text[j] = input_text[i]; j++;
	     end_of_sentence = FALSE;
          }
	  /* first character of a word, add space and the character */
	  else if (space > 0)
	  {
	      trimmed_text[j] = 32; j++;
	      trimmed_text[j] = input_text[i]; j++;
	      space=0;
	      end_of_sentence = FALSE;
	  }
	 /* other regular characters, ignore newline characters */
          else if (input_text[i] != 10)
          {
	     space =0;
             trimmed_text[j] = input_text[i];
             j++;
	     end_of_sentence = FALSE;
          }
          i++;
       }

       trimmed_text[j] = input_text[i]; j++;	     
       trimmed_text[j] = '\0';
       if (DEBUG) printf("TRIMMED STRING: %d|%s\n",strlen(trimmed_text), trimmed_text);

       return trimmed_text;
}
