#ifndef PHRA_H
#define PHRA_H

#include <cmath>
#include <iostream>
#include <fstream>
#include "runn.h"
#include "Btree.h"
using namespace std;
namespace iret {

#define LOWERCASE 0
#define ALLCASE  1
#define STOPWORDS 1
#define NOSTOPWORDS 0

class           Word {

	public:
	char           *txt;	/* Input text */
	long            length;	/* Length of input text */
	                Word();	/* Building btree for stop words and
				 * allocating space for the output of
				 * singlets, multiplets and phrases */
	                Word(long wrd_spc); // As previous but sets word_space to
                                // wrd_spc.
	               ~Word();	/* Destroying zlt_convert[[] and list[] */
	long            wordf(long &i, long len);	
				/* Given beginning and ending
				 * positions of input text,
				 * it produces words */
	void            single();	/* Given input text, it produces
					 * singlets between punctuation marks
					 * set by set_map() */
	void            multiple(int nplet);	/* Given input text, it
						 * produces multiplets
						 * between punctuation marks
						 * set by set_map() */
	void            phrase();	/* Given input text, it produces
					 * phrases between punctuation marks
					 * set by set_map(); */
	void            convert(char *text,long len);	
                 			/* Mapping ASCI characters to
					 * integers, stored in array
					 * zlt_convert[] */
        void    	modify(char c);
                                /* Removes any occurrence of c at beginning of
                                 * a word */
        void		remove_sgml_tags(long n,char *text);
                        //Maps tags to nulls.
        void		remove_html_list_commas(long n,char *text);
                        //Maps commas to nulls.
	char          **list;	/* List of singlets, multiplets and phrases
				 * produced by single(), multiple(int nplet)
				 * and phrase() */
	long            cnt;	/* Given input text, the number of singlets,
				 * multiplets and phrases produced by
				 * single(), multiple(int nplet) and phrase()*/
	void            clear_list();	/* Destroying list */
	void            set_map(char *punct, char mark, int case_cas);	
 				/* Given character array
				 * punct[], setting
				 * punctuation marts and
				 * kase = LOWERCASE for
				 * the lower case, and
				 * kase = UPPERCASE for
				 * the case sensitive */
	void            restore_map(char *restore);	
				/* Restoring given
				 * characters array
				 * restore[] to the
				 * input text with their
				 * original character */
	void            erase_map(char *erase);	
				/* Re-setting characters 0 in
				 * the zlt[] */ 
        void            all_print_map(); //Maps all printable
                                //characters to themselves.
        int             byte_lim;  /*Set it to 0 for allowing
				 * pure digits, otherwise 65 */
        int  		back_lim;  /*Set it to limit for backing off at
                                 * end of words. */
	int             stop;   /* Set it to 0 for allowing  stop words in
				 * singlets, multiplets and phrases,
				 * otherwise 1 */
	void            step_zero();	/* Initializing be = 0 and en =0 */
	long            step_next();	/* Finding  the character positions
					 * of beginning and ending
					 * punctuation mark  */
	char            wrd[max_str];	/* Array for holding the string
					 * produced by wordf(); */
	int             al;	/* 1 if all letter in the array wrd[] are
               			 *above the byte_lim, otherwise 0.*/

	                private:
	char           *zlt_convert;	/* Array mapped by zlt[] */
	long            beg;	/* Posintion of beginning mark set by
				 * function set_map() */
	long     	end;	/* Position of ending mark set by function
				 * set_map() */
	Btree          *btr;	/* Btree for stop words */
        char            mark;   /* Marker for punctuation */
        char           *zlt;    /* Used to map ascii */
        long           word_space; //Number of singlets, multiplets or phrases
                                   //allowed. Bound on list size. 

};
}
#endif
