

/* 
   prevent sccs_id_miscutil_p_h from being re-defined in case
   miscutil_p.h is loaded multiple times in same compilation 
*/

#ifndef sccs_id_miscutil_p_h
static char sccs_id_miscutil_p_h[] = "@(#)miscutil_p.h	1.3 09/27/06";
#define sccs_id_miscutil_p_h 1
#endif

/* ======================================================== 
   Prototypes for miscutil.c
   ========================================================  */

extern int *obsolete_get_fields(
		       char *line,			/* line to be broken up */
		       char sep,			/* separator char */
		       int *numF 			/* number of fields */
		       );

extern void * realloc_buf(
			  void *buf,		/* pointer to buffer */
			  int unit,		/* size of one of what buf points to in bytes */
			  int size,		/* current size of buffer in units */
			  int *alloc,		/* current allocation of buffer in units */
			  int n,		/* units needed */
			  int incr 		/* increment size in units */
			  );


extern void llip( register char *s );


extern int skipto(
		  char **cpp,
		  char c 
		  );

extern int skiptoby(
		    char **cpp,
		    char c,
		    int n 
		    );


extern void rip( char *s);

extern int alnumlen( char *s);


extern int strpunctcmp(
		       char *s,
		       char *t 
		       );


extern int strnpunctcmp(
			char *s,
			char *t,
			int n 
			);


extern int strnpunctcmpr(
			 char *s,
			 int sl,			/* length of s */
			 char *t,
			 int tl 			/* length of t */
			 );

extern int strnpunctcasecmp(
			    char *s,
			    char *t,
			    int n 
			    );

/* ======================================================== 
   Prototypes for lvg_min.c
   ========================================================  */

extern signed int lvg_min( 
			  signed int a, /* Input */
			  signed int b  /* Input */
			  );
