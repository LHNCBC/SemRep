#ifndef EXUTILS
#define EXUTILS


/*
** exutils.h
**
** Header file which contains the defines and prototypes for the utility
** functions in exutils.c
** 
*/

/* Sccsid %Z% %M% %I% %G% */

/*****************************************************************************
** 
** defines and typedefs used 
** 
*****************************************************************************/

#ifndef MAX
#define MAX(X,Y)	(((X) > (Y)) ? (X) : (Y))
#endif

#ifndef MIN
#define MIN(X,Y)	(((X) < (Y)) ? (X) : (Y))
#endif

/*
** Maximum character buffer for displaying a column
*/
#define MAX_CHAR_BUF	1024

/*
** Define structure where row data is bound.
*/
typedef struct _ex_column_data
{
	CS_INT		indicator;
	CS_CHAR		*value;
	CS_INT		valuelen;
} EX_COLUMN_DATA;

/*****************************************************************************
** 
** protoypes for all public functions 
** 
*****************************************************************************/
/* exutils.c */
extern CS_INT CS_PUBLIC ex_display_dlen PROTOTYPE((
	CS_DATAFMT *column
	));
extern CS_RETCODE CS_PUBLIC ex_display_header PROTOTYPE((
	CS_INT numcols,
	CS_DATAFMT columns[]
	));
extern CS_RETCODE CS_PUBLIC ex_display_column PROTOTYPE((
	CS_CONTEXT *context,
	CS_DATAFMT *colfmt,
	CS_VOID *data,
	CS_INT datalength,
	CS_INT indicator
	));
extern CS_VOID CS_PUBLIC ex_panic PROTOTYPE((
	char *msg
	));
extern CS_VOID CS_PUBLIC ex_error PROTOTYPE((
	char *msg
	));
extern CS_RETCODE CS_PUBLIC ex_clientmsg_cb PROTOTYPE((
	CS_CONTEXT *context,
	CS_CONNECTION *connection,
	CS_CLIENTMSG *errmsg
	));
extern CS_RETCODE CS_PUBLIC ex_servermsg_cb PROTOTYPE((
	CS_CONTEXT *context,
	CS_CONNECTION *connection,
	CS_SERVERMSG *srvmsg
	));
extern CS_RETCODE CS_PUBLIC ex_init PROTOTYPE((
	CS_CONTEXT **context
	));
extern CS_RETCODE CS_PUBLIC ex_connect PROTOTYPE((
	CS_CONTEXT *context,
	CS_CONNECTION **connection,
	CS_CHAR *appname,
	CS_CHAR *username,
	CS_CHAR *password,
	CS_CHAR *server
	));
extern CS_RETCODE CS_PUBLIC ex_con_cleanup PROTOTYPE((
	CS_CONNECTION *connection,
	CS_RETCODE status
	));
extern CS_RETCODE CS_PUBLIC ex_ctx_cleanup PROTOTYPE((
	CS_CONTEXT *context,
	CS_RETCODE status
	));
extern CS_RETCODE CS_PUBLIC ex_execute_cmd PROTOTYPE((
	CS_CONNECTION *connection,
	CS_CHAR *cmdbuf
	));
extern CS_RETCODE CS_PUBLIC ex_fetch_data PROTOTYPE((
	CS_COMMAND *cmd
	));
extern CS_RETCODE CS_PUBLIC ex_create_db PROTOTYPE((
	CS_CONNECTION *connection,
	char *dbname
	));
extern CS_RETCODE ex_remove_db PROTOTYPE((
	CS_CONNECTION *connection,
	char *dbname
	));
extern CS_RETCODE ex_use_db PROTOTYPE((
	CS_CONNECTION *connection,
	char *dbname
	));

#endif // EXUTILS
