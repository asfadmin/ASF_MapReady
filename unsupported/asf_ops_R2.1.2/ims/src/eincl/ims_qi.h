/******************************************************************************
**
** File:        ims_qi.h
**
** Function:    Header file for Query Interface library functions.
**
** Date:        4/5/90
**
** Modified:    11/28/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**              Added the macro IMS_CONNECTED() along with the corresponding
**              member in the QI descriptor structure. Modified the
**              IMS_RETVALUENTS() macro to include a pointer cast to 
**              alleviate some compiler warnings.
**
**              1/27/95 - S. Hardman - R1B
**              Cleaned up the file and added the macro IMS_SET_USERDATA().
**
** Notes:
**
** 1. Access to the data structures is by macro.  Every macro has at least
** one argument, 'x', which is the address of a query descriptor returned
** by a call to ims_qiDescAlloc () or the pointer to a IMS_QI_DESC_OBJ object.
** Some macros have a second argument, 'y', which is the address of a value
** to which an element of the descriptor structure is to be set.
**
******************************************************************************/

#ifndef _IMS_QI_H
#define _IMS_QI_H

static char *sccsQi = "@(#)ims_qi.h	5.2  05/09/97";

/*
** Definitions.
*/
#define IMS_SYB_DEADLOCK     1205
#define IMS_MAXQUERYDESCS     128
#define IMS_DEFAULT_ROWCOUNT "512"

/*
** Enumeration of states for query.
*/
enum IMS_QSTATES
{
   IMS_INITIAL,
   IMS_STATEMENTSENT,
   IMS_RESULTS,
   IMS_DONE
};

/*
** Return statuses for stdNextRow().
*/
#define IMS_ROWRETURNED       1
#define IMS_ENDOFQUERY        2
#define IMS_ENDOFTRANSACTION  3

/*
** Define lengths for coerced to NTS attributes. Buffer lengths for numbers
** allow for a leading sign (+/-).
*/
#define IMS_LENINT1    3
#define IMS_LENINT2    6
#define IMS_LENINT4   11
#define IMS_LENFLT8   19
#define IMS_LENMONEY  25
#define IMS_LENDATE   26

/*
** Define the maximum amount of text and image to be displayed.
*/
#define IMS_LENTEXT   1024
#define IMS_LENIMAGE  1024

/*
** Macro Definitions
*/

/* Macros for database server access parameters. */
#define IMS_SETUSER(x,y) (void) strcpy (x->username, y)
#define IMS_SETPSWD(x,y) (void) strcpy (x->password, y)
#define IMS_SETPROG(x,y) (void) strcpy (x->program, y)
#define IMS_SETSERVER(x,y) (void) strcpy (x->server, y)
#define IMS_SETDBNAME(x,y) (void) strcpy (x->dbname, y)
#define IMS_SETBCP(x,y) x->bcp = y

/* Macros for executing a database server command. */
#define IMS_SETCMD(x,y) x->cmd = y
#define IMS_SETATTRSNTS(x) x->coerceToNTS = IMS_ON
#define IMS_SETATTRSNative(x) x->coerceToNTS = IMS_OFF
#define IMS_SETBUFCOUNT(x,y) x->bufferCount = y
#define IMS_SETROWCOUNT(x,y) dbsetopt (x->dbproc, DBROWCOUNT, y, -1);

/* Macros for binding data to program variables. */
#define IMS_BIND(x,y,z,v,w) dbbind (x->dbproc, y, z, v, (BYTE *) w)
#define IMS_SETNULL(x,y,z,v) dbsetnull (x->dbproc, y, z, v)

/* Macros for returning query results. */
#define IMS_AFFECTED(x) x->count
#define IMS_DBSSEVERITY(x) x->dbsSeverity
#define IMS_MSGNO(x) x->msgNo
#define IMS_MSGTEXT(x) x->msgText
#define IMS_VALUE(x,y) x->valAddr[y]
#define IMS_VALUELENGTH(x,y) x->valLength[y]
#define IMS_ATTRCOUNT(x) x->attrCount
#define IMS_ATTRNAME(x,y) x->attrName[y]
#define IMS_ATTRNAMELENGTH(x,y) x->attrNameLen[y]
#define IMS_ATTRLENGTH(x,y) x->attrLength[y]
#define IMS_ATTRTYPE(x,y) x->attrType[y]

/* Macros for manipulating query results. */
#define IMS_CONVERT(x, y, u, v, w) dbconvert (x->dbproc, IMS_ATTRTYPE(x, y),\
(BYTE *) IMS_VALUE(x, y), IMS_VALUELENGTH(x, y), u, (BYTE *) v, w)

/*
** Macros for getting stored procedure return parameters.
** Note:  The valAddr/len arrays are re-used for return values. Remember,
** when the return values are available, there are no more rows for this
** particular part of the command batch.  Since macros are used for access,
** the internal storage of return values can be changed at any time.
*/
#define IMS_HASRETSTAT(x) x->hasretstat
#define IMS_PROCRETURN(x) x->procReturn
#define IMS_RETVALUECOUNT(x) x->retValueCount
#define IMS_RETVALUE(x,y) x->valAddr[y]
#define IMS_RETVALUELENGTH(x,y) x->valLength[y]
#define IMS_RETVALUENAME(x,y) x->attrName[y]
#define IMS_RETVALUETYPE(x,y) x->attrType[y]
#define IMS_RETVALUENTS(x,y,z,w) \
dbconvert (x->dbproc, IMS_RETVALUETYPE(x,y), (BYTE *) IMS_RETVALUE(x,y),\
IMS_RETVALUELENGTH(x,y), SYBCHAR, z, w); z[IMS_RETVALUELENGTH(x,y)] = '\0'

/*
** Special macros for controlling NTS numerical conversions.  The first
** two set the precision that reals and floating point numbers use.
** Note:  The default for reals is 2 (domain > 0, breaks down at 7),
**        and the default for doubles is 2, (domain > 0, suns,
**        breaks down at 16).  Note: The precision equals the fractional
**        portion of the float display.  If the display is set to
**        exponential format, then the precision really is the precision,
**        so you might want to change the setting.  Note: the application
**        must do its own domain checking.  Domain violations will result
**        in undefined results.
*/
#define IMS_DEFAULT_REAL_PREC 2
#define IMS_DEFAULT_FLT_PREC  2

/*
** Overhead for Reals and Floats.  For eg. -1.23456e+245,
** where precision + overhead = length, NOT including '\0'.
*/
#define IMS_RF_OVERHEAD 7 + 5
#define IMS_SET_RF_MAX_WIDTH(x,y) x->fltMaxWidth = y
#define IMS_SET_REAL_PRECISION(x,y) x->realPrec = y
#define IMS_SET_FLT_PRECISION(x,y) x->fltPrec = y

/* Domain (REALFLTDISPLAY): { 'f', 'g', 'G', 'e', 'E' }. */
#define IMS_SET_REALFLTDISPLAY(x,y) x->floatConv = y

/*
** How to justify numbers, tinyints to floats when converting to
** strings.  Default: NUMRIGHT.
*/
#define IMS_SET_NUMRIGHT(x) x->numLeft = IMS_OFF
#define IMS_SET_NUMLEFT(x) x->numLeft = IMS_ON

/* Other macros. */
#define IMS_SET_VERBOSE(x,y) x->verboseLevel = y
#define IMS_SET_TIMEOUT(x,y) x->timeout = y
#define IMS_CONNECTED(x) (x)->connected
#define IMS_GET_MSGDESC(x) x->msgDesc
#define IMS_ISCOERCED(x) x->coerced
#define IMS_SETPACKET(x,y) x->maxNetBuf = y
#define IMS_SET_USERDATA(x) \
(void) dbsetuserdata (x->dbproc, (BYTE *) x->msgDesc)

/*
** Define BIND return status.
*/
#define IMS_FAILED FAILED
#define IMS_SUCCEED SUCCEED

/*
** These are the variable bind types for the SYBASE definitions.
** These may be redefined for other RDBMS systems, if needed.
*/
#define	IMS_CHARBIND          CHARBIND
#define	IMS_STRINGBIND        STRINGBIND
#define	IMS_NTBSTRINGBIND     NTBSTRINGBIND
#define	IMS_VARYCHARBIND      VARYCHARBIND
#define	IMS_BINARYBIND        BINARYBIND
#define	IMS_VARYBINBIND       VARYBINBIND
#define	IMS_TINYBIND          TINYBIND
#define	IMS_SMALLBIND         SMALLBIND
#define	IMS_INTBIND           INTBIND
#define	IMS_FLT8BIND          FLT8BIND
#define	IMS_REALBIND          REALBIND
#define	IMS_BITBIND           BITBIND
#define	IMS_DATETIMEBIND      DATETIMEBIND
#define	IMS_SMALLDATETIMEBIND SMALLDATETIMEBIND
#define	IMS_MONEYBIND         MONEYBIND
#define	IMS_SMALLMONEYBIND    SMALLMONEYBIND
#define IMS_BOUNDARYBIND      BOUNDARYBIND
#define IMS_SENSITIVITYBIND   SENSITIVITYBIND

/*
** These are the server types for the Sybase definitions.
** These may be redefined for other RDBMS systems, if needed.
*/
#define	IMS_CHAR              SYBCHAR
#define	IMS_TEXT              SYBTEXT
#define	IMS_BINARY            SYBBINARY
#define	IMS_IMAGE             SYBIMAGE
#define	IMS_TINYINT           SYBINT1
#define	IMS_SMALLINT          SYBINT2
#define	IMS_INT               SYBINT4
#define	IMS_FLT8              SYBFLT8
#define	IMS_REAL              SYBREAL
#define	IMS_BIT               SYBBIT
#define	IMS_DATETIME          SYBDATETIME
#define	IMS_SMALLDATETIME     SYBDATETIME4
#define	IMS_MONEY             SYBMONEY
#define	IMS_SMALLMONEY        SYBMONEY4
#define IMS_BOUNDARY          SYBBOUNDARY
#define IMS_SENSITIVITY       SYBSENSITIVITY

/*
** The Query Interface descriptor structure.
*/
typedef struct ims_qiDesc
{
	IMS_MSG_STRUCT *msgDesc;    /* Message Facility Descriptor. */
	/*
	** Supplied by client process.
	*/
	char username[IMS_NAME_LEN+1];
	char password[IMS_NAME_LEN+1];
	char program[IMS_PROGRAM_LEN+1];
	char server[IMS_NAME_LEN+1];
	char dbname[IMS_NAME_LEN+1];
	short bcp;                  /* Allow bulk copy operations. (TRUE or FALSE) */
	short maxNetBuf;            /* Change the size of the network buffer. */
	short timeout;              /* How long to wait for server response. */
	char *cmd;
	/*
	** Defined by the client process.
	*/
	short coerceToNTS;          /* IMS_TRUE or IMS_FALSE (default) */
	short fltMaxWidth;
	unsigned char fltPrec;      /* Display precision of floats */
	unsigned char realPrec;     /* Display precision of reals */
	char floatConv;             /* The floating point conversion character. */
	short numLeft;              /* NTS justification of numbers. */
	char *bufferCount;          /* Number of rows to buffer. (default "512") */
	char *fltFmtStr;            /* Format string for reals. */
	char *realFmtStr;           /* Format string for floats. */

	DBPROCESS *dbproc;          /* Connection to target database server. */
	short attrCount;            /* Number of attributes returned */
	unsigned char verboseLevel; /* Just how much do we show? */
	/*
	** Supplied by function ims_qiNextRow().
	*/
	char **valAddr;             /* Ptr to array of addresses of values */
	long *valLength;            /* Ptr to array of lengths of values */
	/*
	** Supplied by function ims_qiTblDesc().
	*/
	char **attrName;            /* Ptr to array of attribute names */
	short *attrNameLen;         /* Ptr to array of attribute name lengths. */
	long *attrLength;           /* Ptr to array of max value lengths */
	short *attrType;            /* Ptr to array of attribute types. */
	int count;                  /* Number of rows affected. */
	int msgNo;                  /* Sybase message number. */
	int dbsSeverity;            /* Sybase error severity. */
	char *msgText;              /* Sybase message text. */
	short connected;            /* Server connection? (TRUE or FALSE) */
	short hasretstat;           /* Procedure return status? (TRUE or FALSE) */
	int procReturn;             /* Return status from execution of procedure. */
	int retValueCount;          /* Count of return values from exec call. */
	/*
	** Internal data storage.  Used only by internal functions and are
	** not part of the interface.
	*/
	enum IMS_QSTATES qState;    /* Current state of query. */
	short coerced;              /* Internal flag, protected from user. */
	int queryIdx;               /* Internal accounting. */
} IMS_QI_DESC_OBJ;

/*
** Function Prototype for the ims_qi.c module.
*/
#ifndef __cplusplus
IMS_QI_DESC_OBJ *ims_qiDescAlloc (IMS_MSG_STRUCT *);
int ims_qiTblDesc (IMS_QI_DESC_OBJ *);
int ims_qiNextRow (IMS_QI_DESC_OBJ *);
void ims_qiExit (void);
int ims_qiCancel (IMS_QI_DESC_OBJ *);
int ims_qiCancelAll (void);
int ims_qiResetDesc (IMS_QI_DESC_OBJ *);
int ims_qiFreeDesc (IMS_QI_DESC_OBJ *);
int ims_qiLogoff (IMS_QI_DESC_OBJ *);
int ims_qiLogin (IMS_QI_DESC_OBJ *);
int ims_getResultsStatus (IMS_QI_DESC_OBJ *);
void ims_qiLastMsg (IMS_QI_DESC_OBJ *);
#else
extern "C" IMS_QI_DESC_OBJ *ims_qiDescAlloc (IMS_MSG_STRUCT *);
extern "C" int ims_qiTblDesc (IMS_QI_DESC_OBJ *);
extern "C" int ims_qiNextRow (IMS_QI_DESC_OBJ *);
extern "C" void ims_qiExit (void);
extern "C" int ims_qiCancel (IMS_QI_DESC_OBJ *);
extern "C" int ims_qiCancelAll (void);
extern "C" int ims_qiResetDesc (IMS_QI_DESC_OBJ *);
extern "C" int ims_qiFreeDesc (IMS_QI_DESC_OBJ *);
extern "C" int ims_qiLogoff (IMS_QI_DESC_OBJ *);
extern "C" int ims_qiLogin (IMS_QI_DESC_OBJ *);
extern "C" int ims_getResultsStatus (IMS_QI_DESC_OBJ *);
extern "C" void ims_qiLastMsg (IMS_QI_DESC_OBJ *);
#endif	/* !__cplusplus */

#endif	/* !_IMS_QI_H */
