/*		Sccsid @(#) oscompat.h 87.1 12/29/93 */

/*
**		Sybase OS-LIBRARY
**		Confidential Property of Sybase, Inc.
**		(c) Copyright Sybase, Inc. 1991 - 1993
**		All rights reserved
**
** oscompat.h	This file maps all defines and typedefs required to maintain
**		backward compatibility.  All defines, typedefs, and routines
**		prototyped in this file are obsolete and will be removed
**		in the next major release of Open Server.
*/
#if	!defined(__OSCOMPAT_H__)
#define __OSCOMPAT_H__

/*
** Undefine DB-Lib typedefs and redefine them to use CS typedefs
*/
#if	defined(DBTINYINT)
#undef	DBTINYINT
#endif	/* DBTINYINT */

#if	defined(DBSMALLINT)
#undef	DBSMALLINT
#endif	/* DBSMALLINT */

#if	defined(DBINT)
#undef	DBINT
#endif	/* DBINT */

#if	defined(DBCHAR)
#undef	DBCHAR
#endif	/* DBCHAR */

#if	defined(DBBINARY)
#undef	DBBINARY
#endif	/* DBBINARY */

#if	defined(DBBIT)
#undef	DBBIT
#endif	/* DBBIT */

#if	defined(DBDATETIME)
#undef	DBDATETIME
#endif	/* DBDATETIME */

#if	defined(DBDATETIME4)
#undef	DBDATETIME4
#endif	/* DBDATETIME4 */

#if	defined(DBMONEY)
#undef	DBMONEY
#endif	/* DBMONEY */

#if	defined(DBMONEY4)
#undef	DBMONEY4
#endif	/* DBMONEY4 */

#if	defined(DBFLT8)
#undef	DBFLT8
#endif	/* DBFLT8 */

#if	defined(DBREAL)
#undef	DBREAL
#endif	/* DBREAL */

#if	defined(BYTE)
#undef	BYTE
#endif	/* BYTE */

#if	defined(DBBOOL)
#undef	DBBOOL
#endif	/* DBBOOL */

#if	defined(RETCODE)
#undef	RETCODE
#endif	/* RETCODE */

#if	defined(BOOL)
#undef	BOOL
#endif	/* BOOL */

#if	defined(DBUSMALLINT)
#undef	DBUSMALLINT
#endif	/* DBUSMALLINT */

#if	defined(DBPROGNLEN)
#undef	DBPROGNLEN
#endif	/* DBPROGNLEN */

#if	defined(FAIL)
#undef	FAIL
#endif	/* FAIL */

#if	defined(SUCCEED)
#undef	SUCCEED
#endif	/* SUCCEED */

#if	defined(DBLOGINFO)
#undef	DBLOGINFO
#endif	/* DBLOGINFO */

#if	defined(TRUE)
#undef	TRUE
#endif	/* TRUE */

#if	defined(FALSE)
#undef	FALSE
#endif	/* FALSE */

/*
** Map old data types and defines to new CS data types and defines
*/
#define	DBTINYINT	CS_TINYINT
#define DBSMALLINT	CS_SMALLINT
#define	DBINT		CS_INT
#define	DBCHAR		CS_CHAR
#define	DBBINARY	CS_BINARY
#define DBBIT		CS_BIT
#define DBDATETIME	CS_DATETIME
#define DBDATETIME4	CS_DATETIME4
#define DBMONEY		CS_MONEY
#define DBMONEY4	CS_MONEY4
#define	DBFLT8		CS_FLOAT
#define DBREAL		CS_REAL
#define DBLOGINFO	CS_LOGINFO
#define BYTE		CS_BYTE
#define DBBOOL		CS_BOOL
#define	BOOL		CS_BOOL
#define	DBUSMALLINT	unsigned short
#define	SRV_NULLTERM	CS_NULLTERM
#define	RETCODE		CS_RETCODE
#define	FAIL		CS_FAIL
#define	SUCCEED		CS_SUCCEED
#define	TRUE		CS_TRUE
#define	FALSE		CS_FALSE


/*
** Define utility function pointers for backward compatibility.
** Take care not to redefine these if sybfront.h has already
** been included.
*/
#if	!defined(__sybfront__)
typedef CS_INT		(CS_PUBLIC * INTFUNCPTR)PROTOTYPE((SRV_PROC *));
typedef CS_VOID		(CS_PUBLIC * VOIDFUNCPTR)PROTOTYPE((SRV_PROC *));
typedef CS_BOOL		(CS_PUBLIC * BOOLFUNCPTR)PROTOTYPE((SRV_PROC *));
#endif	/* __sybfront__ */

/*
** Define the VOIDPTR for backward compatibility.
*/
#if	!defined(SRV__INTERNAL_STRUCTS)
typedef CS_VOID		*VOIDPTR;
#endif	/* SRV__INTERNAL_STRUCTS */


/*
** Defines for Open Server data types.  These defines are only here for
** backward compatibility with old programs.
**
*/
#define	SRVCHAR		(CS_INT)-100
#define	SRVVARCHAR	(CS_INT)-101
#define	SRVBINARY	(CS_INT)-102
#define	SRVVARBINARY	(CS_INT)-103
#define	SRVINT1		(CS_INT)-104
#define	SRVINT2		(CS_INT)-105
#define	SRVINT4		(CS_INT)-106
#define	SRVREAL		(CS_INT)-107
#define	SRVFLT8		(CS_INT)-108
#define	SRVBIT		(CS_INT)-109
#define	SRVDATETIME	(CS_INT)-110
#define	SRVDATETIME4	(CS_INT)-111
#define	SRVMONEY	(CS_INT)-112
#define	SRVMONEY4	(CS_INT)-113
#define	SRVINTN		(CS_INT)-114
#define	SRVDATETIMN	(CS_INT)-115
#define	SRVMONEYN	(CS_INT)-116
#define	SRVFLTN		(CS_INT)-117
#define SRVVOID		(CS_INT)-118


/*
** Obsolete exit values for Open Server applications
*/
#if	!defined(ERREXIT)
#define	ERREXIT		0
#endif	/* ERREXIT */

#if	!defined(STDEXIT)
#define	STDEXIT		1
#endif	/* STDEXIT */

/*
** Obsolete return values for callback routines
*/
#define	SRV_CONTINUE		CS_SUCCEED
#define	SRV_DEBUG		(CS_RETCODE)(CS_SUCCEED + 1)
#define	SRV_TERMINATE		(CS_RETCODE)(CS_SUCCEED + 2)

/*
** Obsolete values for thread priorities. The values
** SRV_C_MAXPRIORITY, SRV_C_DEFPRIORITY, SRV_C_LOWPRIORITY
** specified in ospublic.h should be used.
*/
#define SRV_MAXPRIORITY		16
#define SRV_DEFPRIORITY		8
#define SRV_LOWPRIORITY 	0

#if	defined(POLLIN)
/*
** Map platform specific definitions to Open Server public defines
** for the SRV_POLLFD structure
*/
typedef struct pollfd	SRV_POLLFD;
#define	srv_fd		fd
#define	srv_events	events
#define	srv_revents	revents

#define	SRV_POLLIN	POLLIN
#define	SRV_POLLPRI	POLLPRI
#define	SRV_POLLOUT	POLLOUT
#define	SRV_POLLERR	POLLERR
#define	SRV_POLLHUP	POLLHUP
#define	SRV_POLLNVAL	POLLNVAL
#endif	/* POLLIN */

/* 
** General purpose macros.  These macros have been replaced by the server
** and thread property routines.
*/
#define SRV_CURPROC  		((SRV_PROC *)srv__curproc())
#define	SRV_EVENTDATA(s)	((CS_VOID *)srv__eventdata(s))
#define	SRV_GETCONFIG(s)	((SRV_CONFIG *)srv__getconfig(s))
#define	SRV_GETSERVER(s)	((SRV_SERVER *)srv__getserver(s))
#define SRV_GETVERSION()	((CS_CHAR *)srv__getversion())
#define	SRV_GOT_ATTENTION(s)	((CS_BOOL)srv__got_attention(s))
#define	SRV_IODEAD(s)		((CS_BOOL)srv__iodead(s))
#define	SRV_SITEPROC(s)		((CS_BOOL)srv__siteproc(s))
#define SRV_TDSVERSION(s)	((CS_INT)srv__tdsversion(s))

/* 
** Macros to manipulate bit fields.  These macros have been replaced by the
** srv_mask routine.
*/
#define	SRV_MASK_ZERO(ma)	srv_bzero((CS_VOID *)(ma), CS_SIZEOF(*(ma)))
#define	SRV_MASK_SET(ma,bit)	((ma)->mask_bits[(bit)/(sizeof(unsigned int) \
					* CS_BITS_PER_BYTE)] |= \
					(1 << ((bit) % (sizeof(unsigned int) \
					* CS_BITS_PER_BYTE))))

#define	SRV_MASK_CLEAR(ma,bit)	((ma)->mask_bits[(bit)/(sizeof(unsigned int) \
					* CS_BITS_PER_BYTE)] &= \
					~(1 << ((bit) % (sizeof(unsigned int) \
					* CS_BITS_PER_BYTE))))

#define	SRV_MASK_ISSET(ma,bit)	((ma)->mask_bits[(bit)/(sizeof(unsigned int) \
					* CS_BITS_PER_BYTE)] & \
					(1 << ((bit) % (sizeof(unsigned int) \
					* CS_BITS_PER_BYTE))))

/*
** Macros to check, set, and clear trace bits.  These macros have been replaced
** by the server properties routine.
*/
#define SRV_TRACE(bit)		(srv__trace(CS_GET,bit))
#define SRV_TRACEON(bit)	(srv__trace(CS_SET,bit))
#define SRV_TRACEOFF(bit)	(srv__trace(CS_CLEAR,bit))


/* 
** Defines for `field' argument to srv_pfield().  This functionality has
** been replaced by the thread properties API (srv_thread_props)
*/
#define	SRV_SPID		(CS_INT)100
#define	SRV_NETSPID		(CS_INT)101
#define	SRV_TYPE		(CS_INT)102
#define	SRV_STATUS		(CS_INT)103
#define	SRV_MACHINE		(CS_INT)105
#define	SRV_USER		(CS_INT)107
#define	SRV_PWD			(CS_INT)108
#define	SRV_CPID		(CS_INT)109
#define	SRV_APPLNAME		(CS_INT)110
#define	SRV_RMTSERVER		(CS_INT)111
#define	SRV_TDS			(CS_INT)112
#define	SRV_CLIB		(CS_INT)113
#define	SRV_CLIBVERS		(CS_INT)114
#define SRV_ROWSENT		(CS_INT)115

/* 
** Defines for `field' argument to srv_sfield().  This functionality has
** been replaced by the server properties API (srv_props)
*/
#define	SRV_SERVERNAME		(CS_INT)0
#define	SRV_CONNECTIONS		(CS_INT)1
#define	SRV_PREEMPT		(CS_INT)2
#define	SRV_TIMESLICE		(CS_INT)3
#define	SRV_STACKSIZE		(CS_INT)4
#define	SRV_MSGPOOL		(CS_INT)5
#define	SRV_USEREVENTS		(CS_INT)6
#define	SRV_LOGFILE		(CS_INT)7
#define SRV_SRVPROCS    	(CS_INT)8
#define SRV_REMCONNECTS 	(CS_INT)9
#define SRV_REMSITES    	(CS_INT)10
#define SRV_REMPREREAD  	(CS_INT)11
#define SRV_MSGQUEUES   	(CS_INT)12
#define SRV_MUTEXES     	(CS_INT)13
#define SRV_MAXNETBUF   	(CS_INT)14
#define SRV_VIRTCLKRATE 	(CS_INT)15
#define SRV_VIRTTIMER		(CS_INT)16
#define SRV_CHARSET		(CS_INT)17
#define SRV_SORTORDER		(CS_INT)18

/*
** Symbols used by registered procedure routines.
*/
#define SRV_NOTIFY_ONCE         CS_NOTIFY_ONCE
#define SRV_NOTIFY_ALWAYS       CS_NOTIFY_ALWAYS

/* 
** Trace type definitions for SRV_TRACE macro.  This functionality has
** been replaced by the server properties API (srv_props)
*/
#define SRV__TR_TDSHDR		SRV_TR_TDSHDR
#define SRV__TR_TDSDATA		SRV_TR_TDSDATA 
#define SRV__TR_MSGQ		SRV_TR_MSGQ
#define SRV__TR_ATTN		SRV_TR_ATTN

/*
** Obsolete trace defines
*/
#define SRV__TR_IO		(CS_INT)0x00000000
#define SRV__TR_MSGUTIL		(CS_INT)0x00000000
#define SRV__TR_RUNQ		(CS_INT)0x00000000

/*
** Max server configuration values used by srv_config
*/
#define SRV_MAX_NOMAX           -1
#define SRV_MAX_CONNECTIONS     4096
#define SRV_MAX_TIMESLICE       SRV_MAX_NOMAX
#define SRV_MAX_STACKSIZE       SRV_MAX_NOMAX
#define SRV_MAX_USEREVENTS      4096
#define SRV_MAX_SRVPROCS        (SRV_MAX_CONNECTIONS * 3)
#define SRV_MAX_REMSITES        1024
#define SRV_MAX_REMPREREAD      1024
#define SRV_MAX_REMCONNECTS     SRV_MAX_CONNECTIONS
#define SRV_MAX_MUTEXES         4096
#define SRV_MAX_MAXNETBUF       1000 * 1024
#define SRV_MAX_MSGQUEUES       4096
#define SRV_MAX_MSGMAX          SRV_MAX_NOMAX
#define SRV_MAX_VIRTCLKRATE     SRV_MAX_NOMAX
#define SRV_MAX_LOGSIZE         SRV_MAX_NOMAX

/*
** Default server configuration values used by srv_config
*/
#define SRV_DEF_CONNECTIONS     30
#define SRV_DEF_USEREVENTS      0
#define SRV_DEF_SRVPROCS        50
#define SRV_DEF_REMSITES        10
#define SRV_DEF_REMPREREAD       3
#define SRV_DEF_REMCONNECTS     5
#define SRV_DEF_MUTEXES         128
#define SRV_DEF_MSGQUEUES       (SRV_DEF_SRVPROCS + 128)
#if	NETWARE386
#define SRV_DEF_MSGMAX          (SRV_DEF_MSGQUEUES * 32)
#else
#define SRV_DEF_MSGMAX          (SRV_DEF_MSGQUEUES * 256)
#endif	/* NETWARE386 */
#define	SRV_DEF_MAXNETBUF	2048

/*
** Unused defines for stacksize.  They could not have been used anywhere
** because no 2.0 routines expected them as arguments.
*/
#define SRV_C_MINSTACKSIZE	SRV_MINSTACKSIZE
#define SRV_C_DEFSTACKSIZE	SRV_DEF_STACKSIZE

/*
**  Message types used by srv_sendmsg
*/
#define	SRV_MSG_INFO		(CS_INT)1
#define	SRV_MSG_ERROR		(CS_INT)2

/*
** Maximum length of messages supported by srv_sendmsg and srv_log
*/
#define	SRV_MAXMSG		CS_MAX_MSG

/*
** Maximum length of an object name
*/
#define SRV_MAXNAME		CS_MAX_NAME

/*
** Bit field define for srv_paramstatus that indicates that this is
** a return parameter.  This flag is also used by srv_rpcoptions to indicate
** that this RPC requested recompilation before execution
*/
#define	SRV_PARAMRETURN		(CS_INT)0x01

/*
** Macro to determine if sigvec() is supported on this platform.
** If it is, we will PROTOTYPE srv_sigvec().  
**
** 1/6/95 - S. Hardman
** Removed the statement where USE_SIGVEC is defined to 0 if
** the SV_* defines are not defined. The problem is that all
** we do is check to see if is defined which is always true
** in this case.
*/
#if	(defined(SV_ONSTACK) || defined(SV_INTERRUPT))
#define	USE_SIGVEC	1
#endif	/* (defined(SV_ONSTACK) || defined(SV_INTERRUPT)) */

/*
** Open Server prototypes for routines that are obsolute and will be 
** removed from the Open Server library in the next release
*/

CS_START_EXTERN_C

extern SRV_CONFIG * CS_PUBLIC srv_config_alloc PROTOTYPE((void));
extern CS_RETCODE CS_PUBLIC srv_config PROTOTYPE((
	SRV_CONFIG *config,
	CS_INT option,
	CS_CHAR *value,
	CS_INT valuelen
	));
extern CS_BOOL CS_PUBLIC srv_willconvert PROTOTYPE((
	CS_INT srctype,
	CS_INT desttype
	));
extern CS_INT CS_PUBLIC srv_convert PROTOTYPE((
	SRV_PROC *srvproc,
	CS_INT srctype,
	CS_VOID *src,
	CS_INT srclen,
	CS_INT desttype,
	CS_VOID *dest,
	CS_INT destlen
	));
extern CS_INT CS_PUBLIC	srv_describe PROTOTYPE((
	SRV_PROC *srvproc,
	CS_INT colnumber,
	CS_CHAR *colname,
	CS_INT namelen,
	CS_INT desttype,
	CS_INT destlen,
	CS_INT srctype,
	CS_INT srclen,
	CS_VOID *srcdata
	));
extern CS_RETCODE CS_PUBLIC srv_setuserdata PROTOTYPE((
	SRV_PROC *srvproc,
	CS_VOID *ptr
	));
extern CS_VOID * CS_PUBLIC srv_getuserdata PROTOTYPE((
	SRV_PROC *srvproc
	));
extern CS_VOID * CS_PUBLIC srv_paramdata PROTOTYPE((
	SRV_PROC *srvproc,
	CS_INT	n
	));
extern CS_INT CS_PUBLIC srv_paramlen PROTOTYPE((
	SRV_PROC *srvproc,
	CS_INT	n
	));
extern CS_INT CS_PUBLIC srv_parammaxlen PROTOTYPE((
	SRV_PROC *srvproc,
	CS_INT n
	));
extern CS_CHAR * CS_PUBLIC srv_paramname PROTOTYPE((
	SRV_PROC *srvproc,
	CS_INT n,
	CS_INT *namelen
	));
extern CS_RETCODE CS_PUBLIC srv_paramset PROTOTYPE((
	SRV_PROC *srvproc,
	CS_INT	 n,
	CS_VOID *data,
	CS_INT	 len
	));
extern CS_INT CS_PUBLIC srv_paramnumber PROTOTYPE((
	SRV_PROC *srvproc,
	CS_CHAR *name,
	CS_INT namelen
	));
extern CS_INT CS_PUBLIC srv_paramstatus PROTOTYPE((
	SRV_PROC *srvproc,
	CS_INT n
	));
extern CS_INT CS_PUBLIC srv_paramtype PROTOTYPE((
	SRV_PROC *srvproc,
	CS_INT n
	));
extern CS_CHAR * CS_PUBLIC srv_pfield PROTOTYPE((
	SRV_PROC *srvproc,
	CS_INT	field,
	CS_INT	*len
	));
extern CS_RETCODE CS_PUBLIC srv_returnval PROTOTYPE((
	SRV_PROC *srvproc,
	CS_CHAR *valuename,
	CS_INT namelen,
	CS_INT status,
	CS_INT type,
	CS_INT maxlen,
	CS_INT datalen,
	CS_VOID *value
	));
extern CS_INT CS_PUBLIC srv_rpcparams PROTOTYPE((
	SRV_PROC *srvproc
	));
extern CS_RETCODE CS_PUBLIC srv_sendmsg PROTOTYPE((
	SRV_PROC *srvproc,
	CS_INT msgtype,
	CS_INT error,
	CS_INT severity,
	CS_INT state,
	CS_CHAR *rpcname,
	CS_INT rpcnamelen,
	CS_INT linenum,
	CS_CHAR *msg,
	CS_INT msglen
	));
extern CS_RETCODE CS_PUBLIC srv_sendrow PROTOTYPE((
	SRV_PROC *srvproc
	));
extern CS_RETCODE CS_PUBLIC srv_setcoldata PROTOTYPE((
	SRV_PROC *srvproc,
	CS_INT column,
	CS_VOID *data
	));
extern CS_RETCODE CS_PUBLIC srv_setcollen PROTOTYPE((
	SRV_PROC *srvproc,
	CS_INT column,
	CS_INT len
	));
extern CS_RETCODE CS_PUBLIC srv_setcolutype PROTOTYPE((
	SRV_PROC *srvproc,
	CS_INT column,
	CS_INT utype
	));
extern CS_RETCODE CS_PUBLIC srv_setustate PROTOTYPE((
	SRV_PROC *spp,
	CS_CHAR *state,
	CS_INT statelen
	));
extern CS_CHAR * CS_PUBLIC srv_sfield PROTOTYPE((
	SRV_SERVER *server,
	CS_INT field,
	CS_INT *len
	));
extern CS_VOID CS_PUBLIC srv_setifile PROTOTYPE((
	CS_CHAR *filename,
	CS_INT	namelen
	));
#if	defined(USE_SIGVEC)
extern CS_INT CS_PUBLIC srv_sigvec PROTOTYPE((
	CS_INT sig,
	struct sigvec *vec,
	struct sigvec *ovec
	));
#endif	/* (USE_SIGVEC) */
extern SRV_PROC * CS_PUBLIC srv__curproc PROTOTYPE((void));
extern CS_VOID * CS_PUBLIC srv__eventdata PROTOTYPE((
	SRV_PROC *sp
	));
extern SRV_CONFIG * CS_PUBLIC srv__getconfig PROTOTYPE((
	SRV_SERVER *server
	));
extern SRV_SERVER * CS_PUBLIC srv__getserver PROTOTYPE((
	SRV_PROC *sp
	));
extern CS_CHAR * CS_PUBLIC srv__getversion PROTOTYPE((void));
extern CS_BOOL CS_PUBLIC srv__got_attention PROTOTYPE((
	SRV_PROC *sp
	));
extern CS_BOOL CS_PUBLIC srv__iodead PROTOTYPE((
	SRV_PROC *sp
	));
extern CS_BOOL CS_PUBLIC	srv__siteproc PROTOTYPE((
	SRV_PROC *sp
	));
extern CS_INT CS_PUBLIC srv__tdsversion PROTOTYPE((
	SRV_PROC *sp
	));
extern CS_INT CS_PUBLIC srv__trace PROTOTYPE((
	CS_INT action,
	CS_INT val
	));
extern CS_BOOL CS_PUBLIC srv__mask PROTOTYPE((
	CS_INT cmd,
	SRV_MASK_ARRAY *mp,
	CS_INT bit
	));
CS_END_EXTERN_C		
#endif	/* __OSCOMPAT_H__ */
