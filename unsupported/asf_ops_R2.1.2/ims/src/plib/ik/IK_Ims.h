/* File: $RCSfile$
 * 
 * Synopsis: structures, macros and manifest constants used across all IMS
 * programs.
 * 
 * Notes: ANSI C code */
/*--------------------------------------------------
 | $Log$
 | Revision 1.1  2004/02/03 03:32:54  pdenny
 | Initial revision
 |
 * Revision 5.0  1995/11/06  13:03:46  ims
 * PROMOTION FROM GUI_4_5_8_951030 FOR GUI_PROMO_5_0_951106
 *
 * Revision 4.5  1995/07/27  18:42:21  ims
 * PROMOTION FROM GUI_4_4_8_950501 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.4.1.1  1995/02/13  22:35:53  ims
 * COPIED FROM 4.3.1.1 FOR GUI_PROMO_4_4_950213
 *
 * Revision 4.3.1.1  1994/08/26  11:54:43  ims
 * COPIED FROM REVISION 4.0.1.10.
 *
 * Revision 4.0.1.10  1994/07/25  15:56:24  winter
 * Changed #ifndef FILENAME_MAX to #ifdef SUNOS413 for conditional definition
 * of FILENAME_MAX.
 *
 * Revision 4.0.1.9  1994/06/21  20:56:47  winter
 * Added #define free ims_free.
 *
 * Revision 4.0.1.8  1994/06/20  19:59:09  winter
 * Changed ansi_realloc() to ims_realloc().
 *
 * Revision 4.0.1.7  1994/06/20  16:05:46  kerbel
 * Added conditional #define for FILENAME_MAX for port to SunOS 4.1.3
 *
 * Added #define realloc to ansi_realloc which will ensure that
 * our ansi complient realloc is used instead of the default provided
 * by the system which may not be complient with the standard.
 *
 * Revision 4.0.1.6  1994/06/16  11:41:07  winter
 * Changed REVNAME to CHUI_REV4.
 *
 * Revision 4.0.1.5  1994/06/15  11:38:25  winter
 * Changed REVNAME to BUILD_CHUI_94JUN15.
 *
 * Revision 4.0.1.4  1994/06/14  20:44:38  vaffajou
 * Changed REVNAME from BUILD_CHUI_94JUN10 to BUILD_94JUN14 on welcome screen.
 *
 * Revision 4.0.1.3  1994/06/14  20:31:20  vaffajou
 * Fixed the REVNAME from REV4.0 to BUILD_CHUI_94JUN10 for the 6/10 chui rebuild
 *
 * Revision 4.0.1.2  1994/06/14  19:05:11  ryan
 * removed hard-coded definition for REVNAME.
 *
 * Revision 4.0.1.1  1994/06/14  19:03:48  ryan
 * new branch
 *
 * Revision 4.0  1994/06/08  17:10:23  ims
 * PROMOTION FROM REV4
 *
 * Revision 3.2.1.8  1994/06/07  18:56:26  ryan
 * hacked to set REVNAME to "REV4.0" (which RCS wouldn't like).
 *
 * Revision 3.2.1.7  1994/06/07  18:28:39  vaffajou
 * Changed REVNAME from BUILD_94JUN01 to REV4.0
 *
 * Revision 3.2.1.6  1994/06/03  17:34:38  ryan
 * SMR #452
 * changed handling of --with-revname so that it will use a
 * default value from IK_Ims.h.
 *
 * Revision 3.2.1.5  1994/05/31  22:03:47  vaffajou
 * Changed the size of IK_MAXBUFLEN from 80 to 300
 *
 * Revision 3.2.1.4  1994/05/18  12:09:01  ryan
 * removed definition of FUNCPROTO as per CCB decision.
 *
 * Revision 3.2.1.3  1994/05/10  16:15:01  winter
 * Un-commented definitions of GAEA_LOGIN and GAEADATA_DIR macros so that
 * the ChUI would compile on 5/10/94.
 *
 * Revision 3.2.1.2  1994/05/10  15:54:07  kerbel
 * Added a macro defineition for GAEATMP_DIR to be used by IK_SysConfig
 * if GAEATMP_DIR environment variable is not defined
 *
 * Revision 3.2.1.1  1994/05/10  14:03:02  kerbel
 * new branch
 *
 * Revision 3.2  1994/05/03  20:11:29  ims
 * Promotion from BUILD_94MAY03.
 *
 * Revision 3.1.1.3.1.1  1994/04/19  19:05:58  winter
 * Eric and Shiva transplanted change from 3.0.1.4: changed PROFILE_FILE
 * to "profile2.odl".
 *
 * Revision 3.1.1.3  1994/04/19  14:49:29  ryan
 * Moved some commandline definitions to header file.
 * Added a few more comments.
 *
 * Revision 3.1.1.2  1994/04/07  17:57:55  ryan
 * added PROTOCOL_VERSION.
 * removed DBMALLOC stuff.
 *
 * Revision 3.1.1.1  1994/04/06  19:50:12  ryan
 * new branch
 *
 * Revision 3.1  1994/04/05  09:49:52  ims
 * Promotion from MAR28_RELEASE
 *
 * Revision 3.0.1.3  1994/03/24  20:57:09  ryan
 * changed HELP_FILE definition.
 *
 * Revision 3.0.1.2  1994/02/22  14:10:26  ryan
 * #included config file from Autoconf.  This file should be included
 * everywhere.
 *
 * Revision 3.0.1.1  1994/02/02  15:45:17  ryan
 * make changes related to use of Autoconf
 *
 * Revision 3.0  1993/12/29  15:18:49  winter
 * ALPHA-1
 *
 | Revision 1.20  1993/08/10  21:12:04  sylvain
 | added SEARCH_DIR define to be the directory (off of $USRDATA_DIR)
 | 	that contains the saved searches.
 |
 | Revision 1.19  1993/06/28  20:55:45  sylvain
 | changed NVAlIDS to all upercase (NVALIDS).  This is for building the tag
 | databases
 |
 | Revision 1.18  1993/02/19  16:51:19  sylvain
 | added definitions for the dependant valids tag file
 |
 | Revision 1.17  1993/01/11  15:32:41  sylvain
 | added the dbmalloc.h header file to check for malloc errors when DBMALLOC is
 | defined
 |
 | Revision 1.16  1993/01/06  23:21:34  sylvain
 | added definition for dependant valids gdbm file
 |
 | Revision 1.15  1992/10/26  16:34:50  honce
 | Added PACKAGE_FILE and ORDER_FILE
 |
 | Revision 1.14  1992/10/22  00:31:51  honce
 | Added prototype of IK_Exit() and define for NOTICE_FILE
 |
 | Revision 1.13  1992/10/21  18:11:25  sylvain
 | added the filename that contains the image filenames (IMAGEID_FILE)
 |
 | Revision 1.12  1992/10/21  15:49:56  honce
 | Added IK_vDPRINTF() to support IK_vSyslog().  Added IK_Boolean as well as
 | IK_BOOLEAN which was defined earlier.
 |
 | Revision 1.11  1992/10/02  14:48:20  sylvain
 | took out definitions for IMS and IMS_LOGIN
 |
 | Revision 1.10  1992/10/01  18:12:37  sylvain
 | moved the CPP define to IK_Ingest.h
 |
 | Revision 1.9  1992/10/01  17:41:51  honce
 | Corrected spelling of IK_SysConfig().
 |
 | Revision 1.8  1992/10/01  16:53:25  honce
 | Added more filename macros.  Added login id macro.  Rearranged file
 | so that tweekables were at top, while nonservice-ables were at the
 | bottom.
 |
 | Revision 1.7  1992/09/30  19:34:09  honce
 | Added prototype for IK_SysConf().
 |
 | Revision 1.6  1992/09/28  19:42:20  sylvain
 | #defined the odl label file for input to IK_GetArchive()
 | (DATA_CENTER_FILE)
 |
 | Revision 1.5  1992/09/28  13:13:11  sylvain
 | moved the definitions for cpp and IMS_LOGIN into this file from IK_Msg.h
 | so that people can find out where the ims account is
 |
 | Revision 1.4  1992/09/22  20:49:54  honce
 | IK_Archive stuff back
 |
 | Revision 1.2  1992/09/22  20:28:18  honce
 | file purpose should have been file synopsis.  All orphan typedef's and
 | function prototypes gathered to this file.
 |
 | Revision 1.1  1992/09/11  14:40:38  honce
 | Put all easy tweekables in this file.
 |
 | Revision 1.0  1992/08/31  08:45:42  sylvain
 | added some more debugging definitions to allow function calls dependant on
 | whether we're in debug mode or not.  Added the definition EOSDIS to get
 | the correct function prototypes from the ODL code to write ODL messages
 | to the syslog file.
 |
 | Revision 0.5  92/08/25  10:11:39  sylvain
 | initail header file needed for the message passing code.
 |
 +------------------------------------------------*/
/*-------------------------------------------------------------------------
 | Modification History - pre RCS
 |
 | 1992/08/13 JWH       Renamed definitions into IK_ namespace.
 | 1992/08/12 GMS       Added BOOLEAN typedef, changed DPRINTF to goto
 |                         IK_Syslog instead of stdout
 | 1991/08/27 JWH	Authored
 +-----------------------------------------------------------------------*/

#ifndef	__IMS_H__
#define	__IMS_H__

#ifdef Comment
static const char *rcsid =
"$Id$";
#endif

/* See if we can include the Autoconf configuration file */
#ifdef HAVE_CONFIG_H
#include "Gaea_config.h"
#endif /* HAVE_CONFIG_H */

/* Set a default value for the revision name.
 * This needs to be updated for *every* build. */

/* A hack until we come up with something better.  -pmr */
#ifndef REVNAME
#  define REVNAME "CHUI_REV4"
#endif /* !REVNAME */


/*---------------------------------------------------------------*/
/*--------------------  Should tweek "well"  --------------------*/
/*---------------------------------------------------------------*/

#ifndef NARCHIVES		/* Max no. of archives supported */
#   define NARCHIVES	255	/* 0 reserved */
#define IK_ARCHIVE_TAG_SZ	1	/* should be derived from
					 * NARCHIVES, but cpp is too
					 * stupid */
#endif

#ifndef NDATASETS		/* Max no. of datasets supported */
#   define	NDATASETS	65535	/* 0 reserved */
#define	IK_DATASET_TAG_SZ	2	/* should be derived from
					 * NDATASETS, but cpp is too
					 * stupid */
#endif

#ifndef NVALIDS
#   define      NVALIDS         NDATASETS
#   define      IK_VALID_TAG_SZ 4


#endif


/* The following dozen lines or so are deprecated due to the
 * new code which knows about a default location for the client's
 * support files.
 * -pmr
 */

/* The following define the name of files that Gaea reads information
 * from.  These files are shared between all the Gaea users
 * 
 * The default path is: ~GAEA_LOGIN/GAEADATA_DIR/file */

#ifndef GAEA_LOGIN
#   define	GAEA_LOGIN	"ims"
#endif

#ifndef GAEADATA_DIR
#   define	GAEADATA_DIR	"ims_data"
#endif

#ifndef ARCHIVETAG_FILE
#   define ARCHIVETAG_FILE	"tagarch.gdb"
#endif

#ifndef DATASETTAG_FILE
#   define DATASETTAG_FILE	"tagdset.gdb"
#endif

#ifndef VALIDTAG_FILE
#   define VALIDTAG_FILE	"tagdval.gdb"
#endif

#ifndef DATA_CENTER_FILE
#   define DATA_CENTER_FILE	"archive.odl"
#endif

#ifdef GUI_CLIENT
#   define HELP_FILE	"gui_help.gdb"
#endif /* GUI_CLIENT */

#ifdef CHUI_CLIENT
#   define HELP_FILE	"chui_help.gdb"
#endif /* CHUI_CLIENT */

#ifndef HELP_FILE
#   define HELP_FILE	"help.gdb"
#endif /* default help file name */

#ifndef MESSAGE_FILE
#   define MESSAGE_FILE	"message.gdb"
#endif

#ifndef NOTICE_FILE
#   define NOTICE_FILE	"notice.txt"
#endif

#ifndef PACKAGE_FILE
#   define PACKAGE_FILE	"package.gdb"
#endif

#ifndef VALID_FILE
#   define VALID_FILE	"valids.gdb"
#endif

#ifndef DEP_VALID_FILE
#   define DEP_VALID_FILE	"depvalid.gdb"
#endif

/*
 *  Define the protocol version.
 */

#ifndef PROTOCOL_VERSION
#  define PROTOCOL_VERSION 3.2
#endif /* !PROTOCOL_VERSION */


/* The following define the name of files that Gaea reads and writes
 * information from/to.  These files are unique to each the Gaea users
 * 
 * The default path for temporary files is: $HOME/GAEATMP_DIR/file
 * The default path for saved searches and results is: $HOME/USRDATA_DIR/file*/

#ifndef GAEATMP_DIR
#   define      GAEATMP_DIR     "gaea_tmp"
#endif

#ifndef	USRDATA_DIR
#   define	USRDATA_DIR	"gaea_data"
#endif

/* The following defines the place that gaea reads and writes search 
 * criteria files to/from.
 * This is a subdirectory of USRDATA_DIR */

#ifndef SEARCH_DIR
#   define      SEARCH_DIR      "searches"
#endif

#ifndef COMMENT_FILE
#   define COMMENT_FILE	"comment.odl"
#endif

#ifndef DATASET_FILE
#   define DATASET_FILE	"dataset.gdb"
#endif

#ifndef GRANULE_FILE
#   define GRANULE_FILE	"granule.gdb"
#endif

#ifndef IMAGEID_FILE
#   define IMAGEID_FILE  "hdfnames.gdb"
#endif				/* !IMAGEID_FILE */

#ifndef ORDER_FILE
#   define ORDER_FILE    "order.gdb"
#endif

/* this is needed for port to SunOS 4.1.3 */
#ifdef SUNOS413
#define FILENAME_MAX 255
#endif /* SUNOS413 */


/*
 * Moved these definitions from the command line to here.
 * -pmr
 */

#ifndef EOSDIS
#  define EOSDIS
#endif /* EOSDIS */

#ifndef IK_DEPVALID
#  define IK_DEPVALID
#endif /* IK_DEPVALID */

/* because some platforms don't have ansi compliant realloc we use */
/* our own which ensures correct handling of using a NULL pointer  */
/* as the base address. Ditto for free(). */
#define realloc ims_realloc
#define free ims_free

/* change User Profile filename to accomodate both Pre-Alpha1 and Alpha1 */
/* versions simulataneously 						a*/

#ifndef PROFILE_FILE
#   define	PROFILE_FILE	"profile2.odl"
#endif

/*---------------------------------------------------------------*/
/*-------------------  No need to tweek below  ------------------*/
/*-------------------     No usable parts      ------------------*/
/*---------------------------------------------------------------*/
#ifdef NDEBUG			/* If no debugging ... */
#   define IK_DPRINTF(n,pri,args)	/* ... don't include debugging
					 * code */
#   define IK_vDPRINTF(n,args)
#   define IK_DSET_LEVEL(n)
#   define IK_DINC_LEVEL(n)
#   define IK_DFUN_CALL(fun)	/* don't make the function call */
#   define IK_NDFUN_CALL(fun)       (void) (fun);	/* make the call, we're
							 * not debugging */
#else				/* else if verbose enough print */
extern int      DebugLevel;

#   define IK_DPRINTF(n,pri,msg) ((DebugLevel >= n) ? \
	(IK_Syslog ((pri),(msg))): (void) 0)
#   define IK_vDPRINTF(n,args) ((DebugLevel >= n) ? \
	(IK_vSyslog args): (void) 0)
#   define IK_DSET_LEVEL(n)   DebugLevel = n;
#   define IK_DINC_LEVEL(n)   DebugLevel += n;
#   define IK_DFUN_CALL(fun)  (void) (fun);	/* it's a debuging
						 * dependant function call */
#   define IK_NDFUN_CALL(fun)	/* don't make the function call */
#endif				/* NDEBUG */

typedef enum {
    IK_FALSE, IK_TRUE
} IK_BOOLEAN   , IK_Boolean;

#ifndef EOSDIS
#   define EOSDIS		/* defined to include the correct def's
				 * from the ODL */
#endif

#ifndef IK_MAXBUFLEN
#   define IK_MAXBUFLEN   300	/* max length of an error buffer */
#endif

/*---------------------------------------------------------------*/
/*-------------  Orphan's not worth another h file  -------------*/
/*---------------------------------------------------------------*/

typedef unsigned char IK_ArchiveTag[IK_ARCHIVE_TAG_SZ];
extern void    *IK_MapArchiveIdent(void *, unsigned int);

extern char    *IK_SysConfig(char *);

extern void     IK_Exit(int);


#endif				/* !__IMS_H__ */
/*-- QED --*/

