/*
 * Name: IK_Errno.h
 *
 * Description: This file contains data structures and manifest
 * constants for IK_ImsErrno.
 * 
 * Notes:
 *
 * 1) *NEVER* *NEVER* *NEVER* run indent on this file!
 *
 *-------------------------------------------------------------------------
 *
 * RCS information:
 *
 * $RCSfile$
 *
 * $Id$
 *
 * $Log$
 * Revision 1.1  2004/02/03 03:32:54  pdenny
 * Initial revision
 *
 * Revision 5.0.1.1  1995/11/06  13:12:24  ims
 * COPIED FROM 4.5.1.1 FOR GUI_PROMO_5_0_951106
 *
 * Revision 4.5.1.1  1995/09/25  15:18:49  leonid
 * Changed IK_EBAD_PROCLVL from 113 to 153 to prevent a duplicate case compiler
 * warning caused by having the same value as EHOSTUNREACH.
 *
 * Revision 4.5  1995/07/27  18:42:06  ims
 * PROMOTION FROM GUI_4_4_8_950501 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.4  1995/02/13  22:20:02  ims
 * PROMOTION FROM GUI_4_4_950106 FOR GUI_PROMO_4_4_950213
 *
 * Revision 4.3  1994/08/26  10:49:22  ims
 * COPIED FROM GUI_4_3_940824.
 *
 * Revision 4.0.1.3  1994/08/15  18:12:16  heather
 * added IK_ENOGCMDBASE
 *
 * Revision 4.0.1.2  1994/06/20  19:23:08  ryan
 * put QED back in at the bottom of the file since IK_InMsg.c expects it.
 *
 * Revision 4.0.1.1  1994/06/16  20:06:40  winter
 * Fixed bug for error code 0 - comment was missing.
 *
 * Revision 4.0  1994/06/08  17:04:51  ims
 * PROMOTION FROM REV4
 *
 * Revision 3.2.1.1  1994/05/04  19:19:14  winter
 * COPIED FROM REVISION 3.1.1.2
 *
 * Revision 3.1.1.2  1994/04/19  16:40:04  winter
 * Re-added changes from revision 3.0.1.1 and 3.1.0.1 (IK_ENOSAVE,
 * IK_ENOFILE, and IK_SAVE_SEARCH).
 *
 * NOTE: THIS IS THE CURRENTLY DEFINITIVE VERSION OF THIS FILE!
 *
 * Revision 3.1.1.1  1994/04/18  13:00:23  winter
 * Extensive code cleanup by Eric Winter.
 *
 * Revision 3.1  1994/04/05  09:49:52  ims
 * Promotion from MAR28_RELEASE
 *
 * Revision 3.0  1993/12/29  15:18:49  winter
 * ALPHA-1
 *
 * Revision 1.35  1993/08/13  19:34:09  sylvain
 * added IK_EOUTDATED and IK_EALLDEL for the scroll and mark routines.
 *
 * Revision 1.34  1993/07/22  17:36:50  sylvain
 * speel checked the error messages
 *
 * Revision 1.33  1993/02/26  19:48:27  sylvain
 * re-added IK_ENO_DIF and IK_ENO_DAAC
 *
 * Revision 1.33  1993/02/26  19:43:16  sylvain
 * re-added IK_ENO_DIF and IK_ENO_DAAC
 *
 * Revision 1.32  1993/02/26  19:26:16  sylvain
 * added IK_FATAL, IK_BUILD_VALID and changed IK_EBAD_VALID to reflec
 * dependant valids list
 *
 * Revision 1.29  1992/11/24  18:53:01  sylvain
 * added IK_ENO_GRANULE, there was no granule selected
 *
 * Revision 1.28  1992/11/18  16:52:58  sylvain
 * changed the IK_EBAD_HELP message
 *
 * Revision 1.27  1992/11/17  16:52:21  sylvain
 * added IK_ENO_BROWSE
 *
 * Revision 1.26  1992/11/11  21:14:30  sylvain
 * added IK_SEARCH_RUNNING
 *
 * Revision 1.25  1992/11/10  15:42:57  sylvain
 * added IK_ENO_DATASETS
 *
 * Revision 1.24  1992/11/09  17:09:52  sylvain
 * added IK_ENO_ADDR
 *
 * Revision 1.23  1992/11/02  21:15:28  sylvain
 * add IK_ENO_HITS
 *
 * Revision 1.22  1992/11/02  21:14:20  sylvain
 * *** empty log message ***
 *
 * Revision 1.21  1992/11/02  19:48:28  sylvain
 * added IK_ENO_SORT
 *
 * Revision 1.20  1992/11/02  15:03:09  sylvain
 * fixed some error messages to appease Heather :-)
 *
 * Revision 1.19  1992/10/23  14:26:10  sylvain
 * added IK_EATNOMORE for heather.
 *
 * Revision 1.18  1992/10/20  23:11:06  sylvain
 * remove the "previous" from the IK_SEARCH_INPROGRESS and
 * IK_SEARCH_ABORT error messages
 *
 * Revision 1.17  1992/10/16  20:45:50  honce
 * Changed message assoc'ed with IK_EINVSRCH_MINCOND
 *
 * Revision 1.16  1992/10/16  19:39:32  sylvain
 * added IK_EATTOP and IK_EATBOT
 *
 * Revision 1.15  1992/10/13  19:31:22  sylvain
 * made the IK_ENOERROR message be a 75 charater string of spaces.
 *
 * Revision 1.14  1992/10/12  13:21:16  sylvain
 * made some more error messages a little more user friendly.
 *
 * Revision 1.13  1992/10/09  15:08:58  sylvain
 * added IK_EBAD_SEARCH and improved the IK_NOMEM message
 *
 * Revision 1.12  1992/10/07  20:47:02  honce
 * Made some error messages more user friendly.
 *
 * Revision 1.11  1992/10/07  15:30:22  sylvain
 * added IK_EBAD_GRANCAP for Prasad, and stripped out the "IMS" flag.
 *
 * Revision 1.10  1992/09/30  18:53:08  sylvain
 * added more errno values that JAM (Heather) needed
 *
 * Revision 1.9  1992/09/30  18:13:01  sylvain
 * deleted IK_ERRORCOUNT
 *
 * Revision 1.8  1992/09/26  17:17:27  sylvain
 * evaluated all errno values to be integers (no more expressions allowed)
 *
 * Revision 1.7  1992/09/26  14:36:28  sylvain
 * moved net error codes into IK_* name space.
 *
 * Revision 1.6  1992/09/25  21:41:37  sylvain
 * made IK_Errno values #defines for simplicity , and moved IK_Errno's into
 * error range groupings for simplicity.
 *
 * Revision 1.5  1992/09/21  15:38:53  honce
 * Added IK_ENO_STARTINTRVL, IK_ENO_STOPINTRVL, and IK_ESTARTINTRVL2BIG
 *
 * Revision 1.4  1992/09/17  21:58:50  honce
 * Added IK_EBAD_TIME
 *
 * Revision 1.3  1992/09/15  19:15:53  ims
 * added IK_EBAD_CONNUM for bad connection number
 *
 * Revision 1.2  1992/09/15  18:07:10  ims
 * Added Net Mgr errors of IK_E2MANYCONN, IK_EBAD_NMDS & IK_EBAD_ODLTREE
 *
 * Revision 1.1  1992/09/14  22:34:54  honce
 * Initial revision
 *
*/

/*****************************************************************************/

#ifndef __IK_ERRNO_H__
#define __IK_ERRNO_H__

/*****************************************************************************/

/* Define the application error data type. */
typedef unsigned int IK_Errno;

/*****************************************************************************/

/* The error codes... */

/* 1 through 34 are the same for SunOS, IRIX, and VMS C, and copied from
 * errno.h, 35 on came from everywhere 
 * ranges of errors that have been defined so far :
 * 
 * common unix error      : 1-34
 * bad field name         : 100-125
 * minimun search cond    : 150
 * no value specified     : 160-200
 * value given is too big : 200-204
 * net errors             : 350-379
 * screen errors          : 400-
 */

/*-------------------------------------------------------------------------*/

/* common unix errors */

#define IK_ENOERROR  (0)    /*                                                                      */
#define IK_EPERM     (1)    /* Permission denied */
#define IK_ENOENT    (2)    /* No such file or directory */
#define IK_ESRCH     (3)    /* No such process */
#define IK_EINTR     (4)    /* Interrupted system call */
#define IK_EIO       (5)    /* Error performing file I/O, notify User
			       support. */
#define IK_ENXIO     (6)    /* No such device or address */
#define IK_E2BIG     (7)    /* Argument list too long */
#define IK_ENOEXEC   (8)    /* Exec format error */
#define IK_EBADF     (9)    /* Bad file number */
#define IK_ECHILD    (10)   /* No children */
#define IK_EAGAIN    (11)   /* No more processes */
#define IK_ENOMEM    (12)   /* Not enough memory, notify User Support. */
#define IK_EACCES    (13)   /* Permission denied */
#define IK_EFAULT    (14)   /* Bad address */
#define IK_ENOTBLK   (15)   /* Block device required */
#define IK_EBUSY     (16)   /* Mount device busy */
#define IK_EEXIST    (17)   /* File exists */
#define IK_EXDEV     (18)   /* Cross-device link */
#define IK_ENODEV    (19)   /* No such device */
#define IK_ENOTDIR   (20)   /* Not a directory */
#define IK_EISDIR    (21)   /* Is a directory */
#define IK_EINVAL    (22)   /* Invalid argument */
#define IK_ENFILE    (23)   /* File table overflow */
#define IK_EMFILE    (24)   /* Too many open files */
#define IK_ENOTTY    (25)   /* Not a typewriter */
#define IK_ETXTBSY   (26)   /* Text file busy */
#define IK_EFBIG     (27)   /* File too large */
#define IK_ENOSPC    (28)   /* No space left on device */
#define IK_ESPIP     (29)   /* Illegal seek */
#define IK_EROFS     (30)   /* Read-only file system */
#define IK_EMLINK    (31)   /* Too many links */
#define IK_EPIPE     (32)   /* Broken pipe */
#define IK_EDOM      (33)   /* Argument too large */
#define IK_ERANGE    (34)   /* Result too large */
#define IK_EPROTO    (35)   /* Problem with receiving data. Notify
			       System Staff. */
#define IK_ENOSAVE   (36)   /* There is not enough disk space to save
			       the file. */

/*-------------------------------------------------------------------------*/

/* Invalid field values given */

#define IK_E2MANYCONN    (100)   /* Can't create required connections */
#define IK_EBAD_ARCHIVE  (101)	 /* Invalid Data Center ID */
#define IK_EBAD_CAMPAIGN (102)	 /* Invalid Campaign/Project */
#define IK_EBAD_CONNUM   (103)	 /* Bad connection number passed */
#define IK_EBAD_DATASET  (104)	 /* Invalid Dataset ID */
#define IK_EBAD_DATE     (105)	 /* Invalid Date */
#define IK_EBAD_GEOINFO  (106)	 /* Provide all geographic info */
#define IK_EBAD_INTERVAL (107)	 /* Invalid Time Interval */
#define IK_EBAD_LAT      (108)	 /* Invalid Latitude */
#define IK_EBAD_LONG     (109)	 /* Invalid Longitude */
#define IK_EBAD_NMDS     (110)	 /* Bad network manager data structures */
#define IK_EBAD_ODLTREE  (111)	 /* Bad ODL tree */
#define IK_EBAD_PARAMETER (112)	 /* Invalid Parameter */
#define IK_EBAD_PROCLVL  (153)	 /* Invalid Processing Level */
#define IK_EBAD_SENSOR   (114)	 /* Invalid Sensor */
#define IK_EBAD_SIDEREAL (115)	 /* Invalid Day/Night */
#define IK_EBAD_SOURCE   (116)	 /* Invalid Source/Platform */
#define IK_EBAD_TIME     (117)	 /* Invalid time */
#define IK_EBAD_XY       (118)	 /* Invalid X/Y distance */
#define IK_EBAD_MENU     (119)   /* Problem with menu bar. Notify
				    System Staff */
#define IK_EBAD_SCREEN   (120)   /* Problem with Display
				    Screen. Notify System Staff */
#define IK_EBAD_CONTEXTHELP (121)/* Problem with getting Context
				    Sensitive Help. Notify System
				    Staff */
#define IK_EBAD_HELP     (122)   /* Help is not available at this
				    time. */
#define IK_EBAD_VALID    (123)   /* Problem with getting Dependant
				    Valids. Notify System Staff */
#define IK_EBAD_VALIDATE (124)	 /* Problem validating value. Notify
				    System Staff */
#define IK_EBAD_GRANCAP  (125)	 /* Invalid Granules per Dataset. */
#define IK_EBAD_SEARCH   (126)   /* A problem was encountered
				    executing the search. Notify User
				    Support. */
#define IK_ENOFILE       (127)   /* No file name given. */

#define IK_ENOGCMDBASE   (128)   /* GCMD Database is down at this time.  Please                                    try again later. */
/*-------------------------------------------------------------------------*/

/* Didn't fit anyewhere else */

#define IK_EINVSRCH_MINCOND (150)   /* Search requires Geographical
				       information and either sensor
				       or parameter or dataset */
#define IK_BUILD_VALID      (151)   /* Building Dependant Valid List.
				       Please be patient */
#define IK_FATAL            (152)   /* Fatal error. Exiting the IMS
				       system. */

/*-------------------------------------------------------------------------*/

/* No value given */

#define IK_ENO_LAT         (160)   /* Latitude is empty */
#define IK_ENO_LONG        (161)   /* Longitude is empty */
#define IK_ENO_STARTDATE   (162)   /* Start date is empty */
#define IK_ENO_STARTINTRVL (163)   /* Start time interval is empty */
#define IK_ENO_STARTTIME   (164)   /* Start time is empty */
#define IK_ENO_STOPDATE    (165)   /* End date is empty */
#define IK_ENO_STOPINTRVL  (166)   /* Stop time interval is empty */
#define IK_ENO_STOPTIME    (167)   /* Stop time is empty */
#define IK_ENO_SEARCHTREE  (168)   /* Problem with building search
				      message. Notify System Staff */
#define IK_ENO_SEARCH      (169)   /* No Search to Abort */
#define IK_ENO_VALID       (170)   /* This Field has no valids list */
#define IK_ENO_SORT        (171)   /* Can not sort until all of the
				      results are in */
#define IK_ENO_HITS        (172)   /* There were no results found for
				      the given search */
#define IK_ENO_EMAIL_ADDR  (173)   /* An E-Mail address is required. */
#define IK_ENO_DATASETS    (174)   /* Need to select Dataset(s). */
#define IK_ENO_BROWSE      (175)   /* Browse image is not available */
#define IK_ENO_GRANULE     (176)   /* There was no granule selected. */
#define IK_ENO_DAAC        (177)   /* Unable to get next DAAC for
				      current sort type */
#define IK_ENO_DIF         (178)   /* No Dataset Information available
				      at this time. */
#define IK_EOUTDATED       (179)   /* memkey outdated in IC Scroll and
				      mark */
#define IK_EALLDEL         (180)   /* whole segment is deleted */

/*-------------------------------------------------------------------------*/

/* Screen field errors */

#define IK_ESOUTH2BIG       (200)   /* Southernmost lat exceeds
				       Northernmost lat */
#define IK_ESTARTDATE2BIG   (201)   /* End date exceeds Start date */
#define IK_ESTARTINTRVL2BIG (202)   /* End interval exceeds Start
				       interval */
#define IK_ESTARTTIME2BIG   (203)   /* End time exceeds Start time */
#define IK_EWEST2BIG        (204)   /* Western-most long exceeds
				       Easternmost long */
#define IK_EDIR_BUSY        (205)   /* Still receiving data. Please be
				       patient */

/*-------------------------------------------------------------------------*/

/* search error msgs */

#define IK_SEARCH_INPROGRESS (300)   /* Search Executing. */
#define IK_SEARCH_ABORT      (301)   /* Search has been aborted. */
#define IK_SEARCH_RUNNING    (302)   /* Search in Progress. Only one
					search may may executed at a
					time. */
#define IK_SAVE_SEARCH       (303)   /* Search has been saved to the
					file. */

/*-------------------------------------------------------------------------*/

/* Networking error numbers (tcp/ip) */

/* Non-blocking and interrupt i/o */
#define IK_EWOULDBLOCK	(351)   /* Operation would block */
#define IK_EINPROGRESS	(352)   /* Operation now in progress */
#define IK_EALREADY	(353)   /* Operation already in progress */

/* Argument errors */
#define IK_ENOTSOCK        (354)   /* Socket operation on non-socket */
#define IK_EDESTADDRREQ    (355)   /* Destination address required */
#define IK_EMSGSIZE        (356)   /* Message too long */
#define IK_EPROTOTYPE      (357)   /* Protocol wrong type for socket */
#define IK_ENOPROTOOPT     (358)   /* Option not supported by protocol */
#define IK_EPROTONOSUPPORT (359)   /* Protocol not supported */
#define IK_ESOCKTNOSUPPORT (360)   /* Socket type not supported */
#define IK_EOPNOTSUPP      (361)   /* Op not supported on socket */
#define IK_EPFNOSUPPORT    (362)   /* Protocol family not supported */
#define IK_EAFNOSUPPORT    (363)   /* Address family not supported by
				      protocol family */
#define IK_EADDRINUSE      (364)   /* Address already in use */
#define IK_EADDRNOTAVAIL   (365)   /* Can't assign requested address */

/* Operational errors */
#define IK_ENETDOWN     (366)   /* Network is down */
#define IK_ENETUNREACH  (367)   /* Network is unreachable */
#define IK_ENETRESET    (368)   /* Network dropped connection on reset */
#define IK_ECONNABORTED (369)   /* Software caused connection abort */
#define IK_ECONNRESET   (370)   /* Connection reset by peer */
#define IK_ENOBUFS      (371)   /* No buffer space available */
#define IK_EISCONN      (372)   /* Socket is already connected */
#define IK_ENOTCONN     (373)   /* Socket is not connected */
#define IK_ESHUTDOWN    (374)   /* Can't send after socket shutdown */
#define IK_ETOOMANYREFS	(375)   /* Too many references: can't splice */
#define IK_ETIMEDOUT    (376)   /* Connection timed out */
#define IK_ECONNREFUSED (377)   /* Connection refused */

/* Random errors */
#define IK_EHOSTDOWN    (378)   /* Host is down */
#define IK_EHOSTUNREACH (379)   /* No route to host */
#define	IK_ETCP_ELIMIT  (EHOSTUNREACH)

/* Screen maninpulation errors */
#define IK_EATTOP    (400)   /* At the Top. */
#define IK_EATBOT    (401)   /* At the bottom. */
#define IK_EATNOMORE (402)   /* No More. */

/*****************************************************************************/

/* This is the global IMS error variale. */
extern IK_Errno IK_ImsErrno;

/*****************************************************************************/

#endif   /* !__IK_ERRNO_H__ */
/*-- QED --*/
