/*
 * Name: IK_Syslog.h
 *
 * Description: This file contains definitions needed to implement the
 * system log.
 *
 * Notes:
 *
 *-------------------------------------------------------------------------
 *
 * RCS Information:
 *
 * $RCSfile$
 *
 * $Id$
 *
 * $Log$
 * Revision 1.1  2004/02/03 03:32:54  pdenny
 * Initial revision
 *
 * Revision 5.0  1995/11/06  13:04:19  ims
 * PROMOTION FROM GUI_4_5_8_951030 FOR GUI_PROMO_5_0_951106
 *
 * Revision 4.5  1995/07/27  18:43:23  ims
 * PROMOTION FROM GUI_4_4_8_950501 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.4.1.1  1995/02/13  22:38:06  ims
 * COPIED FROM 4.3.1.1 FOR GUI_PROMO_4_4_950213
 *
 * Revision 4.3.1.1  1994/08/26  12:27:05  ims
 * COPIED FROM REVISION 4.0.1.1.
 *
 * Revision 4.0.1.1  1994/06/08  17:31:13  ims
 * COPIED FROM REVISION 3.1.1.1.
 *
 * Revision 3.1.1.1  1994/04/18  14:01:56  winter
 * Extensive code cleanup during analysis by Eric Winter.
 *
 * Revision 3.1  1994/04/05  09:49:52  ims
 * Promotion from MAR28_RELEASE
 *
 * Revision 3.0  1993/12/29  15:18:49  winter
 * ALPHA-1
 *
 * Revision 1.4  1992/10/26  15:03:09  sylvain
 * added some more includes for the stat call that was added in IK_CloseSyslog
 *
 * Revision 1.3  1992/10/20  23:58:08  honce
 * Added prototype for IK_vSyslog().
 *
 * Revision 1.2  1992/09/16  21:54:55  sylvain
 * took out the needless include IK_Ims.h
 *
 * Revision 1.1  1992/09/03  21:04:41  sylvain
 * printed out the time zone for VMS machines.
 *
 * Revision 1.0  1992/08/31  08:53:05  sylvain
 * converted from sccs to rcs
 *
 * Revision 0.5  92/08/25  10:15:38  sylvain
 * initial data store required to supported the message passing software
 * 
*/

/*****************************************************************************/

#ifndef __IK_SYSLOG_H__
#define __IK_SYSLOG_H__

/*****************************************************************************/

/* #include directives */

/* Standard headers */
/*
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
*/

/*****************************************************************************/

#ifdef vms  /* vms is defined by the VMS C compiler. */

#define LOG_ERR (3)	/* error conditions */
#define LOG_NOTICE (5)	/* normal but significant condition */
#define LOG_INFO (6)	/* informational */
#define LOG_DEBUG (7)	/* debug-level messages */

/* Define the maximum number of characters in a pathname. */
#define MAXPATHLEN (1024)

/* The times that are printed are in the local time zone. */
#define TIME_FUNC localtime

/* VMS-specific headers */
#include <stat.h>
#include "multinet_root:[multinet.include.sys]types.h"
#include "multinet_root:[multinet.include.sys]param.h"

#else  /* !vms */

/* Non-VMS headers */
#include <syslog.h>

#define TIME_FUNC gmtime
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#endif   /* !vms */

/*-------------------------------------------------------------------------*/

/* Define the time zone to use for system log messages. */
#ifndef TZ
#ifdef vms   /* vms only has localtime. */
#define TZ "PST"
#else
#define TZ "GMT"   /* Log messages in ZULU time. */
#endif
#endif

/*-------------------------------------------------------------------------*/

/* Define the length of a timestamp buffer. */
#ifndef IK_TBUFLEN
#define IK_TBUFLEN (26)
#endif

/*-------------------------------------------------------------------------*/

/* Function prototypes */

void IK_Syslog(int, char *);
void IK_vSyslog(int, char *,...);
int IK_NameSyslog(char *);
void IK_CloseSyslog(void);

/*-------------------------------------------------------------------------*/

/* The system needs someplace to log a message in case the system log
 * fails.  But then again, if the system log fails, the world as we
 * know it doesn't exist. */
#ifndef IK_SysErr
#define IK_SysErr(x) fputs((x), stderr)
#endif

/*****************************************************************************/

#endif   /* !__IK_SYSLOG_H_ */
