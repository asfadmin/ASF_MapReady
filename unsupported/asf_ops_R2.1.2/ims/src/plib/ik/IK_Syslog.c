/* File: $RCSfile$
 * 
 * Synopsis: this file contains the source code that implements the 
 *                 SYSLOG data store.
 * 
 * Notes: 
 * 
 */

/*--------------------------------------------------
 | $Log$
 | Revision 1.1  2004/02/03 03:32:54  pdenny
 | Initial revision
 |
 * Revision 5.0  1995/11/06  13:04:18  ims
 * PROMOTION FROM GUI_4_5_8_951030 FOR GUI_PROMO_5_0_951106
 *
 * Revision 4.5.1.2  1995/09/25  18:46:54  wright
 * added Revision Name to first line of client syslog file
 *
 * Revision 4.5.1.1  1995/07/27  19:09:22  ims
 * COPIED FROM 4.4.1.3 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.4.1.3  1995/05/25  15:47:30  ims
 * The source uses FILENAME_MAX to define size for strings that will hold
 * pathnames. Since HP/UX still supports filesystems with short filenames
 * (15 characters), FILENAME_MAX is set to 15 for HP/UX.
 *
 * Revision 4.4.1.2  1995/02/28  13:23:50  ryan
 * Added some error message text which is use in case IK_NameSyslog()
 * cannot open up the syslog file.  Fixed a few spelling errors.
 *
 * Revision 4.4.1.1  1995/02/13  22:37:57  ims
 * COPIED FROM 4.3.1.1 FOR GUI_PROMO_4_4_950213
 *
 * Revision 4.3.1.1  1994/08/26  12:26:16  ims
 * COPIED FROM REVISION 4.0.1.1.
 *
 * Revision 4.0.1.1  1994/07/08  12:00:12  winter
 * #included several headers.
 *
 * Revision 4.0  1994/06/08  15:24:23  ims
 * PROMOTION FROM REV4
 *
 * Revision 3.1.1.4  1994/05/20  19:31:26  heather
 * added global variable SyslogLevel and modify IK_Syslog to check if priority
 * is greater than SyslogLevel if so return and do not write to syslog.log file.
 * So modified IK_NameSyslog to get the value for SyslogLevel from script.
 *
 * Revision 3.1.1.3  1994/05/11  11:46:26  winter
 * Fixed a bug related to bug fixed in 3.1.1.2 - on the next line, a
 * statbuf-> construct was changed to a statbuf., since statbuf is a structure
 * not a pointer to a structure.
 *
 * Revision 3.1.1.2  1994/05/11  10:59:46  winter
 * Fixed the bugs mentioned previously in rev 3.1.1.1 log message. Somebody
 * was not payin attention when they wrote this code. The mistakes caused\
 * problems when stat() was called, since it tried to fill in a file stat
 * buffer at a space of unallocated, random memory.
 *
 * Revision 3.1.1.1  1994/05/11  10:55:58  winter
 * Placeholder revision to start the branch.
 * The purpose of this branch (initially) is to fix two bugs discovered by
 * Tze Hong and Brett Kerbel during the Sun port of the client. Both bugs
 * involve incorrect uses of pointers and addresses. The first uses a *statbuf
 * instead of statbuf, where the latter is correct. The second uses statbuf
 * instead of &statbuf, where the latter is correct.
 *
 * Revision 3.1  1994/04/05  09:49:52  ims
 * Promotion from MAR28_RELEASE
 *
 * Revision 3.0.1.2  1994/02/14  16:38:11  hu
 * This version is for code clean up
 *
 * Revision 3.0  1993/12/29  15:18:49  winter
 * ALPHA-1
 *
 | Revision 1.12  1993/09/02  20:14:38  sylvain
 | inly include IK_DataDic.h if were not on a server (DAAC_SERVER is not
 | 	defined)
 |
 | Revision 1.11  1993/02/12  18:10:28  sylvain
 | increased the size of buf in IK_vSyslog, vsprintf() was trying to
 | put too many characters in it previously
 |
 | Revision 1.10  1993/01/15  02:54:47  honce
 | increased the size of errbuf which is used when the
 | syslog dies.
 |
 | Revision 1.9  1993/01/05  21:02:30  sylvain
 | ifdef'd the stat call out for vms, st_nlink is meaningless on VMS
 |
 | Revision 1.8  1993/01/05  20:43:50  honce
 | bumped up the internal buffer for IK_vSyslog from IK_MAXBUFLEN to 255 
 | charactgers.
 |
 | Revision 1.7  1992/11/09  17:26:44  honce
 | Updated IK_vSyslog() header comments to make a meaning man(1) page.
 |
 | Revision 1.6  1992/10/29  17:38:30  sylvain
 | added IK_ImsErrno values.
 |
 | Revision 1.5  1992/10/26  15:03:43  sylvain
 | added a stat call to the IK_CloseSyslog file so that it will only write a
 | closing log message if in fact it was closing the last link.
 |
 | Revision 1.4  1992/10/21  00:04:46  honce
 | Added IK_Ims.h and IK_DataDic.h
 |
 | Revision 1.3  1992/10/19  23:42:26  honce
 | Added function IK_vSyslog().  IK_Syslog() with a vprintf() interface.
 |
 | Revision 1.2  1992/10/19  23:05:23  honce
 | Added function IK_vSyslog().
 |
 | Revision 1.1  1992/08/31  08:54:00  sylvain
 | converted from sccs to rcs
 |
 | Revision 1.0  92/08/25  10:23:44  sylvain
 | Initial revision
 | 
 +------------------------------------------------*/

static const char *rcsid =
"$Id$";

#include "IK_Ims.h"		/* IMS configuration file */
#include <stdarg.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <time.h>
#ifndef DAAC_SERVER
#   include "IK_DataDic.h"         /* IMS Data Dictionary */
#endif

#include "IK_Syslog.h"

/* HP/UX supports filesystems with short filenames (15 characters). */
#ifdef HPUX
#undef FILENAME_MAX
#define FILENAME_MAX 255
#endif

int SyslogLevel;
static char IK_SyslogFileName[FILENAME_MAX];   /* holds the 
					      * syslog filename  */
static FILE *IK_SyslogFilePtr;	             /* file pointer */

/* Name: IK_Syslog
 * 
 * Description:
 *  write a message (along with a time stamp) to the syslog file 
 *
 *    There are 4 levels of system status messages to log :
 *         
 *       LOG_ERR    - an IMS system error has occured, 
 *     	       	       (i.e. connect refused...)
 *       LOG_NOTICE - log system performance information (i.e. time stamps)
 *       LOG_INFO   - miscelaneous information
 *       LOG_DEBUG  - log debugging information when appropriate. 
 *
 * Arguments:
 *         int   priority,	      - the severity of the error 
 *         char* message 	      - the message to be written 
 *
 * Return values:
 *	   upon success, 0. On failure, -1 and errno, IK_ImsErrno.  
 *
 * Warnings:
 * 	   in the case of precondition 5, IK_SysErr will redirect
 *     	   it's output to stderr, which is normally defined to be the 
 *         screen.  All of the log messages are prepended with a 
 *    	   time stamp.
 *
 * Global Variables Used:
 *
 * Pre- and Post-Conditions
 *     Precondition 1 :	       
 *     	   A perfomance message has arrived. These messages will have 
 *     	   priority of LOG_NOTICE.
 *     	   
 *     Postcondition 1 :
 *         Log the message to syslog, under LOG_NOTICE priority.
 *
 *     Precondition 2 :	       
 *     	   an IMS system error has occurred, this might be 
 *     	   any of the following :
 *         ECONNREFUSED, ETIMEDOUT, ENETDOWN, EHOSTDOWN, ENETUNREACH, 
 *         EADDRINUSE, EHOSTUNREACH, etc.  These messages have a 
 *     	   priority of LOG_ERR.
 *
 *     Postcondition 2 :
 *     	   LOG the error message to the syslog file, under LOG_ERR 
 *     	   priority.
 *
 *     Precondition 3 :
 *     	   A debugging message has arrived. These have a priority 
 *     	   of LOG_DEBUG.
 *
 *     Postcondition 3 :
 *     	   LOG the message to the syslog file, under LOG_DEBUG 
 *     	   priority.
 *
 *     Precondition 4 :	       
 *     	   An informational message has arrived, with a priority of 
 *     	   LOG_INFO.  This is a catch all priority.
 *
 *     Postcondition 4 :
 *         LOG the message to the syslog, under LOG_INFO priority.
 *
 *     Precondition 5 :
 *     	   The syslog data store can not be opened.
 *
 *     Postcondition 5 :
 *     	   LOG the message with IK_SysErr instead.  The system will 
 *     	   probably be comming down soon.
 *
 *     Revision History: 		
 */
void IK_Syslog 
(
 int   priority,	      /*  the severity of the error */
 char* message		      /*  the message to be written */
)
{
   char *timeptr;		      /* the ascii represention of the time */
   char errbuf[IK_MAXBUFLEN*4];	      /* buffer to create error msgs in */
   time_t longtime;		      /* the integer representation of the time */
   
   if (priority > SyslogLevel)
     return;

   longtime = time (NULL);
   timeptr = asctime (TIME_FUNC (&longtime));
   timeptr = strchr(timeptr,':') -2;
   timeptr[8] = 0;	/* overwrite the newline */

   if (!IK_SyslogFilePtr) {
      IK_SysErr ("You have to name the syslog file "
		 "before you can write to it.\n");
      IK_SysErr (message);
      return;
   }
   switch (priority) {
   case LOG_ERR:
      if (fprintf (IK_SyslogFilePtr,"#### %s ERROR : %s\n", 
		   timeptr, message) < 0) 
      {
	 (void) sprintf (errbuf, "%s\n", message);
	 IK_SysErr ("error opening syslog file\n");
         IK_SysErr (errbuf);
      }
      break;
   case LOG_DEBUG:
      if (fprintf (IK_SyslogFilePtr, "**** %s DEBUG : %s\n", 
		 timeptr, message) < 0) {
      
	 (void) sprintf (errbuf, "%s\n", message);
	 IK_SysErr ("error opening syslog file\n");
         IK_SysErr (errbuf);
      }
      break;
   case LOG_INFO:
      if (fprintf (IK_SyslogFilePtr,"++++ %s INFO : %s\n", 
		 timeptr, message) < 0) 
      {
	 (void) sprintf (errbuf, "%s\n", message);
	 IK_SysErr ("error opening syslog file\n");
         IK_SysErr (errbuf);
      }
      break;
   case LOG_NOTICE:
   default:
      if (fprintf (IK_SyslogFilePtr, ">>>> %s NOTICE : %s\n", 
		 timeptr, message) < 0) 
      {
	 (void) sprintf (errbuf, "%s\n", message);
	 IK_SysErr ("error opening syslog file\n");
         IK_SysErr (errbuf);
      }
      break;
   }
   (void) fflush (IK_SyslogFilePtr);
}

/* Name: IK_vSyslog
 * 
 * Description:
 *     print a message to the Syslog using a printf(3)-like interface
 *
 * Arguments:
 * 	int   priority,		- the severity of the error 
 *	char *format,		- see printf(3) 
 *	...			- The Arguments to print 
 *
 * Return values:
 *     None.
 *
 * Warnings:
 *     The internal buffer is limited to 255 characters.
 *
 * Global Variables Used:
 *
 * Pre- ans Post-Conditions:
 *
 * Revision History: 		
 */
void             IK_vSyslog(
 	int   priority,		/* the severity of the error */
	char *format,		/* see printf(3) */
	...			/* The Arguments to print */
)
{
    va_list         arg;
    char            buf[IK_MAXBUFLEN*4];

    va_start(arg, format);
    vsprintf(buf, format, arg);
    va_end(arg);

    IK_Syslog(priority, buf);
}

/* Name: IK_NameSyslog
 * 
 * Description:
 *	this changes the name of the current syslog file 
 * 	overwrite the old syslog filename with the new, so that 
 *     	all subsequent calls to IK_Syslog will go to the new file
 *
 * Arguments:
 *	char* filename		      - the filename to write to 
 *
 * Return values:
 *	Return: upon success, 0. On failure, -1 and errno, IK_ImsErrno.  
 *
 * Warnings:
 *
 * Global Variables Used:
 *
 * Pre- ans Post-Conditions:
 *     Precondition 1 :	       
 *     	   The name of the new syslog file already exists.
 *
 *     Postcondition 1 :
 *         Open the file in append mode, write out a time/date 
 *	   RETURN 0 if all is successful, -1 if there was an error.
 *
 *     Precondition 2 :
 *     	   The name of the new syslog file does NOT already exists.
 *
 *     Postcondition 2 :
 *         Open the file in write mode, write out a time/date 
 *	   RETURN 0 if all is successful, -1 if there was an error.
 *
 * Revision History: 		
 *
 */
int IK_NameSyslog 
(
 char* filename		      /*  the filename to write to */
)
{
   char errbuf[IK_MAXBUFLEN];	/* buffer to create error msgs in */
   time_t longtime;		/* the integer representation of the time */
   char *timebuf;		/* ptr to the ascii time */
   char *IK_revname= REVNAME;   /* ptr to globally defined Revision Name */

   SyslogLevel = atoi(IK_SysConfig("SyslogLevel"));
   if (IK_SyslogFilePtr)
       IK_CloseSyslog ();
   (void) strcpy (IK_SyslogFileName, filename);
   IK_SyslogFilePtr = fopen (IK_SyslogFileName, "w");
   if (IK_SyslogFilePtr == NULL) {
      (void) sprintf (errbuf,
		      "unable to open SYSLOG file \"%s\"\n",
		      IK_SyslogFileName);
      IK_SysErr (errbuf);
      sprintf(errbuf,"\t%s (errno %d)\n",strerror(errno),errno);
      IK_SysErr (errbuf);
      return (-1);
   }
   longtime = time (NULL);
   timebuf = asctime (TIME_FUNC(&longtime));
   timebuf[strlen (timebuf) - 1] = ' ';

   if (fprintf (IK_SyslogFilePtr, "This Syslog file was opened %s%s using %s\n",
		timebuf, TZ, IK_revname) < 0) 
   {
      (void) sprintf (errbuf, "unable to write a time stamp to the syslog \
file %s using %s\n", IK_SyslogFileName,IK_revname);
      IK_SysErr (errbuf);
      return (-1);
   }
   (void) fflush (IK_SyslogFilePtr);
   return 0;
}

   
/* Name: IK_CloseSyslog
 * 
 * Description:
 *	 close the syslog data store 
 *
 * Arguments:
 *
 * Return values:
 *	upon success, 0. On failure, -1 and errno, IK_ImsErrno.  
 *
 * Warnings:
 *
 * Global Variables Used:
 *
 * Pre- ans Post-Conditions:
 *     Precondition 1: The system terminated
 * 
 *     Postcondition 1 : the syslog file was closed.
 *
 * Revision History: 		
 *
 */
void IK_CloseSyslog
(
    void			/*  no parameters */
)
{
   char errbuf[IK_MAXBUFLEN];	      /* buffer to create error msgs in */
   time_t longtime;		      /* the integer representation of the time */
   char *timebuf;		/* ascii representation of the time */
   IK_BOOLEAN write_log = IK_TRUE;	/* whether to write teh log message or not */

#ifndef vms
   struct stat statbuf;	/* used to get the number of links to syslog */

   if (stat (IK_SyslogFileName, &statbuf) == 0) 
       if (statbuf.st_nlink != 1) /* this isn't the last link link */
	   write_log = IK_FALSE;
#endif

   longtime = time (NULL);
   timebuf = asctime (TIME_FUNC (&longtime));

   timebuf[strlen (timebuf) - 1] = ' ';
   if ((int) write_log && IK_SyslogFilePtr) 
       if (fprintf (IK_SyslogFilePtr, "This Syslog file was closed %s%s\n", 
		    timebuf, TZ) < 0) 
	{
	   (void) sprintf (errbuf, "unable to write a time stamp to the syslog \
file %s\n", IK_SyslogFileName);
	   IK_SysErr (errbuf);
	   return;
	}

   (void) fclose (IK_SyslogFilePtr);
}


       
/*--QED--*/

