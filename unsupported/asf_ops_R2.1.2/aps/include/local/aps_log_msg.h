#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*******************************************************************
**
** File:        aps_log_msg.h
**
** Description:
**              contains the following functions:
**              aps_log_msg()
**              aps_fprintf()
**              aps_syslog()
**
**  NOTE:  the #defines must correspond to the FORTRAN version aps_log_msg.inc
**
** Author:      Q. Sun
**
** Date:        01/23/96
**
** Modified:
**
**************************************************************** */
#pragma ident	"@(#)aps_log_msg.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/local/SCCS/s.aps_log_msg.h"

#ifndef _APS_LOG_MSG_H_
#define _APS_LOG_MSG_H_

#include <stdio.h>
#include <syslog.h>

void aps_log_msg(char* progname, int level, char* msg, 
				int sysLogFlag, int printFlag);
void aps_fprintf(FILE* filePtr, char* progName, int level, char* msg);
void aps_syslog(char* progname, int level, char* msg);
void aps_open_syslog(void);
void aps_close_syslog(void);

#define TIME_LEN	21
#define HOSTNAME_LEN	21

#ifndef TRUE 
#define TRUE 1
#endif

/*  the #defines must correspond to the FORTRAN version aps_log_msg.inc  */
#ifndef DO_SYSLOG 
#define DO_SYSLOG 1
#endif

/*  the #defines must correspond to the FORTRAN version aps_log_msg.inc  */
#ifndef DO_PRINT
#define DO_PRINT 1
#endif

#ifndef FALSE 
#define FALSE 0
#endif

/*  the #defines must correspond to the FORTRAN version aps_log_msg.inc  */
#ifndef NO_SYSLOG
#define NO_SYSLOG 0
#endif

/*  the #defines must correspond to the FORTRAN version aps_log_msg.inc  */
#ifndef NO_PRINT
#define NO_PRINT 0
#endif

/* Max msg length for syslog() is 1024 */
#define MSG_LEN			1024
#define MSG_TYPE_LEN	20

/*  the #defines must correspond to the FORTRAN version aps_log_msg.inc  */
#define APS_DEBUG 		0
/*  the #defines must correspond to the FORTRAN version aps_log_msg.inc  */
#define APS_INFO 		-1
/*  the #defines must correspond to the FORTRAN version aps_log_msg.inc  */
#define APS_WARNING 	-2
/*  the #defines must correspond to the FORTRAN version aps_log_msg.inc  */
#define APS_ERROR		-3
/*  the #defines must correspond to the FORTRAN version aps_log_msg.inc  */
#define APS_CRITICAL	-4


#endif  /* _APS_LOG_MSG_H_ */
