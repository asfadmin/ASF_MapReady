#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dapps_defs.h
Description:	This file contains definitions which in all likelihood will be
		needed throughout the DAPPS.
Notes:		10/18/95	Teresa McKillop		added ASF_TIME_STR_LENGTH
			11/03/95	Teresa McKillop		encapsulated "DEBUG" which is
											defined in macros.h
			11/20/95	Teresa McKillop		added NEWLINE

==============================================================================*/
#pragma ident	"@(#)dapps_defs.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/global/SCCS/s.dapps_defs.h"


#ifndef	_DAPPSDEFS_
#define	_DAPPSDEFS_

/*
-- don't include macros.h:
-- if stdlib.h follows it, there are compile errors,
-- it defines DEBUG
*/
/*															! from macros.h
 *	MAX() and MIN() depend on the types of the operands		! from macros.h
 */														/*	! from macros.h */
#define	MAX(a, b) 		((a) < (b) ? (b) : (a))			/*	! from macros.h */
#define	MIN(a, b) 		((a) > (b) ? (b) : (a))			/*	! from macros.h */

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef OK
#define OK 0
#endif

#ifndef ERROR
#define ERROR -1
#endif

#ifndef NULL
#define NULL	(void *) 0
#endif

#define ASF_TIME_STR_LENGTH	21	/* length of the asf time string */

#define ASF_MIN_TIME	1985:001:00:00:00.000     /* 1/1/1985    */
#define ASF_MAX_TIME	2100:001:00:00:00.000     /* 1/1/2100    */

#define ODL_MIN_TIME	1985-001T00:00:00.000     /* 1/1/1985    */
#define ODL_MAX_TIME	2100-001T00:00:00.000     /* 1/1/2100    */

#define EPH_MIN_TIME	2446066.501     /* 1/1/1985    */
#define EPH_MAX_TIME	2488069.500     /* 1/1/2100    */

#define MIN_REV         1
#define MAX_REV     99999

#ifndef NEWLINE		/* newline character */
#define NEWLINE		'\n'
#endif

#ifndef EMPTY_STR	/* string of zero length */
#define EMPTY_STR	""
#endif

#ifndef STREND		/* string termination character */
#define STREND		'\0'
#endif

#endif /*  _DAPPSDEFS_   */
