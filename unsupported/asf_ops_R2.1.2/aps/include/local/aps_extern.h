#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	aps_extern.h
Description:	
Creator:	Unknown
Notes:		
==============================================================================*/
#pragma ident	"@(#)aps_extern.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/local/SCCS/s.aps_extern.h"


#ifndef APS_EXTERN_H
#define APS_EXTERN_H

#include "db_sybint.h"

extern DBPROCESS *APS_dbproc ;	/* the APS DB process */
extern char *APS_progname ;		/* the name of the APS process */
extern char *userid ;			/* the APS and IMS db user name */
extern char *password ;			/* the APS and IMS db user password */

#endif	/* APS_EXTERN_H */
