#ifndef APS2HC_STATUS_H
#define APS2HC_STATUS_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	aps2hc_status.h
Description:	contains function prototypes for aps2hc_status.c functions
Creator:	Teresa McKillop
Notes:		
==============================================================================*/
#pragma ident	"@(#)aps2hc_status.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/aps2hc/SCCS/s.aps2hc_status.h"

int  check_aps_status( error_status_t, char *, int ) ;

int  check_init_status(	error_status_t, int, char *, int ) ;

void check_rpc_status( error_status_t, int, char *, int ) ;

#endif	/* APS2HC_STATUS_H */

