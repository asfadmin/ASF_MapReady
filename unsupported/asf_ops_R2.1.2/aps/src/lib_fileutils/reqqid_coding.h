#ifndef REQQID_CODING_H
#define REQQID_CODING_H

#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:		reqqid_coding.h
Description:	
Creator:        Lawrence Stevens
Notes:			
==============================================================================*/
#pragma ident	"@(#)reqqid_coding.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_fileutils/SCCS/s.reqqid_coding.h"

int reqqid_is_rsp( char *REQQ_id ) ;    /* returns TRUE if RSP request.  */

void reqqid_encode(
    long darid,
    int path,
    int row,
    int phase,
    int image,
    char reqqid[]  ) ;

void reqqid_decode(
    char reqqid[],
    long *darid,
    int *path,
    int *row,
    int *phase,
    int *image  ) ;


#endif	/* REQQID_CODING_H */
