#ifndef REQQ_PHASE_H
#define REQQ_PHASE_H

#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   reqq_phase.h
Description:    prototypes for routines to deal with the REQQ phases.  
Creator:        Larry Stevens Tue Sep 23 16:56:12 PDT 1997
==============================================================================*/
#pragma ident   "@(#)reqq_phase.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.reqq_phase.h"

int get_reqq_phase_no( char *asftime ) ;

int get_current_reqq_phase( 
    int     *reqq_id,      /* output REQQ phase number  */
    char    *due_date,     /* output due date           */
    char    *strttime,     /* output start time         */
    char    *stoptime ) ;  /* output stop time          */


#endif  /* REQQ_PHASE_H */
