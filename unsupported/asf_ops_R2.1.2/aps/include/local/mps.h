#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/********************************************************************
*                                                                   
*  Name: mps.h
*
*  Module Type: header file	Language: c
*
*  $Logfile:   ACS003:[BLD.MPS.LIB]MPS.H_V  $
*                                                                   
*  Purpose: defines all of the MACRO substitutions and include files
*          
*  Modification History:                                            
*                                                                   
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
#pragma ident	"@(#)mps.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/local/SCCS/s.mps.h"


#ifndef _MPS_H_
#define _MPS_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* file transfer stuff */
#define MAXFILE   57
#define MAXFNAME  13
#define MAXFILL   2001
#define NODE_MAX  6
#define DISK_MAX  10
#define DIR_MAX   20
#define FNAME_MAX 9
#define EXT_MAX   3
#define VER_MAX   3

#define ACS_NODE   "SANTA::"
#define ACS_MPSIN  "ACS_MPSIN:"
#define ACS_MPSOUT "ACS_MPSOUT:"
#define MPS_NODE   "RUDOLF::"
#define MPS_ACSIN  "MPS_ACSIN:"
#define MPS_ACSOUT "MPS_ACSOUT:"


/* screen stuff */
#define SCR_BOT   23
#define SCR_TOP   1
#define SCR_WIDTH 80

/* limit checks */
#define LATMIN -90.0
#define LATMAX 90.0
#define LONMIN -180.0
#define LONMAX 180.0
#define REVMIN 0
#define REVMAX 99999
#define IDMIN 0
#define IDMAX 99
                   
#endif	/* _MPS_H_ */
