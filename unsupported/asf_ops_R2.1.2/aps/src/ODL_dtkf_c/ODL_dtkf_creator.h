#ifndef ODL_DTKF_CREATOR_H
#define ODL_DTKF_CREATOR_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	ODL_dtkf_creator.h
Description:	Header file for the ODL dtk file creator routines.
Creator:	Lawrence Stevens
Notes:		
==============================================================================*/
#pragma ident	"@(#)ODL_dtkf_creator.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/ODL_dtkf_c/SCCS/s.ODL_dtkf_creator.h"

#include "fa_defs.h"	/* for FA typedefs and basic definitions */

/* returned from odl_dtkfile_create_record():   */
/* this needs to be zero:   */
#define ODL_DTKFILE_CREATE_RECORD_OK	     0

#define ODL_ERROR_MESSAGE( code ) \
	ODL_error_message[ -(code) ]

#define ODL_UNABLE_TO_CREATE_FILE	        -1
#define ODL_NULL_FIRST_DTK_REC	            -2
#define ODL_CONTROL_DATA_NOT_OBTAINED       -3
#define ODL_DEFAULT_DATA_NOT_OBTAINED       -4
#define ODL_DTK_RECORD_FIELD_NOT_OBTAINED   -5
#define ODL_DEFAULT_FIELD_NOT_OBTAINED      -6
#define ODL_FAIL2CALCULATE_FIRSTLAST_TRACK  -7

extern EQUIV_TABLE ODL_dtk_status[];

#endif      /* ODL_DTKF_CREATOR_H */

