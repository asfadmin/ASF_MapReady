/*==============================================================================
Filename:	ASFsv.h
Description:	
	Header file for ASF state vector file functions.  

	This include file contains the definitions of the ASF State Vector
related data structures and #defines.
 
Creator:        Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:
	Refer to the ASF RGS Interface Memo by Navid Dehghani for a
description of the ASF State Vector file.

SCCS Info:
   %W%
==============================================================================*/

#ifndef _ASFSV_
#define _ASFSV_

#ifndef ERROR
#include "faifdefs.h"
#endif

#define ASFSV_REV        5
#define ASFSV_YEAR       4
#define ASFSV_TIME       16
#define ASFSV_POSITION   11
#define ASFSV_VELOCITY   11
#define ASFSV_RECLEN    ((ASFSV_REV)+1 + (ASFSV_YEAR)+1 + (ASFSV_TIME)+1 + \
			 3*((ASFSV_POSITION)+1) + 3*((ASFSV_VELOCITY)+1))

#define ASFSVMETA_SAT     2
#define ASFSVMETA_PREC   10 
#define ASFSVMETA_CRDSYS 19
#define ASFSVMETA_RECLEN ((ASFSVMETA_SAT)+1 + \
			  (ASFSVMETA_PREC)+1 + \
			  (ASFSVMETA_CRDSYS)+1)

/* ASF State Vector Metadata Record structure
*/
typedef struct asf_svmeta_rec
{
   char sat[ASFSVMETA_SAT+1] ;
   char precision[ASFSVMETA_PREC+1] ;
   char coordsys[ASFSVMETA_CRDSYS+1] ;

} ASF_SVMeta_Record ;

#define ASFSV_MAXPOS  7500
#define ASFSV_MINPOS -7500
#define ASFSV_MAXVEL  7800
#define ASFSV_MINVEL -7800

#define UNASSIGNED 'U'
#define PREDICTED  'P'
#define RESTITUTED 'R'

/* State Vector record structure
*/
typedef struct state_vector_rec
{
   char sat_ID[MAX_SAT_ID_LEN+1] ;            /* Satellite Identification */
   int rev ;                                  /* Rev / Orbit Number */
   char start_time[TIME_STRING_LEN+1] ;       /* Start Time */
   char pred_or_rest_flag ;                   /* Predicted or Restituted? */

   /* Note: Numbers are not translated to their numerical
   --       equivalent to avoid losing precision.
   */
   char *xpos_str, *ypos_str, *zpos_str,      /* Position Vector String */
        *xvel_str, *yvel_str, *zvel_str ;     /* Velocity Vector String */

   double xpos, ypos, zpos,                    /* Position Vector */
	  xvel, yvel, zvel ;                   /* Velocity Vector */

   char *coordinate ;                         /* Coordinate Sytem */
   LOGICAL assigned_time ;                    /* Assigned Flags */
   LOGICAL assigned_pos ; 
   LOGICAL assigned_vel ; 

} State_Vector_Record ;

typedef struct state_vector_rec ASF_SV_Record ;


#define ASF_SVTYPE_PRED   "PREDICTED"
#define ASF_SVTYPE_REST   "RESTITUTED"

#define ASF_SVPRECIS_TE   "TRUE EQUATORIAL"
#define ASF_SVPRECIS_MD   "MEAN OF DATE"

#endif /* _ASFSV_ */


/* End of File */
