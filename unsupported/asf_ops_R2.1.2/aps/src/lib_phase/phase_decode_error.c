#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Function:       phase_decode_error

Description:   decodes error messages from error codes < 0 as defined
in phase_utilities.h.  The correspondence must be kept up by hand...

Parameters:     none

Returns:        none

Creator:        Lawrence Stevens

Creation Date:  Wed Jun  7 17:33:53 PDT 1995

Notes:		
==============================================================================*/
#pragma ident	"@(#)phase_decode_error.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_phase/SCCS/s.phase_decode_error.c"

char    *phase_error_message[] =
{
	"zero is not an error code",
	"PHASE_APS_READER_SESSION_NOT_OPENED", 
	"error code -1002:  unknown phase error code" ,
	"NASDA_DB_QUERY_FAILED" ,
	"NASDA_NO_PHASE_RECS_FOUND", 
	"NASDA_BAD_PHASE_REC_START_TIME" ,
	"NASDA_TWO_PHASE_RECS_WITH_SAME_SAT_AND_NAME", 
	"NASDA_TWO_PHASE_RECS_OVERLAP_IN_REV_NUMBERS", 
	"NASDA_TWO_PHASE_RECS_OVERLAP_IN_TIME", 
	"NASDA_BAD_VALUE_FOR_SEARCH_FLAG" ,
	"NASDA_ERROR_IN_PHASE_START_TIME_OR_ASFTIME",        /*   10     */
	"NASDA_ERROR_IN_PHASE_START_TIME",  
	"NASDA_ERROR_IN_COMPUTED_TIME"  ,
	"NASDA_BAD_ASFTIME" ,
	"NASDA_BAD_RSP_ANGLE" ,
	"NASDA_ASFTIME_NOT_WITHIN_ANY_PHASE" ,
	"NASDA_NOT_A_NASDA_SATELLITE" ,
	"NASDA_BAD_RSP_PATH" ,
	"NASDA_REV_NOT_FOUND_FOR_RSP" ,
	"NASDA_REV_NUMBER_NOT_IN_PHASE" ,
	"NASDA_INPUT_ASFTIME_NOT_WITHIN_INPUT_REV",            /*  20  */
	"NASDA_INPUT_PHASE_REC_IS_NULL" ,
	"NASDA_NOT_A_NASDA_PHASE_RECORD" ,
	"NASDA_RSP_NOT_FOUND_CHECK_PHASE_RELATION" ,
	"NASDA_PHASE_POINTER_IS_NULL", 
	"NASDA_INPUT_ASFTIME_NOT_WITHIN_INPUT_PHASE", 
	"PHASE_BAD_ASFTIME" ,
	"PHASE_ERROR_IN_PHASE_START_TIME" ,
	"PHASE_INPUT_ASFTIME_NOT_WITHIN_INPUT_PHASE" ,
	"PHASE_REV_NUMBER_NOT_IN_PHASE" ,
	"PHASE_ASFTIME_2_PHASE_INPUT_TIME_TOO_LOW" ,         /*  30  */
	"PHASE_ASFTIME_2_PHASE_INPUT_TIME_TOO_HIGH" ,
	"PHASE_DB_QUERY_FAILED" ,
	"PHASE_NO_PHASE_RECS_FOUND" ,
	"PHASE_BAD_PHASE_REC_START_TIME" ,
	"PHASE_TWO_PHASE_RECS_WITH_SAME_SAT_AND_NAME" ,
	"PHASE_TWO_PHASE_RECS_OVERLAP_IN_REV_NUMBERS" ,
	"PHASE_TWO_PHASE_RECS_OVERLAP_IN_TIME" ,
	"PHASE_POINTER_IS_NULL" ,
	"ERROR_CODE_NOT_LT_ZERO" ,
	"PHASE_REV_2_PHASE_INPUT_REV_TOO_LOW" ,              /*  40  */
	"PHASE_REV_2_PHASE_INPUT_REV_TOO_HIGH" ,
	"PHASE_ASFTIME_NOT_WITHIN_ANY_PHASE", 
	"PHASE_REV_NOT_WITHIN_ANY_PHASE", 
	"ERROR_CODE_UNKNOWN", 
	"PHASE_ERROR_ET2ASF", 
	"error code -1046:  unknown phase error code" ,
	"error code -1047:  unknown phase error code" ,
	"error code -1048:  unknown phase error code" ,
	"error code -1049:  unknown phase error code" ,
	"error code -1050:  unknown phase error code"  /* NO COMMA HERE.  */

} ;
