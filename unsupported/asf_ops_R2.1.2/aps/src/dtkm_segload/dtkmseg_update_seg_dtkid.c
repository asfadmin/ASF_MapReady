#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		dtkmseg_update_seg_dtkid.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkmseg_update_seg_dtkid.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/dtkm_segload/SCCS/s.dtkmseg_update_seg_dtkid.c"

#include "dtkm_segload.h"
#include <string.h>



/*==============================================================================
Function:	dtkmseg_update_seg_dtkid

Description:  update a seg record with the data-take id that tells 
which data-take is taking the data described by the seg record.  

Parameters:		
	DBPROCESS *APS_dbproc
	DB_RECORD **dtk_rec       dtk rec that now supports the segment 

	int       darid       /   darid and segid are the two keys      
	int       segid       \   identifying which seg to update.     

Returns:   	
	int
	>= 0  :
		TRUE 

	< 0   :  ERROR

Creator:	Lawrence Stevens

Creation Date:	03/17/1995

Notes:		
==============================================================================*/
int dtkmseg_update_seg_dtkid( 
	DBPROCESS 	*APS_dbproc, 
	DB_RECORD 	**dtk_rec,      /* dtk rec that now supports the segment */
	FILE 		*report_fp ) 
{
	DB_RECORD	**seg_rec ;
	llist		*seg_list ;
	cursor		seg_list_ptr ;

	char     segdate[10];
	char     seg_dtkid[16] ;  /* for storing the value of seg.dtkid 
	                             before updating:  format:  E1/SAR/12345.01   */
	int		nrecs ;

	/*  check for errors   */
	if (dtk_rec == NULL)
		return DTKM_SEGLOAD_ERROR_NULL_RECORD ;

	/*  
	-- Set the character dtk id from dtk_rec.  
	-- format:  E1/SAR/12345.01   
	*/
	sprintf(seg_dtkid, "%2.2s/%3.3s/%05.5ld.%02.2d",
		CAST_DTK_SAT dtk_rec[DTK_SAT],
		CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
		CAST_DTK_REV dtk_rec[DTK_REV],
		CAST_DTK_DTKID dtk_rec[DTK_DTKID] ) ;

	/* get the date for today to put into the seg.segdate field:  */
	tc_systime2yyyycddd(segdate) ;

	/* update the seg.dtkid, seg.date value in the seg record:  */
	sprintf(fields_to_set, "%s = '%s', %s = '%s'", 
		APS_COL(SEG, SEG_DTKID),   seg_dtkid, 
		APS_COL(SEG, SEG_SEGDATE), segdate  ) ;

	sprintf(where_clause, "where %s = %ld and %s = '%s' and %s = '%s' and %s = %ld and %s = '%c' ",
		APS_COL(SEG, SEG_DARID), CAST_DTK_DARID dtk_rec[DTK_DARID], 
		APS_COL(SEG, SEG_SAT), CAST_DTK_SAT dtk_rec[DTK_SAT],
		APS_COL(SEG, SEG_SENSOR), CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
		APS_COL(SEG, SEG_REV), CAST_DTK_REV dtk_rec[DTK_REV],
		APS_COL(SEG, SEG_ASCDSC), CAST_DTK_ASCDSC dtk_rec[DTK_ASCDSC] ) ;

	seg_list = db_get_records(APS_dbproc, APS_TABLE(SEG),
		where_clause, NULL, APS_CDEFS(SEG), ALL_COLS) ;
	if ( seg_list == NULL )
		return DTKM_SEGLOAD_ERROR_DB_QUERY_FAILED ;
	if ( report_fp && NUMELTS( seg_list ) > 0 )
	{
		/* list the segments to be updated.  */
		for (   seg_rec = (DB_RECORD **) FIRST(seg_list, seg_list_ptr);
       		 	seg_rec != NULL ;
        		seg_rec = (DB_RECORD **) NEXT(seg_list, seg_list_ptr)  
    		)
		{
    		/* process the current seg_rec right here.  */
			fprintf( report_fp, 
				"    seg.darid = %ld,  seg.segid = %d :   seg.dtkid = %s\n",
				CAST_SEG_DARID seg_rec[SEG_DARID], 
				CAST_SEG_SEGID seg_rec[SEG_SEGID], 
				seg_dtkid ) ;
		}
	}

	nrecs = db_update_records(APS_dbproc,
		APS_TABLE(SEG), fields_to_set, where_clause) ;

	if ( nrecs <= 0 )
		return DTKM_SEGLOAD_ERROR_SEG_NOT_UPDATED ;
	else if ( nrecs > 1 )
		return DTKM_SEGLOAD_ERROR_TOO_MANY_SEGS_UPDATED ;

	return TRUE ;
}
