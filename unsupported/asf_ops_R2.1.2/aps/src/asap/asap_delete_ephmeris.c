#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		asap_delete_ephmeris.c

Description:	contains asap_delete_ephmeris() which deletes an 
				ephemeris record.  

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)asap_delete_ephmeris.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/asap/SCCS/s.asap_delete_ephmeris.c"

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */

#include "db_ephemeris.h"   /* for the ephemeris relation.              */


/*==============================================================================

Function:       asap_delete_ephmeris

Description:    checks to see if the ephemeris record already exists in 
				the database, then deletes it.

Returns:

Creator:    Lawrence Stevens

Creation Date:  03/21/1995

Notes:

==============================================================================*/

int asap_delete_ephmeris(
	DBPROCESS   *APS_dbproc,    /* Sybase db process          */
	char   		*sat,			/* satellite  E1, J1, etc...  */
	char		*phase_name )   /* phase name A, B, etc...    */
{

	int         number_of_recs ;

#ifdef DEBUG
	printf("%s(%d):  args:   sat=%s, phase_name=%s.\n", __FILE__, __LINE__, 
		sat, phase_name ) ;
#endif

	sprintf(where_clause, "where %s = '%s' and %s = '%s' ",
		APS_COL(EPHEMERIS, EPHEMERIS_SAT), sat,  
		APS_COL(EPHEMERIS, EPHEMERIS_PHASE_NAME), phase_name ) ;

#ifdef DEBUG
	PRINT_DIAG("%s(%d):  where_clause = %s\n", __FILE__, __LINE__, 
		where_clause ) ;
#endif

	number_of_recs = db_delete_records(APS_dbproc, 
		APS_TABLE(EPHEMERIS), where_clause) ;

#ifdef DEBUG
	PRINT_DIAG("%s(%d):  number_of_recs = %d\n", 
		__FILE__, __LINE__, number_of_recs) ;
#endif

	if ( number_of_recs == 1 )
		return 0 ;   /* deleted.  */
	else
		return 1 ;   /* not deleted.  */

}
