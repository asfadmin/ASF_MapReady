#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	cvrg_allowed.c

Description:	
holds cvrg_allowed()

External Functions Defined:
cvrg_allowed()
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)cvrg_allowed.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.cvrg_allowed.c"



/*==============================================================================
Function:       cvrg_allowed

Description:    accepts satellite, sensor, determines if nominal 
				sensor coverage is allowed by the data in satsensor.  
				the field satsensor.cvrg_allowed is the determining 
				factor.  
Returns:
			0	cvrg is allowed.  
			1   cvrg is NOT allowed.  
			< 0 an error occurred.  
					-1	database could not be opened.  
					-2	query failed.  
					-3	no records found
					-4  number of records found was != 1.  

Creator:        Lawrence Stevens

Creation Date:  Thu Oct 12 17:51:00 PDT 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include <stdio.h>        /* for sprintf()     */
#include "db_sybint.h"    /* for APS sybase interface routines.  
								includes dapps_list.h  */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
#include "db_satsensor.h"   /* for satsensor table                  */


int cvrg_allowed( 
	char	*sat,		/* input satellite   */
	char	*sensor )	/* input sensor.   */
{

DB_RECORD       **satsensor_rec ;
llist           *satsensor_list = NULL ;
cursor          satsensor_list_ptr ;

#ifdef PRINT_DIAG
printf("%s(%d):  sat = %s, sensor = %s\n", __FILE__, __LINE__, sat, sensor ) ;
#endif

sprintf(where_clause, "where %s = '%s' and %s = '%s'",
	APS_COL(SATSENSOR, SATSENSOR_SAT), sat,
	APS_COL(SATSENSOR, SATSENSOR_SENSOR), sensor ) ;
 
#ifdef PRINT_DIAG
printf("%s(%d):  where_clause = \n%s\n", __FILE__, __LINE__, where_clause ) ;
#endif

/* must clean up this list later:  */
satsensor_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
	APS_TABLE(SATSENSOR), where_clause, NULL, APS_CDEFS(SATSENSOR), ALL_COLS) ;

if (satsensor_list == NULL)
{
	printf("%s(%d):  DB query failed on relation %s\n", __FILE__, __LINE__, 
		APS_TABLE(SATSENSOR) ) ;
	printf("         where_clause = %s\n", where_clause ) ;
	return -2 ;
}

if ( NUMELTS(satsensor_list) == 0 )
{
	printf("%s(%d):  No records in %s relation found from query.\n", 
		__FILE__, __LINE__, APS_TABLE(SATSENSOR) ) ;
	printf("         where_clause = %s\n", where_clause ) ;
	printf("         There should be one record.  \n" ) ;
	return -3 ;
}

if ( NUMELTS(satsensor_list) != 1 )
{
	printf("%s(%d):  %d records in %s relation from query.\n", 
		__FILE__, __LINE__, NUMELTS(satsensor_list), APS_TABLE(SATSENSOR) ) ;
	printf("         where_clause = %s\n", where_clause ) ;
	printf("         There should be only one record.  \n" ) ;
	DEL_LIST( satsensor_list ) ;
	return -4 ;
}

/* a good result from the query.   */
satsensor_rec = (DB_RECORD **) FIRST(satsensor_list, satsensor_list_ptr) ;

#ifdef PRINT_DIAG
printf("%s(%d):  satsensor_rec = \n", __FILE__, __LINE__ ) ;
db_print_record( satsensor_rec, APS_CDEFS(SATSENSOR) ) ;
#endif


if ( CAST_SATSENSOR_CVRG_ALLOWED satsensor_rec[SATSENSOR_CVRG_ALLOWED] == 'Y' )
{
	DEL_LIST( satsensor_list ) ;
	return 0 ;    /* nominal coverage is allowed.  */
}
else
{
	DEL_LIST( satsensor_list ) ;
	return 1 ;
}

}
