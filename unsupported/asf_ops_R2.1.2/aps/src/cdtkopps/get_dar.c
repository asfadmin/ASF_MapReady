#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

#undef PRINT_DIAG
/********************************************************************
*  Name:        get_dar
*  Module Type: SUBROUTINE      Language: C
*  Purpose:     GIVEN A DARID, gets all dar data 
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  dbproc	*DBPROCESS pointer to info about syabase database session.
*  DARID        *int    darid which identifies the desired dar.
*  Output Parameters:
*       --  fields from the dar relation  -----
*  USERID	*char string
*  REQTIME	*char 22
*  REQSTAT	*char 4
*  PRVDARID	*int
*  PRVREQSTAT	*char 4
*  SAT		*char 3
*  SENSOR	*char 4
*  STRTTIME		*char 22
*  ENDTIME		*char 22
*  SITENAME	*char 33
*  SHAPE	*char 1	shape of dar from dar relation.  
*			P if a point/radius, Q if quadrilataral, R if rectangle
*  RADIUS	*float	radius from dar relation.  
*			if point/radius, this is the radius in km.  otherwise=0
*  NWLAT	*float	if point/radius, the center.  otherwise, the NW corner
*  NWLON	*float	
*
*  NELAT	*float	if point/radius, 0.  otherwise, the NE corner
*  NELON	*float	
*
*  SELAT	*float	if point/radius, 0.  otherwise, the SE corner
*  SELON	*float	
*
*  SWLAT	*float	if point/radius, 0.  otherwise, the SW corner
*  SWLON	*float	
*
*  NOBS		*int
*  FOBS		*char 15
*  REV		*int
*  ascdsc	*char 1
*  USERCMT	*char 201
*  PLNRCMT	*char 201
*
*  IER		*int	return value.  0 if one record found; 1 if not. 
****************************************************************************/
#pragma ident	"@(#)get_dar.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/cdtkopps/SCCS/s.get_dar.c"
 
#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>
 
#include <stdlib.h>

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
 
#include "db_dar.h"       /* for the dar relation.              */
  

void get_dar(
    DBPROCESS       *dbproc,
	DBINT		*DARID,
	DBCHAR		*USERID,
	DBCHAR		*REQTIME,
	DBCHAR		*REQSTAT,
	DBINT		*PRVDARID,
	DBCHAR		*PRVREQSTAT,
	DBCHAR		*SAT,
	DBCHAR		*SENSOR,
	DBCHAR		*STRTTIME,
	DBCHAR		*ENDTIME,
	DBCHAR		*SITENAME,
	DBCHAR		*SHAPE,
	DBREAL		*RADIUS,
	DBREAL		*NWLAT,
	DBREAL		*NWLON,
	DBREAL		*NELAT,
	DBREAL		*NELON,
	DBREAL		*SELAT,
	DBREAL		*SELON,
	DBREAL		*SWLAT,
	DBREAL		*SWLON,
	int			*NOBS,
	DBCHAR		*FOBS,
	DBINT		*REV,
	DBCHAR		*ASCDSC,
	DBCHAR		*USERCMNT,
	DBCHAR		*PLNRCMNT,

	int		*IER    )
	
{

char		buf[100];
int			rcode ;

int			nrecs;
DBTINYINT	nobs;

DB_RECORD   **dar_rec ;
llist       *dar_list = NULL ;
cursor      dar_list_ptr ;

RETCODE	return_code;

#ifdef PRINT_DIAG
printf("get_dar:  dbproc = %x, darid = %d\n", dbproc, *DARID);
#endif


*IER = 0;

sprintf(where_clause, "where %s = %ld ",
	APS_COL(DAR, DAR_DARID), *DARID ) ;

#ifdef PRINT_DIAG
printf("get_dar:  where_clause = >%s<\n", where_clause);
#endif

dar_list = db_get_records(dbproc, APS_TABLE(DAR),
	where_clause, NULL, APS_CDEFS(DAR), ALL_COLS) ;
if (dar_list == NULL)
{
	printf("DB query for dar relation failed.\n" ) ;
	*IER = 1 ;
	return ;
}

nrecs = NUMELTS( dar_list ) ;

if(nrecs <= 0)
{
	printf("get_dar:  no records found in dar relation for\n");
	printf("                darid = %ld\n", *DARID);
	printf("		where_clause used:  \n>%s<\n", where_clause);
	*IER = 1;
	/* 
	-- clean up.  
	*/
	DEL_ALL( dar_list ) ;
	return;
}
else if(nrecs > 1)
{
	printf(
	"get_dar:  more than 1 record found in dar relation for\n");
	printf("                darid = %ld\n", *DARID);
	printf("		where_clause used:  \n>%s<\n", where_clause);
	*IER = 1;
	/* 
	-- clean up. 
	*/
	DEL_ALL( dar_list ) ;
	return;
}

dar_rec = (DB_RECORD **) FIRST(dar_list, dar_list_ptr) ;

#ifdef PRINT_DIAG
printf("get_dar:  retrieved values:\n" ) ;
db_print_record(dar_rec, APS_CDEFS(DAR) ) ;
#endif

/* asign values from the dar_rec to the output parameters.  */
strcpy(USERID, CAST_DAR_USERID dar_rec[DAR_USERID] ) ;
strcpy(REQTIME, CAST_DAR_REQTIME dar_rec[DAR_REQTIME] ) ;
strcpy(REQSTAT, CAST_DAR_REQSTAT dar_rec[DAR_REQSTAT] ) ;
*PRVDARID = CAST_DAR_PRVDARID dar_rec[DAR_PRVDARID] ;
strcpy(PRVREQSTAT, CAST_DAR_PRVREQSTAT dar_rec[DAR_PRVREQSTAT] ) ;
strcpy(SAT, CAST_DAR_SAT dar_rec[DAR_SAT] ) ;
strcpy(SENSOR, CAST_DAR_SENSOR dar_rec[DAR_SENSOR] ) ;
strcpy(STRTTIME, CAST_DAR_STRTTIME dar_rec[DAR_STRTTIME] ) ;
strcpy(ENDTIME, CAST_DAR_ENDTIME dar_rec[DAR_ENDTIME] ) ;
strcpy(SITENAME, CAST_DAR_SITENAME dar_rec[DAR_SITENAME] ) ;
*SHAPE = CAST_DAR_SHAPE dar_rec[DAR_SHAPE] ;
*(SHAPE+1) = '\0' ;
*RADIUS = CAST_DAR_RADIUS dar_rec[DAR_RADIUS] ;

*NWLAT = CAST_DAR_NWLAT dar_rec[DAR_NWLAT] ;
*NWLON = CAST_DAR_NWLON dar_rec[DAR_NWLON] ;
*NELAT = CAST_DAR_NELAT dar_rec[DAR_NELAT] ;
*NELON = CAST_DAR_NELON dar_rec[DAR_NELON] ;
*SELAT = CAST_DAR_SELAT dar_rec[DAR_SELAT] ;
*SELON = CAST_DAR_SELON dar_rec[DAR_SELON] ;
*SWLAT = CAST_DAR_SWLAT dar_rec[DAR_SWLAT] ;
*SWLON = CAST_DAR_SWLON dar_rec[DAR_SWLON] ;

*NOBS = CAST_DAR_NOBS dar_rec[DAR_NOBS] ;
strcpy(FOBS, CAST_DAR_FOBS dar_rec[DAR_FOBS] ) ;
*REV = CAST_DAR_REV dar_rec[DAR_REV] ;
*ASCDSC = CAST_DAR_ASCDSC dar_rec[DAR_ASCDSC] ;

strcpy(USERCMNT, CAST_DAR_USERCMNT dar_rec[DAR_USERCMNT] ) ;
strcpy(PLNRCMNT, CAST_DAR_PLNRCMNT dar_rec[DAR_PLNRCMNT] ) ;

/* 
-- completed assigning the return values.  
-- clean up.  
*/
DEL_ALL( dar_list ) ;

#ifdef PRINT_DIAG
/*
-- print out the return values:
*/
printf("%s(%d):  return values:  \n", __FILE__, __LINE__ ) ;
printf(" *DARID = %d\n", *DARID ) ;
printf(" USERID string = %s.\n", USERID ) ;
printf(" REQTIME string = %s.\n", REQTIME ) ;
printf(" REQSTAT string = %s.\n", REQSTAT ) ;
printf(" *PRVDARID = %d\n", *PRVDARID ) ;
printf(" PRVREQSTAT string = %s.\n", PRVREQSTAT ) ;
printf(" SAT string = %s.\n", SAT ) ;
printf(" SENSOR string = %s.\n", SENSOR ) ;
printf(" STRTTIME string = %s.\n", STRTTIME ) ;
printf(" ENDTIME string = %s.\n", ENDTIME ) ;
printf(" SITENAME string = %s.\n", SITENAME ) ;
printf(" SHAPE string = %c.\n", *SHAPE ) ;
printf(" *RADIUS = %f\n", *RADIUS ) ;
printf(" *NWLAT = %f\n", *NWLAT ) ;
printf(" *NWLON = %f\n", *NWLON ) ;
printf(" *NELAT = %f\n", *NELAT ) ;
printf(" *NELON = %f\n", *NELON ) ;
printf(" *SELAT = %f\n", *SELAT ) ;
printf(" *SELON = %f\n", *SELON ) ;
printf(" *SWLAT = %f\n", *SWLAT ) ;
printf(" *SWLON = %f\n", *SWLON ) ;
printf(" *NOBS = %d\n", *NOBS ) ;
printf(" FOBS string = %s.\n", FOBS ) ;
printf(" *REV = %d\n", *REV ) ;
printf(" ASCDSC string = %s.\n", ASCDSC ) ;
printf(" USERCMNT string = %s.\n", USERCMNT ) ;
printf(" PLNRCMNT string = %s.\n", PLNRCMNT ) ;
#endif

*IER = 0 ;

return;

}
