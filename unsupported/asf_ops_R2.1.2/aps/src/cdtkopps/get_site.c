#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

#define PRINT_DIAG
#undef PRINT_DIAG
/********************************************************************
*  Name:        get_site
*  Module Type: SUBROUTINE      Language: C
*  Purpose:     GIVEN A sitename, gets all site data 
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  APS_dbproc	*DBPROCESS pointer to info about syabase database session.
*  SITENAME	*char 33  sitename which identifies the desired site.
*  Output Parameters:
*       --  fields froam the site relation  -----
*  SHAPE	*char 1	shape of site from site relation.  
*			P if a point/radius, Q if quadrilataral
*  RADIUS	*float	radius from site relation.  
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
*  COMMENTS	*char 51
*
*  IER		*int	return value.  0 if one record found; 1 if not. 
****************************************************************************/
#pragma ident	"@(#)get_site.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/cdtkopps/SCCS/s.get_site.c"
 
#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>
#include <stdlib.h>

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
 
#include "db_site.h"       /* for the site relation.              */

void get_site(

	DBPROCESS       *APS_dbproc,
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
	DBCHAR		*COMMENTS,

	int		*IER    )
	
{

int		rcode, system();
int		nrecs;
char	sitename_buf[65];
int		stat;

DB_RECORD       **site_rec ;
llist           *site_list = NULL ;
cursor          site_list_ptr ;

RETCODE	return_code;

#ifdef PRINT_DIAG
printf("get_site:  APS_dbproc = %x, sitename = %s\n", APS_dbproc, SITENAME);
#endif

*IER = 0;

/* allow for quote ' in sitename:  */

stat = quote_doubler(SITENAME, sitename_buf, sizeof(sitename_buf));
if(stat == -1)
{
	printf("get_site.c:  could not malloc.  out of memory.\n");
	banner_exit(1);
}
if(stat == -2)
{
	printf("get_site.c:  sitename_buff too small.\n");
	printf("             error in code.\n");
	banner_exit(1);
}
if(stat < 0)
{
	printf("get_site.c:  error in code.\n");
	banner_exit(2);
}

sprintf(where_clause, "where %s = '%s'", 
	APS_COL(SITE, SITE_SITENAME), sitename_buf);

#ifdef PRINT_DIAG
printf("get_site:  where_clause = >%s<\n", where_clause);
#endif

site_list = db_get_records(APS_dbproc, APS_TABLE(SITE),
	where_clause, orderby_cols, APS_CDEFS(SITE), ALL_COLS) ;
if (site_list == NULL)
{
    printf("DB query for site relation failed.\n" ) ;
	*IER = 1 ;
	return ;

}

nrecs = NUMELTS( site_list ) ;

if(nrecs <= 0)
{
	printf("get_site:  no records found in site relation for\n");
	printf("                sitename = >%s<\n", SITENAME);
	printf("		where_clause used:  \n>%s<\n", where_clause);
	*IER = 1;
	/*
	-- clean up.
	*/
	DEL_ALL( site_list ) ;
	return;
}
else if(nrecs > 1)
{
	printf(
	"get_site:  more than 1 record found in site relation for\n");
	printf("                sitename = >%s<\n", SITENAME);
	printf("		where_clause used:  \n>%s<\n", where_clause);
	*IER = 1;
	/*
	-- clean up.
	*/
	DEL_ALL( site_list ) ;
	return;
}

site_rec = FIRST(site_list, site_list_ptr ) ;

#ifdef PRINT_DIAG
printf("%s(%d):  retrieved values:\n", __FILE__, __LINE__ );
db_print_record(site_rec, APS_CDEFS(SITE) ) ;
#endif

/* 
-- assign values to output variables.  
*/
/* SHAPE is a single-character:  */
*SHAPE = CAST_SITE_SHAPE site_rec[SITE_SHAPE] ;

*RADIUS = CAST_SITE_RADIUS site_rec[SITE_RADIUS] ;
*NWLAT = CAST_SITE_NWLAT site_rec[SITE_NWLAT] ;
*NWLON = CAST_SITE_NWLON site_rec[SITE_NWLON] ;
*NELAT = CAST_SITE_NELAT site_rec[SITE_NELAT] ;
*NELON = CAST_SITE_NELON site_rec[SITE_NELON] ;
*SELAT = CAST_SITE_SELAT site_rec[SITE_SELAT] ;
*SELON = CAST_SITE_SELON site_rec[SITE_SELON] ;
*SWLAT = CAST_SITE_SWLAT site_rec[SITE_SWLAT] ;
*SWLON = CAST_SITE_SWLON site_rec[SITE_SWLON] ;
strcpy( COMMENTS, CAST_SITE_COMMENTS site_rec[SITE_COMMENTS] ) ;

/*
-- clean up.
*/
DEL_ALL( site_list ) ;
*IER = 0 ;
return;

}
