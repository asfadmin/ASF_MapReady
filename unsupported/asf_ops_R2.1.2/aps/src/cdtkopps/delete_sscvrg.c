#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/********************************************************************
*  Name:        delete_sscvrg
*  Module Type: SUBROUTINE      Language: C
*  Purpose:     GIVEN A darid, sitename, SAT, sensor, time bracket,
*		and the ascdsc flag, delete sscvrg records.  
*		the sscvrg relation contains data-take opportunities.
*
*    note that this routine is called from Fortran; the arguments are pointers
*
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  dbproc	*DBPROCESS 	pointer to a previously established 
*			Sybase process.  
*  DARID	*INT	serial number of dar in question. 
*  SITENAME	*CH*32	name of hypothetical site.  
*  SAT          *CH*2    SATELLITE NAME:  E1, J1, OR RS
*  SENSOR       *CH*3    SENSOR NAME, I.E. SAR, OPS, ETC.  
*			use sensor = "*" if the sensor is not specified.
*  T1		*double	T1 and T2 delimit the time bracket.  The times
*  T2		*double	ARE included in the time bracket.  
*  ASCDSC	*CHAR*1	ascending/descending flag.  
*
*  Output Parameters:
*  n_sscvrg	*int	the  number of records deleted from the cvrg
*			relation.  
*
****************************************************************************/
#pragma ident	"@(#)delete_sscvrg.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/cdtkopps/SCCS/s.delete_sscvrg.c"

#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>
#include <stdlib.h>

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
 
#include "db_sscvrg.h"   /* for the sscvrg relation.              */
 
void delete_sscvrg(
	DBPROCESS 	*dbproc, 
	int			*DARID,
	char 		*SITENAME, 
	double		*T1,
	double		*T2,
	char 		*SAT, 
	char 		*SENSOR, 
	char 		*ASCDSC, 
	DBINT 		*n_sscvrg )
{

RETCODE         return_code;
int				stat;

char 	where2[200];
char	sitename_buf[65];
*n_sscvrg = 0;

printf("delete_sscvrg:  dbproc = %x, darid = %d\n                sitename = \
%.32s\n", dbproc, *DARID, SITENAME);
printf("                t1 = %.9f, t2 = %.9f\n", *T1, *T2);
printf("                sat = %.2s, sensor = %.3s, ascdsc = %.1s\n", 
	SAT, SENSOR, ASCDSC );

sprintf(where_clause, "where %s = %d ",
	APS_COL(SSCVRG, SSCVRG_DARID), *DARID ) ;

if(*DARID == 0)
{
	/* if darid == 0, this is a site, and we need the sitename to     */
	/* identify the sscvrg record.  If != 0, this is a dar, and darid */
	/* is sufficient to identify the sscvrg record; don't need sitename */

	/* allow for a quote ' in the sitename:							*/
	stat = quote_doubler(SITENAME, sitename_buf, sizeof(sitename_buf));
	if(stat == -1)
	{
		printf("delete_sscvrg.c:  could not malloc.  out of memory.\n");
		banner_exit(1);
	}
	if(stat == -2)
	{
		printf("delete_sscvrg.c:  sitename_buff too small.\n");
		printf("                  error in code.\n");
		banner_exit(2);
	}
	if(stat < 0)
	{
		printf("delete_sscvrg.c:  error in code.\n");
		banner_exit(3);
	}

	sprintf(where2, "and %s = '%s' ", 
		APS_COL(SSCVRG, SSCVRG_SITENAME), sitename_buf ) ;
	strcat(where_clause, where2);
}

/* add in more to the where clause.  */
sprintf(where2, 
	"and %s >= %.9f and %s <= %.9f and %s = '%.2s' and %s = '%.3s' ", 
		APS_COL(SSCVRG, SSCVRG_STRTET), *T1, 
		APS_COL(SSCVRG, SSCVRG_STOPET), *T2, 
		APS_COL(SSCVRG, SSCVRG_SAT), SAT, 
		APS_COL(SSCVRG, SSCVRG_SENSOR), SENSOR ) ;

strcat(where_clause, where2);

printf("delete_sscvrg.c:  where_clause = \n%s\n", where_clause);

if(strncmp(ASCDSC, "*", 1) != 0)
{
	sprintf(where2, "and %s = '%.1s'", 
		APS_COL(SSCVRG, SSCVRG_ASCDSC), ASCDSC);
	strcat(where_clause, where2);
	printf("delete_sscvrg.c:  where_clause = %s\n", where_clause);
}

*n_sscvrg = db_delete_records( dbproc, APS_TABLE(SSCVRG),  where_clause ) ;

printf("delete_sscvrg:  returning:  n_sscvrg = %ld\n", *n_sscvrg);

}
