#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

#define PRINT_DIAG
#undef PRINT_DIAG
/*==============================================================================
Filename:	get_cvrg.c

Description:	contains code to retrieve cvrg recs into a sorted linked list 
		and to retrieve from the linked list.  oriented to Fortran 
		calls with all of the parameters in the arguments.  the 
		Fortran code does not need to know about the storage structure,
		only to know the pointers to the current and next records, 
		when processing the cvrg rercords.   

External Functions:
		retreive_cvrg.c	retrieves cvrg records and stores them into
				the linked list.  

		get_cvrg.c	obtains data from a cvrg record and frees 
				the memory for that record.  oriented to a 
				single pass thru the cvrg records retrieved.
	
Static Functions:
	
External Variables Defined:
		only the pointer to the current and next record structures
		are external.  
	
File Scope Static Variables:
	
Notes:

==============================================================================*/
#pragma ident	"@(#)get_cvrg.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/cdtkopps/SCCS/s.get_cvrg.c"

#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>
 
#include <stdlib.h>

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
 
#include "db_cvrg.h"       /* for the cvrg relation.              */

/* static for this file:  */
DB_RECORD       **cvrg_rec ;
llist           *cvrg_list = NULL ;
cursor          cvrg_list_ptr ;

/***************************************************************************
*  Name:	get_cvrg
*  Purpose:	given a pointer, this routine returns the data fields 
*		from the structure pointed to, including the pointer to
*		the next structure, if any.  
*		in addition, the memory is freed for data returned.  
*		oriented for Fortran calling; return values are in double 
*		precision for all real fields; the original data uses 
*		whatever is necessary.
*
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  p		cursor pointer to a data structure containing the desired 
*			cvrg data fields. 
*			NOTE:  when calling this routine from Fortran, use 
*			CALL get_cvrg(%VAL(p), ...  
*			pass this pointer by value.   
*			it is a bit easier this way for the C coder here. 
*
*  Output Parameters:
*  Name         Type    	Definition
*				all of these values are passed/returned by 
*				reference; they are all adresses.
*  p_next	*cursor	pointer to the next cvrg data structure or 0
*				if none.  
*  sat[3]	*char		satellite
*  sensor[4]	*char		sensor
*  mjdate	*double		ephemeris time in real julian date.  
*  rev		*int		rev number
*  sublat	*double		subsatellite lat/lon
*  sublon	*double		
*  nrlat	*double		near and far swath points lat/lon
*  nrlon	*double		
*  farlat	*double		
*  farlon	*double		
*  satalt	*double		satellite altitude in km.
*  sunflag[2]	*char		sun flag:  S or -
*  sunang	*double		solar zenith angle at subsat point.
*  station_id[4]	*char	ASF if within the ASF mask, MCM for McMurdo or 
*                       --- if not.  
*  masks[5]	*char		other mask flags.
*  opermode	*int		currently, not used.  
*  crossflag[2]	*char		D if descending node, N if ascending node, 
*				otherwise _
*  ascdsc[2]	*char		A for ascending, D for descending, S at the 
*				transition point, (inflection point).  
*				indicates that the subsatellite point is 
*				progressing northward, (A), southward (D), 
*				or is at the border between the 2 (S).
*
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
 
void get_cvrg (
	cursor		*p_rec, 
	cursor		*p_next_rec, 
	char 		*sat,
	char 		*sensor,
	double 		*mjdate,
	int 		*rev,
	double 		*sublat,
	double 		*sublon,
	double 		*nrlat,
	double 		*nrlon,
	double 		*farlat,
	double 		*farlon,
	double 		*satalt,
	char 		*sunflag,
	double 		*sunang,
	char 		*station_id,
	char 		*masks,
	int 		*opermode,
	char 		*crossflag,
	char 		*ascdsc		)
			
{

cursor	p_save, p, p_next ;

int	rcode;

#ifdef PRINT_DIAG
	printf("get_cvrg.c:  start:  *p_rec = %x\n", *p_rec );
#endif

if( *p_rec == NULL || *p_rec == 0 )
{
	/* error; a null pointer	*/
	printf("get_cvrg.c:  input pointer p has value NULL; \n");
	printf("get_cvrg.c:  p_rec = %x\n", p_rec );
	if( p_rec != NULL && p_rec != 0 )
		printf("             *p_rec = %x\n", *p_rec);
	printf("             please have the calling code test for NULL \n");
	printf("             before this call.   \n");
	printf("             terminating run.\n");
	rcode = system("banner ERROR");
	exit(101);
}
else
	p = *p_rec;

/* obtain the data for this record	*/
cvrg_rec = (DB_RECORD **) GET(cvrg_list, p) ;
if( cvrg_rec == NULL )
{
	printf("retrieve_cvrg:  %s(%d):  Falied to get next cvrg rec from list.\n",
		__FILE__, __LINE__ ) ;
	printf("pointer p = %x\n", p ) ;
	banner_exit(1) ;
}

strcpy(sat,		CAST_CVRG_SAT cvrg_rec[CVRG_SAT] ) ;
strcpy(sensor,	CAST_CVRG_SENSOR cvrg_rec[CVRG_SENSOR] ) ;

*mjdate		= CAST_CVRG_MJDATE cvrg_rec[CVRG_MJDATE] ;
*rev		= CAST_CVRG_REV cvrg_rec[CVRG_REV] ;
*sublat		= CAST_CVRG_SUBLAT cvrg_rec[CVRG_SUBLAT] ;
*sublon		= CAST_CVRG_SUBLON cvrg_rec[CVRG_SUBLON] ;
*nrlat		= CAST_CVRG_NRLAT cvrg_rec[CVRG_NRLAT] ;
*nrlon		= CAST_CVRG_NRLON cvrg_rec[CVRG_NRLON] ;
*farlat		= CAST_CVRG_FARLAT cvrg_rec[CVRG_FARLAT] ;
*farlon		= CAST_CVRG_FARLON cvrg_rec[CVRG_FARLON] ;
*satalt		= CAST_CVRG_SATALT cvrg_rec[CVRG_SATALT] ;
*sunflag	= CAST_CVRG_SUN cvrg_rec[CVRG_SUN] ;
*(sunflag+1) = '\0' ;
*sunang		= CAST_CVRG_SUNANG cvrg_rec[CVRG_SUNANG] ;

strcpy(station_id,	CAST_CVRG_STATION_ID cvrg_rec[CVRG_STATION_ID] ) ;
strcpy(masks,	CAST_CVRG_MASKS cvrg_rec[CVRG_MASKS] ) ;

*opermode	= CAST_CVRG_OPERMODE cvrg_rec[CVRG_OPERMODE] ;
*crossflag	= CAST_CVRG_CROSSFLAG cvrg_rec[CVRG_CROSSFLAG] ;
*(crossflag+1) = '\0' ;
*ascdsc		= CAST_CVRG_ASCDSC cvrg_rec[CVRG_ASCDSC] ;
*(ascdsc+1) = '\0' ;

/*
-- set up the cursor pointers for next call, using the input pointer
-- as the reference to NEXT.  the 2nd arg to NEXT is an i/o parameter:
*/
p_next = p ;
cvrg_rec = (DB_RECORD **) NEXT(cvrg_list, p_next ) ;
if(p_next == NULL)
{
	p_next = 0;
	/* 
	-- we have returned the LAST record.  data has been stored in 
	-- parameters, so no more need for the current or previous record.  
	-- since there is no next rec, the entire list can be freed at this time;
	-- no more need for list.  
	*/
	DEL_ALL( cvrg_list ) ;
}

p = 0;

/* return the pointers as required	*/
*p_rec 		= p;
*p_next_rec = p_next;

#ifdef PRINT_DIAG
printf("get_cvrg():  %s(%d):  return values:  \n", __FILE__, __LINE__ ) ;
printf("*p_rec 		= %x\n",  *p_rec ) ;
printf("*p_next_rec = %x\n",  *p_next_rec ) ;
printf("sat 		= %s\n",  sat ) ;
printf("sensor 		= %s\n",  sensor ) ;
printf("*mjdate 	= %lf\n", *mjdate ) ;
printf("*rev 		= %d\n",  *rev ) ;
printf("*sublat 	= %lf\n", *sublat ) ;
printf("*sublon 	= %lf\n", *sublon ) ;
printf("*nrlat 		= %lf\n", *nrlat ) ;
printf("*nrlon 		= %lf\n", *nrlon ) ;
printf("*farlat 	= %lf\n", *farlat ) ;
printf("*farlon 	= %lf\n", *farlon ) ;
printf("*satalt 	= %lf\n", *satalt ) ;
printf("sunflag 	= %s\n",  sunflag ) ;
printf("*sunang 	= %lf\n", *sunang ) ;
printf("station_id 	= %s\n",  station_id ) ;
printf("masks 		= %s\n",  masks ) ;
printf("*opermode 	= %d\n",  *opermode ) ;
printf("crossflag 	= %s\n",  crossflag ) ;
printf("ascdsc 		= %s\n",  ascdsc ) ;

#endif

return ;

}  /* end of   void get_cvrg ()   */


/***************************************************************************
*  Name:	retrieve_cvrg
*  Module Type: SUBROUTINE 	Language: C
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]retrieve_cvrg.c  $
*  Purpose:	COPIES COVERAGE DATA for a sat, sensor, rev, and time
*		bracket into a sorted linked list of cvrg structures.  
*		a pointer to the first structure is returned.  
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  APS_dbproc	*DBPROCESS pointer to sybase info structure.  
*  SAT		*CHAR*2	SATELLITE NAME
*  SENSOR	*CHAR*3	SENSOR
*  REV1		*int	rev bracket rev1, rev2
*  REV2		*int
*  T1		*double time bracket t1, t2
*  T2		*double 
*  ASCDSC	*char[2] A for ascending and D for descending, * for either.
*  NLAT		*double	NLAT, SLAT, ELON, WLON are the north/south 
*  SLAT		*double latitude bracket and the east/west longitude
*  ELON		*double	bracket for the retrieve.  
*  WLON		*double		note that West longitude is negative and 
*				East longitude is positive.  
*  INORML	*int	= 0 means a normal query; != 0 means that the 
*			geographical area of interest straddles the 
*			international dateline and it can be wierd, for 
*			example, you can't use the east-west longitude
*			bracket in the natural way.  
*  Output Parameters:
*  Name         Type    Definition
*  NRECS	*int	NUMBER OF RECORDS retrieved and stored.  
*  p_first	*cursor	pointer to the first of the data structures.
*			p_first is the address of the address of the structure.
*			p_first is the address of the pointer.  
*
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
  
void retrieve_cvrg (
	DBPROCESS	*APS_dbproc,
	char		*SAT,
	char		*SENSOR,
	int			*REV1,
	int			*REV2,
	double		*T1,
	double		*T2,
	char		*ASCDSC,
	double 		*NLAT,
	double 		*SLAT,
	double 		*ELON,
	double 		*WLON,
	int			*INORML,
	int			*NRECS,
	cursor 		*p_first	)
{

RETCODE		return_code;

char		buf2[100];


#ifdef PRINT_DIAG
	printf("retrieve_cvrg.c start: APS_dbproc=%x, sat = %.2s, sensor = %.3s\n", 
		APS_dbproc, SAT, SENSOR );
	printf("             REV1 = %d, REV2 = %d\n", *REV1, *REV2);
	printf("             T1 = %.9f, T2 = %.9f\n", *T1, *T2);
	printf("             ASCDSC = %.1s\n", ASCDSC);
	printf("             NLAT = %.5f, SLAT = %.5f\n", *NLAT, *SLAT);
	printf("             ELON = %.5f, WLON = %.5f\n", *ELON, *WLON);
	printf("             INORML = %d\n", *INORML);
#endif

/* initialize the output parameters:	*/
*NRECS = 0;
*p_first = 0;

/*
-- set up where_clause  
*/
if(*INORML == 0)
{
	/* this is a "normal" query.  */
	sprintf(where_clause, "where %s = '%.2s' and %s = '%.3s' and \
    %s >= %d and %s <= %d and \
    %s >= %.9f and %s <= %.9f and \
    %s <= %.5f and %s >= %.5f and \
    %s <= %.5f and %s >= %.5f ",
	APS_COL(CVRG, CVRG_SAT),    SAT,    APS_COL(CVRG, CVRG_SENSOR), SENSOR,
	APS_COL(CVRG, CVRG_REV),  *REV1,    APS_COL(CVRG, CVRG_REV),     *REV2,
	APS_COL(CVRG, CVRG_MJDATE), *T1,    APS_COL(CVRG, CVRG_MJDATE),    *T2,
	APS_COL(CVRG, CVRG_NRLAT), *NLAT,   APS_COL(CVRG, CVRG_NRLAT),    *SLAT,
	APS_COL(CVRG, CVRG_NRLON), *ELON,   APS_COL(CVRG, CVRG_NRLON),    *WLON ) ;

#ifdef PRINT_DIAG
		printf("retrieve_cvrg:  where_clause = \n%s\n", where_clause);
#endif
}
else
{
	/* 
	-- this is an "abnormal" query.  the area of interest 
	-- straddles the international dateline.  
	*/
	sprintf(where_clause, "where %s = '%.2s' and %s = '%.3s' and \
	%s >= %d and %s <= %d and \
	%s >= %.9f and %s <= %.9f and \
	%s <= %.5f and %s >= %.5f and \
	( %s <= %.5f or %s >= %.5f ) ",
	APS_COL(CVRG, CVRG_SAT),    SAT,    APS_COL(CVRG, CVRG_SENSOR), SENSOR,
	APS_COL(CVRG, CVRG_REV),  *REV1,    APS_COL(CVRG, CVRG_REV),     *REV2,
	APS_COL(CVRG, CVRG_MJDATE), *T1,    APS_COL(CVRG, CVRG_MJDATE),    *T2,
	APS_COL(CVRG, CVRG_NRLAT), *NLAT,   APS_COL(CVRG, CVRG_NRLAT),    *SLAT,
	APS_COL(CVRG, CVRG_NRLON), *ELON,   APS_COL(CVRG, CVRG_NRLON),    *WLON ) ;

#ifdef PRINT_DIAG
		printf("retrieve_cvrg: 'abnormal' where_clause = \n%s\n", where_clause);
#endif
}

if( *ASCDSC != '*') 
{
	/* add in the clause about ascending or descending	*/
	sprintf(buf2, " and ( %s = '%.1s' or %s = 'S' ) ",
		APS_COL(CVRG, CVRG_ASCDSC), ASCDSC,     APS_COL(CVRG, CVRG_ASCDSC) ) ;
	strcat(where_clause, buf2);
#ifdef PRINT_DIAG
	printf("retrieve_cvrg:  buf2 = %s\n", buf2);
	printf("retrieve_cvrg:  where_clause = \n%s\n", where_clause);
#endif
}

/* now add in the sort clause	*/
sprintf( orderby_cols, "%s ", APS_COL(CVRG, CVRG_MJDATE) ) ;

#ifdef PRINT_DIAG
	printf("retrieve_cvrg:  orderby_cols = \n%s\n", orderby_cols);
#endif

cvrg_list = db_get_records(APS_dbproc, APS_TABLE(CVRG),
	where_clause, orderby_cols, APS_CDEFS(CVRG), ALL_COLS) ;
if (cvrg_list == NULL)
{
	printf("retrieve_cvrg:  %s(%d):  DB query failed for cvrg relation.\n", 
		__FILE__, __LINE__ ) ;
	printf("where_clause = %s\n", where_clause ) ;
	printf("orderby_cols = %s\n", orderby_cols ) ;
	banner_exit(1) ;
}

*NRECS   = NUMELTS( cvrg_list ) ;
cvrg_rec = (DB_RECORD **) FIRST(cvrg_list, cvrg_list_ptr) ;
*p_first = cvrg_list_ptr ;

#ifdef PRINT_DIAG
printf("retrieve_cvrg:  %s(%d):  *NRECS = %d, cvrg_list_ptr = %x\n",
	__FILE__, __LINE__, *NRECS, cvrg_list_ptr ) ;
#endif

return ;

}
