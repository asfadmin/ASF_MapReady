#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	ask_sat_c.c

Description:	

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
static char SccsCopyright_1[] =
    "@(#) Copyright (c)1995, California Institute of Technology." ;
static char SccsCopyright_2[] =
    "@(#) U.S. Government Sponsorship acknowledged." ;
static char SccsFileID[] = "@(#)ask_sat_c.c	5.1 01/08/98 17:09:38" ;
static char SccsBlankLine[] = "@(#)" ;



/*==============================================================================
Function:       ask_sat_c()

Description:    lists, by number, the valid satellites for the current db. 
				prompts the user for a number.  Then returns the appropriate
				satellite.  

Creator:        Lawrence Stevens

Creation Date:  Mon Jan 22 12:12:13 PST 1996

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/

#include "unistd.h"   /* for sleep     */
#include "db_sybint.h"   /* for APS sybase interface routines. */
#include "aps_db_table.h"         /* for APS DB tables sybase interface */
#include "db_satsensor.h"         /* for satsensor db table.            */
#include "dapps_defs.h"           /* for TRUE and FALSE					*/

static int userstr_num(int *num)
{
/* end of line character <cr> is a ten.     */
#define EOL     10
 
	int     j;
	char    c;
	char	str[6] ;
	int		error_count = 0 ;
 
	j = 0;
	while ((c = getchar()) != EOL)
	{
		if ( error_count )
		{
			error_count ++ ;
			continue ;
		}

		if ( !isdigit(c) )
		{
			if ( !error_count )
				printf("\n Error:  integers only\n" ) ;
			sleep (1) ;
			error_count ++ ;
			continue ;
		}

		if ( j > 1 )
		{
			/* no third digit allowed.  */
			if ( !error_count )
				printf("\n Error:  too many digits\n" ) ;
			sleep (1) ;
			error_count ++ ;
			continue ;
		}

		*(str+j++) = c;

	}

	if ( error_count )
		return FALSE ;
 
	/* end of line:  terminate the user's string   */
	str[j] = '\0';
	*num = atoi(str) ;

	return TRUE ;
}


int ask_sat_c( 
	char	*sat, 
	int		*max_choice  )
{

	char    	str[10];
	int			choice ;
	int			j ;

	llist		*satsensor_list ;
	DB_RECORD	**satsensor_rec ;
	cursor		satsensor_list_ptr ;

	char		previous_sat[] = "12" ;

	*max_choice = 0 ;

	printf("\n SATELLITE:\n" ) ;
	/* 
	-- helpful error messages for 
	-- maintenance programmers when 
	-- changing code: 
	*/
	if ( sat == NULL )
	{
		printf("%s(%d):  sat == NULL\n", __FILE__, __LINE__ ) ;
		return FALSE ;
	}

	strcpy( sat, "" ) ;

	sprintf( where_clause, "where %s = 'Y'",
		APS_COL(SATSENSOR, SATSENSOR_CVRG_ALLOWED) ) ;

	sprintf(orderby_cols, "%s", APS_COL(SATSENSOR, SATSENSOR_SAT) ) ;

	satsensor_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
		APS_TABLE(SATSENSOR), where_clause, orderby_cols, APS_CDEFS(SATSENSOR),
		ALL_COLS) ;

	if ( satsensor_list == NULL)
	{
		printf("%s", " ERROR IN DATABASE QUERY ON SATSENSOR TABLE.  \n" ) ;
		return FALSE ;
	}

	if (   (*max_choice = NUMELTS( satsensor_list ) )  <= 0 )
	{
		printf(" No valid satellites were found.  \n" ) ;
		return FALSE ;
	}

#ifdef PRINT_DIAG
	db_print_list( satsensor_list, APS_CDEFS(SATSENSOR) ) ;
#endif

	/*
	-- remove the duplicate sat records.
	*/
	for (   satsensor_rec = (DB_RECORD **) FIRST(satsensor_list, 
													satsensor_list_ptr);
			satsensor_rec != NULL ;
			satsensor_rec = (DB_RECORD **) NEXT(satsensor_list, 
													satsensor_list_ptr)  
		)
	{
		if ( !strcmp(previous_sat, 
					 CAST_SATSENSOR_SAT satsensor_rec[SATSENSOR_SAT] )  )
		{
			/* same */
			DEL_AT_CURSOR( satsensor_list, satsensor_list_ptr ) ;
		}
		else
		{
			/* different */
			strcpy( previous_sat, 
				CAST_SATSENSOR_SAT satsensor_rec[SATSENSOR_SAT] ) ;
		}
	}

#ifdef PRINT_DIAG
	db_print_list( satsensor_list, APS_CDEFS(SATSENSOR) ) ;
#endif

	/* adjust number of satellites  */
	*max_choice = NUMELTS( satsensor_list ) ;

	j = 0 ;
	for (   satsensor_rec = (DB_RECORD **) FIRST(satsensor_list, 
													satsensor_list_ptr);
			satsensor_rec != NULL ;
			satsensor_rec = (DB_RECORD **) NEXT(satsensor_list, 
													satsensor_list_ptr)  
		)
	{
		printf("           %2d)  %s\n", 
			++j, CAST_SATSENSOR_SAT satsensor_rec[SATSENSOR_SAT] ) ;
	}

	printf("\n Satellite :");
	if ( !userstr_num(&choice) )
	{
		return FALSE ;
	}
	else
	{
#ifdef PRINT_DIAG
		printf(" choice was = %d\n", choice);
#endif
		if ( choice > *max_choice )
		{
			printf("\n Error:  number was too big\n" ) ;
			sleep (1) ;
			return FALSE ;
		}
		if ( choice <= 0 )
		{
			printf("\n Error:  number was too low\n" ) ;
			sleep (1) ;
			return FALSE ;
		}

		/* number was OK */
		satsensor_rec = (DB_RECORD **) FIRST(satsensor_list,satsensor_list_ptr);
		for ( j = 1 ; j < choice ; j ++ )
			satsensor_rec = (DB_RECORD **) NEXT(satsensor_list, 
				satsensor_list_ptr) ;
#ifdef PRINT_DIAG
		printf("sensor was %s\n", 
			CAST_SATSENSOR_SENSOR satsensor_rec[SATSENSOR_SENSOR] ) ;
#endif
		strcpy(sat, CAST_SATSENSOR_SAT satsensor_rec[SATSENSOR_SAT] ) ;
	}

	return TRUE ;
}
