#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*  TESTED:  
/*  test everything:  
#define TEST_NASDAc_phase_rsp2firstrev
#define TEST_NASDAc_phase_rsp1
#define TEST_phase_asftime2rev
#define TEST_NASDAc_phase_rev2rsp
#define TEST_NASDAc_asftime_rsp_angle_2_rev_time
#define TEST_NASDAc_asfdate_rsp_2_rev
#define TEST_check_rev_asftimes
#define TEST_phase_rev2asftime
#define TEST_asftime_2_phase
*/

/*  TEST LATER:  
*/


/* NOW testing:    */
#define TEST_asftime2rev
#define TEST_NASDAc_rev_asftime_2_rsp_angle

#include "nasda.h"
#include "phase_utilities.h"
#include "timeconv.h"
#include "string.h"

/* FOR SYBASE INTERFACES   */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
#include "dapps_list.h"     /* for APS linked list macros           */

/* FOR LAT/LON FIELDS        */
#include "check_lat_lon.h"

static void nasda_decode_condition(int code) ;
static void nasda_decode_condition_2(int code) ;

/*==============================================================================
Filename:		main.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)main.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_phase/SCCS/s.main.c"

void main(int argc, char *argv[])
{

int				return_code ;
int				rcode ;
char			msg[100] ;
int				j ;
int             err_code;
char            dbname[] = "larrycsa";        /* name of database         */
char            sybase_userid[] = "larry"; /* sybase-approved userid   */
char            password[] = "iceland";      /* sybase-approved password */

DB_RECORD	**phase_rec;

llist		*phase_list = NULL ;
cursor		phase_list_ptr;
int         nrecs ;
int			rev ;
int			rsp ;
int			rsp_checkup ;
double		rsp_angle ;
char		asftime_out[22] ;
char		asftime[22] ;
char		asftime_start[22] ;
char		asftime_end[22] ;
double		et_start, et_end ;
double		etime ;
/**************************************************************************/
/*                                                                        */
/*       TABLES THAT CONTAIN TEST DATA                                    */
/*                                                                        */
/**************************************************************************/
#ifdef TEST_check_rev_asftimes
struct _check_rev_asftimes_args
{
	char        *sat;           /* input satellite                */
	int         rev;            /* input rev satellite            */
	char        *strttime;      
	char        *stoptime;      
} ;

struct _check_rev_asftimes_args check_rev_asftimes_args[] = 
{
	{"J1", 464, "1992:073:00:49:40.000", "1992:073:00:59:40.000"}, /* OK */
	{"J1", 465, "1992:073:00:49:40.000", "1992:073:00:59:40.000"}, /* not OK */
	{"J1", 464, "1992:073:00:49:40.000", "1992:073:02:25:48.740"}, /* OK */
	{"J1", 464, "1992:073:00:49:40.000", "1992:073:02:25:48.741"}, /* not OK */
	{"J1", 465, "1992:073:02:25:48.741", "1992:073:04:01:57.481"}, /* OK */
	{"J1", 465, "1992:073:02:25:48.741", "1992:073:04:01:57.482"}, /* not OK */
	{"J1", 465, "1992:073:02:25:48.740", "1992:073:04:01:57.481"}, /* not OK */
	{"J1", 565, "1992:073:02:25:48.740", "1992:073:04:01:57.481"},/*rev after */
	{"J1", 464, "1992:073:02:25:48.741", "1992:073:04:01:57.481"},/*rev before*/
	{"J1", 465, "1992:073:02:25:48.741", "1992:073:04:01:57.481"}, /* OK */
	{"J1", 464, "1992:073:00:49:40.000", "1992:073:02:25:48.741"}, /* not OK */
	{NULL, 0, NULL, NULL},     /* early list terminator  */
	{"J1", 463, "1992:073:00:49:40.000", "1992:073:00:49:40.000"},
	{NULL, 0, NULL, NULL}
} ;
#endif

#ifdef TEST_phase_rev2asftime
struct _phase_rev2asftime_args
{
	char        *sat;         /* input satellite                */
	int         rev;          /* input rev satellite            */
} ;
struct _phase_rev2asftime_args phase_rev2asftime_args[] = 
{
	{"J1", 463},
	{"J1", 464},
	{"J1", 465},
	{"E1", 211},
	{"E1", 212},
	{"E1", 2166},
	{"E1", 2167},
	{"E1", 2353},
	{"E1", 2354},
	{"E1", 3712},
	{"E1", 3713},
	{"E1", 3734},
	{"E1", 3735},
	{"E1", 19247},
	{"E1", 19248},
	{"E1", 19249},
	{"E1", 19215},
	{"E1", 19216},
	{"E1", 21031},
	{"E1", 21032},
	{"R1", 1},
	{"R1", 2},
	{"R1", 230},
	{"R1", 230},
	{NULL, 0},       /* early list terminator  */
	{NULL, 0}
} ;
#endif

char		*test_asftimes[] = 
{
	"1981:212:20:52:38.282",
	"1991:212:20:52:38.282",
	"1991:212:20:52:38.283",
	"1991:212:20:52:38.284",
	"1991:349:06:21:56.422",
	"1991:349:06:21:56.429",
	"1991:362:07:36:18.606",
	"1991:362:07:36:18.607",
	"1991:362:07:36:18.608",
	"1992:073:00:49:40.000",
	"1992:117:00:49:40.000",
	"1996:205:00:10:05.238",
	NULL                     
} ;

char		*test_asftimes2[] = 
{
	"1981:212:20:52:38.282",
	"1992:073:00:48:38.282",
	"1992:073:00:49:39.999",
	"1992:073:00:49:40.000",
	"1992:073:00:49:40.001",
	"1992:073:00:49:41.001",
	"1992:073:00:49:42.001",
	"1992:073:00:49:43.001",
	"1992:117:00:49:43.001",
	"1992:161:00:49:43.001",
	"1992:161:02:29:43.001",
	"1992:161:04:09:43.001",
	"1999:205:00:10:05.238",
	NULL                     
} ;

#ifdef TEST_NASDAc_asftime_rsp_angle_2_rev_time
struct _NASDAc_asftime_rsp_angle_2_rev_time_args
{
	char        sat[3];         /* input NASDA satellite                */
	char        asftime[22];    /* input time.                          */
	int         rsp;            /* input rsp                            */
	double      rsp_angle;      /* input angle                          */
	int         search_flag;    /* SEARCH_FORWARD or SEARCH_BACKWARD    */
} ;

struct _NASDAc_asftime_rsp_angle_2_rev_time_args sub_args[] = 
{
	{"J1", "1991:205:00:10:05.238", 449, 100.0, SEARCH_FORWARD },
	{"J1", "1992:073:00:49:40.000",  60,   0.0, SEARCH_FORWARD },
	{"J1", "1992:073:00:49:40.000",  61,   0.0, SEARCH_FORWARD },
	{"J1", "1992:073:06:49:40.000",  61,   0.0, SEARCH_FORWARD },
	{"J1", "1992:073:12:49:40.000",  61,   0.0, SEARCH_FORWARD },
	{"J1", "1992:073:18:49:40.000",  61,   0.0, SEARCH_FORWARD },
	{"J1", "1992:074:00:40:40.000",  61,   0.0, SEARCH_FORWARD },
	{"J1", "1992:074:00:49:00.000",  61,   0.0, SEARCH_FORWARD },
	{"J1", "1992:074:00:50:00.000",  61,   0.0, SEARCH_FORWARD },
	{"J1", "1992:074:00:52:00.000",  61,   0.0, SEARCH_FORWARD },
	{"J1", "1992:074:01:52:00.000",  61,   0.0, SEARCH_FORWARD },
	{"J1", "1992:074:02:52:00.000",  61,   0.0, SEARCH_FORWARD },
	{"J1", "1992:074:02:52:00.000",  61,   0.0, SEARCH_BACKWARD },
	{"J1", "1992:074:00:49:40.000",  61,   0.0, SEARCH_BACKWARD },
	{"J1", "1992:073:00:49:40.000",  61,   0.0, SEARCH_FORWARD },
	{"J1", "1992:073:00:49:40.000", 104, 180.0, SEARCH_FORWARD },
	{"J1", "1992:073:00:49:40.000",  61, 359.9, SEARCH_FORWARD },
	{"J1", "1992:074:00:49:40.000",  61, 359.9, SEARCH_BACKWARD },
	{"J1", "1992:073:02:25:00.000", 105,   0.0, SEARCH_FORWARD },
	{"J1", "1992:073:02:25:00.000", 106,   0.0, SEARCH_FORWARD },
	{"E1", "1992:073:00:49:40.000", 449, 100.0, SEARCH_FORWARD },
	{"", "", 0, 0.0, 0 }
} ;
#endif

#ifdef TEST_NASDAc_rev_asftime_2_rsp_angle
struct _NASDAc_rev_asftime_2_rsp_angle_args
{
	char        sat[3];         /* input NASDA satellite                */
	char        asftime[22];    /* input time.                          */
	int         rev;            /* input rev                            */
} ;

struct _NASDAc_rev_asftime_2_rsp_angle_args    sub_args_2[] = 
{
	{"J1", "1992:073:00:49:30.000",  464 },
	{"J1", "1992:073:00:49:40.000",  464 },
	{"J1", "1992:073:01:13:42.240",  464 },
	{"J1", "1992:073:01:37:44.380",  464 },
	{"J1", "1992:073:02:25:48.620",  464 },
	{"J1", "1992:117:00:49:40.000", 1123 },
	{"J1", "1992:161:00:49:40.000", 1782 },
	{"E1", "1992:073:00:49:40.000",  555 },
	{"", "", 0 }
} ;
#endif


#ifdef TEST_NASDAc_phase_rev2rsp
/* arguments for NASDAc_phase_rev2rsp() testing   */
int		nasda_revs[] = {
	1, 463, 464, 465, 
	474, 475, 476, 477, 478, 479, 485, 486, 487, 488, 489, 
	508, 9999, 99999, 0} ;
#endif /* TEST_NASDAc_phase_rev2rsp  */

/**************************************************************************/
/*                                                                        */
/*       TEST CODE STARTS HERE                                            */
/*                                                                        */
/**************************************************************************/

return_code = system("banner START") ;
fflush(stdout);

#ifdef TEST_asftime2rev
for ( j = 0 ; test_asftimes[j] != NULL; j++ )
{
	printf("\n%s(%d):  calling asftime2rev E1 %s\n", 
		__FILE__, __LINE__, test_asftimes[j] ) ;
	return_code = asftime2rev("E1", test_asftimes[j], &rev ) ;
	if ( return_code < 0 )
	{
		printf("%s(%d):  %s\n", __FILE__, __LINE__, 
			PHASE_ERROR_MESSAGE( return_code )  ) ;
	}
	else 
	{
		printf("REV NO. = %d\n", rev ) ;
		return_code = rev2asftime("E1", rev, asftime_start, asftime_end ) ;
		if ( return_code < 0 )
		{
			printf("%s(%d):  %s\n", __FILE__, __LINE__, 
				PHASE_ERROR_MESSAGE( return_code )  ) ;
		}
		else
			printf("TIMES FOR REV = %s  %s\n", asftime_start, asftime_end ) ;
	}

	printf("\n%s(%d):  calling asftime2rev J1 %s\n", 
		__FILE__, __LINE__, test_asftimes[j] ) ;
	return_code = asftime2rev("J1", test_asftimes[j], &rev ) ;
	if ( return_code < 0 )
	{
		printf("%s(%d):  %s\n", __FILE__, __LINE__, 
			PHASE_ERROR_MESSAGE( return_code )  ) ;
	}
	else 
	{
		printf("REV NO. = %d\n", rev ) ;
		return_code = rev2asftime("J1", rev, asftime_start, asftime_end ) ;
		if ( return_code < 0 )
		{
			printf("%s(%d):  %s\n", __FILE__, __LINE__, 
				PHASE_ERROR_MESSAGE( return_code )  ) ;
		}
		else
			printf("TIMES = %s  %s\n", asftime_start, asftime_end ) ;
	}
}
#endif

/* get a Nasda phase   */
(void)strcpy(asftime, "1993:001:00:00:00.000" ) ;
printf("\n%s(%d):  calling asftime_2_phase J1 %s\n", 
	__FILE__, __LINE__, asftime ) ;
return_code = tc_asf2et(asftime, &etime ) ;
return_code = asftime_2_phase( "J1", asftime, &phase_rec) ;
if ( return_code < 0 )
{
	printf("%s(%d):  %s\n", __FILE__, __LINE__, 
		PHASE_ERROR_MESSAGE( return_code )  ) ;
	exit (1) ;
}
else 
{
	nasda_decode_condition(return_code ) ;
	printf("\n" ) ;
	db_print_record(phase_rec, APS_CDEFS(PHASE) ) ;
}
fflush(stdout) ;
#ifdef TEST_check_rev_asftimes
for ( j=0; check_rev_asftimes_args[j].sat != NULL; j++)
{
	printf("\n%s(%d):  ---------------------------------------------------\n",
		__FILE__, __LINE__ ) ;

	printf("%s(%d):  calling check_rev_asftimes %s %d \n\t\t\t\t%s %s \n",
		__FILE__, __LINE__, 
		check_rev_asftimes_args[j].sat,
		check_rev_asftimes_args[j].rev, 
		check_rev_asftimes_args[j].strttime, 
		check_rev_asftimes_args[j].stoptime ) ;

	return_code = check_rev_asftimes(
		check_rev_asftimes_args[j].sat,
		check_rev_asftimes_args[j].rev, 
		check_rev_asftimes_args[j].strttime, 
		check_rev_asftimes_args[j].stoptime,
		&rev, asftime_start, asftime_end ) ;

	if ( return_code < 0 )
	{
		printf("%s(%d):  error from check_rev_asftimes:  %d: %s\n", 
			__FILE__, __LINE__,
			return_code, PHASE_ERROR_MESSAGE(return_code) ) ;
		continue ;
	}

	printf( "%s(%d):  returned from phase_rev2asftime: %d\n",
		__FILE__, __LINE__, return_code ) ;
	printf( "      outputs:  rev = %d,\t%s %s\n",
		rev, asftime_start, asftime_end ) ;
}

#endif


#ifdef TEST_phase_rev2asftime
for ( j=0; phase_rev2asftime_args[j].sat != NULL; j++)
{
	printf("\n%s(%d):  ---------------------------------------------------\n",
		__FILE__, __LINE__ ) ;

	printf("%s(%d):  calling rev_2_phase %s %d \n",
		__FILE__, __LINE__, phase_rev2asftime_args[j].sat,
		phase_rev2asftime_args[j].rev ) ;

	return_code = rev_2_phase(
		phase_rev2asftime_args[j].sat, 
		phase_rev2asftime_args[j].rev, 
		&phase_rec ) ;

	if ( return_code < 0 )
	{
		printf("%s(%d):  error from rev_2_phase:  %d: %s\n", __FILE__, __LINE__,
			return_code, PHASE_ERROR_MESSAGE(return_code) ) ;
		continue ;
	}
	else
	{
		nasda_decode_condition_2(return_code) ;
		printf("\n") ;
	}

	printf("%s(%d):  calling phase_rev2asftime %s %d \n",
		__FILE__, __LINE__, phase_rev2asftime_args[j].sat,
		phase_rev2asftime_args[j].rev ) ;

	return_code = phase_rev2asftime( phase_rec, phase_rev2asftime_args[j].rev, 
		asftime_start, asftime_end ) ;

	if ( return_code < 0 )
	{
		printf("%s(%d):  error from phase_rev2asftime:  %d: %s\n", 
			__FILE__, __LINE__, return_code, 
			PHASE_ERROR_MESSAGE(return_code) ) ;
		continue ;
	}

	printf( "%s(%d):  returned from phase_rev2asftime:  \n", __FILE__,__LINE__);
	printf( "      asftime_start = %s, asftime_end = %s\n", 
		asftime_start, asftime_end ) ;
}
#endif

#ifdef TEST_NASDAc_asfdate_rsp_2_rev
(void)strcpy(asftime, "1992:074:00:49:40.000" ) ;
for ( j = 0; j < 661 ; j++ ) 
{
	printf("\n%s(%d):  calling NASDAc_asfdate_rsp_2_rev J1 %s RSP = %d\n",
		__FILE__, __LINE__, asftime, j ) ;

	return_code = NASDAc_asfdate_rsp_2_rev("J1", asftime, j, &rev ) ;

	if ( return_code < 0 )
	{
		rcode = decode_error_message(return_code, msg ) ;
		printf("%s(%d):  return_code = %d  %s\n", __FILE__, __LINE__, 
			return_code, msg ) ;
	}
	else
		printf("%s(%d):  return_code = %d NOT_AN_ERROR\n", __FILE__, __LINE__,
			return_code ) ;

	printf("%s(%d):  returned from NASDAc_asfdate_rsp_2_rev:  rev = %d\n",
		__FILE__, __LINE__, rev ) ;
} 
#endif

#ifdef TEST_NASDAc_phase_rsp2firstrev

/* get a Nasda phase   */
(void)strcpy(asftime, "1993:001:00:00:00.000" ) ;
printf("\n%s(%d):  calling asftime_2_phase J1 %s\n", 
	__FILE__, __LINE__, asftime ) ;
return_code = tc_asf2et(asftime, &etime ) ;
return_code = asftime_2_phase( "J1", asftime, &phase_rec) ;
nasda_decode_condition(return_code ) ;
printf("\n" ) ;
db_print_record(phase_rec, APS_CDEFS(PHASE) ) ;

for ( rsp = 0; rsp < 661 ; rsp++ ) 
{
	printf("\n%s(%d):  calling NASDAc_phase_rsp2firstrev rsp = %d\n",
			__FILE__, __LINE__, rsp ) ;
	return_code = NASDAc_phase_rsp2firstrev( phase_rec, rsp, &rev ) ;
	if ( return_code < 0 )
	{
		printf("%s(%d):  error message %d:  %s\n",
			__FILE__, __LINE__, return_code, PHASE_ERROR_MESSAGE(return_code)) ;
	}
	printf("%s(%d):  return_code = %d, rev = %d\n",
		__FILE__, __LINE__, return_code, rev ) ;
	if ( return_code == NASDA_PHASE_RSP2FIRSTREV_OK )
	{
		/* using NASDAc_phase_rev2rsp() to validate result  */
		return_code = NASDAc_phase_rev2rsp( phase_rec, rev, &rsp_checkup ) ;
		if ( return_code < 0 )
		{
			printf("%s(%d):  error message from NASDAc_phase_rev2rsp %d:  %s\n",
				__FILE__, __LINE__, return_code, 
				PHASE_ERROR_MESSAGE(return_code)) ;
		}
		else if ( rsp == rsp_checkup )
			printf("%s(%d):  RSP CHECKS OUT\n", __FILE__, __LINE__ ) ;
		else
			printf("%s(%d):  RSP SHOULD BE %d\n", __FILE__, __LINE__, 
			rsp_checkup) ;
	}
}
#endif

#ifdef TEST_NASDAc_rev_asftime_2_rsp_angle
for ( j = 0 ; sub_args_2[j].rev != 0 ; j++ )
{
	printf("\n%s(%d):  calling NASDAc_rev_asftime_2_rsp_angle %s \n   %s %d\n",
		__FILE__, __LINE__, sub_args_2[j].sat, 
		sub_args_2[j].asftime, sub_args_2[j].rev ) ;
	return_code = NASDAc_rev_asftime_2_rsp_angle( 
		sub_args_2[j].sat, sub_args_2[j].asftime, sub_args_2[j].rev,
		&rsp, &rsp_angle ) ;
	if ( return_code < 0 )
	{
		printf("%s(%d):  error message %d:  %s\n",
			__FILE__, __LINE__, return_code, PHASE_ERROR_MESSAGE(return_code)) ;
	}
	printf("%s(%d):  return_code = %d, rsp = %d, rsp_angle = %lf\n",
		__FILE__, __LINE__, return_code, rsp, rsp_angle ) ;

}
#endif

#ifdef TEST_phase_asftime2rev
for (j=0; test_asftimes2[j] != NULL ; j++ )
{
	printf("\n%s(%d):  calling phase_asftime2rev %s/%c %s\n", 
		__FILE__, __LINE__, 
		CAST_PHASE_SAT phase_rec[PHASE_SAT], 
		CAST_PHASE_PHASE_NAME phase_rec[PHASE_PHASE_NAME], 
		test_asftimes2[j] ) ;
	return_code = phase_asftime2rev( phase_rec, test_asftimes2[j], &rev ) ;
	if ( return_code < 0 )
	{
		printf("%s(%d):  error message %d:  %s\n",
			__FILE__, __LINE__, return_code, PHASE_ERROR_MESSAGE(return_code)) ;
	}
	printf("%s(%d):  return_code = %d, rev = %d\n",
		__FILE__, __LINE__, return_code, rev ) ;
}
#endif

#ifdef TEST_NASDAc_phase_rsp1
printf("--------------------------------------------------------------\n") ;

(void)strcpy(asftime, "1991:212:20:52:38.282") ;
return_code = tc_asf2et("1991:212:20:52:38.282", &etime ) ;

printf("%s(%d):  asftime = %s   etime = %.9lf\n", __FILE__, __LINE__, 
	asftime, etime ) ;

printf("%s(%d):  calling asftime_2_phase().  \n", __FILE__, __LINE__ ) ;

return_code = asftime_2_phase( "J1", asftime, &phase_rec) ;

printf("%s(%d):  return code:  %d:\n", __FILE__, __LINE__, return_code ) ;
db_print_record(phase_rec, APS_CDEFS(PHASE) ) ;


printf("%s(%d):  calling NASDAc_phase_rsp1() with phase rec.  \n", 
	__FILE__, __LINE__ ) ;
return_code = NASDAc_phase_rsp1(phase_rec, &rev, &rsp ) ;

printf("%s(%d):  return_code = %d, rev = %d, rsp = %d\n", 
	__FILE__, __LINE__, return_code, rev, rsp ) ;

#endif

#ifdef TEST_NASDAc_phase_rev2rsp
printf("--------------------------------------------------------------\n") ;
(void)strcpy(asftime, "1991:212:20:52:38.282" ) ;
printf("%s(%d):  calling tc_asf2et() asftime = %s\n", __FILE__, __LINE__, 
	asftime) ;

return_code = tc_asf2et(asftime, &etime ) ;

printf("%s(%d):  etime = %.9lf\n", __FILE__, __LINE__, etime ) ;

return_code = asftime_2_phase( "J1", asftime, &phase_rec) ;

if ( return_code < 0 )
{
	printf("%s(%d):  error message %d:  %s\n",
		__FILE__, __LINE__, return_code, PHASE_ERROR_MESSAGE(return_code)) ;
}
else
{
	printf("%s(%d):  return code:  %d:\n",
		__FILE__, __LINE__, return_code ) ;
	db_print_record(phase_rec, APS_CDEFS(PHASE) ) ;

	/* NOW start testing the routine NASDAc_phase_rev2rsp() */
	for (j = 0 ; nasda_revs[j] > 0 ; j++ )
	{
		printf("\nCalling NASDAc_phase_rev2rsp(): rev = %d\n", nasda_revs[j] ) ;
		return_code = NASDAc_phase_rev2rsp( phase_rec, nasda_revs[j], &rsp ) ;
		printf("%s(%d):  NASDAc_phase_rev2rsp() return code:  %d:",
				__FILE__, __LINE__, return_code ) ;
		printf("     RSP PATH = %d\n", rsp ) ;
		if ( return_code < 0 )
			printf("%s(%d):  error message %d:  %s\n", __FILE__, __LINE__, 
			return_code, PHASE_ERROR_MESSAGE(return_code) ) ;
	}

	for (rev = 463 ; rev < (463 + 662)  ; rev++ )
	{
		printf("\nCalling NASDAc_phase_rev2rsp(): rev = %d\n", rev ) ;
		return_code = NASDAc_phase_rev2rsp( phase_rec, rev, &rsp ) ;
		printf("%s(%d):  NASDAc_phase_rev2rsp() return code:  %d:",
				__FILE__, __LINE__, return_code ) ;
		printf("     RSP PATH = %d\n", rsp ) ;
		if ( return_code < 0 )
			printf("%s(%d):  error message %d:  %s\n", __FILE__, __LINE__, 
			return_code, PHASE_ERROR_MESSAGE(return_code) ) ;
	}

}
#endif    /* TEST_NASDAc_phase_rev2rsp  */

#ifdef TEST_NASDAc_asftime_rsp_angle_2_rev_time
for ( j = 0 ; sub_args[j].rsp != 0 ; j++ )
{
	printf(
	"\n--------------------------------------------------------------\n" ) ;
	printf("%s(%d): calling NASDAc_asftime_rsp_angle_2_rev_time.\n", 
		__FILE__, __LINE__ ) ;
	printf("%s, %s, %d, %lf, %d\n", 
		sub_args[j].sat, sub_args[j].asftime, sub_args[j].rsp, 
		sub_args[j].rsp_angle, sub_args[j].search_flag ) ;
	return_code = NASDAc_asftime_rsp_angle_2_rev_time(
		sub_args[j].sat, sub_args[j].asftime, sub_args[j].rsp, 
		sub_args[j].rsp_angle, sub_args[j].search_flag,
		&rev, asftime_out ) ;
	printf("%s(%d): returned from NASDAc_asftime_rsp_angle_2_rev_time: %d\n",
		__FILE__, __LINE__, return_code ) ;
	if ( return_code < 0 )
		printf("%s \n", PHASE_ERROR_MESSAGE(return_code) ) ;
	printf("%d, %s\n", rev, asftime_out ) ;
	if ( return_code == NASDA_ASFTIME_RSP_ANGLE_2_REV_TIME_OK )
	{
		return_code = NASDAc_rev_asftime_2_rsp_angle(
			sub_args[j].sat, asftime_out, rev, &rsp, &rsp_angle ) ;
		printf("%s(%d): CHECKOUT:  rsp = %d, angle = %f\n",
			__FILE__, __LINE__, rsp, rsp_angle ) ;
	}
}
#endif

#ifdef TEST_asftime_2_phase
for (j=0; test_asftimes[j] != NULL ; j++ )
{
	printf("--------------------------------------------------------------\n") ;
	printf("%s(%d):  test_asftimes[%d] = %s\n", __FILE__, __LINE__, 
		j, test_asftimes[j] ) ;

	return_code = tc_asf2et(test_asftimes[j], &etime ) ;

	printf("%s(%d):  calling asftime_2_phase E1 %.9lf\n",
		__FILE__, __LINE__, etime ) ;

	return_code = asftime_2_phase( "E1", test_asftimes[j], &phase_rec) ;

	if ( return_code < 0 )
	{
		printf("%s(%d):  error message %d:  %s\n",
			__FILE__, __LINE__, return_code, PHASE_ERROR_MESSAGE(return_code)) ;
	}
	else
	{
		printf("%s(%d):  return code:  %d:  ",
			__FILE__, __LINE__, return_code ) ;
		nasda_decode_condition( return_code ) ;
		printf("\n") ;
		db_print_record(phase_rec, APS_CDEFS(PHASE) ) ;
	}

}
#endif /*   TEST_asftime_2_phase    */

printf("%s(%d):  END OF TESTS\n", __FILE__, __LINE__ ) ;
return ;
}


/* TO DECODE CONDITION CODE FOR asftime_2_phase()  */
static void nasda_decode_condition(int code)
{
	switch( code )
	{
		case PHASE_INPUT_TIME_BEFORE_ALL_PHASES :
			printf("PHASE_INPUT_TIME_BEFORE_ALL_PHASES" ) ;
			break ;
		case PHASE_INPUT_TIME_WITHIN_A_PHASE    :
			printf("PHASE_INPUT_TIME_WITHIN_A_PHASE" ) ;
			break ;
		case PHASE_INPUT_TIME_AFTER_ALL_PHASES  :
			printf("PHASE_INPUT_TIME_AFTER_ALL_PHASES" ) ;
			break ;
		case PHASE_INPUT_TIME_BETWEEN_PHASES    :
			printf("PHASE_INPUT_TIME_BETWEEN_PHASES" ) ;
			break ;
		default:
			printf("unknown condition code." ) ;
			break ;
	}
}

/* TO DECODE CONDITION CODE FOR rev_2_phase()  */
static void nasda_decode_condition_2(int code)
{
	switch( code )
	{
		case PHASE_INPUT_REV_BEFORE_ALL_PHASES :
			printf("PHASE_INPUT_REV_BEFORE_ALL_PHASES" ) ;
			break ;
		case PHASE_INPUT_REV_WITHIN_A_PHASE    :
			printf("PHASE_INPUT_REV_WITHIN_A_PHASE" ) ;
			break ;
		case PHASE_INPUT_REV_AFTER_ALL_PHASES  :
			printf("PHASE_INPUT_REV_AFTER_ALL_PHASES" ) ;
			break ;
		case PHASE_INPUT_REV_BETWEEN_PHASES    :
			printf("PHASE_INPUT_REV_BETWEEN_PHASES" ) ;
			break ;
		default:
			printf("unknown condition code." ) ;
			break ;
	}
}

