#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

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
#pragma ident	"@(#) /home/aps/r2.1.2/src/mapr/SCCS/s.main.c"

/*
C-----------------------------------------------------------------------
C
C SUBROUTINE MAIN
C
C PURPOSE
C
C WRITTEN BY NADIA ADHAMI
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C
C 09/08/95  Nadia Adhami PR#406 invalid start time submitted for query
C           Nadia Adhami Added default start time DEF_START_TIME
C           Nadia Adhami set DEF_START_TIME to 1990:001:00:00:00.000
C-----------------------------------------------------------------------
*/

#include <stdlib.h>
#include <stdio.h>
 
char	printer_name[80];

main (int argc, char *argv[])
{
	char 	dbname[100];
	char 	userid[100];
	char 	password[100];
	char	*laser_printer;
	int	error_code;
	int	testmode = 0;

	extern int  init_vec_lib();      /* initializes vector lib with stoicfile */
	extern int  init_vec_lib_exit(int); /* prints msg, exits if stoic error */

	/* symbols defined in fortran: use '-' suffix when used in C */

	extern	char	window_name_[80];
	extern	char	def_start_time_[80];
	extern	int	def_height_;
	extern	float	minlat_prev_,minlon_prev_,maxlat_prev_,maxlon_prev_;
	extern	int	text_font_;

	extern 	void mapper_(char*, char*, char*, int*);
	extern	int mapr_getdup(int, char *argv[], char*, char*, char*, char*, int*);

	/* INITIALIZATION */

	/* set stdout to unbuffered I/O */
	setbuf( stdout, (char *) NULL ) ;

	/* initialize default character height */
	def_height_ = 0.007;

	/* initialize default character height */
	text_font_ = -2604;

	/* check env var MAPPER_PRINTER */
	/*initialize default printer name if env var MAPPER_PRINTER is not defined*/
	laser_printer = getenv("MAPPER_PRINTER");
	if (laser_printer)
		strncpy(printer_name , laser_printer , 79);
	else
		strncpy(printer_name , "apscl" , 79);

	/* set default window name */
	strncpy(window_name_ , "APS MAPPER", 79);

	/* set default start time */
	strcpy(def_start_time_ , "1990:001:00:00:00.000");

	minlat_prev_ = minlon_prev_ = maxlat_prev_ = maxlon_prev_ = 0;

	error_code =
		mapr_getdup(argc, argv, "st:", dbname, userid, password, &testmode);

	if (error_code != 0)
	{
		switch(error_code)
		{
		case 1:

			printf(
			"\nUsage : %s [-U <userid>] [-L <laserprinter>] -P <password>\n\n",
			argv[0] ); 
			printf(
				"\t\t%s  Version %s %s \n\n", argv[0], __DATE__, __TIME__ ) ;
			break;
		case 2:
			printf("\nAPS_SYBASE_USERID not defined\n\n"); break;
		case 3:
			printf("\nAPSDB not defined\n\n"); break;
		default:
			break;
		}
		exit(1);
	}

	#ifdef DEBUG
		printf("main.c:  dbname = %s, userid = %s, password = %s\n\n",
			dbname, userid, password);
	#endif

	printf("\n Laser Printer set to <%s>\n\n",printer_name);

	if (  (error_code = init_vec_lib())  )
		init_vec_lib_exit( error_code );

	mapper_(dbname, userid, password, &testmode);

}
