/*==============================================================================
Filename:	main.c

Description:	
	Main module for PredOrRes

External Functions:
	None
	
Static Functions:
	main
	
External Variables Defined:
	None

File Scope Static Variables:
	opt
	optlist

Notes:
==============================================================================*/

static char SccsFile[] = "%M%" ;
static char SccsRevision[] = "%R%" ;
static char SccsDate[] = "%G%";
static char SccsLastChanger[] = "%W%";
static char SccsState[] = "%I%";


#include <stdio.h>
#include <string.h>
#include "faifdefs.h"   /* Primary header file */

#ifdef __STDC__
static void usage(char *) ;
void main(int, char **) ;
#else
static void usage() ;
void main() ;
#endif


extern int get_CSAstvec_precision() ;



/*==============================================================================
Function:	usage void usage(char *progname)

Description:
	This function prints the program usage banner for the CSA state
vector translation program.

Parameters:	progname
Returns:	None	
Creator:	Norbert Piega	
Creation Date:	03/03/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
static void 
usage(char *progname)
#else
static void 
usage(progname)
   char *progname ;
#endif
{
   printf("\n") ;
   printf(" Usage:\n") ;
   printf("    %s -f inputCSAstvecfile\n", progname) ;
   printf("\n") ;
} /* usage */





/*==============================================================================
Function:	void main(int argc, char *argv[])

Description:	
	main() for xlatCSAsv (CSA state vector translator)

	This is the main() function for the CSA state vector
translation program.  It obtains the execution arguments as specified
from the command line options and calls the translation function
translate_CSA_stvec.

	Defaults to usage banner on bad command line.  Prints error
message if translate_CSA_stvec returns ERROR.

Parameters:
	argc - number of command line arguments
	argv - array of command line argument strings

Returns:	None
Creator:	Norbert Piega	
Creation Date:	03/03/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
void 
main(int argc, char **argv)
#else
void 
main(argc, argv)
   int argc ;
   char *argv[] ;
#endif
{
   extern char *optarg ;
   int opt;              /* Option letter */
   char *optlist = "f:"; /* Valid options */

   int status ; 
   char *input_filename = NULL ;

   /* Open the system error log
   */
   openlog(argv[0], LOG_PID|LOG_CONS|LOG_NDELAY, LOG_USER) ;
   setlogmask(LOG_UPTO(LOG_DEBUG)) ;
	     
   if (argc < 2)
   {
      usage(argv[0]) ;
      exit(-1) ;
   }

   while ((opt = getopt(argc, argv, optlist)) != EOF)
      switch (opt)
      {
         case 'f' :
            input_filename = (char *) strdup(optarg) ;
            break ;
         default :
            usage(argv[0]) ;
            exit(-1) ;
      } /* endswitch */
   
   if (*++argv != (char *)NULL)
   {
      if (input_filename == (char *)NULL)
         input_filename = (char *) strdup(*argv) ;
   }
   else
   {
      fprintf(stderr, "Error in specification of state vector files.\n") ;
      exit(-1) ;
   }

   if ((status = get_CSAstvec_precision(input_filename)) == ERROR)
   {
      fprintf(stderr,
	 "Error determining CSA state vector file precision for %s.\n",
	 input_filename) ;
      exit(-1) ;
   }

#ifdef VERBOSE
   printf("Precision status = %d (1 definitive, 2 predicted)\n", status+1) ;
#endif   

   exit(status+1) ;
 
} /* main */

/* End of file */
