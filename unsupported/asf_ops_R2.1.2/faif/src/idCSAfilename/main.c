/*==============================================================================
Filename:	main.c

Description:
	Main module that can be used to as a driver for the
identification of a FA (flight agency) file via its filename.
This program is called to check a CSA file only.

External Functions:
	None

Static Functions:
	usage
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
#include "faifdefs.h"

#ifdef __STDC__
static void usage(char *) ;
#else
static void usage() ;
#endif


extern int check_FA_filename() ;
static int opt;                      /* Option letter */
static char *optlist = "f:";         /* Valid options */




/*==============================================================================
Function:	static void usage(progname)

Description:	
	Prints the usage banner for the idfile driver program.

Parameters:
	progname - the name of the idfile executable

Returns:	
	None

Creator:	Norbert Piega	
Creation Date:	06/20/1994
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
   printf(" Usage: %s [-f] input_file_name\n", progname) ;
   printf("\n") ;
   exit(1) ;
} /* usage */





/*==============================================================================
Function:	main

Description:	
	This is the driver for the file identification via filename
program.  Note that this program only checks for a CSA filename.

Parameters:
	argc - number of command line arguments
	argv - array of command line argument strings

Returns:	exit status is the file type code matched
Creator:	Norbert Piega	
Creation Date:	06/20/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
main(int argc, char *argv[])
#else
int
main(argc, argv)
   int argc ;
   char *argv[] ;
#endif
{
   extern int optind ;
   extern char *optarg ;
   char *input_filename = NULL ;
   int status ;
   int fa_code = CSA ;

   if (argc < 2)
      usage(argv[0]) ;

   /* Command line option processing
   */
   while ((opt = getopt(argc, argv, optlist)) != EOF)
      switch (opt) {
         case 'f' :
            input_filename = (char *) strdup(optarg) ;
            break ;
         default :
            usage(argv[0]) ;
      } /* endswitch */
 
   argv = argv + optind;
   argc = argc - optind;

   if (*argv != (char *)NULL)
      if (input_filename == (char *)NULL)
         input_filename = (char *) strdup(*argv) ;
      else
         fprintf(stderr,
            "Input file name %s specified.  %s ignored.\n",
            input_filename, *argv) ;

   if (input_filename != (char *) NULL)
      status = check_FA_filename(input_filename, fa_code) ;

   exit(status) ;

} /* main */


/* End of file */
