/*************************************************************************
 * Copyright (c)1995 California Institute of Technology. U.S. Government *
 * Sponsorship acknowledged.                                             *
 *************************************************************************/

/*==============================================================================
Filename:	client_main.c

Description:
	This module is a sample client program which utilizes the FAxmitserver 
server.  Note that the only call needed is the call to send_file_from_ASF.  
Below is the synopsis for the send_file_from_ASF function:

	int send_file_from_ASF(file_to_send, FA, file_type)

	char *file_to_send is the filename of the outbound file
	char *FA may be "ESA", "NASDA", "CSA", "WALPS", or "ADEOS"
	char *file_type may be NULL or :
	   ESA_RQUS, ESA_REAQ, ESA_RESM, ESA_REEX, ESA_REUG,
	   NASDA_REQQ, NASDA_REQW, NASDA_REAC, NASDA_CATA,
	   NASDA_MSGM, NASDA_MSGF, NASDA_MSGE,
	   CSA_ARCHSTRGRPT, CSA_RECRPT, CSA_RECAVAILRPT,
	   CSA_CALIBRPT, CSA_CALIBAVAILRPT,
	   WALPS_AVAIL_REQ, WALPS_STVEC, WALPS_WOS,
	   ADEOS_STGS, ADEOS_REAC, or ADEOS_SRRD.

External Functions:
	None
	
Static Functions:
	usage
        filetype
	main
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
1.  R. Hoffman - Dec. '97
    Uncomment the final printf so we see that line in APS log files.
==============================================================================*/

static char SccsFile[] = "client_main.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "17 Sep 1996";
static char SccsLastChanger[] = "@(#)client_main.c	1.1";
static char SccsState[] = "1.1";


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include "faifdefs.h"
#include "client_send_data.h"
#include <unistd.h>
#include <dce/nbase.h>
#include <dce/rpc.h>
#include "In_Pipe_State.h"
#include "sendtoFA.h"
#include "check_status.h"

#ifdef __STDC__
static void usage(char *) ;
#else
static void usage() ;
#endif

#ifdef __STDC__
static void check_filetype(char *);
#else
static void check_filetype();
#endif

/* User level server function (send) */
extern int send_file_from_ASF() ;

/* Table of FA names to be passed to the send function */
extern Names_Table_Entry FA_Names_Table[] ;

/* Tables of FA file type strings
-- These tables contain all the file id strings
-- available.  Note that only the outbound file
-- types in these tables may be sent out.
*/
extern File_Identifier ESA_FileTypeStr_Table[] ;
extern File_Identifier CSA_FileTypeStr_Table[] ;
extern File_Identifier NASDA_FileTypeStr_Table[] ;
extern File_Identifier WALPS_FileTypeStr_Table[] ;
extern File_Identifier ADEOS_FileTypeStr_Table[] ;
 


/*==============================================================================
Function:	static void usage(char *progname)

Description:	
	This is the program usage banner that gets executed when
the client program is executed with missing or invalid options.
This banner would like below:

	Usage:
		sendclient -f outbound_file -d destinationFA -t ftype

	destinationFA may be:
	  ESA, NASDA, CSA, Wallops
	ftype may be:
	   ESA_RQUS, ESA_REAQ, ESA_RESM, ESA_REEX, ESA_REUG,
	   NASDA_REQQ, NASDA_REQW, NASDA_REAC, NASDA_CATA,
	   NASDA_MSGM, NASDA_MSGF, NASDA_MSGE,
	   CSA_ARCHSTRGRPT, CSA_RECRPT, CSA_RECAVAILRPT,
	   CSA_CALIBRPT, CSA_CALIBAVAILRPT,
	   WALPS_AVAIL_REQ, WALPS_STVEC, WALPS_WOS,
	   ADEOS_STGS, ADEOS_REAC, or ADEOS_SRRD.

Parameters:
	progname - the name of the executable program

Returns:	None	
Creator:	Norbert Piega	
Creation Date:	06/13/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
static void usage(char *progname)
#else
static void usage(progname)
   char *progname ;
#endif
{
   int i=0;
   printf("\n Usage:\n") ;
   printf("    %s -f outbound_file -d destinationFA -t filetype\n\n", 
      progname) ;
   printf(" destinationFA may be: \n") ;
   printf("   ESA, NASDA, CSA, WALPS or ADEOS\n") ;
   printf(" filetype may be: \n") ;
   for (i=0; CSA_FileTypeStr_Table[i].file_id_number != SENTINEL; i++)
      printf("%s\n", CSA_FileTypeStr_Table[i].file_identifier);
   printf("\n");
   for (i=0; ESA_FileTypeStr_Table[i].file_id_number != SENTINEL; i++)
      printf("%s\n", ESA_FileTypeStr_Table[i].file_identifier);
   printf("\n");
   for (i=0; NASDA_FileTypeStr_Table[i].file_id_number != SENTINEL; i++)
      printf("%s\n", NASDA_FileTypeStr_Table[i].file_identifier);
   printf("\n");
   for (i=0; WALPS_FileTypeStr_Table[i].file_id_number != SENTINEL; i++)
      printf("%s\n", WALPS_FileTypeStr_Table[i].file_identifier);
   printf("\n");
   for (i=0; ADEOS_FileTypeStr_Table[i].file_id_number != SENTINEL; i++)
      printf("%s\n", ADEOS_FileTypeStr_Table[i].file_identifier);
   printf("\n");
} /* usage */




/*==============================================================================
Function:	static void check_filetype(char *filetype)

Description:
        This program makes sure the filetype sent into the mail program is correct
and is accessible by the program. It compares it with the list of possible
filetypes.

Parameters:
	filetype - the type of file being passed into program

Returns:	None	
Creator:	Samah Sohrab
Creation Date:	10/13/96
Notes:		
==============================================================================*/
#ifdef __STDC__
static void check_filetype (char *filetype)
#else
static void check_filetype (filetype)
   char *filetype;
#endif
{
   LOGICAL filetype_ok = FALSE ;    /* if -t option value ok */
   int i;

   for (i=0; ESA_FileTypeStr_Table[i].file_id_number != SENTINEL; i++)
      if (strcmp(filetype, ESA_FileTypeStr_Table[i].file_identifier) == 0)
      {
         filetype_ok = TRUE ;
         break ;
      }
   for (i=0; NASDA_FileTypeStr_Table[i].file_id_number != SENTINEL; i++)
      if (strcmp(filetype, NASDA_FileTypeStr_Table[i].file_identifier) == 0)
      {
         filetype_ok = TRUE ;
         break ;
      }
   for (i=0; CSA_FileTypeStr_Table[i].file_id_number != SENTINEL; i++)
      if (strcmp(filetype, CSA_FileTypeStr_Table[i].file_identifier) == 0)
      {
         filetype_ok = TRUE ;
         break ;
      }
   for (i=0; WALPS_FileTypeStr_Table[i].file_id_number != SENTINEL; i++)
      if (strcmp(filetype, WALPS_FileTypeStr_Table[i].file_identifier) == 0)
      {
         filetype_ok = TRUE ;
         break ;
      }
   for (i=0; ADEOS_FileTypeStr_Table[i].file_id_number != SENTINEL; i++)
      if (strcmp(filetype, ADEOS_FileTypeStr_Table[i].file_identifier) == 0)
      {
         filetype_ok = TRUE ;
         break ;
      }
   if (filetype_ok == FALSE)
   {
      syslog(LOG_WARNING, 
         "ERROR, Invalid file type %s specified in -t option. Exiting.\n", 
          filetype) ;
      exit(1) ;
   }

}


/*==============================================================================
Function:	main(argc, argv) 

Description:
	ASF to FA send file client.  This represents the component
associated with the client side of the send file to FA operation.  Note
that the user level call is through send_file_from_ASF where only the
client filename, the destination flight agency and file type are specified.
The rest of the work is done on lower levels and is accomplished via a DCE
RPC utilizing a DCE pipe.

Parameters:
	argc - number of command line arguments
	argv - array of command line argument strings

Returns:	exit status 0 (success) or 1 (error)
Creator:	Samah Sohrab
Creation Date:	11/13/1996
Notes:		
==============================================================================*/
#ifdef __STDC__
int
main(int argc, char *argv[])
#else
int
main(argc, argv)
   int argc ;
   char **argv ;
#endif
{
   extern char *optarg ;
   int opt;                                 /* Option letter */
   char *optlist = "f:d:t:";                /* Valid options */
   char *infile = NULL ;             /* Name of file to send */
   char *destFA = NULL ;            /* Destination FA string */
   char *filetype = NULL ;           /* Type of file to send */
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;               /* Error log msg string */
   LOGICAL dest_id_ok = FALSE ;     /* if -d option value ok */
   int i, status ;
 
   /* Open system error log
   */
   openlog(argv[0], LOG_PID | LOG_CONS | LOG_NDELAY, SYSLOG_FAIF) ;
   setlogmask(LOG_UPTO(LOG_DEBUG)) ;

   syslog(LOG_INFO, "Starting.");

   /* If invalid invocation
   */
   if (argc < 7) 
   {
      syslog(LOG_ERR, "ERROR in number of arguments.\n") ;
      usage(argv[0]) ;
      exit(1) ;
   }

   /* Parse command line options
   */
   while ((opt = getopt(argc, argv, optlist)) != EOF)
      switch (opt)
      {
         case 'f' :
            infile = (char *) strdup(optarg) ;
            break ;
         case 'd' :
            destFA = (char *) strdup(optarg) ;
            break ;
         case 't' :
            filetype = (char *) strdup(optarg) ;
            break ;
         default :
            usage(argv[0]) ;
            exit(1) ;
      } /* endswitch */

   syslog(LOG_INFO, "Parameters:%s %s %s", infile, destFA, filetype);

   /* If invalid destination specified
   */
   if (getenv(FAXMIT_CDS_ENTRY_EV) == (char *)NULL)
   {
      syslog(LOG_ERR, "ERROR, environment variable %s must be set\n",
	 FAXMIT_CDS_ENTRY_EV) ;
      exit(1) ;
   }

   /* If invalid input file specified
   */
   if (infile == (char *)NULL) 
   {
      syslog(LOG_ERR, 
	 "ERROR, missing or invalid input file\n") ;
      exit(1) ;
   }

   /* If invalid destination specified
   */
   if (destFA == (char *)NULL) 
   {
      syslog(LOG_ERR, 
	 "ERROR, missing or invalid destination id\n") ;
      exit(1) ;
   }

   /* Check destination FA string argument
   */
   for (i=0; FA_Names_Table[i].name_id != SENTINEL; i++)
      if (strcmp(destFA, FA_Names_Table[i].name_identifier) == 0)
      {
	dest_id_ok = TRUE ;
	break ;
      }
   if (dest_id_ok == FALSE)
   {
      syslog(LOG_ERR, 
         "ERROR, Invalid destination flight agency  %s in -d option. Exiting.\n", destFA) ;
      exit(1) ;
   }

   /* Check file type argument 
   */
   if (filetype != (char *)NULL)
   {
      check_filetype(filetype);
   }
   else
   {
      syslog(LOG_ERR, "ERROR, Filetype not specified in -t option. Exiting.\n");
      exit(1);
   }

   status = send_file_from_ASF(infile, destFA, filetype) ;

   if (status == ERROR) 
   {
      syslog(LOG_ERR, "ERROR, Unable to send %s\n", infile) ;
      exit(1) ; 
   }    
   else
   {
      if (strcmp(destFA, ADEOS_STR) == 0)
      {
         syslog(LOG_NOTICE, "File %s copied in FAIF area.  Notification sent to ADEOS\n", infile) ;
      }
      else
      {
         syslog(LOG_NOTICE, "Sent file %s to %s\n", infile, destFA) ;
         printf("Sent file %s to %s\n", infile, destFA) ;
      }
   }

   syslog(LOG_NOTICE, "Done.");
   exit(0) ;

} /* main */


/* End of file */




