/*==============================================================================
Filename:	FAIF_encypt.c

Description:	
	This is the utility program used for encrypting username and password
information used by the FAIF for transferring files to remote flight agency 
hosts.  The encryption libraries used are from the Solaris DES Encryption kit.

External Functions:
	None
	
Static Functions:
	usage
	main
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:

==============================================================================*/

static char SccsFile[] = "main.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "22 Jan 1996";
static char SccsLastChanger[] = "@(#)main.c	1.1";
static char SccsState[] = "1.1";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <des_crypt.h>
#include "faifdefs.h"
#include "CSAget_upw.h"
#include "ADEOSroute_upw.h"
#include "server_upw.h"
#include "getupass.h"

#ifdef __STDC__
static void usage(char *progname) ;
#else
static void usage() ;
#endif

#define ESA_IF          "ESA"
#define ESA_GET_IF      "ESAget"
#define NASDA_IF        "NASDA"
#define NASDA_GET_IF    "NASDAget"
#define WALPS_IF        "WALPS"
#define ADEOS_IF        "ADEOS"
#define CSA_UFDRFF_IF   "CSA_ufdrf_F"
#define CSA_UFCALF_IF   "CSA_ufcal_F"
#define CSA_UFDROCF_IF  "CSA_ufdroc_F"
#define CSA_UFDRFM_IF   "CSA_ufdrf_M"
#define CSA_UFCALM_IF   "CSA_ufcal_M"
#define CSA_UFDROCM_IF  "CSA_ufdroc_M"


/*==============================================================================
Function:	usage(char *progname)

Description:	
	Prints usage banner for FAIF_encrypt application program.

Parameters:
	char *progname - name of application program

Returns:	None
Creator:	Norbert Piega	
Creation Date:	11/29/1994
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
   printf("\nUsage:\n  %s -i interface\n\n", progname) ;
   printf("\twhere interface is the interface for which the password\n") ;
   printf("\tinformation is intended.  Below are valid values.  Note that\n") ;
   printf("\tthe username and the password will be prompted.\n\n") ;

   printf("\t  %s - FAIF to ESA  (ESA username and password)\n", ESA_IF) ;
   printf("\t  %s - ESA  to FAIF (ESA username and password)\n", ESA_GET_IF) ;
   printf("\t  %s - FAIF  to NASDA (NASDA username and password)\n", NASDA_IF) ;
   printf("\t  %s - NASDA to FAIF  (NASDA username and password)\n", NASDA_GET_IF) ;
   printf("\t  %s - FAIF to Wallops (Wallops username and password)\n", WALPS_IF) ;
   printf("\t  %s - ADEOS to FAIF (ADEOS username and password)\n\n", ADEOS_IF) ;
   printf("\t  %s - FAIF/CSA UFDRF for Fairbanks inbound and outbound\n", CSA_UFDRFF_IF) ;
   printf("\t  %s - FAIF/CSA UFCAL for Fairbanks inbound and outbound\n", CSA_UFCALF_IF) ;
   printf("\t  %s - FAIF/CSA UFDROC for Fairbanks inbound and outbound\n", CSA_UFDROCF_IF) ;
   printf("\t  %s - FAIF/CSA UFDRF for McMurdo inbound and outbound\n", CSA_UFDRFM_IF) ;
   printf("\t  %s - FAIF/CSA UFCAL for McMurdo inbound and outbound\n", CSA_UFCALM_IF) ;
   printf("\t  %s - FAIF/CSA UFDROC for McMurdo inbound and outbound\n\n", CSA_UFDROCM_IF) ;
   exit(1) ;

} /* usage */





/*==============================================================================
Function:	main()

Description:	
	This is the driver for the FAIF_enscrypt application program.
It creates password files for specific interfaces handled by the FAIF.

Parameters:	argc, argv
Returns:	exit status 1 error, 0 ok.	
Creator:	Norbert Piega	
Creation Date:	11/29/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
main(int argc, char **argv)
#else
main(argc,argv)
   int argc;
   char *argv[];
#endif
{
   extern char *optarg ;
   static int   opt;                  /* Option letter */
   static char *optlist = "i:" ;      /* Valid options */
   char *interface_id ;
   int   estatus ;
   char  key[KEYSIZE] ;
   char  iv[IVSIZE] ;
   char  buf[BUFSIZE] ;
   char  tmpbuf[BUFSIZE] ;
   char *outfile = NULL ;
   char *bptr = NULL ;
   FILE *outfp = NULL ;
   FILE *copyfp = NULL ;
   char *FAIF_rootpath = NULL ;
   char *fname_complete = NULL ;
   char *copy_complete = NULL ;

   if (argc < 2)
      usage(argv[0]) ;

   while ((opt = getopt(argc, argv, optlist)) != EOF)
      switch(opt)
      {
         case 'i':
            interface_id = (char *)strdup(optarg) ;
            break ;
	    
         default:
	    usage(argv[0]) ;
      }

   if ((FAIF_rootpath = getenv(FAIF_ROOTPATH_EV)) == (char *)NULL)
   {
      fprintf(stderr, 
	 "Error, Environment variable FAIF_ROOTPATH not set.  Using\
	 \ndefault %s\n", FAIF_ROOTPATH_DEF) ;
      FAIF_rootpath = FAIF_ROOTPATH_DEF ;
   }
   if (access(FAIF_rootpath, W_OK) != 0)
   {
      fprintf(stderr,
	 "Error, Unable to access FAIF root path %s.  Exiting\n",
	 FAIF_rootpath) ;
      exit(1) ;
   }

   /* Initialize buffers
   */
   memset(key, 0,(size_t)KEYSIZE) ;
   memset(iv, 0, (size_t)IVSIZE) ;
   memset(buf, 0,(size_t)BUFSIZE) ;
   strcpy(key, FAIF_ENC_KEY) ;

   /* Find out for which interface, the password file is being created
   */
   if (strcmp(interface_id, CSA_UFDRFF_IF) == 0)
   {
	 outfile = CSAUFDRFF_UPW_FILE ;
	 printf("Please enter your CSA host username and password\n") ;
   }
   else if (strcmp(interface_id, CSA_UFCALF_IF) == 0)
   {
	 outfile = CSAUFCALF_UPW_FILE ;
	 printf("Please enter your CSA host username and password\n") ;
   }
   else if (strcmp(interface_id, CSA_UFDROCF_IF) == 0)
   {
	 outfile = CSAUFDROCF_UPW_FILE ;
	 printf("Please enter your CSA host username and password\n") ;
   }
   else if (strcmp(interface_id, CSA_UFDRFM_IF) == 0)
   {
	 outfile = CSAUFDRFM_UPW_FILE ;
	 printf("Please enter your CSA host username and password\n") ;
   }
   else if (strcmp(interface_id, CSA_UFCALM_IF) == 0)
   {
	 outfile = CSAUFCALM_UPW_FILE ;
	 printf("Please enter your CSA host username and password\n") ;
   }
   else if (strcmp(interface_id, CSA_UFDROCM_IF) == 0)
   {
	 outfile = CSAUFDROCM_UPW_FILE ;
	 printf("Please enter your CSA host username and password\n") ;
   }
   else if (strcmp(interface_id, WALPS_IF) == 0)
   {
	 outfile = WALPSSEND_UPW_FILE ;
	 printf("Please enter your Wallops host username and password\n") ;
   }
   else if (strcmp(interface_id, ADEOS_IF) == 0)
   {
	 outfile = ADEOSROUTE_UPW_FILE ;
	 printf("Please enter your ADEOS host username and password\n") ;
   }
   else if (strcmp(interface_id, ESA_IF) == 0)
   {
	 outfile = ESASEND_UPW_FILE ;
	 printf("Please enter your ESA host username and password\n") ;
   }
   else if (strcmp(interface_id, ESA_GET_IF) == 0)
   {
	 outfile = ESAGET_UPW_FILE ;
	 printf("Please enter your ESA host username and password\n") ;
   }
   else if (strcmp(interface_id, NASDA_IF) == 0)
   {
	 outfile = NASDASEND_UPW_FILE ;
	 printf("Please enter your NASDA host username and password\n") ;
   }
   else if (strcmp(interface_id, NASDA_GET_IF) == 0)
   {
	 outfile = NASDAGET_UPW_FILE ;
	 printf("Please enter your NASDA host username and password\n") ;
   }
   else
   {
         fprintf(stderr,
	   "Error, Invalid interface id specified in -i option\n") ;
	 exit(1) ;
   }


   /* Prompt for username 
   */
   bptr = (char *)getpass("Please enter username (100 chars max): ") ;
   if (bptr == (char *)NULL)
   {
      fprintf(stderr, "Username entry error\n");
      exit(1);
   }
   memcpy(buf, bptr, strlen(bptr)) ;
   bptr = (char *)getpass("Enter username again (100 chars max): ") ;
   if (strcmp(bptr, buf) != 0)
   {
      fprintf(stderr, "Username entry error\n");
      exit(1);
   }
   strcat(buf, ",") ;

   /* Prompt for password
   */
   bptr = (char *)getpass("Please enter password (100 chars max): ") ;
   if (bptr == (char *)NULL)
   {
      fprintf(stderr, "Password entry error\n");
      exit(1);
   }
   memcpy(tmpbuf, bptr, strlen(bptr)) ;
   bptr = (char *)getpass("Enter password again (100 chars max): ") ;
   if (strcmp(bptr, tmpbuf) != 0)
   {
      fprintf(stderr, "Password entry error\n");
      exit(1);
   }
   strncat(buf, bptr, strlen(bptr)) ;

   fname_complete = 
      (char *)util_do_malloc(sizeof(char)*(strlen(FAIF_rootpath)+1 +
	 strlen(FAIF_FTPUPW_DIR)+1 + strlen(outfile)+1)) ;
   strcpy(fname_complete, FAIF_rootpath) ;
   strcat(fname_complete, "/") ;
   strcat(fname_complete, FAIF_FTPUPW_DIR) ;
   strcat(fname_complete, "/") ;
   strcat(fname_complete, outfile) ;

   if (access(fname_complete, F_OK) == 0)
   {
      fprintf(stderr,
	 "Error, password file already exists.  Clean-up FTP directories \
	 \nand then retry.  Exiting\n") ;
      exit(1) ;
   }

   if ((outfp = fopen(fname_complete,"w")) == (FILE *)NULL)
   {
      fprintf(stderr, 
	 "Error, Unable to open password file for writing.  Exiting\n") ;
      exit(1) ;
   }

   /* Do data encryption
   */
   des_setparity(key) ;
   estatus = cbc_crypt(key, buf, BUFSIZE, DES_ENCRYPT | DES_SW, iv);
   switch(estatus)
   {
      case DESERR_NONE:
         fprintf(stderr, "Done\n") ;
         break ;
      case DESERR_NOHWDEVICE:
         fprintf(stderr, "Sofware Encryption ok\n") ;
         break ;
      case DESERR_BADPARAM:
         fprintf(stderr, "Bad Encryption parameters\n") ;
         break ;
      default:
         fprintf(stderr, "Unexpected encryption status\n") ;
         break ;
   }

   /* Write encrypted data
   */
   fwrite(buf, sizeof(char), BUFSIZE, outfp);
   fclose(outfp) ;
   if (chmod(fname_complete, S_IRUSR) == -1)
   {
      fprintf(stderr,
	 "Error setting password file permissions.  Exiting\n") ;
      remove(fname_complete) ;
      free(fname_complete) ;
      exit(1) ;
   }
   free(fname_complete) ;

   if (copyfp != (FILE *)NULL)
   {
      fwrite(buf, sizeof(char), BUFSIZE, copyfp);
      fclose(copyfp) ;
      if (chmod(copy_complete, S_IRUSR) == -1)
      {
         fprintf(stderr,
	    "Error setting password file permissions.  Exiting\n") ;
         remove(fname_complete) ;
         free(copy_complete) ;
         exit(1) ;
      }
      free(copy_complete) ;
   }

   exit(0);

} /* main */


/* End of File */
