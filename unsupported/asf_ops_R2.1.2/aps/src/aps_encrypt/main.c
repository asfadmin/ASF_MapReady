#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		APS_encrypt.c

Description:	This is the utility program used for encrypting username
				and password information used by the APS for accessing other
				subsystems.  
	
				Examples are the IMS_DB.  
				
				Used for accessing the IMS db.  

				The encryption libraries used are from the Solaris DES
				Encryption kit.

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
#pragma ident	"@(#)main.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/aps_encrypt/SCCS/s.main.c"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <des_crypt.h>
#include "aps_encrypt.h"
#include "apspath.h"
#include "aps_defs.h"

#ifdef __STDC__
static void usage(char *progname) ;
#else
static void usage() ;
#endif

#define IMS_DB_IF "IMS_DB"


/*==============================================================================
Function:	usage(char *progname)

Description:	
	Prints usage banner for APS_encrypt application program.

Parameters:
	char *progname - name of application program

Returns:	None
Creator:	Norbert Piega	
Creation Date:	12/09/1994
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
    printf("\nUsage:\n  %s -i interface\n", progname) ;
    printf("\twhere interface is the interface for which the password\n") ;
    printf("\tinformation is intended.  The valid values are:\n\n") ;
    printf("\n\t  %s - for IMS DB interface (IMS DB username and password)\n", 
		IMS_DB_IF) ;
    printf("\n") ;
    exit(APS_EXIT_ERROR) ;

} /* usage */





/*==============================================================================
Function:	main()

Description:	
	This is the driver for the APS_encrypt application program.
	It creates password files for the APS IMS_DB interface.

Parameters:	argc, argv
Returns:	exit status APS_EXIT_ERROR or APS_EXIT_OK
Creator:	Norbert Piega	
Creation Date:	12/09/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
main(int argc, char **argv)
#else
main(argc,argv)
   int argc ;
   char *argv[] ;
#endif
{
    extern char *optarg ;
    static int   opt ;                 /* Option letter */
    static char *optlist = "i:" ;      /* Valid options */
    char *interface_id = NULL ;
    int   estatus ;
    char  key[KEYSIZE] ;
    char  iv[IVSIZE] ;
    char  buf[BUFSIZE] ;
    char  tmpbuf[BUFSIZE] ;
    char *outfile = NULL ;
    char *bptr = NULL ;
    FILE *outfp = NULL ;
    char *APS_rootpath = NULL ;
    char *fname_complete = NULL ;

	/* set stdout to unbuffered I/O */
	setbuf( stdout, (char *) NULL ) ;

    if (argc < 2 )
        usage(argv[0]) ;

    while ((opt = getopt(argc, argv, optlist)) != EOF)
	{
        switch(opt)
        {
        case 'i':
            interface_id = strdup(optarg) ;
            break ;
	    
        default:
			usage(argv[0]) ;
        }
	}

	if ( interface_id == NULL )
		usage(argv[0]) ;

    if ((APS_rootpath = aps_fullpath(APS_UPW, NULL)) == (char *)NULL)
    {
        fprintf(stderr, "Unable to determine APS root path.  Exiting\n") ;
        exit(APS_EXIT_ERROR) ;
    }

    if (access(APS_rootpath, W_OK) != 0)
    {
        fprintf(stderr, "Error, Unable to access APS root path %s.  Exiting\n",
	 		APS_rootpath) ;
        exit(APS_EXIT_ERROR) ;
    }

    /* Initialize buffers
    */
    memset(key, 0,(size_t)KEYSIZE) ;
    memset(iv, 0, (size_t)IVSIZE) ;
    memset(buf, 0,(size_t)BUFSIZE) ;
    strcpy(key, APS_ENC_KEY) ;
  
    /* Find out for which interface, the password file is being created
    */
    if (strcmp(interface_id, IMS_DB_IF) == 0)
    {
	    outfile = APS_IMS_DB_UPW_FILE ;
    }
    else
    {
         fprintf(stderr,"Error, Invalid interface id specified in -i option\n");
	     exit(APS_EXIT_ERROR) ;
    }

    fname_complete = (char *)malloc(sizeof(char)*(strlen(APS_rootpath)+1 + 
					   strlen(outfile)+1)) ;
    strcpy(fname_complete, APS_rootpath) ;
    strcat(fname_complete, "/") ;
    strcat(fname_complete, outfile) ;

    if (access(fname_complete, F_OK) == 0)
    {
        fprintf(stderr,
"Error, password file %s \nalready exists.  Remove password file and then retry.  Exiting...\n", fname_complete ) ;
        exit(APS_EXIT_ERROR) ;
    }

    if ((outfp = fopen(fname_complete, "w")) == (FILE *)NULL)
    {
        fprintf(stderr, 
	   "Error, Unable to open password file %s for writing.  Exiting\n", 
			fname_complete ) ;
        exit(APS_EXIT_ERROR) ;
    }

	/* file is ready.  
	*/

    /* Prompt for username 
    */
    bptr = getpass("Please enter username : ") ;
    if (bptr == (char *)NULL)
    {
        fprintf(stderr, "Username entry error\n");
        exit(APS_EXIT_ERROR);
    }
    memcpy(buf, bptr, strlen(bptr)) ;
    bptr = (char *)getpass("Enter username again : ") ;
    if (strcmp(bptr, buf) != 0)
    {
        fprintf(stderr, "Username entry error\n");
        exit(APS_EXIT_ERROR);
    }
    strcat(buf, ",") ;

    /* Prompt for password
    */
    bptr = getpass("Please enter password : ") ;
    if (bptr == (char *)NULL)
    {
        fprintf(stderr, "Password entry error\n");
        exit(APS_EXIT_ERROR);
    }
    memcpy(tmpbuf, bptr, strlen(bptr)) ;
    bptr = (char *)getpass("Enter password again : ") ;
    if (strcmp(bptr, tmpbuf) != 0)
    {
        fprintf(stderr, "Password entry error\n");
        exit(APS_EXIT_ERROR);
    }
    strncat(buf, bptr, strlen(bptr)) ;

    /* Do data encryption
    */
    des_setparity(key) ;
    estatus = cbc_crypt(key, buf, BUFSIZE, DES_ENCRYPT | DES_SW, iv);
    switch(estatus)
    {

    case DESERR_NONE:
         fprintf(stderr, "Encryption for %s written to \n\t%s\n", interface_id, 
			fname_complete ) ;
         break ;

#ifdef SEE_MAN_MAGE_FOR_cbc_crypt
    case DESERR_NOHWDEVICE:
         fprintf(stderr, "Sofware Encryption ok\n") ;
         break ;
#endif

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
        fprintf(stderr, "Error setting password file permissions.  Exiting\n") ;
        remove(fname_complete) ;
        free(fname_complete) ;
        exit(APS_EXIT_ERROR) ;
    }
    free(fname_complete) ;

    exit(APS_EXIT_OK);

} /* main */


/* End of File */
