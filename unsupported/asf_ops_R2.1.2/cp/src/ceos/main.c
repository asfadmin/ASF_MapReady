/*-----------------------------------------------------------------------
 *
 * Executable:   ceos, ceos.pre.rsat, libceos.a
 *
 * Function:     Committee on Earth Observing Systems
 *
 * Author:       Alaska SAR Facility
 *
 * Date:         10/1/96
 *
 * SCCS Header:  sccsid_main_c[] = "@(#)main.c	1.10 96/10/04 14:42:26"
 *
 * Version 6.3   9/12/96  John Ho
 *   1. Add four OBJECTs into Data Set Summary Record of ceos leader file
 *   2. A common ceos library (libceos.a) is created in this package
 *   3. New Makefile generates ceos, ceos.pre.rsat, libceos.a
 *   4. The ceos package has been tested on SGI IRIX, DEC-AXP OSF, Sun
 *      Sparc SunOS 4, Sun Sparc SunOS 5, IBM AIX systems.
 * Version 6.4   10/1/96  John Ho
 *   1. Fixed format unmatch on Data Set Summary Record
 *   2. Remove warning msgs, corrected description header and rewording
 *      the Detail Processing Record in Trailer file
 *   3. Add Cal_status and Cal_comment in Data Quality Summary Record
 *   4. Add the Calibration Data Record in Trail file
 *   5. Baesd on the record size, only print/output the exist part
 *
 *----------------------------------------------------------------------*/
#include <stdio.h>
#include <math.h>
#ifdef SGI
#include <strings.h>
#else
#include <string.h>
#endif
#include <ctype.h>
#include "defs.h"
#include "extern.h"
#include "version.h"

static char sccsid_main_c[] = "@(#)main.c	1.10 96/10/04 14:42:26";

#define USAGE	"\n ldr=leader_file  \n or vdf=vdf_name \n or tape=tape_name \n [ceos_write=leader_file] \n [odl_read=filename] \n [odl_write=filename] \n [debug] \n -V\n"

/* Procedure and Function definitions */
int ldr_fileName(char* opt, char* arg);
int out_fileName(char* opt, char* arg);
int vdf_fileName( char* opt, char* arg);
int tape_deviceName( char* opt, char* arg);
int debug_it( char* opt, char* arg);
int odl_write_fileName( char* opt, char* arg);
int odl_read_fileName( char* opt, char* arg);

/* Added by Dan F. */
int scnOptions( char* opt, char* arg) ;
extern int process_from_ldr( void ) ;
extern int process_from_vdf( void ) ;
extern int process_from_odl( void ) ;

/* Structure for option storage */
typedef struct {
	char *name;
	int (*parser)();
} option;

/* Valid program options */
option Options[] = {
	{"ldr", ldr_fileName},
	{"vdf", vdf_fileName},
        {"ceos_write", out_fileName},
        {"tape", tape_deviceName},
        {"debug",debug_it}, 
	{"odl_write",  odl_write_fileName}, 
	{"odl_read",  odl_read_fileName},
	{NULL, NULL}
};

struct ceos_struct CEOS;

int Debug = FALSE;

int main(argc, argv)
	int argc;
	char **argv;
{
	int  i,j;
	char opt[BUFSIZ], arg[BUFSIZ];

	if (argc == 1) {
		printf("Usage: \n%s %s\n", argv[0], USAGE);
		exit(1);
	}

        init_ceos();

#ifdef HDEBUG
        printf("\n number of arguments %d",argc); printf("\n"); printf("\n");
#endif
	/* process argument list */
	for (i=1; i < argc; i++) {
                for(j=0;j<BUFSIZ;j++) arg[j]='\0';
		sscanf(argv[i], "%[^= ]=%s", opt, arg);
#ifdef HDEBUG
                printf("\n option %s? arg %s?",opt,arg);printf("\n"); printf("\n");
#endif
		if (scnOptions(opt, arg)) {
		  if (strcmp("-V", argv[i]) == 0) {
		    printf("\n ceos version %s\n", ceos_version);
		  } else {
		    printf("%s is invalid!\n", argv[i]);
		    printf("Usage: \n%s %s\n", argv[0], USAGE);
		    exit(0);
		  }
		}
	}

        if (CEOS.inputType == FROM_LDR) {
           process_from_ldr();
        }
        else if (CEOS.inputType == FROM_VDF) { /* assume this is a VDF, tape case handled within*/
           process_from_vdf();
        }
	else if (CEOS.inputType == FROM_ODL) { 
           process_from_odl();
        }
        else {
           printf("\n input file not specified");
        }
	
	CEOS_to_LDR();
	
	CEOS_to_ODL();
	
        /* Free allocated SARL structure */
        Free_SARL( GetCurrentSARL() );

        printf("\n Processing Completed\n");

	return(1);
}


/* @scnoptions */

/* Scan options list for command line option */
int scnOptions(opt, arg)
    char *opt, *arg;
{
    int i;

    for (i=0; Options[i].name != NULL; i++) {
	if (strcmp(opt, Options[i].name) == 0) 
	    return((*Options[i].parser)(opt, arg));
    }	
    return(1);
}

/* @ldr_filename */

/* Store input name file name */
int ldr_fileName(char* opt, char* arg)
{
    strcpy(CEOS.infile, arg);
    CEOS.inputType = FROM_LDR;

#ifdef HDEBUG
    printf("\n input file is %s",CEOS.infile);
#endif	
    return(0);
}

/* @out_filename */

/* Store output CEOS file name */
int out_fileName(char* opt, char* arg)
{
    strcpy(CEOS.outfile, arg);
    CEOS.out = TRUE;

#ifdef HDEBUG
    printf("\n output file is %s",CEOS.outfile);
#endif	
    return(0);
}

/* @vdf_filename */

/* Store input CEOS file name */
int vdf_fileName( char* opt, char* arg)
{
    strcpy(CEOS.infile, arg);
    if ( CEOS.inputType != NONO ) {
       printf("\n Multiple input files disallowed");
       return(1);
    }
    else CEOS.inputType = FROM_VDF;

#ifdef HDEBUG
    printf("\n input file is %s",CEOS.infile);
#endif	
    return(0);
}

/* @odl_write_filename */

/* Store output ODL file name */
int odl_write_fileName( char* opt, char* arg)
{
    strcpy(CEOS.odl_w_file, arg);
    CEOS.odl_out = TRUE;

#ifdef HDEBUG
    printf("\n input file is %s",CEOS.odl_w_file);
#endif	
    return(0);
}

/* @odl_read_filename */

/* Store input ODL file name */
int odl_read_fileName( char* opt, char* arg)
{
    strcpy(CEOS.odl_r_file, arg);
    if ( CEOS.inputType != NONO ) {
       printf("\n Multiple input files disallowed");
       return(1);
    }
    else CEOS.inputType = FROM_ODL;

#ifdef HDEBUG
    printf("\n input file is %s",CEOS.odl_r_file);
#endif	
    return(0);
}



/* @tape_devicename */

/* Store input name file name */
int tape_deviceName( char* opt, char* arg)
{
    strcpy(CEOS.infile, arg);
    CEOS.inputType = FROM_TAPE;

#ifdef HDEBUG
    printf("\n input file is %s",CEOS.infile);
#endif	
    return(0);
}



/* @debug_it */

/* sets debugger on. the default is off */
int debug_it( char* opt, char* arg)
{
        CEOS.debug = 1;
        Debug = TRUE;
	return(0);
}

