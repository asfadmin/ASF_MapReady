#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		main.c

Description:	This is the test driver for the archive_APS_file() 

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident   "@(#)main.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/archive/SCCS/s.main.c"

#include <stdio.h>
#include <string.h>  /* for strncpy, etc. */
#include <stdlib.h>  /* for getenv */
#include <malloc.h>  /* for malloc */
#include "aps_log_msg.h"
#include "aps_defs.h"

#define USERID_LENGTH 256

typedef struct 
		{char *dataSet;
		 char *format;
		 int  fileCount;
		 char **extensions;
		}    VALID_FILE ;


static void usage(char*, VALID_FILE*);

extern int archive_APS_file (char *programName,
                      char *sourceDir,
                      char *file_name,
                      char *platform,
                      char *dataset,
                      char *format,
                      char **extensions,
                      char *userid, char *password, 
					  int  fileCount, int  delFlag) ;

extern void display_error(char *,char *);

/**************************************************************
**
** main ()
**
**
*****************************************************************/

void main (int argc, char *argv[])
{

    extern int  optind ;
    extern char *optarg ;

    int  i,j,opt ;
    int  invalid_data_set = 1;
	int  delFlag = 0;
	int	 fileCount ;
    char *optlist  = "hf:d:P:U:D";
    char dirName[256];
    char fileName[32];
    char *platform = NULL;
    char *format = NULL;
    char *progName = NULL;
    char *dataSet =  NULL;
    char *complete_filename = NULL;
	char **extensions = NULL ;
    char *password =  NULL;
    char *ptr;
    char *userid = NULL;
	static char *D_M_extensions[] = {"M", "D"} ;
	static char *M_extensions[]   = {"M"} ;
	char msg[MSG_LEN];

    VALID_FILE valid_files[] = 
    {
	{"ASF DL TO DTK MAP",	"PMF", 		1,		M_extensions},
    {"ERS-1 RQUS",			"FA",		2,		D_M_extensions},
	{"ERS-2 RQUS",			"FA",		2,		D_M_extensions},
	{"ERS-1 REUG",			"FA",		2,		D_M_extensions},
	{"ERS-2 REUG",			"FA",		2,		D_M_extensions},
	{"JERS-1 REQQ",			"FA",		2,		D_M_extensions},
	{"JERS-1 REQW",			"FA",		2,		D_M_extensions},
	{"JERS-1 MSGF",			"FA",		2,		D_M_extensions},
	{"JERS-1 MSGE",			"FA",		2,		D_M_extensions},
	{"RADARSAT-1 CSA_RECAVAILRPT",	"FA",		2,		D_M_extensions},
	{"RADARSAT-1 CSA_CALIBRPT",		"FA",		2,		D_M_extensions},
	{"RADARSAT-1 CSA_CALIBAVAILRPT","FA",		2,		D_M_extensions},
	{"WFF WALPS_AVAIL_REQ",			"FA",		2,		D_M_extensions},
	{"WFF WALPS_WOS",		"FA",		2,		D_M_extensions},
	{"WFF WALPS_STVEC",		"FA",		2,		D_M_extensions},
	{"ADEOS-1 STGS",		"FA",		2,		D_M_extensions},
	{"HC ASF_WOS",			"FA",		2,		D_M_extensions},
	{"HC ASF_STVEC",		"FA",		2,		D_M_extensions},
	{NULL, NULL, 0, NULL}
	};

	/* set stdout to unbuffered I/O */
	setbuf( stdout, (char *) NULL ) ;
	
	userid = (char *) malloc (USERID_LENGTH);

    progName = (char *) strdup(argv[0]) ;
    aps_open_syslog();
	sprintf(msg, "Program started with arguments: " ) ;
	for( j = 1; j < argc; j++ )
	{
	    strcat(msg, " " ) ;
	    strcat(msg, argv[j] ) ;
	}
	aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
	 
    while((opt = getopt(argc, argv, optlist)) != EOF)
    {
		switch(opt)
		{
 	    	case 'h' :
				usage(progName,valid_files) ;
				break ;

	    	case 'f' :
				complete_filename = (char *) strdup(optarg) ;

				if ((ptr = (char *) strrchr(complete_filename , '/')) 
							!= (char *) NULL)
				{
					*ptr = '\0';
					ptr++;	
				}    
				else
				{
					 sprintf(msg, " Invalid file name '%s'.", complete_filename);
					 aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
					 exit(APS_EXIT_ERROR);
				}

				strcpy( dirName , complete_filename);
				strcpy( fileName, ptr);
				break ;

	    	case 'd' :
				dataSet = (char *) strdup(optarg);
				 for (i=0; valid_files[i].dataSet != NULL; i++)
				 {
					if(strcmp(valid_files[i].dataSet, dataSet) == 0)
					{
						extensions = valid_files[i].extensions ;
						fileCount = valid_files[i].fileCount ;
						format = valid_files[i].format ;
						invalid_data_set = 0;
						break;
					}
				 }

				 if (invalid_data_set)
				 {
					sprintf(msg, " Invalid data set name '%s'.", dataSet);
					aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
					usage(progName,valid_files);
				 }

				 platform = (char *) strdup(dataSet);
				 if ((ptr = (char *) strchr(platform, ' ')) != (char *) NULL)
				 {
					*ptr = '\0';
				 }
				 else
				 {
					sprintf(msg, " Invalid data set name '%s'.", dataSet);
					aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
					exit(APS_EXIT_ERROR);
				 }
				break ;

	    	case 'P' :
				password = (char *) strdup(optarg) ;
		    	break ;

	    	case 'U' :
				userid = (char *) strdup(optarg) ;
				break ;

	    	case 'D' :
				delFlag = 1;
		    	break ;

	    	default :
				sprintf(msg, "Invalid option input on command line.");
				aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
				usage(progName,valid_files) ;
				break ;
	 	}
     }

    /* check that all words are attached to a flag.   */
    if(optind != argc)
    {
	 	sprintf(msg, "Missing input parameters.");
		aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
		usage(progName,valid_files);
    }
 
    /* mandatory flags  */
    if (password == NULL
	||  dataSet == NULL
    ||  complete_filename == NULL )
    {
	 	sprintf(msg, "Missing input parameters.");
		aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
		usage(progName,valid_files);
    }


     if ((strcmp(userid, "") == 0))
	 {
		if ((userid  = (char *)getenv("APS_SYBASE_USERID")) == NULL)
		{
			display_error(progName,"APS_SYBASE_USERID");
		}
	 }


     if (archive_APS_file(progName,dirName,fileName,
						platform,
						dataSet,
						format,
						extensions,
						userid,password,
						fileCount, delFlag) != 0)
     {
		sprintf(msg, "Archiving %s/%s failed!", dirName, fileName) ;	
		aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
		sprintf(msg, "Program exited with errors! (see messages/logs)");
		aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);

		free (userid) ;
		exit(APS_EXIT_ERROR);
     }
     else
     {
		sprintf(msg, "Program completed successfully.");
	  	aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);

		free (userid) ;
		exit(APS_EXIT_OK);
     }
} /* main */

/***************************************************************
**
** usage ()
**
**************************************************************** */
static void 
usage(char *progName, VALID_FILE valid_files[])
{
int i;

	i=0;
	printf("Usage:\n%s -h -f <fileName> -d <dataSet> -P <password> [-U <userid>] [-D]\n", progName) ;
	printf("where....\n") ;
	printf("\n") ;
	printf("\t-f <fileName> -- filename with full path name\n");

	printf("\t-d <dataSet>  -- \n"); 
	for (i=0; valid_files[i].dataSet != NULL; i++)
	{
	    printf("\t\t\t %s |\n",valid_files[i].dataSet);
	}

	printf("\t-P <password> -- SYSBASE user password\n");
	printf("\t[-U <userid>] -- SYBASE userid, APS_SYBASE_USERID will be checked if not entered\n"); 
	printf("\t[-D]  	      -- delete file\n") ;
	printf("\n") ;
	printf("Note: The following enviroment variables should be set properly\n");
	printf("\n");
	printf("\tIMS_SERVER\n");
	printf("\tIMS_DB\n");
	printf("\tAPS_SYBASE_USERID\n");
	printf("\tIMS_ACCOUNT_ID\n");
	exit(APS_EXIT_ERROR) ;
} /* usage */

