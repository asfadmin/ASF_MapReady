/*==============================================================================
Filename:	dirmonMain.c

Description:
	Files are sent periodically by a flight agency and stored in the
corresponding input directory.  The directory monitor is a process which
checks the input directory for incoming files.  The process can be
scheduled to be activated periodically via CRON.  The files found in the
incoming directory are sent to the IMS archive.  Note that when sending
files to the IMS archive, corresponding metadata must be generated and
sent as well.  A log file which lists the names of files forwarded
successfully by the monitor is maintained.

	The directory monitor is initiated from the command line.  A
config file and the log filename may be specified on activation as 
follows (for ESA):

	$ ESAdirmon -c ESA_config_file [-h] [-l ESA_log_file] [-v]

where ESA_config_file = config filename, ESA_log_file = name of the ESA
log file to be used to record the reception and routing of files.  The
-h and -v options are help and verbose options, respectively.

	Except for config filename, all options are optional.  A default
value is used for the log file if it is not specified on the command line.  

	The config file is a simple text file listing variables and
corresponding values.  The variables involved include flight agency
input directory names, destination directory names, log file filenames
and file transfer procedure.

	Note that values specified on the command line override all
other assignments.  For instance, the default log filename is 
overriden by the value from the command line.

	Thus, the following list summarizes which values are used given
the different possible ways of assigning program execution variables:

1. The config filename MUST be specified on the command line.
2. If a log file is specified in the command line, use the command line value.
3. Otherwise, use the default value assigned in this module.

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
	ESA_Config_Names_Table[]
	NASDA_Config_Names_Table[]
	WALPS_Config_Names_Table[]
	ADEOS_Config_Names_Table[]
	
Notes:
1.  May '97 - R. Hoffman
    Modified to stop if no config file specified on command line.
    Clean up syslog messages.
    Remove obsolete DTK_TBL / xlate stuff.
==============================================================================*/

static char SccsFile[] = "dirmonMain.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "23 Feb 1996";
static char SccsLastChanger[] = "@(#)dirmonMain.c	1.2";
static char SccsState[] = "1.2";

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <syslog.h>
#include "faifdefs.h"
#include "semops.h"
#ifdef ESA_DM
#include "ESAconfig.h"
#elif NASDA_DM
#include "NASDAconfig.h"
#elif WALPS_DM
#include "WALPSconfig.h"
#elif ADEOS_DM
#include "ADEOSconfig.h"
#endif

#ifdef __STDC__
static void usage(char *) ;
#else
static void usage() ;
#endif

extern void          *util_do_malloc() ;
extern Config_Record *config_init() ;
extern Config_Record *alloc_config_record() ;
extern int            activate_route() ;

#ifdef ESA_DM
extern Names_Table_Entry ESA_Config_Names_Table[] ;
extern Names_Table_Entry ESA_ReceptDir_Table[] ;
extern Names_Table_Entry ESA_DestDir_Table[] ;
extern Names_Table_Entry ESA_TransDir_Table[] ;
extern File_Identifier ESA_FileTypeStr_Table[] ;

#elif NASDA_DM
extern Names_Table_Entry NASDA_Config_Names_Table[] ;
extern Names_Table_Entry NASDA_ReceptDir_Table[] ;
extern Names_Table_Entry NASDA_DestDir_Table[] ;
extern Names_Table_Entry NASDA_TransDir_Table[] ;
extern File_Identifier NASDA_FileTypeStr_Table[] ;

#elif WALPS_DM
extern Names_Table_Entry WALPS_Config_Names_Table[] ;
extern Names_Table_Entry WALPS_ReceptDir_Table[] ;
extern Names_Table_Entry WALPS_DestDir_Table[] ;
extern Names_Table_Entry WALPS_TransDir_Table[] ;
extern File_Identifier WALPS_FileTypeStr_Table[] ;

#elif ADEOS_DM
extern Names_Table_Entry ADEOS_Config_Names_Table[] ;
extern Names_Table_Entry ADEOS_ReceptDir_Table[] ;
extern Names_Table_Entry ADEOS_DestDir_Table[] ;
extern Names_Table_Entry ADEOS_TransDir_Table[] ;
extern File_Identifier ADEOS_FileTypeStr_Table[] ;

#endif

#define MAX_CONFIG_RECS 20
static Config_Record *configrec_array[MAX_CONFIG_RECS] ;

#define MSG_LEN 255
char		*file_util_progname ; /* required by libfileutils.a */
char		file_util_msg[MSG_LEN]; /* required by libfileutils.a */


/*==============================================================================
Function:	static void usage(progname)

Description:	
	This function prints the directory monitor banner.

Parameters:
	progname - name of the directory monitor executable program.

Returns:	None
Creator:	Norbert Piega	
Creation Date:	06/29/1994
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
   printf(" Usage:\n") ;
   printf("    %s -c config_file_name [-h] [-v] [-l log_file_name]\n",
      progname) ;
   printf("\n") ;
} /* usage */




/*==============================================================================
Function:	main function for directory monitor

Description:	
        This is the main function for the directory monitors.  It contains 
the actions performed by XXXdirmon (where XXX may be ESA, NASDA or WALPS
depending on which one this is compiled for) upon activation from the 
command line.  After opening the system error log and performing the 
initial command line option processing, a semaphore is created or obtained 
and then locked.  If the semaphore is locked successfully, XXXdirmon 
proceeds with its tasks; otherwise, it exits.  The actions performed by 
XXXdirmon while it has the semaphore are as follows:
	- Obtain config variable values from the command line, config file,
environment variables, or default tables (Note that separate routines are 
used to obtain common config values and file type specific ones).

	- Activate the routing function.  The files received from flight 
agencies which they stored in local directories are routed to designated 
destinations in ASF (primarily via NFS).  No files need to be translated 
for ESA, NASDA, WALPS in R1A.
				 
	Upon returning from the route function, XXXdirmon releases the 
semaphore that it is holding and then destroys it.  The process ends at 
this point.

Parameters:
	argc - number of command line arguments
	argv - array of command line argument strings

Returns:	program exit status	
Creator:	Norbert Piega	
Creation Date:	06/29/1994
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
   static int   opt;                  /* Option letter */
   static char *optlist = "hc:l:v" ;  /* Valid options */
   extern char *optarg ;              /* Command line option argument */
   key_t semaphore_key ;              /* Key for semaphore used */
   int semaphore_id ;                 /* Id of semaphore used */
   char *config_file = NULL ;         /* Name of config file */
   char *log_file = NULL ;            /* Name of log file */
   Config_Record *config = NULL;      /* Dirmon config record */
   int   status ;                     /* Return status of monitor activation */
   LOGICAL verbose_flag = FALSE ;     /* Verbose option flag */ 
   LOGICAL help_flag = FALSE ;        /* Help option flag */ 
   char *real_receptdir = NULL ;      /* rootpath+receptdir */
   char *slash = NULL ;               /* ptr to slash char in logfilename */
   char *tempstr ;                    /* temp string for checking logfile */
   int i, j, index ;                  /* Index into ftconfig tables */
   int count = 0 ;                    /* Number of file type configs */
   char putenv_str[MAXLINE];
 
   /* Open the system error log
   */
   openlog(argv[0], LOG_PID|LOG_CONS|LOG_NDELAY, SYSLOG_FAIF) ;
   setlogmask(LOG_UPTO(LOG_DEBUG)) ;

   /* Check command line input
   */
   if (argc < 2) 
   {
      syslog(LOG_ERR, "ERROR: No config file specified.  Exiting.\n") ;
      usage(argv[0]) ;
      exit (1);
   }    

   /* Check options specified in command line
   */
   while ((opt = getopt(argc, argv, optlist)) != EOF)
      switch (opt) 
      {
         case 'h' :
	    help_flag = TRUE ;
            usage(argv[0]) ;
            break ;

         case 'c' :
	    config_file       = (char *) strdup(optarg) ;
            break ;

         case 'l' :
            log_file = (char *) strdup(optarg) ;
            break ;

         case 'v' :
            verbose_flag = TRUE ;
            break ;

         default :
            syslog(LOG_ERR, 
	       "ERROR: Invalid option in %s execution. Exiting.\n", argv[0]) ;
            usage(argv[0]) ;
	    exit(1) ;
      } /* endswitch */

   if (help_flag == TRUE)
      exit(0) ;

   if (config_file == (char *)NULL)
   {
      syslog (LOG_ERR, "ERROR: No config file specified.  Exiting.\n");
      usage(argv[0]) ;
      exit (1);
   }

   /* Check config file. */
   if (config_file != (char *)NULL)
      if (access(config_file, F_OK) != 0)
      {
         syslog(LOG_ERR, 
            "ERROR: Config file %s does not exist.  Exiting.\n", config_file) ;
         exit (1);
      }
      else if (access(config_file, R_OK) != 0)
      {
         syslog(LOG_ERR, 
            "ERROR: Config file %s is not readable.  Exiting.\n", config_file) ;
         exit (1);
      }

   /* Check log filename specified in command line. */
   if (log_file != (char *)NULL)
      if (access(log_file, F_OK) == 0)
      {
         if (access(log_file, W_OK) != 0)
         {
            syslog(LOG_ERR, 
	       "ERROR: No write permission for Log filename %s.\n", log_file) ;
	    log_file = NULL ;
         }
      }
      else
      {
         tempstr = (char *)util_do_malloc(sizeof(char)*(strlen(log_file)+1)) ;
	 strcpy(tempstr, log_file) ;
         slash = strrchr(tempstr, '/') ;
	 *slash = '\0' ;
	 if (access(tempstr, W_OK) != 0)
	 {
            syslog(LOG_ERR,
	       "ERROR: Log file directory %s not writable. \n", tempstr) ;
	    log_file = NULL ;
         }
	 free(tempstr) ;
      }

   /* Obtain configuration settings to use. */
#ifdef ESA_DM
   config = config_init(config_file, log_file, ESA_Config_Names_Table) ;
#elif NASDA_DM
   config = config_init(config_file, log_file, NASDA_Config_Names_Table) ;
#elif WALPS_DM
   config = config_init(config_file, log_file, WALPS_Config_Names_Table) ;
#elif ADEOS_DM
   config = config_init(config_file, log_file, ADEOS_Config_Names_Table) ;
#endif

   if (config == (Config_Record *) NULL)
   {
      syslog(LOG_ERR, "ERROR: Error establishing Config info. Exiting.\n") ;
      exit(1) ;
   }

   /* Initialize array of config records -- one for each incoming file type. */
   for (i=0; i<MAX_CONFIG_RECS; i++)
      configrec_array[i] = (Config_Record *)NULL ;

#ifdef ESA_DM
   for (index=0; ESA_ReceptDir_Table[index].name_id != SENTINEL; index++)
      if (ftconfig_init(NULL, ESA_ReceptDir_Table, ESA_DestDir_Table, 
			      ESA_TransDir_Table, index, config) == ERROR)
#elif NASDA_DM
   for (index=0; NASDA_ReceptDir_Table[index].name_id != SENTINEL; index++)
      if (ftconfig_init(NULL, NASDA_ReceptDir_Table, NASDA_DestDir_Table, 
			      NASDA_TransDir_Table, index, config) == ERROR)
#elif WALPS_DM
   for (index=0; WALPS_ReceptDir_Table[index].name_id != SENTINEL; index++)
      if (ftconfig_init(NULL, WALPS_ReceptDir_Table, WALPS_DestDir_Table, 
			      WALPS_TransDir_Table, index, config) == ERROR)
#elif ADEOS_DM
   for (index=0; ADEOS_ReceptDir_Table[index].name_id != SENTINEL; index++)
      if (ftconfig_init(NULL, ADEOS_ReceptDir_Table, ADEOS_DestDir_Table,
			      ADEOS_TransDir_Table, index, config) == ERROR)
#endif
      {
         syslog(LOG_ERR, 
	    "ERROR: Error obtaining Reception Directory name\n") ;
         free(config) ;
         exit(1) ;
      }
      else
      {
	 configrec_array[count] = (Config_Record *)alloc_config_record() ;

#ifdef ESA_DM
         (configrec_array[count])->FA_filetype = 
            ESA_ReceptDir_Table[count].name_id ;
#elif NASDA_DM
         (configrec_array[count])->FA_filetype =
	    NASDA_ReceptDir_Table[count].name_id ;
#elif WALPS_DM
         (configrec_array[count])->FA_filetype =
	    WALPS_ReceptDir_Table[count].name_id ;
#elif ADEOS_DM
         (configrec_array[count])->FA_filetype =
	    ADEOS_ReceptDir_Table[count].name_id ;
#endif

	 if (config->FA_configfile != (char *)NULL)
	 {
	    (configrec_array[count])->FA_configfile = 
	       (char *)util_do_malloc(sizeof(char *)*
				   (strlen(config->FA_configfile)+1)) ;
            strcpy((configrec_array[count])->FA_configfile, 
                    config->FA_configfile) ;
	 }

	 if (config->FA_logfile != (char *)NULL)
	 {
	    (configrec_array[count])->FA_logfile =
	       (char *)util_do_malloc(sizeof(char *)*
				   (strlen(config->FA_logfile)+1)) ;
            strcpy((configrec_array[count])->FA_logfile, config->FA_logfile) ;
         }

         if (config->FA_srcdir != (char *)NULL)
         {
            (configrec_array[count])->FA_srcdir =
               (char *)util_do_malloc(sizeof(char *)*
                                   (strlen(config->FA_srcdir)+1)) ;
            strcpy((configrec_array[count])->FA_srcdir, config->FA_srcdir) ;
         }
 
         if (config->FA_srchost != (char *)NULL)
         {
            (configrec_array[count])->FA_srchost =
               (char *)util_do_malloc(sizeof(char *)*
                                   (strlen(config->FA_srchost)+1)) ;
            strcpy((configrec_array[count])->FA_srchost, config->FA_srchost) ;
         }
 
         if (config->FA_destspec != (char *)NULL)
         {
            (configrec_array[count])->FA_destspec =
               (char *)util_do_malloc(sizeof(char *)*
                                   (strlen(config->FA_destspec)+1)) ;
            strcpy((configrec_array[count])->FA_destspec, config->FA_destspec);
         }
 
         if (config->FA_desthost != (char *)NULL)
         {
            (configrec_array[count])->FA_desthost =
               (char *)util_do_malloc(sizeof(char *)*
                                   (strlen(config->FA_desthost)+1)) ;
            strcpy((configrec_array[count])->FA_desthost, config->FA_desthost);
         }
 
         if (config->FA_receptdir != (char *)NULL)
	 {
	    (configrec_array[count])->FA_receptdir = 
	       (char *)util_do_malloc(sizeof(char *)*
				   (strlen(config->FA_receptdir)+1)) ;
            strcpy((configrec_array[count])->FA_receptdir, 
                    config->FA_receptdir) ;
	 }

         if (config->FA_transdir != (char *)NULL)
         {
            (configrec_array[count])->FA_transdir =
               (char *)util_do_malloc(sizeof(char *)*
                                   (strlen(config->FA_transdir)+1)) ;
            strcpy((configrec_array[count])->FA_transdir, config->FA_transdir);
         }
 
         if (config->FA_transfercmd != (char *)NULL)
	 {
	    (configrec_array[count])->FA_transfercmd = 
	       (char *)util_do_malloc(sizeof(char *)*
				   (strlen(config->FA_transfercmd)+1)) ;
            strcpy((configrec_array[count])->FA_transfercmd, 
		   config->FA_transfercmd) ;
	 }

         if (config->FA_rootpath != (char *)NULL)
	 {
	    (configrec_array[count])->FA_rootpath = 
	       (char *)util_do_malloc(sizeof(char *)*
				   (strlen(config->FA_rootpath)+1)) ;
            strcpy((configrec_array[count])->FA_rootpath, config->FA_rootpath) ;
	 }
	 count++ ;
      }
   
   if (verbose_flag == TRUE)
   {
      printf("Directory monitor configuration:\n") ;
      if (config->FA_rootpath != (char *)NULL)
         printf("  FAIF rootpath : %s\n", config->FA_rootpath) ;

      if (config->FA_logfile != (char *)NULL)
         printf("  Log file :              %s\n", config->FA_logfile) ;

      if (config->FA_configfile != (char *)NULL)
         printf("  Config file :           %s\n", config->FA_configfile) ;

      printf("--------------------------------------------------------\n") ;
      for (i=0; i<count; i++)
      {
#ifdef ESA_DM
         for (j=0; ESA_FileTypeStr_Table[j].file_id_number != SENTINEL; j++)
	    if (ESA_FileTypeStr_Table[j].file_id_number ==
	        (configrec_array[i])->FA_filetype)  
            {
               printf("  ESA file type id : %s\n", 
		  ESA_FileTypeStr_Table[j].file_identifier) ;
	       break ;
	    }
#elif NASDA_DM
         for (j=0; NASDA_FileTypeStr_Table[j].file_id_number != SENTINEL; j++)
	    if (NASDA_FileTypeStr_Table[j].file_id_number ==
	        (configrec_array[i])->FA_filetype)  
            {
               printf("  NASDA file type id : %s\n", 
		  NASDA_FileTypeStr_Table[j].file_identifier) ;
	       break ;
	    }
#elif WALPS_DM
         for (j=0; WALPS_FileTypeStr_Table[j].file_id_number != SENTINEL; j++)
	    if (WALPS_FileTypeStr_Table[j].file_id_number ==
	        (configrec_array[i])->FA_filetype)  
            {
               printf("  WALPS file type id : %s\n", 
		  WALPS_FileTypeStr_Table[j].file_identifier) ;
	       break ;
	    }
#elif ADEOS_DM
         for (j=0; ADEOS_FileTypeStr_Table[j].file_id_number != SENTINEL; j++)
	    if (ADEOS_FileTypeStr_Table[j].file_id_number ==
	        (configrec_array[i])->FA_filetype)  
            {
               printf("  ADEOS file type id : %s\n", 
		  ADEOS_FileTypeStr_Table[j].file_identifier) ;
	       break ;
	    }
#endif
         if ((configrec_array[i])->FA_srchost != (char *)NULL)
            printf("  Source host : %s\n", (configrec_array[i])->FA_srchost) ;
 
         if ((configrec_array[i])->FA_srcdir != (char *)NULL)
            printf("  Source directory : %s\n",
               (configrec_array[i])->FA_srcdir) ;
 
         if ((configrec_array[i])->FA_desthost != (char *)NULL)
            printf("  Destination host : %s\n",
               (configrec_array[i])->FA_desthost) ;
 
         if ((configrec_array[i])->FA_destspec != (char *)NULL)
            printf("  Destination directory : %s\n",
               (configrec_array[i])->FA_destspec) ;
 
         if ((configrec_array[i])->FA_receptdir != (char *)NULL)
            printf("  Reception directory : %s\n", 
	       (configrec_array[i])->FA_receptdir) ;

         if ((configrec_array[i])->FA_transdir != (char *)NULL)
            printf("  Translated files directory : %s\n",
               (configrec_array[i])->FA_transdir) ;
 
         if ((configrec_array[i])->FA_transfercmd != (char *)NULL)
            printf("  Forwarding protocol/script : %s\n", 
	       (configrec_array[i])->FA_transfercmd) ;

         printf("--------------------------------------------------------\n") ;
      }
   }

   /* Create/obtain semaphore
   */
#ifdef ESA_DM
   semaphore_key = ESA_DIRMON_KEY ;
#elif NASDA_DM
   semaphore_key = NASDA_DIRMON_KEY ;
#elif WALPS_DM
   semaphore_key = WALPS_DIRMON_KEY ;
#elif ADEOS_DM
   semaphore_key = ADEOS_DIRMON_KEY ;
#endif
   if ((semaphore_id = sem_init(semaphore_key)) == ERROR) 
   {
      syslog(LOG_ERR,  "ERROR: Unable to create/obtain semaphore. Exiting.\n") ;
      exit(1) ;
   }

   /* Get the lock for the obtained semaphore
   */
   if (sem_lock(semaphore_id) == ERROR) 
   {
      syslog(LOG_ERR, "ERROR: Unable to perform semaphore lock.  Exiting.\n") ;
      exit(1) ;
   }

   /* Obtained semaphore lock.  Proceed with the dirmon process.
   */

   /* Activate the execution of the directory monitoring function.
   -- Report anomalies resulting from the activation.
   */
   for (i=0; i<count; i++)
   {
#ifdef ESA_DM
      status = activate_route(configrec_array[i], ESA) ;

#elif NASDA_DM
      status = activate_route(configrec_array[i], NASDA) ;

#elif WALPS_DM
      status = activate_route(configrec_array[i], WALPS) ;

#elif ADEOS_DM
      status = activate_route(configrec_array[i], ADEOS) ;

#endif

      if (status == ERROR)
      {
         syslog(LOG_ERR, 
	       "ERROR: Error status returned by activate_route for reception directory %s.\n", (configrec_array[i])->FA_receptdir) ;
      }

      if ((configrec_array[i])->FA_srcdir != (char *)NULL)
         free((configrec_array[i])->FA_srcdir) ;
 
      if ((configrec_array[i])->FA_srchost != (char *)NULL)
         free((configrec_array[i])->FA_srchost) ;
 
      if ((configrec_array[i])->FA_destspec != (char *)NULL)
         free((configrec_array[i])->FA_destspec) ;
 
      if ((configrec_array[i])->FA_desthost != (char *)NULL)
         free((configrec_array[i])->FA_desthost) ;
 
      if ((configrec_array[i])->FA_receptdir != (char *)NULL)
         free((configrec_array[i])->FA_receptdir) ;

      if ((configrec_array[i])->FA_transdir != (char *)NULL)
         free((configrec_array[i])->FA_transdir) ;
 
      if ((configrec_array[i])->FA_configfile != (char *)NULL)
         free((configrec_array[i])->FA_configfile) ;

      if ((configrec_array[i])->FA_logfile != (char *)NULL)
         free((configrec_array[i])->FA_logfile) ;

      if ((configrec_array[i])->FA_transfercmd != (char *)NULL)
         free((configrec_array[i])->FA_transfercmd) ;

      if ((configrec_array[i])->FA_rootpath != (char *)NULL)
         free((configrec_array[i])->FA_rootpath) ;

      free(configrec_array[i]) ;

   } /* EndForLoop */

   free(config) ;

   /* Done processing.  Release semaphore.
   */

   if (sem_unlock(semaphore_id) == ERROR) 
   {
      syslog(LOG_ERR, 
	 "ERROR: Can't unlock semaphore with id %d. Exiting.\n", 
	  semaphore_id) ; 
      exit(1) ; 
   }

   /* Destroy semaphore.
   */
   if (sem_destroy(semaphore_id) == ERROR)
   {
      syslog(LOG_ERR,
	 "ERROR: Can't destroy semaphore with id %d.  Exiting.\n",
	  semaphore_id) ; 
      exit(1) ; 
   }

} /* main */


/* End of file */
