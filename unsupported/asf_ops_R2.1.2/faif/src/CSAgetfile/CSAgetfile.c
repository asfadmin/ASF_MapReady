/*==============================================================================
Filename:	CSAgetfile.c

Description:
	This program represents the CSAgetfile process.  It obtains a
specific type of CSA file from the remote CSA host machine via FTP and
then routes the obtained file to the appropriate ASF destination via
the function activate_route, the common directory monitor routing
function.

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
	CSA_SemKey_Table
Notes:
1.  March '96 - R. Hoffman
    Added log message noting file_type.
2.  May '97 - R. Hoffman
    Quit if no config file specified on command line.
    Clean up error messages.
3.  July '97 - R. Hoffman
    Get rid of putenv for FAIF_BINPATH.
    Make usage() indicate that -c is mandatory.
    Add another usage() call.
    Omit "Destroyed semaphore" syslog message.
==============================================================================*/

static char SccsFile[] = "CSAgetfile.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "06 Jun 1996";
static char SccsLastChanger[] = "@(#)CSAgetfile.c	1.5";
static char SccsState[] = "1.5";

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <syslog.h>
#include "CSA.h"
#include "CSAconfig.h"     /* Config variables tables and default settings */
#include "configrec.h"     /* Definition of Config record */
#include "sendfile.h"      /* Definition of get function and sendprm record */
#include "semops.h"        /* Definition of Semaphore keys */
#include "CSAget_upw.h"    /* CSA get password file */

#ifdef __STDC__
static void usage(char *) ;
#else
static void usage() ;
#endif


/* Table of CSA Semaphore Keys.  Note that each instance
-- of CSAgetfile for a particular file type corresponds to
-- a semaphore with the same key
*/
static Semaphore_Key_Table_Entry CSA_SemKey_Table[] =
{
   { CSA_PREDORBIT,  CSA_PORB_KEY   },
   { CSA_DEFVORBIT,  CSA_DORB_KEY   },
   { CSA_RECRQST,    CSA_RRQ_KEY    },
   { CSA_RECSCHED,   CSA_RSH_KEY    },
   { CSA_CALIBRQST,  CSA_CRQ_KEY    },
   { CSA_CALIBSCHED, CSA_CSH_KEY    },
   { CSA_SARPROCPRM, CSA_SARPP_KEY  },
   { CSA_RRQ_MCM,    CSA_RRQ_M_KEY  },
   { CSA_RSH_MCM,    CSA_RSH_M_KEY  },
   { CSA_CRQ_MCM,    CSA_CRQ_M_KEY  }, 
   { CSA_CSH_MCM,    CSA_CSH_M_KEY  },
   { SENTINEL,       SENTINEL       }
} ;


/* Table of password file to use per flight agency file type */
static File_Identifier CSA_PassFile_Table[] =
{
   { CSA_PREDORBIT,  CSAUFDROCF_UPW_FILE },
   { CSA_DEFVORBIT,  CSAUFDROCF_UPW_FILE },
   { CSA_RECRQST,    CSAUFDRFF_UPW_FILE  },
   { CSA_RECSCHED,   CSAUFDRFF_UPW_FILE  },
   { CSA_CALIBRQST,  CSAUFCALF_UPW_FILE  },
   { CSA_CALIBSCHED, CSAUFCALF_UPW_FILE  },
   { CSA_SARPROCPRM, CSAUFDROCF_UPW_FILE },
   { CSA_RRQ_MCM,    CSAUFDRFM_UPW_FILE  },
   { CSA_RSH_MCM,    CSAUFDRFM_UPW_FILE  },
   { CSA_CRQ_MCM,    CSAUFCALM_UPW_FILE  },
   { CSA_CSH_MCM,    CSAUFCALM_UPW_FILE  },
   { SENTINEL,       NULL                }  
} ;


extern Names_Table_Entry CSA_Config_Names_Table[] ;
extern File_Identifier   CSA_FileTypeStr_Table[] ;
extern Names_Table_Entry CSA_SrcDir_Table[] ;
extern Names_Table_Entry CSA_TransDir_Table[] ;
extern Names_Table_Entry CSA_ReceptDir_Table[] ;
extern Names_Table_Entry CSA_DestDir_Table[] ;

extern File_Transfer_Params *alloc_sendprms_record() ;
extern void                 *util_do_malloc() ;
extern Config_Record        *config_init() ;
extern int                   activate_route() ;
extern int                   get_file_via_ftp() ;
extern char                 *get_config_val() ;
extern int                   sem_init() ;
extern int                   sem_lock() ;
extern int                   sem_unlock() ;
extern int                   sem_destroy() ;


/*==============================================================================
Function:	static void usage(progname)

Description:	
	This function prints the CSAgetfile program banner.

Parameters:
	progname - name of the executable program.

Returns:	None
Creator:	Norbert Piega	
Creation Date:	10/04/1994
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
   int ii ;

   printf(" Usage:\n") ;
   printf("    %s -f filetype -c config_filename [-h] [-v] \n ", progname);
   printf("    [-l log_filename]\n") ;
   printf("\n") ;
   printf("    -f filetype. Get files of type filetype from CSA.\n") ;
   printf("       where filetype may be:\n") ;
   for(ii=0; CSA_FileTypeStr_Table[ii].file_id_number != SENTINEL; ii++)
      if (CSA_FileTypeStr_Table[ii].file_id_number > CSA_CSH_MCM)
	 continue ;
      else
         printf("          %s\n", CSA_FileTypeStr_Table[ii].file_identifier) ;
   printf("\n") ;
   printf("    -h Help.     Prints this banner.\n") ;
   printf("    -v Verbose.  Prints config settings.\n") ;
   printf("    -c config_filename.  Use config info from file specified\n") ;
   printf("                 via config_filename.\n") ;
   printf("    -l log_filename.  Print log info in the file specified via\n") ;
   printf("                 log_filename.\n") ;
   printf("\n") ;
} /* usage */



/*==============================================================================
Function:	main() for CSAgetfile

Description:	
	This is the main function for the CSAgetfile program.  It contains 
the actions performed by CSAgetfile upon activation from the command line.
After opening the system error log and performing the initial command line
option processing, a semaphore is created or obtained and then locked.  If
the semaphore is locked successfully, CSAgetfile proceeds with its tasks;
otherwise, it exits.  The actions performed by CSAgetfile while it has the
semaphore are as follows:
	- Obtain config variable values from the command line, config file,
environment variables, or default tables (Note that separate routines are
used to obtain common config values and file type specific ones).
	- Set up the file get routine parameters then call the file get
routine.  This obtains files from remote directories and stores them in
local directories.
	- Activate the routing function.  The files received from the
remote machines and stored in local directories are routed to designated
destinations in ASF (primarily via NFS).  Files that need to be translated
and renamed are handled by the router accordingly.

	Upon returning from the route function, CSAgetfile releases the
semaphore that it is holding and then destroys it.  The process ends at
this point.

Parameters:
	argc - number of command line arguments
	argv - array of command line argument strings

Returns:	0, 1	
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
   extern char *optarg ;              /* Command line option argument */
   int   opt;                         /* Option letter */
   char *optlist = "hf:c:l:v" ;       /* Valid options */
   key_t semaphore_key ;              /* Key for semaphore used */
   int semaphore_id ;                 /* Id for semaphore used */
   char *config_file = NULL ;         /* Name of config file */
   char *log_file = NULL ;            /* Name of forwarded files log file */
   char *file_type = NULL ;           /* File type string from cmd line */
   Config_Record *config = NULL;      /* Config record */
   int   FA_id ;                      /* id for file type */
   int   status ;                     /* Holds return status of functions */
   int   index ;                      /* Index into file type table */
   LOGICAL verbose_flag = FALSE ;     /* Flag for verbose option */
   LOGICAL help_flag = FALSE ;        /* Flag for help option */
   File_Transfer_Params *sendprm ;    /* Parameters for get file via FTP */
   char *real_receptdir = NULL ;      /* rootpath + receptdir */
   char *getfile_script = NULL ;      /* name of CSA get file script */
   char *tempstr = NULL ;             /* temp string */
   char *slash = NULL ;               /* ptr to slash character */
   char *passwd_file = NULL ;         /* Password file to use */
 
   /* Open the system error log */
   openlog(argv[0], LOG_PID|LOG_CONS|LOG_NDELAY, SYSLOG_FAIF) ;
   setlogmask(LOG_UPTO(LOG_DEBUG)) ;

   /* Check command line input */
   if (argc < 2) 
   {
      syslog(LOG_ERR, "ERROR: No config info specified. Exiting.\n") ;
      usage(argv[0]) ;
      exit (1);
   }

   /* Check options specified in command line */
   while ((opt = getopt(argc, argv, optlist)) != EOF)
      switch (opt) 
      {
         case 'f' :
	    file_type = (char *) strdup(optarg) ;
            break ;

         case 'c' :
	    config_file = (char *) strdup(optarg) ;
            break ;

         case 'l' :
            log_file = (char *) strdup(optarg) ;
            break ;

         case 'v' :
            verbose_flag = TRUE ;
            break ;

         case 'h' :
	    help_flag = TRUE ;
            usage(argv[0]) ;
            break ;

         default :
            usage(argv[0]) ;
	    syslog(LOG_ERR, "ERROR: Invalid command line option.  Exiting.\n"); 
	    exit(1) ;
      } /* endswitch */

   if (help_flag == TRUE)
      exit(0) ;

   if (config_file == (char *)NULL)
   {
      usage(argv[0]) ;
      syslog (LOG_ERR, "ERROR: No config file specified.  Exiting.\n");
      exit (1);
   }

   /* Make sure a file type is specified */
   if (file_type == (char *)NULL)
   {
      usage(argv[0]) ;
      syslog(LOG_ERR,
         "ERROR: Missing file type specification.  Exiting.\n") ;
      exit(1) ;
   }

   /* Check if the file type given is valid. */
   for (index=0; CSA_FileTypeStr_Table[index].file_id_number != SENTINEL;
	index++)
      if (strcmp(CSA_FileTypeStr_Table[index].file_identifier, file_type) == 0)
      {
	 FA_id = CSA_FileTypeStr_Table[index].file_id_number ;
	 break ;
      }
   if (CSA_FileTypeStr_Table[index].file_id_number == SENTINEL)
   {
      usage(argv[0]) ;
      syslog(LOG_ERR, 
	"ERROR: Command line specified file type %s unrecognized.  Exiting.\n", 
        file_type) ;
      exit(1) ;
   }

   syslog (LOG_NOTICE, "NOTICE, file type = %s.\n", file_type);

   /* Assign password file to use based on validated file type */
   for (index=0; CSA_PassFile_Table[index].file_id_number != SENTINEL; index++)
      if (CSA_PassFile_Table[index].file_id_number == FA_id)
      {
	 passwd_file = CSA_PassFile_Table[index].file_identifier ;
	 break ;
      }
   if (CSA_PassFile_Table[index].file_id_number == SENTINEL)
   {
      syslog(LOG_ERR, 
	 "ERROR: Unable to determine password file to use.  Exiting.\n") ;
      exit(1) ;
   }

   /* Print error message to stdout if config file is not readable */
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

   /* Check log filename if specified on command line */
   if (log_file != (char *)NULL)
      if (access(log_file, F_OK) == 0)
      {
         if (access(log_file, W_OK) != 0)
         {
            syslog(LOG_WARNING, 
		"WARNING: No write permission for Log filename %s.  Using default log file instead.\n",
	        log_file) ;
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
            syslog(LOG_WARNING, 
	       "WARNING: Log file directory %s not writable.  Using default log file instead.\n",
	       tempstr) ;
	    log_file = NULL ;
	 }
	 free(tempstr) ;
      }

   /* Obtain configuration settings to use */
   config = config_init(config_file, log_file, CSA_Config_Names_Table) ;
   if (config == (Config_Record *) NULL)
   {
      syslog(LOG_ERR, "ERROR: Error establishing Config info. Exiting.\n") ;
      exit(1) ;
   }

   /* Obtain per file type configuration via ftconfig_init.
   -- But first, find out which index in the Dir tables to use
   */
   for (index=0; CSA_SrcDir_Table[index].name_id != SENTINEL; index++)
      if (CSA_SrcDir_Table[index].name_id == FA_id)
         break ;
   if (CSA_SrcDir_Table[index].name_id == SENTINEL)
   {
      syslog(LOG_ERR, 
	 "ERROR: file type not found in Directory Tables. Exiting.\n") ;
      exit(1) ;
   }
   if (ftconfig_init(CSA_SrcDir_Table, CSA_ReceptDir_Table,
		     CSA_DestDir_Table, CSA_TransDir_Table,
		     index, config) == ERROR)
   {
      syslog(LOG_ERR, 
	 "ERROR: Error obtaining per file type configuration. Exiting.\n") ;
      exit(1) ;
   }
   config->FA_filetype = CSA_SrcDir_Table[index].name_id ;

   if (verbose_flag == TRUE)
   {
      printf("CSAgetfile configuration:\n") ;
      printf("   File type: %s\n", file_type) ;
      printf("   Source host: %s\n", config->FA_srchost) ;
      printf("   Log file:  %s\n", config->FA_logfile) ;
      printf("   Config file: %s\n", config->FA_configfile) ;
      if (config->FA_transfercmd != (char *)NULL)
         printf("   Forwarding protocol/script: %s\n", config->FA_transfercmd) ;
      printf("   Source directory: %s\n", config->FA_srcdir) ;
      printf("   Reception directory: %s\n", config->FA_receptdir) ;
/*      if (config->FA_transdir != (char *)NULL)         printf("   Translation directory: %s\n", config->FA_transdir) ; */

   }

   /* Create/obtain semaphore for this CSAgetfile process instance */
   semaphore_key = CSA_SemKey_Table[index].key ;
   if ((semaphore_id = sem_init(semaphore_key)) == ERROR)
   {
      syslog(LOG_ERR,
	 "ERROR: Unable to create/obtain semaphore for CSAgetfile type %s.  Exiting.\n",
	 CSA_FileTypeStr_Table[index].file_identifier) ;
      exit(1) ;
   }

   /* Get the lock for the obtained semaphore */
   if (sem_lock(semaphore_id) == ERROR)
   {
      syslog(LOG_ERR, 
	 "ERROR: Unable to perform semaphore lock.  Exiting.\n") ;
      exit(1) ;
   }

   /* Successfully obtained semaphore lock.  Proceed with the process. */

   /* Get the files from the remote host via FTP.  Store
   -- pulled files in the designated reception directory.
   */
   sendprm = alloc_sendprms_record() ;
   if (sendprm != (File_Transfer_Params *)NULL)
   {
      /* Assign parameter values for file send operation */
      sendprm->out_file =
	 util_do_malloc(sizeof(char)*
	    (strlen(CSA_FileTypeStr_Table[index].file_identifier)+1)) ;
      strcpy(sendprm->out_file, CSA_FileTypeStr_Table[index].file_identifier) ;

      sendprm->src_host = 
	 util_do_malloc(sizeof(char)*(strlen(config->FA_srchost)+1)) ;
      strcpy(sendprm->src_host, config->FA_srchost) ;

      sendprm->src_dir  =  
	 util_do_malloc(sizeof(char)*(strlen(config->FA_srcdir)+1)) ;
      strcpy(sendprm->src_dir, config->FA_srcdir) ;

      sendprm->dest_dir = 
	 util_do_malloc(sizeof(char)*(strlen(config->FA_receptdir)+1)) ;
      strcpy(sendprm->dest_dir, config->FA_receptdir) ;

      sendprm->mode = util_do_malloc(sizeof(char)*(strlen(ASCII)+1)) ;
      strcpy(sendprm->mode, ASCII) ;

      sendprm->rootpath = 
	 util_do_malloc(sizeof(char)*(strlen(config->FA_rootpath)+1)) ;
      strcpy(sendprm->rootpath, config->FA_rootpath) ;

      sendprm->user = 
	 util_do_malloc(sizeof(char)*(strlen(passwd_file)+1)) ;
      strcpy(sendprm->user, passwd_file) ;

      if ((getfile_script = 
	      (char *)get_config_val(config->FA_configfile,
                                     CSA_GETFILE_SCRIPT_EV)) != (char *)NULL)
      {
         sendprm->send_cmd =
	    util_do_malloc(sizeof(char)*(strlen(getfile_script)+1)) ;
         strcpy(sendprm->send_cmd, getfile_script) ;
	 free(getfile_script) ;
      }
      else
      {
	 sendprm->send_cmd = 
	    util_do_malloc(sizeof(char)*(strlen(CSA_GETFILE_SCRIPT_DEF)+1)) ;
         strcpy(sendprm->send_cmd, CSA_GETFILE_SCRIPT_DEF) ;
      }

      status = get_file_via_ftp(sendprm) ;

      free(sendprm->out_file) ;
      free(sendprm->src_host) ;
      free(sendprm->src_dir) ;
      free(sendprm->dest_dir) ;
      free(sendprm->mode) ;
      free(sendprm->rootpath) ;
      free(sendprm->send_cmd) ;
      free(sendprm) ;

      if (status == ERROR)
      {
	 syslog(LOG_ERR, 
	    "ERROR: Error encountered while getting files from CSA.  Exiting.\n") ;
         if (sem_destroy(semaphore_id) == ERROR)
            syslog(LOG_ERR, 
               "ERROR: Can't destroy semaphore with id %d.  Exiting.\n",
                semaphore_id) ;
         exit(1) ;
      }
   }

   /* Activate the execution of the routing function.
   -- Report anomalies resulting from the activation.
   */
   status = activate_route(config, CSA) ;
   if (status == ERROR)
      syslog(LOG_ERR, "ERROR: Error status returned by activate_route.\n") ;
   free(config) ;

   /* Done processing.  Release semaphore lock. */
   if (sem_unlock(semaphore_id) == ERROR)
   {
      syslog(LOG_ERR, 
	 "ERROR: Can't unlock semaphore with id %d. Exiting.\n", 
	  semaphore_id) ;
      if (sem_destroy(semaphore_id) == ERROR)
         syslog(LOG_ERR, 
            "ERROR: Can't destroy semaphore with id %d.  Exiting.\n",
             semaphore_id) ;
      exit(1) ;
   }
			    
   /* Destroy semaphore. */ 
   if (sem_destroy(semaphore_id) == ERROR)
   {
      syslog(LOG_ERR, 
	 "ERROR: Can't destroy semaphore with id %d. Exiting.\n", 
	  semaphore_id) ;
      exit(1) ;
   }

} /* main */


/* End of file */
