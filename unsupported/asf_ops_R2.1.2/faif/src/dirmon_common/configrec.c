/*==============================================================================
Configname:	configrec.c

Description:
	This module contains the function for allocation, initialization
and modification of a config record.

External Functions:
	config_init
	
Static Functions:
	alloc_config_record
	get_rootpath_from_env
	get_config_default
	
External Variables Defined:
	None
	
Config Scope Static Variables:
	None
	
Notes:
1.  June '96 - R. Hoffman
    Call expand_path_val() to handle environment variables in path strings
    within the config file.
2.  July '97 - R. Hoffman
    Only use environment variables for FAIF_ROOTPATH and FAIF_BINPATH.
    Clean up syslog messages.
==============================================================================*/

static char SccsConfig[] = "configrec.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "12 Jun 1996";
static char SccsLastChanger[] = "@(#)configrec.c	1.2";
static char SccsState[] = "1.2";

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <ctype.h>
#include "configrec.h"
#include "dapps_list.h"
#include "nmalloc.h"



#ifdef __STDC__
Config_Record * alloc_config_record(void) ;
Config_Record * config_init(char *, char *, Names_Table_Entry *) ;
static int    get_rootpath_from_env(Names_Table_Entry *, int, Config_Record *) ;
static int    get_config_default(Names_Table_Entry *, int, Config_Record *) ;
#else
Config_Record * alloc_config_record() ;
Config_Record * config_init() ;
static int      get_rootpath_from_env() ;
static int      get_config_default() ;
#endif


extern void          *util_do_malloc() ;
extern char          *expand_path_val();


/*==============================================================================
Function:	static Config_Record *alloc_config_record(void)

Description:
	Allocate and initialize a config record data structure
This function allocates a config record and then initializes its
fields.  If the allocation fails, NULL is returned.

Parameters:	None
Returns:	pointer to newly allocated record or NULL(if allocate failed) 
Creator:	Norbert L. Piega / JPL
Creation Date:	08/03/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
Config_Record *
alloc_config_record(void)
#else
Config_Record *
alloc_config_record()
#endif
{
   Config_Record *configrec = NULL ;

   configrec = (Config_Record *) NEW(sizeof(Config_Record)) ;
   if (configrec != (Config_Record *)NULL)
   {
      configrec->FA_configfile = NULL ; /* Config filename */
      configrec->FA_logfile = NULL ;    /* Directory monitor log file */
      configrec->FA_srcdir = NULL ;     /* Name of Incoming files directory */
      configrec->FA_srchost = NULL ;    /* Source hostname */
      configrec->FA_destspec = NULL ;   /* Destination specification */
      configrec->FA_desthost = NULL ;   /* Destination hostname */
      configrec->FA_receptdir = NULL ;  /* Name of Received files directory */
      configrec->FA_transdir = NULL ;   /* Name of Translated files directory */
      configrec->FA_transfercmd = NULL ; /* Transfer protocol */
      configrec->FA_rootpath = NULL ;    /* Root path of FAIF dirs */
      configrec->FA_filetype = -1 ;      /* FA file type id */
   }

   return(configrec) ;

} /* alloc_config_record */
 



/*==============================================================================
Function:	Config_Record * config_init(char *, char *, Names_Table_Entry *)

Description:
	This function obtains the values of the environment variables
listed in the passed Config_Names_Table.  The directory monitor variables
are as follows:

	the incoming directory name (SRC_DIR),
	the name of the source host of incoming files (SRC_HOST),
	the destination specification which may be a directory name (DEST_SPEC),
	the destination host name (DEST_HOST),
	the reception directory name (RECEPT_DIR),
	the log filename (LOG_FILE),
	the transfer mechanism (TRANSFER_CMD).

If (config_file is not NULL)
	Open and read variable values specified in config_file.
	Close config_file.
Endif

Allocate, fill and return the config record if possible.
      
If the config data cannot be obtained completely, 
	the ASF standard error logging routine is called.
	The monitor cannot proceed with its task, therefore an ERROR
	is issued and NULL is returned.  This prevents the
	monitor from attempting to perform its routing tasks.
Endif


Parameters:
	char *config_file - This file is used to obtain config
settings if it is not NULL, otherwise environment variables are used.

	Config_Names_Table - A config names table containing names of
config variables corresponding a specific flight agency.

Returns:	pointer to newly allocated and filled config record
		or NULL

Creator:	Norbert Piega	
Creation Date:	06/13/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
Config_Record *
config_init(char *config_file, char *log_file, 
	    Names_Table_Entry *Config_Names_Table)
#else
Config_Record *
config_init(config_file, log_file, Config_Names_Table)
   char *config_file ;
   char *log_file ;
   Names_Table_Entry *Config_Names_Table ;
#endif
{
   FILE *configfp ;                         /* Config file stream */
   char inline[MAXLINE+1] ;                 /* Input file line */
   char *tempstr = NULL ;                   /* temporary string */
   char *slash = NULL ;                     /* ptr to slash char */
   char *start ;                            /* ptr to start of input line */
   char *substr1, *substr2 ;                /* config file line substrings */
   char *expanded_substr2;
   Config_Record *config_rec ;              /* Config record */
   int getenv_status ;                      /* status of getting env value */


   /* Allocate config record */
   config_rec = alloc_config_record() ;
   if (config_rec == (Config_Record *) NULL)
   {
      syslog(LOG_ERR, "ERROR: Error allocating config record.\n") ;
      return(NULL) ;
   }

   /* Get FA_rootpath from $FAIF_ROOTPATH.  Quit if no good. */
   getenv_status =
        get_rootpath_from_env(Config_Names_Table, ROOT_PATH, config_rec) ;
   if (getenv_status == FALSE || getenv_status == ERROR)
   {
      syslog(LOG_ERR, "ERROR: $FAIF_ROOTPATH undefined.  Exiting.\n") ;
      free(config_rec) ;
      return(NULL) ;
   }

   /* Check config file */
   config_rec->FA_configfile = 
	 (char *)util_do_malloc(sizeof(char)*(strlen(config_file)+1)) ;
   strcpy(config_rec->FA_configfile, config_file) ;

   /* Open config file */
   if ((configfp = fopen(config_rec->FA_configfile, "r")) == (FILE *)NULL)
   {
      syslog(LOG_ERR, "ERROR: Unable to open config file %s.\n", 
	 config_rec->FA_configfile) ;
      return (NULL);
   }
   
   /* Take care of FA_logfile */
   if (log_file != (char *)NULL)     /* from command line argument */
   {
      config_rec->FA_logfile = 
	 (char *)util_do_malloc(sizeof(char)*(strlen(log_file)+1)) ;
      strcpy(config_rec->FA_logfile, log_file) ;
   }
   else
   {
      /* if not on command line, always use 
         $FAIF_ROOTPATH/xxx/log/xxxdirmon.log */
      if (get_config_default(Config_Names_Table, LOG_FILE, config_rec) == ERROR)
      {
	 free(config_rec) ;
         return(NULL) ;
      }
   }

      /* FA_rootpath and FA_logfile OK.  Now the rest. */
      /* Parse and process each input line in the config file */
      while (fgets(inline, MAXLINE, configfp) != NULL)
      {
         /* Skip blanks
         */
         start = inline ;
         while (isspace(*start))
            start++ ;
			    
         /* Skip BLANK and COMMENT line
         */
         if (*start == '\0' || *start == '#')
            continue ;
		
         substr1 = strtok(start, " ") ;
         substr2 = strtok(NULL, " \n") ;
 
         if (strchr(substr2, '$') != NULL)
         {
            expanded_substr2 = expand_path_val(substr2);
            substr2 = expanded_substr2; 
         }

         /* For the next statements,
         -- Test if 1st substring matches config variable name
         -- If matched, assign substring 2 to the value of the variable.
         */

         /* Reception directory name */
         if ((strcmp(substr1, 
            Config_Names_Table[RECEPT_DIR].name_identifier) == 0) &&
	    (strlen(Config_Names_Table[RECEPT_DIR].default_value) != 0))
         {
            /* Assign the real reception directory name.
            -- This is rootpath + reception dir.
            */
            config_rec->FA_receptdir = 
	       (char *)util_do_malloc(sizeof(char)*(
			  strlen(config_rec->FA_rootpath) +
                          strlen(substr2) +1 /*for slash*/ +1)) ;
            strcpy(config_rec->FA_receptdir, config_rec->FA_rootpath) ;
            strcat(config_rec->FA_receptdir, "/") ;
            strcat(config_rec->FA_receptdir, substr2) ;

            /* Check if reception dir is RW OK */
	    if (access(config_rec->FA_receptdir, R_OK) != 0 ||
		access(config_rec->FA_receptdir, W_OK) != 0)
            {
	       syslog(LOG_ERR,
		  "ERROR: Insufficient permissions for config file variable %s derived value %s (no read/write permission). \n",
		  substr1, config_rec->FA_receptdir) ;
	       free(config_rec->FA_receptdir) ;
	       config_rec->FA_receptdir = NULL ;
	    }
         }

         /* Source directory name */
         else if ((strcmp(substr1,
            Config_Names_Table[SRC_DIR].name_identifier) == 0) &&
	    (strlen(Config_Names_Table[SRC_DIR].default_value) != 0))
         {
            config_rec->FA_srcdir = 
               (char *)util_do_malloc(sizeof(char)*(strlen(substr2)+1)) ;
            strcpy(config_rec->FA_srcdir, substr2) ;
         }

         /* Destination directory name */
         else if ((strcmp(substr1,
            Config_Names_Table[DEST_DIR].name_identifier) == 0) &&
	    (strlen(Config_Names_Table[DEST_DIR].default_value) != 0))
         {
            config_rec->FA_destspec = 
               (char *)util_do_malloc(sizeof(char)*(strlen(substr2)+1)) ;
            strcpy(config_rec->FA_destspec, substr2) ;
         }

         /* Translated files directory name */
         else if ((strcmp(substr1,
            Config_Names_Table[TRANS_DIR].name_identifier) == 0) &&
	    (strlen(Config_Names_Table[TRANS_DIR].default_value) != 0))
         {
            config_rec->FA_transdir = 
               (char *)util_do_malloc(sizeof(char)*(strlen(substr2)+1)) ;
            strcpy(config_rec->FA_transdir, substr2) ;
         }

         /* Source host */
         else if ((strcmp(substr1,
            Config_Names_Table[SRC_HOST].name_identifier) == 0) &&
	    (strlen(Config_Names_Table[SRC_HOST].default_value) != 0))
         {
            config_rec->FA_srchost = 
               (char *)util_do_malloc(sizeof(char)*(strlen(substr2)+1)) ;
            strcpy(config_rec->FA_srchost, substr2) ;
         }

         /* Destination host */
         else if ((strcmp(substr1,
            Config_Names_Table[DEST_HOST].name_identifier) == 0) &&
	    (strlen(Config_Names_Table[DEST_HOST].default_value) != 0))
         {
            config_rec->FA_desthost = 
               (char *)util_do_malloc(sizeof(char)*(strlen(substr2)+1)) ;
            strcpy(config_rec->FA_desthost, substr2) ;
         }

         /* Transfer protocol */
         else if ((strcmp(substr1,
            Config_Names_Table[TRANSFER_PROTOCOL].name_identifier) == 0) &&
	    (strlen(Config_Names_Table[TRANSFER_PROTOCOL].default_value) != 0))
         {
            config_rec->FA_transfercmd = 
               (char *)util_do_malloc(sizeof(char)*(strlen(substr2)+1)) ;
            strcpy(config_rec->FA_transfercmd, substr2) ;
         }

      } /* Endwhile 2nd pass */ ;

      fclose(configfp) ;


   /* Use default table values for still-unassigned variables. */

   /* Incoming directory */
   if ((config_rec->FA_srcdir == (char *)NULL) &&
       (strlen(Config_Names_Table[SRC_DIR].default_value) != 0))
   {
	 if (get_config_default(Config_Names_Table, SRC_DIR, config_rec)
	     == ERROR)
         {
	    free(config_rec) ;
	    return(NULL) ;
	 }
   }

   /* Reception directory */
   if ((config_rec->FA_receptdir == (char *)NULL) &&
       (strlen(Config_Names_Table[RECEPT_DIR].default_value) != 0))
   {
	 if (get_config_default(Config_Names_Table, RECEPT_DIR, config_rec)
	     == ERROR)
         {
	    free(config_rec) ;
	    return(NULL) ;
	 }
   }

   /* Destination directory */
   if ((config_rec->FA_destspec == (char *)NULL) &&
       (strlen(Config_Names_Table[DEST_DIR].default_value) != 0))
   {
	 if (get_config_default(Config_Names_Table, DEST_DIR, config_rec)
	     == ERROR)
         {
	    free(config_rec) ;
	    return(NULL) ;
	 }
   }

   /* Translated files directory */
   if ((config_rec->FA_transdir == (char *)NULL) &&
       (strlen(Config_Names_Table[TRANS_DIR].default_value) != 0))
   {
	 if (get_config_default(Config_Names_Table, TRANS_DIR, config_rec)
	     == ERROR)
         {
	    free(config_rec) ;
	    return(NULL) ;
	 }
   }

   /* Source host  */
   if ((config_rec->FA_srchost == (char *)NULL) &&
       (strlen(Config_Names_Table[SRC_HOST].default_value) != 0))
   {
	 if (get_config_default(Config_Names_Table, SRC_HOST, config_rec)
	     == ERROR)
         {
	    free(config_rec) ;
	    return(NULL) ;
	 }
   }

   /* Destination host */
   if ((config_rec->FA_desthost == (char *)NULL) &&
       (strlen(Config_Names_Table[DEST_HOST].default_value) != 0))
   {
	 if (get_config_default(Config_Names_Table, DEST_HOST, config_rec)
	     == ERROR)
         {
	    free(config_rec) ;
	    return(NULL) ;
	 }
   }

   /* Transfer protocol */
   if ((config_rec->FA_transfercmd == (char *)NULL) &&
       (strlen(Config_Names_Table[TRANSFER_PROTOCOL].default_value) != 0))
   {
	 if (get_config_default(Config_Names_Table, TRANSFER_PROTOCOL,
			  config_rec) == ERROR) 
         {
	    free(config_rec) ;
	    return(NULL) ;
	 }
   }

   return(config_rec) ;

} /* config_init */





/*==============================================================================
Function:	static int get_rootpath_from_env(
		   Names_Table_Entry Config_Names_Table[], 
		   int index, 
		   Config_Record *config_rec)

Description:	
	This function obtains the name of the FAIF ROOTPATH variable from
a table and obtains the value of the variable via getenv.  The value
obtained is assigned to the appropriate config record field.  The returned
status is OK if an assignment of a value executed, otherwise ERROR is
returned.

Parameters:
	Names_Table_Entry Config_Names_Table[] - table containing names of
environment variables whose values will be obtained and assigned to
config record fields
	int index - index into name table pointing to entry in table
to obtain
	Config_Record *config_rec - config record to be filled with
information obtained from environment variable value

Returns:
	TRUE - config info obtained from environment successfully
	FALSE - config info cannot be obtained from environment
	ERROR - error encountered in obtaining config info

Creator:	Norbert Piega	
Creation Date:	08/05/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
static int
get_rootpath_from_env(Names_Table_Entry Config_Names_Table[],
	           int index, 
		   Config_Record *config_rec)
#else
static int
get_rootpath_from_env(Config_Names_Table, index, config_rec) 
   Names_Table_Entry Config_Names_Table[] ; 
   int index ; 
   Config_Record *config_rec ;
#endif
{
   char *config_var = NULL ;
   char *config_val = NULL ;
   char *tempstr = NULL ;
   char *slash = NULL ;

   config_var = Config_Names_Table[index].name_identifier ;
   config_val = (char *) getenv(config_var) ;
   if (config_val == (char *)NULL) return (FALSE);
   if (access(config_val, W_OK) != 0 || access(config_val, R_OK) != 0) 
      return(FALSE) ;
   config_rec->FA_rootpath = 
               (char *)util_do_malloc(sizeof(char)*(strlen(config_val)+1)) ;
   strcpy(config_rec->FA_rootpath, config_val) ;
   return(TRUE) ;

} /* get_rootpath_from_env */




/*==============================================================================
Function:	static int get_config_default(
		   Names_Table_Entry Config_Names_Table[], 
		   int index, 
		   Config_Record *config_rec)

Description:	
	This function obtains default config values from the config names
table Config_Names_Table and assigns the values in corresponding fields in
the config record.

Parameters:
	Names_Table_Entry Config_Names_Table[] - the table referenced for
default config values
	int index - index into config names table for entry to be referred to
	Config_Record *config_rec - record that will be assigned with
default information

Creator:	Norbert Piega	
Creation Date:	08/05/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
static int
get_config_default(Names_Table_Entry Config_Names_Table[], 
		   int index, 
		   Config_Record *config_rec)
#else
static int
get_config_default(Config_Names_Table, index, config_rec)
   Names_Table_Entry Config_Names_Table[] ; 
   int index ; 
   Config_Record *config_rec ;
#endif
{
   char *slash = NULL ;
   char *tempstr = NULL ;

   switch (index)
   {
      case LOG_FILE:
	 /* Length of default log filename is
         -- root path + logfile lengths (from default table) +
         -- 2 (slash and NULL chars)
         */
         config_rec->FA_logfile = 
               (char *)util_do_malloc(sizeof(char)*
		       (strlen(config_rec->FA_rootpath) +
		        strlen(Config_Names_Table[index].default_value)+2)) ;
         strcpy(config_rec->FA_logfile, config_rec->FA_rootpath) ;
         strcat(config_rec->FA_logfile, "/") ;
         strcat(config_rec->FA_logfile,
                Config_Names_Table[index].default_value) ;

         if (access(config_rec->FA_logfile, F_OK) == 0)
	 {
            if (access(config_rec->FA_logfile, W_OK) != 0)
	    {
               syslog(LOG_ERR, 
		  "ERROR: Insufficient permissions for default log file value %s (no writable)\n", 
		  config_rec->FA_logfile) ;
	       free(config_rec->FA_logfile) ;
	       config_rec->FA_logfile = NULL ;
	       return(ERROR) ;
	    }
	 }

	 tempstr = (char *)util_do_malloc(sizeof(char)*
	       (strlen(config_rec->FA_logfile)+1)) ;
         strcpy(tempstr, config_rec->FA_logfile) ;
         slash = strrchr(tempstr, '/') ;
         *slash = '\0' ;
         if (access(tempstr, W_OK) != 0)
         {
            syslog(LOG_ERR,
		  "ERROR: Insufficient permissions for default log file directory value %s (not writable)\n", 
		  tempstr) ;
	    free(config_rec->FA_logfile) ;
            config_rec->FA_logfile = NULL ;
            free(tempstr) ;
            return(ERROR) ;
         }
         free(tempstr) ;

	 break ;

#ifdef UNUSED
      case RECEPT_DIR:
         syslog(LOG_ERR, 
	      "WARNING: Reception directory unspecified. Using default %s/%s\n",
	       Config_Names_Table[ROOT_PATH].default_value,
	       Config_Names_Table[index].default_value) ;

	 /* Length of default reception directory is
	 -- root path + reception dir name lengths (from default table) +
	 -- 2 (slash and NULL chars)
	 */
         config_rec->FA_receptdir = 
            (char *)util_do_malloc(sizeof(char)*
		       (strlen(Config_Names_Table[ROOT_PATH].default_value)+
		        strlen(Config_Names_Table[index].default_value)+2)) ;
         strcpy(config_rec->FA_receptdir, 
	    Config_Names_Table[ROOT_PATH].default_value) ;
         strcat(config_rec->FA_receptdir, "/") ;
         strcat(config_rec->FA_receptdir, 
            Config_Names_Table[index].default_value) ;

         /* Check if reception dir is RW OK
	 */
	 if (access(config_rec->FA_receptdir, R_OK) != 0 ||
             access(config_rec->FA_receptdir, W_OK) != 0)
         {
            syslog(LOG_ERR,
               "ERROR: Insufficient permissions for default reception directory %s (no read/write permission). \n",
	       config_rec->FA_receptdir) ;
            free(config_rec->FA_receptdir) ;
            config_rec->FA_receptdir = NULL ;
	    return(ERROR) ;
	 }
	 break ;

      case SRC_DIR:
         syslog(LOG_ERR, 
	    "WARNING: Incoming directory unspecified. Using default %s\n",
	    Config_Names_Table[index].default_value) ;
         config_rec->FA_srcdir = 
            (char *)util_do_malloc(
		       sizeof(char)*
		       (strlen(Config_Names_Table[index].default_value)+1)) ;
         strcpy(config_rec->FA_srcdir,
	    Config_Names_Table[index].default_value) ;
	 break ;

      case DEST_DIR:
         syslog(LOG_ERR,
	    "WARNING: Destination directory unspecified. Using default %s\n",
	    Config_Names_Table[index].default_value) ;
         config_rec->FA_destspec = 
            (char *)util_do_malloc(
		       sizeof(char)*
		       (strlen(Config_Names_Table[index].default_value)+1)) ;
         strcpy(config_rec->FA_destspec,
	    Config_Names_Table[index].default_value) ;
	 break ;

      case TRANS_DIR:
         syslog(LOG_ERR, 
	    "WARNING: Translated files directory unspecified. Using default %s\n",
	    Config_Names_Table[index].default_value) ;
         config_rec->FA_transdir = 
            (char *)util_do_malloc(
		       sizeof(char)*
		       (strlen(Config_Names_Table[index].default_value)+1)) ;
         strcpy(config_rec->FA_transdir,
	    Config_Names_Table[index].default_value) ;
	 break ;
#endif

      case SRC_HOST:
         syslog(LOG_ERR, 
	    "WARNING: Source hostname unspecified. Using default %s\n",
	    Config_Names_Table[index].default_value) ;
         config_rec->FA_srchost = 
            (char *)util_do_malloc(
		       sizeof(char)*
		       (strlen(Config_Names_Table[index].default_value)+1)) ;
         strcpy(config_rec->FA_srchost,
	    Config_Names_Table[index].default_value) ;
	 break ;

#ifdef UNUSED
      case DEST_HOST:
         syslog(LOG_ERR, 
	    "WARNING: Destination hostname unspecified. Using default %s\n",
	    Config_Names_Table[index].default_value) ;
         config_rec->FA_desthost = 
            (char *)util_do_malloc(
		       sizeof(char)*
		       (strlen(Config_Names_Table[index].default_value)+1)) ;
         strcpy(config_rec->FA_desthost,
	    Config_Names_Table[index].default_value) ;
	 break ;
#endif

      case TRANSFER_PROTOCOL:
         syslog(LOG_ERR,
	    "WARNING: Transfer protocol unspecified.  Using default %s\n",
	    Config_Names_Table[index].default_value) ;
         config_rec->FA_transfercmd = 
            (char *)util_do_malloc(
		       sizeof(char)*
		       (strlen(Config_Names_Table[index].default_value)+1)) ;
         strcpy(config_rec->FA_transfercmd,
	    Config_Names_Table[index].default_value) ;
	 break ;

      default:
	 syslog(LOG_ERR, 
	    "ERROR: Invalid table index in variable table search\n") ;

         return(ERROR) ;

      } /* EndSwitch */

   return(OK) ;

} /* get_config_default */

/* End of File */
