/*==============================================================================
Configname:	ftconfig.c

Description:
	This module contains the functions for obtaining file type
specific config info.

External Functions:
	ftconfig_init
	
Static Functions:
	get_ftconfig_default
	
External Variables Defined:
	None
	
Config Scope Static Variables:
	None
	
Notes:
1.  June '96 - R. Hoffman
    Call expand_path_val() to handle environment variables in path strings
    within the config file.
2.  July '97 - R. Hoffman
    Check only one time, early, that FA_rootpath is assigned.
    Get rid of get_ftconfig_env()
    Clean up error messages.
==============================================================================*/

static char SccsConfig[] = "ftconfig.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "12 Jun 1996";
static char SccsLastChanger[] = "@(#)ftconfig.c	1.2";
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
int        ftconfig_init(Names_Table_Entry *, Names_Table_Entry *, 
			 Names_Table_Entry *, Names_Table_Entry *,
			 int, Config_Record *) ;
static int get_ftconfig_default(Names_Table_Entry *, 
				  int, Config_Record *, int) ;
#else
int        ftconfig_init() ;
static int get_ftconfig_default() ;
#endif

extern void *util_do_malloc() ;
extern char *expand_path_val();


/*==============================================================================
Function:	int ftconfig_init(Names_Table_Entry *, Names_Table_Entry *,
                                  Names_Table_Entry *, Names_Table_Entry *,
				  int, Config_Record *)

Description:
	This function obtains the values of the config variables
for a specific file agency file type.  The variable names and default 
values are stored in the 4 tables (Source directory table, Reception 
directory table, Destination directory table and Translation directory
table).  For instance, to obtain the config values for CSA_PREDORBIT 
files, the variable name for source directory in the Source Directory 
table is obtained.  The variable name is searched in the config file if 
a config file is available otherwise the default value in the 
Source Directory table is used.  Note that this same algorithm is used 
for the Reception and Destination directory value as well.  The config 
value obtained is stored in the specified config record.
      
If the config data cannot be obtained, 
	the ASF standard error logging routine is called and
	ERROR is returned
Endif


Parameters:
	SrcDir_Table - A table containing names of config variables 
for source directory names per file type  

	ReceptDir_Table - A table containing names of config variables 
for reception directory names per file type  

	DestDir_Table - A table containing names of config variables 
for destination directory names per file type  

	TransDir_Table - A table containing names of config variables 
for destination directory names per file type  

	int index - index into the tables for the file type
whose config variable is to be obtained

	Config_Record * - pointer to config record where obtained
config info are stored

Returns:	OK, ERROR
Creator:	Norbert Piega	
Creation Date:	06/13/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
ftconfig_init(Names_Table_Entry *SrcDir_Table,
               Names_Table_Entry *ReceptDir_Table, 
               Names_Table_Entry *DestDir_Table, 
               Names_Table_Entry *TransDir_Table, 
               int index,
	       Config_Record *config_rec)
#else
int
ftconfig_init(SrcDir_Table, ReceptDir_Table, DestDir_Table, TransDir_Table,
	      index, config_rec)
   Names_Table_Entry *SrcDir_Table ;
   Names_Table_Entry *ReceptDir_Table ;
   Names_Table_Entry *DestDir_Table ;
   Names_Table_Entry *TransDir_Table ;
   int index ;
   Config_Record *config_rec ;
#endif
{
   FILE *configfp ;                         /* Config file stream */
   char inline[MAXLINE+1] ;                 /* Input file line */
   char *start ;                            /* ptr to start of input line */
   char *substr1, *substr2 ;                /* config file line substrings */
   char *expanded_substr2;
   int check_transdir = FALSE ;             /* if transdir must be checked */
   int i ;

   if (config_rec == (Config_Record *) NULL)
   {
      syslog(LOG_ERR, "ERROR: NULL config record\n") ;
      return(ERROR) ;
   }
   
   if (config_rec->FA_rootpath == (char *)NULL)
   {
      syslog(LOG_ERR, "ERROR: Unassigned rootpath. \n") ;
      return(ERROR) ;
   }

   /* Re-initialize Source and Reception directories.
   -- This procedure overrides any previous assignments.
   */
   if (config_rec->FA_srcdir != (char *)NULL)
   {
      free(config_rec->FA_srcdir) ;
      config_rec->FA_srcdir = NULL ;
   }
   if (config_rec->FA_receptdir != (char *)NULL)
   {
      free(config_rec->FA_receptdir) ;
      config_rec->FA_receptdir = NULL ;
   }
   if (config_rec->FA_destspec != (char *)NULL)
   {
      free(config_rec->FA_destspec) ;
      config_rec->FA_destspec = NULL ;
   }

   if (TransDir_Table != NULL)
   {
      for (i=0; TransDir_Table[i].name_id != SENTINEL; i++)
         NULL ; 
 
      if (index < i)
         check_transdir = TRUE ;
 
      if (check_transdir == TRUE)
         if (config_rec->FA_transdir != (char *)NULL)
         {
            free(config_rec->FA_transdir) ;
            config_rec->FA_transdir = NULL ;
         }
   }

   /* Check config file */
   if (config_rec->FA_configfile == (char *)NULL) 
   {
      syslog(LOG_ERR, "ERROR: Missing config file. \n");
      return (ERROR);
   }

   if (access(config_rec->FA_configfile, R_OK) != 0)
   {
      syslog(LOG_ERR, "ERROR: Unable to read config file %s. \n", 
         config_rec->FA_configfile) ;
      return (ERROR);
   }

   if ((configfp = fopen(config_rec->FA_configfile, "r")) == (FILE *)NULL)
   {
      syslog(LOG_ERR, "ERROR: Unable to open config file %s. \n", 
	 config_rec->FA_configfile) ;
      return (ERROR);
   }
   
   /* Parse and process each input line in the config file.
      -- Look for the right variable 
   */

   while (fgets(inline, MAXLINE, configfp) != NULL)
   {
      /* Skip blanks */
      start = inline ;
      while (isspace(*start)) start++ ;
			    
      /* Skip BLANK and COMMENT line */
      if (*start == '\0' || *start == '#') continue ;
		
      substr1 = strtok(start, " ") ;
      substr2 = strtok(NULL, " \n") ;

      if (strchr(substr2, '$') != NULL)
      {
         expanded_substr2 = expand_path_val(substr2);
         substr2 = expanded_substr2; 
      }
 
      /* Test if 1st substring matches config variable name 
	 -- If matched, assign substring 2 to the variable value
      */

      /* Source Directory */
      if (SrcDir_Table != NULL)
      {
         if ((strcmp(substr1, SrcDir_Table[index].name_identifier) == 0) &&
             (strlen(SrcDir_Table[index].default_value) != 0))
         {
            config_rec->FA_srcdir =
                 (char *)util_do_malloc(sizeof(char)*(strlen(substr2)+1)) ;
            strcpy(config_rec->FA_srcdir, substr2) ;
         }
      }
 
      /* Destination Directory */
      if (strcmp(substr1, DestDir_Table[index].name_identifier) == 0)
      {
         config_rec->FA_destspec =
            (char *)util_do_malloc(sizeof(char)*(strlen(substr2)+1)) ;
         strcpy(config_rec->FA_destspec, substr2) ;
      }

      /* Reception directory name */
      if (strcmp(substr1, ReceptDir_Table[index].name_identifier) == 0)
      {
         /* Assign the real reception directory name.
            -- This is rootpath + reception dir.
         */
         config_rec->FA_receptdir = 
	    (char *)util_do_malloc(strlen(config_rec->FA_rootpath) +
            strlen(substr2) +1 /*for slash*/ +1) ;
         strcpy(config_rec->FA_receptdir, config_rec->FA_rootpath) ;
         strcat(config_rec->FA_receptdir, "/") ;
         strcat(config_rec->FA_receptdir, substr2) ;

         /* Check reception directory access permissions */
	 if (access(config_rec->FA_receptdir, R_OK) != 0 ||
             access(config_rec->FA_receptdir, W_OK) != 0)
         {
	    syslog(LOG_ERR, 
		  "ERROR: Insufficient permissions (read,write) for config file variable %s derived value %s. \n",
		  substr1, config_rec->FA_receptdir) ;
	    free(config_rec->FA_receptdir) ;
	    config_rec->FA_receptdir = NULL ;
	 }

      } /* Recept dir */

      /* Translation Directory */
      if (check_transdir == TRUE)
         if ((strcmp(substr1, TransDir_Table[index].name_identifier) == 0) &&
             (strlen(TransDir_Table[index].default_value) != 0))
         {
            config_rec->FA_transdir = 
               (char *)util_do_malloc(sizeof(char)*(strlen(substr2)+1)) ;
            strcpy(config_rec->FA_transdir, substr2) ;
         }

   } /* Endwhile file parse */ ;

   fclose(configfp) ;

   /* Use default table values for all unassigned variables. */
   /* Source directory */
   if (SrcDir_Table != NULL)
   {
      if ((config_rec->FA_srcdir == (char *)NULL) &&
          (strlen(SrcDir_Table[index].default_value) != 0))
      {
         get_ftconfig_default(SrcDir_Table, index, config_rec, SRC_DIR) ;
      }
   }

   /* Reception directory */
   if ((config_rec->FA_receptdir == (char *)NULL) &&
       (strlen(ReceptDir_Table[index].default_value) != 0))
   {
	 if (get_ftconfig_default(ReceptDir_Table, index, config_rec, 
	     RECEPT_DIR) == ERROR)
         {
            syslog(LOG_ERR, 
	       "ERROR: Unable to obtain a valid reception directory name\n") ;
	    return(ERROR) ;
	 }
   }

   /* Destination directory */
   if ((config_rec->FA_destspec == (char *)NULL) &&
       (strlen(DestDir_Table[index].default_value) != 0))
   {
         get_ftconfig_default(DestDir_Table, index, config_rec, DEST_DIR) ;
   }

   /* Translation Directory */
   if (check_transdir == TRUE)
      if ((config_rec->FA_transdir == (char *)NULL) &&
          (strlen(TransDir_Table[index].default_value) != 0))
      {
            get_ftconfig_default(TransDir_Table, index, config_rec, 
               TRANS_DIR) ;
      }

   return(OK) ;

} /* ftconfig_init */




/*==============================================================================
Function:	static int get_ftconfig_default(
		   Names_Table_Entry Config_Names_Table[], 
		   int index, 
		   Config_Record *config_rec,
		   int var_id)

Description:	
	This function obtains the default config value for the config 
variable specified by var_id.  The variable and corresponding value are
obtained from the table Config_Names_Table indexed by index.  The value
obtained is assigned to the appropriate field in the config record.

Parameters:
	Names_Table_Entry Config_Names_Table[] - the table referenced for
default config values
	int index - index into config names table for entry to be referred to
	Config_Record *config_rec - record that will be assigned with
default information
	int var_id - specifies which config item to obtain

Returns:	OK, ERROR
Creator:	Norbert Piega	
Creation Date:	08/05/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
static int
get_ftconfig_default(Names_Table_Entry Config_Names_Table[], 
		   int index, 
		   Config_Record *config_rec,
		   int var_id)
#else
static int
get_ftconfig_default(Config_Names_Table, index, config_rec, var_id)
   Names_Table_Entry Config_Names_Table[] ; 
   int index ; 
   Config_Record *config_rec ;
   int var_id ;
#endif
{

   switch (var_id)
   {
      case RECEPT_DIR:
         syslog(LOG_WARNING,
           "WARNING, Reception directory unspecified. Using default %s/%s\n",
             config_rec->FA_rootpath, 
             Config_Names_Table[index].default_value) ;

	 /* Length of default log filename is
         -- root path + logfile lengths (from default table) +
         -- 2 (slash and NULL chars)
         */
         config_rec->FA_receptdir = 
            (char *)util_do_malloc(sizeof(char)*
		      (strlen(config_rec->FA_rootpath) +
		       strlen(Config_Names_Table[index].default_value)+2)) ;
         strcpy(config_rec->FA_receptdir, config_rec->FA_rootpath) ;
         strcat(config_rec->FA_receptdir, "/") ;
         strcat(config_rec->FA_receptdir, 
	 Config_Names_Table[index].default_value) ;

         /* Check reception directory access permissions */
         if (access(config_rec->FA_receptdir, R_OK) != 0 ||
     	     access(config_rec->FA_receptdir, W_OK) != 0)
         {
            syslog(LOG_ERR,
	       "ERROR: Insufficient permissions (read,write) for \
	       \ndefault reception directory value %s\n",
	        config_rec->FA_receptdir) ;
	    free(config_rec->FA_receptdir) ;
            config_rec->FA_receptdir = NULL ;
            return(ERROR) ;
         }
	 break ;

      case SRC_DIR:
         syslog(LOG_WARNING, 
	    "WARNING, Incoming dir name unspecified. Using default value %s\n",
	    Config_Names_Table[index].default_value) ;
         config_rec->FA_srcdir = 
            (char *)util_do_malloc(
		       sizeof(char)*
		       (strlen(Config_Names_Table[index].default_value)+1)) ;
         strcpy(config_rec->FA_srcdir,
	    Config_Names_Table[index].default_value) ;
	 break ;

      case DEST_DIR:
         syslog(LOG_WARNING, 
	    "WARNING, Destination directory unspecified. Using default %s\n",
	    Config_Names_Table[index].default_value) ;
         config_rec->FA_destspec = 
            (char *)util_do_malloc(
		       sizeof(char)*
		       (strlen(Config_Names_Table[index].default_value)+1)) ;
         strcpy(config_rec->FA_destspec,
	    Config_Names_Table[index].default_value) ;
	 break ;

      case TRANS_DIR:
         syslog(LOG_WARNING, 
	  "WARNING, Translated files directory unspecified. Using default %s\n",
	    Config_Names_Table[index].default_value) ;
         config_rec->FA_transdir = 
            (char *)util_do_malloc(
		       sizeof(char)*
		       (strlen(Config_Names_Table[index].default_value)+1)) ;
         strcpy(config_rec->FA_transdir,
	    Config_Names_Table[index].default_value) ;
	 break ;

      default:
	 syslog(LOG_ERR, "ERROR: Config variable table search error\n") ;
         return(ERROR) ;

   } /* EndSwitch */

   return(OK) ;

} /* get_ftconfig_default */

/* End of File */
