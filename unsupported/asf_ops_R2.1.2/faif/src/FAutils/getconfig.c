/*==============================================================================
Filename:	getconfig.c

Description:	
	This contains the function which obtains the value of a
config variable from a config file or the Unix environment.

External Functions:
	get_config_val
        expand_path_val
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
1.  June '96 - R. Hoffman
    expand_path_val() function for use here and elsewhere
==============================================================================*/

static char SccsFile[] = "getconfig.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "12 Jun 1996";
static char SccsLastChanger[] = "@(#)getconfig.c	1.3";
static char SccsState[] = "1.3";

#include <stdlib.h>
#include <stdio.h>
#include <syslog.h>
#include "faifdefs.h"



/*==============================================================================
Function:	char *expand_path_val(config_str)

Description:	
        Expands an environment variable within path config_str.  
        Assumes config_str is a path string possibly containing one 
environment variable.  If an environment variable is present, config_str
must contain at least a slash after it.  Also, if the environment variable
is not defined, stop everything.

Parameters:
	char *config_str - the config value string to expand. 

Returns:	char * value of fully-expanded path string
Creator:	Rich Norman	
Creation Date:	June 1996
Notes:		
==============================================================================*/
#ifdef __STDC__
char *
expand_path_val(char *config_str)
#else
char *
expand_path_val(config_var)
   char *config_str ;
#endif
{

  char *env_full_str = (char *)NULL;
  char env_str[MAXLINE+1];
  char *expanded_config_str = (char *)NULL;
  char *slash_ptr, *dollar_ptr;
  int  env_str_len;

  /*
  Check for presence of an environment variable name, delimited
  by a $ and a /. The env_full_str is initially set to null so
  that we can later decide how to piece together the output value
  of config_str.
  */

  dollar_ptr = (char *)strchr(config_str, '$');

  /*
  First, check if an environment name is present. If there is,
  find each end of it
  */

  if (dollar_ptr != (char *) NULL)
  {
     slash_ptr = (char *)strchr(dollar_ptr, '/');
     env_str_len = slash_ptr - dollar_ptr - 1;
     strncpy(&env_str, dollar_ptr + 1, env_str_len);
     env_str[env_str_len] = '\0';
     env_full_str = (char *)getenv(env_str);
     if (env_full_str == (char *)NULL)
     {
        syslog(LOG_ERR, "%s in config file, but not in environment.", env_str) ;
        exit(-1);
     }
  }
               
  /*  Construct fully-expanded config_str. */
 
  if (env_full_str != (char *)NULL)
  {
     expanded_config_str = (char *)
	util_do_malloc(sizeof(char)*(strlen(env_full_str) + 1
           + strlen(config_str) + 1
           - (slash_ptr - dollar_ptr + 1))) ;
     strcpy(expanded_config_str, env_full_str);
     strcat(expanded_config_str, slash_ptr);
  }
  else
  {
     expanded_config_str = (char *)
	util_do_malloc(sizeof(char)*(strlen(config_str)+1)) ;
     strcpy(expanded_config_str, config_str);
  }

  return (expanded_config_str) ;
}



/*==============================================================================
Function:	char *get_config_val(config_file, config_var)

Description:	
	Gets the value of the config variable config_var either from
the environment variable value or through the config_file.  Note that
the config file is checked first.

Parameters:
	char *config_file - name of the config file
	char *config_var - name of the config variable whose value
is to be obtained

Returns:	char * value of config variable
Creator:	Norbert Piega	
Creation Date:	10/05/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
char *
get_config_val(char *config_file, char *config_var)
#else
char *
get_config_val(config_file, config_var)
   char *config_file ;
   char *config_var ;
#endif
{
   FILE *configfp ;                                   /* Config file stream */
   char inline[MAXLINE+1] ;                              /* Input file line */
   char *start ;                              /* ptr to start of input line */
   char *substr1, *substr2 ;                 /* config file line substrings */
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;               /* Error log msg string */
   char *config_val = NULL ;
   char *ev_config_val = NULL ;

   if (config_file != (char *)NULL)
      if ((configfp = fopen(config_file, "r")) == (FILE *)NULL)
      {
         sprintf(logmsg, 
	    "WARNING, Unable to open config file %s\n", config_file) ;
         syslog(LOG_ERR, logmsg) ;
      }
      else
      {
         /* Parse and process each input line in the config file
         */
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
	 	
            substr1 = (char *)strtok(start, " ") ;
            substr2 = (char *)strtok(NULL, " \n") ;
 
            /* Test if 1st substring matches config variable name
            -- If matched, assign substring 2 to the value of the variable.
            */

            if (strcmp(substr1, config_var) == 0)
            {

               if (strchr(substr2, '$') != NULL)
                   config_val = expand_path_val(substr2);
               else
	       {
                  config_val = (char *)
                     util_do_malloc(sizeof(char)*(strlen(substr2)+1)) ;
                  strcpy(config_val, substr2);
	       }

	       break ;
            }
         } /* Endwhile */

	 fclose(configfp) ;
      }

   if (config_val == (char *)NULL)
   {
      ev_config_val = getenv(config_var) ;
      if (ev_config_val != (char *)NULL)
      {
	 config_val = 
	    (char *)util_do_malloc(sizeof(char)*(strlen(ev_config_val)+1)) ;
         strcpy(config_val, ev_config_val) ;
      }
   }

   return(config_val) ;

} /* get_config_val */

/* End of File */




