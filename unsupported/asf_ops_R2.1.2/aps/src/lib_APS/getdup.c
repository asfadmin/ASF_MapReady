#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

#pragma ident	"@(#)getdup.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.getdup.c"

#include <stdlib.h>
#include <stdio.h>

/**********************************************************************
* Name : getdup.c
*
*
* Purpose: Obtains database name, userid, and password for the purpose 
*			of starting a database process.  
*
*			the database name is obtained from the environment 
*			variable:  APSDB
*
*			the userid is obtained from the command line: -U userid.  
*			if not found there, from the environment 
*			variable:  APS_SYBASE_USERID
*
*			the password is obtained from the command line:  -P password.
*
*  Input Parameters:
*  Name				Type	Description
*  argc				int		number of arguments in the command line.  
*  argv				*char*const	the values of the arguments.
*  extra_flags		*char	listof extra flags for call to getopts.  
*							if flag has a required values after it, follow 
*							it with a colon.  for example, if s is a flag 
*							and t requires a value after it, use:  "st:"
*
*  Output Parameters:
*  Name				Type		Description
*  dbname			*char	name of the database
*  sybase_userid	*char	Sybase-approved userid.
*  password			*char	password for userid.  
*
*  Return Parameter:
*  Type			Description
*  int			0	no errors.
*				1	error in command line; missing argument(s) etc.  
*				2	no value for APS_SYBASE_USERID in environment.
*				3	no value for APSDB in environment.
*
******************************************************************************
* 
*  Usage example:  	
*		extern int getdup( int argc, char **argv, char	
*			*extra_flags, char *dbname, char *sybase_userid, char *password );
*		.
*		.
*		.
*		err_code = getdup(argc, argv, "srt:", dbname, sybase_userid, password);
*		if(err_code == 1)
*			usage_exit(1);
*		if(err_code == 2)
*			printf("no value for env APS_SYBASE_USERID\n");
*		if(err_code == 3)
*			printf("no value for env APSDB\n");
*		if(err_code != 0)
*			exit(1);
*
******************************************************************************
*
*  Modification History:
*  Author	Revision	Date
*
****************************************************************************/
int getdup(
	int 	argc, 
	char 	*argv[],
	char	*extra_flags,
	char	*dbname,
	char	*sybase_userid,
	char	*password)
{
int 	rcode;

/* get the sybase_userid (optional) and password (mandatory)	*/

/* int 	getopt(int argc, char * const *argv, char *optstring);	*/

extern	char 	*optarg;
extern	int 	optind, opterr, optopt;
/*  extern	char*	getenv(char*);	*/

int		c;		/* used as return character from getopt()		*/
int		Pflag = 0;	/* used to check for mandatory password		*/
int		Uflag = 0;	/* used to check for optional sybase_userid	*/
char	*env_dbname;		/* dbname from environment 		*/
char	*env_sybase_userid;	/* userid from environment 		*/
char	*cmd_sybase_userid;	/* userid from command line.	*/
char	flag_list[100] = "P:U:";

strcat(flag_list, extra_flags);

while ((c = getopt(argc, argv, flag_list)) != EOF)
	switch (c)
	{
		case 'P':
			if(Pflag != 0)
				return 1;
			Pflag++;
			strcpy(password, optarg);
			break;
		case 'U':
			if(Uflag != 0)
				return 1;
			Uflag++;
			strcpy(sybase_userid, optarg);
			break;
		case '?':
			break;
		default:
			/* do  nothing; let calling routine deal with these. */
			break;
	}

if(Pflag <= 0)
	return 1;

if(Uflag == 0)
{
	/* sybase_userid not supplied in command line.  	*/
	env_sybase_userid = getenv("APS_SYBASE_USERID");
	if(env_sybase_userid == NULL)
		/* userid not supplied at all	*/
		return 2;
	else
		/* use the environment sybase_userid.	*/
		strcpy(sybase_userid, env_sybase_userid);
}

env_dbname = getenv("APSDB");
if(env_dbname == NULL)
	/* database name not supplied 	*/
	return 3;

strcpy(dbname, env_dbname);

return 0;
}
