
/* Exec the user program.  This is done based on the way this
*  program was invoked.  We look in a configuration file for
*  a line matching argv[0] and exec that.
*
*  ChangeLog: Change path of ims_telnetd.conf file - mmk.011695
*/

#include <stdio.h>
#define BUFSZ 10240		/* maximum size of a line in the config file */

void do_exec (myname, config_file, environ, host)
	char	*myname;	/* name this program was invoked with (argv[0]) */
	char	*config_file;	/* name of the configuration file */
	char	**environ;	/* environment to pass to the new program */
	char	*host;		/* host name of remote */
	{
	FILE	*config;	/* configuration file */
	char	linebuf[BUFSZ];	/* line from the config file */
	int	sz;		/* size of the line in linebuf */
	char	*token;		/* token from linebuf */
	char	**argv;		/* arguments to program */
	int	argc = 10;	/* number of arguments */
	int	i;		/* loop index */
	char	*progname = 0;	/* name of program to exec */
	char	*delimiters = " \t\n";	/* white space for strtok */
	char	*strtok();
	char	*malloc();

	/*
	 *	Look through the configuration file for a matching line.
	 */
	config = fopen(config_file, "r");
	if (config == NULL)
		{
		printf("cannot open %s\n", config_file);
		return;
		}
	while (fgets(linebuf, BUFSZ, config))
		{
		sz = strlen(linebuf);
		linebuf[--sz] = '\0';	/* kill the linefeed */
		while (linebuf[sz-1] == '\\')
			{
			/*
			 *	Continuation line.
			 */
			sz--;	/* kill the \ */
			fgets(&linebuf[sz], BUFSZ-sz, config);
			sz = strlen(linebuf);
			linebuf[--sz] = '\0';	/* kill the linefeed */
			if (sz >= BUFSZ-1)
				break;	/* line too long */
			}
		token = strtok(linebuf, delimiters);
		if (token && (strcmp(token, myname) == 0))
			{
			progname = strtok(0, delimiters);
			break;
			}
		}
	fclose(config);
	if (!progname)
		{
		printf("cannot find %s in %s\n", myname, config_file);
		return;		/* didn't find a match */
		}

	/*
	 *	We found a match.  Parse the rest of the line.
	 */
	argv = (char **)malloc(argc * sizeof(char *));
	if (!argv)
		{
		printf("malloc failed\n");
		return;
		}
	for (i=0; token = strtok(0, delimiters); i++)
		{
		if (i >= argc)
			{
			argc = argc * 2;
			argv = (char **)realloc(argv, argc * sizeof(char *));
			}
		/*
		 *	If the token is the literal "host", then put
		 *	in the remote host name.  If you really want "host"
		 *	you are out of luck, unless you modify
		 *	this to look for "%host" or some such...
		 */
		if (strcmp(token, "host") == 0)
			argv[i] = host;
		else
			argv[i] = token;
		}
	argv[i] = (char *)0;

	execve(progname, argv, environ);
	}
