#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.  U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       do_prcapsfile_main.c

Description:

External Functions Defined:
				main
	
File Scope Functions:
				process_files
				exec_processor
				set_fp_signals
				file_processing_sighandler
				do_files_cleanup
				create_cmd_string
				print_summary
				usage
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			perror() can't be used because it sends out it's arg in one
				print and then the ": <errno err msg>" in another, this
				comes out as two popups in the aps gui.

==============================================================================*/
#pragma ident	"@(#)do_prcapsfile_main.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.do_prcapsfile_main.c"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>

/* X header files needed for subprocess.h */
#include <X11/Intrinsic.h>

#include "dapps_defs.h"
#include "aps.h"
#include "aps_defs.h"
#include "aps_exe_names.h"
#include "mu_utilities.h"
#include "apspath.h"
#include "apsfiledef.h"

#include "subprocess.h"
#include "gui_defs.h"
#include "prcapsfile_main.h"

/* how to set all pertinent signals */
#define	FP_IGNORE_SIG		0
#define FP_HANDLE_SIG		1

#define FP_FILE_SEPARATORS	" 	"	/* space, tab */

#define MAX_NUM_CHILD_ARGS	20

/*==============================================================================
	Function Declarations
==============================================================================*/
static void		file_processing_sighandler( int signal );

/*==============================================================================
	Global Variable Declarations
==============================================================================*/
static char		*thisCmd;

static int		SignalCaught = 0;	/* signal number if signal'ed to stop */

static char		*infileRoot;
static char		*infileName ;
static FILE		*infileFp = NULL ;	/* if non-NULL, the fp of an open tmp file*/
static int		infileIndex = 0 ;	/* inbound reports index corr. to infileFp*/

static char		*userid = NULL;
static char		*userPasswd = NULL;
static char		*fileName = NULL ;
static char		*filePath = NULL ;

static int		numFilesOk=0;	/* num. of files that processed w/out error */
static int		numFilesErr=0;	/* num. of files that had errors in processing*/
static int		filesSkipped=FALSE ;	/* set TRUE if some files were skipped*/

static PERMREC	*permArray ;	/* malloc'ed array of active file proc. perms */
static int		numPerms ;


/*==============================================================================
Function:		usage()

Description:	Prints the usage message and exits.

Parameters:		

Returns:     	

Creator:		Teresa McKillop

Creation Date:	11/28/95

Notes:		
==============================================================================*/
static void
usage()
{
	(void)fprintf( stderr,
			"Usage: %s -U userid -P passwd [ -t <type> ] fileroot\n\n",
			thisCmd );
	exit( APS_EXIT_ERROR );
}



/*==============================================================================
Function:		print_summary()

Description:	Prints a summary of the processing accomplished.

Parameters:		

Returns:     	

Creator:		Teresa McKillop

Creation Date:	12/05/95

Notes:		
==============================================================================*/
static void
print_summary()
{
	/* print a summary of the finished processing */
	(void) printf( "\n%s: number of files successfully processed = %d\n",
		thisCmd, numFilesOk );
	(void) printf( "%s: number of files containing errors = %d\n",
		thisCmd, numFilesErr );
	if (filesSkipped == TRUE)
	{
		(void) printf( "%s: may have skipped processing of some files\n",
				thisCmd ) ;
	}
	(void) printf( "\n" ) ;

	return;
}



/*==============================================================================
Function:		set_fp_signals

Description:	Sets signals during file processing (this allows for the
				same set of signals to be manipulated throughout file proc.)

Parameters:		action	- FP_SIG_IGN, set to ignore signals
						  FP_SIG_HANDLER, set to use signal handler

Returns:     	

Creator:		Teresa McKillop

Creation Date:	11/17/95

Notes:		
==============================================================================*/
static void
set_fp_signals( int action )
{
	switch (action)
	{
		case FP_HANDLE_SIG:
			(void)signal( SIGINT, file_processing_sighandler );
			(void)signal( SIGTERM, file_processing_sighandler );
			(void)signal( SIGQUIT, file_processing_sighandler );
			break;
		default:
			(void)signal( SIGINT, SIG_IGN );
			(void)signal( SIGTERM, SIG_IGN );
			(void)signal( SIGQUIT, SIG_IGN );
			break;
	}
}


/*==============================================================================
Function:		file_processing_sighandler

Description:	Disables further interrupts and sets indicator that
				a signal was caught by setting the flag to the signal number.

Parameters:		

Returns:     	

Creator:		Teresa McKillop

Creation Date:	11/28/95

Notes:		
==============================================================================*/
static void
file_processing_sighandler( int signal )
{
	/* disable further interrupts */
	set_fp_signals( FP_IGNORE_SIG );

	SignalCaught = signal;

	if (filePath != NULL && fileName != NULL)
	{
		(void) printf( "*** %s (killed) ***: will stop after done with:\n    %s%s\n",
			thisCmd, filePath, fileName );
		(void) fflush( stdout ) ;
	}

	return;
}



/*==============================================================================
Function:		do_files_cleanup

Description:	called when file processing is terminating

				EXITs from here, doesn't return

Parameters:		

Returns:     	EXITs from here, doesn't return

Creator:		Teresa McKillop

Creation Date:	11/16/95

Notes:		
==============================================================================*/
static void
do_files_cleanup()
{
	char	infileBuf[FP_MAX_FILE_LINE+1];

	set_fp_signals( FP_IGNORE_SIG );	/* ignore interrupts */

	/*
	-- list on stdout names of all unprocessed files
	*/
	if (infileFp != NULL)	/* finish current open tmp file */
	{
		while (fgets( infileBuf, FP_MAX_FILE_LINE + 1, infileFp ) != NULL)
		{
			if ((fileName = strtok( infileBuf, FP_FILE_SEPARATORS )) != NULL &&
				(filePath = strtok(      NULL, FP_FILE_SEPARATORS )) != NULL)
			{
				(void) printf(
					"%s (Interrupted): this file not processed:\n    %s%s\n",
					thisCmd, filePath, fileName );
			}
		}

		(void) fclose( infileFp ) ;
		infileIndex++ ;
	}

	for ( ; reports_inbound[infileIndex].type != NULL ; infileIndex++)
	{
		(void) sprintf( infileName, "%s_%s", 
				infileRoot, reports_inbound[infileIndex].type ) ;
		if ((infileFp = fopen( infileName, "r" )) == NULL)
		{
			if (errno != ENOENT)
				filesSkipped = TRUE ;

			continue ;
		}

		while (fgets( infileBuf, FP_MAX_FILE_LINE + 1, infileFp ) != NULL)
		{
			if ((fileName = strtok( infileBuf, FP_FILE_SEPARATORS )) != NULL &&
				(filePath = strtok(      NULL, FP_FILE_SEPARATORS )) != NULL)
			{
				(void) printf(
					"%s (Interrupted): this file not processed:\n    %s%s\n",
					thisCmd, filePath, fileName );
			}
			else
				filesSkipped = TRUE ;
		}

		(void) fclose( infileFp ) ;
	}

	print_summary();

	if (!SignalCaught)
		exit( APS_EXIT_OK );
	else
		exit( SignalCaught );
}



/*==============================================================================
Function:		exec_processor

Description:	

Parameters:		

Returns:     	

Creator:		Teresa McKillop

Creation Date:	12/01/95

Notes:		
==============================================================================*/
static int
exec_processor( char *cmd )
{
	int		childStdin[2];	/* not used, but precludes getting keyboard input */
	int		childStdout[2];
	int		childStderr[2];
	int		childPid;
	char	buf[BUFSIZ];
	int		numRead;
	int		nfds;
	fd_set  readfds;
	int		setStdout = 1 ;	/* flag indicating if stdout should be "select"ed */
	int		setStderr = 1 ;	/* flag indicating if stderr should be "select"ed */
	int		needToKillChild = FALSE;	/* if get read err, finish loop & kill.
										 *		TRUE,  need to kill
										 *		FALSE, no need to kill
										 *		-TRUE, already killed
										 */
	int		childStatus;				/* exit status from the child */
	int		waitRet = -1 ;	/* init to an invalid pid */

	if (pipe( childStdin ) || pipe( childStdout ) || pipe( childStderr ))
	{
		(void) fprintf( stderr,
			"%s: Can't execute this file processor:\n    %s\n    Too many processes running? (errno = %d)\n",
			thisCmd, cmd, errno );
		return (ERROR);
	}

	/* flush, just in case, to be ready for child */
	(void) fflush(stdout) ;
	(void) fflush(stderr) ;

	if ((childPid = fork()) > 0)	/* parent */
	{
		(void) close( childStdin[READ_END]  );
		(void) close( childStdout[WRITE_END]  );
		(void) close( childStderr[WRITE_END]  );

		/* non-blocking read from sub-process */
		(void)fcntl( childStdout[READ_END], F_SETFL, O_NONBLOCK );
		(void)fcntl( childStderr[READ_END], F_SETFL, O_NONBLOCK );

		/* process child's stdout & stderr */
		FD_ZERO( &readfds );
		FD_SET( childStdout[READ_END], &readfds );
		FD_SET( childStderr[READ_END], &readfds );
		nfds = FD_SETSIZE;

		while (select( nfds, &readfds, NULL, NULL, NULL ) != 0)
		{
			if (FD_ISSET( childStdout[READ_END], &readfds ))
			{
				if ((numRead =
					read( childStdout[READ_END], buf, BUFSIZ - 1)) > 0)
				{
					buf[numRead] = STREND;
					/* echo print to stdout */
					(void) printf( "%s", buf );
				}
				else if (numRead == 0)	/* child has died */
				{
					if (waitRet != childPid)
					{
						while( (waitRet = wait( &childStatus )) != childPid )
							;	/* deliberate empty stmt*/
						childStatus = WEXITSTATUS( childStatus );
						(void) close( childStdin[WRITE_END] ) ;
					}
					(void) close( childStdout[READ_END] ) ;
					(void) fflush( stdout );
					setStdout = 0 ;	/* don't select stdout */
				}
				else if (errno != EAGAIN && needToKillChild == FALSE)
				{
					/* read "broke", can't continue */
					needToKillChild = TRUE;
				}
			}
			if (FD_ISSET( childStderr[READ_END], &readfds ))
			{
				if ((numRead =
					read( childStderr[READ_END], buf, BUFSIZ - 1)) > 0)
				{
					buf[numRead] = STREND;
					/* echo print to stdout */
					(void) fprintf( stderr, "%s", buf );
				}
				else if (numRead == 0)	/* child has died */
				{
					if (waitRet != childPid)
					{
						while( (waitRet = wait( &childStatus )) != childPid )
							;	/* deliberate empty stmt*/
						childStatus = WEXITSTATUS( childStatus );
						(void) close( childStdin[WRITE_END] ) ;
					}
					(void) close( childStderr[READ_END] ) ;
					(void) fflush( stderr );
					setStderr = 0 ;	/* don't select stderr */
				}
				else if (errno != EAGAIN && needToKillChild == FALSE)
				{
					/* read "broke", can't continue */
					needToKillChild = TRUE;
				}
			}
			if (needToKillChild == TRUE)	/* irrecoverable error occurred */
			{
				(void) kill( childPid, SIGKILL );	/* kill the process */
				needToKillChild = -TRUE ;		/* set so won't kill again */
			}

			if (!setStdout && !setStderr)
				break ;		/* out of "while" */

			FD_ZERO( &readfds );
			if (setStdout)
				FD_SET( childStdout[READ_END], &readfds );
			if (setStderr)
				FD_SET( childStderr[READ_END], &readfds );
		}
	}
	else
	{
		int		w, c;
		char	*cp;
		char	*argv[MAX_NUM_CHILD_ARGS + 1];	/* null terminating element */
		char	args[MAX_NUM_CHILD_ARGS][128];
		char	quotechar ;

		/* close the unused pipe ends */
		(void) close( childStdin[WRITE_END] );
		(void) close( childStdout[READ_END]  );
		(void) close( childStderr[READ_END]  );

		for (w=0, cp=cmd ; w < MAX_NUM_CHILD_ARGS && *cp ; w++ )
		{
		    while (*cp && *cp <= ' ')
				cp++ ;	/* skip whitespace between args */

			if (*cp == '"' || *cp == '\'')
			{
				quotechar = *cp ;
				/* args[w][0] = quotechar ; */
				cp++ ;
				c = 0 ;
				while (*cp)
				{
					args[w][c] = *cp ;
					if (*cp == quotechar)
					{
						/* c++ ; */ cp++ ;
		    		 	args[w][c] = STREND; 
		    			argv[w] = args[w];
						break ;
					}
					c++ ; cp++ ;
				}
				continue ;
			}
				
		    /* everything up to whitespace is an arg */
		    for (c = 0; c < 127 && ' ' < *cp  ; c++,cp++ )
			{
				args[w][c] = *cp;
		    }
		
		    args[w][c] = STREND;
		    argv[w] = args[w];
		}
		argv[w] = (char*)0;

		/* reattach the used ones to the stdio */
		(void) dup2( childStdin[READ_END],   STDIN_FILENO );  /* make stdin	*/
		(void) dup2( childStdout[WRITE_END], STDOUT_FILENO ); /* make stdout */
		(void) dup2( childStderr[WRITE_END], STDERR_FILENO ); /* make stderr */
		(void) close( childStdin[READ_END] ) ;
		(void) close( childStdout[WRITE_END] ) ;
		(void) close( childStderr[WRITE_END] ) ;

		/* make child ignore signals */
		set_fp_signals( FP_IGNORE_SIG );

		/* execvp( cmd, argv ) ; */
		(void) execvp( argv[0], argv ) ;
		(void) fprintf( stderr,
			"%s: Can't run program: %s\n    Possibly it's not in your path\n    (errno = %d)\n",
			thisCmd, cmd, errno );
		exit(SIGCHLD);
	}

	return (childStatus);
}



/*==============================================================================
Function:		process_files

Description:	

Parameters:		reportType	- the file type requested, or NULL for "all"
				infileRoot	- root name for each tmp file (type is appended)

Returns:     	

Creator:		Teresa McKillop

Creation Date:	12/01/95

Notes:		
==============================================================================*/
static void
process_files( char *reportType, char *infileRoot )
{

	char		cmd_string[FP_MAX_CMD_SIZE];
	char		fullFilename[FP_MAX_FILE_LINE] ;
	char		*fileProcessor;
	char		*fileType;
	char		infileBuf[FP_MAX_FILE_LINE+1];
	char		*bufPtr;
	char		*currType ;
	int			lastPerm = -1 ;	/* indx to last matched perm array entry */
	char		msg[BUFSIZ] ;
	int			retval ;
	int			j ;

	/* process the files listed in the tmp files */
	for (infileIndex = 0
		; (currType = reports_inbound[infileIndex].type) != NULL
		; infileIndex++)
	{
		if (SignalCaught)
			do_files_cleanup();	/* EXITs */

		if (!reportType
				|| strcmp( reportType, reports_inbound[infileIndex].type ) == 0)
		{
			int		tmpPerm ;

			(void) sprintf( infileName, "%s_%s", infileRoot, currType ) ;
			if ((infileFp = fopen( infileName, "r" )) == NULL)
			{
				if (errno != ENOENT)
				{
					(void) fprintf( stderr, "%s: Can't PROCESS %s files\n    open error on temp. input file:\n        %s\n    (errno = %d)\n",
							thisCmd, currType, infileName , errno ) ;
					/* have to skip this tmp file */
					filesSkipped = TRUE ;
				}
				continue ;
			}

			/* continue only if have permission for this report type */
			for (tmpPerm = (lastPerm + 1) % numPerms, j = 0 ; j != numPerms
					; tmpPerm = (tmpPerm + 1) % numPerms, j++)
			{
				if (strcmp( permArray[tmpPerm].type, currType ) == 0)
					break ;
			}
			if (j >= numPerms)	/* not found */
			{
				(void) fprintf( stderr, "%s: Don't have PERMISSION\n\n    **** SKIPPING **** file type %s",
						thisCmd, currType ) ;
				/* skip this tmp file */
				filesSkipped = TRUE ;
				(void) fclose( infileFp ) ;
				(void) unlink( infileName ) ;
				continue ;
			}
			/* validate the permission */
			if ((retval = mu_permission_validate(
					permArray[lastPerm = tmpPerm].id,
					reports_inbound[infileIndex].mu_activityid,
					MU_SINGLE_ACTIVITY_TYPE )) <= 0)
			{
				/* permission isn't valid or couldn't be validated, skip type */
				if (retval < 0)	/* mu lib error occurred */
					(void) sprintf( msg, "Permission validation ERROR:\n" ) ;
				else	/* invalid perm */
					(void) sprintf( msg, "Invalid permission ID\n" ) ;

				(void) fprintf( stderr,
						"%s\n    **** SKIPPING **** file type %s", msg, currType ) ;

				continue ;
			}

			/* permission is valid, process each file in the tmp file */
			while (fgets( infileBuf, FP_MAX_FILE_LINE + 1, infileFp ) != NULL)
			{
				if ((bufPtr =strrchr( infileBuf, NEWLINE )) != NULL)
					*bufPtr = STREND;

				if ((fileName      = strtok( infileBuf, FP_FILE_SEPARATORS ))
						== NULL ||
					(filePath      = strtok(      NULL, FP_FILE_SEPARATORS ))
						== NULL ||
					(fileProcessor = strtok(      NULL, FP_FILE_SEPARATORS ))
						== NULL ||
					(fileType      = strtok(      NULL, FP_FILE_SEPARATORS ))
						== NULL)
				{
					(void) fprintf( stderr, "%s: incorrect line (not processing any more %s files):\n    %s\n",
							thisCmd, currType, infileBuf );
					break ;
				}
				else
				{
					/* args: -U ... -P ... -t ... -p ... <filename> */
					(void) sprintf( cmd_string,
						"%s -U %s -P %s -t %s -p %d %s%s",
						fileProcessor, userid, userPasswd,
						fileType, permArray[lastPerm].id, filePath, fileName ) ;

					if (exec_processor( cmd_string ) == APS_EXIT_OK)
					{
						/* file processor exited normally */
						numFilesOk++;	/* increment ok files count */

						/* rm the processed file */
						(void) sprintf( fullFilename, "%s%s",
								filePath, fileName );
						(void) unlink( fullFilename );
					}
					else
					{
						/* file processor exited abnormally */
						numFilesErr++;	/* increment err files count */
						(void) fprintf( stderr, "%s: error processing file %s;\n    skipping subsequent %s files\n",
								thisCmd, fileName, currType );
						filesSkipped = TRUE ;
						break ;
					}
				}

				if (SignalCaught)
					do_files_cleanup();	/* EXITs */
			}

			(void) fclose( infileFp ) ;
			infileFp = NULL ;

			if (reportType)	/* did the requested type, DONE */
				break ;	/* out of for loop through reports_inbound */
		}
	}

	return;
}



main( int argc, char *argv[] )
{
	extern char		*optarg;
	extern int		optind;
	char			*optList = "U:P:t:";
	int				optChar;

	char		*reportType = NULL ;	/* NULL or the <type> argument */

	/* make stdout unbuffered i/o */ 
	setbuf( stdout, NULL ) ;

	/* parse the arguments and make sure have the required ones */

	thisCmd = argv[0];
	while((optChar = getopt( argc, argv, optList )) != EOF)
	{
		switch( optChar )
		{
			case 'U':
				userid = optarg;
				break;
			case 'P':
				userPasswd = optarg;
				break;
			case 't':
				reportType = optarg ;
				break ;
			default:
				usage();	/* EXITs */
				break;
		}
	}

	if (optind != argc - 1 || !userid || !userPasswd)
		usage();	/* EXITs */

	infileRoot = malloc( strlen( argv[optind] ) + 1 ) ;
	(void) strcpy( infileRoot, argv[optind] ) ;
	infileName = malloc( strlen( infileRoot ) + MAX_RPT_TYPE_LEN + 2 ) ;

	set_fp_signals( FP_HANDLE_SIG );

	permArray = get_active_permissions( thisCmd, &numPerms ) ;

	process_files( reportType, infileRoot );
	
	print_summary();

	return ( APS_EXIT_OK );
}
