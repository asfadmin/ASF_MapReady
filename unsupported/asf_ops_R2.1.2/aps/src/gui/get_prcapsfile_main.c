#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.  U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       get_prcapsfile_main.c

Description:	Usage: get_prcapsfile_main [ -t <type> ] filename

				Uses the info in the reports_inbound table to find
				the files that are ready for file processing.
				Writes the found filenames and their required exe processor
				name to filename.

External Functions Defined:
				main
	
File Scope Functions:
				usage
				set_fp_signals
				file_processing_sighandler
				get_matching_files
				get_matching_read_proc
				get_files_cleanup
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			perror() can't be used because it sends out it's arg in one
				print and then the ": <errno err msg>" in another, this
				comes out as two popups in the aps gui.

==============================================================================*/
#pragma ident	"@(#)get_prcapsfile_main.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.get_prcapsfile_main.c"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>

#include <Xm/Xm.h>	/* Xm hdr: needed for XmDIALOG_ERROR */
/* X header files needed for subprocess.h */
#include <X11/Intrinsic.h>

#include "dapps_defs.h"
#include "aps.h"
#include "aps_defs.h"
#include "aps_exe_names.h"
#include "aps_extern.h"
#include "mu_utilities.h"
#include "apspath.h"
#include "apsfiledef.h"

#include "subprocess.h"
#include "gui_defs.h"
#include "gui_utils.h"
#include "prcapsfile_main.h"

/* at what point in file processing signals are being set */
#define	FP_SIG_IGN			0
#define FP_SIG_HANDLER		1

#define MAX_NUM_CHILD_ARGS	20

/*==============================================================================
	Function Declarations
==============================================================================*/
static void		file_processing_sighandler( int signal );

/*==============================================================================
	Global Variable Declarations
==============================================================================*/
static char		*thisCmd;
static char		*reportType = NULL ;	/* NULL or the <type> argument */
static int		SignalCaught = 0;	/* signal number if signal'ed to stop */

static char		*outFile=NULL ;
static char		*outRoot=NULL;
static int		outFd=-1;		

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
usage( char *cmd )
{
	(void) fprintf( stderr, "Usage: %s [ -t <type> ] fileroot\n\n", cmd );
	exit( APS_EXIT_ERROR );
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
		case FP_SIG_HANDLER:
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
	set_fp_signals( FP_SIG_IGN );

	SignalCaught = signal;

	return;
}



/*==============================================================================
Function:		get_files_cleanup

Description:	performs cleanup and exit

Parameters:		

Returns:     	

Creator:		Teresa McKillop

Creation Date:	11/30/95

Notes:		
==============================================================================*/
static void
get_files_cleanup( int signal )
{
	int		i ;

	/* disable interrupts */
	set_fp_signals( FP_SIG_IGN );

	if (signal)	/* interrupted */
	{
		/* close any currently open tmp output file */
		if (outFd >= 0)
		{
			(void) close( outFd );
		}
		/* remove the tmp output files */
		if (reportType != NULL)
		{
			(void) sprintf( outFile, "%s_%s", outRoot, reportType ) ;
			(void) unlink( outFile );
		}
		else
		{
			for (i = 0 ; reports_inbound[i].type != NULL ; i++)
			{
				(void) sprintf( outFile,
						"%s_%s", outRoot, reports_inbound[i].type ) ;
				(void) unlink( outFile );
			}
		}
	}

	exit( signal );
}



/*==============================================================================
Function:		get_matching_read_proc

Description:	gets the names of the unprocessed files and writes the
				pertinent info to the tmp output files.

Parameters:		buf		buffer containing the stream data read in from
						the child process.

Returns:     	

Creator:		Teresa McKillop

Creation Date:	11/28/95

Notes:			
==============================================================================*/
static void
get_matching_read_proc( REPORT *filetypeInfo, char buf[] )
{
	static char	fullFilename[FP_MAX_FILE_LINE+1];	/* storage for current */
	char		*pathComponent;
	char		*fnameComponent;
	char		*bufStartPtr = buf;		/* ptr to start of full fname in buf */
	char		*bufEndPtr;		/* ptr to end of full filename str in buf */
	char		outLine[FP_MAX_FILE_LINE+1];
	int			outLineLen;
	char		errMsg[APS_GUI_DISPLAY_STR_SIZE] ;

	while (*bufStartPtr != STREND
			&& (bufEndPtr = strchr( bufStartPtr, NEWLINE )) != NULL)
	{
		*bufEndPtr = STREND;	/* replace NEWLINE with STREND in fname */
		if (bufStartPtr == bufEndPtr) /* empty buffer */
			break;
		if ((bufEndPtr - bufStartPtr + (int)strlen( fullFilename ))
			< BUFSIZ)
		{
			/*
			-- store the full filename (contains path and filename)
			*/
			(void)strcat( fullFilename, bufStartPtr );

			pathComponent = path_from_pathname( fullFilename );
			if (pathComponent == NULL)
			{
				pathComponent = malloc( strlen( CURR_DIR_STR ) + 1 ) ;
				(void) strcpy( pathComponent, CURR_DIR_STR ) ;
			}
			fnameComponent = filename_from_pathname( fullFilename );

			outLineLen = strlen( fnameComponent ) + 1 /*space*/ +
						 strlen( pathComponent ) + 1 /*space*/ +
						 strlen(filetypeInfo->create_executable) + 1 /*space*/ +
						 strlen( filetypeInfo->type ) + 1 /*newline*/ ;
			if (outLineLen	> FP_MAX_FILE_LINE)
			{
				(void) sprintf( errMsg,
					"%s: skipping this file (line too long, max len = %d):\n    \"%s %s %s %s\"\n",
					thisCmd, FP_MAX_FILE_LINE, fnameComponent, pathComponent,
					filetypeInfo->create_executable, filetypeInfo->type ) ;
				aps_internal_error( stderr, __FILE__, __LINE__, errMsg ) ;
			}
			else
			{
				(void)sprintf( outLine, "%s %s %s %s\n",
					fnameComponent, pathComponent,
					filetypeInfo->create_executable, filetypeInfo->type );
				if (write( outFd, outLine, (unsigned) outLineLen )
						!= outLineLen)
				{
					(void) fprintf( stderr,
						"%s: QUITTING, can't get list of files\n    (write errno = %d)",
						thisCmd, errno );

					free( pathComponent ) ;
					free( fnameComponent ) ;
					get_files_cleanup( SIGTERM );	/* EXITs */
				}
			}
			free( pathComponent ) ;
			free( fnameComponent ) ;
		}
		else
		{
			/* can't process this one */
			(void)sprintf( errMsg,
				"%s: skipping this file (filename is too long, max len = %d):\n    %s%s\n",
				thisCmd, BUFSIZ - 1, fullFilename, bufStartPtr );
			aps_internal_error( stderr, __FILE__, __LINE__, errMsg ) ;
		}
		bufStartPtr = bufEndPtr + 1;
		*fullFilename = STREND;
	}

	if (*bufStartPtr != STREND)	/* if have a partial full- fname, save */
	{
		(void)strcpy( fullFilename, bufStartPtr );
	}

	return;
}



/*==============================================================================
Function:		get_matching_files

Description:	Executes a command string whose stdout is lines
				of files satisfying the command.

Parameters:		cmd_string

Returns:     	N/A

Creator:		Teresa McKillop

Creation Date:	11/28/95

Notes:		
==============================================================================*/
static void
get_matching_files( char *childCmd, REPORT *report_info )
{
	char	buf[BUFSIZ];
	int		childStdin[2];	/* not used, but precludes getting keyboard input */
	int		childStdout[2];
	int		childPid;
	int		numRead;

	if (pipe( childStdin ) || pipe( childStdout ))
	{
		(void) fprintf( stderr,
			"%s: can't search for files\n    There may be too many processes\n    (errno = %d)\n",
			thisCmd, errno );
		return;
	}

	/* flush, just in case, to be ready for child */
	(void) fflush(stdout) ;

	if (SignalCaught)	/* last chance: last check before forking child */
		get_files_cleanup( SignalCaught );	/* EXITs */

	if ((childPid = fork()) > 0)	/* parent */
	{
		(void) close( childStdin[READ_END]  );
		(void) close( childStdout[WRITE_END]  );

		/* process child's stdout */
		for (;;)
		{
			if ((numRead = read( childStdout[READ_END], buf, BUFSIZ - 1)) > 0)
			{
				buf[numRead] = STREND;
				get_matching_read_proc( report_info, buf );
			}
			else if (numRead == 0)	/* child has died */
			{
				break; /* out of forever */
			}
			else	/* read "broke", can't continue */
			{
				(void) kill( childPid, SIGKILL );
				if (errno != EINTR)
				{
					(void) fprintf( stderr,
						"%s: QUITTING: can't get list of files\n    (read errno = %d)\n",
						thisCmd, errno );
				}
				else
				{
					(void) fprintf( stderr,
						"%s: QUITTING: received interrupt\n", thisCmd );
				}
				SignalCaught = SignalCaught ? SignalCaught : SIGTERM;
				get_files_cleanup( SignalCaught );	/* EXITs */
			}
		}
		(void) close( childStdin[WRITE_END]  );
		(void) close( childStdout[READ_END]  );
	}
	else if (childPid == 0)	/* child - start subprocess */
	{
		int		w, c;
		char	*cp;
		char	*argv[MAX_NUM_CHILD_ARGS + 1];	/* null terminating element */
		char	args[MAX_NUM_CHILD_ARGS][128];
		char	quotechar ;

		(void) close( childStdin[WRITE_END]  );
		(void) close( childStdout[READ_END]  );

		for (w=0, cp=childCmd ; w < MAX_NUM_CHILD_ARGS && *cp ; w++ )
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

		(void) dup2( childStdin[READ_END],  0);	/* make stdin	*/
		(void) dup2( childStdout[WRITE_END], 1);	/* make stdout	*/
		(void) close( childStdin[READ_END] );
		(void) close( childStdout[WRITE_END]  );

		/* make child ignore signals */
		set_fp_signals( FP_SIG_IGN );

		/* execvp( childCmd, argv ) ; */
		(void) execvp( argv[0], argv ) ;
		(void) fprintf( stderr,
			"%s: Can't run program: %s\n    Possibly it's not in your path\n    (errno = %d)\n",
			thisCmd, childCmd, errno );
		exit(SIGCHLD);
	}

	if (SignalCaught)
		get_files_cleanup( SignalCaught );	/* EXITs */

	return;
}



main( int argc, char *argv[] )
{
	extern char	*optarg ;
	extern int	optind ;
	char		*optList = "t:" ;
	int			optChar ;

	PERMREC		*permArray ;	/* malloc'ed array of active file proc. perms */
	int			numPerms ;
	int			lastPerm = -1 ;	/* index to the last matched perm array entry */

	char		*fullFilename;
	char		*filePath;
	char		*fileName;

	REPORT		*report_info;
	char		cmd_string[FP_MAX_CMD_SIZE];

	char		msg[BUFSIZ] ;
	int			retval ;
	int			i, j;

	/* set stdout to unbuffered I/O */
	setbuf( stdout, (char *) NULL ) ;

	/* parse the options (ie, arguments) */
	thisCmd = argv[0];
	while ((optChar = getopt( argc, argv, optList )) != EOF)
	{
		switch( optChar )
		{
			case 't' :
				reportType = optarg ;
				break ;
			default	: 
				usage( thisCmd ) ;	/* EXITs */
				break ;
		}
	}
	if (optind != argc - 1)
		usage( thisCmd ) ;	/* EXITs */
	outRoot = malloc( strlen( argv[optind] ) + 1 ) ;
	(void) strcpy( outRoot, argv[optind] ) ;
	outFile = malloc( strlen( outRoot ) + MAX_RPT_TYPE_LEN + 2 ) ;

	set_fp_signals( FP_SIG_HANDLER );	/* initialize signal handling */

	/* get the active permission ids */
	permArray = get_active_permissions( thisCmd, &numPerms ) ;

	/* get the permitted files */
	for (i = 0 ; reports_inbound[i].type != NULL && !SignalCaught ; i++)
	{
		if (!reportType || strcmp( reportType, reports_inbound[i].type ) == 0)
		{
			int		tmpPerm ;

			report_info = &reports_inbound[i];

			/* continue only if have permission for this report type */
			for (tmpPerm = (lastPerm + 1) % numPerms, j = 0
					; j < numPerms
					; tmpPerm = (tmpPerm + 1) % numPerms, j++)
			{
				if (strcmp( permArray[tmpPerm].type, report_info->type ) == 0)
					break ;
			}
			if (j >= numPerms)	/* not found, skip this type */
				continue ;

			/* validate the permission */
			if ((retval = mu_permission_validate(
					permArray[lastPerm = tmpPerm].id,
					report_info->mu_activityid, MU_SINGLE_ACTIVITY_TYPE )) <= 0)
			{
				/* permission isn't valid or couldn't be validated, skip type */
				if (retval < 0)	/* mu lib error occurred */
					(void) sprintf( msg, "Permission validation ERROR:\n" ) ;
				else	/* invalid perm */
					(void) sprintf( msg, "Invalid permission ID\n" ) ;

				(void) fprintf( stderr,
						"%s\n    **** SKIPPING **** file type %s",
						msg, report_info->type ) ;

				continue ;
			}

			/* find all files of this type */
			if (report_info->mk_local_filename)
			{
				fullFilename = (*(report_info->mk_local_filename))() ;
				filePath = path_from_pathname( fullFilename );
				fileName = filename_from_pathname( fullFilename );
			}
			else
			{
				filePath = aps_fullpath( report_info->aps_fullpath_id, NULL );
				fileName = report_info->dirmask;
			}

			(void)sprintf( cmd_string, "%s %s %s",
				APS_GET_FILE_LIST, filePath, fileName );

			free( filePath ) ;

			if (SignalCaught)
				get_files_cleanup( SignalCaught );	/* EXITs */

			/* open the output file */
			(void) sprintf( outFile,
					"%s_%s", outRoot, reports_inbound[i].type ) ;
			if ((outFd = open( outFile, O_WRONLY|O_EXCL|O_CREAT, 0644 )) < 0)
			{
				(void) fprintf( stderr,
					"%s: EXITING: can't create output file:\n    %s\n    (errno = %d)\n",
					thisCmd, outFile, errno );
				exit (APS_EXIT_ERROR);
			}

			get_matching_files( cmd_string, report_info );
			/* if no matching files, remove the out file */
			if (lseek( outFd, 0, SEEK_CUR ) == 0)
				(void) unlink( outFile ) ;

			(void) close( outFd ) ;
			outFd = -1 ;

			if (reportType)
				break ;	/* out of for loop through reports_inbound (DONE) */
		}
	}

	get_files_cleanup( SignalCaught );	/* EXITs */
	
	return (0);	/* will never get here: needed for LINT */
}
