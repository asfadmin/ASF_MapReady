#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		subprocess.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)subprocess.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.subprocess.c"

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>		/* EWOULDBLOCK, EAGAIN, etc	*/
#include <string.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>	/* for stat call 			*/
#include <sys/stat.h>	/* also for stat call 		*/
#include <sys/wait.h>	/* for wait3 call to clear zombie processes	*/
#include <sys/time.h>	/* for wait3 call to clear zombie processes	*/
#include <sys/resource.h> /* for wait3 call to clear zombie processes	*/
#include <Xm/XmP.h>

#include "dapps_defs.h"
#include "subprocess.h"
#include "apspath.h"
#include "apsfiledef.h"
#include "aps_defs.h"
#include "timeconv.h"

extern void				popup_message() ;

extern char				*sys_errlist[];
extern int				sys_nerr;
extern XtAppContext		UxAppContext ;



/*==============================================================================
Function:		

Description:	
   Function to build file name and open an aps LOG file - GAF 03/10/95

Parameters:		

Returns:     	TRUE/FALSE

Creator:		Gus Faist

Creation Date:	03/15/1995

Notes:		
==============================================================================*/
static void
open_log_file(this)
	PROCESS_INFO *this;
{
	int year, decade, doy, hour, min, sec, msec ;
	char asftime[22] ; 
	char *aps_fullpath_filename ;
	char filename[256] ;
	char msg[512] ;

	/* get system time in asf format */
	tc_systime2asf(asftime) ;

	/* check asf time */ 
	if (!tc_parse_asftime(asftime,
			&year, &decade, &doy, &hour, &min, &sec, &msec))
	{
		(void)sprintf(msg, "WARNING: Unable to create log filename log!\nProcess: %s\nMessages are not being logged\n",
			this->cmd) ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR", msg, XtGrabNone) ;
		return ;
	}	

	/* make file name (this->cmd.yyyyddsdhhmmss.LOG) */

	(void)sprintf(filename,
		"%s.%4d%03d_%02d%02d%02d.LOG",
		this->cmd, year, doy, hour, min, sec) ;

	aps_fullpath_filename = aps_fullpath(APS_LOGS, filename) ;

	/*  Open the file  */
	if ((this->logfile = fopen(aps_fullpath_filename, "a")) == NULL)
	{
		(void)sprintf(msg, "WARNING: Unable to open log file!\nfor process %s\nMessages are not being logged\n",
			this->cmd) ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR", msg, XtGrabNone) ;
		this->logfile = NULL ;
	}	
	else  /* log was opened lets add some diags */
	{
		setbuf( this->logfile, (char *) NULL );  /* make I/O unbuffered */
		(void) fprintf( this->logfile,
			"COMMAND:\n\t%s\nSTARTED:\n\n", this->cmdLine ) ;
	}

	free(aps_fullpath_filename) ;
}


/*==============================================================================
Function:       finish_child

Description:    Does the non-stdin/non-stdout/non-stderr specific processing
				to finish off the child (after the process has died)

Parameters:     

Returns:        

Creator:        Teresa McKillop

Creation Date:  07/26/96

Notes:		
==============================================================================*/
static void
finish_child( PROCESS_INFO *this )
{
	int		status ;

	/*
	-- get process exit code;
	-- see if caught a signal or else get
	-- the exit code from the returned value
	*/

	(void) waitpid( this->childPid, &status, NULL ) ;
	if (WIFSIGNALED( status ))
	{
		if (WCOREDUMP( status ))
			status = APSGUI_EXIT_COREDUMP ;
		else
			status = -WTERMSIG( status ) ;
	}
	else
	{
		if (( status = WEXITSTATUS( status )) != 0)
			status = APS_EXIT_ERROR ;
		else
			status = APS_EXIT_OK ;
	}
	this->exit_status = status ;

	/*
	-- we're done so process the user specified done function
	*/

	if ((this->done_func))
		(*(this->done_func))(this, this->done_parameter) ;

	/*
	-- complete the log file
	*/

	if (this->logfile)
	{
		if (status >= 0)
		{
			(void) fprintf( this->logfile, "\n\nCOMMAND:\n\t%s\nis completed\n",
				this->cmdLine ) ; 
		}
		else if (status == APSGUI_EXIT_COREDUMP)
		{
			(void) fprintf( this->logfile, 
				"\n\nCOMMAND:\n\t%s\nCore Dumped\n",
				this->cmdLine ) ; 
		}
		else	/* signal caught, but no core dump */
		{
			(void) fprintf( this->logfile, 
				"\n\nCOMMAND:\n\t%s\nCaught a SIGNAL (signal = %d)\n",
				this->cmdLine, -status ) ; 
		}
		(void) fclose(this->logfile) ;
	}

	if (this->io[WRITE_END] >= 0)
		(void) close(this->io[WRITE_END]);		/* PR#404, PR#405 */

	destroy_process( this );

	return ;
}




/*==============================================================================
Function:		

Description:	

Parameters:		

Returns:     	this	- process info for the created process
				NULL	- if couldn't allocate memory (pops up an error message)
						  (no process is created)

Creator:		Author Unknown 
                Modified by Ron Green

Creation Date:	XX/XX/XXXX

Notes:		
==============================================================================*/
PROCESS_INFO *
create_process(newCreationString, status, do_log_file,
	write_data, read_func, read_param, done_func, done_param)

	char	*newCreationString ;
	int		*status ;
	int		do_log_file;
	char	*write_data ;
	void	(*read_func)(); void *read_param ;
	void	(*done_func)() ; void *done_param ;
{
    char*	cp;
    int		len;
	PROCESS_INFO *this = NULL;
    char msg[BUFSIZ];

	this = (PROCESS_INFO *) malloc(sizeof(PROCESS_INFO)) ;
	if (this == NULL)
	{
        (void) sprintf( msg,
			"ERROR: Can't allocate PROCESS_INFO for a new process\n" );
		popup_message(XmDIALOG_ERROR, "APS:ERROR", msg, XtGrabNone) ;
		return (NULL);
	}

	this->target_filename = (char *) malloc ( APS_MAX_FILENAME_LENGTH+1 );
	if (this->target_filename == NULL)
	{
        (void) sprintf(msg,"ERROR: Can't allocate memory for a new process\n");
		popup_message(XmDIALOG_ERROR, "APS:ERROR", msg, XtGrabNone) ;
		destroy_process( this ) ;
		return (NULL);
	}

	this->original_filename = (char *) malloc ( APS_MAX_FILENAME_LENGTH+1 );
	if (this->original_filename == NULL)
	{
        (void) sprintf(msg,"ERROR: Can't allocate memory for a new process\n");
		popup_message(XmDIALOG_ERROR, "APS:ERROR", msg, XtGrabNone) ;
		destroy_process( this ) ;
		return (NULL);
	}

	/* malloc & copy newCreationString for struct elements */
    this->creationString = (char *) malloc( strlen( newCreationString ) + 1 );
	if (this->creationString == NULL)
	{
        (void) sprintf(msg,"ERROR: Can't allocate memory for a new process\n");
		popup_message(XmDIALOG_ERROR, "APS:ERROR", msg, XtGrabNone) ;
		destroy_process( this ) ;
		return (NULL);
	}
	else
	 	(void)strcpy( this->creationString, newCreationString );

	/* extract first component from cmdLine -- must be full path of prog */
	for (cp = this->creationString, len=0 ; *cp && ' ' < *cp ; cp++, len++)
		/**/ ;

	/*
	-- copy only the command to execute don't copy any command arguments
	-- len counted up to the first space of newCreationString; 
	-- this should be the number of characters we make part of the
	-- command name
	*/
	this->cmd = (char*) malloc( len + 1 );
	if (this->cmd == NULL)
	{
        (void) sprintf( msg,
			"ERROR: Can't allocate memory for a new cmd string\n" );
		popup_message(XmDIALOG_ERROR, "APS:ERROR", msg, XtGrabNone) ;
		destroy_process( this ) ;
		return (NULL);
	}
	else 
		(void)sprintf(this->cmd, "%.*s", len, this->creationString) ;

    /* 
    ** cmdLine is from last component of 1st arg to end of creationString
    ** Note that cp already points to the end of program path
	** so back up to the /
    */
    for ( ; this->creationString < cp && *cp != '/' ; cp-- )
		/**/ ;

    this->cmdLine = cp;

	/* 
	-- set the output window to NULL 
	-- in case the user forgets 
	*/
	this->display_window = NULL ;

	if (do_log_file == TRUE)
		this->logfile = (FILE *) 1 ;  /*referencing this will cause core dump*/
	else
		this->logfile = NULL ;

	if (write_data != NULL && strlen( write_data ) > 0)
		this->write_data = write_data ;
	else
	{
		this->write_data = NULL ;
		this->wrId = 0 ;
	}

	this->read_func = read_func ;
	this->read_parameter = read_param ;

	this->done_func = done_func ;
	this->done_parameter = done_param ;

	if (status != NULL)
		*status = 1 ;

	this->errBufIdx = 0;

    if ((this->errBuffer = (char *) calloc(1, 1)) == (char *) NULL)
	{  
		/* No memory is available */
		(void) sprintf(msg,
			"ERROR: Can't allocate memory for error msg buffer\n");
		popup_message(XmDIALOG_ERROR, "APS:ERROR", msg, XtGrabNone) ;
		destroy_process( this ) ;
		return (NULL);
	}

	return (this) ;
}



/*==============================================================================
Function:		

Description:	

Parameters:		

Returns:     	

Creator:		Author Unknown 
                Modified by Ron Green

Creation Date:	XX/XX/XXXX

Notes:		
==============================================================================*/
void
destroy_process( this )
	PROCESS_INFO *this;
{
	if (this != NULL)
	{
		if (this->target_filename != NULL)
		{
			free( this->target_filename);
			this->target_filename = NULL;
 		}
		if (this->original_filename != NULL)
		{
			free( this->original_filename);
			this->original_filename = NULL;
 		}
		if (this->cmd != NULL)
		{
			free( this->cmd );
			this->cmd = NULL;
 		}
		if (this->creationString != NULL)
		{
			free( this->creationString );
			this->creationString = NULL;
		}
		if (this->errBuffer != NULL)
		{
			free( this->errBuffer );
			this->errBuffer = NULL;
		}
		free( this );
		this = NULL;	
	}

	return;
}


/*==============================================================================
Function:       SubPr_WriteDataToChild

Description:    

Parameters:     

Returns:        

Creator:        Teresa McKillop

Creation Date:  01/13/97

Notes:		
==============================================================================*/
/* ARGSUSED1 */
static void
SubPr_WriteDataToChild( client_data, source, id )
	XtPointer client_data ;
    int*       source;
    XtInputId* id;
{
    PROCESS_INFO	*this ;
	char			errMsg[256];
	int				dataLen ;
	int				bytesWritten ;
	char			*ptr ;

	/* convert the client data to PROCESS INFO type */
	this = (PROCESS_INFO *) client_data ;

	ptr = this->write_data ;
	bytesWritten = write( this->io[WRITE_END], ptr, dataLen = strlen( ptr ) ) ;

	if (bytesWritten == dataLen)	/* done writing data */
	{
		/* remove the registered Input */
		XtRemoveInput( this->wrId ) ;
		this->wrId = 0 ;
	}
	else if (bytesWritten < 0)	/* write error */
	{
		if (errno != EAGAIN)	/* not "would have blocked" */
		{
			(void) sprintf( errMsg, "ERROR writing data to %s, %s", 
				this->cmd, sys_errlist[errno] ); 
			popup_message(XmDIALOG_ERROR, "APS:ERROR", errMsg, XtGrabNone);
			/* can't send the data; *** KILL *** the child */
			(void) kill( this->childPid, SIGKILL ) ;
		}
	}
	else	/* still some left to write, try it again via registered Input */
	{
		if (bytesWritten > 0)
		{
			/* remove the part that was successfully written */
			this->write_data = ptr + bytesWritten ;
		}
	}

	return ;
}


/*==============================================================================
Function:       

Description:    

Parameters:     

Returns:        

Creator:        Ron Green/Author Unknown (SFOC stuff)

Creation Date:  Thu May 25 17:06:41 PDT 1995

Notes:		
==============================================================================*/
/* ARGSUSED1 */
static void
SubPr_ReadDataFromChild(client_data, source, id)
	XtPointer client_data ;
    int*       source;
    XtInputId* id;
{
    PROCESS_INFO *this ;
    int s;
	char  errMsg[256];
	char buf[BUFSIZ] ;

	/* convert the client data to PROCESS INFO type */
	this = (PROCESS_INFO *) client_data ;

	s = read(this->io[READ_END], buf, BUFSIZ-1) ;
	if (s == -1)
	{
		switch ( errno )
		{
		case EWOULDBLOCK: /* same as EAGAIN */
			return ;
		default: /* I/O error */
			(void)sprintf( errMsg, "ERROR reading output from %s, %s", 
				this->cmd, sys_errlist[errno] ); 
			popup_message(XmDIALOG_ERROR, "APS:ERROR", errMsg, XtGrabNone);
		}
	}
	else if (s == 0) /* Child Process has ended */
	{
		(void) close(this->io[READ_END]) ;
		XtRemoveInput(this->id) ;
		this->id = 0;
		if (this->wrId != 0)
		{
			XtRemoveInput(this->wrId) ;
			this->wrId = 0 ;
		}

		if (this->erId == 0)	/* if stderr already sensed that child is dead*/
			finish_child( this ) ;
	}
	else
	{
		buf[s] = STREND ; /* terminate where the buffer ends */

		/* pass the data to the user specified read handler */
		if ((this->read_func))
			(*(this->read_func))(this->read_parameter, buf) ;

		/* log the information if logfile is availble */
		if (this->logfile)
			(void) fprintf(this->logfile, "%s", buf);
    }
    return ;
}



/*==============================================================================
Function:       

Description:    

Parameters:     

Returns:        

Creator:        Ron Green/Author Unknown (SFOC stuff)

Creation Date:  Thu May 25 17:06:12 PDT 1995

Notes:		
==============================================================================*/
/* ARGSUSED1 */
static void
SubPr_ReadErrFromChild(client_data, source, id)
	XtPointer client_data ;
	int	*source;
	XtInputId	*id ; 
{

	PROCESS_INFO *this ;
	char	errMsg[BUFSIZ];
	int		s;
    char	msg[BUFSIZ];

    (void) memset( errMsg, 0, sizeof(errMsg) );

	/* convert the client data to PROCESS INFO type */
	this = (PROCESS_INFO *) client_data ;

    if( (s = read( this->io[READ_ERR] , errMsg , sizeof( errMsg ) )) < 0 )
    {
		switch ( errno )
		{
		case EWOULDBLOCK: /* same as EAGAIN */
						  /* not enough data, try again later */
			break;
		default: 		  /* I/O error */
			(void)sprintf( errMsg, "Received ERROR from %s, %s", 
				this->cmd, sys_errlist[errno] ); 
			popup_message(XmDIALOG_ERROR, "APS:ERROR", errMsg, XtGrabNone);
		}
 	}
    else if (s == 0) /* child process has ended */
	{
		if (this->errBufIdx > 0 && this->errBuffer != NULL)
		{
			{
				popup_message(XmDIALOG_ERROR, 
							  "APS:ERROR", this->errBuffer, XtGrabNone);
				free( this->errBuffer );
				this->errBuffer = NULL;
			}
 		}

		(void) close(this->io[READ_ERR]) ;
		XtRemoveInput(this->erId) ;
		this->erId = 0;
		if (this->wrId != 0)
		{
			XtRemoveInput(this->wrId) ;
			this->wrId = 0 ;
		}

		if (this->id == 0)	/* if stdout already sensed that child is dead */
			finish_child( this ) ;
	}	
	else /* data was read from the stderr from the child process */
	{
		if ((this->errBuffer =
			realloc( this->errBuffer, s + this->errBufIdx + 1 )) 
															== (char *) NULL)
		{
			/* No memory is available */ 
        	(void) sprintf( msg,
				"ERROR: Can't allocate memory for error msg buffer\n" );
			popup_message(XmDIALOG_ERROR, "APS:ERROR", msg, XtGrabNone) ;
			return ;
		}
		else
		{
			(void) sprintf(this->errBuffer + (this->errBufIdx), "%s", errMsg);
			this->errBufIdx += s;
		}
	}
    return;
}



/*=====================================================*/
int
start_process(this)
	PROCESS_INFO	*this ;
{
	int 		fromParentToChild[2];
	int 		fromChildToParent[2];
	int 		err_ChildToParent[2];
	int			dup2error = FALSE;
	char		quotechar ;


    /* open pipe */

    if (pipe(fromParentToChild) || pipe(fromChildToParent)
			|| pipe(err_ChildToParent) )
    {
		popup_message(XmDIALOG_ERROR, "APS:ERROR", 
			"Failed to open pipes for subprocess", XtGrabNone);

		return 1; 
    }

	(void) fflush(stdout) ;
	(void) fflush(stderr) ;

    if ( this->childPid = fork() )
    {
		/* parent */
		(void) close( fromParentToChild[READ_END]  );
		(void) close( fromChildToParent[WRITE_END] );
		(void) close( err_ChildToParent[WRITE_END] );
	
		this->io[WRITE_END]	= fromParentToChild[WRITE_END];
		this->io[READ_END]	= fromChildToParent[READ_END];
		this->io[READ_ERR]	= err_ChildToParent[READ_END];

		/* non-blocking write to and read from sub-process */
		(void)fcntl( this->io[WRITE_END], F_SETFL, O_NONBLOCK );
		(void)fcntl( this->io[READ_END], F_SETFL, O_NONBLOCK );
		(void)fcntl( this->io[READ_ERR], F_SETFL, O_NONBLOCK );

		/* now assign data and error handlers */
		if (this->write_data != NULL)
		{
			this->wrId = XtAppAddInput( UxAppContext, this->io[WRITE_END],
				(XtInputId *) XtInputWriteMask, SubPr_WriteDataToChild,
				(char *) this ) ;
		}

		this->id = XtAppAddInput(UxAppContext, this->io[READ_END],
			(XtInputId *) XtInputReadMask, SubPr_ReadDataFromChild,
			(char *) this) ;
										  
		this->erId = XtAppAddInput(UxAppContext, this->io[READ_ERR],
			(XtInputId *) XtInputReadMask, SubPr_ReadErrFromChild,
			(char*) this) ;
		
		/* create name and open LOG file for this subprocess */
		if (this->logfile != NULL)
			open_log_file(this) ;

		return 0;
	}
	else
	{
		/* child - start subprocess */
		int w, c;
		char* cp;
		char* argv[20 + 1];	/* null terminating element */
		char  args[20][128];

		for (w=0, cp=this->cmdLine ; w < 20 && *cp ; w++ )
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

#ifdef DEBUG
		(void) printf("EXECVP CMD = %s %d\n" , this->cmd , w);
		for (c=0; c < w; c++)
			(void) printf("argv[%d] = %s#\n", c, argv[c] );
#endif	/*DEBUG*/

		(void) close( fromParentToChild[WRITE_END] );
		(void) close( fromChildToParent[READ_END]  );
		(void) close( err_ChildToParent[READ_END]  );  

		/* make stdin	*/
		if (dup2( fromParentToChild[READ_END],  STDIN_FILENO) == -1)
			dup2error = TRUE ;
		/* make stdout	*/
		else if (dup2( fromChildToParent[WRITE_END], STDOUT_FILENO) == -1)
			dup2error = TRUE ;
		/* make stderr  */
		else if (dup2( err_ChildToParent[WRITE_END], STDERR_FILENO) == -1)
			dup2error = TRUE ;

		if (dup2error == TRUE)
		{
			(void) fprintf( stderr, "Can't execute %s (dup2 failed)\n",
				this->cmd ) ;
			perror("");
			exit(APS_EXIT_ERROR);
		}
		else
		{
			(void) close( fromParentToChild[READ_END] ) ;
			(void) close( fromChildToParent[WRITE_END] ) ;
			(void) close( err_ChildToParent[WRITE_END] ) ;
		}

		/* execvp(this->cmd, argv) ; */
		(void) execvp( this->cmd, argv ) ;
		(void) fprintf( stderr,"execve(%s) failed!\n", this->cmdLine ) ;
		perror("");
		exit(APS_EXIT_ERROR);
	}

	return 0;	/* never reach here, but needed for lint */
}
