static char *sccs = "@(#)ims_childHandler.c	5.1  17 Mar 1996";
/******************************************************************************
**
** File:        ims_childHandler.c
**
** Function:    Function call to fork a child with a command line passed
**              as an argument. Like system (3), but does not wait for
**              termination.
**
** Author:      J. Jacobson
**
** Date:        5/11/89
**
** Modified:    1/14/94 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Added the include files sys/unistd.h and stdarg.h. Removed
**              the include file varargs.h. Modified ims_startChild() to
**              use variable arguments the ANSI way. The Port Team modified
**              the status checking by substituting the W* macros for
**              previous code.
**
**              9/4/95 - S. Hardman - R1B
**              ASF adaptation of code inherited from AMMOS-CDB.
**
******************************************************************************/

#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdarg.h>
#include <errno.h>
#include <signal.h>
#include <string.h>

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_signal.h>
#include <ims_childHandler.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in the ims_childHandler.h
** header file. They are listed here for reference.
**
**	pid_t ims_startChild (IMS_MSG_STRUCT *, char *, ...);
**	pid_t ims_startChild2 (IMS_MSG_STRUCT *, char *, ...);
**	int ims_waitForChild (IMS_MSG_STRUCT *, pid_t);
*/

/******************************************************************************
**
** ims_startChild ()
**
** Forks child, which execs a command passed as an argument.
**
** Returns pid of the started child, -1 otherwise.
**
******************************************************************************/

pid_t ims_startChild (
	IMS_MSG_STRUCT *msgDesc,
	char *image,
	...)
{
	va_list ap;
	pid_t pid;
	static char *argList[MAX_ARGS];
	int argno = 0;

	/*
	** Expand arguments before forking.  This will reduce the amount
	** of time before the child process execs, and improve the chance
	** that we'll sleep long enough during debugging sessions.
	*/
	va_start (ap, image);

	while ((argList[argno++] = va_arg (ap, char *)) != 0);

	va_end (ap);
	
	/*
	** Fork a child process and remember its pid.
	*/
	pid = fork ();

	/*
	** Interpret the process identifier.
	*/
	switch (pid)
	{
		case -1:
			/* Parent:  Could not fork child process. */
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not fork a process for image '%s'. %s",
				image, strerror (errno));
			break;

		case 0:
			/* Child:  Execute our command. */
			(void) execv (image, argList);

			/* Give use some indication of why this failed. */
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Exec of image '%s' failed. %s",
				image, strerror (errno));
			_exit (1);

		default:
			/* Successfully started child process. */
#ifdef DEBUG
			(void) ims_msg (msgDesc, IMS_INFO,
				"Started process '%d' for image '%s'.", pid, image);
#endif  /* DEBUG */
			break;
	}

	/*
	** At this point, pid is either valid or is -1.
	*/
	return (pid);
}

/******************************************************************************
**
** ims_startChild2 ()
**
** Forks child, which execs a command passed as an argument.
**
** The last argument in the argument list is used as a flag to setup
** a bi-directional pipe between parent and child.  When the last
** argument is DUPLEX_PIPE, then the program will setup a bi-
** directional pipe.  The stdin and stdout of the parent are
** connected to the stdout and stdin of the child.  The stdin and 
** stdout of the child are connected to the stdout and stdin of the
** parent, respectively.
**
** Returns pid of the started child, -1 otherwise.
**
******************************************************************************/

pid_t ims_startChild2 (
	IMS_MSG_STRUCT *msgDesc,
	char *image,
	...)
{
	va_list ap;
	pid_t pid;
	static char *argList[MAX_ARGS];
	int argno = 0;
	int pfdout[2];       /* pipe file descriptor */
	int pfdin[2];        /* pipe file descriptor */
	int duplex_pipe = 0; /* duplex pipe flag */

	/*
	** Expand arguments before forking.  This will reduce the amount
	** of time before the child process execs, and improve the chance
	** that we'll sleep long enough during debugging sessions.
	*/
	va_start (ap, image);

	while ((argList[argno++] = va_arg (ap, char *)) != 0);

	va_end (ap);

	/*
	** Look for DUPLEX_PIPE in the last argument
	*/
	if ((strncmp (argList[argno-2], "DUPLEX_PIPE",11) == 0) ||
	    (strncmp (argList[argno-2], "duplex_pipe",11) == 0) )
	{
		duplex_pipe = 1;
		argList[argno-2] = (char *) 0;

		if ((pipe (pfdout) == -1) || (pipe (pfdin) == -1))
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not initiate DUPLEX_PIPE.");
			return (-1);
		}
	}

	/*
	** Fork a child process and remember its pid.
	*/
	pid = fork ();

	/*
	** Interpret the process identifier.
	*/
	switch (pid)
	{
		case -1:
			/* Parent: Could not fork child process. */
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not fork a process for image '%s'. %s",
				image, strerror (errno));
			break;

		case 0:
			/* Child: Execute our command. */

			/*
			** Set-up the pipe for the child.
			** pfdout[0] = stdin, pfdin[1] = stdout
			*/
			if (duplex_pipe)
			{
				if ((close(0) == -1)  ||
				    (dup(pfdout[0]) != 0) ||
				    (close(1) == -1)  ||
				    (dup(pfdin[1]) != 1) ||
				    (close(pfdout[0]) == -1) ||
				    (close(pfdin[1]) == -1) ||
				    (close(pfdin[0]) == -1) ||
				    (close(pfdout[1]) == -1) )
				{
					(void) ims_msg (msgDesc, IMS_FATAL, 
						"Could not set-up duplex-pipe for child process.");
					return (-1);
				}
			}

			/* Execute the command. */
			(void) execvp (image, argList);

			/* Give use some indication of why this failed. */
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Exec of image '%s' failed. %s",
				image, strerror (errno));
			_exit (1);

		default:
			/* Successfully started child process. */

			/* Set-up the pipe for the parent. */
			if (duplex_pipe)
			{
			   	if ((close(pfdout[0]) == -1) ||
			        (close(pfdin[1])  == -1) ||
			        (close(0)         == -1) ||
			        (dup(pfdin[0]) != 0)     ||
			        (close(1)         == -1) ||
			        (dup(pfdout[1]) != 1)    ||
			        (close(pfdin[0])  == -1) ||
			        (close(pfdout[1]) == -1) )
			   	{
			    	(void) ims_msg (msgDesc, IMS_FATAL,
							"Could not set-up duplex-pipe for parent process.");
	
						/* Kill the child. */
		       	if ((kill (pid, SIGTERM)) == -1)
						{
					 		(void) ims_msg (msgDesc, IMS_FATAL,
					    	"Could not kill the child process '%d'.", pid);
						}

		       	return (-1);
	    		}
			}
	}

	/*
	** At this point, pid is either valid or is -1.
	*/
	return (pid);
}

/******************************************************************************
**
** ims_waitForChild ()
**
******************************************************************************/

int ims_waitForChild (
	IMS_MSG_STRUCT *msgDesc,
	pid_t pid)
{
	pid_t retPid;
	int status;
	int signal;
	int code;

	/*
	** Wait for the child process to return.
	*/
	if ((retPid = wait (&status)) == -1)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Wait for child process failed. %s",
			strerror (errno));
		return (IMS_FATAL);
	}

	/*
	** Compare process itendifiers to make sure we
	** have the right one.
	*/
	if (pid != retPid)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"The child process '%d' returning status, is not the process we started '%d'.",
			retPid, pid);
		return (IMS_FATAL);
	}

	/*
	** Check whether process was terminated with a signal.
	*/
	if (WIFSIGNALED (status) != 0)
	{
		/* Interpret returned signal and generate message. */
		signal = WTERMSIG (status);
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Termination of child process '%d' due to signal: %s (%d).",
			retPid, ims_sigMsg (signal), signal);
		return (IMS_ERROR);
	}
	else  /* Child exited without signal termination. */
	{
		/* Check if the child exited normally. */
		if (WIFEXITED (status) != 0)
		{
			/* Check the childs return status. */
			if ((code = WEXITSTATUS (status)) != 0)
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"The child process '%d' exited with a status of '%d'. %s",
					retPid, code, strerror (errno));
				return (IMS_ERROR);
			}
		}
		else
		{
			/* Child didn't exit, should never happen. */
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Child process '%d' never exited.", retPid);
			return (IMS_FATAL);
		}
	}

#ifdef DEBUG
	(void) ims_msg (msgDesc, IMS_INFO,
		"Completed process '%d' successfully.", retPid);
#endif  /* DEBUG */

	return (IMS_OK);
}
