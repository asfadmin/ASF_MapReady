static char *sccs = "@(#)ims_getInput.c	5.1  16 Mar 1996";
/******************************************************************************
**
** File:        ims_getInput.c
**
** Function:    Routines to read interactive terminal input.
**
** Author:      K. Pattaphongse
**
** Date:        01/13/89
**
** Modified:    11/10/93 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Removed cdb_argvLogin(), cdb_checkArg(), cdb_parseArg(),
**              and cdb_getUser()  because they were not called by
**              another function. Replaced calls to ioctl() with tcgetattr()
**              and tcsetattr().  Removed cdb_ioctl() because it is no longer
**              needed. Replaced include file sys/ioctl.h with sys/termio.h
**              and sys/unistd.h. Added the include file string.h.
**
**              8/17/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**              This file was renamed from ims_passwd.c. Moved the function
**              ims_getString() to this file from ims_util.c.
**
******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/termios.h>

#include <ims_const.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_getInput.h.
** They are listed here for reference.
**
**	char *ims_getString (int, char *, int, char *);
**	char *ims_getPassword (char *);
*/

/******************************************************************************
**
** ims_getString ()
**
** Get user input and make sure it fits into the user's
** buffer.  Also, forces the user to provide input if
** the first argument is IMS_TRUE.
**
** Returns a pointer to the user's data buffer if
** successful, or (char *) NULL if end of file or error.
**
******************************************************************************/

char *ims_getString (
	int mustInput,
	char *outBuffer,
	int outLen,
	char *prompt)
{
	int inputLen;
	char buffer[IMS_INPUT_BUF_LEN+1];	/* Temporary input buffer. */
	
	/*
	** If outBuffer pointer is null than return null.
	*/
	if (outBuffer == (char *) NULL)
	{
		return ((char *) NULL);
	}

	/*
	** Do forever loop.
	**
	** lint: warning: constant in conditional context
	** It does what we want.
	*/
	while (1)
	{
		/* Initialize our buffer to the null string. */
		buffer[0] = '\0';

		/*
		** If prompt is null than don't display the prompt.
		*/
		if (prompt != (char *) NULL)
		{
			(void) printf ("%s", prompt);
		}

		/*
		** Get input from user.  Use gets () with maximum sized buffer
		** to easily make sure the input buffer is flushed to EOF.
		** Note: we use our own input buffer for this to prevent
		** core dumps.
		*/
		if (gets (buffer) == (char *) NULL)
		{
			return ((char *) NULL);
		}

		/*
		** Save the length of the actual input.
		*/
		inputLen = strlen (buffer);

		/*
		** Make sure there is enough room in the users output buffer
		** for the user's input, plus '\0'.
		*/
		if (inputLen >= outLen)
		{
			(void) printf (
				"Input length of '%d' exceeded.  Please try again.\n",
				outLen - 1);

			continue;	/* Retry user input. */
		}

		/*
		** Try again if the caller desires non-null input.
		*/
		if ((mustInput == IMS_TRUE) && (inputLen == 0))
		{
			continue;
		}

		/* We're done if we get here. */
		break;
	}

	/*
	** At this point, we have input that will fit into the user's buffer.
	** So, copy our stuff in and return a pointer to the users buffer.
	*/
	(void) strcpy (outBuffer, buffer);

	return (outBuffer);
}

/******************************************************************************
**
** ims_getPassword ()
**
** Get the password from the terminal while toggling the ECHO off and on.
**
** 11/8/93 - Replaced calls to ims_ioctl() with calls to tcgetattr() and
** tcsetattr(). Also replaced the structure sgttyb with termios.
**
******************************************************************************/

char *ims_getPassword (
	char *password)
{
	struct termios tinfo_save;      /* Use to save terminal characteristics. */
	struct termios termbuf;         /* Use to set input echo off. */
	char buffer[IMS_INPUT_BUF_LEN+1]; /* Temporary input buffer. */
	char *readstat;
	register char *bufptr;
	char *prompt = "Password: ";
	void (*func)();                 /* Save previous interrupt handler. */
	int fd;
	
	/*
	** If password pointer is null than return null.
	*/
	if (password == (char *) NULL)
	{
		return ((char *) NULL);
	}

	/*
	** Save terminal I/O characteristics.
	*/
	if (tcgetattr (STDIN_FILENO, &tinfo_save) != 0)
	{
		return ((char *) NULL);
	}

	termbuf = tinfo_save;

	/*
	** Save previous interrupt handler and turn off interrupt.
	*/
	func = signal (SIGINT, SIG_IGN);

	/*
	** Turn input echo off for reading password.
	*/
	termbuf.c_lflag &= ~(ECHO);

	if (tcsetattr (STDIN_FILENO, TCSAFLUSH, &termbuf) != 0)
	{
		return ((char *) NULL);
	}

	(void) strcpy (buffer, "");

	if ((fd = open ("/dev/tty", O_RDWR)) == -1)
	{
		(void) fprintf (stderr, "Cannot open /dev/tty for writing\n");
		perror ("/dev/tty");
		return ((char *) NULL);
	}

	/*
	** Display the prompt.
	*/
	(void) write (fd, prompt, strlen(prompt));
	(void) fflush (stdout);

	/*
	** Get the password.
	*/
	readstat = gets (buffer);
	(void) write (fd,"\n",1);  /* Move cursor down to next line. */
	(void) close (fd);

	/*
	** Restore previous terminal I/O characteristics.
	*/
	if (tcsetattr (STDIN_FILENO, TCSANOW, &tinfo_save) != 0)
	{
		return ((char *) NULL);
	}

	(void) strcpy (password, buffer);

	/*
	** Clear out password typed by user in the buffer area for security.
	*/
	for (bufptr=buffer; *bufptr; bufptr++)
	{
		*bufptr = '\0';
	}

	/*
	** Restore previous interrupt handler
	*/
	(void) signal (SIGINT, func);

	if (readstat == (char *) NULL)
	{
		return ((char *) NULL);
	}

	return (password);
}
