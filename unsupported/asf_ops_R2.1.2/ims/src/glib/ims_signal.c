static char *sccs = "@(#)ims_signal.c	5.1  17 Mar 1996";
/******************************************************************************
**
** File:        ims_signal.c
**
** Function:    Signal handling functions.
**
** Author:      J. Rector
**
** Date:        3/14/89
**
** Modified:    1/13/94 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Removed cdb_ignoreInt() because it was not called by another
**              function. The Port Team replaced calls to sigblock() and
**              sigmask() with calls to sigemptyset(), sigaddset() and
**              sigprocmask(). Also replaced a call to sigsetmask() with
**              sigprocmask().
**
**              8/17/94 - H. Sayah, S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <signal.h>

#include <ims_const.h>
#include <ims_msg.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_protos.h.
** They are listed here for reference.
**
**	int ims_setWrapup (int (*) (int));
**	int ims_blockSigs (void);
**	int ims_unBlockSigs (void);
**	char *ims_sigMsg (int);
*/

/*
** Local Functions
*/
static void ignoreSigs (int);
static void sigHandler (int);

/*
** Local Definitions
*/
#define SIGCOUNT 4	/* Number of signals reported in runDown */

/*
** These three variables hold the state of the original signal mask
** and the new blocked signal mask.
*/
static sigset_t initSigMask;
static sigset_t blockSigMask;
static int sigsBlocked = IMS_FALSE;

/*
** initialState holds the state of the process at the time
** 'setjmp' is called.  This state is restored when longjmp ()
** is called from the signal function, sigHandler ().
*/
static jmp_buf initialState;

/*
** sigRcved holds the signal number of a caught signal.  Used to
** pass signal to the user-supplied wrapup routine.
*/
static int sigRcved;

/******************************************************************************
**
** ims_setWrapup ()
**
** Execution steps:
** 1. Use setjmp to establish the run-down point and process state.
** 2. Declare user-supplied rundown function.
** 3. Return to caller.
** 4. On return from longjmp, execute user-supplied function "rundown".
**
******************************************************************************/

int ims_setWrapup (
	int (*rundown) (int))
{
	int status;
	static int (*func) (int);

	/*
	** Save user-supplied function.
	*/
	func = rundown;

	/*
	** Block the signals and then call setjmp.  When longjmp occurs, we
	** shall return to this point in this process state.
	** Note: When first call to setjmp will return a value of zero.  Only
	** on the return from longjmp will the return value be non-zero.
	** We put the run-down code in the first part of the if statement
	** so that following the longjmp it will be executed.
	*/
	if (setjmp (initialState) != 0)
	{
		/*
		** RUN-DOWN sequence. Get the program to a consistent state
		** before exiting.
		*/
		status = (func) (sigRcved);
		exit (status);
	}

	/*
	** Initialize the signal handler for SIGHUP, SIGINT, and SIGTERM.
	*/
	if (signal (SIGHUP, sigHandler) == IMS_BADSIG)
	{
		return (IMS_FATAL);
	}

	if (signal (SIGINT, sigHandler) == IMS_BADSIG)
	{
		return (IMS_FATAL);
	}

	if (signal (SIGTERM, sigHandler) == IMS_BADSIG)
	{
		return (IMS_FATAL);
	}

	if (signal (SIGQUIT, sigHandler) == IMS_BADSIG)
	{
		return (IMS_FATAL);
	}

	/*
	** Four signals have been setup for rundown.
	*/
	return (IMS_OK);
}

/******************************************************************************
**
** sigHandler ()
**
** Called for SIGHUP, SIGINT, SIGQUIT or SIGTERM when the signals are not
** blocked.  The object of this call is to force the program run-down
** procedure to start by returning from longjmp.
** Note: The signals that had handlers are set to ignore, so that the
** handler will not be called a second time.
**
** The second parameter to long jump will cause the setjmp if statement
** to execute (see the main routine).
**
******************************************************************************/

static void sigHandler (
	int sig)
{
	/*
	** Save caught signal, so it can be used in the wrapup routine.
	*/
	sigRcved = sig;

	/*
	** Ignore signals from now on.
	*/
	ignoreSigs (IMS_OFF);

	longjmp (initialState, 1);
}

/******************************************************************************
**
** ignoreSigs ()
**
** Set the signals SIGHUP, SIGINT, SIGQUIT and SIGTERM to be ingnored.
**
** Argument:	termflag - IMS_OFF -> Ignore SIGTERM too.
**                         IMS_ON  -> Leave SIGTERM "on".
**
******************************************************************************/

static void ignoreSigs (
	int termflag)
{
	(void) signal (SIGHUP, SIG_IGN);
	(void) signal (SIGINT, SIG_IGN);
	(void) signal (SIGQUIT, SIG_IGN);

	/* Turn off term handling. */
	if (termflag == IMS_OFF)
	{
		(void) signal (SIGTERM, SIG_IGN);
	}
}

/******************************************************************************
**
** ims_blockSigs ()
**
** Block the signals SIGHUP, SIGINT, SIGTERM and
** and SIGQUIT.  Also, save the current sigmask.
**
** Note:  This routine is a no-op if called when
** signals are already blocked.
**
******************************************************************************/

int ims_blockSigs (void)
{
	/*
	** NO-OP if signals are already blocked.
	*/
	if (sigsBlocked == IMS_TRUE)
	{
		return (IMS_WARNING);
	}

	/*
	** Add these signals to the blocked set, and save the previous signal
	** mask.
	*/
	(void) sigemptyset (&blockSigMask);
	(void) sigaddset (&blockSigMask, SIGHUP);
	(void) sigaddset (&blockSigMask, SIGTERM);
	(void) sigaddset (&blockSigMask, SIGINT);
	(void) sigaddset (&blockSigMask, SIGQUIT);
	(void) sigprocmask (SIG_BLOCK, &blockSigMask, &initSigMask);

	/*
	** Mark as blocked.  We're safe here, because we are currently blocked.
	*/
	sigsBlocked = IMS_TRUE;

	return (IMS_OK);
}

/******************************************************************************
**
** ims_unBlockSigs ()
**
** Restore the signal mask saved during the
** last call to ims_blockSigs.
**
** Note:  This routine is a no-op if called when
** signals are not blocked.
**
******************************************************************************/

int ims_unBlockSigs (void)
{
	/*
	** NO-OP if signals not blocked.
	*/
	if (sigsBlocked == IMS_FALSE)
	{
		return (IMS_WARNING);
	}

	/*
	** Mark as unblocked.  We're safe here, because we are currently
	** blocked.
	*/
	sigsBlocked = IMS_FALSE;

	/*
	** Restore the previous signal mask.
	*/
	(void) sigprocmask (SIG_SETMASK, &initSigMask, NULL);

	return (IMS_OK);
}

/******************************************************************************
**
** ims_sigMsg ()
**
******************************************************************************/

char *ims_sigMsg (
	int sig)
{
	int maxSig;
	static char *sigMsg[] =
	{
		"Unknown Signal",
		"Hangup",
		"Interrupt",
		"Quit",
		"Illegal Instruction",
		"Trace trap",
		"IOT instruction",
		"EMT instruction",
		"Floating point exception",
		"Kill",
		"Bus error",
		"Segmentation violation",
		"Bad arg to system call",
		"Write on pipe",
		"Alarm clock",
		"Terminate",
		"User signal 1",
		"User signal 2",
		"Death of child",
		"Power fail"
	};

	maxSig = (sizeof (sigMsg) / sizeof (sigMsg[0])) - 1;

	if (sig >= 1 && sig <= maxSig)
	{
		return (sigMsg[sig]);
	}
	else
	{
		return (sigMsg[0]);
	}

}
