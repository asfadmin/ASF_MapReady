static char *sccs = "@(#)ims_fwMsg.c	5.2  03/07/97";
/******************************************************************************
**
** File:        ims_fwMsg.c
**
** Function:    The routine in this file is to be used by the auxiliary
**              processes to forward messages collected in the msgQueue to
**              the FTR process. Messages are reformated into a predefined
**              format acceptable by FTR.
**
** Author:      Hoshyar Sayah
**
** Date:        5/1/90
**
** Modified:    1/5/94 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Added the include file unistd.h. Removed the include file
**              strings.h.
**
**              3/10/95 - S. Hardman - R1B
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              1/29/96 - D. Crichton - R1B'
**              Increase buffer to 1024 to handle V0 messages.
**
******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <ims_const.h>
#include <ims_msg.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in the modules where they
** are called. They are listed here for reference.
**
**	int ims_forwardMsgQueue (IMS_MSG_STRUCT *);
*/

/******************************************************************************
**
** ims_forwardMsgQueue ()
**
** Find all messages in the msgQueue, reformat them into a predefined 
** formatted structure that is acceptable by the FTR process, and then forward
** the messages to the parent (FTR) process.  Note, this routine is to 
** be only used for communication between FTR and AUXP.  The predefined
** message format is: "MSG:integer:integer:LMF:message text\n". The first 
** integer number represents severity.  The second integer number represents
** error number.  LMF is the last message flag (0,1).
**  
******************************************************************************/

int ims_forwardMsgQueue (
	IMS_MSG_STRUCT *msgDesc)
{
	IMS_MSG_QUEUE *msgQueue;
	char msg[IMS_COL1024_LEN+1];

	/*
	** Loop until get and forward all messages in the msgQueue to stdout.
	** Note: stdout, represented by logical descriptor 1, is redirected to
	** either the duplex_pipe connected with FTR or the terminal, depending
	** on the auxiliary process application environment.  
	*/
	while ((msgQueue = ims_msgQueueExtract (msgDesc)) !=
		(IMS_MSG_QUEUE *) NULL)
	{
		(void) sprintf (msg, "MSG:%d::0:%s\n", msgQueue->severity,
			msgQueue->rawMsg);
		(void) write (1, msg, strlen (msg) + 1);
		(void) ims_msgQueueFree (msgQueue);

		/*
		** Delay to allow FTS server to dump the messages before sending
		** the close message.  We currently get spurious pre-mature process
		** termination errors. This seems to resolve it.
		*/
		(void) sleep (1);
		(void) fflush (stdout);
	}

	/*
	** Set the last message flag, LMF
	*/
	(void) sprintf (msg, "MSG:::1:Last Message\n");
	(void) write (1, msg, strlen (msg) + 1);

	return (IMS_OK);
}
