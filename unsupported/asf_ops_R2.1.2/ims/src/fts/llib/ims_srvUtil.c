static char *sccs = "@(#)ims_srvUtil.c	5.3  04/08/97";
/******************************************************************************
**
** File:        ims_srvUtil.c
**
** Function:    Miscellaneous routines used by FTS software.
**
** Author:      Hoshyar Sayah
**
** Modified:    1/5/94 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Added the include files string.h, errno.h and stdlib.h.
**              Removed the include file strings.h. Replaced the use of
**              sys_errlist[] with strerror().
**
**              8/17/94 - H. Sayah, S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              12/12/94 - S. Hardman - R1B
**              Ported to Sybase System 10. See the Open Server
**              Server-Library/C Reference Manual for more information.
**
**              03/03/95 - D. Crichton - R1B
**              Access pathPolicy field for building the pathname in
**              the srvOpenFile function.  This field is setup in the
**              addEventBegin and replaceEventBegin functions.
**
**				4/4/95 - D. Crichton - R1B
**				Add RPC module to send the filetypes back to the client.
******************************************************************************/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <ims_dbms.h>
#include <ospublic.h>
#include <oserror.h>

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_hash.h>
#include <ims_archive.h>
#include <ims_ftsSrvCat.h>
#include <ims_ftsSrv.h>
#include <ims_util.h>
/* #include <ims_krb.h> */

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_ftsSrv.h.
** They are listed here for reference.
**
**	int ims_srvAcceptKrbTicket (SRV_PROC *);
**	int ims_srvAcceptFile (SRV_PROC *);
**	int ims_srvSendFile (SRV_PROC *);
**	int ims_srvSendFileName (SRV_PROC *);
**	int ims_srvOpenFile (SRV_PROC *);
**  int ims_srvCloseFile (SRV_PROC *);
*/

static int getFileSize (char *name);

/******************************************************************************
**
** ims_srvAcceptKrbTicket ()
**
** Called from the server side to accept a Kerberos ticket from client.
** The counter part routine is ims_clientSendKrbTicket().
**
******************************************************************************/

#ifdef KRB
int ims_srvAcceptKrbTicket (
	SRV_PROC *srvproc)
{
	DBINT byteCount;
	DBINT index;
	FTS_PROC_DESC *procDesc;
	IMS_SERVER_KRB *sKrb;
	CS_DATAFMT paramFormat;
	CS_SERVERMSG srvMsg;
	char msg[CS_MAX_MSG];
	int length1;
	int length2;
	int length3;
	int paramCount;

	procDesc = (FTS_PROC_DESC *) NULL;
	length1 = length2 = length3 = 0;
	paramCount = 0;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_srvAcceptKrbTicket.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_srvAcceptKrbTicket.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}
	
	sKrb = &(procDesc->sKrb);
	sKrb->ktxt.length = 0;

	/*
	** Initialize the CS_DATAFMT structure.
	*/
	srv_bzero ((CS_VOID *) &paramFormat, (CS_INT) sizeof (paramFormat));

	/*
	** Bind the parameters with their descriptions.
	*/

	/* byteCount */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat, (CS_BYTE *) &byteCount,
		(CS_INT *) &length1, CS_GOODDATA);

	/* index */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat, (CS_BYTE *) &index,
		(CS_INT *) &length2, CS_GOODDATA);
	
	/* buffer */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 3, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 3, &paramFormat,
		(CS_BYTE *) &(sKrb->ktxt.dat[sKrb->ktxt.length]),
		(CS_INT *) &length3, CS_GOODDATA);
	
	/*
	** Get the parameters from the client.
	*/
	if (srv_xferdata (srvproc, CS_GET, SRV_RPCDATA) == CS_FAIL)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL
			"Could not get parameter data in ims_srvAcceptKrbTicket.");
		return (IMS_FATAL);
	}

	/*
	** Determine the number of parameters passed in and whether they
	** exceed their maximum lengths.
	*/
	(CS_VOID) srv_numparams (srvproc, (CS_INT *) &paramCount);

	if ((paramCount < 3) ||
		(length1 != sizeof (byteCount)) ||
		(length2 != sizeof (index)) ||
		(length3 == 0) || (length3 > IMS_MAX_BUFFER_SIZE))
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL
			"Error in parameter list in ims_srvAcceptKrbTicket.");
		return (IMS_FATAL);
	}

	sKrb->ktxt.length += byteCount;
	return (IMS_OK);
}
#endif	/* KRB */

/******************************************************************************
**
** ims_srvAcceptFile ()
**
** Called from the server side to read a file sent by client.
** The counter part routine is ims_clientSendFile().
**
******************************************************************************/

int ims_srvAcceptFile (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	FTS_GRANULE_DESC *granuleDesc;
	FTS_FILE_LIST *currFile;
	CS_SERVERMSG srvMsg;
	CS_DATAFMT paramFormat;
	CS_BINARY buffer[IMS_MAX_BUFFER_COUNT][IMS_MAX_BUFFER_SIZE];
	int fd;
	int iindex;
	int buffCount;
	int byteCount;
	int lastBuffSize;
	char *fileToWrite;
	int i;
	char msg[CS_MAX_MSG];
	int length1;
	int length2;
	int length3;
	int length[IMS_MAX_BUFFER_COUNT];
	int paramCount;

	procDesc = (FTS_PROC_DESC *) NULL;
	length1 = length2 = length3 = 0;
	paramCount = 0;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_srvAcceptFile.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_srvAcceptFile.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	granuleDesc = &(procDesc->granuleDesc);
	currFile = granuleDesc->currFile;
	fileToWrite = currFile->fileToWrite;

	if (currFile->isFileOpen == IMS_FALSE)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"File is not open in ims_srvAcceptFile: '%s'.", fileToWrite);
		return (IMS_FATAL);
	}

	/*
	** Clean out the buffer data areas.
	*/
	srv_bzero ((CS_VOID *) buffer,
		(IMS_MAX_BUFFER_COUNT * IMS_MAX_BUFFER_SIZE));

	/*
	** Initialize the CS_DATAFMT structure.
	*/
	srv_bzero ((CS_VOID *) &paramFormat, (CS_INT) sizeof (paramFormat));

	/*
	** Bind the parameters with their descriptions.
	*/

	/* buffCount */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat, (CS_BYTE *) &buffCount,
		(CS_INT *) &length1, CS_GOODDATA);

	/* iindex */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat, (CS_BYTE *) &iindex,
		(CS_INT *) &length2, CS_GOODDATA);
	
	/* lastBuffSize */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 3, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 3, &paramFormat, (CS_BYTE *) &lastBuffSize,
		(CS_INT *) &length3, CS_GOODDATA);
	
	/* buffer[i] */
	for (i = 0; i < IMS_MAX_BUFFER_COUNT; i++)
	{
		(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
			(CS_INT) i+4, &paramFormat);

		(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
			(CS_INT) i+4, &paramFormat, (CS_BYTE *) buffer[i],
			(CS_INT *) &length[i], CS_GOODDATA);
	}

	/*
	** Get the parameters from the client.
	*/
	if (srv_xferdata (srvproc, CS_GET, SRV_RPCDATA) == CS_FAIL)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"Could not get parameter data in ims_srvAcceptFile.");
		return (IMS_FATAL);
	}

	/*
	** Determine the number of parameters passed in and whether they
	** exceed their maximum lengths.
	*/
	(CS_VOID) srv_numparams (srvproc, (CS_INT *) &paramCount);

	if ((paramCount < 4) ||
		(length1 != sizeof (buffCount)) ||
		(length2 != sizeof (iindex)) ||
		(length3 != sizeof (lastBuffSize)))
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"Error in parameter list in ims_srvAcceptFile.");
		return (IMS_FATAL);
	}

	if (buffCount > IMS_MAX_BUFFER_COUNT)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"Exceeded maximum number of buffers in ims_srvAcceptFile.");
		return (IMS_FATAL);
	}

	if (iindex != (currFile->fileIndex + 1))
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"File is out of sequence in ims_srvAcceptFile.");
		return (IMS_FATAL);
	}

	fd = currFile->fd;
	currFile->fileIndex = iindex;

	for (i = 0; i < buffCount; i++)
	{
		byteCount = length[i];
		if (((i == buffCount - 1) && (byteCount != lastBuffSize)) ||
			((i != buffCount - 1) && (byteCount != IMS_MAX_BUFFER_SIZE)))
		{
			(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
				"Buffer byte count is incorrect in ims_srvAcceptFile.");
			return (IMS_FATAL);
		}

		if (write (fd, buffer[i], byteCount) != byteCount)
		{
			(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
				"Write into file failed in ims_srvAcceptFile.");
			return (IMS_FATAL);
		}
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_srvSendFile ()
**
** Called from the server side to send a file to the client.
** The counterpart routine is ims_clientAcceptFile ().
**
******************************************************************************/

int ims_srvSendFile (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	FTS_GRANULE_DESC *granuleDesc;
	FTS_FILE_LIST *currFile;
	CS_SERVERMSG srvMsg;
	CS_DATAFMT paramFormat;
	CS_BINARY buffer[IMS_MAX_BUFFER_COUNT][IMS_MAX_BUFFER_SIZE];
	int fd;
	int iindex;
	int buffCount;
	int lastBuffSize;
	int byteCount;
	char *fullPathName;
	int i;
	int endOfFile;
	char msg[CS_MAX_MSG];
	int length1;
	int length2;
	int length3;
	int length[IMS_MAX_BUFFER_COUNT];
	int fileSize;

	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_srvSendFile.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_srvSendFile.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	granuleDesc = &(procDesc->granuleDesc);
	currFile = granuleDesc->currFile;

	/*
	** Open the file to send.
	*/
	fullPathName = currFile->fullPathName;

	fileSize = getFileSize(fullPathName);

	if ((fd = open (fullPathName, O_RDONLY, 00400)) == -1)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"Could not open to read file '%s' for '%s, %s, %s'. Possible policy conflict or installation failure. Contact DBA; %s.",
			ims_extractFileName (fullPathName),
			procDesc->sensorPolicy->platform,
			procDesc->sensorPolicy->sensor,
			procDesc->datasetPolicy->dataset, strerror (errno));
		return (IMS_FATAL);
	}

	/*
	** Initialize the CS_DATAFMT structure.
	*/
	srv_bzero ((CS_VOID *) &paramFormat, (CS_INT) sizeof (paramFormat));
	paramFormat.namelen = 0;

	/*
	** Bind the parameters with their descriptions.
	*/

	/* buffCount */
	paramFormat.datatype = CS_INT_TYPE;
	paramFormat.maxlength = (CS_INT) sizeof (buffCount);
	length1 = sizeof (buffCount);

	(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 1, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 1, &paramFormat, (CS_BYTE *) &buffCount,
		(CS_INT *) &length1, CS_GOODDATA);

	/* iindex */
	paramFormat.datatype = CS_INT_TYPE;
	paramFormat.maxlength = (CS_INT) sizeof (iindex);
	length2 = sizeof (iindex);

	(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 2, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 2, &paramFormat, (CS_BYTE *) &iindex,
		(CS_INT *) &length2, CS_GOODDATA);

	/* lastBuffSize */
	paramFormat.datatype = CS_INT_TYPE;
	paramFormat.maxlength = (CS_INT) sizeof (lastBuffSize);
	length3 = sizeof (lastBuffSize);

	(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 3, &paramFormat);
	
	(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 3, &paramFormat, (CS_BYTE *) &lastBuffSize,
		(CS_INT *) &length3, CS_GOODDATA);

	/* buffer[i] */
	paramFormat.datatype = CS_BINARY_TYPE;
	paramFormat.maxlength = IMS_MAX_BUFFER_SIZE;
	paramFormat.status = CS_CANBENULL;

	for (i = 0; i < IMS_MAX_BUFFER_COUNT; i++)
	{
		(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA,
			(CS_INT) i+4, &paramFormat);
	}

	/*
	** Send as many rows back to the client as needed to transfer
	** the entire file.
	*/
	iindex = 0;
	endOfFile = 0;
	while (endOfFile != 1)
	{
		/* Clean out the buffer data areas. */
		srv_bzero ((CS_VOID *) buffer,
			(IMS_MAX_BUFFER_COUNT * IMS_MAX_BUFFER_SIZE));

		/*
		** Fill as many of the buffer columns (up to the maximum)
		** as needed with data from the file to be transfered.
		*/
		iindex++;
		buffCount=0;
		byteCount = IMS_MAX_BUFFER_SIZE;
		lastBuffSize = 0;


		while ((buffCount < IMS_MAX_BUFFER_COUNT) && 
			(byteCount == IMS_MAX_BUFFER_SIZE) && 
			(fileSize > 0))
			
		{
			/* Read a segment of the file into the buffer. */
			if ((byteCount = read (fd, (char *) buffer[buffCount],
				(int) IMS_MAX_BUFFER_SIZE)) == -1)
			{
				(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
					"ims_srvSendFile: Read error in file '%s'; %s",
					fullPathName, strerror (errno));
				(void) close (fd);
				return (IMS_FATAL);
			}

			 
			fileSize -= byteCount;

			/*
			** Check for case where we already sent the entire file
			** and this pass indicates there are no chunks left.
			*/


			if (byteCount > IMS_MAX_BUFFER_SIZE)
			{
				(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
					"ims_srvSendFile: Byte count error reading file '%s'.", 
					fullPathName);
				(void) close (fd);
				return (IMS_FATAL);
			}

			/* Bind the filled buffer columns. */
			length[buffCount] = byteCount;
			(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA,
				(CS_INT) buffCount+4, &paramFormat,
				(CS_BYTE *) buffer[buffCount],
				(CS_INT *) &length[buffCount], CS_GOODDATA);
	
			buffCount++;	
		}

		/*
		** If the byteCount is not the max, than we reached the 
		** end of the file.
		*/
    if ((lastBuffSize = byteCount) < IMS_MAX_BUFFER_SIZE)
		{
			endOfFile = 1;
   	}


		if (fileSize <= 0)
		{
			endOfFile = 1;
		}

		/*
		** Bind the unfilled buffer columns.
		**
		** Due to my inability to figure out how to send a null
		** row back to the client I am binding the row with a
		** length of 1. This does not affect the client because
		** these columns are ignored.
		*/
		for (i = buffCount; i < IMS_MAX_BUFFER_COUNT; i++)
		{
			length[i] = 1;
			(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA,
				(CS_INT) i+4, &paramFormat, (CS_BYTE *) buffer[i],
				(CS_INT *) &length[i], CS_GOODDATA);
		}


		/*
		** Send the row to the client.
		*/
		if (srv_xferdata (srvproc, CS_SET, SRV_ROWDATA) == CS_FAIL)
		{
			(void) close (fd);
			procDesc->cancelFlag = 1;
			(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
				"Could not send row data in ims_srvSendFile.");
			return (IMS_FATAL);
		}
	}

	(void) close (fd);
	return (IMS_OK);
}

/******************************************************************************
**
** ims_srvSendFileName ()
**
** Send the fileName to the client. This routine is called by the getFile
** remote procedure handler.
**
******************************************************************************/

int ims_srvSendFileName (
	SRV_PROC *srvproc) 
{
	FTS_PROC_DESC *procDesc;
	FTS_CAT_STRUCT *catReq;
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_GRANULE_DESC *granuleDesc;
	FTS_FILE_LIST *currFile;
	CS_SERVERMSG srvMsg;
	CS_DATAFMT paramFormat;
	CS_INT lastFileFlag;
	char *name;
	char *extension;
	char msg[CS_MAX_MSG];
	int length1;
	int length2;
	int length3;

	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_srvSendFileName.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_srvSendFileName.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	catReq = &(procDesc->catReq);
	granuleSpec = catReq->granuleSpec;
	granuleDesc = &(procDesc->granuleDesc);
	currFile = granuleDesc->currFile;

	name = granuleSpec->name;
	extension = currFile->extension;

	/*
	** If the pointer to the next file record is NULL, then we must
	** be processing the last file.  So set the flag accordingly.
	*/
	lastFileFlag = 0;
	if (currFile->next == (FTS_FILE_LIST *) NULL)
	{
		lastFileFlag = 1;
	}

	/*
	** Initialize the CS_DATAFMT structure.
	*/
	srv_bzero ((CS_VOID *) &paramFormat, (CS_INT) sizeof (paramFormat));
	paramFormat.namelen = 0;

	/*
	** Bind the columns with their descriptions.
	*/

	/* name */
	paramFormat.datatype = CS_CHAR_TYPE;
	paramFormat.maxlength = IMS_COL30_LEN;
	length1 = strlen (name);

	(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 1, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 1, &paramFormat, (CS_BYTE *) name,
		(CS_INT *) &length1, CS_GOODDATA);

	/* extension */
	paramFormat.datatype = CS_CHAR_TYPE;
	paramFormat.maxlength = IMS_COL10_LEN;
	length2 = strlen (extension);

	(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 2, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 2, &paramFormat, (CS_BYTE *) extension,
		(CS_INT *) &length2, CS_GOODDATA);
	
	/* lastFileFlag */
	paramFormat.datatype = CS_INT_TYPE;
	paramFormat.maxlength = (CS_INT) sizeof (lastFileFlag);
	length3 = sizeof (lastFileFlag);

	(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 3, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 3, &paramFormat, (CS_BYTE *) &lastFileFlag,
		(CS_INT *) &length3, CS_GOODDATA);
	
	/*
	** Send the row to the client.
	*/
	if (srv_xferdata (srvproc, CS_SET, SRV_ROWDATA) == CS_FAIL)
	{
		procDesc->cancelFlag = 1;
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"Could not send row data in ims_srvSendFileName.");
		return (IMS_FATAL);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_srvOpenFile ()
**
** Accept and open a requested file. 
**
******************************************************************************/

int ims_srvOpenFile (
	SRV_PROC *srvproc) 
{
	FTS_PROC_DESC *procDesc;
	FTS_CAT_STRUCT *catReq;
	FTS_CLIENT_REQUEST *clientReq;
	FTS_GRANULE_DESC *granuleDesc;
	FTS_FILE_LIST *currFile;
	FTS_FILE_LIST *fileList;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_SENSOR_POLICY *sensorPolicy;
	FTS_FILE_POLICY *filePolicy;
	FTS_FORMAT_POLICY *formatPolicy;
	FTS_CAT_GRANULESPEC *granuleSpec;
	CS_SERVERMSG srvMsg;
	CS_DATAFMT paramFormat;
	char name[IMS_COL30_LEN+1];
	char extension[IMS_COL10_LEN+1];
	char fileName[IMS_COL60_LEN+1];
	int matched;
	char msg[CS_MAX_MSG];
	int length1;
	int length2;
	int paramCount;

	procDesc = (FTS_PROC_DESC *) NULL;
	length1 = length2 = 0;
	paramCount = 0;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_srvOpenFile.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_srvOpenFile.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	catReq = &(procDesc->catReq);
	clientReq = &(procDesc->clientReq);
	datasetPolicy = procDesc->datasetPolicy;
	sensorPolicy = procDesc->sensorPolicy;
	filePolicy = procDesc->filePolicy;
	formatPolicy = filePolicy->formatPolicy;
	granuleSpec = catReq->granuleSpec;
	granuleDesc = &(procDesc->granuleDesc);

	/*
	** Initialize the CS_DATAFMT structure.
	*/
	srv_bzero ((CS_VOID *) &paramFormat, (CS_INT) sizeof (paramFormat));

	/*
	** Bind the parameters with their descriptions.
	*/

	/* name */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat, (CS_BYTE *) name,
		(CS_INT *) &length1, CS_GOODDATA);

	/* extension */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat, (CS_BYTE *) extension,
		(CS_INT *) &length2, CS_GOODDATA);
	
	/*
	** Get the parameters from the client.
	*/
	if (srv_xferdata (srvproc, CS_GET, SRV_RPCDATA) == CS_FAIL)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"Could not get parameter data in ims_srvOpenFile.");
		return (IMS_FATAL);
	}

	/*
	** Determine the number of parameters passed in and whether they
	** exceed their maximum lengths.
	*/
	(CS_VOID) srv_numparams (srvproc, (CS_INT *) &paramCount);

	if ((paramCount < 2) ||
		(length1 == 0) || (length1 > IMS_COL30_LEN) ||
		(length2 == 0) || (length2 > IMS_COL10_LEN))
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"Error in parameter list in ims_srvOpenFile.");
		return (IMS_FATAL);
	}

	/*
	** Null terminate the character parameters.
	*/
	name[length1] = '\0';
	extension[length2] = '\0';

	/*
	** Allocate space for the FTS_FILE_LIST structure.
	**
	** lint: pointer cast may result in improper alignment
	** ???? Review this.
	*/
	if ((currFile = (FTS_FILE_LIST *) malloc (sizeof (FTS_FILE_LIST)))
		== (FTS_FILE_LIST *) NULL)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"ims_srvOpenFile: allocation for FTS_FILE_LIST structure failed.");
		return (IMS_FATAL);
	}

	/*
	** Initialize the structure members.
	*/
	currFile->next = (FTS_FILE_LIST *) NULL;
	currFile->isFileOpen = IMS_FALSE;
	currFile->hasFileBeenOpen = IMS_FALSE;
	currFile->fileIndex = 0;
	currFile->fd = -1;
	(void) strcpy (currFile->extension, extension);

	/*
	** Verify the granule's name.
	*/
	if (strcmp (name, clientReq->name) != 0)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"ims_srvOpenFile: requested file name '%s' is not valid.", 
			name);
		(void) free (currFile);
		return (IMS_FATAL);
	}

	/*
	** Verify the file's extension.
	*/
	matched = 0;
	while (formatPolicy != (FTS_FORMAT_POLICY *)NULL && !matched)
	{
		if (strcmp (formatPolicy->extension, currFile->extension) == 0)
		{
			matched = 1;
			break;
		}
		formatPolicy = formatPolicy->next;
	}

	if (!matched) 
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"ims_srvOpenFile: requested file extension '%s' does not match policy.", 
			currFile->extension);
		(void) free (currFile);
		return (IMS_FATAL);
	}

	/*
	** Concatenate the complete file name.
	*/
	if (granuleSpec->version > 0)
	{
		(void) sprintf (fileName, "%s.%s.%d",
			granuleSpec->name, currFile->extension, granuleSpec->version);
	}
	else
	{
		(void) sprintf (fileName, "%s.%s",
			granuleSpec->name, currFile->extension);
	}

	/*
	** Append name stamp if there is one.
	*/

	if (granuleSpec->name_stamp[0] !='\0')
	{
		strcat(fileName, granuleSpec->name_stamp);
	}

	ims_concatFilePath (currFile->fullPathName,
		procDesc->pathPolicy->path, fileName);


	(void) strcpy (currFile->fileToWrite, currFile->fullPathName);

	/*
	** Insert the currFile pointer into list of files. 
	*/
	fileList = granuleDesc->fileList;
	if (fileList == (FTS_FILE_LIST *)NULL)
	{
		granuleDesc->fileList = currFile;
		granuleDesc->currFile = currFile;
		granuleDesc->fileCount = 1;
	}
	else
	{
		granuleDesc->fileCount += 1;
		granuleDesc->currFile = currFile;

		while (fileList->next != (FTS_FILE_LIST *)NULL)
		{
			fileList = fileList->next;
		}
		fileList->next = currFile;
	}

	/*
	** Open the file in the repository directory and save the file
	** descriptor in the procDesc structure. This way we don't have to
	** open the file for every file transfer event processed.
	** Just remember to close the file after the file transfer request
	** is complete.
	*/

	if ((currFile->fd = open (currFile->fullPathName,
		O_WRONLY|O_CREAT|O_EXCL|O_TRUNC, 00666)) == -1)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"Could not open to write file '%s' for '%s, %s, %s'. Possible policy conflict or installation failure. Contact DBA; %s.",
			currFile->fullPathName,
			sensorPolicy->platform, sensorPolicy->sensor,
			datasetPolicy->dataset, strerror (errno));
		return (IMS_FATAL);
	}
	currFile->isFileOpen = IMS_TRUE;
	currFile->hasFileBeenOpen = IMS_TRUE;

	return (IMS_OK);
}

/******************************************************************************
**
** ims_srvCloseFile ()
**
** Close requested file. 
**
******************************************************************************/

int ims_srvCloseFile (
	SRV_PROC *srvproc) 
{
	FTS_PROC_DESC *procDesc;
	FTS_CLIENT_REQUEST *clientReq;
	FTS_GRANULE_DESC *granuleDesc;
	FTS_FILE_LIST *currFile;
	CS_SERVERMSG srvMsg;
	CS_DATAFMT paramFormat;
	char name[IMS_COL30_LEN+1];
	char extension[IMS_COL10_LEN+1];
	char msg[CS_MAX_MSG];
	int length1;
	int length2;
	int paramCount;

	procDesc = (FTS_PROC_DESC *) NULL;
	length1 = length2 = 0;
	paramCount = 0;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_srvCloseFile.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_srvCloseFile.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	clientReq = &(procDesc->clientReq);
	granuleDesc = &(procDesc->granuleDesc);
	currFile = granuleDesc->currFile;

	/*
	** Initialize the CS_DATAFMT structure.
	*/
	srv_bzero ((CS_VOID *) &paramFormat, (CS_INT) sizeof (paramFormat));

	/*
	** Bind the parameters with their descriptions.
	*/

	/* name */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat, (CS_BYTE *) name,
		(CS_INT *) &length1, CS_GOODDATA);

	/* extension */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat, (CS_BYTE *) extension,
		(CS_INT *) &length2, CS_GOODDATA);
	
	/*
	** Get the parameters from the client.
	*/
	if (srv_xferdata (srvproc, CS_GET, SRV_RPCDATA) == CS_FAIL)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"Could not get parameter data in ims_srvCloseFile.");
		return (IMS_FATAL);
	}

	/*
	** Determine the number of parameters passed in and whether they
	** exceed their maximum lengths.
	*/
	(CS_VOID) srv_numparams (srvproc, (CS_INT *) &paramCount);

	if ((paramCount < 2) ||
		(length1 == 0) || (length1 > IMS_COL30_LEN) ||
		(length2 == 0) || (length2 > IMS_COL10_LEN))
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"Error in parameter list in ims_srvCloseFile.");
		return (IMS_FATAL);
	}

	/*
	** Null terminate the character parameters.
	*/
	name[length1] = '\0';
	extension[length2] = '\0';

	/*
	** Verify the file name and extension.
	*/
	if ((strcmp (name, clientReq->name) != 0) ||
			(strcmp (extension, currFile->extension) != 0))
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"ims_srvCloseFile: requested file '%s.%s' is not valid.", 
			name, extension);
		return (IMS_FATAL);
	}

	/* 
	** Verify that the file is open
	*/
	if (currFile->isFileOpen != IMS_TRUE)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"ims_srvCloseFile: requested file '%s.%s' is not open.", 
			name, extension);
		return (IMS_FATAL);
	}

	/*
	** Close the file 
	*/
	(void) close (currFile->fd);
	currFile->fd = -1;
	currFile->isFileOpen = IMS_FALSE;

	return (IMS_OK);
}

/******************************************************************************
**
** ims_sendFileTypes ()
**
** Send file type list to the client. 
**
******************************************************************************/

int ims_sendFileTypes (
	SRV_PROC *srvproc,
	IMS_FILE_TYPES *fileTypes) 
{
	FTS_PROC_DESC *procDesc;
	CS_SERVERMSG srvMsg;
	CS_DATAFMT paramFormat;
	IMS_FILE_TYPES *ptr;
	char msg[CS_MAX_MSG];
	int *length;
	int countTypes;
	int i;

	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_SendFileTypes.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{

		(void) sprintf (msg,
			"Process descriptor is NULL in ims_SendFileTypes.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}


	/*
	** Marshal parameters for return of critical RPC data.
	*/

	srv_bzero((CS_VOID *) &paramFormat, (CS_INT) sizeof(paramFormat));
	paramFormat.namelen = 0;

	ptr = fileTypes;
	countTypes = 0;
	while (ptr != NULL)
	{
		countTypes ++;
		ptr = ptr->next;
	}

	/*
	** Grab memory to hold parameter lengths.
	** 4 items indicate the number of parameters for each
	** link of the list that is being returned to the client.
	** Take a look at the fileTypes structure to verify that
	** there are 4 fields plus the next link.
	*/

	length = malloc(sizeof(i) * (4 * countTypes + 1));

	if (length == NULL)
	{

		(void) sprintf (msg,
			"Could not allocate memory in ims_sendFileTypes.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Set up format descriptors for the returning list.     
	*/

	/*
	** Count
	*/

	i = 0;
	paramFormat.datatype = CS_INT_TYPE;
	paramFormat.maxlength = sizeof(countTypes);
	length[0] = sizeof(countTypes);

	(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA, 
			(CS_INT) 1, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA, (CS_INT) 1,
			&paramFormat, (CS_BYTE *) &(countTypes), 
			(CS_INT *) &length[0], CS_GOODDATA);


	/*
	** Scan through linked list of file types and send to client.
	*/


	ptr = fileTypes;
	i = 1;
	while (ptr != NULL)
	{

		/*
		** File Type 
		*/

		paramFormat.datatype = CS_SMALLINT_TYPE;
		paramFormat.maxlength = sizeof(ptr->type);
		length[i] = sizeof(ptr->type);
	
		(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA, 
				(CS_INT) i + 1, &paramFormat);
	
		(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA, (CS_INT) i + 1,
				&paramFormat, (CS_BYTE *) &(ptr->type), 
				(CS_INT *) &length[i], CS_GOODDATA);


		/*
		** Position
		*/

		paramFormat.datatype = CS_SMALLINT_TYPE;
		paramFormat.maxlength = sizeof(ptr->position);
		length[i + 1] = sizeof(ptr->position);
	
		(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA, 
					(CS_INT) i + 2, &paramFormat);


	
		(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA, (CS_INT) i + 2,
				&paramFormat, (CS_BYTE *) &(ptr->position), 
				(CS_INT *) &length[i+1], CS_GOODDATA);

		/*
		** Extension
		*/

		paramFormat.datatype = CS_CHAR_TYPE;
		paramFormat.maxlength = IMS_COL10_LEN + 1;
		length[i+2] = IMS_COL10_LEN + 1;

		(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA, 
					(CS_INT) i+3, &paramFormat);
		 
		(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA, (CS_INT) i+3,
				&paramFormat, (CS_BYTE *) ptr->extension, 
				(CS_INT *) &length[i+2], CS_GOODDATA);

		/*
		** Local Archive
		*/

		paramFormat.datatype = CS_CHAR_TYPE;
		paramFormat.maxlength = 1;
		length[i+3] = 1;

		(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA, 
					(CS_INT) i+4, &paramFormat);
		 
		(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA, (CS_INT) i+4,
				&paramFormat, (CS_BYTE *) &(ptr->localArchiveFlag), 
				(CS_INT *) &length[i+3], CS_GOODDATA);


		ptr = ptr->next;
		i = i + 4;


	}

	/*
	** Send row data to client
	*/

	if (srv_xferdata (srvproc, CS_SET, SRV_ROWDATA) == CS_FAIL)
	{
		procDesc->cancelFlag = 1;
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"Could not send row data in ims_SendFileTypes");

		return (IMS_FATAL);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_sendVersionNumber ()
**
** Send the version  number for the dataset to the client.
**
******************************************************************************/

int ims_sendVersionNumber (
	SRV_PROC *srvproc) 
{
	FTS_PROC_DESC *procDesc;
	FTS_CLIENT_REQUEST *clientReq;
	FTS_GRANULE_DESC *granuleDesc;
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_CAT_STRUCT *catReq;
	FTS_FILE_LIST *currFile;
	CS_SERVERMSG srvMsg;
	CS_DATAFMT paramFormat;
	char msg[CS_MAX_MSG];
	int version;
	int length1;
	int paramCount;

	procDesc = (FTS_PROC_DESC *) NULL;
	length1 = 0;
	paramCount = 0;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_sendVersionNumber.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_sendVersionNumber.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	catReq = &(procDesc->catReq);
	granuleSpec = catReq->granuleSpec;
	clientReq = &(procDesc->clientReq);
	granuleDesc = &(procDesc->granuleDesc);
	currFile = granuleDesc->currFile;

	/*
	** Initialize the CS_DATAFMT structure.
	*/
	srv_bzero ((CS_VOID *) &paramFormat, (CS_INT) sizeof (paramFormat));

	/*
	** Bind the parameters with their descriptions.
	*/

	paramFormat.datatype = CS_INT_TYPE;
	paramFormat.maxlength = sizeof(version);

	length1 = sizeof(version);
	version = granuleSpec->version;
	paramCount = 1;

	(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA, 
			(CS_INT) 1, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 1, &paramFormat, (CS_BYTE *) &version,
		(CS_INT *) &length1, CS_GOODDATA);

	/*
	** Send the parameters to the client.
	*/

	if (srv_xferdata (srvproc, CS_SET, SRV_ROWDATA) == CS_FAIL)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"Could not set parameter data in ims_sendVersionNumber.");
		return (IMS_FATAL);
	}


	return(IMS_OK);
}


/******************************************************************************
**
** getFileSize ()
**
******************************************************************************/

static int getFileSize (char *name)
{
	struct stat	st_buf;
	int adFd;

	if ((adFd = open (name, O_RDONLY, 00666)) == -1)
	{
		return (IMS_ERROR);
	}
	if (fstat(adFd, &st_buf) == -1)
	{
		(void) close (adFd);
		return (IMS_ERROR);
	}
	(void) close (adFd);

	return (st_buf.st_size);
}

