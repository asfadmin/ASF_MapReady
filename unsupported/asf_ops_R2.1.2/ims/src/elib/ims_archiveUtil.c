static char *sccs = "@(#)ims_archiveUtil.c	5.4  03/26/98";
/******************************************************************************
**
** File:        ims_archiveUtil.c
**
** Function:    Miscellaneous routines used by the FTS client software.
**
** Author:      Hoshyar Sayah
**
** Date:        10/1/90
**
** Modified:    6/18/94 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Added the include file string.h. Removed the include file
**              strings.h. Replaced calls to bcopy() with memcpy().
**
**              8/17/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              12/15/94 - S. Hardman - R1B
**              Ported to Sybase System 10. Modified ims_clntSendFile()
**              to send all buffers with the RPC call to the server, not
**              just the ones that are filled with data.
**
**              4/3/95 - D. Crichton - R1B
**              Added a parameter to ims_clntvalidateLogin() to send the account
**              ID to the server.
**
**				10/8/96 - D. Crichton - R2.1
**				Port to Sybase Open Client Client Library.
**				Add bulk transfer capability to increase performance.
**
******************************************************************************/

#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <limits.h>


#include <ctpublic.h>
#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_archive.h>
#include <ims_util.h>
#include <sys/stat.h>

#ifdef KRB
#include <ims_krb.h>
#endif	/* KRB */
 
/*
** Local Functions Prototypes
*/

static int sendDataFragments(IMS_MSG_STRUCT *, CS_COMMAND *, int, int);
static int getFileSize (char *);
static int procResults(IMS_MSG_STRUCT *, CS_COMMAND *);

/*
** Static Variables.
*/
static DBBINARY buffer[IMS_MAX_BUFFER_COUNT][IMS_MAX_BUFFER_SIZE];

/******************************************************************************
**
** ims_clntAcceptFile ()
**
** Called from the client side to accept the file sent by the server.
** The server's counterpart routine is ims_srvSendFile ().
**
******************************************************************************/

int ims_clntAcceptFile (
	IMS_MSG_STRUCT *msgDesc,
	CS_COMMAND *command,
	char *fullPathName)
{
	int fd;
	int status;
	CS_INT buffCount;
	CS_INT byteCount;
	CS_INT index;
	CS_INT lastBuffSize;
	CS_INT rowCount;
	CS_RETCODE retCode;
	CS_RETCODE retFetch; 
	CS_RETCODE retBind;
	CS_RETCODE retData;
	CS_INT resultType;
	int i;
	char *buffer[IMS_MAX_BUFFER_COUNT];
	CS_DATAFMT datafmt[4];
	int outlen;
	int paramCount;
	CS_INT rowsRead;

	/*
	** Initialize the buffer area.
	*/

	for (i = 0; i < IMS_MAX_BUFFER_COUNT; i++)
		buffer[i] = NULL;

	/*
	** Initialize rpc call 
	*/
	if (ct_command (command, CS_RPC_CMD, "transferFile", CS_NULLTERM,
		CS_NO_RECOMPILE) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"ims_clntAcceptFile: ct_command() failed.");
		return (IMS_FATAL);
	}

	/* 
	** Signal the end of the rpc call.
	*/
	if (ct_send (command) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"ims_clntAcceptFile: ct_send() failed.");
		return (IMS_FATAL);
	}


	/*
	** Process the results.
	*/

	while ((retCode = ct_results(command, &resultType)) == CS_SUCCEED)
	{
		switch ((int) resultType)
		{
			case CS_ROW_RESULT:
				/*
				** Process the returned rows.
				*/

				/*
				** Free the buffer area.
				*/

				for (i = 0; i < IMS_MAX_BUFFER_COUNT; i++)
					if (buffer[i] != NULL)
						free(buffer[i]);

				if (strcmp ((char *) ims_extractFileName (fullPathName), 
							"-") == 0) 
				{
					/* 
					** Set the file descriptor to stdout file pointer.
					*/
					fd = 1;
				}
				else
				{
					if ((fd = open (fullPathName, O_WRONLY | O_CREAT
							| O_TRUNC, 00666)) == -1)
					{
						(void) ct_cancel(NULL, command, CS_CANCEL_ALL);
						(void) ims_msg (msgDesc, IMS_FATAL, 
							"Could not open to write file '%s'.",
							fullPathName);
						return (IMS_FATAL);
					}
				}

				/*
				** Bind the parameters.
				*/

				/*
				** buffCount
				*/

				if ((retBind = ct_describe(command, 1, &datafmt[0])) != 
							CS_SUCCEED)
				{
					(void) ims_msg(msgDesc, IMS_FATAL,
						"Could not describe the buffCount parameter");
					return(IMS_FATAL);
				}


				if ((retBind = ct_bind(command, 1, &datafmt[0], &buffCount,
					NULL, NULL)) != CS_SUCCEED)
				{
					(void) ims_msg(msgDesc, IMS_FATAL,
						"Could not bind the buffCount parameter");
					return(IMS_FATAL);
				}

				/*
				** index
				*/


				if ((retBind = ct_describe(command, 2, &datafmt[1])) != 
							CS_SUCCEED)
				{
					(void) ims_msg(msgDesc, IMS_FATAL,
						"Could not describe the index parameter");
					return(IMS_FATAL);
				}

				if ((retBind = ct_bind(command, 2, &datafmt[1], &index,
					NULL, NULL)) != CS_SUCCEED)
				{
					(void) ims_msg(msgDesc, IMS_FATAL,
						"Could not bind the index parameter");
					return(IMS_FATAL);
				}

				/*
				** lastBuffSize
				*/


				if ((retBind = ct_describe(command, 3, &datafmt[2])) != 
							CS_SUCCEED)
				{
					(void) ims_msg(msgDesc, IMS_FATAL,
						"Could not describe the lastBuffSize parameter");
					return(IMS_FATAL);
				}

				if ((retBind = ct_bind(command, 3, &datafmt[2], &lastBuffSize,
					NULL, NULL)) != CS_SUCCEED)
				{
					(void) ims_msg(msgDesc, IMS_FATAL,
						"Could not bind the lastBuffSize parameter");
					return(IMS_FATAL);
				}


				/*
				** Process each row.
				*/

				rowCount = 0;

				while ((retFetch = ct_fetch(command, CS_UNUSED,
					CS_UNUSED, CS_UNUSED, &rowsRead)) == CS_SUCCEED)
				{
					
					rowCount ++;


					/*
					** Check the buffer counter.
					*/

					if (buffCount > IMS_MAX_BUFFER_COUNT)
					{
						(void) ct_cancel(NULL, command, CS_CANCEL_ALL);

						if (fd != 1)
						{
							(void) close (fd);
							(void) unlink (fullPathName);
						}

						(void) ims_msg (msgDesc, IMS_FATAL, 
							"ims_clntAcceptFile: File fragment buffers exceeded '%d'.",
							IMS_MAX_BUFFER_COUNT);
						status = IMS_FATAL;

						goto free_buffers;
					}

					/* 
					** Rebind the parameters since the rest is
					** just buffers.
					*/

					paramCount = 3;

					for (i = 0; i < buffCount; i++)
					{
						/* 
						** Get the parameter description.
						*/



						/*
						** Allocate memory for new buffer parameter.
						*/

						if (buffer[i] != NULL)
							free(buffer[i]);

						buffer[i] = NULL;		

						if ((buffer[i]  = (char *) malloc(sizeof(CS_CHAR) * 
								IMS_MAX_BUFFER_SIZE)) == NULL)
						{
							(void) ims_msg(msgDesc, IMS_FATAL,
								"Could not allocate memory for parameter %d.",
									paramCount); 
							status = IMS_FATAL;

							goto free_buffers;

						}

						paramCount ++;

						if ((retData = ct_get_data(command, paramCount,
							(CS_VOID *) buffer[i],
							IMS_MAX_BUFFER_SIZE, (CS_INT *) &outlen)) != CS_END_ITEM)
						{
							/*
							** Last call returns end of data.
							*/

							if (retData == CS_END_DATA)
								continue;

							(void) ims_msg(msgDesc, IMS_FATAL,
							"Could not get param %d in ims_clntAcceptFile. retData = %d.",
								paramCount, retData);
							status = IMS_FATAL;

							goto free_buffers;

						}

					}

					/*
					** Make sure file is received in sequence.
					*/
					if (rowCount != index)
					{
						(void) ct_cancel(NULL, command, CS_CANCEL_ALL);

						if (fd != 1)
						{
							(void) close (fd);
							(void) unlink (fullPathName);
						}

						(void) ims_msg (msgDesc, IMS_FATAL, 
							"ims_clntAcceptFile: File fragment out of sequence. %d vs %d",
							rowCount, index);
						status = IMS_FATAL;

						goto free_buffers;
					}

					/*
					** Get the data buffers returned.
					*/
					for (i=0; i < buffCount; i++)
					{
						if (i == (buffCount-1))
						{
							byteCount = lastBuffSize;
						}
						else
						{
							byteCount = IMS_MAX_BUFFER_SIZE;
						}

						/*
						** 6/18/94 - The Port Team modified this call
						** to write() to check if we wrote as many
						** as we requested.
						*/
						if (write (fd, (char *) buffer[i],
							byteCount) != byteCount)
						{
							(void) ct_cancel(NULL, command, CS_CANCEL_ALL);

							if (fd != 1)
							{
								(void) close (fd);
								(void) unlink (fullPathName);
							}

							(void) ims_msg (msgDesc, IMS_FATAL, 
								"ims_clntAcceptFile: Write into file failed or was not complete.");
							status = IMS_FATAL;

							goto free_buffers;
						}
					}
				}

				/*
				** Free the buffer area.
				*/

				for (i = 0; i < IMS_MAX_BUFFER_COUNT; i++)
					if (buffer[i] != NULL)
						free(buffer[i]);
				break;

			case CS_STATUS_RESULT:
				if ((status = ims_checkCtStatus(msgDesc, command)) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"checkStatus returned an error status.");
					return(IMS_ERROR);
				}
				break;
			
			case CS_PARAM_RESULT:
			case CS_CMD_SUCCEED:
			case CS_CMD_DONE:
				break;

			case CS_CMD_FAIL:
				(void) ims_msg(msgDesc, IMS_ERROR,
					"ims_clntAcceptFile: RPC Failed");
				return(IMS_ERROR);


			default:
				(void) ims_msg(msgDesc, IMS_ERROR,
					"ims_clntAcceptFile: Unknown result from server");
				return(IMS_ERROR);
		}
	}

		
	/*
	** File transfer is complete.  Close the file and continue.
	*/
	if (fd != 1)
	{
		(void) close (fd);
	}

	if (retCode != CS_END_RESULTS)
	{
		if (fd != 1)
		{
			(void) unlink (fullPathName);
		}
		return (IMS_ERROR);
	}
	return (IMS_OK);

	/*
	** Free the buffer area.
	*/

free_buffers:

	for (i = 0; i < IMS_MAX_BUFFER_COUNT; i++)
		if (buffer[i] != NULL)
			free(buffer[i]);
	return(status);
}

/******************************************************************************
**
** ims_clntSendKrbTicket ()
**
** Called from the server side to send kerberos ticket to the client.
** The server's counterpart routine is ims_srvAcceptKrbTicket ().
**
******************************************************************************/

#ifdef KRB
int ims_clntSendKrbTicket (
	DBPROCESS *dbproc,
	IMS_CLIENT_KRB *cKrb,
	IMS_MSG_STRUCT *msgDesc)
{
	DBINT byteCount;
	DBINT index;
	int status;
	int length;
	int i;
	int j;

	length = cKrb->ktxt.length;
	index = 0;
	j = 0;
	while (length > 0)
	{
		if (length > IMS_MAX_BUFFER_SIZE) 
		{
			byteCount = IMS_MAX_BUFFER_SIZE;
		}
		else
		{
			byteCount = length;
		}

		(void) memcpy ((char *) buffer[0],
			(char *) &(cKrb->ktxt.dat[j]), byteCount);

		length -= byteCount;
		j += byteCount;
		index++;

		if (dbrpcinit (dbproc, "krbFragment", (DBSMALLINT) 0) == FAIL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"ims_clntSendKrbTicket: dbrpcinit() failed.");
			return (IMS_FATAL);
		}
		if (dbrpcparam (dbproc, "@byteCount", (BYTE) 0, SYBINT4,
			(DBINT) -1, (DBINT) -1, (BYTE *) &byteCount) == FAIL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"ims_clntSendKrbTicket: dbrpcparam() failed; byteCount.");
			return (IMS_FATAL);
		}
		if (dbrpcparam (dbproc, "@index", (BYTE) 0, SYBINT4, (DBINT) -1,
			(DBINT) -1, (BYTE *) &index) == FAIL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"ims_clntSendKrbTicket: dbrpcparam() failed; index.");
			return (IMS_FATAL);
		}
		if (dbrpcparam (dbproc, "@buffer", (BYTE) 0, SYBBINARY,
			(DBINT) -1, (DBINT) byteCount, (BYTE *) buffer[0])
			== FAIL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"ims_clntSendKrbTicket: dbrpcparam() failed; buffer.");
			return (IMS_FATAL);
		}

		/* 
		** Signal the end of the rpc call.
		*/
		if (dbrpcsend (dbproc) == FAIL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"ims_clntSendKrbTicket: dbrpcsend() failed.");
			return (IMS_FATAL);
		}

		/*
		** Check the returned status.  Ignore all other information
		** returned by the FTServer.  
		*/
		if ((status = getRetStatus (dbproc, msgDesc)) < IMS_OK)
		{
			return (status);
		}
	}

	return (status);
}
#endif	/* KRB */

/******************************************************************************
**
** ims_clntOpenFile ()
**
******************************************************************************/

int ims_clntOpenFile (
	IMS_MSG_STRUCT *msgDesc,
	CS_COMMAND *command,
	char *name,
	char *extension)
{
	int status;
	CS_RETCODE retCode;
	int resultType;
	int i;
	CS_DATAFMT datafmt1, datafmt2;


	/*
	** Tell the server to open the file on the server side.
	** Perform the RPC call openFile.
	*/

	if ((retCode = ct_command(command, CS_RPC_CMD, "openFile", CS_NULLTERM,
		CS_NO_RECOMPILE)) != CS_SUCCEED) 
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"ims_clntOpenFile: ct_command() failed.");
		return (IMS_FATAL);
	}

	/*
	** name
	*/

	memset(&datafmt1, 0, sizeof(CS_DATAFMT));
	datafmt1.datatype = CS_CHAR_TYPE;
	datafmt1.maxlength = strlen(name);
	datafmt1.status = CS_INPUTVALUE;

	if ((retCode = ct_param(command, &datafmt1, (CS_VOID *)
		name, strlen(name), NULL)) != CS_SUCCEED)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
			"Could not send parameter 1 for RPC openFile to the server.");
		return(IMS_FATAL);
	}




	/*
	** extension
	*/

	memset(&datafmt2, 0, sizeof(CS_DATAFMT));
	datafmt2.datatype = CS_CHAR_TYPE;
	datafmt2.maxlength = strlen(extension);
	datafmt2.status = CS_INPUTVALUE;

	if ((retCode = ct_param(command, &datafmt1, (CS_VOID *)
		extension, strlen(extension), NULL)) != CS_SUCCEED)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
			"Could not send parameter 2 for RPC openFile to the server.");
		return(IMS_FATAL);
	}


	/*
	** Signal the end of the remote procedure call.
	*/
	if ((retCode = ct_send (command)) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"ims_clntOpenFile: ct_send() failed.");
		return (IMS_FATAL);
	}

	/*
	** At this point we must wait for the status returned by the 
	** remote procedure call.  We are only interested in the status
	** returned.  All other information is ignored.
	*/

	if ((status = procResults (msgDesc, command)) < IMS_OK)
	{
		return (status);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_clntCloseFile ()
**
******************************************************************************/

int ims_clntCloseFile (
	IMS_MSG_STRUCT *msgDesc,
	CS_COMMAND *command,
	char *name,
	char *extension)
{
	int status;
	CS_RETCODE retCode;
	CS_DATAFMT datafmt1, datafmt2;

	/*
	** Tell the server to close the file on the server side.
	** Perform the RPC call closeFile.
	*/

	if ((retCode = ct_command(command, CS_RPC_CMD, "closeFile", CS_NULLTERM,
		CS_NO_RECOMPILE)) != CS_SUCCEED) 
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"ims_clntCloseFile: ct_command() failed.");
		return (IMS_FATAL);
	}

	/*
	** name
	*/

	memset(&datafmt1, 0, sizeof(CS_DATAFMT));
	datafmt1.datatype = CS_CHAR_TYPE;
	datafmt1.maxlength = strlen(name);
	datafmt1.status = CS_INPUTVALUE;

	if ((retCode = ct_param(command, &datafmt1, (CS_VOID *)
		name, strlen(name), NULL)) != CS_SUCCEED)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
			"Could not send parameter 1 for RPC closeFile to the server.");
		return(IMS_FATAL);
	}




	/*
	** extension
	*/

	memset(&datafmt2, 0, sizeof(CS_DATAFMT));
	datafmt2.datatype = CS_CHAR_TYPE;
	datafmt2.maxlength = strlen(extension);
	datafmt2.status = CS_INPUTVALUE;

	if ((retCode = ct_param(command, &datafmt1, (CS_VOID *)
		extension, strlen(extension), NULL)) != CS_SUCCEED)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
			"Could not send parameter 2 for RPC closeFile to the server.");
		return(IMS_FATAL);
	}


	/*
	** Signal the end of the remote procedure call.
	*/
	if ((retCode = ct_send (command)) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"ims_clntCloseFile: ct_send() failed.");
		return (IMS_FATAL);
	}

	/*
	** At this point we must wait for the status returned by the 
	** remote procedure call.  We are only interested in the status
	** returned.  All other information is ignored.
	*/
	if ((status = procResults (msgDesc, command)) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "RPC closeFile returned an error");
		return (status);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_clntSendFile ()
**
** Called from the client side to send a file to the server. The server's
** counterpart routine is ims_srvAcceptFile ().
**
******************************************************************************/

int ims_clntSendFile (
	IMS_MSG_STRUCT *msgDesc,
	CS_COMMAND *command,
	char *fullPathName)

{
	int fd;
	DBINT index;
	DBINT buffCount;
	DBINT lastBuffSize;
	DBINT byteCount;
	int i;
	int status;
	int endOfFile;
	char buffName[IMS_COL30_LEN+1];
	int fsize;

	if ((fsize = getFileSize (fullPathName)) < 0)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
			"Could not determine the file size");
		return(IMS_FATAL);
	}

	/*
	** Open the file on the client side.
	*/
	if ((fd = open (fullPathName, O_RDONLY, 00400)) == -1)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"Could not open to read file '%s'.", fullPathName);
		return (IMS_FATAL);
	}

	/*
	** Send the file fragments to the server.
	*/


	if (sendDataFragments(msgDesc, command, fd, fsize) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not send data fragments for file '%s' to server",
				fullPathName);
		(void) close (fd);
		return(IMS_ERROR);

	}

	(void) close (fd);

	/*
	** At this point we must wait for the status returned by the 
	** remote procedure call.  We are only interested in the status
	** returned.  All other information is ignored.
	*/

	return(procResults (msgDesc, command));
}

/******************************************************************************
**
** ims_clntAcceptFileName ()
**
** Read the granule name and extension sent by the server.
** The server's counterpart routine is ims_srvSendFileName().
**
******************************************************************************/

int ims_clntAcceptFileName (
	IMS_MSG_STRUCT *msgDesc,
	CS_COMMAND *command,
	char *retName,
	char *retExtension,
	DBINT *retLastFileFlag)
{
	int status;
	CS_RETCODE retCode, retBind, retFetch, retData;
	CS_INT resultType;
	CS_DATAFMT dataFormat1, dataFormat2, dataFormat3;
	CS_INT rowsRead;
	int outlen;

	/*
	** Initialize rpc call 
	*/
	if (ct_command (command, CS_RPC_CMD, "transferFileName", CS_NULLTERM,
		CS_NO_RECOMPILE) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"ims_clntAcceptFile: ct_command() failed.");
		return (IMS_FATAL);
	}

	/* 
	** Signal the end of the rpc call.
	*/
	if (ct_send (command) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"ims_clntAcceptFile: ct_send() failed.");
		return (IMS_FATAL);
	}


	/*
	** Process the results.
	*/

	while ((retCode = ct_results(command, &resultType)) == CS_SUCCEED)
	{
		switch ((int) resultType)
		{
			case CS_ROW_RESULT:

				/*
				** Process the returned rows.
				*/




				/*
				** Now, get all of the results for these parameters
				** from the server.
				*/


				while (((retFetch = ct_fetch(command, CS_UNUSED,
					CS_UNUSED, CS_UNUSED, &rowsRead)) == CS_SUCCEED) ||
					(retFetch == CS_ROW_FAIL))
				{
					if (retFetch == CS_ROW_FAIL)
					{
						(void) ims_msg(msgDesc, IMS_ERROR,
				"An error occurred fetching rows in ims_clntAcceptFileName");
						return(IMS_ERROR);
					}


					if ((retData = ct_get_data(command, 1,
						retName, IMS_COL30_LEN,
						(CS_INT *) &outlen)) != CS_END_ITEM)
					{
						(void) ims_msg(msgDesc, IMS_ERROR,
				"Error getting data for param 1 in ims_clntAcceptFileName");
						return(IMS_ERROR);
					}

					retName[outlen] = '\0';
					ims_trim(retName);


					if ((retData = ct_get_data(command, 2,
						retExtension, IMS_COL10_LEN,
						(CS_INT *) &outlen)) != CS_END_ITEM)
					{
						(void) ims_msg(msgDesc, IMS_ERROR,
				"Error getting data for param 2 in ims_clntAcceptFileName");
						return(IMS_ERROR);
					}


					retExtension[outlen] = '\0';
					ims_trim(retExtension);

					if ((retData = ct_get_data(command,3,
						retLastFileFlag, sizeof(DBINT),
						(CS_INT *) &outlen)) != CS_END_ITEM)
					{
						if (retData == CS_END_DATA)
							continue;

						(void) ims_msg(msgDesc, IMS_ERROR,
				"Error getting data for param 3 in ims_clntAcceptFileName");
						return(IMS_ERROR);
					}


				}



				/*
				** Check the return value of ct_fetch.
				*/

				switch ((int) retFetch)
				{

					case CS_END_DATA:
						break;

					case CS_FAIL:
						(void) ims_msg(msgDesc, IMS_FATAL,
							"RPC transferFileName failed.");

						return(IMS_FATAL);		
						break;

					default:
						(void) ims_msg(msgDesc, IMS_FATAL,
					"Unknown error occurred fetching rows in ims_clntAcceptFileName ");
						break;
				}


				break;

			case CS_STATUS_RESULT:
				if ((status = ims_checkCtStatus(msgDesc, command)) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"transferFileName returned an error status.");
					return(IMS_ERROR);
				}
				break;
			
			case CS_PARAM_RESULT:
			case CS_CMD_SUCCEED:
			case CS_CMD_DONE:
				break;

			case CS_CMD_FAIL:
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Command  failed in transferFileName.");
				break;

			default:
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Received unepected result type of %d for RPC transferFileName.",
						resultType);
				return(IMS_ERROR);

		}

	}

	/*
	** Check the retCode parameter since it is not CS_SUCCEED.
	*/

	if (retCode != CS_END_RESULTS)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"An error occurred processing the results in clntAcceptFileName");
		return(IMS_ERROR);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_clntValidateLogin ()
**
** Validate client login.
**
******************************************************************************/

int ims_clntValidateLogin (
	IMS_MSG_STRUCT *msgDesc,
	CS_COMMAND *command,	
	IMS_CLNT_EVENT *request)
{
	int status;
	CS_RETCODE retCode;
	CS_DATAFMT datafmt1, datafmt2, datafmt3, datafmt4;
	char hostName[IMS_HOST_LEN+1];
	char login[IMS_COL30_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	int process_id;
	

	/*
	** Initialize rpc call 
	*/
	if (ct_command (command, CS_RPC_CMD, "validateLogin", CS_NULLTERM,
		CS_NO_RECOMPILE) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"ims_clntValidateLogin: ct_command() failed.");
		return (IMS_FATAL);
	}


	/*
	** Account Id
	*/

	memset(&datafmt1, 0, sizeof(CS_DATAFMT));
	datafmt1.datatype = CS_CHAR_TYPE;
	datafmt1.maxlength = strlen(request->accountId);
	datafmt1.status = CS_INPUTVALUE;

	if ((retCode = ct_param(command, &datafmt1, (CS_VOID *)
		request->accountId, strlen(request->accountId), NULL)) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, "ims_clntValidateLogin: ct_param() failed.");
		return(IMS_FATAL);
	}

	/*
	** Process Username
	*/

	(void) cuserid(login);

	memset(&datafmt2, 0, sizeof(CS_DATAFMT));
	datafmt2.datatype = CS_CHAR_TYPE;
	datafmt2.maxlength = strlen(login);
	datafmt2.status = CS_INPUTVALUE;

	if ((retCode = ct_param(command, &datafmt2, (CS_VOID *)
		(CS_VOID *) login, strlen(login), NULL)) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, "ims_clntValidateLogin: ct_param() failed.");
		return(IMS_FATAL);
	}


	/*
	** Machine Name
	*/

   (void) uname (&uname_info);
   (void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);

	memset(&datafmt3, 0, sizeof(CS_DATAFMT));
	datafmt3.datatype = CS_CHAR_TYPE;
	datafmt3.maxlength = strlen(hostName);
	datafmt3.status = CS_INPUTVALUE;

	if ((retCode = ct_param(command, &datafmt3, (CS_VOID *)
		(char *) hostName, strlen(hostName), NULL)) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, "ims_clntValidateLogin: ct_param() failed.");
		return(IMS_FATAL);
	}

	/*
	** Current Process ID.
	*/

	memset(&datafmt4, 0, sizeof(CS_DATAFMT));
	datafmt4.datatype = CS_INT_TYPE;
	datafmt4.maxlength = sizeof(int);
	datafmt4.status = CS_INPUTVALUE;
	process_id = getpid();

	if ((retCode = ct_param(command, &datafmt4, (CS_VOID *)
		(char *) &process_id, sizeof(int), NULL)) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, "ims_clntValidateLogin: ct_param() failed.");
		return(IMS_FATAL);
	}


	/* 
	** Signal the end of the rpc call.
	*/
	if (ct_send (command) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"ims_clntValidateLogin: ct_send() failed.");
		return (IMS_FATAL);
	}


	/*
	** Check the returned status. Ignore all the other information returned
	** by the FTS server.  
	*/
	status = procResults(msgDesc, command);

	return (status);
}

/******************************************************************************
**
** ims_clntValidateMetadata ()
**
** Request server to validate and process the meta data file.
**
******************************************************************************/

int ims_clntValidateMetadata (
	IMS_MSG_STRUCT *msgDesc,
	CS_COMMAND *command,
	char *name)
{
	int status;
	CS_RETCODE retCode;
	CS_DATAFMT datafmt1;


	/*
	** Initialize the RPC
	*/

	if (ct_command (command, CS_RPC_CMD, "validateMetadata", CS_NULLTERM,
		CS_NO_RECOMPILE) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"ims_clntValidateMetadata: ct_command() failed.");
		return (IMS_FATAL);
	}

	/*
	** Account Id
	*/

	memset(&datafmt1, 0, sizeof(CS_DATAFMT));
	datafmt1.datatype = CS_CHAR_TYPE;
	datafmt1.maxlength = strlen(name);
	datafmt1.status = CS_INPUTVALUE;

	if ((retCode = ct_param(command, &datafmt1, (CS_VOID *)
		name, strlen(name), NULL)) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, "ims_clntValidateMetadata: ct_param() failed.");
		return(IMS_FATAL);
	}

	/* 
	** Signal the end of the rpc call.
	*/
	if (ct_send (command) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"ims_clntValidateMetadata: ct_send() failed.");
		return (IMS_FATAL);
	}

	/*
	** Check the returned status. Ignore all the other information returned
	** by the FTS server.  
	*/
	status = procResults(msgDesc, command);

	return (status);
}



/******************************************************************************
**
** ims_getVersionNumber ()
**
** This function will request for the FTS Server the version number 
** associated with the current dataset.
**
******************************************************************************/

int ims_getVersionNumber (
	IMS_MSG_STRUCT *msgDesc,
	CS_COMMAND *command,
	int *number)
{

	int status;
	CS_RETCODE retCode, retFetch, retBind;
	CS_DATAFMT dataFormat1;
	CS_INT resultType;
	CS_INT rowsRead;

	/*
	** Initialize rpc call 
	*/
	if (ct_command (command, CS_RPC_CMD, "getVersionNumber", CS_NULLTERM,
		CS_NO_RECOMPILE) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"ims_getVersionNumber: ct_command() failed.");
		return (IMS_FATAL);
	}

	/* 
	** Signal the end of the rpc call.
	*/
	if (ct_send (command) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"ims_getVersionNumber: ct_send() failed.");
		return (IMS_FATAL);
	}

	/*
	** Process the results.
	*/

	while ((retCode = ct_results(command, &resultType)) == CS_SUCCEED)
	{
		switch ((int) resultType)
		{
			case CS_ROW_RESULT:

				/*
				** Process the returned rows.
				*/

				if ((retBind = ct_describe(command, 1, 
							&dataFormat1)) != CS_SUCCEED)
				{
					(void) ims_msg(msgDesc, IMS_FATAL,
					"Could not describe column 1 in ims_getVersionNumber");
					return(IMS_FATAL);
				}

				if ((retBind = ct_bind(command, 1, &dataFormat1, number,
					NULL, NULL)) != CS_SUCCEED)
				{
					(void) ims_msg(msgDesc, IMS_FATAL,
					"Could not bind column 1 in ims_getVersionNumber");
					return(IMS_FATAL);
				}

				/*
				** Now, get all of the results for these parameters
				** from the server.
				*/

				while (((retFetch = ct_fetch(command, CS_UNUSED,
					CS_UNUSED, CS_UNUSED, &rowsRead)) == CS_SUCCEED) ||
					(retFetch == CS_ROW_FAIL))
				{
					if (retFetch == CS_ROW_FAIL)
					{
						(void) ims_msg(msgDesc, IMS_ERROR,
				"An error occurred fetching rows in ims_getVersionNumber");
						return(IMS_ERROR);
					}

				}

				/*
				** Check the return value of ct_fetch.
				*/

				switch ((int) retFetch)
				{

					case CS_END_DATA:
						break;

					case CS_FAIL:
						(void) ims_msg(msgDesc, IMS_FATAL,
							"RPC getVersionNumber failed.");

						return(IMS_FATAL);		
						break;

					default:
						(void) ims_msg(msgDesc, IMS_FATAL,
					"Unknown error occurred fetching rows in ims_getVersionNumber");
						break;
				}


				break;

			case CS_STATUS_RESULT:
				if ((status = ims_checkCtStatus(msgDesc, command)) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"getVersionNumber returned an error status.");
					return(IMS_ERROR);
				}
				break;
			
			case CS_PARAM_RESULT:
			case CS_CMD_SUCCEED:
			case CS_CMD_DONE:
				break;

			case CS_CMD_FAIL:
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Command failed in getVersionNumber");
				break;

			default:
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Received unepected result type of %d for RPC getVersionNumber.",
						resultType);
				return(IMS_ERROR);

		}

	}

	/*
	** Check the retCode status returned..
	*/

	if (retCode != CS_END_RESULTS)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"An error occurred processing the results in getVersionNumber");
		return(IMS_ERROR);
	}

	return (IMS_OK);

}


/******************************************************************************
**
** ims_getServerName()
**
******************************************************************************/

int ims_getServerName(
	IMS_MSG_STRUCT *msgDesc,
	IMS_CLNT_EVENT *request,
	CS_COMMAND *command)
{

	int status;
	char qbuf[IMS_COL255_LEN+1];
	static CS_CHAR serverName[IMS_COL64_LEN+1];
	CS_RETCODE retCode, retFetch, retBind;
	CS_DATAFMT dataFormat;
	CS_INT resultType;
	CS_INT rowsRead;


	memset(serverName, 0, sizeof(serverName));

	(void) sprintf(qbuf,
		"fts_get_server_name '%s', '%s', '%s'",
			request->platform, request->sensor, request->dataset);


	/*
	** Set command.
	*/
	
	if ((retCode = ct_command(command, CS_LANG_CMD, qbuf,
		CS_NULLTERM, CS_UNUSED)) != CS_SUCCEED)
	{

		(void) ims_msg(msgDesc, IMS_ERROR,
			"fts_get_server_name failed.");
		(void) ct_cmd_drop(command);
		return(IMS_ERROR);
	}

	/*
	** Send the query...
	*/

	if ((retCode = ct_send(command)) != CS_SUCCEED)
	{

		(void) ims_msg(msgDesc, IMS_ERROR,
			"ct_send failed for procedure fts_get_server_name.");
		(void) ct_cmd_drop(command);
		return(IMS_ERROR);
	}

	/*
	** Process the query results.
	*/

	while ((retCode = ct_results(command, &resultType)) == CS_SUCCEED)
	{
		switch ((int) resultType)
		{
			case CS_ROW_RESULT:


				/*
				** get the server name
				*/

				if ((retBind = ct_describe(command, 1, 
							&dataFormat)) != CS_SUCCEED)
				{
					(void) ims_msg(msgDesc, IMS_FATAL,
					"Could not describe server name column in getServerName");
					return(IMS_FATAL);
				}

				if ((retBind = ct_bind(command, 1, &dataFormat, serverName,
					NULL, NULL)) != CS_SUCCEED)
				{
					(void) ims_msg(msgDesc, IMS_FATAL,
					"Could not describe server name column in getServerName");
					return(IMS_FATAL);
				}

				/*
				** Now, get all of the results for these parameters
				** from the server.
				*/

				while (((retFetch = ct_fetch(command, CS_UNUSED,
					CS_UNUSED, CS_UNUSED, &rowsRead)) == CS_SUCCEED) ||
					(retFetch == CS_ROW_FAIL))
				{
					if (retFetch == CS_ROW_FAIL)
					{
						(void) ims_msg(msgDesc, IMS_ERROR,
							"An error occurred fetching rows in getServerName");
						return(IMS_ERROR);
					}

				}

				/*
				** Check the return value of ct_fetch.
				*/

				switch ((int) retFetch)
				{

					case CS_END_DATA:
						break;

					case CS_FAIL:
						(void) ct_cancel(NULL, command, CS_CANCEL_ALL);
						(void) ims_msg(msgDesc, IMS_FATAL,
							"Error occurred while fetching rows in getServerName");
						return(IMS_FATAL);		
						break;

					default:
						(void) ims_msg(msgDesc, IMS_FATAL,
					"Unknown error occurred fetching rows in getServerName");
						break;
				}


				break;

			case CS_STATUS_RESULT:
				if ((status = ims_checkCtStatus(msgDesc, command)) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"fts_get_server_name returned an error status.");
					return(IMS_ERROR);
				}
				break;
			
			case CS_CMD_SUCCEED:
			case CS_CMD_DONE:
				break;

			case CS_CMD_FAIL:
				(void) ct_cancel(NULL, command, CS_CANCEL_ALL);
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Command  failed in getServerName.");
				break;

			default:
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Received unepected result type of %d for command '%s'",
						resultType, qbuf);
				return(IMS_ERROR);

		}

	}

	/*
	** Check the retCode parameter returned...
	*/

	if (retCode != CS_END_RESULTS)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"An error occurred processing the results in getServerName");
		return(IMS_ERROR);
	}

	if (serverName[0] == '\0')
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
		"Did not locate the FTS server name for platform '%s', dataset '%s'",
			request->platform, request->dataset);
		return(IMS_ERROR);
	}

	request->ftsSrvName = serverName;
	return(IMS_OK);
}


/******************************************************************************
**
** ims_checkFileTypes ()
**
** Get required catalog policy information.
**
******************************************************************************/

IMS_FILE_TYPES *ims_checkFileTypes (
	IMS_MSG_STRUCT *msgDesc,
	CS_COMMAND *command,
	IMS_CLNT_EVENT *request)
{
	IMS_FILE_TYPES *currPtr, *fileTypes;
	CS_RETCODE retCode, retFetch, retBind;
	CS_RETCODE retData;
	CS_DATAFMT datafmt1, datafmt2, datafmt3, datafmt4, datafmt5;
	CS_INT resultType;
	CS_INT rowsRead;
	CS_INT outlen;
	int status;
	int paramCount;
	int countTypes;
	int i;

	currPtr = fileTypes = NULL;

	/*
	** Initialize rpc call 
	*/
	if (ct_command (command, CS_RPC_CMD, "checkFileTypes", CS_NULLTERM,
		CS_NO_RECOMPILE) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"checkFileTypes: ct_command() failed.");
		return (NULL);
	}

 
	/*
	** platform
	*/

	memset(&datafmt1, 0, sizeof(CS_DATAFMT));
	datafmt1.datatype = CS_CHAR_TYPE;
	datafmt1.maxlength = strlen(request->platform);
	datafmt1.status = CS_INPUTVALUE;

	if ((retCode = ct_param(command, &datafmt1, (CS_VOID *)
		request->platform, strlen(request->platform), NULL)) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"checkFileTypes: ct_param() failed.");
		return (NULL);
	}


	/*
	** Sensor
	*/

	memset(&datafmt2, 0, sizeof(CS_DATAFMT));
	datafmt2.datatype = CS_CHAR_TYPE;
	datafmt2.maxlength = strlen(request->sensor);
	datafmt2.status = CS_INPUTVALUE;

	if ((retCode = ct_param(command, &datafmt2, (CS_VOID *)
		request->sensor, strlen(request->sensor), NULL)) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"checkFileTypes: ct_param() failed.");
		return (NULL);
	}


	/*
	** Dataset
	*/

	memset(&datafmt3, 0, sizeof(CS_DATAFMT));
	datafmt3.datatype = CS_CHAR_TYPE;
	datafmt3.maxlength = strlen(request->dataset);
	datafmt3.status = CS_INPUTVALUE;

	if ((retCode = ct_param(command, &datafmt3, (CS_VOID *)
		request->dataset, strlen(request->dataset), NULL)) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"checkFileTypes: ct_param() failed.");
		return (NULL);
	}

	/* 
	** Format
	*/

	memset(&datafmt4, 0, sizeof(CS_DATAFMT));
	datafmt4.datatype = CS_CHAR_TYPE;
	datafmt4.maxlength = strlen(request->format);
	datafmt4.status = CS_INPUTVALUE;

	if ((retCode = ct_param(command, &datafmt4, (CS_VOID *)
		request->format, strlen(request->format), NULL)) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"checkFileTypes: ct_param() failed.");
		return (NULL);
	}


	/* 
	** Signal the end of the rpc call.
	*/
	if (ct_send (command) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"ims_getVersionNumber: ct_send() failed.");
		return (NULL);
	}

	/*
	** Process the results.
	*/

	while ((retCode = ct_results(command, &resultType)) == CS_SUCCEED)
	{
		switch ((int) resultType)
		{
			case CS_ROW_RESULT:

				/*
				** Process the returned rows.
				*/

				if ((retBind = ct_describe(command, 1, 
							&datafmt1)) != CS_SUCCEED)
				{
					(void) ims_msg(msgDesc, IMS_FATAL,
					"Could not describe column 1 in ims_checkFileTypes");
					return(NULL);
				}

				if ((retBind = ct_bind(command, 1, &datafmt1, &countTypes,
					NULL, NULL)) != CS_SUCCEED)
				{
					(void) ims_msg(msgDesc, IMS_FATAL,
					"Could not bind column 1 in ims_checkFileTypes");
					return(NULL);
				}

				/*
				** Now, get all of the results for these parameters
				** from the server.
				*/

				while (((retFetch = ct_fetch(command, CS_UNUSED,
					CS_UNUSED, CS_UNUSED, &rowsRead)) == CS_SUCCEED) ||
					(retFetch == CS_ROW_FAIL))
				{
					
					/*
					** Get the count so we can extract the rest of the
					** rows.
					*/ 

					paramCount = 1;


					for (i = 0; i < countTypes; i++) 
					{

		
						/*
						**  Add node to list.
						*/

						if (i == 0)
						{
							currPtr = (IMS_FILE_TYPES *) malloc(sizeof(IMS_FILE_TYPES));
				 			fileTypes = currPtr;	/* Set front of list */
						}
						else
						{
							currPtr->next = (IMS_FILE_TYPES *) 
									malloc(sizeof(IMS_FILE_TYPES));
							currPtr = currPtr->next;
						}

						if (currPtr == NULL)
						{
							(void) ct_cancel (NULL, command, CS_CANCEL_ALL);
							(void) ims_msg (msgDesc, IMS_FATAL,
								"checkFileTypes: malloc failed.");
							return (NULL);
						}



						paramCount ++;

						if ((retData = ct_get_data(command, paramCount,
							(CS_VOID *) &currPtr->type, 
							sizeof(currPtr->type), &outlen)) != CS_END_ITEM)
						{
							(void) ims_msg(msgDesc, IMS_FATAL,
							"Could not get param %d in ims_checkFileTypes",
								paramCount);
							return(NULL);

						}

						paramCount ++;

						if ((retData = ct_get_data(command, paramCount,
							(CS_VOID *) &currPtr->position, 
							sizeof(currPtr->position), &outlen)) != CS_END_ITEM)
						{
							(void) ims_msg(msgDesc, IMS_FATAL,
							"Could not get param %d in ims_checkFileTypes",
								paramCount);
							return(NULL);

						}

						paramCount ++;

						if ((retData = ct_get_data(command, paramCount,
							(CS_VOID *) &currPtr->extension,
							sizeof(currPtr->extension), &outlen)) != CS_END_ITEM)
						{
							(void) ims_msg(msgDesc, IMS_FATAL,
							"Could not get param %d in ims_checkFileTypes",
								paramCount);
							return(NULL);

						}

						paramCount ++;

						if ((retData = ct_get_data(command, paramCount,
							(CS_VOID *) &currPtr->localArchiveFlag,
						sizeof(currPtr->localArchiveFlag), &outlen)) != CS_END_ITEM)
						{
							if (retData != CS_END_DATA)
							{
								(void) ims_msg(msgDesc, IMS_FATAL,
								"Could not get param %d in ims_checkFileTypes",
									paramCount);
								return(NULL);
							}

						}


						currPtr->next = NULL;
					}
				}

				/*
				** Check the return value of ct_fetch.
				*/

				switch ((int) retFetch)
				{

					case CS_END_DATA:
						break;

					case CS_FAIL:
						(void) ims_msg(msgDesc, IMS_FATAL,
							"RPC checkFileTypes failed.");

						return(NULL);
						break;

					default:
						(void) ims_msg(msgDesc, IMS_FATAL,
					"Unknown error occurred fetching rows in ims_checkFileTypes ");
						return(NULL);
						break;
				}

				break;

			case CS_STATUS_RESULT:
				if ((status = ims_checkCtStatus(msgDesc, command)) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"RPC checkFileTypes returned an error status.");
					return(NULL);
				}
			break;
		
			case CS_PARAM_RESULT:
			case CS_CMD_SUCCEED:
			case CS_CMD_DONE:
				
				break;

			case CS_CMD_FAIL:
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Command  failed in RPC checkFileTyps.");
				break;

			default:
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Received unepected result type of %d for RPC checkFileTypes.",
						resultType);
				return(NULL);
		}
	}

	/*
	** Check the retCode status returned..
	*/

	if (retCode != CS_END_RESULTS)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"An error occurred processing the results in ims_checkFileTypes");

		/*
		** Free the list.
		*/

		while (fileTypes != NULL)
		{
			currPtr = fileTypes;
			fileTypes = fileTypes->next;
			free(currPtr);
		}
		return(NULL);
	}

	return (fileTypes);
}


/******************************************************************************
**
** procResults
**
** This function will send a command to the server, and ignore any rows
** returned.
******************************************************************************/

static int procResults(
	IMS_MSG_STRUCT *msgDesc,
	CS_COMMAND *command) 
{
	CS_INT resultType;
	CS_RETCODE retCode, retFetch;
	int rowsRead;
	int status;

	/*
	** Process the query results.
	*/

	while ((retCode = ct_results(command, &resultType)) == CS_SUCCEED)
	{
		switch ((int) resultType)
		{
			case CS_ROW_RESULT:


				/*
				** Now, get all of the results for these parameters
				** from the server.
				*/

				while (((retFetch = ct_fetch(command, CS_UNUSED,
					CS_UNUSED, CS_UNUSED, (CS_INT *) &rowsRead)) == CS_SUCCEED) ||
					(retFetch == CS_ROW_FAIL))
				{
					if (retFetch == CS_ROW_FAIL)
					{
						(void) ims_msg(msgDesc, IMS_ERROR,
							"An error occurred fetching rows in procResults");
						return(IMS_ERROR);
					}

				}

				/*
				** Check the return value of ct_fetch.
				*/

				switch ((int) retFetch)
				{

					case CS_END_DATA:
						break;

					case CS_FAIL:
						(void) ims_msg(msgDesc, IMS_FATAL,
						"Error occurred while fetching rows in procResults");
						return(IMS_FATAL);		
						break;

					default:
						(void) ims_msg(msgDesc, IMS_FATAL,
					"Unknown error occurred fetching rows in procResults");
						break;
				}


				break;

			case CS_STATUS_RESULT:
				if ((status = ims_checkCtStatus(msgDesc, command)) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"checkStatus returned an error status.");
					return(IMS_ERROR);
				}
				break;
			
			case CS_PARAM_RESULT:
			case CS_CMD_SUCCEED:
			case CS_CMD_DONE:
				break;

			case CS_CMD_FAIL:
				(void) ims_msg(msgDesc, IMS_ERROR,
					       "Processing Results has failed for the command.");
				return(IMS_ERROR);
				break;

			default:
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Received unexpected result type of %d",
						resultType);
				return(IMS_ERROR);

		}

	}

	/*
	** Check the retCode parameter since it is not CS_SUCCEED.
	*/

	if (retCode != CS_END_RESULTS)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"An error occurred processing the results in procResults");
		return(IMS_ERROR);
	}
	return(IMS_OK);
}


/******************************************************************************
**
** sendDataFragment ()
**
******************************************************************************/

static int sendDataFragments (
	IMS_MSG_STRUCT *msgDesc,
	CS_COMMAND *command,
	int fptr,
	int fsize)
{
	CS_DATAFMT datafmt;
	CS_DATAFMT *bufferfmt;
	CS_RETCODE retCode;
	CS_RETCODE retBind;
	CS_RETCODE retFetch;
	CS_INT columnCount;
	CS_INT resultType;
	static char buffer[64000];
	int status;
	int rowCount;
	int buffCount;
	CS_IODESC iodesc;
	int buffer_length;
	int count;



	/*
	** Setup for image data.
	*/

	if (ct_command(command, CS_SEND_DATA_CMD, NULL, CS_UNUSED,
		CS_COLUMN_DATA) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not set up send of image data");
		return(IMS_ERROR);
	}

	memset(&iodesc, 0, sizeof(CS_IODESC));
	iodesc.total_txtlen = fsize;
	iodesc.iotype = CS_IODATA;
	iodesc.log_on_update = CS_FALSE;
	iodesc.datatype = CS_IMAGE_TYPE;
	iodesc.textptrlen = CS_TP_SIZE;
	iodesc.timestamplen = CS_TS_SIZE;

	if (ct_data_info(command, CS_SET, CS_UNUSED, &iodesc) != CS_SUCCEED)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Error setting image data info");
		return(IMS_ERROR);
	}


	if ((buffer_length = read(fptr, buffer, sizeof(buffer))) < 0)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Error reading from input file.");
			return(IMS_ERROR);
	}
	count = 0;

	while (buffer_length > 0)
	{

		if ((retCode = ct_send_data(command, buffer, 
					buffer_length)) != CS_SUCCEED)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not send buffer to server; ct_send_data() failed.");
			return (IMS_FATAL);
		}

		if ((buffer_length = read(fptr, buffer, sizeof(buffer))) < 0)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
					"Error reading from input file.");
				return(IMS_ERROR);
		}
		count +=buffer_length;

	}

	if ((retCode = ct_send(command)) != CS_SUCCEED)
	{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Could not send buffer to server; ct_send failed.");
			return (IMS_ERROR);
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


/******************************************************************************
**
** ims_checkCtStatus
**
******************************************************************************/

int ims_checkCtStatus (
	IMS_MSG_STRUCT *msgDesc, 
	CS_COMMAND *command)
{
	CS_INT status;
	CS_DATAFMT datafmt;
	int proc_status;
	CS_RETCODE retBind, retFetch;
	CS_INT rowsRead;


	if ((retBind = ct_describe(command, 1, 
			&datafmt)) != CS_SUCCEED)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
			"Could not describe server name column in checkStatus()");
		return(IMS_FATAL);
	}

	if ((retBind = ct_bind(command, 1, &datafmt, &status,
			NULL, NULL)) != CS_SUCCEED)
	{
			(void) ims_msg(msgDesc, IMS_FATAL,
				"Could not describe server name column in checkStatus()");
			return(IMS_FATAL);
	}

	/*
	** Now, get all of the results for these parameters
	** from the server.
	*/

	while (((retFetch = ct_fetch(command, CS_UNUSED,
			CS_UNUSED, CS_UNUSED, &rowsRead)) == CS_SUCCEED) ||
			(retFetch == CS_ROW_FAIL))
	{
			if (retFetch == CS_ROW_FAIL)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"An error occurred fetching rows in checkStatus()");
				return(IMS_ERROR);
			}

	}

	/*
	** Check the return value of ct_fetch.
	*/

	switch ((int) retFetch)
	{

		case CS_END_DATA:
			break;


		case CS_FAIL:
			(void) ct_cancel(NULL, command, CS_CANCEL_ALL);
			(void) ims_msg(msgDesc, IMS_FATAL,
				"Error occurred while fetching rows in checkStatus()");
			return(IMS_FATAL);		

		default:
			(void) ims_msg(msgDesc, IMS_FATAL,
				"Unknown error occurred fetching rows in checkStatus()");
			return(IMS_FATAL);
	}

	/*
	** Check the status from the stored procedure.
	*/

	if (status == -103)
		proc_status = IMS_FATAL;
	else if (status == -101)
		proc_status = IMS_WARNING;
	else if (status >= 0)
		proc_status = IMS_OK;
	else
		proc_status = IMS_ERROR;

	return(proc_status);
}
