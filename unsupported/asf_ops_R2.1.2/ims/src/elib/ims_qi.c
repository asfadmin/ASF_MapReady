static char *sccs = "@(#)ims_qi.c	5.4  05/09/97";
/******************************************************************************
**
** File:        ims_qi.c
**
** Function:    A set of functions that makes add hoc queries.
**
** Author:      J. Jacobson
**
** Date:        5/10/89
**
** Modified:    5/30/90 - Hoshyar Sayah
**              Added the capability to define server, database, and
**              interfaces file names through the use of the environment
**              variables CDB_SERVER, CDB_DB, and CDB_INT.
**
**              ? - J. Jacobson
**              Changed into ims_qiInt.c, which restores the library to
**              its original purpose, getting rid of all of the stream
**              stuff.
**
**              2/4/95 - S. Hardman - R1B
**              To date we have removed the automatic call to ims_qiLogin()
**              from ims_qiNextRow(). We have removed the automatic call
**              to dbsetuserdata() from ims_qiLogin() and replaced it with
**              a macro IMS_SET_USERDATA() which should be called after a 
**              call to ims_qiLogin(). We no longer have a default server
**              and database. Removed the function ims_qiDescInit().
**              Replaced calls to free() in ims_qiLogin() with calls to
**              dbloginfree().
**
**              8/3/95 - S. Hardman - R1B
**              Modified all calls to calloc() to make sure the proper
**              space is being allocated for the pointers.
**
******************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_qi.h.
** They are listed here for reference.
**
**	IMS_QI_DESC_OBJ *ims_qiDescAlloc (IMS_MSG_STRUCT * msgDesc);
**	int ims_qiTblDesc (IMS_QI_DESC_OBJ *);
**	int ims_qiNextRow (IMS_QI_DESC_OBJ *);
**	void ims_qiExit (void);
**	int ims_qiCancel (IMS_QI_DESC_OBJ *);
**	int ims_qiResetDesc (IMS_QI_DESC_OBJ *);
**	int ims_qiFreeDesc (IMS_QI_DESC_OBJ *);
**	int ims_qiLogoff (IMS_QI_DESC_OBJ *);
**	int ims_qiLogin (IMS_QI_DESC_OBJ *);
**	int ims_getResultsStatus (IMS_QI_DESC_OBJ *);
**	void ims_qiLastMsg (IMS_QI_DESC_OBJ *);
*/

/*
** Static function declarations.
*/
static int execSql (IMS_QI_DESC_OBJ *);
static int verifyQDesc (IMS_QI_DESC_OBJ *);
static int allocateRetParmBufs (IMS_QI_DESC_OBJ *);
static int allocateAttrBufs (IMS_QI_DESC_OBJ *);
static int allocateDataDescriptors (IMS_QI_DESC_OBJ *);
static int bindForCoercion (IMS_QI_DESC_OBJ *);
static int describeAttr (IMS_QI_DESC_OBJ *);
static void freeDescription (IMS_QI_DESC_OBJ *);
static void freeDataBuffers (IMS_QI_DESC_OBJ *);
static void reinitDesc (IMS_QI_DESC_OBJ *);
static void getDbLoc (char **, char **, char **);
static int stdNextRow (IMS_QI_DESC_OBJ *);
static int getProcRetParms (IMS_QI_DESC_OBJ *);
static int allocStdQueryDescs (IMS_QI_DESC_OBJ *);
static void tinyConvert (IMS_QI_DESC_OBJ *, int colIdx);
static void smallConvert (IMS_QI_DESC_OBJ *, int colIdx);
static void intConvert (IMS_QI_DESC_OBJ *, int colIdx);
static void realConvert (IMS_QI_DESC_OBJ *, int colIdx);
static void floatConvert (IMS_QI_DESC_OBJ *, int colIdx);

/*
** Declaration of variables that are used when reporting database errors.
*/
static char *DBLOGIN = "dblogin()";
static char *DBOPEN = "dbopen()";
static char *DBUSE = "dbuse()";
static char *DBCMD = "dbcmd()";
static char *DBSQLEXEC = "dbsqlexec()";
static char *DBNEXTROW = "dbnextrow()";
static char *DBCOLNAME = "dbcolname()";
static char *DBCOLLEN = "dbcollen()";
static char *DBCOLTYPE = "dbcoltype()";
static char *EDBLIB = "Call '%s' failed for command: %s";
static char *ECOLRANGE = "Column #%d is out of range for the columns being returned.";

/*
** Initialized (static) array of pointers to query descriptors.
*/
static IMS_QI_DESC_OBJ *descPool[IMS_MAXQUERYDESCS];

/*
** Catalog information.
*/
static char *catalogServer;            	/* server name */
static char *catalogDbName;           	/* database name */
static char *interfacesFile;          	/* sybase interfaces file */

static char *DATASERVER = "";           /* default server name */
static char *DATABASE = "";             /* default database name */

/******************************************************************************
**
** ims_qiDescAlloc ()
**
** Allocate and initialize a query descriptor, and return a pointer to it.
**
******************************************************************************/

IMS_QI_DESC_OBJ *ims_qiDescAlloc (
	IMS_MSG_STRUCT * msgDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	int i;
	static int firstTime = IMS_FALSE;

	if (firstTime == IMS_FALSE)
	{
		getDbLoc (&catalogServer, &catalogDbName, &interfacesFile);
		if (interfacesFile != (char *) NULL)
		{
			dbsetifile (interfacesFile);
		}
		firstTime = IMS_TRUE;
	}

	/*
	** Find the next available query descriptor slot.
	*/
	for (i = 0; i < IMS_MAXQUERYDESCS; i++)
	{
		if (descPool[i] == (IMS_QI_DESC_OBJ *) NULL)
		{
			break;
		}
	}

	/*
	** If we're out of query descriptor slots, return an error.
	*/
	if (i >= IMS_MAXQUERYDESCS)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Exceeded maximum amount of query descriptors, %d",
			IMS_MAXQUERYDESCS);
		return ((IMS_QI_DESC_OBJ *) NULL);
	}

	/*
	** calloc (to initialize) a query descriptor, name and remember it. *
	**
	** lint: pointer cast may result in improper alignment
	** It's OK because calloc() aligns on worst-case boundary.
	*/
	if ((qDesc = descPool[i] =
		(IMS_QI_DESC_OBJ *) calloc ((size_t) 1,
		(size_t) sizeof (IMS_QI_DESC_OBJ))) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not calloc query descriptor.");
		return ((IMS_QI_DESC_OBJ *) NULL);
	}

	/*
	** Now, brand this query descriptor.  We'll need to know which one
	** this is when we close it.
	*/
	qDesc->queryIdx = i;

	/*
	** Set the default buffer size.
	*/
	qDesc->bufferCount = IMS_DEFAULT_ROWCOUNT;

	/*
	** Set Default to Native type.  Can be overriden to specify a
	** conversion of all attributes to Null Terminated Strings (NTS).
	*/
	qDesc->coerceToNTS = IMS_OFF;
	qDesc->coerced = IMS_OFF;
	qDesc->numLeft = IMS_OFF;
	qDesc->realPrec = IMS_DEFAULT_REAL_PREC;
	qDesc->fltPrec = IMS_DEFAULT_FLT_PREC;
	IMS_SET_REALFLTDISPLAY (qDesc, 'f');
	IMS_SET_RF_MAX_WIDTH (qDesc, 64);
	IMS_SET_VERBOSE (qDesc, 0);
	IMS_SET_TIMEOUT (qDesc, -1);	/* Take sybase default. */

	/*
	** Initialize status to OK.
	*/
	qDesc->msgNo = SUCCEED;
	qDesc->qState = IMS_INITIAL;

	/*
	** Initialize catalogServer, catalogDbName, and interfacesFile. *
	** getDbLoc provides alternate server, database, and interfaces *
	** file as defined by the environmental variables IMS_SERVER,
	** IMS_DB, * and IMS_INT.
	*/
	(void) strcpy (qDesc->server, catalogServer);
	(void) strcpy (qDesc->dbname, catalogDbName);

	/*
	** Need a pointer to the message desriptor.
	*/
	qDesc->msgDesc = msgDesc;

	/*
	** Other stuff.
	*/
	qDesc->connected = IMS_FALSE;
	qDesc->bcp = IMS_FALSE;

	/*
	** Return a pointer to our query desriptor.
	*/
	return (qDesc);
}

/******************************************************************************
**
** ims_qiTblDesc ()
**
** Get attribute information into our query descriptor.
**
** Note: This function currently only works for standard queries.
**
******************************************************************************/

int ims_qiTblDesc (
	IMS_QI_DESC_OBJ * qDesc)
{
	int numCols;
	int status;
	IMS_MSG_STRUCT *msgDesc = qDesc->msgDesc;

	/*
	** Verify the query descriptor.
	*/
	if (verifyQDesc (qDesc) < IMS_OK)
	{
		return (ims_msgGetSeverity (msgDesc));
	}

	/*
	** Free any space associated with an old description, and assure
	** that any databuffers are also returned.
	*/
	freeDescription (qDesc);
	freeDataBuffers (qDesc);

	/*
	** Check to see if we have a valid login.
	*/
	if (qDesc->dbproc == (DBPROCESS *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"The database process descriptor has not been initialized.");
		return (IMS_FATAL);
	}

	/*
	** In order to get column information, it is necessary to start the
	** query and obtain results (if this has not already been done).
	*/
	if (qDesc->qState != IMS_RESULTS)
	{
		/*
		** Ship the query and obtain the results.  Set state to
		** IMS_RESULTS so ims_qiNextRow() will go straight to getting
		** rows when called.
		*/
		if ((status = execSql (qDesc)) != IMS_OK)
		{
			return (status);
		}
	}

	/*
	** Now get table information into our descriptor structure.
	*/
	qDesc->attrCount = numCols = dbnumcols (qDesc->dbproc);

	/*
	** Better finish this query up if there are no colums. This will
	** cause a return of all status information required, as well as
	** set the stage for subsequent calls.  (No getnext rows will be
	** done when there are no attributes).  Handles the case where a
	** procedure does nothing and returns a bad status.
	*/
	if (numCols == 0)
	{
		return (ims_qiNextRow (qDesc));
	}

	/*
	** Allocate the buffers required to describe the column attributes.
	*/
	if (allocateAttrBufs (qDesc) < IMS_OK)
	{
		return (ims_msgGetSeverity (msgDesc));
	}

	/*
	** Now, get the description into those buffers.
	*/
	if (describeAttr (qDesc) < IMS_OK)
	{
		return (ims_msgGetSeverity (msgDesc));
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_qiNextRow ()
**
** Get the next row from the result set of the query.
**
******************************************************************************/

int ims_qiNextRow (
	IMS_QI_DESC_OBJ *qDesc)
{
	int status;
	IMS_MSG_STRUCT *msgDesc = qDesc->msgDesc;

	/*
	** Verify the query descriptor.
	*/
	if (verifyQDesc (qDesc) < IMS_OK)
	{
		return (ims_msgGetSeverity (msgDesc));
	}

	/*
	** Check to see if we have a valid login.
	*/
	if (qDesc->dbproc == (DBPROCESS *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"The database process descriptor has not been initialized.");
		return (IMS_FATAL);
	}

	/*
	** If results have been obtained already, or we are flushing out the
	** last transaction, then execute the row function.
	*/
	if (qDesc->qState == IMS_RESULTS || qDesc->qState == IMS_DONE)
	{
		return (stdNextRow (qDesc));
	}

	/*
	** If we are coercing to NTS and we have not gotten a table
	** description, return an error status.  To do this w/ the
	** correct justification, etc, requires this.  Also, it does
	** not make sense to do display stuff w/o attr names, etc.
	** Note: If we get here and are not at IMS_RESULTS or IMS_DONE,
	** then the caller has not gotten a table description.
	*/
	if (qDesc->coerceToNTS == IMS_ON)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"A call to ims_qiTblDesc() must be performed before a call to ims_qiNextRow() when coercing to NTS.");
		return (IMS_FATAL);
	}

	/*
	** Get results and then return the first row.
	*/
	if ((status = execSql (qDesc)) != IMS_OK)
	{
		return (status);
	}

	return (stdNextRow (qDesc));
}

/******************************************************************************
**
** stdNextRow ()
**
** Get the next row from the query for standard queries.
**
******************************************************************************/

static int stdNextRow (
	IMS_QI_DESC_OBJ *qDesc)
{
	int status;
	int i;
	IMS_MSG_STRUCT *msgDesc = qDesc->msgDesc;

	/*
	** Get the next row.  If no more rows, then reset the state to
	** IMS_QUERYEXEC and return a status of IMS_ENDOFQUERY.
	*/
	if ((status = dbnextrow (qDesc->dbproc)) != REG_ROW)
	{
		/*
		** Decode status and take appropriate action.
		*/
		switch (status)
		{
		case NO_MORE_ROWS:
			/*
			** Get returned parameters into the value and attribute
			** description arrays, if any have been returned.  Be sure not
			** to preclude return values on statuses other than 0 too.
			*/
			if ((status = getProcRetParms (qDesc)) < IMS_OK)
			{
				return (status);
			}

			/*
			** Reset state and return status to user.
			*/
			if ((status = ims_getResultsStatus (qDesc)) < 0)
			{
				qDesc->qState = IMS_INITIAL;
				return (status);
			}

			qDesc->qState = IMS_STATEMENTSENT;
			return (IMS_ENDOFQUERY);

		default:
			/*
			** All bets are off, so return an error. User is responsible
			** for freeing up the query descriptor.
			*/
			(void) ims_msg (msgDesc, IMS_ERROR, EDBLIB, DBNEXTROW, qDesc->cmd);
			return (IMS_ERROR);
		}
	}

	/*
	** Make sure there wasn't a failure.
	** ???? I don't think that this chunk of code
	** is reachable. Review.
	*/
	if (status == FAIL)
	{
		ims_qiLastMsg (qDesc);
		if (qDesc->verboseLevel > 5)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, EDBLIB, DBSQLEXEC,
				qDesc->cmd);
			return (IMS_ERROR);
		}
		else
		{
			(void) ims_msgSeverity (msgDesc, IMS_ERROR);
			return (IMS_ERROR);
		}
	}

	/*
	** Set column length and address information.
	** 
	** Note:  If the rows are being coerced into Null Terminated strings,
	** data is already available and valArr[i] points to it. Note: The
	** length field is invalid for this kind of binding.
	*/
	if (qDesc->coerced == IMS_OFF)
	{
		/*
		** Just get the pointers to the data in the sybase buffer, plus
		** length information.
		*/
		for (i = 0; i < qDesc->attrCount; i++)
		{
			if ((qDesc->valLength[i] =
				(long) dbdatlen (qDesc->dbproc, i + 1)) < 1)
			{
				if (qDesc->valLength[i] == -1)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"Column is out of range.");
					return (IMS_FATAL);
				}
			}

			/* If there's a value, get its length. */
			if (qDesc->valLength[i] > 0)
			{
				if ((qDesc->valAddr[i] =
					(char *) dbdata (qDesc->dbproc, i + 1)) == (char *) NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"Value address of non-null attrribute not returned.");
					return (IMS_FATAL);
				}
			}
		}
	}
	else
	{
		/*
		** We are coercing to NTS.  But, we no longer allow the DBMS
		** software to do the conversions, so if there are any,
		** we must do the conversion.
		*/
		for (i = 0; i < qDesc->attrCount; i++)
		{
			/*
			** Decode the attribute type, so we can do the "binds" for the
			** numeric types.
			*/
			switch (qDesc->attrType[i])
			{
			case SYBINT1:
				tinyConvert (qDesc, i);
				break;

			case SYBINT2:
				smallConvert (qDesc, i);
				break;

			case SYBINT4:
				intConvert (qDesc, i);
				break;

			case SYBREAL:
				realConvert (qDesc, i);
				break;

			case SYBFLT8:
				floatConvert (qDesc, i);
				break;

			default:
				/*
				** Just keep looking for the conversions we need to
				** perform.
				*/
				break;
			}
		}
	}

	return (IMS_ROWRETURNED);
}

/******************************************************************************
**
** ims_qiCancelAll ()
**
** Cancel all queries.
**
******************************************************************************/

int ims_qiCancelAll (void)
{
	int i;

	for (i = 0; i < IMS_MAXQUERYDESCS; i++)
	{
		if (descPool[i] != (IMS_QI_DESC_OBJ *) NULL)
		{
			(void) ims_qiCancel (descPool[i]);
		}
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_qiExit ()
**
** Close all queries.
**
******************************************************************************/

void ims_qiExit (void)
{
	int i;

	for (i = 0; i < IMS_MAXQUERYDESCS; i++)
	{
		if (descPool[i] != (IMS_QI_DESC_OBJ *) NULL)
		{
			(void) ims_qiFreeDesc (descPool[i]);
		}
	}
}

/******************************************************************************
**
** ims_qiCancel ()
**
** Cancels the current query.
**
******************************************************************************/

int ims_qiCancel (
	IMS_QI_DESC_OBJ *qDesc)
{
	IMS_MSG_STRUCT *msgDesc = qDesc->msgDesc;

	/*
	** Verify the query descriptor.
	*/
	if (verifyQDesc (qDesc) < IMS_OK)
	{
		return (ims_msgGetSeverity (msgDesc));
	}

	/*
	** If there is no connection, just return IMS_OK.
	*/
	if (qDesc->dbproc == (DBPROCESS *) NULL)
	{
		return (IMS_OK);
	}

	/*
	** Cancel any remaining results for this query, unless the dbproc is
	** dead.
	*/
	if (DBDEAD (qDesc->dbproc) == FALSE)
	{
		if (dbcancel (qDesc->dbproc) == FAIL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not cancel the current command batch. The connection may be suspect.");
			return (IMS_FATAL);
		}
	}

	return (IMS_OK);
}

/******************************************************************************
**
** getProcRetParms ()
**
** Get procedure return parameters for caller.
**
******************************************************************************/

static int getProcRetParms (
	IMS_QI_DESC_OBJ *qDesc)
{
	int i;
	int retNumber;
	int valueCount;
	DBPROCESS *dbproc = qDesc->dbproc;
	IMS_MSG_STRUCT *msgDesc = qDesc->msgDesc;

	/*
	** Get the number of return values for the user.
	*/
	valueCount = qDesc->retValueCount = dbnumrets (dbproc);

	/*
	** If there are return parameters, then describe them.  That is,
	** get their names, lengths and pointers to their values into  the
	** query descriptor.  Note:  If the storage for this information  is
	** to be changed, then, be sure to change the IMS_RET macros too.
	*/
	if (valueCount > 0)
	{
		/*
		** Make sure the space allocated for column descriptions and data
		** is freed-up.
		*/
		freeDescription (qDesc);
		freeDataBuffers (qDesc);

		/* Now, allocate arrays for describing returned values. */
		if (allocateRetParmBufs (qDesc) < IMS_OK)
		{
			return (ims_msgGetSeverity (msgDesc));
		}

		/* Describe */
		for (i = 0; i < valueCount; i++)
		{
			retNumber = i + 1;
			qDesc->attrName[i] = dbretname (dbproc, retNumber);
			qDesc->attrType[i] = dbrettype (dbproc, retNumber);
			qDesc->valAddr[i] = (char *) dbretdata (dbproc, retNumber);
			qDesc->valLength[i] = dbretlen (dbproc, retNumber);
		}
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_qiResetDesc ()
**
** Cancels the current query and reinitializes the
** descriptor for a new command.
**
******************************************************************************/

int ims_qiResetDesc (
	IMS_QI_DESC_OBJ *qDesc)
{
	IMS_MSG_STRUCT *msgDesc;
	int status;

	/*
	** Initialize variables.
	*/
	msgDesc = qDesc->msgDesc;
	status = IMS_OK;

	/*
	** Verify the query descriptor.
	*/
	if (verifyQDesc (qDesc) < IMS_OK)
	{
		return (ims_msgGetSeverity (msgDesc));
	}

	/*
	** Make sure any previous command was cancelled.
	** If the call fails we will still free up the structure
	** before returning.
	*/
	status = ims_qiCancel (qDesc);

	/*
	** Free the space allocated for column descriptions from
	** the previous command.
	*/
	freeDescription (qDesc);

	/*
	** Free space allocated for query data from the previous command.
	*/
	freeDataBuffers (qDesc);

	/*
	** Reinitialize portions of query descriptor set by library routines.
	*/
	reinitDesc (qDesc);

	return (status);
}

/******************************************************************************
**
** ims_qiFreeDesc ()
**
** Closes a database server connection and frees all allocated
** space for the query ... including the descriptor.
**
******************************************************************************/

int ims_qiFreeDesc (
	IMS_QI_DESC_OBJ *qDesc)
{
	short currentDesc;
	IMS_MSG_STRUCT *msgDesc = qDesc->msgDesc;

	/*
	** Verify the query descriptor.
	*/
	if (verifyQDesc (qDesc) < IMS_OK)
	{
		return (ims_msgGetSeverity (msgDesc));
	}

	currentDesc = qDesc->queryIdx;

	/*
	** If we're still logged in to the catalog, log off.
	*/
	if (qDesc->dbproc != (DBPROCESS *) NULL)
	{
		/* Log-off from the catalog. */
		(void) dbclose (qDesc->dbproc);
		qDesc->dbproc = (DBPROCESS *) NULL;
	}

	/*
	** Now, free-up all memory associated with the query.
	*/
	freeDescription (qDesc);
	freeDataBuffers (qDesc);

	/*
	** Free the query descriptor itself.
	*/
	(void) free ((void *) qDesc);
	qDesc = (IMS_QI_DESC_OBJ *) NULL;

	/*
	** Now that we're done de-allocating this query, NULL the
	** descriptor-pool pointer, making it available.
	*/
	descPool[currentDesc] = (IMS_QI_DESC_OBJ *) NULL;

	return (IMS_OK);
}

/******************************************************************************
**
** ims_qiLogoff ()
**
** Closes a dataserver connection and reinitializes the query descriptor.
**
******************************************************************************/

int ims_qiLogoff (
	IMS_QI_DESC_OBJ *qDesc)
{
	IMS_MSG_STRUCT *msgDesc = qDesc->msgDesc;

	/*
	** Verify the query descriptor.
	*/
	if (verifyQDesc (qDesc) < IMS_OK)
	{
		return (ims_msgGetSeverity (msgDesc));
	}

	/*
	** If we're still logged in to the catalog, log off.
	*/
	if (qDesc->dbproc != (DBPROCESS *) NULL)
	{
		/* Log-off from the catalog. */
		(void) dbclose (qDesc->dbproc);
		qDesc->dbproc = (DBPROCESS *) NULL;
	}

	/*
	** Now, free-up all memory associated with the query.
	*/
	freeDescription (qDesc);
	freeDataBuffers (qDesc);

	/*
	** Reinitialize portions of query descriptor set by library routines.
	*/
	reinitDesc (qDesc);

	return (IMS_OK);
}

/******************************************************************************
**
** ims_qiLogin ()
**
** Force a login to the catalog database.  Useful for such
** needs as verifying a login w/o actually doing anything.
**
** Also, used internally to establish a login.
**
** Argument:  qDesc - A query descriptor obtained
**                    by a call to ims_qiDescAlloc().
**
** 2/4/95 - Replaced calls to free() with dbloginfree() because we were
**          leaking memory.
**
******************************************************************************/

int ims_qiLogin (
	IMS_QI_DESC_OBJ * qDesc)
{
	LOGINREC *login;
	int savedLevel;
	char errorMsg[IMS_NAME_LEN + 19];
	IMS_MSG_STRUCT *msgDesc;

	msgDesc = qDesc->msgDesc;

	/*
	** Connect to the server CATALOGDBS and use the database catalog.
	** Suppress the message about changing database context by setting
	** the suppress level to 10.  Set the suppress level back to its
	** original setting once done with login.
	*/
	if (qDesc->dbproc == (DBPROCESS *) NULL)
	{
		if ((login = dblogin ()) == (LOGINREC *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				EDBLIB, DBLOGIN, "Catalog login");
			return (IMS_ERROR);
		}

		DBSETLUSER (login, qDesc->username);
		DBSETLPWD (login, qDesc->password);
		DBSETLAPP (login, qDesc->program);
		BCP_SETL (login, qDesc->bcp);

#ifdef NET_BUFFER
		/*
		** Set the TDS buffer size if requested by the client.
		*/
		if (qDesc->maxNetBuf > 512)
		{
			DBSETLPACKET (login, qDesc->maxNetBuf);
		}
#endif	/* NET_BUFFER */

		savedLevel = ims_msgGetSybLevel (msgDesc);
		(void) ims_msgSybLevel (msgDesc, (int) 10);

		/*
		** If a server was not specified, return error.
		*/
		if (qDesc->server[0] == '\0')
		{
			if (catalogServer[0] == '\0')
			{
				dbloginfree (login);
				(void) ims_msg (msgDesc, IMS_ERROR, 
					"Server name not specified; ims_qiLogin aborted.");
				return (IMS_ERROR);
			}
			(void) strcpy (qDesc->server, catalogServer);
		}

		if (qDesc->timeout > -1)
		{
			(void) dbsettime (qDesc->timeout);
		}

		if ((qDesc->dbproc = dbopen (login, qDesc->server))
			== (DBPROCESS *) NULL)
		{
			dbloginfree (login);
			(void) sprintf (errorMsg, "'%s' server connection",
				qDesc->server);
			(void) ims_msg (msgDesc, IMS_ERROR, EDBLIB, DBOPEN, errorMsg);
			return (IMS_ERROR);
		}

		/*
		** For compatability with the FTS server, we no longer call
		** dbsetuserdata() here to save the message descriptor. 
		** For FTS we want to save the process descriptor. We now have
		** a macro named IMS_SET_USERDATA() that must be called 
		** following a successful call to ims_qiLogin().
		*/

		/*
		** We can now free the login structure.
		*/
		dbloginfree (login);

		/*
		** Use the specified database which is set via a call to the
		** macro IMS_SETDBNAME().  However, if there is no database,
		** then skip the dbuse stuff.
		*/
		if (qDesc->dbname != (char *) NULL && qDesc->dbname[0] != '\0')
		{
			if (dbuse (qDesc->dbproc, (char *) qDesc->dbname) == FAIL)
			{
				if (qDesc->verboseLevel > 5)
				{
					(void) sprintf (errorMsg, "use database '%s'.",
						(char *) qDesc->dbname);
					(void) ims_msg (msgDesc, IMS_WARNING, EDBLIB, DBUSE,
						errorMsg);
					return (IMS_WARNING);
				}
				else
				{
					(void) ims_msgSeverity (msgDesc, IMS_WARNING);
					return (IMS_WARNING);
				}
			}
		}

		(void) ims_msgSybLevel (msgDesc, savedLevel);
	}

	qDesc->connected = IMS_TRUE;
	return (IMS_OK);
}

/******************************************************************************
**
** execSql ()
**
** Execute the SQL statement and get the results.  After this,
** set the state to IMS_RESULTS, and return.
**
** ???? Not sure how deadlock works in multiple SQL statement procedures.
**
******************************************************************************/

static int execSql (
	IMS_QI_DESC_OBJ *qDesc)
{
	int status;
	int tryCount;
	IMS_MSG_STRUCT *msgDesc = qDesc->msgDesc;

	/*
	** This is a new sql statement, so initialize the last message
	** information.
	*/
	(void) ims_msgInitSybMsg (msgDesc);

	/*
	** Send SQL Statement if required.
	*/
	if (qDesc->qState == IMS_INITIAL)
	{
		if (qDesc->timeout > -1)
		{
			(void) dbsettime (qDesc->timeout);
		}
#ifdef ROW_BUFFER
		/* Now, set the row buffering. */
		dbsetopt (qDesc->dbproc, DBBUFFER, qDesc->bufferCount);
#endif	/* ROW_BUFFER */

		/* Initialize try count for deadlock retry. */
		tryCount = 1;

DEADLOCK_RETRY:
		/*
		** Populate the command buffer.
		*/
		if (dbcmd (qDesc->dbproc, qDesc->cmd) == FAIL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, EDBLIB, DBCMD, qDesc->cmd);
			return (IMS_ERROR);
		}

		/*
		** Execute the SQL command.
		*/
		if (dbsqlexec (qDesc->dbproc) == FAIL)
		{
			ims_qiLastMsg (qDesc);

#if 0
			/* Check for a deadlock. */
			if (qDesc->msgNo == IMS_SYB_DEADLOCK)
			{
				if (tryCount != IMS_MAXTRIES)
				{
					(void) ims_msg (msgDesc, IMS_WARNING,
						"The SQL command execution encountered a deadlock on try number '%d'.",
						tryCount);
					tryCount ++;
					goto DEADLOCK_RETRY;
				}
				else
				{
					(void) ims_msg (msgDesc, IMS_ERROR,
						"The SQL command execution encountered a deadlock the maximum number of '%d' times.",
						IMS_MAXTRIES);
				}
			}
#endif

			if (qDesc->verboseLevel > 5)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, EDBLIB, DBSQLEXEC,
					qDesc->cmd);
				return (IMS_ERROR);
			}
			else
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"The SQL command execution failed.");
				return (IMS_ERROR);
			}
		}

		/* Send statement only once. */
		qDesc->qState = IMS_STATEMENTSENT;
	}

RESULTS_RETRY:
	if ((status = (int) dbresults (qDesc->dbproc)) != SUCCEED)
	{
		/*
		** Note:  If we get a results error here (FAIL), that means that
		** the current command within a command batch has failed.  If there
		** was only ONE command in the batch, then dbsqlexec() would return
		** fail.  So, what we really need to do is call dbresults() again.
		** If we return SUCCEED, then continue as usual after posting the
		** error.  Note:  dbresults() must always be called until we see
		** no more results, or we have cancelled the batch.
		*/
		if (status == FAIL)
		{
			if (qDesc->verboseLevel > 5)
			{
				(void) ims_msg (msgDesc, IMS_WARNING,
					"Could not obtain results for SQL command: '%s'", qDesc->cmd);
				ims_qiLastMsg (qDesc);
			}
			goto RESULTS_RETRY;
		}

		if (status == NO_MORE_RESULTS)
		{
			/*
			** Now, get any return status from a stored procedure into the
			** query descriptor.
			*/
			if ((status = ims_getResultsStatus (qDesc)) < 0)
			{
				return (status);
			}

			/*
			** Be sure to set to initial state, so we send sql statement
			** next time.
			*/
			qDesc->qState = IMS_INITIAL;
			return (IMS_ENDOFTRANSACTION);
		}
	}

	return (allocStdQueryDescs (qDesc));
}

/******************************************************************************
**
** allocStdQueryDescs ()
**
** Allocate descriptors for standard query.
**
******************************************************************************/

static int allocStdQueryDescs (
	IMS_QI_DESC_OBJ *qDesc)
{
	IMS_MSG_STRUCT *msgDesc = qDesc->msgDesc;

	/*
	** Now that we've executed the sql statement and obtained results,
	** we can get space for the data structures used for describing the
	** returned row.
	*/
	if (allocateDataDescriptors (qDesc) < IMS_OK)
	{
		return (ims_msgGetSeverity (msgDesc));
	}

	/*
	** If we're coercing the data to NTS, then allocate the buffers, do
	** the binds and set the valAddr pointers to them.
	*/
	if (qDesc->coerceToNTS == IMS_ON)
	{
		if (bindForCoercion (qDesc) < IMS_OK)
		{
			return (ims_msgGetSeverity (msgDesc));
		}
	}
	else
	{
		qDesc->coerced = IMS_OFF;
	}

	qDesc->qState = IMS_RESULTS;
	ims_qiLastMsg (qDesc);

	return (IMS_OK);
}

/******************************************************************************
**
** verifyQDesc ()
**
** Make sure this is a valid query descriptor.
**
******************************************************************************/

static int verifyQDesc (
	IMS_QI_DESC_OBJ *qDesc)
{
	IMS_MSG_STRUCT *msgDesc = qDesc->msgDesc;

	/*
	** Make sure this is the real query descriptor.
	*/
	if (qDesc != descPool[qDesc->queryIdx])
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Query descriptor is not valid.");
		return (IMS_ERROR);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** allocateRetParmBufs ()
**
** Allocate space for the stored procedure return parameters.
**
******************************************************************************/

static int allocateRetParmBufs (
	IMS_QI_DESC_OBJ *qDesc)
{
	IMS_MSG_STRUCT *msgDesc = qDesc->msgDesc;
	int numCols = qDesc->retValueCount;

	/*
	** Now, allocate space for the return value description.
	** 
	** lint: pointer cast may result in improper alignment
	** It's OK because calloc() aligns on worst-case boundary.
	** Note: This applies to all of the callocs in this function.
	*/
	if ((qDesc->attrName = (char **) calloc ((size_t) numCols,
		(size_t) sizeof (char *))) == (char **) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for return parameter buffers.");
		return (IMS_FATAL);
	}

	if ((qDesc->attrType = (short *) calloc ((size_t) numCols,
		(size_t) sizeof (short *))) == (short *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for return parameter buffers.");
		return (IMS_FATAL);
	}

	if ((qDesc->valAddr = (char **) calloc ((size_t) numCols,
		(size_t) sizeof (char *))) == (char **) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for return parameter buffers.");
		return (IMS_FATAL);
	}

	if ((qDesc->valLength = (long *) calloc ((size_t) numCols,
		(size_t) sizeof (long *))) == (long *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for return parameter buffers.");
		return (IMS_FATAL);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** allocateAttrBufs ()
**
** Allocate space for the attribute descriptions.
**
******************************************************************************/

static int allocateAttrBufs (
	IMS_QI_DESC_OBJ *qDesc)
{
	IMS_MSG_STRUCT *msgDesc = qDesc->msgDesc;
	int numCols = qDesc->attrCount;

	/*
	** Don't allocate anything if there are no attributes.
	*/
	if (numCols == 0)
	{
		return (IMS_OK);
	}

	/*
	** Now, allocate space for the attribute description.
	** 
	** lint: pointer cast may result in improper alignment
	** It's OK because calloc() aligns on worst-case boundary.
	** Note: This applies to all of the callocs in this function.
	*/
	if ((qDesc->attrName = (char **) calloc
		((size_t) numCols, (size_t) sizeof (char *))) == (char **) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for attribute description buffers.");
		return (IMS_FATAL);
	}

	if ((qDesc->attrNameLen = (short *) calloc
		((size_t) numCols, (size_t) sizeof (short *))) == (short *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for attribute description buffers.");
		return (IMS_FATAL);
	}

	if ((qDesc->attrLength = (long *) calloc
		((size_t) numCols, (size_t) sizeof (long *))) == (long *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for attribute description buffers.");
		return (IMS_FATAL);
	}

	if ((qDesc->attrType = (short *) calloc
		((size_t) numCols, (size_t) sizeof (short *))) == (short *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for attribute description buffers.");
		return (IMS_FATAL);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** allocateDataDescriptors ()
**
** Allocate space for the returned data.
**
******************************************************************************/

static int allocateDataDescriptors (
	IMS_QI_DESC_OBJ *qDesc)
{
	int numCols;
	IMS_MSG_STRUCT *msgDesc = qDesc->msgDesc;

	/*
	** Find out how many columns we have.
	*/
	numCols = dbnumcols (qDesc->dbproc);

	/*
	** Free-up any old databuffers.
	*/
	freeDataBuffers (qDesc);

	/*
	** Assign the attribute count.
	*/
	qDesc->attrCount = numCols;

	/*
	** Just return OK if there are no attributes in this result set.
	** This could happen if a procedure returns a bad status and no
	** rows.  So, just continue on and capture the bad status after  a
	** dbnextrow as specified in the documentation.
	*/
	if (numCols == 0)
	{
		return (IMS_OK);
	}

	/*
	** Allocate decriptors and buffers.
	** 
	** lint: pointer cast may result in improper alignment
	** It's OK because calloc() aligns on worst-case boundary.
	** Note: This applies to all of the callocs in this function.
	*/
	if ((qDesc->valAddr = (char **) calloc
		((size_t) numCols, (size_t) sizeof (char *))) == (char **) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for return data buffers.");
		return (IMS_FATAL);
	}

	if ((qDesc->valLength = (long *) calloc
		((size_t) numCols, (size_t) sizeof (long *))) == (long *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for return data buffers.");
		return (IMS_FATAL);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** bindForCoercion ()
**
** Allocate data buffers and bind them for coercion to
** NULL terminated strings.
**
******************************************************************************/

static int bindForCoercion (
	IMS_QI_DESC_OBJ *qDesc)
{
	int i, type;
	long length, malLength, colNameLength;
	IMS_MSG_STRUCT *msgDesc = qDesc->msgDesc;

	/*
	** Allocate data buffers and do the binds.
	*/
	for (i = 0; i < qDesc->attrCount; i++)
	{
		/* First, get the type and maximum length of the field. */
		type = (int) dbcoltype (qDesc->dbproc, i + 1);
		length = (long) dbcollen (qDesc->dbproc, i + 1);

		switch (type)
		{
		case SYBBINARY:
			length = 2 * length + 2;
			break;

		case SYBINT1:
			length = IMS_LENINT1;
			break;

		case SYBINT2:
			length = IMS_LENINT2;
			break;

		case SYBINT4:
			length = IMS_LENINT4;
			break;

		case SYBREAL:
			length = qDesc->realPrec + IMS_RF_OVERHEAD;
			if (length < qDesc->fltMaxWidth)
			{
				length = qDesc->fltMaxWidth;
			}
			break;

		case SYBFLT8:
			length = qDesc->fltPrec + IMS_RF_OVERHEAD;
			if (length < qDesc->fltMaxWidth)
			{
				length = qDesc->fltMaxWidth;
			}
			break;

		case SYBDATETIME:
			length = IMS_LENDATE;
			break;

		case SYBMONEY:
			length = IMS_LENMONEY;
			break;

#ifdef TEXT_IMAGE_TYPES
		case SYBTEXT:
			length = IMS_LENTEXT;
			break;

		case SYBIMAGE:
			length = IMS_LENIMAGE;
			break;
#endif	/* TEXT_IMAGE_TYPES */

		default:
			break;
		}

		/*
		** Put this maximum length field into valLength.
		*/
		qDesc->valLength[i] = length;

		/*
		** Now, bind this field, if it is not a number.  QI lib does the
		** "bind" ourselves in this case.  Note: We do not do the bind if
		** explicitly called for with IMS_BIND. Note:  This is an AICM,
		** or Anti-implicit Conversion Mask.
		*/
		if (type != SYBREAL && type != SYBFLT8
			&& type != SYBINT1 && type != SYBINT2 && type != SYBINT4)
		{
			/*
			** Allocate the databuffer for this field. We allow space for
			** the null terminator.
			*/
			if ((qDesc->valAddr[i] = malloc (length + 1)) == (char *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Could not allocate data buffer.");
				return (IMS_FATAL);
			}

			if (dbbind (qDesc->dbproc, i + 1, NTBSTRINGBIND,
				length + 1, (BYTE *) qDesc->valAddr[i]) == FAIL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Bind of column %d failed.", i + 1);
				return (IMS_FATAL);
			}
		}
		else
		{
			/*
			** Allocate a buffer for this field.  Make sure it is big
			** enough to accomodate the column length or the name length
			** as required if we are right justified.
			*/
			if (qDesc->numLeft == IMS_OFF)
			{
				/*
				** We right justify, so we need to allocate a buffer large
				** enough to accomodate the column length.
				*/
				colNameLength = strlen (dbcolname (qDesc->dbproc, i + 1));
				if (colNameLength > length)
				{
					malLength = colNameLength;
				}
				else
				{
					malLength = length;
				}

				/*
				** To bullet-proof this stuff, allow for a hugh exponent
				** for floats and reals.
				*/
				if (type == SYBREAL || type == SYBFLT8)
				{
					malLength += 5;
				}
			}
			else
			{
				malLength = length;
			}

			/*
			** Allocate the data buffer for this field. We allow space for
			** the null terminator.
			*/
			if ((qDesc->valAddr[i] = malloc (malLength + 1)) == (char *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Could not allocate data buffer.");
				return (IMS_FATAL);
			}
		}
	}
	qDesc->coerced = IMS_ON;

	return (IMS_OK);
}

/******************************************************************************
**
** describeAttr () 
**
** Get the attribute descriptions.
**
******************************************************************************/

static int describeAttr (
	IMS_QI_DESC_OBJ *qDesc)
{
	int i;
	long length;
	IMS_MSG_STRUCT *msgDesc = qDesc->msgDesc;
	int numCols = qDesc->attrCount;

	/*
	** For each column, get the name, length and data type. Depending on
	** the data type, set the length of the field as it will appear in
	** the row buffer used for output.
	*/
	for (i = 0; i < numCols; i++)
	{
		if ((qDesc->attrName[i]
			= dbcolname (qDesc->dbproc, i + 1)) == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, ECOLRANGE, DBCOLNAME, i + 1);
			return (IMS_FATAL);
		}

		/*
		** Set the attribute name length too.
		*/
		qDesc->attrNameLen[i] = strlen (qDesc->attrName[i]);

		if ((length = (long) dbcollen (qDesc->dbproc, i + 1)) == -1)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, ECOLRANGE, DBCOLLEN, i + 1);
			return (IMS_FATAL);
		}

		if ((qDesc->attrType[i] = dbcoltype (qDesc->dbproc, i + 1)) == -1)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, ECOLRANGE, DBCOLTYPE, i + 1);
			return (IMS_FATAL);
		}

		switch (qDesc->attrType[i])
		{
		case SYBBINARY:
			length = 2 * length + 2;
			break;

		case SYBINT1:
			length = IMS_LENINT1;
			break;

		case SYBINT2:
			length = IMS_LENINT2;
			break;

		case SYBINT4:
			length = IMS_LENINT4;
			break;

		case SYBREAL:
			length = qDesc->realPrec + IMS_RF_OVERHEAD;
			if (length < qDesc->fltMaxWidth)
			{
				length = qDesc->fltMaxWidth;
			}
			break;

		case SYBFLT8:
			length = qDesc->fltPrec + IMS_RF_OVERHEAD;
			if (length < qDesc->fltMaxWidth)
			{
				length = qDesc->fltMaxWidth;
			}
			break;

		case SYBDATETIME:
			length = IMS_LENDATE;
			break;

		case SYBMONEY:
			length = IMS_LENMONEY;
			break;

#ifdef TEXT_IMAGE_TYPES
		case SYBTEXT:
			length = IMS_LENTEXT;
			break;

		case SYBIMAGE:
			length = IMS_LENIMAGE;
			break;
#endif	/* TEXT_IMAGE_TYPES */

		default:
			break;
		}

		qDesc->attrLength[i] = length;
	}

	return (IMS_OK);
}

/******************************************************************************
**
** freeDescription ()
**
** Free the buffers associated with the table description.
**
******************************************************************************/

static void freeDescription (
	IMS_QI_DESC_OBJ *qDesc)
{
	if (qDesc->attrName != (char **) NULL)
	{
		(void) free ((char *) qDesc->attrName);
		qDesc->attrName = (char **) NULL;
	}

	if (qDesc->attrNameLen != (short *) NULL)
	{
		(void) free ((char *) qDesc->attrNameLen);
		qDesc->attrNameLen = (short *) NULL;
	}

	if (qDesc->attrLength != (long *) NULL)
	{
		(void) free ((char *) qDesc->attrLength);
		qDesc->attrLength = (long *) NULL;
	}

	if (qDesc->attrType != (short *) NULL)
	{
		(void) free ((char *) qDesc->attrType);
		qDesc->attrType = (short *) NULL;
	}
}

/******************************************************************************
**
** freeDataBuffers ()
**
** Free  the data buffers associated with this query.
**
******************************************************************************/

static void freeDataBuffers (
	IMS_QI_DESC_OBJ *qDesc)
{
	int i;

	/*
	** If there are data buffers allocated for binding, then free them
	** too.
	*/
	if (qDesc->coerced == IMS_ON)
	{
		for (i = 0; i < qDesc->attrCount; i++)
		{
			if (qDesc->valAddr[i] != (char *) NULL)
			{
				(void) free ((void*)qDesc->valAddr[i]);
			}
		}
	}
	qDesc->attrCount = 0;

	/*
	** As far as we're concerned, no coercion exists any more.
	*/
	qDesc->coerced = IMS_OFF;

	if (qDesc->valAddr != (char **) NULL)
	{
		(void) free ((char *) qDesc->valAddr);
		qDesc->valAddr = (char **) NULL;
	}

	if (qDesc->valLength != (long *) NULL)
	{
		(void) free ((char *) qDesc->valLength);
		qDesc->valLength = (long *) NULL;
	}
}

/******************************************************************************
**
** ims_getResultsStatus ()
**
******************************************************************************/

int ims_getResultsStatus (
	IMS_QI_DESC_OBJ *qDesc)
{
	int severity = IMS_OK;
	IMS_MSG_STRUCT *msgDesc = qDesc->msgDesc;

	/*
	** Get the rows returned count.  If there were none returned, we get
	** back a value of -1.
	*/
	qDesc->count = (int) DBCOUNT (qDesc->dbproc);

	/*
	** If there is a return status from a Sybase procedure, check the
	** count. If the count is not -1, print both the count and the
	** status.  Otherwise just print the count.
	*/
	if (dbhasretstat (qDesc->dbproc))
	{
		qDesc->hasretstat = IMS_TRUE;
		if ((qDesc->procReturn = (int) dbretstatus (qDesc->dbproc)) != 0)
		{
			/* Return a message severity based on the return status. */
			if (qDesc->procReturn == -103)
			{
				severity = IMS_FATAL;
			}
			else if (qDesc->procReturn == -101)
			{
				severity = IMS_WARNING;
			}
			else if (qDesc->procReturn >= 0)
			{
				severity = IMS_OK;
			}
			else
			{
				severity = IMS_ERROR;
			}

			(void) ims_msgSeverity (msgDesc, severity);
		}
	}
	else
	{
		qDesc->hasretstat = IMS_FALSE;
		qDesc->procReturn = 0;
	}

	/*
	** Assign last message information to our query descriptor.
	*/
	ims_qiLastMsg (qDesc);

	return (severity);
}

/******************************************************************************
**
** ims_qiLastMsg ()
**
** Get last message information from the message facility and
** put any required info into the current query descriptor.
**
******************************************************************************/

void ims_qiLastMsg (
	IMS_QI_DESC_OBJ *qDesc)
{
	IMS_MSG_STRUCT *msgDesc = qDesc->msgDesc;

	qDesc->msgNo = ims_msgGetSybMsgNo (msgDesc);
	qDesc->dbsSeverity = ims_msgGetSybMsgSeverity (msgDesc);
}

/******************************************************************************
**
** reinitDesc ()
**
** Reinitialize the query returned pointers and values in
** the query descriptor.
**
******************************************************************************/

static void reinitDesc (
	IMS_QI_DESC_OBJ *qDesc)
{
	/*
	** Initialize portions of the query descriptor set by library routines.
	*/
	qDesc->attrCount = (short) NULL;
	qDesc->valAddr = (char **) NULL;
	qDesc->valLength = (long *) NULL;
	qDesc->attrName = (char **) NULL;
	qDesc->attrNameLen = (short *) NULL;
	qDesc->attrLength = (long *) NULL;
	qDesc->attrType = (short *) NULL;
	qDesc->hasretstat = (short) NULL;
	qDesc->procReturn = (int) NULL;
	qDesc->count = (int) NULL;
	qDesc->msgNo = SUCCEED;
	qDesc->qState = IMS_INITIAL;
	qDesc->retValueCount = 0;
	qDesc->connected = IMS_FALSE;
}

/******************************************************************************
**
**  getDbLoc ()
**
** Get alternate dataServer and database information from
** environment variables, if any are set.
**
**  IMS_SERVER   - Name of alternate database server.
**  IMS_DB       - Name of alternate database.
**  IMS_INT      - Name of alternate SYBASE interfaces file.
**
******************************************************************************/

static void getDbLoc (
	char **dataServer,
	char **database,
	char **interfaces)
{
	/*
	** Look at the environment variables IMS_SERVER, IMS_DB, and
	** IMS_INT.  Assign any of the variables defined to its argument.
	*/
	if ((*dataServer = getenv ("IMS_SERVER")) == (char *) NULL)
	{
		*dataServer = DATASERVER;
	}

	if ((*database = getenv ("IMS_DB")) == (char *) NULL)
	{
		*database = DATABASE;
	}

	*interfaces = getenv ("IMS_INT");
}

/******************************************************************************
**
** tinyConvert ()
**
** Convert a 1 byte integer to a NULL terminated string.
**
******************************************************************************/

static void tinyConvert (
	IMS_QI_DESC_OBJ *qDesc,
	int colIdx)
{
	char fmt[31];
	int maxAttrLength;
	unsigned char uBuf;

	if (dbdatlen (qDesc->dbproc, colIdx + 1) < 1)
	{
		if (qDesc->numLeft == IMS_OFF)
		{
			maxAttrLength = IMS_MAX (IMS_ATTRLENGTH (qDesc, colIdx),
				IMS_ATTRNAMELENGTH (qDesc, colIdx));
			(void) sprintf (fmt, "%%%ds", maxAttrLength);
		}
		else
		{
			(void) sprintf (fmt, "%%-%lds", IMS_ATTRLENGTH (qDesc, colIdx));
		}

		(void) sprintf ((char *) IMS_VALUE (qDesc, colIdx), fmt, "NULL");
		return;
	}
	else
	{
		/* There is some data. */
		uBuf = *(unsigned char *) dbdata (qDesc->dbproc, colIdx + 1);

		/* See if the mode is right justified. */
		if (qDesc->numLeft == IMS_OFF)
		{
			/* The number is right justified. */
			maxAttrLength = IMS_MAX (IMS_ATTRLENGTH (qDesc, colIdx),
				IMS_ATTRNAMELENGTH (qDesc, colIdx));

			(void) sprintf (fmt, "%%%du", maxAttrLength);
		}
		else
		{
			/*
			** The number is left justified.  Since it is, do not pad it
			** on the right. Left-justification is used only for list and
			** export-type formats.
			*/
			(void) strcpy (fmt, "%u");
		}
	}
	(void) sprintf ((char *) IMS_VALUE (qDesc, colIdx), fmt, uBuf);

	return;
}

/******************************************************************************
**
** smallConvert ()
**
** Convert a 2 byte integer to a NULL terinated string.
**
******************************************************************************/

static void smallConvert (
	IMS_QI_DESC_OBJ *qDesc,
	int colIdx)
{
	char fmt[31];
	int maxAttrLength;
	short sBuf;

	if (dbdatlen (qDesc->dbproc, colIdx + 1) < 1)
	{
		if (qDesc->numLeft == IMS_OFF)
		{
			maxAttrLength = IMS_MAX (IMS_ATTRLENGTH (qDesc, colIdx),
				IMS_ATTRNAMELENGTH (qDesc, colIdx));
			(void) sprintf (fmt, "%%%ds", maxAttrLength);
		}
		else
		{
			(void) sprintf (fmt, "%%-%lds", IMS_ATTRLENGTH (qDesc, colIdx));
		}

		(void) sprintf ((char *) IMS_VALUE (qDesc, colIdx), fmt, "NULL");
		return;
	}
	else
	{
		/*
		** There is some data.
		**
		** lint: pointer cast may result in improper alignment
		** ???? Review this.
		*/
		sBuf = *(short *) dbdata (qDesc->dbproc, colIdx + 1);

		/* See if the mode is right justified. */
		if (qDesc->numLeft == IMS_OFF)
		{
			/* The number is right justified. */
			maxAttrLength = IMS_MAX (IMS_ATTRLENGTH (qDesc, colIdx),
				IMS_ATTRNAMELENGTH (qDesc, colIdx));

			(void) sprintf (fmt, "%%%dd", maxAttrLength);
		}
		else
		{
			/*
			** The number is left justified.  Since it is, do not pad it
			** on the right. Left-justification is used only for list and
			** export-type formats.
			*/
			(void) strcpy (fmt, "%d");
		}
	}
	(void) sprintf ((char *) IMS_VALUE (qDesc, colIdx), fmt, sBuf);

	return;
}

/******************************************************************************
**
** intConvert ()
**
** Convert a 4 byte integer to a NULL terminated string.
**
******************************************************************************/

static void intConvert (
	IMS_QI_DESC_OBJ *qDesc,
	int colIdx)
{
	char fmt[31];
	int maxAttrLength;
	int iBuf;

	if (dbdatlen (qDesc->dbproc, colIdx + 1) < 1)
	{
		if (qDesc->numLeft == IMS_OFF)
		{
			maxAttrLength = IMS_MAX (IMS_ATTRLENGTH (qDesc, colIdx),
				IMS_ATTRNAMELENGTH (qDesc, colIdx));
			(void) sprintf (fmt, "%%%ds", maxAttrLength);
		}
		else
		{
			(void) sprintf (fmt, "%%-%lds", IMS_ATTRLENGTH (qDesc, colIdx));
		}

		(void) sprintf ((char *) IMS_VALUE (qDesc, colIdx), fmt, "NULL");
		return;
	}
	else
	{
		/*
		** There is some data.
		**
		** lint: pointer cast may result in improper alignment
		** ???? Review this.
		*/
		iBuf = *(int *) dbdata (qDesc->dbproc, colIdx + 1);

		/* See if the mode is right justified. */
		if (qDesc->numLeft == IMS_OFF)
		{
			/* The number is right justified. */
			maxAttrLength = IMS_MAX (IMS_ATTRLENGTH (qDesc, colIdx),
				IMS_ATTRNAMELENGTH (qDesc, colIdx));

			(void) sprintf (fmt, "%%%dd", maxAttrLength);
		}
		else
		{
			/*
			** The number is left justified.  Since it is, do not pad it
			** on the right. Left-justification is used only for list and
			** export-type formats.
			*/
			(void) strcpy (fmt, "%d");
		}
	}
	(void) sprintf ((char *) IMS_VALUE (qDesc, colIdx), fmt, iBuf);

	return;
}

/******************************************************************************
**
** realConvert ()
**
** Convert a 4 byte float to a NULL terminated string.
**
******************************************************************************/

static void realConvert (
	IMS_QI_DESC_OBJ *qDesc,
	int colIdx)
{
	char fmt[31];
	int maxAttrLength;
	float rBuf;
	char bigBuf[310+255]; /* 310+max(0,ndigit) +  1 */
	char *p;
	int i;

	maxAttrLength = IMS_MAX (IMS_ATTRLENGTH (qDesc, colIdx),
		IMS_ATTRNAMELENGTH (qDesc, colIdx));

	if (dbdatlen (qDesc->dbproc, colIdx + 1) < 1)
	{
		if (qDesc->numLeft == IMS_OFF)
		{
			(void) sprintf (fmt, "%%%ds", maxAttrLength);
		}
		else
		{
			(void) sprintf (fmt, "%%-%lds", IMS_ATTRLENGTH (qDesc, colIdx));
		}

		(void) sprintf ((char *) IMS_VALUE (qDesc, colIdx), fmt, "NULL");
		return;
	}
	else
	{
		/*
		** There is some data.
		**
		** lint: pointer cast may result in improper alignment
		** ???? Review this.
		*/
		rBuf = *(float *) dbdata (qDesc->dbproc, colIdx + 1);

		/* See if the mode is right justified. */
		if (qDesc->numLeft == IMS_OFF)
		{
			/* The number is right justified. */
			(void) sprintf (fmt, "%%#%d.%d%c", maxAttrLength, qDesc->realPrec,
				qDesc->floatConv);
		}
		else
		{
			/*
			** The number is left justified.  Since it is, do not pad it
			** on the right. Left-justification is used only for list and
			** export-type formats.
			*/
			(void) sprintf (fmt, "%%#.%d%c", qDesc->realPrec, qDesc->floatConv);
		}
	}

	if (qDesc->floatConv == 'f')
	{
		/* These conversions are not well behaved and may overflow. */
		(void) sprintf (bigBuf, fmt, rBuf);
		if ((int) strlen (bigBuf) > maxAttrLength)
		{
			p = (char *) IMS_VALUE (qDesc, colIdx);
			for (i = 0; i < maxAttrLength; i++)
			{
				*(p++) = '*';
			}
			*p = '\0';
		}
		else
		{
			(void) strcpy ((char *) IMS_VALUE (qDesc, colIdx), bigBuf);
		}
	}
	else
	{
		(void) sprintf ((char *) IMS_VALUE (qDesc, colIdx), fmt, rBuf);
	}

	return;
}

/******************************************************************************
**
** floatConvert ()
**
** Convert an 8 byte float to a NULL terminated string.
**
******************************************************************************/

static void floatConvert (
	IMS_QI_DESC_OBJ *qDesc,
	int colIdx)
{
	char fmt[31];
	int maxAttrLength;
	double fBuf;
	char bigBuf[310+255]; /* 310+max(0,ndigit) +  1 */
	char *p;
	int i;

	maxAttrLength = IMS_MAX (IMS_ATTRLENGTH (qDesc, colIdx),
		IMS_ATTRNAMELENGTH (qDesc, colIdx));

	if (dbdatlen (qDesc->dbproc, colIdx + 1) < 1)
	{
		if (qDesc->numLeft == IMS_OFF)
		{
			(void) sprintf (fmt, "%%%ds", maxAttrLength);
		}
		else
		{
			(void) sprintf (fmt, "%%-%lds", IMS_ATTRLENGTH (qDesc, colIdx));
		}

		(void) sprintf ((char *) IMS_VALUE (qDesc, colIdx), fmt, "NULL");
		return;
	}
	else
	{
		/*
		** There is some data.
		**
		** lint: pointer cast may result in improper alignment
		** ???? Review this.
		*/
		fBuf = *(double *) dbdata (qDesc->dbproc, colIdx + 1);

		/* See if the mode is right justified. */
		if (qDesc->numLeft == IMS_OFF)
		{
			/* The number is right justified. */
			(void) sprintf (fmt, "%%#%d.%d%c", maxAttrLength, qDesc->fltPrec,
				qDesc->floatConv);
		}
		else
		{
			/*
			** The number is left justified.  Since it is, do not pad it
			** on the right. Left-justification is used only for list and
			** export-type formats.  Note: %c allows the use of  different
			** conversion types.
			*/
			(void) sprintf (fmt, "%%#.%d%c", qDesc->fltPrec, qDesc->floatConv);
		}
	}

	if (qDesc->floatConv == 'f')
	{
		/* These conversions are not well behaved and may overflow. */
		(void) sprintf (bigBuf, fmt, fBuf);
		if ((int) strlen (bigBuf) > maxAttrLength)
		{
			p = (char *) IMS_VALUE (qDesc, colIdx);
			for (i = 0; i < maxAttrLength; i++)
			{
				*(p++) = '*';
			}
			*p = '\0';
		}
		else
		{
			(void) strcpy ((char *) IMS_VALUE (qDesc, colIdx), bigBuf);
		}
	}
	else
	{
		(void) sprintf ((char *) IMS_VALUE (qDesc, colIdx), fmt, fBuf);
	}

	return;
}
