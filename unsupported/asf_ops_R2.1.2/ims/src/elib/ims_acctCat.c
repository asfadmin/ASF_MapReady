static char *sccs = "@(#)ims_acctCat.c	5.1  16 Mar 1996";
/*****************************************************************************
**
** File:        ims_acctCat.c
**
** Function:    Catalog database access facility for the ims_acct.
**
** Author:      Armando Cardona
**
** Date:        6/26/95
**
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_dbms.h>
#include <ims_qi.h>
#include <ims_util.h>
#include <ims_acctCat.h>

/*
** Local Functions (Entry points into this module)
**
** This function prototype can be found in ims_acctCat.h.
** It is listed here for reference.
**
**	int ims_acctCat (IMS_QI_DESC_OBJ *, IMS_MSG_STRUCT *,
**		IMS_ACCT_CAT_EVENT, void *);
*/

/*
** Local Functions.
*/
static int validate_user ( IMS_QI_DESC_OBJ *, IMS_MSG_STRUCT *) ;
static int validate_account ( IMS_QI_DESC_OBJ *, IMS_MSG_STRUCT *, void * ) ;
static int account_transaction (IMS_QI_DESC_OBJ *, IMS_MSG_STRUCT * ) ;
static int get_account_lock ( IMS_QI_DESC_OBJ *, IMS_MSG_STRUCT * );
static int beginTransaction ( IMS_QI_DESC_OBJ *, IMS_MSG_STRUCT * );
static int commitTransaction ( IMS_QI_DESC_OBJ *, IMS_MSG_STRUCT * );
static int rollbackTransaction ( IMS_QI_DESC_OBJ *, IMS_MSG_STRUCT * );
static int execCmd ( IMS_QI_DESC_OBJ *, IMS_MSG_STRUCT *);
static int processRetStatus ( IMS_QI_DESC_OBJ *, IMS_MSG_STRUCT * );

/******************************************************************************
**
** ims_acctCat ()
** 
** Main function handling catalog queries.
**
******************************************************************************/

int ims_acctCat (
	IMS_QI_DESC_OBJ *qDesc,
	IMS_MSG_STRUCT *msgDesc,
	IMS_ACCT_CAT_EVENT event,
	void * param)
{
	int status;


	/*
	** Now, let's do our 'catalog' business according to the type of event
	** passed into the function.  
	*/
	switch (event)
	{

	case IMS_ACCT_VALIDATE_USER:
		status = validate_user (qDesc, msgDesc);
		break;

	case IMS_ACCT_VALIDATE_ACCOUNT:
		status = validate_account (qDesc, msgDesc, param);
		break;

	case IMS_ACCT_ACCOUNT_TRANSACTION:
		status = account_transaction (qDesc, msgDesc);
		break;

	case IMS_ACCT_GETACCOUNTLOCK:
		status = get_account_lock (qDesc, msgDesc);
		break;

	case IMS_ACCT_BEGINTRANSACTION:
		status = beginTransaction (qDesc, msgDesc);
		break;

	case IMS_ACCT_ROLLBACKTRANSACTION:
		status = rollbackTransaction (qDesc, msgDesc);
		break;

	case IMS_ACCT_COMMITTRANSACTION:
		status = commitTransaction (qDesc, msgDesc);
		break;

	default:
		(void) ims_msg (msgDesc, IMS_FATAL,
		"Invalid catalog event passed to ims_acctCat.");
		status = IMS_FATAL;
		break;
	}

	/*
	** Release all query-allocated space and re-initialize qDesc for the
	** next time in, leaving open the connection to the catalog.
	*/
	if (qDesc->dbproc != (DBPROCESS *) NULL)
	{
		/*
		** Re-initialize query descriptor for next command, but do
		** not cancel previous command
		*/
		if (ims_qiResetDesc(qDesc) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"ims_acctCat: Could not reinitialize query descriptor.");
			return(IMS_FATAL);
		}
	}

	/*
	** Return with the appropriate status
	*/
	if (status < IMS_OK) return (status);


	return (IMS_OK);
}

/******************************************************************************
**
** validate_user ()
**
** Execute stored procedure validate_user
**
******************************************************************************/

static int validate_user (
	IMS_QI_DESC_OBJ *qDesc,
	IMS_MSG_STRUCT *msgDesc)
{

	/*
	** Execute stored procedure validate_user
	*/

	if (execCmd(qDesc,msgDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
		"Stored procedure acc_validate_user returned with error.");
		return(IMS_FATAL);
	}
	return(IMS_OK);
}
/******************************************************************************
**
** validate_account ()
**
** Execute stored procedure validate_account
**
******************************************************************************/

static int validate_account (
	IMS_QI_DESC_OBJ *qDesc,
	IMS_MSG_STRUCT *msgDesc,
	void *param)
{
    /*
    ** Execute stored procedure validate_account
    */

    if (execCmd(qDesc,msgDesc) < IMS_OK)
    {
	(void) ims_msg (msgDesc, IMS_FATAL,
            "Stored procedure acc_validate_account returned with error");
	return(IMS_FATAL);
    }

    /* 
    ** copy the returned data 
    */
    (void) memcpy ((char *) param, qDesc->valAddr[0], qDesc->valLength[0]) ;
    return(IMS_OK);
}

/******************************************************************************
**
** account_transaction ()
**
** Execute stored procedure acc_update_balance
**
******************************************************************************/

static int account_transaction (
	IMS_QI_DESC_OBJ *qDesc,
	IMS_MSG_STRUCT *msgDesc)
{
	/*
	** Execute stored procedure acc_update_balance
	*/

	if (execCmd(qDesc,msgDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
		"Stored procedure acc_update_balance returned with error");
		return(IMS_FATAL);
	}
	return(IMS_OK);
}

/****************************************************************************
**
** beginTransaction ()
**
** Open a transaction.
**
****************************************************************************/

static int beginTransaction (
	IMS_QI_DESC_OBJ *qDesc,
	IMS_MSG_STRUCT *msgDesc)
{
	char buffer [IMS_COL128_LEN+1] ;

	(void) sprintf (buffer, "begin transaction");
        qDesc->cmd = buffer ;
	if (execCmd (qDesc,msgDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"ims-acctCat:"
                       " Could not begin transaction.");
		return(IMS_FATAL);
	}
	return(IMS_OK);
}

/******************************************************************************
**
** get_account_lock ()
**
** Execute stored procedure get_account_lock to update table account
**
******************************************************************************/

static int get_account_lock (
	IMS_QI_DESC_OBJ *qDesc,
	IMS_MSG_STRUCT *msgDesc)
{
	char buffer [IMS_COL128_LEN+1] ;

	(void) sprintf (buffer, "get_account_lock");
        qDesc->cmd = buffer ;


	if (execCmd(qDesc,msgDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
		"execution of stored procedure get_account_lock failed.");
		return(IMS_FATAL);
	}
	return(IMS_OK);
}

/******************************************************************************
**
** commitTransaction ()
**
** Commit the transaction opened by openTransaction.
**
******************************************************************************/

static int commitTransaction (
	IMS_QI_DESC_OBJ *qDesc,
	IMS_MSG_STRUCT *msgDesc)
{
	char buffer [IMS_COL128_LEN+1] ;

	(void) sprintf (buffer, "commit transaction");
        qDesc->cmd = buffer ;


	if (execCmd(qDesc,msgDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not commit transaction.");
		return(IMS_FATAL);
	}
	return(IMS_OK);
}

/******************************************************************************
**
** rollbackTransaction ()
**
** Rollback the transaction opened by openTransaction.
**
******************************************************************************/

static int rollbackTransaction (
	IMS_QI_DESC_OBJ *qDesc,
	IMS_MSG_STRUCT *msgDesc)
{
	char buffer [IMS_COL128_LEN+1] ;

	(void) sprintf (buffer, "rollback transaction");
        qDesc->cmd = buffer ;

	if (execCmd(qDesc,msgDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not rollback transaction.");
		return(IMS_FATAL);
	}
	return(IMS_OK);
}

/******************************************************************************
**
** execCmd ()
**
** Execute an SQL procedure that writes data into the catalog database.
** We don't pass a parameter, but assume that when this function is called,
** the declared static buffer 'qDesc->cmd' has been properly filled in with
** the SQL statement to be executed.
**
** THIS ROUTINE IS ONLY USED FOR EXEUTING SQL STATEMENTS THAT WILL NOT RETURN
** ROWS FROM THE DATABASE OR ONLY ONE ROW IS EXPECTED.
**
** If a deadlock occurs, reexecute the operation from the restart point.
**
******************************************************************************/

static int execCmd (
	IMS_QI_DESC_OBJ *qDesc,
	IMS_MSG_STRUCT *msgDesc)
{
	RETCODE status;
	int severity;


	while ( (status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		/*
		** Check the returned status
		*/
		if (status < IMS_OK)
		{
			return (status);
		}
	}

	/*
	** Check the stored procedure status returend value.
	*/
	if ((severity = processRetStatus (qDesc,msgDesc)) < IMS_OK)
	{
		return (severity);
	}

	if (qDesc->msgNo != 0)
	{
		return(ims_msgGetSeverity(msgDesc));
	}
	return (IMS_OK);
}

/****************************************************************************
**
** processRetStatus ()
**
** Check the procedure returned status value.
** When status returned is not an error, then return IMS_OK.
**
****************************************************************************/

static int processRetStatus (
	IMS_QI_DESC_OBJ *qDesc,
	IMS_MSG_STRUCT *msgDesc)
{
	int procReturn, severity;

	/*
	** Check to see if the Sybase procedure returned a status.  If it did
	** and it is not 0 (the OK value for a return), deal with the error.
	** Return status of less than -100 correspond to message facility
	** severity levels modulo 100.
	** Return status of less than -200 correspond to stored procedure
	** error messages.
	*/
	if (IMS_HASRETSTAT (qDesc) == IMS_TRUE)
	{
		if ((procReturn = IMS_PROCRETURN (qDesc)) != 0)
		{
			switch (procReturn)
			{
				case -101:
					severity = IMS_WARNING;
					break;

				case -102:
					severity = IMS_ERROR;
					break;

				case -103:
					severity = IMS_FATAL;
					break;

				default:
					severity = IMS_ERROR;
					(void) ims_msg (msgDesc, severity,
					"Procedure '%s' returned an unrecognized status of '%d'.",
						qDesc->cmd, procReturn);
					break;
			}
			return (severity);
		}
	}
	return (IMS_OK);
}

