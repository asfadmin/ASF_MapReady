static char *sccs = "@(#)ims_qiEx2.c	5.1  03/17/96";
/******************************************************************************
**
** File:        ims_qiEx2.c
**
** Function:    IMS example program for the Query Interface.
**
** Author:      S. Hardman
**
** Date:        1/26/95
**
** Notes:
**
** The following command will make the executable on SunOS Release 5.x:
**
** cc -I/asf/include/imsdads -I$SYBASE/include -L/asf/lib -L$SYBASE/lib \
** ims_qiEx2.c -lims -lsybdb -lnsl -lm -o ims_qiEx2
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <ims_query.h>

/* Local Functions. */
static int createTable (IMS_QI_DESC_OBJ *);
static int createProcedure (IMS_QI_DESC_OBJ *);
static int insertRow (IMS_QI_DESC_OBJ *);
static int printResultsProc (IMS_QI_DESC_OBJ *); 
static int checkReturnStatus (IMS_QI_DESC_OBJ *);
static int dropObjects (IMS_QI_DESC_OBJ *);

/* Static Variables. */
static char *progName = "ims_qiEx2"; 
static IMS_MSG_STRUCT *msgDesc; 

/* Structure definition. */
typedef struct ex_product
{
	int product_index;
	char product_name[30+1];
	char product_date[26+1];
	struct ex_product *next;
} EX_PRODUCT;

/* Database access information variables. */
static char userName[IMS_NAME_LEN + 1];
static char password[IMS_NAME_LEN + 1];
static char server[IMS_NAME_LEN + 1];
static char dbName[IMS_NAME_LEN + 1];

/******************************************************************************
**
** main ()
**
******************************************************************************/

void main (void)
{
	IMS_QI_DESC_OBJ *qDesc;
	int status;

	/*
	** Initialize the IMS Message Facility which will allow messages to
	** be retrieved from the message queue.
	*/
	if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
	{
		(void) fprintf (stderr, 
			"Memory allocation for IMS_MSG_STRUCT structure failed.");
		exit (-1);
	}

	/* Initialize the message facility options. */
	(void) ims_msgSubSystem (msgDesc, "IMS");
	(void) ims_msgBanner (msgDesc, progName, IMS_MSG_ALLBANNER);
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

	/* Allocate a query descriptor */
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"Could not allocate a query descriptor.");
		exit (-1);
	}
       
	/* Obtain database access information. */ 
	(void) fprintf (stdout, "Enter db user name  : ");
	(void) gets (userName);
	(void) fprintf (stdout, "Enter password      : ");
	(void) gets (password);
	(void) fprintf (stdout, "Enter server name   : ");
	(void) gets (server);
	(void) fprintf (stdout, "Enter database name : ");
	(void) gets (dbName);

	/* Initialize the query descriptor for database server access. */
	IMS_SETPROG (qDesc, progName);
	IMS_SETUSER (qDesc, userName);
	IMS_SETPSWD (qDesc, password);
	IMS_SETSERVER (qDesc, server);
	IMS_SETDBNAME (qDesc, dbName);

	/* Login to the specified server. */
	if ((status = ims_qiLogin (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Database server login failed.");
		exit (-1);
	}

	/*
	** Saves the message descriptor for use by
	** Sybase error and message handling.
	*/
	IMS_SET_USERDATA (qDesc);

	/* Create the example table qi_ex_table. */
	if ((status = createTable (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not create the example table.");
		exit (-1);
	}

	/* Create the stored procedure. */
	if ((status = createProcedure (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not create the stored procedure.");
		(void) dropObjects (qDesc);
		exit (-1);
	}

	/* Insert row into table qi_ex_table. */
	if ((status = insertRow (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not insert row into table.");
		(void) dropObjects (qDesc);
		exit (-1);
	}

	/* Perform the stored procedure and print the results. */
	if ((status = printResultsProc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not obtain results from the stored procedure.");	
		(void) dropObjects (qDesc);
		exit (-1);
	}

	/* Drop the objects created in this example. */
	if ((status = dropObjects (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not drop objects.");
	}

	/* Logoff from the server and free up the query descriptor. */
	(void) ims_qiFreeDesc (qDesc);

	/* Free the message descriptor. */
	(void) ims_msgStructFree (msgDesc);

	exit (0);
}

/******************************************************************************
**
** createTable ()
**
******************************************************************************/

static int createTable (
	IMS_QI_DESC_OBJ *qDesc)
{
	int status;
	char *createCommand =
		"create table qi_ex_table ( \
		product_index	int, \
		product_name	varchar(30), \
		product_date	datetime \
		)";

	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Unable to reset query descriptor.");
		return (status);
	}

	IMS_SETCMD (qDesc, createCommand);

	/* We ignore any result rows returned by the server. */
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (status);
		}
	}

	return (IMS_OK);
}

/******************************************************************************
**
** createProcedure ()
**
******************************************************************************/

static int createProcedure (
	IMS_QI_DESC_OBJ *qDesc)
{
	int status;

	char *createProc =
		"create procedure show_qi_ex_table as \
		begin \
		select product_index, product_name, product_date \
		from qi_ex_table \
		end";

	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Unable to reset query descriptor.");
		return (status);
	}

	IMS_SETCMD (qDesc, createProc);

	/* We ignore any result rows returned by the server. */
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (status);
		}
	}

	return (IMS_OK);
}

/******************************************************************************
**
** insertRow ()
**
******************************************************************************/

static int insertRow (
	IMS_QI_DESC_OBJ *qDesc)
{
	int status;
	char commandBuf[1024];

	static char *insRow1 = 
		"insert into qi_ex_table \
		(product_index, product_name, product_date) \
		values (1, 'DEMO1', 'Jan 26 1995 9:45AM')";

	static char *insRow2 = 
		"insert into qi_ex_table \
		(product_index, product_name, product_date) \
		values (2, 'DEMO2', 'Jan 26 1995 11:45AM')";

	static char *insRow3 = 
		"insert into qi_ex_table \
		(product_index, product_name, product_date) \
		values (3, 'DEMO3', 'Jan 26 1995 1:45PM')";

	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Unable to reset query descriptor.");
		return (status);
	}

	(void) sprintf (commandBuf, "%s %s %s", insRow1, insRow2, insRow3);

	IMS_SETCMD (qDesc, commandBuf);

	/* We ignore any result rows returned by the server. */
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (status);
		}
	}

	return (IMS_OK);
}

/******************************************************************************
**
** printResultsProc ()
**
** This function allocates a structure and copies the return data
** into the structure.
**
******************************************************************************/

int printResultsProc (
	IMS_QI_DESC_OBJ *qDesc)
{
	EX_PRODUCT *firstPtr;
	EX_PRODUCT *currPtr;
	EX_PRODUCT *prevPtr;
	int status;
	int rowCount = 0;
	char *procCommand = "show_qi_ex_table";

	firstPtr = currPtr = prevPtr = (EX_PRODUCT *) NULL;

	/* Reset the query descriptor. */
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Unable to reset query descriptor.");
		return (status);
	}

	IMS_SETCMD (qDesc, procCommand);

	/* Load the query descriptor with return attribute information. */
	if ((status = ims_qiTblDesc (qDesc)) < IMS_OK)
	{
		return (status);
	}

	/* Process the rows returned. */
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		/* Something went wrong, so let's return. */
		if (status < IMS_OK)
		{
			return (status);
		}

		/*
		** If the current query has ended, we want to finish out the
		** command batch and return.
		*/
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/* A row has been returned. */
		rowCount++;

		/*
		** Allocate space for EX_PRODUCT structure.
		**
		** lint: pointer cast may result in improper alignment
		** No problem, malloc() aligns on worst case boundary.
		*/
		if ((currPtr = (EX_PRODUCT *) malloc
			((unsigned) sizeof (EX_PRODUCT))) ==
			(EX_PRODUCT *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate memory for EX_PRODUCT structure.");
			(void) ims_qiCancel (qDesc);
			return (IMS_FATAL);
		}

		/* firstPtr points to the first element of the list. */
		if (rowCount == 1)
		{
			firstPtr = currPtr;
		}
		else
		{
			prevPtr->next = currPtr;
		}

		currPtr->next = (EX_PRODUCT *) NULL;

		/* Copy the returned data into the structure. */
		(void) memcpy ((char *) &(currPtr->product_index),
			IMS_VALUE (qDesc, 0), IMS_VALUELENGTH (qDesc, 0));

		(void) memcpy (currPtr->product_name,
			IMS_VALUE (qDesc, 1), IMS_VALUELENGTH (qDesc, 1));
		currPtr->product_name[IMS_VALUELENGTH (qDesc, 1)] = '\0';

		IMS_CONVERT (qDesc, 2, IMS_CHAR, currPtr->product_date, 26);
		currPtr->product_date[26] = '\0';

		prevPtr = currPtr;
	}

	/* Check the return status of the stored procedure. */
	if ((status = checkReturnStatus (qDesc)) < IMS_OK)
	{
		return (status);
	}

	(void) fprintf (stdout, "\nResults from the stored procedure.\n\n");

	/* Print out the rows of data. */
	rowCount = 0;
	currPtr = firstPtr;
	while (currPtr != (EX_PRODUCT *) NULL)
	{
		rowCount++;
		(void) fprintf (stdout, "Row # %d\n", rowCount);

		(void) fprintf (stdout, "product_index = %d\n",
			currPtr->product_index);

		(void) fprintf (stdout, "product_name = %s\n",
			currPtr->product_name);

		(void) fprintf (stdout, "product_date = %s\n",
			currPtr->product_date);

		(void) fprintf (stdout, "\n");

		currPtr = currPtr->next;
	}

	/* Free the allocated structure. */
	currPtr = firstPtr;
	while (currPtr != (EX_PRODUCT *) NULL)
	{
		prevPtr = currPtr->next;
		(void) free ((char *) currPtr);
		currPtr = prevPtr;
	}

	return (IMS_OK);
}

/******************************************************************************
**
** checkReturnStatus ()
**
******************************************************************************/

static int checkReturnStatus (
	IMS_QI_DESC_OBJ *qDesc)
{
	int procReturn;

	/* Check for returned status. */
	if (IMS_HASRETSTAT (qDesc) == IMS_TRUE)
	{
		/* Get the status. */
		if ((procReturn = IMS_PROCRETURN (qDesc)) < 0)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Procedure returned an error status of %d.", procReturn);
			return (IMS_ERROR);
		}
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Procedure did not return a status.");
		return (IMS_FATAL);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** dropObjects ()
**
******************************************************************************/

static int dropObjects (
	IMS_QI_DESC_OBJ *qDesc)
{
	int status;
	char *dropCommand = "\
		drop table qi_ex_table \
		drop procedure show_qi_ex_table"; 

	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Unable to reset query descriptor.");
		return (status);
	}

	IMS_SETCMD (qDesc, dropCommand);

	/* We ignore any result rows returned by the server. */
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (status);
		}
	}

	return (IMS_OK);
}
