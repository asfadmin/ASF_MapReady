static char *sccs = "@(#)ims_qiEx1.c	5.1  03/17/96";
/******************************************************************************
**
** File:        ims_qiEx1.c
**
** Function:    IMS example program for the Query Interface.
**
** Author:      J. Young
**
** Date:        1/18/93
**
** Notes:       This example demonstrates two ways of using the Query
**              Interface to obtain data from a database server.
**              The first example is in the printResultsDirect() function
**              where we access the return values directly in their native
**              data types. The second example is in the printResultsBind()
**              function where we first bind the variables to the return
**              fields and then accesses the data through these variables.
**
** The following command will make the executable on SunOS Release 5.x:
**
** cc -I/asf/include/imsdads -I$SYBASE/include -L/asf/lib -L$SYBASE/lib \
** ims_qiEx1.c -lims -lsybdb -lnsl -lm -o ims_qiEx1
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <ims_query.h>

/* Local Functions. */
static int createTable (IMS_QI_DESC_OBJ *);
static int insertRow (IMS_QI_DESC_OBJ *);
static int printResultsDirect (IMS_QI_DESC_OBJ *); 
static int printResultsBind (IMS_QI_DESC_OBJ *); 
static int dropTable (IMS_QI_DESC_OBJ *);

/* Static Variables. */
static char *progName = "ims_qiEx1"; 
static IMS_MSG_STRUCT *msgDesc; 

/*
** The following variable contains a SQL statement
** that will be utilized in this program.
*/
static char *selectCommand = 
"select intCol, sIntCol, tIntCol, fltCol, realCol, charCol, \
 vCharCol, binCol, vBinCol, bitCol, monCol, sMonCol, dateCol, sDateCol \
 from qi_ex_table where tIntCol = 230";

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

	/* Insert row into table qi_ex_table. */
	if ((status = insertRow (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not insert row into table.");
		(void) dropTable (qDesc);
		exit (-1);
	}

	/* Perform the first query. */
	if ((status = printResultsDirect (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not print out query results from the first query.");
		(void) dropTable (qDesc);
		exit (-1);
	}

	/* Perform the second query. */
	if ((status = printResultsBind (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not print out query results from the second query.");
		(void) dropTable (qDesc);
		exit (-1);
	}

	/* Drop the example table qi_ex_table. */
	if ((status = dropTable (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not drop the example table.");
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
		intCol	int	NULL, \
		sIntCol	smallint	NULL, \
		tIntCol	tinyint	NULL, \
		fltCol	float   NULL, \
		realCol	real	NULL, \
		charCol	char(30)	NULL, \
		vCharCol	varchar(20)	NULL, \
		binCol	binary(2)	NULL, \
		vBinCol	varbinary(4)	NULL, \
		bitCol	bit, \
		monCol	money	NULL, \
		sMonCol	smallmoney	NULL, \
		dateCol	datetime	NULL, \
		sDateCol	smalldatetime	NULL \
		)";

	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Unable to reset database descriptor.");
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
** insertRow ()
**
******************************************************************************/

static int insertRow (
	IMS_QI_DESC_OBJ *qDesc)
{
	int status;
	static char *insCommand = 
		"insert into qi_ex_table (intCol, sIntCol, tIntCol, fltCol, \
		realCol, charCol, vCharCol, binCol, vBinCol, bitCol, monCol, \
		sMonCol, dateCol, sDateCol) \
		values (436734, -3865, 230, 1.4588E3, -17.983, 'Test Row 1', \
		'Created 18 Jan 1993', 0x3A, 0xEF7, 1, 183.44, 8.99, \
		'JAN 18 1993 12:45PM', 'AUG 29 1964 09:05AM')";

	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Unable to reset database descriptor.");
		return (status);
	}

	IMS_SETCMD (qDesc, insCommand);

	/* We ignore any result rows returned by the server. */
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (status);
		}
	}

	/* Print out the number of rows affected. */
	(void) fprintf (stdout, "\nRows inserted: %d\n\n", IMS_AFFECTED (qDesc));

	return (IMS_OK);
}

/******************************************************************************
**
** printResultsDirect ()
**
** This first query accesses the return values directly from the
** RDBMS buffers in the native data types.
** For each attribute in the return set, print out the returned value.
**
******************************************************************************/

int printResultsDirect (
	IMS_QI_DESC_OBJ *qDesc)
{
	int i;
	int status;
	int numAttrs;
	int numRows = 0;

	/*
	** These variables are utilized for the display of attributes which
	** are of the SYBASE native types not directly displayed by an
	** fprintf() statement. Such attributes are converted to a printable
	** (and compatible) C data type.
	*/
	char varChar[30 + 1];     /* Temp holder to allow null termination. */
	char varCharDate[26 + 1]; /* For conversion of all datetime attributes. */
	long varLngInt;           /* For conversion of binary attributes. */
	int varInt;               /* For conversion of bit attributes. */
	float varFloat;           /* For conversion of smallmoney attributes. */
	double varDouble;         /* For conversion of money attributes. */

	/* Reset the query descriptor. */
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Unable to reset database descriptor.");
		return (status);
	}

	/* Now set the select command into the query descriptor. */
	IMS_SETCMD (qDesc, selectCommand);

	/* Load the query descriptor with return attribute information. */
	if ((status = ims_qiTblDesc (qDesc)) < IMS_OK)
	{
		return (status);
	}

	/* Get the number of attributes. */
	numAttrs = IMS_ATTRCOUNT (qDesc);

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

		numRows++;
		(void) fprintf (stdout, "Row # %d\n", numRows);

		/* Loop to print out each attribute's name and value. */
		for (i = 0; i < numAttrs; i ++)
		{
			switch (IMS_ATTRTYPE (qDesc, i))
			{
			case IMS_CHAR:
				(void) memcpy (varChar, (char *) IMS_VALUE (qDesc, i),
					IMS_VALUELENGTH (qDesc, i));
				varChar[IMS_VALUELENGTH (qDesc, i)] = '\0';
				(void) fprintf (stdout, "%s = %s\n", IMS_ATTRNAME (qDesc, i),
					varChar);
				break;

			case IMS_TINYINT:
				(void) fprintf (stdout, "%s = %u\n", IMS_ATTRNAME (qDesc, i),
					*(unsigned char *) IMS_VALUE (qDesc, i));
				break;

			case IMS_SMALLINT:
				(void) fprintf (stdout, "%s = %hd\n", IMS_ATTRNAME (qDesc, i),
					*(short *) IMS_VALUE (qDesc, i));
				break;

			case IMS_INT:
				(void) fprintf (stdout, "%s = %d\n", IMS_ATTRNAME (qDesc, i),
					*(int *) IMS_VALUE (qDesc, i));
				break;

			case IMS_REAL:
				(void) fprintf (stdout, "%s = %f\n", IMS_ATTRNAME (qDesc, i),
					*(float *) IMS_VALUE (qDesc, i));
				break;

			case IMS_FLT8:
				(void) fprintf (stdout, "%s = %f\n", IMS_ATTRNAME (qDesc, i),
					*(double *) IMS_VALUE (qDesc, i));
				break;

			case IMS_BINARY:
				IMS_CONVERT (qDesc, i, IMS_INT, &varLngInt, 0);
				(void) fprintf (stdout, "%s = 0x%x\n",
					IMS_ATTRNAME (qDesc, i), varLngInt);
				break;

			case IMS_BIT:
				IMS_CONVERT (qDesc, i, IMS_INT, &varInt, 0);
				(void) fprintf (stdout, "%s = %d\n",
					IMS_ATTRNAME (qDesc, i), varInt);
				break;

			case IMS_MONEY:
				IMS_CONVERT (qDesc, i, IMS_FLT8, &varDouble, 0);
				(void) fprintf (stdout, "%s = %.2f\n",
					IMS_ATTRNAME (qDesc, i), varDouble);
				break;

			case IMS_SMALLMONEY:
				IMS_CONVERT (qDesc, i, IMS_REAL, &varFloat, 0);
				(void) fprintf (stdout, "%s = %.2f\n",
					IMS_ATTRNAME (qDesc, i), varFloat);
				break;

			case IMS_DATETIME:
				IMS_CONVERT (qDesc, i, IMS_CHAR, varCharDate, 26);
				varCharDate[26] = '\0';
				(void) fprintf (stdout, "%s = %.26s\n",
					IMS_ATTRNAME (qDesc, i), varCharDate);
				break;

			case IMS_SMALLDATETIME:
				IMS_CONVERT (qDesc, i, IMS_CHAR, varCharDate, 19);
				varCharDate[19] = '\0';
				(void) fprintf (stdout, "%s = %.19s\n",
					IMS_ATTRNAME (qDesc, i), varCharDate);
				break;
			}
		}
		(void) fprintf (stdout, "\n");
	}

	/* Print out the number of rows affected. */
	(void) fprintf (stdout, "Rows queried: %d\n\n", IMS_AFFECTED (qDesc));

	if (status < IMS_OK)
	{
		return (status);
	}
	else
	{
		return (IMS_OK);
	}
}

/******************************************************************************
**
** printResultsBind ()
**
** This second query first binds variables to the return fields then
** accesses the data through these variables.
** For each attribute in the return set, print out the returned value.
**
******************************************************************************/

int printResultsBind (
	IMS_QI_DESC_OBJ *qDesc)
{
	int status;
	int rowCount = 0;

	/* Variables for column bindings. */
	long var_intCol;
	short var_sIntCol;
	unsigned char var_tIntCol;
	double var_fltCol;
	float var_realCol;
	char var_charCol[30 + 1];
	char var_vCharCol[20 + 1];
	int var_binCol; 
	int var_vBinCol;
	int var_bitCol;
	double var_monCol;
	float var_sMonCol;
	char var_dateCol[26 + 1];
	char var_sDateCol[26 + 1];

	/* Reset the query descriptor. */
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Unable to reset database descriptor.");
		return (status);
	}

	/* Now set the select command into the query descriptor. */
	IMS_SETCMD (qDesc, selectCommand);

	/* Load the query descriptor with return attribute information. */
	if ((status = ims_qiTblDesc (qDesc)) < IMS_OK)
	{
		return (status);
	}

	IMS_BIND (qDesc,  1, IMS_INTBIND,         0, &var_intCol); 
	IMS_BIND (qDesc,  2, IMS_SMALLBIND,       0, &var_sIntCol);
	IMS_BIND (qDesc,  3, IMS_TINYBIND,        0, &var_tIntCol);
	IMS_BIND (qDesc,  4, IMS_FLT8BIND,        0, &var_fltCol);
	IMS_BIND (qDesc,  5, IMS_REALBIND,        0, &var_realCol);
	IMS_BIND (qDesc,  6, IMS_STRINGBIND,     31, var_charCol);
	IMS_BIND (qDesc,  7, IMS_STRINGBIND,     21, var_vCharCol);
	IMS_BIND (qDesc,  8, IMS_INTBIND,         0, &var_binCol);
	IMS_BIND (qDesc,  9, IMS_INTBIND,         0, &var_vBinCol);
	IMS_BIND (qDesc, 10, IMS_INTBIND,         0, &var_bitCol);
	IMS_BIND (qDesc, 11, IMS_FLT8BIND,        0, &var_monCol);
	IMS_BIND (qDesc, 12, IMS_REALBIND,        0, &var_sMonCol);
	IMS_BIND (qDesc, 13, IMS_STRINGBIND,     27, var_dateCol);
	IMS_BIND (qDesc, 14, IMS_STRINGBIND,     27, var_sDateCol);

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

		rowCount++;
		(void) fprintf (stdout, "Row # %d\n", rowCount);

		(void) fprintf (stdout, "%s = %ld\n",
			IMS_ATTRNAME (qDesc, 0),  var_intCol);
		(void) fprintf (stdout, "%s = %d\n",
			IMS_ATTRNAME (qDesc, 1), var_sIntCol);
		(void) fprintf (stdout, "%s = %d\n",
			IMS_ATTRNAME (qDesc, 2), var_tIntCol);
		(void) fprintf (stdout, "%s = %f\n",
			IMS_ATTRNAME (qDesc, 3), var_fltCol);
		(void) fprintf (stdout, "%s = %f\n",
			IMS_ATTRNAME (qDesc, 4), var_realCol);
		(void) fprintf (stdout, "%s = %s\n",
			IMS_ATTRNAME (qDesc, 5), var_charCol);
		(void) fprintf (stdout, "%s = %s\n",
			IMS_ATTRNAME (qDesc, 6), var_vCharCol);
		(void) fprintf (stdout, "%s = 0x%x\n",
			IMS_ATTRNAME (qDesc, 7), var_binCol);
		(void) fprintf (stdout, "%s = 0x%x\n",
			IMS_ATTRNAME (qDesc, 8), var_vBinCol);
		(void) fprintf (stdout, "%s = %d\n",
			IMS_ATTRNAME (qDesc, 9), var_bitCol);
		(void) fprintf (stdout, "%s = %.2f\n",
			IMS_ATTRNAME (qDesc, 10), var_monCol);
		(void) fprintf (stdout, "%s = %.2f\n",
			IMS_ATTRNAME (qDesc, 11), var_sMonCol);
		(void) fprintf (stdout, "%s = %s\n",
			IMS_ATTRNAME (qDesc, 12), var_dateCol);
		(void) fprintf (stdout, "%s = %s\n",
			IMS_ATTRNAME (qDesc, 13), var_sDateCol);

		(void) fprintf (stdout, "\n");
	}

	/* Print out the number of rows affected. */
	(void) fprintf (stdout, "Rows queried: %d\n\n", IMS_AFFECTED (qDesc));

	if (status < IMS_OK)
	{
		return status;
	}
	else
	{
		return (IMS_OK);
	}
}

/******************************************************************************
**
** dropTable ()
**
******************************************************************************/

static int dropTable (
	IMS_QI_DESC_OBJ *qDesc)
{
	int status;
	char *dropCommand = "drop table qi_ex_table";

	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Unable to reset database descriptor.");
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
