static char *sccs = "@(#)ims_timeConvEx.c	5.2  03/10/97";
/******************************************************************************
**
** File:        ims_timeConvEx.c
**
** Function:    Test the date/time conversion functions.
**
** Author:      Dan Crichton	
**
** Date:        5/12/95
**
** The following command will make the executible on SunOS Release 5.x:
**
** cc -I/asf/include/imsdads -L/asf/lib ims_timeConvEx.c \
** -lims -o ims_timeConvEx
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <syslog.h>

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_timeConv.h>
#include <ims_util.h>
#include <ims_getInput.h>

/******************************************************************************
**
** main ()
**
******************************************************************************/

void main (
	int argc,
	char *argv[])
{
	IMS_MSG_STRUCT *msgDesc;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	char *programName;
	IMS_NUMERIC_DATE dateDef;
	char buffer[IMS_COL40_LEN];
	char inDate[IMS_COL40_LEN];
	int status;
	int days, msecs;

	/*
	** Get node and program name information.
	*/ 
	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
	hostName[IMS_HOST_LEN] = '\0';  /* Just in case. */
	programName = ims_extractFileName (argv[0]);

	/*
	** Allocate the message facility structure.
	*/
	if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
	{
		(void) fprintf (stderr,
			"Memory allocation for IMS_MSG_STRUCT structure failed.");
		exit (1);
	}

	/*
	** Initialize the message facility options.
	*/
	(void) ims_msgSubSystem (msgDesc, "IMS");
	(void) ims_msgProgramName (msgDesc, programName);
	(void) sprintf (banner, "%s::%s", hostName, programName);
	(void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
	(void) ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5);

	/*
	** Get input from the terminal.
	*/
	if (ims_getString (IMS_FALSE, inDate, IMS_COL40_LEN,
		"Enter Date/Time value: ") == (char *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not accept terminal input.");
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	/*
	** Convert date/time string to numeric format.
	*/
	if ((status = ims_timeToNumericDate (msgDesc, inDate,
			&dateDef)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not parse the date/time value '%s'.", inDate);
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	/*
	** Convert the numeric date/time to other formats and
	** print them out to the terminal.
	*/
	ims_numericDateToIMSA (&dateDef, buffer);
	(void) printf ("\nDate/Time in IMS format: %s\n", buffer);

	ims_numericDateToASFA (&dateDef, buffer);
	(void) printf ("Date/Time in ASF format: %s\n", buffer);

	ims_numericDateToACSA (&dateDef, buffer);
	(void) printf ("Date/Time in ACS format: %s\n", buffer);

	ims_numericDateToCSAA (&dateDef, buffer);
	(void) printf ("Date/Time in CSA format: %s\n", buffer);

	ims_numericDateToDBMSA (&dateDef, buffer);
	(void) printf ("Date/Time in DBMS format: %s\n", buffer);

	ims_numericDateToTTDLA (&dateDef, buffer);
	(void) printf ("Date/Time in TTDL format: %s\n", buffer);

	ims_numericDateToV0A (&dateDef, buffer);
	(void) printf ("Date/Time in V0 format: %s\n", buffer);

	ims_numericDateToESAI (&dateDef, &days, &msecs);
	(void) printf ("Date/Time in ESAI format: %d/%d\n", days, msecs);

	ims_dateItoDef(days, msecs, buffer);
	(void) printf ("ESAI to ASC format: %s\n", buffer);

	ims_numericDateToESAA (&dateDef, buffer);
	(void) printf ("Date/Time in ESAA format: %s\n", buffer);

	ims_numericDateToGMTA (&dateDef, buffer);
	(void) printf ("Date/Time in GMT format: %s\n\n", buffer);

	/*
	** Get the current date and display it to the terminal.
	*/
	if ((status = ims_getCurrentDate (msgDesc, &dateDef)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not parse the current date/time value.");
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	ims_numericDateToIMSA (&dateDef, buffer);
	(void) printf ("Current date/time in IMS format: %s\n\n", buffer);

	/*
	** Determine the difference between the given date and
	** the current date.
	*/
	days = 0;
	msecs = 0;
	if ((status = ims_numericDateDiff (msgDesc, inDate, buffer,
		&days, &msecs)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not get the difference between '%s' and '%s'.",
			inDate, buffer);
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	(void) printf ("Date/Time difference: %d/%d (days/milliseconds).\n\n",
		days, msecs);

	(void) ims_msgStructFree (msgDesc);

	exit (0);
}
