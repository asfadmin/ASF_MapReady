static char *sccs = "@(#)ims_filmList.c	5.2 04/29/96";
/***************************************************************
**
** File:        ims_filmList.c
**
** Function:    Routines to create the FPS Thing-To-Do-List (TTDL) for
**              the Laser Technics and Fire Recorder.
**
** Author:      S. Hardman and D. Pass
**
** Date:        6/26/95
**
**************************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/utsname.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_util.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_media.h>
#include <ims_timeConv.h>
#include <ims_archive.h>
#include <ims_childHandler.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_media.h.
** They are listed here for reference.
**
**  int ims_filmList (IMS_MSG_STRUCT *, char *, FILM_REQUEST_LIST *);
**  int ims_filmUpdate (IMS_MSG_STRUCT *, char *, FILM_REQUEST_LIST *);
*/

/*
** Local Functions.
*/
static int writeCmnHeader (IMS_MSG_STRUCT *, IMS_NUMERIC_DATE, int);
static int assignFilmType (IMS_MSG_STRUCT *, FILM_REQUEST_INFO *);
static int copyFiles (IMS_MSG_STRUCT *, FILM_REQUEST_INFO *,
	FILM_FILE_LIST *, char *);
static int writeFilmRequest (IMS_MSG_STRUCT *, FILM_REQUEST_INFO *,
	char *, IMS_NUMERIC_DATE, int);
static int openConnection (IMS_MSG_STRUCT *, MEDIA_USER_SPEC *);
static int getItemInfo (IMS_MSG_STRUCT *, FILM_REQUEST_INFO *);
static int checkTargetQueue (IMS_MSG_STRUCT *, FILM_REQUEST_INFO *);
static int getGranuleInfo (IMS_MSG_STRUCT *, FILM_REQUEST_INFO *);
static int getStageArea (IMS_MSG_STRUCT *, char *, char *);
static FILM_FILE_LIST *getFileList (IMS_MSG_STRUCT *, DBSMALLINT, DBINT,
	DBCHAR *);
static int updateQueueStatus (IMS_MSG_STRUCT *, FILM_REQUEST_LIST *);
static int execCmd (IMS_MSG_STRUCT *);
static int checkRetStatus (IMS_MSG_STRUCT *);
static int updateStageList (IMS_MSG_STRUCT *, char *);
static int removeStagedFiles (IMS_MSG_STRUCT *);
static void freeStageList (FILM_STAGE_LIST *);
static void freeFileList (FILM_FILE_LIST *);

/*
** Global Variables.
*/
static IMS_QI_DESC_OBJ *qDesc;
static FILM_STAGE_LIST *stageListHead;
static FILM_STAGE_LIST *stageListCurr;
static char cmdBuf[255];

/****************************************************************
**
** ims_filmList ()
**
** This function is called by ims_filmListClnt (a stand-alone
** program) or the operator screen (to be implimented later).
** Given a linked-list of order_id, item_id sets (see filmList),
** the program forms an ascii list for SPS to execute.  The only
** functions done here are the calls to the laser technics and
** fire recorder.
**
** Note that if a positive is requested a negative must be generated
** first.
**
**************************************************************** */

int ims_filmList (
	IMS_MSG_STRUCT *msgDesc,
	char *mediaUserSpec,
	FILM_REQUEST_LIST *filmList)
{
	MEDIA_USER_SPEC *userSpec;
	FILM_REQUEST_INFO *filmInfo;
	FILM_FILE_LIST *fileList;
	FILM_REQUEST_LIST *listPtr;
	IMS_NUMERIC_DATE dateStruct;
	char stageHost[IMS_HOST_LEN+1];
	char stagePath[IMS_PATH_LEN+1];
	char ttdlName[IMS_NAME_LEN+1];
	char ttdlSpec[IMS_PATH_LEN+IMS_NAME_LEN+1];
	char filePath[IMS_PATH_LEN+IMS_NAME_LEN+IMS_COL10_LEN+1];
	char fileSpec[IMS_HOST_LEN+IMS_PATH_LEN+IMS_NAME_LEN+IMS_COL10_LEN+1];
	int fd;
	int status;
	int debugFlag;
	size_t specLength;

	/*
	** lint: pointer cast may result in improper alignment
	** The structure definitions are identical.
	*/
	userSpec = (MEDIA_USER_SPEC *) mediaUserSpec;

	qDesc = (IMS_QI_DESC_OBJ *) NULL;
	filmInfo = (FILM_REQUEST_INFO *) NULL;
	fileList = (FILM_FILE_LIST *) NULL;
	listPtr = (FILM_REQUEST_LIST *) NULL;
	stageListHead = (FILM_STAGE_LIST *) NULL;
	stageListCurr = (FILM_STAGE_LIST *) NULL;

	/*
	** debugFlag prints one line/ord-item pair - can be turned on
	** two ways:  with -DDEBUG when compiling, or in the debugger.
	*/
	debugFlag = IMS_FALSE;

#ifdef DEBUG
	debugFlag = IMS_TRUE;
#endif  /* DEBUG */

	/*
	** Open the database server connection.
	*/
	if ((status = openConnection (msgDesc, userSpec)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Get the path for the staging area.
	** The host for the path is ignored at this time because
	** the path is assumed to be local or NFS mounted.
	*/
	if ((status = getStageArea(msgDesc, stageHost, stagePath)) < IMS_OK)
	{
		(void) ims_qiFreeDesc (qDesc);
		return (status);
	}

	/*
	** Assemble the file specification.
	*/
	(void) strcpy (ttdlName, "ttdl.fps");
	ims_concatFilePath (ttdlSpec, stagePath, ttdlName);

	/*
	** Create the TTDL file.
	*/
	if ((fd = open (ttdlSpec, O_WRONLY|O_CREAT|O_EXCL)) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not create the FPS TTDL file '%s'. %s",
			ttdlSpec, strerror (errno));
		(void) ims_qiFreeDesc (qDesc);
		return (IMS_ERROR);
	}

	/*
	** Change the mode of the file.
	*/
	if ((status = chmod (ttdlSpec, 0644)) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not change the mode for the FPS TTDL file '%s'. %s",
		ttdlSpec, strerror (errno));
		status = IMS_ERROR;
		goto ERROR;
	}

	/*
	** Get the current date and convert it to ASF format.
	*/
	if ((status = ims_getCurrentDate (msgDesc, &dateStruct)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not get current system date/time value.");
		goto ERROR;
	}

	/*
	** Write the common header to the TTDL file.  (written
	**  only once)
	*/
	if ((status = writeCmnHeader (msgDesc, dateStruct, fd)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not write the common header to the FPS TTDL "
			"file '%s'.", ttdlName);
		goto ERROR;
	}

	/*
	** Allocate space for the FILM_REQUEST_INFO structure.
	*/
	if ((filmInfo = (FILM_REQUEST_INFO *) malloc
		((size_t) sizeof (FILM_REQUEST_INFO))) ==
		(FILM_REQUEST_INFO *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for FILM_REQUEST_INFO "
			"structure.");
		status = IMS_FATAL;
		goto ERROR;
	}

	/*
	** Query for item information and write a film request
	** line for each item in the film request list.
	*/
	listPtr = filmList;
	while (listPtr != (FILM_REQUEST_LIST *) NULL)
	{
		/*
		** Clear out the filmInfo structure.
		*/
		(void) memset (filmInfo, 0, (size_t) sizeof(FILM_REQUEST_INFO));

		/*
		** Check for valid film target.
		*/
		if ((listPtr->film_target != LASER) &&
			(listPtr->film_target != FIRE))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Item '%d' of order '%ld' has an invalid film target "
				"of '%d'.", listPtr->item_id, listPtr->order_id,
				listPtr->film_target);
			status = IMS_ERROR;
			goto ERROR;
		}

		/*
		** Query for film information for each item.
		*/
		filmInfo->order_id = listPtr->order_id;
		filmInfo->item_id = listPtr->item_id;
		filmInfo->film_target = listPtr->film_target;

		/*
		** Put the query results into the filmInfo structure. Note:
		** this structure is re-used for each order/item pair.
		*/
		if ((status = getItemInfo (msgDesc, filmInfo)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not obtain item information for item '%d' of "
				"order '%ld'.",
				filmInfo->item_id, filmInfo->order_id);
			goto ERROR;
		}

		/*
		** Check for a valid media type.
		*/
		switch (filmInfo->media_type)
		{
			case IMS_LT_NEG:
			case IMS_LT_POS:
			case IMS_LT_PRINT:
			case IMS_FR_NEG:
			case IMS_FR_POS:
			case IMS_FR_PRINT:
			case IMS_PHOTO_CPR:
			case IMS_PHOTO_PTP:
			case IMS_PHOTO_NTP:
				break;

			default:
				(void) ims_msg (msgDesc, IMS_ERROR,
					"Media type '%s', for item '%d' of order '%ld', "
					"is invalid for film generation.",
					ims_mediaDesc (filmInfo->media_type),
					filmInfo->item_id, filmInfo->order_id);
				status = IMS_ERROR;
				goto ERROR;
		}

		/*
		** Check the target queue table for an entry and make sure
		** that entry has the correct status.
		*/
		if ((status = checkTargetQueue (msgDesc, filmInfo)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Status check failed for item '%d' of order '%ld' for "
				"the proper queue table.",
				filmInfo->item_id, filmInfo->order_id);
			goto ERROR;
		}

		/*
		** Print out the film request information.
		*/
		if (debugFlag == IMS_TRUE)
		{
			(void) ims_msg (msgDesc, IMS_INFO,
				"Processing item '%d' of order '%ld' with media "
				"type '%s'.",
				filmInfo->item_id, filmInfo->order_id,
				ims_mediaDesc (filmInfo->media_type));
		}

		/*
		** Get the specific granule information.
		*/
		if ((status = getGranuleInfo (msgDesc, filmInfo)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not obtain granule information for item '%d' "
				"of order '%ld'.",
				filmInfo->item_id, filmInfo->order_id);
			goto ERROR;
		}

		/*
		** Check the granule format.
		*/
		if ((strcmp (filmInfo->format, "CEOS") != 0) &&
			(strcmp (filmInfo->format, "CEOS_OLD") != 0))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Granule '%s', associated with item '%d' of order '%d',"
				" has an invalid format of '%s'. Must be one of the "
				"'CEOS' formats.",
				filmInfo->name, filmInfo->item_id, filmInfo->order_id,
				filmInfo->format);
			status = IMS_ERROR;
			goto ERROR;
		}

		/*
		** Check the status of the granule.
		*/
		if (filmInfo->status != GRANULE_AVAILABLE)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Granule '%s', associated with item '%d' of order '%d',"
				" does not have a status of 'Available'.",
				filmInfo->name, filmInfo->item_id, filmInfo->order_id);
			status = IMS_ERROR;
			goto ERROR;
		}

		/*
		** Assign the film type.
		*/
		if ((status = assignFilmType (msgDesc, filmInfo)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not assign the film type for item '%d' of "
				"order '%ld'.",
				filmInfo->item_id, filmInfo->order_id);
			goto ERROR;
		}

		/*
		** Get the list of files that make up this CEOS product.
		*/
		if ((fileList = getFileList (msgDesc, filmInfo->dataset_idx,
			filmInfo->granule_idx, filmInfo->format)) ==
			(FILM_FILE_LIST *) NULL)
		{
			(void) ims_msg (msgDesc, status,
				"Could not obtain the file list for item '%d' of "
				"order '%ld'.",
				filmInfo->item_id, filmInfo->order_id);
			status = IMS_ERROR;
			goto ERROR;
		}

		/*
		** Set up the file specification for the film request line.
		*/
		ims_concatFilePath (filePath, stagePath, filmInfo->name);

		/*
		** Removed the host name from the file specification due to the
		** 32 character limitation for paths. Also added a check for
		** for this limitation.
		*/

		/* (void) sprintf (fileSpec, "%s:%s", stageHost, filePath); */

		(void) strcpy (fileSpec, filePath);
		specLength = strlen (fileSpec);

		if (specLength > 32)
		{
			status = IMS_ERROR;
			(void) ims_msg (msgDesc, status,
				"The file specification '%s', exceeds the 32 character"
				" limitation.", fileSpec);
			goto ERROR;
		}

		/*
		** Copy the files to the staging area.
		*/
		if ((status = copyFiles (msgDesc, filmInfo, fileList,
			stagePath)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not copy the current granule '%s' to the staging"
				" area, for item '%d' of order '%ld'.",
				filmInfo->name, filmInfo->item_id, filmInfo->order_id);
			goto ERROR;
		}

		/*
		** Write the film request lines to the TTDL file.
		*/
		if ((status = writeFilmRequest (msgDesc, filmInfo, fileSpec,
			dateStruct, fd)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not write the film request to the FPS TTDL "
				"file '%s'.",  ttdlName);
			goto ERROR;
		}

		freeFileList (fileList);

		listPtr = listPtr->next;
	}

	/*
	** Close the TTDL file.
	** Close the server connection and free the query descriptor.
	** Free the filmInfo and stageList structures.
	*/
	(void) close (fd);
	(void) ims_qiFreeDesc (qDesc);
	free (filmInfo);
	freeStageList (stageListHead);

	return (IMS_OK);

ERROR:
	/*
	** Close and remove the TTDL file.
	** Close the server connection and free the query descriptor.
	** Free the filmInfo and fileList structrues.
	*/
	(void) close (fd);
	(void) remove (ttdlSpec);
	(void) ims_qiFreeDesc (qDesc);
	free (filmInfo);
	freeFileList (fileList);

	/*
	** If we were processing the order items then set
	** the status for the current item to error.
	*/
	if (listPtr != (FILM_REQUEST_LIST *) NULL)
	{
		listPtr->status = FILM_ERROR;
	}

	/*
	** Remove the files that have been staged up to this point.
	*/
	if (removeStagedFiles (msgDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"An error occurred cleaning up the staging area.");
	}
	freeStageList (stageListHead);

	return (status);
}

/***************************************************************
**
** ims_filmUpdate ()
**
** Updates the film queue item's status to IN-FIRE or IN-LASER.
**
**************************************************************** */

int ims_filmUpdate (
	IMS_MSG_STRUCT *msgDesc,
	char *mediaUserSpec,
	FILM_REQUEST_LIST *filmList)
{
	MEDIA_USER_SPEC *userSpec;
	int status;

	/*
	** lint: pointer cast may result in improper alignment
	** The structure definitions are identical.
	*/
	userSpec = (MEDIA_USER_SPEC *) mediaUserSpec;

	qDesc = (IMS_QI_DESC_OBJ *) NULL;

	/*
	** Open the database server connection.
	*/
	if ((status = openConnection (msgDesc, userSpec)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Update the status for each item.
	*/
	if ((status = updateQueueStatus (msgDesc, filmList)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not update the item queue status.");
	}

	(void) ims_qiFreeDesc (qDesc);
	return (status);

}

/***************************************************************
**
** writeCmnHeader ()
**
** Writes the first line of the TTDL file.
**
**************************************************************** */

static int writeCmnHeader (
	IMS_MSG_STRUCT *msgDesc,
	IMS_NUMERIC_DATE dateStruct,
	int fd)
{
	char dateStr[IMS_DATETIME_LEN+1];
	char buffer[60];

	/*
	** Convert current date to ASF Date/Time format.
	*/
	ims_numericDateToASFA (&dateStruct, dateStr);

	/*
	** Populate the buffer with the record.
	*/
	(void) sprintf (buffer, "%s SL FPS IMS                  ",
		dateStr);

	/*
	** Write the record out to the given file.
	*/
	(void) strcat (buffer, "\n"); /* add line feed  */
	if (write (fd, buffer, strlen (buffer)) == -1)
		{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not write to the FPS TTDL file. %s",
			strerror (errno));
		return (IMS_ERROR);
	}
	return (IMS_OK);
}   /*  writeCmnHeader   */

/***********************************************************
**
** assignFilmType ()
**
** Assign the film type based on the film target, media type and
**  geocoding.
**
************************************************************ */

static int assignFilmType (
	IMS_MSG_STRUCT *msgDesc,
	FILM_REQUEST_INFO *filmInfo)
{
	int geocodedFlag;

	/*
	** Initialize variables.
	*/
	geocodedFlag = IMS_FALSE;

	/*
	** Determine whether this image is geocoded.
	** The 11th character of the product name determines the
	** projection of the data.  Possible values are:
	**   U, P, L - Geocoded (UTM, Polar Stereo, Lambert)
	**   G - Georeferenced Ground Range
	**   S - Slant Range
	**   T - Terrain Corrected
	**
	** If the product name is not longer than 11 characters
	** than we assume that this is a test and that it is not
	** geocoded.
	*/
	if (strlen (filmInfo->name) > (size_t) 11)
	{
		if ((filmInfo->name[10] == 'U') ||
			(filmInfo->name[10] == 'P') ||
			(filmInfo->name[10] == 'L'))
		{
			geocodedFlag = IMS_TRUE;
		}
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"Non standard product name '%s', non-geocoded projection "
			"assumed.", filmInfo->name);
	}

	/*
	** Determine the film type.
	*/
	if (filmInfo->film_target == LASER)
	{
		/*
		** Check the media type to determine whether this product
		** will be on film or paper.
		*/
		if (filmInfo->media_type != IMS_LT_PRINT)
		{
			/*
			** Check for geocoding.
			*/
			if (geocodedFlag == IMS_FALSE)
			{
				/* Standard Low-Res Film. */
				(void) strcpy (filmInfo->film_type, "SLT");
			}
			else
			{
				/* Geocoded Low-Res Film. */
				(void) strcpy (filmInfo->film_type, "GLT");
			}
		}
		else /* This is a Laser Technics print. */
		{
			/*
			** Check for geocoding.
			*/
			if (geocodedFlag == IMS_FALSE)
			{
				/* Standard Low-Res Paper. */
				(void) strcpy (filmInfo->film_type, "SLP");
			}
			else
			{
				/* Geocoded Low-Res Paper. */
				(void) strcpy (filmInfo->film_type, "GLP");
			}
		}
	}
	else /* FIRE */
	{
		/*
		** Check for geocoding.
		*/
		if (geocodedFlag == IMS_FALSE)
		{
			/* Standard Full-Res Film. */
			(void) strcpy (filmInfo->film_type, "FRF");
		}
		else
		{
			/* Geocoded Full-Res Film. */
			(void) strcpy (filmInfo->film_type, "GFR");
		}
	}

	/*
	** See if we assigned the film type.
	*/
	if (filmInfo->film_type[0] == '\0')
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Unable to determine a value for film type.");
		return (IMS_ERROR);
	}

	return (IMS_OK);
}

/***************************************************************
**
** copyFileItem ()
**
**************************************************************** */

static int copyFileItem (
	IMS_MSG_STRUCT *msgDesc,
	char *target,
	char *dest)
{
	FILE *fptr_i, *fptr_o;
	int size, size_out;
	char buf[1024];


	/*
	** Open target file.
	*/

	fptr_i = fopen(target, "r");

	if (fptr_i == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not open target file '%s'", target);
		return(IMS_ERROR);
	}

	fptr_o = fopen(dest, "w");

	if (fptr_o == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not open destination file '%s'", dest);
		fclose(fptr_i);
		return(IMS_ERROR);
	}

	
	while (!feof(fptr_i))
	{
		size = fread(buf, 1, sizeof(buf), fptr_i);

		if (size < 0)
		{
			fclose(fptr_i);
			fclose(fptr_o);
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not read from input file '%s'.", target);
			return(IMS_ERROR);

		}

		size_out = fwrite(buf, 1, size, fptr_o);

		if (size_out != size)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not write to output file '%s'.", dest);
			fclose(fptr_i);
			fclose(fptr_o);
			return(IMS_ERROR);

		}
	}


	fclose(fptr_i);
	fclose(fptr_o);

	return(IMS_OK);


}

/***************************************************************
**
** copyFiles ()
**
**************************************************************** */

static int copyFiles (
	IMS_MSG_STRUCT *msgDesc,
	FILM_REQUEST_INFO *filmInfo,
	FILM_FILE_LIST *fileList,
	char *stagePath)
{
	FILM_FILE_LIST *filePtr;
	char targetName[IMS_NAME_LEN+IMS_COL10_LEN+1];
	char destName[IMS_NAME_LEN+IMS_COL10_LEN+1];
	char targetSpec[IMS_PATH_LEN+IMS_NAME_LEN+IMS_COL10_LEN+1];
	char destSpec[IMS_PATH_LEN+IMS_NAME_LEN+IMS_COL10_LEN+1];
	pid_t pid;
	int status;
	char *taskName = "/usr/bin/cp";

	filePtr = fileList;
	while (filePtr != (FILM_FILE_LIST *) NULL)
	{
		/*
		** Concatenate the destination file name.
		*/
		(void) sprintf (destName, "%s.%s",
			filmInfo->name, filePtr->extension);

		/*
		** If the target file has a version we must append it
		** to the file name.
		*/
		if (filmInfo->version == -1)
		{
			(void) strcpy (targetName, destName);
		}
		else
		{
			(void) sprintf (targetName, "%s.%d",
				destName, filmInfo->version);
		}

		/*
		** Concatenate the target and destination specifications.
		*/
		ims_concatFilePath (destSpec, stagePath, destName);
		ims_concatFilePath (targetSpec, filePtr->path, targetName);

		/*
		** Add the file to our stage list.
		*/
		if ((status = updateStageList (msgDesc, destSpec)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not add the current stage file '%s', to the "
				"stage list.",
				destName);
			return (status);
		}
#if 0

		/*
		** Start the child process.
		*/
		if ((pid = ims_startChild (msgDesc, taskName, taskName,
			targetSpec, destSpec, (char *) 0)) == -1)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Could not start the copy process for file '%s'.",
				destName);
			return (IMS_ERROR);
		}

		/*
		** Wait for the child process to complete.
		*/
		if ((status = ims_waitForChild (msgDesc, pid)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"The copy process failed for file '%s'.",
				destName);
			return (status);
		}
#else

		if (copyFileItem(msgDesc, targetSpec, destSpec) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"The copy process failed for file '%s'.",
				destName);
			return (status);
		}
#endif

		/*
		** Mark the current file in the stage list as
		** being copied to the stage area.
		*/
		stageListCurr->copiedToStage = IMS_TRUE;

		filePtr = filePtr->next;
	}

	return (IMS_OK);
}

/***************************************************************
**
** writeFilmRequest ()
**
** Adds a line to the TTDL file for the current item.
**
**************************************************************** */

static int writeFilmRequest (
	IMS_MSG_STRUCT *msgDesc,
	FILM_REQUEST_INFO *filmInfo,
	char *fileSpec,
	IMS_NUMERIC_DATE dateStruct,
	int fd)
{
	char dateStr[IMS_DATETIME_LEN+1];
	char buffer[255];
	int status;
	int targetPixel;
	int targetLine;
	char npos_char;

	/*
	** Convert current date to TTDL Date/Time format.
	*/
	ims_numericDateToTTDLA (&dateStruct, dateStr);

	/*
	** Get the target pixel and line: this defined the center
	** of the data for full res pictures.  (the systems can
	** only show 8k x 8k of data, but their is more data than
	** this, so the user can move the picture around in the
	** data.  pixel is no. of words from the center, and
	** line is no. of records or rows from the center.)
	** note:  currently, the full res pictures are square,
	** but this may change.
	**
	** We subtract 192 from the record length to account for the
	** CEOS prefix length and we subtract 1 from the record count
	** to account for the file descriptor record.
	*/
	targetPixel = ((filmInfo->rec_len - 192) / 2) +
		filmInfo->target_pixel;
	targetLine = ((filmInfo->num_recs - 1) / 2) +
		filmInfo->target_line;

	/*
	** Fill out the Process ID.
	*/
	(void) sprintf (buffer, "X%010ld %03d %s %03d ",
		filmInfo->order_id, filmInfo->item_id, dateStr,
		filmInfo->priority);

	/*
	** Determine negative/positive character.
	*/
	switch (filmInfo->media_type)
	{
		case IMS_LT_NEG:
		case IMS_FR_NEG:
		case IMS_PHOTO_CPR:
		case IMS_PHOTO_PTP:
		case IMS_PHOTO_NTP:
			npos_char = '-';
			break;

		case IMS_LT_POS:
		case IMS_LT_PRINT:
		case IMS_FR_POS:
		case IMS_FR_PRINT:
			npos_char = '+';
			break;
	}

	/*
	** Fill out the rest of the film request based on
	** the film target.
	*/
	if (filmInfo->film_target == LASER)
	{
		(void) sprintf (&buffer[30], "%s %c             %-32s ",
			filmInfo->film_type, npos_char,
			fileSpec);
	}
	else  /* Full resolution film requires targets. */
	{
		(void) sprintf (&buffer[30], "%s %c %05d %05d %-32s ",
			filmInfo->film_type, npos_char, targetPixel, targetLine,
			fileSpec);
	}

	(void) strcat (buffer, "\n");

	/*
	** Write out the line to the file.
	*/
	if (write (fd, buffer, strlen (buffer)) == -1)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not write to the FPS TTDL file. %s",
			strerror(errno));
		return (IMS_ERROR);
	}

	return (IMS_OK);
} /* writeFilmRequest */

/***********************************************************
**
** openConnection ()
**
************************************************************ */

static int openConnection (
	IMS_MSG_STRUCT *msgDesc,
	MEDIA_USER_SPEC *userSpec)
{
	int status;

	/*
	** Allocate a query descriptor.
	*/
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate a query descriptor.");
		return (IMS_FATAL);
	}

	/*
	** Setup the descriptor with necessary information about this
	** process.
	*/
	IMS_SETUSER (qDesc, userSpec->username);

	IMS_SETPSWD (qDesc, userSpec->password);

	IMS_SETPROG (qDesc, userSpec->program);

	if (userSpec->server != (char *) NULL)
	{
		IMS_SETSERVER (qDesc, userSpec->server);
	}

	if (userSpec->database != (char *) NULL)
	{
		IMS_SETDBNAME (qDesc, userSpec->database);
	}

	IMS_SET_VERBOSE (qDesc, 10);

	IMS_SETCMD (qDesc, cmdBuf);

	/*
	** Login to the catalog database.
	*/
	if ((status = ims_qiLogin (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not login to the database server.");
		(void) ims_qiFreeDesc (qDesc);
		return (status);
	}

	/*
	** Associate the message descriptor with the dbproc so
	** the Sybase error and message handling can be performed.
	*/
	IMS_SET_USERDATA (qDesc);

	return (IMS_OK);
}

/***************************************************************
**
** getItemInfo ()
**
** Get the necessary information for the current order item.
**
**************************************************************** */

static int getItemInfo (
	IMS_MSG_STRUCT *msgDesc,
	FILM_REQUEST_INFO *filmInfo)
{
	int status;

	(void) sprintf (cmdBuf, "med_get_item_info %ld, %d",
		filmInfo->order_id, filmInfo->item_id);

	/*
	** Process the result row for this query.
	*/
	if ((status = execCmd (msgDesc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Check to see if only one row was returned.
	*/
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"No rows of item information returned.");
		return (IMS_ERROR);
	}
	else if (IMS_AFFECTED (qDesc) > 1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"More than one row of item information returned.");
		return (IMS_ERROR);
	}

	/*
	** Copy the returned data into the structure.
	*/

	/* priority */
	(void) memcpy (&(filmInfo->priority),
		IMS_VALUE (qDesc, 0), IMS_VALUELENGTH (qDesc, 0));

	/* media_type */
	(void) memcpy (&(filmInfo->media_type),
		IMS_VALUE (qDesc, 1), IMS_VALUELENGTH (qDesc, 1));

	/* process_type */
	(void) memcpy (&(filmInfo->process_type),
		IMS_VALUE (qDesc, 2), IMS_VALUELENGTH (qDesc, 2));

	/* p_dataset_idx */
	(void) memcpy (&(filmInfo->dataset_idx),
		IMS_VALUE (qDesc, 3), IMS_VALUELENGTH (qDesc, 3));

	/* p_granule_idx */
	(void) memcpy (&(filmInfo->granule_idx),
		IMS_VALUE (qDesc, 4), IMS_VALUELENGTH (qDesc, 4));

	/* granules_table */
	(void) memcpy (filmInfo->granules_table,
		IMS_VALUE (qDesc, 5), IMS_VALUELENGTH (qDesc, 5));
	filmInfo->granules_table[IMS_VALUELENGTH (qDesc, 5)] = '\0';

	/*
	** These items will be in the catalog someday. Until then
	** we will set them to zero.
	*/
	filmInfo->target_pixel = 0;
	filmInfo->target_line = 0;

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not reset the query descriptor.");
		return (status);
	}

	return (IMS_OK);
} /* getItemInfo */

/***************************************************************
**
** checkTargetQueue ()
**
** Check to see if the item is in the proper queue
** and has a status of NEW.
**
**************************************************************** */

static int checkTargetQueue (
	IMS_MSG_STRUCT *msgDesc,
	FILM_REQUEST_INFO *filmInfo)
{
	int status;

	(void) sprintf (cmdBuf, "med_check_target_queue %ld, %d, %d",
		filmInfo->order_id, filmInfo->item_id, filmInfo->film_target);

	/*
	** Execute the command.
	*/
	if ((status = execCmd (msgDesc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not reset the query descriptor.");
		return (status);
	}

	return (IMS_OK);
} /* checkTargetQueue */

/***************************************************************
**
** getGranuleInfo ()
**
** Get the necessary information from the granules_x table.
**
**************************************************************** */

static int getGranuleInfo (
	IMS_MSG_STRUCT *msgDesc,
	FILM_REQUEST_INFO *filmInfo)
{
	int status;

	(void) sprintf (cmdBuf,
		"select name, format, version, status, \
		IMAGE_MAX_RECORD_LENGTH, IMAGE_RECORD_COUNT \
		from %s \
		where granule_idx = %ld",
		filmInfo->granules_table, filmInfo->granule_idx);

	/*
	** Process the result row for this query.
	*/
	if ((status = execCmd (msgDesc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Check to see if only one row was returned.
	*/
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"No rows of granule information returned.");
		return (IMS_ERROR);
	}
	else if (IMS_AFFECTED (qDesc) > 1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"More than one row of granule information returned.");
		return (IMS_ERROR);
	}

	/*
	** Copy the returned data into the structure.
	*/

	/* name */
	(void) memcpy (filmInfo->name,
		IMS_VALUE (qDesc, 0), IMS_VALUELENGTH (qDesc, 0));
	filmInfo->name[IMS_VALUELENGTH (qDesc, 0)] = '\0';

	/* format */
	(void) memcpy (filmInfo->format,
		IMS_VALUE (qDesc, 1), IMS_VALUELENGTH (qDesc, 1));
	filmInfo->format[IMS_VALUELENGTH (qDesc, 1)] = '\0';
	(void) ims_truncStr (filmInfo->format);

	/* version */
	if ((IMS_VALUELENGTH (qDesc, 2) == 0) ||
		(IMS_VALUE (qDesc, 2) == (char *)NULL))
	{
		/* Version is null. */
		filmInfo->version = -1;
	}
	else
	{
		(void) memcpy ((char *) &(filmInfo->version),
			IMS_VALUE (qDesc, 2), IMS_VALUELENGTH (qDesc, 2));
	}

	/* status */
	(void) memcpy ((char *) &(filmInfo->status),
		IMS_VALUE (qDesc, 3), IMS_VALUELENGTH (qDesc, 3));

	/* IMAGE_MAX_RECORD_LENGTH */
	(void) memcpy ((char *) &(filmInfo->rec_len),
		IMS_VALUE (qDesc, 4), IMS_VALUELENGTH (qDesc, 4));

	/* IMAGE_RECORD_COUNT */
	(void) memcpy ((char *) &(filmInfo->num_recs),
		IMS_VALUE (qDesc, 5), IMS_VALUELENGTH (qDesc, 5));

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not reset the query descriptor.");
		return (status);
	}

	return (IMS_OK);
} /* getGranuleInfo */

/***************************************************************
**
** getStageArea ()
**
**************************************************************** */

static int getStageArea (
	IMS_MSG_STRUCT *msgDesc,
	char *stageHost,
	char *stagePath)
{
	int status;

	/*
	** Set up the command buffer with the stored procedure call.
	*/
	(void) sprintf (cmdBuf, "med_get_stage_area %d", FPS_TTDL);

	/*
	** Process the result rows for this query.
	*/
	if ((status = execCmd (msgDesc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Check to see if a path was returned.
	*/
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not obtain the staging area path.");
		return (IMS_ERROR);
	}
	else if (IMS_AFFECTED (qDesc) > 1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"More than one staging area path was returned.");
		return (IMS_ERROR);
	}

	/*
	** Copy in the returned data.
	*/

	/* host */
	(void) memcpy (stageHost, IMS_VALUE (qDesc, 0),
		IMS_VALUELENGTH (qDesc, 0));
	stageHost[IMS_VALUELENGTH (qDesc, 0)] = '\0';

	/* path */
	(void) memcpy (stagePath, IMS_VALUE (qDesc, 1),
		IMS_VALUELENGTH (qDesc, 1));
	stagePath[IMS_VALUELENGTH (qDesc, 1)] = '\0';

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not reset the query descriptor.");
		return (status);
	}

	return (IMS_OK);
}

/***********************************************************
**
** getFileList ()
**
************************************************************ */

static FILM_FILE_LIST *getFileList (
	IMS_MSG_STRUCT *msgDesc,
	DBSMALLINT dataset_idx,
	DBINT granule_idx,
	DBCHAR *format)
{
	FILM_FILE_LIST *fileList;
	FILM_FILE_LIST *currPtr;
	FILM_FILE_LIST *prevPtr;
	int status;
	int rowCount;

	prevPtr = (FILM_FILE_LIST *) NULL;

	/*
	** Set up the command buffer with the stored procedure call.
	*/
	(void) sprintf (cmdBuf, "med_get_file_list %d, %ld, '%s'",
		dataset_idx, granule_idx, format);

	/*
	** Process the result rows for this query.
	*/
	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return ((FILM_FILE_LIST *) NULL);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/*
		** Allocate space for the FILM_FILE_LIST structure.
		*/
		if ((currPtr = (FILM_FILE_LIST *) malloc
			((size_t) sizeof (FILM_FILE_LIST))) ==
			(FILM_FILE_LIST *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate memory for FILM_FILE_LIST "
				"structure.");
			while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
			return ((FILM_FILE_LIST *) NULL);
		}

		/*
		** fileList points to the first element of the list.
		*/
		if (++rowCount == 1)
		{
			fileList = currPtr;
		}
		else
		{
			prevPtr->next = currPtr;
		}

		currPtr->next = (FILM_FILE_LIST *) NULL;

		/*
		** Copy in the returned data.
		*/

		/* type */
		(void) memcpy (&(currPtr->type),
			IMS_VALUE (qDesc, 0), IMS_VALUELENGTH (qDesc, 0));

		/* extension */
		(void) memcpy (currPtr->extension,
			IMS_VALUE (qDesc, 1), IMS_VALUELENGTH (qDesc, 1));
		currPtr->extension[IMS_VALUELENGTH (qDesc, 1)] = '\0';
		(void) ims_truncStr (currPtr->extension);

		/* path */
		(void) memcpy (currPtr->path,
			IMS_VALUE (qDesc, 2), IMS_VALUELENGTH (qDesc, 2));
		currPtr->path[IMS_VALUELENGTH (qDesc, 2)] = '\0';

		prevPtr = currPtr;
	}

	/*
	** Check the stored procedure status return value.
	*/
	if ((status = checkRetStatus (msgDesc)) < IMS_OK)
	{
		return ((FILM_FILE_LIST *) NULL);
	}

	/*
	** Check to see if any rows were returned.
	*/
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not obtain the path and extensions for the "
			"current granule.");
		return ((FILM_FILE_LIST *) NULL);
	}

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not reset the query descriptor.");
		return ((FILM_FILE_LIST *) NULL);
	}

	return (fileList);
}

/***************************************************************
**
** updateQueueStatus ()
**
** Update the item queue status to IN-FIRE or IN-LASER.
**
**************************************************************** */

static int updateQueueStatus (
	IMS_MSG_STRUCT *msgDesc,
	FILM_REQUEST_LIST *filmList)
{
	FILM_REQUEST_LIST *currList;
	int status;

	/*
	** Perform the update for each item in the list.
	*/
	currList = filmList;
	while (currList != (FILM_REQUEST_LIST *) NULL)
	{
		/*
		** Populate the command buffer with the stored procedure call.
		*/
		(void) sprintf (cmdBuf, "med_update_queue_status %ld, %d, %d",
			currList->order_id,
			currList->item_id, currList->film_target);
		/*
		** Execute the command.
		*/
		if ((status = execCmd (msgDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Unable to update queue status for item '%d' of "
				"order '%ld'.", currList->item_id, currList->order_id);

		}

		/*
		** Reset the query descriptor.
		*/
		if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not reset the query descriptor.");
			return (status);
		}

		currList = currList->next;
	}

	return (IMS_OK);
}

/***********************************************************
**
** execCmd ()
**
** Execute an SQL procedure that writes data into the catalog database.
** We don't pass a parameter, but assume that when this function is
** called, the declared static buffer 'cmdBuf' has been properly filled
** in with the SQL statement to be executed.
**
** THIS ROUTINE IS ONLY USED FOR EXEUTING SQL STATEMENTS THAT WILL ONLY
** RETURN ONE ROW OR LESS FROM THE DATABASE.
**
************************************************************ */

static int execCmd (
	IMS_MSG_STRUCT *msgDesc)
{
	int status;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (ims_msgGetSeverity (msgDesc));
		}
	}

	/*
	** Check the stored procedure status returned value.
	*/
	if ((status = checkRetStatus (msgDesc)) < IMS_OK)
	{
		return (status);
	}

	return (IMS_OK);
}

/***********************************************************
**
** checkRetStatus ()
**
** Check the procedure returned status value.
** When status returned is not an error, then return IMS_OK.
**
************************************************************ */

static int checkRetStatus (
	IMS_MSG_STRUCT *msgDesc)
{
	int procReturn;
	int severity;

	/*
	** Check to see if the Sybase procedure returned a status. If it did
	** and it is not 0 (the OK value for a return), deal with the error.
	** Return status of less than -100 correspond to message facility
	** severity levels modulo 100.
	*/
	if (IMS_HASRETSTAT (qDesc) == IMS_TRUE)
	{
		if ((procReturn = IMS_PROCRETURN (qDesc)) < 0)
		{
			if (procReturn == -103)
			{
				severity = IMS_FATAL;
			}
			else if (procReturn == -102)
			{
				severity = IMS_ERROR;
			}
			else if (procReturn == -101)
			{
				severity = IMS_WARNING;
			}
			else
			{
				severity = IMS_ERROR;
			}
			(void) ims_msg (msgDesc, severity,
				"Sybase procedure '%s' returned a status of %ld",
				qDesc->cmd, procReturn);
			return (severity);
		}
	}

	return (IMS_OK);
}   /*  checkRetStatus   */

/***********************************************************
**
** updateStageList ()
**
** Add an entry in the staged file list for the given file.
**
************************************************************ */

static int updateStageList (
	IMS_MSG_STRUCT* msgDesc,
	char *stageSpec)
{
	FILM_STAGE_LIST *currPtr;

	/*
	** Allocate space for the FILM_STAGE_LIST structure.
	*/
	if ((currPtr = (FILM_STAGE_LIST *) malloc
		((size_t) sizeof (FILM_STAGE_LIST))) ==
		(FILM_STAGE_LIST *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for FILM_STAGE_LIST structure.");
		return (IMS_FATAL);
	}

	/*
	** stageListHead points to the first element of the list.
	*/
	if (stageListHead == (FILM_STAGE_LIST *) NULL)
	{
		stageListHead = currPtr;
	}
	else
	{
		stageListCurr->next = currPtr;
	}

	currPtr->copiedToStage = IMS_FALSE;
	(void) strcpy (currPtr->stageSpec, stageSpec);
	currPtr->next = (FILM_STAGE_LIST *) NULL;

	stageListCurr = currPtr;

	return (IMS_OK);
}

/***********************************************************
**
** removeStagedFiles ()
**
** Remove the staged files from the staging area.
**
************************************************************ */

static int removeStagedFiles (
	IMS_MSG_STRUCT *msgDesc)
{
	FILM_STAGE_LIST *currPtr;
	struct stat statBuf;
	int status;

	status = IMS_OK;
	currPtr = stageListHead;

	/*
	** Go through the list and remove the files.
	*/
	while (currPtr != (FILM_STAGE_LIST *) NULL)
	{
		/* Make sure the file was copied to the staging area. */
		if (currPtr->copiedToStage == IMS_TRUE)
		{
			if (remove (currPtr->stageSpec) == -1)
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"Could not remove staged file '%s'. %s",
					currPtr->stageSpec, strerror (errno));
				status = IMS_ERROR;
			}
		}
		else  /* The file might not have been copied. */
		{
			/* If it exists, remove it. */
			if (stat (currPtr->stageSpec, &statBuf) == 0)
			{
				if (remove (currPtr->stageSpec) == -1)
				{
					(void) ims_msg (msgDesc, IMS_ERROR,
						"Could not remove staged file '%s'. %s",
						currPtr->stageSpec, strerror (errno));
					status = IMS_ERROR;
				}
			}
		}

		currPtr = currPtr->next;
	}

	return (status);
}

/***********************************************************
**
** freeStageList ()
**
** Free the FILM_STAGE_LIST structure.
**
************************************************************ */

static void freeStageList (
	FILM_STAGE_LIST *currPtr)
{
	FILM_STAGE_LIST *nextPtr;

	while (currPtr != (FILM_STAGE_LIST *) NULL)
	{
		nextPtr = currPtr->next;
		free (currPtr);
		currPtr = nextPtr;
	}
}

/***********************************************************
**
** freeFileList ()
**
** Free the FILM_FILE_LIST structure.
**
************************************************************ */

static void freeFileList (
	FILM_FILE_LIST *currPtr)
{
	FILM_FILE_LIST *nextPtr;

	while (currPtr != (FILM_FILE_LIST *) NULL)
	{
		nextPtr = currPtr->next;
		free (currPtr);
		currPtr = nextPtr;
	}
}
