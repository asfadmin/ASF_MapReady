static char *sccs = "@(#)ims_device.c	5.1  03/17/96";
/******************************************************************************
**
** File:        ims_device.c
**
** Function:    Routines to allocate and release tape drive devices.
**
** Author:      S. Hardman
**
** Date:        5/17/95
**
** Modified:    8/14/95 - C. Porter - R1B
**              Added new function ims_deviceTapeCheck().
**
**              8/30/95 - S. Hardman - R1B
**              Modified ims_deviceAlloc(), ims_deviceFree() and
**              ims_deviceTapeCheck() to return IMS_WARNING in
**              certain situations.
**
**              9/20/95 - D. Pass - R1B'
**              Added new function ims_deviceTapeEject().
**
**              9/21/95 - S. Hardman - R1B'
**              Added new function ims_deviceTapeInfo().
**
**              10/11/95 - D. Pass - R1B'
**              Added new function ims_deviceMediaId().
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/utsname.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_media.h>
#include <ims_util.h>

/*
** The following non-POSIX definitions are required by the header
** file that follows.
*/
typedef unsigned char u_char;
typedef unsigned short u_short;

#include <sys/mtio.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_media.h.
** They are listed here for reference.
**
**  int ims_deviceAlloc (IMS_MSG_STRUCT *, char *, DBSMALLINT, DBINT,
**      DEVICE_INFO *);
**  int ims_deviceStatusList (IMS_MSG_STRUCT *, char *, DBSMALLINT,
**      DEVICE_INFO *);
**  int ims_deviceStatusChange (IMS_MSG_STRUCT *, char *, DBSMALLINT,
**      DBSMALLINT, DBCHAR *);
**  int ims_deviceFree (IMS_MSG_STRUCT *, char *, DBSMALLINT);
**  int ims_deviceTapeCheck (IMS_MSG_STRUCT *, DEVICE_INFO *);
**  int ims_deviceTapeEject (IMS_MSG_STRUCT *, DEVICE_INFO *);
**  int ims_deviceTapeInfo (IMS_MSG_STRUCT *, DEVICE_INFO *);
**  int ims_deviceMediaId (IMS_MSG_STRUCT *, char *, DBSMALLINT, DBSMALLINT,
**      DBCHAR *, int);
*/

/*
** Local Functions.
*/
static int openConnection (IMS_MSG_STRUCT *, MEDIA_USER_SPEC *);
static int getDeviceList (IMS_MSG_STRUCT *, DBSMALLINT, char *);
static int allocateDevice (IMS_MSG_STRUCT *, DBSMALLINT, DBSMALLINT, DBINT,
	DEVICE_INFO *);
static int getDeviceStatus (IMS_MSG_STRUCT *, DBSMALLINT, char *,
	DEVICE_INFO *);
static int changeDeviceStatus (IMS_MSG_STRUCT *, DBSMALLINT, DBSMALLINT,
	DBCHAR *);
static int freeDevice (IMS_MSG_STRUCT *, DBSMALLINT);
static int getMediaIdCount (IMS_MSG_STRUCT *, DBSMALLINT, DBCHAR *, int);
static int execCmd (IMS_MSG_STRUCT *);
static int checkRetStatus (IMS_MSG_STRUCT *);
static void freeDeviceList (DEVICE_LIST *);

/*
** Global Variables.
*/
static IMS_QI_DESC_OBJ *qDesc = (IMS_QI_DESC_OBJ *) NULL;
static DEVICE_LIST *deviceList;
static char cmdBuf[255];

/******************************************************************************
**
** ims_deviceAlloc ()
**
** Determine an available device from the list of devices found in the
** catalog.  Check to make sure that the device is truly available to
** the system then update the status of that device in the catalog to
** be in-use.
**
** This function will specifically return IMS_WARNING when
** a device of the given media type is not currently available.
** Because of this possible IMS_WARNING statuses returned by
** the ims_qi*() calls are upgraded to IMS_ERROR.
**
******************************************************************************/

int ims_deviceAlloc (
	IMS_MSG_STRUCT *msgDesc,
	char *mediaUserSpec,
	DBSMALLINT mediaType,
	DBINT orderId,
	DEVICE_INFO *deviceInfo)
{
	MEDIA_USER_SPEC *userSpec;
	DEVICE_LIST *listPtr;
	struct utsname uname_info;    /* Structure for uname() */
	char hostName[IMS_HOST_LEN+1];
	int status;
	int deviceAvailFlag;
	int fd;

	/*
	** lint: pointer cast may result in improper alignment
	** The structure definitions are identical.
	*/
	userSpec = (MEDIA_USER_SPEC *) mediaUserSpec;

	(void) memset (deviceInfo, 0, (size_t) sizeof (DEVICE_INFO));
	deviceList = (DEVICE_LIST *) NULL;

	/*
	** Login to the database server.
	*/
	if ((status = openConnection (msgDesc, userSpec)) < IMS_OK)
	{
		if (status == IMS_WARNING)
		{
			return (IMS_ERROR);
		}
		else
		{
			return (status);
		}
	}

	/*
	** Get the local host name.
	*/
	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
	hostName[IMS_HOST_LEN] = '\0';  /* Just in case. */

	/*
	** Get a list of available tape drive devices from the catalog.
	*/
	if ((status = getDeviceList (msgDesc, mediaType, hostName)) < IMS_OK)
	{
		goto ALLOC_ERROR;
	}

	/*
	** Go through the list of available devices, if needed, to find one
	** that is truly available according to the system.
	*/
	listPtr = deviceList;
	deviceAvailFlag = IMS_FALSE;
	while ((deviceAvailFlag == IMS_FALSE) &&
		(listPtr != (DEVICE_LIST *) NULL))
	{
		/*
		** If the media type is DISK don't worry about status conflicts.
		*/
		if (mediaType == IMS_DISK)
		{
			deviceAvailFlag = IMS_TRUE;
			continue;
		}

		/*
		** Append the tape density to the device path according to
		** the media type. This only pertains to tape devices.
		*/
		(void) strcat (listPtr->path, listPtr->path_extension);

		if ((fd = open (listPtr->path, O_RDONLY)) == -1)
		{
			switch (errno)
			{
			case EIO:    /* No tape in device or device is off-line. */
				deviceAvailFlag = IMS_TRUE;
				break;

			default:     /* Anything else represents a status conflict. */
				(void) ims_msg (msgDesc, IMS_WARNING,
					"'%s' has a status conflict.",
					listPtr->name);
			}

			if (deviceAvailFlag == IMS_TRUE)
			{
				continue;
			}
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_WARNING,
				"'%s' has a status conflict.",
				listPtr->name);
			(void) close (fd);
		}

		listPtr = listPtr->next;
	}

	/*
	** See if we went through the whole list of devices.
	*/
	if (listPtr == (DEVICE_LIST *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"There are no devices available for media type '%s' on host '%s'.",
			ims_mediaDesc (mediaType), hostName);
		status = IMS_WARNING;
		goto ALLOC_ERROR;
	}

	/*
	** Allocate the device and populate the deviceInfo structure.
	*/
	if ((status = allocateDevice (msgDesc, listPtr->device_id,
		mediaType, orderId, deviceInfo)) < IMS_OK)
	{
		status = IMS_ERROR;
		goto ALLOC_ERROR;
	}

	/*
	** Append the tape density to the device path according to
	** the media type. This only pertains to tape devices.
	*/
	(void) strcat (deviceInfo->path, deviceInfo->path_extension);

	/*
	** Free the device list structure.
	** Close the server connection and free the query descriptor.
	*/
	freeDeviceList (deviceList);
	(void) ims_qiFreeDesc (qDesc);

	return (IMS_OK);

ALLOC_ERROR:
	/*
	** Free the device list structure.
	** Close the server connection and free the query descriptor.
	*/
	freeDeviceList (deviceList);
	(void) ims_qiFreeDesc (qDesc);

	return (status);
}

/******************************************************************************
**
** ims_deviceStatusList ()
**
** Return a list of devices with their statuses for the given media type.
**
******************************************************************************/

int ims_deviceStatusList (
	IMS_MSG_STRUCT *msgDesc,
	char *mediaUserSpec,
	DBSMALLINT mediaType,
	DEVICE_INFO *deviceInfo)
{
	MEDIA_USER_SPEC *userSpec;
	DEVICE_INFO *infoPtr;
	struct utsname uname_info;    /* Structure for uname() */
	char hostName[IMS_HOST_LEN+1];
	int status;
	int fd;

	/*
	** lint: pointer cast may result in improper alignment
	** The structure definitions are identical.
	*/
	userSpec = (MEDIA_USER_SPEC *) mediaUserSpec;

	(void) memset (deviceInfo, 0, (size_t) sizeof (DEVICE_INFO));

	/*
	** Login to the database server.
	*/
	if ((status = openConnection (msgDesc, userSpec)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Get the local host name.
	*/
	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
	hostName[IMS_HOST_LEN] = '\0';  /* Just in case. */

	/*
	** Get an available tape drive device from the catalog.
	*/
	if ((status = getDeviceStatus (msgDesc, mediaType, hostName,
		deviceInfo)) < IMS_OK)
	{
		(void) ims_qiFreeDesc (qDesc);
		return (status);
	}

	/*
	** Open each tape drive device in the list and check the catalog
	** status with the system status.
	*/
	infoPtr = deviceInfo;
	while (infoPtr != (DEVICE_INFO *) NULL)
	{
		/*
		** If the this is an FTP device we will skip checking the 
		** system status.
		*/
		if ((strcmp (infoPtr->path, ".") == 0) ||
			(strstr (infoPtr->name, "FTP") != (char *) NULL))
		{
			infoPtr = infoPtr->next;
			continue;
		}

		/*
		** Append the tape density to the device path according to
		** the media type. This only pertains to tape devices.
		*/
		(void) strcat (infoPtr->path, infoPtr->path_extension);

		/*
		** Open the path and interpret the results.
		*/
		if ((fd = open (infoPtr->path, O_RDONLY)) == -1)
		{
			switch (errno)
			{
			case EIO:    /* No tape in device or device is off-line. */
				if (infoPtr->status == DEVICE_INUSE)
				{
					(void) ims_msg (msgDesc, IMS_WARNING,
						"'%s' has no tape in it or the device is off-line which conflicts with its status of '%s'.",
						infoPtr->name, ims_statusDesc (infoPtr->status));
					infoPtr->status_conflict = IMS_TRUE;
				}
				break;

			case ENOENT: /* The device path does not exist. */
				if (infoPtr->status == DEVICE_AVAILABLE)
				{
					(void) ims_msg (msgDesc, IMS_WARNING,
						"'%s' has a non-existent path '%s' which conflicts with its status of '%s'.",
						infoPtr->name, infoPtr->path,
						ims_statusDesc (infoPtr->status));
					infoPtr->status_conflict = IMS_TRUE;
				}
				break;

			case EBUSY:  /* Device in use by another process. */
				if (infoPtr->status == DEVICE_AVAILABLE)
				{
					(void) ims_msg (msgDesc, IMS_WARNING,
						"'%s' is busy which conflicts with its status of '%s'.",
						infoPtr->name, ims_statusDesc (infoPtr->status));
					infoPtr->status_conflict = IMS_TRUE;
				}
				break;

			case ENXIO:  /* The device does not exist. */
				if (infoPtr->status == DEVICE_AVAILABLE)
				{
					(void) ims_msg (msgDesc, IMS_WARNING,
						"'%s' does not exist which conflicts with its status of '%s'.",
						infoPtr->name, ims_statusDesc (infoPtr->status));
					infoPtr->status_conflict = IMS_TRUE;
				}
				break;

			case EPERM:  /* Another system has reserved the device. */
				if (infoPtr->status == DEVICE_AVAILABLE)
				{
					(void) ims_msg (msgDesc, IMS_WARNING,
						"'%s' is reserved which conflicts with its status of '%s'.",
						infoPtr->name, ims_statusDesc (infoPtr->status));
					infoPtr->status_conflict = IMS_TRUE;
				}
				break;

			default:
				if (infoPtr->status == DEVICE_AVAILABLE)
				{
					(void) ims_msg (msgDesc, IMS_WARNING,
						"Could not open '%s'. %s. This conflicts with its status of '%s'",
						infoPtr->name, strerror (errno),
						ims_statusDesc (infoPtr->status));
					infoPtr->status_conflict = IMS_TRUE;
				}
			}
		}
		else
		{
			if (infoPtr->status == DEVICE_AVAILABLE)
			{
				(void) ims_msg (msgDesc, IMS_WARNING,
					"'%s' has a tape in it which conflicts with its status of '%s'.",
					infoPtr->name, ims_statusDesc (infoPtr->status));
				infoPtr->status_conflict = IMS_TRUE;
			}
			(void) close (fd);
		}

		infoPtr = infoPtr->next;
	}

	/*
	** Close the connection and free the query descriptor.
	*/
	(void) ims_qiFreeDesc (qDesc);

	return (IMS_OK);
}

/******************************************************************************
**
** ims_deviceStatusChange ()
**
** Change the status for the given device in the catalog.
**
******************************************************************************/

int ims_deviceStatusChange (
	IMS_MSG_STRUCT *msgDesc,
	char *mediaUserSpec,
	DBSMALLINT deviceId,
	DBSMALLINT deviceStatus,
	DBCHAR *opComment)
{
	MEDIA_USER_SPEC *userSpec;
	int status;

	/*
	** lint: pointer cast may result in improper alignment
	** The structure definitions are identical.
	*/
	userSpec = (MEDIA_USER_SPEC *) mediaUserSpec;

	if ((status = openConnection (msgDesc, userSpec)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Change the status of the given device.
	*/
	status = changeDeviceStatus (msgDesc, deviceId, deviceStatus, opComment);

	/*
	** Close the connection and free the query descriptor.
	*/
	(void) ims_qiFreeDesc (qDesc);

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
** ims_deviceMediaId ()
**
** Return the next mediaId character string, based on the mediaType.
** The format of the media identifier is as follows:
**
**   XXYYYZ999999
**
** XX  - Station Code
**    AF (ASF)
**
** YYY - Tape Type
**    PDC (Product Distribution CEOS)
**    PDT (Product Distribution TAR)
**    PDD (Product Distribution Disk)
**
** Z   - Media Type
**    F (4-MM)
**    G (8-MM)
**    H (9-Track)
**    I (Disk)
**
******************************************************************************/

int ims_deviceMediaId (
	IMS_MSG_STRUCT *msgDesc,
	char *mediaUserSpec,
	DBSMALLINT mediaType,
	DBSMALLINT mediaFormat,
	DBCHAR *mediaId,
	int queryFlag)
{
	MEDIA_USER_SPEC *userSpec;
	DBSMALLINT mediaIdType;
	DBCHAR mediaIdCount[10];
	char *tapeType;
	int status;

	/*
	** lint: pointer cast may result in improper alignment
	** The structure definitions are identical.
	*/
	userSpec = (MEDIA_USER_SPEC *) mediaUserSpec;

	/*
	** Assign the tape type value.
	*/
	if (mediaType == IMS_DISK)
	{
		tapeType = "PDD";
	}
	else /* All other tape media types. */
	{
		if (mediaFormat == IMS_CEOS)
		{
			tapeType = "PDC";
		}
		else if (mediaFormat == IMS_TAR)
		{
			tapeType = "PDT";
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Media format is invalid for determining the media identifier.");
			return (IMS_ERROR);
		}
	}

	/*
	** Convert the media type to the media id type.
	*/
	switch (mediaType)
	{
		case IMS_4_MM:
		case IMS_4_MM_HD:
			mediaIdType = IMS_4M;
			(void) sprintf (mediaId, "AF%sF", tapeType);
			break;

		case IMS_8_MM:
		case IMS_8_MM_HD:
			mediaIdType = IMS_8M;
			(void) sprintf (mediaId, "AF%sG", tapeType);
			break;

		case IMS_9_TRACK:
		case IMS_9_TRACK_HD:
			mediaIdType = IMS_9T;
			(void) sprintf (mediaId, "AF%sH", tapeType);
			break;

		case IMS_DISK:
			mediaIdType = IMS_ET;
			(void) sprintf (mediaId, "AF%sI", tapeType);
			break;

		default:
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Media type '%s' is invalid for obtaining a media identifier.",
				ims_mediaDesc (mediaType));
			return (IMS_ERROR);
	}

	/*
	** Open the database server connection.
	*/
	if ((status = openConnection (msgDesc, userSpec)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Get the media identifier count.
	*/
	if ((status = getMediaIdCount (msgDesc, mediaIdType, mediaIdCount,
		queryFlag)) < IMS_OK)
	{
		(void) ims_qiFreeDesc (qDesc);
		return (status);
	}

	/*
	** Put the media identifier together.
	*/
	(void) strcat (mediaId, mediaIdCount);

	/*
	** Close the connection and free the query descriptor.
	*/
	(void) ims_qiFreeDesc (qDesc);
	return (IMS_OK);
}

/******************************************************************************
**
** ims_deviceFree ()
**
** Update the status for the given device in the catalog to be available.
**
******************************************************************************/

int ims_deviceFree (
	IMS_MSG_STRUCT *msgDesc,
	char *mediaUserSpec,
	DBSMALLINT deviceId)
{
	MEDIA_USER_SPEC *userSpec;
	int status;

	/*
	** lint: pointer cast may result in improper alignment
	** The structure definitions are identical.
	*/
	userSpec = (MEDIA_USER_SPEC *) mediaUserSpec;

	if ((status = openConnection (msgDesc, userSpec)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Set the status for the given device to available.
	*/
	status = freeDevice (msgDesc, deviceId);

	/*
	** Close the connection and free the query descriptor.
	*/
	(void) ims_qiFreeDesc (qDesc);

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
** ims_deviceTapeCheck ()
**
** Checks to see if a tape is present in the given tape device.
**
** This function will specifically return IMS_WARNING when
** the given device does not have a tape in it.
**
******************************************************************************/

int ims_deviceTapeCheck (
	IMS_MSG_STRUCT *msgDesc,
	DEVICE_INFO *deviceInfo)
{
	int fd;
	int status;

	if ((fd = open (deviceInfo->path, O_RDONLY)) == -1)
	{
		switch (errno)
		{
		case EIO:  /* No tape in the device or device is off-line. */
			(void) ims_msg (msgDesc, IMS_WARNING,
			"'%s' has no tape in it or the device is off-line.",
			  deviceInfo->name);
			status = IMS_WARNING;
			break;

		default:  /* Anything else is a status conflict. */
			(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not open '%s'. %s",
			  deviceInfo->name, strerror (errno));
			status = IMS_ERROR;
		}
	}
	else
	{
		status = IMS_OK;
		(void) close (fd);
	}

	return (status);
}

/******************************************************************************
**
** ims_deviceTapeEject()
**
** Rewind the tape in the given device, and then eject it.
**
******************************************************************************/

int ims_deviceTapeEject(
	IMS_MSG_STRUCT *msgDesc,
	DEVICE_INFO *deviceInfo)
{
	int status;
	struct mtop mt_command;  /* ioctl() command structure. */
	struct mtget mt_status;  /* ioctl() results structure. */
	int fd;

	/*
	** Open the tape drive device.
	*/
	if ((fd = open (deviceInfo->path, O_RDONLY)) == -1)
	{
		switch (errno)
		{
		case EIO:  /* No tape in the device or device is off-line. */
			(void) ims_msg (msgDesc, IMS_WARNING,
			"'%s' has no tape in it or the device is off-line.",
			  deviceInfo->name);
			status = IMS_WARNING;
			break;

		default:  /* Anything else is a status conflict. */
			(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not open '%s'. %s",
			  deviceInfo->name, strerror (errno));
			status = IMS_ERROR;
		}

		return (status);
	}

	/*
	** Send a command to rewind and eject the tape.
	*/
	mt_command.mt_op = MTOFFL;
	mt_command.mt_count = 1;

	if (ioctl (fd, MTIOCTOP, &mt_command) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not send a command to '%s'. %s",
			deviceInfo->name, strerror (errno));
		(void) close (fd);
		return (IMS_ERROR);
	}

	/*
	** Get status info.
	*/
	if (ioctl (fd, MTIOCGET, &mt_status) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not obtain the status from '%s'. %s",
			deviceInfo->name, strerror (errno));
		(void) close (fd);
		return (IMS_ERROR);
	}
	else
	{
		if (mt_status.mt_erreg < 0)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Could not rewind and eject the tape in '%s'. %s",
				deviceInfo->name, strerror (errno));
			(void) close (fd);
			return (IMS_ERROR);
		}
	}

	(void) close (fd);

	return (IMS_OK);
}

/******************************************************************************
**
** ims_deviceTapeInfo ()
**
** Obtain information and status from the given device.
**
******************************************************************************/

int ims_deviceTapeInfo (
	IMS_MSG_STRUCT *msgDesc,
	DEVICE_INFO *deviceInfo)
{
	int status;
	struct mtdrivetype mt_type;  /* ioctl() type structure. */
	struct mtop mt_command;      /* ioctl() command structure. */
	struct mtget mt_status;      /* ioctl() results structure. */
	int fd;

	/*
	** Open the tape drive device.
	*/
	if ((fd = open (deviceInfo->path, O_RDONLY)) == -1)
	{
		switch (errno)
		{
		case EIO:  /* No tape in the device or device is off-line. */
			(void) ims_msg (msgDesc, IMS_WARNING,
			"'%s' has no tape in it or the device is off-line.",
			  deviceInfo->name);
			status = IMS_WARNING;
			break;

		default:  /* Anything else is a status conflict. */
			(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not open '%s'. %s",
			  deviceInfo->name, strerror (errno));
			status = IMS_ERROR;
		}

		return (status);
	}

	/*
	** Get tape drive device configuration information.
	*/
	if (ioctl (fd, MTIOCGETDRIVETYPE, &mt_type) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not obtain the configuration for '%s'. %s",
			deviceInfo->name, strerror (errno));
		(void) close (fd);
		return (IMS_ERROR);
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"Tape Drive Device Configuration\nname - %s\nvid - %s\ntype - %c\nbsize - %hd\noptions - %d\nmax_rretries - %d\nmax_wretries - %d\ndensities - %u, %u, %u, %u\ndefault_density - %u\nspeeds - %u, %u, %u, %u",
			mt_type.name, mt_type.vid, mt_type.type, mt_type.bsize,
			mt_type.options, mt_type.max_rretries, mt_type.max_wretries,
			mt_type.densities[0], mt_type.densities[1],
			mt_type.densities[2], mt_type.densities[3],
			mt_type.default_density,
			mt_type.speeds[0], mt_type.speeds[1],
			mt_type.speeds[2], mt_type.speeds[3]);
	}

	/*
	** Send a no-operation command to the device.
	*/
	mt_command.mt_op = MTNOP;
	mt_command.mt_count = 1;

	if (ioctl (fd, MTIOCTOP, &mt_command) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not send a command to '%s'. %s",
			deviceInfo->name, strerror (errno));
		(void) close (fd);
		return (IMS_ERROR);
	}

	/*
	** Get status info.
	*/
	if (ioctl (fd, MTIOCGET, &mt_status) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not obtain the status from '%s'. %s",
			deviceInfo->name, strerror (errno));
		(void) close (fd);
		return (IMS_ERROR);
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"Tape Drive Device Status\ntype - %hd\ndsreg - %hd\nerreg - %hd\nflags - %x\nbf - %hd",
			mt_status.mt_type, mt_status.mt_dsreg,
			mt_status.mt_erreg, mt_status.mt_flags,
			mt_status.mt_bf);
	}

	(void) close (fd);

	return (IMS_OK);
}

/******************************************************************************
**
** openConnection ()
**
******************************************************************************/

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

/******************************************************************************
**
** getDeviceList ()
**
** This function will specifically return IMS_WARNING when
** a device of the given media type is not currently available.
** Because of this, possible IMS_WARNING statuses returned by
** the ims_qi*() calls are upgraded to IMS_ERROR.
**
******************************************************************************/

static int getDeviceList (
	IMS_MSG_STRUCT *msgDesc,
	DBSMALLINT mediaType,
	char *hostName)
{
	DEVICE_LIST *currPtr;
	DEVICE_LIST *prevPtr;
	int status;
	int rowCount;

	prevPtr = (DEVICE_LIST *) NULL;

	/*
	** Set up the command buffer with the stored procedure call.
	*/
	(void) sprintf (cmdBuf, "med_get_device_list %d, '%s'",
		mediaType, hostName);

	/*
	** Process the result rows for this query.
	*/
	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (IMS_ERROR);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/*
		** Allocate space for the DEVICE_LIST structure.
		*/
		if ((currPtr = (DEVICE_LIST *) malloc
			((size_t) sizeof (DEVICE_LIST))) ==
			(DEVICE_LIST *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate memory for DEVICE_LIST structure.");
			while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
			return (IMS_FATAL);
		}

		/*
		** deviceList points to the first element of the list.
		*/
		if (++rowCount == 1)
		{
			deviceList = currPtr;
		}
		else
		{
			prevPtr->next = currPtr;
		}

		currPtr->next = (DEVICE_LIST *) NULL;

		/*
		** Copy in the returned data.
		*/

		/* device_id */
		(void) memcpy (&(currPtr->device_id), IMS_VALUE (qDesc, 0),
			IMS_VALUELENGTH (qDesc, 0));

		/* name */
		(void) memcpy (currPtr->name, IMS_VALUE (qDesc, 1),
			IMS_VALUELENGTH (qDesc, 1));
		currPtr->name[IMS_VALUELENGTH (qDesc, 1)] = '\0';
		(void) ims_truncStr (currPtr->name);

		/* path */
		(void) memcpy (currPtr->path, IMS_VALUE (qDesc, 2),
			IMS_VALUELENGTH (qDesc, 2));
		currPtr->path[IMS_VALUELENGTH (qDesc, 2)] = '\0';

		/* path_extension */
		if ((IMS_VALUELENGTH (qDesc, 3) == 0) ||
			(IMS_VALUE (qDesc, 3) == (char *) NULL))
		{
			(void) strcpy (currPtr->path_extension, "");
		}
		else
		{
			(void) memcpy (currPtr->path_extension, IMS_VALUE (qDesc, 3),
				IMS_VALUELENGTH (qDesc, 3));
			currPtr->path_extension[IMS_VALUELENGTH (qDesc, 3)] = '\0';
			(void) ims_truncStr (currPtr->path_extension);
		}

		prevPtr = currPtr;
	}

	/*
	** Check the stored procedure status return value.
	*/
	if ((status = checkRetStatus (msgDesc)) < IMS_OK)
	{
		return (IMS_ERROR);
	}

	/*
	** Check to see if any devices were returned.
	*/
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"There are no devices available for media type '%s' on host '%s'.",
			ims_mediaDesc (mediaType), hostName);
		return (IMS_WARNING);
	}

	/*
	** If the media type is DISK we should only get one
	** device returned.
	*/
	if ((mediaType == IMS_DISK) && (IMS_AFFECTED (qDesc) > 1))
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"There is more than one device available for media type '%s' on host '%s', when there should only be one.",
			ims_mediaDesc (mediaType), hostName);
		return (IMS_ERROR);
	}

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not reset the query descriptor.");
		return (IMS_ERROR);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** allocateDevice ()
**
******************************************************************************/

static int allocateDevice (
	IMS_MSG_STRUCT *msgDesc,
	DBSMALLINT deviceId,
	DBSMALLINT mediaType,
	DBINT orderId,
	DEVICE_INFO *deviceInfo)
{
	int status;

	/*
	** Set up the command buffer with the stored procedure call.
	*/
	(void) sprintf (cmdBuf, "med_allocate_device %d, %d, %ld",
		deviceId, mediaType, orderId);

	/*
	** Load the query descriptor with return attribute information.
	** This is needed for IMS_CONVERT().
	*/
	if ((status = ims_qiTblDesc (qDesc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Execute the command.
	*/
	if ((status = execCmd (msgDesc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Copy in the returned data.
	*/

	/* device_id */
	(void) memcpy (&(deviceInfo->device_id), IMS_VALUE (qDesc, 0),
		IMS_VALUELENGTH (qDesc, 0));

	/* name */
	(void) memcpy ( deviceInfo->name, IMS_VALUE (qDesc, 1),
		IMS_VALUELENGTH (qDesc, 1));
	deviceInfo->name[IMS_VALUELENGTH (qDesc, 1)] = '\0';
	(void) ims_truncStr (deviceInfo->name);

	/* status */
	(void) memcpy (&(deviceInfo->status), IMS_VALUE (qDesc, 2),
		IMS_VALUELENGTH (qDesc, 2));

	/* order_id */
	(void) memcpy (&(deviceInfo->order_id), IMS_VALUE (qDesc, 3),
		IMS_VALUELENGTH (qDesc, 3));

	/* last_requested - Convert date/time value to character string. */
	IMS_CONVERT (qDesc, 4, IMS_CHAR, deviceInfo->last_requested,
		IMS_DBMS_DATETIME_LEN);
	deviceInfo->last_requested[IMS_DBMS_DATETIME_LEN] = '\0';

	/* host */
	(void) memcpy (deviceInfo->host, IMS_VALUE (qDesc, 5),
		IMS_VALUELENGTH (qDesc, 5));
	deviceInfo->host[IMS_VALUELENGTH (qDesc, 5)] = '\0';
	(void) ims_truncStr (deviceInfo->host);

	/* path */
	(void) memcpy (deviceInfo->path, IMS_VALUE (qDesc, 6),
		IMS_VALUELENGTH (qDesc, 6));
	deviceInfo->path[IMS_VALUELENGTH (qDesc, 6)] = '\0';

	/* description */
	(void) memcpy (deviceInfo->description, IMS_VALUE (qDesc, 7),
		IMS_VALUELENGTH (qDesc, 7));
	deviceInfo->description[IMS_VALUELENGTH (qDesc, 7)] = '\0';

	/* op_comment */
	if ((IMS_VALUELENGTH (qDesc, 8) == 0) ||
		(IMS_VALUE (qDesc, 8) == (char *) NULL))
	{
		(void) strcpy (deviceInfo->op_comment, "");
	}
	else
	{
		(void) memcpy (deviceInfo->op_comment, IMS_VALUE (qDesc, 8),
			IMS_VALUELENGTH (qDesc, 8));
		deviceInfo->op_comment[IMS_VALUELENGTH (qDesc, 8)] = '\0';
	}

	/* path_extension */
	if ((IMS_VALUELENGTH (qDesc, 9) == 0) ||
		(IMS_VALUE (qDesc, 9) == (char *) NULL))
	{
		(void) strcpy (deviceInfo->path_extension, "");
	}
	else
	{
		(void) memcpy (deviceInfo->path_extension, IMS_VALUE (qDesc, 9),
			IMS_VALUELENGTH (qDesc, 9));
		deviceInfo->path_extension[IMS_VALUELENGTH (qDesc, 9)] = '\0';
		(void) ims_truncStr (deviceInfo->path_extension);
	}

	deviceInfo->status_conflict = IMS_FALSE;
	deviceInfo->next = (DEVICE_INFO *) NULL;

	return (IMS_OK);
}

/******************************************************************************
**
** getDeviceStatus ()
**
******************************************************************************/

static int getDeviceStatus (
	IMS_MSG_STRUCT *msgDesc,
	DBSMALLINT mediaType,
	char *hostName,
	DEVICE_INFO *deviceInfo)
{
	DEVICE_INFO *currPtr;
	DEVICE_INFO *prevPtr;
	int status;
	int rowCount;

	prevPtr = (DEVICE_INFO *) NULL;

	/*
	** Set up the command buffer with the stored procedure call.
	*/
	(void) sprintf (cmdBuf, "med_get_device_status '%s', %d",
		hostName, mediaType);

	/*
	** Load the query descriptor with return attribute information.
	** This is needed for IMS_CONVERT().
	*/
	if ((status = ims_qiTblDesc (qDesc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Process the result rows for this query.
	*/
	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (ims_msgGetSeverity (msgDesc));
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/*
		** A row has been returned so let's make sure we have an allocated
		** space to put this stuff.
		**
		** We have a special case here where the first item in the
		** DEVICE_INFO linked list is already allocated by the function
		** passing it in. So for the first row returned we do not need
		** to allocate space.
		*/
		if (++rowCount == 1)
		{
			currPtr = deviceInfo;
		}
		else
		{
			/*
			** Allocate space for the DEVICE_INFO structure.
			*/
			if ((currPtr = (DEVICE_INFO *) malloc
				((size_t) sizeof (DEVICE_INFO))) ==
				(DEVICE_INFO *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Could not allocate memory for DEVICE_INFO structure.");
				while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
				return (IMS_FATAL);
			}

			prevPtr->next = currPtr;
		}

		currPtr->status_conflict = IMS_FALSE;
		currPtr->next = (DEVICE_INFO *) NULL;

		/*
		** Copy in the returned data.
		*/

		/* device_id */
		(void) memcpy (&(currPtr->device_id), IMS_VALUE (qDesc, 0),
			IMS_VALUELENGTH (qDesc, 0));

		/* name */
		(void) memcpy (currPtr->name, IMS_VALUE (qDesc, 1),
			IMS_VALUELENGTH (qDesc, 1));
		currPtr->name[IMS_VALUELENGTH (qDesc, 1)] = '\0';
		(void) ims_truncStr (currPtr->name);

		/* status */
		(void) memcpy (&(currPtr->status), IMS_VALUE (qDesc, 2),
			IMS_VALUELENGTH (qDesc, 2));

		/* order_id */
		if ((IMS_VALUELENGTH (qDesc, 3) == 0) ||
			(IMS_VALUE (qDesc, 3) == (char *) NULL))
		{
			/* order_id is NULL. */
			currPtr->order_id = NULL_ORDER;
		}
		else
		{
			(void) memcpy (&(currPtr->order_id), IMS_VALUE (qDesc, 3),
				IMS_VALUELENGTH (qDesc, 3));
		}

		/* last_requested - Convert date/time value to character string. */
		IMS_CONVERT (qDesc, 4, IMS_CHAR, currPtr->last_requested,
			IMS_DBMS_DATETIME_LEN);
		currPtr->last_requested[IMS_DBMS_DATETIME_LEN] = '\0';

		/* host */
		(void) memcpy (currPtr->host, IMS_VALUE (qDesc, 5),
			IMS_VALUELENGTH (qDesc, 5));
		currPtr->host[IMS_VALUELENGTH (qDesc, 5)] = '\0';
		(void) ims_truncStr (currPtr->host);

		/* path */
		(void) memcpy (currPtr->path, IMS_VALUE (qDesc, 6),
			IMS_VALUELENGTH (qDesc, 6));
		currPtr->path[IMS_VALUELENGTH (qDesc, 6)] = '\0';

		/* description */
		(void) memcpy (currPtr->description, IMS_VALUE (qDesc, 7),
			IMS_VALUELENGTH (qDesc, 7));
		currPtr->description[IMS_VALUELENGTH (qDesc, 7)] = '\0';

		/* op_comment */
		if ((IMS_VALUELENGTH (qDesc, 8) == 0) ||
			(IMS_VALUE (qDesc, 8) == (char *) NULL))
		{
			(void) strcpy (currPtr->op_comment, "");
		}
		else
		{
			(void) memcpy (currPtr->op_comment, IMS_VALUE (qDesc, 8),
				IMS_VALUELENGTH (qDesc, 8));
			currPtr->op_comment[IMS_VALUELENGTH (qDesc, 8)] = '\0';
		}

		/* path_extension */
		if (mediaType == IMS_NO_MEDIA_TYPE)
		{
			(void) strcpy (currPtr->path_extension, "");
		}
		else
		{
			if ((IMS_VALUELENGTH (qDesc, 9) == 0) ||
				(IMS_VALUE (qDesc, 9) == (char *) NULL))
			{
				(void) strcpy (currPtr->path_extension, "");
			}
			else
			{
				(void) memcpy (currPtr->path_extension, IMS_VALUE (qDesc, 9),
					IMS_VALUELENGTH (qDesc, 9));
				currPtr->path_extension[IMS_VALUELENGTH (qDesc, 9)] = '\0';
				(void) ims_truncStr (currPtr->path_extension);
			}
		}

		prevPtr = currPtr;
	}

	/*
	** Check the stored procedure status returned value.
	*/
	if ((status = checkRetStatus (msgDesc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Check to see if any devices were returned.
	*/
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"There are no devices for media type '%s' on host '%s'.",
			ims_mediaDesc (mediaType), hostName);
		return (IMS_ERROR);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** changeDeviceStatus ()
**
******************************************************************************/

static int changeDeviceStatus (
	IMS_MSG_STRUCT *msgDesc,
	DBSMALLINT deviceId,
	DBSMALLINT deviceStatus,
	DBCHAR *opComment)
{
	int status;

	/*
	** Set up the command buffer with the stored procedure call.
	** Only send the comment if it is not NULL.
	*/
	if ((opComment == (DBCHAR *) NULL) || (strlen (opComment) == 0))
	{
		(void) sprintf (cmdBuf, "med_change_device_status %d, %d",
			deviceId, deviceStatus);
	}
	else
	{
		(void) sprintf (cmdBuf, "med_change_device_status %d, %d, '%s'",
			deviceId, deviceStatus, opComment);
	}

	/*
	** Execute the command.
	*/
	if ((status = execCmd (msgDesc)) < IMS_OK)
	{
		return (status);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** getMediaIdCount ()
**
** Get the next media identifier count for the given media id type.
** Pass this value back with the mediaIdCount argument.
**
******************************************************************************/

static int getMediaIdCount (
	IMS_MSG_STRUCT *msgDesc,
	DBSMALLINT mediaIdType,
	DBCHAR *mediaIdCount,
	int queryFlag)
{
	int status;

	/*
	** Check the queryFlag and provide the appropriate input
	** for the stored procedure call.
	*/
	if (queryFlag == IMS_TRUE)
	{
		(void) sprintf (cmdBuf, "med_incr_media_id %d, 'Y'",
			mediaIdType);
	}
	else
	{
		(void) sprintf (cmdBuf, "med_incr_media_id %d, 'N'",
			mediaIdType);
	}

	/*
	** Execute the command.
	*/
	if ((status = execCmd (msgDesc)) < IMS_OK)
	{
		return (status);
	}

	/* mediaIdCount */
	(void) memcpy (mediaIdCount, IMS_VALUE (qDesc, 0),
		IMS_VALUELENGTH (qDesc, 0));
	mediaIdCount[IMS_VALUELENGTH (qDesc, 0)] = '\0';

	return (IMS_OK);
}   /*  getMediaIdCount  */

/******************************************************************************
**
** freeDevice ()
**
******************************************************************************/

static int freeDevice (
	IMS_MSG_STRUCT *msgDesc,
	DBSMALLINT deviceId)
{
	int status;

	/*
	** Set up the command buffer with the stored procedure call.
	*/
	(void) sprintf (cmdBuf, "med_free_device %d", deviceId);

	/*
	** Execute the command.
	*/
	if ((status = execCmd (msgDesc)) < IMS_OK)
	{
		return (status);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** execCmd ()
**
** Execute an SQL procedure that writes data into the catalog database.
** We don't pass a parameter, but assume that when this function is called,
** the declared static buffer 'cmdBuf' has been properly filled in with
** the SQL statement to be executed.
**
** THIS ROUTINE IS ONLY USED FOR EXEUTING SQL STATEMENTS THAT WILL ONLY
** RETURN ONE ROW OR LESS FROM THE DATABASE.
**
******************************************************************************/

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

/******************************************************************************
**
** checkRetStatus ()
**
** Check the return status for the stored procedure.
**
******************************************************************************/

static int checkRetStatus (
	IMS_MSG_STRUCT *msgDesc)
{
	int procReturn;
	int severity;

	/*
	** Check to see if the Sybase procedure returned a status.  If it did
	** and it is not 0 (the OK value for a return), deal with the error.
	** Return status of less than -100 correspond to message facility
	** severity levels modulo 100.
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
					(void) ims_msg (msgDesc, IMS_FATAL,
						"Procedure '%s' returned an unrecognized status of '%d'.",
						qDesc->cmd, procReturn);
					severity = IMS_FATAL;
					break;
			}
			return (severity);
		}
	}
	return (IMS_OK);
}

/******************************************************************************
**
** freeDeviceList ()
**
** Free the DEVICE_LIST structure.
**
******************************************************************************/

static void freeDeviceList (
	DEVICE_LIST *currPtr)
{
	DEVICE_LIST *nextPtr;

	while (currPtr != (DEVICE_LIST *) NULL)
	{
		nextPtr = currPtr->next;
		(void) free ((char *) currPtr);
		currPtr = nextPtr;
	}
}
