static char *sccs = "@(#)ims_version.c	5.2  27 Jun 1996";
/******************************************************************************
**
** File:        ims_version.c
**
** Function:    Contains the functions used to access IMS Version Data.
**
** Author:      J. Jacobson
**
** Date:        1/11/90
**
** Modified:    1/11/94 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Added the include file cdb_dbms.h.
**
**              8/17/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
******************************************************************************/

#include <stdio.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_version.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_verion.h.
** They are listed here for reference.
**
**	int ims_printVersion (FILE *);
**	IMS_VERSION_CLASS *ims_getVersion (void);
*/

/*
** Static Version Information Structure.
*/
static char project[] = {IMS_PROJECT};
static char group[] = {IMS_GROUP};
static char version[] = {IMS_VERSION};
static char versionDate[] = {IMS_VERSION_DATE};
static char copyright1[] = {COPYRIGHT1};
static char copyright2[] = {COPYRIGHT2};
static IMS_VERSION_CLASS versionInfo =
	{project, group, version, versionDate, copyright1, copyright2};

/******************************************************************************
**
** ims_printVersion ()
**
** Print version text to the stream identified by file pointer.
**
******************************************************************************/

int ims_printVersion (
	FILE *fp)
{
	/*
	** Format version text into our file.
	*/
	if (fprintf (fp, "\n%s %s Version %s %s\n\n%s\n%s\n\n%s\n\n",
		versionInfo.project, versionInfo.group, versionInfo.version,
		versionInfo.versionDate, versionInfo.copyright1,
		versionInfo.copyright2, dbversion ()) == EOF)
	{
		/*
		** An error occurred during output.  errno will contain a
		** Unix error number if your interested.
		*/
		return (IMS_ERROR);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_getVersion ()
**
** Return a pointer to a structure containing the version information.
** 
******************************************************************************/

IMS_VERSION_CLASS *ims_getVersion (void)
{
	return (&versionInfo);
}
