/******************************************************************************
**
** File:        ims_version.h
**
** Function:    Defines structures and constants used by functions that
**              access version data.
**
** Author:      J. Jacobson
**
** Date:        1/11/90
**
** Modified:    8/17/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
** Notes:       Change this file whenever a version change is required.  Alter
**              the defines for IMS_GROUP, IMS_VERSION and IMS_VERSION_DATE.
**
******************************************************************************/

#ifndef _IMS_VERSION_H
#define _IMS_VERSION_H

static char *sccsVersion = "@(#)ims_version.h	6.1  03/13/98";

#define IMS_PROJECT "ASF"
#define IMS_GROUP "IMS/DADS"
#define IMS_VERSION "R2.1.2 Build 1"
#define IMS_VERSION_DATE "March, 1998"
#define COPYRIGHT1 \
	"Copyright (C) 1996, California Institute of Technology.  U.S. Government"
#define COPYRIGHT2 \
	"Sponsorship under NASA Contract NAS7-1260 is acknowledged."
	

typedef struct ims_versionClass
{
	char *project;
	char *group;
	char *version;
	char *versionDate;
	char *copyright1;
	char *copyright2;
} IMS_VERSION_CLASS;

/*
** Function Prototypes for the ims_version.c module.
*/
int ims_printVersion (FILE *);
IMS_VERSION_CLASS *ims_getVersion (void);

#endif	/* !_IMS_VERSION_H */
