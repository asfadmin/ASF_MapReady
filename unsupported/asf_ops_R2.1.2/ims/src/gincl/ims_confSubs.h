/******************************************************************************
**
** File:        ims_confSubs.h
**
** Function:    This file includes function prototypes for the
**              ims_confSubs.c file.
**
** Date:        11/1/94
**
******************************************************************************/

#ifndef _IMS_CONFSUBS_H
#define _IMS_CONFSUBS_H

static char *sccsConfSubs = "@(#)ims_confSubs.h	5.1  03/17/96";

/*
** Function Prototypes for the ims_confSubs.c module.
*/
int ims_openLookup (IMS_HASH_STRUCT **, IMS_CMD_CLASS [], unsigned int,
	IMS_MSG_STRUCT *);
char *ims_lookup (IMS_HASH_STRUCT *, char *, char *, IMS_MSG_STRUCT *);
void ims_closeLookup (IMS_HASH_STRUCT *, IMS_MSG_STRUCT *);

#endif	/* !_IMS_CONFSUBS_H */
