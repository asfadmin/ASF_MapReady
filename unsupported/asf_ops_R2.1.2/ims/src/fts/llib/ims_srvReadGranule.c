static char *sccs = "@(#)ims_srvReadGranule.c	5.1  03/18/96";
/******************************************************************************
**
** File:        ims_srvReadGranule.c
**
** Function:    This file contains functions for incrementing and
**              decrementing the readGranule counter.
**
** Author:      Hoshyar Sayah
**
** Date:        10/90
**
** Modified:    9/1/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              12/9/94 - S. Hardman - R1B
**              Ported to Sybase System 10. See the Open Server
**              Server-Library/C Reference Manual for more information.
**
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

#include <ims_dbms.h>
#include <ospublic.h>

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_hash.h>
#include <ims_archive.h>
#include <ims_ftsSrvCat.h>
#include <ims_ftsSrv.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_ftsSrv.h.
** They are listed here for reference.
**
**  FTS_READ_GRANULE *ims_incrReadGranule (IMS_HASH_STRUCT *, DBINT,
**		IMS_MSG_STRUCT *);
**  int ims_decrReadGranule (IMS_HASH_STRUCT *, FTS_READ_GRANULE *,
**		IMS_MSG_STRUCT *);
*/

/******************************************************************************
**
** ims_incrReadGranule ()
**
** Increment readGranule counter in the hash table for the
** granuleIdx input value.
**
******************************************************************************/

FTS_READ_GRANULE *ims_incrReadGranule (
	IMS_HASH_STRUCT *readHashPtr,
	DBINT granule_idx,
	IMS_MSG_STRUCT *msgStruct)
{
	IMS_HASH_ENTRY item;
	IMS_HASH_ENTRY *hashEntry;
	FTS_READ_GRANULE *readGranule;

	item.key = (void *) &granule_idx;
	item.data = (void *) NULL;

	/*
	** Now increment the file read counter.
	*/
	if ((hashEntry = ims_hashSearch (readHashPtr, &item, IMS_FIND,
		msgStruct)) == (IMS_HASH_ENTRY *)NULL)
	{
		/*
		** Entry does not exist, create a new entry for this read file
		** operation.
		*/

		/*
		** Allocate space for the FTS_READ_GRANULE structure.
		**
		** lint: pointer cast may result in improper alignment
		** ???? Review this.
		*/
		if ((readGranule = (FTS_READ_GRANULE *) malloc
			(sizeof (FTS_READ_GRANULE))) == (FTS_READ_GRANULE *)NULL)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Memory allocation for FTS_READ_GRANULE structure failed.");
			return (readGranule);
		}

		/* Initialize structure members. */
		readGranule->granule_idx = granule_idx;
		readGranule->readCounter = 1;

		item.key = (void *) &(readGranule->granule_idx);
		item.data = (void *) readGranule;

		if ((hashEntry = ims_hashSearch (readHashPtr, &item, IMS_ENTER,
			msgStruct)) == (IMS_HASH_ENTRY *)NULL)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Read counter hash entry has failed.");
			(void) free (readGranule);
			return ((FTS_READ_GRANULE *)NULL);
		}
	}
	else /* An entry already exists in the hash table. */
	{
		readGranule = (FTS_READ_GRANULE *) hashEntry->data;
		readGranule->readCounter += 1;
	}
	return (readGranule);
}

/******************************************************************************
**
** ims_decrReadGranule ()
**
** Decrement read granule hash table entry. 
**
******************************************************************************/

int ims_decrReadGranule (
	IMS_HASH_STRUCT *readHashPtr,
	FTS_READ_GRANULE *readGranule,
	IMS_MSG_STRUCT *msgStruct)
{
	IMS_HASH_ENTRY item;
	IMS_HASH_ENTRY *hashEntry;

	/*
	** Decrement the readCount for this file.
	*/
	if (readGranule->readCounter > 1)
	{
		readGranule->readCounter -= 1;
	}
	else /* Remove the last entry from the hash table. */
	{
		item.key = (void *) &(readGranule->granule_idx);

		if ((hashEntry = ims_hashSearch (readHashPtr, &item,
			IMS_EXTRACT, msgStruct)) == (IMS_HASH_ENTRY *)NULL)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Extracting FTS_READ_GRANULE from hash table failed. Contact the DBA.");
			return (IMS_FATAL);
		}

		(void) free (hashEntry->data);
		(void) free (hashEntry);
	}

	return (IMS_OK);
}
