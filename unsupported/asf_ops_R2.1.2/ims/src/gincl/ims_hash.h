/******************************************************************************
**
** File:        ims_hash.h
**
** Function:    Hashing structure definitions.
**
** Modified:    11/28/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
******************************************************************************/

#ifndef _IMS_HASH_H
#define _IMS_HASH_H

static char *sccsHash = "@(#)ims_hash.h	5.1  03/17/96";

enum IMS_HASH_ACTION {IMS_ENTER, IMS_FIND, IMS_EXTRACT};
enum IMS_HASH_KEY_TYPE {IMS_INTEGER, IMS_STRING};

typedef struct ims_hash_entry
{
	void *key;
	void *data;
	struct ims_hash_entry *next;
} IMS_HASH_ENTRY;

typedef struct ims_hash_struct
{
	IMS_HASH_ENTRY  **hashTable;             /* Hash table pointer */
	enum IMS_HASH_KEY_TYPE  keyType;         /* Hash key type */
	int              (*hashFunc) ();         /* Hash function pointer */
	int              (*keyCompare) ();       /* Key compare function */
	int              hashSize;               /* Hash table size */
} IMS_HASH_STRUCT;

/*
** Function Prototypes for the ims_hash.c module.
*/
IMS_HASH_STRUCT *ims_hashCreate (int, enum IMS_HASH_KEY_TYPE,
	IMS_MSG_STRUCT *);
IMS_HASH_ENTRY *ims_hashSearch (IMS_HASH_STRUCT *, IMS_HASH_ENTRY *,
	enum IMS_HASH_ACTION, IMS_MSG_STRUCT *);
void ims_hashDestroy (IMS_HASH_STRUCT *, IMS_MSG_STRUCT *);

#endif	/* !_IMS_HASH_H */
