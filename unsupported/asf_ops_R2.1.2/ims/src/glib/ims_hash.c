static char *sccs = "@(#)ims_hash.c	5.1  17 Mar 1996";
/******************************************************************************
**
** File:        ims_hash.c 
**
** Function:    Routines to create, load, and search hash tables based on 
**              a hash function.  Multiple hashing tables are allowed per
**              image.
**
** Author:      Hoshyar Sayah
**
** Date:        8/30/90
**
** Modified:    2/7/95 - S. Hardman - R1B
**              Cleaned up the file.
**
******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_hash.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_hash.h.
** They are listed here for reference.
**
**	IMS_HASH_STRUCT *ims_hashCreate (int, enum IMS_HASH_KEY_TYPE,
**		IMS_MSG_STRUCT *);
**	IMS_HASH_ENTRY *ims_hashSearch (IMS_HASH_STRUCT *, IMS_HASH_ENTRY *,
**		enum IMS_HASH_ACTION, IMS_MSG_STRUCT *);
**	void ims_hashDestroy (IMS_HASH_STRUCT *, IMS_MSG_STRUCT *);
*/

/*
** Local Functions.
*/
static IMS_HASH_ENTRY *hashFind (IMS_HASH_STRUCT *hashPtr, 
	IMS_HASH_ENTRY *item, IMS_MSG_STRUCT *msgDesc);
static IMS_HASH_ENTRY *hashExtract (IMS_HASH_STRUCT *hashPtr, 
	IMS_HASH_ENTRY *item, IMS_MSG_STRUCT *msgDesc);
static int intHashFunc (IMS_HASH_STRUCT *hashPtr, IMS_HASH_ENTRY *item);
static int strHashFunc (IMS_HASH_STRUCT *hashPtr, IMS_HASH_ENTRY *item);
static int intKeyCompare (void *key1, void *key2);
static int strKeyCompare (void *key1, void *key2);

/*
** Prototypes for function pointers.
*/
static int (*hashFunc) (IMS_HASH_STRUCT *, IMS_HASH_ENTRY *);
static int (*keyCompareFunc) (void *, void *);

/******************************************************************************
**
** ims_hashCreate ()
**
** Create hash table.
**
******************************************************************************/

IMS_HASH_STRUCT *ims_hashCreate (
	int hashSize,
	enum IMS_HASH_KEY_TYPE keyType,
	IMS_MSG_STRUCT *msgDesc)
{
	IMS_HASH_STRUCT *hPtr;   /* Hash structure pointer. */

	if (hashSize <= 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Hash table size cannot be <= 0.");
		return ((IMS_HASH_STRUCT *)NULL);
	}
	
	/*
	** Allocate the space for the hash structure.
	**
	** lint: pointer cast may result in improper alignment
	** No problem, malloc() aligns on worst case boundary.
	*/
	if ((hPtr = (IMS_HASH_STRUCT *)
		malloc ((unsigned) sizeof (IMS_HASH_STRUCT))) ==
		(IMS_HASH_STRUCT *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Memory allocation for IMS_HASH_STRUCT structure failed.");
		return ((IMS_HASH_STRUCT *)NULL);
	}

	/*
	** Allocate space for the hash table based on the number of buckets
	** requested by the user. All hash table entries are initialized 
	** to NULL.
	**
	** lint: pointer cast may result in improper alignment
	** No problem, calloc() aligns on worst case boundary.
	*/
	if ((hPtr->hashTable = (IMS_HASH_ENTRY **)
		calloc ((size_t) hashSize, (size_t) sizeof (IMS_HASH_ENTRY *))) == 
		(IMS_HASH_ENTRY **) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Memory allocation for hash table failed.");
		(void) free ((char *) hPtr);
		return ((IMS_HASH_STRUCT *)NULL);
	}

	/*
	** Populate the hash structure
	*/
	switch (keyType)
	{
	case IMS_INTEGER :
		hPtr->hashFunc = intHashFunc;
		hPtr->keyCompare = intKeyCompare;
		break;

	case IMS_STRING :
		hPtr->hashFunc = strHashFunc;
		hPtr->keyCompare = strKeyCompare;
		break;

	default :
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Hash key type not supported.");
		(void) free ((char *) hPtr->hashTable);
		(void) free ((char *) hPtr);
		return ((IMS_HASH_STRUCT *) NULL);
	}

	hPtr->hashSize = hashSize;
	hPtr->keyType = keyType;
	
	return (hPtr);
}

/******************************************************************************
**
** ims_hashSearch ()
**
** Search the hash table for the given item.  Returns the
** address of the item in the hash table when successful. 
** Returns NULL when failed.
**
**   hashPtr    Pointer to the hash table structure.
**   item       Item to be searched in the hash table.
**   action     Action taken by the function, IMS_ENTER or IMS_FIND.
**   msgDesc    Message Facility descriptor.
**
******************************************************************************/

IMS_HASH_ENTRY *ims_hashSearch (
	IMS_HASH_STRUCT *hashPtr,
	IMS_HASH_ENTRY *workItem,
	enum IMS_HASH_ACTION action,
	IMS_MSG_STRUCT *msgDesc)
{
	IMS_HASH_ENTRY *itemPtr;        /* Temporary pointer. */
	IMS_HASH_ENTRY *item;           /* Item to be placed in hash table. */
	int hashVal;

	/*
	** Assign hash function.
	*/
	hashFunc = hashPtr->hashFunc;

	/*
	** Make sure the hash table exists.
	*/
	if (hashPtr == (IMS_HASH_STRUCT *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Hash table is not initialized.");
		return ((IMS_HASH_ENTRY *)NULL);
	}

	/*
	** Make sure workItem->key is not null.
	*/
	if (workItem->key == (void *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Hash search key parameter is NULL.");
		return ((IMS_HASH_ENTRY *)NULL);
	}

	/*
	** When the action is IMS_ENTER, then place the item in the hash table.
	** When the action is IMS_FIND, then locate the item in the hash table.
	** "item->key" is used by the hash function to determine the
	** location of item in the hash table.
	*/
	switch (action)
	{
	case IMS_ENTER :
		/*
		** Check whether item already exists in the hash table. If item
		** exists then replace old item with the new item. If item does not
		** exist in the hash table then enter new item into the hash table.
		*/

		if ((itemPtr = (IMS_HASH_ENTRY *)
			hashFind (hashPtr, workItem, msgDesc)) ==
			(IMS_HASH_ENTRY *) NULL)
		{
			/*
			** Create entry item to be placed in the hash table.
			**
			** lint: pointer cast may result in improper alignment
			** No problem, malloc() aligns on worst case boundary.
			*/
			if ((item  = (IMS_HASH_ENTRY *) malloc ((unsigned) sizeof 
				(IMS_HASH_ENTRY))) == (IMS_HASH_ENTRY *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Allocation for item failed.");
				return ((IMS_HASH_ENTRY *)NULL); 
			}

			item->key = workItem->key;
			item->data = workItem->data;
			item->next = (IMS_HASH_ENTRY *) NULL;

			/* Get the hash value. */
			hashVal = (hashFunc) (hashPtr, item);

			/* Place item in the hash table. */
			if ((itemPtr = (IMS_HASH_ENTRY *)
				hashPtr->hashTable[hashVal]) == (IMS_HASH_ENTRY *) NULL)
			{
				hashPtr->hashTable[hashVal] = (IMS_HASH_ENTRY *) item;
			}
			else
			{
				while (itemPtr->next != (IMS_HASH_ENTRY *) NULL)
				{
					itemPtr = itemPtr->next;
				}
				itemPtr->next = item;
			}
		}
		else 
		{
			/*
			** Item already exists in the hash table. Replace the old item
			** with the new item.
			*/
			itemPtr->data = workItem->data;
			itemPtr->key = workItem->key;
			item = itemPtr;

			switch (hashPtr->keyType)
			{
			case IMS_STRING: 
				(void) ims_msg (msgDesc, IMS_WARNING,
					"Replaced item '%s' in hash table.",
					(char *)itemPtr->key);
			break;

			case IMS_INTEGER:
				(void) ims_msg (msgDesc, IMS_WARNING,
					"Replaced item '%d' in hash table.",
					*((int *)itemPtr->key));
			break;

			default:
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Hash key type not supported.");
				return ((IMS_HASH_ENTRY *)NULL);
			}
		}
		return (item);

	case IMS_FIND :
		return (hashFind (hashPtr, workItem, msgDesc));

	case IMS_EXTRACT :
		return (hashExtract (hashPtr, workItem, msgDesc));

	default:
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Hash function action not supported.");
		return ((IMS_HASH_ENTRY *)NULL);
	}
}

/******************************************************************************
**
** ims_hashDestroy ()
**
** Free the hash table structure and its associated entries.
**
******************************************************************************/

void ims_hashDestroy (
	IMS_HASH_STRUCT *hashPtr,
	IMS_MSG_STRUCT *msgDesc)
{
	int i;
	IMS_HASH_ENTRY *itemPtr, *tPtr;

	if (hashPtr == (IMS_HASH_STRUCT *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"Could not destroy hash table.");
		return;
	}

	for (i = 0; i < hashPtr->hashSize; i++)
	{
		/* itemPtr points to the list of hash table items. */
		itemPtr = hashPtr->hashTable[i];

		while (itemPtr != (IMS_HASH_ENTRY *) NULL)
		{
			tPtr = itemPtr;
			itemPtr = itemPtr->next;
			(void) free ((char *) tPtr);
		}
	}

	(void) free ((char *) hashPtr->hashTable);
	(void) free ((char *) hashPtr);

	return;
}

/******************************************************************************
**
** hashFind ()
**
** Find the item in the hash table.  
**
******************************************************************************/

static IMS_HASH_ENTRY *hashFind (
	IMS_HASH_STRUCT *hashPtr,
	IMS_HASH_ENTRY *item,
	IMS_MSG_STRUCT *msgDesc)
{
	int hashVal;
	IMS_HASH_ENTRY *itemPtr;

	/*
	** Assign hash functions.
	*/
	hashFunc = hashPtr->hashFunc;
	keyCompareFunc = hashPtr->keyCompare;

	if (hashPtr == (IMS_HASH_STRUCT *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Hashfind called without a table.");
		return ((IMS_HASH_ENTRY *) NULL);
	}

	/*
	** Search the hash table for the item.
	*/
	hashVal = (hashFunc) (hashPtr, item);

	itemPtr = (IMS_HASH_ENTRY *) hashPtr->hashTable[hashVal];

	while (itemPtr != (IMS_HASH_ENTRY *) NULL)
	{
		if ((keyCompareFunc) (item->key, itemPtr->key))
		{
			return (itemPtr);
		}
		itemPtr = itemPtr->next;
	}

	return (itemPtr);
}

/******************************************************************************
**
** hashExtract ()
**
** Extract the item from the hash table if it exists.  The item 
** will be deleted from the hash table and returned to the 
** caller.  It is the responsibility of the caller to free
** the allocated memory of the item extracted.  When item 
** does not exist then a NULL pointer will be returned.
**
******************************************************************************/

static IMS_HASH_ENTRY *hashExtract (
	IMS_HASH_STRUCT *hashPtr,
	IMS_HASH_ENTRY *item,
	IMS_MSG_STRUCT *msgDesc)
{
	int hashVal;
	IMS_HASH_ENTRY *itemPtr, *prevPtr;

	/*
	** Assign hash and compare functions.
	*/
	hashFunc = hashPtr->hashFunc;
	keyCompareFunc = hashPtr->keyCompare;

	if (hashPtr == (IMS_HASH_STRUCT *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Hashextract called without a table.");
		return ((IMS_HASH_ENTRY *) NULL);
	}

	/*
	** Search for the item in the hash table.
	*/
	hashVal = (hashFunc) (hashPtr, item);

	itemPtr = (IMS_HASH_ENTRY *) hashPtr->hashTable[hashVal];
	prevPtr = (IMS_HASH_ENTRY *) NULL;

	while (itemPtr != (IMS_HASH_ENTRY *) NULL)
	{
		if ((keyCompareFunc) (item->key, itemPtr->key))
		{
			if (prevPtr == (IMS_HASH_ENTRY *)NULL)
			{
				hashPtr->hashTable[hashVal] = itemPtr->next;
				itemPtr->next = (IMS_HASH_ENTRY *) NULL;
				return (itemPtr);
			}
			else
			{
				prevPtr->next = itemPtr->next;
				itemPtr->next = (IMS_HASH_ENTRY *) NULL;
				return (itemPtr);
			}
		}

		prevPtr = itemPtr;
		itemPtr = itemPtr->next;
	}

	return (itemPtr);
}

/******************************************************************************
**
** intHashFunc ()
**
** Hash function when key type is integer.
** 
******************************************************************************/

static int intHashFunc (
	IMS_HASH_STRUCT *hashPtr,
	IMS_HASH_ENTRY *item)
{
	return (abs(*(int *)item->key) % hashPtr->hashSize);
}

/******************************************************************************
**
** strHashFunc ()
**
** Hash function when key type is string.
**
******************************************************************************/

static int strHashFunc (
	IMS_HASH_STRUCT *hashPtr,
	IMS_HASH_ENTRY *item)
{
	int hashVal;
	char *temp;

	temp = (char *)item->key;
	for (hashVal = 0; *temp != '\0';)
	{
		hashVal += *temp++;
	}

	return (hashVal % hashPtr->hashSize);
}

/******************************************************************************
**
** intKeyCompare ()
**
** Compare function for INTEGER hash key types.
**
******************************************************************************/

static int intKeyCompare (
	void *key1,
	void *key2)
{
	if (*(int *)key1 == *(int *)key2)
	{
		return (1);
	}
	else
	{
		return (0);
	}
}

/******************************************************************************
**
** strKeyCompare ()
**
** Compare function for STRING hash key types.
**
******************************************************************************/

static int strKeyCompare (
	void *key1,
	void *key2)
{
	if (strcmp ((char *)key1, (char *)key2) == 0)
	{
		return (1);
	}
	else
	{
		return (0);
	}
}
