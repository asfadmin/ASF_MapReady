static char *sccs = "@(#)ims_confSubs.c	5.1  17 Mar 1996";
/******************************************************************************
**
** File:        ims_confSubs.c
**
** Function:    Functions used to initialize and retrieve valid
**              command data structures for command file parsing.
** 
** Author:      J. Jacobson
**
** Date:        1/22/90
**
** Modified:    4/10/90 - J. Jacobson
**              Change to pass separate configuration structure
**              instead for "values", instead of using an array of
**              structures which include a value field (addr).
**              By the way, this routine is general enough to be used
**              for keyword / value file parsing, as well as command
**              line parsing, cause we're looking as command line options
**              as keyword value pairs.  Note that the structure value
**              address members must correspond to the valid keywords
**              passed in the configuration structure. That means be
**              in the same order.  Options that take no argument, i.e.
**              the lowercase options, will cause its associated data
**              member to be set to IMS_TRUE if seen.  Otherwise, it
**              will remain the same (which should be IMS_FALSE).
**
**              2/7/95 - S. Hardman - R1B
**              Cleaned up this file and changed ims_closeLookup() to
**              be a void function.
**
** Notes:       All this thing does in initialize the hash table for
**              commands, and provides an interface for recovering them.
**
******************************************************************************/

#include  <stdio.h>
#include  <ctype.h>
#include  <string.h>
#include  <stdlib.h>

#include  <ims_const.h>
#include  <ims_msg.h>
#include  <ims_cmd.h>
#include  <ims_hash.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_confSubs.h.
** They are listed here for reference.
**
**	int ims_openLookup (IMS_HASH_STRUCT **, IMS_CMD_CLASS [], unsigned int,
**		IMS_MSG_STRUCT *);
**	char *ims_lookup (IMS_HASH_STRUCT *, char *, char *, IMS_MSG_STRUCT *);
**	void ims_closeLookup (IMS_HASH_STRUCT *, IMS_MSG_STRUCT *);
*/

/******************************************************************************
**
** ims_openLookup ()
**
** Initialize hash table with valid keys/values passed in an array.
**
******************************************************************************/

int ims_openLookup (
	IMS_HASH_STRUCT **hashPtr,
	IMS_CMD_CLASS validConfigList[],
	unsigned int count,
	IMS_MSG_STRUCT *msgDesc)
{
	register i;
	IMS_HASH_ENTRY item;

	if ((*hashPtr = ims_hashCreate (count, IMS_STRING, msgDesc)) ==
		(IMS_HASH_STRUCT *)NULL)
	{
		(void) ims_msg (msgDesc, ims_msgGetSeverity (msgDesc),
			"Could not create hash table.");
		return (ims_msgGetSeverity (msgDesc));
	}

	/* 
	** Initialize the hash table :
	** Hash table keys are initialized with valid parameter keyword names.
	** Hash table data is initialized with a pointer to the valid
	** parameter element.
	*/
	for (i = 0; i < count; i++)
	{
		item.key = validConfigList[i].paramKeyword;
		item.data = (char *) &(validConfigList[i]);

		if (ims_hashSearch (*hashPtr, &item, IMS_ENTER, msgDesc)
			== (IMS_HASH_ENTRY *) NULL)
		{
			(void) ims_msg (msgDesc, ims_msgGetSeverity (msgDesc),
				"ims_hashSearch for '%s' failed at IMS_ENTER.",
				(char *) item.key);
			return (ims_msgGetSeverity (msgDesc));
		}
	}

	return(IMS_OK);
}

/******************************************************************************
**
** ims_lookup ()
**
** Search for the keyword and add a value to the
** data portion.  This will force the config/cmd
** structure to contain the value.
**
******************************************************************************/

char *ims_lookup (
	IMS_HASH_STRUCT *hashPtr,
	char *keyword,
	char *value,
	IMS_MSG_STRUCT *msgDesc)
{
	IMS_HASH_ENTRY item, *found_item;
	char *p;
	IMS_CMD_CLASS *configPtr;

	/*
	** Set keyword interface for hash routines.
	*/
	item.key = keyword;

	/*
	** Search the parameter name just read in the hash table.
	** If the parameter is not in the hash table then it is
	** an invalid parameter name that will be IGNORED.
	*/
	if ((found_item = ims_hashSearch(hashPtr, &item, IMS_FIND, msgDesc)) 
		== (IMS_HASH_ENTRY *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Keyword '%s' does not exist.", (char *) item.key);
		return ((char *) NULL);
	}

	/*
	** Get pointer to parameter, return error if it has already been
	** set.
	*/
	configPtr = (IMS_CMD_CLASS *) found_item->data;
	if (*configPtr->value != (char *) NULL)
	{
#ifdef DUPLICATE_VALUE
		/* We've already seen this value, so return an error. */
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Attempt to insert duplicate value for option '%s'.",
			keyword);
		return ((char *) NULL);
#else
		(void) free ((char *) *configPtr->value);
		*configPtr->value = (char *) NULL;
#endif	/* DUPLICATE_VALUE */
	}

	/*
	** Now that the value has been found, malloc some space for it
	** and set this entries pointer to it.
	*/
	if ((p = malloc (strlen (value) + 1)) == (char *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for the value found.");
		return ((char *) NULL);
	}
	/*
	** Now, save the value and force our structure to point to it.
	*/
	(void) strcpy (p, value);
	*configPtr->value = p;

	return ((char *) found_item->data);
}

/******************************************************************************
**
** ims_closeLookup ()
**
** Free data structures associated with the table lookup functions.
**
******************************************************************************/

void ims_closeLookup (
	IMS_HASH_STRUCT *hashPtr,
	IMS_MSG_STRUCT *msgDesc)
{
	/*
	** Actually, just destroy our hash table and return to the
	** user.  The users data structure, which should be a command
	** or a configuration structure, will be used to access any
	** configuration values.
	*/
	ims_hashDestroy (hashPtr, msgDesc);
	return;
}
