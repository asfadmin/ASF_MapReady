static char *sccs = "@(#)ims_v0Sort.c	1.2 12/19/96";
/******************************************************************************
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
**
** File:	ims_v0Sort.c
**
** Function: sort the granules list for the V0 server.
**
** Author: Dan Crichton	
**
** Date:	9/12/96
**
**
******************************************************************************/

#undef _POSIX_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>

#include <ims_dbms.h>
#include <ims_query.h> 
#include <ims_getInput.h>
#include <ims_util.h>
#include <syslog.h>

#include <IK_Network.h>
#include <odlinter.h>
#include <ims_v0.h>


static int insertOrderedLink(V0_GRANULE_LIST **, V0_GRANULE_LIST *);
static int v0Compare(V0_GRANULE_LIST *item1, V0_GRANULE_LIST *item2);

/****************************************************************************
**
** ims_v0SortList
** 
** ims_sortV0List will free the original list for the caller.
**
****************************************************************************/

int ims_v0SortList(
	IMS_MSG_STRUCT *msgDesc,
	V0_GRANULE_LIST *v0ListIn,
	V0_GRANULE_LIST **v0ListOut)
{
	V0_GRANULE_LIST *tmpItem = NULL;

	/*
	**
	** Perform insertion sort on orginal list to new list.
	**
	**/

	tmpItem = v0ListIn;	
	*v0ListOut = (V0_GRANULE_LIST *)NULL;


	while (tmpItem != NULL)
	{
		if (insertOrderedLink(v0ListOut, tmpItem) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"v0Sort of granules failed to insert link.");
			return(IMS_ERROR);
		}
		tmpItem = tmpItem->next_p;
		
	}

	return(IMS_OK);
}


/****************************************************************************
**
** insertOrderedLink
**
****************************************************************************/
static int insertOrderedLink(V0_GRANULE_LIST **v0ListIn, V0_GRANULE_LIST *item)
{
	V0_GRANULE_LIST *newItem = NULL, *oldPtr;
	V0_GRANULE_LIST *v0List, *saveList;

	saveList = v0List = *v0ListIn;

	oldPtr = NULL;

	while (v0List != NULL)
	{
		if (v0Compare(v0List, item) > 0)
		{
			break;
		}

		oldPtr = v0List;
		v0List = v0List->next_p;
	}

	/*
	** Did not find an insertion point, add to tail of list, or head if empty.
	*/

	newItem = malloc(sizeof(V0_GRANULE_LIST));

	if (newItem == NULL)
	{
		return(IMS_ERROR);
	}

	memcpy(newItem, item, sizeof(V0_GRANULE_LIST));

	if (oldPtr != NULL)
	{
		newItem->next_p = oldPtr->next_p;
		oldPtr->next_p = newItem;

		/*
		** Reset the head of the list back to the orignal head since
		** it hasn't changed.
		*/

		v0List = saveList;
		
	}
	else
	{
		/*
		** If the list doesn't exist, then add it as the new head.
		*/

		if (v0List == NULL)
		{
			v0List = newItem;
			v0List->next_p = NULL;
		}
		else
		{
			/*
			** A list exists, but this is the new head.
			*/
			newItem->next_p = v0List;
			v0List = newItem;
		}
	}

	/*
	** Pass back the new list and return.
	*/

	*v0ListIn = v0List;
	return(IMS_OK);
}

/****************************************************************************
**
** v0Compare
**
** Returns: 
**		< 0 : item1 is less than item2
**		= 0 : item1 equals item2
**		> 0 : item1 is greater than item2
****************************************************************************/
static int v0Compare(V0_GRANULE_LIST *item1, V0_GRANULE_LIST *item2)
{
	return(strcmp(item1->start_time, item2->start_time));
}
