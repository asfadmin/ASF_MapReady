#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		DARconversions.c
				(formerly src/gui/aps_ims_dar_tables.c)

Description:	contains code for manipulating ims/aps translation tables

External Functions Defined:
				table_lookupIMS2APS
				table_lookupAPS2IMS
				fa_table_lookup2APS
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			Translation tables must be tables containing these 2
				elements: ims_value and aps_value.  The last element in
				each table must contain NULL values for ims_value and
				aps_value, and no other element may contain NULL for either

				*** WARNING *** (different from the other files in this lib)
				no memory is allocated for the returned values from the
				translation functions; a ptr into the translation table
				is returned, instead.

==============================================================================*/
#pragma ident	"@(#)DARconversions.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_interface/SCCS/s.DARconversions.c"

#include <stdio.h>
#include <string.h>      /* for strcmp()  */

#include "DARconversions.h"

/* needed for fa table lookup */
#include "fa_defs.h"

/*==============================================================================
--	Global Variables

--	Example translation table declaration:
--		IMS_XLAT_TBL dar_satellite[] =
--			{	{ "ERS-1",		"E1" },
--				{ "ERS-2",		"E1" },
--				{ "JERS-1",		"J1" },
--				{ NULL,				NULL }
--			};
==============================================================================*/

IMS_XLAT_TBL dar_status[] =
	{	{ IMS_DB_IN_PLN_STATUS,		APS_QUEUED_STATUS },	/* default values */
		{ IMS_DB_NEW_STATUS,		APS_QUEUED_STATUS },
		{ IMS_DB_IN_PLN_STATUS, 	APS_PLANNED_STATUS },
		{ IMS_DB_REJECTED_STATUS,	APS_REJECTED_STATUS },
		{ IMS_DB_COMPLETE_STATUS,	APS_COMPLETED_STATUS },
		{ NULL,						NULL }
	};
/*	[NOTE:	if xlating from ims to aps: in-planning will return aps_queued,
--				not aps_planned (ie, the first one found);
--			if xlating from aps to ims: aps_queued will retrun in-planning,
--				not new (ie, the first one found).]
*/


/*==============================================================================
Function:       table_lookupIMS2APS

Description:    translate IMS string value to the equivalent APS
				string value

Parameters:     xlat_table	a pointer to the xlation table
				imsString	a pointer to the IMS string value

				apsString	pointer to a pointer to the equivalent APS string
							(or NULL, if return value is NULL)

Returns:        the APS string value
				NULL if the IMS string value is not in the table
							or an input parameter is NULL

Creator:        Teresa McKillop

Creation Date:  01/12/96

Notes:			see NOTES, above, in the file header for a description of
				the xlat_table.
==============================================================================*/
char *
table_lookupIMS2APS(
	IMS_XLAT_TBL *xlat_table, char *imsString, char **apsString )
{
	char	*retStr = NULL;
	int		i;

	if (xlat_table != NULL && imsString != NULL)
	{
		for (i = 0 ; xlat_table[i].ims_value != NULL ; i++)
		{
			if (strcmp( xlat_table[i].ims_value, imsString ) == 0)
			{
				retStr = xlat_table[i].aps_value;
				break;
			}
		}
	}

	if (apsString != NULL)
		*apsString = retStr;

	return (retStr);
}



/*==============================================================================
Function:       table_lookupAPS2IMS

Description:    translate APS string value to the equivalent IMS
				string value

Parameters:     xlat_table	a pointer to the xlation table
				apsString	a pointer to the APS string value

				imsString	pointer to a pointer to the equivalent IMS string
							(or NULL, if return value is NULL)

Returns:        the IMS string value
				NULL if the APS string value is not in the table
							or an input parameter is NULL

Creator:        Teresa McKillop

Creation Date:  01/12/96

Notes:			see NOTES, above, in the file header for a description of
				the xlat_table.
==============================================================================*/
char *
table_lookupAPS2IMS(
	IMS_XLAT_TBL *xlat_table, char *apsString, char **imsString )
{
	char	*retStr = NULL;
	int		i;

	if (xlat_table != NULL && apsString != NULL)
	{
		for (i = 0 ; xlat_table[i].aps_value != NULL ; i++)
		{
			if (strcmp( xlat_table[i].aps_value, apsString ) == 0)
			{
				retStr = xlat_table[i].ims_value;
				break;
			}
		}
	}

	if (imsString != NULL)
		*imsString = retStr;

	return (retStr);
}



/*==============================================================================
Function:       fa_table_lookup2APS

Description:    using the fa tables, translate IMS/FA string value to the
				equivalent APS string value.

Parameters:     xlat_table	a pointer to the xlation table
				faString	a pointer to the IMS/FA string value

				apsString	pointer to a pointer to the equivalent APS string
							(or NULL, if return value is NULL)

Returns:        the APS string value
				NULL if the fa string value is not in the table
							or an input parameter is NULL

Creator:        Teresa McKillop

Creation Date:  01/12/96

Notes:			
==============================================================================*/
char *
fa_table_lookup2APS(
	EQUIV_TABLE *xlat_table, char *faString, char **apsString )
{
	char	*retStr = NULL;
	int		i;

	if (xlat_table != NULL && faString != NULL)
	{
		for (i = 0 ; xlat_table[i].fa_string != NULL ; i++)
		{
			if (strcmp( xlat_table[i].fa_string, faString ) == 0)
			{
				retStr = xlat_table[i].aps_string;
				break;
			}
		}
	}

	if (apsString != NULL)
		*apsString = retStr;

	return (retStr);
}
