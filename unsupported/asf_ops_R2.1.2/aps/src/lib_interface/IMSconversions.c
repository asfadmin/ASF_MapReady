#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		IMSconversions.c

Description:	contains code for manipulating ims/aps translation tables

External Functions Defined:
	
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
#pragma ident	"@(#)IMSconversions.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_interface/SCCS/s.IMSconversions.c"

#include <stdio.h>
#include <string.h>

#include "DARconversions.h"     /* for IMS_XLAT_TBL typedef   */

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

/*
--
--   USE THESE ROUTINES (located in DARconversions.c) to translate 
--   using this table.  Both routines 
--	 return the result pointer in the return variable.  If you want 
--	 a string copy into your previously allocated storage, use a 
--	 non-NULL in the output string parameter, which is the 2nd parameter:
--
--   	char *table_lookupIMS2APS( 
--			IMS_XLAT_TBL *xlat_table, 
--			char *imsString, 
-- 			char **apsString )
--
--		char *table_lookupAPS2IMS(
--			IMS_XLAT_TBL *xlat_table, 
-- 			char *apsString, 
-- 			char **imsString )
--
*/

IMS_XLAT_TBL ims_station_id_table[] =
	{	{ "FA",	"ASF" },
		{ "MC",	"MCM" },
		{ NULL,	NULL }
	} ;

/* used in the frame generator interface:  */
IMS_XLAT_TBL ims_dtk_status_table[] =
	{	{ "PLANNED",	"PLN" },
		{ "SCHEDULED",	"SCH" },
		{ "ACQUIRED",	"BIG ERROR" },  /* not used in IMS-APS interface  */
		{ "REJECTED",	"DEL" },  
		{ "REJECTED",	"REJ" },  
		{ NULL,	NULL }
	} ;


/*==============================================================================
Function:       fa_table_lookupAPS2

Description:    using the fa tables, translate APS string value to the
				equivalent IMS/FA string value.

Parameters:     xlat_table	a pointer to the xlation table

				apsString	pointer to a pointer to the equivalent APS string

				faString	a pointer to the IMS/FA string value
							(or NULL, if return value is NULL)

Returns:        the IMS/FA string value
				NULL if the IMS/FA string value is not in the table
							or an input parameter is NULL

Creator:        Lawrence Stevens, by copying Teresa McKillop's 
				fa_table_lookup2APS()

Creation Date:  Tue Feb 27 15:43:52 PST 1996

Notes:			
==============================================================================*/
char *
fa_table_lookupAPS2(
	EQUIV_TABLE *xlat_table, 
	char *apsString, 
	char **faString  )
{
	char	*retStr = NULL;
	int		i;

	if (xlat_table != NULL && apsString != NULL)
	{
		for (i = 0 ; xlat_table[i].aps_string != NULL ; i++)
		{
			if (strncmp( xlat_table[i].aps_string, apsString, 
                                    strlen(xlat_table[i].aps_string) ) == 0)
			{
				retStr = xlat_table[i].fa_string;
				break;
			}
		}
	}

	if (faString != NULL)
		*faString = retStr;

	return (retStr);
}
