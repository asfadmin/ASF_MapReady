#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	IMSconversions.h
Description:	contains tables and routines to transform IMS values to and
				from APS values.  
Creator:	Lawrence Stevens


Notes:		
==============================================================================*/
#pragma ident	"@(#)IMSconversions.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/local/SCCS/s.IMSconversions.h"


#ifndef _IMSCONVERSIONS_H_
#define _IMSCONVERSIONS_H_

#include "fa_defs.h"		  /* needed for EQUIV_TABLE */
#include "DARconversions.h"	  /* needed for IMS_XLAT_TBL */


/*==============================================================================
--	Translation Tables
==============================================================================*/

extern IMS_XLAT_TBL		ims_station_id_table[];
extern IMS_XLAT_TBL		ims_dtk_status_table[];

/*==============================================================================
--	Function Prototypes
==============================================================================*/

extern char *fa_table_lookupAPS2(
	EQUIV_TABLE *xlat_table, char *apsString, char **faString );

#endif	/* _IMSCONVERSIONS_H */
