#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	DARconversions.h
Description:	contains tables/macros needed to transform ims dars into
				aps dars
Creator:	Teresa McKillop
Notes:		
==============================================================================*/
#pragma ident	"@(#)DARconversions.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/local/SCCS/s.DARconversions.h"


#ifndef DARCONVERSIONS_H
#define DARCONVERSIONS_H

#include "fa_defs.h"		/* needed for EQUIV_TABLE */

/*==============================================================================
--	Constants/Macros
==============================================================================*/
/* IMS DAR status values */
#define IMS_DB_NEW_STATUS		"VALIDATED"
#define IMS_DB_IN_PLN_STATUS	"IN-PLANNING"
#define IMS_DB_COMPLETE_STATUS	"COMPLETED"
#define IMS_DB_REJECTED_STATUS	"REJECTED"

#define	APS_QUERY_STAT_TYPE		IMS_DB_NEW_STATUS
#define DEF_IMS_DAR_ITEM_ID		1		/* this is the sole value for IMS DARs*/

/* APS DAR status values */
#define APS_QUEUED_STATUS		"QUE"
#define APS_PLANNED_STATUS		"PLN"
#define APS_REJECTED_STATUS		"REJ"
#define APS_COMPLETED_STATUS	"COM"

/*==============================================================================
--	Translation Tables
==============================================================================*/

typedef struct IMS_XLAT_TBL
{
	char	*ims_value;
	char	*aps_value;
} IMS_XLAT_TBL;

extern IMS_XLAT_TBL		dar_status[];

/*==============================================================================
--	Function Prototypes
==============================================================================*/

extern char *table_lookupIMS2APS(
	IMS_XLAT_TBL *xlat_table, char *imsString, char **apsString );
extern char *table_lookupAPS2IMS(
	IMS_XLAT_TBL *xlat_table, char *apsString, char **imsString );
extern char *fa_table_lookup2APS(
	EQUIV_TABLE *xlat_table, char *faString, char **apsString );

#endif	/* DARCONVERSIONS_H */
