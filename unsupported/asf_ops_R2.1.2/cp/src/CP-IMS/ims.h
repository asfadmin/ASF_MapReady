/*============================================================================
 |  @(#)ims.h	1.2 96/05/23 14:46:41
 |
 |  CP-IMS Interface Module Include File
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *===========================================================================*/
#ifndef _IMS_H_
#define _IMS_H_

#include "ims_query.h"
#include "ims_cmnQuery.h"
#include "odl.h"

static char sccsid_ims_h[] =
	"@(#)ims.h	1.2 96/05/23 14:46:41";

extern  int ims_process(ODL msg, IMS_CMN_QUERY* query, char *errorbuf);

#endif /*!_IMS_H_*/
