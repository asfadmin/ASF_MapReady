/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	process_ims_dub.c

Description:
	This module contains the routines used for processing  IMS messages. 

External Functions:
	process_IMS_DubReq
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)process_ims_dub.c	1.1    11/21/96";

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "defs.h"
#include "PPSdefs.h"
#include "PPSextern.h"
#include "db_sybint.h"
#include "PPSerr.h"

/*==============================================================================
Function:	
	int process_IMS_DubReq(Common_Header_Record *common_hdr, IMS_DubReq_Record *rec)

Description:	
	This function processes an IMS Dub Request.

Parameters:
	Common_Header_Record *common_hdr 	- IMS hdr info
	IMS_DubReq_Record *rec		- extracted data

Returns:
Creator:	Nadia Adhami	
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int process_IMS_DubReq(Common_Header_Record *common_hdr, IMS_DubReq_Record *rec)
#else
int process_IMS_DubReq(common_hdr, rec)
   Common_Header_Record *common_hdr;
   IMS_DubReq_Record *rec)
#endif
{
	printf("process_IMS_DubReq\n");
	return(ER_NO_ERROR);

} /* process_IMS_DubReq */


/* End of File */
