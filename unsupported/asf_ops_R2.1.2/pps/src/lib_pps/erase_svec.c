/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	erase_svec.c

Description:
	This module erases the state vector information of various requests.

External Functions:
	scanreq_erase_svec
	l1req_erase_svec
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)erase_svec.c	1.1    11/21/96";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "PPSdefs.h"
#include "PPShdr.h"


/*==============================================================================
Function:	scanreq_erase_svec
Description:	Erase the state vector info of the scan request
Parameters: 	rec		scan request record
Returns:	None
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
void scanreq_erase_svec(IMS_ScanReq_Record *rec)
#else
void scanreq_erase_svec( rec )
	IMS_ScanReq_Record *rec;
#endif
{
	if (!rec)
		return;

	memset(rec->sv.precision, 0, SVEC_TYPE_STRLEN+1);
	memset(rec->sv.coord_sys, 0, COORD_SYS_STRLEN+1);
	memset(rec->sv.platform, 0, PLATFORM_STRLEN+1);
	rec->sv.rev = 0;
	rec->sv.x_pos = rec->sv.y_pos = rec->sv.z_pos = 0;
	rec->sv.x_vel = rec->sv.y_vel = rec->sv.z_vel = 0;
}


/*==============================================================================
Function:	l1req_erase_svec
Description:	Erase the state vector info of the L1 product request
Parameters: 	rec		L1 product request record
Returns:	None
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
void l1req_erase_svec(IMS_L1PReq_Record *rec)
#else
void l1req_erase_svec( rec )
	IMS_L1PReq_Record *rec;
#endif
{
	if (!rec)
		return;

	memset(rec->sv.precision, 0, SVEC_TYPE_STRLEN+1);
	memset(rec->sv.coord_sys, 0, COORD_SYS_STRLEN+1);
	memset(rec->sv.platform, 0, PLATFORM_STRLEN+1);
	rec->sv.rev = 0;
	rec->sv.x_pos = rec->sv.y_pos = rec->sv.z_pos = 0;
	rec->sv.x_vel = rec->sv.y_vel = rec->sv.z_vel = 0;
}


/*==============================================================================
Function:	l1req_erase_tape
Description:	Erase the tape info of the L1 product request
Parameters: 	rec		L1 product request record
Returns:	None
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
void l1req_erase_tape(IMS_L1PReq_Record *rec)
#else
void l1req_erase_tape( rec )
	IMS_L1PReq_Record *rec;
#endif
{
	if (!rec)
		return;
	rec->media_id[0] = '\0';
	rec->media_type[0] = '\0';
	rec->media_location[0] = '\0';
}


/* End of File */
