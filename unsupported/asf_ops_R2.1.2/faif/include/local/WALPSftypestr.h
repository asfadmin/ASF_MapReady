/*************************************************************************
 * Copyright (c)1995, 1996 California Institute of Technology.           *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
 
/*==============================================================================
Filename:	WALPSftypestr.h

Description:	
	This module contains the definition of WALPS_FileType_Table.
Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		
1.  Oct. '96 - RH
    Update for ASF/WFF ISD Version 2.3

SCCS Info:
   @(#)WALPSftypestr.h	1.1
==============================================================================*/

#ifndef _WALPSFTYPESTR_
#define _WALPSFTYPESTR_

#include "WALPS.h"

File_Identifier WALPS_FileTypeStr_Table[] =
{
   { WALPS_RES,  WALPS_RES_STR },
   { WALPS_WOS,  WALPS_WOS_STR },
   { WALPS_EPH,  WALPS_EPH_STR },
   { WALPS_DNL,  WALPS_DNL_STR },
   { WALPS_MSH,  WALPS_MSH_STR },
   { WALPS_REQ,  WALPS_REQ_STR },
   { WALPS_TPR,  WALPS_TPR_STR },
   { SENTINEL,   NULL          }
} ;

#endif /* _WALPSFTYPESTR_ */

/* End of File */
