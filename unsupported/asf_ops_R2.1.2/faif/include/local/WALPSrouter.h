/*************************************************************************
 * Copyright (c)1995, 1996 California Institute of Technology.           *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
 
/*==============================================================================
Filename:	WALPSrouter.h

Description:	
	Contains default route config settings for WALPS.  Includes names 
of source, reception, translation and destination directory names.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		
1.  October '96 - RH
    Updated for ISD Version 2.3
2.  July '97 - R. Hoffman
    Get rid of some default values.
    Get rid of outbound WFF file types in WALPS_ReceptDir_Table
SCCS Info:
   @(#)WALPSrouter.h	1.1
==============================================================================*/

#ifndef _WALPSROUTER_
#define _WALPSROUTER_

#include "WALPS.h"

/* Note actual is rootpath + below
-- ie. rootpath is /local/faif, WALPS_RES_RDIR_DEF is WALPS
--     actual recept dir is /local/faif/WALPS
*/
#define WALPS_WOS_RDIR_DEF "WALPS"
#define WALPS_DNL_RDIR_DEF "WALPS"
#define WALPS_EPH_RDIR_DEF "WALPS"
#define WALPS_REQ_RDIR_DEF "WALPS"
#define WALPS_RES_RDIR_DEF "WALPS"
#define WALPS_MSH_RDIR_DEF "WALPS"
#define WALPS_TPR_RDIR_DEF "WALPS"

/* Default Destination directories
*/
#define WALPS_RES_DDIR_DEF  NULL
#define WALPS_DNL_DDIR_DEF  NULL
#define WALPS_MSH_DDIR_DEF  NULL
 
/* Default Translation directories
*/
#define WALPS_RES_TDIR_DEF  ""
#define WALPS_DNL_TDIR_DEF  ""
#define WALPS_MSH_TDIR_DEF  ""


/* Table of WALPS Reception Directory names
*/
Names_Table_Entry WALPS_ReceptDir_Table[] =
{
   { WALPS_RES, "WALPS_RES_RDIR",   WALPS_RES_RDIR_DEF   },
   { WALPS_DNL, "WALPS_DNL_RDIR",   WALPS_DNL_RDIR_DEF   },
   { WALPS_MSH, "WALPS_MSH_RDIR",   WALPS_MSH_RDIR_DEF   },
   { SENTINEL,   NULL,              NULL                 }
} ;


/* Table of WALPS Destination Directory names
*/
Names_Table_Entry WALPS_DestDir_Table[] =
{
   { WALPS_RES,  "WALPS_RES_DDIR",  WALPS_RES_DDIR_DEF },
   { WALPS_DNL,  "WALPS_DNL_DDIR",  WALPS_DNL_DDIR_DEF },
   { WALPS_MSH,  "WALPS_MSH_DDIR",  WALPS_MSH_DDIR_DEF },
   { SENTINEL,    NULL,             NULL               }
} ;
 
/* Table of WALPS Translated Files Directory names
*/
Names_Table_Entry WALPS_TransDir_Table[] =
{
   { SENTINEL,    NULL,             NULL               },
   { WALPS_RES,  "WALPS_RES_TDIR",  WALPS_RES_TDIR_DEF },
   { WALPS_DNL,  "WALPS_DNL_TDIR",  WALPS_DNL_TDIR_DEF },
   { WALPS_MSH,  "WALPS_MSH_TDIR",  WALPS_MSH_TDIR_DEF },
   { SENTINEL,    NULL,             NULL               }
} ;

#endif /* _WALPSROUTER_ */

/* End of File */
