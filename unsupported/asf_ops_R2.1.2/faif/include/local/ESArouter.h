/*************************************************************************
 * Copyright (c)1995, 1996 California Institute of Technology.           *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
 
/*==============================================================================
Filename:	ESArouter.h

Description:	
	Contains tables of route config variables and default config
settings for ESA.  Includes names of source, reception, translation and 
destination directory names.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		
1.  R. Hoffman - Sept. 96
    Added SHAQP
2.  April '97 - RAN, RH
    Enabled SHAQP_DDIR to be different from SHAQ_DDIR

SCCS Info:
   @(#)ESArouter.h	1.2
==============================================================================*/

#ifndef _ESAROUTER_
#define _ESAROUTER_

#include "ESA.h"

/* Note actual is rootpath + below
-- ie. rootpath is /LOCAL, ESA_ORPD_RDIR_DEF is ESA
--     actual recept dir is /LOCAL/ESA
*/
#define ESA_ORPD_RDIR_DEF  "ESA"
#define ESA_ORRE_RDIR_DEF  "ESA"
#define ESA_ODMC_RDIR_DEF  "ESA"
#define ESA_ODMR_RDIR_DEF  "ESA"
#define ESA_SHAQ_RDIR_DEF  "ESA"
#define ESA_RQST_RDIR_DEF  "ESA"
#define ESA_PATC_RDIR_DEF  "ESA"
#define ESA_RQVR_RDIR_DEF  "ESA"
#define ESA_MPSG_RDIR_DEF  "ESA"
#define ESA_SHAQP_RDIR_DEF "ESA"

/* Destination directories for ESA files
*/
#define ESA_ORPD_DDIR_DEF  ""
#define ESA_ORRE_DDIR_DEF  ""
#define ESA_ODMC_DDIR_DEF  ""
#define ESA_ODMR_DDIR_DEF  ""
#define ESA_SHAQ_DDIR_DEF  ""
#define ESA_RQST_DDIR_DEF  ""
#define ESA_PATC_DDIR_DEF  ""
#define ESA_RQVR_DDIR_DEF  ""
#define ESA_MPSG_DDIR_DEF  ""
#define ESA_SHAQP_DDIR_DEF  ""
 
/* Translation directories for ESA files
*/
#define ESA_ORPD_TDIR_DEF  ""
#define ESA_ORRE_TDIR_DEF  ""
#define ESA_ODMC_TDIR_DEF  ""
#define ESA_ODMR_TDIR_DEF  ""
#define ESA_SHAQ_TDIR_DEF  ""
#define ESA_RQST_TDIR_DEF  ""
#define ESA_PATC_TDIR_DEF  ""
#define ESA_RQVR_TDIR_DEF  ""
#define ESA_MPSG_TDIR_DEF  ""
#define ESA_SHAQP_TDIR_DEF  ""

/* Table of ESA Reception Directory names
*/
Names_Table_Entry ESA_ReceptDir_Table[] =
{
   { ESA_ORPD,  "ESA_ORPD_RDIR", ESA_ORPD_RDIR_DEF },
   { ESA_ORRE,  "ESA_ORRE_RDIR", ESA_ORRE_RDIR_DEF },
   { ESA_ODMC,  "ESA_ODMC_RDIR", ESA_ODMC_RDIR_DEF },
   { ESA_ODMR,  "ESA_ODMR_RDIR", ESA_ODMR_RDIR_DEF },
   { ESA_SHAQ,  "ESA_SHAQ_RDIR", ESA_SHAQ_RDIR_DEF },
   { ESA_RQST,  "ESA_RQST_RDIR", ESA_RQST_RDIR_DEF },
   { ESA_PATC,  "ESA_PATC_RDIR", ESA_PATC_RDIR_DEF },
   { ESA_RQVR,  "ESA_RQVR_RDIR", ESA_RQVR_RDIR_DEF },
   { ESA_MPSG,  "ESA_MPSG_RDIR", ESA_MPSG_RDIR_DEF },
   { ESA_SHAQP, "ESA_SHAQ_RDIR", ESA_SHAQ_RDIR_DEF },   /* same as SHAQ */
   { SENTINEL,   NULL,           NULL              }
} ;

/* Table of ESA Destination Directory names
*/
Names_Table_Entry ESA_DestDir_Table[] =
{
   { ESA_ORPD,  "ESA_ORPD_DDIR", ESA_ORPD_DDIR_DEF },
   { ESA_ORRE,  "ESA_ORRE_DDIR", ESA_ORRE_DDIR_DEF },
   { ESA_ODMC,  "ESA_ODMC_DDIR", ESA_ODMC_DDIR_DEF },
   { ESA_ODMR,  "ESA_ODMR_DDIR", ESA_ODMR_DDIR_DEF },
   { ESA_SHAQ,  "ESA_SHAQ_DDIR", ESA_SHAQ_DDIR_DEF },
   { ESA_RQST,  "ESA_RQST_DDIR", ESA_RQST_DDIR_DEF },
   { ESA_PATC,  "ESA_PATC_DDIR", ESA_PATC_DDIR_DEF },
   { ESA_RQVR,  "ESA_RQVR_DDIR", ESA_RQVR_DDIR_DEF },
   { ESA_MPSG,  "ESA_MPSG_DDIR", ESA_MPSG_DDIR_DEF },
   { ESA_SHAQP, "ESA_SHAQP_DDIR", ESA_SHAQP_DDIR_DEF },
   { SENTINEL,   NULL,           NULL              }
} ;
 
/* Table of ESA Translated Files Directory names
*/
Names_Table_Entry ESA_TransDir_Table[] =
{
   { ESA_ORPD,  "ESA_ORPD_TDIR", ESA_ORPD_TDIR_DEF },
   { ESA_ORRE,  "ESA_ORRE_TDIR", ESA_ORRE_TDIR_DEF },
   { ESA_ODMC,  "ESA_ODMC_TDIR", ESA_ODMC_TDIR_DEF },
   { ESA_ODMR,  "ESA_ODMR_TDIR", ESA_ODMR_TDIR_DEF },
   { ESA_SHAQ,  "ESA_SHAQ_TDIR", ESA_SHAQ_TDIR_DEF },
   { ESA_RQST,  "ESA_RQST_TDIR", ESA_RQST_TDIR_DEF },
   { ESA_PATC,  "ESA_PATC_TDIR", ESA_PATC_TDIR_DEF },
   { ESA_RQVR,  "ESA_RQVR_TDIR", ESA_RQVR_TDIR_DEF },
   { ESA_MPSG,  "ESA_MPSG_TDIR", ESA_MPSG_TDIR_DEF },
   { ESA_SHAQP, "ESA_SHAQ_TDIR", ESA_SHAQ_TDIR_DEF },  /* same as SHAQ */
   { SENTINEL,   NULL,           NULL              }
} ;

#endif  /* _ESAROUTER_ */

/* End of file */
