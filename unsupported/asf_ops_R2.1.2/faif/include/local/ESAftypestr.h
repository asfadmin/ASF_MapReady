/*************************************************************************
 * Copyright (c)1995, 1996 California Institute of Technology.           *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
 
/*==============================================================================
Filename:	ESAftypestr.h

Description:	
	This module contains the definition of ESA_FileType_Table.
Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		
1.  R. Hoffman - Sept. 96
    Added SHAQP

SCCS Info:
   @(#)ESAftypestr.h	1.1
==============================================================================*/

#ifndef _ESAFTYPESTR_
#define _ESAFTYPESTR_

#include "ESA.h"

File_Identifier ESA_FileTypeStr_Table[] =
{
   { ESA_ORPD,  ESA_ORPD_STR },
   { ESA_ORRE,  ESA_ORRE_STR },
   { ESA_ODMC,  ESA_ODMC_STR },
   { ESA_ODMR,  ESA_ODMR_STR },
   { ESA_SHAQ,  ESA_SHAQ_STR },
   { ESA_RQST,  ESA_RQST_STR },
   { ESA_PATC,  ESA_PATC_STR },
   { ESA_RQVR,  ESA_RQVR_STR },
   { ESA_MPSG,  ESA_MPSG_STR },
   { ESA_SHAQP, ESA_SHAQP_STR},
   { ESA_RQUS,  ESA_RQUS_STR },
   { ESA_REAQ,  ESA_REAQ_STR },
   { ESA_RESM,  ESA_RESM_STR },
   { ESA_REEX,  ESA_REEX_STR },
   { ESA_REUG,  ESA_REUG_STR },
   { SENTINEL,  NULL         }
} ;

#endif /* _ESAFTYPESTR_ */

/* End of File */
