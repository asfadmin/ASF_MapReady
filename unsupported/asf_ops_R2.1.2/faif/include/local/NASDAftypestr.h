/*==============================================================================
Filename:	NASDAftypestr.h

Description:	
	This module contains the definition of NASDA_FileType_Table.
Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _NASDAFTYPESTR_
#define _NASDAFTYPESTR_

#include "NASDA.h"

File_Identifier NASDA_FileTypeStr_Table[] =
{
   { NASDA_ELMF, NASDA_ELMF_STR },
   { NASDA_OPLN, NASDA_OPLN_STR },
   { NASDA_REQA, NASDA_REQA_STR },
   { NASDA_REQM, NASDA_REQM_STR },
   { NASDA_MSGC, NASDA_MSGC_STR },
   { NASDA_MSGN, NASDA_MSGN_STR },
   { NASDA_REQQ, NASDA_REQQ_STR },
   { NASDA_REQW, NASDA_REQW_STR },
   { NASDA_REAC, NASDA_REAC_STR },
   { NASDA_CATA, NASDA_CATA_STR },
   { NASDA_MSGM, NASDA_MSGM_STR },
   { NASDA_MSGF, NASDA_MSGF_STR },
   { NASDA_MSGE, NASDA_MSGE_STR },
   { SENTINEL,   NULL           }
} ;

#endif /* _NASDAFTYPESTR_ */

/* End of File */
