/*==============================================================================
Filename:	ADEOSftypestr.h

Description:	
	This module contains the definition of ADEOS_FileType_Table.
Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   @(#)ADEOSftypestr.h	1.2
==============================================================================*/

#ifndef _ADEOSFTYPESTR_
#define _ADEOSFTYPESTR_

#include "ADEOS.h"

File_Identifier ADEOS_FileTypeStr_Table[] =
{
   { ADEOS_REQR,  ADEOS_REQR_STR }, 
   { ADEOS_RDRD,  ADEOS_RDRD_STR }, 
   { ADEOS_ELMD,  ADEOS_ELMD_STR },
   { ADEOS_ELMP,  ADEOS_ELMP_STR },
   { ADEOS_OPL1,  ADEOS_OPL1_STR },
   { ADEOS_RPLN,  ADEOS_RPLN_STR },
   { ADEOS_ORST,  ADEOS_ORST_STR },
   { ADEOS_STAD,  ADEOS_STAD_STR },
   { ADEOS_STGS,  ADEOS_STGS_STR },
   { ADEOS_REAC,  ADEOS_REAC_STR },
   { ADEOS_SRRD,  ADEOS_SRRD_STR },
   { ADEOS_TMDF,  ADEOS_TMDF_STR },
   { SENTINEL,    NULL }
} ;

#endif /* _ADEOSFTYPESTR_ */

/* End of File */
