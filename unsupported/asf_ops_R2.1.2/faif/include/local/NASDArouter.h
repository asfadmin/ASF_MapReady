/*==============================================================================
Filename:	NASDArouter.h

Description:	
	Contains default route config settings for NASDA.  Includes names 
of source, reception, translation and destination directory names.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _NASDAROUTER_ 
#define _NASDAROUTER_

#include "NASDA.h"

/* Note actual is rootpath + below
-- ie. rootpath is /local/faif, NASDA_ELMF_RDIR_DEF is NASDA
--     actual recept dir is /local/faif/NASDA
*/
#define NASDA_ELMF_RDIR_DEF "NASDA"
#define NASDA_OPLN_RDIR_DEF "NASDA"
#define NASDA_REQA_RDIR_DEF "NASDA"
#define NASDA_REQM_RDIR_DEF "NASDA"
#define NASDA_MSGC_RDIR_DEF "NASDA"
#define NASDA_MSGN_RDIR_DEF "NASDA"

/* Destination directories
*/
#define NASDA_ELMF_DDIR_DEF ""
#define NASDA_OPLN_DDIR_DEF ""
#define NASDA_REQA_DDIR_DEF ""
#define NASDA_REQM_DDIR_DEF ""
#define NASDA_MSGC_DDIR_DEF ""
#define NASDA_MSGN_DDIR_DEF ""
 
/* Translation directories
*/
#define NASDA_ELMF_TDIR_DEF ""
#define NASDA_OPLN_TDIR_DEF ""
#define NASDA_REQA_TDIR_DEF ""
#define NASDA_REQM_TDIR_DEF ""
#define NASDA_MSGC_TDIR_DEF ""
#define NASDA_MSGN_TDIR_DEF ""


/* Table of NASDA Reception Directory names
*/
Names_Table_Entry NASDA_ReceptDir_Table[] =
{
   { NASDA_ELMF, "NASDA_ELMF_RDIR", NASDA_ELMF_RDIR_DEF },
   { NASDA_OPLN, "NASDA_OPLN_RDIR", NASDA_OPLN_RDIR_DEF },
   { NASDA_REQA, "NASDA_REQA_RDIR", NASDA_REQA_RDIR_DEF },
   { NASDA_REQM, "NASDA_REQM_RDIR", NASDA_REQM_RDIR_DEF },
   { NASDA_MSGC, "NASDA_MSGC_RDIR", NASDA_MSGC_RDIR_DEF },
   { NASDA_MSGN, "NASDA_MSGN_RDIR", NASDA_MSGN_RDIR_DEF },
   { SENTINEL,    NULL,             NULL                }
} ;


/* Table of NASDA Destination Directory names
*/
Names_Table_Entry NASDA_DestDir_Table[] =
{
   { NASDA_ELMF, "NASDA_ELMF_DDIR", NASDA_ELMF_DDIR_DEF },
   { NASDA_OPLN, "NASDA_OPLN_DDIR", NASDA_OPLN_DDIR_DEF },
   { NASDA_REQA, "NASDA_REQA_DDIR", NASDA_REQA_DDIR_DEF },
   { NASDA_REQM, "NASDA_REQM_DDIR", NASDA_REQM_DDIR_DEF },
   { NASDA_MSGC, "NASDA_MSGC_DDIR", NASDA_MSGC_DDIR_DEF },
   { NASDA_MSGN, "NASDA_MSGN_DDIR", NASDA_MSGN_DDIR_DEF },
   { SENTINEL,    NULL,             NULL                }
} ;
 
/* Table of NASDA Translated Files Directory names
*/
Names_Table_Entry NASDA_TransDir_Table[] =
{
   { NASDA_ELMF, "NASDA_ELMF_TDIR", NASDA_ELMF_TDIR_DEF },
   { NASDA_OPLN, "NASDA_OPLN_TDIR", NASDA_OPLN_TDIR_DEF },
   { NASDA_REQA, "NASDA_REQA_TDIR", NASDA_REQA_TDIR_DEF },
   { NASDA_REQM, "NASDA_REQM_TDIR", NASDA_REQM_TDIR_DEF },
   { NASDA_MSGC, "NASDA_MSGC_TDIR", NASDA_MSGC_TDIR_DEF },
   { NASDA_MSGN, "NASDA_MSGN_TDIR", NASDA_MSGN_TDIR_DEF },
   { SENTINEL,    NULL,             NULL                }
} ;

#endif /* _NASDAROUTER_ */

/* End of File */
