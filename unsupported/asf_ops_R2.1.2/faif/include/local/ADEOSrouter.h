/*==============================================================================
Filename:	ADEOSrouter.h

Description:	
	Contains default route config settings for ADEOS.  Includes names 
of source, reception, translation and destination directory names.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   @(#)ADEOSrouter.h	1.2
==============================================================================*/

#ifndef _ADEOSROUTER_
#define _ADEOSROUTER_

#include "ADEOS.h"

/* Note actual is rootpath + below
-- ie. rootpath is /local/faif, ADEOS_ARESP_RDIR_DEF is ADEOS
--     actual recept dir is /local/faif/ADEOS
*/
#define ADEOS_REQR_RDIR_DEF "ADEOS"
#define ADEOS_RDRD_RDIR_DEF "ADEOS"
#define ADEOS_ELMD_RDIR_DEF "ADEOS"
#define ADEOS_ELMP_RDIR_DEF "ADEOS"
#define ADEOS_OPL1_RDIR_DEF "ADEOS"
#define ADEOS_RPLN_RDIR_DEF "ADEOS"
#define ADEOS_ORST_RDIR_DEF "ADEOS"
#define ADEOS_STAD_RDIR_DEF "ADEOS"
#define ADEOS_TMDF_RDIR_DEF "ADEOS"

/* Default Destination directories
*/
#define ADEOS_REQR_DDIR_DEF ""
#define ADEOS_RDRD_DDIR_DEF ""
#define ADEOS_ELMD_DDIR_DEF ""
#define ADEOS_ELMP_DDIR_DEF ""
#define ADEOS_OPL1_DDIR_DEF ""
#define ADEOS_RPLN_DDIR_DEF ""
#define ADEOS_ORST_DDIR_DEF ""
#define ADEOS_STAD_DDIR_DEF ""
#define ADEOS_TMDF_DDIR_DEF ""

/* Default Translation directories
*/
#define ADEOS_ELMD_TDIR_DEF "/adeos_archive/state_vectors/restituted"
#define ADEOS_ELMP_TDIR_DEF "/adeos_archive/state_vectors/predicted"


/* Table of ADEOS Reception Directory names
*/
Names_Table_Entry ADEOS_ReceptDir_Table[] =
{
   { ADEOS_REQR, "ADEOS_REQR_RDIR", ADEOS_REQR_RDIR_DEF },
   { ADEOS_RDRD, "ADEOS_RDRD_RDIR", ADEOS_RDRD_RDIR_DEF },
   { ADEOS_ELMD, "ADEOS_ELMD_RDIR", ADEOS_ELMD_RDIR_DEF },
   { ADEOS_ELMP, "ADEOS_ELMP_RDIR", ADEOS_ELMP_RDIR_DEF },
   { ADEOS_OPL1, "ADEOS_OPL1_RDIR", ADEOS_OPL1_RDIR_DEF },
   { ADEOS_RPLN, "ADEOS_RPLN_RDIR", ADEOS_RPLN_RDIR_DEF },
   { ADEOS_ORST, "ADEOS_ORST_RDIR", ADEOS_ORST_RDIR_DEF },
   { ADEOS_STAD, "ADEOS_STAD_RDIR", ADEOS_STAD_RDIR_DEF },
   { ADEOS_TMDF, "ADEOS_TMDF_RDIR", ADEOS_TMDF_RDIR_DEF },
   { SENTINEL,    NULL,             NULL                }
} ;

/* Table of ADEOS Destination Directory names
*/
Names_Table_Entry ADEOS_DestDir_Table[] =
{
   { ADEOS_REQR, "ADEOS_REQR_DDIR", ADEOS_REQR_DDIR_DEF },
   { ADEOS_RDRD, "ADEOS_RDRD_DDIR", ADEOS_RDRD_DDIR_DEF },
   { ADEOS_ELMD, "ADEOS_ELMD_DDIR", ADEOS_ELMD_DDIR_DEF },
   { ADEOS_ELMP, "ADEOS_ELMP_DDIR", ADEOS_ELMP_DDIR_DEF },
   { ADEOS_OPL1, "ADEOS_OPL1_DDIR", ADEOS_OPL1_DDIR_DEF },
   { ADEOS_RPLN, "ADEOS_RPLN_DDIR", ADEOS_RPLN_DDIR_DEF },
   { ADEOS_ORST, "ADEOS_ORST_DDIR", ADEOS_ORST_DDIR_DEF },
   { ADEOS_STAD, "ADEOS_STAD_DDIR", ADEOS_STAD_DDIR_DEF },
   { ADEOS_TMDF, "ADEOS_TMDF_DDIR", ADEOS_TMDF_DDIR_DEF },
   { SENTINEL,    NULL,             NULL                }
} ;

/* Table of ADEOS Translation Directory names
*/
Names_Table_Entry ADEOS_TransDir_Table[] =
{
   { ADEOS_ELMD, "ADEOS_ELMD_TDIR", ADEOS_ELMD_TDIR_DEF },
   { ADEOS_ELMP, "ADEOS_ELMP_TDIR", ADEOS_ELMP_TDIR_DEF },
   { SENTINEL,    NULL,             NULL                }
} ;

#endif /* _ADEOSROUTER_ */

/* End of File */
