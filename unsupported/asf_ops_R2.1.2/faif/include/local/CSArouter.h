/*==============================================================================
Filename:	CSArouter.h

Description:	
	Contains default route config settings for CSA.  Includes
names of reception, translation and destination directory names per CSA 
file type.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   @(#)CSArouter.h	1.2
==============================================================================*/

#ifndef _CSAROUTER_
#define _CSAROUTER_

#include "CSA.h"

#define CSA_PORB_SDIR_DEF  "mcs_to_ufdroc_1"
#define CSA_DORB_SDIR_DEF  "mcs_to_ufdroc_1"
#define CSA_SARPP_SDIR_DEF "mcs_to_ufdroc_1"
#define CSA_RRQ_SDIR_DEF   "mcs_to_ufdrf_f"
#define CSA_RSH_SDIR_DEF   "mcs_to_ufdrf_f"
#define CSA_CRQ_SDIR_DEF   "mcs_to_ufcal_1"
#define CSA_CSH_SDIR_DEF   "mcs_to_ufcal_1"
#define CSA_RRQ_M_SDIR_DEF "mcs_to_ufdrf_m"
#define CSA_RSH_M_SDIR_DEF "mcs_to_ufdrf_m"
#define CSA_CRQ_M_SDIR_DEF "mcs_to_ufcal_2"
#define CSA_CSH_M_SDIR_DEF "mcs_to_ufcal_2"

#define CSA_PORB_RDIR_DEF  "CSA/CSA_PREDORBIT"
#define CSA_DORB_RDIR_DEF  "CSA/CSA_DEFVORBIT"
#define CSA_SARPP_RDIR_DEF "CSA/CSA_SARPROCPRM"
#define CSA_RRQ_RDIR_DEF   "CSA/CSA_RECRQST/ASF"
#define CSA_RSH_RDIR_DEF   "CSA/CSA_RECSCHED/ASF"
#define CSA_CRQ_RDIR_DEF   "CSA/CSA_CALIBRQST/ASF"
#define CSA_CSH_RDIR_DEF   "CSA/CSA_CALIBSCHED/ASF"
#define CSA_RRQ_M_RDIR_DEF "CSA/CSA_RECRQST/MCMURDO"
#define CSA_RSH_M_RDIR_DEF "CSA/CSA_RECSCHED/MCMURDO"
#define CSA_CRQ_M_RDIR_DEF "CSA/CSA_CALIBRQST/MCMURDO"
#define CSA_CSH_M_RDIR_DEF "CSA/CSA_CALIBSCHED/MCMURDO"

#define CSA_PORB_DDIR_DEF  ""
#define CSA_DORB_DDIR_DEF  ""
#define CSA_SARPP_DDIR_DEF ""
#define CSA_RRQ_DDIR_DEF   "/csa_archive/reception_request_asf"
#define CSA_RSH_DDIR_DEF   "/csa_archive/reception_schedule_asf"
#define CSA_CRQ_DDIR_DEF   "/csa_archive/calibration_request_asf"
#define CSA_CSH_DDIR_DEF   "/csa_archive/calibration_schedule_asf"
#define CSA_RRQ_M_DDIR_DEF "/csa_archive/reception_request_mcm"
#define CSA_RSH_M_DDIR_DEF "/csa_archive/reception_schedule_mcm"
#define CSA_CRQ_M_DDIR_DEF "/csa_archive/calibration_request_mcm"
#define CSA_CSH_M_DDIR_DEF "/csa_archive/calibration_schedule_mcm"

#define CSA_PORB_TDIR_DEF  "/csa_archive/state_vectors/predicted/tran"
#define CSA_DORB_TDIR_DEF  "/csa_archive/state_vectors/restituted/tran"
#define CSA_SARPP_TDIR_DEF ""
#define CSA_RRQ_TDIR_DEF   ""
#define CSA_RSH_TDIR_DEF   ""
#define CSA_CRQ_TDIR_DEF   ""
#define CSA_CSH_TDIR_DEF   ""
#define CSA_RRQ_M_TDIR_DEF ""
#define CSA_RSH_M_TDIR_DEF ""
#define CSA_CRQ_M_TDIR_DEF ""
#define CSA_CSH_M_TDIR_DEF ""


/* Table of CSA Source Directory names
*/
Names_Table_Entry CSA_SrcDir_Table[] =
{
   { CSA_PREDORBIT,  "CSA_PORB_SDIR",   CSA_PORB_SDIR_DEF  },
   { CSA_DEFVORBIT,  "CSA_DORB_SDIR",   CSA_DORB_SDIR_DEF  },
   { CSA_RECRQST,    "CSA_RRQ_SDIR",    CSA_RRQ_SDIR_DEF   },
   { CSA_RECSCHED,   "CSA_RSH_SDIR",    CSA_RSH_SDIR_DEF   },
   { CSA_CALIBRQST,  "CSA_CRQ_SDIR",    CSA_CRQ_SDIR_DEF   },
   { CSA_CALIBSCHED, "CSA_CSH_SDIR",    CSA_CSH_SDIR_DEF   },
   { CSA_SARPROCPRM, "CSA_SARPP_SDIR",  CSA_SARPP_SDIR_DEF },
   { CSA_RRQ_MCM,    "CSA_RRQ_M_SDIR",  CSA_RRQ_M_SDIR_DEF },
   { CSA_RSH_MCM,    "CSA_RSH_M_SDIR",  CSA_RSH_M_SDIR_DEF },
   { CSA_CRQ_MCM,    "CSA_CRQ_M_SDIR",  CSA_CRQ_M_SDIR_DEF },
   { CSA_CSH_MCM,    "CSA_CSH_M_SDIR",  CSA_CSH_M_SDIR_DEF },
   { SENTINEL,        NULL,             NULL               }
} ;


/* Table of CSA Source Reception names
*/
Names_Table_Entry CSA_ReceptDir_Table[] =
{
   { CSA_PREDORBIT,  "CSA_PORB_RDIR",  CSA_PORB_RDIR_DEF  },
   { CSA_DEFVORBIT,  "CSA_DORB_RDIR",  CSA_DORB_RDIR_DEF  },
   { CSA_RECRQST,    "CSA_RRQ_RDIR",   CSA_RRQ_RDIR_DEF   },
   { CSA_RECSCHED,   "CSA_RSH_RDIR",   CSA_RSH_RDIR_DEF   },
   { CSA_CALIBRQST,  "CSA_CRQ_RDIR",   CSA_CRQ_RDIR_DEF   },
   { CSA_CALIBSCHED, "CSA_CSH_RDIR",   CSA_CSH_RDIR_DEF   },
   { CSA_SARPROCPRM, "CSA_SARPP_RDIR", CSA_SARPP_RDIR_DEF },
   { CSA_RRQ_MCM,    "CSA_RRQ_M_RDIR", CSA_RRQ_M_RDIR_DEF },
   { CSA_RSH_MCM,    "CSA_RSH_M_RDIR", CSA_RSH_M_RDIR_DEF },
   { CSA_CRQ_MCM,    "CSA_CRQ_M_RDIR", CSA_CRQ_M_RDIR_DEF },
   { CSA_CSH_MCM,    "CSA_CSH_M_RDIR", CSA_CSH_M_RDIR_DEF },
   { SENTINEL,        NULL,            NULL               }
} ;

/* Table of CSA Destination Directory names
*/
Names_Table_Entry CSA_DestDir_Table[] =
{
   { CSA_PREDORBIT,  "CSA_PORB_DDIR",  CSA_PORB_DDIR_DEF  },
   { CSA_DEFVORBIT,  "CSA_DORB_DDIR",  CSA_DORB_DDIR_DEF  },
   { CSA_RECRQST,    "CSA_RRQ_DDIR",   CSA_RRQ_DDIR_DEF   },
   { CSA_RECSCHED,   "CSA_RSH_DDIR",   CSA_RSH_DDIR_DEF   },
   { CSA_CALIBRQST,  "CSA_CRQ_DDIR",   CSA_CRQ_DDIR_DEF   },
   { CSA_CALIBSCHED, "CSA_CSH_DDIR",   CSA_CSH_DDIR_DEF   },
   { CSA_SARPROCPRM, "CSA_SARPP_DDIR", CSA_SARPP_DDIR_DEF },
   { CSA_RRQ_MCM,    "CSA_RRQ_M_DDIR", CSA_RRQ_M_DDIR_DEF },
   { CSA_RSH_MCM,    "CSA_RSH_M_DDIR", CSA_RSH_M_DDIR_DEF },
   { CSA_CRQ_MCM,    "CSA_CRQ_M_DDIR", CSA_CRQ_M_DDIR_DEF },
   { CSA_CSH_MCM,    "CSA_CSH_M_DDIR", CSA_CSH_M_DDIR_DEF },
   { SENTINEL,        NULL,            NULL               }
} ;

/* Table of CSA Translated Files Directory names
*/
Names_Table_Entry CSA_TransDir_Table[] =
{
   { CSA_PREDORBIT,  "CSA_PORB_TDIR",  CSA_PORB_TDIR_DEF  },
   { CSA_DEFVORBIT,  "CSA_DORB_TDIR",  CSA_DORB_TDIR_DEF  },
   { CSA_RECRQST,    "CSA_RRQ_TDIR",   CSA_RRQ_TDIR_DEF   },
   { CSA_RECSCHED,   "CSA_RSH_TDIR",   CSA_RSH_TDIR_DEF   },
   { CSA_CALIBRQST,  "CSA_CRQ_TDIR",   CSA_CRQ_TDIR_DEF   },
   { CSA_CALIBSCHED, "CSA_CSH_TDIR",   CSA_CSH_TDIR_DEF   },
   { CSA_SARPROCPRM, "CSA_SARPP_TDIR", CSA_SARPP_TDIR_DEF },
   { CSA_RRQ_MCM,    "CSA_RRQ_M_TDIR", CSA_RRQ_M_TDIR_DEF },
   { CSA_RSH_MCM,    "CSA_RSH_M_TDIR", CSA_RSH_M_TDIR_DEF },
   { CSA_CRQ_MCM,    "CSA_CRQ_M_TDIR", CSA_CRQ_M_TDIR_DEF },
   { CSA_CSH_MCM,    "CSA_CSH_M_TDIR", CSA_CSH_M_TDIR_DEF },
   { SENTINEL,        NULL,            NULL               }
} ;


#endif /* _CSAROUTER_ */

/* End of file */
