/*==============================================================================
Filename:	server_send.h

Description:	
	This file contains the #define's for used by in server_send.c.
Primarily, it contains #define's for the default values in the remote
host tables, external directory tables, transfer command tables per
flight agency and/or flight agency file type.  This file also contains 
the said tables.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   @(#)server_send.h	1.1
==============================================================================*/

#ifndef _SERVERSEND_
#define _SERVERSEND_

#ifndef ERROR
#include "defs.h"
#endif

#include "server_upw.h"

#include "ESA.h"
#include "NASDA.h"
#include "CSA.h"
#include "WALPS.h"
#include "ADEOS.h"

#define ESA_HOSTADDR_DEF    "cogito" 
#define NASDA_HOSTADDR_DEF  "cogito"
#define CSA_HOSTADDR_DEF    "mcs_vdn" 
#define WALPS_HOSTADDR_DEF  "cogito"
#define ADEOS_HOSTADDR_DEF  "cogito"

#define ESA_TRANSFER_CMD_DEF   "FAIF_VAX_ftpsend.csh"
#define NASDA_TRANSFER_CMD_DEF "FAIF_UNIX_ftpsend.csh"
#define CSA_TRANSFER_CMD_DEF   "FAIF_UNIX_ftpsend.csh"
#define WALPS_TRANSFER_CMD_DEF "FAIF_UNIX_ftpsend.csh"
#define ADEOS_TRANSFER_CMD_DEF "FAIF_UNIX_ftpsend.csh"

#define ESA_EXTDIR_DEF      "/ub/pps/pps/toESA"
#define ESA_RQUS_EDIR_DEF   "/ub/pps/pps/toESA"
#define ESA_REAQ_EDIR_DEF   "/ub/pps/pps/toESA"
#define ESA_RESM_EDIR_DEF   "/ub/pps/pps/toESA"
#define ESA_REEX_EDIR_DEF   "/ub/pps/pps/toESA"
#define ESA_REUG_EDIR_DEF   "/ub/pps/pps/toESA"

#define NASDA_EXTDIR_DEF    "/ub/pps/pps/toNASDA"
#define NASDA_REQQ_EDIR_DEF "/ub/pps/pps/toNASDA"
#define NASDA_REQW_EDIR_DEF "/ub/pps/pps/toNASDA"
#define NASDA_REAC_EDIR_DEF "/ub/pps/pps/toNASDA"
#define NASDA_CATA_EDIR_DEF "/ub/pps/pps/toNASDA"
#define NASDA_MSGM_EDIR_DEF "/ub/pps/pps/toNASDA"
#define NASDA_MSGF_EDIR_DEF "/ub/pps/pps/toNASDA"
#define NASDA_MSGE_EDIR_DEF "/ub/pps/pps/toNASDA"

#define CSA_EXTDIR_DEF   "/ub/pps/pps/MCS"
#define CSA_ASR_EDIR_DEF "/ub/pps/pps/MCS/ufdroc_1_to_mcs"
#define CSA_RRP_EDIR_DEF "/ub/pps/pps/MCS/ufdrf_f_to_mcs"
#define CSA_RAR_EDIR_DEF "/ub/pps/pps/MCS/ufdrf_f_to_mcs"
#define CSA_CRP_EDIR_DEF "/ub/pps/pps/MCS/ufcal_1_to_mcs"
#define CSA_CAR_EDIR_DEF "/ub/pps/pps/MCS/ufcal_1_to_mcs"

#define CSA_RRP_M_EDIR_DEF "/ub/pps/pps/MCS/ufdrf_m_to_mcs"
#define CSA_RAR_M_EDIR_DEF "/ub/pps/pps/MCS/ufdrf_m_to_mcs"
#define CSA_CRP_M_EDIR_DEF "/ub/pps/pps/MCS/ufcal_2_to_mcs"
#define CSA_CAR_M_EDIR_DEF "/ub/pps/pps/MCS/ufcal_2_to_mcs"

#define WALPS_EXTDIR_DEF     "/ub/pps/pps/toWALPS"
#define WALPS_AREQ_EDIR_DEF  "/ub/pps/pps/toWALPS"
#define WALPS_WOS_EDIR_DEF   "/ub/pps/pps/toWALPS"
#define WALPS_STVEC_EDIR_DEF "/ub/pps/pps/toWALPS"

#define ADEOS_EXTDIR_DEF     "/local/faif/ADEOS"
#define ADEOS_STGS_EDIR_DEF  "/local/faif/ADEOS"
#define ADEOS_REAC_EDIR_DEF  "/local/faif/ADEOS"
#define ADEOS_SRRD_EDIR_DEF  "/local/faif/ADEOS"

/* Table of FAxmitserver variables
*/
Names_Table_Entry Xmit_HostVar_Table[] =
{
   { ESA,      "ESA_HOSTADDR",   ESA_HOSTADDR_DEF   },
   { NASDA,    "NASDA_HOSTADDR", NASDA_HOSTADDR_DEF }, 
   { CSA,      "CSA_HOSTADDR",   CSA_HOSTADDR_DEF   },
   { WALPS,    "WALPS_HOSTADDR", WALPS_HOSTADDR_DEF }, 
   { ADEOS,    "ADEOS_HOSTADDR", ADEOS_HOSTADDR_DEF }, 
   { SENTINEL,  NULL,            NULL               }
} ;


/* Table of ESA remote dir variables
*/
Names_Table_Entry ESA_ExtDir_Table[] =
{
   { ESA,      "ESA_EXTDIR",     ESA_EXTDIR_DEF    },
   { ESA_RQUS, "ESA_RQUS_EDIR",  ESA_RQUS_EDIR_DEF },
   { ESA_REAQ, "ESA_REAQ_EDIR",  ESA_REAQ_EDIR_DEF },
   { ESA_RESM, "ESA_RESM_EDIR",  ESA_RESM_EDIR_DEF },
   { ESA_REEX, "ESA_REEX_EDIR",  ESA_REEX_EDIR_DEF },
   { ESA_REUG, "ESA_REUG_EDIR",  ESA_REUG_EDIR_DEF },
   { SENTINEL,  NULL,            NULL              }
} ;


/* Table of NASDA remote dir variables
*/
Names_Table_Entry NASDA_ExtDir_Table[] =
{
   { NASDA,      "NASDA_EXTDIR",    NASDA_EXTDIR_DEF    },
   { NASDA_REQQ, "NASDA_REQQ_EDIR", NASDA_REQQ_EDIR_DEF },
   { NASDA_REQW, "NASDA_REQW_EDIR", NASDA_REQW_EDIR_DEF },
   { NASDA_REAC, "NASDA_REAC_EDIR", NASDA_REAC_EDIR_DEF },
   { NASDA_CATA, "NASDA_CATA_EDIR", NASDA_CATA_EDIR_DEF },
   { NASDA_MSGM, "NASDA_MSGM_EDIR", NASDA_MSGM_EDIR_DEF },
   { NASDA_MSGF, "NASDA_MSGF_EDIR", NASDA_MSGF_EDIR_DEF },
   { NASDA_MSGE, "NASDA_MSGE_EDIR", NASDA_MSGE_EDIR_DEF },
   { SENTINEL,    NULL,             NULL                }
} ;


/* Table of CSA remote dir variables
*/
Names_Table_Entry CSA_ExtDir_Table[] =
{
   { CSA,               "CSA_EXTDIR",     CSA_EXTDIR_DEF     },
   { CSA_ARCHSTRGRPT,   "CSA_ASR_EDIR",   CSA_ASR_EDIR_DEF   },
   { CSA_RECRPT,        "CSA_RRP_EDIR",   CSA_RRP_EDIR_DEF   },
   { CSA_RECAVAILRPT,   "CSA_RAR_EDIR",   CSA_RAR_EDIR_DEF   }, 
   { CSA_CALIBRPT,      "CSA_CRP_EDIR",   CSA_CRP_EDIR_DEF   }, 
   { CSA_CALIBAVAILRPT, "CSA_CAR_EDIR",   CSA_CAR_EDIR_DEF   }, 
   { CSA_RRP_MCM,       "CSA_RRP_M_EDIR", CSA_RRP_M_EDIR_DEF },
   { CSA_RAR_MCM,       "CSA_RAR_M_EDIR", CSA_RAR_M_EDIR_DEF }, 
   { CSA_CRP_MCM,       "CSA_CRP_M_EDIR", CSA_CRP_M_EDIR_DEF }, 
   { CSA_CAR_MCM,       "CSA_CAR_M_EDIR", CSA_CAR_M_EDIR_DEF }, 
   { SENTINEL,           NULL,            NULL               }
} ;


/* Table of WALLOPS remote dir variables
*/
Names_Table_Entry WALPS_ExtDir_Table[] =
{
   { WALPS,            "WALPS_EXTDIR",     WALPS_EXTDIR_DEF     },
   { WALPS_WOS,        "WALPS_WOS_EDIR",   WALPS_WOS_EDIR_DEF   },
   { WALPS_EPH,      "WALPS_STVEC_EDIR", WALPS_STVEC_EDIR_DEF },
   { WALPS_REQ,  "WALPS_AREQ_EDIR",  WALPS_AREQ_EDIR_DEF  },
   { SENTINEL,          NULL,              NULL                 }
} ;


/* Table of ADEOS remote dir variables
*/
Names_Table_Entry ADEOS_ExtDir_Table[] =
{
   { ADEOS,            "ADEOS_EXTDIR",     ADEOS_EXTDIR_DEF     },
   { ADEOS_STGS,       "ADEOS_STGS_EDIR",  ADEOS_STGS_EDIR_DEF  },
   { ADEOS_REAC,       "ADEOS_REAC_EDIR",  ADEOS_REAC_EDIR_DEF  },
   { ADEOS_SRRD,       "ADEOS_SRRD_EDIR",  ADEOS_SRRD_EDIR_DEF  },
   { SENTINEL,          NULL,              NULL                 }
} ;


/* Table of file transfer script names where it applies
*/
Names_Table_Entry FA_TransferCmd_Table[] =
{
   { ESA,      "ESA_TRANSFER_CMD",   ESA_TRANSFER_CMD_DEF   },
   { NASDA,    "NASDA_TRANSFER_CMD", NASDA_TRANSFER_CMD_DEF },
   { CSA,      "CSA_TRANSFER_CMD",   CSA_TRANSFER_CMD_DEF   },
   { WALPS,    "WALPS_TRANSFER_CMD", WALPS_TRANSFER_CMD_DEF },
   { ADEOS,    "ADEOS_TRANSFER_CMD", ADEOS_TRANSFER_CMD_DEF },
   { SENTINEL,  NULL,                NULL                   } 
} ;


/* Table of names of password files per FA, CSA file type
*/
Names_Table_Entry FA_PassFile_Table[] =
{
   { ESA,               ESA_STR,   ESASEND_UPW_FILE    },
   { NASDA,             NASDA_STR, NASDASEND_UPW_FILE  },
   { CSA_ARCHSTRGRPT,   CSA_STR,   CSAUFDROCF_UPW_FILE },
   { CSA_RECRPT,        CSA_STR,   CSAUFDRFF_UPW_FILE  },
   { CSA_RECAVAILRPT,   CSA_STR,   CSAUFDRFF_UPW_FILE  },
   { CSA_CALIBRPT,      CSA_STR,   CSAUFCALF_UPW_FILE  },
   { CSA_CALIBAVAILRPT, CSA_STR,   CSAUFCALF_UPW_FILE  },
   { CSA_RRP_MCM,       CSA_STR,   CSAUFDRFM_UPW_FILE  },
   { CSA_RAR_MCM,       CSA_STR,   CSAUFDRFM_UPW_FILE  },
   { CSA_CRP_MCM,       CSA_STR,   CSAUFCALM_UPW_FILE  },
   { CSA_CAR_MCM,       CSA_STR,   CSAUFCALM_UPW_FILE  },
   { WALPS,             WALPS_STR, WALPSSEND_UPW_FILE  },
   { SENTINEL,          NULL,      NULL                }
} ;
			       

#endif /* _SERVERSEND_ */

/* End of File */
