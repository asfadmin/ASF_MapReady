/*==============================================================================
Filename:	CSAftypestr.h

Description:	
	This module contains the definition of CSA_FileType_Table.
Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _CSAFTYPESTR_
#define _CSAFTYPESTR_

#include "CSA.h"

File_Identifier CSA_FileTypeStr_Table[] =
{
   { CSA_PREDORBIT,      CSA_PREDORBIT_STR     }, /* Predicted Orbit */
   { CSA_DEFVORBIT,      CSA_DEFVORBIT_STR     }, /* Definitive Orbit */
   { CSA_RECRQST,        CSA_RECRQST_STR       }, /* Reception request */
   { CSA_RECSCHED,       CSA_RECSCHED_STR      }, /* Reception schedule */
   { CSA_CALIBRQST,      CSA_CALIBRQST_STR     }, /* Calibration request */
   { CSA_CALIBSCHED,     CSA_CALIBSCHED_STR    }, /* Calibration schedule */
   { CSA_SARPROCPRM,     CSA_SARPROCPRM_STR    }, /* SAR Processing params */
   { CSA_RRQ_MCM,        CSA_RRQ_MCM_STR       }, /* McMurdo Reception Req. */
   { CSA_RSH_MCM,        CSA_RSH_MCM_STR       }, /* McMurdo Reception Sched. */
   { CSA_CRQ_MCM,        CSA_CRQ_MCM_STR       }, /* McMurdo Calib. Req. */
   { CSA_CSH_MCM,        CSA_CSH_MCM_STR       }, /* McMurdo Calib Sched. */

   { CSA_ARCHSTRGRPT,    CSA_ARCHSTRGRPT_STR   }, /* Archive Storage Report */
   { CSA_RECRPT,         CSA_RECRPT_STR        }, /* Reception Report */
   { CSA_RECAVAILRPT,    CSA_RECAVAILRPT_STR   }, /* Reception Avail. Report */
   { CSA_CALIBRPT,       CSA_CALIBRPT_STR      }, /* Calibration Report */
   { CSA_CALIBAVAILRPT,  CSA_CALIBAVAILRPT_STR }, /* Calib. Avail. Report */
   { CSA_RRP_MCM,        CSA_RRP_MCM_STR       }, /* McMurdo Recep. Report */
   { CSA_RAR_MCM,        CSA_RAR_MCM_STR       }, /* McMurdo Recep. Avail. Report */
   { CSA_CRP_MCM,        CSA_CRP_MCM_STR       }, /* McMurdo Calib. Report */
   { CSA_CAR_MCM,        CSA_CAR_MCM_STR       }, /* McMurdo Calib. Avail. Report */
   { SENTINEL,           NULL                  }
} ;

#endif /* _CSAFTYPESTR_ */

/* End of File */
