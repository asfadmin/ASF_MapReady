/*************************************************************************
 * Copyright (c)1995, 1996 California Institute of Technology.           *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
 
/*==============================================================================
Filename:	xlatASFtoWALPS.h

Description:	
	This header file contains the type definition for the common
structure used to store ASF to Wallops mapping information.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		
1.  Oct. '96 - RH
    Update for ISD Version 2.3; change keyword abbreviations

SCCS Info:
   @(#)xlatASFtoWALPS.h	1.1
==============================================================================*/

#ifndef _XLATASFTOWALPS_
#define _XLATASFTOWALPS_

#ifndef ERROR
#include "faifdefs.h"
#endif

#include "xlatMapTablEntry.h"
#include "WALPS.h"
#include "ASF.h"


Map_Table_Entry Map_Satellite_Table[] =
{
   { ASF_SATNAME_RS, WALPS_SATNAME_R1 },
   { ASF_SATNAME_E1, WALPS_SATNAME_E1 },
   { ASF_SATNAME_E2, WALPS_SATNAME_E2 },
   { ASF_SATNAME_J1, WALPS_SATNAME_J1 },
   { ASF_SATNAME_A1, WALPS_SATNAME_A1 },
   { NULL,           NULL             }
} ;


Map_Table_Entry Map_Sensor_Table[] =
{
   { ASF_SENSOR_DUMP, WALPS_SENSOR_DUMP },
   { ASF_SENSOR_SAR,  WALPS_SENSOR_SAR  },
   { ASF_SENSOR_OPS,  WALPS_SENSOR_OPS  },
   { ASF_SENSOR_VNIR, WALPS_SENSOR_VNIR },
   { NULL,            NULL              }
} ;


Map_Table_Entry Map_Agency_Table[] =
{
   { ASF_AGENCY_ASF,   WALPS_AGENCY_ASF   },
   { ASF_AGENCY_ESA,   WALPS_AGENCY_ESA   },
   { ASF_AGENCY_CSA,   WALPS_AGENCY_CSA   },
   { ASF_AGENCY_ESF,   WALPS_AGENCY_ESF   },
   { ASF_AGENCY_CEF,   WALPS_AGENCY_CSF   },
   { ASF_AGENCY_NAS,   WALPS_AGENCY_NASDA },
   { ASF_AGENCY_NSF,   WALPS_AGENCY_NSF   },
   { NULL,             NULL               }
} ;


Map_Table_Entry Map_SensorMode_Table[] =
{
   { ASF_SMODE_STD1,  WALPS_SMODE_STD1 },
   { ASF_SMODE_STD2,  WALPS_SMODE_STD2 },
   { ASF_SMODE_STD3,  WALPS_SMODE_STD3 },
   { ASF_SMODE_STD4,  WALPS_SMODE_STD4 },
   { ASF_SMODE_STD5,  WALPS_SMODE_STD5 },
   { ASF_SMODE_STD6,  WALPS_SMODE_STD6 },
   { ASF_SMODE_STD7,  WALPS_SMODE_STD7 },
   { ASF_SMODE_WIDE1, WALPS_SMODE_WIDE1 },
   { ASF_SMODE_WIDE2, WALPS_SMODE_WIDE2 },
   { ASF_SMODE_WIDE3, WALPS_SMODE_WIDE3 },
   { ASF_SMODE_FINE1, WALPS_SMODE_FINE1 },
   { ASF_SMODE_FINE2, WALPS_SMODE_FINE2 },
   { ASF_SMODE_FINE3, WALPS_SMODE_FINE3 },
   { ASF_SMODE_FINE4, WALPS_SMODE_FINE4 },
   { ASF_SMODE_FINE5, WALPS_SMODE_FINE5 },
   { ASF_SMODE_ELOW1, WALPS_SMODE_ELOW1 },
   { ASF_SMODE_EHI1,  WALPS_SMODE_EHI1 },
   { ASF_SMODE_EHI2,  WALPS_SMODE_EHI2 },
   { ASF_SMODE_EHI3,  WALPS_SMODE_EHI3 },
   { ASF_SMODE_EHI4,  WALPS_SMODE_EHI4 },
   { ASF_SMODE_EHI5,  WALPS_SMODE_EHI5 },
   { ASF_SMODE_EHI6,  WALPS_SMODE_EHI6 },
   { ASF_SMODE_SCNS1, WALPS_SMODE_SCNS1 },
   { ASF_SMODE_SCNS2, WALPS_SMODE_SCNS2 },
   { ASF_SMODE_SCNS3, WALPS_SMODE_SCNS3 },
   { ASF_SMODE_SCNS4, WALPS_SMODE_SCNS4 },
   { NULL,            NULL }
} ;


Map_Table_Entry Map_Activity_Table[] =
{
   { ASF_ACT_RTSAR,   WALPS_ACT_DWNLNK },
   { ASF_ACT_DUMP,    WALPS_ACT_DUMP   },
   { NULL,            NULL }
} ;


Map_Table_Entry Map_TransmitterId_Table[] =
{
   { ASF_XMITTER_J8150, WALPS_XMITTER_J8150 },
   { ASF_XMITTER_J8350, WALPS_XMITTER_J8350 },
   { ASF_XMITTER_R8105, WALPS_XMITTER_R8105 },
   { ASF_XMITTER_R8230, WALPS_XMITTER_R8230 },
   { NULL,              NULL                }
} ;


Map_Table_Entry Map_SrcDest_Table[] =
{
   { ASF_SRCDEST_APS,    WALPS_SRCDEST_ASF   },
   { ASF_SRCDEST_ASF,    WALPS_SRCDEST_ASF   },
   { ASF_SRCDEST_RGS,    WALPS_SRCDEST_ASF   },
   { ASF_SRCDEST_WALPS,  WALPS_SRCDEST_WALPS },
   { NULL,               NULL                }
} ;


Map_Table_Entry Map_MsgType_Table[] =
{
   { ASF_MSGTYPE_DT,   DNL_KEYWD }, 
   { ASF_MSGTYPE_SV,   EPH_KEYWD }, 
   { ASF_MSGTYPE_WOS,  WOS_KEYWD },
   { ASF_MSGTYPE_AREQ, REQ_KEYWD },
   { NULL,             NULL      }
} ;

#endif /* _XLATASFTOWALPS_ */

/* End of File */
