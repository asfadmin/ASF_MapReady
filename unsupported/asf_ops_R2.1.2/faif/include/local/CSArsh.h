/*==============================================================================
Filename:	CSArsh.h

Description:	
	This file contains definitions specific to the CSA reception 
schedule file.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		
	Refer to FAIF Software Requirements Document (JPL-D11909) for a
list of file types supported through this header file.

SCCS Info:
   %W%
==============================================================================*/

#ifndef _CSARSH_
#define _CSARSH_

#ifndef ERROR
#include "faifdefs.h"
#endif

#include "CSA.h"

#define CSARSH_FACILITY_ID      "RECEPTION_FACILITY_ID"
#define CSARSH_SCHED_ID         "RECEPTION_SCHEDULE_ID"
#define CSARSH_ORBIT_NUM        "RECEPTION_ORBIT_NUMBER"

#define CSARSH_RECEPT_ACT_SPECS "RECEPTION_ACTIVITY_SPECS"
#define CSARSH_START_PLAYBACK   "IMG_DATA_DL_START"
#define CSARSH_STOP_PLAYBACK    "IMG_DATA_DL_STOP"

#endif /* _CSARSH_ */

/* End of File */
