/*==============================================================================
Filename:	CSAcsh.h

Description:	
	This file contains definitions specific to the CSA calibration 
schedule file.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		
	Refer to FAIF Software Requirements Document (JPL-D11909) for a
list of file types supported through this header file.

SCCS Info:
   %W%
==============================================================================*/

#ifndef _CSACSH_
#define _CSACSH_

#ifndef ERROR
#include "faifdefs.h"
#endif

#include "CSA.h"

#define CSACSH_ABS_ORBIT_NUM    "ORBIT_NB"

#define CSACSH_RECEPT_ACT_SPECS "RECEPTION_ACTIVITY_SPECS"
#define CSACSH_START_CALTIME    "CALIB_ACT_START_TIME"
#define CSACSH_STOP_CALTIME     "CALIB_ACT_STOP_TIME"

#endif /* _CSACSH_ */

/* End of File */
