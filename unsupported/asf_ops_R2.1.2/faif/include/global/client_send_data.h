/*==============================================================================
Filename:	client_send_data.h

Description:	
	Include file to be used by FAxmitserver client applications.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _CLIENTSENDDATA_
#define _CLIENTSENDDATA_

/* Definition of SENTINEL in defs.h
*/
#include "faifdefs.h"

/* Tables of FA file type id strings that
-- can be used by FAxmitserver client apps
*/
#include "ESAftypestr.h"
#include "NASDAftypestr.h"
#include "CSAftypestr.h"
#include "WALPSftypestr.h"
#include "ADEOSftypestr.h"

#define FAXMIT_CDS_ENTRY_EV   "FAXMIT_CDS_ENTRY"

/* Table of valid Flight Agency Names that
-- can be passed to the send server function
*/
Names_Table_Entry FA_Names_Table[] =
{
   { ESA,      ESA_STR,   NULL },
   { NASDA,    NASDA_STR, NULL },
   { CSA,      CSA_STR,   NULL },
   { WALPS,    WALPS_STR, NULL },
   { ADEOS,    ADEOS_STR, NULL },
   { SENTINEL, NULL,      NULL }
} ;

#endif /* _CLIENTSENDDATA_ */

/* End of File */
