/*==============================================================================
Filename:	xlatASFsvtoODL.h

Description:	ASF to WALPS State Vector file identifiers mapping tables	
Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _XLATASFSVTOODL_
#define _XLATASFSVTOODL_

#include "xlatMapTablEntry.h"

Map_Table_Entry Map_SVPrecis_Table[] =
{
   { ASF_SVTYPE_PRED,  WALPS_SVTYPE_PRED },
   { ASF_SVTYPE_REST,  WALPS_SVTYPE_REST },
   { ASF_SENSOR_OPS,  WALPS_SENSOR_OPS  },
   { ASF_SENSOR_VNIR, WALPS_SENSOR_VNIR },
   { NULL,            NULL              }
} ;


Map_Table_Entry Map_SVCoordsys_Table[] =
{
   { ASF_SVPRECIS_TE, WALPS_SVPRECIS_TE },
   { ASF_SVPRECIS_MD, WALPS_SVPRECIS_MD },
   { NULL,            NULL              }
} ;

#endif /* _XLATASFSVTOODL_ */


/* End of File */
