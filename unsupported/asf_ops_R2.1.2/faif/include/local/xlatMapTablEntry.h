/*==============================================================================
Filename:	xlatMapTablEntry.h

Description:	
	This header file contains the type definition for the common
structure used to store ASF to flight agency/Wallops data  mapping 
information.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _XLATMAPTABLENTRY_
#define _XLATMAPTABLENTRY_

#ifndef ERROR
#include "faifdefs.h"
#endif

/* Used for ASF to FA or Wallops map tables
*/
typedef struct map_table_entry
{
   char *asf ;
   char *fa ;

} Map_Table_Entry ;


#endif /* _XLATMAPTABLENTRY_ */

/* End of File */
