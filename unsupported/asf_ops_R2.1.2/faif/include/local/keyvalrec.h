/*==============================================================================
Filename:	keyvalrec.h

Description:	
	This header file contains the typedef for the keyword value
statement record.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _KEYVALREC_
#define _KEYVALREC_

/* Keyword Value Statement record structure
*/
typedef struct keyword_value_stmt
{
   char *keyword ;      /* Keyword string */
   char *value_string ; /* Value string */

} Keyword_Value_Stmt ;

#endif /* _KEYVALREC_ */

/* End of File */
