#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	check_lat_lon.c

Description:	File for routines that check latitude & longitude

External Functions Defined:
int check_latitude( float latitude )
int check_longitude( float latitude )
	
File Scope Functions:
None
	
External Variables Defined:
None
	
File Scope Variables:
None
	
Notes:
They return TRUE if OK and FALSE if not; TRUE = 1; FALSE = 0 ;
Use #include check_lat_lon.h for prototypes.  

==============================================================================*/
#pragma ident	"@(#)check_lat_lon.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.check_lat_lon.c"


#include "check_lat_lon.h"
#include <math.h>       /* REQUIRED for fabs()   */


/*==============================================================================

Function:       check_latitude

Description:    checks a latitude value for -90 <= lat <= +90

Parameters:

Returns:
	int
	= TRUE   field is OK.
	= FALSE  field is not OK. 

Creator:    Lawrence Stevens

Creation Date:  04/02/1995

Notes:

==============================================================================*/
int check_latitude( float latitude )
{

	if ( fabs(latitude) > 90.0  )
		return FALSE ;

	return TRUE ;

}



/*==============================================================================

Function:       check_longitude

Description:    checks the longitude value for 180 <= longitude <= +180

Returns:
	int
	= TRUE   the field is OK.
	= FALSE  the field is not OK>  

Creator:    Lawrence Stevens

Creation Date:  04/02/1995

Notes:

==============================================================================*/
int check_longitude( float longitude )
{

	if ( fabs(longitude)  > 180.0  )
		return FALSE ;

	return TRUE ;

}
