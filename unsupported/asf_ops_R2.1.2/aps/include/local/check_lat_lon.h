#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	check_lat_lon.h

Description:	
    Function Declarations for checking a latitude or longitude field.  
    Also return code #defines.  

	
Notes:

==============================================================================*/
#pragma ident	"@(#)check_lat_lon.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/local/SCCS/s.check_lat_lon.h"


#ifndef _CHECK_LAT_LON_H_

#include "dapps_defs.h"

int check_latitude( float latitude ) ;

int check_longitude( float longitude ) ;

#endif  /* _CHECK_LAT_LON_H_ */
