/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	convert_odldate.c

Description:	Function to store ODL date/time info into PPS time record	

External Functions:
	convert_ODLdate_to_timerec
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:

==============================================================================*/

static char SccsFileId[] = "@(#)convert_odldate.c	1.1    11/21/96";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "defs.h"
#include "timerec.h"
#include "odldef.h"
#include "odlinter.h"

void convert_ODLdate_to_timerec(struct ODLDate *, Time_Record *) ;



/*==============================================================================
Function:	void convert_ODLdate_to_timerec (struct ODLDate *odl_date,
	                                         Time_Record *time_rec)
Description:	Copy contents of ODL date record into a different time record
Parameters:	Record containing ODL date, Other time record to hold same info
Returns:	None	
Creator:	Nadia Adhami
Creation Date:	Tue Oct 17 14:12:14 PDT 1995
Notes:		
==============================================================================*/
void convert_ODLdate_to_timerec ( struct ODLDate *odl_date, Time_Record *time_rec)
{

	time_rec->year	  = odl_date->year;
	time_rec->day 	  = odl_date->doy;
	time_rec->hour 	  = (int)odl_date->hours;
	time_rec->minute  = (int)odl_date->minutes;
	time_rec->seconds = (double)odl_date->seconds;

	/* add the nanoseconds */
	time_rec->seconds += (double)odl_date->nanoseconds / 1000000000.0;
	

	sprintf(time_rec->time_string, "%d-%03dT%02d:%02d:%06.03f",
		time_rec->year, time_rec->day, time_rec->hour,
		time_rec->minute, time_rec->seconds);

}

/* End of File */
