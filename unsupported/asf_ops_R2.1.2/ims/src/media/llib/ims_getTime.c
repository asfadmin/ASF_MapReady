static char *sccs = "@(#)ims_getTime.c	5.1  03/17/96";
/********************************************************************
*
*  Name: GetTime
*
*  Module Type: PROCEDURE     Language: C
*
*  Purpose: Get the current system time and store as a character string.
*
*
*  Input Parameters:
*
*  Name          Type        Description
*  NONE
*
*
*  Output Parameters:
*
*  Name          Type        Description
*  curr_time     *char       Pointer to character string with current
*                            time in yyyy:ddd:hh:mm:ss.ccc format.
*
*
*  Modification History:
*
*  Date:   18 Oct 1988 14:25:10   Revision:   1.0   Author:   TOM
*  Date:   17 Jul 1990 13:00:34   Revision:   2.0   Author:   DBMAN
*
*********************************************************************/

#include    <stdio.h>
#include    <time.h>
#include    <string.h>

void  GetTime(curr_time)
char    *curr_time;
{

long sec_1970;
struct  tm  *time_ptr;
char    *asc_time;
char    time_tmp[4];
char    *time_template = "yyyy:ddd:hh:mm:ss.000";


/*  Get the current time */

(void) time (&sec_1970);
time_ptr = localtime (&sec_1970);
asc_time = asctime (time_ptr);

/*  Get the number of days into the year */

(void) sprintf ( time_tmp, "%03d", (time_ptr->tm_yday)+1);


/* Copy template into buffer */

(void) strcpy (curr_time, time_template);


/* Store time fields into buffer.. */

(void) strncpy (curr_time,      asc_time+20, 4);
(void) strncpy (curr_time+5,  time_tmp, 3);
(void) strncpy (curr_time+9,  asc_time+11, 2);
(void) strncpy (curr_time+12, asc_time+14, 2);
(void) strncpy (curr_time+15, asc_time+17, 2);

return;
}   /*  GetTime   */
