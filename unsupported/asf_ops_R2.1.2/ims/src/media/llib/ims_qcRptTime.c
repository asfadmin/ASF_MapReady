static char *sccs = "@(#)ims_qcRptTime.c	5.1  03/17/96";
/* *******************************************************************
*
*  Name: RptTime
*
*  Module Type: PROCEDURE     Language: C
*
*  Purpose: Get the current system time and store as a character string
*               in format dd-mmm-yyyy hh:mm:ss (Ingres report format).
*
*   RptTime is a modified version of GetTime.
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
*  Name          Type   Description
*  curr_time     *char  Pointer to character string with current time
*                       in dd-mmm-yyyy hh:mm:ss format
*
*
*  Modification History:
*
*   Date:   18 Jul 1990 10:25:56    Revision:   2.0   Author:   DBMAN
*
******************************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <time.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_util.h>
#include <ims_msg.h>

void    RptTime(curr_time)

char    *curr_time;

{
long sec_1970;
int idigit;
char    digit[2];
struct  tm  *time_ptr;
char    *asc_time;
static  char  *time_template={"dd-mmm-yyyy hh:mm:ss"};

/*********************************************************************/

/*  Get the current time */

(void) time (&sec_1970);
time_ptr = localtime (&sec_1970);
asc_time = asctime (time_ptr);

/* Copy template into buffer */

(void) strcpy (curr_time, time_template);


/* Store time fields into buffer.. */

(void) strncpy (digit,asc_time+8,1);
idigit = atoi (digit);
if( idigit == 0) (void) strncpy (asc_time+8,"0",1);
(void) strncpy (curr_time,   asc_time+8, 2);
(void) strncpy (curr_time+3, asc_time+4, 3);
(void) strncpy (curr_time+7, asc_time+20, 4);
(void) strncpy (curr_time+12, asc_time+11, 2);
(void) strncpy (curr_time+15, asc_time+14, 2);
(void) strncpy (curr_time+18, asc_time+17, 2);

return;
}   /*  RptTime */
