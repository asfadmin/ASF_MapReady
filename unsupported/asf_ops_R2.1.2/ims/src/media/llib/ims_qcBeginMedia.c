static char *sccs = "@(#)ims_qcBeginMedia.c	5.1  03/17/96";
/* *******************************************************************
*
*  Name: BeginMedia
*
*  Module Type: Procedure     Language: C
*
*  Purpose:
*    This routine mounts the media and opens the temporary report files.
*
*  Input Parameters:
*
*  Name          Type        Description
*  nummedia       int           Media number
*  device         char          Device
*  media_typ      char          Type of media (tape, disk, next)
*  report_typ     char          Type of report (brief,standard,full)
*
*  Output Parameters:
*
*  Name          Type        Description
*  media_buf      char          Media section of report
*   tmp1file      FILE          Pointer to temporary file 1
*   tmp2file      FILE          Pointer to temporary file 2
*   istat         int           Return status, SS$_NORMAL, SS$_ABORT
*
*  Modification History:
*
*  Date:   09 Nov 1989 08:45:00    Revision:   1.0    Author:   CAROL
*  Date:   18 Jul 1990 10:17:02    Revision:   2.0    Author:   DBMAN
*  Date:   06 Aug 1990 10:37:32    Revision:   2.1    Author:   CAROL
*
******************************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/utsname.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_util.h>
#include <ims_msg.h>

#define ERR  -1

int BeginMedia( msgDesc, path, report_typ, media_buf,
        tmp1file, tmp2file, time_stamp)
IMS_MSG_STRUCT  * msgDesc;
char    path[];
char    report_typ[];
char    media_buf[][76];
FILE  **tmp1file, **tmp2file;
char    time_stamp[];
{
int    istat;
char  name[128];


/* assume tape is mounted */
/* build media section of report */

/* open temporary report files */

istat = IMS_OK;
if( strcmp (report_typ,"Brief") != 0){
    strcpy( name, "/tmp/qt1_" ); /* note: qt => qualtmp   */
    strcat( name, time_stamp );
    strcat( name, ".txt" );
    *tmp1file = fopen (name,"w");
    if( tmpfile == NULL){
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Error opening temporary report file #1.\n");
        istat = IMS_ERROR;
    }
}
if( strcmp (report_typ, "Full") == 0){
    strcpy( name, "/tmp/qt2_" ); /* note: qt => qualtmp   */
    strcat( name, time_stamp );
    strcat( name, ".txt" );
    *tmp2file = fopen (name,"w");
    if( tmp2file == NULL){
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Error opening temporary report file #2.\n");
        istat = IMS_ERROR;
    }
}

return(istat);
}   /*  beginmedia   */
