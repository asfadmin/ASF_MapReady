static char *sccs = "@(#)ims_qcCheckVdr.c	5.1  03/17/96";
/* *******************************************************************
*
*  Name: CheckVDR
*
*  Module Type: Procedure     Language: C
*
*  Purpose:
*  This routine extracts information from VDR records.
*
*  Input Parameters:
*
*   Name       Type        Description
*   rec_buf     int         Record buffer
*   media_buf   char        Media section of report
*
*  Output Parameters:
*
*   Name           Type        Description
*   numrec_vdf      int         Number of VDF records (field in VDR)
*   numrec_fpr      int         Number of FPR records (field in VDR)
*
*  Modification History:
*
*  Date:   08 Jan 1990 14:54:50    Revision:   1.0    Author:   CAROL
*  Date:   18 Jul 1990 10:21:54    Revision:   2.0    Author:   DBMAN
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

int   CheckVDR (rec_buf,media_buf,numrec_vdf,numrec_fpr)
int rec_buf[], *numrec_vdf, *numrec_fpr;
char    media_buf[][76];
{

struct vdrrec {
    char    info[360];
    };
struct vdrrec *vdr;

char    count[11];

/* ************************************************************** */

vdr = (struct vdrrec *) rec_buf;

/* order id  :  note that getQualInfo used to be called here, but
    changed to ims_qcCheckFile  */

(void) strncpy (media_buf[0]+10, vdr->info+60, 16);

/* creation date, media id, CEOS document, media sequence */

(void) strncpy (media_buf[7]+22, vdr->info+116, 2);
(void) strncpy (media_buf[7]+24, "-", 1);
(void) strncpy (media_buf[7]+25, vdr->info+118, 2);
(void) strncpy (media_buf[7]+27, "-", 1);
(void) strncpy (media_buf[7]+28, vdr->info+112, 4);

/*
**  mediaId may be input: check
*/
if(  media_buf[3][12]  ==  ' ' ){
    (void) strncpy (media_buf[3]+11, vdr->info+44, 16);
}

/*
**  this taken out:  CEOS:  CCB-CCT-0002-E   is an example
**  (void) strncpy (media_buf[8]+54, vdr->info+16, 14);
**  (void) strncpy (media_buf[8]+66, "-", 1);
*/

(void) strncpy (media_buf[6]+16, vdr->info+98, 2);
(void) strncpy (media_buf[6]+19, "of", 2);
(void) strncpy (media_buf[6]+22, vdr->info+92, 2);

/* save number of VDF and FPR records */

(void) strcpy( count, "          ");
(void) strncpy (count,vdr->info+164,4);
(void) sscanf (count,"%d",numrec_vdf);

(void) strncpy (count,vdr->info+160,4);
(void) sscanf (count,"%d",numrec_fpr);

return ( IMS_OK);
}   /*  CheckVDR   */
