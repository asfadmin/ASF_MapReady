static char *sccs = "@(#)ims_qcCheckTxt.c	5.1  03/17/96";
/* ******************************************************************
*
*  Name: CheckTXT
*
*  Module Type: Procedure     Language: C
*
*  Purpose:
*  This routine extracts information from TXT records.
*
*  Input Parameters:
*
*   Name            Type        Description
*   tmp1file        FILE        Pointer to temporary file 1
*   rec_buf         int         Record buffer
*   numprod         int         Product number on this media
*   fileptr         struct      Pointer to first file structure
*   media_buf       char        Media section of report
*
*  Output Parameters:
*
*  Name             Type        Description
*   NONE
*
*  Modification History:
*
*  Date:   21 Nov 1989 04:55:00    Revision:   1.0    Author:   CAROL
*  Date:   18 Jul 1990 10:20:50    Revision:   2.0    Author:   DBMAN
*  Date:   06 Aug 1990 10:47:12    Revision:   2.1    Author:   CAROL
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

struct  file_str {              /* file structure template */
    short   filenum;
    char    line_id[3];
    char    prod_id[20];
    char    f_class[4];
    char    f_cont[4];
    char    rectyp[6];
    char    recform[4];
    char    f_lenrec[6];
    int f_numrec;
    short   f_io_err;
    struct file_str *fnext;
};


void  CheckTXT( tmp1file,rec_buf,numprod,fileptr,media_buf)
FILE    *tmp1file;
int     rec_buf[];
int     numprod;
struct file_str  *fileptr;
char    media_buf[][76];
{

char    temp[76];
struct txtrec {
    char    info[360];
    };
struct txtrec *txt;

static struct file_str *fcurr;

/* *********************************************************** */

txt = (struct txtrec *) rec_buf;

if( numprod == 1)
    fcurr = fileptr;

/* find next DAT file in file structure and extract filenum */

do{
    fcurr = fcurr->fnext;
    } while (strcmp (fcurr->f_class, "DAT") != 0  &&
            fcurr->fnext != NULL);


/* build and write text records from fields within TXT record */

(void) strncpy (temp, txt->info+16, 40);
(void) strncpy (temp+40, "\0", 1);
(void) fprintf (tmp1file,"Product: %3d  %s\n",numprod,temp);

(void) strncpy (temp, txt->info+56, 60);
(void) strncpy (temp+60, "\0", 1);
(void) fprintf (tmp1file,"File:    %3d  %s\n",fcurr->filenum,temp);

(void) strncpy (temp, txt->info+116, 40);
(void) strncpy (temp+40, "\0", 1);
(void) fprintf (tmp1file,"              %s\n",temp);

(void) strncpy (temp, txt->info+156, 40);
(void) strncpy (temp+40, "\0", 1);
(void) fprintf (tmp1file,"              %s\n",temp);

(void) strncpy (temp, txt->info+196, 40);
(void) strncpy (temp+40, "\0", 1);
(void) fprintf (tmp1file,"              %s\n  \n",temp);


return;
}   /*  CheckTXT  */
