static char *sccs = "@(#)ims_qcCheckFpr.c	5.1  03/17/96";
/* ****************************************************************
*
*  Name: CheckFPR
*
*  Module Type: Procedure     Language: C
*
*  Purpose:
*  This routine extracts information from FPR records.
*
*  Input Parameters:
*
*  Name            Type            Description
*   rec_buf         int         Record buffer
*   fileptr         struct      Pointer to first file structure
*   tot_files       int         Number of files on this media
*
*  Output Parameters:
*
*  Name             Type        Description
*   NONE
*
*  Modification History:
*
*  Date:   21 Nov 1989 02:50:00    Revision:   1.0    Author:   CAROL
*  Date:   18 Jul 1990 10:19:12    Revision:   2.0    Author:   DBMAN
*  Date:   06 Aug 1990 10:45:02    Revision:   2.1    Author:   CAROL
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

void    CheckFPR (rec_buf,fileptr,tot_files)

int rec_buf[];
struct file_str *fileptr;
int *tot_files;

{
struct fprrec {
    char    info[360];
    };
struct fprrec *fpr;

static struct file_str *fcurr;

char    temp[9]="        ";
int itemp1, itemp2;

/********************************************************************/

fpr = (struct fprrec *) rec_buf;

/* check that FPR points to file on this media */

(void) strncpy (temp,fpr->info+144,8);
(void) sscanf (temp,"%d",&itemp1);
(void) strncpy (temp,fpr->info+152,8);
(void) sscanf (temp,"%d",&itemp2);
/***** printf ("Itemp1, itemp2: %d,  %d\n",itemp1,itemp2); *****/

if( itemp1 != 0 && itemp2 != 0){

    /* link and allocate file structure */
    (*tot_files)++;
    if( *tot_files <= 2) fcurr = fileptr;
    fcurr->fnext = malloc (sizeof(struct file_str));
    fcurr = fcurr->fnext;

    /* build file structure from fields within FPR record */

    fcurr->filenum = *tot_files;

    (void) strncpy (fcurr->line_id, fpr->info+260, 2);
    (void) strncpy (fcurr->line_id+2,"\0",1);
    (void) strncpy (fcurr->prod_id, fpr->info+20, 16);
    (void) strncpy (fcurr->prod_id+16,"\0",1);

    if( strncmp (fpr->info+64, "SARL",4) == 0)
        (void) strcpy( fcurr->f_class, "LDR");
    if( strncmp (fpr->info+64, "IMOP",4) == 0)
        (void) strcpy( fcurr->f_class, "DAT");
    if( strncmp (fpr->info+64, "SART",4) == 0)
        (void) strcpy( fcurr->f_class, "TLR");

    if( strncmp (fpr->info+140, fpr->info+142, 2) == 0)
        (void) strcpy( fcurr->f_cont, "No ");
    else
        (void) strcpy( fcurr->f_cont, "Yes");

    if( strncmp (fpr->info+96, "MBAA",4) == 0)
        (void) strcpy( fcurr->rectyp, "MIXED");
    if( strncmp (fpr->info+96, "BINO",4) == 0)
        (void) strcpy( fcurr->rectyp, "BINRY");
    if( strncmp (fpr->info+96, "COMP",4) == 0)
        (void) strcpy( fcurr->rectyp, "CMPLX");
    if( strncmp (fpr->info+96, "REAL",4) == 0)
        (void) strcpy( fcurr->rectyp, "REAL ");

    (void) strncpy (fcurr->recform, fpr->info+136, 3);
    (void) strncpy (fcurr->recform+3,"\0",1);

    (void) strncpy (fcurr->f_lenrec, fpr->info+119, 5);
    (void) strncpy (fcurr->f_lenrec+5,"\0",1);

    fcurr->f_numrec = 0;
    fcurr->f_io_err = 0;
    fcurr->fnext = NULL;
}

return;
}   /*  checkFPR  */
