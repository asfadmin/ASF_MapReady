static char *sccs = "@(#)ims_qcMakeReport.c	5.1  03/17/96";
/* *****************************************************************
*
*  Name: MakReport
*
*  Module Type: Procedure     Language: C
*
*  Purpose:
*  This routine formats the file and associated record structures prior
*  to writing to the quality report.
*
*  Input Parameters:
*
*  Name            Type        Description
*   report_typ      char        Type of report (brief,standard,full)
*   fileptr         struct      Pointer to file structure
*   recknts         struct      Array of record structures
*   maxrec          int         Number of record structures
*
*  Output Parameters:
*
*  Name             Type        Description
*   data                char            Data section of the report
*   datalines       int         Number of data lines
*
*  Modification History:
*
*  Date:   18 Jul 1990 10:24:26    Revision:   2.0   Author:   DBMAN
*           1/4/96 - changed data printed: prod_id now 16 chars, no
*               split.  David Pass.   R1b'
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

struct rec_str {        /* record structure template */
    int r_numrec;
    short   r_io_err;
    char    r_class[4];
    int r_lenrec;
};


void    MakReport (report_typ,fileptr,recknts,maxrec,data,datalines)
char    report_typ[];
struct file_str *fileptr;
struct rec_str  recknts[];
int maxrec, *datalines;
char    data[][76];
{

int i;
char    temp[6];

/* **************************************************************** */

/* format fields of file structure */

(void) sprintf (temp,"%3d",fileptr->filenum);
(void) strncpy( data[0],temp,strlen(temp));

(void) strncpy( data[0]+5,fileptr->line_id,strlen(fileptr->line_id));
/* prod_id may now be up to 16 characters   */
(void) strncpy( data[0]+10,fileptr->prod_id,strlen(fileptr->prod_id));
(void) strncpy( data[0]+31,fileptr->f_class,strlen(fileptr->f_class));
/* *******  split info (Yes or No) taken out
    (void) strncpy( data[0]+31,fileptr->f_cont,strlen(fileptr->f_cont));
*/
(void) strncpy( data[0]+36,fileptr->rectyp,strlen(fileptr->rectyp));
(void) strncpy( data[0]+43,fileptr->recform,strlen(fileptr->recform));
(void) strncpy( data[0]+48,fileptr->f_lenrec,strlen(fileptr->f_lenrec));

(void) sprintf (temp,"%5d",fileptr->f_numrec);
(void) strncpy( data[0]+54,temp,strlen(temp));

(void) sprintf (temp,"%4d",fileptr->f_io_err);
(void) strncpy( data[0]+60, temp, strlen(temp));
*datalines = 1;

/* format fields of record structure */

if( strcmp (report_typ,"Brief") != 0){
    for (i=1; i <= maxrec; i++){
        if( strcmp (recknts[i-1].r_class,"   ") != 0){
            (void) sprintf (temp,"%5d",recknts[i-1].r_numrec);
            (void) strncpy( data[*datalines]+54, temp, strlen(temp));
            (void) sprintf (temp,"%4d",recknts[i-1].r_io_err);
            (void) strncpy( data[*datalines]+60, temp, strlen(temp));
            (void) strncpy( data[*datalines]+66,recknts[i-1].r_class,
                            strlen(recknts[i-1].r_class));
            (void) sprintf (temp, "%5d", recknts[i-1].r_lenrec);
            (void) strncpy( data[*datalines]+70, temp, strlen(temp));
            (*datalines)++;
        }
    }
}

return;
}   /*  MakReport   */
