static char *sccs = "@(#)ims_qcWrtReport.c	5.2  03/07/97";
/* ***************************************************************
*
*  Name: WrtReport
*
*  Module Type: Procedure     Language: C
*
*  Purpose:
*  This routine writes the preformatted report sections.
*
*  Input Parameters:
*
*  Name            Type    Description
*   rptfile         FILE    Pointer to report file
*   newpage         int     New page flag (0=same page, 1=new page)
*   title           char    Title section
*   numtit          int     Number of title section lines
*   header          char    Header section
*   numhdr          int     Number of header section lines
*   data            char    Data section
*   numdat          int     Number of data section lines
*
*  Output Parameters:
*
*  Name            Type        Description
*   NONE
*
*  Modification History:
*
*   Date:   18 Jul 1990 10:27:04    Revision:   2.0   Author:   DBMAN
*
***************************************************************** */

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

#define MAXLINES 50

void   WrtReport( rptfile,newpage,title,numtit,header,numhdr,data,
        numdat)
FILE    *rptfile;
int     newpage, numtit, numhdr, numdat;
char    title[][76], header[][76], data[][76];
{

static int  remain = MAXLINES;
static int  pageno = 0;
int  i;
/* ********************************************************* */

/*  if new page - write title and header */

if( newpage == TRUE  ||  numdat > remain){
    remain = MAXLINES;
    pageno++;
    (void) sprintf (title[0]+69,"%3d",pageno);
    if(  pageno   ==  1  )  (void) fprintf (rptfile,"    %s\n",title[0]);
    else  (void) fprintf (rptfile,"\f    %s\n",title[0]);
    for (i= 1; i <= numtit-1; i++)
        (void) fprintf (rptfile,"    %s\n",title[i]);
    (void) fprintf (rptfile," \n");
    remain = remain - (numtit + 1);

    for (i=0; i <= numhdr-1; i++)
        (void) fprintf (rptfile,"    %s\n",header[i]);
    (void) fprintf (rptfile," \n");
    remain = remain - (numhdr + 1);
}

/* write data */

for (i=0; i <= numdat-1; i++)
    (void) fprintf (rptfile,"    %s\n",data[i]);
(void) fprintf (rptfile," \n");
remain = remain - (numdat + 1);

return;
}   /*  WrtReport   */
