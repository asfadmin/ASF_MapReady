static char *sccs = "@(#)ims_qcCheckNdf.c	5.1  03/17/96";
/* ****************************************************************
*
*  Name: CheckNDF
*
*  Module Type: Procedure     Language: C
*
*  Purpose:
*    This routine reads the NDF file.
*
*  Input Parameters:
*
*   Name       Type        Description
*   numfile     int         Number of the file from start of media
*   numrec      int         Number of record in the file
*   mediafile   int         File descriptor of media file to read
*   fileptr     struct      Pointer to first file structure
*   recknts     struct      Array of record structures
*   report_typ  char        Type of report (brief,standard,full)
*   title_buf   char        Title section of report
*   hdr_buf     char        File header section of report
*   rptfile     FILE        Pointer to report file
*
*  Output Parameters:
*
*  Name        Type        Description
*   istat       int         Return status -1=error, 0=eof
*
*  Modification History:
*
*   Date:   08 Jan 1990 14:54:12    Revision:   1.0    Author:   CAROL
*   Date:   18 Jul 1990 10:19:42    Revision:   2.0    Author:   DBMAN
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

#define  EOFS 0
#define ERR -1
#define MAXLEN 10000
#define MAXREC 20
#define TITLINES 3
#define HDRLINES 4

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


int CheckNDF (numfile,numrec,mediafile,fileptr,recknts,
    report_typ,title_buf,hdr_buf,rptfile, message, media_typ)
int *numfile, *numrec, mediafile;
struct file_str *fileptr;
struct rec_str  recknts[];
char    report_typ[];
char    title_buf[][76];
char    hdr_buf[][76];
FILE    *rptfile;
char  * message;  /* for error messages  */
char  * media_typ;  /*  for tape or disk flag  */
{
int istat, mstat, ceosflag;
int     rec_buf[MAXLEN];
int newpage, datalines;
char    type[4];
short  is_tape;
long  rsize;
char  * pnt_chars;

static char data[5][76]=
{"                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             "};

void  MakReport( char *, struct file_str *, struct rec_str *,
        int, char [][], int * );
void  WrtReport( FILE *, int, char [][], int, char [][], int,
        char [][], int );
int  GetRecTyp( int *, int, int *, char *, char * );

/* ************************************************************* */

/* Overlay fields of VDF file structure with NDF fields */

fileptr->filenum = *numfile;
(void) strcpy( fileptr->line_id,"  ");
(void) strcpy( fileptr->prod_id,"              ");
(void) strcpy( fileptr->f_class,"NDF");
(void) strcpy( fileptr->f_cont,"No");
(void) strcpy( fileptr->rectyp,"MIXED");
(void) strcpy( fileptr->recform,"VAR");
(void) strcpy( fileptr->f_lenrec,"  360");
fileptr->f_numrec = 0;
fileptr->f_io_err = 0;
fileptr->fnext = NULL;

istat = 1;

/* read records from NDF file */

pnt_chars = (char *) rec_buf;
if(  media_typ[0]  ==  'T' )  is_tape = IMS_TRUE;
else  is_tape = IMS_FALSE;

while  (istat != EOFS){
    if(  is_tape ) istat = read ( mediafile, rec_buf, MAXLEN);
    else istat = read ( mediafile, pnt_chars, 12);

    if( istat > 0)  {
        (*numrec)++;
        (fileptr->f_numrec)++;

        if(  !is_tape ){ /* read the rest of the buffer  */
            rsize = cvt2int( (unsigned char *) &pnt_chars[8] );
            istat = read( mediafile, pnt_chars+12, rsize-12 );
            if( istat  <=  ERR){
                (fileptr->f_io_err)++;
                if( fileptr->f_io_err > 10){
                    (void) sprintf ( message,
    "Excessive I/O errors encountered while reading NDF file.");
                    istat = IMS_ERROR;
                    goto exit;
                }
            }
            istat += 12;
        }

        mstat = GetRecTyp (rec_buf,istat,&ceosflag,type,message);
        if( mstat <  IMS_OK ) {
            istat = mstat;
            goto exit;
        }

        if( strcmp (type,"NUL") == 0){
            (void) strcpy( recknts[0].r_class,"NUL");
            recknts[0].r_numrec++;
            recknts[0].r_lenrec = istat;
        }
        else{
            (void) strcpy( recknts[1].r_class,"UNK");
            recknts[1].r_numrec++;
            recknts[1].r_lenrec = istat;
        }
    }
    else if( istat  <=  ERR){
        (fileptr->f_io_err)++;
        if( fileptr->f_io_err > 10){
            (void) sprintf ( message,
        "Excessive I/O errors encountered while reading NDF file.");
            istat = IMS_ERROR;
            goto exit;
        }
    }
}
istat = IMS_OK;
MakReport (report_typ,fileptr,recknts,MAXREC,data,&datalines);
newpage = FALSE;
WrtReport (rptfile,newpage,title_buf,TITLINES,hdr_buf,HDRLINES,data,
                datalines);

/* at eof check records counters */
/* must have NUL to continue */

if( recknts[0].r_numrec == 0){
    (void) sprintf ( message,
        "NDF file has no NUL record - quality check will continue.");
    istat = IMS_ERROR;
}

exit: return(istat);
}   /*  CheckNDF  */
