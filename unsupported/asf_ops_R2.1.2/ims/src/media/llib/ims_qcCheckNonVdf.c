static char *sccs = "@(#)ims_qcCheckNonVdf.c	5.3  08/15/97";
/* *******************************************************************
*
*  Name: CheckNonVDF
*
*  Module Type: Procedure     Language: C
*
*  Purpose:
*    This routine reads non-VDF files (e.g. CEOS leader, data, trailer).
*
*  Input Parameters:
*
*   Name           Type        Description
*   numfile         int         Number of the file from start of media
*   numrec          int         Number of record in the file
*   mediafile       int         File descriptor of media file to read
*   fcurr           struct      Pointer to file currently processing
*   recknts         struct      Array of record structures
*   report_typ      char        Type of report (brief,standard,full)
*   title_buf       char        Title section of report
*   hdr_buf         char        Header section of report
*   rptfile         FILE        Pointer to report file
*   tmp2file        FILE        Pointer to temporary file 2
*
*  Output Parameters:
*
*   Name           Type        Description
*   istat           int         Return status -1=error, 0=eof
*
*  Modification History:
*
*  Date:   08 Jan 1990 14:54:20    Revision:   1.0    Author:   CAROL
*  Date:   25 May 1990 12:00:00    Revision:   1.1    Author:   CAROL
*  Date:   18 Jul 1990 10:20:16    Revision:   2.0    Author:   DBMAN
*  Date:   06 Aug 1990 10:49:42    Revision:   2.1    Author:   CAROL
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
#define MAXREC 20
#define MAXLDR 17
#define MAXLEN 40000
#define TITLINES 3
#define MEDIALINES 10
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

int CheckNonVDF (numfile,numrec,mediafile,fcurr,recknts,
    report_typ,title_buf,hdr_buf,rptfile,tmp2file, message,
    media_typ )
int *numfile, *numrec, mediafile;
struct file_str *fcurr;
struct rec_str  recknts[];
char    report_typ[];
char    title_buf[][76];
char    hdr_buf[][76];
FILE    *rptfile, *tmp2file;
char  * message;  /* for error messages  */
char  * media_typ;  /*  for tape or disk flag  */
{

struct rec_str fdrrecknts[MAXLDR];
int istat, mstat, ceosflag;
int     rec_buf[MAXLEN];
int i, j;
long  rsize;
short  is_tape;
static int  numprod;
int newpage, datalines, match;
char    type[4], prvtype[4];
char  * pnt_chars;
long  rec_len_max;

static char data[21][76]=
{"                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             ",
 "                                                              \
             "};

void    CheckFDR(  int *, char *, FILE *, int, int, char *,
        struct rec_str * );
void    MakReport( char *, struct file_str *, struct rec_str *, int,
            char [][], int * );
void    WrtReport( FILE *, int, char [][], int, char [][], int,
            char [][], int );
int     GetRecTyp( int *, int, int *, char *, char * );
int     cvt2int( unsigned char * );
/* ************************************************************* */
/* reset product number */

if(  media_typ[0]  ==  'T' )  is_tape = IMS_TRUE;
else  is_tape = IMS_FALSE;

if( *numfile == 2)  numprod = 0;
if( strcmp (fcurr->f_class, "DAT") == 0) numprod++;
(void) strcpy( prvtype,"   ");

/* read records from non-VDF file */

pnt_chars = (char *) rec_buf;
istat = 1;
rec_len_max = 0;
while  (istat  !=  EOFS){
    if(  is_tape ) istat = read ( mediafile, rec_buf, MAXLEN);
    else istat = read ( mediafile, pnt_chars, 12);

    if( istat > 0){
        (*numrec)++;
        (fcurr->f_numrec)++;

        if(  !is_tape ){ /* read the rest of the buffer  */
            rsize = (long) cvt2int( (unsigned char *) &pnt_chars[8] );
            istat = read( mediafile, pnt_chars+12, rsize-12 );
            if( istat  <=  ERR){
                (fcurr->f_io_err)++;
                if( fcurr->f_io_err > 10){
                    (void) sprintf ( message,
    "Excessive I/O errors encountered while reading non-VDF file.");
                    istat = IMS_ERROR;
                    goto exit;
                }
            }
            istat += 12;
        }
        mstat = GetRecTyp (rec_buf,istat,&ceosflag,type, message );
        if( mstat  <  IMS_OK){
            istat = IMS_ERROR;
            goto exit;
        }

        /* store record counters by type */
        if( strcmp (type, prvtype) != 0){
            i = 0;
            while (i < MAXREC && strcmp (recknts[i].r_class,"   ") != 0)
                i++;

            if( i < MAXREC){
                (void) strcpy( prvtype,type);
                (void) strcpy( recknts[i].r_class,type);
                recknts[i].r_numrec++;
                recknts[i].r_lenrec = istat;
                if(  istat  >  rec_len_max ) rec_len_max = istat;

                if( strcmp (type, "FDR") == 0 && ceosflag == TRUE)
                    CheckFDR (rec_buf,report_typ,tmp2file,
                    fcurr->filenum,numprod,fcurr->f_class,fdrrecknts);
            }
            else{
                (void) sprintf(  message,
            "Too many record types encountered processing file %d.",
                    *numfile);
                istat = IMS_ERROR;
            }
        }
        else{ /* up the number of this record  */
            recknts[i].r_numrec++;
        }
    }
    else if( istat <= ERR ){
        (fcurr->f_io_err)++;
        if( fcurr->f_io_err > 10){
            (void) sprintf(  message,
    "Excessive I/O errors encountered while reading Non-VDF file.");
            istat = IMS_ERROR;
            goto exit;
        }
    }
}

/* at eof check records counters */
istat = IMS_OK;
if( strcmp (fcurr->f_class,"LDR") == 0  ||
    strcmp (fcurr->f_class,"TLR") == 0){
    for (i=0; i < MAXLDR; i++){
        if( fdrrecknts[i].r_numrec != 0){
            match = FALSE;
            for (j=0; j < MAXREC; j++){
                if( strcmp (fdrrecknts[i].r_class,
                    recknts[j].r_class) == 0 ||
                    (strcmp (fdrrecknts[i].r_class,"FAC") == 0 &&
                    (strcmp (recknts[j].r_class, "FSR") == 0 ||
                     strcmp (recknts[j].r_class, "FIM") == 0 ||
                     strcmp (recknts[j].r_class, "FIC") == 0 ||
                     strcmp (recknts[j].r_class, "FWS") == 0))){
                    match = TRUE;
                    if( fdrrecknts[i].r_numrec != recknts[j].r_numrec ||
                    fdrrecknts[i].r_lenrec != recknts[j].r_lenrec){
                        (void) sprintf( message,
    "Rec count and/or length for type %s doesn't match FDR in file %d",
                            recknts[j].r_class, *numfile);
                        istat = IMS_ERROR;
                        goto exit;
                    }
                    break;
                }
            }
            if( match == FALSE) {
                (void) sprintf( message,
                    "Record type %s missing from file.",
                    fdrrecknts[i].r_class);
                istat = IMS_ERROR;
                goto exit;
            }
        }
    }
}
if( istat  >=  IMS_ERROR){
    /*
    **  put in length of largest record as ceos is sometimes off for
    **      the trailer file
    */
    (void) sprintf( fcurr->f_lenrec, "%5d", rec_len_max );
    MakReport (report_typ,fcurr,recknts,MAXREC,data,&datalines);
    newpage = FALSE;
    WrtReport (rptfile,newpage,title_buf,TITLINES,hdr_buf,HDRLINES,data,
                    datalines);
}

exit: return(istat);
}   /*  CheckNonVDF */
