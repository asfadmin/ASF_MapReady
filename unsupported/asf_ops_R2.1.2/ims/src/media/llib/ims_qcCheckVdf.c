static char *sccs = "@(#)ims_qcCheckVdf.c	5.1  03/17/96";
/* ***************************************************************
*
*  Name: CheckVDF
*
*  Module Type: Procedure     Language: C
*
*  Purpose:
*    This routine reads the VDF file.
*
*  Input Parameters:
*
*   Name        Type        Description
*   numfile     int         Number of the file from start of media
*   numrec      int         Number of record in the file
*   mediafile   int         File descriptor of media file to read
*   media_typ   char        Type of media (tape, disk, next)
*   fileptr     struct      Pointer to first file structure
*   recknts     struct      Array of record structures
*   report_typ  char        Type of report (brief,standard,full)
*   title_buf   char        Title section of report
*   rptfile     FILE        Pointer to report file
*   tmp1file    FILE        Pointer to temporary file 1
*
*  Output Parameters:
*
*   Name        Type        Description
*   media_buf   char        Media section of report
*   hdr_buf     char        File header section of report
*   tot_files   int         Total files on this media
*   istat       int         Return status -1=error, 0=eof
*
*  Modification History:
*
*  Date:   08 Jan 1990 14:54:40    Revision:   1.0   Author:   CAROL
*  Date:   25 May 1990 12:00:00    Revision:   1.1   Author:   CAROL
*  Date:   18 Jul 1990 10:21:22    Revision:   2.0   Author:   DBMAN
*  Date:   06 Aug 1990 10:42:52    Revision:   2.1   Author:   CAROL
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

#define ERR -1
#define EOFS  0
#define MAXLEN 65532
#define MAXREC 20
#define TITLINES 3
#define MEDIALINES 9
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


int CheckVDF (msgDesc,numfile,numrec,mediafile,media_typ,fileptr,
        recknts,report_typ,title_buf,media_buf,hdr_buf,rptfile,
        tmp1file,tot_files )
IMS_MSG_STRUCT  * msgDesc;
int *numfile, *numrec, mediafile, *tot_files;
char    media_typ[];
struct file_str *fileptr;
struct rec_str  recknts[];
char    report_typ[];
char    title_buf[][76];
char    media_buf[][76];
char    hdr_buf[][76];
FILE    *rptfile, *tmp1file;
{

int istat, mstat, ceosflag;
int     rec_buf[MAXLEN/4];
int numrec_vdf, numrec_fpr;
int numprod;
int newpage, datalines;
char    message[256]; /* for error message from getrectyp  */
char    type[4];
short  rsize;
short  is_tape;
char  * pnt_chars;
char  str[128];
long   i,j;
char    file_name[129]; /* file name fir disk files  */
char    media_type_n[33];
char    media_fmt_type_n[33];
char    username[33], acctid[13], media_id[33];
short   vdr_read;
int  istat2;

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

int     CheckVDR( int *, char [][], int *, int * );
void    CheckFPR( int *, struct file_str *, int *);
void    CheckTXT( FILE *, int *, int,  struct file_str *, char [][] );
void    MakReport( char *, struct file_str *, struct rec_str *, int,
            char [][], int * );
void    WrtReport( FILE *, int, char [][], int, char [][], int,
            char [][], int );
int  cvt2int( unsigned char *);
int     GetRecTyp( int *, int, int *, char *, char * );
int    getQualInfo(  IMS_MSG_STRUCT *, long, long, char *, char *,
    char  *, char *, char *, char * );


/* *************************************************************** */

/* fill in fields of file structure for file type of VDF */

fileptr->filenum = *numfile;
(void) strcpy( fileptr->line_id,"  ");
(void) strcpy( fileptr->prod_id,"              ");
(void) strcpy( fileptr->f_class,"VDF");
(void) strcpy( fileptr->f_cont,"No");
(void) strcpy( fileptr->rectyp,"MIXED");
(void) strcpy( fileptr->recform,"VAR");
(void) strcpy( fileptr->f_lenrec,"  360");
fileptr->f_numrec = 0;
fileptr->f_io_err = 0;
fileptr->fnext = NULL;


if(  strcmp( media_typ, "Tape" )  ==  0  )  is_tape = IMS_TRUE;
else  is_tape = IMS_FALSE;

numprod = 0;
istat = 1;
*tot_files = 1;
pnt_chars = (char *) rec_buf;
vdr_read = 0;

/* read records from VDF file */
while  (istat  !=  EOFS ){
    if(  is_tape  )  istat = read( mediafile, rec_buf, MAXLEN );
    else istat = read ( mediafile, pnt_chars, 12);
    if( istat > 0) {
        (*numrec)++;
        (fileptr->f_numrec)++;

        if(  !is_tape  ){ /* read the rest of the file  */
            rsize = cvt2int( (unsigned char *) &pnt_chars[8] );
            istat = read( mediafile, pnt_chars+12, rsize-12 );
            if(  istat <=  ERR ){
                (fileptr->f_io_err)++;
                if( fileptr->f_io_err > 10){
                    (void) ims_msg( msgDesc, IMS_ERROR,
    "Excessive I/O errors encountered while reading VDF file.\n");
                    istat = IMS_ERROR;
                    goto exit;
                }
            }
            istat += 12;
        }
        mstat = GetRecTyp (rec_buf,istat,&ceosflag,type,message);
        if( mstat <  IMS_OK ){
            (void)  ims_msg( msgDesc, IMS_ERROR, message );
            istat = mstat;
            goto exit;
        }
        if( strcmp (type,"VDR") == 0){
            (void) strcpy( recknts[0].r_class,"VDR");
            recknts[0].r_numrec++;
            recknts[0].r_lenrec = istat;
            mstat = CheckVDR (rec_buf, media_buf, &numrec_vdf,
                &numrec_fpr);
            if( mstat  <  IMS_OK ){
                istat = IMS_ERROR;
                goto exit;
            }
            vdr_read = 1;
        }
        else if( strcmp (type,"FPR") == 0){
            (void) strcpy( recknts[1].r_class,"FPR");
            recknts[1].r_numrec++;
            recknts[1].r_lenrec = istat;
            CheckFPR (rec_buf,fileptr,tot_files);
        }
        else if( strcmp (type,"TXT") == 0){
            (void) strcpy( recknts[2].r_class,"TXT");
            recknts[2].r_numrec++;
            recknts[2].r_lenrec = istat;
            numprod++;
            if( strcmp (report_typ,"Brief") != 0)
               CheckTXT (tmp1file, rec_buf,numprod,fileptr,media_buf);
        }
        else{
            (void) strcpy( recknts[3].r_class,"UNK");
            recknts[3].r_numrec++;
            recknts[3].r_lenrec = istat;
        }
    }
    else if( istat <= ERR){ /* error from read  */
        (fileptr->f_io_err)++;
        if( fileptr->f_io_err > 10){
            (void) ims_msg( msgDesc, IMS_ERROR,
        "Excessive I/O errors encountered while reading VDF file.\n");
            istat = IMS_ERROR;
            goto exit;
        }
    }
}

istat = IMS_OK;
/* at eof check records counters */
/* must have VDR and at least one FPR record to continue */

if( recknts[0].r_numrec == 0) {
    (void) ims_msg( msgDesc, IMS_ERROR,
    "VDF file has no VDR record - unable to continue quality check.");
    istat = IMS_ERROR;
}
else if( fileptr->f_numrec != numrec_vdf){
    (void) ims_msg( msgDesc, IMS_ERROR,
"Count of VDF records does not match count stored in VDR record.");
    istat = IMS_ERROR;
}

if( recknts[1].r_numrec == 0){
    (void) ims_msg( msgDesc, IMS_ERROR,
"VDF file has no FPR records - unable to continue quality check.");
    istat = IMS_ERROR;
}
else if( recknts[1].r_numrec != numrec_fpr){
    (void) ims_msg( msgDesc, IMS_ERROR,
"Count of FPR records does not match count stored in VDR record.");
    istat = IMS_ERROR;
}

if( istat  !=  IMS_ERROR ){
    if(  vdr_read ){
        /* do not need this every time: need to get header
            info once.  this is not necessary to the running
            of the program, but it would be nice to know.
            do not end program if error.  note: QualInfo also
            called in CheckFile for non-tape cases.
            Note: done here so that the first header has the
            info, but we need the first line_id (item_id) value
            for the sql statement.  */
        i = atol( fileptr->fnext->line_id ); /* also known as
            item_id  */
        /* need to get order id */
        (void) strncpy (str, media_buf[0]+10, 10);
        str[10] = '\0';
        (void) sscanf (str, "%ld", &j );/* order_id */

        istat2 = getQualInfo( msgDesc, j, i, username,
            acctid, str, media_type_n,
            media_fmt_type_n, media_id );
        if(  istat2  >=  IMS_OK  ){ /* sucessful: use the
            data  */
            (void) strncpy (media_buf[2]+12, acctid, 12);
            (void) strncpy (media_buf[1]+12, username, 32);
            /*
            **  media_id may be input to ims_qc.  if not,
            **      then this value is used.
            */
            if(  media_buf[3][12]  ==  ' ' )
                (void) strncpy (media_buf[3]+11, media_id,
                strlen( media_id ));
            (void) strncpy (str, media_buf[3]+11,16);
            (void) strncpy (str+16, "\0", 1);
            (void) ims_truncStr( str );
            (void) ims_msg( msgDesc, IMS_INFO,
            "Checking order %ld, media %s, accnt %s for %s",
                j, str, acctid, username);
            /* put media values into header  */
            (void) strncpy( media_buf[4]+13, media_type_n,
                strlen( media_type_n ) );
            (void) strncpy( media_buf[5]+15,
                media_fmt_type_n,
                strlen( media_fmt_type_n ) );
            if(  strcmp( media_fmt_type_n, "CEOS" )  ==  0 )
                (void) strncpy( media_buf[8]+25, "CEOS_reader",
                    11 );
            else  if( strcmp( media_fmt_type_n, "TAR" )  ==  0 )
                (void) strncpy( media_buf[8]+25,
                    "tar xvf /dev/rmt/0", 18 );
        }
        else{
            if(  istat2  ==  IMS_ERROR ){ /* error in database */
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Error reading database in getQualInfo." );
                return( IMS_ERROR );
            }
            else{
                (void) ims_msg( msgDesc, IMS_INFO,
        "No catalog entry for this tape:  order %ld, item %ld.",
                    j, i );
            }
        }
        newpage = TRUE;
        WrtReport (rptfile,newpage,title_buf,TITLINES,media_buf,
            MEDIALINES,hdr_buf,HDRLINES);
    }
    MakReport (report_typ,fileptr,recknts,MAXREC,data,&datalines);
    newpage = FALSE;
    WrtReport (rptfile,newpage,title_buf,TITLINES,hdr_buf,HDRLINES,data,
                    datalines);
}

exit:;
return(istat);
}   /*  CheckVDF  */
