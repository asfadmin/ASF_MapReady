static char *sccs = "@(#)ims_qcCheckFile.c	5.1  03/17/96";
/* **************************************************************
*
*  Name: CheckFile
*
*  Module Type: Procedure     Language: C
*
*  Purpose:
*    This routine opens/closes each file and calls processing routine.
*
*  Input Parameters:
*
*  Name        Type        Description
*  numfile      int         Number of the file from start of media
*  numrec       int         Number of record in the file
*  report_typ   char        Type of report (brief,standard,full)
*  media_typ    char        Type of media (tape,disk,next)
*  title_buf    char        Title section of report
*  rptfile      int         Pointer to report file
*  tmp1file     int         Pointer to temporary file 1
*  tmp2file     int         Pointer to temporary file 2
*
*  Output Parameters:
*
*  Name        Type        Description
*   tot_files   int         Total number of files to check on media
*   media_buf   char        Media section of report
*   istat       int         Return status, SS$_ABORT
*
*  Modification History:
*
*  Date:   08 Jan 1990 14:53:56    Revision:   1.0    Author:   CAROL
*  Date:   29 May 1990 11:43:00    Revision:   1.1    Author:   DBMAN
*  Date:   18 Jul 1990 10:18:18    Revision:   2.0    Author:   DBMAN
*  Date:   06 Aug 1990 10:40:24    Revision:   2.1    Author:   CAROL
*
*************************************************************** */
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

/* **** usually defined in sys/types, but ifndef with
    if defined(_KERNEL) || !defined(_POSIX_C_SOURCE)
    is used and is skipped  */

typedef unsigned char     u_char;
typedef unsigned short    u_short;

#include    <sys/ioctl.h>
#include    <sys/mtio.h>

/*  #define  EOF 0      */
#define ERR -1
#define MAXREC 20
#define MAXDIR 760
#define MAXLEN 65532

int CheckFile ( msgDesc, path, numfile, numrec, report_typ,
    media_typ, tot_files, title_buf, media_buf, rptfile,
    tmp1file, tmp2file, line_id, archive_check )
IMS_MSG_STRUCT  * msgDesc;
char *  path;
int     *numfile, *numrec, *tot_files;
char    report_typ[], media_typ[];
char    title_buf[][76];
char    media_buf[][76];
FILE    *rptfile, *tmp1file, *tmp2file;
char    *line_id;
int     archive_check; /* if true, go to archive to check
            disk files  */
{

int istat;
long i,j;
char  str[256];
short  flag;
static  char    dir_buf[MAXDIR+1];
short   reading_tape; /*  if true, reading tape  */
char    message[256];
char    file_name[129]; /* file name fir disk files  */
int     status;
char    media_type_n[33];
char    media_fmt_type_n[33];

typedef struct  file_str__t *pnt_file_str_t;
typedef struct  file_str__t{       /* file structure template */
    short   filenum;
    char    line_id[3];
    char    prod_id[20];
    char    f_class[4];
    char    f_cont[4];
    char    rectyp[6];
    char    recform[4];
    char    f_lenrec[6];
    int     f_numrec;
    short   f_io_err;
    struct file_str__t  *fnext;
} file_str_t;

static file_str_t *fileptr;
static file_str_t *fcurr;
static  char  last_path[33] = ""; /* last path: for each line_id */
static  short  last_line_id = -1; /* last line_id (item_id) */
char    username[33], acctid[13], media_id[33];

typedef struct rec_str__t *pnt_rec_str_t;
typedef struct rec_str__t {        /* record structure template */
    int r_numrec;
    short   r_io_err;
    char    r_class[4];
    int r_lenrec;
} rec_str_t;

static rec_str_t   recknts[MAXREC];

static char hdr_buf[4][76] =
{"                                               Max.         No.   \
         ",
 "File Line Product             File  Rec.  Rec. Rec.   No.   I/O  R\
ec.  Rec.",
"No.  Id.  Id.                 Class Type  Form Len.   Rec.  Err. Cl\
ass Len.",
"----|----|-------------------|-----|-----|----|------|-----|----|--\
---|----"};

static  int  tdesc;
struct  mtop   mt_command; /*  ioctl command structure  */
struct  mtget  mt_status; /*  ioctl results structure  */

int  CheckVDF( IMS_MSG_STRUCT *, int *, int *, int, char *,
    pnt_file_str_t, pnt_rec_str_t, char *, char [][],
    char [][],  char [][], FILE *, FILE *, int * );

int  CheckNDF( int *, int *, int, pnt_file_str_t,
    pnt_rec_str_t, char *, char [][], char [][], FILE *, char *,
    char * );

int  CheckNonVDF( int *, int *, int, pnt_file_str_t,
    pnt_rec_str_t, char *, char [][], char [][], FILE *, FILE *,
    char *, char * );

int    getQualInfo(  IMS_MSG_STRUCT *, long, long, char *, char *,
    char  *, char *, char *, char * );

/* ***************************************************************** */


/* allocate first file structures */

if( *numfile == 1) fileptr = malloc (sizeof( file_str_t ));

line_id[0] = '\0';
/* open media file */
if( strcmp (media_typ,"Tape") == 0)  reading_tape = TRUE;
else  reading_tape = FALSE;
if( reading_tape ){     /* tape */
    if( *numfile == 1)                      /* VDF file */
        fcurr = fileptr;
    else                                            /* non-VDF files */
        fcurr = fcurr->fnext;

    /* only need to open for the first file  */
    if(  *numfile  ==  1  ){
        tdesc = open( path, O_RDONLY );
        if(  tdesc  ==  -1  ){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "** Error opening tape %s:  %s\n",
                path, strerror( errno ) );
            return( IMS_ERROR );
        }
        /* need to make sure it is rewound  */
        mt_command.mt_op = MTREW;
        mt_command.mt_count = 1;
        (void) ioctl( tdesc, MTIOCTOP, &mt_command );
        (void) ioctl( tdesc, MTIOCGET, (char *)&mt_status );
        if(  mt_status.mt_erreg  <  0  ){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "** Error rewinding tape %s:  %s\n",
                path, strerror( errno ) );
            return ( IMS_ERROR );
        }
        last_line_id = -1;
    }
    else{ /* need to skip over the end-of-file mark.  */
        mt_command.mt_op = MTFSF;
        mt_command.mt_count = 1;
        (void) ioctl( tdesc, MTIOCTOP, &mt_command );
        (void) ioctl( tdesc, MTIOCGET, (char *)&mt_status );
        if(  mt_status.mt_erreg  <  0  ){
            ims_msg( msgDesc, IMS_ERROR,
                " *****  Error forwarding tape: %s  ****",
                strerror( errno ) );
            return ( IMS_ERROR );
        }
    }
}
else{  /* disk files:  first file is vdf file: name is input.
    after that, names are obtained from the vdf files.  the
    order id and line id values are used.  this version assumes
    that all the files are in the same directory as path */
    if( *numfile == 1){                      /* VDF file */
        fcurr = fileptr;
        (void) strcpy( file_name, path );/* first file name */
    }
    else{                                  /* non-VDF files */
        fcurr = fcurr->fnext;
        /* have to handle the null vdf file differently.
            the fcurr pointer is null in this cases, and
            the name is the same as the vdf file with the
            .V changed to .N.  other cases, use the fcurr
            info  */
        (void) strcpy( file_name, path );
        if( *numfile == *tot_files  ){ /* NDF file  */
            i = ims_strIndex( path, ".V" );
            file_name[i+1] = 'N';
        }
        else{
            i = atol( fcurr->line_id ); /* also known as
                item_id  */
            if(  i  ==  last_line_id  )
                (void) strcpy( file_name, last_path );
            else  if( archive_check ){ /* need new path: do
                database query. need to get order id */
                (void) strncpy (str, media_buf[0]+10, 10);
                str[10] = '\0';
                (void) sscanf (str, "%ld", &j );/* order_id */

                istat = getQualInfo( msgDesc, j, i, username,
                    acctid, last_path, media_type_n,
                    media_fmt_type_n, media_id );
                if(  istat  <  IMS_OK  ){
                    (void) ims_msg (msgDesc, IMS_ERROR,
                      "There is no data for order id %d, item id %d.",
                        j, i );
                    return( IMS_ERROR );
                }
            }
            /* two cases:  use same directory as vdf file
                (archive_check = false), or go to path
                in directory to check.  assume the name
                is the same.  */
            if(  archive_check  ){
                /*  get the path from the database:  only
                    for ldr,tlr, or dat files.  */
                (void) strcpy( file_name, last_path );
            }
            else{
                /* find last slash in path: then add
                    order id, .extension (f_type) */
                i = strlen( file_name );/* file_name is path  */
                flag = IMS_TRUE;
                for( j=i-1 ; j  >= 0 && flag ; j-- )
                    if(  file_name[j]  ==  '/' )  flag = IMS_FALSE;
                if(  !flag )  file_name[j+1] = '\0';
                else  file_name[0] = '\0'; /* no slashes */
            }


            if(  file_name[0]  !=  '\0' )
                (void) strcat( file_name, "/" );
            (void) strcat( file_name, ims_truncStr(
                fcurr->prod_id ) );
            (void) strcat( file_name, "." );
            /* only use first character of f_class. */
            (void) strcpy( str, ims_truncStr(
                fcurr->f_class ) );
            str[1] = '\0';
            (void) strcat( file_name, str );
        }
    }
    if(  *numfile  !=  1  ){
        /* close the last file checked  */
        status = close( tdesc );
        if(  status  ==  -1  ){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "** Error closing disk file before %s: %s",
                file_name, strerror( errno ) );
            return( IMS_ERROR );
        }
    }
    tdesc = open( file_name, O_RDONLY );
    if(  tdesc  ==  -1  ){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "** Error opening disk file %s:  %s",
            file_name, strerror( errno ) );
        return( IMS_ERROR );
    }
}

if( *numfile == *tot_files ){
    /*  NDF file: null directory file  */
    /* search for .V and change to .N */
    i = strlen (dir_buf);
    while (strncmp (dir_buf+i,".",1) != 0  &&  i > 0)
        i--;
    (void) strncpy (dir_buf+(i+1),"N",1);
}

/* initialize record structure */
for (i=0; i < MAXREC; i++){
    recknts[i].r_numrec = 0;
    recknts[i].r_io_err = 0;
    (void) strcpy( recknts[i].r_class,"   ");
    recknts[i].r_lenrec = 0;
}

if( *numfile == 1){
    istat = CheckVDF( msgDesc,numfile,numrec,tdesc,
        media_typ,fileptr,recknts,report_typ,title_buf,
        media_buf,hdr_buf,rptfile,tmp1file,tot_files);

    if( istat <  IMS_OK ){
        return ( IMS_ERROR );
    }

    /* tot_files = # FPR on this volume + 1 VDF + 1 NDF */
    (*tot_files)++;
}
else if( *numfile == *tot_files ){
    istat = CheckNDF(numfile,numrec,tdesc,fileptr,
        recknts, report_typ,title_buf,hdr_buf,rptfile,message,
        media_typ );
    if( istat  <  IMS_OK  ){
        (void) ims_msg( msgDesc, IMS_ERROR,
            message );
        return ( IMS_ERROR );
    }
    status = close( tdesc );
    if(  status  ==  -1  ){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "** Error closing NDF file %s: %s",
            file_name, strerror( errno ) );
        return( IMS_ERROR );
    }
}
else{
    istat = CheckNonVDF (numfile,numrec,tdesc,fcurr,
        recknts,report_typ,title_buf,hdr_buf,rptfile,tmp2file,
        message, media_typ );
    if( istat  <  IMS_OK  ){
        (void) ims_msg( msgDesc, IMS_ERROR,
            message );
        return ( IMS_ERROR );
    }
    (void) strcpy( line_id, fcurr->line_id ); /* may not be set  */
}
return( IMS_OK );
}   /*  checkfile   */
